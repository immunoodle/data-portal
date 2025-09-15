# python_scripts/db_functions.py
import os
import psycopg2
from dotenv import load_dotenv
import json

# --- Environment Variable Loading ---
# Construct the path to the .env file relative to this script's location
# This makes it robust to where your R process is running from.
dotenv_path = os.path.join(os.path.dirname(__file__), '.env')

if os.path.exists(dotenv_path):
    load_dotenv(dotenv_path)
    # print(f"Loaded environment variables from: {dotenv_path}") # For debugging
else:
    # This case is expected in Docker/production where env vars are set directly.
    print(f"Info: .env file not found at {dotenv_path}. Relying on preset environment variables.")


def format_date_for_immport(date_val):
    if not date_val:
        return None
    return date_val.strftime("%d-%b-%Y")


def get_sample_json():
    sample_data = {
        "name": "Alice",
        "age": 30,
        "city": "Lebanon, NH",
        "interests": ["reading", "hiking", "coding"]
    }
    return json.dumps(sample_data, indent=4)



def get_protocol_data(conn, study_accession):
    """
    Retrieve protocol data for a given study accession and format it according to the ImmPort template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with protocol data formatted according to the ImmPort template
    """
    result = {
        "templateType": "single",
        "fileName": "protocols.json",
        "name": "protocols",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Query to get all protocols linked to the study through study_2_protocol
    query = """
        SELECT DISTINCT
            p.protocol_accession,
            p.file_name,
            p.name,
            p.type,
            p.description
        FROM madi_dat.protocol p
        JOIN madi_dat.study_2_protocol s2p ON p.protocol_accession = s2p.protocol_accession
        WHERE s2p.study_accession = %s
        
        UNION
        
        -- Also include protocols linked through experiments
        SELECT DISTINCT
            p.protocol_accession,
            p.file_name,
            p.name,
            p.type,
            p.description
        FROM madi_dat.protocol p
        JOIN madi_dat.experiment_2_protocol e2p ON p.protocol_accession = e2p.protocol_accession
        JOIN madi_dat.experiment e ON e2p.experiment_accession = e.experiment_accession
        WHERE e.study_accession = %s
        
        UNION
        
        -- Include protocols linked through lab_test_panel
        SELECT DISTINCT
            p.protocol_accession,
            p.file_name,
            p.name,
            p.type,
            p.description
        FROM madi_dat.protocol p
        JOIN madi_dat.lab_test_panel_2_protocol l2p ON p.protocol_accession = l2p.protocol_accession
        JOIN madi_dat.lab_test_panel ltp ON l2p.lab_test_panel_accession = ltp.lab_test_panel_accession
        WHERE ltp.study_accession = %s
        
        ORDER BY protocol_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving protocol data for study {study_accession}...")
        cur.execute(query, (study_accession, study_accession, study_accession))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} protocols")
        
        for row in rows:
            # Create entry with required fields
            protocol = {
                "userDefinedId": row[0],  # protocol_accession
                "fileName": row[1],       # file_name
                "name": row[2],           # name
                "type": row[3]            # type
            }
            
            # Add optional description if present
            if row[4]:
                protocol["description"] = row[4]
            
            # Only add entries that have all required fields
            if all(protocol.get(field) for field in ["userDefinedId", "fileName", "name", "type"]):
                result["data"].append(protocol)
        
        print(f"Processed {len(result['data'])} valid protocols")
    
    return result


def get_study_data(conn, study_accession):
    """
    Retrieve study data for a given study accession and format it according to the ImmPort template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with study data formatted according to the ImmPort template
    """
    # Initialize the result dictionary
    result = {
        "templateType": "compound",
        "fileName": "basic_study_design.json",
        "name": "basic_study_design",
        "schemaVersion": "3.37",
        "validationLevel": "1"
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get study data
    study_query = """
        SELECT * FROM madi_dat.study 
        WHERE study_accession = %s
    """
    study_data = execute_query(study_query, (study_accession,))
    if not study_data:
        return None
        
    # Get column names for study table
    with conn.cursor() as cur:
        cur.execute("SELECT column_name FROM information_schema.columns WHERE table_schema = 'madi_dat' AND table_name = 'study' ORDER BY ordinal_position")
        study_columns = [col[0] for col in cur.fetchall()]
    
    # Create dictionary from tuple using column names
    study = dict(zip(study_columns, study_data[0]))
    
    result["study"] = {
        "userDefinedId": study_accession,
        "briefTitle": study.get("brief_title"),
        "officialTitle": study.get("official_title"),
        "description": study.get("description"),
        "briefDescription": study.get("brief_description"),
        "interventionAgent": study.get("intervention_agent"),
        "endpoints": study.get("endpoints"),
        "sponsoringOrganization": study.get("sponsoring_organization"),
        "ageUnit": study.get("age_unit"),
        "actualStartDate": format_date_for_immport(study.get("actual_start_date")),
        "hypothesis": study.get("hypothesis"),
        "objectives": study.get("objectives"),
        "targetEnrollment": study.get("target_enrollment"),
        "minimumAge": study.get("minimum_age"),
        "maximumAge": study.get("maximum_age")
    }
    
    # Get study categorization
    cat_query = """
        SELECT research_focus FROM madi_dat.study_categorization 
        WHERE study_accession = %s
    """
    cat_data = execute_query(cat_query, (study_accession,))
    result["studyCategorization"] = {
        "researchFocus": cat_data[0][0] if cat_data else None
    }
    
    # Get conditions/diseases
    cond_query = """
        SELECT condition_reported FROM madi_dat.study_2_condition_or_disease 
        WHERE study_accession = %s
    """
    cond_data = execute_query(cond_query, (study_accession,))
    result["study2ConditionOrDisease"] = [
        {"conditionReported": row[0]} 
        for row in cond_data
    ]
    
    # Get arms/cohorts
    arm_query = """
        SELECT arm_accession, name, description, type_reported
        FROM madi_dat.arm_or_cohort 
        WHERE study_accession = %s
    """
    arm_data = execute_query(arm_query, (study_accession,))
    result["armOrCohort"] = [
        {
            "userDefinedId": row[0],
            "name": row[1],
            "description": row[2],
            "typeReported": row[3]
        }
        for row in arm_data
    ]
    
    # Get inclusion/exclusion criteria
    incl_query = """
        SELECT criterion_accession, criterion, criterion_category 
        FROM madi_dat.inclusion_exclusion 
        WHERE study_accession = %s
    """
    incl_data = execute_query(incl_query, (study_accession,))
    result["inclusionExclusion"] = [
        {
            "userDefinedId": row[0],
            "criterion": row[1],
            "criterionCategory": row[2]
        }
        for row in incl_data
    ]
    
    # Get study personnel
    pers_query = """
        SELECT person_accession, honorific, first_name, last_name, 
               suffixes, organization, orcid, email, title_in_study, 
               role_in_study, site_name 
        FROM madi_dat.study_personnel 
        WHERE study_accession = %s
    """
    pers_data = execute_query(pers_query, (study_accession,))
    result["studyPersonnel"] = [
        {
            "userDefinedId": row[0],
            "honorific": row[1],
            "firstName": row[2],
            "lastName": row[3],
            "suffixes": row[4],
            "organization": row[5],
            "orcidId": row[6],
            "email": row[7],
            "titleInStudy": row[8],
            "roleInStudy": row[9],
            "siteName": row[10]
        }
        for row in pers_data
    ]
    
    # Get planned visits
    visit_query = """
        SELECT planned_visit_accession, name, order_number, 
               min_start_day, max_start_day, start_rule, end_rule
        FROM madi_dat.planned_visit 
        WHERE study_accession = %s
    """
    visit_data = execute_query(visit_query, (study_accession,))
    result["plannedVisit"] = [
        {
            "userDefinedId": row[0],
            "name": row[1],
            "orderNumber": row[2],
            "minStartDay": row[3],
            "maxStartDay": row[4],
            "startRule": row[5],
            "endRule": row[6]
        }
        for row in visit_data
    ]
    
    # Get protocols
    prot_query = """
        SELECT protocol_accession FROM madi_dat.study_2_protocol 
        WHERE study_accession = %s
    """
    prot_data = execute_query(prot_query, (study_accession,))
    result["study2Protocol"] = [
        {"protocolId": row[0]} 
        for row in prot_data
    ]
    
    # Get study files
    file_query = """
        SELECT file_name, description, study_file_type 
        FROM madi_dat.study_file 
        WHERE study_accession = %s
    """
    file_data = execute_query(file_query, (study_accession,))
    result["studyFile"] = [
        {
            "fileName": row[0],
            "description": row[1],
            "studyFileType": row[2]
        }
        for row in file_data
    ]
    
    # Get study links
    link_query = """
        SELECT name, value, type
        FROM madi_dat.study_link 
        WHERE study_accession = %s
    """
    link_data = execute_query(link_query, (study_accession,))
    result["studyLink"] = [
        {
            "name": row[0],
            "value": row[1],
            "type": row[2]
        }
        for row in link_data
    ]
    
    # Get Pubmed references
    pub_query = """
        SELECT pubmed_id, doi, title, journal, year, month, issue, pages, authors
        FROM madi_dat.study_pubmed 
        WHERE study_accession = %s
    """
    pub_data = execute_query(pub_query, (study_accession,))
    result["studyPubmed"] = [
        {
            "pubmedId": row[0],
            "doi": row[1],
            "title": row[2],
            "journal": row[3],
            "year": row[4],
            "month": row[5],
            "issue": row[6],
            "pages": row[7],
            "authors": row[8]
        }
        for row in pub_data
    ]
    
    # Remove any empty arrays
    for key in list(result.keys()):
        if isinstance(result[key], list) and not result[key]:
            del result[key]
    
    # Remove None values from nested dictionaries
    for key, value in result.items():
        if isinstance(value, dict):
            result[key] = {k: v for k, v in value.items() if v is not None}
    
    return result

def get_experiment_data(conn, study_accession):
    """
    Retrieve experiment data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "experiments.json",
        "name": "experiments",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get experiments data
    exp_query = """
        SELECT experiment_accession, name, description, measurement_technique
        FROM madi_dat.experiment 
        WHERE study_accession = %s
    """
    exp_data = execute_query(exp_query, (study_accession,))
    
    # Get protocols for each experiment
    prot_query = """
        SELECT protocol_accession 
        FROM madi_dat.experiment_2_protocol 
        WHERE experiment_accession = %s
    """
    
    # Process each experiment
    for exp in exp_data:
        # Get protocols for this experiment
        prot_data = execute_query(prot_query, (exp[0],))
        protocol_ids = [row[0] for row in prot_data]
        
        # Create experiment entry
        experiment = {
            "userDefinedId": exp[0],
            "name": exp[1],
            "description": exp[2],
            "measurementTechnique": exp[3],
            "studyId": study_accession,
            "protocolIds": protocol_ids
        }
        
        result["data"].append(experiment)
    
    return result

def get_lab_test_panel_data(conn, study_accession):
    """
    Retrieve lab test panel data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "labTestPanels.json",
        "name": "labtestpanels",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get lab test panels data with required fields
    panel_query = """
        SELECT ltp.lab_test_panel_accession,
               ltp.name_reported,
               ltp.name_preferred
        FROM madi_dat.lab_test_panel ltp
        WHERE ltp.study_accession = %s
        ORDER BY ltp.lab_test_panel_accession
    """
    panel_data = execute_query(panel_query, (study_accession,))
    
    # Get protocols for each lab test panel
    prot_query = """
        SELECT protocol_accession 
        FROM madi_dat.lab_test_panel_2_protocol 
        WHERE lab_test_panel_accession = %s
        ORDER BY protocol_accession
    """
    
    # Process each lab test panel
    for panel in panel_data:
        # Get protocols for this panel
        prot_data = execute_query(prot_query, (panel[0],))
        protocol_ids = [row[0] for row in prot_data] if prot_data else []
        
        # Create lab test panel entry with required fields
        lab_test_panel = {
            "userDefinedId": panel[0],         # lab_test_panel_accession
            "studyId": study_accession,        # study_accession
            "nameReported": panel[1]           # name_reported
        }
        
        # Add optional fields
        optional_fields = {
            "namePreferred": panel[2],         # name_preferred
            "protocolIds": protocol_ids        # protocol_accessions
        }
        
        # Add non-null optional fields
        for key, value in optional_fields.items():
            if value:  # Add only if value exists and is not empty
                if key == "protocolIds" and isinstance(value, list):
                    if value:  # Only add if list is not empty
                        lab_test_panel[key] = value
                else:
                    lab_test_panel[key] = value
        
        result["data"].append(lab_test_panel)
    
    return result


def get_lab_test_results_data(conn, study_accession):
    """
    Retrieve lab test results data linked to lab test panels for a given study.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "labTest_Results.json",
        "name": "labtest_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get all lab test results for the study through lab test panels
    test_query = """
        SELECT lt.lab_test_accession,
               lt.lab_test_panel_accession,
               lt.biosample_accession,
               lt.name_reported,
               lt.result_value_reported,
               lt.result_unit_reported
        FROM madi_dat.lab_test lt
        JOIN madi_dat.lab_test_panel ltp ON lt.lab_test_panel_accession = ltp.lab_test_panel_accession
        WHERE ltp.study_accession = %s
        ORDER BY lt.lab_test_accession
    """
    test_data = execute_query(test_query, (study_accession,))
    
    # Process each lab test result
    for test in test_data:
        # Create lab test entry with required fields
        lab_test = {
            "userDefinedId": test[0],           # lab_test_accession
            "labTestPanelId": test[1],         # lab_test_panel_accession
            "biosampleId": test[2],            # biosample_accession
            "nameReported": test[3],           # name_reported
            "resultValueReported": test[4],    # result_value_reported
            "resultUnitReported": test[5]      # result_unit_reported
        }
        
        # Remove any None values from required fields
        if None in lab_test.values():
            continue  # Skip this record if any required field is None
        
        result["data"].append(lab_test)
    
    return result

def get_assessment_panel_data(conn, study_accession):
    """
    Retrieve assessment panel data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "assessmentpanel.json",
        "name": "assessmentpanel",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get assessment panels data
    panel_query = """
        SELECT assessment_panel_accession,
               name_reported,
               assessment_type,
               status
        FROM madi_dat.assessment_panel 
        WHERE study_accession = %s
        ORDER BY assessment_panel_accession
    """
    panel_data = execute_query(panel_query, (study_accession,))
    
    # Process each assessment panel
    for panel in panel_data:
        # Create base panel entry with required fields
        assessment_panel = {
            "userDefinedId": panel[0],         # assessment_panel_accession
            "studyId": study_accession,        # study_accession
            "nameReported": panel[1]           # name_reported
        }
        
        # Add optional fields if they have values
        optional_fields = {
            "assessmentType": panel[2],        # assessment_type
            "status": panel[3]                 # status
        }
        
        # Add non-null optional fields
        for key, value in optional_fields.items():
            if value is not None:
                assessment_panel[key] = value
        
        result["data"].append(assessment_panel)
    
    return result

def get_assessment_component_data(conn, study_accession):
    """
    Retrieve assessment component data linked to assessment panels for a given study.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "assessmentcomponent.json",
        "name": "assessmentcomponent",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get all assessment components for the study through assessment panels
    component_query = """
        SELECT ac.assessment_component_accession,
               ac.assessment_panel_accession,
               ac.subject_accession,
               ac.planned_visit_accession,
               ac.name_reported,
               ac.study_day,
               ac.result_value_reported,
               ac.result_unit_reported,
               ac.result_value_category,
               ac.age_at_onset_reported,
               ac.age_at_onset_unit_reported,
               ac.is_clinically_significant,
               ac.location_of_finding_reported,
               ac.organ_or_body_system_reported,
               ac.subject_position_reported,
               ac.time_of_day,
               ac.verbatim_question,
               ac.who_is_assessed
        FROM madi_dat.assessment_component ac
        JOIN madi_dat.assessment_panel ap ON ac.assessment_panel_accession = ap.assessment_panel_accession
        WHERE ap.study_accession = %s
        ORDER BY ac.assessment_component_accession
    """
    component_data = execute_query(component_query, (study_accession,))
    
    # Process each component
    for comp in component_data:
        # Create base component entry with required fields
        component = {
            "userDefinedId": comp[0],            # assessment_component_accession
            "assessmentPanelId": comp[1],        # assessment_panel_accession
            "subjectId": comp[2],                # subject_accession
            "plannedVisitId": comp[3],           # planned_visit_accession
            "nameReported": comp[4],             # name_reported
            "studyDay": comp[5]                  # study_day
        }
        
        # Add optional fields if they have values
        optional_fields = {
            "resultValueReported": comp[6],
            "resultUnitReported": comp[7],
            "resultValueCategory": comp[8],
            "ageAtOnsetReported": comp[9],
            "ageAtOnsetUnitReported": comp[10],
            "isClinicallySignificant": comp[11],
            "locationOfFindingReported": comp[12],
            "organOrBodySystemReported": comp[13],
            "subjectPositionReported": comp[14],
            "timeOfDay": comp[15],
            "verbatimQuestion": comp[16],
            "whoIsAssessed": comp[17]
        }
        
        # Add non-null optional fields
        for key, value in optional_fields.items():
            if value is not None:
                component[key] = value
        
        result["data"].append(component)
    
    return result

def get_adverse_events_data(conn, study_accession):
    """
    Retrieve adverse events data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "adverseEvents.json",
        "name": "adverseevents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get adverse events data
    events_query = """
        SELECT adverse_event_accession,
               subject_accession,
               name_reported,
               name_preferred,
               severity_reported,
               severity_preferred,
               outcome_reported,
               outcome_preferred,
               start_study_day,
               end_study_day,
               description,
               relation_to_study_treatment,
               location_of_reaction_reported,
               organ_or_body_system_reported,
               study_treatment_action_taken,
               relation_to_nonstudy_treatment,
               causality,
               start_time,
               end_time
        FROM madi_dat.adverse_event 
        WHERE study_accession = %s
        ORDER BY adverse_event_accession
    """
    events_data = execute_query(events_query, (study_accession,))
    
    # Process each adverse event
    for event in events_data:
        # Create base event entry with required fields
        adverse_event = {
            "userDefinedId": event[0],           # adverse_event_accession
            "subjectId": event[1],               # subject_accession
            "studyId": study_accession,          # study_accession
            "nameReported": event[2],            # name_reported
            "severityReported": event[4],        # severity_reported
            "outcomeReported": event[6],         # outcome_reported
            "relationToStudyTreatment": event[11]  # relation_to_study_treatment
        }
        
        # Add optional fields if they have values
        optional_fields = {
            "namePreferred": event[3],
            "severityPreferred": event[5],
            "outcomePreferred": event[7],
            "startStudyDay": event[8],
            "endStudyDay": event[9],
            "description": event[10],
            "locationOfReactionReported": event[12],
            "organOrBodySystemReported": event[13],
            "studyTreatmentActionTaken": event[14],
            "relationToNonstudyTreatment": event[15],
            "causality": event[16],
            "startTime": event[17],
            "endTime": event[18]
        }
        
        # Add non-null optional fields
        for key, value in optional_fields.items():
            if value is not None:
                adverse_event[key] = value
                
        # Handle numeric fields specifically
        numeric_fields = ["startStudyDay", "endStudyDay"]
        for field in numeric_fields:
            if field in adverse_event and adverse_event[field] is not None:
                adverse_event[field] = float(adverse_event[field])
        
        result["data"].append(adverse_event)
    
    return result


def get_subject_humans_data(conn, study_accession):
    """
    Retrieve subject humans data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "combined-result",
        "fileName": "subjectHumans.json",
        "name": "subjecthumans",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # First get all subjects for the study through arm_or_cohort
    subject_query = """
        SELECT DISTINCT s.subject_accession,
               s.gender,
               a2s.min_subject_age,
               a2s.max_subject_age,
               a2s.age_unit,
               a2s.age_event,
               a2s.age_event_specify,
               a2s.subject_phenotype,
               a2s.subject_location,
               s.ethnicity,
               s.race,
               s.race_specify,
               s.description,
               ac.arm_accession as arm_or_cohort_id
        FROM madi_dat.subject s
        JOIN madi_dat.arm_2_subject a2s ON s.subject_accession = a2s.subject_accession
        JOIN madi_dat.arm_or_cohort ac ON a2s.arm_accession = ac.arm_accession
        WHERE ac.study_accession = %s
        AND s.species = 'Homo sapiens'  -- Only include human subjects
    """
    subject_data = execute_query(subject_query, (study_accession,))
    
    # Get immune exposure data for each subject
    exposure_query = """
        SELECT exposure_process_reported,
               exposure_material_reported,
               exposure_material_id,
               disease_reported,
               disease_ontology_id,
               disease_stage_reported
        FROM madi_dat.immune_exposure
        WHERE subject_accession = %s
    """
    
    # Process each subject
    for subject in subject_data:
        # Create metadata for subject
        metadata = {
            "subjectId": subject[0],
            "sex": subject[1],
            "minSubjectAge": subject[2],
            "maxSubjectAge": subject[3],
            "ageUnit": subject[4],
            "ageEvent": subject[5],
            "ageEventSpecify": subject[6],
            "subjectPhenotype": subject[7],
            "subjectLocation": subject[8],
            "ethnicity": subject[9],
            "race": subject[10],
            "raceSpecify": subject[11],
            "description": subject[12],
            "armOrCohortId": subject[13]
        }
        
        # Remove None values from metadata
        metadata = {k: v for k, v in metadata.items() if v is not None}
        
        # Get immune exposures for this subject
        exposure_data = execute_query(exposure_query, (subject[0],))
        
        # Create result data from immune exposures
        result_data = []
        if exposure_data:
            for exp in exposure_data:
                exposure = {
                    "exposureProcessReported": exp[0],
                    "exposureMaterialReported": exp[1],
                    "exposureMaterialId": exp[2],
                    "diseaseReported": exp[3],
                    "diseaseOntologyId": exp[4],
                    "diseaseStageReported": exp[5]
                }
                # Remove None values
                exposure = {k: v for k, v in exposure.items() if v is not None}
                result_data.append(exposure)
        
        # Create combined subject entry
        subject_entry = {
            "metaData": metadata,
            "resultData": result_data
        }
        
        result["data"].append(subject_entry)
    
    return result


def get_subject_animals_data(conn, study_accession):
    """
    Retrieve subject animals data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "combined-result",
        "fileName": "subjectAnimals.json",
        "name": "subjectanimals",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # First get all subjects for the study through arm_or_cohort
    subject_query = """
        SELECT DISTINCT s.subject_accession,
               s.gender,
               a2s.min_subject_age,
               a2s.max_subject_age,
               a2s.age_unit,
               a2s.age_event,
               a2s.age_event_specify,
               a2s.subject_phenotype,
               a2s.subject_location,
               s.species,
               s.strain,
               s.strain_characteristics,
               ac.arm_accession as arm_or_cohort_id
        FROM madi_dat.subject s
        JOIN madi_dat.arm_2_subject a2s ON s.subject_accession = a2s.subject_accession
        JOIN madi_dat.arm_or_cohort ac ON a2s.arm_accession = ac.arm_accession
        WHERE ac.study_accession = %s
        AND s.species IS NOT NULL  -- Filter for animal subjects
        AND s.species != 'Homo sapiens'  -- Exclude human subjects
    """
    subject_data = execute_query(subject_query, (study_accession,))
    
    # Get immune exposure data for each subject
    exposure_query = """
        SELECT exposure_process_reported,
               exposure_material_reported,
               exposure_material_id,
               disease_reported,
               disease_ontology_id,
               disease_stage_reported
        FROM madi_dat.immune_exposure
        WHERE subject_accession = %s
    """
    
    # Process each subject
    for subject in subject_data:
        # Create metadata for subject
        metadata = {
            "subjectId": subject[0],
            "sex": subject[1],
            "minSubjectAge": subject[2],
            "maxSubjectAge": subject[3],
            "ageUnit": subject[4],
            "ageEvent": subject[5],
            "ageEventSpecify": subject[6],
            "subjectPhenotype": subject[7],
            "subjectLocation": subject[8],
            "species": subject[9],
            "strain": subject[10],
            "strainCharacteristics": subject[11],
            "armOrCohortId": subject[12]
        }
        
        # Remove None values from metadata
        metadata = {k: v for k, v in metadata.items() if v is not None}
        
        # Get immune exposures for this subject
        exposure_data = execute_query(exposure_query, (subject[0],))
        
        # Create result data from immune exposures
        result_data = []
        if exposure_data:
            for exp in exposure_data:
                exposure = {
                    "exposureProcessReported": exp[0],
                    "exposureMaterialReported": exp[1],
                    "exposureMaterialId": exp[2],
                    "diseaseReported": exp[3],
                    "diseaseOntologyId": exp[4],
                    "diseaseStageReported": exp[5]
                }
                # Remove None values
                exposure = {k: v for k, v in exposure.items() if v is not None}
                result_data.append(exposure)
        
        # Create combined subject entry
        subject_entry = {
            "metaData": metadata,
            "resultData": result_data
        }
        
        result["data"].append(subject_entry)
    
    return result



def get_biosamples_data(conn, study_accession):
    """
    Retrieve biosample data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "bioSamples.json",
        "name": "biosamples",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get biosamples data
    biosample_query = """
        SELECT b.biosample_accession,
               b.type,
               b.subtype,
               b.name,
               b.description,
               b.subject_accession,
               b.planned_visit_accession,
               b.study_time_collected,
               b.study_time_collected_unit,
               b.study_time_t0_event,
               b.study_time_t0_event_specify
        FROM madi_dat.biosample b
        WHERE b.study_accession = %s
    """
    biosample_data = execute_query(biosample_query, (study_accession,))
    
    # Get treatments for each biosample
    treatment_query = """
        SELECT treatment_accession 
        FROM madi_dat.biosample_2_treatment 
        WHERE biosample_accession = %s
    """
    
    # Process each biosample
    for sample in biosample_data:
        # Get treatments for this biosample
        treatment_data = execute_query(treatment_query, (sample[0],))
        treatment_ids = [row[0] for row in treatment_data] if treatment_data else None
        
        # Create biosample entry
        biosample = {
            "userDefinedId": sample[0],
            "type": sample[1],
            "subtype": sample[2],
            "name": sample[3],
            "description": sample[4],
            "subjectId": sample[5],
            "studyId": study_accession,
            "plannedVisitId": sample[6],
            "studyTimeCollected": sample[7],
            "studyTimeCollectedUnit": sample[8],
            "studyTimeT0Event": sample[9],
            "studyTimeT0EventSpecify": sample[10]
        }
        
        # Add treatments if any exist
        if treatment_ids:
            biosample["treatmentIds"] = treatment_ids
        
        # Remove None values
        biosample = {k: v for k, v in biosample.items() if v is not None}
        
        result["data"].append(biosample)
    
    return result

def get_interventions_data(conn, study_accession):
    """
    Retrieve interventions data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "interventions.json",
        "name": "interventions",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get interventions data
    intervention_query = """
        SELECT intervention_accession,
               subject_accession,
               name_reported,
               compound_name_reported,
               compound_role,
               dose_reported,
               start_day,
               end_day,
               status,
               reported_indication,
               formulation,
               dose,
               dose_units,
               dose_freq_per_interval,
               route_of_admin_reported,
               is_ongoing,
               start_time,
               end_time,
               duration,
               duration_unit
        FROM madi_dat.intervention
        WHERE study_accession = %s
    """
    intervention_data = execute_query(intervention_query, (study_accession,))
    
    # Process each intervention
    for intervention in intervention_data:
        # Create intervention entry with required fields
        intervention_entry = {
            "userDefinedId": intervention[0],      # intervention_accession
            "subjectId": intervention[1],          # subject_accession
            "studyId": study_accession,
            "nameReported": intervention[2],       # name_reported
            "compoundNameReported": intervention[3], # compound_name_reported
            "compoundRole": intervention[4],       # compound_role
            "doseReported": intervention[5]        # dose_reported
        }
        
        # Optional fields mapping
        optional_fields = {
            "startDay": intervention[6],
            "endDay": intervention[7],
            "status": intervention[8],
            "reportedIndication": intervention[9],
            "formulation": intervention[10],
            "dose": intervention[11],
            "doseUnits": intervention[12],
            "doseFreqPerInterval": intervention[13],
            "routeOfAdminReported": intervention[14],
            "isOngoing": intervention[15],
            "startTime": intervention[16],
            "endTime": intervention[17],
            "duration": intervention[18],
            "durationUnit": intervention[19]
        }
        
        # Add non-null optional fields
        for key, value in optional_fields.items():
            if value is not None:
                intervention_entry[key] = value
        
        result["data"].append(intervention_entry)
    
    return result

def get_immune_exposure_data(conn, study_accession):
    """
    Retrieve immune exposure data for a given study accession and format it according to the template.
    """
    # Initialize the result dictionary
    result = {
        "templateType": "single",
        "fileName": "immuneExposure.json",
        "name": "immuneexposure",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    # Get immune exposure data
    exposure_query = """
        SELECT ie.exposure_accession,
               ie.subject_accession,
               ie.arm_accession,
               ie.exposure_process_reported,
               ie.exposure_material_reported,
               ie.exposure_material_id,
               ie.disease_reported,
               ie.disease_ontology_id,
               ie.disease_stage_reported
        FROM madi_dat.immune_exposure ie
        JOIN madi_dat.arm_or_cohort ac ON ie.arm_accession = ac.arm_accession
        WHERE ac.study_accession = %s
        ORDER BY ie.exposure_accession
    """
    exposure_data = execute_query(exposure_query, (study_accession,))
    
    # Process each immune exposure
    for exposure in exposure_data:
        # Create exposure entry with required fields
        exposure_entry = {
            "userDefinedId": exposure[0],        # immune_exposure_accession
            "subjectId": exposure[1],            # subject_accession
            "armOrCohortId": exposure[2],        # arm_accession
            "exposureProcessReported": exposure[3]  # exposure_process_reported
        }
        
        # Add optional fields based on exposure process requirements
        optional_fields = {
            "exposureMaterialReported": exposure[4],
            "exposureMaterialId": exposure[5],
            "diseaseReported": exposure[6],
            "diseaseOntologyId": exposure[7],
            "diseaseStageReported": exposure[8]
        }
        
        # Check exposure process type and add required fields
        exposure_process = exposure[3]
        if exposure_process:
            # Add exposure material for processes that require it
            if exposure_process in [
                "administering substance in vivo",
                "documented exposure without evidence for disease",
                "environmental exposure to endemic/ubiquitous agent without evidence for disease",
                "exposure to substance without evidence for disease",
                "exposure with existing immune reactivity without evidence for disease",
                "infectious challenge",
                "occurrence of allergy",
                "occurrence of asymptomatic infection",
                "occurrence of infectious disease",
                "transplantation or transfusion",
                "vaccination"
            ]:
                if optional_fields["exposureMaterialReported"]:
                    exposure_entry["exposureMaterialReported"] = optional_fields["exposureMaterialReported"]
                    if optional_fields["exposureMaterialId"]:
                        exposure_entry["exposureMaterialId"] = optional_fields["exposureMaterialId"]
            
            # Add disease fields for processes that require them
            if exposure_process in [
                "occurrence of allergy",
                "occurrence of autoimmune disease",
                "occurrence of cancer",
                "occurrence of disease",
                "occurrence of infectious disease"
            ]:
                if optional_fields["diseaseReported"]:
                    exposure_entry["diseaseReported"] = optional_fields["diseaseReported"]
                    if optional_fields["diseaseOntologyId"]:
                        exposure_entry["diseaseOntologyId"] = optional_fields["diseaseOntologyId"]
                    if optional_fields["diseaseStageReported"]:
                        exposure_entry["diseaseStageReported"] = optional_fields["diseaseStageReported"]
        
        result["data"].append(exposure_entry)
    
    return result


def get_lab_tests_data(conn, study_accession):
    """
    Retrieve combined lab tests data for a given study accession and format it according to the template.
    """
    result = {
        "templateType": "combined-result",
        "fileName": "labTests.json",
        "name": "labtest",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Single query to get all required data with proper joins
    combined_query = """
        WITH panel_protocols AS (
            SELECT 
                ltp.lab_test_panel_accession,
                ltp.name_reported as panel_name,
                string_agg(l2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.lab_test_panel ltp
            LEFT JOIN madi_dat.lab_test_panel_2_protocol l2p 
            ON ltp.lab_test_panel_accession = l2p.lab_test_panel_accession
            WHERE ltp.study_accession = %s
            GROUP BY ltp.lab_test_panel_accession, ltp.name_reported
        )
        SELECT 
            b.biosample_accession,
            b.subject_accession,
            b.planned_visit_accession,
            b.type,
            b.subtype,
            b.name,
            b.description,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            lt.lab_test_accession,
            lt.lab_test_panel_accession,
            lt.name_reported as test_name,
            lt.result_value_reported,
            lt.result_unit_reported,
            pp.panel_name,
            pp.protocol_ids
        FROM madi_dat.biosample b
        LEFT JOIN madi_dat.lab_test lt ON b.biosample_accession = lt.biosample_accession
        LEFT JOIN panel_protocols pp ON lt.lab_test_panel_accession = pp.lab_test_panel_accession
        WHERE b.study_accession = %s
        ORDER BY b.biosample_accession, lt.lab_test_panel_accession, lt.lab_test_accession
    """
    
    with conn.cursor() as cur:
        cur.execute(combined_query, (study_accession, study_accession))
        rows = cur.fetchall()
    
    # Process results using dictionaries for faster lookups
    current_biosample = None
    current_entry = None
    panels_seen = set()
    
    for row in rows:
        biosample_id = row[0]
        
        # If we've moved to a new biosample, create new entry
        if biosample_id != current_biosample:
            if current_entry:
                result["data"].append(current_entry)
            
            # Create metadata section
            metadata = {
                "studyId": study_accession,
                "biosampleId": row[0],
                "subjectId": row[1],
                "plannedVisitId": row[2],
                "type": row[3]
            }
            
            # Add optional biosample fields
            optional_fields = {
                "subtype": row[4],
                "name": row[5],
                "description": row[6],
                "studyTimeCollected": row[7],
                "studyTimeCollectedUnit": row[8],
                "studyTimeT0Event": row[9],
                "studyTimeT0EventSpecify": row[10]
            }
            
            metadata.update({k: v for k, v in optional_fields.items() if v is not None})
            
            current_entry = {
                "metaData": metadata,
                "resultData": []
            }
            current_biosample = biosample_id
            panels_seen = set()
        
        # Process lab test result if exists
        if row[11]:  # if lab_test_accession exists
            panel_id = row[12]
            
            # Find or create panel in resultData
            if panel_id not in panels_seen:
                panel_data = {
                    "labTestPanelId": panel_id,
                    "nameReported": row[16],  # panel_name
                    "protocolIds": row[17].split(';') if row[17] else [],
                    "results": []
                }
                current_entry["resultData"].append(panel_data)
                panels_seen.add(panel_id)
            
            # Add test result to appropriate panel
            panel_index = len(current_entry["resultData"]) - 1
            current_entry["resultData"][panel_index]["results"].append({
                "userDefinedId": row[11],
                "nameReported": row[13],
                "resultValueReported": row[14],
                "resultUnitReported": row[15]
            })
    
    # Add last entry if exists
    if current_entry:
        result["data"].append(current_entry)
    
    return result

def get_control_samples_data(conn, study_accession, immport_template_settings=None):
    """
    Retrieve control samples data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        immport_template_settings: Dict with global settings for ImmPort template fields
                                 {
                                     "immportTemplate": "Yes" or "No",
                                     "resultFileName": "filename.txt" (if immportTemplate is "No"),
                                     "additionalResultFileNames": "file1.txt;file2.txt"
                                 }
    """
    # Default settings if none provided
    if immport_template_settings is None:
        immport_template_settings = {
            "immportTemplate": "Yes",  # Default to Yes for MBAA standard template
            "resultFileName": None,    # Only needed if immportTemplate is "No"
            "additionalResultFileNames": None
        }
    
    result = {
        "templateType": "combined",
        "fileName": "controlSamples.json",
        "name": "controlsamples",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            cs.control_sample_accession,
            cs.source,
            cs.catalog_id,
            cs.dilution_factor,
            cs.assay_id,
            cs.assay_group_id,
            cs.lot_number,
            e.experiment_accession,
            e.study_accession,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique,
            string_agg(DISTINCT ep.protocol_accession, ';') as protocol_ids
        FROM madi_dat.control_sample cs
        JOIN madi_dat.experiment e ON cs.experiment_accession = e.experiment_accession
        LEFT JOIN madi_dat.experiment_2_protocol ep ON e.experiment_accession = ep.experiment_accession
        WHERE e.study_accession = %s
        GROUP BY 
            cs.control_sample_accession,
            cs.source,
            cs.catalog_id,
            cs.dilution_factor,
            cs.assay_id,
            cs.assay_group_id,
            cs.lot_number,
            e.experiment_accession,
            e.study_accession,
            e.name,
            e.description,
            e.measurement_technique
        ORDER BY cs.control_sample_accession
    """
    
    with conn.cursor() as cur:
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        for row in rows:
            # Create flat entry structure
            entry = {
                "controlSampleId": row[0],      # control_sample_accession
                "source": row[1],               # source
                "catalogId": row[2],            # catalog_id
                "dilutionFactor": row[3],       # dilution_factor
                "assayId": row[4],              # assay_id
                "experimentId": row[7],          # experiment_accession
                "studyId": row[8],              # study_accession
                "name": row[9],                 # experiment_name
                "measurementTechnique": row[11], # measurement_technique
                "protocolIds": row[12].split(';') if row[12] else []  # protocol_ids
            }
            
            # Add ImmPort template fields (same for all samples in this batch)
            entry["immportTemplate"] = immport_template_settings["immportTemplate"]
            
            # Only add result file name if immportTemplate is "No"
            if immport_template_settings["immportTemplate"] == "No" and immport_template_settings.get("resultFileName"):
                entry["resultFileName"] = immport_template_settings["resultFileName"]
                
            # Add additional result files if specified
            if immport_template_settings.get("additionalResultFileNames"):
                entry["additionalResultFileNames"] = immport_template_settings["additionalResultFileNames"]
            
            # Add optional fields if they have values
            optional_fields = {
                "assayGroupId": row[5],         # assay_group_id
                "lotNumber": row[6],            # lot_number
                "description": row[10]           # experiment_description
            }
            
            for key, value in optional_fields.items():
                if value is not None:
                    entry[key] = value
            
            result["data"].append(entry)
    
    return result


def get_assessments_data(conn, study_accession):
    """
    Retrieve combined assessment data for a given study accession.
    """

    def convert_to_float(value):
        """Helper function to safely convert values to float"""
        if value is None:
            return None
        try:
            return float(value)
        except (ValueError, TypeError):
            return value  # Return original value if conversion fails
    
    result = {
        "templateType": "combined-result",
        "fileName": "assessments.json",
        "name": "assessments",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Query to get assessment panels with their components
    query = """
        WITH panel_data AS (
            SELECT DISTINCT
                ap.assessment_panel_accession,
                ap.name_reported as panel_name,
                ap.assessment_type,
                ap.status,
                ac.subject_accession,
                ac.assessment_component_accession,
                ac.planned_visit_accession,
                ac.name_reported as component_name,
                ac.study_day,
                ac.age_at_onset_reported,
                ac.age_at_onset_unit_reported,
                ac.is_clinically_significant,
                ac.location_of_finding_reported,
                ac.organ_or_body_system_reported,
                ac.result_value_reported,
                ac.result_unit_reported,
                ac.result_value_category,
                ac.subject_position_reported,
                ac.time_of_day,
                ac.verbatim_question,
                ac.who_is_assessed
            FROM madi_dat.assessment_panel ap
            LEFT JOIN madi_dat.assessment_component ac 
                ON ap.assessment_panel_accession = ac.assessment_panel_accession
            WHERE ap.study_accession = %s
            ORDER BY ap.assessment_panel_accession, ac.assessment_component_accession
        )
        SELECT * FROM panel_data
    """
    
    with conn.cursor() as cur:
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        # Track current subject and panel for grouping
        current_subject = None
        current_panel = None
        current_entry = None
        
        for row in rows:
            subject_id = row[4]  # subject_accession
            panel_id = row[0]    # assessment_panel_accession
            
            # If we've moved to a new subject-panel combination
            if subject_id != current_subject or panel_id != current_panel:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "subjectId": subject_id,
                    "assessmentPanelId": panel_id,
                    "studyId": study_accession,
                    "nameReported": row[1]  # panel_name
                }
                
                # Add optional metadata fields
                optional_metadata = {
                    "assessmentType": row[2],
                    "status": row[3]
                }
                metadata.update({k: v for k, v in optional_metadata.items() if v is not None})
                
                # Initialize new entry
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_subject = subject_id
                current_panel = panel_id
            
            # Add component data if it exists
            if row[5]:  # if assessment_component_accession exists
                component = {
                    "userDefinedId": row[5],         # assessment_component_accession
                    "plannedVisitId": row[6],        # planned_visit_accession
                    "nameReported": row[7],          # component_name
                    "studyDay": float(row[8]) if row[8] is not None else None  # study_day
                }
                
                # Add optional component fields
                optional_fields = {
                    "ageAtOnsetReported": float(row[9]) if row[9] is not None else None,
                    "ageAtOnsetUnitReported": row[10],
                    "isClinicallySignificant": row[11],
                    "locationOfFindingReported": row[12],
                    "organOrBodySystemReported": row[13],
                    "resultValueReported": convert_to_float(row[14]),  # Using safe conversion
                    "resultUnitReported": row[15],
                    "resultValueCategory": row[16],
                    "subjectPositionReported": row[17],
                    "timeOfDay": row[18],
                    "verbatimQuestion": row[19],
                    "whoIsAssessed": row[20]
                }
                
                # Add non-null optional fields
                component.update({k: v for k, v in optional_fields.items() if v is not None})
                
                # Skip if missing required fields
                if all(component.get(field) is not None for field in ["userDefinedId", "plannedVisitId", "nameReported", "studyDay"]):
                    current_entry["resultData"].append(component)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
    
    return result


def get_cytof_derived_data(conn, study_accession):
    """
    Retrieve CyTOF derived data for a given study accession and format it according to the template.
    """
    result = {
        "templateType": "multiple",
        "fileName": "CyTOF_Derived_data.json",
        "name": "cytof_derived_data",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Query to get CyTOF derived data
    query = """
        WITH experiment_files AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(f.name, ';') as workspace_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        )
        SELECT 
            far.expsample_accession,
            far.population_name_reported,
            far.population_defnition_reported,
            far.parent_population_reported,
            far.population_statistic_reported,
            far.population_stat_unit_reported,
            ef.workspace_files,
            far.comments
        FROM madi_dat.fcs_analyzed_result far
        JOIN madi_dat.expsample_2_file_info e2f 
            ON far.expsample_accession = e2f.expsample_accession
        JOIN experiment_files ef 
            ON far.expsample_accession = ef.expsample_accession
        JOIN madi_dat.expsample e 
            ON far.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp 
            ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY far.expsample_accession
    """
    
    with conn.cursor() as cur:
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expSampleId": row[0],
                "populationNameReported": row[1],
                "gatingDefinitionReported": row[2],
                "populationStatisticCountPercentileEtc": row[4],
                "populationStatUnitReported": row[5],
                "workspaceFile": row[6].split(';')[0] if row[6] else None  # Take first workspace file
            }
            
            # Add optional fields if they have values
            if row[3]:  # parent_population_reported
                entry["parentPopulationReported"] = row[3]
            
            if row[7]:  # comments
                entry["comments"] = row[7]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in [
                "expSampleId", 
                "populationNameReported", 
                "gatingDefinitionReported",
                "populationStatisticCountPercentileEtc",
                "populationStatUnitReported",
                "workspaceFile"
            ]):
                result["data"].append(entry)
    
    return result


def get_elisa_results_data(conn, study_accession):
    """
    Retrieve ELISA results data for a given study accession and format it according to the template.
    """
    result = {
        "templateType": "multiple",
        "fileName": "ELISA_Results.json",
        "name": "elisa_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            er.expsample_accession,
            er.analyte_reported,
            er.value_reported,
            er.unit_reported,
            er.comments
        FROM madi_dat.elisa_result er
        JOIN madi_dat.expsample e 
            ON er.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp 
            ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY er.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving ELISA results for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} ELISA results")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expSampleId": row[0],
                "analyteReported": row[1],
                "valueReported": row[2],
                "unitReported": row[3]
            }
            
            # Add optional comments if present
            if row[4]:
                entry["comments"] = row[4]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in [
                "expSampleId",
                "analyteReported", 
                "valueReported",
                "unitReported"
            ]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid ELISA results")
    
    return result


def get_elispot_results_data(conn, study_accession):
    """
    Retrieve ELISPOT results data for a given study accession and format it according to the template.
    """
    result = {
        "templateType": "multiple",
        "fileName": "ELISPOT_Results.json",
        "name": "elispot_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            er.expsample_accession,
            er.analyte_reported,
            er.spot_number_reported,
            er.cell_number_reported,
            er.comments
        FROM madi_dat.elispot_result er
        JOIN madi_dat.expsample e 
            ON er.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp 
            ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY er.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving ELISPOT results for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} ELISPOT results")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expSampleId": row[0],
                "analyteReported": row[1],
                "spotNumberReported": row[2],
                "cellNumberReported": row[3]
            }
            
            # Add optional comments if present
            if row[4]:
                entry["comments"] = row[4]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in [
                "expSampleId",
                "analyteReported", 
                "spotNumberReported",
                "cellNumberReported"
            ]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid ELISPOT results")
    
    return result



def get_cytof_experiment_samples_data(conn, study_accession):
    """
    Retrieve CYTOF experiment samples data for a given study accession.
    """
    result = {
        "templateType": "combined-multiple",
        "fileName": "experimentSamples.CYTOF.json",
        "name": "cytof",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT 
            es.expsample_accession as expsample_id,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession as biosample_id,
            b.subject_accession as subject_id,
            b.planned_visit_accession as planned_visit_id,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession as experiment_id,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique = 'CyTOF'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving CYTOF experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} CYTOF experiment samples")
        
        for row in rows:
            # Split file names into result file and additional files
            files = row[5].split(';') if row[5] else []
            result_file = files[0] if files else None
            additional_files = files[1:] if len(files) > 1 else None
            
            entry = {
                "studyId": study_accession,
                "expsampleId": row[0],
                "reagentIds": row[3].split(';') if row[3] else None,
                "treatmentIds": row[4].split(';') if row[4] else None,
                "expsampleName": row[1],
                "expsampleDescription": row[2],
                "resultFileName": result_file,
                "additionalResultFileNames": additional_files,
                "biosampleId": row[6],
                "subjectId": row[7],
                "plannedVisitId": row[8],
                "type": row[9],
                "subtype": row[10],
                "biosampleName": row[11],
                "biosampleDescription": row[12],
                "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                "studyTimeCollectedUnit": row[14],
                "studyTimeT0Event": row[15],
                "studyTimeT0EventSpecify": row[16],
                "experimentId": row[17],
                "protocolIds": row[18].split(';') if row[18] else None,
                "experimentName": row[19],
                "experimentDescription": row[20],
                "measurementTechnique": row[21]
            }
            
            # Remove None values
            entry = {k: v for k, v in entry.items() if v is not None}
            
            result["data"].append(entry)
            
        print(f"Processed {len(result['data'])} CYTOF experiment samples")
    
    return result


def get_virus_neutralization_experiment_samples_data(conn, study_accession):
    """
    Retrieve Virus Neutralization experiment samples data for a given study accession and format it according to the template.
    """
    result = {
        "templateType": "combined-result",
        "fileName": "experimentSamples.Virus_Neutralization.json",
        "name": "virus_neutralization",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Helper function to execute queries and fetch results
    def execute_query(query, params=None):
        with conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchall()
    
    print(f"\nRetrieving virus neutralization experiment samples for study {study_accession}...")
    
    # Get virus neutralization experiment samples with all required data using the optimized query
    main_query = """
    WITH file_info AS (
        SELECT
            e2f.expsample_accession,
            string_agg(DISTINCT f.name, ';') as result_files
        FROM madi_dat.expsample_2_file_info e2f
        JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
        GROUP BY e2f.expsample_accession
    ),
    reagent_info AS (
        SELECT
            e2r.expsample_accession,
            string_agg(e2r.reagent_accession, ';') as reagent_ids
        FROM madi_dat.expsample_2_reagent e2r
        GROUP BY e2r.expsample_accession
    ),
    treatment_info AS (
        SELECT
            e2t.expsample_accession,
            string_agg(e2t.treatment_accession, ';') as treatment_ids
        FROM madi_dat.expsample_2_treatment e2t
        GROUP BY e2t.expsample_accession
    ),
    protocol_info AS (
        SELECT
            e2p.experiment_accession,
            string_agg(e2p.protocol_accession, ';') as protocol_ids
        FROM madi_dat.experiment_2_protocol e2p
        GROUP BY e2p.experiment_accession
    )
    SELECT
        es.expsample_accession,
        es.name as expsample_name,
        es.description as expsample_description,
        r.reagent_ids,
        t.treatment_ids,
        f.result_files,
        b.biosample_accession,
        b.type,
        b.subtype,
        b.name as biosample_name,
        b.description as biosample_description,
        b.subject_accession,
        b.planned_visit_accession,
        b.study_time_collected,
        b.study_time_collected_unit,
        b.study_time_t0_event,
        b.study_time_t0_event_specify,
        e.experiment_accession,
        p.protocol_ids,
        e.name as experiment_name,
        e.description as experiment_description,
        e.measurement_technique,
        vn.virus_strain_reported,
        vn.value_reported,
        vn.unit_reported,
        vn.comments
    FROM madi_dat.expsample es
    JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
    JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
    JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
    LEFT JOIN madi_dat.neut_ab_titer_result vn ON es.expsample_accession = vn.expsample_accession
    LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
    LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
    LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
    LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
    WHERE e.study_accession = %s
    AND vn.expsample_accession IS NOT NULL  -- <-- This is the corrected line
    ORDER BY es.expsample_accession, vn.virus_strain_reported
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving virus neutralization experiment samples for study {study_accession}...")
        cur.execute(main_query, (study_accession,))
        rows = cur.fetchall()
        
        current_sample = None
        current_entry = None
        
        for row in rows:
            expsample_id = row[0]
            
            # If we've moved to a new sample, create new entry
            if expsample_id != current_sample:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "studyId": study_accession,
                    "expsampleId": expsample_id,
                    "expsampleName": row[1],
                    "expsampleDescription": row[2],
                    "reagentIds": row[3].split(';') if row[3] else None,
                    "treatmentIds": row[4].split(';') if row[4] else None,
                    "additionalResultFileNames": row[5].split(';') if row[5] else None,
                    "biosampleId": row[6],
                    "type": row[7],
                    "subtype": row[8],
                    "biosampleName": row[9],
                    "biosampleDescription": row[10],
                    "subjectId": row[11],
                    "plannedVisitId": row[12],
                    "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                    "studyTimeCollectedUnit": row[14],
                    "studyTimeT0Event": row[15],
                    "studyTimeT0EventSpecify": row[16],
                    "experimentId": row[17],
                    "protocolIds": row[18].split(';') if row[18] else None,
                    "experimentName": row[19],
                    "experimentDescription": row[20],
                    "measurementTechnique": row[21]
                }
                
                # Remove None values from metadata
                metadata = {k: v for k, v in metadata.items() if v is not None}
                
                # Initialize new entry with empty result data
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_sample = expsample_id
            
            # Add result data if it exists
            if row[22]:  # if virus_strain_reported exists
                result_data = {
                    "virusStrainReported": row[22],
                    "valueReported": row[23],
                    "unitReported": row[24]
                }
                
                if row[25]:  # comments
                    result_data["comments"] = row[25]
                
                current_entry["resultData"].append(result_data)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
        
        print(f"Processed {len(result['data'])} virus neutralization experiment samples")
    
    return result


def get_elisa_experiment_samples_data(conn, study_accession):
    """
    Retrieve ELISA experiment samples data for a given study accession.
    """
    result = {
        "templateType": "combined-result",
        "fileName": "experimentSamples.ELISA.json",
        "name": "elisa",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.subject_accession,
            b.planned_visit_accession,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique,
            er.analyte_reported,
            er.value_reported,
            er.unit_reported,
            er.comments
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN madi_dat.elisa_result er ON es.expsample_accession = er.expsample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique = 'ELISA'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving ELISA experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        current_sample = None
        current_entry = None
        
        for row in rows:
            expsample_id = row[0]
            
            # If we've moved to a new sample, create new entry
            if expsample_id != current_sample:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "studyId": study_accession,
                    "expsampleId": expsample_id,
                    "expsampleName": row[1],
                    "expsampleDescription": row[2],
                    "reagentIds": row[3].split(';') if row[3] else None,
                    "treatmentIds": row[4].split(';') if row[4] else None,
                    "biosampleId": row[6],
                    "type": row[7],
                    "subtype": row[8],
                    "biosampleName": row[9],
                    "biosampleDescription": row[10],
                    "subjectId": row[11],
                    "plannedVisitId": row[12],
                    "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                    "studyTimeCollectedUnit": row[14],
                    "studyTimeT0Event": row[15],
                    "studyTimeT0EventSpecify": row[16],
                    "experimentId": row[17],
                    "protocolIds": row[18].split(';') if row[18] else None,
                    "experimentName": row[19],
                    "experimentDescription": row[20],
                    "measurementTechnique": row[21]
                }
                
                # Remove None values from metadata
                metadata = {k: v for k, v in metadata.items() if v is not None}
                
                # Initialize new entry with empty result data
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_sample = expsample_id
            
            # Add result data if it exists
            if row[22]:  # if analyte_reported exists
                result_data = {
                    "analyteReported": row[22],
                    "valueReported": row[23],
                    "unitReported": row[24]
                }
                
                if row[25]:  # comments
                    result_data["comments"] = row[25]
                
                current_entry["resultData"].append(result_data)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
        
        print(f"Processed {len(result['data'])} ELISA experiment samples")
    
    return result


def get_elispot_experiment_samples_data(conn, study_accession):
    """
    Retrieve ELISPOT experiment samples data for a given study accession.
    """
    result = {
        "templateType": "combined-result",
        "fileName": "experimentSamples.ELISPOT.json",
        "name": "elispot",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.subject_accession,
            b.planned_visit_accession,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique,
            er.analyte_reported,
            er.spot_number_reported,
            er.cell_number_reported,
            er.comments
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN madi_dat.elispot_result er ON es.expsample_accession = er.expsample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique = 'ELISPOT'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving ELISPOT experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        current_sample = None
        current_entry = None
        
        for row in rows:
            expsample_id = row[0]
            
            # If we've moved to a new sample, create new entry
            if expsample_id != current_sample:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "studyId": study_accession,
                    "expsampleId": expsample_id,
                    "expsampleName": row[1],
                    "expsampleDescription": row[2],
                    "reagentIds": row[3].split(';') if row[3] else None,
                    "treatmentIds": row[4].split(';') if row[4] else None,
                    "biosampleId": row[6],
                    "type": row[7],
                    "subtype": row[8],
                    "biosampleName": row[9],
                    "biosampleDescription": row[10],
                    "subjectId": row[11],
                    "plannedVisitId": row[12],
                    "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                    "studyTimeCollectedUnit": row[14],
                    "studyTimeT0Event": row[15],
                    "studyTimeT0EventSpecify": row[16],
                    "experimentId": row[17],
                    "protocolIds": row[18].split(';') if row[18] else None,
                    "experimentName": row[19],
                    "experimentDescription": row[20],
                    "measurementTechnique": row[21]
                }
                
                # Remove None values from metadata
                metadata = {k: v for k, v in metadata.items() if v is not None}
                
                # Initialize new entry with empty result data
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_sample = expsample_id
            
            # Add result data if it exists
            if row[22]:  # if analyte_reported exists
                result_data = {
                    "analyteReported": row[22],
                    "spotNumberReported": row[23],
                    "cellNumberReported": row[24]
                }
                
                if row[25]:  # comments
                    result_data["comments"] = row[25]
                
                current_entry["resultData"].append(result_data)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
        
        print(f"Processed {len(result['data'])} ELISPOT experiment samples")
    
    return result


def get_flow_cytometry_experiment_samples_data(conn, study_accession):
    """
    Retrieve Flow Cytometry experiment samples data for a given study accession.
    """
    result = {
        "templateType": "combined-multiple",
        "fileName": "experimentSamples.Flow_Cytometry.json",
        "name": "flow_cytometry",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Query remains the same until the WHERE clause
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT 
                    CASE 
                        WHEN f.name LIKE '%.fcs' THEN f.name
                    END, ';') as fcs_files,
                string_agg(DISTINCT 
                    CASE 
                        WHEN f.name NOT LIKE '%.fcs' THEN f.name
                    END, ';') as other_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.fcs_files,
            f.other_files,
            b.biosample_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.subject_accession,
            b.planned_visit_accession,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %(study_id)s
        AND e.measurement_technique = 'Flow Cytometry'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Flow Cytometry experiment samples for study {study_accession}...")
        cur.execute(query, {'study_id': study_accession})  # Using named parameter
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} Flow Cytometry experiment samples")
        
        for row in rows:
            entry = {
                "studyId": study_accession,
                "expsampleId": row[0],
                "expsampleName": row[1],
                "expsampleDescription": row[2],
                "reagentIds": row[3].split(';') if row[3] else None,
                "treatmentIds": row[4].split(';') if row[4] else None,
                "fcsResultFile": row[5].split(';')[0] if row[5] else None,  # Take first .fcs file
                "additionalResultFileNames": row[6].split(';') if row[6] else None,
                "biosampleId": row[7],
                "type": row[8],
                "subtype": row[9],
                "biosampleName": row[10],
                "biosampleDescription": row[11],
                "subjectId": row[12],
                "plannedVisitId": row[13],
                "studyTimeCollected": float(row[14]) if row[14] is not None else None,
                "studyTimeCollectedUnit": row[15],
                "studyTimeT0Event": row[16],
                "studyTimeT0EventSpecify": row[17],
                "experimentId": row[18],
                "protocolIds": row[19].split(';') if row[19] else None,
                "experimentName": row[20],
                "experimentDescription": row[21],
                "measurementTechnique": row[22]
            }
            
            # Remove None values
            entry = {k: v for k, v in entry.items() if v is not None}
            
            result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} Flow Cytometry experiment samples")
    
    return result


def get_mbaa_experiment_samples_data(conn, study_accession):
    """
    Retrieve MBAA (Multiplex Bead Array Assay) experiment samples data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with MBAA experiment samples data formatted according to the ImmPort template
    """
    result = {
        "templateType": "combined-multiple",
        "fileName": "experimentSamples.MBAA.json",
        "name": "mbaa",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        ),
        mbaa_details AS (
            SELECT 
                emd.expsample_accession,
                emd.assay_id,
                emd.dilution_factor,
                emd.assay_group_id,
                emd.plate_type
            FROM madi_dat.expsample_mbaa_detail emd
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            md.assay_id,
            md.dilution_factor,
            md.assay_group_id,
            md.plate_type,
            b.biosample_accession,
            b.subject_accession,
            b.planned_visit_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN mbaa_details md ON es.expsample_accession = md.expsample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique = 'Luminex xMAP'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving MBAA experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} MBAA experiment samples")
        
        for row in rows:
            # Split file names
            files = row[5].split(';') if row[5] else []
            result_file = files[0] if files else None
            additional_files = files[1:] if len(files) > 1 else None
            
            # Create entry
            entry = {
                "studyId": study_accession,
                "expsampleId": row[0],
                "expsampleName": row[1],
                "expsampleDescription": row[2],
                "reagentIds": row[3].split(';') if row[3] else None,
                "treatmentIds": row[4].split(';') if row[4] else None,
                "resultFileName": result_file,
                "additionalResultFileNames": additional_files,
                "immportTemplate": "Yes",  # Default to Yes as recommended
                "assayId": row[6],
                "dilutionFactor": row[7],
                "assayGroupId": row[8],
                "plateType": row[9],
                "biosampleId": row[10],
                "subjectId": row[11],
                "plannedVisitId": row[12],
                "type": row[13],
                "subtype": row[14],
                "biosampleName": row[15],
                "biosampleDescription": row[16],
                "studyTimeCollected": float(row[17]) if row[17] is not None else None,
                "studyTimeCollectedUnit": row[18],
                "studyTimeT0Event": row[19],
                "studyTimeT0EventSpecify": row[20],
                "experimentId": row[21],
                "protocolIds": row[22].split(';') if row[22] else None,
                "experimentName": row[23],
                "experimentDescription": row[24],
                "measurementTechnique": row[25]
            }
            
            # Remove None values
            entry = {k: v for k, v in entry.items() if v is not None}
            
            result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} MBAA experiment samples")
    
    return result

def get_neut_antibody_titer_experiment_samples_data(conn, study_accession):
    """
    Retrieve Neutralizing Antibody Titer experiment samples data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with Neutralizing Antibody Titer experiment samples data formatted according to the ImmPort template
    """
    result = {
        "templateType": "combined-result",
        "fileName": "experimentSamples.Neutralizing_Antibody_Titer.json",
        "name": "neutralizing_antibody_titer",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        ),
        neut_ab_results AS (
            SELECT 
                nar.expsample_accession,
                nar.virus_strain_reported,
                nar.value_reported,
                nar.unit_reported,
                nar.comments
            FROM madi_dat.neut_ab_titer_result nar
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.subject_accession,
            b.planned_visit_accession,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique,
            nar.virus_strain_reported,
            nar.value_reported,
            nar.unit_reported,
            nar.comments
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN neut_ab_results nar ON es.expsample_accession = nar.expsample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique = 'Neutralizing Antibody Titer'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Neutralizing Antibody Titer experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} Neutralizing Antibody Titer experiment samples")
        
        current_sample = None
        current_entry = None
        
        for row in rows:
            expsample_id = row[0]
            
            # If we've moved to a new sample, create new entry
            if expsample_id != current_sample:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "studyId": study_accession,
                    "expsampleId": expsample_id,
                    "expsampleName": row[1],
                    "expsampleDescription": row[2],
                    "reagentIds": row[3].split(';') if row[3] else None,
                    "treatmentIds": row[4].split(';') if row[4] else None,
                    "biosampleId": row[6],
                    "type": row[7],
                    "subtype": row[8],
                    "biosampleName": row[9],
                    "biosampleDescription": row[10],
                    "subjectId": row[11],
                    "plannedVisitId": row[12],
                    "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                    "studyTimeCollectedUnit": row[14],
                    "studyTimeT0Event": row[15],
                    "studyTimeT0EventSpecify": row[16],
                    "experimentId": row[17],
                    "protocolIds": row[18].split(';') if row[18] else None,
                    "experimentName": row[19],
                    "experimentDescription": row[20],
                    "measurementTechnique": row[21]
                }
                
                # Remove None values from metadata
                metadata = {k: v for k, v in metadata.items() if v is not None}
                
                # Create new entry with empty result data
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_sample = expsample_id
            
            # Add result data if it exists
            if row[22]:  # if virus_strain_reported exists
                result_data = {
                    "virusStrainReported": row[22],
                    "valueReported": row[23],
                    "unitReported": row[24]
                }
                
                if row[25]:  # comments
                    result_data["comments"] = row[25]
                
                current_entry["resultData"].append(result_data)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
        
        print(f"Processed {len(result['data'])} Neutralizing Antibody Titer experiment samples")
    
    return result

def get_hai_results_data(conn, study_accession):
    """
    Retrieve HAI (Hemagglutination Inhibition Assay) results data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with HAI results data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "HAI_Results.json",
        "name": "hai_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            hr.expsample_accession,
            hr.virus_strain_reported,
            hr.value_reported,
            hr.unit_reported,
            hr.comments
        FROM madi_dat.hai_result hr
        JOIN madi_dat.expsample e ON hr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY hr.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving HAI results for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} HAI results")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expsampleId": row[0],
                "virusStrainReported": row[1],
                "valueReported": row[2],
                "unitReported": row[3]
            }
            
            # Add optional comments if present
            if row[4]:
                entry["comments"] = row[4]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in [
                "expsampleId",
                "virusStrainReported", 
                "valueReported",
                "unitReported"
            ]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid HAI results")
    
    return result


def get_hla_typing_data(conn, study_accession):
    """
    Retrieve HLA typing data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with HLA typing data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "HLA_Typing.json",
        "name": "hla_typing",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # First, let's check the actual column names in the hla_typing_result table
    with conn.cursor() as cur:
        cur.execute("""
            SELECT column_name 
            FROM information_schema.columns 
            WHERE table_schema = 'madi_dat' 
            AND table_name = 'hla_typing_result'
        """)
        columns = [col[0] for col in cur.fetchall()]
        print(f"Actual columns in hla_typing_result: {columns}")
    
    # Now let's query using a simpler approach first to understand the data structure
    query = """
        SELECT *
        FROM madi_dat.hla_typing_result htr
        JOIN madi_dat.expsample e ON htr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        LIMIT 5
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving HLA typing data sample for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        column_names = [desc[0] for desc in cur.description]
        
        print(f"Sample data columns: {column_names}")
        print(f"Sample data: {rows[:2]}")
    
    # Based on what we find above, let's adjust our main query
    # This is a simplified version that just retrieves the data as is
    query = """
        SELECT 
            htr.expsample_accession,
            htr.ancestral_population,
            -- Add other available columns based on what we discover
            htr.*
        FROM madi_dat.hla_typing_result htr
        JOIN madi_dat.expsample e ON htr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY htr.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving HLA typing data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        column_names = [desc[0] for desc in cur.description]
        
        print(f"Found {len(rows)} HLA typing results")
        print(f"Data columns: {column_names}")
        
        # Mapping from column names to JSON field names (will need to be adjusted based on actual columns)
        field_mapping = {
            'expsample_accession': 'expsampleId',
            'ancestral_population': 'ancestralPopulation',
            'comments': 'comments'
            # Additional fields will be added based on actual schema
        }
        
        # Process each row
        for row in rows:
            # Create a dict with column names as keys
            row_dict = dict(zip(column_names, row))
            
            # Create entry with required fields
            entry = {
                "expsampleId": row_dict['expsample_accession'],
                "ancestralPopulation": row_dict['ancestral_population']
            }
            
            # Add other fields based on what's available in the actual schema
            # This part will need to be customized based on what we discover
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in ["expsampleId", "ancestralPopulation"]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid HLA typing results")
    
    return result


def get_mbaa_results_data(conn, study_accession):
    """
    Retrieve MBAA (Multiplex Bead Array Assay) results data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with MBAA results data formatted according to the ImmPort template
    """
    result = {
        "templateType": "single",
        "fileName": "MBAA_Results.json",
        "name": "mbaa_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Based on the actual schema from the output, we know the columns:
    # ['result_id', 'analyte_accession', 'analyte_preferred', 'analyte_reported', 
    # 'arm_accession', 'assay_group_id', 'assay_id', 'biosample_accession', 'comments', 
    # 'concentration_unit_reported', 'concentration_unit_preferred', 
    # 'concentration_value_reported', 'concentration_value_preferred', 'experiment_accession', 
    # 'mfi', 'mfi_coordinate', 'repository_accession', 'repository_name', 'source_accession', 
    # 'source_type', 'study_accession', 'study_time_collected', 'study_time_collected_unit', 
    # 'subject_accession', 'workspace_id']
    
    # Now we can create a query that directly filters by study_accession
    query = """
        SELECT 
            mr.source_accession,
            mr.source_type,
            mr.assay_id,
            mr.assay_group_id,
            mr.analyte_reported,
            mr.mfi,
            mr.concentration_value_reported,
            mr.concentration_unit_reported,
            mr.mfi_coordinate,
            mr.comments
        FROM madi_dat.mbaa_result mr
        WHERE mr.study_accession = %s
    """
    
    with conn.cursor() as cur:
        try:
            print(f"\nRetrieving MBAA results for study {study_accession}...")
            cur.execute(query, (study_accession,))
            rows = cur.fetchall()
            
            print(f"Found {len(rows)} MBAA results")
            
            for row in rows:
                # Create entry with required fields
                entry = {
                    "sourceId": row[0],               # source_accession
                    "sourceType": row[1],             # source_type
                    "assayId": row[2],                # assay_id
                    "analyteReported": row[4],        # analyte_reported
                    "mfi": row[5],                    # mfi
                    "concentrationValueReported": row[6],  # concentration_value_reported
                    "concentrationUnitReported": row[7]    # concentration_unit_reported
                }
                
                # Add optional fields if they have values
                if row[3]:  # assay_group_id
                    entry["assayGroupId"] = row[3]
                    
                if row[8]:  # mfi_coordinate
                    entry["mfiCoordinate"] = row[8]
                    
                if row[9]:  # comments
                    entry["comments"] = row[9]
                
                # Only add entries that have all required fields
                if all(entry.get(field) for field in [
                    "sourceId", 
                    "sourceType", 
                    "assayId",
                    "analyteReported",
                    "mfi",
                    "concentrationValueReported",
                    "concentrationUnitReported"
                ]):
                    result["data"].append(entry)
            
            print(f"Processed {len(result['data'])} valid MBAA results")
            
        except Exception as e:
            print(f"Error executing MBAA results query: {e}")
            
            # Reset the transaction state
            conn.rollback()
            
            # If the initial query fails, try a more basic approach
            try:
                print("Trying a more basic approach...")
                basic_query = """
                    SELECT *
                    FROM madi_dat.mbaa_result
                    WHERE study_accession = %s
                """
                
                cur.execute(basic_query, (study_accession,))
                rows = cur.fetchall()
                column_names = [desc[0] for desc in cur.description]
                
                print(f"Found {len(rows)} MBAA results using basic query")
                
                # Map database column names to expected JSON field names
                field_mapping = {
                    'source_accession': 'sourceId',
                    'source_type': 'sourceType',
                    'assay_id': 'assayId',
                    'assay_group_id': 'assayGroupId',
                    'analyte_reported': 'analyteReported',
                    'mfi': 'mfi',
                    'concentration_value_reported': 'concentrationValueReported',
                    'concentration_unit_reported': 'concentrationUnitReported',
                    'mfi_coordinate': 'mfiCoordinate',
                    'comments': 'comments'
                }
                
                for row in rows:
                    # Create a dict with column names as keys
                    row_dict = dict(zip(column_names, row))
                    
                    # Create entry with required and optional fields based on available columns
                    entry = {}
                    
                    # Map database fields to JSON fields
                    for db_field, json_field in field_mapping.items():
                        if db_field in row_dict and row_dict[db_field] is not None:
                            entry[json_field] = row_dict[db_field]
                    
                    # Check if entry has all required fields
                    required_fields = [
                        "sourceId", 
                        "sourceType", 
                        "assayId",
                        "analyteReported",
                        "mfi",
                        "concentrationValueReported",
                        "concentrationUnitReported"
                    ]
                    
                    if all(field in entry for field in required_fields):
                        result["data"].append(entry)
                
                print(f"Processed {len(result['data'])} valid MBAA results")
                
            except Exception as e2:
                print(f"Error with basic approach: {e2}")
                conn.rollback()  # Reset the transaction state
                print("Unable to retrieve MBAA results due to schema issues")
    
    return result


def get_treatments_data(conn, study_accession):
    """
    Retrieve treatments data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with treatments data formatted according to the ImmPort template
    """
    result = {
        "templateType": "single",
        "fileName": "treatments.json",
        "name": "treatments",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Get all treatments used in the study through biosample_2_treatment or expsample_2_treatment
    query = """
        SELECT DISTINCT 
            t.treatment_accession,
            t.name,
            t.amount_value,
            t.amount_unit,
            t.duration_value,
            t.duration_unit, 
            t.temperature_value,
            t.temperature_unit,
            t.comments
        FROM madi_dat.treatment t
        LEFT JOIN madi_dat.biosample_2_treatment b2t ON t.treatment_accession = b2t.treatment_accession
        LEFT JOIN madi_dat.biosample b ON b2t.biosample_accession = b.biosample_accession
        LEFT JOIN madi_dat.expsample_2_treatment e2t ON t.treatment_accession = e2t.treatment_accession
        LEFT JOIN madi_dat.expsample e ON e2t.expsample_accession = e.expsample_accession
        LEFT JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE b.study_accession = %s OR exp.study_accession = %s
        ORDER BY t.treatment_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving treatments data for study {study_accession}...")
        cur.execute(query, (study_accession, study_accession))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} treatments")
        
        for row in rows:
            # Determine if treatment has any values (amount, duration, or temperature)
            has_values = any([row[2], row[4], row[6]])  # amount_value, duration_value, temperature_value
            use_treatment = "Yes" if has_values else "No"
            
            # Create treatment entry with required fields
            treatment = {
                "userDefinedId": row[0],  # treatment_accession
                "name": row[1],          # name
                "useTreatment": use_treatment
            }
            
            # Add optional fields in pairs (value/unit) if they exist
            if row[2]:  # amount_value
                treatment["amountValue"] = row[2]
                treatment["amountUnit"] = row[3] if row[3] else ""
                
            if row[4]:  # duration_value
                treatment["durationValue"] = row[4]
                treatment["durationUnit"] = row[5] if row[5] else ""
                
            if row[6]:  # temperature_value
                treatment["temperatureValue"] = row[6]
                treatment["temperatureUnit"] = row[7] if row[7] else ""
                
            if row[8]:  # comments
                treatment["comments"] = row[8]
            
            # Only add entries that have all required fields
            if all(treatment.get(field) for field in ["userDefinedId", "name", "useTreatment"]):
                result["data"].append(treatment)
        
        print(f"Processed {len(result['data'])} valid treatments")
    
    return result

def get_virus_neutralization_results_data(conn, study_accession):
    """
    Retrieve virus neutralization results data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with virus neutralization results data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "Virus_Neutralization_Results.json",
        "name": "virus_neutralization_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            natr.expsample_accession,
            natr.virus_strain_reported,
            natr.value_reported,
            natr.unit_reported,
            natr.comments
        FROM madi_dat.neut_ab_titer_result natr
        JOIN madi_dat.expsample e ON natr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY natr.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving virus neutralization results for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} virus neutralization results")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expsampleId": row[0],
                "virusStrainReported": row[1],
                "valueReported": row[2],
                "unitReported": row[3]
            }
            
            # Add optional comments if present
            if row[4]:
                entry["comments"] = row[4]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in [
                "expsampleId",
                "virusStrainReported", 
                "valueReported",
                "unitReported"
            ]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid virus neutralization results")
    
    return result

def get_public_repositories_data(conn, study_accession):
    """
    Retrieve public repository data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection.
        study_accession: Study accession ID.
        
    Returns:
        Dictionary with public repository data formatted according to the ImmPort template.
    """
    result = {
        "templateType": "single",
        "fileName": "publicRepositories.json",
        "name": "public_repositories",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            epr.expsample_accession,
            epr.repository_name,
            epr.repository_accession
        FROM madi_dat.expsample_public_repository epr
        JOIN madi_dat.expsample e ON epr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY epr.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving public repository data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} public repository records")
        
        for row in rows:
            entry = {
                "expsampleId": row[0],
                "repositoryName": row[1],
                "repositoryAccession": row[2]
            }
            # Only add records having all required fields
            if all(entry.get(field) for field in ["expsampleId", "repositoryName", "repositoryAccession"]):
                result["data"].append(entry)
                
        print(f"Processed {len(result['data'])} valid public repository records")
    
    return result


def get_reagent_sets_data(conn, study_accession):
    """
    Retrieve reagent sets data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with reagent sets data formatted according to the ImmPort template.
        Required fields: userDefinedId, reagentIds, description, name, type
    """
    result = {
        "templateType": "single",
        "fileName": "Reagent_Sets.json",
        "name": "reagent_sets",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find reagents associated with the study through experiment samples
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.description,
            r.name,
            r.type,
            r.analyte_reported,
            r.manufacturer
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving reagent sets data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} reagent sets")
        
        # For each reagent, get its related reagents if it's a set
        for row in rows:
            reagent_accession = row[0]
            
            # Get related reagents if this is a reagent set
            cur.execute("""
                SELECT reagent_accession
                FROM madi_dat.reagent_set_2_reagent
                WHERE reagent_set_accession = %s
            """, (reagent_accession,))
            related_reagents = [r[0] for r in cur.fetchall()]
            
            # If no related reagents were found, use the reagent's own accession
            reagent_ids = related_reagents if related_reagents else [reagent_accession]
            
            # Create entry with fields from database without adding defaults
            entry = {
                "userDefinedId": reagent_accession,
                "reagentIds": ";".join(reagent_ids)
            }
            
            # Only add fields if they exist in the database
            if row[1]:  # description
                entry["description"] = row[1]
                
            if row[2]:  # name
                entry["name"] = row[2]
                
            if row[3]:  # type
                entry["type"] = row[3]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in ["userDefinedId", "reagentIds", "description", "name", "type"]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid reagent sets")
    
    return result


def get_array_reagents_data(conn, study_accession):
    """
    Retrieve array reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with array reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.Array.json",
        "name": "array_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find array reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'Array'  -- Filter for array reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving array reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} array reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]        # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in ["userDefinedId", "manufacturer", "catalogNumber"]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid array reagents")
    
    return result

def get_cytof_reagents_data(conn, study_accession):
    """
    Retrieve CyTOF reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with CyTOF reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.CyTOF.json",
        "name": "cytof_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find CyTOF reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact,
            r.analyte_reported,
            r.antibody_registry_id,
            r.clone_name,
            r.reporter_name
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'CyTOF'  -- Filter for CyTOF reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving CyTOF reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} CyTOF reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4],      # catalog_number
                "analyteReported": row[8],    # analyte_reported
                "cloneName": row[10],         # clone_name
                "reporterName": row[11]       # reporter_name
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
                
            if row[9]:  # antibody_registry_id
                entry["antibodyRegistryId"] = row[9]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber", 
                "analyteReported", 
                "cloneName", 
                "reporterName"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid CyTOF reagents")
    
    return result

def get_elisa_reagents_data(conn, study_accession):
    """
    Retrieve ELISA reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with ELISA reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.ELISA.json",
        "name": "elisa_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find ELISA reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact,
            r.analyte_reported,
            r.antibody_registry_id
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'ELISA'  -- Filter for ELISA reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving ELISA reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} ELISA reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4],      # catalog_number
                "analyteReported": row[8]     # analyte_reported
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
                
            if row[9]:  # antibody_registry_id
                entry["antibodyRegistryId"] = row[9]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber", 
                "analyteReported" 
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid ELISA reagents")
    
    return result

def get_elispot_reagents_data(conn, study_accession):
    """
    Retrieve ELISPOT reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with ELISPOT reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.ELISPOT.json",
        "name": "elispot_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find ELISPOT reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact,
            r.analyte_reported,
            r.antibody_registry_id
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'ELISPOT'  -- Filter for ELISPOT reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving ELISPOT reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} ELISPOT reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4],      # catalog_number
                "analyteReported": row[8]     # analyte_reported
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
                
            if row[9]:  # antibody_registry_id
                entry["antibodyRegistryId"] = row[9]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber", 
                "analyteReported" 
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid ELISPOT reagents")
    
    return result

def get_flow_cytometry_reagents_data(conn, study_accession):
    """
    Retrieve Flow Cytometry reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with Flow Cytometry reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.Flow_Cytometry.json",
        "name": "flow_cytometry_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find Flow Cytometry reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact,
            r.analyte_reported,
            r.antibody_registry_id,
            r.clone_name,
            r.reporter_name
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'Flow Cytometry'  -- Filter for Flow Cytometry reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Flow Cytometry reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} Flow Cytometry reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4],      # catalog_number
                "analyteReported": row[8],    # analyte_reported
                "cloneName": row[10],         # clone_name
                "reporterName": row[11]       # reporter_name
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
                
            if row[9]:  # antibody_registry_id
                entry["antibodyRegistryId"] = row[9]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber", 
                "analyteReported", 
                "cloneName", 
                "reporterName"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid Flow Cytometry reagents")
    
    return result

def get_hai_reagents_data(conn, study_accession):
    """
    Retrieve HAI (Hemagglutination Inhibition) reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with HAI reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.HAI.json",
        "name": "hai_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find HAI reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'Hemagglutination Inhibition'  -- Filter for HAI reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving HAI reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} HAI reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid HAI reagents")
    
    return result

def get_hla_typing_reagents_data(conn, study_accession):
    """
    Retrieve HLA Typing reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with HLA Typing reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.HLA_Typing.json",
        "name": "hla_typing_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find HLA Typing reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'HLA Typing'  -- Filter for HLA Typing reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving HLA Typing reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} HLA Typing reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid HLA Typing reagents")
    
    return result

def get_kir_typing_reagents_data(conn, study_accession):
    """
    Retrieve KIR Typing reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with KIR Typing reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.KIR_Typing.json",
        "name": "kir_typing_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find KIR Typing reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'KIR Typing'  -- Filter for KIR Typing reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving KIR Typing reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} KIR Typing reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid KIR Typing reagents")
    
    return result

def get_mbaa_reagents_data(conn, study_accession):
    """
    Retrieve MBAA (Multiplex Bead Array Assay) reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with MBAA reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.MBAA.json",
        "name": "mbaa_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find MBAA reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact,
            r.analyte_reported
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'Luminex xMAP'  -- Filter for MBAA reagents only/Luminex xMAP
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving MBAA reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} MBAA reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4],      # catalog_number
                "analyteReported": row[8]     # analyte_reported
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber", 
                "analyteReported"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid MBAA reagents")
    
    return result

def get_neutralizing_antibody_titer_reagents_data(conn, study_accession):
    """
    Retrieve Neutralizing Antibody Titer reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with Neutralizing Antibody Titer reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.Neutralizing_Antibody_Titer.json",
        "name": "neutralizing_antibody_titer_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find Neutralizing Antibody Titer reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'Neutralizing Antibody Titer'  -- Filter for Neutralizing Antibody Titer reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Neutralizing Antibody Titer reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} Neutralizing Antibody Titer reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid Neutralizing Antibody Titer reagents")
    
    return result

def get_other_reagents_data(conn, study_accession):
    """
    Retrieve Other reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with Other reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.Other.json",
        "name": "other_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find Other reagents associated with the study through expsample_2_reagent
    # that are not covered by other specific reagent types
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type NOT IN (
            'Array', 'CyTOF', 'ELISA', 'ELISPOT', 'Flow Cytometry',
            'HAI', 'HLA Typing', 'KIR Typing', 'MBAA', 'Neutralizing Antibody Titer'
        )
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Other reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} Other reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid Other reagents")
    
    return result

def get_pcr_reagents_data(conn, study_accession):
    """
    Retrieve PCR reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with PCR reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.PCR.json",
        "name": "pcr_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find PCR reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'PCR'  -- Filter for PCR reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving PCR reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} PCR reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid PCR reagents")
    
    return result

def get_sequencing_reagents_data(conn, study_accession):
    """
    Retrieve Sequencing reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with Sequencing reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.Sequencing.json",
        "name": "sequencing_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find Sequencing reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'Sequencing'  -- Filter for Sequencing reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Sequencing reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} Sequencing reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid Sequencing reagents")
    
    return result

def get_virus_neutralization_reagents_data(conn, study_accession):
    """
    Retrieve Virus Neutralization reagent data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with Virus Neutralization reagent data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "reagents.Virus_Neutralization.json",
        "name": "virus_neutralization_reagents",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find Virus Neutralization reagents associated with the study through expsample_2_reagent
    query = """
        SELECT DISTINCT
            r.reagent_accession,
            r.name,
            r.description,
            r.manufacturer,
            r.catalog_number,
            r.lot_number,
            r.weblink,
            r.contact
        FROM madi_dat.reagent r
        JOIN madi_dat.expsample_2_reagent e2r ON r.reagent_accession = e2r.reagent_accession
        JOIN madi_dat.expsample e ON e2r.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        AND r.type = 'Virus Neutralization'  -- Filter for Virus Neutralization reagents only
        ORDER BY r.reagent_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Virus Neutralization reagent data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} Virus Neutralization reagents")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "userDefinedId": row[0],      # reagent_accession
                "manufacturer": row[3],       # manufacturer
                "catalogNumber": row[4]       # catalog_number
            }
            
            # Add optional fields if they exist
            if row[1]:  # name
                entry["name"] = row[1]
                
            if row[2]:  # description
                entry["description"] = row[2]
                
            if row[5]:  # lot_number
                entry["lotNumber"] = row[5]
                
            if row[6]:  # weblink
                entry["weblink"] = row[6]
                
            if row[7]:  # contact
                entry["contact"] = row[7]
            
            # Only add entries that have all required fields
            required_fields = [
                "userDefinedId", 
                "manufacturer", 
                "catalogNumber"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid Virus Neutralization reagents")
    
    return result


def get_rna_seq_results_data(conn, study_accession):
    """
    Retrieve RNA SEQ results data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with RNA SEQ results data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "RNA_SEQ_Results.json",
        "name": "rna_seq_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    # Find RNA SEQ results associated with the study through experiment samples
    query = """
        SELECT DISTINCT
            rsr.expsample_accession,
            rsr.reference_transcript_id,
            rsr.repository_name,
            rsr.transcript_type_reported,
            rsr.result_unit_reported,
            rsr.value_reported,
            rsr.comments
        FROM madi_dat.rna_seq_result rsr
        JOIN madi_dat.expsample e ON rsr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY rsr.expsample_accession, rsr.reference_transcript_id
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving RNA SEQ results for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        print(f"Found {len(rows)} RNA SEQ results")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expsampleId": row[0],                # expsample_accession
                "referenceTranscriptId": row[1],      # reference_transcript_id
                "repositoryName": row[2],             # repository_name
                "transcriptTypeReported": row[3],     # transcript_type_reported
                "resultUnitReported": row[4],         # result_unit_reported
                "valueReported": row[5]               # value_reported
            }
            
            # Add optional comment if present
            if row[6]:  # comments
                entry["comments"] = row[6]
            
            # Only add entries that have all required fields
            required_fields = [
                "expsampleId", 
                "referenceTranscriptId", 
                "repositoryName", 
                "transcriptTypeReported", 
                "resultUnitReported", 
                "valueReported"
            ]
            
            if all(entry.get(field) for field in required_fields):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid RNA SEQ results")
    
    return result


def get_standard_curves_data(conn, study_accession):
    """
    Retrieve standard curve data for a given study accession and format it according to the template.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with standard curve data formatted according to the ImmPort template
    """
    result = {
        "templateType": "combined",
        "fileName": "standardCurves.json",
        "name": "standardcurves",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    try:
        # First check if the standard_curve table exists
        check_query = """
            SELECT EXISTS (
                SELECT FROM information_schema.tables 
                WHERE table_schema = 'madi_dat' 
                AND table_name = 'standard_curve'
            );
        """
        
        with conn.cursor() as cur:
            cur.execute(check_query)
            table_exists = cur.fetchone()[0]
            
            if not table_exists:
                print("Table madi_dat.standard_curve does not exist in the database")
                return result
            
            # Check the structure of the file_info table to see available columns
            check_file_info_query = """
                SELECT column_name 
                FROM information_schema.columns
                WHERE table_schema = 'madi_dat' 
                AND table_name = 'file_info';
            """
            
            cur.execute(check_file_info_query)
            file_info_columns = [row[0] for row in cur.fetchall()]
            print(f"Available columns in file_info table: {file_info_columns}")
            
            # Simplified query without the result_file filter
            query = """
                WITH protocol_info AS (
                    SELECT 
                        e2p.experiment_accession,
                        string_agg(e2p.protocol_accession, ';') as protocol_ids
                    FROM madi_dat.experiment_2_protocol e2p
                    GROUP BY e2p.experiment_accession
                ),
                file_info AS (
                    SELECT 
                        s2f.standard_curve_accession,
                        f.name as result_file_name
                    FROM madi_dat.standard_curve_2_file_info s2f
                    JOIN madi_dat.file_info f ON s2f.file_info_id = f.file_info_id
                    GROUP BY s2f.standard_curve_accession, f.name
                ),
                add_file_info AS (
                    SELECT 
                        s2f.standard_curve_accession,
                        string_agg(f.name, ';') as additional_files
                    FROM madi_dat.standard_curve_2_file_info s2f
                    JOIN madi_dat.file_info f ON s2f.file_info_id = f.file_info_id
                    GROUP BY s2f.standard_curve_accession
                )
                SELECT 
                    sc.standard_curve_accession,
                    sc.formula,
                    sc.analyte_reported,
                    sc.assay_id,
                    sc.assay_group_id,
                    sc.lower_limit,
                    sc.lower_limit_unit,
                    sc.upper_limit,
                    sc.upper_limit_unit,
                    fi.result_file_name,
                    afi.additional_files,
                    e.experiment_accession,
                    e.study_accession,
                    p.protocol_ids,
                    e.name,
                    e.description,
                    e.measurement_technique
                FROM madi_dat.standard_curve sc
                JOIN madi_dat.experiment e ON sc.experiment_accession = e.experiment_accession
                LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
                LEFT JOIN file_info fi ON sc.standard_curve_accession = fi.standard_curve_accession
                LEFT JOIN add_file_info afi ON sc.standard_curve_accession = afi.standard_curve_accession
                WHERE e.study_accession = %s
                ORDER BY sc.standard_curve_accession
            """
            
            print(f"\nRetrieving standard curve data for study {study_accession}...")
            cur.execute(query, (study_accession,))
            rows = cur.fetchall()
            print(f"Found {len(rows)} standard curves")
            
            for row in rows:
                # Create entry with required fields
                entry = {
                    "standardCurveId": row[0],    # standard_curve_accession
                    "experimentId": row[11]       # experiment_accession
                }
                
                # Add optional fields if they exist
                if row[1]:  # formula
                    entry["formula"] = row[1]
                
                if row[2]:  # analyte_reported
                    entry["analyteReported"] = row[2]
                
                if row[3]:  # assay_id
                    entry["assayId"] = row[3]
                
                if row[4]:  # assay_group_id
                    entry["assayGroupId"] = row[4]
                
                if row[5]:  # lower_limit
                    entry["lowerLimit"] = row[5]
                
                if row[6]:  # lower_limit_unit
                    entry["lowerLimitUnit"] = row[6]
                
                if row[7]:  # upper_limit
                    entry["upperLimit"] = row[7]
                
                if row[8]:  # upper_limit_unit
                    entry["upperLimitUnit"] = row[8]
                
                # Add immportTemplate based on the presence of result file
                if row[9]:  # result_file_name
                    entry["immportTemplate"] = "No"
                    entry["resultFileName"] = row[9]
                else:
                    entry["immportTemplate"] = "Yes"
                
                if row[10]:  # additional_files
                    entry["additionalResultFileNames"] = row[10]
                
                if row[12]:  # study_accession
                    entry["studyId"] = row[12]
                
                if row[13] and row[13] != "":  # protocol_ids
                    entry["protocolIds"] = row[13].split(';')
                
                if row[14]:  # name
                    entry["name"] = row[14]
                
                if row[15]:  # description
                    entry["description"] = row[15]
                
                if row[16]:  # measurement_technique
                    entry["measurementTechnique"] = row[16]
                
                # Only add entries that have all required fields
                required_fields = [
                    "standardCurveId", 
                    "experimentId"
                ]
                
                if all(entry.get(field) for field in required_fields):
                    result["data"].append(entry)
            
            print(f"Processed {len(result['data'])} valid standard curves")
            
    except Exception as e:
        print(f"Error retrieving standard curve data: {e}")
        conn.rollback()  # Reset the transaction state
    
    return result

def get_hai_experiment_samples_data(conn, study_accession):
    """
    Retrieve HAI (Hemagglutination Inhibition Assay) experiment samples data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with HAI experiment samples data formatted according to the ImmPort template
    """
    result = {
        "templateType": "combined-result",
        "fileName": "experimentSamples.HAI.json",
        "name": "hai",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.subject_accession,
            b.planned_visit_accession,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique,
            hr.virus_strain_reported,
            hr.value_reported,
            hr.unit_reported,
            hr.comments
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN madi_dat.hai_result hr ON es.expsample_accession = hr.expsample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND es.result_schema = 'HAI'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving HAI experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        current_sample = None
        current_entry = None
        
        for row in rows:
            expsample_id = row[0]
            
            # If we've moved to a new sample, create new entry
            if expsample_id != current_sample:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "studyId": study_accession,
                    "expsampleId": expsample_id,
                    "expsampleName": row[1],
                    "expsampleDescription": row[2],
                    "reagentIds": row[3].split(';') if row[3] else None,
                    "treatmentIds": row[4].split(';') if row[4] else None,
                    "additionalResultFileNames": row[5].split(';') if row[5] else None,
                    "biosampleId": row[6],
                    "type": row[7],
                    "subtype": row[8],
                    "biosampleName": row[9],
                    "biosampleDescription": row[10],
                    "subjectId": row[11],
                    "plannedVisitId": row[12],
                    "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                    "studyTimeCollectedUnit": row[14],
                    "studyTimeT0Event": row[15],
                    "studyTimeT0EventSpecify": row[16],
                    "experimentId": row[17],
                    "protocolIds": row[18].split(';') if row[18] else None,
                    "experimentName": row[19],
                    "experimentDescription": row[20],
                    "measurementTechnique": row[21]
                }
                
                # Remove None values from metadata
                metadata = {k: v for k, v in metadata.items() if v is not None}
                
                # Initialize new entry with empty result data
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_sample = expsample_id
            
            # Add result data if it exists
            if row[22]:  # if virus_strain_reported exists
                result_data = {
                    "virusStrainReported": row[22],
                    "valueReported": row[23],
                    "unitReported": row[24]
                }
                
                if row[25]:  # comments
                    result_data["comments"] = row[25]
                
                current_entry["resultData"].append(result_data)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
        
        print(f"Processed {len(result['data'])} HAI experiment samples")
    
    return result

def get_hla_experiment_samples_data(conn, study_accession):
    """
    Retrieve HLA (Human Leukocyte Antigen) experiment samples data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with HLA experiment samples data formatted according to the ImmPort template
    """
    result = {
        "templateType": "combined-multiple",
        "fileName": "experimentSamples.HLA.json",
        "name": "hla",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession,
            b.subject_accession,
            b.planned_visit_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique = 'HLA Typing'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving HLA experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} HLA experiment samples")
        
        for row in rows:
            # Split file names
            files = row[5].split(';') if row[5] else []
            result_file = files[0] if files else None
            additional_files = files[1:] if len(files) > 1 else None
            
            # Create entry
            entry = {
                "studyId": study_accession,
                "expsampleId": row[0],
                "expsampleName": row[1],
                "expsampleDescription": row[2],
                "reagentIds": row[3].split(';') if row[3] else None,
                "treatmentIds": row[4].split(';') if row[4] else None,
                "resultFileName": result_file,
                "additionalResultFileNames": additional_files,
                "immportTemplate": "Yes",  # Default to Yes as recommended
                "biosampleId": row[6],
                "subjectId": row[7],
                "plannedVisitId": row[8],
                "type": row[9],
                "subtype": row[10],
                "biosampleName": row[11],
                "biosampleDescription": row[12],
                "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                "studyTimeCollectedUnit": row[14],
                "studyTimeT0Event": row[15],
                "studyTimeT0EventSpecify": row[16],
                "experimentId": row[17],
                "protocolIds": row[18].split(';') if row[18] else None,
                "experimentName": row[19],
                "experimentDescription": row[20],
                "measurementTechnique": row[21]
            }
            
            # Remove None values
            entry = {k: v for k, v in entry.items() if v is not None}
            
            result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} HLA experiment samples")
    
    return result

def get_neutralizing_antibody_titer_experiment_samples_data(conn, study_accession):
    """
    Retrieve Neutralizing Antibody Titer experiment samples data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with Neutralizing Antibody Titer experiment samples data formatted according to the ImmPort template
    """
    result = {
        "templateType": "combined-result",
        "fileName": "experimentSamples.Neutralizing_Antibody_Titer.json",
        "name": "neutralizing_antibody_titer",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.subject_accession,
            b.planned_visit_accession,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique,
            nar.virus_strain_reported,
            nar.value_reported,
            nar.unit_reported,
            nar.comments
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN madi_dat.neut_ab_titer_result nar ON es.expsample_accession = nar.expsample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique = 'Neutralizing Antibody Titer'
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving Neutralizing Antibody Titer experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} Neutralizing Antibody Titer experiment samples")
        
        current_sample = None
        current_entry = None
        
        for row in rows:
            expsample_id = row[0]
            
            # If we've moved to a new sample, create new entry
            if expsample_id != current_sample:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "studyId": study_accession,
                    "expsampleId": expsample_id,
                    "expsampleName": row[1],
                    "expsampleDescription": row[2],
                    "reagentIds": row[3].split(';') if row[3] else None,
                    "treatmentIds": row[4].split(';') if row[4] else None,
                    "additionalResultFileNames": row[5].split(';') if row[5] else None,
                    "biosampleId": row[6],
                    "type": row[7],
                    "subtype": row[8],
                    "biosampleName": row[9],
                    "biosampleDescription": row[10],
                    "subjectId": row[11],
                    "plannedVisitId": row[12],
                    "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                    "studyTimeCollectedUnit": row[14],
                    "studyTimeT0Event": row[15],
                    "studyTimeT0EventSpecify": row[16],
                    "experimentId": row[17],
                    "protocolIds": row[18].split(';') if row[18] else None,
                    "experimentName": row[19],
                    "experimentDescription": row[20],
                    "measurementTechnique": row[21]
                }
                
                # Remove None values from metadata
                metadata = {k: v for k, v in metadata.items() if v is not None}
                
                # Initialize new entry with empty result data
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_sample = expsample_id
            
            # Add result data if it exists
            if row[22]:  # if virus_strain_reported exists
                result_data = {
                    "virusStrainReported": row[22],
                    "valueReported": row[23],
                    "unitReported": row[24]
                }
                
                if row[25]:  # comments
                    result_data["comments"] = row[25]
                
                current_entry["resultData"].append(result_data)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
        
        print(f"Processed {len(result['data'])} Neutralizing Antibody Titer experiment samples")
    
    return result

def get_qrt_pcr_experiment_samples_data(conn, study_accession):
    """
    Retrieve qRT-PCR experiment samples data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with qRT-PCR experiment samples data formatted according to the ImmPort template
    """
    result = {
        "templateType": "combined-result",
        "fileName": "experimentSamples.QRT-PCR.json",
        "name": "qrt-pcr",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        WITH file_info AS (
            SELECT 
                e2f.expsample_accession,
                string_agg(DISTINCT f.name, ';') as result_files
            FROM madi_dat.expsample_2_file_info e2f
            JOIN madi_dat.file_info f ON e2f.file_info_id = f.file_info_id
            GROUP BY e2f.expsample_accession
        ),
        reagent_info AS (
            SELECT 
                e2r.expsample_accession,
                string_agg(e2r.reagent_accession, ';') as reagent_ids
            FROM madi_dat.expsample_2_reagent e2r
            GROUP BY e2r.expsample_accession
        ),
        treatment_info AS (
            SELECT 
                e2t.expsample_accession,
                string_agg(e2t.treatment_accession, ';') as treatment_ids
            FROM madi_dat.expsample_2_treatment e2t
            GROUP BY e2t.expsample_accession
        ),
        protocol_info AS (
            SELECT 
                e2p.experiment_accession,
                string_agg(e2p.protocol_accession, ';') as protocol_ids
            FROM madi_dat.experiment_2_protocol e2p
            GROUP BY e2p.experiment_accession
        )
        SELECT 
            es.expsample_accession,
            es.name as expsample_name,
            es.description as expsample_description,
            r.reagent_ids,
            t.treatment_ids,
            f.result_files,
            b.biosample_accession,
            b.type,
            b.subtype,
            b.name as biosample_name,
            b.description as biosample_description,
            b.subject_accession,
            b.planned_visit_accession,
            b.study_time_collected,
            b.study_time_collected_unit,
            b.study_time_t0_event,
            b.study_time_t0_event_specify,
            e.experiment_accession,
            p.protocol_ids,
            e.name as experiment_name,
            e.description as experiment_description,
            e.measurement_technique,
            pr.gene_symbol_name,
            pr.value_reported,
            pr.unit_reported,
            pr.gene_id,
            pr.gene_name,
            pr.other_gene_accession,
            pr.comments
        FROM madi_dat.expsample es
        JOIN madi_dat.experiment e ON es.experiment_accession = e.experiment_accession
        JOIN madi_dat.expsample_2_biosample e2b ON es.expsample_accession = e2b.expsample_accession
        JOIN madi_dat.biosample b ON e2b.biosample_accession = b.biosample_accession
        LEFT JOIN madi_dat.pcr_result pr ON es.expsample_accession = pr.expsample_accession
        LEFT JOIN file_info f ON es.expsample_accession = f.expsample_accession
        LEFT JOIN reagent_info r ON es.expsample_accession = r.expsample_accession
        LEFT JOIN treatment_info t ON es.expsample_accession = t.expsample_accession
        LEFT JOIN protocol_info p ON e.experiment_accession = p.experiment_accession
        WHERE e.study_accession = %s
        AND e.measurement_technique in ('PCR', 'qPCR', 'RT-PCR', 'qRT-PCR')
        ORDER BY es.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving qRT-PCR experiment samples for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} qRT-PCR experiment samples")
        
        current_sample = None
        current_entry = None
        
        for row in rows:
            expsample_id = row[0]
            
            # If we've moved to a new sample, create new entry
            if expsample_id != current_sample:
                if current_entry:
                    result["data"].append(current_entry)
                
                # Create metadata section
                metadata = {
                    "studyId": study_accession,
                    "expsampleId": expsample_id,
                    "expsampleName": row[1],
                    "expsampleDescription": row[2],
                    "reagentIds": row[3].split(';') if row[3] else None,
                    "treatmentIds": row[4].split(';') if row[4] else None,
                    "additionalResultFileNames": row[5].split(';') if row[5] else None,
                    "biosampleId": row[6],
                    "type": row[7],
                    "subtype": row[8],
                    "biosampleName": row[9],
                    "biosampleDescription": row[10],
                    "subjectId": row[11],
                    "plannedVisitId": row[12],
                    "studyTimeCollected": float(row[13]) if row[13] is not None else None,
                    "studyTimeCollectedUnit": row[14],
                    "studyTimeT0Event": row[15],
                    "studyTimeT0EventSpecify": row[16],
                    "experimentId": row[17],
                    "protocolIds": row[18].split(';') if row[18] else None,
                    "experimentName": row[19],
                    "experimentDescription": row[20],
                    "measurementTechnique": row[21]
                }
                
                # Remove None values from metadata
                metadata = {k: v for k, v in metadata.items() if v is not None}
                
                # Initialize new entry with empty result data
                current_entry = {
                    "metaData": metadata,
                    "resultData": []
                }
                
                current_sample = expsample_id
            
            # Add result data if it exists
            if row[22]:  # if gene_symbol_name exists
                result_data = {
                    "geneSymbolName": row[22],
                    "valueReported": row[23],
                    "unitReported": row[24]
                }
                
                # Add optional fields if they exist
                if row[25]:  # gene_id
                    result_data["geneId"] = row[25]
                
                if row[26]:  # gene_name
                    result_data["geneName"] = row[26]
                
                if row[27]:  # other_gene_accession
                    result_data["otherGeneAccession"] = row[27]
                
                if row[28]:  # comments
                    result_data["comments"] = row[28]
                
                current_entry["resultData"].append(result_data)
        
        # Add last entry if exists
        if current_entry:
            result["data"].append(current_entry)
        
        print(f"Processed {len(result['data'])} qRT-PCR experiment samples")
    
    return result

def get_kir_typing_data(conn, study_accession):
    """
    Retrieve KIR (Killer-cell Immunoglobulin-like Receptor) typing data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with KIR typing data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "KIR_Typing.json",
        "name": "kir_typing",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            ktr.expsample_accession,
            ktr.kir_haplotype,
            ktr.allele_1,
            ktr.allele_2,
            ktr.comments
        FROM madi_dat.kir_typing_result ktr
        JOIN madi_dat.expsample e ON ktr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY ktr.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving KIR typing data for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} KIR typing results")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expsampleId": row[0],
                "kirHaplotype": row[1]
            }
            
            # Add optional fields if they exist
            if row[2]:  # allele_1
                entry["allele1"] = row[2]
                
            if row[3]:  # allele_2
                entry["allele2"] = row[3]
                
            if row[4]:  # comments
                entry["comments"] = row[4]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in ["expsampleId", "kirHaplotype"]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid KIR typing results")
    
    return result

def get_pcr_results_data(conn, study_accession):
    """
    Retrieve PCR results data for a given study accession.
    
    Args:
        conn: Database connection
        study_accession: Study accession ID
        
    Returns:
        Dictionary with PCR results data formatted according to the ImmPort template
    """
    result = {
        "templateType": "multiple",
        "fileName": "PCR_Results.json",
        "name": "pcr_results",
        "schemaVersion": "3.37",
        "validationLevel": "1",
        "data": []
    }
    
    query = """
        SELECT 
            pr.expsample_accession,
            pr.gene_symbol_name,
            pr.value_reported,
            pr.unit_reported,
            pr.gene_id,
            pr.gene_name,
            pr.other_gene_accession,
            pr.comments
        FROM madi_dat.pcr_result pr
        JOIN madi_dat.expsample e ON pr.expsample_accession = e.expsample_accession
        JOIN madi_dat.experiment exp ON e.experiment_accession = exp.experiment_accession
        WHERE exp.study_accession = %s
        ORDER BY pr.expsample_accession
    """
    
    with conn.cursor() as cur:
        print(f"\nRetrieving PCR results for study {study_accession}...")
        cur.execute(query, (study_accession,))
        rows = cur.fetchall()
        
        print(f"Found {len(rows)} PCR results")
        
        for row in rows:
            # Create entry with required fields
            entry = {
                "expsampleId": row[0],
                "geneSymbolName": row[1],
                "valueReported": row[2],
                "unitReported": row[3]
            }
            
            # Add optional fields if they exist
            if row[4]:  # gene_id
                entry["geneId"] = row[4]
                
            if row[5]:  # gene_name
                entry["geneName"] = row[5]
                
            if row[6]:  # other_gene_accession
                entry["otherGeneAccession"] = row[6]
                
            if row[7]:  # comments
                entry["comments"] = row[7]
            
            # Only add entries that have all required fields
            if all(entry.get(field) for field in ["expsampleId", "geneSymbolName", "valueReported", "unitReported"]):
                result["data"].append(entry)
        
        print(f"Processed {len(result['data'])} valid PCR results")
    
    return result

def _get_db_connection():
    """
    Establishes and returns a new database connection using credentials
    from environment variables.
    Raises ConnectionError if configuration is incomplete or connection fails.
    """
    # Fetch credentials from environment variables each time the function is called.
    # This ensures that if they were somehow updated (less common for a running process),
    # the new ones would be used.
    db_user = os.getenv('DB_USER')
    db_password = os.getenv('DB_PASSWORD')
    db_host = os.getenv('DB_HOST')
    db_port = os.getenv('DB_PORT')
    db_name = os.getenv('DB_NAME')

    # For context logging
    current_time_edt = "2025-05-12T19:33:16EDT" # Current time
    location = "Hanover, New Hampshire, United States"

    # Check if all necessary environment variables are loaded
    missing_vars = []
    if not db_user: missing_vars.append("DB_USER")
    if db_password is None: missing_vars.append("DB_PASSWORD") # Allow empty string for local dev (no password)
    if not db_host: missing_vars.append("DB_HOST")
    if not db_port: missing_vars.append("DB_PORT")
    if not db_name: missing_vars.append("DB_NAME")

    if missing_vars:
        error_message = (
            f"PY DB Connection Error ({current_time_edt}, {location}): "
            f"Database configuration is incomplete. Missing environment variable(s): {', '.join(missing_vars)}."
        )
        print(error_message)
        raise ConnectionError(error_message)

    try:
        # print(f"PY: Attempting DB connection ({current_time_edt}, {location}) " # Optional debug line
        #       f"to {db_name}@{db_host}:{db_port} as user {db_user}...")
        conn = psycopg2.connect(
            dbname=db_name,
            user=db_user,
            password=db_password,
            host=db_host,
            port=db_port
        )
        # print(f"PY: DB connection successful ({current_time_edt}).") # Optional debug line
        return conn
    except psycopg2.Error as e:
        error_message = (
            f"PY DB Connection Error ({current_time_edt}, {location}): "
            f"Failed to connect to database '{db_name}' at '{db_host}:{db_port}'. Error: {e}"
        )
        print(error_message)
        # Re-raise as a more generic ConnectionError or a custom DB error if you prefer
        raise ConnectionError(error_message) from e
    


def process_and_return_data_py(study_accession_str, file_numbers_list_str, immport_template_settings=None):
    conn = None
    generated_data_for_r = {}
    processing_messages_for_r = []

    # Convert immport_template_settings from R format if provided
    template_settings = None
    if immport_template_settings is not None:
        template_settings = {
            "immportTemplate": immport_template_settings.get("immportTemplate", "Yes"),
            "resultFileName": immport_template_settings.get("resultFileName"),
            "additionalResultFileNames": immport_template_settings.get("additionalResultFileNames")
        }
        print(f"DEBUG: Received ImmPort template settings: {template_settings}")

    data_fetch_functions = {
        1: ('basic_study_design.json', get_study_data),
        2: ('experiments.json', get_experiment_data),
        3: ('labTestPanels.json', get_lab_test_panel_data),
        4: ('labTest_Results.json', get_lab_test_results_data),
        5: ('assessmentpanel.json', get_assessment_panel_data),
        6: ('assessmentcomponent.json', get_assessment_component_data),
        7: ('adverseEvents.json', get_adverse_events_data),
        8: ('subjectHumans.json', get_subject_humans_data),
        9: ('subjectAnimals.json', get_subject_animals_data),
        10: ('bioSamples.json', get_biosamples_data),
        11: ('interventions.json', get_interventions_data),
        12: ('immuneExposure.json', get_immune_exposure_data),
        13: ('labTests.json', get_lab_tests_data),
        14: ('controlSamples.json', get_control_samples_data),
        15: ('assessments.json', get_assessments_data),
        16: ('CyTOF_Derived_data.json', get_cytof_derived_data),
        17: ('ELISA_Results.json', get_elisa_results_data),
        18: ('ELISPOT_Results.json', get_elispot_results_data),
        19: ('experimentSamples.CYTOF.json', get_cytof_experiment_samples_data),
        20: ('experimentSamples.ELISA.json', get_elisa_experiment_samples_data),
        21: ('experimentSamples.ELISPOT.json', get_elispot_experiment_samples_data),
        22: ('experimentSamples.MBAA.json', get_mbaa_experiment_samples_data),
        23: ('experimentSamples.Neutralizing_Antibody_Titer.json', get_neut_antibody_titer_experiment_samples_data),
        24: ('HAI_Results.json', get_hai_results_data),
        25: ('HLA_Typing.json', get_hla_typing_data),
        26: ('protocols.json', get_protocol_data),
        27: ('MBAA_Results.json', get_mbaa_results_data),
        28: ('treatments.json', get_treatments_data),
        29: ('Virus_Neutralization_Results.json', get_virus_neutralization_results_data),
        30: ('publicRepositories.json', get_public_repositories_data),
        31: ('Reagent_Sets.json', get_reagent_sets_data),
        32: ('reagents.Array.json', get_array_reagents_data),
        33: ('reagents.CyTOF.json', get_cytof_reagents_data),
        34: ('reagents.ELISA.json', get_elisa_reagents_data),
        35: ('reagents.ELISPOT.json', get_elispot_reagents_data),
        36: ('reagents.Flow_Cytometry.json', get_flow_cytometry_reagents_data),
        37: ('reagents.HAI.json', get_hai_reagents_data),
        38: ('reagents.HLA_Typing.json', get_hla_typing_reagents_data),
        39: ('reagents.KIR_Typing.json', get_kir_typing_reagents_data),
        40: ('reagents.MBAA.json', get_mbaa_reagents_data),
        41: ('reagents.Neutralizing_Antibody_Titer.json', get_neutralizing_antibody_titer_reagents_data),
        42: ('reagents.Other.json', get_other_reagents_data),
        43: ('reagents.PCR.json', get_pcr_reagents_data),
        44: ('reagents.Sequencing.json', get_sequencing_reagents_data),
        45: ('reagents.Virus_Neutralization.json', get_virus_neutralization_reagents_data),
        46: ('RNA_SEQ_Results.json', get_rna_seq_results_data),
        47: ('standardCurves.json', get_standard_curves_data),
        48: ('experimentSamples.HAI.json', get_hai_experiment_samples_data),
        49: ('experimentSamples.HLA.json', get_hla_experiment_samples_data),
        50: ('experimentSamples.Neutralizing_Antibody_Titer.json', get_neutralizing_antibody_titer_experiment_samples_data),
        51: ('experimentSamples.QRT-PCR.json',  get_qrt_pcr_experiment_samples_data),
        52: ('KIR_Typing.json', get_kir_typing_data),
        53: ('PCR_Results.json', get_pcr_results_data),
        54: ('experimentSamples.Virus_Neutralization.json', get_virus_neutralization_experiment_samples_data)

    }


    try:
        processing_messages_for_r.append(f"PY: Connecting to DB for study '{study_accession_str}'...")
        conn = _get_db_connection()
        processing_messages_for_r.append("PY: DB Connection successful.")

        for number_str in file_numbers_list_str:
            try:
                number = int(number_str)
                if number in data_fetch_functions:
                    filename, fetch_function_object = data_fetch_functions[number] # fetch_function_object IS the function
                    processing_messages_for_r.append(f"PY: Processing {filename} (Number {number})...")
                    
                    # Special handling for control samples with ImmPort template settings
                    if number == 14 and template_settings is not None:  # controlSamples.json
                        processing_messages_for_r.append(f"PY: Using ImmPort template settings for {filename}")
                        data_for_this_file = fetch_function_object(conn, study_accession_str, template_settings)
                    else:
                        # Standard call for other templates
                        data_for_this_file = fetch_function_object(conn, study_accession_str)
                    
                    generated_data_for_r[filename] = data_for_this_file
                    processing_messages_for_r.append(f"PY: Data for {filename} processed.")
                else:
                    processing_messages_for_r.append(f"PY: File number {number} is not recognized in data_fetch_functions.")
            except ValueError:
                processing_messages_for_r.append(f"PY: Invalid file number '{number_str}' encountered.")
            except Exception as e_item:
                item_filename_default = f"unknown_file_for_number_{number_str}"
                item_name_for_error = data_fetch_functions.get(int(number_str) if number_str.isdigit() else -1, (item_filename_default, None))[0]
                processing_messages_for_r.append(f"PY: Error processing item for file '{item_name_for_error}' (Number {number_str}): {str(e_item)}")
                generated_data_for_r[item_name_for_error] = {"error_processing_item": str(e_item)}

    except ConnectionError as db_conn_err: # Catching specific ConnectionError from _get_db_connection
        error_msg = f"PY DB Connection Error: {str(db_conn_err)}"
        processing_messages_for_r.append(error_msg)
        return {"error_summary": error_msg, "generated_data": generated_data_for_r, "messages": processing_messages_for_r}
    except psycopg2.Error as db_op_err: # Catching other DB operation errors
        error_msg = f"PY DB Operation Error: {str(db_op_err)}"
        processing_messages_for_r.append(error_msg)
        return {"error_summary": error_msg, "generated_data": generated_data_for_r, "messages": processing_messages_for_r}
    except Exception as e:
        error_msg = f"PY General Error: {str(e)}"
        processing_messages_for_r.append(error_msg)
        return {"error_summary": error_msg, "generated_data": generated_data_for_r, "messages": processing_messages_for_r}
    finally:
        if conn:
            conn.close()
            processing_messages_for_r.append("PY: DB Connection closed.")

    return {"generated_data": generated_data_for_r, "messages": processing_messages_for_r}
