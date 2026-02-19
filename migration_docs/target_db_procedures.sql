-- Target Database Procedures from madi_dat schema
-- Database: madi-dev-7605-db.c.dartmouth.edu
-- Schema: madi_dat
-- Retrieved: 2025-09-15

-- =====================================================
-- MASTER PROCEDURE: call_procedures
-- =====================================================
-- Purpose: Orchestrates the complete data loading process into final ImmPort schema
-- Parameter: experiment_accession - filters all data operations

CREATE OR REPLACE PROCEDURE madi_dat.call_procedures(IN experiment_accession character varying)
LANGUAGE plpgsql
AS $procedure$
begin
    CALL madi_dat.insert_biosamples (call_procedures.experiment_accession);
    CALL madi_dat.insert_expsamples (call_procedures.experiment_accession);
    CALL madi_dat.insert_expsample_2_biosample (call_procedures.experiment_accession);
    CALL madi_dat.insert_expsample_mbaa_detail (call_procedures.experiment_accession);
    CALL madi_dat.insert_mbaa_results (call_procedures.experiment_accession);
    CALL madi_dat.insert_control_samples (call_procedures.experiment_accession);
    CALL madi_dat.insert_standard_curves (call_procedures.experiment_accession);
    CALL madi_dat.insert_ctrl_results (call_procedures.experiment_accession);
    --CALL madi_dat.truncate_table (call_procedures.experiment_accession);
end;
$procedure$;

-- =====================================================
-- PROCEDURE: insert_biosamples
-- =====================================================
-- Purpose: Loads biosample metadata from mbaa_samples into final biosample table
-- Source: madi_results.mbaa_samples → madi_dat.biosample

CREATE OR REPLACE PROCEDURE madi_dat.insert_biosamples(IN experiment_accession character varying)
LANGUAGE plpgsql
AS $procedure$
begin
    INSERT INTO madi_dat.biosample (
        biosample_accession, 
        name, 
        planned_visit_accession, 
        study_accession, 
        study_time_collected, 
        study_time_collected_unit, 
        study_time_t0_event, 
        subject_accession, 
        type, 
        workspace_id
    )
    SELECT  
        biosample_accession, 
        biosample_name, 
        planned_visit_accession, 
        study_accession, 
        study_time_collected, 
        study_time_collected_unit, 
        study_time_t0_event, 
        subject_accession, 
        biosample_type, 
        workspace_id
    FROM madi_results.mbaa_samples
    WHERE mbaa_samples.experiment_accession = insert_biosamples.experiment_accession;
end;
$procedure$;

-- =====================================================
-- PROCEDURE: insert_control_samples  
-- =====================================================
-- Purpose: Loads control sample metadata into final control_sample table
-- Source: madi_results.db_control_samples → madi_dat.control_sample

CREATE OR REPLACE PROCEDURE madi_dat.insert_control_samples(IN experiment_accession character varying)
LANGUAGE plpgsql
AS $procedure$
begin
    INSERT INTO madi_dat.control_sample (
        control_sample_accession, 
        assay_group_id, 
        assay_id, 
        catalog_id, 
        dilution_factor, 
        experiment_accession, 
        lot_number, 
        result_schema, 
        source, 
        upload_result_status, 
        workspace_id
    )
    SELECT 
        control_sample_accession, 
        assay_group_id, 
        assay_id, 
        catalog_id, 
        dilution_factor, 
        db_control_samples.experiment_accession, 
        lot_number, 
        result_schema, 
        source, 
        upload_result_status, 
        workspace_id
    FROM madi_results.db_control_samples
    WHERE db_control_samples.experiment_accession = insert_control_samples.experiment_accession;
end;
$procedure$;

-- =====================================================
-- PROCEDURE: insert_mbaa_results
-- =====================================================
-- Purpose: Loads main MBAA measurement results into final mbaa_result table
-- Source: madi_results.mbaa_samples → madi_dat.mbaa_result

CREATE OR REPLACE PROCEDURE madi_dat.insert_mbaa_results(IN experiment_accession character varying)
LANGUAGE plpgsql
AS $procedure$
begin
    INSERT INTO madi_dat.mbaa_result (
        analyte_reported, 
        arm_accession, 
        assay_group_id, 
        assay_id, 
        biosample_accession, 
        concentration_unit_reported, 
        concentration_value_reported, 
        experiment_accession, 
        mfi, 
        mfi_coordinate, 
        source_accession, 
        source_type, 
        study_accession, 
        study_time_collected, 
        study_time_collected_unit, 
        subject_accession, 
        workspace_id
    )
    SELECT 
        analyte_reported, 
        arm_accession, 
        assay_group_id, 
        assay_id, 
        biosample_accession, 
        concentration_unit_reported, 
        concentration_value_reported, 
        experiment_accession, 
        mfi, 
        mfi_coordinate, 
        source_accession, 
        source_type, 
        study_accession, 
        study_time_collected, 
        study_time_collected_unit, 
        subject_accession, 
        workspace_id
    FROM madi_results.mbaa_samples
    WHERE mbaa_samples.experiment_accession = insert_mbaa_results.experiment_accession;
end;
$procedure$;

-- =====================================================
-- Additional Procedures in the Pipeline:
-- =====================================================
-- insert_expsamples - Load experimental sample metadata
-- insert_expsample_2_biosample - Link experimental samples to biosamples  
-- insert_expsample_mbaa_detail - Load MBAA experimental details
-- insert_standard_curves - Load standard curve data
-- insert_ctrl_results - Load control results
-- truncate_table - Clean up (commented out in call_procedures)

-- Note: These procedures all follow the same pattern:
-- 1. Take experiment_accession as parameter
-- 2. INSERT INTO madi_dat.[final_table] 
-- 3. SELECT FROM madi_results.[intermediate_table]
-- 4. WHERE [table].experiment_accession = [procedure].experiment_accession
