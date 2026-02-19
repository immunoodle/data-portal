-- =====================================================
-- 4. PROCEDURE: mbaa_for_database (Main MBAA Migration)
-- =====================================================
-- Purpose: Creates mbaa_samples table - the main procedure for migrating MBAA (Multiplex Bead Array Assay) data
-- This is the most complex procedure that joins multiple tables to create the final sample data
-- Parameters: study_accession, experiment_accession, concentration_unit_reported, source_type, 
--            study_time_collected_unit, result_schema, study_time_t0_event, workspace_id

CREATE OR REPLACE PROCEDURE madi_results.mbaa_for_database(
    IN study_accession character varying, 
    IN experiment_accession character varying, 
    IN concentration_unit_reported character varying, 
    IN source_type character varying, 
    IN study_time_collected_unit character varying, 
    IN result_schema character varying, 
    IN study_time_t0_event character varying, 
    IN workspace_id integer
)
LANGUAGE plpgsql
AS $procedure$
begin
    CREATE TABLE IF NOT EXISTS madi_results.mbaa_samples AS
    SELECT
        xs.xmap_sample_id AS result_id,
        subj.study_accession,
        mbaa_for_database.experiment_accession,
        xs.experiment_accession AS assay_group_id,
        CONCAT(xs.experiment_accession, '_', xs.dilution, '_', xmap_header.plateid) AS assay_id,
        xs.dilution,
        CONCAT(xs.experiment_accession, '|', xs.antigen) AS analyte_reported,
        xs.antibody_au AS concentration_value_reported,
        mbaa_for_database.concentration_unit_reported,
        xs.antibody_mfi AS mfi,
        xs.well AS mfi_coordinate,
        mbaa_for_database.source_type,
        
        subj.subject_accession AS subject_accession,
        visit.planned_visit_accession AS planned_visit_accession,
        subj.arm_accession AS arm_accession,
        timing.actual_visit_day AS study_time_collected,
        mbaa_for_database.study_time_collected_unit,
        
        CONCAT('ES', SUBSTRING(mbaa_for_database.experiment_accession, 4, LENGTH(mbaa_for_database.experiment_accession)),'_', ROW_NUMBER() OVER (ORDER BY xs.xmap_sample_id)) AS source_accession,
        CONCAT('BS', SUBSTRING(subj.subject_accession, 7, LENGTH(subj.subject_accession)), SUBSTRING(xs.timeperiod, 1, 3), '_', ROW_NUMBER() OVER (ORDER BY xs.xmap_sample_id)) AS biosample_accession,
        mbaa_for_database.result_schema,
        CONCAT(subj.subject_accession, '_', xs.timeperiod) AS biosample_name,
        CONCAT(xs.experiment_accession, '_', xs.dilution, subj.subject_accession, xs.timeperiod) AS expsample_name,
        visit.type as biosample_type,
        mbaa_for_database.study_time_t0_event,
        mbaa_for_database.workspace_id
        
    FROM madi_results.xmap_sample xs
    JOIN madi_results.xmap_subjects subj ON xs.patientid = subj.xmap_patientid
    JOIN madi_results.xmap_planned_visit visit ON xs.timeperiod = visit.timepoint_name
    JOIN madi_results.xmap_sample_timing timing ON xs.patientid = timing.patientid AND xs.timeperiod = timing.timeperiod
    JOIN madi_results.xmap_header ON xs.plate_id = xmap_header.plate_id AND xs.study_accession = xmap_header.study_accession
    WHERE xs.study_accession = mbaa_for_database.study_accession;
end;
$procedure$;
