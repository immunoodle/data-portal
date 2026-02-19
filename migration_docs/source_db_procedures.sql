-- Source Database Procedures from madi_results schema
-- Database: mlr-c3d7-db.c.dartmouth.edu
-- Schema: madi_results
-- Retrieved: 2025-09-15

-- =====================================================
-- 1. PROCEDURE: database_control_samples
-- =====================================================
-- Purpose: Creates db_control_samples table by combining data from xmap_buffer, xmap_control, and xmap_standard
-- Parameters: study_accession, experiment_accession, result_schema, workspace_id

CREATE OR REPLACE PROCEDURE madi_results.database_control_samples(
    IN study_accession character varying, 
    IN experiment_accession character varying, 
    IN result_schema character varying, 
    IN workspace_id integer
)
LANGUAGE plpgsql
AS $procedure$
begin
    CREATE TABLE IF NOT EXISTS madi_results.db_control_samples AS
    SELECT
        xmap_buffer_id AS control_sample_accession, 
        CONCAT(database_control_samples.study_accession,'_plates') AS assay_group_id, 
        REPLACE(split_part(plate_id,'\',-1),' ','.') AS assay_id, 
        NULL AS catalog_id,
        CASE WHEN dilution IS NULL THEN '1' ELSE dilution END AS dilution_factor, 
        database_control_samples.experiment_accession AS experiment_accession, 
        NULL AS lot_number,
        database_control_samples.result_schema AS result_schema, 
        CONCAT('negative_control|buffer') AS source, 
        'in_QC_database' AS upload_result_status, 
        database_control_samples.workspace_id AS workspace_id
    FROM madi_results.xmap_buffer
    WHERE xmap_buffer.study_accession = database_control_samples.study_accession
    
    UNION
    
    SELECT
        xmap_control_id AS control_sample_accession, 
        CONCAT(database_control_samples.study_accession,'_plates') AS assay_group_id, 
        REPLACE(split_part(plate_id,'\',-1),' ','.') AS assay_id, 
        NULL AS catalog_id,
        CASE WHEN dilution IS NULL THEN '1' ELSE dilution END AS dilution_factor, 
        database_control_samples.experiment_accession AS experiment_accession, 
        NULL AS lot_number,
        database_control_samples.result_schema AS result_schema, 
        CONCAT('positive_control|',source) AS source, 
        'in_QC_database' AS upload_result_status, 
        database_control_samples.workspace_id AS workspace_id
    FROM madi_results.xmap_control
    WHERE xmap_control.study_accession = database_control_samples.study_accession
    
    UNION
    
    SELECT
        xmap_standard_id AS control_sample_accession, 
        CONCAT(database_control_samples.study_accession,'_plates') AS assay_group_id, 
        REPLACE(split_part(plate_id,'\',-1),' ','.') AS assay_id, 
        NULL AS catalog_id,
        CASE WHEN dilution IS NULL THEN '1' ELSE dilution END AS dilution_factor, 
        database_control_samples.experiment_accession AS experiment_accession, 
        NULL AS lot_number,
        database_control_samples.result_schema AS result_schema, 
        CONCAT('standard_curve|',source) AS source, 
        'in_QC_database' AS upload_result_status, 
        database_control_samples.workspace_id AS workspace_id
    FROM madi_results.xmap_standard
    WHERE xmap_standard.study_accession = database_control_samples.study_accession;
end;
$procedure$;
