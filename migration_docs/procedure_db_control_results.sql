-- =====================================================
-- 3. PROCEDURE: db_control_results
-- =====================================================
-- Purpose: Creates db_control_results table by combining control data from xmap_buffer, xmap_control, and xmap_standard
-- Parameters: experiment_accession, study_accession, workspace_id, study_id, concentration_unit_reported

CREATE OR REPLACE PROCEDURE madi_results.db_control_results(
    IN experiment_accession character varying, 
    IN study_accession character varying, 
    IN workspace_id integer, 
    IN study_id character varying, 
    IN concentration_unit_reported character varying
)
LANGUAGE plpgsql
AS $procedure$
begin
    CREATE TABLE IF NOT EXISTS madi_results.db_control_results AS
    SELECT 
        CONCAT(xmap_buffer.experiment_accession, '|', antigen) AS analyte_reported, 
        NULL AS arm_accession, 
        xmap_buffer.experiment_accession AS assay_group_id,
        CONCAT(xmap_buffer.experiment_accession, '_', dilution, '_', REPLACE(split_part(plate_id,'\',-1),' ','.')) AS assay_id, 
        NULL AS biosample_accession, 
        db_control_results.concentration_unit_reported,
        1/dilution AS concentration_value_reported, 
        db_control_results.experiment_accession, 
        antibody_mfi AS mfi, 
        well AS mfi_coordinate, 
        CONCAT('blank', xmap_buffer_id) AS source_accession, 
        'CONTROL SAMPLE' AS source_type, 
        db_control_results.study_accession,
        NULL AS study_time_collected, 
        'Not Specified' AS study_time_collected_unit, 
        NULL AS subject_accession, 
        db_control_results.workspace_id
    FROM madi_results.xmap_buffer
    WHERE xmap_buffer.study_accession = db_control_results.study_id
    
    UNION
    
    SELECT 
        CONCAT(xmap_control.experiment_accession, '|', antigen) AS analyte_reported, 
        NULL AS arm_accession, 
        xmap_control.experiment_accession AS assay_group_id,
        CONCAT(xmap_control.experiment_accession, '_', dilution, '_', REPLACE(split_part(plate_id,'\',-1),' ','.')) AS assay_id, 
        NULL AS biosample_accession, 
        db_control_results.concentration_unit_reported,
        1/dilution AS concentration_value_reported, 
        db_control_results.experiment_accession, 
        antibody_mfi AS mfi, 
        well AS mfi_coordinate, 
        CONCAT('posc', xmap_control_id) AS source_accession, 
        'CONTROL SAMPLE' AS source_type, 
        db_control_results.study_accession,
        NULL AS study_time_collected, 
        'Not Specified' AS study_time_collected_unit, 
        NULL AS subject_accession, 
        db_control_results.workspace_id
    FROM madi_results.xmap_control
    WHERE xmap_control.study_accession = db_control_results.study_id
    
    UNION
    
    SELECT 
        CONCAT(xmap_standard.experiment_accession, '|', antigen) AS analyte_reported, 
        NULL AS arm_accession, 
        xmap_standard.experiment_accession AS assay_group_id,
        CONCAT(xmap_standard.experiment_accession, '_', dilution, '_', REPLACE(split_part(plate_id,'\',-1),' ','.')) AS assay_id, 
        NULL AS biosample_accession, 
        db_control_results.concentration_unit_reported,
        1/dilution AS concentration_value_reported, 
        db_control_results.experiment_accession, 
        antibody_mfi AS mfi, 
        well AS mfi_coordinate, 
        CONCAT('stand', xmap_standard_id) AS source_accession, 
        'STANDARD CURVE' AS source_type, 
        db_control_results.study_accession,
        NULL AS study_time_collected, 
        'Not Specified' AS study_time_collected_unit, 
        NULL AS subject_accession, 
        db_control_results.workspace_id
    FROM madi_results.xmap_standard
    WHERE xmap_standard.study_accession = db_control_results.study_id;
end;
$procedure$;
