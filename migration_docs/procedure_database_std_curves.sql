-- =====================================================
-- 2. PROCEDURE: database_std_curves
-- =====================================================
-- Purpose: Creates db_standard_curves table from xmap_standard_fits data
-- Parameters: study_accession, experiment_accession, result_schema, limit_unit, workspace_id

CREATE OR REPLACE PROCEDURE madi_results.database_std_curves(
    IN study_accession character varying, 
    IN experiment_accession character varying, 
    IN result_schema character varying, 
    IN limit_unit character varying, 
    IN workspace_id integer
)
LANGUAGE plpgsql
AS $procedure$
begin
    CREATE TABLE IF NOT EXISTS madi_results.db_standard_curves AS
    SELECT 
        xmap_standard_fits AS standard_curve_accession, 
        CONCAT(antigen, '_', analyte) AS analyte_reported, 
        CONCAT(database_std_curves.study_accession, '_plates') AS assay_group_id, 
        plateid AS assay_id,
        database_std_curves.experiment_accession AS experiment_accession, 
        formula AS formula, 
        llod as lower_limit, 
        database_std_curves.limit_unit as lower_limit_unit, 
        database_std_curves.result_schema as result_schema,
        'in_QC_database' AS upload_result_status, 
        ulod AS upper_limit, 
        database_std_curves.limit_unit as upper_limit_unit, 
        database_std_curves.workspace_id as workspace_id
        --antigen, iter, status, crit, l_asy, r_asy, x_mid, scale, bendlower, bendupper, llod, ulod, loglik, aic, bic, deviance, dfresidual, nobs, rsquare_fit, source, g, mse, cv, lloq, uloq, loq_method, bkg_method, is_log_mfi_axis, linear_center
    FROM madi_results.xmap_standard_fits xsf
    WHERE xsf.study_accession = database_std_curves.study_accession;
end;
$procedure$;
