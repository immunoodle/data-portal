-- =========================================================================
-- MADI Data Portal - SQL Schema Changelog
-- All migration-related schema changes tracked in chronological order
-- =========================================================================

-- =========================================================================
-- v0.9 - Pre-requisite Tables (2026-02-18)
-- Created earlier, but documented here for completeness.
-- =========================================================================

CREATE TABLE IF NOT EXISTS madi_dat.sample_qc_data (
    sample_qc_id SERIAL PRIMARY KEY,
    expsample_accession VARCHAR(100) NOT NULL,
    workspace_id INTEGER NOT NULL,
    plate_number VARCHAR(100),
    plate_well VARCHAR(50),
    nominal_sample_dilution VARCHAR(100),
    analyte_name VARCHAR(200),
    reagent_name VARCHAR(200),
    timeperiod VARCHAR(100),
    subject_id VARCHAR(200),
    sample_dilution NUMERIC,
    pctaggbeads NUMERIC,
    samplingerrors VARCHAR(500),
    bead_count INTEGER,
    mfi NUMERIC,
    predicted_concentration DOUBLE PRECISION,
    se_concentration NUMERIC,
    gate_class_loq VARCHAR(50),
    gate_class_lod VARCHAR(50),
    CONSTRAINT fk_sample_qc_expsample FOREIGN KEY (expsample_accession) REFERENCES madi_dat.expsample (expsample_accession)
);

CREATE TABLE IF NOT EXISTS madi_dat.model_qc_data (
    model_qc_id SERIAL PRIMARY KEY,
    standard_curve_accession VARCHAR(100) NOT NULL,
    workspace_id INTEGER NOT NULL,
    plate_number VARCHAR(100),
    nominal_sample_dilution VARCHAR(100),
    source_standard VARCHAR(200),
    analyte_name VARCHAR(200),
    reagent_name VARCHAR(200),
    formula_name TEXT,
    formula TEXT,
    a NUMERIC,
    b NUMERIC,
    c NUMERIC,
    d NUMERIC,
    g NUMERIC,
    bkg_method VARCHAR(100),
    is_log_response BOOLEAN,
    is_log_x BOOLEAN,
    apply_prozone BOOLEAN,
    dfresidual NUMERIC,
    nobs NUMERIC,
    lloq NUMERIC,
    uloq NUMERIC,
    lloq_y NUMERIC,
    uloq_y NUMERIC,
    llod NUMERIC,
    ulod NUMERIC,
    inflect_x NUMERIC,
    inflect_y NUMERIC,
    dydx_inflect NUMERIC,
    aic NUMERIC,
    bic NUMERIC,
    loglik NUMERIC,
    mse DOUBLE PRECISION,
    CONSTRAINT fk_model_qc_standard_curve FOREIGN KEY (standard_curve_accession) REFERENCES madi_dat.standard_curve (standard_curve_accession)
);

-- =========================================================================
-- v1.0 - Initial Schema Updates (2026-02-16)
-- For MBAA Results and Standard Curve migration
-- =========================================================================

-- 1. Widen MBAA Result assay_id to support long file paths (e.g., from plate_id)
ALTER TABLE madi_dat.mbaa_result
ALTER COLUMN assay_id TYPE VARCHAR(250);

-- 2. Widen Standard Curve assay_id (same reason)
ALTER TABLE madi_dat.standard_curve
ALTER COLUMN assay_id TYPE VARCHAR(250);

-- 3. Widen Standard Curve analyte_accession to support 'antigen|feature'
ALTER TABLE madi_dat.standard_curve
ALTER COLUMN analyte_accession TYPE VARCHAR(100);

-- 4. Widen Standard Curve formula to support parameter string
ALTER TABLE madi_dat.standard_curve
ALTER COLUMN formula TYPE VARCHAR(1000);

-- =========================================================================
-- v1.1 - Model QC New Columns (2026-02-18)
-- Boss added mindc, minrdl, maxdc, maxrdl to best_glance_all in source DB.
-- We need matching columns in our model_qc_data table.
-- =========================================================================

ALTER TABLE madi_dat.model_qc_data
ADD COLUMN IF NOT EXISTS mindc NUMERIC;

ALTER TABLE madi_dat.model_qc_data
ADD COLUMN IF NOT EXISTS minrdl NUMERIC;

ALTER TABLE madi_dat.model_qc_data
ADD COLUMN IF NOT EXISTS maxdc NUMERIC;

ALTER TABLE madi_dat.model_qc_data
ADD COLUMN IF NOT EXISTS maxrdl NUMERIC;

-- =========================================================================
-- v1.2 - Migration Audit Log Table (2026-02-18)
-- Track all migration operations with operator info and results
-- =========================================================================

CREATE TABLE IF NOT EXISTS madi_dat.migration_audit_log (
    audit_id SERIAL PRIMARY KEY,
    operator_name VARCHAR(255) NOT NULL,
    operator_designation VARCHAR(255),
    migration_mode VARCHAR(10) NOT NULL, -- 'TEST' or 'LIVE'
    study_accession VARCHAR(50),
    experiment_accession VARCHAR(50),
    source_study VARCHAR(50),
    source_experiment VARCHAR(50),
    status VARCHAR(20) NOT NULL, -- 'STARTED', 'COMPLETED', 'FAILED'
    subjects_validated INTEGER DEFAULT 0,
    expsamples_inserted INTEGER DEFAULT 0,
    expsamples_failed INTEGER DEFAULT 0,
    mbaa_results_inserted INTEGER DEFAULT 0,
    controls_inserted INTEGER DEFAULT 0,
    standard_curves_inserted INTEGER DEFAULT 0,
    model_qc_inserted INTEGER DEFAULT 0,
    sample_qc_inserted INTEGER DEFAULT 0,
    error_message TEXT,
    started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    completed_at TIMESTAMP,
    workspace_id INTEGER
);