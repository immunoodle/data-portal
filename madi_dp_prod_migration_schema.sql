-- ============================================================================
-- MADI Data Portal - Production Migration Schema Script
-- Run this script against the production database BEFORE starting the data migration.
-- ============================================================================

BEGIN;

-- ============================================================================
-- 1. ADD NEW ENUMS / LOOKUP VALUES
-- ============================================================================
INSERT INTO
    madi_dat.lk_source_type (name, description)
VALUES (
        'CONTROL SAMPLE',
        'Sample used to perform quality control of assay results.'
    )
ON CONFLICT (name) DO NOTHING;

-- ============================================================================
-- 2. CREATE NEW TABLES FOR EXPERIMENTAL SAMPLES & CONTROLS
-- ============================================================================

CREATE TABLE IF NOT EXISTS madi_dat.expsample_mbaa_detail (
    expsample_accession VARCHAR(15) NOT NULL,
    assay_group_id VARCHAR(100),
    assay_id VARCHAR(100),
    dilution_factor VARCHAR(100),
    plate_type VARCHAR(50),
    CONSTRAINT expsample_mbaa_detail_pkey PRIMARY KEY (expsample_accession),
    CONSTRAINT fk_expsample_mbaa_detail_1 FOREIGN KEY (expsample_accession) REFERENCES madi_dat.expsample (expsample_accession) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT fk_expsample_mbaa_detail_2 FOREIGN KEY (plate_type) REFERENCES madi_dat.lk_plate_type (name)
);

CREATE TABLE IF NOT EXISTS madi_dat.control_sample (
    control_sample_accession VARCHAR(15) NOT NULL,
    assay_group_id VARCHAR(100),
    assay_id VARCHAR(250),
    catalog_number VARCHAR(100),
    dilution_factor VARCHAR(100),
    experiment_accession VARCHAR(15) NOT NULL,
    lot_number VARCHAR(100),
    result_schema VARCHAR(50) NOT NULL,
    source_type VARCHAR(50),
    upload_result_status VARCHAR(20),
    workspace_id INTEGER NOT NULL,
    CONSTRAINT control_sample_pkey PRIMARY KEY (control_sample_accession),
    CONSTRAINT fk_control_sample_1 FOREIGN KEY (workspace_id) REFERENCES madi_dat.workspace (workspace_id) ON DELETE CASCADE,
    CONSTRAINT fk_control_sample_2 FOREIGN KEY (experiment_accession) REFERENCES madi_dat.experiment (experiment_accession) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT fk_control_sample_3 FOREIGN KEY (result_schema) REFERENCES madi_dat.lk_expsample_result_schema (name)
);

CREATE TABLE IF NOT EXISTS madi_dat.control_sample_2_file_info (
    control_sample_accession VARCHAR(15) NOT NULL,
    file_info_id INTEGER NOT NULL,
    result_schema VARCHAR(50) NOT NULL,
    CONSTRAINT control_sample_2_file_info_pkey PRIMARY KEY (
        control_sample_accession,
        file_info_id
    ),
    CONSTRAINT fk_control_sample_2_file_info_1 FOREIGN KEY (control_sample_accession) REFERENCES madi_dat.control_sample (control_sample_accession) ON DELETE CASCADE,
    CONSTRAINT fk_control_sample_2_file_info_2 FOREIGN KEY (file_info_id) REFERENCES madi_dat.file_info (file_info_id) ON DELETE CASCADE,
    CONSTRAINT fk_control_sample_2_file_info_3 FOREIGN KEY (result_schema) REFERENCES madi_dat.lk_expsample_result_schema (name)
);

CREATE TABLE IF NOT EXISTS madi_dat.standard_curve (
    standard_curve_accession VARCHAR(15) NOT NULL,
    analyte_accession VARCHAR(100),
    analyte_preferred VARCHAR(100),
    analyte_reported VARCHAR(100),
    assay_group_id VARCHAR(100),
    assay_id VARCHAR(250),
    experiment_accession VARCHAR(15) NOT NULL,
    formula VARCHAR(1000),
    lower_limit VARCHAR(100),
    lower_limit_unit VARCHAR(100),
    result_schema VARCHAR(50) NOT NULL,
    upload_result_status VARCHAR(20),
    upper_limit VARCHAR(100),
    upper_limit_unit VARCHAR(100),
    workspace_id INTEGER NOT NULL,
    CONSTRAINT standard_curve_pkey PRIMARY KEY (standard_curve_accession),
    CONSTRAINT fk_standard_curve_1 FOREIGN KEY (workspace_id) REFERENCES madi_dat.workspace (workspace_id) ON DELETE CASCADE,
    CONSTRAINT fk_standard_curve_2 FOREIGN KEY (experiment_accession) REFERENCES madi_dat.experiment (experiment_accession) ON DELETE CASCADE,
    CONSTRAINT fk_standard_curve_3 FOREIGN KEY (result_schema) REFERENCES madi_dat.lk_expsample_result_schema (name),
    CONSTRAINT fk_standard_curve_4 FOREIGN KEY (analyte_accession) REFERENCES madi_dat.lk_analyte (analyte_accession)
);

CREATE TABLE IF NOT EXISTS madi_dat.standard_curve_2_file_info (
    standard_curve_accession VARCHAR(15) NOT NULL,
    file_info_id INTEGER NOT NULL,
    result_schema VARCHAR(50) NOT NULL,
    CONSTRAINT standard_curve_2_file_info_pkey PRIMARY KEY (
        standard_curve_accession,
        file_info_id
    ),
    CONSTRAINT fk_standard_curve_2_file_info_1 FOREIGN KEY (standard_curve_accession) REFERENCES madi_dat.standard_curve (standard_curve_accession) ON DELETE CASCADE,
    CONSTRAINT fk_standard_curve_2_file_info_2 FOREIGN KEY (file_info_id) REFERENCES madi_dat.file_info (file_info_id) ON DELETE CASCADE,
    CONSTRAINT fk_standard_curve_2_file_info_3 FOREIGN KEY (result_schema) REFERENCES madi_dat.lk_expsample_result_schema (name)
);

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
    mindc NUMERIC,
    minrdl NUMERIC,
    maxdc NUMERIC,
    maxrdl NUMERIC,
    CONSTRAINT fk_model_qc_standard_curve FOREIGN KEY (standard_curve_accession) REFERENCES madi_dat.standard_curve (standard_curve_accession)
);

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

-- ============================================================================
-- 3. ALTER EXISTING TABLES TO RELAX CONSTRAINTS FOR NEW DATA
-- ============================================================================

-- Widen MBAA Result assay_id to support long file paths
ALTER TABLE madi_dat.mbaa_result
ALTER COLUMN assay_id TYPE VARCHAR(250);

-- Widen lk_analyte and mbaa_result analyte_accession to support 'washington_72|Total_IgG'
ALTER TABLE madi_dat.lk_analyte
ALTER COLUMN analyte_accession TYPE VARCHAR(100);

ALTER TABLE madi_dat.mbaa_result
ALTER COLUMN analyte_accession TYPE VARCHAR(100);

-- ============================================================================
-- 4. APPLY SCHEMA FIX FOR WORKSPACE SEQUENCE (If it hasn't been applied yet)
-- ============================================================================
CREATE SEQUENCE IF NOT EXISTS madi_dat.workspace_workspace_id_seq START
WITH
    1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE madi_dat.workspace
ALTER COLUMN workspace_id
SET DEFAULT nextval(
    'madi_dat.workspace_workspace_id_seq'::regclass
);

ALTER SEQUENCE madi_dat.workspace_workspace_id_seq OWNED BY madi_dat.workspace.workspace_id;

-- Ensure workspace_access table exists (for sharing workspaces)
CREATE SEQUENCE IF NOT EXISTS madi_track.workspace_access_access_id_seq START
WITH
    1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

CREATE TABLE IF NOT EXISTS madi_track.workspace_access (
    access_id INTEGER NOT NULL DEFAULT nextval(
        'madi_track.workspace_access_access_id_seq'::regclass
    ),
    workspace_id INTEGER NOT NULL,
    user_email CHARACTER VARYING(255) NOT NULL,
    role CHARACTER VARYING(50) DEFAULT 'member'::character varying,
    joined_at TIMESTAMP WITHOUT TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    joined_via_key CHARACTER VARYING(50),
    is_active BOOLEAN DEFAULT true,
    CONSTRAINT workspace_access_pkey PRIMARY KEY (access_id),
    CONSTRAINT workspace_access_workspace_id_user_email_key UNIQUE (workspace_id, user_email),
    CONSTRAINT workspace_access_role_check CHECK (
        role::text = ANY (
            ARRAY[
                'owner'::character varying::text,
                'admin'::character varying::text,
                'member'::character varying::text
            ]
        )
    ),
    CONSTRAINT workspace_access_workspace_id_fkey FOREIGN KEY (workspace_id) REFERENCES madi_dat.workspace (workspace_id)
);

CREATE INDEX IF NOT EXISTS idx_workspace_access_user_email ON madi_track.workspace_access USING btree (user_email);

CREATE INDEX IF NOT EXISTS idx_workspace_access_workspace_id ON madi_track.workspace_access USING btree (workspace_id);

ALTER SEQUENCE madi_track.workspace_access_access_id_seq OWNED BY madi_track.workspace_access.access_id;

COMMIT;