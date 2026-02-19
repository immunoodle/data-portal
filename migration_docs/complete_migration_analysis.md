# Complete MADI Data Migration Pipeline Analysis

## Overview
The migration is a sophisticated **ETL (Extract, Transform, Load) pipeline** that moves data from raw Luminex format to ImmPort-standardized format across two databases.

## Complete Data Flow

### Phase 1: Source Database Transformation (mlr-c3d7-db.c.dartmouth.edu)
**Raw Luminex Data → Standardized Format**

#### Input: Raw `xmap_*` tables
- `xmap_sample` - Raw sample measurements
- `xmap_subjects` - Subject/patient data  
- `xmap_buffer` - Negative controls
- `xmap_control` - Positive controls
- `xmap_standard` - Standard curve samples
- `xmap_standard_fits` - Curve fitting results
- `xmap_planned_visit` - Visit schedules
- `xmap_sample_timing` - Actual visit timing
- `xmap_header` - Plate/batch info

#### Transformation Procedures:
1. **`database_control_samples`** → Creates `db_control_samples`
2. **`database_std_curves`** → Creates `db_standard_curves` 
3. **`db_control_results`** → Creates `db_control_results`
4. **`mbaa_for_database`** → Creates `mbaa_samples` (main data)

#### Output: Standardized tables ready for export
- `db_control_samples` - Standardized control metadata
- `db_standard_curves` - Standard curve parameters
- `db_control_results` - Control measurement results  
- `mbaa_samples` - Main sample data with ImmPort-compatible format

---

### Phase 2: Target Database Loading (madi-dev-7605-db.c.dartmouth.edu)
**Standardized Data → Final ImmPort Schema**

#### Input: Imported standardized tables in `madi_results` schema
- `madi_results.db_control_samples`
- `madi_results.db_standard_curves` 
- `madi_results.db_control_results`
- `madi_results.mbaa_samples`

#### Master Orchestration Procedure:
**`madi_dat.call_procedures(experiment_accession)`** executes in sequence:

1. **`insert_biosamples`**
   ```sql
   INSERT INTO madi_dat.biosample (...) 
   SELECT biosample_accession, biosample_name, planned_visit_accession, study_accession, 
          study_time_collected, study_time_collected_unit, study_time_t0_event, 
          subject_accession, biosample_type, workspace_id
   FROM madi_results.mbaa_samples
   ```

2. **`insert_expsamples`** - Load experimental sample metadata

3. **`insert_expsample_2_biosample`** - Link experimental samples to biosamples

4. **`insert_expsample_mbaa_detail`** - Load MBAA experimental details

5. **`insert_mbaa_results`**
   ```sql
   INSERT INTO madi_dat.mbaa_result (...)
   SELECT analyte_reported, arm_accession, assay_group_id, assay_id, biosample_accession,
          concentration_unit_reported, concentration_value_reported, experiment_accession,
          mfi, mfi_coordinate, source_accession, source_type, study_accession,
          study_time_collected, study_time_collected_unit, subject_accession, workspace_id
   FROM madi_results.mbaa_samples
   ```

6. **`insert_control_samples`**
   ```sql
   INSERT INTO madi_dat.control_sample (...)
   SELECT control_sample_accession, assay_group_id, assay_id, catalog_id, dilution_factor,
          experiment_accession, lot_number, result_schema, source, upload_result_status, workspace_id
   FROM madi_results.db_control_samples
   ```

7. **`insert_standard_curves`** - Load standard curve data

8. **`insert_ctrl_results`** - Load control results

#### Output: Final ImmPort-compliant tables in `madi_dat` schema
- `madi_dat.biosample` - Final biosample records
- `madi_dat.mbaa_result` - Final MBAA measurement results
- `madi_dat.control_sample` - Final control sample records
- `madi_dat.standard_curve` - Final standard curve records
- Plus related linking and detail tables

---

## Current Manual Process vs. Automated Solution

### Current Manual Process:
1. Run source transformation procedures manually
2. Export `db_*` and `mbaa_samples` tables to Excel
3. Manually import Excel files to target database `madi_results` schema
4. Run `call_procedures(experiment_accession)` to load into final `madi_dat` schema

### Proposed Automated Process:
1. **UI Input**: Migration parameters (study_accession, experiment_accession, etc.)
2. **Source Transform**: Execute source procedures automatically
3. **Data Transfer**: Direct database-to-database transfer (skip Excel)
4. **Target Load**: Execute `call_procedures` automatically
5. **Validation**: Verify data integrity and provide reports

---

## Key Insights

### Data Transformations:
- **Accession Generation**: Creates standardized IDs (ES*, BS* prefixes)
- **Metadata Enrichment**: Joins subject, visit, timing, and plate information
- **Format Standardization**: Converts Luminex format to ImmPort schema
- **Quality Control**: Marks data as 'in_QC_database' status

### Critical Dependencies:
- Source procedures expect specific `xmap_*` table structure
- Target procedures expect data in `madi_results` schema 
- All procedures filter by `experiment_accession`
- Workspace_id ties data to specific analysis workspace

### Error Points:
- Missing source data in `xmap_*` tables
- Incorrect experiment_accession references
- Schema permissions between source and target
- Data type mismatches during transfer

This analysis reveals that the migration is much more than simple data copying - it's a complete data transformation and standardization pipeline that converts proprietary Luminex data into NIH ImmPort-compliant format.
