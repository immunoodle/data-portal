# Migration Procedures Summary

## Overview
The migration involves 4 main procedures that extract and transform data from the source MADI database into standardized format:

## Procedure Flow

### 1. Control Data Migration
- **database_control_samples()** → Creates `db_control_samples` table
- **db_control_results()** → Creates `db_control_results` table
- Sources: `xmap_buffer`, `xmap_control`, `xmap_standard` tables

### 2. Standard Curves Migration  
- **database_std_curves()** → Creates `db_standard_curves` table
- Source: `xmap_standard_fits` table

### 3. Main Sample Data Migration
- **mbaa_for_database()** → Creates `mbaa_samples` table (main data)
- Sources: Complex join across multiple tables:
  - `xmap_sample` (main sample data)
  - `xmap_subjects` (subject information)
  - `xmap_planned_visit` (visit schedule)
  - `xmap_sample_timing` (timing data)
  - `xmap_header` (plate information)

## Key Parameters Needed for Migration

### Common Parameters:
- `study_accession` - Study identifier
- `experiment_accession` - Experiment identifier
- `workspace_id` - Target workspace ID
- `result_schema` - Target schema name

### Specific Parameters:
- `concentration_unit_reported` - Unit for concentration values
- `source_type` - Type of source data
- `study_time_collected_unit` - Time unit for collection
- `study_time_t0_event` - Time zero event definition
- `limit_unit` - Unit for detection limits
- `study_id` - Alternative study identifier

## Current Manual Process (to be automated):
1. Run procedures in source database
2. Export resulting tables to Excel
3. Import Excel files to target PostgreSQL database
4. Run target database procedures to finalize data

## Proposed Automated Process:
1. UI collects migration parameters
2. Connect to source database
3. Execute procedures directly
4. Transfer data programmatically to target database
5. Execute target procedures
6. Provide migration status and validation
