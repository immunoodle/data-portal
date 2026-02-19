# Database Schema Documentation

## Source Database: madi_results schema

### Tables (23 total):
- `db_control_results` - Control results data
- `db_control_samples` - Control sample data  
- `db_standard_curves` - Standard curves data
- `mbaa_samples` - MBAA sample data
- `xmap_antigen_family` - Antigen family mappings
- `xmap_arm_reference` - Arm reference data
- `xmap_buffer` - Buffer configurations
- `xmap_control` - Control configurations
- `xmap_dilution_analysis` - Dilution analysis data
- `xmap_dilution_parameters` - Dilution parameters
- `xmap_header` - Header information
- `xmap_planned_visit` - Planned visit data
- `xmap_profile` - Profile data
- `xmap_sample` - Sample data
- `xmap_sample_timing` - Sample timing information
- `xmap_standard` - Standard data
- `xmap_standard_fit_tab` - Standard fit tables
- `xmap_standard_fits` - Standard fit data
- `xmap_standard_preds` - Standard predictions
- `xmap_standard_stor` - Standard storage
- `xmap_study_config` - Study configuration
- `xmap_subjects` - Subject data
- `xmap_users` - User data

### Key Data Flow:
1. **Control Data**: `db_control_samples` → `db_control_results`
2. **Standard Curves**: `db_standard_curves` with related `xmap_standard_*` tables
3. **MBAA Data**: `mbaa_samples` connected to various `xmap_*` tables
4. **Study Management**: `xmap_study_config` → `xmap_subjects` → `xmap_sample`

### Migration Procedures:
1. `database_control_samples()` - Migrates control sample data
2. `database_std_curves()` - Migrates standard curves 
3. `db_control_results()` - Migrates control results
4. `mbaa_for_database()` - Main MBAA data migration (most complex)

### Common Parameters:
- `study_accession` - Study identifier
- `experiment_accession` - Experiment identifier  
- `workspace_id` - Workspace identifier
- `result_schema` - Target schema for results
- `concentration_unit_reported` - Unit for concentration values
