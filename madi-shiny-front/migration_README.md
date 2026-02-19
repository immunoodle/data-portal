# MADI Data Migration Tool

## Overview
This R Shiny application automates the data migration process between MADI databases, replacing the manual Excel export/import workflow with direct database connections.

## Files Structure
```
madi-shiny-front/
├── migration_app.R              # Main application entry point
├── data_migration_ui.R          # User interface module
├── data_migration_server.R      # Server logic module  
├── migration_db_config.R        # Database configuration utilities
└── migration_README.md          # This documentation
```

## Installation & Setup

### Required R Packages
```r
install.packages(c(
  "shiny",
  "shinydashboard", 
  "shinyWidgets",
  "DBI",
  "RPostgreSQL",
  "DT"
))
```

### Database Requirements
- **Source Database**: PostgreSQL with `madi_results` schema containing migration procedures
- **Target Database**: PostgreSQL database where migrated data will be stored
- **Network Access**: Ability to connect to both databases from the R environment

## Usage

### 1. Launch the Application
```r
# In R console
source("migration_app.R")
migration_app()
```

### 2. Configure Database Connections

#### Source Database (Default Configuration)
- **Host**: mlr-c3d7-db.c.dartmouth.edu
- **Port**: 5432
- **Database**: postgres
- **Username**: admin
- **Password**: [Required - enter in UI]
- **Schema**: madi_results

#### Target Database
Configure your target PostgreSQL database connection parameters in the UI.

### 3. Set Migration Parameters
- **Study Accession**: Unique study identifier (e.g., SDY123)
- **Experiment Accession**: Unique experiment identifier (e.g., EXP123)
- **Workspace ID**: Target workspace identifier (numeric)
- **Concentration Unit**: Unit for concentration values (e.g., pg/mL)
- **Source Type**: Biological source type (e.g., SERUM, PLASMA)
- **Time Collection Unit**: Time measurement unit (e.g., Days)
- **T0 Event**: Reference event for time measurements (e.g., vaccination)
- **Limit Unit**: Unit for detection limits (e.g., pg/mL)

### 4. Execute Migration
1. Test both database connections using the "Test Connection" buttons
2. Verify all parameters are correctly entered
3. Click "Start Migration" to begin the automated process
4. Monitor progress in the Status tab

## Migration Process

The tool executes 4 stored procedures in sequence:

### 1. database_control_samples
- **Purpose**: Creates `db_control_samples` table
- **Sources**: `xmap_buffer`, `xmap_control`, `xmap_standard`
- **Output**: Control sample metadata for negative controls, positive controls, and standards

### 2. database_std_curves  
- **Purpose**: Creates `db_standard_curves` table
- **Sources**: `xmap_standard_fits`
- **Output**: Standard curve parameters and detection limits

### 3. db_control_results
- **Purpose**: Creates `db_control_results` table  
- **Sources**: `xmap_buffer`, `xmap_control`, `xmap_standard`
- **Output**: Control sample measurement results

### 4. mbaa_for_database
- **Purpose**: Creates `mbaa_samples` table (main data)
- **Sources**: Complex join of `xmap_sample`, `xmap_subjects`, `xmap_planned_visit`, `xmap_sample_timing`, `xmap_header`
- **Output**: Primary MBAA measurement data with subject, visit, and timing information

## Data Flow

```
Source Database (madi_results schema)
├── xmap_* tables (raw data)
├── Migration procedures execute
├── Temporary result tables created
└── Data extracted for transfer

Target Database  
├── Receive migrated data
├── Load into target schema
└── Execute target procedures (if needed)
```

## Troubleshooting

### Connection Issues
- Verify database credentials and network connectivity
- Check if SSL/TLS settings are configured correctly
- Ensure database allows connections from your IP address

### Schema Validation Errors
- Confirm `madi_results` schema exists in source database
- Verify all required procedures are present:
  - `database_control_samples`
  - `database_std_curves` 
  - `db_control_results`
  - `mbaa_for_database`
- Check that required tables exist (xmap_sample, xmap_subjects, etc.)

### Migration Failures
- Review migration log for specific error messages
- Verify all parameters are correctly formatted
- Check database permissions for procedure execution
- Ensure target schema has write permissions

### Performance Considerations
- Large datasets may take several minutes to migrate
- Monitor database connection timeouts for long-running procedures
- Consider running during off-peak hours for production databases

## Security Notes
- Database passwords are not stored and must be entered each session
- Connections use SSL when available
- Consider using read-only database users when possible
- Audit migration activities through database logs

## Integration with Existing App
To integrate with the main MADI Shiny application:

1. Add migration modules to existing app structure
2. Include navigation menu item for migration tool
3. Source migration files in main app.R:
```r
source("data_migration_ui.R")
source("data_migration_server.R") 
source("migration_db_config.R")
```

4. Add migration module to server function:
```r
migration_server("migration_module")
```

## Support
For technical issues or questions about the migration tool, refer to:
- Database schema documentation in `/migration_docs/`
- Source procedure definitions in `/migration_docs/source_db_procedures.sql`
- Migration process documentation in `/migration_docs/migration_summary.md`
