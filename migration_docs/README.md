# Data Migration Documentation

## Overview
This project implements a data migration UI that connects two different applications with mapped data. The migration process involves:

1. **Source Database**: `mlr-c3d7-db.c.dartmouth.edu` (PostgreSQL)
   - Host: mlr-c3d7-db.c.dartmouth.edu
   - Database: postgres
   - User: admin
   - Schema: madi_results (contains migration procedures)

2. **Current Process**:
   - Procedures exist in both app databases
   - Procedures pull data and create temp tables
   - Manual export from source DB using Excel
   - Upload to target PostgreSQL
   - Run procedures to populate final tables

3. **Goal**: 
   - Create R Shiny UI for automated migration
   - Provide DB credentials interface
   - Automate the manual Excel export/import process

## Files Structure
- `source_db_procedures.sql` - Stored procedures from madi_results schema
- `source_db_schema.sql` - Schema structure and table definitions
- `target_db_schema.sql` - Target database schema structure
- `migration_mapping.md` - Data mapping between source and target
- `connection_config.R` - R database connection configuration

## Next Steps
1. Extract procedures from madi_results schema
2. Document table structures and relationships
3. Create R Shiny migration interface
4. Implement automated data transfer mechanism
