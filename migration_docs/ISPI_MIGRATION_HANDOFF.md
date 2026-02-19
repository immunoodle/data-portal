# I-SPI Migration Handoff & Context

**Date:** December 1, 2025
**Branch:** `spidb`

## 🎯 Primary Goal
Migrate I-SPI study data from the intermediate `madi_results` schema (specifically `xmap_sample` and related tables) into the final `madi_dat` production schema using the MADI Data Portal Shiny App.

## 🚧 Current Status
The migration infrastructure is **fully implemented** but requires **testing in Docker**.

### ✅ Completed Work
1.  **Branch Setup**:
    *   Created `spidb` branch.
    *   Merged latest changes from `main` (SSL fixes).
    *   Restored previously stashed I-SPI migration work.

2.  **UI Implementation (`madi-shiny-front/ispi_migration.R`)**:
    *   Added "Migrate Data for I-SPI" to the sidebar (`user.R`, `sidebar_body_panel.R`).
    *   Built a complete migration dashboard:
        *   **Connection Setup**: Connect to source DB.
        *   **Data Preview**: View source data.
        *   **Mapping Interface**:
            *   Timeperiods → Planned Visits (with order, day ranges).
            *   Analysis Groups (Agroups) → Arms/Cohorts (with types).
    *   **Execution Controls**:
        *   **Test Migration**: Runs with `ROLLBACK` to verify data without saving.
        *   **Live Migration**: Runs with `COMMIT` (requires "CONFIRM" input).

3.  **Backend Logic (`madi-shiny-front/ispi_migration_queries.R`)**:
    *   Implemented transaction-safe SQL functions:
        *   `insert_subjects()`
        *   `create_planned_visits()`
        *   `create_arms()`
        *   `insert_arm_subject_associations()`
        *   `insert_experiment_samples()`
    *   **Failure Tracking**:
        *   Captures individual item failures (e.g., duplicate IDs, constraint violations).
        *   Captures skipped items (e.g., samples with unmapped timeperiods).
        *   Returns detailed lists of errors for the UI.

4.  **Reporting**:
    *   The UI now displays a detailed summary after Test/Live runs.
    *   Shows counts of Inserted, Existing, Failed, and Skipped items.
    *   Lists specific error messages for the first 10 failures/skips to help debugging.

5.  **Environment**:
    *   `docker.env` configured with `LOCAL_DEV=1` to bypass Dex authentication for easier local testing.

## 📋 Next Steps (To-Do)

1.  **Docker Testing**:
    *   Build the Docker image: `docker-compose build`
    *   Run the container: `docker-compose up`
    *   Access the app at `http://localhost:3838`.

2.  **Verify Migration Workflow**:
    *   Navigate to "Migrate Data for I-SPI".
    *   Connect to the database.
    *   Map all Timeperiods and Agroups.
    *   Run **Test Migration**.
    *   **CRITICAL**: Check the "Items Needing Attention" report in the modal.

3.  **Refinement**:
    *   If there are failures, fix the data or the mapping logic.
    *   If `arm_subject_associations` needs failure tracking, add it (currently implemented for Subjects and Samples).

4.  **Live Execution**:
    *   Once the Test Migration is clean (0 failures), execute the **Live Migration**.

## 📂 Key Files
*   `madi-shiny-front/ispi_migration.R`: Main UI and Server logic.
*   `madi-shiny-front/ispi_migration_queries.R`: SQL functions and transaction management.
*   `madi-shiny-front/user.R`: Sidebar menu definition.
*   `docker.env`: Environment variables.

## � Database Procedures & Documentation
The `migration_docs/` folder contains reference SQL procedures and analysis:
*   `target_db_procedures.sql`: Stored procedures for the target `madi_dat` schema (e.g., `call_procedures`, `insert_biosamples`).
*   `source_db_procedures.sql`: Procedures from the source database.
*   `procedure_mbaa_for_database.sql`: MBAA specific procedures.
*   `complete_migration_analysis.md`: Analysis of the migration requirements.

## �💡 Notes for Next Session
*   The system is designed to be safe. Always run a **Test Migration** first.
*   If you see "Skipped Items", it usually means a Timeperiod in the source data wasn't mapped to a Planned Visit in the UI.
