# I-SPI Migration Fix Plan

**Date:** December 1, 2025  
**Issue:** Subject inserts failing due to missing required `gender` column

---

## Root Cause Analysis

### Current Problem:
```
ERROR: null value in column "gender" of relation "subject" violates not-null constraint
```

### Why It's Happening:
1. We're only reading from `xmap_sample` table
2. `xmap_sample` doesn't have `gender`, `species`, or other subject metadata
3. We're inserting subjects with only `subject_accession` and `workspace_id`
4. The `madi_dat.subject` table **requires** `gender` (NOT NULL constraint)

---

## Solution: Join with xmap_subjects Table

### Data Source Changes Required:

#### Current Query (in ispi_migration.R, line ~621):
```r
base_query <- paste0("SELECT * FROM ", input$source_schema, ".", input$preview_table)
```

This only gets `xmap_sample` data, which has:
- `patientid`
- `timeperiod`
- `agroup`
- `sampleid`
- ... (assay data)

#### NEW Query Needed: 
We need to join `xmap_sample` with `xmap_subjects` to get complete subject information:

```sql
SELECT 
    xs.*,
    subj.gender,
    subj.species,
    subj.ethnicity,
    subj.race,
    subj.race_specify,
    subj.strain,
    subj.strain_characteristics,
    subj.description as subject_description,
    subj.subject_accession as existing_subject_accession,
    subj.arm_accession as existing_arm_accession
FROM madi_results.xmap_sample xs
LEFT JOIN madi_results.xmap_subjects subj 
    ON xs.patientid = subj.xmap_patientid
WHERE xs.study_accession = $1
LIMIT $2
```

This enriched data will provide:
- All original `xmap_sample` columns
- Required subject fields (`gender`, `species`)
- Optional subject fields (ethnicity, race, strain, etc.)
- Pre-mapped identifiers if they exist

---

## Code Changes Required

### 1. Update Data Preview Query (ispi_migration.R)

**Location:** Around line 605-650

**Change:** When `preview_table == "xmap_sample"`, use the enriched join query instead of simple SELECT

```r
observeEvent(input$load_data_preview, {
  req(migration_values$source_connected, input$preview_table)
  
  showNotification("Loading data preview...", type = "message", duration = 3)
  
  tryCatch({
    conn_preview <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = input$source_host,
      port = input$source_port,
      dbname = input$source_database,
      user = input$source_user,
      password = input$source_password
    )
    
    # Special handling for xmap_sample - join with xmap_subjects
    if (input$preview_table == "xmap_sample") {
      base_query <- paste0(
        "SELECT xs.*, ",
        "subj.gender, subj.species, subj.ethnicity, subj.race, ",
        "subj.race_specify, subj.strain, subj.strain_characteristics, ",
        "subj.description as subject_description, ",
        "subj.subject_accession as existing_subject_accession, ",
        "subj.arm_accession as existing_arm_accession ",
        "FROM ", input$source_schema, ".xmap_sample xs ",
        "LEFT JOIN ", input$source_schema, ".xmap_subjects subj ",
        "ON xs.patientid = subj.xmap_patientid"
      )
    } else {
      base_query <- paste0("SELECT * FROM ", input$source_schema, ".", input$preview_table)
    }
    
    # ... rest of filtering logic stays the same
```

### 2. Update insert_subjects() Function (ispi_migration_queries.R)

**Location:** Lines 26-105

**Changes Needed:**
- Accept `source_data` with joined subject fields
- Extract subject metadata from source_data
- Insert with all required and optional fields

```r
insert_subjects <- function(conn, source_data, workspace_id, subject_prefix = "SUB_", commit = FALSE) {
  
  # Get unique patients with their subject metadata
  unique_patients <- source_data %>%
    select(patientid, gender, species, ethnicity, race, race_specify, 
           strain, strain_characteristics, subject_description) %>%
    filter(!is.na(patientid)) %>%
    distinct(patientid, .keep_all = TRUE)
  
  cat("📊 Found", nrow(unique_patients), "unique patients to migrate\n")
  
  if(nrow(unique_patients) == 0) {
    cat("⚠️  No patients to insert\n")
    return(list(success = TRUE, inserted = 0, existing = 0, failed = list()))
  }
  
  # Start transaction
  if(!commit) {
    DBI::dbExecute(conn, "BEGIN")
    cat("🔄 Transaction started (TEST MODE - will rollback)\n")
  }
  
  tryCatch({
    inserted_count <- 0
    existing_count <- 0
    failed_items <- list()
    
    for(i in 1:nrow(unique_patients)) {
      patient <- unique_patients[i, ]
      subject_acc <- paste0(subject_prefix, patient$patientid)
      
      # Create savepoint for this insert
      savepoint_name <- paste0("sp_subject_", gsub("[^0-9]", "", patient$patientid))
      
      tryCatch({
        # Set savepoint
        DBI::dbExecute(conn, paste0("SAVEPOINT ", savepoint_name))
        
        # Check if subject already exists
        existing <- DBI::dbGetQuery(conn,
          "SELECT COUNT(*) as count FROM madi_dat.subject WHERE subject_accession = $1",
          params = list(subject_acc)
        )
        
        if(existing$count > 0) {
          cat("  ℹ️  Subject", subject_acc, "already exists - skipping\n")
          existing_count <- existing_count + 1
          DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", savepoint_name))
          next
        }
        
        # Validate required fields
        if(is.na(patient$gender) || is.null(patient$gender) || patient$gender == "") {
          stop("Missing required field: gender")
        }
        
        # Insert new subject with all available fields
        DBI::dbExecute(conn,
          "INSERT INTO madi_dat.subject 
           (subject_accession, workspace_id, gender, species, ethnicity, race, 
            race_specify, strain, strain_characteristics, description) 
           VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
          params = list(
            subject_acc, 
            workspace_id,
            patient$gender,
            if(is.na(patient$species)) NULL else patient$species,
            if(is.na(patient$ethnicity)) NULL else patient$ethnicity,
            if(is.na(patient$race)) NULL else patient$race,
            if(is.na(patient$race_specify)) NULL else patient$race_specify,
            if(is.na(patient$strain)) NULL else patient$strain,
            if(is.na(patient$strain_characteristics)) NULL else patient$strain_characteristics,
            if(is.na(patient$subject_description)) NULL else patient$subject_description
          )
        )
        
        cat("  ✅ Inserted subject:", subject_acc, "Gender:", patient$gender, "\n")
        inserted_count <- inserted_count + 1
        
        # Release savepoint on success
        DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", savepoint_name))
        
      }, error = function(e) {
        # Rollback to savepoint on error
        tryCatch({
          DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", savepoint_name))
        }, error = function(rollback_err) {
          # Ignore rollback errors
        })
        
        cat("  ❌ Failed to insert subject", subject_acc, ":", e$message, "\n")
        failed_items[[length(failed_items) + 1]] <<- list(
          type = "subject",
          identifier = patient$patientid,
          accession = subject_acc,
          error = e$message
        )
      })
    }
    
    if(commit) {
      DBI::dbExecute(conn, "COMMIT")
      cat("✅ COMMITTED: Inserted", inserted_count, "subjects,", existing_count, "already existed,", length(failed_items), "failed\n")
    } else {
      DBI::dbExecute(conn, "ROLLBACK")
      cat("🔙 ROLLED BACK: Would have inserted", inserted_count, "subjects,", existing_count, "already existed,", length(failed_items), "would fail\n")
    }
    
    return(list(
      success = TRUE, 
      inserted = inserted_count, 
      existing = existing_count,
      failed = failed_items
    ))
    
  }, error = function(e) {
    DBI::dbExecute(conn, "ROLLBACK")
    cat("❌ ERROR:", e$message, "\n")
    cat("🔙 Transaction rolled back\n")
    return(list(success = FALSE, error = e$message, failed = list()))
  })
}
```

---

## Testing Plan

### 1. Verify xmap_subjects Structure
First, check what columns actually exist in the source database:

```sql
-- Run this in source database to verify structure
SELECT column_name, data_type, is_nullable
FROM information_schema.columns
WHERE table_schema = 'madi_results'
  AND table_name = 'xmap_subjects'
ORDER BY ordinal_position;
```

### 2. Test Join Query
Verify the join works and returns expected data:

```sql
SELECT 
    xs.patientid,
    xs.sampleid,
    xs.timeperiod,
    xs.agroup,
    subj.gender,
    subj.species,
    subj.subject_accession
FROM madi_results.xmap_sample xs
LEFT JOIN madi_results.xmap_subjects subj 
    ON xs.patientid = subj.xmap_patientid
WHERE xs.study_accession = 'your_study_accession_here'
LIMIT 10;
```

### 3. Run Test Migration
- Load enriched data preview
- Map timeperiods and agroups
- Run Test Migration (commit=FALSE)
- Verify no more gender errors
- Check that subjects are being inserted with proper metadata

### 4. Verify Data Quality
After successful test:
- Check that all patients have gender
- Check that species is populated (or defaulted appropriately)
- Verify subject count matches expected

---

## Next Steps

1. **First:** Check xmap_subjects table structure in source DB
2. **Then:** Update ispi_migration.R with enriched query
3. **Then:** Update insert_subjects() in ispi_migration_queries.R
4. **Finally:** Test with Docker and verify results

---

## Notes

- The `mbaa_for_database` procedure already does this join - we're just following that pattern
- If `xmap_subjects` doesn't exist or has different columns, we'll need to adjust
- May need to handle cases where patient exists in `xmap_sample` but not in `xmap_subjects`
- Consider adding data validation in the UI to warn about missing gender before migration
