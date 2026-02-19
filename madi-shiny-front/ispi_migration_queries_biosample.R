# =====================================================
# 2. INSERT BIOSAMPLES (with hybrid check-create logic)
# =====================================================
# Checks if biosamples exist, creates if missing, reuses if exists
insert_biosamples <- function(conn, source_data, workspace_id, study_accession, 
                              timeperiod_mapping, config, commit = FALSE) {
  
  cat("📊 Processing biosamples\n")
  
  # Get unique samples with all biosample data
  unique_samples <- source_data %>%
    select(patientid, subject_accession, timeperiod, sampleid, 
           biosample_type, actual_visit_day) %>%
    filter(!is.na(patientid), !is.na(subject_accession), !is.na(timeperiod)) %>%
    distinct(sampleid, .keep_all = TRUE)
  
  if(nrow(unique_samples) == 0) {
    cat("⚠️  No samples to process\n")
    return(list(success = FALSE, error = "No samples found", inserted = 0, existing = 0, failed = list()))
  }
  
  # Start transaction
  if(!commit) {
    DBI::dbExecute(conn, "BEGIN")
    cat("🔄 Transaction started (TEST MODE - will rollback)\n")
  }
  
  inserted_count <- 0
  existing_count <- 0
  failed_biosamples <- list()
  biosample_map <- list()  # Track which biosample each sample uses
  
  for(i in 1:nrow(unique_samples)) {
    row <- unique_samples[i, ]
    
    # Get planned_visit from timeperiod mapping
    planned_visit_acc <- timeperiod_mapping[[row$timeperiod]]
    if(is.null(planned_visit_acc) || planned_visit_acc == "NEW") {
      cat("  ⚠️  Skipping sample", row$sampleid, "- timeperiod not mapped\n")
      next
    }
    
    tryCatch({
      # Generate unique biosample accession using pattern from SQL
      # Pattern: BS{subject_num}{timeperiod_short}_{row_num}
      subject_num <- gsub("SUB", "", row$subject_accession)
      timeperiod_short <- substr(row$timeperiod, 1, 3)
      biosample_acc <- sprintf("BS%s%s_%d", subject_num, timeperiod_short, i)
      
      # Generate biosample name
      biosample_name <- sprintf("%s_%s", row$subject_accession, row$timeperiod)
      
      # Check if biosample already exists for this subject+planned_visit
      existing <- DBI::dbGetQuery(conn,
        "SELECT biosample_accession FROM madi_dat.biosample 
         WHERE subject_accession = $1 
           AND planned_visit_accession = $2 
           AND workspace_id = $3
         LIMIT 1",
        params = list(
          as.character(row$subject_accession)[1],
          as.character(planned_visit_acc)[1],
          as.integer(workspace_id)[1]
        )
      )
      
      if(nrow(existing) > 0) {
        # Biosample exists - reuse it
        biosample_acc <- existing$biosample_accession[1]
        existing_count <- existing_count + 1
        
        if(existing_count <= 3) {
          cat("  ✅ Reusing biosample:", biosample_acc, "for sample", row$sampleid, "\n")
        } else if(existing_count == 4) {
          cat("  ... (showing first 3 reused)\n")
        }
      } else {
        # Biosample doesn't exist - create new
        DBI::dbExecute(conn,
          "INSERT INTO madi_dat.biosample (
             biosample_accession, name, planned_visit_accession, 
             study_accession, study_time_collected, 
             study_time_collected_unit, study_time_t0_event,
             subject_accession, type, workspace_id
           ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
          params = list(
            biosample_acc,
            biosample_name,
            as.character(planned_visit_acc)[1],
            as.character(study_accession)[1],
            if(is.na(row$actual_visit_day)) NULL else as.numeric(row$actual_visit_day)[1],
            as.character(config$time_unit %||% "Days")[1],
            as.character(config$t0_event %||% "Time of enrollment")[1],
            as.character(row$subject_accession)[1],
            as.character(row$biosample_type %||% "blood")[1],
            as.integer(workspace_id)[1]
          )
        )
        
        inserted_count <- inserted_count + 1
        
        if(inserted_count <= 3) {
          cat("  ✅ Created biosample:", biosample_acc, "for sample", row$sampleid, "\n")
        } else if(inserted_count == 4) {
          cat("  ... (showing first 3 created)\n")
        }
      }
      
      # Track biosample for this sample
      biosample_map[[as.character(row$sampleid)]] <- biosample_acc
      
    }, error = function(e) {
      cat("  ❌ Failed to process biosample for sample", row$sampleid, ":", e$message, "\n")
      failed_biosamples[[length(failed_biosamples) + 1]] <- list(
        sampleid = row$sampleid,
        error = e$message
      )
    })
  }
  
  # Rollback or commit
  if(!commit) {
    DBI::dbExecute(conn, "ROLLBACK")
    cat("🔙 ROLLED BACK: Would have inserted", inserted_count, "biosamples,", 
        existing_count, "already existed,", length(failed_biosamples), "would fail\n")
  } else {
    DBI::dbExecute(conn, "COMMIT")
    cat("✅ Committed", inserted_count, "new biosamples,", existing_count, "reused\n")
  }
  
  return(list(
    success = length(failed_biosamples) == 0,
    inserted = inserted_count,
    existing = existing_count,
    failed = failed_biosamples,
    biosample_map = biosample_map  # Return mapping for linking step
  ))
}
# =====================================================
# 5. INSERT EXPERIMENT SAMPLES (Link Biosample -> Experiment)
# =====================================================
insert_experiment_samples <- function(conn, biosample_accessions, experiment_accession,
                                     workspace_id, study_accession, commit = FALSE) {
  
  cat("📊 Processing experiment samples (linking biosamples to experiment)...\n")
  
  unique_biosamples <- unique(biosample_accessions)
  if(length(unique_biosamples) == 0) {
    return(list(success = TRUE, inserted = 0, existing = 0, failed = list(), expsample_map = list()))
  }
  
  # Start transaction if not already in one (but usually we are)
  # Assuming external transaction management for this step
  
  inserted_count <- 0
  existing_count <- 0
  failed_items <- list()
  expsample_map <- list() # Map: biosample_acc -> expsample_acc
  
  for(bs_acc in unique_biosamples) {
    if(is.na(bs_acc) || bs_acc == "") next
    
    tryCatch({
      # Generate expected Accession: ES_{experiment}_{biosample}
      # To keep it deterministic and simpler
      # Procedure uses: 'ES_' || experiment_accession || '_' || biosample_accession
      # We'll use format: ES_EXPACC_BSACC (cleaned)
      
      # Clean accessions for ID generation
      clean_exp <- gsub("EXP", "", experiment_accession)
      clean_bs <- gsub("BS", "", bs_acc) # Shorten
      
      expsample_acc <- paste0("ES_", clean_exp, "_", clean_bs)
      
      # Check if exists
      exists_query <- "SELECT expsample_accession FROM madi_dat.expsample WHERE expsample_accession = $1"
      existing <- DBI::dbGetQuery(conn, exists_query, params = list(expsample_acc))
      
      if(nrow(existing) > 0) {
        existing_count <- existing_count + 1
        expsample_map[[bs_acc]] <- expsample_acc
      } else {
        # Insert
        insert_query <- "
          INSERT INTO madi_dat.expsample (
            expsample_accession, biosample_accession, experiment_accession,
            study_accession, workspace_id, upload_result_status
          ) VALUES ($1, $2, $3, $4, $5, $6)
        "
        
        DBI::dbExecute(conn, insert_query, params = list(
          expsample_acc, bs_acc, experiment_accession,
          study_accession, workspace_id, "new"
        ))
        
        inserted_count <- inserted_count + 1
        expsample_map[[bs_acc]] <- expsample_acc
      }
      
    }, error = function(e) {
      failed_items[[length(failed_items) + 1]] <- list(biosample = bs_acc, error = e$message)
    })
  }
  
  if(commit) {
    cat(paste0("  ✅ COMMITTED: ", inserted_count, " experiment samples created, ", existing_count, " reused\n"))
  } else {
    cat(paste0("  🔙 PROCESSED: ", inserted_count, " experiment samples (Test Mode)\n"))
  }
  
  return(list(
    success = length(failed_items) == 0,
    inserted = inserted_count,
    existing = existing_count,
    failed = failed_items,
    expsample_map = expsample_map
  ))
}
