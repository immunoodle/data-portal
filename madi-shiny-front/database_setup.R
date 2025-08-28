# Database Connection Setup
# This file provides safe database connection functions for the MADI Data Portal

# Function to create a safe database connection with proper error handling
create_safe_db_connection <- function() {
  tryCatch({
    # Create database connection using environment variables
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("db"),
      host = Sys.getenv("db_host"),
      port = Sys.getenv("db_port"),
      user = Sys.getenv("db_userid_x"),
      password = Sys.getenv("db_pwd_x"),
      options = "-c search_path=madi_dat"  # Default schema for most operations
    )
    
    # Test the connection
    test_query <- DBI::dbGetQuery(conn, "SELECT 1 as test_connection")
    if (nrow(test_query) == 1 && test_query$test_connection == 1) {
      print("✅ Database connection established successfully")
      return(conn)
    } else {
      stop("Database connection test failed")
    }
    
  }, error = function(e) {
    error_msg <- paste("❌ Failed to connect to database:", e$message)
    print(error_msg)
    
    # Print debug information about environment variables
    print("DEBUG: Database connection environment variables:")
    print(paste("  db:", Sys.getenv("db")))
    print(paste("  db_host:", Sys.getenv("db_host")))
    print(paste("  db_port:", Sys.getenv("db_port")))
    print(paste("  db_userid_x:", if(nzchar(Sys.getenv("db_userid_x"))) "SET" else "NOT SET"))
    print(paste("  db_pwd_x:", if(nzchar(Sys.getenv("db_pwd_x"))) "SET" else "NOT SET"))
    
    stop(error_msg)
  })
}

# Function to create connection with specific schema
create_db_connection_with_schema <- function(schema = "madi_dat") {
  tryCatch({
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("db"),
      host = Sys.getenv("db_host"),
      port = Sys.getenv("db_port"),
      user = Sys.getenv("db_userid_x"),
      password = Sys.getenv("db_pwd_x"),
      options = paste0("-c search_path=", schema)
    )
    
    print(paste("✅ Database connection established with schema:", schema))
    return(conn)
    
  }, error = function(e) {
    error_msg <- paste("❌ Failed to connect to database with schema", schema, ":", e$message)
    print(error_msg)
    stop(error_msg)
  })
}

# Function to safely close database connection
close_db_connection <- function(conn) {
  if (!is.null(conn)) {
    tryCatch({
      DBI::dbDisconnect(conn)
      print("✅ Database connection closed")
    }, error = function(e) {
      warning(paste("Warning closing database connection:", e$message))
    })
  }
}

# Function to create an empty studies dataframe with the correct structure
create_empty_studies_dataframe <- function() {
  # Create an empty dataframe with the expected study columns for filters
  empty_df <- data.frame(
    study_accession = character(0),
    sponsoring_organization = character(0),
    brief_title = character(0),
    research_focus = character(0),
    clinical_trial = character(0),
    tree_id = character(0),
    actual_enrollment = numeric(0),
    madi_program = character(0),
    condition_preferred = character(0),
    type = character(0),
    measurement_technique = character(0),
    species = character(0),
    sex = character(0),
    have_assessment = character(0),
    have_test = character(0),
    actual_completion_date = as.Date(character(0)),
    actual_start_date = as.Date(character(0)),
    age_unit = character(0),
    brief_description = character(0),
    condition_studied = character(0),
    dcl_id = character(0),
    description = character(0),
    doi = character(0),
    endpoints = character(0),
    gender_included = character(0),
    hypothesis = character(0),
    initial_data_release_date = as.Date(character(0)),
    initial_data_release_version = character(0),
    intervention_agent = character(0),
    latest_data_release_date = as.Date(character(0)),
    stringsAsFactors = FALSE
  )
  
  print("✅ Created empty studies dataframe with filter columns")
  return(empty_df)
}

# Function to get studies data for a specific workspace
get_studies_data_direct <- function(conn, workspace_id) {
  if (is.null(conn) || is.null(workspace_id)) {
    print("⚠️ Invalid connection or workspace_id, returning empty dataframe")
    return(create_empty_studies_dataframe())
  }
  
  tryCatch({
    # First get the actual columns from the study table
    study_query <- "
      SELECT 
        s.study_accession,
        COALESCE(s.sponsoring_organization, '') as sponsoring_organization,
        COALESCE(s.brief_title, '') as brief_title,
        COALESCE(s.actual_enrollment, 0) as actual_enrollment,
        COALESCE(s.clinical_trial, '') as clinical_trial,
        COALESCE(s.study_accession, '') as tree_id,
        '' as research_focus,
        '' as madi_program,
        '' as condition_preferred,
        '' as type,
        '' as measurement_technique,
        '' as species,
        '' as sex,
        '' as have_assessment,
        '' as have_test
      FROM madi_dat.study s
      WHERE s.workspace_id = $1
      ORDER BY s.study_accession
    "
    
    studies_data <- DBI::dbGetQuery(conn, study_query, params = list(workspace_id))
    
    if (nrow(studies_data) > 0) {
      print(paste("✅ Retrieved", nrow(studies_data), "studies for workspace", workspace_id))
    } else {
      print(paste("ℹ️ No studies found for workspace", workspace_id))
    }
    
    return(studies_data)
    
  }, error = function(e) {
    warning(paste("❌ Error fetching studies data for workspace", workspace_id, ":", e$message))
    print("📝 Returning empty studies dataframe due to error")
    return(create_empty_studies_dataframe())
  })
}

# Function to get user workspace studies (for all_study_list)
get_user_workspace_studies <- function(conn, workspace_id) {
  if (is.null(conn) || is.null(workspace_id)) {
    print("⚠️ Invalid connection or workspace_id for study list")
    return(data.frame())
  }
  
  tryCatch({
    # Simple query to get study list
    study_list_query <- "
      SELECT study_accession, brief_title, sponsoring_organization
      FROM madi_dat.study
      WHERE workspace_id = $1
      ORDER BY study_accession
    "
    
    study_list <- DBI::dbGetQuery(conn, study_list_query, params = list(workspace_id))
    
    print(paste("✅ Retrieved study list with", nrow(study_list), "entries for workspace", workspace_id))
    return(study_list)
    
  }, error = function(e) {
    warning(paste("❌ Error fetching study list for workspace", workspace_id, ":", e$message))
    return(data.frame())
  })
}