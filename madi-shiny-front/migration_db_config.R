# Database Configuration for MADI Migration
# This file contains helper functions for database connections and migration utilities

library(DBI)
library(RPostgreSQL)

# Default connection parameters
DEFAULT_SOURCE_CONFIG <- list(
  host = "mlr-c3d7-db.c.dartmouth.edu",
  port = 5432,
  database = "postgres",
  schema = "madi_results"
)

# Helper function to validate database connection parameters
validate_connection_params <- function(host, port, database, user, password) {
  errors <- c()
  
  if (is.null(host) || host == "") {
    errors <- c(errors, "Host is required")
  }
  
  if (is.null(port) || !is.numeric(port)) {
    errors <- c(errors, "Port must be a number")
  }
  
  if (is.null(database) || database == "") {
    errors <- c(errors, "Database name is required")
  }
  
  if (is.null(user) || user == "") {
    errors <- c(errors, "Username is required")
  }
  
  if (is.null(password) || password == "") {
    errors <- c(errors, "Password is required")
  }
  
  return(errors)
}

# Create a secure database connection
create_db_connection <- function(host, port, database, user, password, ssl_mode = "prefer") {
  tryCatch({
    # Validate parameters first
    validation_errors <- validate_connection_params(host, port, database, user, password)
    
    if (length(validation_errors) > 0) {
      return(list(
        success = FALSE,
        error = paste("Validation errors:", paste(validation_errors, collapse = ", "))
      ))
    }
    
    # Create connection
    conn <- dbConnect(
      RPostgreSQL::PostgreSQL(),
      host = host,
      port = port,
      dbname = database,
      user = user,
      password = password
    )
    
    # Test connection
    test_query <- dbGetQuery(conn, "SELECT 1 as test")
    
    if (nrow(test_query) == 1) {
      return(list(
        success = TRUE,
        connection = conn,
        message = "Connection successful"
      ))
    } else {
      dbDisconnect(conn)
      return(list(
        success = FALSE,
        error = "Connection test failed"
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Connection failed:", e$message)
    ))
  })
}

# Check if required schema and tables exist
check_source_schema <- function(conn, schema_name = "madi_results") {
  tryCatch({
    # Check if schema exists
    schema_query <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name = $1"
    schema_result <- dbGetQuery(conn, schema_query, params = list(schema_name))
    
    if (nrow(schema_result) == 0) {
      return(list(
        valid = FALSE,
        error = paste("Schema", schema_name, "not found")
      ))
    }
    
    # Check for required procedures
    proc_query <- paste0(
      "SELECT routine_name FROM information_schema.routines ",
      "WHERE routine_schema = $1 AND routine_type = 'FUNCTION'"
    )
    proc_result <- dbGetQuery(conn, proc_query, params = list(schema_name))
    
    required_procs <- c("database_control_samples", "database_std_curves", 
                       "db_control_results", "mbaa_for_database")
    
    missing_procs <- setdiff(required_procs, proc_result$routine_name)
    
    if (length(missing_procs) > 0) {
      return(list(
        valid = FALSE,
        error = paste("Missing procedures:", paste(missing_procs, collapse = ", "))
      ))
    }
    
    # Check for key tables
    table_query <- paste0(
      "SELECT table_name FROM information_schema.tables ",
      "WHERE table_schema = $1"
    )
    table_result <- dbGetQuery(conn, table_query, params = list(schema_name))
    
    required_tables <- c("xmap_sample", "xmap_subjects", "xmap_standard_fits")
    missing_tables <- setdiff(required_tables, table_result$table_name)
    
    if (length(missing_tables) > 0) {
      return(list(
        valid = FALSE,
        error = paste("Missing tables:", paste(missing_tables, collapse = ", "))
      ))
    }
    
    return(list(
      valid = TRUE,
      message = "Schema validation successful",
      procedures = proc_result$routine_name,
      tables = table_result$table_name
    ))
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      error = paste("Schema validation failed:", e$message)
    ))
  })
}

# Get preview data from source tables
get_source_preview <- function(conn, schema_name = "madi_results", table_name = "xmap_sample", limit = 10) {
  tryCatch({
    query <- paste0("SELECT * FROM ", schema_name, ".", table_name, " LIMIT $1")
    result <- dbGetQuery(conn, query, params = list(limit))
    return(result)
  }, error = function(e) {
    return(data.frame(Error = paste("Could not retrieve data:", e$message)))
  })
}

# Clean up database connections
cleanup_connections <- function(connections) {
  for (conn in connections) {
    if (!is.null(conn) && dbIsValid(conn)) {
      tryCatch({
        dbDisconnect(conn)
      }, error = function(e) {
        # Silent cleanup
      })
    }
  }
}
