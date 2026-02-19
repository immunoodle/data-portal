# run_locally.R - Script to launch MADI Data Portal locally
# Usage: Rscript run_locally.R

# 1. Define required packages from Dockerfile
required_packages <- c(
  "shiny", "shinyjs", "shinyalert", "shinydashboard", "shinyWidgets", 
  "visNetwork", "DT", "sqldf", "rsconnect", "shinyFiles", 
  "rhandsontable", "sendmailR", "reactable", "auth0", "datamods", 
  "openxlsx", "aws.s3", "reticulate", "jsonlite", "shinyAce", 
  "jose", "httr2", "openssl", "urltools", "httr", "strex", "purrr",
  "dplyr", "readr", "stringr", "lubridate", "glue", "DBI", "RPostgres",
  "tibble", "tidyr", "ggplot2", "forcats"
)

# Check and install missing packages
cat("Checking for installed packages...\n")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  cat("⚠️  The following packages are missing:\n")
  print(missing_packages)
  cat("\nInstalling missing packages now (this may take a while)...\n")
  
  # Configure for optimal Apple Silicon installation
  options(
    timeout = 600,  # 10 minutes for large packages
    repos = c(CRAN = "https://cloud.r-project.org"),
    pkgType = "both"  # Prefer binaries but fall back to source if needed
  )
  
  install.packages(missing_packages, dependencies = TRUE)
  
  # Verify installation
  still_missing <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if(length(still_missing) > 0) {
    cat("\n❌ Failed to install:\n")
    print(still_missing)
  } else {
    cat("\n✅ All required R packages are now installed.\n")
  }
} else {
  cat("✅ All required R packages are installed.\n")
}


# 3. Load environment variables from docker.env
if(file.exists("docker.env")) {
  cat("Loading environment variables from docker.env...\n")
  env_lines <- readLines("docker.env")
  for(line in env_lines) {
    # Skip comments and empty lines
    if(nchar(trimws(line)) > 0 && !startsWith(trimws(line), "#")) {
      parts <- strsplit(line, "=")[[1]]
      if(length(parts) >= 2) {
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        
        # SKIP RETICULATE_PYTHON if it points to the Docker path
        if (key == "RETICULATE_PYTHON" && startsWith(value, "/opt/miniconda")) {
          cat("⚠️  Skipping RETICULATE_PYTHON (points to Docker path):", value, "\n")
        } else {
          cat(sprintf("DEBUG: Setting env var '%s' = '%s' (Length: %d)\n", key, value, nchar(value)))
          args <- list(value)
          names(args) <- key
          do.call(Sys.setenv, args)
        }
      }
    }
  }
} else {
  cat("⚠️  docker.env file not found! App may fail to connect to database.\n")
}

# Explicitly unset RETICULATE_PYTHON if it was somehow set to the docker path
if (startsWith(Sys.getenv("RETICULATE_PYTHON"), "/opt/miniconda")) {
  Sys.unsetenv("RETICULATE_PYTHON")
  cat("Removed RETICULATE_PYTHON environment variable (was pointing to Docker path).\n")
}

# Set LOCAL_DEV to 1 to bypass Dex if not already set
if(Sys.getenv("LOCAL_DEV") == "") {
  Sys.setenv(LOCAL_DEV = "1")
  cat("Set LOCAL_DEV=1 (Dev mode)\n")
}

# 4. Launch the App
cat("\n🚀 Launching MADI Data Portal...\n")
cat("Open http://127.0.0.1:3838 in your browser.\n")
cat("Press Ctrl+C to stop.\n\n")

shiny::runApp('madi-shiny-front', port = 3838, launch.browser = TRUE)
