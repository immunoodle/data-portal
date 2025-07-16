# R_helpers/initialize_reticulate_python.R

message("Initializing Python environment for R/Shiny session...")

# --- Configuration ---
# This should match the name used in setup_python_env.R
# It's assumed to be in the project root. If your CWD for Shiny is different, adjust path.
python_env_name <- "project_py_env" # This is now a Conda environment name


# Path to your main Python script, relative to the project root.
python_script_path <- "python_scripts/immport_template_generator.py"

source("R_helpers/setup_python_env.R")


# --- Script Logic ---


# 2. Check if the Python utility script exists
if (!file.exists(python_script_path)) {
  stop(
    paste0(
      "CRITICAL: Python utility script not found at: '", python_script_path, "'. ",
      "Please ensure the path is correct relative to your Shiny app's working directory (usually project root). ",
      "Current working directory: '", getwd(), "'."
    )
  )
}

conda_envs_list <- reticulate::conda_list()
project_conda_env_details <- subset(conda_envs_list, name == python_env_name)


# 3. Check if the Python virtual environment exists and use it
if (nrow(project_conda_env_details) > 0) {
  message(paste("Attempting to use Python Conda environment:", python_env_name))
  tryCatch({
    reticulate::use_condaenv(condaenv = python_env_name, required = TRUE) # Use use_condaenv
    config <- reticulate::py_config()
    message(paste("Successfully configured reticulate to use Python from Conda env:", config$python))
    # ... (rest of the module checks and script sourcing)
  }, error = function(e) {
    stop(paste0("Failed to activate Python Conda environment '", python_env_name, "'. Error: ", e$message))
  })
} else {
  stop(
    paste0(
      "CRITICAL: Python Conda environment '", python_env_name, "' not found. ",
      "Please run 'R_helpers/setup_python_env.R' from the project root to create and configure it."
    )
  )
}

# 4. Source the Python script to make its functions available in R
message(paste("Sourcing Python script from:", python_script_path))
tryCatch({
  reticulate::source_python(python_script_path)
  message("Python script sourced successfully. Python functions should now be available in R.")
  # Example: check if one of your Python functions is now available in the R global environment
  # if(exists("get_protocol_data_py") && is.function(get_protocol_data_py)) {
  #   message("'get_protocol_data_py' function is loaded.")
  # } else {
  #   warning("'get_protocol_data_py' function NOT loaded. Check Python script for errors or naming.")
  # }
}, error = function(e) {
  stop(paste0("CRITICAL: Failed to source Python script '", python_script_path, "'. Error: ", e$message,
             "\nCheck the Python script for syntax errors, issues with its imports, or problems during its initial execution (e.g., DB config errors printed by Python)."))
})

message("Python environment initialization complete.")