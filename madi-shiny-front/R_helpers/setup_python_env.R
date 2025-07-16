# R_helpers/setup_python_env.R (Modified Snippet to auto-install Miniconda)

# --- Configuration ---
python_env_name <- "project_py_env" # Name for the Conda environment
# Desired Python version for the Conda environment
desired_python_version <- "3.9" # Or "3.10", "3.11", etc.
required_py_packages <- c("psycopg2-binary", "python-dotenv")

# --- Script Logic ---
message("Starting Python environment setup for the R/Shiny project...")

if (!requireNamespace("reticulate", quietly = TRUE)) {
  message("Installing 'reticulate' R package...")
  install.packages("reticulate")
}
library(reticulate)

# Attempt to find a suitable Python or offer to install Miniconda
python_to_use <- NULL
conda_envs <- reticulate::conda_list()
# Try to get the python executable path from the conda_list dataframe
project_conda_entry <- subset(conda_envs, name == python_env_name)
project_conda_env_path <- if (nrow(project_conda_entry) > 0) project_conda_entry$python[1] else character(0)


if (length(project_conda_env_path) > 0 && nzchar(project_conda_env_path) && file.exists(project_conda_env_path)) {
    message(paste("Found existing Conda environment '", python_env_name, "' with Python at: ", project_conda_env_path, sep=""))
    python_to_use <- project_conda_env_path
} else {
    message(paste("Conda environment '", python_env_name, "' not found or Python path is invalid.", sep=""))
    # Check if Miniconda itself is installed by reticulate
    # reticulate::miniconda_path() gives the root of the Miniconda install
    if (!dir.exists(reticulate::miniconda_path()) || !file.exists(file.path(reticulate::miniconda_path(), "bin", "conda"))) { # More robust check for conda binary
        message("Miniconda managed by reticulate is not installed or is incomplete.")
        message("Attempting to install Miniconda locally for this R project automatically...") # MODIFIED PART
        tryCatch({
            reticulate::install_miniconda(force = TRUE) # Add force = TRUE if you want to ensure it tries to install even if remnants exist
            message("Miniconda installed successfully.")
        }, error = function(e) {
            stop(paste("Failed to install Miniconda. Error:", e$message,
                       "\nPlease ensure you have internet connectivity and write permissions.",
                       "You might need to install Python 3.7+ manually and ensure it's on the PATH."))
        })
    } else {
        message(paste("Miniconda managed by reticulate found at:", reticulate::miniconda_path()))
    }

    # Now that Miniconda should be available, create the project's Conda environment
    # Before creating, ensure the Miniconda path is valid and conda binary exists
    conda_binary_path <- reticulate::conda_binary()
    if (is.null(conda_binary_path) || !file.exists(conda_binary_path)) {
        stop(paste("Could not find conda binary after Miniconda installation/check. Path: ", ifelse(is.null(conda_binary_path), "NULL", conda_binary_path)))
    }
    message(paste("Using conda binary at:", conda_binary_path))

    message(paste("Creating Conda environment '", python_env_name, "' with Python ", desired_python_version, "...", sep=""))
    tryCatch({
        # conda_create will use the Python from the installed Miniconda
        reticulate::conda_create(envname = python_env_name, python_version = desired_python_version, conda = conda_binary_path)
        conda_envs_updated <- reticulate::conda_list(conda = conda_binary_path) # Refresh list
        
        new_env_entry <- subset(conda_envs_updated, name == python_env_name)
        python_to_use <- if (nrow(new_env_entry) > 0) new_env_entry$python[1] else character(0)

        if (length(python_to_use) == 0 || !nzchar(python_to_use) || !file.exists(python_to_use)) {
            stop(paste("Failed to create or find Python in Conda environment '", python_env_name, "' after creation attempt. Checked path: '", python_to_use, "'. Please check Conda setup.", sep=""))
        }
        message(paste("Conda environment '", python_env_name, "' created successfully with Python at: ", python_to_use, sep=""))
    }, error = function(e) {
        # If conda_create fails, provide more context if possible
        all_conda_envs <- reticulate::conda_list(conda = conda_binary_path) # Use the explicit conda binary
        message("Current list of conda environments after failed creation attempt:")
        print(all_conda_envs)
        stop(paste("Failed to create Conda environment '", python_env_name, "'. Error: ", e$message,
                   "\nCheck if Miniconda is correctly installed and working. Attempted to use conda at: '", conda_binary_path, "'.", sep=""))
    })
}

# At this point, 'python_to_use' should have the path to the Python in our project_py_env (Conda env)
# Ensure python_to_use is not NULL or empty before proceeding
if (is.null(python_to_use) || !nzchar(python_to_use) || !file.exists(python_to_use)) {
    stop(paste("Python executable path for environment '", python_env_name, "' was not properly set or found. Path: '", python_to_use, "'. Cannot install packages.", sep=""))
}


# 4. Install required Python packages into the Conda environment
message(paste("Installing/checking Python packages (", paste(required_py_packages, collapse=", "), ") in Conda environment '", python_env_name, "' using Python: ", python_to_use, "...", sep=""))
tryCatch({
  # Use conda_install for Conda environments
  reticulate::conda_install(envname = python_env_name, packages = required_py_packages, pip = TRUE, pip_options = "--no-cache-dir", conda = reticulate::conda_binary())
  message("Required Python packages are up to date in '", python_env_name, "'.")
}, error = function(e) {
  warning(paste("An error occurred while installing Python packages into Conda environment '", python_env_name, "'. Error: ", e$message, sep=""))
})

# 5. Final Instructions
message("\n--- Python Environment Setup (using Conda) Complete ---")
message(paste("Conda Environment Name: '", python_env_name, "' (managed by reticulate's Miniconda installation or existing Conda)", sep=""))
if (!is.null(python_to_use) && nzchar(python_to_use)) {
    message("Python executable: ", python_to_use)
} else {
    message("Python executable: Not found or not set (this is an issue).")
}
message("Remember to:")
message("  1. Create or verify the '.env' file at 'python_scripts/.env' with your local database credentials.")
message("  2. Add 'python_scripts/.env' to your .gitignore file.")
message("--------------------------------------")