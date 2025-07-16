# immport_server_logic.R

# --- Server Logic (to be sourced inside the main server function) ---

# NOTE: This code assumes it runs INSIDE the main server function.
# It uses the main 'input', 'output', 'session' objects.
# It MUST use the manually prefixed IDs defined in immport_ui_def.R

# Reactive value to store generated file info (prefix name to avoid conflicts)'
staged_manual_files <- reactiveVal(NULL)

# Enhanced file management with individual add/remove capability
staged_files_list <- reactiveVal(list())

# --- Reactive Values ---
# Stores the retrieved authentication token
immport_auth_token <- reactiveVal(NULL)

# Stores generated file info (prefix name to avoid conflicts)
immport_generated_files_info <- reactiveVal(NULL)

# --- Reactive values for workspace management ---
rv_immport <- reactiveValues(
  selected_workspace_id = NULL
)


# --- ImmPort Authentication Logic ---

# Function to get user agent string
get_useragent <- function() {
  # Attempt to get ImmPortR version, handle error if package not installed
  immportr_version <- tryCatch(
     as.character(utils::packageVersion("ImmPortR")),
     error = function(e) "unknown"
   )

   paste0(
     "R/", R.version$major, ".", R.version$minor,
     " (", Sys.info()["sysname"], " ", Sys.info()["machine"], ")",
     " ImmPortR/", immportr_version
   )
}

# ImmPort login observer - FIXED
observeEvent(input$immport_login_button, {
  print("DEBUG: ImmPort login button clicked")
  
  # Clear previous token
  immport_auth_token(NULL)

  # Get credentials directly from UI inputs
  username <- input$immport_username
  password <- input$immport_password

  # Validation
  if (is.null(username) || nchar(username) == 0) {
    output$immport_login_status <- renderText({ "Username cannot be empty." })
    return()
  }
   if (is.null(password) || nchar(password) == 0) {
    output$immport_login_status <- renderText({ "Password cannot be empty." })
    return()
  }

  # Update status
  output$immport_login_status <- renderText({ "Attempting login..." })

  tryCatch({
    print("DEBUG: Making POST request to ImmPort auth endpoint")
    
    # Make the POST request
    response <- httr::POST(
      url = "https://auth.immport.org/auth/token",
      body = list(username = username, password = password),
      encode = "form",
      httr::config(useragent = get_useragent()),
      httr::timeout(30)
    )

    # Check response status
    status <- httr::status_code(response)
    print(paste("DEBUG: Response status:", status))

    if (status == 200) {
      # Success! Parse content
      content_parsed <- httr::content(response, as = "parsed", encoding = "UTF-8")
      access_token <- content_parsed$access_token

      if (!is.null(access_token)) {
        immport_auth_token(access_token) # Store the token
        print("DEBUG: Token stored successfully")

        # Display success message (hide token for security)
        output$immport_login_status <- renderText({
            paste("Login successful! Token acquired:", substr(access_token, 1, 8), "...")
        })

      } else {
        immport_auth_token(NULL)
        output$immport_login_status <- renderText({ "Login succeeded but access token not found in response." })
        print("DEBUG: No access token in response")
      }

    } else {
      # Error status code
      immport_auth_token(NULL)
      error_message <- paste("Login failed: Status", status)
      try({
        error_content <- httr::content(response, as = "parsed", encoding = "UTF-8")
        if (!is.null(error_content$error)) {
           error_message <- paste(error_message, "-", error_content$error)
        } else if (is.character(error_content)) {
           error_message <- paste(error_message, "-", substr(error_content, 1, 100))
        }
      }, silent = TRUE)
      output$immport_login_status <- renderText({ error_message })
      print(paste("DEBUG: Login failed:", error_message))
    }

  }, error = function(e) {
    # Handle lower-level connection errors
    immport_auth_token(NULL)
    output$immport_login_status <- renderText({ paste("Login error:", e$message) })
    print(paste("DEBUG: Login error:", e$message))
  })
})

# Function to fetch workspaces from ImmPort API
list_workspaces <- function(token) {
  print("DEBUG: Fetching workspaces from ImmPort API")
  url <- "https://immport-upload.niaid.nih.gov:9443/workspaces"

  response <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("bearer", token)),
    httr::timeout(30)
  )

  if (httr::status_code(response) == 200) {
    # Parse JSON response
    workspaces_json <- httr::content(response, as = "text", encoding = "UTF-8")
    workspaces_data <- jsonlite::fromJSON(workspaces_json)
    print(paste("DEBUG: Found", length(workspaces_data$workspaces), "workspaces"))
    return(workspaces_data$workspaces)
  } else {
    stop(paste("API request failed with status:", httr::status_code(response)))
  }
}

# Observer for the fetch workspaces button
observeEvent(input$immport_fetch_workspaces_button, {
  print("DEBUG: Fetch workspaces button clicked")
  output$immport_process_status <- renderText("Fetching workspaces... Please wait.")

  withProgress(message = 'Loading Workspaces...', {
    # Check authentication
    current_token <- immport_auth_token()
    if (is.null(current_token) || !nzchar(current_token)) {
      output$immport_process_status <- renderText("Please log in to ImmPort first.")
      updateSelectInput(session, "immport_workspace_select", choices = list("Login required" = ""))
      return()
    }

    # Fetch workspaces from API
    tryCatch({
      workspaces_data <- list_workspaces(token = current_token)

      if (!is.null(workspaces_data) && length(workspaces_data) > 0) {
        # Create choices for dropdown (display "ID - Name", value is just ID)
        workspace_choices <- setNames(
          workspaces_data$workspaceId,  # Values (just the numbers)
          paste(workspaces_data$workspaceId, "-", workspaces_data$name)  # Labels (ID - Name)
        )

        updateSelectInput(session, "immport_workspace_select",
                          label = "Select Workspace:",
                          choices = workspace_choices)

        output$immport_process_status <- renderText(paste(length(workspace_choices), "workspaces loaded."))

      } else {
        updateSelectInput(session, "immport_workspace_select",
                          choices = list("No workspaces found" = ""))
        output$immport_process_status <- renderText("No workspaces found.")
      }

    }, error = function(e) {
      print(paste("DEBUG: Error fetching workspaces:", e$message))
      output$immport_process_status <- renderText(paste("Error:", e$message))
      updateSelectInput(session, "immport_workspace_select",
                        choices = list("Error loading" = ""))
    })
  })
})

# Observer for workspace selection
observeEvent(input$immport_workspace_select, {
  if (!is.null(input$immport_workspace_select) && nzchar(input$immport_workspace_select)) {
    rv_immport$selected_workspace_id <- as.numeric(input$immport_workspace_select)
    
    # Get user's local workspace for comparison
    user_context <- get_validated_user_context()
    local_workspace <- if (user_context$valid) user_context$workspace_id else "unknown"
    
    if (rv_immport$selected_workspace_id != local_workspace) {
      output$immport_process_status <- renderText(
        paste("Selected ImmPort workspace:", rv_immport$selected_workspace_id, 
              "(Cross-workspace upload from your local workspace", local_workspace, ")")
      )
      print(paste("DEBUG: Cross-workspace upload enabled - Local:", local_workspace, "-> ImmPort:", rv_immport$selected_workspace_id))
    } else {
      output$immport_process_status <- renderText(
        paste("Selected ImmPort workspace:", rv_immport$selected_workspace_id, "(matches your local workspace)")
      )
      print(paste("DEBUG: Same workspace upload - Local & ImmPort:", rv_immport$selected_workspace_id))
    }
  }
})

# Logout observer
observeEvent(input$immport_logout_button, {
  # Clear the authentication token from reactive values
  immport_auth_token(NULL)

  # Reset the login status message
  output$immport_login_status <- renderText({ "Logged out. Please log in again." })

  # Clear the workspace dropdown as it's no longer valid
  updateSelectInput(session, "immport_workspace_select",
                    choices = list("Login required to load workspaces" = ""),
                    selected = "")

  showNotification("Session token has been cleared.", type = "message")
})

# Render the upload UI
output$immport_upload_ui <- renderUI({
  # Calls the UI function defined in immport_ui_def.R
  immport_upload_ui()
})

# --- Outputs ---
# Initial login status message
output$immport_login_status <- renderText({
  # Show token status if available, otherwise default message
  token <- immport_auth_token()
  if (!is.null(token)) {
    "Login successful (Token acquired)."
  } else {
    "Awaiting login..."
  }
})

output$immport_process_status <- renderText({ "System ready." })

# --- CRITICAL FIX: Enhanced upload observer with proper package posting ---
observeEvent(input$immport_zip_upload_button, {
  print("DEBUG: Upload button clicked")
  
  # 1. Validate user context
  user_context_data <- get_validated_user_context()
  if (!user_context_data$valid) {
    output$immport_process_status <- renderText({
      paste("Access denied:", user_context_data$message)
    })
    return()
  }

  # Get ImmPort context for ticket logging
  immport_context <- user_context()

  # 2. Get data to upload
  current_templates <- staged_templates_list()
  current_files <- staged_files_list()

  # 3. Check authentication token
  current_token <- immport_auth_token()
  if (is.null(current_token)) {
    output$immport_process_status <- renderText({"Please log in first."})
    return()
  }
  
  # 4. Check workspace selection
  selected_workspace_id <- rv_immport$selected_workspace_id
  if (is.null(selected_workspace_id)) {
    output$immport_process_status <- renderText({"Please select a workspace first."})
    return()
  }

  # 5. Check if we have data to upload
  has_templates <- length(current_templates) > 0
  has_files <- length(current_files) > 0

  if (!has_templates && !has_files) {
    output$immport_process_status <- renderText({"No data to upload. Please generate templates or add files first."})
    return()
  }

  output$immport_process_status <- renderText({
    paste("Preparing files for upload to ImmPort workspace", selected_workspace_id, "...")
  })

  # 6. Create zip file for upload
  temp_zip_dir <- tempdir()
  zip_filename <- paste0("ImmPort_Submission_WS", selected_workspace_id, "_", strftime(Sys.time(), "%Y%m%d%H%M%S"), ".zip")
  zip_filepath <- file.path(temp_zip_dir, zip_filename)
  temp_files_for_zip_dir <- file.path(temp_zip_dir, "submission_files")
  dir.create(temp_files_for_zip_dir, recursive = TRUE)
  on.exit(unlink(temp_files_for_zip_dir, recursive = TRUE), add = TRUE)

  all_files_to_zip_paths <- c()

  # 7. Process templates
  if (has_templates) {
    print(paste("DEBUG: Processing", length(current_templates), "templates for upload"))
    for (template_name in names(current_templates)) {
      template_entry <- current_templates[[template_name]]
      template_data <- template_entry$data
      
      if (!is.null(template_data)) {
        path <- file.path(temp_files_for_zip_dir, template_name)
        tryCatch({
          json_string <- jsonlite::toJSON(template_data, auto_unbox = TRUE, pretty = FALSE, null = "null")
          writeLines(json_string, path, useBytes = TRUE)
          all_files_to_zip_paths <- c(all_files_to_zip_paths, path)
          print(paste("DEBUG: Added template to package:", template_name))
        }, error = function(e) {
          output$immport_process_status <- renderText({paste("Error writing", template_name, ":", e$message)})
          return()
        })
      }
    }
  }

  # 8. Process files
  if (has_files) {
    print(paste("DEBUG: Processing", length(current_files), "files for upload"))
    for (file_id in names(current_files)) {
      file_info <- current_files[[file_id]]
      destination_path <- file.path(temp_files_for_zip_dir, file_info$name)
      
      tryCatch({
        file.copy(file_info$datapath, destination_path, overwrite = TRUE)
        all_files_to_zip_paths <- c(all_files_to_zip_paths, destination_path)
        print(paste("DEBUG: Added file to package:", file_info$name))
      }, error = function(e) {
        output$immport_process_status <- renderText({paste("Error copying", file_info$name, ":", e$message)})
        return()
      })
    }
  }

  if (length(all_files_to_zip_paths) == 0) {
    output$immport_process_status <- renderText({"Failed to prepare files for zipping. Please check data."})
    return()
  }

  # 9. Create zip package
  output$immport_process_status <- renderText({"Creating submission package..."})
  print(paste("DEBUG: Creating zip package with", length(all_files_to_zip_paths), "files"))
  print(paste("DEBUG: Files to zip:", paste(basename(all_files_to_zip_paths), collapse = ", ")))
  
  tryCatch({
    zip::zipr(zipfile = zip_filepath, files = all_files_to_zip_paths, root = temp_files_for_zip_dir)
    print(paste("DEBUG: Zip package created successfully:", zip_filepath))
    print(paste("DEBUG: Package size:", round(file.size(zip_filepath) / (1024^2), 2), "MB"))
  }, error = function(e_zip) {
    output$immport_process_status <- renderText({paste("Error during zipping:", e_zip$message)})
    return()
  })

  # 10. CRITICAL: Upload package to ImmPort API with ENHANCED DEBUGGING
  output$immport_process_status <- renderText({ paste("Uploading package", basename(zip_filepath), "to ImmPort workspace", selected_workspace_id, "...") })

  upload_url <- "https://immport-upload.niaid.nih.gov:8443/data/upload/type/online"
  
  # Enhanced upload notes with detailed information
  study_info <- if (!is.null(input$immport_study_accession) && nzchar(trimws(input$immport_study_accession))) {
    trimws(input$immport_study_accession)
  } else {
    "Unknown_Study"
  }
  
  upload_notes_value <- paste(
    "MADI Data Portal submission -",
    "User:", user_context_data$username,
    "| Study:", study_info,
    "| Templates:", length(current_templates),
    "| Files:", length(current_files),
    "| Local WS:", user_context_data$workspace_id,
    "| Target WS:", selected_workspace_id,
    "| Time:", strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  server_name_value <- Sys.info()[["nodename"]]

  # CRITICAL FIX: Ensure workspace ID is sent as STRING for form data
  workspace_id_string <- as.character(selected_workspace_id)
  package_name_string <- paste0("MADI_", study_info, "_", strftime(Sys.time(), "%Y%m%d_%H%M%S"))

  # Prepare multipart form data for API with explicit string conversion
  body_params <- list(
    workspaceId = workspace_id_string,    # ✅ FIXED: Explicitly convert to string
    packageName = package_name_string,    # ✅ Enhanced package name
    uploadNotes = upload_notes_value,
    uploadPurpose = "uploadData",         # ✅ Fixed value as per API spec
    serverName = server_name_value,
    file = httr::upload_file(zip_filepath)
  )

  # ENHANCED DEBUG OUTPUT
  print("DEBUG: *** IMMPORT API UPLOAD PARAMETERS ***")
  print(paste("DEBUG: Upload URL:", upload_url))
  print(paste("DEBUG: Workspace ID (original):", selected_workspace_id, "| Type:", typeof(selected_workspace_id)))
  print(paste("DEBUG: Workspace ID (string):", workspace_id_string, "| Type:", typeof(workspace_id_string)))
  print(paste("DEBUG: Package Name:", package_name_string))
  print(paste("DEBUG: Upload Purpose:", body_params$uploadPurpose))
  print(paste("DEBUG: Server Name:", server_name_value))
  print(paste("DEBUG: File size:", round(file.size(zip_filepath) / (1024^2), 2), "MB"))
  print(paste("DEBUG: Upload notes (first 100 chars):", substr(upload_notes_value, 1, 100), "..."))
  print(paste("DEBUG: Authorization token (first 20 chars):", substr(current_token, 1, 20), "..."))
  
  # Verify all required form parameters are present
  required_params <- c("workspaceId", "packageName", "uploadNotes", "uploadPurpose", "serverName", "file")
  missing_params <- required_params[!required_params %in% names(body_params)]
  if (length(missing_params) > 0) {
    print(paste("DEBUG: ERROR - Missing required parameters:", paste(missing_params, collapse = ", ")))
    output$immport_process_status <- renderText({paste("Missing required parameters:", paste(missing_params, collapse = ", "))})
    return()
  }
  print("DEBUG: All required parameters present ✅")

  print("DEBUG: POSTING PACKAGE TO IMMPORT API...")

  res <- NULL
  tryCatch({
    res <- httr::POST(
      url = upload_url,
      body = body_params,
      encode = "multipart",      # ✅ Correct encoding for form data + file upload
      httr::add_headers(
        Authorization = paste("bearer", current_token),  # ✅ Correct format
        "User-Agent" = get_useragent()
      ),
      httr::timeout(300),        # ✅ 5 minute timeout for large files
      httr::verbose(data_out = FALSE, data_in = FALSE, info = TRUE, ssl = FALSE)  # ✅ Enable verbose logging
    )
    
    print(paste("DEBUG: HTTP response status:", httr::status_code(res)))
    print("DEBUG: API POST completed successfully")
    
    # Enhanced response debugging
    response_headers <- httr::headers(res)
    print("DEBUG: Response headers:")
    for (header_name in names(response_headers)) {
      print(paste("  ", header_name, ":", response_headers[[header_name]]))
    }
    
  }, error = function(e) {
    print(paste("DEBUG: ERROR during API POST:", e$message))
    print(paste("DEBUG: Error class:", class(e)))
    print("DEBUG: Full error details:")
    print(e)
    output$immport_process_status <- renderText({
      paste("Error during package upload to ImmPort:", e$message)
    })
    return()
  })

  if (is.null(res)) {
    print("DEBUG: ERROR - No response received from API")
    output$immport_process_status <- renderText({"No response received from ImmPort API. Check network connection."})
    return()
  }

  # 11. Process API response and extract ticket with enhanced debugging
  status <- httr::status_code(res)
  print(paste("DEBUG: Processing API response with status:", status))
  
  # Get response content regardless of status for debugging
  response_content_raw <- httr::content(res, as = "text", encoding = "UTF-8")
  print(paste("DEBUG: Raw response content (first 500 chars):", substr(response_content_raw, 1, 500)))
  
  if (status == 200) {
    print("DEBUG: Upload successful! Processing response...")
    
    # Try to parse as JSON
    res_content <- NULL
    tryCatch({
      res_content <- httr::content(res, as = "parsed", encoding = "UTF-8")
      print("DEBUG: Successfully parsed response as JSON")
      print("DEBUG: Response content structure:")
      print(str(res_content))
    }, error = function(e) {
      print(paste("DEBUG: Failed to parse response as JSON:", e$message))
      print("DEBUG: Treating as plain text response")
      res_content <<- list(raw_text = response_content_raw)
    })

    # Extract ticket number from response with enhanced detection
    ticket_num <- NULL
    possible_fields <- c("ticketNumber", "uploadTicketNumber", "ticket_number", "ticket", "id", "registrationId")
    
    if (is.list(res_content)) {
      for (f in possible_fields) {
        if (!is.null(res_content[[f]])) { 
          ticket_num <- res_content[[f]]
          print(paste("DEBUG: Found ticket in field", f, ":", ticket_num))
          break 
        }
      }
    }
    
    # If no ticket found in parsed content, try regex on raw text
    if (is.null(ticket_num)) {
      print("DEBUG: No ticket found in parsed response, trying regex on raw text...")
      
      # Common ticket patterns
      ticket_patterns <- c(
        "ticketNumber[\"']?\\s*:\\s*[\"']?([A-Za-z0-9_-]+)",
        "uploadTicketNumber[\"']?\\s*:\\s*[\"']?([A-Za-z0-9_-]+)",
        "ticket[\"']?\\s*:\\s*[\"']?([A-Za-z0-9_-]+)",
        "registration[\"']?\\s*:\\s*[\"']?([A-Za-z0-9_-]+)",
        "([A-Z0-9]{10,})"  # Generic pattern for long alphanumeric strings
      )
      
      for (pattern in ticket_patterns) {
        matches <- regmatches(response_content_raw, regexec(pattern, response_content_raw, ignore.case = TRUE))
        if (length(matches[[1]]) > 1) {
          ticket_num <- matches[[1]][2]
          print(paste("DEBUG: Found ticket using regex pattern", pattern, ":", ticket_num))
          break
        }
      }
    }

    if(!is.null(ticket_num)) {
      output$immport_process_status <- renderText({
        paste("🎉 UPLOAD SUCCESSFUL! Ticket Number:", ticket_num, "| Workspace:", selected_workspace_id, "| Package:", basename(zip_filepath))
      })
      
      showNotification(
        paste("✅ Package uploaded to ImmPort!", 
              "Ticket:", ticket_num, 
              "| Workspace:", selected_workspace_id,
              "| Templates:", length(current_templates),
              "| Files:", length(current_files)), 
        type = "message", 
        duration = 20
      )
      
      print("DEBUG: *** UPLOAD SUCCESS SUMMARY ***")
      print(paste("DEBUG: ✅ Ticket Number:", ticket_num))
      print(paste("DEBUG: ✅ Target Workspace:", selected_workspace_id))
      print(paste("DEBUG: ✅ Package Name:", package_name_string))
      print(paste("DEBUG: ✅ Package File:", basename(zip_filepath)))
      print(paste("DEBUG: ✅ Templates Count:", length(current_templates)))
      print(paste("DEBUG: ✅ Files Count:", length(current_files)))
      print(paste("DEBUG: ✅ Upload Notes:", substr(upload_notes_value, 1, 100), "..."))
      print("DEBUG: *** END UPLOAD SUCCESS SUMMARY ***")

      # Log the ticket activity if we have ImmPort context
      if (immport_context$is_valid) {
        tryCatch({
          log_success <- log_ticket_activity_on_connection(
            db_connection = conn,
            username = immport_context$username,
            ticket_number = as.character(ticket_num),
            workspace_id = selected_workspace_id,
            action_type = "file_upload",
            file_details = paste("ZIP package:", basename(zip_filepath),
                                 "- Templates:", paste(names(current_templates), collapse = ", "),
                                 "- Files:", paste(sapply(current_files, function(f) f$name), collapse = ", ")),
            study_accession_id = study_info,
            notes = paste("SUCCESSFUL PACKAGE UPLOAD to ImmPort workspace", selected_workspace_id,
                          "- Package name:", package_name_string,
                          "- Upload notes:", upload_notes_value),
            status_summary_cache = paste("Package upload successful at", Sys.time(), "- Ticket:", ticket_num)
          )

          if (log_success) {
            # Trigger ticket list refresh
            current_trigger_val <- isolate(rv_tickets_trigger())
            rv_tickets_trigger(current_trigger_val + 1)
            print("DEBUG: Ticket logged and list refreshed")
          } else {
            warning("Failed to log successful upload activity to database")
          }
        }, error = function(e) {
          warning(paste("Error logging upload activity:", e$message))
        })
      }

    } else {
      print("DEBUG: Upload successful but no ticket number found in response")
      print("DEBUG: This might indicate an API change or unexpected response format")
      output$immport_process_status <- renderText({
        paste("✅ Package uploaded successfully to workspace", selected_workspace_id, 
              "but no ticket number detected. Response:", substr(response_content_raw, 1, 200), "...")
      })
      
      showNotification("Package uploaded but no ticket number received. Check ImmPort directly.", type = "warning", duration = 15)
    }

  } else {
    # Upload failed
    print(paste("DEBUG: Upload failed with HTTP status:", status))
    print(paste("DEBUG: Response content:", response_content_raw))
    
    # Try to extract error message
    error_message <- "Unknown error"
    tryCatch({
      error_content <- httr::content(res, as = "parsed", encoding = "UTF-8")
      if (is.list(error_content)) {
        # Common error fields
        error_fields <- c("error", "message", "errorMessage", "detail", "description")
        for (field in error_fields) {
          if (!is.null(error_content[[field]])) {
            error_message <- error_content[[field]]
            break
          }
        }
      }
    }, error = function(e) {
      error_message <- substr(response_content_raw, 1, 300)
    })
    
    output$immport_process_status <- renderText({
      paste("❌ Upload failed. HTTP Status:", status, "| Error:", error_message)
    })
    
    showNotification(
      paste("Upload failed with status", status, "| Error:", substr(error_message, 1, 100)), 
      type = "error", 
      duration = 20
    )
    
    print("DEBUG: *** UPLOAD FAILURE SUMMARY ***")
    print(paste("DEBUG: ❌ HTTP Status:", status))
    print(paste("DEBUG: ❌ Target Workspace:", selected_workspace_id))
    print(paste("DEBUG: ❌ Package Name:", package_name_string))
    print(paste("DEBUG: ❌ Error Message:", error_message))
    print(paste("DEBUG: ❌ Raw Response:", substr(response_content_raw, 1, 500)))
    print("DEBUG: *** END UPLOAD FAILURE SUMMARY ***")
  }
})

# Handle single file uploads and add to the list
observeEvent(input$immport_single_file_upload, {
  req(input$immport_single_file_upload)
  
  new_file <- input$immport_single_file_upload
  current_files <- staged_files_list()
  
  # Create unique ID for this file
  file_id <- paste0("file_", length(current_files) + 1, "_", gsub("[^A-Za-z0-9]", "", Sys.time()))
  
  # Add the new file to the list
  new_file_entry <- list(
    id = file_id,
    name = new_file$name,
    size = new_file$size,
    type = new_file$type,
    datapath = new_file$datapath,
    added_time = Sys.time()
  )
  
  current_files[[file_id]] <- new_file_entry
  staged_files_list(current_files)
  
  # Reset the file input for next upload
  shinyjs::reset("immport_single_file_upload")
  
  showNotification(paste("Added file:", new_file$name), type = "message", duration = 3)
})

# Dynamic UI for file list with remove buttons
output$immport_dynamic_file_list <- renderUI({
  files <- staged_files_list()
  
  if (length(files) == 0) {
    return(div(
      style = "text-align: center; color: #6c757d; padding: 20px; font-style: italic;",
      "📁 No files staged yet",
      br(),
      "Use the 'Browse & Add...' button above to add files"
    ))
  }
  
  # Create a div for each file with remove button
  file_divs <- lapply(names(files), function(file_id) {
    file_info <- files[[file_id]]
    
    # Format file size
    size_mb <- round(file_info$size / (1024^2), 2)
    size_display <- if (size_mb >= 1) {
      paste0(size_mb, " MB")
    } else {
      paste0(round(file_info$size / 1024, 1), " KB")
    }
    
    div(
      style = "display: flex; align-items: center; justify-content: space-between; padding: 8px 12px; margin: 5px 0; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; transition: all 0.2s ease;",
      onmouseover = "this.style.backgroundColor='#e9ecef'; this.style.borderColor='#adb5bd';",
      onmouseout = "this.style.backgroundColor='#f8f9fa'; this.style.borderColor='#dee2e6';",
      
      # File info section
      div(
        style = "flex: 1; min-width: 0;", # min-width: 0 allows text to truncate
        div(
          style = "font-weight: 600; color: #495057; font-size: 0.95em; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
          title = file_info$name, # Tooltip shows full name
          "📄 ", file_info$name
        ),
        div(
          style = "font-size: 0.8em; color: #6c757d; margin-top: 2px;",
          paste("Size:", size_display, "•", "Type:", file_info$type %||% "Unknown")
        )
      ),
      
      # Remove button
      div(
        style = "margin-left: 10px; flex-shrink: 0;",
        actionButton(
          inputId = paste0("remove_file_", file_id),
          label = NULL,
          icon = icon("times", class = "fa-sm"),
          class = "btn-outline-danger btn-sm",
          style = "padding: 4px 8px; border-radius: 4px; font-size: 0.8em;",
          title = paste("Remove", file_info$name),
          onclick = paste0("this.style.transform='scale(0.95)'; setTimeout(() => this.style.transform='scale(1)', 100);")
        )
      )
    )
  })
  
  do.call(tagList, file_divs)
})

# Handle file removal with reduced notifications
observe({
  files <- staged_files_list()
  
  # Create observers for each remove button
  lapply(names(files), function(file_id) {
    button_id <- paste0("remove_file_", file_id)
    
    observeEvent(input[[button_id]], {
      current_files <- staged_files_list()
      file_name <- current_files[[file_id]]$name
      
      # Remove the file from the list
      current_files[[file_id]] <- NULL
      staged_files_list(current_files)
      
      # Only show one notification, not multiple
      showNotification(paste("Removed:", file_name), type = "warning", duration = 2)
    }, ignoreInit = TRUE, once = TRUE)  # Add once = TRUE to prevent multiple observers
  })
})

# File summary output
output$immport_file_summary <- renderUI({
  files <- staged_files_list()
  
  if (length(files) == 0) {
    return(span("No files staged", style = "color: #6c757d;"))
  }
  
  total_size <- sum(sapply(files, function(f) f$size))
  total_size_mb <- round(total_size / (1024^2), 2)
  
  size_display <- if (total_size_mb >= 1) {
    paste0(total_size_mb, " MB")
  } else {
    paste0(round(total_size / 1024, 1), " KB")
  }
  
  div(
    span(
      style = "font-weight: 600; color: #495057;",
      paste(length(files), if(length(files) == 1) "file" else "files", "staged")
    ),
    " • ",
    span(
      style = "color: #6c757d;",
      "Total size:", size_display
    )
  )
})

# **CRITICAL MISSING FUNCTIONS - FROM OLD FILE**

# Enhanced user context with workspace validation
get_validated_user_context <- function() {
  context <- user_data()  # Get current user data
  
  # Check if user is authenticated
  if (is.null(context) || !isTRUE(context$is_authenticated)) {
    return(list(
      valid = FALSE,
      message = "User not authenticated",
      workspace_id = NULL,
      username = NULL
    ))
  }
  
  # Get user's workspace from database
  tryCatch({
    user_query <- "SELECT workspace_id, username FROM madi_track.users WHERE email = $1 AND is_active = true"
    user_result <- DBI::dbGetQuery(conn, user_query, params = list(context$email))
    
    if (nrow(user_result) > 0) {
      return(list(
        valid = TRUE,
        message = "User context validated",
        workspace_id = user_result$workspace_id[1],
        username = user_result$username[1],
        email = context$email
      ))
    } else {
      return(list(
        valid = FALSE,
        message = "User not found or inactive in database",
        workspace_id = NULL,
        username = NULL
      ))
    }
    
  }, error = function(e) {
    warning(paste("Error getting user context:", e$message))
    return(list(
      valid = FALSE,
      message = paste("Database error:", e$message),
      workspace_id = NULL,
      username = NULL
    ))
  })
}

# Enhanced study validation - only check study exists, not workspace restriction for ImmPort uploads
validate_study_access <- function(study_accession, user_workspace_id, db_conn) {
  # Input validation
  if (is.null(study_accession) || !is.character(study_accession) || nchar(trimws(study_accession)) == 0) {
    return(list(valid = FALSE, message = "Study accession is required."))
  }
  
  if (is.null(user_workspace_id) || !is.numeric(user_workspace_id)) {
    return(list(valid = FALSE, message = "Valid workspace ID is required."))
  }
  
  if (is.null(db_conn) || !DBI::dbIsValid(db_conn)) {
    return(list(valid = FALSE, message = "Database connection is invalid."))
  }
  
  tryCatch({
    # First, check if study exists in user's local data portal workspace
    query <- "SELECT study_accession, brief_title, workspace_id 
              FROM madi_dat.study 
              WHERE study_accession = $1 AND workspace_id = $2"
    
    result <- DBI::dbGetQuery(db_conn, query, 
                              params = list(trimws(study_accession), 
                                          as.integer(user_workspace_id)))
    
    if (nrow(result) > 0) {
      return(list(
        valid = TRUE, 
        message = paste("Study access validated:", result$brief_title[1]),
        study_info = result[1, ]
      ))
    } else {
      # For ImmPort uploads, allow any study that exists in the database
      # Users might be uploading studies from other workspaces to ImmPort
      check_exists_query <- "SELECT study_accession, brief_title, workspace_id FROM madi_dat.study WHERE study_accession = $1 LIMIT 1"
      exists_result <- DBI::dbGetQuery(db_conn, check_exists_query, params = list(trimws(study_accession)))
      
      if (nrow(exists_result) > 0) {
        print(paste("DEBUG: Study", trimws(study_accession), "found in workspace", exists_result$workspace_id[1], "- allowing ImmPort upload"))
        return(list(
          valid = TRUE, 
          message = paste("Study found:", exists_result$brief_title[1], "- ImmPort upload allowed"),
          study_info = exists_result[1, ]
        ))
      } else {
        return(list(
          valid = FALSE, 
          message = paste("Study", trimws(study_accession), "does not exist in the database")
        ))
      }
    }
    
  }, error = function(e) {
    warning(paste("Database error in validate_study_access:", e$message))
    return(list(
      valid = FALSE, 
      message = paste("Error validating study access:", e$message)
    ))
  })
}

# Enhanced template management with dynamic selection
staged_templates_list <- reactiveVal(list())

# Replace the existing generate button observer with enhanced template management
observeEvent(input$immport_generate_button, {
  # 1. Validate user context first
  user_context <- get_validated_user_context()
  if (!user_context$valid) {
    showModal(modalDialog(
      title = "Access Denied", 
      paste("Authentication required:", user_context$message),
      easyClose = TRUE
    ))
    return()
  }
  
  # 2. Get inputs from UI
  current_study_accession <- trimws(input$immport_study_accession)
  selected_file_numbers <- input$immport_template_select

  # 3. Basic R-side validation
  if (is.null(current_study_accession) || nchar(current_study_accession) == 0) {
    showModal(modalDialog(title = "Input Missing", "Please enter a Study Accession ID.", easyClose = TRUE))
    return()
  }
  if (is.null(selected_file_numbers) || length(selected_file_numbers) == 0) {
    showModal(modalDialog(title = "Input Missing", "Please select at least one template to generate.", easyClose = TRUE))
    return()
  }

  # 4. CRITICAL: Validate study access in user's workspace
  study_validation <- validate_study_access(
    study_accession = current_study_accession,
    user_workspace_id = user_context$workspace_id,
    db_conn = conn
  )
  
  if (!study_validation$valid) {
    showModal(modalDialog(
      title = "Study Access Denied", 
      div(
        p(strong("Access to this study is not authorized.")),
        p(study_validation$message),
        p(paste("Your workspace ID:", user_context$workspace_id)),
        p("Please contact your administrator if you believe this is an error.")
      ),
      easyClose = TRUE
    ))
    return()
  }

  message(paste("Generate button clicked. Study:", current_study_accession,
                "| Workspace:", user_context$workspace_id,
                "| User:", user_context$username,
                "| Selected file numbers:", paste(selected_file_numbers, collapse = ", ")))

  # Show notification with study validation success
  id <- showNotification(
    paste("Processing validated study:", study_validation$study_info$brief_title, "..."), 
    type = "message", 
    duration = NULL
  )
  on.exit(removeNotification(id), add = TRUE)

  # 5. Call the Python function
  python_output_list <- NULL
  tryCatch({
    python_output_list <- process_and_return_data_py(
      study_accession_str = current_study_accession,
      file_numbers_list_str = as.list(selected_file_numbers)
    )
  }, error = function(e) {
    message(paste("R Error: Call to Python function 'process_and_return_data_py' failed:", e$message))
    showNotification(paste("Error communicating with Python:", e$message), type = "error", duration = 10)
    return()
  })

  # 6. Process Python output and add to staged templates
  if (!is.null(python_output_list)) {
    message("Received a result from Python.")
    
    if (!is.null(python_output_list$error_summary)) {
      message(paste("Python reported a summary error:", python_output_list$error_summary))
      showNotification(paste("Python processing error:", python_output_list$error_summary), type = "error", duration = 10)
    } else if (!is.null(python_output_list$generated_data)) {
      # Add new templates to staged list with conflict checking
      current_templates <- staged_templates_list()
      new_templates <- python_output_list$generated_data
      conflicts <- c()
      
      # Check for conflicts (existing templates with same name)
      for (template_name in names(new_templates)) {
        if (template_name %in% names(current_templates)) {
          conflicts <- c(conflicts, template_name)
        }
      }
      
      if (length(conflicts) > 0) {
        # Show confirmation dialog for overwrites
        showModal(modalDialog(
          title = "Template Conflicts Detected",
          div(
            p(strong("The following templates already exist and will be overwritten:")),
            tags$ul(lapply(conflicts, function(x) tags$li(x))),
            p("Do you want to proceed? This will replace the existing templates with new versions.")
          ),
          footer = tagList(
            actionButton("confirm_overwrite", "Yes, Overwrite", class = "btn-warning"),
            modalButton("Cancel")
          ),
          easyClose = FALSE
        ))
        
        # Store the new templates temporarily for the confirmation handler
        session$userData$pending_templates <- new_templates
        session$userData$current_study <- current_study_accession
        return()
      } else {
        # No conflicts, add directly
        add_templates_to_staged(new_templates, current_study_accession)
      }
    } else {
      message("Python output did not contain 'generated_data' as expected.")
      showNotification("Unexpected data structure from Python.", type = "warning", duration = 7)
    }
  } else {
    message("Python function call returned NULL to R.")
    showNotification("No valid response received from Python.", type = "error", duration = 7)
  }
})

# Function to add templates to staged list
add_templates_to_staged <- function(new_templates, study_accession) {
  current_templates <- staged_templates_list()
  
  # Add templates with metadata
  for (template_name in names(new_templates)) {
    template_id <- paste0("template_", length(current_templates) + 1, "_", gsub("[^A-Za-z0-9]", "", Sys.time()))
    
    current_templates[[template_name]] <- list(
      id = template_id,
      name = template_name,
      data = new_templates[[template_name]],
      study = study_accession,
      created_time = Sys.time(),
      source = "generated"
    )
  }
  
  staged_templates_list(current_templates)
  
  showNotification(
    paste("Added", length(new_templates), "templates for study:", study_accession), 
    type = "message", 
    duration = 4
  )
}

# Replace the rv_results observer with staged templates management
observeEvent(staged_templates_list(), {
  current_templates <- staged_templates_list()
  current_viewer_selection <- isolate(input$immport_template_viewer_select)

  if (!is.null(current_templates) && length(current_templates) > 0) {
    template_names <- names(current_templates)
    choices_list <- setNames(as.list(template_names), template_names)

    selected_choice <- if (!is.null(current_viewer_selection) &&
                           nzchar(current_viewer_selection) &&
                           current_viewer_selection %in% template_names) {
      current_viewer_selection
    } else {
      template_names[1]
    }

    updateSelectInput(session, "immport_template_viewer_select",
                      label = "Select Template to View/Edit:",
                      choices = choices_list,
                      selected = selected_choice)
    message(paste("Updated template viewer. Available templates:", length(template_names), "| Selected:", selected_choice))
  } else {
    updateSelectInput(session, "immport_template_viewer_select",
                      label = "Select Template to View/Edit:",
                      choices = list("No templates staged yet" = ""),
                      selected = "")
    message("Template viewer reset - no templates available.")
  }
}, ignoreNULL = FALSE, ignoreInit = TRUE)

# Update the ACE editor content observer
observe({
  selected_filename <- input$immport_template_viewer_select
  current_templates <- staged_templates_list()

  req(selected_filename, nzchar(selected_filename), current_templates)

  if (selected_filename == "" || !(selected_filename %in% names(current_templates))) {
    shinyAce::updateAceEditor(session, "immport_json_editor",
                              value = paste("Select a valid template from the dropdown.\nCurrently selected:", selected_filename))
    return()
  }

  template_entry <- current_templates[[selected_filename]]
  data_to_edit <- template_entry$data

  if (!is.null(data_to_edit)) {
    tryCatch({
      json_string_for_editor <- jsonlite::toJSON(data_to_edit, auto_unbox = TRUE, pretty = TRUE, null = "null")
      shinyAce::updateAceEditor(session, "immport_json_editor", value = json_string_for_editor)
      message(paste("Loaded", selected_filename, "into ACE editor."))
    }, error = function(e) {
      error_msg_for_editor <- paste("Error preparing JSON for editor:", e$message,
                                     "\n\nCannot display for editing.",
                                     "\nRaw R structure:\n",
                                     paste(capture.output(str(data_to_edit)), collapse="\n"))
      shinyAce::updateAceEditor(session, "immport_json_editor", value = error_msg_for_editor)
      message(error_msg_for_editor)
    })
  } else {
    shinyAce::updateAceEditor(session, "immport_json_editor",
                              value = paste("No data (NULL) for selected template:", selected_filename))
    message(paste("Data for", selected_filename, "is NULL."))
  }
})

# Update the save changes observer
observeEvent(input$immport_save_json_changes_btn, {
  selected_filename_to_update <- isolate(input$immport_template_viewer_select)
  edited_json_string <- input$immport_json_editor

  req(selected_filename_to_update, nzchar(selected_filename_to_update), edited_json_string)

  current_templates <- staged_templates_list()
  
  if (selected_filename_to_update == "" || !(selected_filename_to_update %in% names(current_templates))) {
      showNotification("No valid template selected to save changes for.", type = "warning")
      return()
  }

  message(paste("Attempting to save changes for:", selected_filename_to_update))

  parsed_r_list <- NULL
  tryCatch({
    parsed_r_list <- jsonlite::fromJSON(edited_json_string, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  }, error = function(e) {
    showNotification(paste("Invalid JSON format in editor:", e$message), type = "error", duration = 10)
    message(paste("Error parsing JSON from ACE editor:", e$message))
    return()
  })

  if (!is.null(parsed_r_list)) {
    # Update the template data in staged list
    current_templates[[selected_filename_to_update]]$data <- parsed_r_list
    current_templates[[selected_filename_to_update]]$modified_time <- Sys.time()
    staged_templates_list(current_templates)

    showNotification(paste("Template '", selected_filename_to_update, "' updated successfully!"),
                     type = "message", duration = 3)
    message(paste("Updated staged template for", selected_filename_to_update))
  }
})

# Add missing ticket functionality back
rv_tickets_trigger <- reactiveVal(0)

# Function to extract username from JWT bearer token (if not already defined)
if (!exists("get_username_from_bearer_token")) {
  get_username_from_bearer_token <- function(bearer_token) {
    tryCatch({
      # Remove "bearer " prefix if present
      token <- gsub("^bearer\\s+", "", bearer_token, ignore.case = TRUE)

      # Split JWT into parts
      parts <- strsplit(token, "\\.")[[1]]

      if (length(parts) != 3) {
        return(NULL)
      }

      # Get payload (middle part)
      payload_encoded <- parts[2]

      # Add padding for base64 decoding
      padding_needed <- 4 - (nchar(payload_encoded) %% 4)
      if (padding_needed != 4) {
        payload_encoded <- paste0(payload_encoded, paste(rep("=", padding_needed), collapse = ""))
      }

      # Decode and parse
      payload_decoded <- rawToChar(base64enc::base64decode(payload_encoded))
      payload_json <- jsonlite::fromJSON(payload_decoded)

      # Return username from 'sub' field
      return(payload_json$sub)

    }, error = function(e) {
      return(NULL)
    })
  }
}

# Database logging function for ticket activities
log_ticket_activity_on_connection <- function(db_connection,
                                              username,
                                              ticket_number,
                                              workspace_id,
                                              action_type,
                                              file_details = NULL,
                                              study_accession_id = NULL,
                                              notes = NULL,
                                              status_summary_cache = NULL) {

  # Validate essential inputs
  if (missing(db_connection) || !inherits(db_connection, "DBIConnection")) {
    stop("A valid 'db_connection' (DBIConnection object) is required.")
  }
  if (!DBI::dbIsValid(db_connection)) {
    stop("The provided 'db_connection' is not valid or has been closed.")
  }
  if (missing(username) || !is.character(username) || nchar(username) == 0) {
    stop("'username' is required and must be a non-empty character string.")
  }
  if (missing(ticket_number) || !is.character(ticket_number) || nchar(ticket_number) == 0) {
    stop("'ticket_number' is required and must be a non-empty character string.")
  }
  if (missing(workspace_id) || (!is.numeric(workspace_id) && !is.null(workspace_id))) {
    stop("'workspace_id' is required and must be a numeric value or NULL.")
  }
  if (missing(action_type) || !is.character(action_type) || nchar(action_type) == 0) {
    stop("'action_type' is required and must be a non-empty character string.")
  }

  # Handle NULL values
  file_details <- if(is.null(file_details)) NA_character_ else as.character(file_details)
  study_accession_id <- if(is.null(study_accession_id)) NA_character_ else as.character(study_accession_id)
  notes <- if(is.null(notes)) NA_character_ else as.character(notes)
  status_summary_cache <- if(is.null(status_summary_cache)) NA_character_ else as.character(status_summary_cache)

  tryCatch({
    # 1. Check if ticket exists
    check_ticket_sql <- "SELECT ticket_id FROM madi_dat.user_tickets WHERE username = $1 AND ticket_number = $2 AND workspace = $3"
    existing_ticket <- RPostgres::dbGetQuery(db_connection, check_ticket_sql,
                                             params = list(username, ticket_number, workspace_id))

    if (nrow(existing_ticket) == 0) {
      # Insert new ticket
      insert_ticket_sql <- "INSERT INTO madi_dat.user_tickets (username, ticket_number, workspace, updated_at, status) VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active')"
      RPostgres::dbExecute(db_connection, insert_ticket_sql,
                           params = list(username, ticket_number, workspace_id))

      # Get the newly created ticket_id
      current_ticket_id <- RPostgres::dbGetQuery(db_connection, check_ticket_sql,
                                                 params = list(username, ticket_number, workspace_id))$ticket_id[1]
    } else {
      # Update existing ticket timestamp
      current_ticket_id <- existing_ticket$ticket_id[1]
      update_ticket_sql <- "UPDATE madi_dat.user_tickets SET updated_at = CURRENT_TIMESTAMP WHERE ticket_id = $1"
      RPostgres::dbExecute(db_connection, update_ticket_sql,
                           params = list(current_ticket_id))
    }

    # 2. Log the action
    log_action_sql <- "INSERT INTO madi_dat.ticket_actions (username, ticket_number, workspace_id, action_type, file_details, study_accession_id, notes, status_summary) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"

    RPostgres::dbExecute(db_connection, log_action_sql,
                         params = list(username,
                                       ticket_number,
                                       workspace_id,
                                       action_type,
                                       file_details,
                                       study_accession_id,
                                       notes,
                                       status_summary_cache))

    return(TRUE)

  }, error = function(e) {
    warning(paste0("Database error in log_ticket_activity_on_connection for user '", username,
                   "', ticket '", ticket_number, "', workspace '", workspace_id,
                   "', action '", action_type, "': ", conditionMessage(e)))
    return(FALSE)
  })
}

# Function to validate user and workspace in database
validate_user_and_workspace_in_db <- function(username, workspace_id, db_conn) {
  # Basic input validation
  if (is.null(username) || !is.character(username) || nchar(username) == 0) {
    return(FALSE)
  }
  if (is.null(workspace_id) || !is.numeric(workspace_id) || is.na(workspace_id)) {
    return(FALSE)
  }

  if (is.null(db_conn) || !DBI::dbIsValid(db_conn)) {
    warning("validate_user_and_workspace_in_db: Database connection is NULL or not valid.")
    return(FALSE)
  }

  tryCatch({
    # Construct the query using glue_sql for safe parameterization
    query <- glue::glue_sql(
      "SELECT EXISTS (SELECT 1 FROM madi_dat.user_tickets WHERE username = {username_val} AND workspace = {workspace_id_val} LIMIT 1);",
      username_val = username,
      workspace_id_val = as.integer(workspace_id),
      .con = db_conn
    )

    result_df <- DBI::dbGetQuery(db_conn, query)

    if (nrow(result_df) == 1 && !is.null(result_df[[1]]) && result_df[[1]] == TRUE) {
      return(TRUE)
    } else {
      return(FALSE)
    }

  }, error = function(e) {
    warning(paste("Database error in validate_user_and_workspace_in_db for user '", username, "' and workspace '", workspace_id, "': ", e$message, sep=""))
    return(FALSE)
  })
}

# Enhanced user context function for ticket system
user_context <- reactive({
  token <- immport_auth_token() # Your reactive bearer token
  current_workspace_id <- rv_immport$selected_workspace_id

  # Initial checks
  if (is.null(token) || nchar(token) == 0) {
    return(list(username = NULL, token = NULL, workspace_id = current_workspace_id, is_valid = FALSE, message = "Bearer token not available."))
  }

  # Check if a workspace is selected
  if (is.null(current_workspace_id)) {
    return(list(username = NULL, token = token, workspace_id = NULL, is_valid = FALSE, message = "Workspace not selected."))
  }

  # Get username from token
  username <- get_username_from_bearer_token(token)
  if (is.null(username)) {
    return(list(username = NULL, token = token, workspace_id = current_workspace_id, is_valid = FALSE, message = "Failed to derive username from token."))
  }

  # Validate username and workspace_id against the database (for ticket system only)
  is_combo_valid <- validate_user_and_workspace_in_db(
    username = username,
    workspace_id = current_workspace_id,
    db_conn = conn
  )

  if (!is_combo_valid) {
    # For ImmPort uploads, we still allow the action but note the validation failure
    print(paste("DEBUG: Ticket validation failed for user", username, "in workspace", current_workspace_id, "but allowing ImmPort operations"))
  }

  # Return valid context regardless of ticket validation for ImmPort operations
  return(list(
    username = username,
    token = token,
    workspace_id = current_workspace_id,
    is_valid = TRUE,
    message = "User context is valid.",
    ticket_validation_passed = is_combo_valid
  ))
})

# Function to fetch tickets for user/workspace from database
fetch_tickets_for_user_workspace_from_db <- function(username, workspace_id, db_conn) {
  if (is.null(username) || !nzchar(username) || is.null(workspace_id) || !is.numeric(workspace_id)) {
    return(list("Invalid user/workspace for ticket query" = ""))
  }
  if (is.null(db_conn) || !DBI::dbIsValid(db_conn)) {
    warning("DB connection invalid in fetch_tickets_for_user_workspace_from_db")
    return(list("Database connection error" = ""))
  }
  tryCatch({
    query <- glue::glue_sql(
      "SELECT ticket_number FROM madi_dat.user_tickets WHERE username = {uname} AND workspace = {ws_id} ORDER BY updated_at DESC, ticket_number DESC;",
      uname = username, ws_id = as.integer(workspace_id), .con = db_conn)
    ticket_data <- DBI::dbGetQuery(db_conn, query)
    if (nrow(ticket_data) > 0) {
      setNames(as.character(ticket_data$ticket_number), paste("Ticket:", as.character(ticket_data$ticket_number)))
    } else {
      list("No tickets found for this user/workspace" = "")
    }
  }, error = function(e) {
    warning(paste("DB error in fetch_tickets_for_user_workspace_from_db:", e$message))
    list("Error fetching tickets from DB" = "")
  })
}

# App state for ticket system
app_state <- reactiveValues(
  current_ticket_status = "Please authenticate and select a workspace.",
  current_ticket_summary = "",
  historical_ticket_status_log = "",
  historical_ticket_summary_log = "",
  display_mode_status = "current",
  display_mode_summary = "current"
)

# Reactive for ticket choices
ticket_choices_r <- reactive({
  rv_tickets_trigger()
  context <- user_context()

  if (context$is_valid && context$ticket_validation_passed) {
    actual_choices <- fetch_tickets_for_user_workspace_from_db(
      username = context$username,
      workspace_id = context$workspace_id,
      db_conn = conn
    )
    return(list(
      choices = actual_choices,
      is_valid_context = TRUE,
      username = context$username,
      workspace_id = context$workspace_id,
      message = "Tickets loaded."
    ))
  } else {
    message_for_dropdown <- if (!is.null(context$message) && nzchar(context$message)) {
      if (context$is_valid && !context$ticket_validation_passed) {
        "No tickets found - upload files to create tickets"
      } else {
        context$message
      }
    } else {
      "Authentication or workspace selection required."
    }
    return(list(
      choices = setNames("", message_for_dropdown),
      is_valid_context = FALSE,
      message = message_for_dropdown
    ))
  }
})

# Observer to update ticket dropdown
observe({
  req(ticket_choices_r())
  ticket_data <- ticket_choices_r()
  previous_selected_ticket <- isolate(input$immport_ticket_select)

  updateSelectInput(
    session,
    "immport_ticket_select",
    choices = ticket_data$choices,
    selected = {
      new_ticket_values <- unlist(ticket_data$choices)

      if (ticket_data$is_valid_context) {
        if (!is.null(previous_selected_ticket) && previous_selected_ticket %in% new_ticket_values) {
          previous_selected_ticket
        } else if (length(new_ticket_values) > 0 && nzchar(new_ticket_values[1])) {
          if (names(ticket_data$choices)[1] != "" && !grepl("No tickets found|Invalid|Error", names(ticket_data$choices)[1], ignore.case=TRUE)){
            new_ticket_values[1]
          } else {
            ""
          }
        } else {
          ""
        }
      } else {
        ""
      }
    }
  )

  # Reset app_state for display outputs based on context validity
  if (!ticket_data$is_valid_context) {
    app_state$current_ticket_status <- ticket_data$message %||% "Context not valid."
    app_state$current_ticket_summary <- ""
    app_state$historical_ticket_status_log <- ""
    app_state$historical_ticket_summary_log <- ""
    app_state$display_mode_status <- "current"
    app_state$display_mode_summary <- "current"
  } else {
    app_state$current_ticket_status <- "Select a ticket and request current status."
    app_state$current_ticket_summary <- "Select a ticket and request current summary."
    app_state$historical_ticket_status_log <- "No status history loaded or available."
    app_state$historical_ticket_summary_log <- "No summary history loaded or available."
    app_state$display_mode_status <- "current"
    app_state$display_mode_summary <- "current"
  }
})

# Helper for null coalescing
'%||%' <- function(a, b) if (!is.null(a)) a else b

# Request Current Ticket Status (via API)
observeEvent(input$immport_request_ticket_status_btn, {
  withProgress(message = 'Requesting ticket status...', detail = 'This may take a moment.', {
    context <- user_context()

    req(context$is_valid, message = "User context is not valid. Please authenticate and select a workspace.")
    req(input$immport_ticket_select, input$immport_ticket_select != "", message = "Please select a ticket first.")

    selected_ticket_number <- input$immport_ticket_select

    api_url <- paste0("https://immport-upload.niaid.nih.gov:8443/data/upload/registration/",
                      URLencode(selected_ticket_number, reserved = TRUE),
                      "/status")

    api_response_content <- "Error: Could not fetch status from API."

    tryCatch({
      response <- httr::GET(
        url = api_url,
        httr::add_headers(Authorization = paste("Bearer", context$token)),
        httr::timeout(15)
      )

      response_text <- httr::content(response, "text", encoding = "UTF-8")

      if (httr::status_code(response) == 200) {
        api_response_content <- response_text
        showNotification(paste("Status fetched for ticket:", selected_ticket_number), type = "message")
      } else {
        api_response_content <- paste("API Error (Status ", httr::status_code(response), "): ", response_text, sep="")
        showNotification(paste("Failed to fetch status for", selected_ticket_number), type = "error", duration = 10)
      }
    }, error = function(e) {
      api_response_content <- paste("R Error during API call for status:", e$message)
      warning(api_response_content)
      showNotification("An R error occurred while trying to fetch status.", type = "error", duration = 10)
    })

    app_state$current_ticket_status <- api_response_content
    app_state$display_mode_status <- "current"

    # Log this action if ticket validation passed
    if (context$ticket_validation_passed) {
      tryCatch(
        log_ticket_activity_on_connection(
          db_connection = conn, username = context$username, ticket_number = selected_ticket_number,
          workspace_id = context$workspace_id, action_type = "REQUEST_API_STATUS",
          notes = paste("User requested current status via API. URL:", api_url),
          status_summary_cache = if (nchar(api_response_content) > 2000) substr(api_response_content, 1, 2000) else api_response_content
        ), error = function(e) {
          warning(paste("Logging error for REQUEST_API_STATUS for ticket", selected_ticket_number, ":", e$message))
        })
    }
  })
})

# Request Current Ticket Summary (via API)
observeEvent(input$immport_request_ticket_summary_btn, {
  withProgress(message = 'Requesting ticket summary...', detail = 'This may take a moment.', {
    context <- user_context()

    req(context$is_valid, message = "User context is not valid. Please authenticate and select a workspace.")
    req(input$immport_ticket_select, input$immport_ticket_select != "", message = "Please select a ticket first.")

    selected_ticket_number <- input$immport_ticket_select

    api_url <- paste0("https://immport-upload.niaid.nih.gov:8443/data/upload/registration/",
                      URLencode(selected_ticket_number, reserved = TRUE),
                      "/reports/summary")

    api_response_content <- "Error: Could not fetch summary from API."

    tryCatch({
      response <- httr::GET(
        url = api_url,
        httr::add_headers(Authorization = paste("Bearer", context$token)),
        httr::timeout(15)
      )

      response_text <- httr::content(response, "text", encoding = "UTF-8")

      if (httr::status_code(response) == 200) {
        api_response_content <- response_text
        showNotification(paste("Summary fetched for ticket:", selected_ticket_number), type = "message")
      } else {
        api_response_content <- paste("API Error (Summary ", httr::status_code(response), "): ", response_text, sep="")
        showNotification(paste("Failed to fetch summary for", selected_ticket_number), type = "error", duration = 10)
      }
    }, error = function(e) {
      api_response_content <- paste("R Error during API call for summary:", e$message)
      warning(api_response_content)
      showNotification("An R error occurred while trying to fetch summary.", type = "error", duration = 10)
    })

    app_state$current_ticket_summary <- api_response_content
    app_state$display_mode_summary <- "current"

    # Log this action if ticket validation passed
    if (context$ticket_validation_passed) {
      tryCatch(
        log_ticket_activity_on_connection(
          db_connection = conn,
          username = context$username,
          ticket_number = selected_ticket_number,
          workspace_id = context$workspace_id,
          action_type = "REQUEST_API_SUMMARY",
          notes = paste("User requested current summary via API. URL:", api_url),
          status_summary_cache = if (nchar(api_response_content) > 2000) substr(api_response_content, 1, 2000) else api_response_content
        ),
        error = function(e) {
          warning(paste("Logging error for REQUEST_API_SUMMARY for ticket", selected_ticket_number, ":", e$message))
        }
      )
    }
  })
})

# Output renderers for ticket status and summary
output$immport_ticket_status_output <- renderPrint({
  text_to_display <- "Status will appear here. Please authenticate and select a ticket."
  context <- user_context()

  if (context$is_valid) {
    if (app_state$display_mode_status == "current") {
      if (!is.null(app_state$current_ticket_status) && nzchar(app_state$current_ticket_status)) {
        text_to_display <- app_state$current_ticket_status
      } else {
        text_to_display <- "Request current status or view history."
      }
    } else if (app_state$display_mode_status == "history") {
      text_to_display <- app_state$historical_ticket_status_log %||% "No status history loaded. Select 'View Status History'."
    }
  } else {
    text_to_display <- app_state$current_ticket_status %||% (context$message %||% "Please authenticate and select workspace.")
  }

  cat(as.character(text_to_display))
})

output$immport_summary_output <- renderPrint({
  text_to_display <- "Summary will appear here. Please authenticate and select a ticket."
  context <- user_context()

  if (context$is_valid) {
    if (app_state$display_mode_summary == "current") {
      if (!is.null(app_state$current_ticket_summary) && nzchar(app_state$current_ticket_summary)) {
        text_to_display <- app_state$current_ticket_summary
      } else {
        text_to_display <- "Request current summary or view history."
      }
    } else if (app_state$display_mode_summary == "history") {
      text_to_display <- app_state$historical_ticket_summary_log %||% "No summary history loaded. Select 'View Summary History'."
    }
  } else {
    text_to_display <- app_state$current_ticket_summary %||% (context$message %||% "Please authenticate and select workspace.")
  }

  cat(as.character(text_to_display))
})

# --- Download Handler - FIXED ---
output$immport_download_files_btn <- downloadHandler(
  filename = function() {
    # 1. Validate user context first
    user_context <- get_validated_user_context()
    if (!user_context$valid) {
      return("access_denied.txt")
    }
    
    # 2. Check if we have templates or files to download
    current_templates <- staged_templates_list()
    current_files <- staged_files_list()
    
    if (length(current_templates) == 0 && length(current_files) == 0) {
      return("no_data_to_download.txt")
    }
    
    # 3. Generate filename based on study and workspace
    study_id <- if (!is.null(input$immport_study_accession) && nzchar(trimws(input$immport_study_accession))) {
      gsub("[^A-Za-z0-9_\\-]", "", trimws(input$immport_study_accession))
    } else if (length(current_templates) > 0) {
      # Get study from first template
      first_template <- current_templates[[1]]
      if (!is.null(first_template$study)) {
        gsub("[^A-Za-z0-9_\\-]", "", first_template$study)
      } else {
        "ImmPort_Submission"
      }
    } else {
      "ImmPort_Submission"
    }
    
    # Include workspace and timestamp
    paste0(study_id, "_WS", user_context$workspace_id, "_", strftime(Sys.time(), "%Y%m%d-%H%M%S"), ".zip")
  },

  content = function(file) {
    print("DEBUG: Download handler content function called")
    
    # 1. Validate user context
    user_context <- get_validated_user_context()
    if (!user_context$valid) {
      writeLines("Access denied: User not authenticated or authorized.", file)
      return()
    }
    
    # 2. Get current data
    current_templates <- staged_templates_list()
    current_files <- staged_files_list()
    
    print(paste("DEBUG: Templates count:", length(current_templates)))
    print(paste("DEBUG: Files count:", length(current_files)))
    
    # 3. Check if we have anything to download
    has_templates <- length(current_templates) > 0
    has_files <- length(current_files) > 0
    
    if (!has_templates && !has_files) {
      writeLines("No data to download. Please generate templates or add files first.", file)
      return()
    }

    showNotification("Preparing zip file for download...", type = "message", duration = 5)

    # 4. Create temporary directory
    temp_zip_root_dir <- file.path(tempdir(), "immport_submission_files")
    dir.create(temp_zip_root_dir, recursive = TRUE)
    on.exit(unlink(temp_zip_root_dir, recursive = TRUE), add = TRUE)

    all_files_to_zip_paths <- c()

    # 5. Process templates
    if (has_templates) {
      print("DEBUG: Processing templates for download")
      
      for (template_name in names(current_templates)) {
        template_entry <- current_templates[[template_name]]
        template_data <- template_entry$data
        
        if (!is.null(template_data)) {
          path <- file.path(temp_zip_root_dir, template_name)
          tryCatch({
            json_string <- jsonlite::toJSON(template_data, auto_unbox = TRUE, pretty = TRUE, null = "null")
            writeLines(json_string, path, useBytes = TRUE)
            all_files_to_zip_paths <- c(all_files_to_zip_paths, path)
            print(paste("DEBUG: Added template to zip:", template_name))
          }, error = function(e) {
            warning(paste("Error writing template", template_name, ":", e$message))
          })
        }
      }
    }

    # 6. Process files
    if (has_files) {
      print("DEBUG: Processing files for download")
      
      for (file_id in names(current_files)) {
        file_info <- current_files[[file_id]]
        destination_path <- file.path(temp_zip_root_dir, file_info$name)
        
        tryCatch({
          file.copy(file_info$datapath, destination_path, overwrite = TRUE)
          all_files_to_zip_paths <- c(all_files_to_zip_paths, destination_path)
          print(paste("DEBUG: Added file to zip:", file_info$name))
        }, error = function(e) {
          warning(paste("Error copying file", file_info$name, ":", e$message))
        })
      }
    }

    # 7. Create zip file
    if (length(all_files_to_zip_paths) == 0) {
      writeLines("Failed to prepare any files for zipping. Download cancelled.", file)
      return()
    }

    print(paste("DEBUG: Creating zip with", length(all_files_to_zip_paths), "files"))
    
    tryCatch({
      zip::zipr(zipfile = file, files = all_files_to_zip_paths, root = temp_zip_root_dir)
      print("DEBUG: Zip file created successfully")
      showNotification("Download ready!", type = "message", duration = 3)
    }, error = function(e) {
      print(paste("DEBUG: Error creating zip:", e$message))
      writeLines(paste("Error creating zip file:", e$message), file)
    })
  },
  contentType = "application/zip"
)

# Handle template overwrite confirmation
observeEvent(input$confirm_overwrite, {
  if (!is.null(session$userData$pending_templates)) {
    add_templates_to_staged(session$userData$pending_templates, session$userData$current_study)
    session$userData$pending_templates <- NULL
    session$userData$current_study <- NULL
  }
  removeModal()
})

