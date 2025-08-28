# Workspace Access Management UI and Server Logic

# Note: Global reactive values (current_workspace, studies_trigger, etc.) are defined in app.R

# Update current workspace when user data changes - BUT ONLY ON INITIAL LOAD
observe({
  user_data_info <- userData_upload()
  if (!is.null(user_data_info) && nrow(user_data_info) > 0) {
    # Only update current workspace if it's not already set (initial load only)
    if (is.null(current_workspace()) || is.na(current_workspace())) {
      print("DEBUG: Setting initial workspace from user data")
      current_workspace(user_data_info$workspace_id[1])
    } else {
      print(paste("DEBUG: Workspace already set to", current_workspace(), "- skipping update from user data"))
    }
  }
})

# UI for Workspace Management Tab
output$workspace_management_ui <- renderUI({
  
  # Add reactive dependencies to trigger re-rendering
  workspace_list_trigger()  # This will cause re-render when workspaces change
  access_keys_trigger()     # This will cause re-render when access keys change
  
  # Get current user info from authenticated user data
  current_user_data <- user_data()
  if (is.null(current_user_data) || !isTRUE(current_user_data$is_authenticated)) {
    return(div(
      style = "padding: 20px; text-align: center;",
      h4("Please log in to access workspace management", style = "color: #dc3545;")
    ))
  }
  
  # Get the actual logged-in user email
  auth_user_data <- user_data()
  print("DEBUG: auth_user_data structure:")
  print(str(auth_user_data))
  current_user_email <- if (!is.null(auth_user_data) && !is.null(auth_user_data$email)) {
    print(paste("DEBUG: Found email in auth_user_data:", auth_user_data$email))
    auth_user_data$email
  } else {
    print("DEBUG: No email in auth_user_data, using fallback")
    "dev_user@dartmouth.edu"  # fallback only
  }
  current_workspace_id <- current_workspace()  # Use reactive value
  
  # Get user's workspaces - SIMPLE QUERY
  user_workspaces <- tryCatch({
    print(paste("DEBUG: Querying workspaces for user:", current_user_email))
    result <- DBI::dbGetQuery(conn, 
      "SELECT w.workspace_id, w.name as workspace_name, w.category, wa.role
       FROM madi_track.workspace_access wa
       JOIN madi_dat.workspace w ON wa.workspace_id = w.workspace_id
       WHERE wa.user_email = $1 AND wa.is_active = TRUE
       ORDER BY w.name", 
      params = list(current_user_email))
    print(paste("DEBUG: Found", nrow(result), "workspaces for user"))
    print("DEBUG: Workspace result:")
    print(result)
    result
  }, error = function(e) {
    warning(paste("Workspace query failed:", e$message))
    print(paste("DEBUG: Workspace query FAILED for user:", current_user_email))
    data.frame()
  })
  
  # Get workspace keys user can manage (where they are owner/admin)
  workspace_keys <- tryCatch({
    DBI::dbGetQuery(conn, 
      "SELECT wk.*, w.name as workspace_name 
       FROM madi_track.workspace_keys wk
       JOIN madi_dat.workspace w ON wk.workspace_id = w.workspace_id
       JOIN madi_track.workspace_access wa ON wk.workspace_id = wa.workspace_id
       WHERE wa.user_email = $1 AND wa.role IN ('owner', 'admin') AND wk.is_active = TRUE
       ORDER BY w.name, wk.created_at DESC", 
      params = list(current_user_email))
  }, error = function(e) {
    data.frame()
  })
  
  # Check if workspace tables exist
  tables_exist <- tryCatch({
    table_check <- DBI::dbGetQuery(conn, 
      "SELECT EXISTS (
         SELECT FROM information_schema.tables 
         WHERE table_schema = 'madi_track' 
         AND table_name = 'workspace_access'
       )")
    table_check$exists[1]
  }, error = function(e) {
    FALSE
  })
  
  fluidPage(
    style = "padding: 20px;",
    
    # Header
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px; margin-bottom: 30px;",
      h2("🏢 Workspace Management", style = "margin: 0; font-weight: 600;"),
      p(paste("Currently in:", if(is.null(current_workspace_id)) "Loading..." else current_workspace_id, "- Manage your workspaces and access"), style = "margin: 10px 0 0 0; opacity: 0.9;")
    ),
    
    # Show setup message if tables don't exist
    if(!tables_exist) {
      div(
        style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 20px; border-radius: 8px; margin-bottom: 30px;",
        h4("⚠️ Database Setup Required", style = "color: #856404; margin-bottom: 15px;"),
        p("The workspace management tables haven't been created yet. Please run the database schema setup first:", style = "color: #856404; margin-bottom: 15px;"),
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; font-family: monospace; border: 1px solid #dee2e6;",
          "Execute the workspace_access_schema.sql file in your PostgreSQL database to create the necessary tables."
        ),
        br(),
        p("After running the schema, refresh this page to access workspace management features.", style = "color: #856404; font-weight: 500;")
      )
    },
    
    # Main content in tabs (always show but may be empty if no tables)
    tabsetPanel(
      type = "tabs",
      
      # Tab 1: My Workspaces & Switch
      tabPanel(
        title = "🔄 My Workspaces",
        value = "my_workspaces",
        br(),
        
        fluidRow(
          column(6,
            div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #28a745;",
              h4("🏠 Your Workspaces", style = "color: #155724; margin-bottom: 15px;"),
              
              if(nrow(user_workspaces) > 0) {
                tagList(
                  # Current workspace highlight
                  if(current_workspace_id %in% user_workspaces$workspace_id) {
                    div(
                      style = "background-color: #d4edda; padding: 15px; border-radius: 6px; border: 2px solid #28a745; margin-bottom: 15px;",
                      div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        div(
                          h5(paste("📍", user_workspaces[user_workspaces$workspace_id == current_workspace_id, ]$workspace_name), 
                             style = "color: #155724; margin: 0;"),
                          p(paste("ID:", current_workspace_id, "• Role:", user_workspaces[user_workspaces$workspace_id == current_workspace_id, ]$role), 
                            style = "color: #155724; margin: 5px 0 0 0; font-size: 0.9em;")
                        ),
                        span("CURRENT", style = "background-color: #28a745; color: white; padding: 4px 8px; border-radius: 12px; font-size: 0.8em; font-weight: 600;")
                      )
                    )
                  },
                  
                  # Other workspaces
                  lapply(seq_len(nrow(user_workspaces)), function(i) {
                    ws <- user_workspaces[i, ]
                    if(ws$workspace_id != current_workspace_id) {
                      div(
                        style = "background-color: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; margin-bottom: 10px;",
                        div(
                          style = "display: flex; justify-content: space-between; align-items: center;",
                          div(
                            h6(ws$workspace_name, style = "color: #495057; margin: 0;"),
                            p(paste("ID:", ws$workspace_id, "• Role:", ws$role), 
                              style = "color: #6c757d; margin: 5px 0 0 0; font-size: 0.85em;")
                          ),
                          actionButton(
                            paste0("switch_to_", ws$workspace_id),
                            "Switch",
                            class = "btn btn-outline-primary btn-sm",
                            style = "border-radius: 15px;"
                          )
                        )
                      )
                    }
                  })
                )
              } else {
                div(
                  style = "text-align: center; padding: 30px;",
                  h5("No additional workspaces found", style = "color: #6c757d;"),
                  p("Create a new workspace or join existing ones using access keys.")
                )
              }
            )
          ),
          
          column(6,
            div(
              style = "background-color: #fff3cd; padding: 20px; border-radius: 8px; border-left: 4px solid #856404;",
              h4("➕ Quick Actions", style = "color: #856404; margin-bottom: 15px;"),
              
              # Create New Workspace
              div(
                style = "background-color: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
                h6("Create New Workspace", style = "color: #495057; margin-bottom: 10px;"),
                textInput("new_workspace_name", "Workspace Name:", placeholder = "Enter workspace name..."),
                selectInput("new_workspace_category", "Category:", 
                           choices = c("NIH", "Custom", "Research", "Clinical"), 
                           selected = "Custom"),
                selectInput("new_workspace_type", "Type:", 
                           choices = c("PW" = "PW", "Project" = "PROJECT", "Study" = "STUDY"), 
                           selected = "PW"),
                actionButton("create_workspace_btn", "🏗️ Create Workspace", class = "btn btn-success", style = "border-radius: 20px; width: 100%;")
              ),
              
              # Join Workspace
              div(
                style = "background-color: white; padding: 15px; border-radius: 6px;",
                h6("Join Existing Workspace", style = "color: #495057; margin-bottom: 10px;"),
                p("Enter workspace ID and access key to gain access:", style = "color: #6c757d; font-size: 0.9em; margin-bottom: 15px;"),
                numericInput("join_workspace_id", "Workspace ID:", value = NULL, min = 1),
                textInput("join_access_key", "Access Key:", placeholder = "Enter access key..."),
                br(),
                actionButton("join_workspace_btn", "🔗 Join Workspace", class = "btn btn-info", style = "border-radius: 20px; width: 100%;")
              )
            )
          )
        )
      ),
      
      # Tab 2: Manage Access Keys
      tabPanel(
        title = "🔑 Access Keys",
        value = "access_keys",
        br(),
        
        fluidRow(
          column(8,
            div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px;",
              h4("🔑 Workspace Access Keys", style = "color: #495057; margin-bottom: 15px;"),
              
              if(nrow(workspace_keys) > 0) {
                lapply(seq_len(nrow(workspace_keys)), function(i) {
                  key_info <- workspace_keys[i, ]
                  div(
                    style = "background-color: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; margin-bottom: 15px;",
                    div(
                      style = "display: flex; justify-content: space-between; align-items: start;",
                      div(
                        style = "flex: 1;",
                        h6(paste("🏢", key_info$workspace_name), style = "color: #495057; margin: 0;"),
                        p(paste("Workspace ID:", key_info$workspace_id), style = "color: #6c757d; margin: 5px 0; font-size: 0.9em;"),
                        div(
                          style = "background-color: #f8f9fa; padding: 8px; border-radius: 4px; font-family: monospace; margin: 10px 0;",
                          strong("Access Key: "), 
                          span(key_info$access_key, style = "color: #007bff; font-weight: 600;")
                        ),
                        if(!is.null(key_info$description) && nzchar(key_info$description)) {
                          p(paste("📝", key_info$description), style = "color: #6c757d; font-size: 0.85em; margin: 5px 0;")
                        },
                        p(paste("Created:", format(as.Date(key_info$created_at), "%Y-%m-%d"), 
                               "• Uses:", key_info$current_uses, "/", ifelse(is.null(key_info$max_uses), "∞", key_info$max_uses)),
                          style = "color: #6c757d; font-size: 0.8em; margin: 5px 0 0 0;")
                      ),
                      div(
                        actionButton(
                          paste0("deactivate_key_", key_info$key_id),
                          "❌ Deactivate",
                          class = "btn btn-outline-danger btn-sm",
                          style = "border-radius: 15px;"
                        )
                      )
                    )
                  )
                })
              } else {
                div(
                  style = "text-align: center; padding: 30px;",
                  h5("No access keys found", style = "color: #6c757d;"),
                  p("Create access keys for workspaces you own or manage.")
                )
              }
            )
          ),
          
          column(4,
            div(
              style = "background-color: #e7f3ff; padding: 20px; border-radius: 8px; border-left: 4px solid #007bff;",
              h4("🆕 Create Access Key", style = "color: #004085; margin-bottom: 15px;"),
              
              # Only show workspaces where user is owner/admin
              if(nrow(user_workspaces[user_workspaces$role %in% c("owner", "admin"), ]) > 0) {
                tagList(
                  selectInput("key_workspace_id", "Select Workspace:",
                             choices = setNames(
                               user_workspaces[user_workspaces$role %in% c("owner", "admin"), ]$workspace_id,
                               paste(user_workspaces[user_workspaces$role %in% c("owner", "admin"), ]$workspace_name, 
                                    "(ID:", user_workspaces[user_workspaces$role %in% c("owner", "admin"), ]$workspace_id, ")")
                             )),
                  textInput("key_description", "Description (optional):", placeholder = "Purpose of this key..."),
                  numericInput("key_max_uses", "Max Uses (optional):", value = NULL, min = 1),
                  p("Leave empty for unlimited uses", style = "font-size: 0.85em; color: #6c757d; margin-top: -5px;"),
                  dateInput("key_expires", "Expires (optional):", value = NULL, min = Sys.Date()),
                  br(),
                  actionButton("create_key_btn", "🔑 Generate Access Key", 
                              class = "btn btn-primary", style = "border-radius: 20px; width: 100%;")
                )
              } else {
                div(
                  style = "text-align: center; padding: 20px;",
                  h6("No manageable workspaces", style = "color: #6c757d;"),
                  p("You need to be an owner or admin of a workspace to create access keys.")
                )
              }
            )
          )
        )
      )
    )
  )
})

# Server Logic for Workspace Management

# Create new workspace
observeEvent(input$create_workspace_btn, {
  req(input$new_workspace_name)
  
  # Check if tables exist first
  tables_exist <- tryCatch({
    workspace_table_check <- DBI::dbGetQuery(conn, 
      "SELECT EXISTS (
         SELECT FROM information_schema.tables 
         WHERE table_schema = 'madi_dat' 
         AND table_name = 'workspace'
       )")
    access_table_check <- DBI::dbGetQuery(conn, 
      "SELECT EXISTS (
         SELECT FROM information_schema.tables 
         WHERE table_schema = 'madi_track' 
         AND table_name = 'workspace_access'
       )")
    workspace_table_check$exists[1] && access_table_check$exists[1]
  }, error = function(e) {
    FALSE
  })
  
  if (!tables_exist) {
    showNotification(
      "❌ Database tables not found. Please run the workspace schema setup first.",
      type = "error",
      duration = 8
    )
    return()
  }
  
  # Get current user email for workspace creation
  current_user_email <- tryCatch({
    auth_user_data <- user_data()
    if (!is.null(auth_user_data) && !is.null(auth_user_data$email)) {
      print(paste("DEBUG: Create workspace using email:", auth_user_data$email))
      auth_user_data$email
    } else {
      print("DEBUG: No email in create workspace, using fallback")
      "dev_user@dartmouth.edu"  # fallback only
    }
  }, error = function(e) {
    print(paste("DEBUG: Error getting email for create workspace:", e$message))
    "dev_user@dartmouth.edu"
  })
  
  tryCatch({
    # Insert into madi_dat.workspace table
    workspace_result <- DBI::dbGetQuery(conn,
      "INSERT INTO madi_dat.workspace (name, category, type) 
       VALUES ($1, $2, $3) RETURNING workspace_id",
      params = list(input$new_workspace_name, input$new_workspace_category, input$new_workspace_type)
    )
    
    new_workspace_id <- workspace_result$workspace_id[1]
    
    # Grant owner access to creator in workspace_access table
    DBI::dbExecute(conn,
      "INSERT INTO madi_track.workspace_access (workspace_id, user_email, role) 
       VALUES ($1, $2, 'owner')",
      params = list(new_workspace_id, current_user_email)
    )
    
    # Show success message
    showNotification(
      paste("✅ Workspace '", input$new_workspace_name, "' created successfully! (ID: ", new_workspace_id, ")"),
      type = "message",
      duration = 5
    )
    
    # Clear inputs
    updateTextInput(session, "new_workspace_name", value = "")
    updateSelectInput(session, "new_workspace_category", selected = "Custom")
    updateSelectInput(session, "new_workspace_type", selected = "PW")
    
    # Trigger UI refresh
    workspace_list_trigger(workspace_list_trigger() + 1)
    
  }, error = function(e) {
    showNotification(
      paste("❌ Error creating workspace:", e$message),
      type = "error",
      duration = 8
    )
  })
})

# Join workspace with access key
observeEvent(input$join_workspace_btn, {
  req(input$join_workspace_id, input$join_access_key)
  
  # Get current user email for joining workspace
  current_user_email <- tryCatch({
    auth_user_data <- user_data()
    if (!is.null(auth_user_data) && !is.null(auth_user_data$email)) {
      print(paste("DEBUG: Join workspace using email:", auth_user_data$email))
      auth_user_data$email
    } else {
      print("DEBUG: No email in join workspace, using fallback")
      "dev_user@dartmouth.edu"  # fallback only
    }
  }, error = function(e) {
    print(paste("DEBUG: Error getting email for join workspace:", e$message))
    "dev_user@dartmouth.edu"
  })
  
  tryCatch({
    # Validate access key - simple query with expiration check
    key_check <- DBI::dbGetQuery(conn,
      "SELECT wk.*, w.name as workspace_name 
       FROM madi_track.workspace_keys wk
       JOIN madi_dat.workspace w ON wk.workspace_id = w.workspace_id
       WHERE wk.workspace_id = $1 AND wk.access_key = $2 AND wk.is_active = TRUE 
       AND (wk.expires_at IS NULL OR wk.expires_at > NOW())",
      params = list(input$join_workspace_id, input$join_access_key)
    )
    
    if(nrow(key_check) == 0) {
      showNotification("❌ Invalid workspace ID or access key", type = "error", duration = 5)
      return()
    }
    
    # Check if user already has access
    existing_access <- DBI::dbGetQuery(conn,
      "SELECT * FROM madi_track.workspace_access 
       WHERE workspace_id = $1 AND user_email = $2",
      params = list(input$join_workspace_id, current_user_email)
    )
    
    if(nrow(existing_access) > 0) {
      showNotification("ℹ️ You already have access to this workspace", type = "warning", duration = 5)
      return()
    }
    
    # Grant access
    DBI::dbExecute(conn,
      "INSERT INTO madi_track.workspace_access (workspace_id, user_email, role, joined_via_key) 
       VALUES ($1, $2, 'member', $3)",
      params = list(input$join_workspace_id, current_user_email, input$join_access_key)
    )
    
    # Update key usage count
    DBI::dbExecute(conn,
      "UPDATE madi_track.workspace_keys 
       SET current_uses = current_uses + 1 
       WHERE access_key = $1",
      params = list(input$join_access_key)
    )
    
    showNotification(
      paste("🎉 Successfully joined workspace:", key_check$workspace_name[1]),
      type = "message",
      duration = 5
    )
    
    # Clear inputs
    updateNumericInput(session, "join_workspace_id", value = NULL)
    updateTextInput(session, "join_access_key", value = "")
    
    # Trigger UI refresh for workspace list
    workspace_list_trigger(workspace_list_trigger() + 1)
    
  }, error = function(e) {
    showNotification(
      paste("❌ Error joining workspace:", e$message),
      type = "error",
      duration = 8
    )
  })
})

# Create access key
observeEvent(input$create_key_btn, {
  req(input$key_workspace_id)
  
  # Check if tables exist first
  tables_exist <- tryCatch({
    table_check <- DBI::dbGetQuery(conn, 
      "SELECT EXISTS (
         SELECT FROM information_schema.tables 
         WHERE table_schema = 'madi_dat' 
         AND table_name = 'workspace'
       )")
    table_check$exists[1]
  }, error = function(e) {
    FALSE
  })
  
  if (!tables_exist) {
    showNotification(
      "❌ Database tables not found. Please run the workspace schema setup first.",
      type = "error",
      duration = 8
    )
    return()
  }
  
  # Get current user email for creating access key
  current_user_email <- tryCatch({
    auth_user_data <- user_data()
    if (!is.null(auth_user_data) && !is.null(auth_user_data$email)) {
      print(paste("DEBUG: Create key using email:", auth_user_data$email))
      auth_user_data$email
    } else {
      print("DEBUG: No email in create key, using fallback")
      "dev_user@dartmouth.edu"  # fallback only
    }
  }, error = function(e) {
    print(paste("DEBUG: Error getting email for create key:", e$message))
    "dev_user@dartmouth.edu"
  })
  
  tryCatch({
    # Generate unique access key
    access_key <- paste0("WS", input$key_workspace_id, "-", toupper(substr(digest::digest(paste(Sys.time(), runif(1)), algo = "md5"), 1, 8)))
    
    # Prepare expiration date
    expires_at <- if(!is.null(input$key_expires)) as.character(input$key_expires) else NULL
    
    DBI::dbExecute(conn,
      "INSERT INTO madi_track.workspace_keys 
       (workspace_id, access_key, created_by, description, max_uses, expires_at) 
       VALUES ($1, $2, $3, $4, $5, $6)",
      params = list(
        input$key_workspace_id,
        access_key,
        current_user_email,
        input$key_description %||% "",
        input$key_max_uses,
        expires_at
      )
    )
    
    showNotification(
      paste("🔑 Access key created:", access_key),
      type = "message",
      duration = 10
    )
    
    # Clear inputs
    updateTextInput(session, "key_description", value = "")
    updateNumericInput(session, "key_max_uses", value = NULL)
    updateDateInput(session, "key_expires", value = NULL)
    
    # Trigger UI refresh for access keys
    access_keys_trigger(access_keys_trigger() + 1)
    
  }, error = function(e) {
    showNotification(
      paste("❌ Error creating access key:", e$message),
      type = "error",
      duration = 8
    )
  })
})

# Observer for deactivate key buttons - Enhanced to handle dynamic UI updates
observe({
  # React to access keys UI refresh trigger
  access_keys_trigger()
  
  # Get all active keys to create dynamic observers for their deactivate buttons
  all_keys <- tryCatch({
    DBI::dbGetQuery(conn, "
      SELECT key_id 
      FROM madi_track.workspace_keys 
      WHERE is_active = TRUE
    ")
  }, error = function(e) {
    data.frame(key_id = integer(0))
  })
  
  # Check each deactivate button dynamically
  for (i in seq_len(nrow(all_keys))) {
    key_id <- all_keys$key_id[i]
    button_id <- paste0("deactivate_key_", key_id)
    
    # Check if this button was clicked
    button_value <- input[[button_id]]
    
    if (!is.null(button_value) && button_value > 0) {
      # This button was clicked, process the deactivation
      print(paste("DEBUG: Deactivate button clicked for key:", key_id))
      
      # Check if we've already processed this button click to prevent loops
      processed_key <- paste0("processed_deactivate_", key_id, "_", button_value)
      if (!is.null(session$userData[[processed_key]])) {
        print(paste("DEBUG: Already processed this deactivate click, skipping to prevent loop"))
        next
      }
      
      # Mark this button click as processed
      session$userData[[processed_key]] <- TRUE
      
      tryCatch({
        # Deactivate the key in the database
        result <- DBI::dbExecute(conn, "
          UPDATE madi_track.workspace_keys 
          SET is_active = FALSE
          WHERE key_id = $1
        ", params = list(key_id))
        
        if (result > 0) {
          showNotification(
            "✅ Access key deactivated successfully",
            type = "message",
            duration = 3
          )
          
          print(paste("DEBUG: Successfully deactivated key:", key_id))
          
          # Trigger UI refresh for access keys
          access_keys_trigger(access_keys_trigger() + 1)
          
        } else {
          showNotification(
            "❌ Failed to deactivate access key",
            type = "error",
            duration = 5
          )
          print(paste("DEBUG: Failed to deactivate key - no rows affected:", key_id))
        }
        
      }, error = function(e) {
        print(paste("DEBUG: Error deactivating key:", key_id, "-", e$message))
        showNotification(
          paste("❌ Error deactivating key:", e$message),
          type = "error",
          duration = 5
        )
      })
      
      break  # Exit the loop once we've processed one button
    }
  }
})

# Observer for switch workspace buttons - Enhanced to handle dynamic UI updates
observe({
  # React to workspace list UI refresh trigger
  workspace_list_trigger()
  
  # Get user's workspaces to check for switch button clicks
  auth_user_data <- user_data()
  current_user_email <- if (!is.null(auth_user_data) && !is.null(auth_user_data$email)) {
    auth_user_data$email
  } else {
    "dev_user@dartmouth.edu"
  }
  
  user_workspaces <- tryCatch({
    DBI::dbGetQuery(conn, 
      "SELECT w.workspace_id, w.name as workspace_name, w.category, wa.role
       FROM madi_track.workspace_access wa
       JOIN madi_dat.workspace w ON wa.workspace_id = w.workspace_id
       WHERE wa.user_email = $1 AND wa.is_active = TRUE
       ORDER BY w.name", 
      params = list(current_user_email))
  }, error = function(e) {
    data.frame(workspace_id = numeric(0), workspace_name = character(0), 
               category = character(0), role = character(0))
  })
  
  # Check each switch workspace button
  for (i in seq_len(nrow(user_workspaces))) {
    ws_id <- user_workspaces$workspace_id[i]
    button_id <- paste0("switch_to_", ws_id)
    
    # Check if this button was clicked
    button_value <- input[[button_id]]
    
    if (!is.null(button_value) && button_value > 0) {
      print(paste("DEBUG: Switch workspace button clicked for workspace:", ws_id))
      
      # Check if we've already processed this button click to prevent loops
      processed_key <- paste0("processed_switch_", ws_id, "_", button_value)
      if (!is.null(session$userData[[processed_key]])) {
        print(paste("DEBUG: Already processed this button click, skipping to prevent loop"))
        next
      }
      
      # Mark this button click as processed
      session$userData[[processed_key]] <- TRUE
      
      tryCatch({
        # Get current user email
        current_user_email <- if (!is.null(auth_user_data) && !is.null(auth_user_data$email)) {
          auth_user_data$email
        } else {
          "dev_user@dartmouth.edu"
        }
        
        # Update the reactive value for immediate UI update
        current_workspace(ws_id)
        
        # Also update the user's workspace_id in the database for persistence
        current_user_compress <- gsub("[[:punct:][:blank:]]+", "", current_user_email)
        result <- DBI::dbExecute(conn,
          "UPDATE madi_track.users 
           SET workspace_id = $1, updated_at = CURRENT_TIMESTAMP 
           WHERE regexp_replace(oauth_unique_id, '[[:punct:][:blank:]]', '', 'g') = $2",
          params = list(ws_id, current_user_compress)
        )
        
        # Get workspace name for better notification
        workspace_info <- DBI::dbGetQuery(conn,
          "SELECT name FROM madi_dat.workspace WHERE workspace_id = $1",
          params = list(ws_id)
        )
        
        workspace_name <- if (nrow(workspace_info) > 0) {
          workspace_info$name[1]
        } else {
          paste("Workspace", ws_id)
        }
        
        showNotification(
          paste("✅ Switched to:", workspace_name),
          type = "message",
          duration = 5
        )
        
        print(paste("DEBUG: Workspace switched to", ws_id, "- database updated:", result > 0))
        
        # Refresh global study data for the new workspace
        tryCatch({
          study <<- get_studies_data_direct(conn, ws_id)
          all_study_list <<- get_user_workspace_studies(conn, ws_id)
          print(paste("DEBUG: Refreshed study data for workspace", ws_id, "- found", nrow(study), "studies"))
        }, error = function(e) {
          print(paste("DEBUG: Error refreshing study data:", e$message))
        })
        
        # Trigger studies refresh ONLY (not workspace list to avoid loop)
        studies_trigger(studies_trigger() + 1)
        
      }, error = function(e) {
        showNotification(
          paste("❌ Error switching workspace:", e$message),
          type = "error",
          duration = 5
        )
      })
      
      break  # Exit the loop once we've processed one button
    }
  }
})
