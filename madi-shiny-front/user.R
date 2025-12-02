# --- Reactive values ---
# Provide meaningful default values
currentuser <- reactiveVal("Unknown User")
# Initialize userData_upload to NULL to clearly indicate no user data initially
userData_upload <- reactiveVal(NULL)

# Shows the SQL query *string* that was constructed (for debugging)
output$select_user_str_display <- renderText({
  # This reactive generates the SQL string only when this output is rendered
  current_user_id <- NULL

  # Check if user is authenticated and get user ID
  if (!isTRUE(session$userData$app_logic_initialized)) {
    current_user_id <- "dev_user_local" # Default for testing/local dev
  } else if (isTRUE(session$userData$app_logic_initialized)) {
    # Get user ID from authenticated user data
    ud <- user_data()
    if (!is.null(ud) && isTRUE(ud$is_authenticated) && !is.null(ud$email)) {
      current_user_id <- toString(ud$email)
      req(nzchar(current_user_id))
    } else {
      req(FALSE) # Stop if no valid user data
    }
  } else {
    req(FALSE) # Stop if auth state is unknown
  }

  current_user_compress <- gsub("[[:punct:][:blank:]]+", "", current_user_id)
  # Reconstruct the query string safely for display
  paste0("Attempted query logic: ... WHERE regexp_replace(oauth_unique_id, '[[:punct:][:blank:]]', '', 'g') = '", current_user_compress, "'")
})

# Function to auto-register new users
auto_register_user <- function(email, name, compressed_id) {
  print(paste0("DEBUG: Auto-registering new user: ", email))
  
  # Extract username from email (part before @)
  username <- strsplit(email, "@")[[1]][1]
  
  # Ensure all parameters have exactly one value (no NULL, no empty vectors)
  safe_param <- function(value, default = "") {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return(default)
    }
    return(as.character(value)[1])  # Take first element and ensure it's character
  }
  
  # Prepare parameters safely
  param_username <- safe_param(username)
  param_full_name <- safe_param(name %||% email)
  param_email <- safe_param(email)
  param_oauth_id <- safe_param(compressed_id)
  param_display_name <- safe_param(name %||% username)
  param_created_by <- safe_param("system_auto_register")
  param_project <- safe_param("USER_WORKSPACE")  # Updated project name
  
  print(paste0("DEBUG: Parameters prepared - Username: ", param_username, ", Email: ", param_email))
  print(paste0("DEBUG: Creating new workspace for user"))
  
  # First, create a new workspace for the user in the existing madi_dat.workspace table
  # Create unique workspace name with timestamp
  current_time <- format(Sys.time(), "%Y%m%d_%H%M")
  workspace_name <- paste0(param_username, "_workspace_", current_time)
  
  # Let the database sequence generate the workspace_id automatically
  workspace_insert_query <- "
    INSERT INTO madi_dat.workspace (category, name, type)
    VALUES ($1, $2, $3)
    RETURNING workspace_id;"
  
  new_workspace_result <- tryCatch({
    DBI::dbGetQuery(conn, workspace_insert_query, params = list(
      "Project",   # Using "Project" as category for user workspaces
      workspace_name,
      "PW"         # Using "PW" as type (seems to be the standard type)
    ))
  }, error = function(e) {
    warning(paste("Failed to create workspace for new user:", e$message))
    return(NULL)
  })
  
  if (is.null(new_workspace_result) || nrow(new_workspace_result) == 0) {
    warning("Failed to create workspace, using fallback")
    new_workspace_id <- NA
  } else {
    new_workspace_id <- new_workspace_result$workspace_id[1]
    print(paste0("DEBUG: Created workspace ID: ", new_workspace_id))
    
    # Grant the user owner access to their new workspace
    access_insert_query <- "
      INSERT INTO madi_track.workspace_access (workspace_id, user_email, role)
      VALUES ($1, $2, 'owner');"
    
    tryCatch({
      DBI::dbExecute(conn, access_insert_query, params = list(
        new_workspace_id,
        param_email
      ))
      print(paste0("DEBUG: Granted owner access to workspace ", new_workspace_id))
    }, error = function(e) {
      warning(paste("Failed to grant workspace access:", e$message))
    })
  }
  
  # Enhanced insert query with new workspace assignment
  insert_query <- "
    INSERT INTO madi_track.users (
      username, full_name, email, oauth_unique_id, display_name, 
      is_active, created_by, created_at, project, workspace_id
    ) VALUES ($1, $2, $3, $4, $5, $6, $7, CURRENT_TIMESTAMP, $8, $9)
    ON CONFLICT (username) DO UPDATE SET
      oauth_unique_id = EXCLUDED.oauth_unique_id,
      email = EXCLUDED.email,
      full_name = EXCLUDED.full_name,
      display_name = EXCLUDED.display_name,
      workspace_id = COALESCE(EXCLUDED.workspace_id, madi_track.users.workspace_id),
      updated_at = CURRENT_TIMESTAMP
    RETURNING users_id, is_active, workspace_id;"
  
  result <- tryCatch({
    DBI::dbGetQuery(conn, insert_query, params = list(
      param_username,              # $1
      param_full_name,             # $2
      param_email,                 # $3
      param_oauth_id,              # $4
      param_display_name,          # $5
      TRUE,                        # $6 - is_active (TRUE by default for new users)
      param_created_by,            # $7
      param_project,               # $8
      new_workspace_id             # $9 - newly created workspace_id
    ))
  }, error = function(e) {
    warning(paste("Failed to auto-register user:", e$message))
    print(paste0("DEBUG: SQL parameters were:"))
    print(paste0("  Username: '", param_username, "'"))
    print(paste0("  Full name: '", param_full_name, "'"))
    print(paste0("  Email: '", param_email, "'"))
    print(paste0("  OAuth ID: '", param_oauth_id, "'"))
    print(paste0("  Display name: '", param_display_name, "'"))
    print(paste0("  Created by: '", param_created_by, "'"))
    print(paste0("  Project: '", param_project, "'"))
    print(paste0("  Workspace ID: ", new_workspace_id, " (New User Workspace)"))
    
    # Return a more detailed error response
    data.frame(
      users_id = NA, 
      is_active = FALSE, 
      workspace_id = NA,
      error = e$message
    )
  })
  
  print(paste0("DEBUG: Auto-registration result: ", if(nrow(result) > 0 && !is.na(result$users_id[1])) "SUCCESS" else "FAILED"))
  return(result)
}

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x) || !nzchar(x)) y else x
}

# --- Main Observer for User Authentication and UI Setup ---
observe({
  # Add comprehensive error handling around the entire observer
  tryCatch({
    current_user_id <- NULL
    current_login_prefix <- "User: " # Default

    # 1. Determine Current User ID and Login Prefix
    if (is_local_dev()) {
      # Local development mode
      current_user_id <- "dev_user_local" # Use hardcoded user for local dev
      current_login_prefix <- "Local Dev as "
    } else {
      # Production mode with Dex OIDC
      ud <- user_data()
      
      # Wait until user is authenticated and has email
      req(ud, isTRUE(ud$is_authenticated), ud$email)
      
      current_user_id <- ud$email
      req(nzchar(current_user_id)) # Stop if email is empty
      current_login_prefix <- "Logged in as "
    }

    # 2. Prepare User ID for DB Query (Compression)
    # Ensure this compression logic EXACTLY matches the one used in the WHERE clause
    current_user_compress <- gsub("[[:punct:][:blank:]]+", "", current_user_id)

    # 3. Query Database Efficiently and Safely for the Specific User
    # Updated query to include is_active check
    sql_query <- "
      SELECT
        oauth_unique_id, username, workspace_id, display_name, project, full_name,
        CAST(users_id AS INTEGER) AS users_id, is_active
      FROM madi_track.users
      WHERE regexp_replace(oauth_unique_id, '[[:punct:][:blank:]]', '', 'g') = $1;" # Parameter placeholder

    found_user_data <- tryCatch({
      # Execute parameterized query
      DBI::dbGetQuery(conn, sql_query, params = list(current_user_compress))
    }, error = function(e) {
      # Handle potential database errors gracefully
      warning(paste("Database query failed for user:", current_user_compress, "Error:", e$message))
      data.frame() # Return an empty data frame signifies user not found/error
    })

    # 4. Handle user registration and access control
    user_access_status <- "unknown"
    
    if (nrow(found_user_data) == 0) {
      # User not found - auto-register them
      print(paste0("DEBUG: User not found, auto-registering: ", current_user_id))
      
      # Get user name from authentication data
      ud <- user_data()
      user_name <- ud$name %||% current_user_id
      
      # Auto-register the user
      registration_result <- auto_register_user(current_user_id, user_name, current_user_compress)
      
      if (!is.na(registration_result$users_id)) {
        print(paste0("DEBUG: User auto-registered with ID: ", registration_result$users_id))
        print(paste0("DEBUG: User granted immediate access to their workspace"))
        
        # Re-query to get the newly registered user
        found_user_data <- tryCatch({
          DBI::dbGetQuery(conn, sql_query, params = list(current_user_compress))
        }, error = function(e) {
          warning(paste("Failed to re-query after registration:", e$message))
          data.frame()
        })
        
        # Set the current workspace if user has one
        if (nrow(found_user_data) > 0 && !is.na(found_user_data$workspace_id[1])) {
          new_user_workspace_id <- found_user_data$workspace_id[1]
          print(paste0("DEBUG: Setting initial workspace from user data: ", new_user_workspace_id))
          current_workspace(new_user_workspace_id)
        }
        
        user_access_status <- "active"  # New users are now active immediately
      } else {
        user_access_status <- "registration_failed"
      }
    } else {
      # User found - check if they're active and have a workspace
      is_active_value <- found_user_data$is_active[1]
      print(paste0("DEBUG: User found in database. is_active value: ", is_active_value, " (type: ", typeof(is_active_value), ")"))
      
      if (isTRUE(is_active_value)) {
        user_access_status <- "active"
        print("DEBUG: User is ACTIVE - will grant full access")
        
        # Check if user has a workspace assigned, if not create one
        user_workspace_id <- found_user_data$workspace_id[1]
        if (is.null(user_workspace_id) || is.na(user_workspace_id)) {
          print("DEBUG: User has no workspace, creating one...")
          
          # Extract username from email for workspace creation
          username <- strsplit(current_user_id, "@")[[1]][1]
          current_time <- format(Sys.time(), "%Y%m%d_%H%M")
          workspace_name <- paste0(username, "_workspace_", current_time)
          
          # Get the next workspace_id
          workspace_id_query <- "
            SELECT COALESCE(MAX(workspace_id), 0) + 1 AS next_id 
            FROM madi_dat.workspace;"
          
          next_workspace_id <- tryCatch({
            result <- DBI::dbGetQuery(conn, workspace_id_query)
            as.integer(result$next_id[1])
          }, error = function(e) {
            warning(paste("Failed to get next workspace ID:", e$message))
            return(NULL)
          })
          
          if (!is.null(next_workspace_id) && !is.na(next_workspace_id)) {
            # Create workspace with explicit workspace_id
            workspace_insert_query <- "
              INSERT INTO madi_dat.workspace (workspace_id, name, category, type)
              VALUES ($1, $2, $3, $4)
              RETURNING workspace_id;"
            
            new_workspace_result <- tryCatch({
              DBI::dbGetQuery(conn, workspace_insert_query, params = list(
                next_workspace_id,
                workspace_name,
                "Project",   # Using "Project" as category
                "PW"         # Standard type
              ))
            }, error = function(e) {
              warning(paste("Failed to create workspace for existing user:", e$message))
              return(NULL)
            })
            
            if (!is.null(new_workspace_result) && nrow(new_workspace_result) > 0) {
              new_workspace_id <- new_workspace_result$workspace_id[1]
              print(paste0("DEBUG: Created workspace ID: ", new_workspace_id, " for existing user"))
              
              # Update user's workspace_id
              update_user_query <- "UPDATE madi_track.users SET workspace_id = $1 WHERE oauth_unique_id = $2"
              tryCatch({
                DBI::dbExecute(conn, update_user_query, params = list(new_workspace_id, current_user_compress))
                print("DEBUG: Updated user's workspace_id")
              }, error = function(e) {
                warning(paste("Failed to update user workspace:", e$message))
              })
              
              # Grant owner access
              access_insert_query <- "
                INSERT INTO madi_track.workspace_access (workspace_id, user_email, role)
                VALUES ($1, $2, 'owner');"
              
              tryCatch({
                DBI::dbExecute(conn, access_insert_query, params = list(new_workspace_id, current_user_id))
                print(paste0("DEBUG: Granted owner access to workspace ", new_workspace_id))
                
                # Re-query user data to get updated workspace info
                found_user_data <- tryCatch({
                  DBI::dbGetQuery(conn, sql_query, params = list(current_user_compress))
                }, error = function(e) {
                  warning(paste("Failed to re-query after workspace assignment:", e$message))
                  found_user_data  # Return original data
                })
                
                # Set the current workspace after successful creation
                if (nrow(found_user_data) > 0 && !is.na(found_user_data$workspace_id[1])) {
                  print(paste0("DEBUG: Setting workspace after creation: ", found_user_data$workspace_id[1]))
                  current_workspace(found_user_data$workspace_id[1])
                }
              }, error = function(e) {
                warning(paste("Failed to grant workspace access:", e$message))
              })
            }
          }
        }
      } else {
        user_access_status <- "inactive"
        print("DEBUG: User is INACTIVE - will deny access")
      }
    }

    # 5. Update Reactive Values
    userData_upload(found_user_data) # Store the query result

    # Debugging print
    print(paste0("User query rows: ", nrow(found_user_data), " for compressed ID: ", current_user_compress))
    print(paste0("Original user ID: ", current_user_id))
    print(paste0("User access status: ", user_access_status))

    # 6. Conditionally Render UI based on user access status
    if (nrow(found_user_data) > 0 && user_access_status == "active") {
      # --- User FOUND and ACTIVE in Database - GRANT FULL ACCESS ---
      print("DEBUG: *** GRANTING FULL ACCESS TO ACTIVE USER ***")
      
      user_info <- found_user_data[1, ]

      # Check workspace assignment
      has_workspace <- !is.null(user_info$workspace_id) && !is.na(user_info$workspace_id)
      
      print(paste0("DEBUG: User workspace info - ID: ", user_info$workspace_id, ", has_workspace: ", has_workspace))
      
      # Get workspace information from the new workspace system
      workspace_info <- if (has_workspace) {
        # Query actual workspace details from madi_dat.workspace
        workspace_query <- "
          SELECT w.name as workspace_name, w.category, COALESCE(wa.role, 'member') as role
          FROM madi_dat.workspace w
          LEFT JOIN madi_track.workspace_access wa ON w.workspace_id = wa.workspace_id AND wa.user_email = $2
          WHERE w.workspace_id = $1"
        
        workspace_result <- tryCatch({
          DBI::dbGetQuery(conn, workspace_query, params = list(user_info$workspace_id, current_user_id))
        }, error = function(e) {
          warning(paste("Failed to query workspace details:", e$message))
          data.frame(workspace_name = "Unknown Workspace", category = "", role = "member")
        })
        
        if (nrow(workspace_result) > 0) {
          list(
            name = workspace_result$workspace_name[1],
            description = paste("Category:", workspace_result$category[1] %||% "Project"),
            color = "#0c5460",           # Darker blue for better contrast
            bg_color = "#e1f5f9",        # Lighter background
            icon = "🚀",
            message = paste("Welcome to", workspace_result$workspace_name[1]),
            type = workspace_result$role[1] %||% "member"
          )
        } else {
          # Fallback for workspace not found in new system
          list(
            name = paste("Workspace", user_info$workspace_id),
            description = "Workspace details unavailable",
            color = "#0d5016",           # Darker green for better contrast
            bg_color = "#e8f5e8",        # Lighter background
            icon = "📁",
            message = paste("You have access to workspace", user_info$workspace_id),
            type = "member"
          )
        }
      } else {
        list(
          name = "No Workspace",
          description = "Contact administrator",
          color = "#721c24",             # Darker red for better contrast
          bg_color = "#fdeaea",          # Lighter background
          icon = "⚠️",
          message = "No workspace assigned",
          type = "none"
        )
      }

      # Update reactiveVal for current user's display name
      display_name <- if (!is.null(user_info$full_name) && nzchar(user_info$full_name)) {
        user_info$full_name
      } else if (!is.null(user_info$display_name) && nzchar(user_info$display_name)) {
        user_info$display_name
      } else {
        current_user_id # Fallback to email
      }
      
      currentuser(display_name)

      # Remove the 'userData' tab as it's not needed for active users
      tryCatch({
        removeTab(inputId = "body_panel_id", target = "userData")
      }, error = function(e) {
        # Non-critical error
      })

      # Render Primary Sidebar Panel with improved colors and contrast
      output$primarysidepanel <- renderUI({
        # Add reactive dependencies for workspace changes
        workspace_list_trigger()
        studies_trigger()
        current_workspace()  # React to workspace changes
        
        # Get current workspace info - make this reactive to workspace changes
        current_workspace_id <- current_workspace()
        user_info <- userData_upload()
        
        # Recalculate workspace info when workspace changes
        workspace_info <- if (!is.null(current_workspace_id) && !is.na(current_workspace_id)) {
          # Query actual workspace details from madi_dat.workspace
          workspace_query <- "
            SELECT w.name as workspace_name, w.category, COALESCE(wa.role, 'member') as role
            FROM madi_dat.workspace w
            LEFT JOIN madi_track.workspace_access wa ON w.workspace_id = wa.workspace_id AND wa.user_email = $2
            WHERE w.workspace_id = $1"
          
          workspace_result <- tryCatch({
            DBI::dbGetQuery(conn, workspace_query, params = list(current_workspace_id, current_user_id))
          }, error = function(e) {
            warning(paste("Failed to query workspace details:", e$message))
            data.frame(workspace_name = "Unknown Workspace", category = "", role = "member")
          })
          
          if (nrow(workspace_result) > 0) {
            # Use role directly without sandbox concept
            workspace_type <- workspace_result$role[1] %||% "member"
            
            list(
              name = workspace_result$workspace_name[1],
              description = paste("Category:", workspace_result$category[1] %||% "Project"),
              color = "#0c5460",           # Darker blue for better contrast
              bg_color = "#e1f5f9",        # Lighter background
              icon = "🚀",
              message = paste("Welcome to", workspace_result$workspace_name[1]),
              type = workspace_type
            )
          } else {
            # Fallback for workspace not found in new system
            list(
              name = paste("Workspace", current_workspace_id),
              description = "Workspace details unavailable",
              color = "#0d5016",           # Darker green for better contrast
              bg_color = "#e8f5e8",        # Lighter background
              icon = "📁",
              message = paste("You have access to workspace", current_workspace_id),
              type = "member"
            )
          }
        } else {
          list(
            name = "No Workspace",
            description = "Contact administrator",
            color = "#721c24",             # Darker red for better contrast
            bg_color = "#fdeaea",          # Lighter background
            icon = "⚠️",
            message = "No workspace assigned",
            type = "none"
          )
        }
        
        div(
          style = "padding: 15px; max-width: 100%; overflow-x: hidden;",
          
          # Add CSS to override Shiny input text colors globally
          tags$style(HTML("
            .radio label, .checkbox label {
              color: #2c3e50 !important;
              font-weight: 500 !important;
            }
            .shiny-input-radiogroup label, 
            .shiny-input-checkboxgroup label {
              color: #2c3e50 !important;
              font-weight: 500 !important;
            }
            .shiny-input-radiogroup .radio-inline label,
            .shiny-input-checkboxgroup .checkbox-inline label {
              color: #2c3e50 !important;
              font-weight: 500 !important;
            }
            .control-label {
              color: #2c3e50 !important;
              font-weight: 600 !important;
            }
          ")),
          
          # Welcome section with improved contrast
          div(
            style = paste0("background-color: ", workspace_info$bg_color, "; padding: 15px; border-radius: 8px; margin-bottom: 20px; border: 2px solid ", workspace_info$color, ";"),
            div(
              style = "display: flex; align-items: center; margin-bottom: 8px;",
              span(workspace_info$icon, style = "font-size: 1.5em; margin-right: 10px;"),
              h5("Access Granted", style = paste0("color: ", workspace_info$color, "; margin: 0; font-weight: bold;"))
            ),
            p(paste("Welcome,", display_name), style = paste0("color: ", workspace_info$color, "; margin: 5px 0; font-size: 1.1em; font-weight: 600;")),
            
            # Workspace-specific messaging with better contrast
            if (workspace_info$type == "none") {
              div(
                style = "background-color: #ffffff; padding: 12px; border-radius: 6px; margin: 10px 0; border: 1px solid #721c24;",
                p("⚠️ No workspace assigned!", style = "color: #721c24; margin: 0; font-weight: 600; font-size: 0.95em;"),
                p("Contact administrator to assign a workspace for data access.", style = "color: #721c24; margin: 5px 0 0 0; font-size: 0.9em;")
              )
            } else {
              div(
                style = "background-color: #ffffff; padding: 12px; border-radius: 6px; margin: 10px 0; border: 1px solid #0c5460;",
                p("🎯 You're in your workspace!", style = "color: #0c5460; margin: 0; font-weight: 600; font-size: 0.95em;"),
                p("Start by adding your studies and data to this workspace.", style = "color: #0c5460; margin: 5px 0 0 0; font-size: 0.9em;")
              )
            },
            
            div(
              style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid rgba(0,0,0,0.2);",
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                span("Workspace:", style = paste0("color: ", workspace_info$color, "; font-size: 0.9em; font-weight: 600;")),
                span(workspace_info$name, style = paste0("color: ", workspace_info$color, "; font-size: 0.9em; font-weight: 500;"))
              )
            )
          ),
          
          # Main action selection with improved colors and white background
          div(
            style = "margin-bottom: 20px;",
            h5("Choose Data Action", 
               style = "color: #ffffff; background-color: #2c3e50; margin-bottom: 15px; font-weight: 600; padding: 10px; border-radius: 5px; text-align: center;"),
            div(
              style = if (workspace_info$type != "none") {
                "background-color: #ffffff; padding: 15px; border-radius: 8px; border: 2px solid #2c3e50;"
              } else {
                "background-color: #ffffff; padding: 15px; border-radius: 8px; border: 2px solid #2c3e50; opacity: 0.6;"
              },
              if (workspace_info$type != "none") {
                radioButtons("rb", NULL,
                             choiceNames = list(
                               div(
                                 style = "display: flex; align-items: center; padding: 8px; background-color: #ffffff;",
                                 tags$i(class = "fa fa-plus-circle", style = "color: #e74c3c; margin-right: 10px; font-size: 1.1em;"),
                                 span("Add New Data", style = "color: #2c3e50; font-weight: 600;")
                               ),
                               div(
                                 style = "display: flex; align-items: center; padding: 8px; background-color: #ffffff;",
                                 tags$i(class = "fa fa-eye", style = "color: #e74c3c; margin-right: 10px; font-size: 1.1em;"),
                                 span("View Study", style = "color: #2c3e50; font-weight: 600;")
                               ),
                               div(
                                 style = "display: flex; align-items: center; padding: 8px; background-color: #ffffff;",
                                 tags$i(class = "fa fa-users", style = "color: #e74c3c; margin-right: 10px; font-size: 1.1em;"),
                                 span("Manage Workspaces", style = "color: #2c3e50; font-weight: 600;")
                               ),
                               div(
                                 style = "display: flex; align-items: center; padding: 8px; background-color: #ffffff;",
                                 tags$i(class = "fa fa-upload", style = "color: #e74c3c; margin-right: 10px; font-size: 1.1em;"),
                                 span("Upload to Immport", style = "color: #2c3e50; font-weight: 600;")
                               )
                             ),
                             choiceValues = list("addData", "viewStudy", "manageWorkspaces", "immportUpload"),
                             selected = "addData"
                )
              } else {
                div(
                  style = "text-align: center; padding: 20px; background-color: #ffffff;",
                  p("🚫 Data actions disabled", style = "color: #6c757d; font-size: 1.1em; margin-bottom: 10px;"),
                  p("Workspace assignment required for data access", style = "color: #6c757d; font-size: 0.9em; margin: 0;")
                )
              }
            )
          ),
          
          # Conditional panels with improved colors and contrast
          if (workspace_info$type != "none") {
            tagList(
              conditionalPanel(
                condition = "input.rb == 'addData'",
                div(
                  style = "background-color: #ffffff; padding: 15px; border-radius: 8px; border: 2px solid #f39c12;",
                  h6("➕ Add New Data", style = "color: #d68910; margin-bottom: 10px; font-weight: 600;"),
                  div(
                    p("🎯 Add your data to your workspace", style = "color: #d68910; font-size: 0.9em; margin-bottom: 10px; font-weight: 500;"),
                    p("Start by creating a new study!", style = "color: #d68910; font-size: 0.9em; font-weight: 600; margin-bottom: 10px;")
                  ),
                  # Enhanced radio buttons with proper text styling
                  div(
                    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
                    tags$style(HTML("
                      #rb_add .radio label {
                        color: #2c3e50 !important;
                        font-weight: 500 !important;
                        font-size: 0.95em !important;
                      }
                    ")),
                    radioButtons("rb_add", 
                                tags$span("Select workflow step:", style = "color: #d68910; font-weight: 600;"),
                                choiceNames = list(
                                  tags$span("Create new study", style = "color: #2c3e50; font-weight: 500;"),
                                  tags$span("Add documents and raw files", style = "color: #2c3e50; font-weight: 500;"),
                                  tags$span("Add templates", style = "color: #2c3e50; font-weight: 500;")
                                ),
                                choiceValues = list("createStudy", "addAssociated", "addData2Study"),
                                selected = character(0)
                    )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.rb == 'immportUpload'",
                div(
                  style = "background-color: #ffffff; padding: 15px; border-radius: 8px; border: 2px solid #16a085;",
                  h6("☁️ Immport Upload", style = "color: #138d75; margin-bottom: 10px; font-weight: 600;"),
                  div(
                    p("🎯 Upload your data to Immport", style = "color: #138d75; font-size: 0.9em; margin-bottom: 10px; font-weight: 500;"),
                    p("Upload data from your workspace to the Immport system", style = "color: #138d75; font-size: 0.9em; margin-bottom: 10px;")
                  ),
                  div(
                    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #16a085;",
                    p("Select your data files and configure upload settings.", style = "color: #138d75; margin: 0; font-size: 0.9em; font-weight: 500;")
                  )
                )
              )
            )
          }
        )
      })

    } else {
      # --- User NOT ACTIVE or Access Denied - DENY ACCESS ---
      print(paste0("DEBUG: *** DENYING ACCESS TO USER - Status: ", user_access_status, " ***"))

      # Determine appropriate message based on status
      message_info <- switch(user_access_status,
        "registered_pending_approval" = list(
          title = "Account Pending Approval",
          message = "Your account has been created but is pending administrator approval.",
          color = "#856404",
          bg_color = "#fff3cd",
          icon = "⏳"
        ),
        "inactive" = list(
          title = "Account Inactive",
          message = "Your account exists but has been deactivated. Please contact the administrator.",
          color = "#721c24",
          bg_color = "#f8d7da",
          icon = "🔒"
        ),
        "registration_failed" = list(
          title = "Registration Failed",
          message = "Failed to create your account. Please contact the administrator.",
          color = "#721c24",
          bg_color = "#f8d7da",
          icon = "❌"
        ),
        default = list(
          title = "Access Denied",
          message = "Unable to determine your access status. Please contact the administrator.",
          color = "#721c24",
          bg_color = "#f8d7da",
          icon = "🚫"
        )
      )

      # Update reactiveVal to show status
      currentuser(paste0(current_user_id, " (", user_access_status, ")"))

      # Render Primary Sidebar Panel (showing access denied message)
      # This is what non-active users see - NO APPLICATION FUNCTIONALITY
      output$primarysidepanel <- renderUI({
        # Add reactive dependencies for workspace changes
        workspace_list_trigger()
        studies_trigger()
        current_workspace()  # React to workspace changes
        
        div(
          style = "padding: 15px; max-width: 100%; overflow-x: hidden;",
          
          # Main status message with improved styling
          div(
            style = paste0("background: linear-gradient(135deg, ", message_info$bg_color, " 0%, ", adjustcolor(message_info$bg_color, alpha.f = 0.8), " 100%); padding: 20px; border-radius: 10px; border-left: 6px solid ", message_info$color, "; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
            div(
              style = "display: flex; align-items: center; margin-bottom: 15px;",
              span(message_info$icon, style = "font-size: 2em; margin-right: 15px;"),
              h4(message_info$title, style = paste0("color: ", message_info$color, "; margin: 0; font-weight: 600;"))
            ),
            p(message_info$message, style = "font-size: 1.1em; margin-bottom: 15px; line-height: 1.4;"),
            
            # Status indicators
            if (user_access_status == "registered_pending_approval") {
              div(
                style = "margin-top: 15px;",
                div(
                  style = "display: flex; align-items: center; margin-bottom: 8px;",
                  tags$i(class = "fa fa-check-circle", style = "color: #28a745; margin-right: 8px;"),
                  span("Authentication successful", style = "color: #28a745; font-weight: 500;")
                ),
                div(
                  style = "display: flex; align-items: center; margin-bottom: 8px;",
                  tags$i(class = "fa fa-check-circle", style = "color: #28a745; margin-right: 8px;"),
                  span("Account created in database", style = "color: #28a745; font-weight: 500;")
                ),
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  tags$i(class = "fa fa-clock-o", style = "color: #856404; margin-right: 8px;"),
                  span("Waiting for administrator approval", style = "color: #856404; font-weight: 500;")
                ),
                div(
                  style = "background-color: rgba(255,255,255,0.7); padding: 12px; border-radius: 6px; border-left: 3px solid #856404;",
                  p("You will receive access once an administrator activates your account.", style = "margin: 0; font-weight: 600;")
                )
              )
            } else if (user_access_status == "inactive") {
              div(
                style = "margin-top: 15px;",
                div(
                  style = "display: flex; align-items: center; margin-bottom: 8px;",
                  tags$i(class = "fa fa-check-circle", style = "color: #28a745; margin-right: 8px;"),
                  span("Authentication successful", style = "color: #28a745; font-weight: 500;")
                ),
                div(
                  style = "display: flex; align-items: center; margin-bottom: 8px;",
                  tags$i(class = "fa fa-check-circle", style = "color: #28a745; margin-right: 8px;"),
                  span("Account found in database", style = "color: #28a745; font-weight: 500;")
                ),
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  tags$i(class = "fa fa-lock", style = "color: #dc3545; margin-right: 8px;"),
                  span("Account is deactivated", style = "color: #dc3545; font-weight: 500;")
                ),
                div(
                  style = "background-color: rgba(255,255,255,0.7); padding: 12px; border-radius: 6px; border-left: 3px solid #dc3545;",
                  p("Please contact the administrator to reactivate your account.", style = "margin: 0; font-weight: 600;")
                )
              )
            } else {
              div(
                style = "background-color: rgba(255,255,255,0.7); padding: 12px; border-radius: 6px; border-left: 3px solid ", message_info$color, ";",
                p("Please contact the administrator with the information below.", style = "margin: 0; font-weight: 600;")
              )
            }
          ),
          
          # User information section with improved styling
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6; margin-bottom: 15px;",
            h6("👤 User Information for Admin", style = "color: #495057; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center;"),
            div(
              style = "background-color: #ffffff; padding: 12px; border-radius: 6px; border: 1px solid #e9ecef;",
              div(
                style = "display: grid; grid-template-columns: auto 1fr; gap: 10px; font-family: 'Courier New', monospace; font-size: 0.9em;",
                span("📧 Email:", style = "font-weight: 600; color: #495057;"),
                span(current_user_id, style = "color: #007bff; word-break: break-all;"),
                span("🔗 ID:", style = "font-weight: 600; color: #495057;"),
                span(current_user_compress, style = "color: #6c757d; word-break: break-all;"),
                span("📊 Status:", style = "font-weight: 600; color: #495057;"),
                span(user_access_status, style = "color: #dc3545; font-weight: 600;"),
                if (nrow(found_user_data) > 0) {
                  tagList(
                    span("🆔 User ID:", style = "font-weight: 600; color: #495057;"),
                    span(found_user_data$users_id[1], style = "color: #28a745;"),
                    span("✅ Active:", style = "font-weight: 600; color: #495057;"),
                    span(as.character(found_user_data$is_active[1]), style = if(found_user_data$is_active[1]) "color: #28a745; font-weight: 600;" else "color: #dc3545; font-weight: 600;"),
                    span("🏢 Workspace:", style = "font-weight: 600; color: #495057;"),
                    span(found_user_data$workspace_id[1] %||% "None", style = "color: #6c757d;")
                  )
                }
              )
            )
          ),
          
          # Action button with improved styling
          div(
            style = "text-align: center;",
            actionButton("show_tech_details", "📋 Show Technical Details", 
                        class = "btn btn-outline-primary", 
                        style = "border-radius: 20px; padding: 8px 20px; font-weight: 500;")
          ),
          
          # Hidden technical details
          div(id = "tech_details", style = "display: none; margin-top: 15px;",
            div(
              style = "background-color: #e9ecef; padding: 15px; border-radius: 8px; border: 1px solid #ced4da;",
              h6("🔧 Technical Details", style = "color: #495057; margin-bottom: 10px;"),
              verbatimTextOutput("select_user_str_display")
            )
          )
        )
      })

      # Add debug info tab for administrators
      tryCatch({
        insertTab(inputId = "body_panel_id",
                  tabPanel(value = "userData",
                           title = "Admin Information",
                           shinydashboard::box(title = "Administrator Actions Required",
                                               width = 12, status = "warning", solidHeader = TRUE,
                                               tags$div(
                                                 style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                                                 h4("⚠️ User Access Management", style = "color: #856404; margin-bottom: 10px;"),
                                                 p("This user requires administrator action to access the application.")
                                               ),
                                               tags$hr(),
                                               tags$h5("User Details:"),
                                               tags$div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 3px; font-family: monospace;",
                                                 tags$pre(paste("Email:", current_user_id)),
                                                 tags$pre(paste("Compressed ID:", current_user_compress)),
                                                 tags$pre(paste("Access Status:", user_access_status)),
                                                 if (nrow(found_user_data) > 0) {
                                                   tagList(
                                                     tags$pre(paste("Database User ID:", found_user_data$users_id[1])),
                                                     tags$pre(paste("Is Active:", found_user_data$is_active[1])),
                                                     tags$pre(paste("Workspace ID:", found_user_data$workspace_id[1] %||% "None")),
                                                     tags$pre(paste("Username:", found_user_data$username[1] %||% "None"))
                                                   )
                                                 }
                                               ),
                                               tags$hr(),
                                               tags$h5("Administrator Actions:"),
                                               tags$div(
                                                 style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                                                 h6("✅ To ACTIVATE this user:", style = "color: #155724;"),
                                                 tags$code(paste0("UPDATE madi_track.users SET is_active = true WHERE oauth_unique_id = '", current_user_compress, "';"),
                                                           style = "background-color: #f8f9fa; padding: 5px; border-radius: 3px; display: block; margin: 5px 0;")
                                               ),
                                               tags$div(
                                                 style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                                                 h6("🏢 To ASSIGN WORKSPACE ACCESS (use workspace management system):", style = "color: #0c5460;"),
                                                 p("Use the 'Workspace Access' tab in the application to manage workspace permissions.", style = "color: #0c5460; font-size: 0.9em; margin: 5px 0;"),
                                                 tags$code("Or grant access directly:", style = "background-color: #f8f9fa; padding: 5px; border-radius: 3px; display: block; margin: 5px 0;"),
                                                 tags$code(paste0("INSERT INTO madi_track.workspace_access (workspace_id, user_email, role, granted_by, granted_at) VALUES (workspace_id, '", current_user_id, "', 'member', 'admin', CURRENT_TIMESTAMP);"),
                                                           style = "background-color: #f8f9fa; padding: 5px; border-radius: 3px; display: block; margin: 5px 0; font-size: 0.8em;")
                                               ),
                                               tags$div(
                                                 style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                                                 h6("❌ To DEACTIVATE this user:", style = "color: #721c24;"),
                                                 tags$code(paste0("UPDATE madi_track.users SET is_active = false WHERE oauth_unique_id = '", current_user_compress, "';"),
                                                           style = "background-color: #f8f9fa; padding: 5px; border-radius: 3px; display: block; margin: 5px 0;")
                                               ),
                                               tags$hr(),
                                               tags$h5("Database Query Details:"),
                                               verbatimTextOutput("select_user_str_display")
                           )
                  ),
                  target = NULL,
                  position = "after",
                  select = TRUE
        )
      }, error = function(e) {
        warning(paste("Failed to insert admin tab:", e$message))
      })
    }
  }, error = function(e) {
    # Global error handler for the entire user observer
    warning(paste("CRITICAL ERROR in user observer:", e$message))
    print(paste("Error details:", toString(e)))
    print("Stack trace:")
    print(traceback())
    
    # Set a safe fallback state
    currentuser("System Error - Contact Admin")
    
    # Render error message in sidebar
    output$primarysidepanel <- renderUI({
      # Add reactive dependencies for workspace changes
      workspace_list_trigger()
      studies_trigger()
      current_workspace()  # React to workspace changes
      
      mainPanel(
        div(
          style = "background-color: #f8d7da; padding: 20px; border-radius: 5px; border-left: 4px solid #dc3545;",
          h4("System Error", style = "color: #721c24;"),
          p("An error occurred while processing your user information."),
          p("Please contact the administrator with the following details:"),
          tags$pre(paste("Error:", e$message), style = "background-color: #f8f9fa; padding: 10px; border-radius: 3px;"),
          tags$pre(paste("Time:", Sys.time()), style = "background-color: #f8f9fa; padding: 10px; border-radius: 3px;")
        )
      )
    })
  })
})


