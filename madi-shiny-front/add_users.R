# Add users 

observe({
  req(input$rb_admin == "add_users")
  output$add_users_ui <- renderUI({
    tagList(
      br(), 
      box(title = "MADI", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          reactable::reactableOutput("users_table", width = "100%"),
          actionButton("users_add_button", "Add", icon("plus")),
          actionButton("users_edit_button", "Edit", icon("edit"))
      )
    )
  })
  #tabs_panel_names <- sapply(tagQuery(ui)$find("#body_panel_id")$find("a")$selectedTags(), function(x){tagGetAttribute(x, "data-value")})
  #print(tabs_panel_names)
  # # Use purrr's walk command to cycle through each
  # # panel tabs and remove them
  # tab_list %>%
  #   walk(~removeTab("tabs", .x))
  # tab_list <<- NULL
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "add_users_tab" #create users
                     , title = "Add Users" #Create New Study
                     , uiOutput("add_users_ui"))) #createNewStudy
  showTab(inputId = "body_panel_id", target = "add_users_tab", select = TRUE, session = getDefaultReactiveDomain())
  
})


users_form <- reactive({
  users_form <- update_db(operation = "select", table_name = "users", schema = "madi_track")
})

users_form_selected_row <- reactive(getReactableState("users_table", "selected"))
selected_row_users <- reactiveVal()

output$users_table <- reactable::renderReactable({
  table_data <- users_form()
  reactable(table_data,
            selection = "single",
            onClick = "select",
            details = colDef(
              name = "",
              details = JS("function(rowInfo) {
                        return `Details for row: ${rowInfo.index + 1}` +
                        `<pre>${JSON.stringify(rowInfo.values, null, 2)}</pre>`
                        }"),
              html = TRUE
            ),
            theme = reactableTheme(
              rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
            ),
            wrap = F)
})


#List of mandatory fields for submission
# Removing "link" and "short_name" from list of mandatory columns
fieldsMandatory_users <- c("users.username","users.full_name", "users.email")

#define which input fields are mandatory
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_users,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_users", condition = mandatoryFilled)
})

entry_form_users <- function(button_id){
  showModal(
    modalDialog(
      div(id=("entry_form_users"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              cellWidths = c("250px", "100px"),
              cellArgs = list(style = "vertical-align: top"),
              textInput("users.username", labelMandatory("Username")),
              textInput("users.full_name", labelMandatory("Full Name")),
              textInput("users.email", labelMandatory("Email")),
              textInput("users.created_by", "Created By"),
              textInput("users.updated_by", "Updated By"),
              textInput("users.oauth_unique_id", "OAuth Unique ID"),
              textInput("users.project", "Project"),
              numericInput("users.workspace_id", "Workspace ID", value = 6101),
              textInput("users.display_name", "Display name"),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

users_formData <- reactive({
  formData <- data.frame(username = input$users.username,
                         full_name = input$users.full_name,
                         email = input$users.email,
                         created_by = input$users.created_by, 
                         updated_by = input$users.updated_by, 
                         oauth_unique_id = input$users.oauth_unique_id,
                         project = input$users.project,
                         workspace_id = input$users.workspace_id, 
                         display_name = input$users.display_name,
                         stringsAsFactors = FALSE)
  
  # Add additional data to complete requirements
  current_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS6%z")
  formData$created_at <- current_timestamp
  formData$updated_at <- current_timestamp
  formData$users_id <- as.numeric(DBI::dbGetQuery(conn, "SELECT MAX(users_id) + 1 as next_users_id FROM madi_track.users;")[[1]])
  return(formData)
})

#Add data
append_users_data <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "users", schema = "madi_track")
}

observeEvent(input$users_add_button, priority = 20,{
  entry_form_users("submit_users")
})

observeEvent(input$submit_users, priority = 20,{
  print(users_formData())
  append_users_data(users_formData())
  shinyjs::reset("entry_form_users")
  removeModal()
})

observeEvent(input$users_edit_button, priority = 20,{
  SQL_df <- update_db(operation = "select", table_name = "users", schema = "madi_track")
  print(users_form_selected_row())
  print(SQL_df)
  test_SQL_df <<- SQL_df
  
  SQL_df <- SQL_df[users_form_selected_row(),]
  selected_row_users(SQL_df)
  if(length(users_form_selected_row()) == 1 ){
    entry_form_users("submit_users_edit")
    
    # updateTextInput(session, "program_id", value = SQL_df[program_form_selected_row(), "program_id"])
    updateTextInput(session, "users.username", value = SQL_df[["username"]])
    updateTextInput(session, "users.full_name", value = SQL_df[["full_name"]])
    updateTextInput(session, "users.email", value = SQL_df[["email"]])
    updateTextInput(session, "users.created_by", value = SQL_df[["created_by"]])
    updateTextInput(session, "users.updated_by", value = SQL_df[["updated_by"]])
    updateTextInput(session, "users.oauth_unique_id", value = SQL_df[["oauth_unique_id"]])
    updateTextInput(session, "users.project", value = SQL_df[["project"]])
    updateNumericInput(session, "users.workspace_id", value = SQL_df[["workspace_id"]])
    updateTextInput(session, "users.display_name", value = SQL_df[["display_name"]])
    
  }
})

observeEvent(input$submit_users_edit, priority = 20, {
  
  # Read the inputs and updated date times from the formData reactive
  updated_data <- users_formData()
  
  # The formData reactive updates the usersID to the next userID. So fetch from the selected users_id
  updated_data$users_id <- selected_row_users()$users_id
  
  print(updated_data)
  
  update_db(operation = "update", table_name = "users",schema = "madi_track", db_data = updated_data, update_where = c("username" = updated_data$username))
  removeModal()
})