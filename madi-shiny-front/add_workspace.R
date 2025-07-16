# Workspace form

observe({
  req(input$rb_admin == "add_workspace")
  output$add_workspace_ui <- renderUI({ #createNewStudy
    tagList(
      br(), 
      box(title = "MADI", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          reactable::reactableOutput("workspace_table", width = "100%"),
          actionButton("workspace_add_button", "Add", icon("plus")),
          actionButton("workspace_edit_button", "Edit", icon("edit"))
      )
    )
  })
  
  # tabs_panel_names <- sapply(tagQuery(ui)$find("#body_panel_id")$find("a")$selectedTags(), function(x){tagGetAttribute(x, "data-value")})
  # print(tabs_panel_names)
  # # Use purrr's walk command to cycle through each
  # # panel tabs and remove them
  # tab_list %>%
  #   walk(~removeTab("tabs", .x))
  # tab_list <<- NULL
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "add_workspace_tab" #create workspace
                     , title = "Add workspace" #Create New Study
                     , uiOutput("add_workspace_ui"))) #createNewStudy
  showTab(inputId = "body_panel_id", target = "add_workspace_tab", select = TRUE, session = getDefaultReactiveDomain())
  
})

workspace_form <- reactive({
  workspace_form <- update_db(operation = "select", table_name = "workspace")
})

workspace_form_selected_row <- reactive(getReactableState("workspace_table", "selected"))
selected_row_workspace <- reactiveVal()

output$workspace_table <- reactable::renderReactable({
  table_data <- workspace_form()
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
fieldsMandatory_workspace <- c("workspace.workspace_id","workspace.category", "workspace.name","workspace.type")

#define which input fields are mandatory
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_workspace,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_workspace", condition = mandatoryFilled)
})

entry_form_workspace <- function(button_id){
  showModal(
    modalDialog(
      div(id=("entry_form_workspace"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              cellWidths = c("250px", "100px"),
              cellArgs = list(style = "vertical-align: top"),
              numericInput("workspace.workspace_id", labelMandatory("Workspace ID"), value = as.numeric(DBI::dbGetQuery(conn, "SELECT MAX(workspace_id)+1 as next_workspace_id FROM madi_dat.workspace;"))),
              textInput("workspace.category", labelMandatory("Category")),
              textInput("workspace.name", labelMandatory("Name")),
              textInput("workspace.type", labelMandatory("Type")),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}


workspace_formData <- reactive({
  formData <- data.frame(workspace_id = input$workspace.workspace_id,
                         category = input$workspace.category,
                         name = input$workspace.name,
                         type = input$workspace.type,
                         stringsAsFactors = FALSE)
  return(formData)
})

#Add data
append_workspace_data <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "workspace")
}

observeEvent(input$workspace_add_button, priority = 20,{
  entry_form_workspace("submit_workspace")
})

observeEvent(input$submit_workspace, priority = 20,{
  print(workspace_formData())
  append_workspace_data(workspace_formData())
  shinyjs::reset("entry_form_workspace")
  removeModal()
})

observeEvent(input$workspace_edit_button, priority = 20,{
  SQL_df <- update_db(operation = "select", table_name = "workspace")
  print(workspace_form_selected_row())
  print(SQL_df)
  
  SQL_df <- SQL_df[workspace_form_selected_row(),]
  selected_row_workspace(SQL_df)
  if(length(workspace_form_selected_row()) == 1 ){
    entry_form_workspace("submit_workspace_edit")
    
    # updateTextInput(session, "program_id", value = SQL_df[program_form_selected_row(), "program_id"])
    updateNumericInput(session, "workspace.workspace_id", value = SQL_df[["workspace_id"]])
    updateTextInput(session, "workspace.category", value = SQL_df[["category"]])
    updateTextInput(session, "workspace.name", value = SQL_df[["name"]])
    updateTextInput(session, "workspace.type", value = SQL_df[["type"]])
    
    
  }
})

observeEvent(input$submit_workspace_edit, priority = 20, {
  
  # Read the inputs and updated date times from the formData reactive
  updated_data <- workspace_formData()
  print(updated_data)
  update_db(operation = "update", table_name = "workspace",db_data = updated_data, update_where = c("workspace_id" = selected_row_workspace()$workspace_id))
  removeModal()
  
})
