# Add new program_2_personnel tab

observe({
  req(input$rb_add == "add_program_2_personnel")
  output$add_personnel_ui <- renderUI({ #createNewStudy
    tagList(
      br(), 
      box(title = "MADI", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          reactable::reactableOutput("program_2_personnel_table", width = "100%"),
          actionButton("program_2_personnel_add_button", "Add", icon("plus")),
          actionButton("program_2_personnel_edit_button", "Edit", icon("edit"))
      )
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "program_2_personnel" #createStudy
                     , title = "Add Program 2 Personnel" #Create New Study
                     , uiOutput("add_personnel_ui"))) #createNewStudy
})

program_2_personnel <- reactive({
  # input$submit_program
  # input$submit_program_edit
  # program <- DBI::dbGetQuery(conn,
  #                            "SELECT program_id, category, description, end_date, link, name, short_name, start_date FROM madi_dat.program;")
  
  program_2_personnel <- update_db(operation = "select", table_name = "program_2_personnel")
})

program_2_personnel_form_selected_row <- reactive(getReactableState("program_2_personnel_table", "selected"))

output$program_2_personnel_table <- reactable::renderReactable({
  table_data <- program_2_personnel()
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
fieldsMandatory_program_2_personnel <- c("program_2_personnel.program_id", "program_2_personnel.personnel_id")

#define which input fields are mandatory
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_program_2_personnel,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_program_2_personnel", condition = mandatoryFilled)
})

entry_form_program_2_personnel <- function(button_id){
  print("inside program2personnel")
  showModal(
    modalDialog(
      div(id=("entry_form_program_2_personnel"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              cellWidths = c("250px", "100px"),
              cellArgs = list(style = "vertical-align: top"),
              selectInput("program_2_personnel.program_id", "Program id", choices = DBI::dbGetQuery(conn, "SELECT DISTINCT(program_id) FROM madi_dat.program;")),
              selectInput("program_2_personnel.personnel_id", "Personnel id", choices = DBI::dbGetQuery(conn, "SELECT DISTINCT(personnel_id) FROM madi_dat.personnel;")),
              selectInput("program_2_personnel.role_type", "Role Type", choices = DBI::dbGetQuery(conn, "SELECT DISTINCT(name) FROM madi_dat.lk_user_role_type;")),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData <- reactive({
  formData <- data.frame(program_id = as.numeric(input$program_2_personnel.program_id),
                         personnel_id = as.numeric(input$program_2_personnel.personnel_id),
                         role_type = input$program_2_personnel.role_type,
                         stringsAsFactors = FALSE)
  return(formData)
})

#Add data
append_program_2_personnel_data <- function(data){

  update_db(operation = "insert", db_data = data, table_name = "program_2_personnel")
  
}

observeEvent(input$program_2_personnel_add_button, priority = 20,{
  entry_form_program_2_personnel("submit_program_2_personnel")
})

observeEvent(input$submit_program_2_personnel, priority = 20,{
  append_program_2_personnel_data(formData())
  shinyjs::reset("entry_form_program_2_personnel")
  removeModal()
})

observeEvent(input$program_2_personnel_edit_button, priority = 20,{
  SQL_df <- update_db(operation = "select", table_name = "program_2_personnel")
  print(program_2_personnel_form_selected_row())
  SQL_df <- SQL_df[program_2_personnel_form_selected_row(),]
  if(length(program_2_personnel_form_selected_row()) == 1 ){
    entry_form_program_2_personnel("submit_program_2_personnel_edit")
    # updateTextInput(session, "program_id", value = SQL_df[program_form_selected_row(), "program_id"])
    updateSelectInput(session, "program_2_personnel.program_id", selected = SQL_df[["program_id"]])
    updateSelectInput(session, "program_2_personnel.personnel_id", selected = SQL_df[["personnel_id"]])
    updateSelectInput(session, "program_2_personnel.role_type", selected = SQL_df[["role_type"]])
  }
})

observeEvent(input$submit_program_2_personnel_edit, priority = 20, {

  updated_data <- tibble(
    program_id = input$program_2_personnel.program_id,
    personnel_id = input$program_2_personnel.personnel_id,
    role_type = input$program_2_personnel.role_type
  )
  # print(updated_data)
  update_db(operation = "update", table_name = "program_2_personnel", db_data = updated_data, update_where = c("personnel_id" = input$program_2_personnel.personnel_id))
  
  removeModal()
})
