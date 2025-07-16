# Add new study_personnel tab

observe({
  req(input$rb_add == "add_study_personnel")
  output$add_study_personnel_ui <- renderUI({ #createNewStudy
    tagList(
      br(), 
      box(title = "MADI", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          reactable::reactableOutput("study_personnel_table", width = "100%"),
          actionButton("study_personnel_add_button", "Add", icon("plus")),
          actionButton("study_personnel_edit_button", "Edit", icon("edit"))
      )
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "study_personnel" #createStudy
                     , title = "Add Study Personnel" #Create New Study
                     , uiOutput("add_study_personnel_ui"))) #createNewStudy
})

study_personnel <- reactive({
  study_personnel <- update_db(operation = "select", table_name = "study_personnel")
})

study_personnel_form_selected_row <- reactive(getReactableState("study_personnel_table", "selected"))

output$study_personnel_table <- reactable::renderReactable({
  table_data <- study_personnel()
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
fieldsMandatory_study_personnel <- c("study_personnel.person_accession", "study_personnel.site_name","study_personnel.study_accession","study_personnel.workspace_id")

#define which input fields are mandatory
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_study_personnel,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_study_personnel", condition = mandatoryFilled)
})


entry_form_study_personnel <- function(button_id){
  print("inside study_personnel")
  showModal(
    modalDialog(
      div(id=("entry_form_study_personnel"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              cellWidths = c("250px", "100px"),
              cellArgs = list(style = "vertical-align: top"),
              column(6, 
                     textInput("study_personnel.person_accession", "Person Accession"),
                     textInput("study_personnel.site_name", "Site Name"),
                     textInput("study_personnel.email", "Email"),
                     textInput("study_personnel.first_name", "First Name"),
                     textInput("study_personnel.honorific", "Honorific"),
                     textInput("study_personnel.last_name", "Last Name"),
                     numericInput("study_personnel.workspace_id", "Workspace ID", value = 1),
                     helpText(labelMandatory(""), paste("Mandatory field.")),
                     actionButton(button_id, "Submit")
                     ),
              column(6, 
                     textInput("study_personnel.orcid", "Orcid"),
                     textInput("study_personnel.organization", "organization"),
                     selectInput("study_personnel.role_in_study", "Role in Study", choices = DBI::dbGetQuery(conn, "SELECT DISTINCT(name) FROM madi_dat.lk_personnel_role;")),
                     selectInput("study_personnel.study_accession", "Study Accession", choices = DBI::dbGetQuery(conn, "SELECT DISTINCT(study_accession) FROM madi_dat.study;")),
                     textInput("study_personnel.suffixes", "Suffixes"),
                     textInput("study_personnel.title_in_study", "Title in Study"))
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData <- reactive({
  formData <- tibble(
    person_accession = input$study_personnel.person_accession,
    site_name = input$study_personnel.site_name,
    email = input$study_personnel.email,
    first_name = input$study_personnel.first_name,
    honorific = input$study_personnel.honorific,
    last_name = input$study_personnel.last_name,
    orcid = input$study_personnel.orcid,
    organization = input$study_personnel.organization,
    role_in_study = input$study_personnel.role_in_study,
    study_accession = input$study_personnel.study_accession,
    suffixes = input$study_personnel.suffixes,
    title_in_study = input$study_personnel.title_in_study,
    workspace_id = input$study_personnel.workspace_id
  )
  return(formData)
})

#Add data
append_study_personnel_data <- function(data){
  
  update_db(operation = "insert", db_data = data, table_name = "study_personnel")
  
}

observeEvent(input$study_personnel_add_button, priority = 20,{
  entry_form_study_personnel("submit_study_personnel")
})

observeEvent(input$submit_study_personnel, priority = 20,{
  append_study_personnel_data(formData())
  shinyjs::reset("entry_form_study_personnel")
  removeModal()
})

observeEvent(input$study_personnel_edit_button, priority = 20,{
  SQL_df <- update_db(operation = "select", table_name = "study_personnel")
  print(study_personnel_form_selected_row())
  SQL_df <- SQL_df[study_personnel_form_selected_row(),]
  if(length(study_personnel_form_selected_row()) == 1 ){
    entry_form_study_personnel("submit_study_personnel_edit")
    # updateTextInput(session, "program_id", value = SQL_df[program_form_selected_row(), "program_id"])
    updateTextInput(session, "study_personnel.person_accession", value = SQL_df[["person_accession"]])
    updateTextInput(session, "study_personnel.site_name", value = SQL_df[["site_name"]])
    updateTextInput(session, "study_personnel.email", value = SQL_df[["email"]])
    updateTextInput(session, "study_personnel.first_name", value = SQL_df[["first_name"]])
    updateTextInput(session, "study_personnel.honorific", value = SQL_df[["honorific"]])
    updateTextInput(session, "study_personnel.last_name", value = SQL_df[["last_name"]])
    updateTextInput(session, "study_personnel.orcid", value = SQL_df[["orcid"]])
    updateTextInput(session, "study_personnel.organization", value = SQL_df[["organization"]])
    updateSelectInput(session, "study_personnel.role_in_study", selected = SQL_df[["role_in_study"]])
    updateSelectInput(session, "study_personnel.study_accession", selected = SQL_df[["study_accession"]])
    updateTextInput(session, "study_personnel.suffixes", value = SQL_df[["suffixes"]])
    updateTextInput(session, "study_personnel.title_in_study", value = SQL_df[["title_in_study"]])
    updateNumericInput(session, "study_personnel.workspace_id", value = SQL_df[["workspace_id"]])
  }
})

observeEvent(input$submit_study_personnel_edit, priority = 20, {
  SQL_df <- update_db(operation = "select", table_name = "study_personnel")
  db_person_accession <- SQL_df[study_personnel_form_selected_row(), "person_accession"]
  
  update_db(operation = "update", table_name = "study_personnel", db_data = formData(), update_where = c("person_accession" = db_person_accession))
  
  removeModal()
})
