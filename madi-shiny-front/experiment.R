### madi_dat.experiment

exptb_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createExperiment <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("experiment_table"), #newstudy_table
                          actionButton("add_button_exp", "Add", icon("plus")),
                          actionButton("edit_button_exp", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_exp", "Close Tab", icon("xmark"))
    )
  })
  # insertTab(inputId = "body_panel_id",
  #           tabPanel(value = "ExperimentPanel" #createStudy
  #                    , title = "Experiment" #Create New Study
  #                    , uiOutput("createExperiment"))) #createNewStudy
  
} )

#Label mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

#load users and make reactive to inputs  
exptb <- reactive({
  input$submit_add_exp
  input$submit_edit_exp
  exptb <- update_db(operation = "select", table_name = "experiment", select_where = list("study_accession" = selected_study_accession()))
})

#List of mandatory fields for submission
fieldsMandatory_exp <- c("experiment_accession", "measurement_technique", "workspace_id")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_exp,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_exp", condition = mandatoryFilled)
  
  output$experiment_table <- DT::renderDataTable({
    table <- exptb()
    names(table) <- c("experiment_accession", "description", "measurement_technique", 
                      "name", "study_accession", "workspace_id"
    )
    table <- datatable(table, 
                       rownames = FALSE,
                       options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  })
})

# Form for data entry
entry_form_exp <- function(button_id_exp){
  showModal(
    modalDialog(
      div(id=("entry_form_exp"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              column(
                width = 6,
                disabled(
                  textInput("experiment_accession", labelMandatory("Experiment Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT('EXP88',MAX(CAST(SUBSTRING(experiment_accession,6) AS INTEGER))+1) AS next_experiment_accession FROM madi_dat.experiment;"))
                ),
                textInput("description", "Description", placeholder = ""),
                selectizeInput("measurement_technique", labelMandatory("Measurement Technique"), options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn," SELECT name FROM madi_dat.lk_exp_measurement_tech;"))
              ),
              column(
                width = 6,
                textInput("name", "Name", placeholder = ""),
                disabled(
                  textInput("study_accession", "Study Accession"),
                  textInput("workspace_id", labelMandatory("Workspace ID"))
                )
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_exp, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_exp <- reactive({
  formData_exp <- data.frame(experiment_accession = input$experiment_accession,
                             description = input$description,
                             measurement_technique = input$measurement_technique,
                             name = input$name,
                             study_accession = input$study_accession,
                             workspace_id = input$workspace_id,
                             stringsAsFactors = FALSE)
  return(formData_exp)
})

#Add data
appendData_exp <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "experiment")
}

observeEvent(input$add_button_exp, priority = 20,{
  entry_form_exp("submit_add_exp")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
  updateTextInput(session, "study_accession", value = paste(selected_study_accession()))
})

observeEvent(input$submit_add_exp, priority = 20,{
  appendData_exp(formData_exp())
  shinyjs::reset("entry_form_exp")
  removeModal()
})

observeEvent(input$experiment_table_rows_selected, {
  exptb_selected_row(exptb()[input$experiment_table_rows_selected, "experiment_accession"])
})

#edit data
observeEvent(input$edit_button_exp, priority = 20,{
  
  SQL_df <- update_db(operation = "select", table_name = "experiment", select_where = list("experiment_accession" = exptb_selected_row()))
  print(paste("The selected experiment accession is", exptb_selected_row()))
  
  showModal(
    if(length(input$experiment_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$experiment_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$experiment_table_rows_selected) == 1 ){
    entry_form_exp("submit_edit_exp")
    updateTextInput(session, "experiment_accession", value = SQL_df[, "experiment_accession"])
    updateTextInput(session, "description", value = SQL_df[, "description"])
    updateTextInput(session, "measurement_technique", value = SQL_df[, "measurement_technique"])
    updateTextInput(session, "name", value = SQL_df[, "name"])
    updateTextInput(session, "study_accession", value = SQL_df[, "study_accession"])
    updateTextInput(session, "workspace_id", value = SQL_df[, "workspace_id"])
  }
})

observeEvent(input$submit_edit_exp, priority = 20, {
  
  updated_data <- tibble(
    experiment_accession = input$experiment_accession,
    description = input$description,
    measurement_technique = input$measurement_technique,
    name = input$name,
    study_accession = input$study_accession,
    workspace_id = input$workspace_id,
  )
  
  update_db(operation = "update", table_name = "experiment", db_data = updated_data, update_where = c("experiment_accession" = exptb_selected_row()))
  
  removeModal()
})

# close tab
observeEvent(input$close_button_exp, {
  
  deselect<-which(components()[, "table_name"] == "experiment")
  print(paste("deselect", deselect))
  
  selectedRowIndex <- input$report_list_table_rows_selected
  print(paste("selectedRowIndex", selectedRowIndex))
  
  # Check if a row is selected
  if (!is.null(selectedRowIndex)) {
    # Deselect the row
    remained_selected <- selectedRowIndex[-which(selectedRowIndex == deselect)]
    print(paste("remained", remained_selected))
    
    selectedRow(remained_selected)
    afterselected<-selectedRow()
    print(paste("after", afterselected))
  }
  
  removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
})