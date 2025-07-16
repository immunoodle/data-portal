### study link

studylink_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createStudyLink <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("studylink_table"), #newstudy_table
                          actionButton("add_button_sl", "Add", icon("plus")),
                          actionButton("edit_button_sl", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_sl", "Close Tab", icon("xmark"))
    )
  })
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
studylink <- reactive({
  input$submit_add_sl
  input$submit_edit_sl
  studylink <- update_db(operation = "select", table_name = "study_link", select_where = list("study_accession" = selected_study_accession()))
})

#List of mandatory fields for submission
fieldsMandatory_sl <- c("study_link_id", "study_accession", "workspace_id")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_sl,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_sl", condition = mandatoryFilled)
})

output$studylink_table <- DT::renderDataTable({ #newstudy_table
  table <- studylink() 
  names(table) <- c("study_link_id", "name", "study_accession", "type",
                    "value", "workspace_id")
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_sl <- function(button_id_sl){
  showModal(
    modalDialog(
      div(id=("entry_form_sl"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              column(
                width = 6,
                disabled(
                  textInput("study_link_id", labelMandatory("Study Link ID"), value=DBI::dbGetQuery(conn,"SELECT CONCAT(MAX(CAST(study_link_id AS INTEGER))+1) AS next_study_link_id FROM madi_dat.study_link;"))
                ),
                textInput("name", "Name", placeholder = ""),
                disabled(
                  textInput("study_accession", labelMandatory("Study Accession"))
                )
              ),
              column(
                width = 6,
                textInput("type", "Type", placeholder = ""),
                textInput("value", "Value", placeholder = ""),
                disabled(
                  textInput("workspace_id", labelMandatory("workspace_id"))
                )
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_sl, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_sl <- reactive({
  formData_sl <- data.frame(
    study_link_id = input$study_link_id,
    name = input$name,
    study_accession = input$study_accession,
    type = input$type,
    value = input$value,
    workspace_id = input$workspace_id,
    stringsAsFactors = FALSE)
  return(formData_sl)
})

#Add data
appendData_sl <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "study_link")
}

observeEvent(input$add_button_sl, priority = 20,{
  entry_form_sl("submit_add_sl")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
  updateTextInput(session, "study_accession", value = paste(selected_study_accession()))
})

observeEvent(input$submit_add_sl, priority = 20,{
  appendData_sl(formData_sl())
  shinyjs::reset("entry_form_sl")
  removeModal()
})

observeEvent(input$studylink_table_rows_selected, {
  studylink_selected_row(studylink()[input$studylink_table_rows_selected,"study_link_id"])
})

#edit data
observeEvent(input$edit_button_sl, priority = 20,{
  
  SQL_df <- update_db(operation = "select", table_name = "study_link", select_where = list("study_link_id" = studylink_selected_row()))
  print(paste("The selected study link id is", studylink_selected_row()))
  
  showModal(
    if(length(input$studylink_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$studylink_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$studylink_table_rows_selected) == 1 ){
    entry_form_sl("submit_edit_sl")
    updateTextInput(session, "study_link_id", value = SQL_df[, "study_link_id"])
    updateTextInput(session, "name", value = SQL_df[, "name"])
    updateTextInput(session, "study_accession", value = SQL_df[, "study_accession"])
    updateTextInput(session, "type", value = SQL_df[, "type"])
    updateTextInput(session, "value", value = SQL_df[, "value"])
    updateTextInput(session, "workspace_id", value = SQL_df[, "workspace_id"])
  }
})

observeEvent(input$submit_edit_sl, priority = 20, {
  
  updated_data <- tibble(
    study_link_id = input$study_link_id,
    name = input$name,    
    study_accession = input$study_accession,
    type = input$type,
    value = input$value,
    workspace_id = input$workspace_id,
  )
  
  update_db(operation = "update", table_name = "study_link", db_data = updated_data, update_where = c("study_link_id" = studylink_selected_row()))
  
  removeModal()
})

# close tab

# formname <- reactive({
#   formname <- "arm_or_cohort"
# })
# 
# panelname <- reactive({
#   panelname <- "ArmCohortPanel"
# })
# 
# paneltitle <- reactive({
#   paneltitle <- "Arm or Cohort"
# })
# 
# paneluiop <- reactive({
#   paneluiop <- "createArmCohort"
# })

# insert_the_tab <- function(){
#   insertTab(inputId = "body_panel_id",
#               tabPanel(value = "ArmCohortPanel" #createStudy
#                        , title = "Arm or Cohort" #Create New Study
#                        , uiOutput("createArmCohort"))) #createNewStudy
# }

observeEvent(input$close_button_sl, {
  
  deselect<-which(components()[, "table_name"] == "study_link")
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
  
  removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
})
