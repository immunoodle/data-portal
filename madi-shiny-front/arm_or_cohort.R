### arm or cohort tab

armcohort_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createArmCohort <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          DT::dataTableOutput("armcohort_table"), #newstudy_table
          actionButton("add_button_ac", "Add", icon("plus")),
          actionButton("edit_button_ac", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_ac", "Close Tab", icon("xmark"))
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
armcohort <- reactive({
  input$submit_add_ac
  input$submit_edit_ac
  armcohort <- update_db(operation = "select", table_name = "arm_or_cohort", select_where = list("study_accession" = selected_study_accession()))
})

#List of mandatory fields for submission
fieldsMandatory_ac <- c("arm_accession", "study_accession", "workspace_id")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_ac,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_ac", condition = mandatoryFilled)
})

output$armcohort_table <- DT::renderDataTable({ #newstudy_table
  table <- armcohort()[,-6] # type preferred is hidden
  names(table) <- c("arm_accession", "description", "name", "study_accession", 
                    "type_reported", "workspace_id")
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_ac <- function(button_id_ac){
  showModal(
    modalDialog(
      div(id=("entry_form_ac"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              column(
                width = 6,
                disabled(
                  textInput("arm_accession", labelMandatory("Arm Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT('ARM',MAX(CAST(SUBSTRING(arm_accession,4) AS INTEGER))+1) AS next_arm_accession FROM madi_dat.arm_or_cohort;"))
                ),
                textInput("description", "Description", placeholder = ""),
                textInput("name", "Name", placeholder = ""),
                disabled(
                  textInput("study_accession", labelMandatory("Study Accession"))
                )
              ),
              column(
                width = 6,
                textInput("type_reported", "Type Reported", placeholder = ""),
                selectizeInput("type_preferred", "Type Preferred", options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_arm_type;")),
                disabled(
                  textInput("workspace_id", labelMandatory("workspace_id"))
                )
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_ac, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_ac <- reactive({
  formData_ac <- data.frame(
                         study_accession = input$study_accession,
                         arm_accession = input$arm_accession,
                         description = input$description,
                         name = input$name,
                         type_reported = input$type_reported,
                         type_preferred = input$type_preferred,
                         workspace_id = input$workspace_id,
                         stringsAsFactors = FALSE)
  return(formData_ac)
})

#Add data
appendData_ac <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "arm_or_cohort")
}

observeEvent(input$add_button_ac, priority = 20,{
  entry_form_ac("submit_add_ac")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
  updateTextInput(session, "study_accession", value = paste(selected_study_accession()))
})

observeEvent(input$submit_add_ac, priority = 20,{
  appendData_ac(formData_ac())
  shinyjs::reset("entry_form_ac")
  removeModal()
})

observeEvent(input$armcohort_table_rows_selected, {
  armcohort_selected_row(armcohort()[input$armcohort_table_rows_selected,"arm_accession"])
})
  
#edit data
observeEvent(input$edit_button_ac, priority = 20,{
  
  SQL_df <- update_db(operation = "select", table_name = "arm_or_cohort", select_where = list("arm_accession" = armcohort_selected_row()))
  print(paste("The selected arm accession is", armcohort_selected_row()))
  
  showModal(
    if(length(input$armcohort_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$armcohort_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$armcohort_table_rows_selected) == 1 ){
    entry_form_ac("submit_edit_ac")
    updateTextInput(session, "arm_accession", value = SQL_df[, "arm_accession"])
    updateTextInput(session, "description", value = SQL_df[, "description"])
    updateTextInput(session, "name", value = SQL_df[, "name"])
    updateTextInput(session, "study_accession", value = SQL_df[, "study_accession"])
    updateTextInput(session, "type_reported", value = SQL_df[, "type_reported"])
    updateTextInput(session, "type_preferred", value = SQL_df[, "type_preferred"])
    updateTextInput(session, "workspace_id", value = SQL_df[, "workspace_id"])
  }
})

observeEvent(input$submit_edit_ac, priority = 20, {

  updated_data <- tibble(
    study_accession = input$study_accession,
    arm_accession = input$arm_accession,
    description = input$description,
    name = input$name,
    type_reported = input$type_reported,
    type_preferred = input$type_preferred,
    workspace_id = input$workspace_id,
  )
  
  update_db(operation = "update", table_name = "arm_or_cohort", db_data = updated_data, update_where = c("arm_accession" = armcohort_selected_row()))
  
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

observeEvent(input$close_button_ac, {
  
  deselect<-which(components()[, "table_name"] == "arm_or_cohort")
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
