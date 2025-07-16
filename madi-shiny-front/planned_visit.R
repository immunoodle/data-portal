### madi_dat.planned_visit

plannedvisit_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createPlannedVist <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("plannedvisit_table"), #newstudy_table
                          actionButton("add_button_pv", "Add", icon("plus")),
                          actionButton("edit_button_pv", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_pv", "Close Tab", icon("xmark"))
    )
  })
  # insertTab(inputId = "body_panel_id",
  #           tabPanel(value = "PlannedVisitPanel" #createStudy
  #                    , title = "Planned Visit" #Create New Study
  #                    , uiOutput("createPlannedVist"))) #createNewStudy
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
plannedvisit <- reactive({
  input$submit_add_pv
  input$submit_edit_pv
  plannedvisit <- update_db(operation = "select", table_name = "planned_visit", select_where = list("study_accession" = selected_study_accession()))
})

#List of mandatory fields for submission
fieldsMandatory_pv <- c("planned_visit_accession", "order_number", "workspace_id")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_pv,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_pv", condition = mandatoryFilled)
})

output$plannedvisit_table <- DT::renderDataTable({ #newstudy_table
  table <- plannedvisit()[, -c(2:4)] #newstudy
  names(table) <- c("planned_visit_accession", "name", 
                    "order_number", "start_rule", 
                    "study_accession",  "workspace_id"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_pv <- function(button_id_pv){
  showModal(
    modalDialog(
      div(id=("entry_form_pv"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              column(
                width = 6,
                disabled(
                  textInput("planned_visit_accession", labelMandatory("Planned Visit Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT('PV',MAX(CAST(SUBSTRING(planned_visit_accession,3) AS INTEGER))+1) AS next_planned_visit_accession FROM madi_dat.planned_visit;"))
                ),
                textInput("end_rule", "End Rule", placeholder = ""),
                numericInput("max_start_day", "Max Start Day", value = NA),
                numericInput("min_start_day", "Min Start Day", value = NA),
                textInput("name", "Name", placeholder = "")
              ),
              column(
                width = 6,
                numericInput("order_number", labelMandatory("Order Number"), value = NA),
                textInput("start_rule", "Start Rule", placeholder = ""),
                disabled(
                  textInput("study_accession", "Study Accession"),
                  textInput("workspace_id", labelMandatory("Workspace ID"))
                )
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_pv, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_pv <- reactive({
  formData_pv <- data.frame(planned_visit_accession = input$planned_visit_accession,
                            end_rule = input$end_rule,
                            max_start_day = input$max_start_day,
                            min_start_day = input$min_start_day,
                            name = input$name,
                            order_number = input$order_number,
                            start_rule = input$start_rule,
                            study_accession = input$study_accession,
                            workspace_id = input$workspace_id,
                            stringsAsFactors = FALSE)
  return(formData_pv)
})

#Add data
appendData_pv <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "planned_visit")
}

observeEvent(input$add_button_pv, priority = 20,{
  entry_form_pv("submit_add_pv")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
  updateTextInput(session, "study_accession", value = paste(selected_study_accession()))
})

observeEvent(input$submit_add_pv, priority = 20,{
  appendData_pv(formData_pv())
  shinyjs::reset("entry_form_pv")
  removeModal()
})

observeEvent(input$plannedvisit_table_rows_selected, {
  plannedvisit_selected_row(plannedvisit()[input$plannedvisit_table_rows_selected,"planned_visit_accession"])
})

#edit data
observeEvent(input$edit_button_pv, priority = 20,{
  
  SQL_df <- update_db(operation = "select", table_name = "planned_visit", select_where = list("planned_visit_accession" = plannedvisit_selected_row()))
  print(paste("The selected planned visit accession is", plannedvisit_selected_row()))
  
  showModal(
    if(length(input$plannedvisit_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$plannedvisit_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$plannedvisit_table_rows_selected) == 1 ){
    entry_form_pv("submit_edit_pv")
    updateTextInput(session, "planned_visit_accession", value = SQL_df[, "planned_visit_accession"])
    updateTextInput(session, "end_rule", value = SQL_df[, "end_rule"])
    updateNumericInput(session, "max_start_day", value = SQL_df[, "max_start_day"])
    updateNumericInput(session, "min_start_day", value = SQL_df[, "min_start_day"])
    updateTextInput(session, "name", value = SQL_df[, "name"])
    updateNumericInput(session, "order_number", value = SQL_df[, "order_number"])
    updateTextInput(session, "start_rule", value = SQL_df[, "start_rule"])
    updateTextInput(session, "study_accession", value = SQL_df[, "study_accession"])
    updateTextInput(session, "workspace_id", value = SQL_df[, "workspace_id"])
  }
})

observeEvent(input$submit_edit_pv, priority = 20, {
  
  updated_data <- tibble(
    planned_visit_accession = input$planned_visit_accession,
    end_rule = input$end_rule,
    max_start_day = input$max_start_day,
    min_start_day = input$min_start_day,
    name = input$name,
    order_number = input$order_number,
    start_rule = input$start_rule, 
    study_accession = input$study_accession,
    workspace_id = input$workspace_id,
  )
  
  update_db(operation = "update", table_name = "planned_visit", db_data = updated_data, update_where = c("planned_visit_accession" = plannedvisit_selected_row()))
  
  removeModal()
})

# close tab
observeEvent(input$close_button_pv, {
  
  deselect<-which(components()[, "table_name"] == "planned_visit")
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
  
  removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
})
