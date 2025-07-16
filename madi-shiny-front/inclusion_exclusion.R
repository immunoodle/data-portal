### inclusion_exclusion

incluexclu_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createIncluExclu <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("incluexclu_table"), #newstudy_table
                          actionButton("add_button_iec", "Add", icon("plus")),
                          actionButton("edit_button_iec", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_iec", "Close Tab", icon("xmark"))
    )
  })
  # insertTab(inputId = "body_panel_id",
  #           tabPanel(value = "InclusionExclusionPanel" #createStudy
  #                    , title = "Inclusion Exclusion" #Create New Study
  #                    , uiOutput("createIncluExclu"))) #createNewStudy
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
incluexclu <- reactive({
  input$submit_add_iec
  input$submit_edit_iec
  incluexclu <- update_db(operation = "select", table_name = "inclusion_exclusion", select_where = list("study_accession" = selected_study_accession()))
})

#List of mandatory fields for submission
fieldsMandatory_iec <- c("criterion_accession", "study_accession", "workspace_id")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_iec,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_iec", condition = mandatoryFilled)
})

output$incluexclu_table <- DT::renderDataTable({ #newstudy_table
  table <- incluexclu()
  names(table) <- c("criterion_accession", "criterion", "criterion_category", 
                    "study_accession", "workspace_id"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_iec <- function(button_id_iec){
  showModal(
    modalDialog(
      div(id=("entry_form_iec"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              column(
                width = 6,
                disabled(
                  textInput("criterion_accession", labelMandatory("Criterion Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT('CRIT',MAX(CAST(SUBSTRING(criterion_accession,5) AS INTEGER))+1) AS next_criterion_accession FROM madi_dat.inclusion_exclusion;"))
                ),
                textInput("criterion", "Criterion", placeholder = ""),
                selectizeInput("criterion_category", labelMandatory("Criterion Category"), options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn," SELECT name FROM madi_dat.lk_criterion_category;"))
              ),
              column(
                width = 6,
                disabled(
                  textInput("study_accession", labelMandatory("Study Accession")),
                  textInput("workspace_id", labelMandatory("Workspace ID"))
                )
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_iec, "Submit")
            )
          ),
          easyClose = TRUE
      )
    )
  )
}

#save form data into data_frame format
formData_iec <- reactive({
  formData_iec <- data.frame(
    criterion_accession = input$criterion_accession,
    criterion = input$criterion,
    criterion_category = input$criterion_category,
    study_accession = input$study_accession,
    workspace_id = input$workspace_id,
    stringsAsFactors = FALSE)
  return(formData_iec)
})

#Add data
appendData_iec <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "inclusion_exclusion")
}

observeEvent(input$add_button_iec, priority = 20,{
  entry_form_iec("submit_add_iec")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
  updateTextInput(session, "study_accession", value = paste(selected_study_accession()))
})

observeEvent(input$submit_add_iec, priority = 20,{
  appendData_iec(formData_iec())
  shinyjs::reset("entry_form_iec")
  removeModal()
})

observeEvent(input$incluexclu_table_rows_selected, {
  incluexclu_selected_row(incluexclu()[input$incluexclu_table_rows_selected,"criterion_accession"])
})

#edit data
observeEvent(input$edit_button_iec, priority = 20,{
  
  SQL_df <- update_db(operation = "select", table_name = "inclusion_exclusion", select_where = list("criterion_accession" = incluexclu_selected_row()))
  print(paste("The selected criterion accession is", incluexclu_selected_row()))
  
  showModal(
    if(length(input$incluexclu_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$incluexclu_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$incluexclu_table_rows_selected) == 1 ){
    entry_form_iec("submit_edit_iec")
    updateTextInput(session, "criterion_accession", value = SQL_df[, "criterion_accession"])
    updateTextInput(session, "criterion", value = SQL_df[, "criterion"])
    updateTextInput(session, "criterion_category", value = SQL_df[, "criterion_category"])
    updateTextInput(session, "study_accession", value = SQL_df[, "study_accession"])
    updateTextInput(session, "workspace_id", value = SQL_df[, "workspace_id"])
  }
})

observeEvent(input$submit_edit_iec, priority = 20, {
  
  updated_data <- tibble(
    criterion_accession = input$criterion_accession,
    criterion = input$criterion,
    criterion_category = input$criterion_category,
    study_accession = input$study_accession,
    workspace_id = input$workspace_id,
  )
  
  update_db(operation = "update", table_name = "inclusion_exclusion", db_data = updated_data, update_where = c("criterion_accession" = incluexclu_selected_row()))
  
  removeModal()
})

# close tab
observeEvent(input$close_button_iec, {
  
  deselect<-which(components()[, "table_name"] == "inclusion_exclusion")
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
  
  removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
})
