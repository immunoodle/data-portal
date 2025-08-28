### condition or disease tab - madi_dat.study_2_condition_or_disease

# conditiondisease_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createConditionDisease <- renderUI({  #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          DT::dataTableOutput("condition_or_disease_table"), #newstudy_table
          actionButton("add_button_cd", "Add", icon("plus")),
          actionButton("edit_button_cd", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_cd", "Close Tab", icon("xmark"))
    )
  })
  # insertTab(inputId = "body_panel_id",
  #           tabPanel(value = "ConditionDiseasePanel" #createStudy
  #                    , title = "Condition or Disease" #Create New Study
  #                    , uiOutput("createConditionDisease"))) #createNewStudy
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
# conditiondisease <- reactive({ #newstudy
#   input$submit_cd
#   input$submit_edit_cd
#   cd_query<-paste0("SELECT study.study_accession, condition_reported, condition_preferred
# 	FROM madi_dat.study_2_condition_or_disease 
# 	INNER JOIN madi_dat.study
#   ON study.study_accession = study_2_condition_or_disease.study_accession
#   WHERE study.workspace_id IN (6101,6102,6103,6104,6105)
#   AND study_2_condition_or_disease.study_accession IN ('", selected_study_accession(), "');")
#   conditiondisease <- DBI::dbGetQuery(conn, cd_query)
# }) 
conditiondisease <- reactive({
  input$submit_add_cd
  input$submit_edit_cd
  conditiondisease <- update_db(operation = "select", table_name = "study_2_condition_or_disease", select_where = list("study_accession" = selected_study_accession()))
})

#List of mandatory fields for submission
fieldsMandatory_cd <- c("study_accession", "condition_reported")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_cd,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_cd", condition = mandatoryFilled)
})

output$condition_or_disease_table <- DT::renderDataTable({ #newstudy_table
  table <- conditiondisease()[,-3] #hide condition_preferred
  names(table) <- c("study_accession", "condition_reported")
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_cd <- function(button_id_cd){
  showModal(
    modalDialog(
      div(id=("entry_form_cd"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              disabled(
                textInput("study_accession", labelMandatory("Study Accession"))
              ),
              textInput("condition_reported", "Condition Reported", placeholder = ""),
              selectizeInput("condition_preferred", "Condition Preferred", options = list(
                onInitialize = I('function() { this.setValue(""); }')
              ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_disease;"))
            ),
            
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_cd, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
}

rv_selected_disease<-reactiveValues()
observeEvent(input$condition_reported, priority = 20,{
  if (input$condition_reported == "Other Disease") {
    rv_selected_disease=input$otherinput
  } else {
    rv_selected_disease=input$condition_reported
  }
})

# selected_disease <- reactive({
#   if (input$condition_reported == "Other Disease") {
#     return(input$otherinput)
#   } else {
#     return(input$condition_reported)
#   }
# })

#save form data into data_frame format
formData_cd <- reactive({
  formData_cd <- data.frame(
                         study_accession = input$study_accession,
                         # condition_reported = rv_selected_disease,
                         condition_reported = input$condition_reported,
                         condition_preferred = input$condition_preferred,
                         stringsAsFactors = FALSE)
  return(formData_cd)
})

#Add data
# appendData_cd <- function(data){
#   dbSendQuery(conn,paste0("INSERT INTO study_2_condition_or_disease 
#   (study_accession, condition_reported, condition_preferred)
# 	VALUES ('", input$study_accession, 
#                           "', '", rv_selected_disease, 
#                           "', '", input$condition_preferred, 
#                           "');"))
# }
appendData_cd <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "study_2_condition_or_disease")
}

observeEvent(input$add_button_cd, priority = 20,{
  entry_form_cd("submit_add_cd")
})

observeEvent(input$submit_add_cd, priority = 20,{
  appendData_cd(formData_cd())
  shinyjs::reset("entry_form_cd")
  removeModal()
})

# observeEvent(input$condition_or_disease_table_rows_selected, {
#   conditiondisease_selected_row(conditiondisease()[input$condition_or_disease_table_rows_selected,"arm_accession"])
# })

#edit data
observeEvent(input$edit_button_cd, priority = 20,{
#   SQL_df <- DBI::dbGetQuery(conn, "SELECT study.study_accession, condition_reported, condition_preferred
# 	FROM madi_dat.study_2_condition_or_disease 
# 	INNER JOIN madi_dat.study
#   ON study.study_accession = study_2_condition_or_disease.study_accession
#   WHERE workspace_id IN (6101,6102,6103,6104,6105);")
  
  SQL_df <- update_db(operation = "select", table_name = "study_2_condition_or_disease", select_where = list("study_accession" = armcohort_selected_row()))
  # print(paste("The selected arm accession is", armcohort_selected_row()))
  
  showModal(
    if(length(input$condition_or_disease_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$condition_or_disease_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$condition_or_disease_table_rows_selected) == 1 ){
    entry_form_cd("submit_edit_cd")
    
    # Get user's workspace ID for study choices
    user_workspace <- session$userData$user_workspace_id
    study_choices <- if (!is.null(user_workspace)) {
      DBI::dbGetQuery(conn,"SELECT study.study_accession 
    FROM madi_dat.study_2_condition_or_disease 
	  INNER JOIN madi_dat.study
    ON study.study_accession = study_2_condition_or_disease.study_accession
    WHERE workspace_id = $1;", params = list(user_workspace))
    } else {
      character(0)
    }
    
    updateSelectInput(session, "study_accession", choices = study_choices)
    updateTextInput(session, "condition_reported", value = SQL_df[input$condition_or_disease_table_rows_selected, "condition_reported"])
    updateTextInput(session, "condition_preferred", value = SQL_df[input$condition_or_disease_table_rows_selected, "condition_preferred"])
  }
})

observeEvent(input$submit_edit_cd, priority = 20, {
  # Get user's workspace ID from session data
  user_workspace <- session$userData$user_workspace_id
  if (is.null(user_workspace)) {
    showNotification("No workspace assigned to user", type = "error")
    return()
  }
  
  getQuery_str <- "SELECT study.study_accession, condition_reported, condition_preferred
	FROM madi_dat.study_2_condition_or_disease 
	INNER JOIN madi_dat.study
  ON study.study_accession = study_2_condition_or_disease.study_accession
  WHERE workspace_id = $1;"
  SQL_df <- DBI::dbGetQuery(conn, getQuery_str, params = list(user_workspace))
  row_select_cd <- SQL_df[input$condition_or_disease_table_row_last_clicked, "study_accession"]
  row_selection_cd <- dbQuoteLiteral(conn, row_select_cd)
  
  update_str <- paste0("UPDATE study_2_condition_or_disease SET 
                       condition_reported =  NULLIF('", rv_selected_disease, "', ''),
                       condition_preferred =  NULLIF('", input$condition_preferred, "', '')
                       WHERE study_accession =  ", row_selection_cd, ";")
  
  DBI::dbSendQuery(conn, update_str)
  removeModal()
})

# close tab
observeEvent(input$close_button_cd, {
  deselect<-which(components()[, "table_name"] == "study_2_condition_or_disease")
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
removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
})
