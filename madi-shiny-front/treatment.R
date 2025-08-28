### madi_dat.treatment 

treatment_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createTreatment <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("treatment_table"), #newstudy_table
                          actionButton("add_button_tr", "Add", icon("plus")),
                          actionButton("edit_button_tr", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_tr", "Close Tab", icon("xmark"))
    )
  })
  # insertTab(inputId = "body_panel_id",
  #           tabPanel(value = "TreatmentPanel" #createStudy
  #                    , title = "Treatment" #Create New Study
  #                    , uiOutput("createTreatment"))) #createNewStudy
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
treatment <- reactiveVal({
  # input$submit_add_tr
  # input$submit_edit_tr
  
  # Get user's workspace ID from session data
  user_workspace <- session$userData$user_workspace_id
  if (is.null(user_workspace)) {
    # Fallback to empty data if no workspace assigned
    return(data.frame(treatment_accession = character(0), amount_unit = character(0), 
                     amount_value = numeric(0), comments = character(0),
                     duration_unit = character(0), duration_value = numeric(0), 
                     name = character(0), temperature_unit = character(0), 
                     temperature_value = numeric(0), workspace_id = numeric(0),
                     stringsAsFactors = FALSE))
  }
  
  tr_select_query <- paste0("SELECT treatment_accession, amount_unit, amount_value, comments,
                               duration_unit, duration_value, name, temperature_unit, temperature_value, workspace_id
	FROM madi_dat.treatment WHERE workspace_id = $1;")
  
  treatment <- DBI::dbGetQuery(conn, tr_select_query, params = list(user_workspace))
})

#List of mandatory fields for submission
fieldsMandatory_tr <- c("treatment_accession", "workspace_id")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_tr,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_tr", condition = mandatoryFilled)
})

output$treatment_table <- DT::renderDataTable({ #newstudy_table
  table <- treatment() #newstudy
  names(table) <- c("treatment_accession", "amount_unit", "amount_value", "comments", 
                    "duration_unit", "duration_value", "name", "temperature_unit", 
                    "temperature_value", "workspace_id"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_tr <- function(button_id_tr){
  showModal(
    modalDialog(
      div(id=("entry_form_tr"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              column(
                width = 6,
                disabled(
                  textInput("treatment_accession", labelMandatory("Treatment Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT('TRT',MAX(CAST(SUBSTRING(treatment_accession,4) AS INTEGER))+1) AS next_treatment_accession FROM madi_dat.treatment;"))
                ),
                selectInput("amount_unit", "Amount Unit", selected = "Not Specified", choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_unit_of_measure;")),
                # selectizeInput("amount_unit", "Amount Unit", options = list(
                #   onInitialize = I('function() { this.setValue(""); }')
                # ), selected="Not Specified", choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_unit_of_measure;")), 
                textInput("amount_value", "Amount Value", placeholder = ""),
                textInput("comments", "Comments", placeholder = ""),
                selectInput("duration_unit", "Duration Unit", selected = "Not Specified", choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_time_unit;")),
                # selectizeInput("duration_unit", "Duration Unit", options = list(
                #   onInitialize = I('function() { this.setValue(""); }')
                # ), selected="Not Specified", choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_time_unit;"))
              ),
              column(
                width = 6,
                textInput("duration_value", "Duration Value", placeholder = ""),
                textInput("name", "Name", placeholder = ""),
                # selectizeInput("temperature_unit", "Temperature Unit", options = list(
                #   onInitialize = I('function() { this.setValue(""); }')
                # ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_unit_of_measure;")), 
                selectInput("temperature_unit", "Temperature Unit", selected = 'Not Specified',choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_unit_of_measure;")),
                textInput("temperature_value", "Temperature Value", placeholder = ""),
                disabled(
                  textInput("workspace_id", labelMandatory("workspace_id"))
                )
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_tr, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_tr <- reactive({
  formData_tr <<- data.frame(treatment_accession = input$treatment_accession,
                             # amount_unit = input$amount_unit,
                             amount_unit = ifelse(length(input$amount_unit)==0, "Not Specified", input$amount_unit),
                             amount_value = input$amount_value,
                             comments = input$comments,
                             # duration_unit = input$duration_unit,
                             duration_unit = ifelse(length(input$duration_unit)==0, "Not Specified", input$duration_unit),
                             duration_value = input$duration_value,
                             name = input$name,
                             # temperature_unit = input$temperature_unit,
                             temperature_unit = ifelse(length(input$temperature_unit)==0, "Not Specified", input$temperature_unit),
                             temperature_value = input$temperature_value,
                             workspace_id = input$workspace_id,
                             stringsAsFactors = FALSE)
  return(formData_tr)
})

#Add data
appendData_tr <- function(data){
  update_db(operation = "insert", db_data = data, table_name = "treatment")
}

observeEvent(input$add_button_tr, priority = 20,{
  entry_form_tr("submit_add_tr")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
})

observeEvent(input$submit_add_tr, priority = 20,{
  appendData_tr(formData_tr())
  shinyjs::reset("entry_form_tr")
  removeModal()
  # treatment(update_db(operation="select", table_name="treatment"))
  
  # Get user's workspace ID from session data
  user_workspace <- session$userData$user_workspace_id
  if (!is.null(user_workspace)) {
    tr_select_query <- paste0("SELECT treatment_accession, amount_unit, amount_value, comments,
                                 duration_unit, duration_value, name, temperature_unit, temperature_value, workspace_id
      FROM madi_dat.treatment WHERE workspace_id = $1;")
    
    treatment(DBI::dbGetQuery(conn, tr_select_query, params = list(user_workspace)))
  } else {
    # No workspace assigned - set empty data
    treatment(data.frame(treatment_accession = character(0), amount_unit = character(0), 
                        amount_value = numeric(0), comments = character(0),
                        duration_unit = character(0), duration_value = numeric(0), 
                        name = character(0), temperature_unit = character(0), 
                        temperature_value = numeric(0), workspace_id = numeric(0),
                        stringsAsFactors = FALSE))
  }
})

observeEvent(input$treatment_table_rows_selected, {
  treatment_selected_row(treatment()[input$treatment_table_rows_selected,"treatment_accession"])
})

#edit data
observeEvent(input$edit_button_tr, priority = 20,{
  
  # Get user's workspace ID from session data
  user_workspace <- session$userData$user_workspace_id
  if (is.null(user_workspace)) {
    showNotification("No workspace assigned to user", type = "error")
    return()
  }
  
  tr_edit_select_query <- paste0("SELECT treatment_accession, amount_unit, amount_value, 
                                 comments, duration_unit, duration_value, name, temperature_unit, 
                                 temperature_value, workspace_id FROM madi_dat.treatment 
                                 WHERE workspace_id = $1 
                                 AND treatment_accession = $2;")
  print(tr_edit_select_query)
  SQL_df <- DBI::dbGetQuery(conn, tr_edit_select_query, params = list(user_workspace, treatment_selected_row()))
  
  #   tr_select_query<-paste0("SELECT treatment_accession, amount_unit, amount_value, comments,
  #                                duration_unit, duration_value, name, temperature_unit, temperature_value, workspace_id
  # 	FROM madi_dat.treatment WHERE workspace_id = $1;")
  #   
  #   SQL_df <- update_db(operation = "select", table_name = "treatment", select_where = list("treatment_accession" = treatment_selected_row()))
  print(paste("The selected treatment accession is", treatment_selected_row()))
  
  showModal(
    if(length(input$treatment_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$treatment_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$treatment_table_rows_selected) == 1 ){
    entry_form_tr("submit_edit_tr")
    updateTextInput(session, "treatment_accession", value = SQL_df[, "treatment_accession"])
    updateTextInput(session, "amount_unit", value = SQL_df[, "amount_unit"])
    updateTextInput(session, "amount_value", value = SQL_df[, "amount_value"])
    updateTextInput(session, "comments", value = SQL_df[, "comments"])
    updateTextInput(session, "duration_unit", value = SQL_df[, "duration_unit"])
    updateTextInput(session, "duration_value", value = SQL_df[, "duration_value"])
    updateTextInput(session, "name", value = SQL_df[, "name"])
    updateTextInput(session, "temperature_unit", value = SQL_df[, "temperature_unit"])
    updateTextInput(session, "temperature_value", value = SQL_df[, "temperature_value"])
    updateTextInput(session, "workspace_id", value = SQL_df[, "workspace_id"])
  }
})

observeEvent(input$submit_edit_tr, priority = 20, {
  
  updated_data <- tibble(
    treatment_accession = input$treatment_accession,
    amount_unit = ifelse(length(input$amount_unit)==0, "Not Specified", input$amount_unit),
    amount_value = input$amount_value,
    comments = input$comments,
    duration_unit = ifelse(length(input$duration_unit)==0, "Not Specified", input$duration_unit),
    duration_value = input$duration_value,
    name = input$name, 
    temperature_unit = ifelse(length(input$temperature_unit)==0, "Not Specified", input$temperature_unit),
    temperature_value = input$temperature_value,
    workspace_id = input$workspace_id,
  )
  
  update_db(operation = "update", table_name = "treatment", db_data = updated_data, update_where = c("treatment_accession" = treatment_selected_row()))
  
  # Get user's workspace ID from session data and refresh the treatment data
  user_workspace <- session$userData$user_workspace_id
  if (!is.null(user_workspace)) {
    tr_select_query <- paste0("SELECT treatment_accession, amount_unit, amount_value, comments,
                                 duration_unit, duration_value, name, temperature_unit, temperature_value, workspace_id
      FROM madi_dat.treatment WHERE workspace_id = $1;")
    
    treatment(DBI::dbGetQuery(conn, tr_select_query, params = list(user_workspace)))
  }
  
  removeModal()
})

# close tab
observeEvent(input$close_button_tr, {
  
  deselect<-which(components()[, "table_name"] == "treatment")
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
  
  removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
})
