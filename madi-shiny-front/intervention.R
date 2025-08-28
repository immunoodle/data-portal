### madi_dat.intervention

observe( {
  req(input$rb_add == "createStudy")
  output$createIntervention <- renderUI({ 
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("intervention_table"),
                          actionButton("add_button_itv", "Add", icon("plus")),
                          actionButton("edit_button_itv", "Edit", icon("edit"))
      )
    )
  })
  # insertTab(inputId = "body_panel_id",
  #           tabPanel(value = "InterventionPanel"
  #                    , title = "Intervention"
  #                    , uiOutput("createIntervention")))
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
intervention <- reactive({
  input$submit_itv
  input$submit_edit_itv
  itv_query<-paste0("SELECT intervention_accession, compound_name_reported, 
                              compound_role, dose, dose_freq_per_interval, dose_reported, 
                              dose_units, duration, duration_unit, end_day, end_time, 
                              formulation, is_ongoing, name_preferred, name_reported, 
                              reported_indication, route_of_admin_preferred, 
                              route_of_admin_reported, start_day, start_time, status, 
                              study_accession, subject_accession, workspace_id
	FROM madi_dat.intervention
  AND study_accession IN ('", selected_study_accession(), "');")
  intervention <- DBI::dbGetQuery(conn, itv_query)
})  

#List of mandatory fields for submission
fieldsMandatory <- c("intervention_accession", "study_accession", 
                     "subject_accession", "workspace_id")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_itv", condition = mandatoryFilled)
})

output$intervention_table <- DT::renderDataTable({
  table <- intervention()[,]
  names(table) <- c("intervention_accession", "compound_name_reported", "compound_role", "dose",
                    "dose_freq_per_interval", "dose_reported", 
                    "dose_units", "duration", "duration_unit", "end_day", "end_time", 
                    "formulation", "is_ongoing", "name_preferred", "name_reported", 
                    "reported_indication", "route_of_admin_preferred", 
                    "route_of_admin_reported", "start_day", "start_time", "status", 
                    "study_accession", "subject_accession", "workspace_id"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_itv <- function(button_id_itv){
  showModal(
    modalDialog(
      div(id=("entry_form_itv"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          # tags$head(tags$style("#test .modal-dialog {width: fit-content !important;}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              column(
                width=4,
                textInput("intervention_accession", labelMandatory("Intervention Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT('SM', MAX(CAST(SUBSTRING(intervention_accession,6) AS INTEGER))+1) AS next_intervention_accession FROM madi_dat.intervention;")),
                textInput("compound_name_reported", "Compound Name Reported", placeholder = ""),
                textInput("compound_role", "Compound Role", placeholder = ""),
                textInput("dose", "Dose", placeholder = ""),
                textInput("dose_freq_per_interval", "Doss Freq per Interval", placeholder = ""),
                textInput("dose_reported", "Dose Reported", placeholder = ""),
                textInput("dose_units", "Dose Units", placeholder = ""),
                textInput("duration", labelMandatory("Duration"), placeholder = ""),
                textInput("duration_unit", "Duration_unit", placeholder = "")
              ),
              column(
                width=4,
                numericInput("end_day", "End Day", value = NA),
                textInput("end_time", "End Time", placeholder = ""),
                textInput("formulation", "Formulation", placeholder = ""),
                textInput("is_ongoing", "Is Ongoing", placeholder = ""),
                textInput("name_preferred", "Name Preferred", placeholder = ""),
                textInput("name_reported", "Name Peported", placeholder = ""),
                textInput("reported_indication", "Reported Indication", placeholder = ""),
                textInput("route_of_admin_preferred", "Route of Admin Preferred", placeholder = "")
              ),
              column(
                width = 4,
                textInput("route_of_admin_reported", "Route of Admin Reported", placeholder = ""),
                numericInput("start_day", "Start Day", value = NA),
                textInput("start_time", "Start Time", placeholder = ""),
                textInput("status", "Status", placeholder = ""),
                textInput("study_accession", labelMandatory("Study Accession"), placeholder=""),
                # selectInput("study_accession", labelMandatory("Study Accession"), 
                #             choices=DBI::dbGetQuery(conn,"SELECT study.study_accession 
                #                                           FROM madi_dat.intervention
                #                                       	  INNER JOIN madi_dat.study
                #                                           ON study.study_accession = intervention.study_accession;")),
                textInput("subject_accession", labelMandatory("Subject Accession"), placeholder = ""),
                selectInput("workspace_id", labelMandatory("workspace_id"), selected=NULL, choices = {
                  user_workspace <- session$userData$user_workspace_id
                  if (!is.null(user_workspace)) {
                    DBI::dbGetQuery(conn, "SELECT workspace_id FROM madi_dat.workspace WHERE workspace_id = $1;", params = list(user_workspace))
                  } else {
                    character(0)
                  }
                })
              ),
              
              
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_itv, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_itv <- reactive({
  formData_itv <- data.frame(row_id = UUIDgenerate(),
                             intervention_accession = input$intervention_accession,
                             compound_name_reported = input$compound_name_reported,
                             compound_role = input$compound_role,
                             dose = input$dose,
                             dose_freq_per_interval = input$dose_freq_per_interval,
                             dose_reported = input$dose_reported,
                             dose_units = input$dose_units,
                             duration = input$duration,
                             duration_unit = input$duration_unit,
                             end_day = input$end_day,
                             end_time = input$end_time,
                             formulation = input$formulation,
                             is_ongoing = input$is_ongoing,
                             name_preferred = input$name_preferred,
                             name_reported = input$name_reported,
                             reported_indication = input$reported_indication,
                             route_of_admin_preferred = input$route_of_admin_preferred,
                             route_of_admin_reported = input$route_of_admin_reported,
                             start_day = input$start_day,
                             start_time = input$start_time,
                             status = input$status,
                             study_accession = input$study_accession,
                             subject_accession = input$subject_accession,
                             workspace_id = input$workspace_id,
                             stringsAsFactors = FALSE)
  return(formData_itv)
})

#Add data
appendData_itv <- function(data){
  dbSendQuery(conn,paste0("INSERT INTO madi_dat.intervention(
	intervention_accession, compound_name_reported, compound_role, dose, 
	dose_freq_per_interval, dose_reported, dose_units, duration, duration_unit, 
	end_day, end_time, formulation, is_ongoing, name_preferred, name_reported, 
	reported_indication, route_of_admin_preferred, route_of_admin_reported, 
	start_day, start_time, status, study_accession, subject_accession, workspace_id)
	VALUES ('", input$intervention_accession, "', 
                          '", input$compound_name_reported, 
                          "', '", input$compound_role, 
                          "', '", input$dose, 
                          "', '", input$dose_freq_per_interval, 
                          "', '", input$dose_reported, 
                          "', '", input$dose_units, 
                          "', '", input$duration, 
                          "', '", input$duration_unit, 
                          "', '", input$end_day, 
                          "', '", input$end_time, 
                          "', '", input$formulation, 
                          "', '", input$is_ongoing, 
                          "', '", input$name_preferred, 
                          "', '", input$name_reported, 
                          "', '", input$reported_indication, 
                          "', '", input$route_of_admin_preferred, 
                          "', '", input$route_of_admin_reported, 
                          "', '", input$start_day, 
                          "', '", input$start_time, 
                          "', '", input$status, 
                          "', '", input$study_accession, 
                          "', '", input$subject_accession, 
                          "', '", input$workspace_id, 
                          "');"))
}

observeEvent(input$add_button_itv, priority = 20,{
  entry_form_itv("submit")
})

observeEvent(input$submit_itv, priority = 20,{
  appendData_itv(formData_itv())
  shinyjs::reset("entry_form_itv")
  removeModal()
})

#edit data
observeEvent(input$edit_button_itv, priority = 20,{
  SQL_df <- DBI::dbGetQuery(conn, "SELECT intervention_accession, compound_name_reported, 
                              compound_role, dose, dose_freq_per_interval, dose_reported, 
                              dose_units, duration, duration_unit, end_day, end_time, 
                              formulation, is_ongoing, name_preferred, name_reported, 
                              reported_indication, route_of_admin_preferred, 
                              route_of_admin_reported, start_day, start_time, status, 
                              study_accession, subject_accession, workspace_id
	FROM madi_dat.intervention;")
  showModal(
    if(length(input$intervention_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$intervention_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$intervention_table_rows_selected) == 1 ){
    entry_form_itv("submit_edit_itv")
    updateTextInput(session, "intervention_accession", value = SQL_df[input$intervention_table_rows_selected, "intervention_accession"])
    updateTextInput(session, "compound_name_reported", value = SQL_df[input$intervention_table_rows_selected, "compound_name_reported"])
    updateTextInput(session, "compound_role", value = SQL_df[input$intervention_table_rows_selected, "compound_role"])
    updateTextInput(session, "dose", value = SQL_df[input$intervention_table_rows_selected, "dose"])
    updateTextInput(session, "dose_freq_per_interval", value = SQL_df[input$intervention_table_rows_selected, "dose_freq_per_interval"])
    updateTextInput(session, "dose_reported", value = SQL_df[input$intervention_table_rows_selected, "dose_reported"])
    updateTextInput(session, "dose_units", value = SQL_df[input$intervention_table_rows_selected, "dose_units"])
    updateTextInput(session, "duration", value = SQL_df[input$intervention_table_rows_selected, "duration"])
    updateTextInput(session, "duration_unit", value = SQL_df[input$intervention_table_rows_selected, "duration_unit"])
    updateNumericInput(session, "end_day", value = SQL_df[input$intervention_table_rows_selected, "end_day"])
    updateTextInput(session, "end_time", value = SQL_df[input$intervention_table_rows_selected, "end_time"])
    updateTextInput(session, "formulation", value = SQL_df[input$intervention_table_rows_selected, "formulation"])
    updateTextInput(session, "is_ongoing", value = SQL_df[input$intervention_table_rows_selected, "is_ongoing"])
    updateTextInput(session, "name_preferred", value = SQL_df[input$intervention_table_rows_selected, "name_preferred"])
    updateTextInput(session, "name_reported", value = SQL_df[input$intervention_table_rows_selected, "name_reported"])
    updateTextInput(session, "reported_indication", value = SQL_df[input$intervention_table_rows_selected, "reported_indication"])
    updateTextInput(session, "route_of_admin_preferred", value = SQL_df[input$intervention_table_rows_selected, "route_of_admin_preferred"])
    updateTextInput(session, "route_of_admin_reported", value = SQL_df[input$intervention_table_rows_selected, "route_of_admin_reported"])
    updateNumericInput(session, "start_day", value = SQL_df[input$intervention_table_rows_selected, "start_day"])
    updateTextInput(session, "start_time", value = SQL_df[input$intervention_table_rows_selected, "start_time"])
    updateTextInput(session, "status", value = SQL_df[input$intervention_table_rows_selected, "status"])
    updateTextInput(session, "study_accession", value = SQL_df[input$intervention_table_rows_selected, "study_accession"])
    updateTextInput(session, "subject_accession", value = SQL_df[input$intervention_table_rows_selected, "subject_accession"])
    updateTextInput(session, "workspace_id", value = SQL_df[input$intervention_table_rows_selected, "workspace_id"])
  }
})

observeEvent(input$submit_edit_itv, priority = 20, {
  SQL_df <- DBI::dbGetQuery(conn, "SELECT intervention_accession, compound_name_reported, compound_role, 
  dose, dose_freq_per_interval, dose_reported, dose_units, duration, duration_unit, 
  end_day, end_time, formulation, is_ongoing, name_preferred, name_reported, 
  reported_indication, route_of_admin_preferred, route_of_admin_reported, start_day, 
  start_time, status, study_accession, subject_accession, workspace_id
	FROM madi_dat.intervention;")
  row_select_itv <- SQL_df[input$intervention_table_row_last_clicked, "study_accession"]
  row_selection_itv <- dbQuoteLiteral(conn, row_select_itv)
  
  update_str_itv <- paste0("UPDATE intervention SET 
                       intervention_accession =  NULLIF('", input$intervention_accession, "', ''),
                       compound_name_reported =  NULLIF('", input$compound_name_reported, "', ''),
                       compound_role =  NULLIF('", input$compound_role, "', ''),
                       dose =  NULLIF('", input$dose, "', ''),
                       dose_freq_per_interval =  NULLIF('", input$dose_freq_per_interval, "', ''),
                       dose_reported =  NULLIF('", input$dose_reported, "', ''),
                       dose_units =  NULLIF('", input$dose_units, "', ''),
                       duration =  NULLIF('", input$duration, "', ''),
                       duration_unit =  NULLIF('", input$duration_unit, "', ''),
                       end_day = ", ifelse(is.na(input$end_day), "NULL", paste0("CAST(", input$end_day, " AS FLOAT)")),"
                       end_time =  NULLIF('", input$end_time, "', ''),
                       formulation =  NULLIF('", input$formulation, "', ''),
                       is_ongoing =  NULLIF('", input$is_ongoing, "', ''),
                       name_preferred = NULLIF('", input$name_preferred, "', ''),
                       name_reported = NULLIF('", input$name_reported, "', ''),
                       reported_indication =  NULLIF('", input$reported_indication, "', ''),
                       route_of_admin_preferred =  NULLIF('", input$route_of_admin_preferred, "', ''),
                       route_of_admin_reported =  NULLIF('", input$route_of_admin_reported, "', ''),
                       start_day = ", ifelse(is.na(input$start_day), "NULL", paste0("CAST(", input$start_day, " AS FLOAT)")),"
                       start_time = NULLIF('", input$start_time, "', ''),
                       status =  NULLIF('", input$status, "', ''),
                       study_accession =  NULLIF('", input$study_accession, "', ''),
                       subject_accession =  NULLIF('", input$subject_accession, "', ''),
                       workspace_id = ", ifelse(is.na(input$workspace_id), "NULL", paste0("CAST(", input$workspace_id, " AS FLOAT)")),"
                       WHERE intervention_accession =  ", row_selection_itv, ";")
  
  DBI::dbSendQuery(conn, update_str_itv)
  removeModal()
})

# close tab
observeEvent(input$close_button_itv, {
  
  deselect<-which(components()[, "table_name"] == "intervention")
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
  
  removeTab(inputId = "body_panel_id", target = "InterventionPanel")
})
