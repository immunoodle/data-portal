### immune exposure tab

observe( {
  req(input$rb_add == "immuneExposure") #createStudy
  output$createImmuExpo <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("immuexpo_table"), #newstudy_table
                          actionButton("add_button_ie", "Add", icon("plus")),
                          actionButton("edit_button_ie", "Edit", icon("edit")),
                          br(),
                          actionButton("close_button_ie", "Close Tab", icon("xmark"))
      )
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "ImmuExpoPanel" #createStudy
                     , title = "Immune Exposure" #Create New Study
                     , uiOutput("createImmuExpo"))) #createNewStudy
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
immuexpo <- reactive({ #newstudy
  input$submit_ie
  input$submit_edit_ie
  ie_query<-paste0("SELECT exposure_accession, arm_or_cohort.arm_accession, disease_ontology_id,
                               disease_preferred, disease_reported, disease_stage_preferred,
                               disease_stage_reported, exposure_material_id, exposure_material_preferred,
                               exposure_material_reported, exposure_process_preferred,
                               exposure_process_reported, subject_accession
    FROM madi_dat.immune_exposure
    INNER JOIN madi_dat.arm_or_cohort
    ON immune_exposure.arm_accession = arm_or_cohort.arm_accession
    WHERE workspace_id IN (6101,6102,6103,6104,6105)
    AND study_accession IN ('", selected_study_accession(), "');")
  immuexpo <- DBI::dbGetQuery(conn, ie_query)
}) 

#List of mandatory fields for submission
fieldsMandatory <- c("exposure_accession", "disease_reported")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_ie", condition = mandatoryFilled)
})

output$immuexpo_table <- DT::renderDataTable({ #newstudy_table
  table <- immuexpo()[,-c(3:7,9,11,13)] #newstudy
  names(table) <- c("exposure_accession", "arm_accession", "exposure_material_id", 
                    "exposure_material_reported", "exposure_process_reported"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_ie <- function(button_id_ie){
  showModal(
    modalDialog(
      div(id=("entry_form_ie"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              column(
                width = 6,
                
                textInput("exposure_accession", labelMandatory("Exposure Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT(MAX(CAST(SUBSTRING(exposure_accession,4) AS INTEGER))+1) AS next_exposure_accession FROM madi_dat.immune_exposure;")),
                selectInput("arm_accession", labelMandatory("Arm Accession"), selected=NULL, choices = DBI::dbGetQuery(conn,"SELECT arm_or_cohort.arm_accession 
    FROM madi_dat.immune_exposure
	  INNER JOIN madi_dat.arm_or_cohort
    ON immune_exposure.arm_accession = arm_or_cohort.arm_accession
    WHERE workspace_id IN (6101,6102,6103,6104,6105);")),
                textInput("disease_ontology_id", "Disease Ontology ID", placeholder = ""),
                textInput("disease_preferred", "Disease Preferred", placeholder = ""),
                textInput("disease_reported", "Disease Reported", placeholder = ""),
                textInput("disease_stage_preferred", "Disease Stage Preferred", placeholder = ""),
                textInput("disease_stage_reported", "Disease Stage Reported", placeholder = "")
                
              ),
              column(
                width = 6,
                textInput("exposure_material_id", "Exposure Material ID", placeholder = ""),
                textInput("exposure_material_preferred", "Exposure Material Preferred", placeholder = ""),
                textInput("exposure_material_reported", "Exposure Material Reported", placeholder = ""),
                textInput("exposure_process_preferred", "Exposure Process Preferred", placeholder = ""),
                textInput("exposure_process_reported", "Exposure Process Reported", placeholder = ""),
                textInput("subject_accession", labelMandatory("Subject Accession"), placeholder = ""),
                
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_ie, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_ie <- reactive({
  formData_ie <- data.frame(row_id = UUIDgenerate(),
                            exposure_accession = input$exposure_accession,
                            arm_accession = input$arm_accession,
                            disease_ontology_id = input$disease_ontology_id,
                            disease_preferred = input$disease_preferred,
                            disease_reported = input$disease_reported,
                            disease_stage_preferred = input$disease_stage_preferred,
                            disease_stage_reported = input$disease_stage_reported,
                            exposure_material_id = input$exposure_material_id,
                            exposure_material_preferred = input$exposure_material_preferred,
                            exposure_material_reported = input$exposure_material_reported,
                            exposure_process_preferred = input$exposure_process_preferred,
                            exposure_process_reported = input$exposure_process_reported,
                            subject_accession = input$subject_accession,
                            stringsAsFactors = FALSE)
  return(formData_ie)
})

#Add data
appendData_ie <- function(data){
  dbSendQuery(conn,paste0("INSERT INTO immune_exposure (exposure_accession, arm_accession, disease_ontology_id,
                               disease_preferred, disease_reported, disease_stage_preferred,
                               disease_stage_reported, exposure_material_id, exposure_material_preferred,
                               exposure_material_reported, exposure_process_preferred,
                               exposure_process_reported, subject_accession
                               )
	VALUES ('", input$exposure_accession,
                          "', '", input$arm_accession,
                          "', '", input$disease_ontology_id,
                          "', '", input$disease_preferred,
                          "', '", input$disease_reported,
                          "', '", input$input$disease_stage_preferred,
                          "', '", input$disease_stage_reported,
                          "', '", input$exposure_material_id,
                          "', '", input$exposure_material_preferred,
                          "', '", input$exposure_material_reported,
                          "', '", input$exposure_process_preferred,
                          "', '", input$input$exposure_process_reported,
                          "', '", input$subject_accession,
                          "');"))
}

observeEvent(input$add_button_ie, priority = 20,{
  entry_form_ie("submit_ie")
})

observeEvent(input$submit_ie, priority = 20,{
  appendData_ie(formData_ie())
  shinyjs::reset("entry_form_ie")
  removeModal()
})

#edit data
observeEvent(input$edit_button_ie, priority = 20,{
  SQL_df <- DBI::dbGetQuery(conn, "SELECT exposure_accession, arm_or_cohort.arm_accession, disease_ontology_id,
                               disease_preferred, disease_reported, disease_stage_preferred,
                               disease_stage_reported, exposure_material_id, exposure_material_preferred,
                               exposure_material_reported, exposure_process_preferred,
                               exposure_process_reported, subject_accession
    FROM madi_dat.immune_exposure
    INNER JOIN madi_dat.arm_or_cohort
    ON immune_exposure.arm_accession = arm_or_cohort.arm_accession
    WHERE workspace_id IN (6101,6102,6103,6104,6105);")
  showModal(
    if(length(input$immuexpo_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$immuexpo_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$immuexpo_table_rows_selected) == 1 ){
    entry_form_ie("submit_edit_ie")
    updateTextInput(session, "exposure_accession", value = SQL_df[input$immuexpo_table_rows_selected, "exposure_accession"])
    updateTextInput(session, "arm_accession", value = SQL_df[input$immuexpo_table_rows_selected, "arm_accession"])
    updateTextInput(session, "disease_ontology_id", value = SQL_df[input$immuexpo_table_rows_selected, "disease_ontology_id"])
    updateTextInput(session, "disease_preferred", value = SQL_df[input$immuexpo_table_rows_selected, "disease_preferred"])
    updateTextInput(session, "disease_reported", value = SQL_df[input$immuexpo_table_rows_selected, "disease_reported"])
    updateTextInput(session, "disease_stage_preferred", value = SQL_df[input$immuexpo_table_rows_selected, "disease_stage_preferred"])
    updateTextInput(session, "disease_stage_reported", value = SQL_df[input$immuexpo_table_rows_selected, "disease_stage_reported"])
    updateTextInput(session, "exposure_material_id", value = SQL_df[input$immuexpo_table_rows_selected, "exposure_material_id"])
    updateTextInput(session, "exposure_material_preferred", value = SQL_df[input$immuexpo_table_rows_selected, "exposure_material_preferred"])
    updateTextInput(session, "exposure_material_reported", value = SQL_df[input$immuexpo_table_rows_selected, "exposure_material_reported"])
    updateTextInput(session, "exposure_process_preferred", value = SQL_df[input$immuexpo_table_rows_selected, "exposure_process_preferred"])
    updateTextInput(session, "exposure_process_reported", value = SQL_df[input$immuexpo_table_rows_selected, "exposure_process_reported"])
    updateTextInput(session, "subject_accession", value = SQL_df[input$immuexpo_table_rows_selected, "subject_accession"])
    
  }
})

observeEvent(input$submit_edit_ie, priority = 20, {
  getQuery_str_ie <- "SELECT exposure_accession, arm_or_cohort.arm_accession, disease_ontology_id,
                               disease_preferred, disease_reported, disease_stage_preferred,
                               disease_stage_reported, exposure_material_id, exposure_material_preferred,
                               exposure_material_reported, exposure_process_preferred,
                               exposure_process_reported, subject_accession
    FROM madi_dat.immune_exposure
    INNER JOIN madi_dat.arm_or_cohort
    ON immune_exposure.arm_accession = arm_or_cohort.arm_accession
    WHERE workspace_id IN (6101,6102,6103,6104,6105);"
  SQL_df_ie <- DBI::dbGetQuery(conn, getQuery_str_ie)
  row_select_ie <- SQL_df_ie[input$immuexpo_table_rows_selected, "exposure_accession"]
  row_selection_ie <- dbQuoteLiteral(conn, row_select_ie)
  
  update_str_ie <- paste0("UPDATE immune_exposure SET 
                       arm_accession =  NULLIF('", input$arm_accession, "', ''),
                       disease_ontology_id =  NULLIF('", input$disease_ontology_id, "', ''),
                       disease_preferred =  NULLIF('", input$disease_preferred, "', ''),
                       disease_reported =  NULLIF('", input$disease_reported, "', ''),
                       disease_stage_preferred =  NULLIF('", input$disease_stage_preferred, "', ''),
                       disease_stage_reported =  NULLIF('", input$disease_stage_reported, "', ''),
                       exposure_material_id =  NULLIF('", input$exposure_material_id, "', ''),
                       exposure_material_preferred =  NULLIF('", input$exposure_material_preferred, "', ''),
                       exposure_material_reported =  NULLIF('", input$exposure_material_reported, "', ''),
                       exposure_process_preferred =  NULLIF('", input$exposure_process_preferred, "', ''),
                       exposure_process_reported = NULLIF('", input$exposure_process_reported, "', ''),
                       subject_accession = NULLIF('", input$subject_accession, "', ''),
                       WHERE exposure_accession =  ", row_selection_ie, ";")
  
  DBI::dbSendQuery(conn, update_str_ie)
  removeModal()
})

# close tab
observeEvent(input$close_button_ie, {
  
  deselect<-which(components()[, "table_name"] == "immune_exposure")
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
  
  removeTab(inputId = "body_panel_id", target = "ImmnueExposurePanel")
})
