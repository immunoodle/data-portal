### Create new study through an editable madi_dat.study table.


selected_study_accession <- reactiveVal()
tab_name <- reactiveVal()
selectedRow <- reactiveVal(NULL)
formname <- reactiveVal()
panelname <- reactiveVal()
paneltitle <- reactiveVal()
paneluiop <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy")
  output$createNewStudy <- renderUI({ 
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("newstudy_table"),
                          actionButton("add_button", "Add", icon("plus")),
                          actionButton("edit_button", "Edit", icon("edit"))
      ),
      shinydashboard::box(title = "Study Components", width = 12,  status = "primary", 
                          solidHeader = T, collapsible = F,
                          DT::dataTableOutput("report_list_table")
      )
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "createStudy"
                     , title = "Create New Study"
                     , uiOutput("createNewStudy")))
} )

#Label mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

#Load studies and make the studies table reactive to inputs  
newstudy <- reactive({
  input$submit_add
  input$submit_edit
  # newstudy <- update_db(operation = "select", table_name = "study", select_where = list("workspace_id" = c(6101,6102,6103,6104,6105)))
  combined_study_query <- paste0("SELECT study.study_accession, actual_completion_date, actual_enrollment, actual_start_date, age_unit, 
brief_description, brief_title
, clinical_trial, condition_studied, dcl_id, description, doi, endpoints, gender_included, hypothesis, 
initial_data_release_date
, initial_data_release_version, intervention_agent, latest_data_release_date, latest_data_release_version, 
maximum_age, minimum_age
, objectives, official_title, sponsoring_organization, target_enrollment, workspace_id
, condition_reported, condition_preferred
, research_focus
	FROM madi_dat.study
	LEFT OUTER JOIN madi_dat.study_2_condition_or_disease AS cd ON cd.study_accession = study.study_accession
	LEFT OUTER JOIN madi_dat.study_categorization AS sc ON sc.study_accession = study.study_accession
	WHERE workspace_id IN (6101,6102,6103,6104,6105);") 
  
  # WHERE workspace_id = ", userData_upload()$workspace_id,";")
  
  newstudy <- DBI::dbGetQuery(conn, combined_study_query)
})  

observeEvent(input$newstudy_table_rows_selected, {
  selected_study_accession(newstudy()[input$newstudy_table_rows_selected,"study_accession"])
  print(paste("study selected", selected_study_accession()))
})

components <- reactive({
  query<-paste0("SELECT * FROM madi_meta.in_front_study WHERE study_accession IN ('", selected_study_accession(), "');")
  components<-DBI::dbGetQuery(conn, query)
})

#List of mandatory fields for submission
fieldsMandatory_newstudy <- c("study_accession", "clinical_trial", 
                              "dcl_id", "workspace_id", "research_focus", "condition_reported", "condition_preferred")

#Define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_newstudy,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add", condition = mandatoryFilled)
})

output$newstudy_table <- DT::renderDataTable({
  table <- newstudy()[,c(1,6,7,9,28,30)]
  names(table) <- c("study_accession", "brief_description", "brief_title", "condition_studied", 
                    "condition_reported", "research_focus"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form <- function(button_id){
  showModal(
    modalDialog(
      div(id=("entry_form"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              column(
                width=4,
                disabled(
                  textInput("study_accession", labelMandatory("Study Accession"), value=DBI::dbGetQuery(conn,"SELECT CONCAT('SDY', MAX(CAST(SUBSTRING(study_accession,4) AS INTEGER))+1) AS next_study_accession FROM madi_dat.study;"))
                ),
                dateInput("actual_completion_date", "Actual Completion Date", value = NA, format = "yyyy-mm-dd"),
                numericInput("actual_enrollment", "Actual Enrollment", value = NA),
                dateInput("actual_start_date", "Actual Start Date", value = NA, format = "yyyy-mm-dd"),
                selectizeInput("age_unit", "Age Unit", options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_time_unit;")),
                textInput("brief_description", "Brief Description", placeholder = ""),
                textInput("brief_title", "Brief Title", placeholder = ""),
                textInput("clinical_trial", labelMandatory("Clinical Trial"), placeholder = ""),
                textInput("condition_studied", "Condition Studied", placeholder = ""),
                selectizeInput("dcl_id", labelMandatory("DCL ID"), options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT id FROM madi_dat.lk_data_completeness;"))
              ),
              column(
                width = 4,
                textInput("description", "Description", placeholder = ""),
                textInput("doi", "DOI", placeholder = ""),
                textInput("endpoints", "Endpoints", placeholder = ""),
                textInput("gender_included", "Gender Included", placeholder = ""),
                textInput("hypothesis", "Hypothesis", placeholder = ""),
                dateInput("initial_data_release_date", "initial_data_release_date", value=NA, format = "yyyy-mm-dd"),
                textInput("initial_data_release_version", "initial_data_release_version", placeholder = ""),
                textInput("intervention_agent", "intervention_agent", placeholder = ""),
                dateInput("latest_data_release_date", "latest_data_release_date", value=NA, format = "yyyy-mm-dd"),
                textInput("latest_data_release_version", "latest_data_release_version", placeholder = "")
              ),
              column(
                width=4,
                numericInput("maximum_age", "maximum_age", value = NA),
                numericInput("minimum_age", "minimum_age", value = NA),
                textInput("objectives", "objectives", placeholder = ""),
                textInput("official_title", "official_title", placeholder = ""),
                textInput("sponsoring_organization", "sponsoring_organization", placeholder = ""),
                numericInput("target_enrollment", "target_enrollment", value = NA),
                disabled(
                  textInput("workspace_id", labelMandatory("workspace_id"))
                ),
                selectizeInput("research_focus", labelMandatory("Research Focus"), options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_research_focus;")), 
                textInput("condition_reported", labelMandatory("Condition Reported"), placeholder = ""),
                selectizeInput("condition_preferred", "Condition Preferred", options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_disease;"))
              ),
              
              title="New Study",
              size="m",
              
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData <- reactive({
  formData <- data.frame(study_accession = input$study_accession,
                         actual_completion_date = ifelse(length(input$actual_completion_date)==0, NA, as.character(format(input$actual_completion_date, format="%Y-%m-%d"))),
                         actual_enrollment = ifelse(length(input$actual_enrollment)==0, NA, input$actual_enrollment),
                         actual_start_date = ifelse(length(input$actual_start_date)==0, NA, as.character(format(input$actual_start_date, format="%Y-%m-%d"))),
                         age_unit = ifelse(length(input$age_unit)==0, NA, input$age_unit),
                         brief_description = input$brief_description,
                         brief_title = input$brief_title,
                         clinical_trial = input$clinical_trial,
                         condition_studied = input$condition_studied,
                         dcl_id = as.integer(input$dcl_id),
                         description = input$description,
                         doi = input$doi,
                         endpoints = input$endpoints,
                         gender_included = input$gender_included,
                         hypothesis = input$hypothesis,
                         initial_data_release_date = ifelse(length(input$initial_data_release_date)==0, NA, as.character(format(input$initial_data_release_date, format="%Y-%m-%d"))),
                         initial_data_release_version = input$initial_data_release_version,
                         intervention_agent = input$intervention_agent,
                         latest_data_release_date = ifelse(length(input$latest_data_release_date)==0, NA, as.character(format(input$latest_data_release_date, format="%Y-%m-%d"))),
                         latest_data_release_version = input$latest_data_release_version,
                         maximum_age = input$maximum_age,
                         minimum_age = input$minimum_age,
                         objectives = input$objectives,
                         official_title = input$official_title,
                         sponsoring_organization = input$sponsoring_organization,
                         target_enrollment = input$target_enrollment,
                         workspace_id = as.integer(input$workspace_id),
                         stringsAsFactors = FALSE)
  return(formData)
})

formData_cate <- reactive({
  formData_cate <- data.frame(ifelse(length(research_focus)==0, NA, input$research_focus),
                              study_accession = input$study_accession,
                              stringsAsFactors = FALSE)
  return(formData_cate)
})

formData_cd <- reactive({
  formData_cd <- data.frame(study_accession = input$study_accession,
                            condition_reported = input$condition_reported,
                            ifelse(length(condition_preferred)==0, NA, input$condition_preferred),
                            stringsAsFactors = FALSE)
  return(formData_cd)
})

#Add data
appendData <- function(data){
  print("before add study")
  update_db(operation = "insert", db_data = data, table_name = "study")
  print("after add study")
}

appendData_cate <- function(data){
  dbSendQuery(conn,paste0("INSERT INTO study_categorization (research_focus, study_accession)
	VALUES ('", input$research_focus, "', '", input$study_accession, "');"))
}

appendData_cd <- function(data){
  dbSendQuery(conn,paste0("INSERT INTO study_2_condition_or_disease 
  (study_accession, condition_reported, condition_preferred)
	VALUES ('", input$study_accession, 
                          "', '", input$condition_reported, 
                          "', '", input$condition_preferred, 
                          "');"))
}

observeEvent(input$add_button, priority = 20,{
  entry_form("submit_add")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
})

observeEvent(input$submit_add, priority = 20,{
  appendData(formData())
  appendData_cate(formData_cate())
  appendData_cd(formData_cd())
  shinyjs::reset("entry_form")
  removeModal()
})

#edit data
observeEvent(input$edit_button, priority = 20,{
  
  SQL_df_study <- update_db(operation = "select", table_name = "study", select_where = c("study_accession" = selected_study_accession()))
  SQL_df_cd <- update_db(operation = "select", table_name = "study_2_condition_or_disease", select_where = list("study_accession" = selected_study_accession()))
  SQL_df_cate <- update_db(operation = "select", table_name = "study_categorization", select_where = list("study_accession" = selected_study_accession()))
  
  showModal(
    if(length(input$newstudy_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$newstudy_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$newstudy_table_rows_selected) == 1 ){
    entry_form("submit_edit")
    updateTextInput(session, "study_accession", value = SQL_df_study[, "study_accession"])
    updateDateInput(session, "actual_completion_date", value = as.Date(SQL_df_study[, "actual_completion_date"]))
    updateNumericInput(session, "actual_enrollment", value = SQL_df_study[, "actual_enrollment"])
    updateDateInput(session, "actual_start_date", value = as.Date(SQL_df_study[, "actual_start_date"]))
    updateTextInput(session, "age_unit", value = SQL_df_study[, "age_unit"])
    updateTextInput(session, "brief_description", value = SQL_df_study[, "brief_description"])
    updateTextInput(session, "brief_title", value = SQL_df_study[, "brief_title"])
    updateTextInput(session, "clinical_trial", value = SQL_df_study[, "clinical_trial"])
    updateTextInput(session, "condition_studied", value = SQL_df_study[, "condition_studied"])
    updateTextInput(session, "dcl_id", value = SQL_df_study[, "dcl_id"])
    updateTextInput(session, "description", value = SQL_df_study[, "description"])
    updateTextInput(session, "doi", value = SQL_df_study[, "doi"])
    updateTextInput(session, "endpoints", value = SQL_df_study[, "endpoints"])
    updateTextInput(session, "gender_included", value = SQL_df_study[, "gender_included"])
    updateTextInput(session, "hypothesis", value = SQL_df_study[, "hypothesis"])
    updateDateInput(session, "initial_data_release_date", value = as.Date(SQL_df_study[, "initial_data_release_date"]))
    updateTextInput(session, "initial_data_release_version", value = SQL_df_study[, "initial_data_release_version"])
    updateTextInput(session, "intervention_agent", value = SQL_df_study[, "intervention_agent"])
    updateDateInput(session, "latest_data_release_date", value = as.Date(SQL_df_study[, "latest_data_release_date"]))
    updateTextInput(session, "latest_data_release_version", value = SQL_df_study[, "latest_data_release_version"])
    updateNumericInput(session, "maximum_age", value = SQL_df_study[, "maximum_age"])
    updateNumericInput(session, "minimum_age", value = SQL_df_study[, "minimum_age"])
    updateTextInput(session, "objectives", value = SQL_df_study[, "objectives"])
    updateTextInput(session, "official_title", value = SQL_df_study[, "official_title"])
    updateTextInput(session, "sponsoring_organization", value = SQL_df_study[, "sponsoring_organization"])
    updateNumericInput(session, "target_enrollment", value = SQL_df_study[, "target_enrollment"])
    updateTextInput(session, "workspace_id", value = SQL_df_study[, "workspace_id"])
    ## ?
    updateTextInput(session, "condition_reported", value = SQL_df_cd[, "condition_reported"])
    updateTextInput(session, "condition_preferred", value = SQL_df_cd[, "condition_preferred"])
    ## ?
    updateTextInput(session, "research_focus", value = SQL_df_cate[, "research_focus"])
  }
})

observeEvent(input$submit_edit, priority = 20, {
  
  updated_data_study <- tibble(
    study_accession = input$study_accession,
    actual_completion_date = ifelse(length(input$actual_completion_date)==0, NA, as.character(format(input$actual_completion_date, format="%Y-%m-%d"))),
    actual_enrollment = input$actual_enrollment,
    actual_start_date= ifelse(length(input$actual_start_date)==0, NA, as.character(format(input$actual_start_date, format="%Y-%m-%d"))),
    age_unit= ifelse(length(input$age_unit)==0, NA, input$age_unit),
    brief_description=input$brief_description,
    brief_title= input$brief_title,
    clinical_trial= input$clinical_trial,
    condition_studied = input$condition_studied,
    dcl_id = input$dcl_id,
    description = input$description,
    doi = input$doi,
    endpoints = input$endpoints,
    gender_included = input$gender_included,
    hypothesis = input$hypothesis,
    initial_data_release_date = ifelse(length(input$initial_data_release_date)==0, NA, as.character(format(input$initial_data_release_date, format="%Y-%m-%d"))),
    initial_data_release_version = input$initial_data_release_version,
    intervention_agent = input$intervention_agent,
    latest_data_release_date = ifelse(length(input$latest_data_release_date)==0, NA, as.character(format(input$latest_data_release_date, format="%Y-%m-%d"))),
    latest_data_release_version = input$latest_data_release_version,
    maximum_age = input$maximum_age,
    minimum_age = input$minimum_age,
    objectives = input$objectives,
    official_title = input$official_title,
    sponsoring_organization = input$sponsoring_organization,
    target_enrollment = input$target_enrollment,
    workspace_id = input$workspace_id
  )
  
  update_db(operation = "update", table_name = "study", db_data = updated_data_study, update_where = c("study_accession" = selected_study_accession()))
  
  updated_data_cd <- tibble(
    study_accession = input$study_accession,
    condition_reported = input$condition_reported,
    condition_preferred = ifelse(length(input$condition_preferred)==0, NA, input$condition_preferred),
  )
  
  update_db(operation = "update", table_name = "study_2_condition_or_disease", db_data = updated_data_cd, update_where = c("study_accession" = selected_study_accession()))
  
  update_str_cate <- paste0("UPDATE study_categorization SET 
                       research_focus =  NULLIF('", input$research_focus, "', ''),
                       WHERE study_accession =  ", selected_study_accession(), ";")
  
  DBI::dbSendQuery(conn, update_str_cate)
  
  removeModal()
})


############ report ################
output$report_list_table <- DT::renderDataTable({
  components_tab <- components()
  names(components_tab) <- c("study_accession", "form", "status")
  components_tab <- datatable(components_tab,
                              rownames = FALSE,
                              selection = list(target="row", selected=selectedRow()),
                              options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})


############ insert & close tab ################
opened_tab <- reactiveVal(NULL)

observeEvent(input$report_list_table_rows_selected, priority = 20, {
  com_table <- components()
  opened_tab(com_table[input$report_list_table_row_last_clicked, "table_name"])
  print(opened_tab())
})

observeEvent(opened_tab(), priority = 20, {
  com_table <- components()
  if ("arm_or_cohort" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "ArmCohortPanel" #createStudy
                       , title = "Arm or Cohort" #Create New Study
                       , uiOutput("createArmCohort"))) #createNewStudy
  }
  if ("study_2_condition_or_disease" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "ConditionDiseasePanel" #createStudy
                       , title = "Condition or Disease" #Create New Study
                       , uiOutput("createConditionDisease"))) #createNewStudy
  }
  if ("planned_visit" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "PlannedVisitPanel" #createStudy
                       , title = "Planned Visit" #Create New Study
                       , uiOutput("createPlannedVist"))) #createNewStudy
  }
  if ("experiment" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "ExperimentPanel" #createStudy
                       , title = "Experiment" #Create New Study
                       , uiOutput("createExperiment"))) #createNewStudy
  }
  if ("intervention" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "InterventionPanel"
                       , title = "Intervention"
                       , uiOutput("createIntervention")))
  }
  if ("inclusion_exclusion" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "InclusionExclusionPanel" #createStudy
                       , title = "Inclusion Exclusion" #Create New Study
                       , uiOutput("createIncluExclu"))) #createNewStudy
  }
  if ("treatment" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "TreatmentPanel" #createStudy
                       , title = "Treatment" #Create New Study
                       , uiOutput("createTreatment"))) #createNewStudy
  }
  if ("study_categorization" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "CategorizationPanel" #createStudy
                       , title = "Categorization" #Create New Study
                       , uiOutput("createCategorization"))) #createNewStudy
  }
  if ("study_link" %in% opened_tab()) {
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "StudyLinkPanel" #createStudy
                       , title = "Study Link" #Create New Study
                       , uiOutput("createStudyLink"))) #createNewStudy
  }
})
