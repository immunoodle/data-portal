### Create new study through an editable madi_dat.study table.

# Helper functions for handling NULL/empty values
`%||%` <- function(a, b) if (is.null(a)) b else a
nv <- function(x) { if (is.null(x) || length(x)==0 || identical(x, "") ) NA_character_ else as.character(x)[1] }

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

  ws <- as.integer(userData_upload()$workspace_id)
  sql <- "
    SELECT study.study_accession, actual_completion_date, actual_enrollment, actual_start_date, age_unit,
           brief_description, brief_title, clinical_trial, condition_studied, dcl_id, description, doi,
           endpoints, gender_included, hypothesis, initial_data_release_date, initial_data_release_version,
           intervention_agent, latest_data_release_date, latest_data_release_version, maximum_age, minimum_age,
           objectives, official_title, sponsoring_organization, target_enrollment, workspace_id,
           condition_reported, condition_preferred, research_focus
    FROM madi_dat.study
    LEFT JOIN madi_dat.study_2_condition_or_disease AS cd USING (study_accession)
    LEFT JOIN madi_dat.study_categorization AS sc USING (study_accession)
    WHERE workspace_id = $1;
  "
  DBI::dbGetQuery(conn, sql, params = list(ws))
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
                  textInput(
                    "study_accession", 
                    labelMandatory("Study Accession"), 
                    value = DBI::dbGetQuery(conn, "SELECT CONCAT('SDY', MAX(CAST(SUBSTRING(study_accession,4) AS INTEGER))+1) AS next FROM madi_dat.study;")[["next"]][1]
                  )
                ),
                dateInput("actual_completion_date", "Actual Completion Date", value = NULL, format = "yyyy-mm-dd"),
                numericInput("actual_enrollment", "Actual Enrollment", value = NA),
                dateInput("actual_start_date", "Actual Start Date", value = NULL, format = "yyyy-mm-dd"),
                selectizeInput("age_unit", "Age Unit", options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_time_unit;")[["name"]]),
                textInput("brief_description", "Brief Description", placeholder = ""),
                textInput("brief_title", "Brief Title", placeholder = ""),
                textInput("clinical_trial", labelMandatory("Clinical Trial"), placeholder = "Type Y or N"),
                textInput("condition_studied", "Condition Studied", placeholder = ""),
                selectizeInput("dcl_id", labelMandatory("DCL ID"), options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT id FROM madi_dat.lk_data_completeness;")[["id"]])
              ),
              column(
                width = 4,
                textInput("description", "Description", placeholder = ""),
                textInput("doi", "DOI", placeholder = ""),
                textInput("endpoints", "Endpoints", placeholder = ""),
                textInput("gender_included", "Gender Included", placeholder = ""),
                textInput("hypothesis", "Hypothesis", placeholder = ""),
                dateInput("initial_data_release_date", "initial_data_release_date", value=NULL, format = "yyyy-mm-dd"),
                textInput("initial_data_release_version", "initial_data_release_version", placeholder = ""),
                textInput("intervention_agent", "intervention_agent", placeholder = ""),
                dateInput("latest_data_release_date", "latest_data_release_date", value=NULL, format = "yyyy-mm-dd"),
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
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_research_focus;")[["name"]]), 
                textInput("condition_reported", labelMandatory("Condition Reported"), placeholder = ""),
                selectizeInput("condition_preferred", "Condition Preferred", options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_disease;")[["name"]])
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
  print(paste("DEBUG: appendData called with data:", class(data)))
  
  # Build a Postgres-style parameterized INSERT
  sql_insert <- "
INSERT INTO madi_dat.study (
  study_accession, actual_completion_date, actual_enrollment, actual_start_date, age_unit,
  brief_description, brief_title, clinical_trial, condition_studied, dcl_id, description, doi,
  endpoints, gender_included, hypothesis, initial_data_release_date, initial_data_release_version,
  intervention_agent, latest_data_release_date, latest_data_release_version, maximum_age,
  minimum_age, objectives, official_title, sponsoring_organization, target_enrollment, workspace_id
) VALUES (
  $1, $2::date, $3::int, $4::date, $5::text,
  $6::text, $7::text, $8::text, $9::text, $10::int, $11::text, $12::text,
  $13::text, $14::text, $15::text, $16::date, $17::text,
  $18::text, $19::date, $20::text, $21::text,
  $22::text, $23::text, $24::text, $25::text, $26::int, $27::int
);
"

  # Convert inputs to correctly-typed R values (so NULL/NA become SQL NULLs)
  p1  <- if (isTruthy(input$study_accession)) as.character(input$study_accession) else NA_character_
  p2  <- if (length(input$actual_completion_date)) as.Date(input$actual_completion_date) else as.Date(NA)
  p3  <- if (isTruthy(input$actual_enrollment)) as.integer(input$actual_enrollment) else NA_integer_
  p4  <- if (length(input$actual_start_date)) as.Date(input$actual_start_date) else as.Date(NA)
  p5  <- if (isTruthy(input$age_unit)) as.character(input$age_unit) else NA_character_
  p6  <- nv(input$brief_description)
  p7  <- nv(input$brief_title)
  p8  <- nv(toupper(substr(trimws(input$clinical_trial %||% ""), 1, 1))) # "Y"/"N"
  p9  <- nv(input$condition_studied)
  p10 <- if (isTruthy(input$dcl_id)) as.integer(input$dcl_id) else NA_integer_
  p11 <- nv(input$description)
  p12 <- nv(input$doi)
  p13 <- nv(input$endpoints)
  p14 <- nv(input$gender_included)
  p15 <- nv(input$hypothesis)
  p16 <- if (length(input$initial_data_release_date)) as.Date(input$initial_data_release_date) else as.Date(NA)
  p17 <- nv(input$initial_data_release_version)
  p18 <- nv(input$intervention_agent)
  p19 <- if (length(input$latest_data_release_date)) as.Date(input$latest_data_release_date) else as.Date(NA)
  p20 <- nv(input$latest_data_release_version)
  p21 <- nv(as.character(input$maximum_age)) # Keep as text since DB expects varchar
  p22 <- nv(as.character(input$minimum_age)) # Keep as text since DB expects varchar
  p23 <- nv(input$objectives)
  p24 <- nv(input$official_title)
  p25 <- nv(input$sponsoring_organization)
  p26 <- if (isTruthy(input$target_enrollment)) as.integer(input$target_enrollment) else NA_integer_
  p27 <- as.integer(input$workspace_id)

  params <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27)

  print("DEBUG: Parameter summary:")
  for(i in 1:length(params)) {
    param_val <- params[[i]]
    print(paste("  Param", i, ":", class(param_val), "length:", length(param_val), "value:", toString(param_val)))
  }

  tryCatch({
    result <- DBI::dbExecute(conn, sql_insert, params = params)
    print(paste("Study inserted successfully! Rows affected:", result))
    
    # Only proceed with categorization and condition if study was inserted successfully
    if(result > 0) {
      print("Study insert successful, proceeding with categorization and condition...")
      return(TRUE)  # Signal success
    } else {
      print("Study insert failed - no rows affected")
      return(FALSE)
    }
  }, error = function(e) {
    print(paste("SQL insert failed:", e$message))
    return(FALSE)
  })
  
  print("after add study")
}

insert_study_categorization <- function(){
  rf <- if (length(input$research_focus)) as.character(input$research_focus[[1]]) else NA_character_
  sa <- as.character(input$study_accession)

  sql <- "INSERT INTO madi_dat.study_categorization (research_focus, study_accession)
          VALUES ($1, $2)"
  
  print(paste("insert_study_categorization SQL ==>", sql))
  print(paste("insert_study_categorization params ==> rf='", rf, "' sa='", sa, "'"))

  DBI::dbExecute(
    conn,
    sql,
    params = list(rf, sa)
  )
}

appendData_cd <- function(data){
  DBI::dbExecute(
    conn,
    "INSERT INTO madi_dat.study_2_condition_or_disease
       (study_accession, condition_reported, condition_preferred)
     VALUES ($1, $2, $3)",
    params = list(
      input$study_accession,
      input$condition_reported %||% NA_character_,
      input$condition_preferred %||% NA_character_
    )
  )
}

observeEvent(input$add_button, priority = 20,{
  entry_form("submit_add")
  updateTextInput(session, "workspace_id", value = paste(userData_upload()$workspace_id))
})

observeEvent(input$submit_add, priority = 20,{
  ok <- FALSE
  tryCatch({
    DBI::dbWithTransaction(conn, {
      if (!appendData(formData())) stop("study insert failed")
      insert_study_categorization()
      appendData_cd(formData_cd())
    })
    ok <- TRUE
  }, error = function(e) {
    print(paste("Create study transaction failed:", e$message))
  })

  if (ok) print("✅ Study + categorization + condition committed")
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
    updateSelectizeInput(session, "age_unit", selected = SQL_df_study[, "age_unit"])
    updateTextInput(session, "brief_description", value = SQL_df_study[, "brief_description"])
    updateTextInput(session, "brief_title", value = SQL_df_study[, "brief_title"])
    updateTextInput(session, "clinical_trial", value = SQL_df_study[, "clinical_trial"])
    updateTextInput(session, "condition_studied", value = SQL_df_study[, "condition_studied"])
    updateSelectizeInput(session, "dcl_id", selected = SQL_df_study[, "dcl_id"])
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
    updateSelectizeInput(session, "condition_preferred", selected = SQL_df_cd[, "condition_preferred"])
    ## ?
    updateSelectizeInput(session, "research_focus", selected = SQL_df_cate[, "research_focus"])
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
  
  DBI::dbExecute(
    conn,
    "UPDATE madi_dat.study_categorization
       SET research_focus = NULLIF($1,'')
     WHERE study_accession = $2",
    params = list(input$research_focus, input$study_accession)
  )
  
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
