### sidebar controls body panel management
source("helpers.R", local=TRUE) # references functions found in the helpers.R file

### sidebar controls body panel management
observeEvent(input$rb, {
    shinyjs::show("body_panel_id")

    if(input$rb == "addData"){
      updateTabsetPanel(session, "body_panel_id", selected = paste0("panel", input$rb))
      removeTab(inputId = "body_panel_id", target="add_workspace_tab")
      removeTab(inputId = "body_panel_id", target = "add_users_tab")
      removeTab(inputId = "body_panel_id", target = "study_personnel")
      removeTab(inputId = "body_panel_id", target = "program_2_personnel")
      removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
      removeTab(inputId = "body_panel_id", target = "addData2Study")
      removeTab(inputId = "body_panel_id", target = "getBlankTemplatePanel")
      removeTab(inputId = "body_panel_id", target = "previewTemplate")
      removeTab(inputId = "body_panel_id", target = "createStudy")
      removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
      removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
      removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
      removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
      removeTab(inputId = "body_panel_id", target = "InterventionPanel")
      removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
      removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
      removeTab(inputId = "body_panel_id", target = "ImmuExpoPanel")
      removeTab(inputId = "body_panel_id", target = "add_program_tab")
      removeTab(inputId = "body_panel_id", target = "reagentPanel")
      removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
      removeTab(inputId = "body_panel_id", target="standalone.temp")
      removeTab(inputId = "body_panel_id", target="temp.var.std")
      removeTab(inputId = "body_panel_id", target="primary.temp.instr")
      removeTab(inputId = "body_panel_id", target="primary.temp")
      removeTab(inputId = "body_panel_id", target="temp.var.prm")
      removeTab(inputId = "body_panel_id", target="addAssociated")
      removeTab(inputId = "body_panel_id", target="showAssociated")
      removeTab(inputId = "body_panel_id", target="associated_files")
      removeTab(inputId = "body_panel_id", target="immportUpload")
      removeTab(inputId = "body_panel_id", target="study")
      removeTab(inputId = "body_panel_id", target="viewStudyTab")
      
      # For Add New Data, don't show any initial tab - just show sidebar options
      rv_data$data <- NULL
      reset('upload_to_shiny')
      delete_downstream_tabs("study")
    }

  if(input$rb == "immportUpload"){
    removeTab(inputId = "body_panel_id", target = "study")
    removeTab(inputId = "body_panel_id", target = "about_study")
    removeTab(inputId = "body_panel_id", target = "immportUpload")
    rv_data$data <- NULL
    reset('upload_to_shiny')
    delete_downstream_tabs("study")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target = "addData2Study")
    removeTab(inputId = "body_panel_id", target = "getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target="associated_files")

    insertTab(inputId = "body_panel_id",
              tabPanel(value = "immportUpload"
                       , title = "Immport Upload"
                       , uiOutput("immport_upload_ui")
              ),
              target = NULL,
              position = "after"
    )

    showTab(inputId = "body_panel_id", target = "immportUpload", select = TRUE, session = getDefaultReactiveDomain())
  }

  if(input$rb == "manageWorkspaces"){
    # Remove all existing tabs
    removeTab(inputId = "body_panel_id", target = "study")
    removeTab(inputId = "body_panel_id", target = "about_study")
    removeTab(inputId = "body_panel_id", target = "immportUpload")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target = "addData2Study")
    removeTab(inputId = "body_panel_id", target = "getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target="associated_files")
    removeTab(inputId = "body_panel_id", target="workspaceManagementTab")

    # Insert workspace management tab
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "workspaceManagementTab"
                       , title = "Workspace Management"
                       , uiOutput("workspace_management_ui")
              ),
              target = NULL,
              position = "after"
    )

    showTab(inputId = "body_panel_id", target = "workspaceManagementTab", select = TRUE, session = getDefaultReactiveDomain())
    rv_data$data <- NULL
    reset('upload_to_shiny')
    delete_downstream_tabs("study")
  }

  if(input$rb == "viewStudy"){
    # Remove all existing tabs
    removeTab(inputId = "body_panel_id", target = "study")
    removeTab(inputId = "body_panel_id", target = "about_study")
    removeTab(inputId = "body_panel_id", target = "immportUpload")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target = "addData2Study")
    removeTab(inputId = "body_panel_id", target = "getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target="associated_files")
    removeTab(inputId = "body_panel_id", target="workspaceManagementTab")
    removeTab(inputId = "body_panel_id", target="viewStudyTab")

    # Insert the original "Available Studies" table
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "study"
                       , title = "Available Studies"
                       , UI_lookup_table("study")
                       , shinydashboard::box(title = span( icon("filter"), "Filter Studies")
                                             , width=14, status="primary", solidHeader=T, collapsible=T, collapsed=T
                                             , datamods::select_group_ui(
                                               id = "studyfilters",
                                               params = list(
                                                 research_focus = list(inputId = "research_focus", label = "Research Focus", placeholder = 'Select'),
                                                 condition_preferred = list(inputId = "condition_preferred", label = "Condition", placeholder = 'Select'),
                                                 type = list(inputId = "type", label = "Biosample Type", placeholder = 'Select'),
                                                 measurement_technique = list(inputId = "measurement_technique", label = "Assay Method", placeholder = 'Select'),
                                                 sponsoring_organization = list(inputId = "sponsoring_organization", label = "Sponsor", placeholder = 'Select'),
                                                 study_accession = list(inputId = "study_accession", label = "Study Accession", placeholder = 'Select'),
                                                 clinical_trial = list(inputId = "clinical_trial", label = "Clinical Trial", placeholder = 'Select'),
                                                 species = list(inputId = "species", label = "Species", placeholder = 'Select'),
                                                 sex = list(inputId = "sex", label = "Sex", placeholder = 'Select'),
                                                 have_test = list(inputId = "have_test", label = "Has Lab Test", placeholder = 'Select'),
                                                 have_assessment = list(inputId = "have_assessment", label = "Has Lab Assessment", placeholder = 'Select')
                                               ), inline = FALSE
                                             ))
                       , shinydashboard::box(title = "Available Studies: Data Table"
                             , width=14, status="primary", solidHeader=T
                             , DT::dataTableOutput("studiestab2")
                       )
              ),
              target = NULL,
              position = "after"
    )

    showTab(inputId = "body_panel_id", target = "study", select = TRUE, session = getDefaultReactiveDomain())
    rv_data$data <- NULL
    reset('upload_to_shiny')
    delete_downstream_tabs("study")
  }

    updateTabsetPanel(session, "body_panel_id",selected = input$rb)

    updateRadioButtons(session, "rb_add",
                       choiceNames = list(
                         HTML("<p style='color:#2c3e50; font-weight: 500;'>Create new study</p>")
                         ,HTML("<p style='color:#2c3e50; font-weight: 500;'>Add reagent</p>")
                         ,HTML("<p style='color:#2c3e50; font-weight: 500;'>Add documents and raw files</p>")
                         ,HTML("<p style='color:#2c3e50; font-weight: 500;'>Add templates</p>")
                         ,HTML("<p style='color:#2c3e50; font-weight: 500;'>Add study personnel</p>")
                       ),
                       choiceValues = list("createStudy", "addReagent", "addAssociated", "addData2Study", "add_study_personnel"
                       ), selected = character(0)
    )

    updateSelectInput(session, "addData_selectStudy"
                      , choices = c("Click here", unique(all_study_list$study_accession))
                      , selected = NULL)

    updateSelectInput(session, "getTemplate_selectStudy"
                      , choices = c("Click here", unique(all_study_list$study_accession))
                      , selected = NULL)

    upload_state_value$upload_state <- 'reset'



    updateTabsetPanel(session, "body_panel_id", selected = input$rb)

})

observeEvent(input$rb_add, {
  if(input$rb_add == "createStudy"){
    removeTab(inputId = "body_panel_id", target = "add_workspace_tab")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target = "standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target = "standalone.temp")
    removeTab(inputId = "body_panel_id", target = "temp.var.std")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "primary.temp.instr")
    removeTab(inputId = "body_panel_id", target = "primary.temp")
    removeTab(inputId = "body_panel_id", target = "temp.var.prm")
    removeTab(inputId = "body_panel_id", target = "addData2Study")
    removeTab(inputId = "body_panel_id", target = "getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target ="addAssociated")
    removeTab(inputId = "body_panel_id", target = "showAssociated")
    removeTab(inputId = "body_panel_id", target = "previewTemplate")

  }

  if(input$rb_add == "addData2Study"){
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target = "add_users")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
    rv_data$data <- NULL
    reset('upload_to_shiny')
  }

  if(input$rb_add == "addAssociated"){
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addData2Study")
    removeTab(inputId = "body_panel_id", target="getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target="previewTemplate")
  }

  if(input$rb_add == "immuneExposure"){
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target = "standalone.temp")
    removeTab(inputId = "body_panel_id", target = "temp.var.std")
    removeTab(inputId = "body_panel_id", target = "primary.temp.instr")
    removeTab(inputId = "body_panel_id", target = "primary.temp")
    removeTab(inputId = "body_panel_id", target = "temp.var.prm")
    removeTab(inputId = "body_panel_id", target = "addData2Study")
    removeTab(inputId = "body_panel_id", target = "getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target = "addAssociated")
    removeTab(inputId = "body_panel_id", target = "showAssociated")
    removeTab(inputId = "body_panel_id", target = "previewTemplate")
  }

  if(input$rb_add == "addReagent"){
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "study_personnel")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target ="ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addData2Study")
    removeTab(inputId = "body_panel_id", target="getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
    removeTab(inputId = "body_panel_id", target="previewTemplate")
  }

  if(input$rb_add == "add_study_personnel"){
    removeTab(inputId = "body_panel_id", target="add_workspace_tab")
    removeTab(inputId = "body_panel_id", target = "add_users_tab")
    removeTab(inputId = "body_panel_id", target = "program_2_personnel")
    removeTab(inputId = "body_panel_id", target = "add_personnel_tab")
    removeTab(inputId = "body_panel_id", target ="ImmuExpoPanel")
    removeTab(inputId = "body_panel_id", target = "add_program_tab")
    removeTab(inputId = "body_panel_id", target = "createStudy")
    removeTab(inputId = "body_panel_id", target = "ArmCohortPanel")
    removeTab(inputId = "body_panel_id", target = "ConditionDiseasePanel")
    removeTab(inputId = "body_panel_id", target = "PlannedVisitPanel")
    removeTab(inputId = "body_panel_id", target = "ExperimentPanel")
    removeTab(inputId = "body_panel_id", target = "InterventionPanel")
    removeTab(inputId = "body_panel_id", target = "InclusionExclusionPanel")
    removeTab(inputId = "body_panel_id", target = "TreatmentPanel")
    removeTab(inputId = "body_panel_id", target = "reagentPanel")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addData2Study")
    removeTab(inputId = "body_panel_id", target="getBlankTemplatePanel")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
    removeTab(inputId = "body_panel_id", target="previewTemplate")
  }

  updateTabsetPanel(session, "body_panel_id", selected = input$rb_add)
})







