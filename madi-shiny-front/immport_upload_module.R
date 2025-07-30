# immport_ui_def.R

# Load libraries needed JUST for the UI definition IF NOT loaded in main app already
# library(shiny)
# library(shinyWidgets) # Needed for pickerInput

# Define constants/choices needed for the UI
immport_template_choices <- local({
  template_names <- c(
      'basic_study_design.json', 'experiments.json', 'labTestPanels.json',
      'labTest_Results.json', 'assessmentpanel.json', 'assessmentcomponent.json',
      'adverseEvents.json', 'subjectHumans.json', 'subjectAnimals.json',
      'bioSamples.json', 'interventions.json', 'immuneExposure.json',
      'labTests.json', 'controlSamples.json', 'assessments.json',
      'CyTOF_Derived_data.json', 'ELISA_Results.json', 'ELISPOT_Results.json',
      'experimentSamples.CYTOF.json', 'experimentSamples.ELISA.json',
      'experimentSamples.ELISPOT.json', 'experimentSamples.MBAA.json',
      'experimentSamples.Neutralizing_Antibody_Titer.json', # Value 23
      'HAI_Results.json', 'HLA_Typing.json', 'protocols.json', 'MBAA_Results.json',
      'treatments.json', 'Virus_Neutralization_Results.json',
      'publicRepositories.json', 'Reagent_Sets.json', 'reagents.Array.json',
      'reagents.CyTOF.json', 'reagents.ELISA.json', 'reagents.ELISPOT.json',
      'reagents.Flow_Cytometry.json', 'reagents.HAI.json', 'reagents.HLA_Typing.json',
      'reagents.KIR_Typing.json', 'reagents.MBAA.json',
      'reagents.Neutralizing_Antibody_Titer.json', # Value 41
      'reagents.Other.json', 'reagents.PCR.json', 'reagents.Sequencing.json',
      'reagents.Virus_Neutralization.json', 'RNA_SEQ_Results.json',
      'standardCurves.json', 'experimentSamples.HAI.json',
      'experimentSamples.HLA.json',
      'experimentSamples.Neutralizing_Antibody_Titer_Alt.json', # Adjusted name for Value 50
      'experimentSamples.QRT-PCR.json', 'KIR_Typing.json', 'PCR_Results.json',
      'experimentSamples.Virus_Neutralization.json'
  )
  stopifnot(length(template_names) == 54)
  setNames(as.list(1:54), template_names)
})

immport_upload_ui <- function() {
  # Define a common style for bordered sections for a professional look
  section_style <- "border: 1px solid #ddd; padding: 15px; margin-top: 8px; margin-bottom: 18px; border-radius: 4px; background-color: #f9f9f9; box-shadow: 0 1px 2px rgba(0,0,0,0.04);"

  fluidRow( # Main overall row

    column(width = 12,
           tags$details(
             open = TRUE, # Start with instructions open by default
             style = "border: 1px solid #007bff; border-radius: 4px; padding: 15px; margin-bottom: 20px; background-color: #f0f8ff;",
             tags$summary(style = "cursor: pointer; font-weight: bold; color: #007bff;", "Click to toggle submission instructions"),
             tags$div(style = "margin-top: 15px;",

                      tags$p(tags$strong("Welcome to the ImmPort Uploader."), " This tool streamlines the process of preparing and uploading your study data directly to an ImmPort workspace."),

                      tags$div(style="border-left: 4px solid #ffc107; padding-left: 15px; background-color: #fff3cd; margin: 10px 0;",
                               tags$p(tags$strong("Important:"), " You are responsible for verifying the accuracy and completeness of all information within the generated templates. Please ensure you also upload all necessary supporting documentation (e.g., protocols, study documents) as required by ImmPort submission guidelines using the 'Add Extra Files' feature below.")
                      ),

                      h4("Workflow Steps:", style="margin-top: 20px;"),
                      tags$ul(
                        tags$li(tags$strong("1. ImmPort Authentication:"), " Provide your ImmPort credentials to obtain a secure, temporary authorization token. Your password is used only for this single login and is never stored."),
                        tags$li(tags$strong("2. Workspace Operations:"), " Click 'Load/Refresh Workspaces' to fetch your available ImmPort workspaces. Select the specific workspace you intend to upload your files to from the dropdown menu."),
                        tags$li(tags$strong("3. Specify Study:"), " Enter the official Study Accession ID (e.g., SDY123). This ID links all templates and files to the correct study in ImmPort."),
                        tags$li(tags$strong("4. Select Data to Upload:"),
                                tags$ul(
                                  tags$li(tags$strong("4a. Select Templates:"), " Choose one or more official ImmPort metadata templates. The tool will generate these files with the correct structure for you to edit."),
                                  tags$li(tags$strong("4b. Add Extra Files (Optional):"), " Upload any additional files (e.g., protocols, raw data) required for your submission. These will be bundled with your metadata templates into a single zip archive.")
                                )
                        ),
                        tags$li(tags$strong("5. Actions (Generate, Download & Upload):"),
                                tags$ul(
                                  tags$li(tags$strong("Generate Templates:"), " Creates the selected templates in memory. You can then view and edit them in the 'Template Viewer & Editor' panel on the right."),
                                  tags$li(tags$strong("Download Generated Files:"), " Packages all generated templates and any extra files you've added into a single .zip file for you to save locally."),
                                  tags$li(tags$strong("Zip & Upload to ImmPort:"), " Automatically bundles everything into a .zip file and uploads it directly to your selected ImmPort workspace.")
                                )
                        ),
                        tags$li(tags$strong("6. Monitor Status & Tickets:"), " After an upload, a ticket number is created by ImmPort. Use the 'Ticket Information' section to track the processing status and view the validation summary for your submission.")
                      )
             )
           )
    ),

    # --- Left Column (Controls) ---
    column(width = 5,
           tagList(
             h3("1. ImmPort Authentication"),
             div(style = section_style,
                 fluidRow(
                   column(width = 6, textInput("immport_username", "ImmPort Username")),
                   column(width = 6, passwordInput("immport_password", "ImmPort Password"))
                 ),
                 actionButton("immport_login_button", "Login", width = "100%"),
                 br(),br(),
                 verbatimTextOutput("immport_login_status"),
                 conditionalPanel(
                   condition = "output.immport_login_status && output.immport_login_status.includes('successful')",
                   actionButton("immport_logout_button", "Logout (Clear Token)",
                                width = "100%", class = "btn-danger",
                                style = "margin-top: 10px;")
                 )
             ),

             h3("2. Workspace Operations"),
             div(style = section_style,
                 actionButton("immport_fetch_workspaces_button", "Load/Refresh Workspaces & Select", width = "100%"),
                 br(),br(),
                 selectInput("immport_workspace_select", "Select ImmPort Workspace:",
                             choices = list("Load workspaces first" = ""), selected = "", width = "100%"),
                 div(style = "font-size: 0.9em; color: #6c757d; margin-top: 5px; padding: 8px; background-color: #fff3cd; border-radius: 4px;",
                     "💡 Note: You can upload to any ImmPort workspace you have access to, regardless of your local data portal workspace.")
             ),

             h3("3. Specify Study"),
             div(style = section_style,
                 textInput("immport_study_accession", "Study Accession ID (e.g., SDY123)", width = "100%")
             ),

             h3("4a. Select Templates"),
             div(style = section_style,
                 shinyWidgets::pickerInput(
                   inputId = "immport_template_select", label = "Select Templates:",
                   choices = immport_template_choices, multiple = TRUE, width = "100%",
                   options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                                                         selectedTextFormat = "count > 3",
                                                         countSelectedText = "{0} templates selected")
                 )
             ),

             h3("4b. Add Extra Files (Optional)"),
             div(style = section_style,
                 # Enhanced file upload with individual file management
                 div(
                   style = "margin-bottom: 15px;",
                   fileInput("immport_single_file_upload",
                            "Add a file to the submission:",
                            multiple = FALSE,
                            buttonLabel = "Browse & Add...",
                            placeholder = "Choose a file to add"),
                   div(style = "font-size: 0.9em; color: #6c757d; margin-top: 5px;",
                       "💡 Tip: Select one file at a time. You can add multiple files by repeating this process.")
                 ),
                 
                 hr(style = "margin: 15px 0;"),
                 
                 h5("📋 Files Staged for Upload:", style = "color: #495057; margin-bottom: 10px;"),
                 
                 # Dynamic list of staged files with remove buttons
                 div(id = "staged_files_container",
                     style = "min-height: 60px; max-height: 200px; overflow-y: auto; background-color: #ffffff; border: 1px solid #dee2e6; border-radius: 4px; padding: 10px;",
                     uiOutput("immport_dynamic_file_list")
                 ),
                 
                 # Summary info
                 div(style = "margin-top: 10px; padding: 8px; background-color: #e9ecef; border-radius: 4px; font-size: 0.9em;",
                     uiOutput("immport_file_summary")
                 )
             ),

             h3("5. Actions: Generate, Download & Upload"),
             div(style = section_style,
                 actionButton("immport_generate_button", "Generate Templates", width="100%"),
                 br(),br(),
                 downloadButton("immport_download_files_btn", "Download Generated Files", style = "width:100%; margin-top: 5px; margin-bottom: 5px;"),
                 br(),br(),
                 actionButton("immport_zip_upload_button", "Zip & Upload to ImmPort", width="100%", style = "margin-top: 5px;")
             )
           ) # End tagList Left
    ), # End Left Column

    # --- Right Column (Template Viewer/Editor & Ticket Information) ---
    column(width = 7,
           tagList(

             # --- MOVED OPERATION STATUS TO TOP RIGHT ---
             h3("Operation Status"),
             div(style = section_style,
                 verbatimTextOutput("immport_process_status")
             ),

             h3("Template Viewer & Editor"),
             div(style = section_style,
                 selectInput("immport_template_viewer_select", "Select Generated Template to View:",
                             choices = list("No templates generated yet" = ""), selected = "", width = "100%"),
                 actionButton("immport_view_template_btn", "View Content", width = "100%"),
                 hr(),
                 h4("Template Content (Editable):"),
                 actionButton("immport_save_json_changes_btn", "Update Stored Template", class = "btn-primary", width = "100%"),
                 br(),br(),
                 shinyAce::aceEditor(
                   outputId = "immport_json_editor",
                   value = "Select a template to view and edit its content.",
                   mode = "json",
                   theme = "chrome",
                   height = "300px",
                   readOnly = FALSE
                 )
             ),

             h3("Ticket Information"),
             div(style = section_style,
                 selectInput("immport_ticket_select", "Select Ticket ID:",
                             choices = list("No tickets available" = ""), selected = "", width = "100%"),
                 br(),
                 fluidRow(
                   column(width = 6, actionButton("immport_request_ticket_status_btn", "Request Ticket Status", width = "100%")),
                   column(width = 6, actionButton("immport_request_ticket_summary_btn", "Request Ticket Summary", width = "100%"))
                 ),
                 br(),
                 h4("Ticket Status:"),
                 verbatimTextOutput("immport_ticket_status_output"),
                 br(),
                 h4("Ticket Summary:"),
                 div(style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 10px; border-radius: 4px; word-wrap: break-word;",
                     verbatimTextOutput("immport_summary_output")
                 )
             )
           ) # End tagList Right
    ) # End Right Column
  ) # End fluidRow
}
