##### Change on previewing a new file - upload_to_shiny
upload_state_value <- reactiveValues(upload_state = NULL)
rv_value_button <- reactiveValues(valueButton = 0)
rv_data <- reactiveValues(data = NULL)

# create upload pathname
output$upload_path_text <- renderText({
  paste(stri_replace_all_charclass(Sys.getenv("upload_template_path"), "\\p{WHITE_SPACE}", ""))
})

getData <- reactive({
  if(is.null(input$upload_to_shiny)) return(NULL)
})

output$fileUploaded <- reactive({
  return(!is.null(getData()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

### main observe that creates base tabs for getting blank templates and uploading filled templates
observe( {
  req(input$rb_add == "addData2Study")
  
  ### create Blank Templates UI 
  output$getBlankTemplates <- renderUI({ 
    tagList(
      selectInput("getTemplate_selectStudy",
                  "Select Existing Study Accession",
                  choices = c("Click here", unique(madi_study_list$study_accession)), selected = NULL, multiple = FALSE)
      , conditionalPanel(
        condition = "input.getTemplate_selectStudy != 'Click here'"
        , radioButtons("rb_templates", "Please choose primary templates that are necessary 
                               for a study or standalone templates that can be used as additional materials:",
                       choiceNames = list(
                         HTML("<p style='color:black;'>Primary Templates</p>"),
                         HTML("<p style='color:black;'>Standalone Templates</p>")
                       ),
                       choiceValues = list(
                         "primary", "standalone"
                       ), selected = character(0))
        , conditionalPanel(
          condition = "input.rb_templates == 'primary'"
          , shinydashboard::box(title = "Template Set", width = 12,  status = "primary", 
                solidHeader = T, collapsible = F,
                selectInput("temp_set_selected_prm", "Select A Template Set",
                            choices=template_set[which(template_set$template_set_group=="primary"),]$template_set_name),
                shinydashboard::box("Note", width=12,
                    textOutput("prm.set.instr.box"))
          )
          , shinydashboard::box(title = "Templates in the set:", width = 12,  status = "primary", 
                solidHeader = T, collapsible = F,
                DT::dataTableOutput("primary.tab"),
                downloadButton("download_temp_prm", 'Download')
          )
        )
        , conditionalPanel(
          condition = "input.rb_templates == 'standalone'"
          ,  shinydashboard::box(title = "Template Set", width = 12,  status = "primary", 
                 solidHeader = T, collapsible = F,
                 selectInput("temp_set_selected_std", "Select A Template Set",
                             choices=template_set[which(template_set$template_set_group=="standalone"),]$template_set_name),
                 shinydashboard::box("Note", width=12,
                     textOutput("std.set.instr.box"))
          )
          ,  shinydashboard::box(title = "Templates in the set:", width = 12,  status = "primary", 
                 solidHeader = T, collapsible = F,
                 DT::dataTableOutput("standalone.tab"),
                 downloadButton("download_temp_std", 'Download')
                 
          )
        )
      )
    )
  })
  
  ### create Filled Templates UI
  output$addFilledTemplates <- renderUI({ 
    tagList(
      selectInput("addData_selectStudy",
                  "Select Existing Study Accession",
                  choices = c("Click here", unique(madi_study_list$study_accession)), selected = NULL, multiple = FALSE)
      , conditionalPanel(
        condition = "input.addData_selectStudy != 'Click here'"
        ,fileInput("upload_to_shiny"
                   ,label="Upload a file(only accept xlsx, xls)"
                   ,accept=c(".xlsx",".xls")
                   ,multiple=FALSE)
        ,verbatimTextOutput("rows_in_preview")
        , conditionalPanel(
          condition = "!is.null(input.upload_to_shiny)==true"
          ,textInput("upload_comment",label="Comment for uploaded file", placeholder="Enter comment...")
          ,tags$hr()
          ,actionButton("upload_to_server",label="Click to upload template")
          ,textOutput("validation")
          , conditionalPanel(
            condition = "!is.null(input.upload_to_server)==true"
            , shinydashboard::box(title = "Templates in queue waiting for ingestion processing:"
                  , width = 14,  status = "primary"
                  , solidHeader = T, collapsible = F
                  , DT::dataTableOutput("upload_files")
            )
            , shinydashboard::box(title = "Tracking for the uploaded template:"
                  , width = 14,  status = "primary"
                  , solidHeader = T, collapsible = F
                  , reactable::reactableOutput("event_uploaded_template", width = "100%")
            )
          )
        )
      )
    )
  })
  
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "getBlankTemplatePanel"
                     , title = "Get blank templates"
                     , uiOutput("getBlankTemplates")))
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "addData2Study"
                     , title = "Add Filled Templates"
                     , uiOutput("addFilledTemplates")))
} )

#### create the list of previously uploaded files.####
rfv_list <- reactive({
  if(input$upload_to_server > 0 || (input$rb_add == "addData2Study")){      
    list.files(Sys.getenv("upload_template_path"))
  }
})

rfv_user <- reactive({
  if(input$upload_to_server > 0 || (input$rb_add == "addData2Study")){      
    list <- list.files(Sys.getenv("upload_template_path"))
    list <- as.data.frame(list)
    if (length(list) != 0) {
      stringr::str_detect(list$list,userData_upload()$username)
    } else {
      TRUE
    }
  }
})

output$upload_files <- DT::renderDataTable({
  list_uploaded <- rfv_list()
  for (i in seq_along(list_uploaded)) {
    list_uploaded[i] <- paste0("filename-", i, " ", list_uploaded[i] )
  }
  listdf <- as.data.frame(list_uploaded)
  listdf <- listdf[rfv_user(),]
  listdf <- as.data.frame(listdf)
  colnames(listdf) <- c('Uploaded Files in Queue')
  listdf <- as.data.frame(listdf)
})

### dataTable for the box that shows relevant events
row_count <- 0L
previous_row_count <- 0L
event_display_data <- reactiveVal(NULL)
track_events_upload <- reactivePoll(
  1000, session,
  # This function returns the time that log_file was last modified
  checkFunc = function() {
    ## print(paste("Running checkFunc:", Sys.time()))
    if(DBI::dbIsValid(conn) && dbExistsTable(conn, Id(schema = "madi_track", table = "track_events"))){
      ## print(paste("Verified table and connection:", Sys.time()))
      row_count <<- DBI::dbGetQuery(conn, "SELECT MAX(event_id) FROM madi_track.track_events;")[[1]]
      ### print(paste("Polling max event_id from track_events:", Sys.time()," row count: ", row_count))
      
    } else {
      print(paste("Failed to verify table and connection:", Sys.time()))
      0L
    }
  },
  # This function returns the content of log_file
  valueFunc = function() {
    if(DBI::dbIsValid(conn) && dbExistsTable(conn, Id(schema = "madi_track", table = "track_events"))) {
      ### print(paste("Pulling table data from track_events:", Sys.time()," row count: ", row_count))
      track_upload_query <- glue_sql(.con = conn, 
       "SELECT event_id, event_catalog_id, template_path, template_filename, created_at, comment FROM madi_track.track_events WHERE workspace_id IN ({userData_upload()$workspace_id}) ORDER BY event_id DESC LIMIT 6;")
      ### print(paste0("upload_tracking_events:", track_upload_query))
      DT <- setDT(DBI::dbGetQuery(conn, track_upload_query))
      previous_row_count <<- row_count
      DT
    }
  }
)

observeEvent(track_events_upload(), {
  event_display_data(track_events_upload())
  }) 
  
#output$event_uploaded_template <- DT::renderDataTable({
#  req(event_display_data())}, server = FALSE)
output$event_uploaded_template <- reactable::renderReactable({
  table_data <- event_display_data()
  reactable(table_data,
            selection = "single",
            onClick = "select",
            details = colDef(
              name = "",
              details = JS("function(rowInfo) {
                        return `Details for row: ${rowInfo.index + 1}` +
                        `<pre>${JSON.stringify(rowInfo.values, null, 2)}</pre>`
                        }"),
              html = TRUE
            ),
            theme = reactableTheme(
              rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
            ),
            wrap = F)
})



### create the preview template data table
output$preview = DT::renderDataTable({
  # validate(
  #   need(input$rb_add,'addData2Study'), 
  #   need(input$upload_to_shiny !='', 'Please upload the dataset')
  # )
  req(input$rb_add == "addData2Study")
  df_preview <- rv_data$data
  nrows_preview <- tally(df_preview)
  head(df_preview, 7)
})

### read template and create the preview template tab
observeEvent(input$upload_to_shiny, {
  rv_data$data <- read_excel(input$upload_to_shiny$datapath)
  output$rows_in_preview = renderText({paste0("Number of rows in template: ", nrow(rv_data$data))})
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "previewTemplate",
                     title = "Template Preview",
                     DT::dataTableOutput("preview")))
  updateActionButton(session, inputId = "upload_to_server", label = "Click to upload template")
  rv_value_button$valueButton = 0
})

#### Each time upload_to_server button is click, add 1 to custom value of the button
observeEvent(input$upload_to_server, {
  
  rv_value_button$valueButton = rv_value_button$valueButton + 1 
  reset('upload_to_shiny')
  updateActionButton(session, inputId = "upload_to_server", label = "Uploaded to Ingestion Server")
  
  sel_study_accession <- input$addData_selectStudy
  template_original_name <- input$upload_to_shiny$name
  timestamp_file <- stri_replace_all_charclass(format(Sys.time(),"%Y-%m-%d_%H.%M.%S"), "\\p{WHITE_SPACE}", "")
  template_filename <- paste(userData_upload()$username,timestamp_file,template_original_name,sep="_")
  
  t_upload_dir <- stri_replace_all_charclass(Sys.getenv("upload_template_path"), "\\p{WHITE_SPACE}", "")
  template_path <- file.path(t_upload_dir,template_filename)
  print(template_path)
  
  p_upload_dir <- str_replace(t_upload_dir, "madi-preprod", "data")
  python_path <- file.path(p_upload_dir,template_filename)
  print(python_path)
  
  ### pop up with the actual path used to store file and the path sent to track_events to 
  ### ensure that the correct path arrives for the python engine
  showModal(modalDialog(
    title = "Where the template is stored: ",
    paste0("stored_path: ", template_path, "\n  python_path: ", python_path)
  ))
  
  file.copy(input$upload_to_shiny$datapath,template_path)
  
  #browser()
  
  # validate payload -- a result of 'valid' indicates that the payload will work for the ingest endpoint
  tryCatch({
    upload_task_id <- httr2::request(endpoints$INGEST) %>%
    httr2::req_body_json(
      list(
        user_id = userData_upload()$users_id,
        workspace_id = userData_upload()$workspace_id,
        file_path = python_path,
        template_id = template_original_name
      )
    ) %>%
    httr2::req_headers(`X-API-KEY` = Sys.getenv("madi_api_key")) %>%  
    httr2::req_perform() %>% 
    httr2::resp_body_string()
    
  },
  #if an error occurs, tell me the error
  error=function(e) {
    message('An Error Occurred')
    print(e)
  },
  #if a warning occurs, tell me the warning
  warning=function(w) {
    message('A Warning Occurred')
    print(w)
    return(NA)
  }
  )
  # print(paste0(sel_study_accession, " ", userData_upload()$username, " ", userData_upload()$workspace_id, " ", template_filename))
  # print(input$upload_comment)
  
  if (file.exists(template_path)){
    insert_query_track <- glue("INSERT INTO madi_track.track_events (event_id, event_catalog_id,
          study_accession, user_name, comment, created_by, created_at, updated_by, updated_at,
          template_path, template_original_name, template_filename, workspace_id)
           VALUES (nextval('madi_track.event_id_seq'), 'Ingest_Project_staff_Stored_Data_Template_ToFileserver',
           '{sel_study_accession}', '{userData_upload()$username}', '{input$upload_comment}',
           '{userData_upload()$username}', current_timestamp, '{userData_upload()$username}', current_timestamp,
           '{python_path}', '{template_original_name}', '{template_filename}', '{userData_upload()$workspace_id}');")
   
     ### popup to verify the structure of the insert command to the track_events table.
    showModal(modalDialog(
      title = "INSERT command for track_events: ",
      paste0(insert_query_track)
    ))
    DBI::dbSendQuery(conn, glue_sql(.con = conn,insert_query_track))
    
  }
  

  
})

observeEvent(input$body_panel_id, {
  if (input$body_panel_id == "getBlankTemplatePanel"){
    updateSelectInput(session, "addData_selectStudy"
                      , choices = c("Click here", unique(madi_study_list$study_accession))
                      , selected = NULL)
    output$rows_in_preview = NULL
    rv_data$data <- NULL
    reset('upload_to_shiny')
    upload_state_value$upload_state <- 'reset'
    updateActionButton(session, inputId = "upload_to_server", label = "Click to upload template")
    rv_value_button$valueButton = 0
    removeTab(inputId = "body_panel_id", target = "previewTemplate")
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
  }
  if (input$body_panel_id == "addData2Study"){
    updateSelectInput(session, "getTemplate_selectStudy"
                      , choices = c("Click here", unique(madi_study_list$study_accession))
                      , selected = NULL)
    upload_state_value$upload_state <- 'reset'
    updateActionButton(session, inputId = "upload_to_server", label = "Click to upload template")
    rv_value_button$valueButton = 0
    removeTab(inputId = "body_panel_id", target="standalone.temp.instr")
    removeTab(inputId = "body_panel_id", target="standalone.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.std")
    removeTab(inputId = "body_panel_id", target="primary.temp.instr")
    removeTab(inputId = "body_panel_id", target="primary.temp")
    removeTab(inputId = "body_panel_id", target="temp.var.prm")
    removeTab(inputId = "body_panel_id", target="addAssociated")
    removeTab(inputId = "body_panel_id", target="showAssociated")
  }
})







