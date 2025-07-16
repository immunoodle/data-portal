library(aws.s3)
library(glue)
source("helpers.R", local=TRUE) # references functions found in the helpers.R file

#### filtering of the studies
filter_mod <- datamods::select_group_server(
    id = "studyfilters",
    data_r = study,
    vars_r = c("madi_program", "research_focus", "condition_preferred", "type", "measurement_technique", "species", "sex", "clinical_trial", "have_assessment", "have_test", "actual_enrollment", "sponsoring_organization", "study_accession", "tree_id")
  )

# To access study accession selected
rv_study_selected <- reactiveValues(study_accession = NA,
                                    brief_title = NA)

#### showing the all studies table with applied filters
output$studiestab2 <- DT::renderDataTable({
  # input$reset
  full_row <- filter_mod() %>%
    dplyr::filter(madi_program %in% input$madi_studies)
  unq_row <- full_row %>%
    dplyr::select(study_accession, sponsoring_organization, brief_title, research_focus, clinical_trial, tree_id) %>%
    distinct(study_accession, .keep_all = TRUE)

  # Updating the rv_study_selected reactiveVal
  rv_study_selected$study_accession <- unq_row$study_accession
  rv_study_selected$brief_title <- unq_row$brief_title

  unq_row
}, selection = 'single',
options = list(scrollX = TRUE)
)

## All Studies
# filtering only unique study_accessions
study_table_value <- reactive({
  out <- filter_mod() %>%
    dplyr::filter(madi_program %in% input$madi_studies) %>%
    distinct(study_accession, .keep_all = TRUE)
})

study_table_module <- select_group_server(
  id = "study-select",
  data = study_table_value,
  vars = c("study_accession", "sponsoring_organization", "brief_title", "research_focus", "clinical_trial", "tree_id")
  )


################ observeEvent chunks for each table ################

observeEvent(input$studiestab2_rows_selected, {
  selRow = study_table_module()[input$studiestab2_rows_selected,]
  ## Removing old tabs
  removeTab(inputId = "body_panel_id", target = "about_study")
  removeTab(inputId = "body_panel_id", target = "associated_files")
  removeTab(inputId = "body_panel_id", target = "study_protocol_files")
  delete_downstream_tabs('study')

  withProgress(message = paste0("Loading study: ", selRow$study_accession),
               value = 0.5, style = "old", {

    downstream_of_study <- parameter_df %>%
      dplyr::filter(parent_table == "study") %>%
      # Filtering out automatic creation of study protocol table
      filter(child_table != "study_protocol") %>%
      select(child_table)

    # create output tables for all child tables whose parent is "study"
    for (table in downstream_of_study$child_table){
      #print(downstream_of_study$child_table)
      output_name <- paste0(table, "_table")
      output[[output_name]] <- create_table_and_module(table, selRow)
    }

    ################################## About_study table
    # this table has a strange SQL query, so it is the only one that is still hard-coded
    insertTab(inputId = "body_panel_id",
              tabPanel(value = "about_study",
                       title = "About Study",
                       shinydashboard::box(title = "About Study"
                         , width=14, status="primary", solidHeader=T
                         , DT::dataTableOutput("study_glance_out")
                       )
                     ),
              target = "study"
    )
    study_val <- selRow[["study_accession"]]
    study_glance <- dbGetQuery(conn, paste0("SELECT 100 AS report_order, 'Study Accession' AS Study_Attribute, study.study_accession AS Value FROM madi_dat.study WHERE study_accession = '",study_val,"'
       UNION (SELECT 101 AS report_order, 'Study Title' AS Study_Attribute, brief_title FROM madi_dat.study WHERE study_accession = '",study_val,"')
       UNION (SELECT 102 AS report_order, 'Condition Studied' AS Study_Attribute, condition_reported FROM madi_dat.study_2_condition_or_disease WHERE study_accession = '",study_val,"')
       UNION (SELECT 103 AS report_order, 'End Point' AS Study_Attribute, endpoints FROM madi_dat.study WHERE study_accession = '",study_val,"')
       UNION (SELECT (ROW_NUMBER() OVER ( ORDER BY (SELECT NULL) )) + 300 as report_order, CONCAT(gender, ' - ', species) AS Study_Attribute, CONCAT('Subjects: ',count(8)) As Value FROM madi_dat.subject INNER JOIN madi_dat.arm_2_subject AS arm ON arm.subject_accession = subject.subject_accession INNER JOIN madi_dat.arm_or_cohort AS armc ON armc.arm_accession = arm.arm_accession WHERE study_accession = '",study_val,"' GROUP BY CONCAT(gender, ' - ', species) ORDER BY CONCAT(gender, ' - ', species))
       UNION (SELECT (ROW_NUMBER() OVER ( ORDER BY (SELECT NULL) )) + 400 as report_order, CONCAT('Experiment: ', experiment.experiment_accession) AS Study_Attribute, CONCAT(experiment.name, ' - Results: ', upload_result_status) FROM madi_dat.experiment INNER JOIN madi_dat.expsample ON expsample.experiment_accession = experiment.experiment_accession WHERE study_accession = '",study_val,"' GROUP BY experiment.experiment_accession, experiment.name,upload_result_status ORDER BY experiment.experiment_accession, experiment.name,upload_result_status)
       UNION (SELECT (ROW_NUMBER() OVER ( ORDER BY (SELECT NULL) )) + 500 as report_order, CONCAT('Biosample - ',type) AS Study_Attribute, CONCAT(subtype, ': ', count(*)) As Value FROM madi_dat.biosample WHERE study_accession = '",study_val,"' GROUP BY type, subtype ORDER BY type, subtype)
       ORDER by report_order;"))

    output$study_glance_out <- DT::renderDataTable({
      study_glance
      }, selection = 'single',
    options = list(scrollX = TRUE)
    )
    ############################ End of About_Study table

    files_df <- read_files_from_directory()
    files_rf <- reactiveVal()
    files_others <- reactiveVal()
    files_protocol <- reactiveVal()

    rv_set_name$set_name_std = selRow$study_accession

    files_base <- files_df[files_df$file_study == rv_set_name$set_name_std,c("file_type","file_name","file_directory","file_study")]

    # Fetch files from the files in the db
    update_db(operation = "select",table_name = "study_protocol", schema = "madi_dat") %>%
      mutate(file_type = "Protocol") %>%
      rename(file_study = study_accession) %>%
      select(file_type, file_name, file_study, type) %>%
      filter(file_study == rv_set_name$set_name_std) -> db_files

    # Fetch file type from db for files in the local
    DBI::dbGetQuery(conn,
      glue_sql("SELECT t2.file_name, t2.type
                FROM madi_dat.study_2_protocol t1
                LEFT JOIN madi_dat.protocol t2
                ON t1.protocol_accession = t2.protocol_accession
                WHERE study_accession = {rv_set_name$set_name_std};",
                           .con = conn)) -> filetype_db_table


    # Setting reactive for non protocol files
    files_others(
      files_base %>%
        filter(file_type != "Protocol")
    )

    # Setting reactive for only protocol files
    files_protocol(
      files_base %>%
        filter(file_type == "Protocol") %>%
        left_join(filetype_db_table,
                  by = "file_name") %>%
        bind_rows(db_files)
    )

    # Associated Files Tab
    if(nrow(files_others())>=1){

      insertTab(inputId = "body_panel_id",
                tab=tabPanel(value = "associated_files", title = "Associated Files",
                             fluidRow(style = "padding-top:20px",
                                      shinydashboard::box(title = "Associated Files From this study:", width = 12,  status = "primary",
                                                          solidHeader = T, collapsible = F,
                                                          DT::dataTableOutput("associated_files_table"),
                                                          downloadButton("download_associated_files", 'Download')
                                      )

                             )
                ),
                target = "about_study")

      # Associated files
      output$associated_files_table <- DT::renderDataTable({

        # Do not show protocol files
        files_others()

      }, selection = 'single',
      options = list(scrollX = TRUE)
      )

      observeEvent(input$associated_files_table_rows_selected, {
        showNotification(paste("Wait! Checking if file exists."))
        study_files <- files_others()
        selected_row <- input$associated_files_table_rows_selected
        if (is.na(selected_row) || !selected_row > 0) {
          return()
        }
        file_name <- study_files[input$associated_files_table_rows_selected,"file_name"]
        file_name <- study_files[selected_row, "file_name"]
        bucketName <- Sys.getenv("S3_BUCKET_NAME")
        file_exists_s3 <- object_exists(object = file_name, bucket = bucketName)
        if(file_exists_s3){
          output$download_associated_files <- downloadHandler(
            filename <- function(){
              file_name
            },
            content <- function(target_path){
              s3_download_result <- save_object(object = file_name, bucket = bucketName)
            }
          )
          showNotification(paste("Success, file ready for download."))

        }else{
          showNotification(paste("File does not exists."))
        }
      })

    }

    # Study Protocol Tab


    if(nrow(files_protocol())>=1){
      insertTab(inputId = "body_panel_id",
                tab=tabPanel(value = "study_protocol_files", title = "Study Protocol",
                             fluidRow(style = "padding-top:20px",
                                      shinydashboard::box(title = "Associated Protocol Files From this study:", width = 12,  status = "primary",
                                                          solidHeader = T, collapsible = F,
                                                          DT::dataTableOutput("study_protocol_table"),
                                                          downloadButton("download_study_protocol_files", 'Download')
                                      )

                             )
                ),
                target = "arm_or_cohort")

      # Associated files
      output$study_protocol_table <- DT::renderDataTable({

        files_protocol()

      }, selection = 'single',
      options = list(scrollX = TRUE)
      )

      observeEvent(input$study_protocol_table_rows_selected, {
        showNotification(paste("Wait! Checking if file exists."))
        study_files <- files_protocol()
        selected_row <- input$study_protocol_table_rows_selected
        if (is.na(selected_row) || !selected_row > 0) {
          return()
        }
        file_name <- study_files[selected_row, "file_name"]
        bucketName <- Sys.getenv("S3_BUCKET_NAME")
        file_exists_s3 <- object_exists(object = file_name, bucket = bucketName)

        if(file_exists_s3){
        output$download_study_protocol_files <- downloadHandler(
          filename <- function(){
            file_name
          },
          content <- function(target_path){
            s3_download_result <- save_object(object = file_name, bucket = bucketName, file = target_path)
          }
        )
        showNotification(paste("Success, file ready for download."))

        }else{
          showNotification(paste("File does not exists."))
        }
      })

    }

    setProgress(1)})
})


# ---- Downloading excel ----

# This reactive saves Experiment Tab contents

experiment_data <- reactiveVal()

# Reactives to store queries and excel data
all_queries_reactive <- reactiveVal()
all_exp_data_reactive <- reactiveVal()

observe({

  if(is.null(input$experiment_radio)){
    output$experiment_table <- DT::renderDataTable(experiment_data(),selection = 'single', options = list(scrollX = TRUE)
)

    output$experiment_download <- renderUI({      # Empty

    })

  } else if(length(input$experiment_radio) == 1 && input$experiment_radio == "Work with Select Experiments"){

    output$experiment_download <- renderUI({
      fluidRow(actionButton("download_excel_check", "Download experiment(s)")
               , actionButton("launchRadiant", label="Launch Explorer Toolset")
               #, actionButton("transformSet", label="Transform data")
               )
    })

    ## Experiment
    temp <- experiment_data()

    n <- nrow(temp)
    temp$cb <- shinyInput(checkboxInput, n, 'cb_', value = FALSE, width='1px')

    temp <- temp %>% relocate(cb)

    output$experiment_table <- DT::renderDataTable(

      datatable(temp,
                escape = FALSE, selection = 'none',
                height = 100,
                options = list(
                  # dom = 't',
                  # paging = FALSE,
                  # ordering = FALSE,
                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                ))
    )




  } else if(length(input$experiment_radio) == 1 && input$experiment_radio == "Upload Transformed Experiment"){


    output$experiment_table <- DT::renderDataTable(experiment_data(),selection = 'single')

    output$experiment_download <- renderUI({
      fileInput("upload_experiment", "Upload Experiment", multiple = FALSE, accept = ".xlsx")
    })

  } else if(length(input$experiment_radio)==2){

    output$experiment_download <- renderUI({
      fluidRow(actionButton("download_excel_check", "Download Experiment(s)")
               , actionButton("launchRadiant", label="Launch Explorer Toolset")
#               , actionButton("transformSet", label="Transform data")
               , fileInput("upload_experiment", "Upload Experiment", multiple = FALSE, accept = ".xlsx")
               )
    })

    ## Experiment
    temp <- experiment_data()

    n <- nrow(temp)
    temp$cb <- shinyInput(checkboxInput, n, 'cb_', value = FALSE, width='1px')

    temp <- temp %>% relocate(cb)

    output$experiment_table <- DT::renderDataTable(

      datatable(temp,
                escape = FALSE, selection = 'none',
                height = 100,
                options = list(
                  # dom = 't',
                  # paging = FALSE,
                  # ordering = FALSE,
                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                ))
    )



  }

})

observeEvent(
  input$launchRadiant,
  {
    url_root <- paste0(Sys.getenv('madi_toolset_url'))
    print(paste0("madi_toolset_url: ",url_root))
    row_nums_selected <- shinyValue('cb_', nrow(experiment_data())) %>%
      enframe() %>%
      dplyr::filter(value == TRUE) %>%
      pull(name)
    experiment_list <- as.data.frame(experiment_data())
    selected_study_exp <- paste0(sqldf("SELECT DISTINCT study_accession FROM experiment_list[row_nums_selected,]"),collapse = ",")
    selected_experiments <- paste0(experiment_list[row_nums_selected,]$experiment_accession, collapse = ",")
    selected_url_str <- paste0("window.open('",url_root, "?", "study_accession=", selected_study_exp, "&", "experiment_accession_list=", selected_experiments, "', '_blank')" )
    runjs(sprintf(selected_url_str))
  } )



download_experiment <- function(selected_experiment) {
  tryCatch(
    {
      print("inside try")

      selected_query <- paste0("SELECT experiment_accession, result_table, query_str1
                                FROM madi_results.exp_analysis_queries WHERE experiment_accession = '",selected_experiment$experiment_accession,"';")

      # Fetch the associated query for the experiment download
      experiment_query_fetch <- DBI::dbGetQuery(conn,
                                                selected_query) %>% as_tibble()

      # Concatenate the query with the required experiment
      experiment_query <- paste0(experiment_query_fetch$query_str1, "'",experiment_query_fetch$experiment_accession,"'")

      # Download the experiment
      experiment_data <- DBI::dbGetQuery(conn,
                                         experiment_query) %>% as_tibble()


      if(nrow(experiment_data) == 0){
        data_description <- "No data"
      }else{
        data_description <- "Data present"
      }

      return(list(query = selected_experiment,
                  experiment = experiment_data,
                  works = TRUE,
                  description = data_description))

    },
    error = function(e) {
      print("error fetching data")

      return(list(query = selected_experiment,
                  experiment = "Error fetching data",
                  works = FALSE,
                  description = "Error fetching"))
    },
    warning = function(w) {
      message("A warning occurred: ", w$message)

      return(list(query = selected_experiment,
                  experiment = "Error fetching data",
                  works = FALSE,
                  description = "Error fetching"))
    }
  )
}

observeEvent(input$download_excel_check, {

  # Extracting row selected from the study table
  row_nums_selected <- shinyValue('cb_', nrow(experiment_data())) %>%
    enframe() %>%
    dplyr::filter(value == TRUE) %>%
    pull(name)

  if(length(row_nums_selected) == 0){
    showNotification("No experiment selected", duration = 3)
  }

  validate(
    need(length(row_nums_selected)>0, label = "No checkbox selected")
  )

  withProgress(message = "Fetching Experiments", value = 0,{

    all_studies <- experiment_data() %>% dplyr::as_tibble()

    print("row nums selected :", row_nums_selected)


    # List to keep all queries and experiments
    all_queries <- list()
    all_experiments <- list()
    error_experiments <- tibble(study = character(0),
                                experiment = character(0),
                                table = character(0),
                                description = character(0))

    progress_length <- length(row_nums_selected)
    count <- 1
    for(i in row_nums_selected){

      print(i)
      selected_experiment <- all_studies %>% slice(i)

      incProgress(amount = 1/progress_length, detail = paste0(" - ",selected_experiment$experiment_accession))

      print(selected_experiment)

      result <- download_experiment(selected_experiment)

      if(result$works == TRUE & result$description == "Data present"){

        print("result == TRUE")

        all_queries[[count]] <- result$query
        all_experiments[[count]] <- result$experiment

      } else if(result$works == TRUE & result$description == "No data"){

        all_queries[[count]] <- result$query
        all_experiments[[count]] <- "No data"

        error_experiments %>%
          rbind(tibble(study = selected_experiment$study_accession,
                       experiment = selected_experiment$experiment_accession,
                       table = selected_experiment$result_table,
                       description = result$description
          )) -> error_experiments

      } else if(result$works == FALSE){
        print("result == FALSE")

        all_queries[[count]] <- result$query
        all_experiments[[count]] <- "Error fetching data"

        error_experiments %>%
          rbind(tibble(study = selected_experiment$study_accession,
                       experiment = selected_experiment$experiment_accession,
                       table = selected_experiment$result_table,
                       description = result$description
          )) -> error_experiments
      }

      count <- count + 1

    }

  })

  # Storing data in reactive values to pass into downloadHandler
  all_queries_reactive(all_queries)
  all_exp_data_reactive(all_experiments)

  # test_queries <<- all_queries
  # test <<- all_experiments
  # test_error_experiments <<- error_experiments

  if(nrow(error_experiments) == 0){
    print("insideModal")

    shinyalert(html = TRUE, text = tagList(
      h4("All experiments fetched. No problems found"),
      downloadButton("modal_download", "Download")
    ), type = "success",
    showConfirmButton	= FALSE,
    closeOnClickOutside = TRUE,
    )

  } else {

    # Show a modal when the button is pressed
    shinyalert(html = TRUE, text = tagList(
      h5("Following experiments had issues while fetching data. Proceed to download anyway?"),
      renderTable(error_experiments),
      downloadButton("modal_download", "Download*"),
      br(),
      br(),
      h6("* Listed experiments will have a blank sheet")
    ), type = "warning",
    size = "m",
    showConfirmButton	= FALSE,
    closeOnClickOutside = TRUE)

  }

})

output$modal_download <- downloadHandler(
  filename = "experiments.xlsx",
  content = function(file){

    withProgress(message = "Downloading excel", value = 1, {

      # Create new workbook
      wb <- createWorkbook("experiments.xlsx")

      # ---- Creating styles ----
      hs <- createStyle(
        fgFill = "#DCE6F1", halign = "CENTER",
        border = "Bottom"
      )

      instructions_header_style <- createStyle(
        fgFill = "red", halign = "CENTER",
        fontSize = 20,
        fontColour = "white"
      )

      instructions_style <- createStyle(
        fgFill = "red",
        fontSize = 12,
        fontColour = "white",
        wrapText = TRUE,
        halign = "left",
        valign = "center"
      )

      fill_style <- createStyle(
        fgFill = "yellow",
        halign = "center",
        valign = "center",
        wrapText = TRUE,
        border = "TopBottomLeftRight"
      )

      meta_data_header <- createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center",
        textDecoration = "bold"
      )

      meta_data_values <- createStyle(
        halign = "center",
        valign = "center",
        wrapText = TRUE,
        border = "TopBottomLeftRight"
      )

      info_style <- createStyle(
        halign = "center",
        valign = "center",
        wrapText = TRUE,
        border = "TopBottomLeftRight",
        fontColour = "grey"
      )


      # -----

      # test_queries[[1]] %>% colnames()

      tibble("name"="",
             "description"="") %>%
        t() %>%
        as_tibble(rownames = "Details") %>%
        rename(Value = V1) -> new_meta

      instructions <- tibble("READ CAREFULLY" = "This excel is for uploading a new transformed dataset
            \n 1. For the sheet 'New Meta Data' - only write in the highlighted yellow cells.
             \n 2. After tranforming data, please copy paste the new data into the sheet 'New Data' starting from cell A1 (headers will be automatically included)
             \n 3. Uploading Instructions : Only the sheets 'New Meta Data' and 'New Data' need to be present while uploading as the data will be read from them. The rest of the sheets are not required and won't effect in the uploading process")

      addWorksheet(wb, "New Meta Data",
                   tabColour = "red")

      mergeCells(wb, "New Meta Data",
                 cols = 1:2,
                 rows = 1)

      # Write the instructions
      writeData(wb,"New Meta Data",
                x = instructions,
                startCol = 1,
                startRow = 1)

      mergeCells(wb, "New Meta Data",
                 cols = 1:2,
                 rows = 2)

      # Applying styles to the first sheet
      addStyle(wb, "New Meta Data",
               style = instructions_header_style,
               rows = 1,
               cols = 1)

      addStyle(wb, "New Meta Data",
               style = instructions_style,
               rows = 2,
               cols = 1)

      # Adding style to meta data
      addStyle(wb, "New Meta Data",
               style = meta_data_header,
               col = 1:2,
               rows = 5)

      addStyle(wb, "New Meta Data",
               style = meta_data_values,
               col = 1,
               rows = 6:7)

      addStyle(wb, "New Meta Data",
               style = fill_style,
               col = 2,
               rows = 6:7)

      addStyle(wb, "New Meta Data",
               style = info_style,
               col = 3,
               rows = 7)

      # Write the new meta data
      writeData(wb,"New Meta Data",
                x = new_meta,
                startCol = 1,
                startRow = 5)

      writeData(wb,"New Meta Data",
                x = "(Describe the transformations performed in detail)",
                startCol = 3,
                startRow = 7)


      setRowHeights(wb, "New Meta Data",
                    rows = 7,
                    heights = 100)

      setColWidths(wb, "New Meta Data",
                   cols = 1,
                   widths = 50)

      setColWidths(wb, "New Meta Data",
                   cols = 2:3,
                   widths = 15)

      setRowHeights(wb, "New Meta Data",
                    rows = 2,
                    heights = 200)

      # Add a new worksheet for transformed data
      addWorksheet(wb, "New Data",
                   tabColour = "red")

      progress_len <- 1

      all_queries_reactive_temp <<- all_queries_reactive()
      all_exp_data_reactive_temp <<- all_exp_data_reactive()

      print("above the for loop")

      for(i in 1:length(all_queries_reactive())){
        print("begin the for loop")
        setProgress(value = (progress_len/length(all_queries_reactive()))-0.1)

        query <- all_queries_reactive()[[i]]
        excel_data <- all_exp_data_reactive()[[i]]

        # Checking if there is data. If not then convert string to dataframe

        # suppressWarnings({
        #   # If excel_data has data it will show warning
        #   if(excel_data == "No data" | excel_data == "Error fetching data"){
        #     excel_data <- as.data.frame(excel_data)
        #   }
        #
        # })


        query %>%
          t() %>%
          as_tibble(rownames = "Details") %>%
          rename(Value = V1) -> meta_data

        addWorksheet(wb, query$experiment_accession,
                     tabColour = "#4F81BD")

        writeData(wb, sheet = query$experiment_accession,
                  x = meta_data,
                  startCol = 1,
                  startRow = 1,
                  headerStyle = hs)

        writeDataTable(wb, sheet = query$experiment_accession,
                       x = excel_data,
                       startCol = 5,
                       startRow = 1)

        progress_len <- progress_len + 1
        print("end the for loop")
      }
      test_wb <<- wb
      # saveWorkbook(wb, file = "Example.xlsx", overwrite = TRUE)

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

    })
  }
)

#  ----- Upload Excel ----
rv_meta_data <- reactiveVal()
rv_new_data <- reactiveVal()

meta_check <- function(data){

  error_values <- c()

  # Checking name
  data %>%
    dplyr::filter(Details == "name") %>%
    pull(Value) -> given_name

  if(is.na(given_name) | given_name == "" | nchar(given_name)>500){

    error_values <- c(error_values, paste0("Error in name: Value cannot be blank or exceed 500 characters"))

  }

  # Checking description
  data %>%
    dplyr::filter(Details == "description") %>%
    pull(Value) -> given_description

  if(is.na(given_description) | given_description == "" | nchar(given_description)>400){

    error_values <- c(error_values, paste0("Error in description: Value cannot be blank or exceed 4000 characters"))

  }

  return(list(data = data,
              error_values = error_values))
}

observe({

  withProgress(message = "Uploading File", min = 0, max = 10, value = 5,{

    req(input$upload_experiment)

    raw_sheet1 <- tryCatch({
      openxlsx::read.xlsx(input$upload_experiment$datapath, sheet = "New Meta Data", cols = 1:2, rows = 5:7)
    }, error = function(e) {
      return(NULL)
    })

    raw_sheet2 <- tryCatch({
      data_sheet2 <- openxlsx::read.xlsx(input$upload_experiment$datapath, sheet = "New Data")
      #print(paste0("data sheet 2 read : ", head(data_sheet2)))

      if(is.null(data_sheet2)) tibble("No Data" = "No Data") else print(data_sheet2)

    }, error = function(e) {
      return(NULL)
    })


    # Show meta data
    if(!is.null(raw_sheet1) & !is.null(raw_sheet2)){

      meta_check_data <- meta_check(raw_sheet1)
      raw_sheet1 <- meta_check_data$data
      error_values <- meta_check_data$error_values

      # Updating the meta data reactiveVal and new data reactiveVal
      rv_meta_data(raw_sheet1)
      rv_new_data(raw_sheet2)

      showModal(modalDialog(
        title = "Preview",
        id = "table_modal",
        easyClose = TRUE,
        tabsetPanel(
          tabPanel("Meta Data",
                   rHandsontableOutput("meta_contents"),
                   verbatimTextOutput("meta_check")),
          tabPanel("New Data", rHandsontableOutput("data")),
          br()
        ),
        footer = actionButton("confirm_excel_upload", "Confirm", icon = icon("upload"))
      ))

      output$meta_contents <- renderRHandsontable({
        rhandsontable(raw_sheet1) %>%
          hot_col(c("Details", "Value"), readOnly = TRUE)
      })

      output$meta_check <- renderText(error_values, sep = "\n")

      output$data <- renderRHandsontable({
        rhandsontable(head(raw_sheet2, 20)) %>%
          hot_col(colnames(raw_sheet2), readOnly = TRUE)
      })

    }else{
      shinyalert(title = "Error uploading", type = "error")
    }

  })

})

observeEvent(input$confirm_excel_upload, {


  print("------ Action button pressed to upload data to db -----")
  # Fetch required variables

  # Study accession
  study_accession_selected <- rv_study_selected$study_accession[input$studiestab2_rows_selected]
  print(paste0("study accession : ", study_accession_selected))

  # experiment_accession
  next_ext_accession_query_str <- "SELECT 'experiment' AS table, CONCAT('EXT', MAX(CAST(SUBSTRING(experiment_accession, 4) AS INTEGER))+1) AS next_accession FROM madi_dat.experiment;"
  experiment_accession <- DBI::dbGetQuery(conn, next_ext_accession_query_str)$next_accession
  print(paste0("Next experiment accession : ", experiment_accession))

  # result_schema - do not need

  # name from the upload
  uploaded_meta_data <- rv_meta_data()
  experiment_name <- uploaded_meta_data %>%
              dplyr::filter(Details == "name") %>%
              pull(Value)
  print(paste0("Experiment name : ", experiment_name))

  # Experiment Technique
  measurement_technique <- "Analysis Transform"
  print(paste0("Measurement technique : ", measurement_technique))

  # Experiment Accession
  result_table <- experiment_accession
  print(paste0("Result Table : ", result_table))

  brief_title <- rv_study_selected$brief_title[input$studiestab2_rows_selected]
  print(paste0("Brief Title : ", brief_title))

  # experiment_name - do not need
  number_expsamples <- nrow(rv_new_data())
  print(paste0("Number of expsamples : ", number_expsamples))

  # description from the upload
  description <- uploaded_meta_data %>%
    dplyr::filter(Details == "description") %>%
    pull(Value)
  print(paste0("Description : ", description))

  print("adding table to db")

  # Fetch workspace_id
  workspace_id <- userData_upload()$workspace_id
  print(paste0("Workspace id : ", workspace_id))

  # ---- 1. Add table to db ----
  conr <- dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("db"),
                    host=Sys.getenv("db_host"), port=Sys.getenv("db_port"), user=Sys.getenv("db_userid_x"),
                    password=Sys.getenv("db_pwd_x"), options="-c search_path=madi_results")
  print("connection made")
  # make a table name
  experiment_name <- tolower(experiment_name)
  print(paste0("new experiment name", experiment_name))

  table_data <- rv_new_data()

  table_data$study_accession <- study_accession_selected
  table_data$experiment_accession <- experiment_accession
  table_data$measurement_technique <- "Analysis Transform"

  new_table_name <- tolower(paste0(study_accession_selected,"_",experiment_accession))
  temp_new_table_name <<- new_table_name
  temp_table_data <<- table_data

  # write the dataframe to a new database table
  DBI::dbWriteTable(conr, new_table_name, table_data, overwrite = TRUE, row.names = FALSE) #

  # insert experiment reference to the madi_dat.experiment table
  insert_reference_to_madi_dat.experiment(conr, experiment_accession, description, measurement_technique, experiment_name, study_accession_selected, workspace_id)

  # insert experiment reference to the madi_results.exp_analysis_query table
  insert_reference_to_madi_results.exp_analysis_query(conr, study_accession_selected, experiment_accession, measurement_technique,new_table_name ,nrow(table_data), userData_upload()$username, userData_upload()$username)

  # insert experiment reference to the madi_meta.explore_parms table
  insert_reference_to_madi_meta.explore_parms(conr, experiment_accession, new_table_name, study_accession_selected)

  # Refresh materialised views
  refresh_query_str <- "REFRESH MATERIALIZED VIEW madi_dat.studies_shiny"
  DBI::dbSendQuery(conn, refresh_query_str)

  dbDisconnect(conr)

  shinyalert(html = TRUE, text = tagList(
    h4("Experiment uploaded")
  ), type = "success",
  showConfirmButton	= FALSE,
  closeOnClickOutside = TRUE,
  )

  removeModal()
})

observeEvent(input$launchRadiant, {
  url_root <- paste0("https://rstudio-connect-dev.dartmouth.edu/content/77700223-91f6-447c-b463-75cf9ae32103")
  row_nums_selected <- shinyValue('cb_', nrow(experiment_data())) %>%
    enframe() %>%
    dplyr::filter(value == TRUE) %>%
    pull(name)
  experiment_list <- as.data.frame(experiment_data())
  selected_study_exp <- paste0(sqldf("SELECT DISTINCT study_accession FROM experiment_list[row_nums_selected,]"),collapse = ",")
  selected_experiments <- paste0(experiment_list[row_nums_selected,]$experiment_accession, collapse = ",")
  selected_url_str <- paste0("window.open('",url_root, "?", "study_accession=", selected_study_exp, "&", "experiment_accession_list=", selected_experiments, "', '_blank')" )
})

## Subject
observeEvent(input$arm_or_cohort_table_rows_selected, {
  selRow = arm_or_cohort_table_module()[input$arm_or_cohort_table_rows_selected,]
  loop_downstream_tabs('arm_or_cohort', selRow)

})


## Biosample
observeEvent(input$subject_table_rows_selected, {
  selRow = subject_table_module()[input$subject_table_rows_selected,]
  loop_downstream_tabs('subject', selRow)
})



observeEvent(input$experiment_table_rows_selected, {
  selRow <- experiment_table_module()[input$experiment_table_rows_selected,]
  resultColumn = selRow$result_table
  delete_downstream_tabs('experiment')

  withProgress(message = paste0("Loading Experiment: ", selRow$experiment_accession),
               value = 0.5, style = "old", {

    ## Experiment_protocol
    output$experiment_protocol_table <- create_table_and_module("experiment_protocol", selRow)

    ## Experiment samples table
    output$expsample_table <- create_table_and_module("expsample", selRow)

    ## Control samples table
    output$control_table <- create_table_and_module("control_sample", selRow)

    ## Standard curves table
    output$standard_curve_table <- create_table_and_module("standard_curve", selRow)

    # downstream_of_experiment <- parameter_df %>%
    #   filter(parent_table == "experiment" & !grepl("result", child_table)) %>%
    #   select(child_table)
    #
    # # create output tables for all child tables whose parent is "study"
    # for (table in downstream_of_experiment$child_table){
    #   output_name <- paste0(table, "_table")
    #   output[[output_name]] <- create_table_and_module(table, selRow)
    # }

    #### The remaining outputs under the Experiment chunk are RESULT TABLES
    #### These only load if they exist in the selected experiment
    if (resultColumn %in% parameter_df$child_table){
      output_name = paste0(resultColumn, "_table")
      output[[output_name]] <- create_table_and_module(resultColumn, selRow)
    }
    setProgress(1)})
})

observeEvent(input$lab_test_panel_table_rows_selected, {
  selRow = lab_test_panel_table_module()[input$lab_test_panel_table_rows_selected,]
  loop_downstream_tabs('lab_test_panel', selRow)
})


observeEvent(input$assessment_panel_table_rows_selected, {
  selRow = assessment_panel_table_module()[input$assessment_panel_table_rows_selected,]
  loop_downstream_tabs('assessment_panel', selRow)
})


######### OBSERVE EVENTS FOR LOOKUP TABLES

observeEvent(input$study_lookup_select, {
  output$study_lookup_table <- load_lookup_table(input$study_lookup_select)
})

observeEvent(input$arm_or_cohort_lookup_select, {
  output$arm_or_cohort_lookup_table <- load_lookup_table(input$arm_or_cohort_lookup_select)
})

observeEvent(input$subject_lookup_select, {
  output$subject_lookup_table <- load_lookup_table(input$subject_lookup_select)
})

observeEvent(input$biosample_lookup_select, {
  output$biosample_lookup_table <- load_lookup_table(input$biosample_lookup_select)
})

observeEvent(input$experiment_lookup_select, {
  output$experiment_lookup_table <- load_lookup_table(input$experiment_lookup_select)
})

observeEvent(input$expsample_lookup_select, {
  output$expsample_lookup_table <- load_lookup_table(input$expsample_lookup_select)
})

observeEvent(input$reagent_lookup_select, {
  output$reagent_lookup_table <- load_lookup_table(input$reagent_lookup_select)
})


######## WORKING ON METHOD THAT WILL PERMIT NOT NEEDING TO ADD A NEW OBSERVE EVENT FOR EVERY TABLE
# tables <- reactive({
#   tables <- parameter_df %>% select(child_table)
#   print(tables)
#   return(tables)
# })
#
# for (table in tables){
#   input_var <- paste0(table, "_lookup_select")
#   output_var <- paste0(table, "_lookup_table")
#   observeEvent(input[[input_var]], {
#     print(input[[input_var]])
#     output[[output_var]] <- load_lookup_table(input[[input_var]])
#   })
# }
