library(aws.s3)
observe({
  req(input$rb_add == "addAssociated")
  #####################################
  ### Show Add Associated Files Tab ###
  
  output$addAssociated <- renderUI({ 
    tagList(
      selectInput("addAssociated_selectStudy",
                  "Select Existing Study Accession",
                  choices = c("Click here", unique(all_study_list$study_accession)), selected = NULL, multiple = FALSE)
      , conditionalPanel(
        condition = "input.addAssociated_selectStudy != 'Click here'"
        ,fileInput("upload_associated",
                   label="Pre-load files",
                   multiple=FALSE)
        ,selectInput("file_type", "Select file type from the list", choices = c("Click here","Protocol", update_db(operation = "select", table_name = "lk_study_file_type")$name)) 
        ,conditionalPanel(condition = "input.file_type == 'Protocol'",
                          textInput("protocol_name", "Protocol Name"),
                          selectInput("protocol_type", "Select protocol type from the list", choices = dbGetQuery(conn, "SELECT name, description FROM madi_dat.lk_protocol_type WHERE name <> 'Type';")$name )
        )
        ,conditionalPanel(condition = "input.file_type != 'Click here'"
                          ,textInput("upload_description",label="File description",
                                     placeholder="Enter description"), 
                          conditionalPanel(
                            condition = "input.file_type != 'Click here' & input.file_type != 'Protocol'",
                            selectInput("file_detail", "File detail", choices = unique(update_db(operation = "select", table_name = "lk_file_detail")$name))
                          )
                          ,actionButton("upload_to_associated_server",label="Upload to MADI Repository")
                          ,tags$hr()
                          ,textOutput("validation")              
        )
      )
    )
  })
  print("addAssociated render UI")
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "addAssociated", title = "Add Associated Files", uiOutput("addAssociated")
            )
  )
})

output$associated_file_type <- DT::renderDataTable({
  # associated_file_type <- dbGetQuery(conn, "SELECT name, description FROM madi_dat.lk_study_file_type;")
  # Using the update_db function for uniformity
  update_db(operation = "select", table_name = "lk_study_file_type") %>% 
    select(name, description) -> associated_file_type
})

output$associated_protocol_type <- DT::renderDataTable({
  
  # Keeping this SQL query as it is. update_db function does not support <> functionality yet
  associated_protocol_type <- dbGetQuery(conn, "SELECT name, description FROM madi_dat.lk_protocol_type WHERE name <> 'Type';")
  
})

observeEvent(input$upload_to_associated_server, {
  
  req(input$upload_associated)
  file <- input$upload_associated
  
  bucket_name <- Sys.getenv("S3_BUCKET_NAME")
  
  # Generate a unique name for the file to be stored in S3
  targetassoc_name <- paste0(Sys.getenv("USER"), "_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", file$name)
  
  # Upload the file to S3
  s3_upload_result <- put_object(file$datapath, object = targetassoc_name, bucket = bucket_name)
  
  # Check the result of the S3 upload and notify the user accordingly
  if (s3_upload_result == TRUE) {
    showNotification(paste("Upload successful. URL: "))
  } else {
    showNotification("Upload failed.", type = "error")
  }
  
  associated_study <- input$addAssociated_selectStudy
  associated_filename <- targetassoc_name
  associated_filesize <- file$size
  file_type <- input$file_type
  protocol_type <- input$protocol_type
  
  # Assuming userData_upload() retrieves current user/session data
  username <- userData_upload()$username
  
  # Generate a more specific target name if needed, or use the same as for S3
  #targetassoc_name <- paste0(username, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_", associated_filename)
  
  if (input$file_type == "Protocol") {
    # Get the next protocol accession and update the database
    # Get the next protocol accession
          nextProtocol <- dbGetQuery(conn, "SELECT 'protocol' AS table, CONCAT('PTL',MAX(CAST(SUBSTRING(protocol_accession,4) AS INTEGER))+1) AS next_accession FROM madi_dat.protocol;")

          # Update protocol table
          protocol_table <- tibble(protocol_accession = nextProtocol$next_accession,
                                   name = input$protocol_name,
                                   description = input$upload_description,
                                   file_name = targetassoc_name,
                                   original_file_name = associated_filename,
                                   type = protocol_type,
                                   workspace_id = userData_upload()$workspace_id)

          update_db(operation = "insert", table_name = "protocol", db_data = protocol_table)

          # # Update study_2_protocol table
          study_2_protocol_table <- tibble(protocol_accession = nextProtocol$next_accession,
                                           study_accession = associated_study)

          update_db(operation = "insert", table_name = "study_2_protocol", db_data = study_2_protocol_table)
  } else {
    # Update file_info table and study_file table
          file_info_table <- tibble(
            file_info_id = as.numeric(DBI::dbGetQuery(conn, "SELECT MAX(file_info_id) + 1 as next_file_info_id FROM madi_dat.file_info;")),
            detail = input$file_detail,
            filesize_bytes = associated_filesize,
            name = targetassoc_name,
            original_file_name = associated_filename,
            workspace_id = userData_upload()$workspace_id)

          update_db(operation = "insert", table_name = "file_info", db_data = file_info_table)

          # Update study_file table
          study_file_table <- tibble(study_file_accession = as.character(DBI::dbGetQuery(conn, "SELECT CONCAT('SFL',MAX(SUBSTRING(study_file_accession, 4)::INTEGER) + 1) as next_study_file_accession FROM madi_dat.study_file;")),
                                     description = input$upload_description,
                                     file_name = targetassoc_name,
                                     study_accession = associated_study,
                                     study_file_type = file_type,
                                     workspace_id = userData_upload()$workspace_id)

          update_db(operation = "insert", table_name = "study_file", db_data = study_file_table)
  }
  
  showNotification("Upload and database update completed successfully.")
  
})


rv_set_name <- reactiveValues()

read_files_from_directory <- function() {
  directory_path <- Sys.getenv("upload_file_path")
  all_files <- as.data.frame(list.files(directory_path, full.names = TRUE, recursive = TRUE))
  colnames(all_files)[1] = "full_path"
  file_name <- basename(all_files$full_path)
  file_directory <- dirname(all_files$full_path)
  fil_colnames <- c("file_study","file_type")
  file_sub_parse <- data.frame(str_split_fixed(str_remove_all(file_directory,directory_path), "/", 3))[-c(1)]
  colnames(file_sub_parse) = fil_colnames
  file_df <- cbind(file_name, file_directory, file_sub_parse)
  return(file_df)
}

files_df <- read_files_from_directory()
files_rf <- reactiveVal()

