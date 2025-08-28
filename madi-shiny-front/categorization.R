# categorization

categorization_selected_row <- reactiveVal()

observe( {
  req(input$rb_add == "createStudy") #createStudy
  output$createCategorization <- renderUI({ #createNewStudy
    tagList(
      br(), 
      box(title = "MADI Program Studies:", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          DT::dataTableOutput("categorization_table"), #newstudy_table
          actionButton("add_button_cate", "Add", icon("plus")),
          actionButton("edit_button_cate", "Edit", icon("edit"))
      ),
      br(),
      actionButton("close_button_cate", "Close Tab", icon("xmark"))
    )
  })
  # insertTab(inputId = "body_panel_id",
  #           tabPanel(value = "CategorizationPanel" #createStudy
  #                    , title = "Categorization" #Create New Study
  #                    , uiOutput("createCategorization"))) #createNewStudy
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
# categorization <- reactive({
#   input$submit_cate
#   input$submit_edit_cate
#   cate_query<-paste0("SELECT study_categorization.study_categorization_id, 
#                               study_categorization.research_focus, study_categorization.study_accession
#     FROM madi_dat.study_categorization INNER JOIN madi_dat.study 
#     ON study_categorization.study_accession=study.study_accession 
#     WHERE study.workspace_id IN (6101,6102,6103,6104,6105) AND study_categorization.study_accession IN ('", selected_study_accession(), "');")
#   categorization <- DBI::dbGetQuery(conn, cate_query)
# })  
categorization <- reactive({
  input$submit_add_cate
  input$submit_edit_cate
  categorization <- update_db(operation = "select", table_name = "study_categorization", select_where = list("study_accession" = selected_study_accession()))
})

#List of mandatory fields for submission
fieldsMandatory_cate <- c(
  # study_categorization_id is auto-generated, not mandatory
  "research_focus", "study_accession")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_cate,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_add_cate", condition = mandatoryFilled)
})

output$categorization_table <- DT::renderDataTable({
  table <- categorization()
  names(table) <- c("study_categorization_id", "research_focus", "study_accession"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_cate <- function(button_id_cate){
  showModal(
    modalDialog(
      div(id=("entry_form_cate"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          # tags$head(tags$style("#test .modal-dialog {width: fit-content !important;}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            shinyjs::useShinyjs(),
            fluidRow(
              # textInput("study_categorization_id", labelMandatory("Study Categorization ID"), placeholder = ""),
              
              # disabled(
              #   textInput("study_categorization_id", labelMandatory("Study Categorization ID"), value=DBI::dbGetQuery(conn,"SELECT MAX(CAST(study_categorization_id AS INTEGER))+1 AS next_study_categorization_id FROM madi_dat.study_categorization;"))
              # ),
              
              # textInput("research_focus", labelMandatory("Research Focus"), placeholder = ""),
              selectizeInput("research_focus", labelMandatory("Research Focus"), options = list(
                onInitialize = I('function() { this.setValue(""); }')
              ), selected=NULL, choices = DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_research_focus;")), 
              
              disabled(
                textInput("study_accession", labelMandatory("Study Accession"))
              ),
              
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_cate, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_cate <- reactive({
  formData_cate <- data.frame(
                         # study_categorization_id is auto-generated, don't include it
                         research_focus = input$research_focus,
                         study_accession = input$study_accession,
                         stringsAsFactors = FALSE)
  return(formData_cate)
})

#Add data
appendData_cate <- function(data){
  # Use PostgreSQL-style parameterized query
  sql_insert <- "INSERT INTO madi_dat.study_categorization (research_focus, study_accession) VALUES ($1, $2)"
  
  tryCatch({
    result <- DBI::dbExecute(conn, sql_insert, params = list(data$research_focus, data$study_accession))
    print(paste("Study categorization inserted successfully! Rows affected:", result))
  }, error = function(e) {
    print(paste("SQL categorization insert failed:", e$message))
  })
}

observeEvent(input$add_button_cate, priority = 20,{
  entry_form_cate("submit_add_cate")
  updateTextInput(session, "study_accession", value = paste(selected_study_accession()))
})

observeEvent(input$submit_add_cate, priority = 20,{
  appendData_cate(formData_cate())
  shinyjs::reset("entry_form_cate")
  removeModal()
})

observeEvent(input$categorization_table_rows_selected, {
  categorization_selected_row(categorization()[input$categorization_table_rows_selected,"study_categorization_id"])
})

#edit data
observeEvent(input$edit_button_cate, priority = 20,{
  # SQL_df <- DBI::dbGetQuery(conn, "SELECT study_categorization.study_categorization_id, 
  #                             study_categorization.research_focus, study_categorization.study_accession
  #   FROM madi_dat.study_categorization INNER JOIN madi_dat.study 
  #   ON study_categorization.study_accession=study.study_accession 
  #   WHERE workspace_id IN (6101,6102,6103,6104,6105);")
  
  SQL_df <- update_db(operation = "select", table_name = "study_categorization", select_where = list("study_categorization_id" = categorization_selected_row()))
  print(paste("The selected study categorization id is", categorization_selected_row()))
  
  showModal(
    if(length(input$categorization_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$categorization_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(input$categorization_table_rows_selected) == 1 ){
    entry_form_cate("submit_edit_cate")
    updateTextInput(session, "study_categorization_id", value = SQL_df[, "study_categorization_id"])
    updateTextInput(session, "research_focus", value = SQL_df[, "research_focus"])
    updateTextInput(session, "study_accession", value = SQL_df[, "study_accession"])
  }
})

observeEvent(input$submit_edit_cate, priority = 20, {
  # getQuery_str <- "SELECT study_categorization.study_categorization_id, 
  #   study_categorization.research_focus, study_categorization.study_accession
  #   FROM madi_dat.study_categorization INNER JOIN madi_dat.study 
  #   ON study_categorization.study_accession=study.study_accession 
  #   WHERE workspace_id IN (6101,6102,6103,6104,6105);"
  # SQL_df <- DBI::dbGetQuery(conn, getQuery_str)
  # row_select <- SQL_df[input$categorization_table_row_last_clicked, "study_accession"]
  # row_selection <- dbQuoteLiteral(conn, row_select)
  # study_categorization_id<-paste0(input$study_categorization_id)
  # 
  # update_str <- paste0("UPDATE study SET study_categorization_id = NULLIF('", input$study_categorization_id, "', ''),
  #                      research_focus =  NULLIF('", input$research_focus, "', ''),
  #                      WHERE study_accession =  ", row_selection, ";")
  # 
  # DBI::dbSendQuery(conn, update_str)
  
  updated_data <- tibble(
    study_categorization_id = input$study_categorization_id,
    research_focus = input$research_focus,
    study_accession = input$study_accession
  )
  
  update_db(operation = "update", table_name = "study_categorization", db_data = updated_data, update_where = c("study_categorization_id" = categorization_selected_row()))
  
  removeModal()
})

# close tab
observeEvent(input$close_button_cate, priority = 20, {
  removeTab(inputId = "body_panel_id", target = "CategorizationPanel")
  
  selectedRow <- reactiveVal(input$report_list_table_row_last_clicked)
  proxy = dataTableProxy("report_list_table")
  selectedRowIndex <- selectedRow()
  # Check if a row is selected
  if (!is.null(selectedRowIndex)) {
    # Deselect the row
    proxy %>% selectRows(NULL)
    selectedRow(NULL)
  }
})

# Close tab
observeEvent(input$close_button_cate, {
  
  deselect<-which(components()[, "table_name"] == "categorization")
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
  
  removeTab(inputId = "body_panel_id", target = "CategorizationPanel")
})
