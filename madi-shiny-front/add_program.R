# Add new program tab

observe({
  req(input$rb_add == "add_program")
  output$add_program_ui <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          reactable::reactableOutput("program_table", width = "100%"),
          actionButton("program_add_button", "Add", icon("plus")),
          actionButton("program_edit_button", "Edit", icon("edit"))
      )
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "add_program_tab" #createStudy
                     , title = "Add Program" #Create New Study
                     , uiOutput("add_program_ui"))) #createNewStudy
})

program <- reactive({
  # input$submit_program
  # input$submit_program_edit
  # program <- DBI::dbGetQuery(conn,
  #                            "SELECT program_id, category, description, end_date, link, name, short_name, start_date FROM madi_dat.program;")
  
  program <- update_db(operation = "select", table_name = "program")
})

program_form_selected_row <- reactive(getReactableState("program_table", "selected"))

output$program_table <- reactable::renderReactable({
  table_data <- program()
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

#List of mandatory fields for submission
# Removing "link" and "short_name" from list of mandatory columns
fieldsMandatory <- c("category", "name")

#define which input fields are mandatory
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_program", condition = mandatoryFilled)
})
#
# #Form for data entry
entry_form_program <- function(button_id){
  showModal(
    modalDialog(
      div(id=("entry_form_program"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              cellWidths = c("250px", "100px"),
              cellArgs = list(style = "vertical-align: top"),
              # textOutput("program_id"), # Use for updating, extract the next program_id from sql
              selectInput("program_category", labelMandatory("Category"),
                          choices = DBI::dbGetQuery(conn, "SELECT DISTINCT(name), description FROM madi_dat.lk_visibility_category;")$name,
                          selected = "NIH"),
              # textInput("category", labelMandatory("Category"), placeholder = ""),
              textInput("program_description", "Description", placeholder = ""),
              dateInput("program_end_date", "End date"),
              textInput("program_link", "Link"),
              textInput("program_name", labelMandatory("Name")),
              textInput("program_short_name", "Short name"),
              dateInput("program_start_date", "Start date"),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}
#
#save form data into data_frame format
formData <- reactive({
  formData <- data.frame(category = input$program_category,
                         description = input$program_description,
                         end_date = ifelse(length(input$program_end_date)==0, NA,as.character(format(input$program_end_date, format="%Y-%m-%d"))),
                         link = input$program_link,
                         name = input$program_name,
                         short_name = input$program_short_name,
                         start_date = ifelse(length(input$program_start_date)==0, NA,as.character(format(input$program_start_date, format="%Y-%m-%d"))),
                         stringsAsFactors = FALSE)
  return(formData)
})
#
#Add data
append_program_data <- function(data){

  next_program_id_db <- DBI::dbGetQuery(conn, "SELECT MAX(program_id)+1 as next_program_id FROM madi_dat.program;")

 # If NULL is set to a column name it removes the column. So "" or NA is the placeholder
 #  query <- paste0("INSERT INTO madi_dat.program (
 #  	program_id, category, description, end_date, link, name, short_name, start_date)
 # 	  VALUES (
 #         ",data$program_id,",
 #         '",data$category,"',
 #         '",data$description,"',
 #         '",data$end_date,"',
 #         '",data$link,"',
 #         '", data$name,"',
 #         '", data$short_name,"',
 #         '", data$start_date,"');")
 #
 # #  dbSendQuery(conn, query)
  
  data <- tibble(
    category = input$program_category,
    description = input$program_description,
    end_date= input$program_end_date,
    link= input$program_link,
    name=input$program_name,
    short_name= input$program_short_name,
    start_date= input$program_start_date,
  )
  
  data$program_id <- next_program_id_db$next_program_id
  
  print(data)

  update_db(operation = "insert", db_data = data, table_name = "program")

}
#
observeEvent(input$program_add_button, priority = 20,{
  entry_form_program("submit_program")
})

observeEvent(input$submit_program, priority = 20,{
  append_program_data(formData())
  shinyjs::reset("entry_form_program")
  removeModal()
})
#
# #edit data
observeEvent(input$program_edit_button, priority = 20,{
  SQL_df <- DBI::dbGetQuery(conn, "SELECT program_id, category, description, end_date, link, name, short_name, start_date FROM madi_dat.program;")
  showModal(
    if(length(program_form_selected_row()) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(program_form_selected_row()) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  if(length(program_form_selected_row()) == 1 ){
    entry_form_program("submit_program_edit")
    # updateTextInput(session, "program_id", value = SQL_df[program_form_selected_row(), "program_id"])
    updateTextInput(session, "program_category", value = SQL_df[program_form_selected_row(), "category"])
    updateTextInput(session, "program_description", value = SQL_df[program_form_selected_row(), "description"])
    updateDateInput(session, "program_end_date", value = SQL_df[program_form_selected_row(), "end_date"])
    updateTextInput(session, "program_link", value = SQL_df[program_form_selected_row(), "link"])
    updateTextInput(session, "program_name", value = SQL_df[program_form_selected_row(), "name"])
    updateTextInput(session, "program_short_name", value = SQL_df[program_form_selected_row(), "short_name"])
    updateDateInput(session, "program_start_date", value = SQL_df[program_form_selected_row(), "start_date"])
  }
})
#
observeEvent(input$submit_program_edit, priority = 20, {
  # SQL_df <- DBI::dbGetQuery(conn, "SELECT program_id, category, description, end_date, link, name, short_name, start_date FROM madi_dat.program;")
  SQL_df <- update_db(operation = "select", table_name = "program")

  program_id <- SQL_df[program_form_selected_row(), "program_id"]
  print(input$program_start_date)
  # program_id <- dbQuoteLiteral(conn, program_id)
  # query <- paste0("UPDATE madi_dat.program
  #         SET category= '",input$program_category,"',
  #         description= '",input$program_description,"',
  #         end_date= '",input$program_end_date,"',
  #         link= '",input$program_link,"',
  #         name='",input$program_name,"',
  #         short_name= '",input$program_short_name,"'        ,
  #         start_date= '",input$program_start_date,"'
  #         WHERE program_id = ",program_id,";"
  # )
  #
  # DBI::dbSendQuery(conn, query)

  updated_data <- tibble(
    program_id = program_id,
    category = input$program_category,
    description = input$program_description,
    end_date= input$program_end_date,
    link= input$program_link,
    name=input$program_name,
    short_name= input$program_short_name,
    start_date= input$program_start_date,
  )

  update_db(operation = "update", table_name = "program", db_data = updated_data, update_where = c("program_id" = program_id))

  removeModal()
})