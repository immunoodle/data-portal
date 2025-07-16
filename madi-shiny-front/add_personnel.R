# Add new personnel tab

observe({
  req(input$rb_add == "add_personnel")
  output$add_personnel_ui <- renderUI({
    tagList(
      br(), 
      box(title = "MADI", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          reactable::reactableOutput("personnel_table", width = "100%"),
          actionButton("personnel_add_button", "Add", icon("plus")),
          actionButton("personnel_edit_button", "Edit", icon("edit"))
      )
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "add_personnel_tab" 
                     , title = "Add personnel" 
                     , uiOutput("add_personnel_ui")))
  showTab(inputId = "body_panel_id", target = "add_personnel_tab", select = TRUE, session = getDefaultReactiveDomain())
})

personnel <- reactive({
                    print(update_db(operation = "select", table_name = "personnel"))
                    personnel <- update_db(operation = "select", table_name = "personnel")
                    
                  })

personnel_form_selected_row <- reactive(getReactableState("personnel_table", "selected"))

output$personnel_table <- reactable::renderReactable({
  table_data <- personnel()
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
fieldsMandatory_personnel <- c("personnel_first_name", "personnel_last_name","personnel_organisation")

#define which input fields are mandatory
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory_personnel,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_personnel", condition = mandatoryFilled)
})

entry_form_personnel <- function(button_id){
  showModal(
    modalDialog(
      div(id=("entry_form_personnel"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              cellWidths = c("250px", "100px"),
              cellArgs = list(style = "vertical-align: top"),
              numericInput("personnel_id", "Personnel_id", value = DBI::dbGetQuery(conn, "SELECT MAX(personnel_id)+1 as next_personnel_id FROM madi_dat.personnel;")),
              textInput("personnel_email", "Email"),
              textInput("personnel_first_name", "First Name"),
              textInput("personnel_last_name", "Last Name"),
              textInput("personnel_organisation", "Organisation"),
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
# formData <- reactive({
#   print("insideReactive")
#   formData <- data.frame(personnel_id = input$personnel_id,
#                          email = input$personnel_email,
#                          first_name = input$personnel_first_name,
#                          last_name = input$personnel_last_name,
#                          organization = input$personnel_organisation)
#   test <<- formData
#   print(formData)
#   return(formData)
# })

#Add data
append_personnel_data <- function(data){
  
  # data$personnel_id <- input$personnel_id
  print(data)
  update_db(operation = "insert", db_data = data, table_name = "personnel")
  
}

observeEvent(input$personnel_add_button, priority = 20,{
  entry_form_personnel("submit_personnel")
})

observeEvent(input$submit_personnel, priority = 20,{
  
  personnel_data <- tibble(personnel_id = input$personnel_id,
                 email = input$personnel_email,
                 first_name = input$personnel_first_name,
                 last_name = input$personnel_last_name,
                 organization = input$personnel_organisation)
  
  append_personnel_data(personnel_data)
  # append_personnel_data(formData())
  shinyjs::reset("entry_form_personnel")
  removeModal()
})

observeEvent(input$personnel_edit_button, priority = 20,{
  SQL_df <- update_db(operation = "select", table_name = "personnel")
  print(personnel_form_selected_row())
  SQL_df <- SQL_df[personnel_form_selected_row(),]
  if(length(personnel_form_selected_row()) == 1 ){
    entry_form_personnel("submit_personnel_edit")
    # updateTextInput(session, "program_id", value = SQL_df[program_form_selected_row(), "program_id"])
    updateNumericInput(session, "personnel_id", value = SQL_df[["personnel_id"]])
    updateTextInput(session, "personnel_email", value = SQL_df[["email"]])
    updateTextInput(session, "personnel_first_name", value = SQL_df[["first_name"]])
    updateTextInput(session, "personnel_last_name", value = SQL_df[["last_name"]])
    updateTextInput(session, "personnel_organisation", value = SQL_df[["organization"]])
  }
})

observeEvent(input$submit_personnel_edit, priority = 20, {
  # SQL_df <- update_db(operation = "select", table_name = "personnel")
  # personnel_id <- SQL_df[program_form_selected_row(), "personnel_id"]
  
  updated_data <- tibble(
    personnel_id = input$personnel_id,
    email = input$personnel_email,
    first_name = input$personnel_first_name,
    last_name= input$personnel_last_name,
    organization= input$personnel_organisation
  )
  # print(updated_data)
  update_db(operation = "update", table_name = "personnel", db_data = updated_data, update_where = c("personnel_id" = input$personnel_id))
  
  removeModal()
})