### reagent tab

observe( {
  req(input$rb_add == "addReagent") #createStudy
  output$createReagent <- renderUI({ #createNewStudy
    tagList(
      br(), 
      shinydashboard::box(title = "MADI Program Studies:", width = 12,  status = "primary", 
          solidHeader = T, collapsible = F,
          selectInput("reagent_study",
                      "Select Existing Study Accession",
                      choices = c("Click here", unique(all_study_list$study_accession)), selected = NULL, multiple = FALSE)
          , conditionalPanel(
            condition = "input.reagent_study != 'Click here'",
          DT::dataTableOutput("reagent_table"), #newstudy_table
          actionButton("add_button_ie", "Add", icon("plus")),
          actionButton("edit_button_ie", "Edit", icon("edit"))
          )
      )
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "reagentPanel" #createStudy
                     , title = "Reagent" #Create New Study
                     , uiOutput("createReagent"))) #createNewStudy
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
reagent <- reactive({ #newstudy
  input$submit_ac
  input$submit_edit_ac
  reagent <- DBI::dbGetQuery(conn, paste0("SELECT DISTINCT reagent.reagent_accession, reagent.name, reagent.analyte_accession, 
  reagent.analyte_preferred, reagent.analyte_reported, antibody_registry_id, catalog_number, clone_name, contact, 
  reagent.description, is_set, lot_number, manufacturer, reporter_name, type, weblink, reagent.workspace_id, study_accession
  FROM madi_dat.reagent INNER JOIN madi_dat.expsample_2_reagent 
  ON expsample_2_reagent.reagent_accession = reagent.reagent_accession 
  INNER JOIN madi_dat.expsample ON expsample.expsample_accession = expsample_2_reagent.expsample_accession 
  INNER JOIN madi_dat.experiment ON experiment.experiment_accession = expsample.experiment_accession 
                                          WHERE study_accession = '", input$reagent_study, "';"))

}) 

#List of mandatory fields for submission
fieldsMandatory <- c("reagent_accession", "name")

#define which input fields are mandatory 
observe({
  mandatoryFilled <-
    vapply(fieldsMandatory,
           function(x) { !is.null(input[[x]]) && input[[x]] != "" },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  shinyjs::toggleState(id = "submit_ie", condition = mandatoryFilled)
})

output$reagent_table <- DT::renderDataTable({ #newstudy_table
  table <- reagent()[,c(1:5)] #newstudy
  names(table) <- c("reagent_accession", "name", "analyte_accession", "analyte_preferred", 
                    "analyte_reported"
  )
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})

# Form for data entry
entry_form_ie <- function(button_id_ie){
  showModal(
    modalDialog(
      div(id=("entry_form_ie"),
          tags$head(tags$style(".modal-dialog{ width:800px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              column(
                width = 6,
                textInput("reagent_accession", labelMandatory("Reagent Accession"), placeholder = ""),
                textInput("analyte_accession", "Analyte Accession", placeholder = ""), # From drop down list!
                textInput("analyte_preferred", "Analyte Preferred", placeholder = ""),
                textInput("analyte_reported", "Analyte Reported", placeholder = ""),
                textInput("antibody_registry_id", "Antibody Registry ID", placeholder = ""),
                textInput("catalog_number", "Catalog Number", placeholder = ""),
                textInput("clone_name", "Clone Name", placeholder = ""),
                textInput("contact", "Contact", placeholder = ""),
                textInput("weblink", "Web Link", placeholder = ""),
                
              ),
              column(
                width = 6,
                textInput("description", "Description", placeholder = ""),
                selectInput("is_set", "Is set?", choices = c("N","Y")),
                textInput("lot_number", "Lot Number", placeholder = ""),
                textInput("manufacturer", "Manufacturer", placeholder = ""),
                textInput("reporter_name", "Reporter Name", placeholder = ""),
                textInput("name", labelMandatory("Name"), placeholder = ""),
                textInput("type", "Type", placeholder = ""),
                selectInput("workspace_id", "Workspace", choices = dbGetQuery(conn, "SELECT name FROM madi_dat.workspace WHERE workspace_id IN (6101, 6102, 6103, 6104, 6105);")),
                #textInput("workspace_id", "Workspace ID", placeholder = ""), # drop down
              ),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton(button_id_ie, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#save form data into data_frame format
formData_ie <- reactive({
  formData_ie <- data.frame(row_id = UUIDgenerate(),
                            reagent_accession = input$reagent_accession,
                            analyte_accession = input$analyte_accession,
                            analyte_preferred = input$analyte_preferred,
                            analyte_reported = input$analyte_reported,
                            antibody_registry_id = input$antibody_registry_id,
                            catalog_number = input$catalog_number,
                            clone_name = input$clone_name,
                            description = input$description,
                            is_set = input$is_set,
                            lot_number = input$lot_number,
                            manufacturer = input$manufacturer,
                            name = input$name,
                            reporter_name = input$reporter_name,
                            type = input$type,
                            weblink = input$weblink,
                            workspace_id = input$workspace_id,
                            stringsAsFactors = FALSE)
  return(formData_ie)
})

#Add data
appendData_ie <- function(data){
  dbSendQuery(conn,paste0("INSERT INTO reagent (reagent_accession, analyte_accession, analyte_preferred,
                               analyte_reported, antibody_registry_id, catalog_number,clone_name,description,
                               is_set, lot_number, manufacturer,name, reporter_name, type,
                               weblink, workspace_id)
	VALUES ('", input$exposure_accession,
                          "', '", input$reagent_accession,
                          "', '", input$analyte_accession,
                          "', '", input$analyte_preferred,
                          "', '", input$analyte_reported,
                          "', '", input$antibody_registry_id,
                          "', '", input$catalog_number,
                          "', '", input$clone_name,
                          "', '", input$description,
                          "', '", input$is_set,
                          "', '", input$lot_number,
                          "', '", input$manufacturer,
                          "', '", input$name,
                          "', '", input$type,
                          "', '", input$weblink,
                          "', '", input$workspace_id,                       
                          "');"))
}

observeEvent(input$add_button_ie, priority = 20,{
  entry_form_ie("submit_ie")
})

observeEvent(input$submit_ie, priority = 20,{
  appendData_ie(formData_ie())
  shinyjs::reset("entry_form_ie")
  removeModal()
})

#edit data
observeEvent(input$edit_button_ie, priority = 20,{
  showModal(
    if(length(input$reagent_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$reagent_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })
  SQL_df <- reagent()[input$reagent_table_rows_selected,]
  
  if(length(input$reagent_table_rows_selected) == 1 ){
    entry_form_ie("submit_edit_ie")
    updateTextInput(session, "reagent_accession", value = SQL_df$reagent_accession)
    updateTextInput(session, "analyte_accession", value = SQL_df$analyte_accession)
    updateTextInput(session, "analyte_preferred", value = SQL_df$analyte_preferred)
    updateTextInput(session, "analyte_reported", value = SQL_df$analyte_reported)
    updateTextInput(session, "antibody_registry_id", value = SQL_df$antibody_registry_id)
    updateTextInput(session, "catalog_number", value = SQL_df$catalog_number)
    updateTextInput(session, "clone_name", value = SQL_df$clone_name)
    updateTextInput(session, "contact", value = SQL_df$contact)
    updateTextInput(session, "description", value = SQL_df$description)
    updateTextInput(session, "is_set", value = SQL_df$is_set)
    updateTextInput(session, "lot_number", value = SQL_df$lot_number)
    updateTextInput(session, "manufacturer", value = SQL_df$manufacturer)
    updateTextInput(session, "name", value = SQL_df$name)
    updateTextInput(session, "reporter_name", value = SQL_df$reporter_name)
    updateTextInput(session, "type", value = SQL_df$type)
    updateTextInput(session, "weblink", value = SQL_df$weblink)
    updateTextInput(session, "workspace_id", value = SQL_df$workspace_id)
  }
})

observeEvent(input$submit_edit_ie, priority = 20, {
  #   getQuery_str_ie <- "SELECT reagent_accession, analyte_accession, analyte_preferred,
  #   analyte_reported, antibody_registry_id, catalog_number, clone_name, contact, description, is_set,
  #   lot_number, manufacturer, name, reporter_name, type, weblink, workspace_id
  # 	FROM madi_dat.reagent;"
  #   SQL_df_ie <- DBI::dbGetQuery(conn, getQuery_str_ie)
  #   row_select_ie <- SQL_df_ie[input$reagent_table_rows_selected, "reagent_accession"]
  SQL_df <- reagent()
  row_selection_ie <- SQL_df[input$reagent_table_rows_selected, "reagent_accession"]
  #reagent()[input$reagent_table_rows_selected]$reagent_accession
  
  update_str_ie <- paste0("UPDATE reagent SET 
                       analyte_accession =  NULLIF('", input$analyte_accession, "', ''),
                       analyte_preferred =  NULLIF('", input$disease_ontology_id, "', ''),
                       analyte_reported =  NULLIF('", input$disease_preferred, "', ''),
                       antibody_registry_id =  NULLIF('", input$disease_reported, "', ''),
                       catalog_number =  NULLIF('", input$disease_stage_preferred, "', ''),
                       clone_name =  NULLIF('", input$disease_stage_reported, "', ''),
                       contact =  NULLIF('", input$exposure_material_id, "', ''),
                       description =  NULLIF('", input$exposure_material_preferred, "', ''),
                       is_set =  NULLIF('", input$exposure_material_reported, "', ''),
                       lot_number =  NULLIF('", input$exposure_process_preferred, "', ''),
                       manufacturer = NULLIF('", input$exposure_process_reported, "', ''),
                       name = NULLIF('", input$subject_accession, "', ''),
                       reporter_name =  NULLIF('", input$reporter_name, "', ''),
                       type =  NULLIF('", input$type, "', ''),
                       weblink = NULLIF('", input$weblink, "', ''),
                       workspace_id = NULLIF('", input$workspace_id, "', ''),
                       WHERE reagent_accession =  ", row_selection_ie, ";")
  
  DBI::dbSendQuery(conn, update_str_ie)
  removeModal()
})


