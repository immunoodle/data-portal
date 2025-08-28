filter_row_from_table <- function(df, selected_table){
  df <- df %>% 
    dplyr::filter(child_table == selected_table)
  df
}

createLink <- function(val) {
  #sprintf(paste0('<a href="', URLdecode(val),'" target="_blank">', substr(val, 1, 25) ,'</a>'))
  sprintf(paste0('<a href="', val,'" target="_blank">', substr(val, 1, 254) ,'</a>'))
}

prismCodeBlock <- function(code) {
  tagList(
    HTML(code),
    tags$script("Prism.highlightAll()")
  )
}

prismDependencies <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
  tags$link(rel = "stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)

prismSqlDependency <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-sql.min.js")
)

find_downstream_tabs <- function(current_table)
  ### Finds tabs that are directly downstream of the current tab
  return((parameter_df %>%
            dplyr::filter(parent_table == current_table) %>%
            select(child_table)
  )$child_table
  )

delete_downstream_tabs <- function(current_table){
  ### Recursively finds all tabs downstream of current tab
  ### Removes old downstream tabs
  downstream_tabs <- find_downstream_tabs(current_table)
  for (tab_id in downstream_tabs){
    delete_downstream_tabs(tab_id)
    removeTab(inputId = "body_panel_id", target = tab_id)
  }
}

insert_new_tab <- function(params){
  if (length(load_lookup_choices(params$child_table)) > 0){
    #print(load_lookup_choices(params$child_table))
    #print(paste0("in new tab if: ", params$child_table, params$tab_label))
    tab <- tabPanel(value = params$child_table
                    , title = params$tab_label
                    , UI_lookup_table(params)
                    , UI_data_table(params)
    )
  } else if (grepl("ext", params$child_table)){
    #print(paste0("in new tab elseif: ", params$child_table, params$tab_label))
    tab <- tabPanel(value = params$child_table
                    , title = params$tab_label
                    , UI_lookup_table(params)
                    , UI_data_table(params)
    )
    
  } else {
    #print(paste0("in new tab else: ", params$child_table, params$tab_label))
    tab <- tabPanel(value = params$child_table
                    , title = params$tab_label
                    , UI_data_table(params)
    )
    #print(tab)
  }
  
  insertTab(inputId = "body_panel_id"
            , tab
            , target = params$parent_table)
}

UI_lookup_table <- function(params){
  # if no lookup tabs, return nothing
  # also update insert_tab, to have no if statements anymore
  
  if (is.data.frame(params)) {
    tab_label = params$tab_label
    tab_ID = params$child_table
  } else if (params == "study") {
    tab_label = "All Studies"
    tab_ID = params
  }
  
  if (grepl("ext", tab_ID)){
    study_accession <- tolower(experiment_table_module()[1,"study_accession"])
    view_definition <- toString(DBI::dbGetQuery(conn, paste0("SELECT view_definition FROM madi_meta.vw_definition_matview WHERE table_schema='madi_results' AND table_name = 'mv_",study_accession,"_",tab_ID,"';")))
    view_definition_text <- paste("<pre><code class='language-sql'>", view_definition, "</code></pre>")

    return(shinydashboard::box(title = paste0("SQL Query")
        , width=14, status="primary", solidHeader=T, collapsible=T, collapsed=T
        , prismDependencies
        , prismSqlDependency
        , renderUI({
          prismCodeBlock(view_definition_text)
        })
                    )
           )
  } 
  
  return(shinydashboard::box(title = span( icon("table-list"), paste0(tab_label, ": Lookup Tables"))
      , width=14, status="primary", solidHeader=T, collapsible=T, collapsed=T
      , radioGroupButtons(
        inputId = paste0(tab_ID, "_lookup_select"),
        choices = load_lookup_choices(tab_ID),
        selected = character(0),
        status = "primary"
      )
      , DT::dataTableOutput(paste0(tab_ID, "_lookup_table"))
    )

  )
}

UI_data_table <- function(params){
  output_table_name <- paste0(params$child_table, "_table")
  #print(paste0(params$child_table, "_table"))
  
  if (params$tab_label == "Experiment"){
    return(shinydashboard::box(title = paste0(params$tab_label, ": Data Table")
        , width=14, status="primary", solidHeader=T
        , DT::dataTableOutput(output_table_name)
        , checkboxGroupInput("experiment_radio", NULL, choices = c("Work with Select Experiments", "Upload Transformed Experiment"), selected = NULL)
        , uiOutput("experiment_download")
      )
    )
  }
  
  return(shinydashboard::box(title = paste0(params$tab_label, ": Data Table")
      , width=14, status="primary", solidHeader=T
      , DT::dataTableOutput(output_table_name)
    )
  )
}

load_lookup_table <- function(choice){
  if (choice == "Reagents"){
    experiment_accession = expsample_table_module()[1,"experiment_accession"]
    lookup_table <- DT::renderDataTable({
      query <- dbGetQuery(conn, paste0("SELECT DISTINCT c.reagent_accession, c.analyte_accession, c.analyte_preferred, c.analyte_reported, c.antibody_registry_id, c.catalog_number, c.clone_name, c.contact, c.description, c.is_set, c.lot_number, c.manufacturer, c.name, c.reporter_name, c.type, c.weblink, c.workspace_id 
FROM madi_dat.expsample AS e 
INNER JOIN madi_dat.expsample_2_reagent AS b ON b.expsample_accession = e.expsample_accession 
INNER JOIN madi_dat.reagent AS c ON c.reagent_accession = b.reagent_accession 
WHERE experiment_accession = '", experiment_accession, "';"))
    }, selection = 'single',
    options = list(scrollX = TRUE)
    )
  } else {
    choice_splitted <- strsplit(choice, split = " ")[[1]][2]
    choice_splitted_2 <- strsplit(choice_splitted, split = "\\[")[[1]][1]
    lookup_table <- DT::renderDataTable({
      query <- dbGetQuery(conn, paste0("SELECT * FROM madi_dat.", choice_splitted_2))
    }, selection = 'single',
    options = list(scrollX = TRUE)
    )
  }
  return(lookup_table)
}

load_lookup_choices <- function(tab_name){
  lookup_table_value <- dbGetQuery(conn, paste0("SELECT table_name, CONCAT(column_name, ': ', foreign_key_table_name, '[', foreign_key_column_name, ']') AS column_lookup_link FROM madi_meta.vw_table_indexes WHERE table_name = '",tab_name,"' AND POSITION('lk_' IN foreign_key_table_name) > 0;"))$column_lookup_link
  if (tab_name == "expsample"){
    lookup_table_value <- c(lookup_table_value, "Reagents")
  } 
  
  return(lookup_table_value)
}


create_table_and_module <- function(new_table_name, selected_row){
  params <- filter_row_from_table(parameter_df, new_table_name)
  withProgress(message = paste('Loading', params$tab_label), value = 0.1, {
    
    ### Render the output table
    output_table <- DT::renderDataTable({
        table_value()
      }
      , selection = 'single'
      , options = list(scrollX = TRUE)
      , escape = FALSE
      #, sanitize.text.function = function(x) x
    )
   
    
    ### table selector
    table_module <- select_group_server(
      id = paste0(new_table_name,"-select"), # parameter_df$table_mod_id,
      data = table_value,
      vars = names(table_value())
      )
    
    
    # Storing values in a reactiveExpression for experiments tab
    
    if(new_table_name == "experiment"){
      out <- dbGetQuery(conn, paste0(params$table_src_query, "'", selected_row[[params$match_query_on]],"';"))
      # Putting data in the experiment_data reactiveValue
      experiment_data(out)
    } 
    
    ### Fill the output table with values
    table_value <- reactive({
      tryCatch({
        out <- dbGetQuery(conn, paste0(params$table_src_query, "'", selected_row[[params$match_query_on]],"';"))
        
        # Handle empty results safely
        if (nrow(out) == 0) {
          return(out)
        }
        
        if(new_table_name == "study_link" && "value" %in% names(out)) {
          # Only process if we have data and the value column exists
          if (nrow(out) > 0) {
            for (i in seq_len(nrow(out))) {
              arp_i <- out$value[i]
              if (!is.na(arp_i) && nchar(arp_i) > 5) {
                if (startsWith(arp_i, "http") || startsWith(arp_i, "ftp")) {
                  out$value[i] <- createLink(arp_i)
                }
              }
            }
          }
        }
        return(out)
      }, error = function(e) {
        warning(paste("Error in table_value for", new_table_name, ":", e$message))
        # Return empty data frame with at least one column to prevent errors
        return(data.frame(message = paste("Error loading", new_table_name, "data")))
      })
    })
    
    # if the output table is not empty, then insert a tab for the new table and fill the table
    if (nrow(table_value()[1]) > 0){
      #print(params)
      insert_new_tab(params)
      table_module_name <- paste0(new_table_name, "_table_module")
      assign(table_module_name, table_module, envir = .GlobalEnv)
    }
    
    setProgress(1)
  })
  
  # assign() function is used to assign table_module globally, under the name: [name]_table_module
  # Global assignment means that they become variables in the whole environment, not just within the function.
  
  return(output_table)
}

create_metadata_table <- function(node_selected){

  popup_metadata1 <- DT::renderDataTable({
    metadata_value1()
  }, options = list(scrollX = TRUE)
  )
  
  popup_metadata2 <- DT::renderDataTable({
    metadata_value2()
  }, options = list(scrollX = TRUE)
  )
  
  popup_metadata3 <- DT::renderDataTable({
    metadata_value3()
  }, options = list(scrollX = TRUE)
  )
  
  metadata_value1 <- reactive({
    metadata <- dbGetQuery(conn, paste0("SELECT column_name, description, data_type, is_nullable, is_identity FROM madi_meta.vw_table_description WHERE table_name='",node_selected,"';"))
    return(metadata)
  })
  
  metadata_value2 <- reactive({
    metadata <- dbGetQuery(conn, paste0("SELECT column_name, constraint_name, foreign_key_table_name, foreign_key_column_name FROM madi_meta.vw_table_indexes WHERE table_name='",node_selected,"';"))
    return(metadata)
  })
  
  metadata_value3 <- reactive({
    metadata <- dbGetQuery(conn, paste0("SELECT reference_table_name, reference_column_name, constraint_name, reference_schema FROM madi_meta.vw_table_reference_by_others WHERE table_name = '",node_selected,"';"))
    return(metadata)
  })
  
  table_description <- reactive({
    metadata <- dbGetQuery(conn, paste0("SELECT description FROM madi_meta.vw_table_description WHERE data_type = 'table' AND table_name = '",node_selected,"';"))
    return(metadata)
  })
  
  return(c(table_description(),popup_metadata1,popup_metadata2,popup_metadata3))
}

insert_reference_to_madi_dat.experiment <- function(connection, experiment_accession, description, measurement_technique, experiment_name, study_accession, workspace_id){
  
  insert_experiment_row <- paste0("INSERT INTO madi_dat.experiment(experiment_accession, 
                                                                    description, 
                                                                    measurement_technique, 
                                                                    name, 
                                                                    study_accession, 
                                                                    workspace_id)
                                                                    
                                  VALUES (
                                  '",experiment_accession,"',
                                  '",description,"',
                                  '",measurement_technique,"',
                                  '",experiment_name,"',
                                  '",study_accession,"',
                                  '",workspace_id,"');"
  )
  
  # Send query
  DBI::dbSendQuery(connection, insert_experiment_row)                            
  
}

insert_reference_to_madi_results.exp_analysis_query <- function(connection, study_accession, experiment_accession, measurement_technique, new_table_name ,n_expsamples, created_by, updated_by){
  
  DBI::dbSendQuery(connection, "SELECT * FROM madi_results.exp_analysis_queries ORDER BY exp_aquery_id ASC") %>% 
    print()
  
  # insert reference to the madi_results.exp_analysis_query table
  
  insert_expaq_row <- paste0("INSERT INTO madi_results.exp_analysis_queries(
                          study_accession,
                          experiment_accession,
                          measurement_technique,
                          result_schema,
                          result_table,
                          upload_result_status,
                          n_expsamples,
                          query_str1,
                          query_str2,
                          created_by,
                          created_at,
                          updated_by,
                          updated_at)
VALUES (
                           '",study_accession,"',
                           '",experiment_accession,"',
                           '",measurement_technique,"',
                           'madi_results',
                           '",new_table_name,"',
                           'loaded',
                           '",n_expsamples,"',
                           'SELECT * FROM madi_results.",new_table_name," WHERE experiment_accession = ', 
                           '_none_',
                            '",created_by,"',
                           current_timestamp,
                           '",updated_by,"',
                           current_timestamp);"
  )
  
  
  
  DBI::dbSendQuery(connection, insert_expaq_row)                                           
  
  
}

insert_reference_to_madi_meta.explore_parms <- function(connection, experiment_accession, new_table_name, study_accession){
  
  # insert reference to the madi_meta.explore_parms table
  insert_exploreparms_row <- paste0("INSERT INTO madi_meta.explore_parms(
                                  child_table, 
                                  table_src_query, 
                                  parent_table, 
                                  match_query_on, 
                                  tab_label, 
                                  tree_level, 
                                  node_group, 
                                  node_title)
                                  
VALUES                            (
                                  '",tolower(new_table_name),"',
                                  'SELECT * FROM madi_results.",new_table_name," WHERE experiment_accession = ', 
                                  'experiment', 
                                  'experiment_accession', 
                                  '",toupper(experiment_accession)," results',
                                  4, 
                                  'fk', 
                                  '",experiment_accession,"');") 
  
  DBI::dbSendQuery(connection, insert_exploreparms_row)
  
}


loop_downstream_tabs <- function(parent_table, selected_row){
  parent_params <- filter_row_from_table(parameter_df, parent_table)
  
  withProgress(message = paste0("Loading ", parent_params$tab_label), 
               value = 0.5, style = "old", {
                 
                 delete_downstream_tabs(parent_table)
                 directly_downstream_tabs <- find_downstream_tabs(parent_table)
                 
                 for (table in directly_downstream_tabs){
                   output_name <- paste0(table, "_table")
                   output[[output_name]] <- create_table_and_module(table, selected_row)
                 }
                 
                 setProgress(1)})
}

update_db <- function(..., schema = "madi_dat",help = FALSE){
  
  args <- list(...)
  
  if(help == T){
    if(length(args) == 0){
      return(cat("This function helps perform select, insert and update operations on tables 
                \n It accepts 3 inputs \n - operation : 'select', 'insert', 'update' \n - table_name : name of the table from the db schema madi_dat \n - db_data : Dataframe containing values as a single row for insertion or updation. Dataframe should have equal number of columns as the table \n - update_where : named vector (Example - c(program_id = 1)) \n \nUse the help = TRUE for any guidance with the inputs \n \n For example: update_db(table_name = 'program', help = TRUE)"))      
    }else if(length(args$table_name) != 0){
      DBI::dbGetQuery(conn, 
                      glue_sql(.con = conn, 
                               "SELECT table_name, column_name, is_nullable, data_type, character_maximum_length FROM information_schema.columns WHERE table_schema = {schema} AND table_name IN ({args$table_name*});")
      )
    }
  } else if(help == F){
    if(length(args$operation) != 0){
      
      stopifnot(length(args$operation) == 1, length(args$table_name) == 1)
      
      # Check if schema is entered 
      if(!is.null(args$schema)){
        schema <- args$schema
      }
      
      if(args$operation == "insert"){
        print("inside insert")
        
        # Get column names for the table requested
        get_column_names <-DBI::dbGetQuery(conn, 
                                           glue_sql(.con = conn, 
                                                    "SELECT column_name FROM information_schema.columns WHERE table_schema = {schema} AND table_name IN ({args$table_name*});")
        )
        
        # Constructing the SQL query in 1 parts.
        # "INSERT INTO madi_dat.<table_name> (part1) VALUES (part2)"
        
        part1 <- paste(get_column_names$column_name, collapse = ",")
        part2 <- paste0("{", get_column_names$column_name, "}", collapse = ", ")
        
        # Using assign instead of attach to create variables locally within the function
        # attach(args$db_data)
        
        for (col_name in names(args$db_data)) {
          assign(col_name, args$db_data[[col_name]])
        }
        
        glue_str <- glue(
          "INSERT INTO {schema}.{args$table_name} ({part1}) VALUES ({part2});"
        )
        
        
        print(glue_sql(.con = conn,glue_str))

        DBI::dbSendQuery(conn, glue_sql(.con = conn,glue_str))

        print("insert complete")
        
        # No need to use detach as we are not using attach anymore
        # detach(args$db_data)
      }
      
      if(args$operation == "select"){
        
        print("inside select")
        
        if(is.null(args$select_where)){
          print("selecting all columns_start")
          glue_str <- glue(
            "SELECT * FROM {schema}.{args$table_name};"
          )
          
          print(glue_sql(.con = conn, glue_str))
          
          result <- DBI::dbGetQuery(conn, glue_sql(.con = conn, glue_str))
          
          print("selecting all columns_end")
          
          return(result)
        }
        else if (length(args$select_where[[1]]) == 1){
          
          print(names(args$select_where))
          select_where_col <- names(args$select_where)
          
          if(class(args$select_where[[1]]) %in% c("numeric", "integer")){
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} = {args$select_where};"
            )
          }else if(class(args$select_where[[1]])=="character"){
            # Adding an extra parenthesis {} because glue would convert the variable to a character in here and then not have quotes later
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} = {{args$select_where}};"
            )
          }
          
          result <- DBI::dbGetQuery(conn, glue_sql(.con = conn, glue_str))
          
          print(glue_sql(.con = conn, glue_str))
          
          print(paste0("selecting where ", names(args$select_where) ,"=",args$select_where))
          
          return(result)
        }
        else{
          
          select_where_col <- names(args$select_where)
          
          if(class(args$select_where[[1]]) %in% c("numeric", "integer")){
            select_where_ids <- paste0(args$select_where[[1]], collapse = ",")
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} IN ({select_where_ids});"
            )
          }else if(class(args$select_where[[1]])=="character"){
            select_where_ids <- paste0("'", args$select_where[[1]], "'", collapse = ",")
            # Adding an extra parenthesis {} because glue would convert the variable to a character in here and then not have quotes later
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} IN ({select_where_ids});"
            )
          } 
          else {
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name};"
            )
          }
          print("location HH")
          
          result <- DBI::dbGetQuery(conn, glue_sql(.con = conn, glue_str))
          
          print(paste0("selecting where ", names(args$select_where) ,"=",args$select_where))
          
          return(result)
          
        }
        
        
        
      }
      
      if(args$operation == "update"){
        
        print("inside update")
        
        get_column_names <-DBI::dbGetQuery(conn, 
                                           glue_sql(.con = conn, 
                                                    "SELECT column_name FROM information_schema.columns WHERE table_schema = {schema} AND table_name IN ({args$table_name*});")
        )
        
        db_cols <- paste(get_column_names$column_name)
        update_where_col <- names(args$update_where)
        
        # attach(args$db_data)
        for (col_name in names(args$db_data)) {
          assign(col_name, args$db_data[[col_name]])
        }
        
        # Use paste() to concatenate strings with values from a and b vectors
        result <- paste(db_cols, "=", "{", db_cols, "}", collapse = ", ")
        
        if(class(args$update_where) %in% c("numeric", "integer")){
          glue_str <- glue(
            "UPDATE {schema}.{args$table_name} SET {result} WHERE {update_where_col} = {args$update_where};"
          )
        }else if(class(args$update_where)=="character"){
          # Adding an extra parenthesis {} because glue would convert the variable to a character in here and then not have quotes later
          glue_str <- glue(
            "UPDATE {schema}.{args$table_name} SET {result} WHERE {update_where_col} = {{args$update_where}};"
          )
        }
        print(glue_sql(.con = conn, glue_str))
        
        DBI::dbSendQuery(conn, glue_sql(.con = conn, glue_str))
        print("update complete")
        
        # detach(args$db_data)
        
      }
      
      
    }
  }
  
}
