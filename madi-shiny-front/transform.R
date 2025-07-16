### creates an SQL query transformer based on work from Egemen Sahin and the querytool.com site

### create a script box containing SQL code used to join or transform datasets
### this script box has buttons to allow the results of other controls to be queries 
### and stored as a materialized view that will show up as Analysis transform dataset
### in the experiments list.

from_line_sql <- reactiveVal(NULL)
select_line_sql <- reactiveVal(NULL)
where_line_sql <- reactiveVal(NULL)
union_line_sql <- reactiveVal(NULL)

observeEvent(input$transformSet, 
  {    
  ### get list of tables with data for study
  stdy_sel <- rv_study_selected$study_accession
  print(paste0("selected_study_accession: ", stdy_sel))
  tables_with_rows_query <- glue("SELECT study_accession, table_name FROM (
	SELECT 1 AS table_group, study_accession, table_name
	FROM madi_meta.in_front_study
	WHERE n_rows > 0 and study_accession = '{browse_study_accession()}'
	) AS a ORDER BY table_group, table_name;")
  print(glue_sql(.con = conn, tables_with_rows_query))
  tables_with_rows <- DBI::dbGetQuery(conn, glue_sql(.con = conn, tables_with_rows_query))
  print(tables_with_rows)
    
  row_nums_selected <- shinyValue('cb_', nrow(experiment_data())) %>%
    enframe() %>%
    dplyr::filter(value == TRUE) %>%
    pull(name)
  experiment_list <- as.data.frame(experiment_data())
    
  for (row in 1:nrow(experiment_list[row_nums_selected,])) {
      study_accession_str <- experiment_list[row, "study_accession"]
      study_accession_str_low <- tolower(study_accession_str)
      experiment_accession_str  <- experiment_list[row, "experiment_accession"]
      experiment_accession_str_low  <- tolower(experiment_accession_str)
      mat_vw_schema <- "madi_results"
      mat_vw_exp <- paste0("mv_", study_accession_str_low, "_", experiment_accession_str_low)
      # check that the materialized view for each experiment_exists and if not, make it
      # use glue
      check_mv_query <-paste0("SELECT EXISTS (
   SELECT FROM pg_catalog.pg_class c
   INNER JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
   WHERE n.nspname = '", mat_vw_schema, "' AND c.relkind = 'm' AND c.relname = '", mat_vw_exp, "') AS pg_catalog;")
      check_mv <- dbGetQuery(conn,check_mv_query)
      
      print(paste0("check_mv: ", check_mv))
      if(check_mv == TRUE) {
        
      } else {
        ## create materialized view for experiment
      }
      
      if (row == 1) {
        row1_exp_accession_str =  experiment_accession_str
        select_line_sql(paste0("SELECT * "))
        from_line_part <- paste0("FROM ", mat_vw_schema, ".", mat_vw_exp, " AS ", experiment_accession_str )
        where_line_sql(paste0(";")) 
      } else {
        from_line_next <- paste0("INNER JOIN ", mat_vw_schema, ".", mat_vw_exp, " ON ", experiment_accession_str, ".biosample_accession = ", row1_exp_accession_str, ".biosample_accession" )
        from_line_part <- paste(from_line_part, from_line_next, sep="\n")
        where_line_sql(paste0(";"))
      }
  }
  
  from_line_sql(from_line_part)
  selected_sqlquery <- toString(paste(select_line_sql(), from_line_sql(), where_line_sql(), sep="\n" ))
  print(paste0("SQL query: ", selected_sqlquery))
  select_query_str <- paste("<pre><code class='language-sql'>", selected_sqlquery, "</code></pre>")
  
        output$transform_script <- renderUI({
          shinydashboard::box(title = paste0("SQL Query")
              , width=14, status="primary", solidHeader=T, collapsible=T, collapsed=T
              , prismDependencies
              , prismSqlDependency
              , renderUI({
                prismCodeBlock(select_query_str)
              })
          )
        })

### create a TABLES box with a table selector for pulling in tables from the 
### MADI database and assigning them to joins.
### selections here contribute to the SQL code in the script box.
        output$transform_tables <- renderUI({
          shinydashboard::box(title = "Tables and Joins", width=14, status="primary"
              , solidHeader=T, collapsible=T, collapsed=T
              , reactable::reactableOutput("tables_table", width = "100%")
              , actionButton("program_add_button", "Add", icon("plus"))
              , actionButton("program_edit_button", "Edit", icon("edit"))
              , reactable::reactableOutput("joins_table", width = "100%")
              , actionButton("joins_add_button", "Add", icon("plus"))
              , actionButton("joins_delete_button", "Delete", icon("delete"))
          )              
        })



### create a COLUMNS box tab with a column selector for choosing a recoding columns from the 
### MADI database tables.
### selections here contribute to the SQL code in the script box.


### create a WHERE box tab with controls to build up a where command from columns 
### MADI database tables. pull from the widgets version of the [https://harryfish.shinyapps.io/qbr_demo/]
### selections here contribute to the SQL code in the script box.


### Create the tab panel and assemble the boxes for this app
      insertTab(inputId = "body_panel_id",
                tabPanel(value = "transform"
                         , title = "Transform data sets"
                         , uiOutput("transform_tables")
                         , uiOutput("transform_script")
                         
                )
      ) 
  })
