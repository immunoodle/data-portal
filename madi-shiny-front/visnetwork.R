#### Create structures for the table model
# madi_meta.explore_parms convert to nodes 
L_nodes <- DBI::dbGetQuery(conn, "SELECT child_table AS id, tab_label AS label, node_group AS group, node_title AS title FROM madi_meta.explore_parms ORDER BY explore_parm_id;")
nodes4madi <- function() {L_nodes}

# madi_meta.explore_parms convert to edges 
L_edges <- DBI::dbGetQuery(conn, "SELECT parent_table AS from, child_table AS to, match_query_on AS title FROM madi_meta.explore_parms;")
edges4madi <- function() {L_edges}

nodes <- reactive({
  nodes <- nodes4madi()
  nodes[nrow(nodes) + 1,] <- c("study","study","primary","Provides description of a STUDY")
  # analysis sets
  as.data.frame(nodes)
})

observeEvent(input$tablemodel, {
  print('node clicked')
})


edges <- reactive({
  edges <- edges4madi()
  edges[nrow(edges) + 1,] <- c("biosample","lab_test","biosample_accession")
  edges[nrow(edges) + 1,] <- c("biosample","expsample","biosample_accession")
  exp_to_expsample <- which(edges$from == "experiment" 
                            & edges$to != "control_sample"
                            & edges$to != "standard_curve"
                            & edges$to != "experiment_protocol"
                            & edges$to != "expsample"
                            & !grepl("ext", edges$to))
  edges[exp_to_expsample,1] <- "expsample"
  edges[exp_to_expsample,3] <- "expsample_accession"
  
  # control_accession may not actually exist, i'm not sure
  edges[nrow(edges) + 1,] <- c("control_sample","mbaa_result","control_accession")
  edges[nrow(edges) + 1,] <- c("standard_curve","mbaa_result","standard_curve_accession")
  edges[nrow(edges) + 1,] <- c("planned_visit","biosample","planned_visit_accession")
  edges[nrow(edges) + 1,] <- c("assessment_panel","assessment_component","assessment_panel_accession")
  
  as.data.frame(edges)
})

output$nodestable <- DT::renderDataTable({
  nodes_data <- nodes()
  datatable(nodes_data, extensions = 'Responsive',selection = 'single')
})

require(visNetwork)

visualize_network <- function(pruned_nodes){
  renderVisNetwork({
    edges <- edges()
    visNetwork(pruned_nodes, edges, width = "100%") %>%
      visEdges(shadow = TRUE,arrows =list(to = list(enabled = TRUE, scaleFactor = 2))) %>%
      visHierarchicalLayout(direction = "LR",levelSeparation = 300,sortMethod = "directed",nodeSpacing=300) %>%
      visInteraction(navigationButtons = TRUE) %>% 
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click', nodes.nodes[0]);
                Shiny.onInputChange('node_selected', nodes.nodes.length);
                ;}")
      
  })
}

output$tablemodel <- visualize_network(nodes())

observeEvent(input$click, {
  metadata_tables <- create_metadata_table(input$click)
  table_description <- metadata_tables[1]
  popup_metadata1 <- metadata_tables[2]
  popup_metadata2 <- metadata_tables[3]
  popup_metadata3 <- metadata_tables[4]
  
  showModal(modalDialog(
    title = paste0("Table Definition: ", str_to_title(input$click)),
    shinydashboard::box(title = table_description
        , width = 14
        , solidHeader = T
    )
    , shinydashboard::box(title = "Columns", 
        width = 14,
        status = "primary", 
        solidHeader = T, 
        popup_metadata1
    )
    , shinydashboard::box(title = "Indexes", 
          width = 14,
          status = "primary", 
          solidHeader = T, 
          popup_metadata2
    )
    , shinydashboard::box(title = "Tables that Reference this Table", 
          width = 14,
          status = "primary", 
          solidHeader = T, 
          popup_metadata3
    )
    , fade=F
    , easyClose = TRUE
  ))
})

observe({
  req(input$body_panel_id)
  req(input$rb)
  output$visnetworkbox <- renderUI({
    conditionalPanel(
      condition = "input.rb == 'exploreData' && input.body_panel_id !== 'About Study'",
      shinydashboard::box(
        title = textOutput("box_state")
        ,width = "100%"
        ,"MADI Program Data Model"
        , id = "treebox"
        , collapsible = TRUE
        , collapsed = TRUE
        , closable = TRUE
        , visNetworkOutput('tablemodel')
      )
    )
  })
  
  visNetworkProxy("tablemodel") %>% visFocus(id = input$body_panel_id
                                             , scale=1
                                             , locked=FALSE) %>% visOptions(highlightNearest = T
                                             , nodesIdSelection = list(enabled=TRUE 
                                                                       ,selected = input$body_panel_id 
                                                                       ,values = input$body_panel_id 
                                                                       ,useLabels = FALSE))
})

observeEvent(input$studiestab2_rows_selected, {
  selRow = study_table_module()[input$studiestab2_rows_selected,]
  study_val <- selRow[["study_accession"]]
  
  remove_nodes <- dbGetQuery(conn, paste0("SELECT * FROM madi_results.node_to_remove WHERE study_accession IN ('",study_val,"');"))

  
  if ("mbaa_result" %in% remove_nodes[,2]){
    remove_nodes <- rbind(remove_nodes, c(" ","control_sample"), c(" ","standard_curve"))
  }
  
  ### Incomplete code, may be used in the future for obtaining the nodes to keep, rather than the ones to remove.
  # print(remove_nodes)
  
  # keep_nodes <- dbSendQuery(conn, paste0("SELECT DISTINCT result_table FROM madi_results.exp_analysis_queries WHERE study_accession = ('",study_val,"');"))
  # keep_nodes <- dbFetch(keep_nodes)
  # 
  # print(keep_nodes)
  # 
  # if (!"mbaa_result" %in% keep_nodes){
  #   keep_nodes <- keep_nodes[-which(keep_nodes$id %in% c("standard_curve", "control_sample"))]
  # }
  
  nodes <- nodes()[-which(nodes()$id %in% remove_nodes[,2]),]
  
  output$tablemodel <- visualize_network(nodes)
})