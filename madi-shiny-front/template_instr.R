
# template set table
template_set <- DBI::dbGetQuery(conn, "SELECT * FROM madi_meta.template_set_instructions;")

# templates table
templates <- DBI::dbGetQuery(conn, "SELECT * FROM madi_meta.template_instr;")

# template variable table
template_var <- DBI::dbGetQuery(conn, "SELECT * FROM madi_meta.template_variable_instructions;")

select_query <- paste0("SELECT template_id, template_variable_name,
                       string_agg(TRIM(BOTH FROM CONCAT('[',instruction_type, ': ', var_instructions, ']'))::text,' ')
                       AS var_attrib
                       FROM (SELECT template_id, template_variable_name,instruction_type,var_instructions
                              FROM madi_meta.template_variable_instructions WHERE var_instructions!='None'
                              ORDER BY template_id, template_variable_name,instruction_type DESC
                       ) AS a GROUP BY template_id, template_variable_name 
                       ORDER BY template_id, template_variable_name;")
#print(select_query)
template_var <- DBI::dbGetQuery(conn,select_query)
template_var$var_attrib <- gsub(pattern = "\n", replacement = "] [", x = template_var$var_attrib)

################# Save the selected set ########################################
rv_set_name<-reactiveValues()

observeEvent(input$temp_set_selected_prm, {
  rv_set_name$set_name_prm=input$temp_set_selected_prm
#  print(paste0("The selected set name is ", rv_set_name$set_name_prm))
#  print(template_set[template_set$template_set_name==rv_set_name$set_name_prm,])
})

observeEvent(input$temp_set_selected_std, {
  rv_set_name$set_name_std=input$temp_set_selected_std
#  print(paste0("The selected set name is ", rv_set_name$set_name_std))
#  print(template_set[template_set$template_set_name==rv_set_name$set_nam_std,])
})

############### Template set instruction text box ##############################
output$prm.set.instr.box <- renderText({
  unique(template_set[which(template_set$template_set_name==rv_set_name$set_name_prm), ]$set_instructions)
})

output$std.set.instr.box <- renderText({
  unique(template_set[which(template_set$template_set_name==rv_set_name$set_name_std), ]$set_instructions)
})

########### "Primary Templates" and "Standalone Templates" in the set ##########

output$primary.tab <- DT::renderDataTable({
  #print(paste0("This is the current value of sel.set.prm: ", toString(sel.set.prm)))
  templates[which(templates$template_id==template_set[which(template_set$template_set_name==rv_set_name$set_name_prm), 3]), 1:4]
}, selection = 'single')

output$standalone.tab <- DT::renderDataTable({
  #print(paste0("This is the current value of sel.set.std: ", toString(sel.set.std)))
  templates[which(templates$template_id==template_set[which(template_set$template_set_name==rv_set_name$set_name_std), 3]), 1:4]
}, selection = 'single')

############################# Template Tab Table ############################# 
primary_temp_table <- reactive({
  templates[which(templates$template_id==template_set[which(template_set$template_set_name==rv_set_name$set_name_prm), 3]), 1:4]
})

standalone_temp_table <- reactive({
  templates[which(templates$template_id==template_set[which(template_set$template_set_name==rv_set_name$set_name_std), 3]), 1:4]
})

primary_tab_module <- select_group_server(
  id = "primary.tab",
  data = primary_temp_table,
  vars = names(primary_temp_table())
  )

standalone_tab_module <- select_group_server(
  id = "standalone.tab",
  data = standalone_temp_table,
  vars = names(standalone_temp_table())
  )


################################################################################ 
######################### Variable Information Tab #############################
################################################################################

############################### Primary ########################################

########################## Show Var Info tab after clicking ####################

observeEvent(input$primary.tab_rows_selected, {
  prm.temp.sel.row <- primary_tab_module()[input$primary.tab_rows_selected,]
  prm_template_id_selected <- prm.temp.sel.row$template_id
  ## Removing old tabs
  removeTab(inputId = "body_panel_id", target = "temp.var.prm")
  ## Var info table
  insertTab(inputId = "body_panel_id",
            tab=tabPanel(value = "temp.var.prm",
                         title = "Variable Information",
                         DT::dataTableOutput("var.prm.tab"),
                         target="primary.temp.instr"
            ))
  
  output$var.prm.tab <- DT::renderDataTable({
    template_var[which(template_var$template_id==prm_template_id_selected),]
  }
  )

  ############################### Download #####################################
  
  ####### download button shows up after clicking
  # observeEvent(prm.temp.sel.row,{
  #   if(is.null(prm.temp.sel.row) == T){
  #     shinyjs::hide("download_temp_prm")
  #   }else{
  #     shinyjs::show("download_temp_prm")
  #   }
  # }) 
  # 
  # shinyjs::hide("download_temp_prm")
  # 
  # observeEvent(prm.temp.sel.row, {
  #   shinyjs::show("download_temp_prm")
  # })
  
  print(paste0("the selected row", prm.temp.sel.row))
  download_path_text <- paste0(stri_replace_all_charclass(Sys.getenv("blank_template_path"), "\\p{WHITE_SPACE}", ""))
  full_file_name <- paste0(download_path_text,"/", prm.temp.sel.row$template_id, collapse = NULL)
#  print(paste0("the full file name is:", full_file_name))
  output$template_selected <- renderText({
    paste(full_file_name)
  })
  output$download_temp_prm <- downloadHandler(
    filename = function() {
      paste(prm.temp.sel.row$template_id)
    },
    content = function(file) {
      file.copy(full_file_name, file)
    }
  )
#  print(paste0("file name used in the download handler: ",full_file_name))
})

################################################################################
############################### Standalone #####################################
################################################################################

observeEvent(input$standalone.tab_rows_selected, {
  std.temp.sel.row <- standalone_tab_module()[input$standalone.tab_rows_selected,]
  std_template_id_selected <- std.temp.sel.row$template_id
  ## Removing old tabs
  removeTab(inputId = "body_panel_id", target = "temp.var.std")
  ## Var info table
  insertTab(inputId = "body_panel_id",
            tab=tabPanel(value = "temp.var.std", 
                         title = "Variable Information", 
                         DT::dataTableOutput("var.std.tab"),
                         target="standalone.temp.instr"
            ))
  
  output$var.std.tab <- DT::renderDataTable({
    template_var[which(template_var$template_id==std_template_id_selected),]
  })
  
  ############################### Download #####################################
  print(paste0("the selected row", std.temp.sel.row))
  download_path_text <- paste0(stri_replace_all_charclass(Sys.getenv("blank_template_path"), "\\p{WHITE_SPACE}", ""))
  full_file_name <- paste0(download_path_text,"/", std.temp.sel.row$template_id, collapse = NULL)
#  print(paste0("the full file name is:", full_file_name))
  output$template_selected <- renderText({
    paste(full_file_name)
  })
  output$download_temp_std <- downloadHandler(
    filename = function() {
      paste(std.temp.sel.row$template_id)
    },
    content = function(file) {
      file.copy(full_file_name, file)
    }
  )
#  print(paste0("file name used in the download handler: ",full_file_name))
})
