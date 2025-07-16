observe( {
  req(input$rb_n_user == "sendMessage")
  output$sendMessage <- renderUI({ 
    tagList(
      textInput("n_user_name",label="Name",
                placeholder="Enter your name here")
      , textInput("institution", label="Your institution")
      , textInput("email_from", label="Your email address") # gmail 
      , textInput("gmail", label = "Your gmail address - Required for authentication ")
      , textInput("subject", label = "Subject")
      , textAreaInput("message", "Your message", "", width = "100%", height = "200px")
      , actionButton("send", "Send")
    )
  })
  insertTab(inputId = "body_panel_id",
            tabPanel(value = "sendMessage", title = "Send Message to Admin", uiOutput("sendMessage")
            )
  )

})

observeEvent(input$send, {
  from <- isolate(input$email_from)
  to <- "michael.s.zens@dartmouth.edu"
  subject <- isolate(input$subject)
  msg <- isolate(input$message)
  gmail <- isolate(input$gmail)
  institution <- isolate(input$institution)
  current_user <- Sys.getenv("LOGNAME")
  
  # sql_send_to_track <- paste0("INSERT INTO madi_track.track_events(
  #                              event_catalog_id, comment, created_by, created_at, updated_by, updated_at) VALUES(," 
  #                              ,"'", "Admin_Portal_User_User_New_Reported_ToAdmin","'",","
  #                              ,"'", paste0("[",current_user,"]", "[", from,"]", "[", msg,"]","[", institution,"]","[", gmail,"]"),"'",","
  #                              ,"'", "shinyu1","'",","
  #                              ,"'", Sys.time(),"'",","
  #                              ,"'", "shinyu1","'",","
  #                              ,"'", Sys.time(),"'",");")
  
  sql_send_to_track <- glue_sql("INSERT INTO madi_track.track_events( 
                                event_catalog_id, comment, created_by, created_at, updated_by, updated_at) 
                                VALUES('Admin_Portal_User_User_New_Reported_ToAdmin', {user}, 'shinyu1'
                                {time}, 'shinyu1',{time})",
                                user = current_user, time = Sys.time(), .con = conn)
  dbSendQuery(conn, sql_send_to_track)
  
  sendmail(from, to, subject, msg, control=list(smtpServer="smtp.dartmouth.edu"))
  dbSendQuery(conn, sql_send_to_track)
  showNotification("Your email has been send.")
})
