function(input, output, session) {
  # PAGE ACCEUIL
  observeEvent(input$enter_app, {
    shinyjs::hide("home_page")
    shinyjs::show("main_app")
  })
  
  # RETOUR ACCEUIL
  observeEvent(input$back_home, {
    shinyjs::hide("main_app")
    shinyjs::show("home_page")
  })
  observeEvent(input$nav_page, {
    session$sendCustomMessage("triggerResize", list())
  })  
  
  ##################
  # NOS PAGFES
  ##################
  pageAccueilServer("home", dataset = gender)
  pageComparaisonServer("comp", dataset = gender)
  pageRecapPaysServer("recap", dataset = gender)
  pageACPServer("acp", dataset = gender)
  pageServer_stat_desc("statdesc", dataset = gender)
  pageTempComparaisonServer("temp_comp", dataset = gender)
  pageVisualisationServer("visu", dataset = gender)
  pageCarteServer("carte", dataset = gender)
  pageCarteThresholdServer("carte_threshold",dataset = gender)
}
