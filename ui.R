library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Exploration des indicateurs éducatifs"),
  
  sidebarLayout(
    sidebarPanel(
      # menu déroulant pays
      selectInput(
        inputId = "country",
        label = "Choisir un pays :",
        choices = sort(unique(data_clean_glo$REF_AREA_LABEL)),
        selected = "Estonia"
      ),
      
      # menu déroulant indicateur
      selectInput(
        inputId = "indicator",
        label = "Choisir un indicateur :",
        choices = sort(unique(data_clean_glo$INDICATOR_LABEL)),
        selected = "Literacy rate (%)"
      ),
      
      # afficher/masquer les sexes
      checkboxInput("showFemale", "Afficher Femmes", value = TRUE),
      checkboxInput("showMale", "Afficher Hommes", value = TRUE)
    ),
    
    mainPanel(
      plotOutput(outputId = "indicatorPlot")
    )
  )
))