############
# UI Accueil
############
pageAccueilUI <- function(id, dataset) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel(
      h1("A worldwide analysis tool of gender inequalities", style = "justify-content: center")
         ),
    
    tabsetPanel(
      tabPanel("Homepage",
               br(),
               div(
                 style = "text-align: center; max-width: 900px; margin: auto;",
                 
                 h3("Explore gender inequalities across the world"),
                 
                 p("Our main goal with this app was to create a tool to showcase the different gender-disaggregated indicators that we found in the World Bank Data Bank. With this app, we wanted users to easily browse through indicators and gain a better understanding of the world we live in from 2000 to 2023. They can explore different dimensions of the gender gaps with our maps, exploratory descriptive statistical analysis, and PCA."),
                 
                 p("We aimed this website at sociologists without a R visualisation background in the field to make data visualization more accessible, while still providing experts the flexibility to explore and analyze the data according to their specific interests."),
                 
                 p("During the development of this app, we faced many challenges due mainly to a few reasons: the data structure, the missing data, and the restrictions at Rennes 2 University that prevented Git downloads.")
               )
      ),
      tabPanel("DataSet",
               br(),
               tags$a(href="https://data360.worldbank.org/en/search?database=WB_GS&tab=indicator&themeAndTopics=P2_000002", "Gender Statistics | DataBank"),
               DTOutput(ns("table_data"))
      ),
      tabPanel("Color Palette",
               br(),
               div(
                 style = "text-align: center; max-width: 600px; margin: auto;",
                 
                 h3("Choose a color palette"),
                 
                 radioButtons(ns("palette_choice"), 
                              label = "Select your preferred color palette:",
                              choices = c("Default" = "default",
                                          "Colorblind-friendly" = "daltonian"),
                              selected = "default",
                              inline = TRUE)
               )
      )
    )
  )
}


############
# Server Accueil
############
pageAccueilServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    output$table_data <- renderDT({
      datatable(
        dataset,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # palette
    observe({
      if(input$palette_choice == "default") {
        palette_choice(palette_6)
      } else {
        palette_choice(palette_daltonian)
      }
    })
    
  })
}
