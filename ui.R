library(shiny)

ui <- fluidPage(
  titlePanel("Application de visualisation"),
  
  tabsetPanel(
    type = "tabs",
    
    # Onglet 1 : Visualisation par pays
    tabPanel(
      title = "Visualisation des données par pays",
      br(),
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "country",
            label = "Pays",
            choices = sort(unique(data_clean_glo$REF_AREA_LABEL)),
            multiple = FALSE,
            selected = "Estonia"
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "indicator",
            label = "Indicateur",
            choices = sort(unique(data_clean_glo$INDICATOR_LABEL)),
            multiple = FALSE,
            selected = "Literacy rate (%)"
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "age",
            label = "Âge",
            choices = NULL,  # mis à jour dynamiquement
            multiple = FALSE
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "sex",
            label = "Sexe",
            choices = c("Female", "Male", "Total"),
            multiple = TRUE,
            selected = c("Female", "Male", "Total")
          )
        )
      ),
      
      br(),
      fluidRow(
        column(
          width = 12,
          plotOutput("graph")
        )
      ),
      
      br(),
      hr(style = "border-top: 2px solid #bbb;"),
      br(),
      
      # Ajout de la sélection d’année
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "year",
            label = "Année",
            choices = NULL,  # mise à jour dynamique
            multiple = FALSE
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          plotOutput("graph_bar")
        )
      )
    ),
    
    # Onglet 2 : Comparaison entre pays
    tabPanel(
      title = "Comparaison des données entre pays",
      br(),
      h4("Cet onglet est encore en construction...")
    )
  )
)
