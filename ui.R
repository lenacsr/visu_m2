library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Application de visualisation"),
  
  tabsetPanel(
    type = "tabs",
    
    # ONGLET 1 : visualisation par pays
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
            choices = NULL,
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
      
      fluidRow(column(width = 12, plotOutput("graph"))),
      hr(),
      
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "year_bar",
            label = "Année pour le graphique en barres",
            choices = NULL
          )
        )
      ),
      
      fluidRow(column(width = 12, plotOutput("graph_bar")))
    ),
    
    # ONGLET 2 : comparaison entre pays
    tabPanel(
      title = "Comparaison des données entre pays",
      br(),
      
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "country_compare",
            label = "Pays (sélection multiple)",
            choices = sort(unique(data_clean_glo$REF_AREA_LABEL)),
            multiple = TRUE,
            selected = c("Estonia", "Finland")
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "indicator_compare",
            label = "Indicateur (disponible pour tous les pays sélectionnés)",
            choices = NULL
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "age_compare",
            label = "Âge (sélection multiple)",
            choices = NULL,
            multiple = TRUE
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "sex_compare",
            label = "Sexe (sélection multiple)",
            choices = c("Female", "Male", "Total"),
            multiple = TRUE,
            selected = c("Female", "Male", "Total")
          )
        )
      ),
      
      fluidRow(column(width = 12, plotOutput("graph_compare"))),
      
      br(), hr(),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("ℹ️ Aide à la lecture"),
            tags$ul(
              tags$li("Les indicateurs affichés sont uniquement ceux disponibles pour tous les pays sélectionnés."),
              tags$li("Les libellés d’âge sont regroupés : les pays associés sont indiqués entre parenthèses."),
              tags$li("Les couleurs des courbes sont propres à chaque pays."),
              tags$li("Les styles de lignes varient selon le sexe :"),
              tags$ul(
                tags$li("Trait plein → Female"),
                tags$li("Tireté long → Male"),
                tags$li("Tireté double → Total")
              )
            ),
            style = "background-color:#f8f9fa; border:1px solid #ddd; border-radius:10px;"
          )
        )
      )
    ),
    
    # === ONGLET 3 : Aperçu / Statistiques ===
    tabPanel(
      title = "Aperçu / Statistiques",
      br(),
      
      tabsetPanel(
        type = "tabs",
        
        # --- Sous-onglet 1 : Aperçu des données ---
        tabPanel(
          title = "Aperçu des données",
          
          # --- En-tête ---
          fluidRow(
            column(width = 12, h3("Aperçu / Statistiques du dataset")),
            column(width = 12, tags$hr())
          ),
          
          # --- Cartes de synthèse ---
          fluidRow(
            column(width=3, wellPanel(
              h4("Nombre total d’observations", style="font-weight:bold; color:#2c3e50;"),
              textOutput("total_obs"),
              style="background-color:#f9fafb; text-align:center; border-left:5px solid #3498db;"
            )),
            column(width=3, wellPanel(
              h4("Nombre de pays", style="font-weight:bold; color:#2c3e50;"),
              textOutput("nb_pays"),
              style="background-color:#f9fafb; text-align:center; border-left:5px solid #2ecc71;"
            )),
            column(width=3, wellPanel(
              h4("Nombre d’indicateurs", style="font-weight:bold; color:#2c3e50;"),
              textOutput("nb_indicateurs"),
              style="background-color:#f9fafb; text-align:center; border-left:5px solid #f1c40f;"
            )),
            column(width=3, wellPanel(
              h4("Nombre d’années", style="font-weight:bold; color:#2c3e50;"),
              textOutput("nb_annees"),
              style="background-color:#f9fafb; text-align:center; border-left:5px solid #e67e22;"
            ))
          ),
          
          br(), br(),
          
          # --- Section Répartition des données ---
          fluidRow(
            column(
              width = 12,
              h3("Répartition des données", style="font-weight:bold; color:#2c3e50;"),
              tags$hr(style="border-top:2px solid #ccc;")
            )
          ),
          
          fluidRow(
            column(width=6, h4("Par sexe"), plotOutput("sex_distribution_plot", height="300px")),
            column(width=6, h4("Par âge"), plotOutput("age_distribution_plot", height="300px"))
          ),
          
          br(),
          
          fluidRow(
            column(width=12, h4("Par unité de mesure"), plotOutput("unit_distribution_plot", height="250px"))
          )
        ),
        
        # --- Sous-onglet 2 : Aperçu des NA ---
        tabPanel(
          title = "Aperçu des NA",
          fluidRow(column(width = 12, h4("Graphique de gestion des NA"), plotOutput("na_plot"))),
          fluidRow(column(width = 12, h4("Résumé des valeurs manquantes"), verbatimTextOutput("na_summary")))
        )
      )
    )
  )
)
