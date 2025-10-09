# === pages/page3_statdesc.R ===

# ---- PACKAGES ----
library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(VIM)     # pour la fonction aggr()
library(scales)

# ---- UI ----
pageUI_stat_desc <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    tabsetPanel(
      type = "tabs",
      
      # --- Onglet 1 : Aperçu des données ---
      tabPanel("Aperçu des données",
               fluidRow(
                 column(12, h3("Aperçu / Statistiques du dataset")),
                 column(12, tags$hr())
               ),
               fluidRow(
                 column(3, wellPanel(h4("Nombre total d’observations"),
                                     textOutput(ns("total_obs")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #3498db;")),
                 column(3, wellPanel(h4("Nombre de pays"),
                                     textOutput(ns("nb_pays")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #2ecc71;")),
                 column(3, wellPanel(h4("Nombre d’indicateurs"),
                                     textOutput(ns("nb_indicateurs")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #f1c40f;")),
                 column(3, wellPanel(h4("Nombre d’années"),
                                     textOutput(ns("nb_annees")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #e67e22;"))
               ),
               br(),
               fluidRow(
                 column(6, h4("Par sexe"), plotOutput(ns("sex_distribution_plot"), height="300px")),
                 column(6, h4("Par âge"), plotOutput(ns("age_distribution_plot"), height="300px"))
               ),
               br(),
               fluidRow(column(12, h4("Par unité de mesure"), plotOutput(ns("unit_distribution_plot"), height="250px")))
      ),
      
      # --- Onglet 2 : Aperçu des NA ---
      tabPanel("Aperçu des NA",
               fluidRow(column(12, h4("Graphique de gestion des NA"), plotOutput(ns("na_plot")))),
               fluidRow(column(12, h4("Résumé des valeurs manquantes"), verbatimTextOutput(ns("na_summary"))))
      )
    )
  )
}

# ---- SERVER ----
pageServer_stat_desc <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    output$total_obs <- renderText({
      format(nrow(dataset), big.mark = " ")
    })
    
    output$nb_pays <- renderText({
      length(unique(dataset$REF_AREA_LABEL))
    })
    
    output$nb_indicateurs <- renderText({
      length(unique(dataset$INDICATOR_LABEL))
    })
    
    output$nb_annees <- renderText({
      length(unique(dataset$year))
    })
    
    output$sex_distribution_plot <- renderPlot({
      df <- dataset %>%
        group_by(SEX_LABEL) %>%
        summarise(Nombre = n()) %>%
        arrange(desc(Nombre))
      
      ggplot(df, aes(x = reorder(SEX_LABEL, Nombre), y = Nombre, fill = SEX_LABEL)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = scales::comma(Nombre)), vjust = -0.5) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Nombre d’observations")
    })
    
    output$age_distribution_plot <- renderPlot({
      df <- dataset %>%
        group_by(AGE_LABEL) %>%
        summarise(Nombre = n()) %>%
        arrange(desc(Nombre)) %>%
        head(15)
      
      ggplot(df, aes(x = reorder(AGE_LABEL, Nombre), y = Nombre, fill = Nombre)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_viridis_c(option = "plasma") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Nombre d’observations")
    })
    
    output$unit_distribution_plot <- renderPlot({
      df <- dataset %>%
        group_by(UNIT_MEASURE_LABEL) %>%
        summarise(Nombre = n()) %>%
        arrange(desc(Nombre))
      
      ggplot(df, aes(x = reorder(UNIT_MEASURE_LABEL, Nombre), y = Nombre, fill = UNIT_MEASURE_LABEL)) +
        geom_col(width = 0.7) +
        coord_flip() +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Nombre d’observations")
    })
    
    output$na_plot <- renderPlot({
      aggr(dataset,
           col = c("skyblue", "orange"),
           numbers = TRUE,
           sortVars = TRUE,
           cex.axis = .7,
           gap = 2,
           ylab = c("Données présentes", "Valeurs manquantes"))
    })
    
    output$na_summary <- renderPrint({
      sapply(dataset, function(x) sum(is.na(x)))
    })
  })
}
