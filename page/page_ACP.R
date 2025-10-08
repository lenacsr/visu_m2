# ---- page/page_acp.R ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(FactoMineR)
library(factoextra)
library(DT)

# ---- UI ----
pageACPUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Analyse en Composantes Principales (ACP)"),
    wellPanel(
      fluidRow(
        column(4,
               selectInput(ns("axe"), "Choisir un axe :", choices = names(indicateurs_axes))
        ),
        column(4,
               selectInput(ns("sexe"), "Choisir le sexe :", choices = c("Male", "Female", "Total"))
        ),
        column(4,
               sliderInput(ns("annee"), "Choisir l'année :", min = 2000, max = 2020, value = 2020, step = 1)
        )
      )
    ),
    fluidRow(
      column(8, plotlyOutput(ns("plot_acp"))),
      column(4, DTOutput(ns("table_coord")))
    )
  )
}

# ---- SERVER ----
pageACPServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    data_acp <- reactive({
      req(input$axe, input$sexe, input$annee)
      
      indicateurs <- indicateurs_axes[[input$axe]]
      
      df <- dataset %>%
        filter(
          SEX_LABEL == input$sexe,
          INDICATOR_LABEL %in% indicateurs
        ) %>%
        fenetre_annee(input$annee, n = 1) %>%
        group_by(REF_AREA_LABEL, INDICATOR_LABEL) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = INDICATOR_LABEL, values_from = value)
      
      df_clean <- df %>% drop_na()
      if (nrow(df_clean) < 3) return(NULL)
      
      df_acp <- df_clean %>% column_to_rownames("REF_AREA_LABEL")
      
      res.pca <- FactoMineR::PCA(df_acp, scale.unit = TRUE, graph = FALSE)
      
      list(res = res.pca, coords = df_acp)
    })
    
    output$plot_acp <- renderPlotly({
      req(data_acp())
      
      pca_res <- data_acp()$res
      df_plot <- as.data.frame(pca_res$ind$coord)
      df_plot$Pays <- rownames(df_plot)
      
      p <- ggplot(df_plot, aes(x = Dim.1, y = Dim.2, label = Pays)) +
        geom_point(color = "#FF6600", size = 3) +
        geom_text(vjust = -0.7, size = 3) +
        labs(
          x = paste0("Dim 1 (", round(pca_res$eig[1, 2], 1), "%)"),
          y = paste0("Dim 2 (", round(pca_res$eig[2, 2], 1), "%)"),
          title = "ACP des pays selon les indicateurs"
        ) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    output$table_coord <- renderDT({
      req(data_acp())
      
      pca_res <- data_acp()$res
      coords <- as.data.frame(pca_res$ind$coord)
      datatable(round(coords, 2))
    })
  })
}
