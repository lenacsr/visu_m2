# === pages/page3_statdesc.R ===

# ---- PACKAGES ----
library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(VIM)
library(scales)
library(tidyr)

# ---- PALETTE ----
palette_6 <- c("#FFE100", "#F58442", "#D96C81", "#CB3452", "#A2BDF4", "#BFB74C")

# ---- UI ----
pageUI_stat_desc <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    tabsetPanel(
      type = "tabs",
      
      # --- Onglet 1 : Aperçu des données ---
      tabPanel("Aperçu des données",
               fluidRow(column(12, tags$hr())),
               
               # Statistiques globales
               fluidRow(
                 column(3, wellPanel(h4("Nombre total d'observations"),
                                     textOutput(ns("total_obs")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #3498db;")),
                 column(3, wellPanel(h4("Nombre de pays"),
                                     textOutput(ns("nb_pays")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #2ecc71;")),
                 column(3, wellPanel(h4("Nombre d'indicateurs"),
                                     textOutput(ns("nb_indicateurs")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #f1c40f;")),
                 column(3, wellPanel(h4("Nombre d'années"),
                                     textOutput(ns("nb_annees")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #e67e22;"))
               ),
               
               br(),
               # Distribution par sexe et âge
               fluidRow(
                 column(6, h4("Par sexe"), plotOutput(ns("sex_distribution_plot"), height="300px")),
                 column(6, h4("Par âge"), plotOutput(ns("age_distribution_plot"), height="300px"))
               ),
               
               br(),
               # Distribution par unité
               fluidRow(
                 column(12, h4("Par unité de mesure"), plotOutput(ns("unit_distribution_plot"), height="300px"))
               ),
               
               br(),
               # Par pays (top/bottom 15)
               fluidRow(
                 column(12, h4("Par pays"))
               ),
               fluidRow(
                 column(6, plotOutput(ns("top_countries_plot"), height = "400px")),
                 column(6, plotOutput(ns("bottom_countries_plot"), height = "400px"))
               )
      ),
      
      # --- Onglet 2 : Aperçu des NA ---
      tabPanel("Aperçu des NA",
               fluidRow(
                 column(
                   12,
                   textOutput(ns("na_value_pct")),
                   br(),
                   checkboxInput(ns("all_countries"), "Tous les pays", value = TRUE),
                   uiOutput(ns("select_countries_ui")),
                   selectInput(ns("select_indicators"), "Sélectionnez les indicateurs",
                               choices = NULL, multiple = TRUE),
                   br(),
                   plotOutput(ns("na_plot"), height = "700px")
                 )
               )
      )
    )
  )
}

# ---- SERVER ----
pageServer_stat_desc <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # --- Indicateurs globaux ---
    output$total_obs <- renderText({ format(nrow(dataset), big.mark = " ") })
    output$nb_pays <- renderText({ length(unique(dataset$REF_AREA_LABEL)) })
    output$nb_indicateurs <- renderText({ length(unique(dataset$INDICATOR_LABEL)) })
    output$nb_annees <- renderText({ length(unique(dataset$year)) })
    
    # --- Graphique par sexe ---
    output$sex_distribution_plot <- renderPlot({
      df <- dataset %>%
        group_by(SEX_LABEL) %>%
        summarise(Nombre = n()) %>%
        arrange(desc(Nombre))
      
      n_cat <- nrow(df)
      fill_colors <- if (n_cat <= length(palette_6)) palette_6[1:n_cat] else colorRampPalette(palette_6)(n_cat)
      
      ggplot(df, aes(x = reorder(SEX_LABEL, Nombre), y = Nombre, fill = SEX_LABEL)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = comma(Nombre)), vjust = -0.5) +
        scale_fill_manual(values = fill_colors) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Nombre d'observations")
    })
    
    # --- Graphique par âge ---
    output$age_distribution_plot <- renderPlot({
      df <- dataset %>%
        group_by(AGE_LABEL) %>%
        summarise(Nombre = n()) %>%
        arrange(desc(Nombre)) %>%
        head(15)
      
      palette_continue <- colorRampPalette(palette_6)
      
      ggplot(df, aes(x = reorder(AGE_LABEL, Nombre), y = Nombre, fill = Nombre)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_gradientn(colours = palette_continue(100)) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Nombre d'observations")
    })
    
    # --- Graphique par unité ---
    output$unit_distribution_plot <- renderPlot({
      df <- dataset %>%
        group_by(UNIT_MEASURE_LABEL) %>%
        summarise(Nombre = n()) %>%
        arrange(desc(Nombre))
      
      n_cat <- nrow(df)
      fill_colors <- if (n_cat <= length(palette_6)) palette_6[1:n_cat] else colorRampPalette(palette_6)(n_cat)
      
      ggplot(df, aes(x = reorder(UNIT_MEASURE_LABEL, Nombre), y = Nombre, fill = UNIT_MEASURE_LABEL)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = comma(Nombre)), vjust = -0.5) +
        scale_fill_manual(values = fill_colors) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5)
        ) +
        labs(x = NULL, y = "Nombre d'observations")
    })
    
    # --- Graphique Top 15 pays ---
    output$top_countries_plot <- renderPlot({
      df <- dataset %>%
        filter(!is.na(value)) %>%
        group_by(REF_AREA_LABEL) %>%
        summarise(Nombre = n(), .groups = 'drop') %>%
        arrange(desc(Nombre)) %>%
        head(15)
      
      palette_continue <- colorRampPalette(palette_6)
      
      ggplot(df, aes(x = reorder(REF_AREA_LABEL, Nombre), y = Nombre, fill = Nombre)) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        geom_text(aes(label = comma(Nombre)), hjust = -0.1, size = 3.5) +
        scale_y_continuous(labels = comma_format(), limits = c(0, max(df$Nombre) * 1.15)) +
        scale_fill_gradientn(colours = palette_continue(100)) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        labs(x = NULL, y = "Nombre de valeurs non manquantes",
             title = "Top 15 pays avec le plus de valeurs")
    })
    
    # --- Graphique Bottom 15 pays ---
    output$bottom_countries_plot <- renderPlot({
      df <- dataset %>%
        filter(!is.na(value)) %>%
        group_by(REF_AREA_LABEL) %>%
        summarise(Nombre = n(), .groups = 'drop') %>%
        arrange(Nombre) %>%
        head(15)
      
      palette_continue <- colorRampPalette(palette_6)
      
      ggplot(df, aes(x = reorder(REF_AREA_LABEL, Nombre), y = Nombre, fill = Nombre)) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        geom_text(aes(label = comma(Nombre)), hjust = -0.1, size = 3.5) +
        scale_y_continuous(labels = comma_format(), limits = c(0, max(df$Nombre) * 1.15)) +
        scale_fill_gradientn(colours = palette_continue(100)) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        labs(x = NULL, y = "Nombre de valeurs non manquantes",
             title = "Top 15 pays avec le moins de valeurs")
    })
    
    # --- Onglet 2 : NA ---
    observe({
      updateSelectInput(session, "select_indicators",
                        choices = sort(unique(dataset$INDICATOR_LABEL)),
                        selected = unique(dataset$INDICATOR_LABEL)[1:2])
    })
    
    output$select_countries_ui <- renderUI({
      if (!input$all_countries) {
        selectInput(session$ns("select_countries"), "Sélectionnez les pays",
                    choices = sort(unique(dataset$REF_AREA_LABEL)),
                    selected = unique(dataset$REF_AREA_LABEL)[1:3],
                    multiple = TRUE)
      }
    })
    
    output$na_value_pct <- renderText({
      pct_na <- mean(is.na(dataset$value)) * 100
      paste0("Au total, la part de valeurs manquantes est de : ",
             round(pct_na, 2), " %")
    })
    
    output$na_plot <- renderPlot({
      df_filtered <- dataset %>%
        filter(INDICATOR_LABEL %in% input$select_indicators)
      
      if (!input$all_countries && !is.null(input$select_countries)) {
        df_filtered <- df_filtered %>%
          filter(REF_AREA_LABEL %in% input$select_countries)
      }
      
      df_wide <- df_filtered %>%
        summarise(value = mean(value, na.rm = TRUE), .by = c(REF_AREA_LABEL, INDICATOR_LABEL)) %>% 
        tidyr::pivot_wider(names_from = INDICATOR_LABEL, values_from = value, names_sep = "_")
      
      par(mar = c(10, 10, 10, 10))
      VIM::aggr(df_wide,
                col = c(palette_6[1], palette_6[4]),
                numbers = TRUE,
                sortVars = TRUE,
                labels = names(df_wide),
                gap = 3,
                ylab = c("Proportion of missing", "Combinaisons"),
                cex.axis = 0.6,
                cex.numbers = 0.7,
                right = TRUE,
                las = 2)
    })
  })
}
