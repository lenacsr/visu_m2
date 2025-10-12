### Stat descrip
library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(VIM)
library(scales)
library(tidyr)

# Palette de couleurs (la dynamique marche pas)
palette_6 <- c("#FFE100", "#F58442", "#D96C81", "#CB3452", "#A2BDF4", "#BFB74C")

## UI
pageUI_stat_desc <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    tabsetPanel(
      type = "tabs",
      
      # Onglet 1 Aperçu des données
      tabPanel("Glimpse at the data",
               fluidRow(column(12, tags$hr())),
               
               # Stats globales
               fluidRow(
                 column(3, wellPanel(h4("Number of observations"),
                                     textOutput(ns("total_obs")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #3498db;")),
                 column(3, wellPanel(h4("Number of available countries"),
                                     textOutput(ns("nb_pays")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #2ecc71;")),
                 column(3, wellPanel(h4("Nombre of indicators"),
                                     textOutput(ns("nb_indicateurs")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #f1c40f;")),
                 column(3, wellPanel(h4("Nombre years"),
                                     textOutput(ns("nb_annees")),
                                     style="background-color:#f9fafb; text-align:center; border-left:5px solid #e67e22;"))
               ),
               
               br(),
               # Distribution par sexe et âge
               fluidRow(
                 column(6, h4("By sex"), plotOutput(ns("sex_distribution_plot"), height="300px")),
                 column(6, h4("By age"), plotOutput(ns("age_distribution_plot"), height="300px"))
               ),
               
               br(),
               # Distribution par unité
               fluidRow(
                 column(12, h4("By unit of measurement"), plotOutput(ns("unit_distribution_plot"), height="300px"))
               ),
               
               br(),
               # Distribution par pays 
               fluidRow(
                 column(12, h4("By countries"))
               ),
               fluidRow(
                 column(6, plotOutput(ns("top_countries_plot"), height = "400px")),
                 column(6, plotOutput(ns("bottom_countries_plot"), height = "400px"))
               )
      ),
      
      # Onglet 2 Aperçu des NA
      tabPanel("NA glimpse",
               fluidRow(
                 column(
                   12,
                   textOutput(ns("na_value_pct")),
                   br(),
                   checkboxInput(ns("all_countries"), "All countries", value = TRUE),
                   uiOutput(ns("select_countries_ui")),
                   selectInput(ns("select_indicators"), "Select an indicator",
                               choices = NULL, multiple = TRUE),
                   br(),
                   plotOutput(ns("na_plot"), height = "700px")
                 )
               )
      )
    )
  )
}

## Server
pageServer_stat_desc <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Stats globales
    output$total_obs <- renderText({ format(nrow(dataset), big.mark = " ") })
    output$nb_pays <- renderText({ length(unique(dataset$REF_AREA_LABEL)) })
    output$nb_indicateurs <- renderText({ length(unique(dataset$INDICATOR_LABEL)) })
    output$nb_annees <- renderText({ length(unique(dataset$year)) })
    
    # Distribution par sexe
    output$sex_distribution_plot <- renderPlot({
      df <- dataset |>
        group_by(SEX_LABEL) |>
        summarise(Nombre = n()) |>
        arrange(desc(Nombre))
      
      n_cat <- nrow(df)
      fill_colors <- if (n_cat <= length(palette_6)) palette_6[1:n_cat] else colorRampPalette(palette_6)(n_cat)
      
      ggplot(df, aes(x = reorder(SEX_LABEL, Nombre), y = Nombre, fill = SEX_LABEL)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = comma(Nombre)), vjust = -0.5) +
        scale_fill_manual(values = fill_colors) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Number of observations")
    })
    
    # Distribution par âge
    output$age_distribution_plot <- renderPlot({
      df <- dataset |>
        group_by(AGE_LABEL) |>
        summarise(Nombre = n()) |>
        arrange(desc(Nombre)) |>
        head(15)
      
      palette_continue <- colorRampPalette(palette_6)
      
      ggplot(df, aes(x = reorder(AGE_LABEL, Nombre), y = Nombre, fill = Nombre)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_gradientn(colours = palette_continue(100)) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Number of observations")
    })
    
    # Distribution par unité
    output$unit_distribution_plot <- renderPlot({
      df <- dataset |>
        group_by(UNIT_MEASURE_LABEL) |>
        summarise(Nombre = n()) |>
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
        labs(x = NULL, y = "Number of observations")
    })
    
    # Distribution par pays
    output$top_countries_plot <- renderPlot({
      df <- dataset |>
        filter(!is.na(value)) |>
        group_by(REF_AREA_LABEL) |>
        summarise(Nombre = n(), .groups = 'drop') |>
        arrange(desc(Nombre)) |>
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
        labs(x = NULL, y = "Number of values",
             title = "Top 15 countries with most available values")
    })
    
    output$bottom_countries_plot <- renderPlot({
      df <- dataset |>
        filter(!is.na(value)) |>
        group_by(REF_AREA_LABEL) |>
        summarise(Nombre = n(), .groups = 'drop') |>
        arrange(Nombre) |>
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
        labs(x = NULL, y = "Number of values",
             title = "Top 15 countries with least available values")
    })
    
    # Onglet 2 NA
    observe({
      updateSelectInput(session, "select_indicators",
                        choices = sort(unique(dataset$INDICATOR_LABEL)),
                        selected = unique(dataset$INDICATOR_LABEL)[1:2])
    })
    
    output$select_countries_ui <- renderUI({
      if (!input$all_countries) {
        selectInput(session$ns("select_countries"), "Select countries",
                    choices = sort(unique(dataset$REF_AREA_LABEL)),
                    selected = unique(dataset$REF_AREA_LABEL)[1:3],
                    multiple = TRUE)
      }
    })
    
    output$na_value_pct <- renderText({
      pct_na <- mean(is.na(dataset$value)) * 100
      paste0("Part of missing values is : ",
             round(pct_na, 2), " %")
    })
    
    output$na_plot <- renderPlot({
      df_filtered <- dataset |>
        filter(INDICATOR_LABEL %in% input$select_indicators)
      
      if (!input$all_countries && !is.null(input$select_countries)) {
        df_filtered <- df_filtered |>
          filter(REF_AREA_LABEL %in% input$select_countries)
      }
      
      df_wide <- df_filtered |>
        summarise(value = mean(value, na.rm = TRUE), .by = c(REF_AREA_LABEL, INDICATOR_LABEL)) |> 
        tidyr::pivot_wider(names_from = INDICATOR_LABEL, values_from = value, names_sep = "_")
      
      par(mar = c(10, 10, 10, 10))
      VIM::aggr(df_wide,
                col = c(palette_6[1], palette_6[4]),
                numbers = TRUE,
                sortVars = TRUE,
                labels = names(df_wide),
                gap = 3,
                ylab = c("Proportion of missing", "Combinations"),
                cex.axis = 0.6,
                cex.numbers = 0.7,
                right = TRUE,
                las = 2)
    })
  })
}
