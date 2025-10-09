library(shiny)
library(dplyr)
library(tidyr)
library(DT)

pageComparaisonUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Comparaison des pays par Axes"),
    tags$style(HTML("
      .irs--single .irs-bar,
      .irs-bar,
      .noUi-connect,
      .slider-track .slider-selection {
        background: transparent !important;
        border: none !important;
      }
      input[type='range']::-webkit-slider-runnable-track { background: transparent !important; }
      input[type='range']::-moz-range-track { background: transparent !important; }
    ")),
    wellPanel(
      fluidRow(
        column(
          width = 8,
          selectInput(ns("pays"), "Choisir les pays :", choices = NULL, multiple = TRUE)
        ),
        column(
          width = 4,
          selectInput(ns("axe"), "Choisir l'axe :", choices = names(indicateurs_axes))
        )
      ),
      fluidRow(
        column(
          width = 11,
          sliderInput(ns("annee"), "Année :", min = 2000, max = 2020, value = 2020, step = 1, sep = "")
        ),
        column(
          width = 1,
          numericInput(ns("fenetre"), "Fenêtre (+- années) :", min = 0, max = 10, value = 2, step = 1)
        )
      )
    ),
    fluidRow(column(width = 12, DTOutput(ns("table"))))
  )
}

pageComparaisonServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Mettre à jour la liste des pays
    observe({
      updateSelectInput(session, "pays", choices = sort(unique(dataset$REF_AREA_LABEL)))
    })
    
    # Création du DF filtré
    data_filtre <- reactive({
      req(input$pays, input$axe, input$annee)
      indicateurs <- indicateurs_axes[[input$axe]]
      
      dataset |>
        filter(REF_AREA_LABEL %in% input$pays,
               INDICATOR_LABEL %in% indicateurs,
               SEX_LABEL %in% c("Male", "Female", "Total")) |>
        fenetre_annee(annee = as.integer(input$annee), n = as.integer(input$fenetre)) |>
        mutate(
          value_text = ifelse(is.na(value), NA_character_, round(value, 2)),
          Indicateurs = paste(INDICATOR_LABEL, "-", SEX_LABEL)
        ) |>
        select(Indicateurs, REF_AREA_LABEL, value_text) |>
        tidyr::complete(Indicateurs, REF_AREA_LABEL) |>
        pivot_wider(names_from = REF_AREA_LABEL, values_from = value_text)
    })
    
    # Rendu DT
    output$table <- renderDT({
      datatable(as.data.frame(data_filtre()), rownames = FALSE, options = list(pageLength = 100))
    })
    
  })
}
