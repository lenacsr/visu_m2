# === pages/page_visualisation.R ===

pageVisualisationUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    fluidRow(
      column(3, selectInput(ns("country"), "Pays",
                            choices = NULL,
                            selected = "Estonia")),
      column(3, selectInput(ns("indicator"), "Indicateur",
                            choices = NULL,
                            selected = "Literacy rate (%)")),
      column(3, selectInput(ns("age"), "Âge", choices = NULL)),
      column(3, selectInput(ns("sex"), "Sexe",
                            choices = c("Female", "Male", "Total"),
                            multiple = TRUE,
                            selected = c("Female", "Male", "Total")))
    ),
    
    fluidRow(column(12, plotOutput(ns("graph")))),
    hr(),
    
    fluidRow(
      column(3, selectInput(ns("year_bar"), "Année pour le graphique en barres", choices = NULL))
    ),
    fluidRow(column(12, plotOutput(ns("graph_bar"))))
  )
}


pageVisualisationServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- MISE À JOUR DES CHOIX ---
    observe({
      updateSelectInput(session, "country",
                        choices = sort(unique(dataset$REF_AREA_LABEL)),
                        selected = "Estonia")
    })
    
    observeEvent(input$country, {
      available_indicators <- dataset %>%
        filter(REF_AREA_LABEL %in% input$country, !is.na(value)) %>%
        pull(INDICATOR_LABEL) %>% unique() %>% sort()
      updateSelectInput(session, "indicator",
                        choices = available_indicators,
                        selected = "Literacy rate (%)")
    })
    
    observeEvent(input$indicator, {
      available_ages <- dataset %>%
        filter(INDICATOR_LABEL %in% input$indicator) %>%
        pull(AGE_LABEL) %>% unique() %>% sort()
      updateSelectInput(session, "age",
                        choices = available_ages,
                        selected = available_ages[1])
    })
    
    # --- RÉACTIFS ---
    data_filtered <- reactive({
      req(input$age, input$sex)
      dataset %>%
        filter(REF_AREA_LABEL %in% input$country,
               INDICATOR_LABEL %in% input$indicator,
               AGE_LABEL == input$age,
               SEX_LABEL %in% input$sex) %>%
        na.omit() %>%
        mutate(year = as.factor(year))
    })
    
    # --- GRAPHIQUE PRINCIPAL ---
    output$graph <- renderPlot({
      df <- data_filtered()
      if (nrow(df) == 0) return(NULL)
      
      unit_label <- unique(df$UNIT_MEASURE_LABEL)
      y_label <- ifelse(any(grepl("Percentage|Ratio", unit_label, ignore.case = TRUE)),
                        "Valeur (%)", "Valeur")
      
      ggplot(df, aes(x = year, y = value, group = SEX_LABEL, color = SEX_LABEL)) +
        geom_point() + geom_line() +
        labs(title = input$indicator,
             subtitle = paste("Pays :", input$country, "\nÂge :", input$age),
             x = "Année", y = y_label, color = "Sexe") +
        theme_minimal(base_size = 13)
    })
    
    # --- MISE À JOUR DES ANNÉES ---
    observeEvent(list(input$country, input$indicator, input$age, input$sex), {
      available_years <- dataset %>%
        filter(REF_AREA_LABEL %in% input$country,
               INDICATOR_LABEL %in% input$indicator,
               AGE_LABEL %in% input$age,
               !is.na(value)) %>%
        pull(year) %>% unique() %>% sort()
      updateSelectInput(session, "year_bar",
                        choices = available_years,
                        selected = tail(available_years, 1))
    })
    
    # --- DONNÉES POUR GRAPHIQUE EN BARRES ---
    data_bar <- reactive({
      req(input$country, input$indicator, input$age, input$sex, input$year_bar)
      dataset %>%
        filter(REF_AREA_LABEL %in% input$country,
               INDICATOR_LABEL %in% input$indicator,
               AGE_LABEL %in% input$age,
               SEX_LABEL %in% input$sex,
               year == input$year_bar) %>%
        na.omit()
    })
    
    # --- GRAPHIQUE EN BARRES ---
    output$graph_bar <- renderPlot({
      df <- data_bar()
      if (nrow(df) == 0) return(NULL)
      
      min_val <- min(df$value, na.rm = TRUE)
      max_val <- max(df$value, na.rm = TRUE)
      range_val <- max_val - min_val
      ylim_inf <- min_val - 0.1 * range_val
      ylim_sup <- max_val + 0.1 * range_val
      
      ggplot(df, aes(x = SEX_LABEL, y = value, fill = SEX_LABEL)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(value, 2)), vjust = -0.5) +
        coord_cartesian(ylim = c(ylim_inf, ylim_sup)) +
        labs(title = input$indicator,
             subtitle = paste(input$country, "-", input$year_bar, "\nÂge :", input$age),
             x = "Sexe", y = "Valeur (%)", fill = "Sexe") +
        theme_minimal(base_size = 14)
    })
  })
}
