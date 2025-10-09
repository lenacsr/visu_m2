# === pages/page_visualisation.R ===

# ---- PALETTE ----
palette_6 <- c("#FFE100", "#F58442", "#D96C81", "#CB3452", "#A2BDF4", "#BFB74C")

pageVisualisationUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    fluidRow(
      column(3, selectInput(ns("country"), "Country",
                            choices = NULL,
                            selected = "Brazil")),
      column(3, selectInput(ns("indicator"), "Indicator",
                            choices = NULL,
                            selected = "Literacy rate (%)")),
      column(3, selectInput(ns("age"), "Age", choices = NULL)),
      column(3, selectInput(ns("sex"), "Sex",
                            choices = c("Female", "Male", "Total"),
                            multiple = TRUE,
                            selected = c("Female", "Male", "Total")))
    ),
    
    fluidRow(
      column(3, uiOutput(ns("comp_breakdown_ui"))),
      column(3, uiOutput(ns("comp_sous_breakdown_ui")))
    ),
    
    fluidRow(column(12, plotOutput(ns("graph")))),
    hr(),
    
    fluidRow(
      column(3, selectInput(ns("year_bar"), "Select a year", choices = NULL))
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
                        selected = "Brazil")
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
    
    # --- UI dynamique : Breakdown & Sub-breakdown ---
    observeEvent(input$indicator, {
      req(input$indicator)
      
      df_indicator <- dataset %>% filter(INDICATOR_LABEL == input$indicator)
      
      if (!all(df_indicator$COMP_BREAKDOWN_1_LABEL %in% c("Not Applicable", NA))) {
        breakdown_choices <- sort(unique(df_indicator$COMP_BREAKDOWN_1_LABEL))
        output$comp_breakdown_ui <- renderUI({
          selectInput(ns("comp_breakdown"), "Breakdown",
                      choices = breakdown_choices,
                      selected = breakdown_choices[1],
                      multiple = FALSE)
        })
      } else {
        output$comp_breakdown_ui <- renderUI(NULL)
      }
      
      if (!all(df_indicator$COMP_BREAKDOWN_2_LABEL %in% c("Not Applicable", NA))) {
        sous_breakdown_choices <- sort(unique(df_indicator$COMP_BREAKDOWN_2_LABEL))
        output$comp_sous_breakdown_ui <- renderUI({
          selectInput(ns("comp_sous_breakdown"), "Sous-breakdown",
                      choices = sous_breakdown_choices,
                      selected = sous_breakdown_choices[1],
                      multiple = FALSE)
        })
      } else {
        output$comp_sous_breakdown_ui <- renderUI(NULL)
      }
    })
    
    # --- RÉACTIF PRINCIPAL ---
    data_filtered <- reactive({
      req(input$age, input$sex)
      df <- dataset %>%
        filter(REF_AREA_LABEL %in% input$country,
               INDICATOR_LABEL %in% input$indicator,
               AGE_LABEL == input$age,
               SEX_LABEL %in% input$sex)
      
      if (!is.null(input$comp_breakdown)) {
        df <- df %>% filter(COMP_BREAKDOWN_1_LABEL == input$comp_breakdown)
      }
      if (!is.null(input$comp_sous_breakdown)) {
        df <- df %>% filter(COMP_BREAKDOWN_2_LABEL == input$comp_sous_breakdown)
      }
      
      df %>% na.omit() %>% mutate(year = as.factor(year))
    })
    
    # --- GRAPHIQUE PRINCIPAL ---
    output$graph <- renderPlot({
      df <- data_filtered()
      if (nrow(df) == 0) return(NULL)
      
      unit_label <- unique(df$UNIT_MEASURE_LABEL)
      y_label <- ifelse(any(grepl("Percentage|Ratio", unit_label, ignore.case = TRUE)),
                        "Value (%)", "Value")
      
      subtitle_text <- paste(
        "Pays :", input$country,
        "\nÂge :", input$age,
        if (!is.null(input$comp_breakdown)) paste("\nBreakdown :", input$comp_breakdown) else "",
        if (!is.null(input$comp_sous_breakdown)) paste("\nSous-breakdown :", input$comp_sous_breakdown) else ""
      )
      
      n_cat <- length(unique(df$SEX_LABEL))
      colors <- if (n_cat <= length(palette_6)) palette_6[1:n_cat] else colorRampPalette(palette_6)(n_cat)
      
      ggplot(df, aes(x = year, y = value, group = SEX_LABEL, color = SEX_LABEL)) +
        geom_point(size = 2) + geom_line(linewidth = 1) +
        scale_color_manual(values = colors) +
        labs(title = input$indicator,
             subtitle = subtitle_text,
             x = "Year", y = y_label, color = "Sex") +
        theme_minimal(base_size = 13)
    })
    
    # --- MISE À JOUR DES ANNÉES ---
    observeEvent(list(input$country, input$indicator, input$age, input$sex,
                      input$comp_breakdown, input$comp_sous_breakdown), {
                        available_years <- dataset %>%
                          filter(REF_AREA_LABEL %in% input$country,
                                 INDICATOR_LABEL %in% input$indicator,
                                 AGE_LABEL %in% input$age,
                                 !is.na(value)) %>%
                          {
                            if (!is.null(input$comp_breakdown)) filter(., COMP_BREAKDOWN_1_LABEL == input$comp_breakdown) else .
                          } %>%
                          {
                            if (!is.null(input$comp_sous_breakdown)) filter(., COMP_BREAKDOWN_2_LABEL == input$comp_sous_breakdown) else .
                          } %>%
                          pull(year) %>% unique() %>% sort()
                        
                        updateSelectInput(session, "year_bar",
                                          choices = available_years,
                                          selected = tail(available_years, 1))
                      })
    
    # --- DONNÉES POUR GRAPHIQUE EN BARRES ---
    data_bar <- reactive({
      req(input$country, input$indicator, input$age, input$sex, input$year_bar)
      df <- dataset %>%
        filter(REF_AREA_LABEL %in% input$country,
               INDICATOR_LABEL %in% input$indicator,
               AGE_LABEL %in% input$age,
               SEX_LABEL %in% input$sex,
               year == input$year_bar)
      
      if (!is.null(input$comp_breakdown)) {
        df <- df %>% filter(COMP_BREAKDOWN_1_LABEL == input$comp_breakdown)
      }
      if (!is.null(input$comp_sous_breakdown)) {
        df <- df %>% filter(COMP_BREAKDOWN_2_LABEL == input$comp_sous_breakdown)
      }
      
      df %>% na.omit()
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
      
      subtitle_text <- paste(
        input$country, "-", input$year_bar,
        "\nÂge :", input$age,
        if (!is.null(input$comp_breakdown)) paste("\nBreakdown :", input$comp_breakdown) else "",
        if (!is.null(input$comp_sous_breakdown)) paste("\n 2nd breakdown :", input$comp_sous_breakdown) else ""
      )
      
      n_cat <- length(unique(df$SEX_LABEL))
      fill_colors <- if (n_cat <= length(palette_6)) palette_6[1:n_cat] else colorRampPalette(palette_6)(n_cat)
      
      ggplot(df, aes(x = SEX_LABEL, y = value, fill = SEX_LABEL)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(value, 2)), vjust = -0.5) +
        scale_fill_manual(values = fill_colors) +
        coord_cartesian(ylim = c(ylim_inf, ylim_sup)) +
        labs(title = input$indicator,
             subtitle = subtitle_text,
             x = "Sex", y = "Value (%)", fill = "Sex") +
        theme_minimal(base_size = 14)
    })
  })
}
