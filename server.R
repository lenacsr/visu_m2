library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {
  
  # --- Mise à jour dynamique des indicateurs selon le pays ---
  observeEvent(input$country, {
    available_indicators <- data_clean_glo %>%
      filter(
        REF_AREA_LABEL %in% input$country,
        !is.na(value)
      ) %>%
      pull(INDICATOR_LABEL) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      inputId = "indicator",
      choices = available_indicators,
      selected = available_indicators[1]
    )
  })
  
  # --- Mise à jour dynamique des âges selon l’indicateur ---
  observeEvent(input$indicator, {
    available_ages <- data_clean_glo %>%
      filter(INDICATOR_LABEL %in% input$indicator) %>%
      pull(AGE_LABEL) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      inputId = "age",
      choices = available_ages,
      selected = available_ages[1]
    )
  })
  
  # --- Mise à jour dynamique des années selon pays + indicateur + âge ---
  observeEvent({
    input$country
    input$indicator
    input$age
  }, {
    req(input$country, input$indicator, input$age)
    
    available_years <- data_clean_glo %>%
      filter(
        REF_AREA_LABEL %in% input$country,
        INDICATOR_LABEL %in% input$indicator,
        AGE_LABEL == input$age,
        !is.na(value)  # ✅ uniquement les années avec données disponibles
      ) %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      inputId = "year",
      choices = available_years,
      selected = tail(available_years, 1)  # dernière année par défaut
    )
  })
  
  # --- Données filtrées pour le graphique temporel ---
  data_filtered <- reactive({
    req(input$age, input$sex)
    data_clean_glo %>%
      filter(
        REF_AREA_LABEL %in% input$country,
        INDICATOR_LABEL %in% input$indicator,
        AGE_LABEL == input$age,
        SEX_LABEL %in% input$sex
      ) %>%
      na.omit() %>%
      mutate(year = as.factor(year))
  })
  
  # --- Graphique temporel dynamique ---
  output$graph <- renderPlot({
    df <- data_filtered()
    if (nrow(df) == 0) return(NULL)
    
    unit_label <- unique(df$UNIT_MEASURE_LABEL)
    y_label <- case_when(
      any(grepl("Count", unit_label, ignore.case = TRUE)) ~ "Valeur",
      any(grepl("Percentage|Ratio", unit_label, ignore.case = TRUE)) ~ "Valeur (%)",
      TRUE ~ "Valeur"
    )
    
    ggplot(df, aes(x = year, y = value, group = SEX_LABEL, color = SEX_LABEL)) +
      geom_point(size = 2.5) +
      geom_line(linewidth = 1) +
      labs(
        title = paste(input$indicator, "—", input$country),
        subtitle = paste("Âge :", input$age),
        x = "Année",
        y = y_label,
        color = "Sexe"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # --- Données filtrées pour le graphique en barres ---
  data_bar_filtered <- reactive({
    req(input$country, input$indicator, input$age, input$year, input$sex)
    data_clean_glo %>%
      filter(
        REF_AREA_LABEL %in% input$country,
        INDICATOR_LABEL %in% input$indicator,
        AGE_LABEL == input$age,
        year == input$year,
        SEX_LABEL %in% input$sex
      ) %>%
      na.omit()
  })
  
  # --- Graphique en barres dynamique ---
  output$graph_bar <- renderPlot({
    df <- data_bar_filtered()
    if (nrow(df) == 0) return(NULL)
    
    min_val <- min(df$value, na.rm = TRUE)
    max_val <- max(df$value, na.rm = TRUE)
    range_val <- max_val - min_val
    
    if (range_val < 5) {
      ylim_inf <- min_val - 0.5 * range_val
      ylim_sup <- max_val + 0.5 * range_val
    } else {
      ylim_inf <- min_val - 0.1 * range_val
      ylim_sup <- max_val + 0.1 * range_val
    }
    
    note_text <- paste0(
      "Note: échelle ajustée automatiquement (zoom ≈ 80% de la plage des valeurs). ",
      "Intervalle affiché : [", round(ylim_inf, 2), " ; ", round(ylim_sup, 2), "]"
    )
    
    ggplot(df, aes(x = SEX_LABEL, y = value, fill = SEX_LABEL)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = round(value, 2)), vjust = -0.5, size = 5) +
      coord_cartesian(ylim = c(ylim_inf, ylim_sup)) +
      labs(
        title = paste(input$indicator, "—", input$country, "(", input$year, ")"),
        subtitle = paste("Âge :", input$age),
        x = "Sexe",
        y = "Valeur",
        fill = "Sexe",
        caption = note_text
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        plot.caption = element_text(size = 9, hjust = 0, color = "gray40", face = "italic")
      )
  })
}
