library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$indicatorPlot <- renderPlot({
    
    # Filtrer selon pays + indicateur
    data_plot <- data_clean_glo %>%
      filter(
        REF_AREA_LABEL == input$country,
        INDICATOR_LABEL == input$indicator
      )
    
    # Filtrer selon les cases cochées (sexe)
    sexes_to_keep <- c()
    if (input$showFemale) sexes_to_keep <- c(sexes_to_keep, "Female")
    if (input$showMale) sexes_to_keep <- c(sexes_to_keep, "Male")
    
    data_plot <- data_plot %>% filter(SEX_LABEL %in% sexes_to_keep)
    
    # Définir toutes les années (pour garder l’espace)
    all_years <- sort(unique(data_clean_glo$year))
    data_plot$year <- factor(data_plot$year, levels = all_years)
    
    # Années avec valeurs
    years_with_values <- data_plot %>%
      filter(!is.na(value)) %>%
      pull(year)
    
    # Graphique
    ggplot(data_plot, aes(x = year, y = value, fill = SEX_LABEL)) +
      geom_col(position = "dodge", na.rm = TRUE) +
      scale_x_discrete(breaks = years_with_values) +
      labs(
        title = paste(input$indicator, "-", input$country),
        x = "Année",
        y = "Valeur",
        fill = "Sexe"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des années
  })
})