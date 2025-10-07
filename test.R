
data_clean_glo <- read.csv("CSV_GENDER_STAT_CLEAN.csv", 
                           sep = ",",        
                           header = TRUE,    
                           stringsAsFactors = FALSE)
head(data_clean_glo)


# vérifie si plusieurs lignes par combinaison année/sexe
data_clean_glo %>%
  filter(REF_AREA_LABEL == "Estonia",
         INDICATOR_LABEL == "Literacy rate (%)") %>%
  group_by(year, SEX_LABEL) %>%
  summarise(nb_lignes = n(),
            valeurs = paste(unique(value), collapse = ", ")) %>%
  arrange(year, SEX_LABEL)


### geom line

library(dplyr)
library(ggplot2)

data_estonia <- data_clean_glo %>%
  filter(REF_AREA_LABEL == "Estonia",
         INDICATOR_LABEL == "Literacy rate (%)",
         AGE_LABEL == "15 to 24 years old") %>%
  na.omit() %>%
  mutate(year = as.factor(year))

years_with_values <- data_estonia %>%
  filter(!is.na(value)) %>%
  pull(year)

p1 <- data_estonia %>% 
  group_by(SEX_LABEL) %>% 
  ggplot() + 
  aes(x = year, y = value, group = SEX_LABEL, color = SEX_LABEL) + 
  labs( title = "Literacy rate Estonie (F)", x = "Année", y = "Taux (%)" ) + 
  scale_x_discrete(breaks = years_with_values) + # n’affiche que les années avec valeurs 
  geom_point() + 
  geom_line() + 
  theme_minimal() 
p1



#########################################################

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Visualisation des indicateurs"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = "Sélectionnez le pays",
        choices = sort(unique(data_clean_glo$REF_AREA_LABEL)),
        multiple = TRUE,
        selected = "Estonia"
      ),
      
      selectInput(
        inputId = "indicator",
        label = "Sélectionnez l'indicateur",
        choices = sort(unique(data_clean_glo$INDICATOR_LABEL)),
        multiple = TRUE,
        selected = "Literacy rate (%)"
      ),
      
      selectInput(
        inputId = "age",
        label = "Sélectionnez la tranche d'âge",
        choices = sort(unique(data_clean_glo$AGE_LABEL)),
        multiple = FALSE,
        selected = "15 to 24 years old"
      )
    ),
    
    mainPanel(
      plotOutput("graph")
    )
  )
)

server <- function(input, output, session) {
  
  data_filtered <- reactive({
    data_clean_glo %>%
      filter(
        REF_AREA_LABEL %in% input$country,
        INDICATOR_LABEL %in% input$indicator,
        AGE_LABEL == input$age
      ) %>%
      na.omit() %>%
      mutate(year = as.factor(year))
  })
  
  output$graph <- renderPlot({
    df <- data_filtered()
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    years_with_values <- df %>%
      filter(!is.na(value)) %>%
      pull(year) %>%
      unique()
    
    ggplot(df, aes(x = year, y = value, group = SEX_LABEL, color = SEX_LABEL)) +
      geom_point() +
      geom_line() +
      labs(
        title = paste("Indicateur :", paste(input$indicator, collapse = ", "),
                      "\nPays :", paste(input$country, collapse = ", "),
                      "\nÂge :", input$age),
        x = "Année",
        y = "Valeur (%)",
        color = "Sexe"
      ) +
      scale_x_discrete(breaks = years_with_values) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)


################################################################ BARPLOT

library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrage
plot_data <- data_clean_glo %>%
  filter(
    REF_AREA_LABEL == "Estonia",
    INDICATOR_LABEL == "Literacy rate (%)",
    year == 2021,
    AGE_LABEL == "15 years old and over",
    !is.na(value)
  )

# Calcul des bornes dynamiques : plage ≈ 80% de la hauteur
min_val <- min(plot_data$value, na.rm = TRUE)
max_val <- max(plot_data$value, na.rm = TRUE)
range_val <- max_val - min_val

if (range_val < 5) {
  ylim_inf <- min_val - 0.5 * range_val
  ylim_sup <- max_val + 0.5 * range_val
} else {
  ylim_inf <- min_val - 0.1 * range_val
  ylim_sup <- max_val + 0.1 * range_val
}

# Texte d’indication automatique
note_text <- paste0(
  "Note: échelle ajustée automatiquement (zoom ≈ 80% de la plage des valeurs). ",
  "Intervalle affiché : [", round(ylim_inf, 2), " ; ", round(ylim_sup, 2), "]"
)

# Création du graphique
ggplot(plot_data, aes(x = SEX_LABEL, y = value, fill = SEX_LABEL)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(value, 2)), vjust = -0.5, size = 5) +
  coord_cartesian(ylim = c(ylim_inf, ylim_sup)) +
  labs(
    title = "Literacy rate (%) in Estonia (2021)",
    subtitle = "Population aged 15 years old and over",
    x = "Sex",
    y = "Value (%)",
    fill = "Sex",
    caption = note_text  # 👈 petite note discrète sous le graphique
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40", face = "italic")
  )





################################################################



str(data_clean_glo$value)

unique(data_clean_glo$year)

unique(data_clean_glo$UNIT_MEASURE_LABEL)


############################################################## NA


library(VIM)

output$na_plot <- renderPlot({
  aggr(data_clean_glo,
       col = c("skyblue", "orange"),
       numbers = TRUE,
       sortVars = TRUE,
       cex.axis = .7,
       gap = 2,
       ylab = c("Données présentes", "Valeurs manquantes"))
})
output$na_summary <- renderPrint({
  sapply(data_clean_glo, function(x) sum(is.na(x)))
})



