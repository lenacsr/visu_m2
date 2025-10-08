# === pages/page_comparaison.R ===

# --- UI du module ---
pageTempComparaisonUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    fluidRow(
      column(width = 3,
             selectInput(ns("country_compare"), "Pays (sélection multiple)",
                         choices = NULL,
                         multiple = TRUE,
                         selected = c("Estonia", "Finland"))),
      column(width = 3,
             selectInput(ns("indicator_compare"), "Indicateur", choices = NULL)),
      column(width = 3,
             selectInput(ns("age_compare"), "Âge", choices = NULL, multiple = TRUE)),
      column(width = 3,
             selectInput(ns("sex_compare"), "Sexe",
                         choices = c("Female", "Male", "Total"),
                         multiple = TRUE,
                         selected = c("Female", "Male", "Total")))
  ),
  fluidRow(
    column(width = 12, 
           plotOutput(ns("graph_compare")
                      )
           )
    ),
  br(), 
  hr(),
  fluidRow(column(width = 12,
                  wellPanel(
                    h4("ℹ️ Aide à la lecture"),
                    tags$ul(
                      tags$li("Les indicateurs affichés sont uniquement ceux disponibles pour tous les pays sélectionnés."),
                      tags$li("Les libellés d’âge sont regroupés : les pays associés sont indiqués entre parenthèses."),
                      tags$li("Les couleurs des courbes sont propres à chaque pays."),
                      tags$li("Les styles de lignes varient selon le sexe :"),
                      tags$ul(
                        tags$li("Trait plein → Female"),
                        tags$li("Tireté long → Male"),
                        tags$li("Tireté double → Total"),
                        tags$li("Forme des points : rond/triangle/carré selon sexe")
                      )
                    )
                  )))
  )
}

# --- Server du module ---
pageTempComparaisonServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Initialisation des pays ---
    observe({
      updateSelectInput(session, "country_compare",
                        choices = sort(unique(dataset$REF_AREA_LABEL)),
                        selected = c("Estonia", "Finland"))
    })
    
    # --- Mise à jour des indicateurs selon les pays ---
    observeEvent(input$country_compare, {
      req(input$country_compare)
      
      indicator_lists <- lapply(input$country_compare, function(p) {
        dataset %>%
          filter(REF_AREA_LABEL == p, !is.na(value)) %>%
          pull(INDICATOR_LABEL) %>%
          unique()
      })
      
      common_indicators <- sort(Reduce(intersect, indicator_lists))
      updateSelectInput(session, "indicator_compare", choices = common_indicators)
    })
    
    # --- Mise à jour des âges selon pays et indicateur ---
    observeEvent(list(input$country_compare, input$indicator_compare), {
      req(input$country_compare, input$indicator_compare)
      
      age_country_map <- dataset %>%
        filter(REF_AREA_LABEL %in% input$country_compare,
               INDICATOR_LABEL %in% input$indicator_compare) %>%
        select(REF_AREA_LABEL, AGE_LABEL) %>%
        distinct()
      
      age_grouped <- age_country_map %>%
        group_by(AGE_LABEL) %>%
        summarise(pays = paste(sort(unique(REF_AREA_LABEL)), collapse = ", ")) %>%
        mutate(age_country = paste0(AGE_LABEL, " (", pays, ")")) %>%
        pull(age_country) %>%
        sort()
      
      updateSelectInput(session, "age_compare", choices = age_grouped)
    })
    
    # --- Graphique principal ---
    output$graph_compare <- renderPlot({
      req(input$country_compare, input$indicator_compare, input$age_compare, input$sex_compare)
      
      selected_ages <- gsub(" \\(.*\\)$", "", input$age_compare)
      
      df <- dataset %>%
        filter(REF_AREA_LABEL %in% input$country_compare,
               INDICATOR_LABEL %in% input$indicator_compare,
               AGE_LABEL %in% selected_ages,
               SEX_LABEL %in% input$sex_compare) %>%
        na.omit() %>%
        mutate(year = as.factor(year))
      
      if (nrow(df) == 0) return(NULL)
      
      # Palette des pays
      country_palette <- RColorBrewer::brewer.pal(min(8, length(unique(df$REF_AREA_LABEL))), "Set2")
      names(country_palette) <- unique(df$REF_AREA_LABEL)
      
      # Styles de ligne et forme des points selon le sexe
      sex_linetypes <- c("Female" = "solid", "Male" = "longdash", "Total" = "twodash")
      sex_shapes <- c("Female" = 16, "Male" = 17, "Total" = 15)
      
      y_label <- ifelse(any(grepl("Percentage|Ratio", unique(df$UNIT_MEASURE_LABEL))),
                        "Valeur (%)", "Valeur")
      
      ggplot(df, aes(x = year, y = value,
                     group = interaction(REF_AREA_LABEL, SEX_LABEL),
                     color = REF_AREA_LABEL,
                     linetype = SEX_LABEL,
                     shape = SEX_LABEL)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        scale_color_manual(values = country_palette, name = "Pays") +
        scale_linetype_manual(values = sex_linetypes, name = "Sexe") +
        scale_shape_manual(values = sex_shapes, name = "Sexe") +
        labs(title = input$indicator_compare,
             subtitle = paste("Âge(s):", paste(input$age_compare, collapse = ", "),
                              "\nComparaison entre pays:", paste(input$country_compare, collapse = ", ")),
             x = "Année", y = y_label) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold"),
              plot.subtitle = element_text(size = 11, color = "gray30"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.text = element_text(size = 10))
    })
  })
}
