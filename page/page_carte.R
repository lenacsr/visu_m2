##################################UI########################

PageCarteUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Browse through our gender-disaggregated indicators"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        selectInput(ns("indicator"), 
                    "Select an indicator:",
                    choices = NULL),
       
        uiOutput(ns("comp_breakdown_ui")),
        
        
        uiOutput(ns("comp_sous_breakdown_ui")),
        
        
        uiOutput(ns("c_age")), 
        
# Pour avoir l'explication du Femal/Male Ratio à coté de select a gender      
        div(
          tags$label(
            `for` = ns("sex"),
            "Select a gender: ",
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #A2BDF4; cursor: help;",
              title = "Select Female/Male Ratio to compare. Value > 1 = female advantage, < 1 = male advantage.",
              `data-toggle` = "tooltip",
              `data-placement` = "right"
            )
          ),
          selectInput(ns("sex"), 
                      label = NULL,
                      choices = NULL)
        ),
        hr(),
        selectInput(ns("year"), 
                    "Select a year:",
                    choices = NULL),
                
        hr(),
        
        numericInput(ns("window"), 
                    "Sliding window (years):",
                    min = 0,
                    max = 10,
                    value = 0),
        hr(),
        checkboxInput(ns("show_data_preview"), 
                      "Show a glimpse of the dataset", 
                      value = FALSE),
        
        hr(),
        h4("Statistics"),
        verbatimTextOutput(ns("stats")),
        
        hr(),
        helpText("(*)Select Female/Male Ratio to compare.
                 Value > 1 = female advantage, < 1 = male advantage."),
      ),
      
      mainPanel(
        width = 9,
        leafletOutput(ns("map"), height = "600px"),
        br(),
        #L'apercu des données est optionel il peut être validé ou non grace a une check box
        conditionalPanel(
          condition = "input.show_data_preview == true",
          ns = ns,  # Important pour que la condition fonctionne dans un module
          h4("Aperçu des données (10 premiers pays)"),
          tableOutput(ns("filtered_data"))
        )
      )
    )
  )
}

#################################SERVER###########################
pageCarteServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Charger les données géographiques
    c_world <- reactive({
      ne_countries(scale = "medium", returnclass = "sf")
    })
    
    # Initialiser les choix d'indicateurs
    observe({
      if (!exists("gender")) return(NULL)
      
      c_indicators <- gender |>
        select(INDICATOR, INDICATOR_LABEL) |>
        distinct() |>
        arrange(INDICATOR_LABEL)
      
      c_choices_ind <- setNames(c_indicators$INDICATOR, c_indicators$INDICATOR_LABEL)
      updateSelectInput(session, "indicator", choices = c_choices_ind)
    })
    
    # Mettre à jour les autres filtres en fonction de l'indicateur sélectionné
    observe({
      if (!exists("gender")) return(NULL)
      req(input$indicator)
      
      c_filtered <- gender |> filter(INDICATOR == input$indicator)
      
      # Années disponibles
      c_years <- c_filtered |>
        filter(!is.na(value)) |>
        pull(year) |>
        unique() |>
        sort(decreasing = TRUE)
      
      if (length(c_years) > 0) {
        updateSelectInput(session, "year", choices = c_years, selected = c_years[1])
      }
      
      # Sexe et Ratio
      c_sex_choices <- c_filtered |>
        select(SEX, SEX_LABEL) |>
        distinct() |>
        arrange(SEX_LABEL)
      
      if (nrow(c_sex_choices) > 0) {
        c_sex_opts <- setNames(c_sex_choices$SEX, c_sex_choices$SEX_LABEL)
        # et l'option Ratio 
        c_sex_opts <- c(c_sex_opts,"(*)Female/Male Ratio" = "RATIO")
        updateSelectInput(session, "sex", choices = c_sex_opts, selected = c_sex_opts[1])
      }
      
      # Âge
      c_age_choices <- c_filtered |>
        select(AGE, AGE_LABEL) |>
        distinct() |>
        arrange(AGE_LABEL)
      
      if (nrow(c_age_choices) > 0) {
        c_age_opts <- setNames(c_age_choices$AGE, c_age_choices$AGE_LABEL)
        updateSelectInput(session, "age", choices = c_age_opts, selected = c_age_opts[1])
      }
    })
    
    # Données filtrées avec fenêtre glissante
    c_filtered_data <- reactive({
      if (!exists("gender")) return(NULL)
      req(input$indicator, input$year, input$sex, input$window)
      
      # si on ne peut pas breakdown age
      c_age_value <- if (is.null(input$age)) {
        gender |>
          filter(INDICATOR == input$indicator) |>
          pull(AGE) |>
          first()
      } else {
        input$age
      }
      
      # si on ne peut pas breakdown en catégorie de l'indicateur
      c_comp_value <- if (is.null(input$comp_breakdown)) {
        # Si NULL, ne pas filtrer sur COMP_BREAKDOWN_1_LABEL
        NULL
      } else {
        input$comp_breakdown
      }
      
      # si on ne peut pas breakdown en sous catégorie de l'indicateur
      c_sous_comp_value <- if (is.null(input$comp_sous_breakdown)) {
        # Si NULL, ne pas filtrer sur COMP_BREAKDOWN_2_LABEL
        NULL
      } else {
        input$comp_sous_breakdown
      }
      
      c_target_year <- as.numeric(input$year)
      c_window_size <- input$window
      
      # Si ratio de genre activé (valeur "RATIO")
      if (input$sex == "RATIO") {
        c_female_code <- "F"
        c_male_code <- "M"
        
        # Filtrer pour Femmes
        c_data_female <- gender |>
          filter(
            INDICATOR == input$indicator,
            SEX == c_female_code,
            AGE == c_age_value,
            !is.na(value)
          )
        #pour updater la carte avec les catégories de l'indicateur
        if (!is.null(c_comp_value)) {
          c_data_female <- c_data_female |>
            filter(COMP_BREAKDOWN_1_LABEL == c_comp_value)
        }
        #pour updater la carte avec les sous catégories
        if (!is.null(c_sous_comp_value)) {
          c_data_female <- c_data_female |>
            filter(COMP_BREAKDOWN_2_LABEL == c_sous_comp_value)
        }
        # Filtrer pour Hommes
        c_data_male <- gender |>
          filter(
            INDICATOR == input$indicator,
            SEX == c_male_code,
            AGE == c_age_value,
            !is.na(value)
          )
        #pour updater la carte avec les catégories de l'indicateur
        if (!is.null(c_comp_value)) {
          c_data_male <- c_data_male |>
            filter(COMP_BREAKDOWN_1_LABEL == c_comp_value)
        }
        
        #pour updater la carte avec les sous catégories
        if (!is.null(c_sous_comp_value)) {
          c_data_male <- c_data_male |>
            filter(COMP_BREAKDOWN_2_LABEL == c_sous_comp_value)
        }
        if (nrow(c_data_female) == 0 || nrow(c_data_male) == 0) {
          showNotification("Pas de données disponibles pour calculer le ratio", type = "warning")
          return(NULL)
        }
        
        # Appliquer la fenêtre glissante pour les femmes
        if (c_window_size == 0) {
          c_female_filtered <- c_data_female |>
            filter(year == c_target_year) |>
            select(REF_AREA, REF_AREA_LABEL, year_exact_f = year, value_female = value)
        } else {
          c_female_filtered <- c_data_female |>
            filter(year >= (c_target_year - c_window_size) & 
                     year <= (c_target_year + c_window_size)) |>
            mutate(year_diff = abs(year - c_target_year)) |>
            group_by(REF_AREA) |>
            slice_min(year_diff, n = 1, with_ties = FALSE) |>
            ungroup() |>
            select(REF_AREA, REF_AREA_LABEL, year_exact_f = year, value_female = value)
        }
        
        # Appliquer la fenêtre glissante pour les hommes
        if (c_window_size == 0) {
          c_male_filtered <- c_data_male |>
            filter(year == c_target_year) |>
            select(REF_AREA, year_exact_m = year, value_male = value)
        } else {
          c_male_filtered <- c_data_male |>
            filter(year >= (c_target_year - c_window_size) & 
                     year <= (c_target_year + c_window_size)) |>
            mutate(year_diff = abs(year - c_target_year)) |>
            group_by(REF_AREA) |>
            slice_min(year_diff, n = 1, with_ties = FALSE) |>
            ungroup() |>
            select(REF_AREA, year_exact_m = year, value_male = value)
        }
        
        # Joindre et calculer le ratio
        c_result <- c_female_filtered |>
          inner_join(c_male_filtered, by = "REF_AREA") |>
          mutate(
            value = value_female / value_male,
            INDICATOR_LABEL = paste0(unique(c_data_female$INDICATOR_LABEL), " - Ratio F/M"),
            SEX_LABEL = "Ratio Femmes/Hommes",
            AGE_LABEL = unique(c_data_female$AGE_LABEL),
            year = c_target_year,
            year_exact = if (c_window_size == 0) {
              as.character(year_exact_f)
            } else {
              paste0("F:", year_exact_f, " M:", year_exact_m)
            }
          ) |>
          select(REF_AREA, REF_AREA_LABEL, INDICATOR_LABEL, 
                 SEX_LABEL, AGE_LABEL, year, year_exact, value, 
                 value_female, value_male)
        # En cas de prblm (filtre trop spécifique)
        if (nrow(c_result) == 0) {
          showNotification("Aucun pays avec données pour les deux sexes", type = "warning")
          return(NULL)
        }
        
        return(c_result)
        
      } else {
        # Mode normal (sexe spécifique) (sans ratio)
        c_base_data <- gender |>
          filter(
            INDICATOR == input$indicator,
            SEX == input$sex,
            AGE == c_age_value,
            !is.na(value)
          )
        if (!is.null(c_sous_comp_value)) {
          c_base_data <- c_base_data |>
            filter(COMP_BREAKDOWN_2_LABEL == c_sous_comp_value)
        }
        if (c_window_size == 0) {
          c_result <- c_base_data |>
            filter(year == c_target_year) |>
            select(REF_AREA, REF_AREA_LABEL, INDICATOR_LABEL, 
                   SEX_LABEL, AGE_LABEL, year, value, year_exact = year)
        } else {
          c_result <- c_base_data |>
            filter(year >= (c_target_year - c_window_size) & 
                     year <= (c_target_year + c_window_size)) |>
            mutate(year_diff = abs(year - c_target_year)) |>
            group_by(REF_AREA) |>
            slice_min(year_diff, n = 1, with_ties = FALSE) |>
            ungroup() |>
            select(REF_AREA, REF_AREA_LABEL, INDICATOR_LABEL, 
                   SEX_LABEL, AGE_LABEL, year_exact = year, value, year_diff) |>
            mutate(year = c_target_year)
        }
        
        return(c_result)
      }
    })
    # Pour les comp_label
    c_comp_options <- reactive({
      req(input$indicator)
      
      c_options <- gender |>
        filter(INDICATOR == input$indicator,
               COMP_BREAKDOWN_1_LABEL != "Not applicable",
               !is.na(COMP_BREAKDOWN_1_LABEL)) |>
        pull(COMP_BREAKDOWN_1_LABEL) |>
        unique() |>
        sort()
      
      return(c_options)
    })
    
    # Pour les sous_comp_label
    c_comp_sous_options <- reactive({
      req(input$indicator)
      
      c_options <- gender |>
        filter(INDICATOR == input$indicator,
               COMP_BREAKDOWN_2_LABEL != "Not applicable",
               !is.na(COMP_BREAKDOWN_2_LABEL)) |>
        pull(COMP_BREAKDOWN_2_LABEL) |>
        unique() |>
        sort()
      
      return(c_options)
    })
    
    output$c_age<- renderUI({
      req(input$indicator)
      
      ns <- session$ns  # Récupérer ns() depuis session
      
      # Récupérer les options disponibles pour cet indicateur
      c_options <- gender |>
        filter(
          INDICATOR == input$indicator,
          AGE_LABEL != "All age ranges or no breakdown by age",
          !is.na(AGE_LABEL)
        ) |>
        pull(AGE_LABEL) |>
        unique() |>
        sort()
      
      # Afficher le selectInput seulement s'il y a des options
      if (length(c_options) > 0) {
        selectInput(
          inputId = ns("age"),
          label = "Select an age group:",
          choices = c_options,
          selected = c_options[1]
        )
      } else {
        NULL  # Ne rien afficher si pas d'options
      }
    })
    
    output$comp_breakdown_ui <- renderUI({
      req(input$indicator)
      
      ns <- session$ns
      
      c_options <- gender |>
        filter(
          INDICATOR == input$indicator,
          COMP_BREAKDOWN_1_LABEL != "Not Applicable",
          !is.na(COMP_BREAKDOWN_1_LABEL)
        ) |>
        pull(COMP_BREAKDOWN_1_LABEL) |>
        unique() |>
        sort()
      
      if (length(c_options) > 0) {
        selectInput(
          inputId = ns("comp_breakdown"),
          label = "Breakdown:",
          choices = c_options,
          selected = c_options[1]
        )
      } else {
        NULL #Pareil ne pas l'afficher si on ne peut pas breakdown l'indicateur
      }
    })
    
    # Définition séparée pour comp_sous_breakdown_ui
    output$comp_sous_breakdown_ui <- renderUI({
      req(input$indicator)
      
      ns <- session$ns
      
      c_options <- gender |>
        filter(
          INDICATOR == input$indicator,
          COMP_BREAKDOWN_2_LABEL != "Not Applicable",
          !is.na(COMP_BREAKDOWN_2_LABEL)
        ) |>
        pull(COMP_BREAKDOWN_2_LABEL) |>
        unique() |>
        sort()
      
      if (length(c_options) > 0) {
        selectInput(
          inputId = ns("comp_sous_breakdown"),
          label = "2nd breakdown:",
          choices = c_options,
          selected = c_options[1]
        )
      } else {
        NULL # Ne rienf afficher si on ne peut pas breakdown l'indicateur
      }
    })
      
      
  
    # Joindre avec les données géographiques
    c_map_data <- reactive({
      c_data <- c_filtered_data()
      if (is.null(c_data) || nrow(c_data) == 0) return(NULL)
      
      req(c_world())
      
      if (input$sex == "RATIO") {
        c_world() |>
          left_join(
            c_data |> select(REF_AREA, value, year_exact, value_female, value_male),
            by = c("iso_a3" = "REF_AREA")
          )
      } else {
        c_world() |>
          left_join(
            c_data |> select(REF_AREA, value, year_exact),
            by = c("iso_a3" = "REF_AREA")
          )
      }
    })
    #Créer la légende (faut le faire avant la carte) (c'est juste pour mettre les na sous la barre et non à côté)
    c_create_legend_html <- function(c_pal, c_values, c_sex_input, c_indicator_code) {
      c_vals <- c_values[!is.na(c_values)]
      
      # Convertir en numérique si nécessaire
      c_vals <- as.numeric(c_vals)
      c_vals <- c_vals[!is.na(c_vals)]  # Retirer les NA créés par la conversion
      
      # Vérifier qu'il y a des valeurs
      if (length(c_vals) == 0) {
        return('<div class="info legend leaflet-control" style="background: white; padding: 6px 8px;">Aucune donnée</div>')
      }
      
      c_min_val <- min(c_vals, na.rm = TRUE)
      c_max_val <- max(c_vals, na.rm = TRUE)
      
      # Récupérer le label de l'indicateur
      c_indicator_label <- gender |>
        filter(INDICATOR == c_indicator_code) |>
        pull(INDICATOR_LABEL) |>
        first()
      
      #adaptation de la légende si c'est un ratio/ pourcentage ou une valeur
      if (c_sex_input == "RATIO") {
        c_legend_title <- "Ratio"
        c_format_max <- sprintf('%.2f', c_max_val)
        c_format_min <- sprintf('%.2f', c_min_val)
      } else {
        if (grepl("%", c_indicator_label)){
          c_legend_title <- "Percentage"
          c_format_max <- sprintf('%.0f%%', c_max_val)
          c_format_min <- sprintf('%.0f%%', c_min_val)
        } else {
          c_legend_title <- "Value"
          c_format_max <- sprintf('%.2f', c_max_val)
          c_format_min <- sprintf('%.2f', c_min_val)
        }
      }
      # Ca c'est la legende de la carte NE PAS Y TOUCHER
      sprintf('
  <div class="info legend leaflet-control" style="background: white; padding: 6px 8px; border: 2px solid rgba(0,0,0,0.2); border-radius: 5px;">
    <div style="font-weight: bold; margin-bottom: 8px;">%s</div>
    <div style="background: linear-gradient(to bottom, #CB3452,#D96C81,#F58442,#FFE100); width: 20px; height: 120px; margin-bottom: 5px;"></div>
    <div style="margin-top: -125px; margin-left: 28px; line-height: 24px;">
      <div>%s</div>
      <div style="margin-top: 93px;">%s</div>
    </div>
    <div style="margin-top: 10px; clear: both;">
      <i style="background: #E0E0E0; width: 20px; height: 18px; float: left; margin-right: 8px; opacity: 0.7;"></i>
      <span>Pas de données</span>
    </div>
  </div>', c_legend_title, c_format_max, c_format_min)
    }
    # Créer la carte
    output$map <- renderLeaflet({
      c_data <- c_map_data()
      
      # Vérification des données
      if (is.null(c_data)) {
        return(leaflet() |>
                 addProviderTiles(providers$CartoDB.Positron) |>
                 addControl(
                   html = "<div style='background: white; padding: 10px;'>Aucune donnée disponible</div>",
                   position = "topright"
                 ))
      }
      
      if (all(is.na(c_data$value))) {
        leaflet(c_data) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(
            fillColor = "#E0E0E0",
            weight = 1,
            opacity = 1,
            color = "white",
            fillOpacity = 0.7
          )
      } else {
        c_pal <- colorNumeric(
          palette = rev(palette_carte_continue(100)),
          domain = c_data$value,
          na.color = "#E0E0E0"
        )
        
        # Créer les labels pour le survol
        if (input$sex == "RATIO") {
          c_labels <- sprintf(
            "<strong>%s</strong><br/>Ratio F/M: %s<br/>Femmes: %s<br/>Hommes: %s<br/>Année: %s",
            c_data$name,
            ifelse(is.na(c_data$value), "Pas de données", sprintf("%.3f", c_data$value)),
            ifelse(is.na(c_data$value_female), "", sprintf("%.2f", c_data$value_female)),
            ifelse(is.na(c_data$value_male), "", sprintf("%.2f", c_data$value_male)),
            ifelse(is.na(c_data$year_exact), "", as.character(c_data$year_exact))
          ) |> lapply(htmltools::HTML)
        } else {
          c_labels <- sprintf(
            "<strong>%s</strong><br/>Valeur: %s<br/>Année: %s",
            c_data$name,
            ifelse(is.na(c_data$value), "Pas de données", sprintf("%.2f", c_data$value)),
            ifelse(is.na(c_data$year_exact), "", as.character(c_data$year_exact))
          ) |> lapply(htmltools::HTML)
        }
        
        # Créer le titre de la carte
        req(input$indicator, input$year)
        
        c_indicator_name <- gender |>
          filter(INDICATOR == input$indicator) |>
          pull(INDICATOR_LABEL) |>
          first()
        
        if (input$sex == "RATIO") {
          c_sex_part <- "Ratio Female/Male"
        } else {
          c_sex_label_data <- gender |>
            filter(SEX == input$sex) |>
            pull(SEX_LABEL)
          
          if (length(c_sex_label_data) > 0) {
            c_sex_label <- first(c_sex_label_data)
            c_sex_part <- ifelse(is.na(c_sex_label) || c_sex_label == "", "...", c_sex_label)
          } else {
            c_sex_part <- "..."
          }
        }
        
        c_year_part <- paste0("in ", input$year)
        
        if (input$window > 0) {
          c_window_part <- paste0(" with a sliding window of ", input$window, 
                                ifelse(input$window == 1, " year", " years"))
        } else {
          c_window_part <- ""
        }
        
        #Pour afficher la catégorie dans le titre si il y en a
        if (!is.null(input$comp_breakdown)) {
          c_comp_part <- paste0(input$comp_breakdown, " - ")
        } else {
          c_comp_part <- ""
        }
        
        #Pour afficher la tranche d'age dans le titre (s'il y en a)
        if (!is.null(input$age)) {
          c_age_label_data <- gender |>
            filter(AGE == input$age) |>
            pull(AGE_LABEL) |>
            first()
          
          if (!is.na(c_age_label_data) && c_age_label_data != "All age ranges or no breakdown by age") {
            c_age_part <- paste0(" (", c_age_label_data, ")")
          } else {
            c_age_part <- ""
          }
        } else {
          c_age_part <- ""
        }
        
        if (c_comp_part != "" && grepl("^Cause of death", c_comp_part)) {
          c_titre_sur_carte <- paste0(c_comp_part, " for ", c_sex_part, c_age_part, " ", c_year_part, c_window_part)
        } else {
          c_titre_sur_carte <- paste0(c_indicator_name, " - ", c_comp_part, " for ", c_sex_part, c_age_part, " ", c_year_part, c_window_part)
        }
        
        
        c_titre_html <- tags$div(
          style = "background: white; 
                 padding: 12px 15px; 
                 text-align: center;
                 border-bottom: 2px solid #ddd; 
                 font-size: 16px; 
                 white-space: normal;
                 z-index: 1000;",
          c_titre_sur_carte
        )
        
        # Créer un nom de fichier pour l'export
        c_titre_carte_png <- to_snake_case(c_titre_sur_carte)
        
        #LA CARTE
        c_map <- leaflet(c_data, options = leafletOptions(zoomControl = FALSE)) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addControl(
            html = as.character(c_titre_html),
            position = "topright",
            className = "map-title"
          ) |>
          addPolygons(
            fillColor = ~c_pal(value),
            weight = 1,
            opacity = 1,
            color = "white",
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = c_labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) |>
          addControl(
            html = c_create_legend_html(c_pal, c_data$value, input$sex, input$indicator),
            position = "bottomright"
          ) |>
          addEasyprint(options = easyprintOptions(
            title = 'Télécharger la carte',
            position = 'bottomleft',
            exportOnly = TRUE,
            sizeModes = c('CurrentSize', 'A4Landscape'),
            filename = c_titre_carte_png
          ))
      }
      
      return(c_map)
    })
    
    # Afficher les statistiques
    output$stats <- renderPrint({
      c_data <- c_filtered_data()
      
      if (is.null(c_data) || nrow(c_data) == 0) {
        cat("No data available for this selection")
        return()
      }
      
      if(nrow(c_data) > 0) {
        cat("Number of country:", nrow(c_data), "\n")
        cat("Minimum:", round(min(c_data$value, na.rm = TRUE), 2), "\n")
        cat("Maximum:", round(max(c_data$value, na.rm = TRUE), 2), "\n")
        cat("Mean:", round(mean(c_data$value, na.rm = TRUE), 2), "\n")
        cat("Median:", round(median(c_data$value, na.rm = TRUE), 2), "\n")
        
        if (input$sex == "RATIO") {
          cat("\n--- Gender analysis ---\n")
          c_above_1 <- sum(c_data$value > 1, na.rm = TRUE)
          c_below_1 <- sum(c_data$value < 1, na.rm = TRUE)
          c_equal_1 <- sum(abs(c_data$value - 1) < 0.01, na.rm = TRUE)
          cat("Female advantage (>1):", c_above_1, "country\n")
          cat("Male advantage (<1):", c_below_1, "country\n")
          cat("Equality (~1):", c_equal_1, "country\n")
        }
        
        if ("year_diff" %in% colnames(c_data) && input$window > 0) {
          cat("\n--- Sliding window ---\n")
          cat("Target year:", input$year, "\n")
          if (input$sex != "RATIO") {
            cat("year min-max:", 
                min(c_data$year_exact), "-", max(c_data$year_exact), "\n")
            cat("Average gap:", round(mean(c_data$year_diff), 1), "years\n")
          }
        }
      } else {
        cat("No data available for this selection")
      }
    })
    
    # Afficher un aperçu des données filtrées
    output$filtered_data <- renderTable({
      data <- c_filtered_data()
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame(Message = "No data available for this selection"))
      }
      head(data, 10)
    }, striped = TRUE, hover = TRUE)
  })
}