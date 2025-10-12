### Comparaison pays

## UI
pageTempComparaisonUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    fluidRow(
      column(width = 3,
             selectInput(ns("country_compare"), "Country (multiple selection)",
                         choices = NULL,
                         multiple = TRUE,
                         selected = c("Estonia", "Brazil"))),
      column(width = 3,
             selectInput(ns("indicator_compare"), 
                         "Indicateur", 
                         choices = NULL,
                         selected = "Literacy rate (%)")),
      column(width = 3,
             selectInput(ns("age_compare"), 
                         "Âge", 
                         choices = NULL, 
                         multiple = TRUE,
                         selected = "15 to 24 years old")),
      column(width = 3,
             selectInput(ns("sex_compare"), "Sex",
                         choices = c("Female", "Male", "Total"),
                         multiple = TRUE,
                         selected = c("Female", "Male", "Total")))
    ),
    # Breakdown
    fluidRow(
      column(width = 3, uiOutput(ns("comp_breakdown_ui"))),
      column(width = 3, uiOutput(ns("comp_sous_breakdown_ui")))
    ),
    br(),
    fluidRow(
      column(width = 12, plotOutput(ns("graph_compare")))
    )
  )
}

## Server
pageTempComparaisonServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Choix pays
    observe({
      updateSelectInput(session, "country_compare",
                        choices = sort(unique(dataset$REF_AREA_LABEL)),
                        selected = c("Estonia", "Brazil"))
    })
    
    # Choix indicateurs en fonction des pays
    observeEvent(input$country_compare, {
      req(input$country_compare)
      
      indicator_lists <- lapply(input$country_compare, function(p) {
        dataset |>
          filter(REF_AREA_LABEL == p, !is.na(value)) |>
          pull(INDICATOR_LABEL) |>
          unique()
      })
      
      common_indicators <- sort(Reduce(intersect, indicator_lists))
      updateSelectInput(session, "indicator_compare", choices = common_indicators, selected = "Literacy rate (%)")
    })
    
    # Breakdown
    output$comp_breakdown_ui <- renderUI({
      req(input$indicator_compare)
      df <- dataset |>
        filter(INDICATOR_LABEL == input$indicator_compare) |>
        filter(!is.na(COMP_BREAKDOWN_1_LABEL),
               COMP_BREAKDOWN_1_LABEL != "Not applicable") |>
        distinct(COMP_BREAKDOWN_1_LABEL)
      
      if (nrow(df) > 1) {
        selectInput(ns("comp_breakdown"),
                    "Breakdown",
                    choices = sort(unique(df$COMP_BREAKDOWN_1_LABEL)),
                    multiple = FALSE)
      }
    })
    
    # Sub Breakdown
    output$comp_sous_breakdown_ui <- renderUI({
      req(input$indicator_compare, input$comp_breakdown)
      df <- dataset |>
        filter(INDICATOR_LABEL == input$indicator_compare,
               COMP_BREAKDOWN_1_LABEL == input$comp_breakdown) |>
        filter(!is.na(COMP_BREAKDOWN_2_LABEL),
               COMP_BREAKDOWN_2_LABEL != "Not applicable") |>
        distinct(COMP_BREAKDOWN_2_LABEL)
      
      if (nrow(df) > 1) {
        selectInput(ns("comp_sous_breakdown"),
                    "Sub-breakdown",
                    choices = sort(unique(df$COMP_BREAKDOWN_2_LABEL)),
                    multiple = FALSE)
      }
    })
    
    # Choix age en fonction pays et indicateurs
    observeEvent(list(input$country_compare, input$indicator_compare), {
      req(input$country_compare, input$indicator_compare)
      
      age_country_map <- dataset |>
        filter(REF_AREA_LABEL %in% input$country_compare,
               INDICATOR_LABEL %in% input$indicator_compare) |>
        select(REF_AREA_LABEL, AGE_LABEL) |>
        distinct()
      
      age_grouped <- age_country_map |>
        group_by(AGE_LABEL) |>
        summarise(pays = paste(sort(unique(REF_AREA_LABEL)), collapse = ", ")) |>
        mutate(age_country = paste0(AGE_LABEL, " (", pays, ")")) |>
        pull(age_country) |>
        sort()
      
      updateSelectInput(session, "age_compare", choices = age_grouped, selected = "15 to 24 years old (Brazil, Estonia)")
    })
    
    # Graph
    output$graph_compare <- renderPlot({
      req(input$country_compare, input$indicator_compare, input$age_compare, input$sex_compare)
      
      selected_ages <- gsub(" \\(.*\\)$", "", input$age_compare)
      
      df <- dataset |>
        filter(REF_AREA_LABEL %in% input$country_compare,
               INDICATOR_LABEL %in% input$indicator_compare,
               AGE_LABEL %in% selected_ages,
               SEX_LABEL %in% input$sex_compare)
      
      # Filtre Breakdown
      if (!is.null(input$comp_breakdown))
        df <- df |> filter(COMP_BREAKDOWN_1_LABEL == input$comp_breakdown)
      
      if (!is.null(input$comp_sous_breakdown))
        df <- df |> filter(COMP_BREAKDOWN_2_LABEL == input$comp_sous_breakdown)
      
      df <- df |> na.omit() |> mutate(year = as.factor(year))
      if (nrow(df) == 0) return(NULL)
      
      # Palette de couleurs (la dynamique marche pas)
      palette_6 <- c("#FFE100", "#F58442", "#D96C81", "#CB3452", "#A2BDF4", "#BFB74C")
      country_colors <- rep(palette_6, length.out = length(unique(df$REF_AREA_LABEL)))
      names(country_colors) <- unique(df$REF_AREA_LABEL)
      country_palette <- country_colors
      
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
        scale_color_manual(values = country_palette, name = "Country") +
        scale_linetype_manual(values = sex_linetypes, name = "Sex") +
        scale_shape_manual(values = sex_shapes, name = "Sex") +
        labs(title = input$indicator_compare,
             subtitle = paste(
               "Âge(s):", paste(input$age_compare, collapse = ", "),
               if (!is.null(input$comp_breakdown)) paste0("\nBreakdown: ", input$comp_breakdown) else "",
               if (!is.null(input$comp_sous_breakdown)) paste0(" / ", input$comp_sous_breakdown) else "",
               "\nComparison between countries:", paste(input$country_compare, collapse = ", ")
             ),
             x = "year", y = y_label) +
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
