##################################UI########################

PageCarteThresholdUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
    .irs--shiny .irs-bar {
      background: #A2BDF4 !important;
      border-top: 1px solid #A2BDF4 !important;
      border-bottom: 1px solid #A2BDF4 !important;
      height: 8px !important;
    }
    
    .irs--shiny .irs-from, 
    .irs--shiny .irs-to, 
    .irs--shiny .irs-single {
      background: #A2BDF4 !important;
    }
  "))
    ),
    titlePanel("Filter countries by indicator threshold"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        # Mode selection
        radioButtons(ns("mode"), 
                     "Mode:",
                     choices = c("Single indicator" = "single",
                                 "Multiple indicators" = "multiple"),
                     selected = "single"),
        
        hr(),
        
        # Single indicator mode UI
        conditionalPanel(
          condition = "input.mode == 'single'",
          ns = ns,
          
          selectInput(ns("indicator"), 
                      "Select an indicator:",
                      choices = NULL),
          
          uiOutput(ns("comp_breakdown_ui")),
          
          uiOutput(ns("comp_sous_breakdown_ui")),
          
          uiOutput(ns("c_age")), 
          
          selectInput(ns("sex"), 
                      "Select a gender:",
                      choices = NULL),
          
          hr(),
          
          uiOutput(ns("year_display")),
          
          hr(),
          
          selectInput(ns("threshold_type"),
                       "Threshold type:",
                       choices = c("Between two values" = "between",
                                   "Above a value(>)" = "above",
                                   "Below a value(<)" = "below"),
                       selected = "between"),
          
          conditionalPanel(
            condition = "input.threshold_type == 'between'",
            ns = ns,
            uiOutput(ns("threshold_range_ui"))
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'above'",
            ns = ns,
            uiOutput(ns("threshold_above_ui"))
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'below'",
            ns = ns,
            uiOutput(ns("threshold_below_ui"))
          )
        ),
        
        # Multiple indicators mode UI
        conditionalPanel(
          condition = "input.mode == 'multiple'",
          ns = ns,
          
          h4("Indicators Selection"),
          
          uiOutput(ns("multi_indicators_ui")),
          
          actionButton(ns("add_indicator"), 
                       "Add Indicator", 
                       icon = icon("plus"),
                       class = "btn-primary btn-sm"),
          
          hr(),
          
          helpText("Countries will be highlighted if they meet ALL selected thresholds")
        ),
        
        hr(),
        
        h4("Statistics"),
        verbatimTextOutput(ns("stats")),
        
        hr(),
        
        checkboxInput(ns("show_data_preview"), 
                      "Show filtered countries list", 
                      value = FALSE),
        
        hr(),
        helpText("Each country displays its most recent available year.")
      ),
      
      mainPanel(
        width = 9,
        leafletOutput(ns("map"), height = "600px"),
        br(),
        conditionalPanel(
          condition = "input.show_data_preview == true",
          ns = ns,
          h4("Filtered countries"),
          tableOutput(ns("filtered_data"))
        )
      )
    )
  )
}

#################################SERVER###########################

pageCarteThresholdServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store multiple indicators
    multi_indicators <- reactiveVal(list())
    
    # Counter for unique IDs
    indicator_counter <- reactiveVal(0)
    
    # Charger les données géographiques
    c_world <- reactive({
      ne_countries(scale = "medium", returnclass = "sf")
    })
    
    # Initialize indicator choices
    observe({
      if (!exists("gender")) return(NULL)
      
      c_indicators <- gender %>%
        select(INDICATOR, INDICATOR_LABEL) %>%
        distinct() %>%
        arrange(INDICATOR_LABEL)
      
      c_choices_ind <- setNames(c_indicators$INDICATOR, c_indicators$INDICATOR_LABEL)
      updateSelectInput(session, "indicator", choices = c_choices_ind)
    })
    
    # Dynamic UI for multiple indicators
    output$multi_indicators_ui <- renderUI({
      ns <- session$ns
      indicators_list <- multi_indicators()
      
      if (length(indicators_list) == 0) {
        return(helpText("Click 'Add Indicator' to start"))
      }
      
      lapply(seq_along(indicators_list), function(i) {
        ind <- indicators_list[[i]]
        ind_id <- ind$id
        
        tagList(
          div(
            style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px; background-color: #f9f9f9;",
            
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
              h5(paste("Indicator", i), style = "margin: 0;"),
              actionButton(ns(paste0("remove_", ind_id)), 
                           "Remove", 
                           icon = icon("trash"),
                           class = "btn-danger btn-xs")
            ),
            
            selectInput(ns(paste0("indicator_", ind_id)),
                        "Indicator:",
                        choices = get_indicator_choices(),
                        selected = ind$indicator),
            
            uiOutput(ns(paste0("comp_breakdown_ui_", ind_id))),
            
            uiOutput(ns(paste0("comp_sous_breakdown_ui_", ind_id))),
            
            uiOutput(ns(paste0("age_ui_", ind_id))),
            
            selectInput(ns(paste0("sex_", ind_id)),
                        "Gender:",
                        choices = NULL),
            
            radioButtons(ns(paste0("threshold_type_", ind_id)),
                         "Threshold type:",
                         choices = c("Between" = "between",
                                     "Above" = "above",
                                     "Below" = "below"),
                         selected = "between",
                         inline = TRUE),
            
            uiOutput(ns(paste0("threshold_ui_", ind_id)))
          )
        )
      })
    })
    
    # Helper function to get indicator choices
    get_indicator_choices <- function() {
      if (!exists("gender")) return(NULL)
      
      c_indicators <- gender %>%
        select(INDICATOR, INDICATOR_LABEL) %>%
        distinct() %>%
        arrange(INDICATOR_LABEL)
      
      setNames(c_indicators$INDICATOR, c_indicators$INDICATOR_LABEL)
    }
    
    # Add new indicator
    observeEvent(input$add_indicator, {
      new_id <- indicator_counter() + 1
      indicator_counter(new_id)
      
      current_list <- multi_indicators()
      
      # Get first available indicator
      all_indicators <- get_indicator_choices()
      first_indicator <- if (length(all_indicators) > 0) all_indicators[1] else NULL
      
      new_indicator <- list(
        id = new_id,
        indicator = first_indicator,
        threshold_type = "between"
      )
      
      multi_indicators(c(current_list, list(new_indicator)))
    })
    
    # Remove indicator (dynamic observers)
    observe({
      indicators_list <- multi_indicators()
      
      lapply(indicators_list, function(ind) {
        ind_id <- ind$id
        button_id <- paste0("remove_", ind_id)
        
        observeEvent(input[[button_id]], {
          current_list <- multi_indicators()
          updated_list <- Filter(function(x) x$id != ind_id, current_list)
          multi_indicators(updated_list)
        }, ignoreInit = TRUE)
      })
    })
    
    # Dynamic UI for each indicator in multiple mode
    observe({
      indicators_list <- multi_indicators()
      
      lapply(indicators_list, function(ind) {
        ind_id <- ind$id
        
        # Age UI
        output[[paste0("age_ui_", ind_id)]] <- renderUI({
          ns <- session$ns
          indicator_val <- input[[paste0("indicator_", ind_id)]]
          if (is.null(indicator_val)) return(NULL)
          
          c_ages <- gender %>%
            filter(INDICATOR == indicator_val) %>%
            select(AGE, AGE_LABEL) %>%
            distinct() %>%
            arrange(AGE_LABEL)
          
          if (nrow(c_ages) > 1) {
            selectInput(ns(paste0("age_", ind_id)),
                        "Age:",
                        choices = setNames(c_ages$AGE, c_ages$AGE_LABEL))
          }
        })
        
        # Breakdown UI
        output[[paste0("comp_breakdown_ui_", ind_id)]] <- renderUI({
          ns <- session$ns
          indicator_val <- input[[paste0("indicator_", ind_id)]]
          if (is.null(indicator_val)) return(NULL)
          
          age_val <- input[[paste0("age_", ind_id)]]
          if (is.null(age_val)) {
            age_val <- gender %>%
              filter(INDICATOR == indicator_val) %>%
              pull(AGE) %>%
              first()
          }
          
          c_options <- gender %>%
            filter(
              INDICATOR == indicator_val,
              AGE == age_val,
              COMP_BREAKDOWN_1_LABEL != "Not Applicable",
              !is.na(COMP_BREAKDOWN_1_LABEL)
            ) %>%
            pull(COMP_BREAKDOWN_1_LABEL) %>%
            unique() %>%
            sort()
          
          if (length(c_options) > 0) {
            selectInput(ns(paste0("comp_breakdown_", ind_id)),
                        "Category:",
                        choices = c_options)
          }
        })
        
        # Sub-breakdown UI
        output[[paste0("comp_sous_breakdown_ui_", ind_id)]] <- renderUI({
          ns <- session$ns
          indicator_val <- input[[paste0("indicator_", ind_id)]]
          comp_val <- input[[paste0("comp_breakdown_", ind_id)]]
          if (is.null(indicator_val)) return(NULL)
          
          age_val <- input[[paste0("age_", ind_id)]]
          if (is.null(age_val)) {
            age_val <- gender %>%
              filter(INDICATOR == indicator_val) %>%
              pull(AGE) %>%
              first()
          }
          
          data_filtered <- gender %>%
            filter(
              INDICATOR == indicator_val,
              AGE == age_val,
              !is.na(COMP_BREAKDOWN_2_LABEL),
              COMP_BREAKDOWN_2_LABEL != "Not Applicable"
            )
          
          if (!is.null(comp_val)) {
            data_filtered <- data_filtered %>%
              filter(COMP_BREAKDOWN_1_LABEL == comp_val)
          }
          
          c_options <- data_filtered %>%
            pull(COMP_BREAKDOWN_2_LABEL) %>%
            unique() %>%
            sort()
          
          if (length(c_options) > 0) {
            selectInput(ns(paste0("comp_sous_breakdown_", ind_id)),
                        "Sub-category:",
                        choices = c_options)
          }
        })
        
        # Sex choices
        observe({
          indicator_val <- input[[paste0("indicator_", ind_id)]]
          if (is.null(indicator_val)) return(NULL)
          
          c_sex_choices <- gender %>%
            filter(INDICATOR == indicator_val, !is.na(value)) %>%
            select(SEX, SEX_LABEL) %>%
            distinct() %>%
            arrange(SEX_LABEL)
          
          if (nrow(c_sex_choices) > 0) {
            c_sex_opts <- setNames(c_sex_choices$SEX, c_sex_choices$SEX_LABEL)
            updateSelectInput(session, paste0("sex_", ind_id), 
                              choices = c_sex_opts, 
                              selected = c_sex_opts[1])
          }
        })
        
        # Threshold UI
        output[[paste0("threshold_ui_", ind_id)]] <- renderUI({
          ns <- session$ns
          indicator_val <- input[[paste0("indicator_", ind_id)]]
          threshold_type <- input[[paste0("threshold_type_", ind_id)]]
          
          if (is.null(indicator_val) || is.null(threshold_type)) return(NULL)
          
          # Calculate value range
          c_range <- calculate_value_range_multi(ind_id)
          
          if (threshold_type == "between") {
            sliderInput(ns(paste0("threshold_range_", ind_id)),
                        "Value range:",
                        min = c_range$min,
                        max = c_range$max,
                        value = c(c_range$min, c_range$max),
                        step = 0.1)
          } else if (threshold_type == "above") {
            numericInput(ns(paste0("threshold_above_", ind_id)),
                         "Minimum value:",
                         value = c_range$max * 0.9,
                         min = c_range$min,
                         max = c_range$max,
                         step = 0.1)
          } else if (threshold_type == "below") {
            numericInput(ns(paste0("threshold_below_", ind_id)),
                         "Maximum value:",
                         value = c_range$min + (c_range$max - c_range$min) * 0.1,
                         min = c_range$min,
                         max = c_range$max,
                         step = 0.1)
          }
        })
      })
    })
    
    # Helper function to calculate value range for multi mode
    calculate_value_range_multi <- function(ind_id) {
      indicator_val <- input[[paste0("indicator_", ind_id)]]
      sex_val <- input[[paste0("sex_", ind_id)]]
      
      if (is.null(indicator_val) || is.null(sex_val)) {
        return(list(min = 0, max = 100))
      }
      
      age_val <- input[[paste0("age_", ind_id)]]
      if (is.null(age_val)) {
        age_val <- gender %>%
          filter(INDICATOR == indicator_val) %>%
          pull(AGE) %>%
          first()
      }
      
      c_filtered <- gender %>%
        filter(
          INDICATOR == indicator_val,
          SEX == sex_val,
          AGE == age_val,
          !is.na(value)
        )
      
      comp_val <- input[[paste0("comp_breakdown_", ind_id)]]
      if (!is.null(comp_val)) {
        c_filtered <- c_filtered %>%
          filter(COMP_BREAKDOWN_1_LABEL == comp_val)
      }
      
      sous_comp_val <- input[[paste0("comp_sous_breakdown_", ind_id)]]
      if (!is.null(sous_comp_val)) {
        c_filtered <- c_filtered %>%
          filter(COMP_BREAKDOWN_2_LABEL == sous_comp_val)
      }
      
      if (nrow(c_filtered) == 0) {
        return(list(min = 0, max = 100))
      }
      
      c_filtered <- c_filtered %>%
        group_by(REF_AREA) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()
      
      c_min <- floor(min(c_filtered$value, na.rm = TRUE))
      c_max <- ceiling(max(c_filtered$value, na.rm = TRUE))
      
      list(min = c_min, max = c_max)
    }
    
    # [SINGLE MODE CODE - keeping all your original single mode code]
    # Year display for single mode
    output$year_display <- renderUI({
      if (!exists("gender") || input$mode != "single") return(NULL)
      req(input$indicator)
      
      c_years <- gender %>%
        filter(INDICATOR == input$indicator, !is.na(value)) %>%
        pull(year) %>%
        unique() %>%
        sort(decreasing = TRUE)
      
      if (length(c_years) == 0) {
        helpText("No data available")
      } else {
        tags$div(
          tags$strong("Available years: "),
          tags$span(style = "color: #0066cc; font-size: 14px;", 
                    paste(c_years, collapse = ", "))
        )
      }
    })
    
    # Value range for single mode
    c_value_range <- reactive({
      if (!exists("gender") || input$mode != "single") return(list(min = 0, max = 100))
      req(input$indicator, input$sex)
      
      c_age_value <- if (is.null(input$age)) {
        gender %>%
          filter(INDICATOR == input$indicator) %>%
          pull(AGE) %>%
          first()
      } else {
        input$age
      }
      
      c_filtered <- gender %>%
        filter(
          INDICATOR == input$indicator,
          SEX == input$sex,
          AGE == c_age_value,
          !is.na(value)
        )
      
      if (!is.null(input$comp_breakdown)) {
        c_filtered <- c_filtered %>%
          filter(COMP_BREAKDOWN_1_LABEL == input$comp_breakdown)
      }
      
      if (!is.null(input$comp_sous_breakdown)) {
        c_filtered <- c_filtered %>%
          filter(COMP_BREAKDOWN_2_LABEL == input$comp_sous_breakdown)
      }
      
      if (nrow(c_filtered) == 0) {
        return(list(min = 0, max = 100))
      }
      
      c_filtered <- c_filtered %>%
        group_by(REF_AREA) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()
      
      c_min <- floor(min(c_filtered$value, na.rm = TRUE))
      c_max <- ceiling(max(c_filtered$value, na.rm = TRUE))
      
      list(min = c_min, max = c_max)
    })
    
    # Threshold UIs for single mode
    output$threshold_range_ui <- renderUI({
      if (input$mode != "single") return(NULL)
      ns <- session$ns
      c_range <- c_value_range()
      
      sliderInput(ns("threshold_range"),
                  "Value range:",
                  min = c_range$min,
                  max = c_range$max,
                  value = c(c_range$min, c_range$max),
                  step = 0.1)
    })
    
    output$threshold_above_ui <- renderUI({
      if (input$mode != "single") return(NULL)
      ns <- session$ns
      c_range <- c_value_range()
      
      numericInput(ns("threshold_above"),
                   "Minimum value:",
                   value = c_range$max * 0.9,
                   min = c_range$min,
                   max = c_range$max,
                   step = 0.1)
    })
    
    output$threshold_below_ui <- renderUI({
      if (input$mode != "single") return(NULL)
      ns <- session$ns
      c_range <- c_value_range()
      
      numericInput(ns("threshold_below"),
                   "Maximum value:",
                   value = c_range$min + (c_range$max - c_range$min) * 0.1,
                   min = c_range$min,
                   max = c_range$max,
                   step = 0.1)
    })
    
    # Update sex choices for single mode
    observe({
      if (!exists("gender") || input$mode != "single") return(NULL)
      req(input$indicator)
      
      c_filtered <- gender %>% 
        filter(INDICATOR == input$indicator, !is.na(value))
      
      if (!is.null(input$comp_breakdown)) {
        c_filtered <- c_filtered %>% 
          filter(COMP_BREAKDOWN_1_LABEL == input$comp_breakdown)
      }
      
      if (!is.null(input$comp_sous_breakdown)) {
        c_filtered <- c_filtered %>% 
          filter(COMP_BREAKDOWN_2_LABEL == input$comp_sous_breakdown)
      }
      
      c_sex_choices <- c_filtered %>%
        select(SEX, SEX_LABEL) %>%
        distinct() %>%
        arrange(SEX_LABEL)
      
      if (nrow(c_sex_choices) > 0) {
        c_sex_opts <- setNames(c_sex_choices$SEX, c_sex_choices$SEX_LABEL)
        updateSelectInput(session, "sex", choices = c_sex_opts, selected = c_sex_opts[1])
      }
    })
    
    # Breakdown UIs for single mode
    output$comp_breakdown_ui <- renderUI({
      if (input$mode != "single") return(NULL)
      req(input$indicator)
      ns <- session$ns
      
      c_age_value <- if (is.null(input$age)) {
        gender %>%
          filter(INDICATOR == input$indicator) %>%
          pull(AGE) %>%
          first()
      } else {
        input$age
      }
      
      c_options <- gender %>%
        filter(
          INDICATOR == input$indicator,
          AGE == c_age_value,
          COMP_BREAKDOWN_1_LABEL != "Not Applicable",
          !is.na(COMP_BREAKDOWN_1_LABEL)
        ) %>%
        pull(COMP_BREAKDOWN_1_LABEL) %>%
        unique() %>%
        sort()
      
      if (length(c_options) > 0) {
        selectInput(
          inputId = ns("comp_breakdown"),
          label = "Category:",
          choices = c_options,
          selected = c_options[1]
        )
      } else {
        NULL
      }
    })
    
    output$comp_sous_breakdown_ui <- renderUI({
      if (input$mode != "single") return(NULL)
      req(input$indicator)
      ns <- session$ns
      
      c_age_value <- if (is.null(input$age)) {
        gender %>%
          filter(INDICATOR == input$indicator) %>%
          pull(AGE) %>%
          first()
      } else {
        input$age
      }
      
      data_filtered <- gender %>%
        filter(
          INDICATOR == input$indicator,
          AGE == c_age_value,
          !is.na(COMP_BREAKDOWN_2_LABEL),
          COMP_BREAKDOWN_2_LABEL != "Not Applicable"
        )
      
      if (!is.null(input$comp_breakdown)) {
        data_filtered <- data_filtered %>%
          filter(COMP_BREAKDOWN_1_LABEL == input$comp_breakdown)
      }
      
      c_options <- data_filtered %>%
        pull(COMP_BREAKDOWN_2_LABEL) %>%
        unique() %>%
        sort()
      
      if (length(c_options) == 0) {
        return(NULL)
      }
      
      selectInput(
        inputId = ns("comp_sous_breakdown"),
        label = "Sub-category:",
        choices = c_options,
        selected = c_options[1]
      )
    })
    
    # Filtered data - SINGLE MODE
    c_filtered_data_single <- reactive({
      if (!exists("gender") || input$mode != "single") return(NULL)
      req(input$indicator, input$sex, input$threshold_type)
      
      c_age_value <- if (is.null(input$age)) {
        gender %>%
          filter(INDICATOR == input$indicator) %>%
          pull(AGE) %>%
          first()
      } else {
        input$age
      }
      
      c_base_data <- gender %>%
        filter(
          INDICATOR == input$indicator,
          SEX == input$sex,
          AGE == c_age_value,
          !is.na(value)
        )
      
      if (!is.null(input$comp_breakdown)) {
        c_base_data <- c_base_data %>%
          filter(COMP_BREAKDOWN_1_LABEL == input$comp_breakdown)
      }
      
      if (!is.null(input$comp_sous_breakdown)) {
        c_base_data <- c_base_data %>%
          filter(COMP_BREAKDOWN_2_LABEL == input$comp_sous_breakdown)
      }
      
      if (nrow(c_base_data) == 0) {
        return(NULL)
      }
      
      c_base_data <- c_base_data %>%
        group_by(REF_AREA) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()
      
      if (nrow(c_base_data) == 0) {
        return(NULL)
      }
      
      if (input$threshold_type == "between") {
        req(input$threshold_range)
        c_result <- c_base_data %>%
          mutate(meets_threshold = value >= input$threshold_range[1] & 
                   value <= input$threshold_range[2])
      } else if (input$threshold_type == "above") {
        req(input$threshold_above)
        c_result <- c_base_data %>%
          mutate(meets_threshold = value >= input$threshold_above)
      } else if (input$threshold_type == "below") {
        req(input$threshold_below)
        c_result <- c_base_data %>%
          mutate(meets_threshold = value <= input$threshold_below)
      }
      
      c_result %>%
        select(REF_AREA, REF_AREA_LABEL, INDICATOR_LABEL, 
               SEX_LABEL, AGE_LABEL, year, value, meets_threshold)
    })
    
    # Filtered data - MULTIPLE MODE
    c_filtered_data_multi <- reactive({
      if (!exists("gender") || input$mode != "multiple") return(NULL)
      
      indicators_list <- multi_indicators()
      if (length(indicators_list) == 0) return(NULL)
      
      # Get all countries from world data
      all_countries <- c_world() %>%
        st_drop_geometry() %>%
        select(iso_a3, name) %>%
        rename(REF_AREA = iso_a3, REF_AREA_LABEL = name)
      
      # For each indicator, get countries that meet threshold
      countries_by_indicator <- lapply(indicators_list, function(ind) {
        ind_id <- ind$id
        indicator_val <- input[[paste0("indicator_", ind_id)]]
        sex_val <- input[[paste0("sex_", ind_id)]]
        threshold_type <- input[[paste0("threshold_type_", ind_id)]]
        
        if (is.null(indicator_val) || is.null(sex_val) || is.null(threshold_type)) {
          return(NULL)
        }
        
        age_val <- input[[paste0("age_", ind_id)]]
        if (is.null(age_val)) {
          age_val <- gender %>%
            filter(INDICATOR == indicator_val) %>%
            pull(AGE) %>%
            first()
        }
        
        c_data <- gender %>%
          filter(
            INDICATOR == indicator_val,
            SEX == sex_val,
            AGE == age_val,
            !is.na(value)
          )
        
        comp_val <- input[[paste0("comp_breakdown_", ind_id)]]
        if (!is.null(comp_val)) {
          c_data <- c_data %>%
            filter(COMP_BREAKDOWN_1_LABEL == comp_val)
        }
        
        sous_comp_val <- input[[paste0("comp_sous_breakdown_", ind_id)]]
        if (!is.null(sous_comp_val)) {
          c_data <- c_data %>%
            filter(COMP_BREAKDOWN_2_LABEL == sous_comp_val)
        }
        
        if (nrow(c_data) == 0) return(NULL)
        
        c_data <- c_data %>%
          group_by(REF_AREA) %>%
          filter(year == max(year, na.rm = TRUE)) %>%
          slice(1) %>%
          ungroup()
        
        # Apply threshold
        if (threshold_type == "between") {
          threshold_val <- input[[paste0("threshold_range_", ind_id)]]
          if (is.null(threshold_val)) return(NULL)
          c_data <- c_data %>%
            filter(value >= threshold_val[1] & value <= threshold_val[2])
        } else if (threshold_type == "above") {
          threshold_val <- input[[paste0("threshold_above_", ind_id)]]
          if (is.null(threshold_val)) return(NULL)
          c_data <- c_data %>%
            filter(value >= threshold_val)
        } else if (threshold_type == "below") {
          threshold_val <- input[[paste0("threshold_below_", ind_id)]]
          if (is.null(threshold_val)) return(NULL)
          c_data <- c_data %>%
            filter(value <= threshold_val)
        }
        
        c_data %>%
          select(REF_AREA) %>%
          distinct()
      })
      
      # Remove NULL results
      countries_by_indicator <- countries_by_indicator[!sapply(countries_by_indicator, is.null)]
      
      if (length(countries_by_indicator) == 0) return(NULL)
      
      # Find countries that appear in ALL indicators (intersection)
      meeting_countries <- Reduce(function(x, y) {
        inner_join(x, y, by = "REF_AREA")
      }, countries_by_indicator)
      
      if (is.null(meeting_countries) || nrow(meeting_countries) == 0) return(NULL)
      
      # Join with country names
      result <- all_countries %>%
        inner_join(meeting_countries, by = "REF_AREA") %>%
        mutate(meets_threshold = TRUE)
      
      result
    })
    
    # Combined filtered data reactive
    c_filtered_data <- reactive({
      if (input$mode == "single") {
        c_filtered_data_single()
      } else {
        c_filtered_data_multi()
      }
    })
    
    # Map data
    c_map_data <- reactive({
      c_data <- c_filtered_data()
      if (is.null(c_data) || nrow(c_data) == 0) return(NULL)
      
      req(c_world())
      
      if (input$mode == "single") {
        c_world() %>%
          left_join(
            c_data %>% select(REF_AREA, value, meets_threshold, year),
            by = c("iso_a3" = "REF_AREA")
          )
      } else {
        # Multiple mode: uniform color for countries meeting all thresholds
        c_world() %>%
          left_join(
            c_data %>% select(REF_AREA, meets_threshold),
            by = c("iso_a3" = "REF_AREA")
          ) %>%
          mutate(meets_threshold = ifelse(is.na(meets_threshold), FALSE, meets_threshold))
      }
    })
    
    # Create legend HTML
    c_create_legend_html <- function(c_pal, c_values, c_sex_input, c_indicator_code) {
      c_vals <- c_values[!is.na(c_values)]
      c_vals <- as.numeric(c_vals)
      c_vals <- c_vals[!is.na(c_vals)]
      
      if (length(c_vals) == 0) {
        return('<div class="info legend leaflet-control" style="background: white; padding: 6px 8px;">No data</div>')
      }
      
      c_min_val <- min(c_vals, na.rm = TRUE)
      c_max_val <- max(c_vals, na.rm = TRUE)
      
      c_indicator_label <- gender %>%
        filter(INDICATOR == c_indicator_code) %>%
        pull(INDICATOR_LABEL) %>%
        first()
      
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
      <span>No data</span>
    </div>
  </div>', c_legend_title, c_format_max, c_format_min)
    }
    
    # Create legend for multiple mode
    c_create_legend_multi <- function() {
      return('<div class="info legend leaflet-control" style="background: white; padding: 6px 8px; border: 2px solid rgba(0,0,0,0.2); border-radius: 5px;">
        <div style="font-weight: bold; margin-bottom: 8px;">Countries Status</div>
        <div style="margin-bottom: 5px;">
          <i style="background: #F58442; width: 20px; height: 18px; float: left; margin-right: 8px; opacity: 0.7;"></i>
          <span>Meet ALL thresholds</span>
        </div>
        <div style="clear: both;">
          <i style="background: #E0E0E0; width: 20px; height: 18px; float: left; margin-right: 8px; opacity: 0.7;"></i>
          <span>Do not meet all thresholds</span>
        </div>
      </div>')
    }
    
    # Render map
    output$map <- renderLeaflet({
      c_data <- c_map_data()
      
      if (is.null(c_data)) {
        return(leaflet() %>%
                 addProviderTiles(providers$CartoDB.Positron) %>%
                 addControl(
                   html = "<div style='background: white; padding: 10px;'>No data available</div>",
                   position = "topright"
                 ))
      }
      
      if (input$mode == "single") {
        # SINGLE MODE MAP
        req(input$indicator)
        
        c_indicator_name <- gender %>%
          filter(INDICATOR == input$indicator) %>%
          pull(INDICATOR_LABEL) %>%
          first()
        
        c_sex_label <- gender %>%
          filter(SEX == input$sex) %>%
          pull(SEX_LABEL) %>%
          first()
        
        c_years <- c_data %>%
          filter(!is.na(year)) %>%
          pull(year)
        
        if (length(c_years) > 0) {
          c_min_year <- min(c_years)
          c_max_year <- max(c_years)
          
          if (c_min_year == c_max_year) {
            c_year_text <- paste0("in ", c_max_year)
          } else {
            c_year_text <- paste0("(", c_min_year, "-", c_max_year, " - most recent per country)")
          }
        } else {
          c_year_text <- ""
        }
        
        if (input$threshold_type == "between") {
          c_threshold_text <- sprintf("between %.1f and %.1f", 
                                      input$threshold_range[1], 
                                      input$threshold_range[2])
        } else if (input$threshold_type == "above") {
          c_threshold_text <- sprintf("above %.1f", input$threshold_above)
        } else {
          c_threshold_text <- sprintf("below %.1f", input$threshold_below)
        }
        
        c_comp_part <- if (!is.null(input$comp_breakdown)) {
          paste0(input$comp_breakdown, " - ")
        } else {
          ""
        }
        
        c_sous_comp_part <- if (!is.null(input$comp_sous_breakdown)) {
          paste0(input$comp_sous_breakdown, " - ")
        } else {
          ""
        }
        
        c_age_part <- if (!is.null(input$age)) {
          c_age_label <- gender %>%
            filter(AGE == input$age) %>%
            pull(AGE_LABEL) %>%
            first()
          
          if (!is.na(c_age_label) && c_age_label != "All age ranges or no breakdown by age") {
            paste0(" (", c_age_label, ")")
          } else {
            ""
          }
        } else {
          ""
        }
        
        if (c_comp_part != "" && grepl("^Cause of death", c_comp_part)) {
          c_titre <- paste0(c_comp_part, c_sous_comp_part," for " ,c_sex_label, c_age_part, " ", c_year_text, " (", c_threshold_text, ")")
        } else {
          c_titre <- paste0(c_indicator_name," ", c_comp_part, " ",c_sous_comp_part," for ", c_sex_label, c_age_part, " ", c_year_text, " (", c_threshold_text, ")")
        }
        
        c_titre_html <- tags$div(
          style = "background: white; 
                   padding: 12px 15px; 
                   text-align: center;
                   border-bottom: 2px solid #ddd; 
                   font-size: 16px; 
                   white-space: normal;
                   z-index: 1000;",
          c_titre
        )
        
        c_labels <- sprintf(
          "<strong>%s</strong><br/>Value: %s<br/>Year: %s<br/>%s",
          c_data$name,
          ifelse(is.na(c_data$value), "No data", sprintf("%.2f", c_data$value)),
          ifelse(is.na(c_data$year), "", as.character(c_data$year)),
          ifelse(is.na(c_data$meets_threshold), "",
                 ifelse(c_data$meets_threshold, 
                        "<span style='color: green;'>Meets threshold</span>",
                        "<span style='color: red;'>Does not meet threshold</span>"))
        ) %>% lapply(htmltools::HTML)
        
        c_data <- c_data %>%
          mutate(display_value = ifelse(meets_threshold %in% TRUE, value, NA))
        
        c_pal <- colorNumeric(
          palette = rev(palette_carte_continue(100)),
          domain = c_data$display_value,
          na.color = "#E0E0E0"
        )
        
        c_fill_color <- ~c_pal(display_value)
        
        leaflet(c_data, options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addControl(
            html = as.character(c_titre_html),
            position = "topright"
          ) %>%
          addPolygons(
            fillColor = c_fill_color,
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
          ) %>%
          addControl(
            html = c_create_legend_html(c_pal, c_data$value, input$sex, input$indicator),
            position = "bottomright") %>% 
          addEasyprint(options = easyprintOptions(
            title = 'Download map',
            position = 'bottomleft',
            exportOnly = TRUE,
            sizeModes = c('CurrentSize', 'A4Landscape'),
            filename = gsub("[^A-Za-z0-9_-]", "_", c_titre)
          ))
        
      } else {
        # MULTIPLE MODE MAP
        indicators_list <- multi_indicators()
        
        # Build title
        c_titre <- paste0("Countries meeting ALL thresholds for ", length(indicators_list), " indicators")
        
        c_titre_html <- tags$div(
          style = "background: white; 
                   padding: 12px 15px; 
                   text-align: center;
                   border-bottom: 2px solid #ddd; 
                   font-size: 16px; 
                   white-space: normal;
                   z-index: 1000;",
          c_titre
        )
        
        c_labels <- sprintf(
          "<strong>%s</strong><br/>%s",
          c_data$name,
          ifelse(c_data$meets_threshold %in% TRUE, 
                 "<span style='color: green;'> Meets ALL thresholds</span>",
                 "<span style='color: #999;'>Does not meet all thresholds</span>")
        ) %>% lapply(htmltools::HTML)
        
        c_fill_color <- ifelse(c_data$meets_threshold %in% TRUE, "#F58442", "#E0E0E0")
        
        leaflet(c_data, options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addControl(
            html = as.character(c_titre_html),
            position = "topright"
          ) %>%
          addPolygons(
            fillColor = c_fill_color,
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
          ) %>%
          addControl(
            html = c_create_legend_multi(),
            position = "bottomright") %>% 
          addEasyprint(options = easyprintOptions(
            title = 'Download map',
            position = 'bottomleft',
            exportOnly = TRUE,
            sizeModes = c('CurrentSize', 'A4Landscape'),
            filename = "multiple_indicators_map"
          ))
      }
    })
    
    # Statistics
    output$stats <- renderPrint({
      c_data <- c_filtered_data()
      
      if (is.null(c_data) || nrow(c_data) == 0) {
        cat("No data available")
        return()
      }
      
      if (input$mode == "single") {
        c_meeting <- sum(c_data$meets_threshold, na.rm = TRUE)
        c_total <- nrow(c_data)
        c_pct <- round(c_meeting / c_total * 100, 1)
        
        cat("Total countries:", c_total, "\n")
        cat("Meeting threshold:", c_meeting, "(", c_pct, "%)\n")
        cat("Not meeting:", c_total - c_meeting, "\n\n")
        
        c_data_meeting <- c_data %>% filter(meets_threshold)
        
        if (nrow(c_data_meeting) > 0) {
          cat("--- Countries meeting threshold ---\n")
          cat("Min:", round(min(c_data_meeting$value, na.rm = TRUE), 2), "\n")
          cat("Max:", round(max(c_data_meeting$value, na.rm = TRUE), 2), "\n")
          cat("Mean:", round(mean(c_data_meeting$value, na.rm = TRUE), 2), "\n")
          cat("Median:", round(median(c_data_meeting$value, na.rm = TRUE), 2), "\n")
        }
      } else {
        # Multiple mode statistics
        indicators_list <- multi_indicators()
        c_meeting <- sum(c_data$meets_threshold, na.rm = TRUE)
        
        cat("Number of indicators selected:", length(indicators_list), "\n")
        cat("Countries meeting ALL thresholds:", c_meeting, "\n\n")
        
        if (c_meeting > 0) {
          cat("--- Countries meeting all thresholds ---\n")
          meeting_countries <- c_data %>% 
            filter(meets_threshold) %>%
            pull(REF_AREA_LABEL)
          cat(paste(meeting_countries, collapse = ", "))
        }
      }
    })
    
    # Data preview table
    output$filtered_data <- renderTable({
      c_data <- c_filtered_data()
      
      if (is.null(c_data) || nrow(c_data) == 0) {
        return(data.frame(Message = "No data available"))
      }
      
      if (input$mode == "single") {
        c_data %>%
          filter(meets_threshold) %>%
          arrange(desc(value)) %>%
          select(Country = REF_AREA_LABEL, Value = value, Year = year) %>%
          head(20)
      } else {
        c_data %>%
          filter(meets_threshold) %>%
          arrange(REF_AREA_LABEL) %>%
          select(Country = REF_AREA_LABEL) %>%
          head(50)
      }
    }, striped = TRUE, hover = TRUE)
    
    observeEvent(input$nav_page, {
      if (input$nav_page == "page2") {
        leafletProxy("map") %>% leaflet::invalidateSize()
      }
    })
  })
}