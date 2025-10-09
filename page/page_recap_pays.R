############
# Lib
############
library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(countrycode)
library(RCurl)
library(rvest)




############
#  UI
############
pageRecapPaysUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Country report by gendered variable"),
    
    wellPanel(
      fluidRow(
        column(4, selectInput(ns("select_pays"), "Select a Country :", choices = NULL)),
        column(4, selectInput(ns("variable"), "Select an Indicator :", choices = NULL)),
        column(4, uiOutput(ns("age_selector")))
      ),
      fluidRow(
        column(6, uiOutput(ns("comp_selector"))),
        column(6, uiOutput(ns("comp2_selector")))
      )
    ),
    
    fluidRow(
      column(width = 2, uiOutput(ns("flag"))),
      column(width = 10, uiOutput(ns("nom_pays")))
    ),
    
    br(),
    
    fluidRow(
      column(12, DTOutput(ns("table")))
    )
  )
}




############
# Serveur
############
pageRecapPaysServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # les machins pour la selection
    observe({
      updateSelectInput(session, "select_pays", 
                        choices = sort(unique(dataset$REF_AREA_LABEL)), 
                        selected = "Brazil")
    })
    
    observe({
      updateSelectInput(session, "variable", 
                        choices = sort(unique(dataset$INDICATOR_LABEL)), 
                        selected = "Cause of death (%)")
    })
    
    output$age_selector <- renderUI({
      req(input$variable, input$select_pays)
      age_vals <- dataset |>
        filter(REF_AREA_LABEL == input$select_pays,
               INDICATOR_LABEL == input$variable) |>
        distinct(AGE_LABEL) |>
        pull() |>
        na.omit()
      
      #filtre age
      age_filtre <- setdiff(age_vals, c("All age ranges or no breakdown by age", "All age ranges"))
      if(length(age_filtre) > 0) {
        selectInput(session$ns("age"), "Age group :", choices = age_filtre, selected = age_filtre[1])
      } else {
        all_age <- age_vals[grepl("All age", age_vals)]
        if(length(all_age) > 0) {
          selectInput(session$ns("age"), "Age group :", choices = all_age, selected = all_age[1])
        } else return(NULL)
      }
    })
    
    #filtre comP1
    output$comp_selector <- renderUI({
      req(input$variable, input$select_pays)
      
      comp_vals <- dataset |>
        filter(REF_AREA_LABEL == input$select_pays,
               INDICATOR_LABEL == input$variable) |>
        distinct(COMP_BREAKDOWN_1_LABEL) |>
        pull() |>
        na.omit()

      comp_vals <- comp_vals[comp_vals != "Not Applicable"]
      
      if(length(comp_vals) > 0) {
        selectInput(session$ns("comp"), "Component 1 :", choices = comp_vals, selected = comp_vals[1])
      } else {
        return(NULL)
      }
    })
    
    # filtre comp2
    output$comp2_selector <- renderUI({
      req(input$variable, input$select_pays, input$comp)
      
      comp2_vals <- dataset |>
        filter(
          REF_AREA_LABEL == input$select_pays,
          INDICATOR_LABEL == input$variable,
          COMP_BREAKDOWN_1_LABEL == input$comp
        ) |>
        distinct(COMP_BREAKDOWN_2_LABEL) |>
        pull() |>
        na.omit()
      comp2_vals <- comp2_vals[comp2_vals != "Not Applicable"]
      
      if(length(comp2_vals) > 0) {
        selectInput(session$ns("comp2"), "Component 2 :", 
                    choices = comp2_vals, selected = comp2_vals[1])
      } else {
        return(NULL)  # sinon on n'affiche rien
      }
    })
    
    
    # Nom du pays affiché
    output$nom_pays <- renderUI({
      req(input$select_pays)
      h1(input$select_pays, style = "margin-top: 35px; color: #333; text-align:center; font-size: 5em; font-weight: bold")
    })
    
    # et son petit drapeau (pas peu fier de ça)
    output$flag <- renderUI({
      req(input$select_pays)
      iso2 <- dataset |>
        filter(REF_AREA_LABEL == input$select_pays) |>
        pull(REF_AREA) |>
        unique() |>
        countrycode(origin = "iso3c", destination = "iso2c") |>
        tolower()
      
      url_flagcdn <- paste0("https://flagcdn.com/w320/", iso2, ".png")
      
      if(length(iso2) == 1 && RCurl::url.exists(url_flagcdn)) {
        tags$img(src = url_flagcdn,
                 width = "220px", height = "160px",
                 style = "border-radius: 8px; box-shadow: 0 0 5px rgba(0,0,0,0.3);")
      } else {
        wiki_page <- paste0("https://en.wikipedia.org/wiki/Flag_of_", gsub(" ", "_", input$select_pays))
        tryCatch({
          page <- read_html(wiki_page)
          img_url <- page |>
            html_node("table.infobox img") |>
            html_attr("src")
          if(!is.na(img_url)) {
            if(!grepl("^http", img_url)) img_url <- paste0("https:", img_url)
            tags$img(src = img_url, width = "120px", height = "80px",
                     style = "border-radius: 8px; box-shadow: 0 0 5px rgba(0,0,0,0.3);")
          } else tags$p("Drapeau non disponible", style = "color: gray;")
        }, error = function(e) { tags$p("Drapeau non disponible", style = "color: gray;") })
      }
    })
    
    # DF
    data_filtre <- reactive({
      req(input$select_pays, input$variable)
      df <- dataset |>
        filter(
          REF_AREA_LABEL == input$select_pays,
          INDICATOR_LABEL == input$variable,
          SEX_LABEL %in% c("Male", "Female", "Total")
        )
      
      # Filtre age
      if(!is.null(input$age)) df <- df |> filter(AGE_LABEL == input$age)
      else df <- df |> filter(AGE_LABEL %in% c("All age ranges or no breakdown by age", "All age ranges"))
      
      # Filtre component 1
      if(!is.null(input$comp)) df <- df |> filter(COMP_BREAKDOWN_1_LABEL == input$comp)
      else df <- df |> filter(is.na(COMP_BREAKDOWN_1_LABEL) | COMP_BREAKDOWN_1_LABEL == "Not Applicable")
      
      # Filtre component 2
      if(!is.null(input$comp2)) df <- df |> filter(COMP_BREAKDOWN_2_LABEL == input$comp2)
      
      df |>
        mutate(
          Indicateur_Gendered = paste(INDICATOR_LABEL, "-", SEX_LABEL),
          value = round(value, 2)
        ) |>
        select(Indicateur_Gendered, year, value) |>
        pivot_wider(names_from = year, values_from = value) |>
        select(Indicateur_Gendered, where(~any(!is.na(.))))
    })
    
    output$table <- renderDT({
      datatable(
        data_filtre(),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
  })
}

