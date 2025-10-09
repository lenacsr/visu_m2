########################
#LIBRARIES
########################
# Shiny
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(fontawesome)
library(htmltools)
library(DT)

# Manip
library(dplyr)
library(tidyr)
library(tidyverse)
library(snakecase)

# Visu
library(ggplot2)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# WebScrapping
library(httr)
library(rvest)

########################
#IMPORT DU DATA FRAME
########################

gender <- read.csv("src/GENDER_STAT_CLEAN.csv", sep = ",")



########################
#FONCTIONS
########################

### Fenetre_glissante
fenetre_annee <- function(df, annee, n = 2) {
  df_filtre  <- df |>
    mutate(year = as.integer(as.character(year))) |> 
    filter(year >= (annee - n), year <= (annee + n))
  
  df_final <- df_filtre |>
    mutate(dist = abs(year - annee)) |>
    group_by(REF_AREA_LABEL, INDICATOR_LABEL, SEX_LABEL) |>
    slice_min(dist, with_ties = FALSE) |>
    ungroup()
  
  return(df_final)
}


### Fonction recup drapeau
get_flag_url <- function(iso2) {
  iso2 <- tolower(iso2)
  cdn_url <- paste0("https://flagcdn.com/w320/", iso2, ".png")
  
  res <- tryCatch(httr::HEAD(cdn_url), error = function(e) NULL)
  if (!is.null(res) && httr::status_code(res) == 200) {
    return(cdn_url)
  }
  return(NULL)
}




########################
# PAGES SOURCES
########################

source("page/page_tableau_comparaison.R")
source("page/page_recap_pays.R")
source("page/page_ACP.R")
source("page/page_visualisation.R")
source("page/page_comparaison.R")
source("page/page_statdesc.R")
source("page/page_carte.R")
source("page/page_carte_threshold.R")
source("page/page_accueil.R")


########################
# RESSOURCE POUR L'APP
########################

### Axes

indicateurs_axes <- list(
  "Éducation" = c(
    "Literacy rate (%)",
    "Children out of school, primary (Number)",
    "Expected Years of School",
    "Expected years of schooling"
  ),
  "Économie" = c(
    "Children in employment (% of children ages 7-14)",
    "Borrowed any money in the past year (% age 15+)",
    "Borrowed for education or school fees (% age 15+)",
    "Borrowed for health or medical purposes (% age 15+)",
    "Borrowed from a financial institution (% age 15+)",
    "Borrowed from a savings club (% age 15+)",
    "Borrowed from a store by buying on credit (% age 15+)",
    "Borrowed from family or friends (% age 15+)",
    "Unemployment (%)",
    "Unemployment by level of education (%)",
    "Youth illiterate population, 15-24 years (number)"
  ),
  "Santé" = c(
    "Age population, interpolated",
    "Belief that religion requires female genital mutilation (% who have heard about FGM)",
    "Condom use at last high-risk sex (% ages 15-49)",
    "Life expectancy at age 60 (years)",
    "Life expectancy at birth (years)",
    "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)",
    "Mortality rate, adult (per 1,000 adults)",
    "Mortality rate, infant (per 1,000 live births)",
    "Mortality rate, under-5 (per 1,000 live births)",
    "Suicide mortality rate (per 100,000 population)",
    "Survival Rate from Age 15-60",
    "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)"
  ),
  "Politique" = c(
    "Human Capital Index (HCI) (scale 0-1)",
    "Length of paid leave (calendar days)",
    "Mean age at first marriage",
    "Population (number)",
    "Rural population (%)",
    "Urban population (%)"
  )
)


### 

palette_choice <- reactiveVal("default")


palette_daltonian <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
palette_6 <- c("#FFE100","#F58442","#D96C81","#CB3452","#A2BDF4","#BFB74C")
palette_continue <- colorRampPalette(palette_6)

palette_carte <-  c("#CB3452","#D96C81","#F58442","#FFE100")
palette_carte_continue <- colorRampPalette(palette_carte)