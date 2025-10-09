########################
#LIBRARIES
########################
# UI / Shiny
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(fontawesome)
library(htmltools)

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

# TablesShiny
library(DT)

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



########################
# RESSOURCE POUR L'APP
########################

### Axes

indicateurs_axes <- list(
  "Éducation" = c(
    "Literacy rate (%)",
    "Children out of school, primary (Number)"
  ),
  "Économie" = c(
    "Children in employment (% of children ages 7-14)",
    "Borrowed any money in the past year (% age 15+)",
    "Borrowed for education or school fees (% age 15+)",
    "Borrowed for health or medical purposes (% age 15+)",
    "Borrowed from a financial institution (% age 15+)",
    "Borrowed from a savings club (% age 15+)",
    "Borrowed from a store by buying on credit (% age 15+)",
    "Borrowed from family or friends (% age 15+)"
  ),
  "Santé" = c(
    "Age population, interpolated",
    "Belief that religion requires female genital mutilation (% who have heard about FGM)"
  ),
  "Politique" = c()
)


### Palettes
palette_6 <- c("#FFE100","#F58442","#D96C81","#CB3452","#A2BDF4","#BFB74C")
palette_continue <- colorRampPalette(palette_6)

palette_carte <-  c("#CB3452","#D96C81","#F58442","#FFE100")
palette_carte_continue <- colorRampPalette(palette_carte)