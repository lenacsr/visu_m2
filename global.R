library(shiny)
library(dplyr)
library(tidyr)
library(httr)
library(rvest)
library(DT)
library(ggplot2)
library(tidyverse)
library(leaflet)

library(sf)
library(rnaturalearth)

library(rnaturalearthdata)
#pour exporter la carte en png
library(leaflet.extras2)
#pour le titre a exporter de la carte
library(snakecase)
#pour mettre le titre de la carte sur la carte pour pouvoir le voir quand tu l'exportes
library(htmltools )
library(shinyWidgets)

#Por l'esthetique de l'appli
library(bslib) #theme
library(fontawesome)
library(shinycssloaders)  # Pour loading spinners
library(shinyjs)

# DataFrame Global
gender <- read.csv("src/GENDER_STAT_CLEAN.csv", sep = ",")

# Fonctions

## FENETRE DES ANNEES 
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


### SCRAPING DES DRAPEAUX 
get_flag_url <- function(iso2) {
  iso2 <- tolower(iso2)
  cdn_url <- paste0("https://flagcdn.com/w320/", iso2, ".png")
  
  # Vérifie si l'image existe
  res <- tryCatch(httr::HEAD(cdn_url), error = function(e) NULL)
  if (!is.null(res) && httr::status_code(res) == 200) {
    return(cdn_url)
  }
  
  # Si le CDN ne répond pas, renvoie NULL
  return(NULL)
}




# Sources
source("page/page_tableau_comparaison.R")
source("page/page_recap_pays.R")
source("page/page_ACP.R")
source("page/page_visualisation.R")
source("page/page_comparaison.R")
source("page/page_statdesc.R")
source("page/page_carte.R")
source("page/page_carte_threshold.R")



# Axes
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


# Palette
palette_6 <- c("#FFE100","#F58442","#D96C81","#CB3452","#A2BDF4","#BFB74C")
palette_continue <- colorRampPalette(palette_6)

palette_carte <-  c("#CB3452","#D96C81","#F58442","#FFE100")
palette_carte_continue <- colorRampPalette(palette_carte)
