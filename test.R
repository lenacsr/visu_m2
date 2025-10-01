
data_clean_glo <- read.csv("CSV_GENDER_STAT_CLEAN.csv", 
                           sep = ",",        
                           header = TRUE,    
                           stringsAsFactors = FALSE)
head(data_clean_glo)




### Histogramme 

library(ggplot2)
library(dplyr)

data_estonia <- data_clean_glo %>%
  filter(REF_AREA_LABEL == "Estonia", 
         INDICATOR_LABEL == "Literacy rate (%)",
         SEX_LABEL == "Female")

all_years <- sort(unique(data_clean_glo$year))# tri années par ordre croissant et valeurs uniques (au cas ou double relevé ??)
data_estonia$year <- factor(data_estonia$year, levels = all_years) 

years_with_values <- data_estonia %>%
  filter(!is.na(value)) %>%
  pull(year)

ggplot(data_estonia, aes(x = year, y = value)) +
  geom_col(fill = "steelblue", na.rm = TRUE) +
  scale_x_discrete(breaks = years_with_values) +  # axe que années avec valeurs
  labs(title = "Literacy rate Estonie (F)",
       x = "Année",
       y = "Taux (%)") +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Filtrer les données pour l'Estonie et Literacy rate, tous sexes
data_estonia_sex <- data_clean_glo %>%
  filter(REF_AREA_LABEL == "Estonia", 
         INDICATOR_LABEL == "Literacy rate (%)",
         SEX_LABEL %in% c("Female", "Male"))

# Toutes les années pour garder l'espace
all_years <- sort(unique(data_clean_glo$year))
data_estonia_sex$year <- factor(data_estonia_sex$year, levels = all_years)

# Années avec au moins une valeur
years_with_values <- data_estonia_sex %>%
  filter(!is.na(value)) %>%
  pull(year)

# Graphique
ggplot(data_estonia_sex, aes(x = year, y = value, fill = SEX_LABEL)) +
  geom_col(position = "dodge", na.rm = TRUE) +  # position dodge pour barres côte à côte
  scale_x_discrete(breaks = years_with_values) + # afficher seulement années avec valeur
  labs(title = "Literacy rate Estonie par sexe",
       x = "Année",
       y = "Taux (%)",
       fill = "Sexe") +
  theme_minimal()