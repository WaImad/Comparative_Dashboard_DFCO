library(googlesheets4)
library(purrr)
library(tidyverse)

## Test sur un seul sheet 
gs4_deauth()
url_sheet_pro <- "https://docs.google.com/spreadsheets/d/1Bq7vBAklxPBupVOcGMod0jzO8Pro5GTnd3U9V613q4g/edit?gid=0#gid=0"
url_sheet_n3 <- "https://docs.google.com/spreadsheets/d/16yzu12xlkSSEUQ-r_0B575c2s77Og8OkAIJiTM3zrsc/edit?gid=0#gid=0"
url_sheet_u19 <- "https://docs.google.com/spreadsheets/d/1-tFSTH6SR4ah1pdolSx7PXDySDNgdUmjiRK6xQclF98/edit?gid=0#gid=0"


urls <- c("Pros" = url_sheet_pro, "National3" = url_sheet_n3,"U19" = url_sheet_u19)

# Fonction avec pause pour éviter le blocage Google du nombre de requete
recup_data <- function(url){
  # On récupère les noms des sheets
  mes_onglets <- sheet_names(url)
  #on read_sheet() dans toute les données et on les combine dans un seul df
  mes_onglets |> 
    set_names() |> 
    map_dfr(~ { Sys.sleep(3) # on ralentit le process
      read_sheet(url, sheet = .x,col_names = TRUE, range= "A1:N17")}
      ,.id = "Nom_Match")
}

#On applique la fonction sur chaque Niveau (Pros,N3,u19)
#Recupère data pros
all_data_pro <- recup_data(url_sheet_pro)

#Recupère data N3
all_data_n3 <- recup_data(url_sheet_n3)

#Récupère data U19

all_data_u19 <- recup_data(url_sheet_u19)

# On combine les données dans un dataframe avec un col 'Niveau'
# pour savoir d'où vient chaque ligne.
df <- bind_rows(
  "Pros" = all_data_pro, 
  "Reserve" = all_data_n3, 
  "Formation" = all_data_u19, 
  .id = "Niveau"
)

# On sauvegarde en local
write.csv(df, "app/data_gps.csv", row.names = FALSE)
print("Fichier CSV actualisé !")
