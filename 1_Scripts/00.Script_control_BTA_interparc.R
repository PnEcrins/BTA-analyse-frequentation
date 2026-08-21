
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps/3.BTA_interparcs/")

library(sf)
library(ggplot2)
library(dplyr) ; library(plyr)
library(reshape2)
library(terra) ; library(tidyterra)
library(htmltools)
library(gridExtra)
library(tools)
library(readxl)
library(DescTools)

## Source functions
source("../1.Scripts/0.BTA fonctions preparation donnees.R")
source("1.Scripts/BTA2 fonctions interparc.R")
PROJ_Lambert <- st_crs(2154)

# Liste parcs
parclist <- c("PNEcrins", "PNMercantour", "PNVanoise", "RNAsters", "Gran_Paradiso", "Alpi_Marittime", "Alpi_Cozie", "PNRVercors", "PNRQueyras", "PNRChartreuse", "PNRBauges", "Ossola", "Mont-Avic")


## PREPARATION DES DONNEES ----

for(PARC_nom in parclist){
  print(paste0("START Preparation - ", PARC_nom, " - ", Sys.time()))
  ### Choix des parametres
  COUNTRY <- ifelse(PARC_nom %in% c("PNEcrins", "PNMercantour", "PNVanoise", "RNAsters", "PNRVercors", "PNRChartreuse", "PNRBauges", "PNRQueyras"), "FR", "IT")
  Run_Sentiers <- ifelse(PARC_nom %in% c("Gran_Paradiso"), F, T)
  Run_Outdoor <- ifelse(COUNTRY=="FR", T, F)
  Run_EcoTempo <- ifelse(PARC_nom %in% c("Alpi_Cozie", "GranParadiso", "Ossola"), F, T)
  
  ### Actions qui doivent etre faites une fois, puis ensuite si on refait tourner le script on peut reutiliser des fichiers sauvegardes :
  Run_Download_iNat <- F # Passer en T si vous souhaitez telecharger les donnees iNaturalist (sinon le script utilise des donnees deja telechargees pour faire gagner du temps)
  Run_Prepare_Strava <- F # Passer en T si vous souhaitez formatter les donnees Strava (sinon le script utilise des donnees deja formattées pour faire gagner du temps)
  
  ### A lancer parc par parc pour charger et preparer ses donnees jusqu'a l'enregistrement du fichier Compiled_data.rds
  source("1.Scripts/01.Script_Prepare_Data.R")
}

## ANALYSE DES DONNEES ----
# Calcul des PoidsRaster (doit etre fait sur l'ensemble des parcs)
Res_PoidsRaster <- BTA_PoidsRaster()
saveRDS(Res_PoidsRaster, "2.Outputs/2.Analysed_data/Saved_Poids_raster.rds")
plot(Res_PoidsRaster$G_corr_FR)
plot(Res_PoidsRaster$G_corr_IT)

# Analyse parc par parc (une fois que PoidsRaster est calcule sur l'ensemble des parcs)
for(PARC_nom in parclist){
  print(paste0("START Analyses - ", PARC_nom, " - ", Sys.time()))
  source("1.Scripts/02.Script_Analyse_Data.R")
}


## COMPARAISON INTERPARC ----
source("1.Scripts/03.Script_Comparison.R")



## PREPARATION DU RMD ----
library(rmarkdown)
library(prettydoc)
library(knitr)
library(kableExtra)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(DT)

for(PARC_i in parclist){

  # Preparer interpretation
  Interpretation_parc <- BTA_Interpretations(PARC_i)

  # Preparer nom complet du parc (minuscule à 'national' et 'naturel regional') + genre.
  Parc_NomComplet <- revalue(PARC_i, c(
    "PNEcrins"="Parc national des Ecrins",
    "PNMercantour"="Parc national du Mercantour",
    "PNVanoise"="Parc national de la Vanoise",
    "RNAsters"="Réserves naturelles de Haute-Savoie",
    "Gran_Paradiso"="Parco nazionale del Gran Paradiso",
    "Alpi_Marittime"="Aree protette delle Alpi Marittime",
    "Alpi_Cozie"="Aree protette delle Alpi Cozie",
    "Alpi_Liguri"="Parco Naturale Regionale delle Alpi Liguri",
    "PNRVercors"="Parc naturel régional du Vercors",
    "PNRChartreuse"="Parc naturel régional de la Chartreuse",
    "PNRBauges"="Parc naturel régional des Bauges",
    "PNRQueyras"="Parc naturel régional du Queyras",
    "Mont-Avic"="Parco naturale del Mont Avic",
    "Ossola"="Aree Protette dell’Ossola"
  ), warn_missing=F)

  GENRE <- ifelse(PARC_i %in% c("PNEcrins", "PNMercantour", "PNVanoise", "PNRVercors", "PNRChartreuse", "PNRBauges", "PNRQueyras"), "M", "F")

  ### Charger les resultats du parc
  Data_saved <- readRDS(paste0("2.Outputs/2.Analysed_data/Analysed_data_", PARC_i, ".rds"))
  list2env(Data_saved, .GlobalEnv)
  Res_PoidsRaster <- readRDS("2.Outputs/2.Analysed_data/Saved_Poids_raster.rds")
  Comparison_results <- readRDS("2.Outputs/2.Analysed_data/Comparison_results.rds")
  strava_raster <- rast(paste0("2.Outputs/1.Compiled_data/strava_raster_", PARC_i, ".tif"))
  inat_raster <- rast(paste0("2.Outputs/1.Compiled_data/inat_raster_", PARC_i, ".tif"))
  freq_raster <- rast(paste0("2.Outputs/1.Compiled_data/freqSTD_raster_", PARC_i, ".tif"))
  eco_stock <- readRDS("2.Outputs/2.Analysed_data/All_compteurs.rds")

  if(Run_Outdoor==T){
    outdoor_raster <- rast(paste0("2.Outputs/1.Compiled_data/outdoor_raster_", PARC_i, ".tif"))
  } else {outdoor_raster <- NULL}

  ### Carte interactive
  CarteInter <- BTA_CarteInteractive(PARC_raw, ecocompteursSF, ecocompteurs_quinz, Sentiers, inat_data, inat_raster, strava_raster, outdoor_raster, COUNTRY)


  ### Creer le RMD
  render(paste0("1.Scripts/BTA2_CreateMarkDownReport.rmd"),
         output_file=paste0("Rapport_parc_", PARC_i,"_V6.html"),
         output_dir=paste0("2.Outputs/Rapports_parcs/")
  )

  ### Sauvegarder fichiers spatiaux
  rast_output <- rast()
  rast_output$freqSTD_raster <- freq_raster
  rast_output$strava_raster <- strava_raster
  rast_output$inat_raster <- inat_raster
  if(Run_Outdoor==T){rast_output$outdoor_raster <- outdoor_raster}
  writeRaster(rast_output, paste0("2.Outputs/Outputs_spatiaux/", PARC_i, "_Freq_raster.tif"), overwrite=T)

  st_write(Res_FreqSentiers, paste0("2.Outputs/Outputs_spatiaux/", PARC_i, "_Freq_sentiers.gpkg"), append=F)

  # Print
  print(paste0("RMD terminé pour ", PARC_i))
}

