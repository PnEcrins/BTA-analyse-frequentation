
### Limites du parc ----
PARC_file <- paste0("0.Data/Limites_shp/", PARC_nom, "/") %>% list.files(.) %>% .[grepl(".shp", .) | grepl(".gpkg", .) | grepl(".geojson", .)] %>% paste0("0.Data/Limites_shp/", PARC_nom, "/", .)

if(PARC_nom=="PNVanoise"){PARC_raw <- rbind.fillSF(st_read(PARC_file, layer="PNV_RNN"), st_read(PARC_file, layer="PNV_limites_coeur_2024"))}
if(PARC_nom=="PNRVercors"){PARC_raw <- st_read(PARC_file, layer="parc_perimetre_2025_2040")}
if(PARC_nom %in% c("PNMercantour")){PARC_raw <- rbind.fillSF(st_read(PARC_file[1]), st_read(PARC_file[2]))}
if(! PARC_nom %in% c("PNVanoise", "PNRVercors", "PNMercantour")){PARC_raw <- st_read(PARC_file)}


# Filtrer certains parcs
if(PARC_nom == "Alpi_Liguri"){PARC_raw <- subset(PARC_raw, denominazi=="Parco Naturale Regionale delle Alpi Liguri")}
if(PARC_nom == "PNRBauges"){PARC_raw <- subset(PARC_raw, name=="Parc naturel r\xe9gional du Massif des Bauges")}
if(PARC_nom == "PNRQueyras"){PARC_raw <- subset(PARC_raw, name=="Parc naturel r\xe9gional du Queyras")}

# Buffer 1m pour enlever des lignes au milieu de quelques parcs
if(PARC_nom %in% c("PNRQueyras", "PNVanoise")){PARC_raw <- st_buffer(PARC_raw,20)}
if(PARC_nom %in% c("PNMercantour")){PARC_raw <- st_buffer(PARC_raw,200)}

# Formater le fichier
PARC_shp <- PARC_raw %>%
  st_transform(., PROJ_Lambert) %>%
  st_union() %>%
  st_as_sf() %>%
  st_make_valid()

# Ajouter le nom du parc
PARC_shp <- PARC_shp %>% mutate(Parc=PARC_nom)
PARC_raw <- PARC_raw %>% mutate(Parc=PARC_nom)

# Plot
ggplot(PARC_shp)+geom_sf()+ggtitle(PARC_nom)+theme_minimal()





### Raster vide ----
EXT <- as.vector(raster::extent(PARC_shp))
RES_raster <- 500
empty_raster <- rast(xmin=EXT[1], xmax=EXT[2]+RES_raster, ymin=EXT[3], ymax=EXT[4]+RES_raster, resolution=RES_raster, vals=1:50000, crs=PROJ_Lambert)
crs(empty_raster) <- st_crs(2154)$proj4string
plot(empty_raster)


### Sentiers ----
if(Run_Sentiers==T){

  Sentiers_files <- paste0("0.Data/Sentiers/", PARC_nom, "/") %>% list.files(.) %>% .[grepl(".shp", .) | grepl(".gpkg", .) | grepl(".geojson", .)] %>% paste0("0.Data/Sentiers/", PARC_nom, "/", .)
  for(i in 1:length(Sentiers_files)){if(i==1){Sentiers <- st_read(Sentiers_files[i])} else {Sentiers <- rbind.fillSF(Sentiers, st_read(Sentiers_files[i]))}}

  # Ajouter crs manuellement pour certains fichiers
  if(PARC_nom == "RNAsters"){st_crs(Sentiers) <- st_crs(2154)}
  if(PARC_nom == "PNRChartreuse"){st_crs(Sentiers) <- st_crs(3857)}
  
  Sentiers <- Sentiers %>%
    st_transform(., PROJ_Lambert) %>%
    mutate(Parc=PARC_nom)

  # Ajuster les noms
  if(PARC_nom=="Alpi_Liguri"){Sentiers$name <- Sentiers$layer}
  if(PARC_nom=="Alpi_Cozie"){Sentiers$name <- Sentiers$nome}
  if(PARC_nom=="Mont-Avic"){Sentiers$name[is.na(Sentiers$name)] <- Sentiers$NOME[is.na(Sentiers$name)]}
  if(PARC_nom=="Ossola"){Sentiers$name[is.na(Sentiers$name)] <- Sentiers$CODICE[is.na(Sentiers$name)]}
  Sentiers$Nom_chemin <- as.data.frame(Sentiers)[, names(Sentiers)[names(Sentiers) %in% c("NOMPNE", "name", "NOM_ITINER", "Name.fr")]]

  # Filtrer manuellement pour PNE
  if(PARC_nom=="PNEcrins"){Sentiers <- subset(Sentiers, grepl("SUPPRIMER", NOMPNE)==F)}

  # Grouper par nom d'itineraire
  Sentiers <- Sentiers[as.numeric(st_length(Sentiers))>0,]
  Sentiers <- Sentiers %>%
    st_filter(., PARC_shp, join=st_intersects) %>%
    st_intersection(., PARC_shp) %>%
    dplyr::group_by(Parc, Nom_chemin) %>%
    dplyr::summarise()

  # Plot
  ggplot(Sentiers)+
    geom_sf(data=PARC_shp, fill="grey80", col=NA)+
    geom_sf(aes(col=Nom_chemin), show.legend=F)+
    theme_minimal()

} else {Sentiers <- NULL}



### Strava -----
if(Run_Prepare_Strava==T){
  strava_raw <- BTA_ChargerStrava(PARC_shp)
  BTA_PrepareStrava(strava_raw, PARC_shp, empty_raster)
}

strava_grid <- st_read(paste0("2.Outputs/0.Strava_ready/", PARC_nom, "/prep_strava_grid.shp"))
strava_vector <- st_read(paste0("2.Outputs/0.Strava_ready/", PARC_nom, "/prep_strava_vector.shp"))
strava_NOTcut <- st_read(paste0("2.Outputs/0.Strava_ready/", PARC_nom, "/prep_strava_vectorNOTCUT.shp"))

# Transformer en raster
strava_raster <- rasterize(st_transform(strava_grid, PROJ_Lambert), empty_raster, field = "Freq") %>%
  replace(., is.na(.), 0) %>%
  mask(., PARC_shp)

plot(sqrt(strava_raster))


### Outdoorvision -----
if(Run_Outdoor==T){

  outdoor_flux <- BTA_ChargerOutdoor(PARC_shp)

  # Calculer le produit de distance * count, ce qui me donne un nombre de km parcourus (une course de 5km par une personne = une course de 1 km par 5 personnes); pas exactement ce qu'on veut mais avantage est qu'on peut les ajouter.
  outdoor_flux$Length <- as.numeric(st_length(outdoor_flux))/1000
  outdoor_flux$DistPers <- round(outdoor_flux$T * outdoor_flux$Length, 3)

  # Preparer la grille (contrairement a Strava, ici on a des toutes petites sections donc je les assigne a une cellule sans les couper)
  StravOut_grid <- empty_raster %>% as.polygons(.) %>% st_as_sf(.) %>% mutate(Cell=paste0("C", 1:nrow(.))) %>% st_transform(., PROJ_Lambert) # C'est la meme projection en realite mais pas le meme code, donc besoin de transformer
  outdoor_flux$Cell <- st_join(st_centroid(outdoor_flux), StravOut_grid, join=st_intersects)$Cell

  # Sommer par cellule
  outdoor_sum <- outdoor_flux %>% dplyr::group_by(Cell) %>% dplyr::summarise(Frequentation=sum(DistPers))
  StravOut_grid$Freq_Outdoor <- outdoor_sum$Frequentation[match(StravOut_grid$Cell, outdoor_sum$Cell)]

  # Transformer en raster
  outdoor_raster <- rasterize(StravOut_grid, empty_raster, field = "Freq_Outdoor") %>%
    replace(., is.na(.), 0) %>%
    mask(., PARC_shp)

  plot(outdoor_raster)
  plot(outdoor_raster==0)

} else {outdoor_raster <- NULL ; outdoor_flux <- NULL}





### iNaturalist ----

# Telecharger et sauvegarder les donnees
if(Run_Download_iNat==T){
  library(rinat)
  BTA_Telecharge_iNaturalist(PARC_shp)
}

# Read data
inat_data <- readRDS(paste0("2.Outputs/0.iNaturalist_ready/", PARC_nom, "/", list.files(paste0("2.Outputs/0.iNaturalist_ready/", PARC_nom))[1]))
inat_data <- subset(inat_data, inat_data$coordinates_obscured=="false")

# Remove some observers from Mont-Avic (staff)
if(PARC_nom=="Mont-Avic"){inat_data <- subset(inat_data, ! user_login %in% c("giannabosio", "robertofacchini", "federicafoghino", "annafoieri", "giadacignetti", "alessandrodurando1", "pietro_ruggeri", "pietroruggeri", "alainlaniece", "danielebaroni", "andreamainetti", "andrebatt", "sofiakoliopoulus", "sarah_bitsch", "marques_f"))}
if(PARC_nom=="Alpi_Cozie"){inat_data <- subset(inat_data, ! user_login %in% c("davidegiuliano", "luca_maurino", "barbara_rizzioli", "elisa_ramassa", "gianfra61"))}

### Creer un raster d'effort (en observateur.jour pour chaque cellule)
inat_data$Raster_grid <- terra::extract(empty_raster, inat_data)$lyr.1
inat_obs.date <- inat_data %>% distinct(user_login, as.Date(datetime), Raster_grid, .keep_all = T)
inat_raster <- terra::rasterize(st_coordinates(inat_obs.date), empty_raster, fun=length)
inat_raster <- replace(inat_raster, is.na(inat_raster), 0) %>% mask(., PARC_shp)
plot(sqrt(inat_raster), main=PARC_nom)
plot(sqrt(inat_raster)==0, main=PARC_nom)

inat_data <- st_filter(inat_data, PARC_shp)


### Ecocompteurs ----
if(PARC_nom %in% c("Alpi_Cozie", "Ossola")){

  ecocompteursSF <- ecocompteurs_quinz <- ecocompteurs_raw <- NULL

} else {

  ecocompteurs_raw <- BTA_PrepareEcocompteurs(PARC_nom, F)
  ecocompteurs_quinz <- BTA_QuinzEcocompteurs(ecocompteurs_raw, PARC_nom)
  ecocompteursSF <- BTA_LocEcocompteurs(PARC_nom, ecocompteurs_raw)
  print(table(unique(ecocompteurs_raw$compteur) %in% ecocompteursSF$compteur))
  print(unique(ecocompteurs_raw$compteur)[! unique(ecocompteurs_raw$compteur) %in% ecocompteursSF$compteur])
  print(table(ecocompteursSF$compteur %in% ecocompteurs_raw$compteur))
  print(ecocompteursSF$compteur)
  print(list.files(paste0("0.Data/Ecocompteurs/", PARC_nom)))
  print(table(ecocompteurs_raw$compteur, is.na(ecocompteurs_raw$donnees))) # Verifier qu'il n'y a pas de compteur avec seulement des NA

  ecocompteursSF <- subset(ecocompteursSF, compteur %in% ecocompteurs_raw$compteur)
  ecocompteursSF$InParc <- sapply(st_intersects(ecocompteursSF, PARC_shp), length)>0
  ecocompteurs_raw <- subset(ecocompteurs_raw, compteur %in% ecocompteursSF$compteur)
  ecocompteurs_quinz <- subset(ecocompteurs_quinz, compteur %in% ecocompteursSF$compteur)
  
  plot(
    ggplot()+
      geom_sf(data=PARC_shp)+
      geom_sf_text(data=ecocompteursSF, aes(label=compteur), col="gray40")+
      geom_sf(data=ecocompteursSF, aes(col=InParc))+
      theme_minimal()
  )

}



### Prepare correlation eco/extrnal ----
library(igraph)
if(is.null(ncol(ecocompteursSF))==F){
  Prep_corr <- BTA_PrepareCorrelation(PARC_shp, ecocompteursSF, ecocompteurs_quinz, strava_NOTcut, inat_data, outdoor_flux, Manual_check=F)
} else {Prep_corr <- NULL}


### Save compiled data ----
Data_tosave <- list(

  COUNTRY=COUNTRY,
  Run_Sentiers=Run_Sentiers,
  Run_Outdoor=Run_Outdoor,
  Run_EcoTempo=Run_EcoTempo,

  PARC_shp=PARC_shp,
  PARC_raw=PARC_raw,
  ecocompteursSF=ecocompteursSF,
  ecocompteurs_quinz=ecocompteurs_quinz,
  ecocompteurs_raw=ecocompteurs_raw,

  strava_NOTcut=strava_NOTcut,

  inat_data=inat_data,

  Prep_corr = Prep_corr,

  outdoor_flux = outdoor_flux,

  Sentiers = Sentiers

)


saveRDS(Data_tosave, paste0("2.Outputs/1.Compiled_data/Compiled_data_", PARC_nom, ".rds"))
writeRaster(strava_raster, paste0("2.Outputs/1.Compiled_data/strava_raster_", PARC_nom, ".tif"), overwrite=T)
writeRaster(inat_raster, paste0("2.Outputs/1.Compiled_data/inat_raster_", PARC_nom, ".tif"), overwrite=T)
if(Run_Outdoor==T){writeRaster(outdoor_raster, paste0("2.Outputs/1.Compiled_data/outdoor_raster_", PARC_nom, ".tif"), overwrite=T)}

beepr::beep(2)
