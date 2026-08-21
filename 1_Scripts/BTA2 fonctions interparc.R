
####################################################################################
### PREPARATION DE QUELQUES FONCTIONS UTILISEES DANS 0.BTA preparation donnees.R ###
####################################################################################


### Une fonction pour regrouper deux shapefiles avec des colonnes differentes
rbind.fillSF <- function(poly1, poly2){

  # If one of them is null, return the othesr one
  if(is.null(poly1)){return(poly2)}
  if(is.null(poly2)){return(poly1)}

  ### Homogenise column names
  st_geometry(poly1) <- "geometry"
  st_geometry(poly2) <- "geometry"

  ### Add missing columns to poly1
  for(C1 in names(poly2)[! names(poly2) %in% names(poly1)]){poly1[,C1]<-NA}

  ### Add missing columns to poly2
  for(C2 in names(poly1)[! names(poly1) %in% names(poly2)]){poly2[,C2]<-NA}

  ### Merge both
  poly_merged <- rbind(poly1, poly2)

  ### If binomial or id_no present but not complete, complete them
  if("binomial" %in% names(poly_merged)){if(T %in% is.na(poly_merged$binomial)){poly_merged$binomial[is.na(poly_merged$binomial)] <- poly_merged$binomial[is.na(poly_merged$binomial)==F][1]}}
  if("id_no" %in% names(poly_merged)){if(T %in% is.na(poly_merged$id_no)){poly_merged$id_no[is.na(poly_merged$id_no)] <- poly_merged$id_no[is.na(poly_merged$id_no)==F][1]}}

  ### Return
  return(poly_merged)
}


### Fonction pour transformer les dates des ecocompteurs avec velo
date_trans <- function(Times){
  Dates_mois_trans <- Times %>%
    sub("1er", "1", .) %>%
    sub(" janv. ", "-01-", .) %>%
    sub(" févr. ", "-02-", .) %>%
    sub(" mars ", "-03-", .) %>%
    sub(" avr. ", "-04-", .) %>%
    sub(" mai ", "-05-", .) %>%
    sub(" juin ", "-06-", .) %>%
    sub(" juil. ", "-07-", .) %>%
    sub(" août ", "-08-", .) %>%
    sub(" sept. ", "-09-", .) %>%
    sub(" oct. ", "-10-", .) %>%
    sub(" nov. ", "-11-", .) %>%
    sub(" déc. ", "-12-", .) %>%
    data.frame(Original=.)

  Dates_mois_trans$Date <- sapply(strsplit(Dates_mois_trans$Original, " "), function(x){x[1]})
  for(i in 1:nrow(Dates_mois_trans)){if(nchar(Dates_mois_trans$Date[i])==9){Dates_mois_trans$Date[i] <- paste0("0", Dates_mois_trans$Date[i])}}
  Dates_mois_trans$Time <- sapply(strsplit(Dates_mois_trans$Original, " "), function(x){x[2]})

  Dates_mois_trans$Date2 <- paste(substr(Dates_mois_trans$Date, 7,10), substr(Dates_mois_trans$Date, 4,5), substr(Dates_mois_trans$Date, 1,2), sep="-")
  Dates_mois_trans$Time2 <- paste0(Dates_mois_trans$Time, ":00")

  New_date <- paste0(Dates_mois_trans$Date2, "T", Dates_mois_trans$Time2)

  return(New_date)
}




### Fonction qui transforme une date jour/mois/annee en annee-mois-jour
Rev_date <- function(x){
  x2 <- x %>% strsplit(., " ") %>% unlist(.)
  x3 <- x2[1] %>% strsplit(., "/") %>% unlist(.)
  rev_date <- paste0(x3[3], "-", x3[2], "-", x3[1]) %>% paste0(., " ", x2[2])
  return(rev_date)
}




### Fonction pour telecharger les donnees iNaturalist
BTA_Telecharge_iNaturalist <- function(PARC_shp){

  PARC_nom=PARC_shp$Parc[1]

  if(file.exists(paste0("2.Outputs/0.iNaturalist_ready/", PARC_nom))==F){stop("Folder not existing in 2.Outputs/0.iNaturalist_ready")}

  # Couper la region en grille pour ne pas atteindre le nombre limites de donnees telechargeables (maximum a 10,000 alors qu'on en a plus)
  Grid_inat <- PARC_shp %>% st_transform(., st_crs(4326)) %>% st_as_sfc() %>% st_make_grid(n=c(3, 3)) %>% st_as_sf() %>% mutate(OCC=NA) %>% st_filter(., st_transform(PARC_shp, st_crs(4326)), .predicate = st_intersects)
  STOP=0

  # Set inat download limit
  LIM_INAT <- 4000
  if(PARC_nom=="PNRVercors"){LIM_INAT <- 2500} # Some manual fixes to avoid bugs (could be improved)
  if(PARC_nom=="PNRBauges"){LIM_INAT <- 5000}
  if(PARC_nom %in% c("Alpi_Cozie")){LIM_INAT <- 3000}

  while(STOP==0){

    # Calculate OCC per grid cells
    for(i in which(is.na(Grid_inat$OCC))){
      Grid_inat$OCC[i] <- rgbif::occ_count(geometry=st_as_text(st_as_sfc(st_transform(Grid_inat[i,], st_crs(4326)))[1]),
                              datasetKey="50c9509d-22c7-4a22-a47d-8c48425ef4a7")
    }

    # if no cell > 4000, stop, otherwise divide these cells in 4 and start again
    if(max(Grid_inat$OCC, na.rm=T) < LIM_INAT){STOP <- 1} else {

      for(i in which(Grid_inat$OCC>=LIM_INAT)){
        Grid_new <- Grid_inat[i,] %>% st_make_grid(n=c(2,2)) %>% st_as_sf() %>% st_filter(., st_transform(PARC_shp, st_crs(4326)), .predicate = st_intersects)
        Grid_inat <- rbind.fillSF(st_as_sf(Grid_inat[-i,]), Grid_new)
      }
    }
  }

  # Restrict each cell to the content of the parc data (extended by 1km)
  sf::sf_use_s2(FALSE)

  Parc_extended <- PARC_shp %>% st_buffer(., 1000) %>% st_transform(., st_crs(Grid_inat))
  for(i in 1:nrow(Grid_inat)){
    tryCatch({
      Grid_inat[i,] <- st_crop(Grid_inat[i,], st_intersection(Grid_inat[i,], Parc_extended))
      }, error=function(e){cat("Fail in crop (not a problem though)")})
  }
  plot(Grid_inat)

  # Telecharger dans cette grille
  for(i in 1:nrow(Grid_inat)){
    print(paste0("Iteration ", i, "/", nrow(Grid_inat)))
    Sys.sleep(5)

    inat_grid_data <- data.frame()
    try({
      inat_grid_data <- get_inat_obs(geo=T, bounds=as.vector(raster::extent(st_as_sf(Grid_inat[i,])))[c(3,1,4,2)], maxresults=10000) %>% mutate(Download_batch=i)
    })
    print(nrow(inat_grid_data))
    if(nrow(inat_grid_data)==10000){stop("More than 10,000 records")}
    if(nrow(inat_grid_data)>0){
      if(i==1){inat_raw_data <- inat_grid_data} else {inat_raw_data <- rbind(inat_raw_data, inat_grid_data)}
    }
  }

  # Verifier qu'on n'a pas atteint la limite de telechargement pour aucune des grilles
  print("Verifier qu'aucune cellule n'a atteint 10 000, sinon il y a probablement des donnees qui n'ont pas ete telechargees")
  print(table(inat_raw_data$Download_batch))

  # Supprime les doublons qui sont a cheval de la grille
  inat_raw_data <- distinct(inat_raw_data, id, .keep_all=T) %>%
    st_as_sf(., coords = c("longitude","latitude"), remove = FALSE)
  st_crs(inat_raw_data)<-st_crs(4326)

  # Ajouter colonne Annee
  inat_raw_data$Annee <- format(as.Date(inat_raw_data$observed_on), "%Y")
  inat_raw_data$Parc_nom <- PARC_nom

  # Ajouter date de telechargement
  inat_raw_data$Date_telechargement <- c(as.character(Sys.Date()), rep(NA, nrow(inat_raw_data)-1))

  # Projection
  inat_raw_data <- st_transform(inat_raw_data, st_crs(2154))

  # Ne garder que les donnees des dix dernieres annees
  inat_raw_data <- subset(inat_raw_data, Annee>2015 & Annee<2026)

  ### Supprime les donnees hors du parc
  inat_raw_data <- st_filter(inat_raw_data, PARC_shp, .predicate = st_intersects)

  # Sauvegarder les donnees
  saveRDS(inat_raw_data, paste0("2.Outputs/0.iNaturalist_ready/", PARC_nom, "/inat_saved_data_", PARC_nom, ".rds"))

  return(paste0("Les donnees iNaturalist ont ete telechargees et sauvegardees dans le fichier '2.Outputs/0.iNaturalist_ready/", PARC_nom, "/inat_saved_data_", PARC_nom, ".rds'"))

}





### Charger les donnees brutes de Strava (manuellement pour bien verifier chaque extraction)
BTA_ChargerStrava <- function(PARC_shp){

  ### Retrouver nom parc
  PARC_nom <- PARC_shp$Parc[1]


  if(PARC_nom == "PNEcrins"){ # Isere et Hautes-Alpes
    ## Charger shapefile et supprimer doublons ou segments hors PNEcrins (c'est le meme shp pour les differentes annees)
    strava_shp <- rbind(
      st_read(paste0("0.Data/Strava/Isere/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.shp")),
      st_read(paste0("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2023_ped_HA/3efc864349c6eaad9e40509ea36a4da8152f4a188059fed722884a65a4bcde31-1756465891784.shp"))
    ) %>%
      distinct(., osmId, edgeUID, .keep_all = T) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., st_transform(PARC_shp, st_crs(.)), .predicate = st_intersects)

    ## Charger comptages par annee, supprimer les doublons (ce sont les chemins a cheval entre deux departements qui apparaissent dans les deux mais ne doivent etre comptes qu'une fois)
    strava21_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2021_ped_IS/42369ff676f3bf88c65915a55c757fc30677701ce572573b5147c7a172ea2031-1759841837849.csv"),
      read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2021_ped_HA/a86a6abfdc546df61d355ed81ff26d3399fce2aa771b9bfde2608f89d1b48fe7-1756471629970.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2021)

    strava22_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2022_ped_IS/b3bd4045af8bab6c0dfae0270895801a5a54393d0da24c95248918f6742e7cd6-1756371314087.csv"),
      read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2022_ped_HA/d8319123485012fb7af66913914f09eaa0d3b1fa4a7f1cc4cd7e12c9de6e0a31-1756469822776.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2022)

    strava23_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.csv"),
      read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2023_ped_HA/3efc864349c6eaad9e40509ea36a4da8152f4a188059fed722884a65a4bcde31-1756465891784.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2023)

    strava24_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2024_ped_IS/4fc1fbc0b0b2f82b183a61c36a7361727bd914cb8df6f598e437553057fce9bc-1755699327570.csv"),
      read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2024_ped_HA/b5c8685287bd143eebc47f180c86efd71573b3a26165fa61ce75e94b8aa71afe-1756456670245.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2024)

    strava25_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2025_ped_IS/bd52d3be11779f768d000357842d6016b585ca86eb0a7fd8da669c66474bb916-1769078143012.csv"),
      read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2025_ped_HA/23c5a6d1c9b5a87a90e48a70a6bfaea0c716990062b3d1a221072af3a2535f71-1769682860750.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts)

  }


  if(PARC_nom == "PNRBauges"){ # Haute-Savoie et Savoie
    ## Charger shapefile et supprimer doublons ou segments hors parc (c'est le meme shp pour les differentes annees)
    strava_shp <- rbind(
      st_read(paste0("0.Data/Strava/Haute-Savoie/2023/12c052d91e04b93d1d8945d8355de9d5ffb5d592b51b467acb3ef86e866708f8-1769502359834.shp")),
      st_read(paste0("0.Data/Strava/Savoie/all_edges_yearly_2023_ped_savoie/2f7cf289b86accf242ea673fc7e267f296ce7afffb4723c6364fb65e81cb4ae4-1769699558887.shp"))
    ) %>%
      distinct(., osmId, edgeUID, .keep_all = T) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)

    ## Charger comptages par annee, supprimer les doublons (ce sont les chemins a cheval entre deux departements qui apparaissent dans les deux mais ne doivent etre comptes qu'une fois)
    strava21_counts <- rbind(
      read.csv("0.Data/Strava/Haute-Savoie/2021/9eed5200dc1a55a31a9c992fd9692c71a9c0915127c471ad6343455cfee211bf-1769437093289.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2021_ped_savoie/c55ff48b4f8b868dfda0ea89f557e12c9b52d83cbdb075219ba70419520daced-1769702650932.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2021)

    strava22_counts <- rbind(
      read.csv("0.Data/Strava/Haute-Savoie/2022/44c043269759e79f6d3ff6d90d232dbc749de862ffbce81b9d219916b977c95e-1769508807460.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2022_ped_savoie/217fc915dfeec9be26cb5822abae210ac869966be55868696a032a6dccc53f19-1769701625468.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2022)

    strava23_counts <- rbind(
      read.csv("0.Data/Strava/Haute-Savoie/2023/12c052d91e04b93d1d8945d8355de9d5ffb5d592b51b467acb3ef86e866708f8-1769502359834.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2023_ped_savoie/2f7cf289b86accf242ea673fc7e267f296ce7afffb4723c6364fb65e81cb4ae4-1769699558887.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2023)

    strava24_counts <- rbind(
      read.csv("0.Data/Strava/Haute-Savoie/2024/9bc3332372899968fbc5f24b9b0af7545ce490ee488f7619320c32de888a2606-1769439870449.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2024_ped_savoie/2cb9dc4841aeca5e9d5b9890ee0c67c74749a2b1b122905f99abacdbfd490e03-1769697223011.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2024)

    strava25_counts <- rbind(
      read.csv("0.Data/Strava/Haute-Savoie/2025/c5800aa334980ea6320389dd5720b29db5d9452914ddb3161d9f007786856fee-1769438503793.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2025_ped_savoie/9f3e74f1b6773ff904833d9d9c2124338af593016775eb1232906cdcf901354f-1769695940333.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts)

  }


  if(PARC_nom == "PNRChartreuse"){ # Isere et Savoie
    ## Charger shapefile et supprimer doublons ou segments hors parc (c'est le meme shp pour les differentes annees)
    strava_shp <- rbind(
      st_read(paste0("0.Data/Strava/Isere/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.shp")),
      st_read(paste0("0.Data/Strava/Savoie/all_edges_yearly_2023_ped_savoie/2f7cf289b86accf242ea673fc7e267f296ce7afffb4723c6364fb65e81cb4ae4-1769699558887.shp"))
    ) %>%
      distinct(., osmId, edgeUID, .keep_all = T) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)

    ## Charger comptages par annee, supprimer les doublons (ce sont les chemins a cheval entre deux departements qui apparaissent dans les deux mais ne doivent etre comptes qu'une fois)
    strava21_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2021_ped_IS/42369ff676f3bf88c65915a55c757fc30677701ce572573b5147c7a172ea2031-1759841837849.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2021_ped_savoie/c55ff48b4f8b868dfda0ea89f557e12c9b52d83cbdb075219ba70419520daced-1769702650932.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2021)

    strava22_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2022_ped_IS/b3bd4045af8bab6c0dfae0270895801a5a54393d0da24c95248918f6742e7cd6-1756371314087.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2022_ped_savoie/217fc915dfeec9be26cb5822abae210ac869966be55868696a032a6dccc53f19-1769701625468.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2022)

    strava23_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2023_ped_savoie/2f7cf289b86accf242ea673fc7e267f296ce7afffb4723c6364fb65e81cb4ae4-1769699558887.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2023)

    strava24_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2024_ped_IS/4fc1fbc0b0b2f82b183a61c36a7361727bd914cb8df6f598e437553057fce9bc-1755699327570.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2024_ped_savoie/2cb9dc4841aeca5e9d5b9890ee0c67c74749a2b1b122905f99abacdbfd490e03-1769697223011.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2024)

    strava25_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2025_ped_IS/bd52d3be11779f768d000357842d6016b585ca86eb0a7fd8da669c66474bb916-1769078143012.csv"),
      read.csv("0.Data/Strava/Savoie/all_edges_yearly_2025_ped_savoie/9f3e74f1b6773ff904833d9d9c2124338af593016775eb1232906cdcf901354f-1769695940333.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts)

  }


  if(PARC_nom == "PNRVercors"){ # Isere et Drome
    ## Charger shapefile et supprimer doublons ou segments hors parc (c'est le meme shp pour les differentes annees)
    strava_shp <- rbind(
      st_read(paste0("0.Data/Strava/Isere/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.shp")),
      st_read(paste0("0.Data/Strava/Drome/all_edges_yearly_2023_ped_drome/93b7e29970767b4616070af239499219e3930c69455604d72a24e472b7f6f5d0-1769597888610.shp"))
    ) %>%
      distinct(., osmId, edgeUID, .keep_all = T) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)

    ## Charger comptages par annee, supprimer les doublons (ce sont les chemins a cheval entre deux departements qui apparaissent dans les deux mais ne doivent etre comptes qu'une fois)
    strava21_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2021_ped_IS/42369ff676f3bf88c65915a55c757fc30677701ce572573b5147c7a172ea2031-1759841837849.csv"),
      read.csv("0.Data/Strava/Drome/all_edges_yearly_2021_ped_drome/888f3658a7f56951f38fd5adb6bfd21e8a669092ac88f6272aa4e51bbdc675eb-1769605114346.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2021)

    strava22_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2022_ped_IS/b3bd4045af8bab6c0dfae0270895801a5a54393d0da24c95248918f6742e7cd6-1756371314087.csv"),
      read.csv("0.Data/Strava/Drome/all_edges_yearly_2022_ped_drome/5376a7b1ae3b89ff49b0cb2eaf8b181b2d8477d1b1576963def758abb21444c4-1769603399124.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2022)

    strava23_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.csv"),
      read.csv("0.Data/Strava/Drome/all_edges_yearly_2023_ped_drome/93b7e29970767b4616070af239499219e3930c69455604d72a24e472b7f6f5d0-1769597888610.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2023)

    strava24_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2024_ped_IS/4fc1fbc0b0b2f82b183a61c36a7361727bd914cb8df6f598e437553057fce9bc-1755699327570.csv"),
      read.csv("0.Data/Strava/Drome/all_edges_yearly_2024_ped_drome/b371db3a86c8769b2b0e4f04a8ca7b0db7c86d99d55e2ac79031a527472aae95-1769595757297.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2024)

    strava25_counts <- rbind(
      read.csv("0.Data/Strava/Isere/all_edges_yearly_2025_ped_IS/bd52d3be11779f768d000357842d6016b585ca86eb0a7fd8da669c66474bb916-1769078143012.csv"),
      read.csv("0.Data/Strava/Drome/all_edges_yearly_2025_ped_drome/9e48dadfaff5a7dfb53fb03178abf85e3ade700fe954a5e600b0936bd9a1c3d8-1769594847159.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts)

  }


  if(PARC_nom == "PNRQueyras"){ # Hautes-Alpes
    ## Charger shapefile
    strava_shp <- st_read(paste0("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2023_ped_HA/3efc864349c6eaad9e40509ea36a4da8152f4a188059fed722884a65a4bcde31-1756465891784.shp")) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)

    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2021_ped_HA/a86a6abfdc546df61d355ed81ff26d3399fce2aa771b9bfde2608f89d1b48fe7-1756471629970.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2022_ped_HA/d8319123485012fb7af66913914f09eaa0d3b1fa4a7f1cc4cd7e12c9de6e0a31-1756469822776.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2023_ped_HA/3efc864349c6eaad9e40509ea36a4da8152f4a188059fed722884a65a4bcde31-1756465891784.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2024_ped_HA/b5c8685287bd143eebc47f180c86efd71573b3a26165fa61ce75e94b8aa71afe-1756456670245.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/Hautes-Alpes/all_edges_yearly_2025_ped_HA/23c5a6d1c9b5a87a90e48a70a6bfaea0c716990062b3d1a221072af3a2535f71-1769682860750.csv") %>% mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)

  }


  if(PARC_nom == "PNVanoise"){ # Savoie
    ## Charger shapefile
    strava_shp <- st_read(paste0("0.Data/Strava/Savoie/all_edges_yearly_2021_ped_savoie/c55ff48b4f8b868dfda0ea89f557e12c9b52d83cbdb075219ba70419520daced-1769702650932.shp")) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)

    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/Savoie/all_edges_yearly_2021_ped_savoie/c55ff48b4f8b868dfda0ea89f557e12c9b52d83cbdb075219ba70419520daced-1769702650932.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/Savoie/all_edges_yearly_2022_ped_savoie/217fc915dfeec9be26cb5822abae210ac869966be55868696a032a6dccc53f19-1769701625468.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/Savoie/all_edges_yearly_2023_ped_savoie/2f7cf289b86accf242ea673fc7e267f296ce7afffb4723c6364fb65e81cb4ae4-1769699558887.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/Savoie/all_edges_yearly_2024_ped_savoie/2cb9dc4841aeca5e9d5b9890ee0c67c74749a2b1b122905f99abacdbfd490e03-1769697223011.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/Savoie/all_edges_yearly_2025_ped_savoie/9f3e74f1b6773ff904833d9d9c2124338af593016775eb1232906cdcf901354f-1769695940333.csv") %>% mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)

  }


  if(PARC_nom == "PNMercantour"){ # Alpes Maritimes et Alpes Haute-Provence deja combinees

    ## Charger shapefile et supprimer doublons ou segments hors PNEcrins (c'est le meme shp pour les differentes annees)
    strava_shp <- st_read("0.Data/Strava/AlpesMaritimesEtHP/strava_2020_pnm.geojson") %>% # J'ai supprime strava2021-2025 apres avoir verifier que c'etaient exactement les memes
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)

    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/AlpesMaritimesEtHP/2021.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/AlpesMaritimesEtHP/2022.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/AlpesMaritimesEtHP/2023.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/AlpesMaritimesEtHP/2024.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/AlpesMaritimesEtHP/2025.csv") %>% mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)
  }


  if(PARC_nom == "RNAsters"){ # Haute-Savoie

    strava_shp <- st_read("0.Data/Strava/Haute-Savoie/2020/3110e37b0b0649956aaa74a8f635ae6c697e798e2b4a5f21316ccd71bcd61c8c-1768211737252.shp") # J'exclus les annees 2021-2025 apres avoir verifier que c'etaient exactement les memes

    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/Haute-Savoie/2021/9eed5200dc1a55a31a9c992fd9692c71a9c0915127c471ad6343455cfee211bf-1769437093289.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/Haute-Savoie/2022/44c043269759e79f6d3ff6d90d232dbc749de862ffbce81b9d219916b977c95e-1769508807460.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/Haute-Savoie/2023/12c052d91e04b93d1d8945d8355de9d5ffb5d592b51b467acb3ef86e866708f8-1769502359834.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/Haute-Savoie/2024/9bc3332372899968fbc5f24b9b0af7545ce490ee488f7619320c32de888a2606-1769439870449.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/Haute-Savoie/2025/c5800aa334980ea6320389dd5720b29db5d9452914ddb3161d9f007786856fee-1769438503793.csv") %>% mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)

  }
  
  
  if(PARC_nom == "Gran_Paradiso"){ # Italy_MontAvic et Italy_APAC
    
    ## Charger shapefile et supprimer doublons ou segments hors parc (c'est le meme shp pour les differentes annees)
    strava_shp <- rbind(
      st_read(paste0("0.Data/Strava/Italy_APAC/all_edges_yearly_2020_ped-APAC/0a16513e48c76df090a2891d1a7cfeca837fff1e40ff952216ed621ed3ddf0df-1771329494543.shp")),
      st_read(paste0("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2021_ped/98484352d35703536728bbc87e61c00b90dcb32bc95fdafc2d2633a0b56fdd98-1782895863967.shp"))
    ) %>%
      distinct(., osmId, edgeUID, .keep_all = T) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)
    
    ## Charger comptages par annee, supprimer les doublons (ce sont les chemins a cheval entre deux departements qui apparaissent dans les deux mais ne doivent etre comptes qu'une fois)
    strava21_counts <- rbind(
      read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2021_ped-APAC/ba2e79aa1c9d58a13ef29138dad656f63dc3364b31bee32c4bd60a65082d4d5a-1765289455355.csv"),
      read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2021_ped/98484352d35703536728bbc87e61c00b90dcb32bc95fdafc2d2633a0b56fdd98-1782895863967.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2021)
    
    strava22_counts <- rbind(
      read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2022_ped-APAC/c99898a05c1e04c2da7cadaa7b693e5c9b12c9f326f5bc178ea6ef6088c39f9d-1771330840558.csv"),
      read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2022_ped/58e482a56314a45119723d1fa612379800d0449a894d1a208c4a3e0eb1403d4e-1782896896798.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2022)
    
    strava23_counts <- rbind(
      read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2023_ped-APAC/3d647f495f3444efb045835153b8a4a0a97ed60fd98b6499208aa03fe60a4df8-1765286170710.csv"),
      read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2023_ped/9e579098b244cf6081b91d889ad7642abfeba4e2e65ec8b33e964fa9fd5c7d9a-1782903346828.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2023)
    
    strava24_counts <- rbind(
      read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2024_ped-APAC/2886bf244e25f3c0b7dd213c2bd3c6f4d8c07b412a31b8d9eac05366318f08d0-1771332133916.csv"),
      read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2024_ped/8eeb01214be4906ce22b1526badf2f30c4d05633ac3e54ce67810f4b1c738a48-1782906767961.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2024)
    
    strava25_counts <- rbind(
      read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2025_ped_APAC/1b7d5206dd805f0a851de935cb6a7fd1b11f6a49c9b5a02db259a46c993f14ee-1771333825485.csv"),
      read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2025_ped/e0536796d837ff338ad6c7719c73e04649dbf0194a78336f300b51bf76e79b4b-1782907751755.csv")
    ) %>%
      distinct(., edge_uid, .keep_all=T) %>%
      subset(., edge_uid %in% strava_shp$edgeUID) %>%
      mutate(Annee=2025)
    
    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts)
    
  }
  
  
  if(PARC_nom == "Mont-Avic"){ # Italy_MontAvic
    strava_shp <- st_read("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2021_ped/98484352d35703536728bbc87e61c00b90dcb32bc95fdafc2d2633a0b56fdd98-1782895863967.shp")
    
    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2021_ped/98484352d35703536728bbc87e61c00b90dcb32bc95fdafc2d2633a0b56fdd98-1782895863967.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2022_ped/58e482a56314a45119723d1fa612379800d0449a894d1a208c4a3e0eb1403d4e-1782896896798.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2023_ped/9e579098b244cf6081b91d889ad7642abfeba4e2e65ec8b33e964fa9fd5c7d9a-1782903346828.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2024_ped/8eeb01214be4906ce22b1526badf2f30c4d05633ac3e54ce67810f4b1c738a48-1782906767961.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/Italy_MontAvic/all_edges_yearly_2025_ped/e0536796d837ff338ad6c7719c73e04649dbf0194a78336f300b51bf76e79b4b-1782907751755.csv") %>% mutate(Annee=2025)
    
    ## Combiner
    strava_counts_raw <- rbind.fill(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)
    
  }

  if(PARC_nom == "Alpi_Cozie"){ # Italy_APAC

    strava_shp <- st_read("0.Data/Strava/Italy_APAC/all_edges_yearly_2020_ped-APAC/0a16513e48c76df090a2891d1a7cfeca837fff1e40ff952216ed621ed3ddf0df-1771329494543.shp") # J'exclus les annees 2021-2025 apres avoir verifier que c'etaient exactement les memes

    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2021_ped-APAC/ba2e79aa1c9d58a13ef29138dad656f63dc3364b31bee32c4bd60a65082d4d5a-1765289455355.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2022_ped-APAC/c99898a05c1e04c2da7cadaa7b693e5c9b12c9f326f5bc178ea6ef6088c39f9d-1771330840558.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2023_ped-APAC/3d647f495f3444efb045835153b8a4a0a97ed60fd98b6499208aa03fe60a4df8-1765286170710.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2024_ped-APAC/2886bf244e25f3c0b7dd213c2bd3c6f4d8c07b412a31b8d9eac05366318f08d0-1771332133916.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/Italy_APAC/all_edges_yearly_2025_ped_APAC/1b7d5206dd805f0a851de935cb6a7fd1b11f6a49c9b5a02db259a46c993f14ee-1771333825485.csv") %>% mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind.fill(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)

  }

  if(PARC_nom == "Alpi_Marittime"){ # Italy_APAM

    strava_shp <- st_read("0.Data/Strava/Italy_APAM/all_edges_yearly_2021_ped_APAM/2b7209c408471ce25864ce53c2dd3e5b5a83380c325028bd858ad8197271dd5f-1773048264251.shp") # J'exclus les annees 2021-2025 apres avoir verifier que c'etaient exactement les memes

    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/Italy_APAM/all_edges_yearly_2021_ped_APAM/2b7209c408471ce25864ce53c2dd3e5b5a83380c325028bd858ad8197271dd5f-1773048264251.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/Italy_APAM/all_edges_yearly_2022_ped_APAM/99e5c60b184fba9b61d736e8dfb03e301584c3f6b96346a90418a5f3760adda9-1773046869166.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/Italy_APAM/all_edges_yearly_2023_ped_APAM/2d8e3ace3e96f81ee4fe87e03d62382d3042a682c10b470ff5f6b9f3d8780f0f-1766069526428.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/Italy_APAM/all_edges_yearly_2024_ped_APAM/03c5db0e3b5449a96ea4f87a895f9a28ce8571bb574a025f4b469023ee0456ae-1766064561955.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/Italy_APAM/all_edges_yearly_2025_ped_APAM/8f0ba89ba4285ad42015c0b2518e941289fe869b702d871cc010f54e7dcc5ba6-1772548987919.csv") %>% mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind.fill(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)

  }

  if(PARC_nom == "Ossola"){ # Ossola
    ## Charger shapefile
    strava_shp <- st_read(paste0("0.Data/Strava/Italy_Ossola/all_edges_yearly_2021_ped_ossola/f754f89a1f79ec269fdb48d103d230724ec97eda8ccb9fa61796384e2207a83f-1772091223797.shp")) %>%
      st_transform(., st_crs(2154)) %>%
      st_filter(., PARC_shp, .predicate = st_intersects)

    ## Charger comptages par annee
    strava21_counts <- read.csv("0.Data/Strava/Italy_Ossola/all_edges_yearly_2021_ped_ossola/f754f89a1f79ec269fdb48d103d230724ec97eda8ccb9fa61796384e2207a83f-1772091223797.csv") %>% mutate(Annee=2021)
    strava22_counts <- read.csv("0.Data/Strava/Italy_Ossola/all_edges_yearly_2022_ped_ossola/bacd9ab652e93c8093667c8f4ff2063e7dcf522b30af94842dfa1424edafe705-1772038584343.csv") %>% mutate(Annee=2022)
    strava23_counts <- read.csv("0.Data/Strava/Italy_Ossola/all_edges_yearly_2023_ped_ossola/12f93b31b58439a1f0d5ecf8b54d152f5ef0d02e899a90c2e0e448df10366edd-1772029181800.csv") %>% mutate(Annee=2023)
    strava24_counts <- read.csv("0.Data/Strava/Italy_Ossola/all_edges_yearly_2024_ped_ossola/811de66dc49a9b4a060e67a11dedb7c2472b91d6bbaaa994b4918f2623f0ae0b-1772023743813.csv") %>% mutate(Annee=2024)
    strava25_counts <- read.csv("0.Data/Strava/Italy_Ossola/all_edges_yearly_2025_ped_ossola/6811ba0b2e2d0f6c59b09d1e505bb95a2776eab8eec7e4e72c1c44a6277e48ea-1772026490530.csv") %>% mutate(Annee=2025)

    ## Combiner
    strava_counts_raw <- rbind(strava21_counts, strava22_counts, strava23_counts, strava24_counts, strava25_counts) %>%
      subset(., edge_uid %in% strava_shp$edgeUID)

  }

  ## Sommer les differentes annees et diviser par le nombre d'annee (nombre moyen de passages par an)
  NY <- length(unique(strava_counts_raw$Annee))
  strava_counts <- strava_counts_raw %>%
    dplyr::group_by(edge_uid) %>%
    dplyr::summarise(total_trip_count = round(sum(total_trip_count, na.rm=T)/NY,1), N_years=n())

  strava_shp$count <- strava_counts$total_trip_count[match(strava_shp$edgeUID, strava_counts$edge_uid)]
  strava_shp$count[is.na(strava_shp$count)] <- 0
  strava_shp$years <- paste0(unique(strava_counts_raw$Annee), collapse=",")
  strava_shp$Parc <- PARC_nom

  return(strava_shp)

}



### Fonction pour preparer les donnees Strava (parce que c'est un peu long de les couper par la grille de raster)
BTA_PrepareStrava <- function(strava_shp, PARC_shp, empty_raster){

  ### Filtrer
  strava_shp <- strava_shp %>% st_filter(., st_transform(PARC_shp, st_crs(.)), .predicate = st_intersects) %>% subset(., is.na(count)==F)

  ## Preparer la grille
  StravOut_grid <- empty_raster %>% as.polygons(.) %>% st_as_sf(.) %>% mutate(Cell=paste0("C", 1:nrow(.))) %>% st_filter(., st_transform(PARC_shp, st_crs(.)), .predicate = st_intersects) %>% st_transform(., st_crs(strava_shp))
  strava_cut <- st_intersection(strava_shp, StravOut_grid)

  ## Calculer le produit de distance * count, ce qui me donne un nombre de km parcourus (une course de 5km par une personne = une course de 1 km par 5 personnes); pas exactement ce qu'on veut mais avantage est qu'on peut les ajouter.
  strava_cut$Length <- as.numeric(st_length(strava_cut))/1000 # Je coupe par grille pour avoir la longueur parcourue dans chaque grille
  strava_cut$DistPers <- round(strava_cut$count * strava_cut$Length, 3)

  ## Sommer par cellule de la grille
  strava_cut_sum <- strava_cut %>% group_by(Cell) %>% summarise(Frequentation=sum(DistPers, na.rm=T))
  StravOut_grid$Freq <- strava_cut_sum$Frequentation[match(StravOut_grid$Cell, strava_cut_sum$Cell)]

  # Transform
  strava_shp <- st_make_valid(st_transform(strava_shp, st_crs(2154)))

  ## Sauvergarder les fichiers
  st_write(StravOut_grid, paste0("2.Outputs/0.Strava_ready/", PARC_nom, "/prep_strava_grid.shp"), append=F)
  st_write(strava_cut, paste0("2.Outputs/0.Strava_ready/", PARC_nom, "/prep_strava_vector.shp"), append=F)
  st_write(strava_shp, paste0("2.Outputs/0.Strava_ready/", PARC_nom, "/prep_strava_vectorNOTCUT.shp"), append=F)

  return("Les donnees Strava ont ete preparees et sauvegardees")
}



### Charger les donnees Outdoorvision
BTA_ChargerOutdoor <- function(PARC_shp){

  ### Retrouver nom parc
  PARC_nom <- PARC_shp$Parc[1]

  if(PARC_nom == "PNEcrins"){
    outdoor_raw <- st_read("0.Data/Outdoorvision/PNE_Ecrins_AOA_running-walking.gpkg") %>%
      distinct(., id.segment, .keep_all = T)
  }

  if(PARC_nom == "PNMercantour"){
    outdoor_raw <- rbind(
      st_read("0.Data/Outdoorvision/PNE_Mercantour_running-walking.gpkg"),
      st_read("0.Data/Outdoorvision/PNE_Mercantour_AA_running-walking.gpkg")
    ) %>%
      distinct(., id.segment, .keep_all = T)
  }

  if(PARC_nom == "PNVanoise"){
    outdoor_raw <- st_read("0.Data/Outdoorvision/PNE_Vanoise_AOA_running-walking.gpkg") %>%
      distinct(., id.segment, .keep_all = T)
  }

  if(PARC_nom == "PNRQueyras"){
    outdoor_raw <- st_read("0.Data/Outdoorvision/PNE_Queyras_running-walking.gpkg")
  }

  if(PARC_nom == "PNRChartreuse"){
    outdoor_raw <- st_read("0.Data/Outdoorvision/PNE_Chartreuse2_running_walking_running-walking.gpkg")
  }

  if(PARC_nom == "PNRVercors"){
    outdoor_raw <- st_read("0.Data/Outdoorvision/PNE_Vercors2_running_walking_running-walking.gpkg")
  }

  if(PARC_nom == "PNRBauges"){
    outdoor_raw <- st_read("0.Data/Outdoorvision/PNE_Bauges2_running_walking_running-walking.gpkg")
  }

  if(PARC_nom == "RNAsters"){
    outdoor_raw <- rbind(
      st_read("0.Data/Outdoorvision/PNE_Aiguilles-Rouges_running-walking.gpkg"),
      st_read("0.Data/Outdoorvision/PNE_Vallon-de-Berard_running-walking.gpkg"),
      st_read("0.Data/Outdoorvision/PNE_Carlaveyron_running-walking.gpkg"),
      st_read("0.Data/Outdoorvision/PNE_Contamines-Montjoie_running-walking.gpkg"),
      st_read("0.Data/Outdoorvision/PNE_Sixt_running-walking.gpkg")) %>%
      distinct(., id.segment, .keep_all = T)
  }

  outdoor_raw$Parc <- PARC_nom
  outdoor_raw <- outdoor_raw %>% st_transform(., st_crs(2154)) %>% st_make_valid()

  return(outdoor_raw)
}




### Charger les donnees d'ecocompteurs
BTA_PrepareEcocompteurs <- function(PARC_nom, Manuel_check){

  ### Charger donnees compteurs
  ecocompteurs_data <- data.frame()

  List_files <- list.files(paste0("0.Data/Ecocompteurs/", PARC_nom))

  for(FILE in List_files){

    ### Lire (en csv ou xlsx)
    if(file_ext(FILE)=="csv"){
      eco_new <- read.csv2(paste0("0.Data/Ecocompteurs/", PARC_nom, "/", FILE), sep=";")
      if(ncol(eco_new)==1){ # On a deux formats, un avec des points-virgules, un avec des virgules
        eco_new <- read.csv2(paste0("0.Data/Ecocompteurs/", PARC_nom, "/", FILE), sep=",")
      }
      if(ncol(eco_new)==1){ # On a deux formats, un avec des points-virgules, un avec des virgules
        eco_new <- read.csv2(paste0("0.Data/Ecocompteurs/", PARC_nom, "/", FILE), sep="\t")
      }
    }

    if(file_ext(FILE)=="xlsx"){eco_new <- read_excel(paste0("0.Data/Ecocompteurs/", PARC_nom, "/", FILE), guess_max=1000000)}

    ### Ajustements manuels
    if(FILE=="Gordolasque_av_Cabane_B&S.csv"){eco_new$compteur <- "Gordolasque_av_Cabane"}
    if(FILE=="Col_de_la_Cayolle_cote_04.csv"){eco_new$compteur <- "Col-Cayolle"}
    if(FILE=="PNV_Barmettes_Pralo_2009-2025.csv"){eco_new$compteur <- "Barmettes"}
    if(FILE=="PNV_SASSIERE-RIVE-GAUCHE_HT_2020-2025.csv"){eco_new$compteur <- "Sassiere_rive_gauche"}
    if(FILE=="PNV_SassierePiste_HT_2020-2025.csv"){eco_new$compteur <- "Sassiere_piste"}
    if(FILE=="Lauzanier.csv"){eco_new$compteur <- "Lauzanier"}
    if(FILE=="ColLonget.csv"){eco_new$compteur <- "ColLonget"}
    if(FILE=="Lac_lestio.csv"){eco_new$compteur <- "Lac_lestio"}
    if(FILE=="Monte-Fronte_APAL.csv"){eco_new$compteur <- "Monte Fronte"}
    if(PARC_nom=="PNEcrins"){if(sum(eco_new$person, na.rm=T)==0){eco_new$person <- eco_new$date ; eco_new$date <- rownames(eco_new) ; rownames(eco_new)<-1:nrow(eco_new)}}
    if(PARC_nom=="PNEcrins" & ("Piétons.IN" %in% names(eco_new))){eco_new$date <- eco_new$Time ; eco_new$person <- eco_new$Piétons.IN + eco_new$Piétons.OUT ; eco_new <- eco_new[c("date", "person")]}
    if(PARC_nom=="RNAsters"){names(eco_new)[which(names(eco_new)=="Temps")]<-"date" ; eco_new <- reshape2::melt(eco_new, id.vars="date", variable.name="compteur", value.name="person")}
    if(PARC_nom=="Alpi_Liguri" & ("Nome.flusso" %in% names(eco_new))){eco_new$date <- eco_new$Nome.flusso}
    if(PARC_nom=="Alpi_Marittime" & ("Date" %in% names(eco_new))){eco_new$date <- eco_new$Date}
    if(FILE=="Chartreuse_alpette.csv"){eco_new$donnees <- eco_new$entree + eco_new$sortie}
    if(PARC_nom=="PNRBauges"){eco_new <- subset(eco_new, is.na(donnees)==F)}

    ### Homogeneiser les dates
    if(grepl("-", eco_new$date[1])==F){for(i in 1:nrow(eco_new)){eco_new$date[i] <- Rev_date(eco_new$date[i])}}
    if(FILE=="PNV_COETET_HM_2009-2025.csv"){
      for(i in 1:nrow(eco_new)){
        if(grepl("-", eco_new$date[i])==F){eco_new$date[i] <- Rev_date(eco_new$date[i])} # Certaines dates inversees dans ce fichier
      }
    }

    ### Nom colonnes
    if((! "donnees" %in% names(eco_new)) & ("donnees_total" %in% names(eco_new))){eco_new$donnees <- eco_new$donnees_total}
    if((! "donnees" %in% names(eco_new)) & ("donnee" %in% names(eco_new))){eco_new$donnees <- eco_new$donnee}
    if((! "donnees" %in% names(eco_new)) & ("total" %in% names(eco_new))){eco_new$donnees <- eco_new$total}
    if((! "donnees" %in% names(eco_new)) & ("TOTAL" %in% names(eco_new))){eco_new$donnees <- eco_new$TOTAL}
    if((! "donnees" %in% names(eco_new)) & ("person" %in% names(eco_new))){eco_new$donnees <- eco_new$person}
    if((! "donnees" %in% names(eco_new)) & ("Comptage" %in% names(eco_new))){eco_new$donnees <- eco_new$Comptage}
    if((! "donnees" %in% names(eco_new)) & ("comptage" %in% names(eco_new))){eco_new$donnees <- eco_new$comptage}

    ### Arreter le code pour verifier manuellement
    if(Manuel_check==T){
      print(head(eco_new))
      print(summary(eco_new))
      readline("ok?")
    }

    ### Ajouter colonne avec le nom du fichier source
    eco_new$filename <- FILE %>% sub(".csv", "", .) %>% sub(".xlsx", "", .)

    # Ajouter au tableau global
    ecocompteurs_data <- rbind.fill(ecocompteurs_data, eco_new)

  }
  if(Manuel_check==T){print("Processing last step...")}

  ### Formatter les dates
  ecocompteurs_data$mois <- format(as.Date(ecocompteurs_data$date), "%m/%Y")
  if(PARC_nom %in% c("PNRChartreuse", "PNRVercors")){ecocompteurs_data$mois[grepl("_MENSUEL", ecocompteurs_data$filename)] <- paste0(substr(ecocompteurs_data$date[grepl("_MENSUEL", ecocompteurs_data$filename)], 6,7), "/", substr(ecocompteurs_data$date[grepl("_MENSUEL", ecocompteurs_data$filename)], 1,4))}
  ecocompteurs_data$quinzaine <- ifelse(as.numeric(format(as.Date(ecocompteurs_data$date), "%d"))<15.5, "_1", "_2")

  ### Formater les donnees
  ecocompteurs_data$donnees[ecocompteurs_data$donnees=="NaN"] <- NA

  ### Corrections manuelles PNE pour enlever des donnees de troupeaux (choix faits par Juliette)
  if(PARC_nom=="PNEcrins"){
    ecocompteurs_data$compteur <- sub("Ecocompteur", "", ecocompteurs_data$filename)
    ecocompteurs_data$Day_Cpt <- paste0(as.Date(ecocompteurs_data$date), ecocompteurs_data$compteur)
    ecocompteurs_data <- subset(ecocompteurs_data, ! ecocompteurs_data$Day_Cpt %in% c("2021-08-05Confolens", "2021-08-06Confolens", "2021-08-08Confolens", "2021-08-09Confolens", "2021-08-10Confolens",
                                                                                      "2021-06-06Danchere", "2021-07-11Danchere", "2022-06-18Danchere",
                                                                                      "2021-07-14LesGourniers", "2023-07-14LesGourniers", "2024-07-14LesGourniers", "2022-07-15LesGourniers", "2023-07-15LesGourniers", "2022-07-17LesGourniers", "2022-07-19LesGourniers", "2021-08-21LesGourniers",
                                                                                      "2021-08-17PreMmeCarle",
                                                                                      "2021-09-12Surette", "2022-07-22Surette", "2024-07-23Surette",
                                                                                      "2023-09-23Valsenestre", "2022-09-24Valsenestre", "2023-09-26Valsenestre", "2022-09-28Valsenestre", "2022-09-29Valsenestre", "2022-09-30Valsenestre",
                                                                                      "2019-07-31LacDouche", "2019-09-09LacDouche", "2019-09-10LacDouche", "2019-09-11LacDouche",
                                                                                      "2015-06-29LacVallon"

    ))

    # Donnees aberrantes notamment nocturnes Danchere: on supprime quand plus de +1500 passages/heure, et +200 entre 22 et 5h du matin
    ecocompteurs_data$Heure <- substr(ecocompteurs_data$date, 12, 13)

    ecocompteurs_data <- subset(ecocompteurs_data, (compteur != "Danchere") | (person < 1500) | is.na(person))
    ecocompteurs_data <- subset(ecocompteurs_data, (compteur != "Danchere") | (! Heure %in% c("22", "23", "00", "01", "02", "03", "04")) | (person<200) | is.na(person))

    # Donnees aberrantes notamment nocturnes LacDouche: on supprime quand plus de +500 passages/heure, et +100/h entre 22 et 5h du matin
    ecocompteurs_data <- subset(ecocompteurs_data, (compteur != "LacDouche") | (person < 500) | is.na(person))
    ecocompteurs_data <- subset(ecocompteurs_data, (compteur != "LacDouche") | (! Heure %in% c("22", "23", "00", "01", "02", "03", "04")) | (person<100) | is.na(person) )

    ecocompteurs_data$Day_Cpt<-NULL
  }

  ### Corrections manuelles Chartreuse
  if(PARC_nom=="PNRChartreuse"){
    ecocompteurs_data <- subset(ecocompteurs_data, compteur != "Col_de_lAlpette" | (! substr(mois,4,7) %in% c("2011", "2012", "2013")))
    ecocompteurs_data <- subset(ecocompteurs_data, compteur != "Chalais" | substr(mois,4,7)!="2018")
  }

  return(ecocompteurs_data)
}



### Agreger les donnees par quinzaine
BTA_QuinzEcocompteurs <- function(ecocompteurs_data, PARC_nom){

  eco_quinz <- ddply(subset(ecocompteurs_data, is.na(donnees)==F & is.na(quinzaine)==F), .(compteur, mois, quinzaine), function(x){data.frame(

    Annee=format(as.Date(x$date[1]), "%Y"), Mois=format(as.Date(x$date[1]), "%m"),
    Jour1=min(as.Date(x$date[is.na(x$donnees)==F]), na.rm=T),
    Jour2=max(as.Date(x$date[is.na(x$donnees)==F]), na.rm=T),
    Nb_jours=length(unique(as.Date(x$date))),
    Value=sum(as.numeric(x$donnees), na.rm=T)# %>% replace(., .==0, NA)
  )}) %>%
    mutate(Mois_nom=Mois_noms$Nom[match(as.numeric(.$Mois), Mois_noms$Num)]) %>%
    mutate(quinzaine=paste0(.$Mois_nom, .$quinzaine))

  # Ajouter les ecocompteurs mensuels de Chartreuse et Vercors
  if(PARC_nom %in% c("PNRChartreuse", "PNRVercors")){
    mensuel <- subset(ecocompteurs_data, is.na(quinzaine)) %>%
      mutate(Annee=substr(mois,4,7),
             Mois=substr(mois,1,2),
             Value=replace(donnees, donnees=="HS", NA)
             ) %>%
      mutate(Mois_nom=Mois_noms$Nom[match(as.numeric(.$Mois), Mois_noms$Num)],
             Nb_jours=ifelse(as.numeric(Mois)==2, 28, ifelse(as.numeric(Mois) %in% c(1,3,5,7,8,10,12), 31, 30)))
    eco_quinz <- rbind.fill(eco_quinz, mensuel)
    eco_quinz$Value <- as.numeric(eco_quinz$Value)
    }

  return(eco_quinz)
}




### Lire le tableur en ligne de la localisation des écocompteurs
BTA_LocEcocompteurs <- function(PARC_nom, ecocompteurs_raw){

  ### Utiliser le vieux fichier pour les Ecrins
  if(PARC_nom=="PNEcrins"){
    ecocompteursSF <- st_read("../0.Data/0.Carto_monitoring/ecocompteur.shp") %>%
      st_transform(., st_crs(2154)) %>%
      mutate(Localisation_X=st_coordinates(st_transform(., 4326))[,1], Localisation_Y=st_coordinates(st_transform(., 4326))[,2])

    eco_correspondance <- read.table("../0.Data/Ecocompteurs/Table_correspondance_Ecocompteurs.txt")
    ecocompteursSF$compteur <- eco_correspondance$Nom_data[match(ecocompteursSF$name, eco_correspondance$Nom_spatial)]

    return(ecocompteursSF)
  }

  ### Lire la bonne feuille
  SHEET <- revalue(PARC_nom, c(
    "PNEcrins"="PNE",
    "PNMercantour"="PNM",
    "PNVanoise"="PNV",
    "Alpi_Marittime"="APAM",
    "Alpi_Liguri"="APAL",
    "Alpi_Cozie"="APAC",
    "Gran_Paradiso"="PNGP",
    "PNRVercors"="Vercors",
    "PNRChartreuse"="Chartreuse",
    "PNRBauges"="Massif des Bauges",
    "PNRQueyras"="Queyras",
    "RNAsters"="ASTERS RNN 74",
    "Mont-Avic"="Mont-Avic",
    "Ossola"="Ossola"
  ), warn_missing = F)

  raw_loc <- read_excel("0.Data/Ecocompteurs/BTA - données partenaires _ dati partner.xlsx", sheet=SHEET, col_names=F)

  loc <- raw_loc[4:nrow(raw_loc),] ; names(loc) <- gsub(" ", "_", gsub("\n", "_", raw_loc[3,]))
  loc <- as.data.frame(loc)

  # Noms de colonnes
  loc$TypeDispositif <- loc[,grepl("Type_de_dispositif", names(loc))]
  loc$EnvoyeVictor <- as.character(loc[,which(grepl("Victor", names(loc)))])
  loc$Localisation_X <- as.character(loc[,which(grepl("Localisation_X", names(loc)))])
  loc$Localisation_Y <- as.character(loc[,which(grepl("Localisation_Y", names(loc)))])
  loc$compteur <- as.character(loc[,which(grepl("Nom_du_compteur", names(loc)))])
  if("Correspondance_csv" %in% names(loc)){loc$compteur <- loc$Correspondance_csv}

  # Subset
  loc <- subset(loc, loc$TypeDispositif=="Compteur pédestre / contatore pedonale" & loc$EnvoyeVictor=="oui" & grepl("supprimer", loc$Localisation_Y)==F)
  loc <- subset(loc, is.na(Localisation_X)==F & is.na(Localisation_Y)==F)

  ### Transformer loc en sf
  loc$Localisation_X <- sub(",", ".", loc$Localisation_X) %>% as.numeric()
  loc$Localisation_Y <- sub(",", ".", loc$Localisation_Y) %>% as.numeric()

  locSF <- st_as_sf(loc, coords = c("Localisation_X","Localisation_Y"), remove = FALSE, crs=st_crs(4326)) %>% st_transform(., st_crs(2154))

  return(locSF)
}



### Fonction pour preparer le calcul de correlation entre ecocompteurs et donnees externes
BTA_PrepareCorrelation <- function(PARC_shp, ecocompteursSF, ecocompteurs_quinz, strava_NOTcut, inat_data, outdoor_flux, Manual_check=F){

  ### Identifier traces strava proches des ecocompteurs
  strava_NOTcut <- st_transform(strava_NOTcut, st_crs(2154))
  strava_ecocpt <- strava_NOTcut[strava_NOTcut$edgeUID %in% st_filter(st_buffer(strava_NOTcut, 500), ecocompteursSF, .predicate = st_intersects)$edgeUID,] %>% rename(., Count_Strava = count)
  if(nrow(strava_ecocpt)==0){return(NULL)}
  ecocompteursSF$edgeUID <- apply(st_distance(strava_ecocpt, ecocompteursSF), 2, which.min) %>% strava_ecocpt$edgeUID[.]
  strava_ecocpt$Ecocompteur <- ecocompteursSF$compteur[match(strava_ecocpt$edgeUID, ecocompteursSF$edgeUID)]
  strava_ecocpt <- subset(strava_ecocpt, is.na(Ecocompteur)==F)
  strava_ecocpt$Length <- round(as.numeric(st_length(strava_ecocpt)))

  if(Manual_check==T){
    plot(ggplot()+
            geom_sf(data=PARC_shp, fill="gray90", col=NA)+
            geom_sf(data=st_buffer(strava_ecocpt, 1000), aes(col=as.factor(edgeUID)))+
            geom_sf(data=ecocompteursSF, aes(col=as.factor(edgeUID)))+
            theme_minimal())

    plot(ggplot()+
            geom_sf(data=PARC_shp, fill="gray90", col=NA)+
            geom_sf(data=strava_NOTcut)+
            geom_sf(data=ecocompteursSF, col="coral2", show.legend=F, size=5)+
            theme_minimal())
  }


  ### Ajout stat ecocompteur
  eco_ete <- subset(ecocompteurs_quinz, ecocompteurs_quinz$Mois_nom %in% c("Jun", "Jul", "Aou", "Sep") & as.numeric(Annee)>2020 & !(ecocompteurs_quinz$compteur %in% c("Carrelet", "AdeleRomanche")))
  #ggplot(eco_ete)+geom_tile(aes(x=paste(Annee, Mois), y=Nom, fill=Nb_jours))

  eco_std <- eco_ete %>%
    subset(., Nb_jours>10) %>%
    dplyr::group_by(compteur) %>%
    dplyr::summarise(Freq_std = mean(Value / Nb_jours, na.rm=T))

  strava_ecocpt$Cpt_std <- eco_std$Freq_std[match(strava_ecocpt$Ecocompteur, eco_std$compteur)]
  strava_ecocpt <- subset(strava_ecocpt, is.na(strava_ecocpt$Cpt_std)==F)


  ### Isoler les chemins qui passent reellement par l'ecocompteur

  ## Creer un buffer de 100m autour du chemin qui est dans les 500m de l'ecocompteur (pour iNat)
  strava_buffer_rawiNat <- ecocompteursSF[, "compteur"] %>%
    st_buffer(., 500) %>% # Buffer de 500m autour de chaque ecocompteur
    st_intersection(., strava_NOTcut) %>% # Extraire tous les chemins strava qui intersectent le buffer de 500m
    st_buffer(., 10) # Faire un buffer de 10m (pour que les lignes quasi adjacentes soient considerees comme un chemin continu)

  strava_buffer_rawiNat$Group <- components(graph_from_adj_list(st_overlaps(strava_buffer_rawiNat)))$membership
  strava_buffer_inat <- subset(strava_buffer_rawiNat, Group %in% unique(strava_buffer_rawiNat$Group[strava_buffer_rawiNat$edgeUID %in% strava_ecocpt$edgeUID])) %>%
    group_by(compteur, Group) %>% summarise(N=n()) %>%
    st_buffer(., 90)

  ## Creer un buffer de 100m autour du chemin qui est dans les 100m de l'ecocompteur (pour Outdoorvision)
  strava_buffer_rawOutdoor <- ecocompteursSF[, "compteur"] %>%
    st_buffer(., 100) %>% # Buffer de 500m autour de chaque ecocompteur
    st_intersection(., strava_NOTcut) %>% # Extraire tous les chemins strava qui intersectent le buffer de 500m
    st_buffer(., 10) # Faire un buffer de 10m (pour que les lignes quasi adjacentes soient considerees comme un chemin continu)

  strava_buffer_rawOutdoor$Group <- components(graph_from_adj_list(st_overlaps(strava_buffer_rawOutdoor)))$membership
  strava_buffer_outdoor <- subset(strava_buffer_rawOutdoor, Group %in% unique(strava_buffer_rawOutdoor$Group[strava_buffer_rawOutdoor$edgeUID %in% strava_ecocpt$edgeUID])) %>%
    group_by(compteur, Group) %>% summarise(N=n()) %>%
    st_buffer(., 90)

  # Visualisation pour verifier
  if(Manual_check==T){
  for(i in 1:nrow(strava_buffer_rawiNat)){
    ECO_TEST=strava_buffer_rawiNat$compteur[i]
    plot(
      ggplot()+ggtitle(ECO_TEST)+
        geom_sf(data=strava_buffer_rawiNat[strava_buffer_rawiNat$compteur==ECO_TEST,], fill="grey40")+
        geom_sf(data=strava_buffer_rawiNat[strava_buffer_rawiNat$compteur==ECO_TEST,], aes(fill=as.factor(Group)), col=NA, alpha=0.3)+
        geom_sf(data=strava_buffer_rawOutdoor[strava_buffer_rawOutdoor$compteur==ECO_TEST,], aes(fill=as.factor(Group)), col="black", alpha=0.3)+
        geom_sf(data=ecocompteursSF[ecocompteursSF$compteur==ECO_TEST,], size=5)+
        geom_sf(data=st_filter(inat_data, st_buffer(ecocompteursSF[ecocompteursSF$compteur==ECO_TEST,], 600), .predicate=st_intersects), aes(col=paste0(user_login, as.Date(datetime))), show.legend=F)
    )
    readline(paste0("Plot ", i))
  }
  ggplot(strava_buffer_rawiNat)+geom_sf()
  }

  ### Calculer nombre d'observateurs iNat dans ces buffers
  inat_obs.date <- st_join(inat_data, strava_buffer_inat, join=st_intersects) %>%
    subset(., is.na(compteur)==F) %>%
    distinct(user_login, as.Date(datetime), compteur, .keep_all = T) %>%
    dplyr::group_by(compteur) %>% dplyr::summarise(N_obs.day=n())
  strava_ecocpt$iNat_obs.day <- inat_obs.date$N_obs.day[match(strava_ecocpt$Ecocompteur, inat_obs.date$compteur)] %>% replace(., is.na(.), 0)

  ### Calculer Outdoor (somme DistPers) dans ces buffers
  if(is.null(outdoor_flux)==F){
    outdoor_intersect <- st_join(outdoor_flux, strava_buffer_outdoor, join=st_intersects) %>%
      dplyr::group_by(compteur) %>% dplyr::summarise(Sum_DistPers=sum(DistPers))
    strava_ecocpt$outdoor <- outdoor_intersect$Sum_DistPers[match(strava_ecocpt$Ecocompteur, outdoor_intersect$compteur)] %>% replace(., is.na(.), 0)
  }

  ### Ajouter quelques colonnes
  strava_ecocpt$Parc <- PARC_shp$Parc[1]
  strava_ecocpt$InParc <- ecocompteursSF$InParc[match(strava_ecocpt$Ecocompteur, ecocompteursSF$compteur)]

  ### Return
  return(strava_ecocpt)
}





### Carte interactive des donnees
BTA_CarteInteractive <- function(PARC_shp, ecocompteursSF, ecocompteurs_quinz, Sentiers, inat_data, inat_raster, strava_raster, outdoor_raster, COUNTRY){

  ### Preparer les popups des ecocompteurs
  if(is.null(nrow(ecocompteursSF))==F){
  ecocompteursSF$Popup <- NA
  for(i in 1:nrow(ecocompteursSF)){

    DF_sub <- subset(ecocompteurs_quinz, compteur==ecocompteursSF$compteur[i] & is.na(Value)==F)

    # Sous titre
    SOUS_titre <- ifelse(COUNTRY=="FR",

                         paste0(
                           "Nombre de jours de suivi : ", sum(DF_sub$Nb_jours, na.rm=T), "\n",
                           "Années de suivi : ", length(unique(DF_sub$Annee)), " (", min(DF_sub$Annee, na.rm=T), "-", max(DF_sub$Annee, na.rm=T), ")\n",
                           "Proportion de jours suivis par an: ", round(100* sum(DF_sub$Nb_jours, na.rm=T)/ (365*length(unique(DF_sub$Annee)))), "%"
                         ),

                         paste0(
                           "Numero di giorni di monitoraggio : ", sum(DF_sub$Nb_jours, na.rm=T), "\n",
                           "Anni di monitoraggio : ", length(unique(DF_sub$Annee)), " (", min(DF_sub$Annee, na.rm=T), "-", max(DF_sub$Annee, na.rm=T), ")\n",
                           "Percentuale di giorni seguiti all'anno : ", round(100* sum(DF_sub$Nb_jours, na.rm=T)/ (365*length(unique(DF_sub$Annee))))
                         )
    )

    ### Cas special pour les ecocompteurs de la Chartreuse qui ont que des ecocompteurs resumes par mois
    if((PARC_shp$Parc[1] %in% c("PNRChartreuse", "PNRVercors")) & is.na(DF_sub$quinzaine[1])==T){

      # Besoin de creer une grille avec des NA sinon on a un probleme quand on a des annees sans donnees (voir par exemple ici : https://stackoverflow.com/questions/71188611/ggplot-geom-tile-is-distorted-in-ggplotly)
      DF_grid <- tidyr::expand_grid(Mois_nom=Mois_noms$Nom,
                                    Annee=as.character(min(ecocompteurs_quinz$Annee):max(ecocompteurs_quinz$Annee)))
      DF_sub_fill <- dplyr::left_join(DF_grid, DF_sub, by=c('Mois_nom','Annee'))

      # Plots
      Pop <- ggplot(DF_sub_fill)+
        geom_tile(aes(x=factor(Mois_nom, Mois_noms$Nom), y=factor(Annee, levels=as.character(min(ecocompteurs_quinz$Annee):max(ecocompteurs_quinz$Annee))), fill=as.numeric(Value)))+
        scale_x_discrete(drop=F)+scale_y_discrete(drop=F)+
        xlab("Mois")+
        ylab("Année")+
        scale_fill_viridis_c(name="Passages \nuniques", na.value=NA)+
        labs(title=ecocompteursSF$compteur[i], subtitle=HTML(SOUS_titre))+
        theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.margin=unit(c(0.12,0,0,0), units="npc"), plot.title=element_text(hjust=0.5, face="bold"))


    ### Cas general
    } else {

      # Besoin de creer une grille avec des NA sinon on a un probleme quand on a des annees sans donnees (voir par exemple ici : https://stackoverflow.com/questions/71188611/ggplot-geom-tile-is-distorted-in-ggplotly)
      DF_grid <- tidyr::expand_grid(quinzaine=paste0(rep(Mois_noms$Nom, each=2), rep(c("_1", "_2"), 12)),
                                    Annee=as.character(min(ecocompteurs_quinz$Annee):max(ecocompteurs_quinz$Annee)))
      DF_sub_fill <- dplyr::left_join(DF_grid, DF_sub, by=c('quinzaine','Annee'))

      # Plots
      Pop <- ggplot(DF_sub_fill)+
        geom_tile(aes(x=factor(quinzaine, paste0(rep(Mois_noms$Nom, each=2), rep(c("_1", "_2"), 12))), y=factor(Annee, levels=as.character(min(ecocompteurs_quinz$Annee):max(ecocompteurs_quinz$Annee))), fill=Value))+
        scale_x_discrete(drop=F)+scale_y_discrete(drop=F)+
        xlab(ifelse(COUNTRY=="FR", "Quinzaine", "Quindicina"))+
        ylab(ifelse(COUNTRY=="FR", "Année", "Anno"))+
        scale_fill_viridis_c(name=ifelse(COUNTRY=="FR", "Passages \nuniques", "Passaggi \nunici"), na.value=NA)+
        labs(title=ecocompteursSF$compteur[i], subtitle=HTML(SOUS_titre))+
        theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.margin=unit(c(0.12,0,0,0), units="npc"), plot.title=element_text(hjust=0.5, face="bold"))

    }

    ecocompteursSF$Popup[i] <- list(Pop)
    }
  }

  ### Definir les icones
  Red_fact <- 2.5
  IconH=128/Red_fact
  IconW=96/Red_fact

  Icon_ecocompteur <- makeIcon(iconUrl = "../0.Data/icons/icone_ecocompteur.png", iconWidth = IconW, iconHeight = IconH, iconAnchorX = IconW/2, iconAnchorY = IconH)
  
  ### Preparer les rasters
  strava_wins <- strava_raster ; strava_Q99 <- as.numeric(global(strava_raster, quantile, probs=c(0.99), na.rm=TRUE)) ; strava_wins[strava_wins>strava_Q99]<-strava_Q99
  inat_wins <- inat_raster ; inat_Q99 <- as.numeric(global(inat_raster, quantile, probs=c(0.99), na.rm=TRUE)) ; inat_wins[inat_wins>inat_Q99]<-inat_Q99
  if(is.null(outdoor_raster)==F){
    outdoor_wins <- outdoor_raster ; outdoor_Q99 <- as.numeric(global(outdoor_raster, quantile, probs=c(0.99), na.rm=TRUE)) ; outdoor_wins[outdoor_wins>outdoor_Q99]<-outdoor_Q99
  }
  
  ### Palettes couleurs
  max_iNat <- terra::global(inat_wins, fun="max", na.rm=T) %>% as.numeric(.)
  ColorPal_iNat<-colorNumeric(c("#deebf7", "#08519c", "#08306b"), domain=c(0, 100, max_iNat), na.color = NA)
  max_Strava <- terra::global(strava_wins, fun="max", na.rm=T) %>% as.numeric(.)
  ColorPal_Strava<-colorNumeric(c("#deebf7", "#08519c", "#08306b"), domain=c(0, 100, max_Strava), na.color = NA)

  if(is.null(outdoor_raster)==F){
    max_Outdoor <- terra::global(outdoor_wins, fun="max", na.rm=T) %>% as.numeric(.)
    ColorPal_Outdoor<-colorNumeric(c("#deebf7", "#08519c", "#08306b"), domain=c(0, 100, max_Outdoor), na.color = NA)
  }

  ### Plugin et Render pour avoir des sous-titres dans la legende
  groupedLayerControlPlugin <- htmlDependency(
    "@groupedLayerControl", "0.6.1",
    src = c(href = 'https://cdn.jsdelivr.net/npm/leaflet-groupedlayercontrol@0.6.1/dist/'),
    script = 'leaflet.groupedlayercontrol.min.js',
    stylesheet = 'leaflet.groupedlayercontrol.min.css'
  )

  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }

  Render_txt <- paste0("
        function() {

      var baseOverlays = {}

      var groupedOverlays = {
        '<u>Donnees parc</u>': {",
          ifelse(is.null(nrow(ecocompteursSF))==F, "'Eco compteurs': this.layerManager.getLayerGroup('Eco compteurs', true)", ""),
          ifelse(is.null(nrow(ecocompteursSF))==F & is.null(Sentiers)==F, ",", ""),
          ifelse(is.null(Sentiers)==F, "'Sentiers': this.layerManager.getLayerGroup('Sentiers', true)", ""),
        "},
        '<br><u>Donnees externes</u>': {
          'iNaturalist': this.layerManager.getLayerGroup('iNaturalist', true),
          'Strava': this.layerManager.getLayerGroup('Strava', true)",

          ifelse(is.null(outdoor_raster)==F, ",'Outdoorvision': this.layerManager.getLayerGroup('Outdoorvision', true)", ""),

        "}
      };

      var layerControl = L.control.groupedLayers(
        baseOverlays,
        groupedOverlays,
        {collapsed: false, position: 'topleft'}
      ).addTo(this);

      L.DomEvent.disableClickPropagation(layerControl._container);
      L.DomEvent.disableScrollPropagation(layerControl._container);

      // Make legend disappear when we hide Group; taken from: https://stackoverflow.com/questions/66920469/how-to-show-hide-legend-with-control-layer-panel-with-leaflet
      var map = this;
      var legends = map.controls._controlsById;
      function addActualLegend() {
         var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
         $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
         map.addControl(legends[sel]);
      }
      $('.leaflet-control-layers-base').on('click', addActualLegend);
      addActualLegend();

    }") # https://stackoverflow.com/questions/79652329/add-subtitles-in-r-leaflet-layer-control-of-overlay-groups
  
  

  ### Carte
  map <- leaflet() %>%
    #addTiles("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", group="OpenStreetMap", options = tileOptions(opacity=0.5)) %>%
    addProviderTiles("Esri.WorldTopoMap", group="Topographie", options = providerTileOptions(opacity=0.5)) %>%
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%

    addPolygons(data=st_transform(PARC_shp, st_crs(4326)), fill=F, stroke=T, weight=5) %>%

    addRasterImage(strava_wins, method="ngb", group="Strava", opacity=0.7, colors=ColorPal_Strava) %>%
    addLegend(colors=ColorPal_Strava(seq(0, max_Strava, length.out=5)), label=round(seq(0, max_Strava, length.out=5)), opacity=0.7, group="Strava", title="Km parcourus") %>%

    addCircleMarkers(lng=inat_data$longitude, lat=inat_data$latitude, group="iNaturalist", radius=2, opacity=0.1, stroke=F, color="#08306b") %>%
    addRasterImage(inat_wins, method="ngb", group="iNaturalist", opacity=0.7, colors=ColorPal_iNat) %>%
    addLegend(colors=ColorPal_iNat(seq(0,max_iNat,length.out=5)), label=round(seq(0,max_iNat,length.out=5)), opacity=0.7, group="iNaturalist", title="Nb observateurs")

  # Ajouter ecocompteurs s'ils sont presents
  if(is.null(nrow(ecocompteursSF))==F){
    map <- map %>%
      addMarkers(lng=ecocompteursSF$Localisation_X, lat=ecocompteursSF$Localisation_Y, group="Eco compteurs", icon = Icon_ecocompteur) %>%
      leafpop::addPopupGraphs(ecocompteursSF$Popup, group = "Eco compteurs", width = 450, height = 300)

  }
  # Ajouter Sentiers s'ils sont presents
  if(is.null(Sentiers)==F){
    Sentiers <- subset(Sentiers, st_geometry_type(Sentiers) %in% c("LINESTRING", "MULTILINESTRING"))
    map <- map %>%
      addPolylines(data=st_transform(Sentiers, st_crs(4326)), group="Sentiers", popup=paste0("<b>Nom du chemin: </b>", Sentiers$Nom_chemin), color="darkred", weight=3)
  }

  # Add outdoorvision if present
  if(is.null(outdoor_raster)==F){
    map <- map %>%
      addRasterImage(outdoor_wins, method="ngb", group="Outdoorvision", opacity=0.7, colors=ColorPal_Outdoor) %>%
      addLegend(colors=ColorPal_Outdoor(seq(0, max_Outdoor, length.out=5)), label=round(seq(0, max_Outdoor, length.out=5)), opacity=0.7, group="Outdoorvision", title="Km parcourus")
  }

  # Layers and render
  HIDE_gr <- c("iNaturalist", "Outdoorvision", "Strava") ; if(is.null(outdoor_raster)){HIDE_gr <- HIDE_gr[HIDE_gr != "Outdoorvision"]}
  map <- map %>%

    addLayersControl(baseGroups=c("Topographie", "Satellite"), position="topleft", options=layersControlOptions(collapsed=F)) %>%
    hideGroup(HIDE_gr) %>%

    addScaleBar(position="bottomright") %>%
    registerPlugin(groupedLayerControlPlugin) %>%
    htmlwidgets::onRender(Render_txt)

  ### Return
  return(map)

}


### Fonction pour calculer les poids a donner aux differents raster (Strava, Outdoor, iNaturalist)
BTA_PoidsRaster <- function(){

  ### FRANCE
  # Charger donnees
  Data_compiled_FR <- rbind.fill(
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_PNEcrins.rds")$Prep_corr %>% mutate(Country="FR"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_PNMercantour.rds")$Prep_corr %>% mutate(Country="FR"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_PNVanoise.rds")$Prep_corr %>% mutate(Country="FR"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_RNAsters.rds")$Prep_corr %>% mutate(Country="FR"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_PNRVercors.rds")$Prep_corr %>% mutate(Country="FR"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_PNRQueyras.rds")$Prep_corr %>% mutate(Country="FR"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_PNRBauges.rds")$Prep_corr %>% mutate(Country="FR"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_PNRChartreuse.rds")$Prep_corr %>% mutate(Country="FR")
  ) %>%
    subset(., InParc==T)

  # Calcul correlation
  FR_strava <- cor.test(Data_compiled_FR$Cpt_std, Data_compiled_FR$Count_Strava)$estimate
  FR_iNat <- cor.test(Data_compiled_FR$Cpt_std, Data_compiled_FR$iNat_obs.day)$estimate
  FR_outdoor <- cor.test(Data_compiled_FR$Cpt_std, Data_compiled_FR$outdoor)$estimate

  # Plots correlation
  G_FR_strava <- ggplot(Data_compiled_FR)+geom_point(aes(x=Cpt_std, y=Count_Strava, col=Parc))+xlab("Eco-compteurs (été)")+ylab("Passages")+labs(title="Strava", subtitle=paste0("Correlation : ", 100*round(FR_strava,2), "%"))
  G_FR_inat <- ggplot(Data_compiled_FR)+geom_point(aes(x=Cpt_std, y=iNat_obs.day, col=Parc))+xlab("Eco-compteurs (été)")+ylab("Observateurs.jours")+labs(title="iNaturalist", subtitle=paste0("Correlation : ", 100*round(FR_iNat,2), "%"))
  G_FR_outdoor <- ggplot(Data_compiled_FR)+geom_point(aes(x=Cpt_std, y=outdoor, col=Parc))+xlab("Eco-compteurs (été)")+ylab("Passages")+labs(title="Outdoorvision", subtitle=paste0("Correlation : ", 100*round(FR_outdoor,2), "%"))
  legend <- gtable::gtable_filter(ggplot_gtable(ggplot_build(G_FR_strava + theme(legend.position="bottom"))), "guide-box")

  G_FR <- grid.arrange(
    arrangeGrob(
      G_FR_strava + theme(legend.position="none"),
      G_FR_outdoor + theme(legend.position="none"),
      G_FR_inat + theme(legend.position="none"),
      nrow=1),
    legend,
    heights=c(0.8,0.2),
    nrow=2)


  # Calcul poids
  Poids_FR=c(
    strava = ifelse(FR_strava>0.25, (1/(1-FR_strava)), 0) %>% as.numeric(),
    inat = ifelse(FR_iNat>0.25, (1/(1-FR_iNat)), 0) %>% as.numeric(),
    outdoor = ifelse(FR_outdoor>0.25, (1/(1-FR_outdoor)), 0) %>% as.numeric()
  )

  # Arrondir tout en ayant une somme de 100%
  Poids_FR <- Poids_FR/sum(Poids_FR)*100  # Standardize result
  Poids_FR_round <- floor(Poids_FR)    # Find integer bits
  rsum_FR <- sum(Poids_FR_round)   # Find out how much we are missing
  if(rsum_FR<100) { # Distribute points based on remainders and a random tie breaker
    o <- order(Poids_FR%%1, sample(length(Poids_FR)), decreasing=TRUE)
    Poids_FR_round[o[1:(100-rsum_FR)]] <- Poids_FR_round[o[1:(100-rsum_FR)]]+1
  }



  ### ITALY
  # Charger donnees
  Data_compiled_IT <- rbind.fill(
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_Alpi_Marittime.rds")$Prep_corr %>% mutate(Country="IT"),
    readRDS("2.Outputs/1.Compiled_data/Compiled_data_Mont-Avic.rds")$Prep_corr %>% mutate(Country="IT")
  )

  # Calcul correlation
  IT_strava <- cor.test(Data_compiled_IT$Cpt_std, Data_compiled_IT$Count_Strava)$estimate
  IT_iNat <- cor.test(Data_compiled_IT$Cpt_std, Data_compiled_IT$iNat_obs.day)$estimate

  # Plots correlation
  G_IT_strava <- ggplot(Data_compiled_IT)+geom_point(aes(x=Cpt_std, y=Count_Strava, col=Parc))+xlab("Contatori (estate)")+ylab("Passaggi")+labs(title="Strava", subtitle=paste0("Correlazione : ", 100*round(IT_strava,2), "%"))
  G_IT_inat <- ggplot(Data_compiled_IT)+geom_point(aes(x=Cpt_std, y=iNat_obs.day, col=Parc))+xlab("Contatori (estate)")+ylab("Osservatori.giorni")+labs(title="iNaturalist", subtitle=paste0("Correlazione : ", 100*round(IT_iNat,2), "%"))
  legend <- gtable::gtable_filter(ggplot_gtable(ggplot_build(G_IT_strava + theme(legend.position="bottom"))), "guide-box")
  G_IT <- grid.arrange(
    arrangeGrob(
      G_IT_strava + theme(legend.position="none"),
      G_IT_inat + theme(legend.position="none"),
      nrow=1),
    legend,
    heights=c(0.85,0.15),
    nrow=2)

  # Calcul poids
  Poids_IT=c(
    strava = ifelse(IT_strava>0.25, (1/(1-IT_strava)), 0) %>% as.numeric(),
    inat = ifelse(IT_iNat>0.25, (1/(1-IT_iNat)), 0) %>% as.numeric()
  )

  # Arrondir tout en ayant une somme de 100%
  Poids_IT <- Poids_IT/sum(Poids_IT)*100  # Standardize result
  Poids_IT_round <- floor(Poids_IT)    # Find integer bits
  rsum_IT <- sum(Poids_IT_round)   # Find out how much we are missing
  if(rsum_IT<100) { # Distribute points based on remainders and a random tie breaker
    o <- order(Poids_IT%%1, sample(length(Poids_IT)), decreasing=TRUE)
    Poids_IT_round[o[1:(100-rsum_IT)]] <- Poids_IT_round[o[1:(100-rsum_IT)]]+1
  }


  # Return
  return(list(
    Poids_FR=Poids_FR_round,
    Poids_IT=Poids_IT_round,
    G_corr_FR=G_FR,
    G_corr_IT=G_IT
  ))

}


### Analyse des rasters pour obtenir un raster de frequentation standardisee
BTA_FreqRaster <- function(Poids, strava_raster, outdoor_raster, inat_raster){

  # Calcul raster
  freq_std <-
    Poids["strava"] * scale(strava_raster) +
    Poids["inat"] * scale(inat_raster)

  if(is.null(outdoor_raster)==F){
    freq_std <- freq_std +
      Poids["outdoor"] * scale(outdoor_raster)
  }

  # Winsorize and normalise
  Winso_Max <- as.numeric(quantile(as.vector(freq_std), 0.98, na.rm=T))
  freq_std <- replace(freq_std, freq_std>Winso_Max, Winso_Max)
  Min_STD <- as.numeric(global(freq_std, "min", na.rm=T))
  Max_STD <- as.numeric(global(freq_std, fun="max", na.rm=T))
  freq_std <- (100*(freq_std-Min_STD) / (Max_STD-Min_STD))

  # Return
  return(freq_std)
}



### Analyse par sentier
BTA_FreqSentiers <- function(Sentiers, strava_NOTcut, outdoor_flux, inat_data){
  sentiers100 <- st_buffer(Sentiers, 100) %>% mutate(Area=as.numeric(st_area(.)))

  # Strava et Outdoor
  Strava_chemin <- st_intersection(strava_NOTcut, sentiers100) %>% mutate(Length=as.numeric(st_length(.))) %>% mutate(DistPers=Length*count) %>% group_by(Nom_chemin) %>% summarise(Sum_DistPers=sum(DistPers, na.rm=T))
  sentiers100$Strava <- Strava_chemin$Sum_DistPers[match(sentiers100$Nom_chemin, Strava_chemin$Nom_chemin)] / sentiers100$Area

  if(is.null(outdoor_flux)==F){
    Outdoor_chemin <- st_intersection(outdoor_flux, sentiers100)
    Outdoor_cheminCOUNT <- data.frame(OutdoorSum=tapply(Outdoor_chemin$DistPers, Outdoor_chemin$Nom_chemin, "sum"))
    sentiers100$Outdoor <- Outdoor_cheminCOUNT$OutdoorSum[match(sentiers100$Nom_chemin, rownames(Outdoor_cheminCOUNT))] / sentiers100$Area
  }

  # Calculer le nombre d'observateurs iNat (pas besoin de plantNet car correlation trop faible) et fit d'un modele de species area relationship pour corriger le biais de distance
  inat_obs.dateCHEMIN <- st_join(inat_data, sentiers100, join=st_intersects) %>%
    subset(., is.na(Nom_chemin)==F) %>%
    distinct(user_login, as.Date(datetime), Nom_chemin, .keep_all = T) %>%
    group_by(Nom_chemin) %>% summarise(N_obs.day=n())
  sentiers100$iNat_obs.day <- inat_obs.dateCHEMIN$N_obs.day[match(sentiers100$Nom_chemin, inat_obs.dateCHEMIN$Nom_chemin)] %>% replace(., is.na(.), 0)

  library(sars)
  fit <- sar_power(as.data.frame(sentiers100)[, c("Area", "iNat_obs.day")])
  plot(fit)
  sentiers100$iNat_residuals <- fit$residuals[match(sentiers100$Area, fit$data$A)]

  # Verifier les patterns
  # ggplot(st_buffer(sentiers100,200))+geom_sf(aes(fill=iNat_residuals), col=NA)+scale_fill_viridis_c()
  # plot(inat_raster)
  # ggplot(st_buffer(sentiers100,200))+geom_sf(aes(fill=sqrt(Strava)), col=NA)+scale_fill_viridis_c()
  # plot(sqrt(strava_raster))
  # ggplot(st_buffer(sentiers100,200))+geom_sf(aes(fill=Outdoor), col=NA)+scale_fill_viridis_c()
  # plot(outdoor_raster)

  # Creer la carte ponderee
  sentiers100$Freq_std <-
    as.numeric(Res_PoidsRaster$Poids["strava"]) * scale(sentiers100$Strava) +
    as.numeric(Res_PoidsRaster$Poids["inat"]) * scale(sentiers100$iNat_residuals)
  if(Run_Outdoor==T){sentiers100$Freq_std <- sentiers100$Freq_std + as.numeric(Res_PoidsRaster$Poids["outdoor"]) * scale(sentiers100$Outdoor)}

  # Standardiser les comptages
  Winso_Max <- as.numeric(quantile(sentiers100$Freq_std, 0.98, na.rm=T))
  sentiers100$Freq_std <- replace(sentiers100$Freq_std, sentiers100$Freq_std>Winso_Max, Winso_Max)
  Min_STD <- min(sentiers100$Freq_std, na.rm=T)
  Max_STD <- max(sentiers100$Freq_std, na.rm=T)
  sentiers100$Freq_std <- as.numeric(100*(sentiers100$Freq_std-Min_STD) / (Max_STD-Min_STD))

  return(sentiers100)
}




### Analyse temporelle des donnees iNaturalist
# BTA_iNatTempo <- function(inat_raw_data, COUNTRY){
#
#   List_return <- list()
#
#   # Format date (julian)
#   inat_raw_data$Date <- as.Date(inat_raw_data$datetime)
#   inat_raw_data$JDay <- format(inat_raw_data$Date, "%j") %>% as.numeric(.)
#
#   # Somme du nombre d'observateurs par jour (apres 2016-2025 seulement)
#   inat_sum <- inat_raw_data %>%
#     dplyr::group_by(Date) %>%
#     dplyr::summarise(N_data=n(), N_observers=length(unique(user_login))) %>%
#     mutate(JDay=format(.$Date, "%j"), Annee=format(.$Date, "%Y")) %>%
#     subset(., Annee>2020 & Annee<2026)
#
#   ## TRANSFORM TO TS: https://stackoverflow.com/questions/52470591/creating-a-ts-time-series-with-missing-values-from-a-data-frame
#   inat_ts <- data.frame(date=inat_sum$Date, value=inat_sum$N_observers) %>%
#     read.zoo(FUN = as.Date) %>%
#     as.ts %>%
#     replace(., is.na(.), 0) %>%
#     ts(., freq=365.25)
#
#   # DECOMPOSE ONCE I HAVE A TS: https://rpubs.com/davoodastaraky/TSA1
#   inat_decomp <- stats::decompose(inat_ts, type="multiplicative")
#   List_return$Data_decompose <- inat_decomp
#
#   ### Identify peaks
#   season <- data.frame(date=1:365, Freq=inat_decomp$seasonal[1:365])
#   season$Cat <- cut(season$Freq, quantile(season$Freq, probs=c(0, 0.5, 0.95, 1)), include.lowest=T, labels=c("0-50%", "50-95%", "95-100%"))
#
#   # Peak 95-100%
#   Peak_ext <- c(min(season$date[season$Cat=="95-100%"]), max(season$date[season$Cat=="95-100%"]))
#   Peak_form <- (Peak_ext-1) %>% as.Date(.) %>% format("%d/%m")
#
#   # Below 50%
#   Low_ext <- c(max(season$date[season$Cat=="0-50%" & season$date<mean(Peak_ext)]), min(season$date[season$Cat=="0-50%" & season$date>mean(Peak_ext)]))
#   Low_form <- (Low_ext-1) %>% as.Date(.) %>% format("%d/%m")
#
#   # plot
#   G_season <- ggplot(season)+
#     geom_point(aes(as.Date(date, origin = as.Date("2018-01-01")), Freq, col=Cat))+
#     geom_vline(xintercept=as.Date(Peak_ext, origin = as.Date("2018-01-01")), col="darkred", linewidth=1.2, linetype="dashed")+
#     geom_vline(xintercept=as.Date(Low_ext, origin = as.Date("2018-01-01")), col="lightblue", linewidth=1.2, linetype="dashed")+
#     scale_colour_manual(values=c("lightblue", "orange", "darkred"), name="Quantile")+
#     scale_x_date(date_labels = "%b", date_breaks="month")+
#     xlab("")+ylab(ifelse(COUNTRY=="FR", "Fréquence des visites journalières", "Frequenza delle visite giornaliere"))+
#     theme_minimal()
#
#   List_return$Plot_season <- G_season
#
#   List_return$Pic <- Peak_form
#   List_return$Basse <- Low_form
#
#   return(List_return)
#
# }

BTA_iNatTempo <- function(inat_raw_data, COUNTRY, PARC_nom){

  List_return <- list()

  # Format date (julian)
  inat_raw_data$Date <- as.Date(inat_raw_data$datetime)
  inat_raw_data$JDay <- format(inat_raw_data$Date, "%j") %>% as.numeric(.)

  # Somme du nombre d'observateurs par jour (apres 2016-2025 seulement)
  inat_sum <- inat_raw_data %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(N_data=n(), N_observers=length(unique(user_login))) %>%
    mutate(JDay=as.numeric(format(.$Date, "%j")), Annee=format(.$Date, "%Y")) %>%
    subset(., Annee>2020 & Annee<2026) %>%
    dplyr::group_by(JDay) %>%
    dplyr::summarise(N_observers=mean(N_observers, na.rm=T))

  if(PARC_nom=="Mont-Avic"){ # Pas assez de donnees pour calculer quantiles Mont-Avic, je fais une approximation manuelle
    inat_sum$Cat <- cut(inat_sum$N_observers, c(0,1.5,2.5,4), include.lowest=T, labels=c("0-50%", "50-95%", "95-100%"))
  } else {
    inat_sum$Cat <- cut(inat_sum$N_observers, quantile(inat_sum$N_observers, probs=c(0, 0.5, 0.95, 1)), include.lowest=T, labels=c("0-50%", "50-95%", "95-100%"))
  }
  
  # Peak 95-100%
  Peak_ext <- c(min(inat_sum$JDay[inat_sum$Cat=="95-100%"]), max(inat_sum$JDay[inat_sum$Cat=="95-100%"]))
  Peak_form <- (Peak_ext-1) %>% as.Date(.) %>% format("%d/%m")

  # Below 50%
  Low_ext <- c(max(inat_sum$JDay[inat_sum$Cat=="0-50%" & inat_sum$JDay<Peak_ext[1]]), min(inat_sum$JDay[inat_sum$Cat=="0-50%" & inat_sum$JDay>Peak_ext[2]]))
  Low_form <- (Low_ext-1) %>% as.Date(.) %>% format("%d/%m")

  # plot
  G_season <- ggplot(inat_sum)+
    geom_point(aes(as.Date(JDay, origin = as.Date("2018-01-01")), N_observers, col=Cat))+
    geom_vline(xintercept=as.Date(Peak_ext, origin = as.Date("2018-01-01")), col="darkred", linewidth=1.2, linetype="dashed")+
    #geom_vline(xintercept=as.Date(Low_ext, origin = as.Date("2018-01-01")), col="lightblue", linewidth=1.2, linetype="dashed")+
    scale_colour_manual(values=c("lightblue", "orange", "darkred"), name="Quantile")+
    scale_x_date(date_labels = "%b", date_breaks="month")+
    xlab("")+ylab(ifelse(COUNTRY=="FR", "Fréquence des visites journalières", "Frequenza delle visite giornaliere"))+
    theme_minimal()

  List_return$Plot_season <- G_season

  List_return$Pic <- Peak_form
  List_return$Basse <- Low_form

  return(List_return)

}


### Ecocompteurs temporel
BTA_TempoEcocompteurs <- function(ecocompteurs_raw, COUNTRY){

  # Supprimer les ecocompteurs de la Chartreuse sans horaire
  ecocompteurs_sub <- subset(ecocompteurs_raw, grepl("_MENSUEL", ecocompteurs_raw$filename)==F)

  ### FORMAT
  ecocompteurs_sub$Heure <- substr(ecocompteurs_sub$date, 12, 13)
  ecocompteurs_sub$MoisNum <- ecocompteurs_sub$mois %>% substr(., 1, 2) %>% as.numeric(.)
  if(COUNTRY=="FR"){
    ecocompteurs_sub$Mois <- Mois_noms$Nom_complet[match(ecocompteurs_sub$MoisNum, Mois_noms$Num)] %>% factor(., levels=Mois_noms$Nom_complet)
  } else {
    ecocompteurs_sub$Mois <- Mois_noms$Nom_italien[match(ecocompteurs_sub$MoisNum, Mois_noms$Num)] %>% factor(., levels=Mois_noms$Nom_italien)
  }
  ecocompteurs_sub$Jour <- as.Date(ecocompteurs_sub$date)
  ecocompteurs_sub$Julian <- format(ecocompteurs_sub$Jour, "%j")
  ecocompteurs_sub$Week <- format(as.Date(ecocompteurs_sub$Julian, "%j"), "%U") # A partir de Julian pour que le jour 7 soit toujours dans la meme semaine (sinon ca varie d'une annee sur l'autre)
  ecocompteurs_sub$Year <- format(ecocompteurs_sub$Jour, "%Y")
  ecocompteurs_sub$donnees <- as.numeric(ecocompteurs_sub$donnees)

  ### PLOT HORAIRE
  eco_heure <- ecocompteurs_sub %>%
    dplyr::group_by(compteur, Mois, Heure) %>%
    dplyr::summarise(Passages=mean(as.numeric(donnees), na.rm=T)) %>%
    subset(., Mois %in% c("Mai", "Juin", "Juillet", "Aout", "Septembre", "Octobre", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre"))

  # Supprimer les compteurs avec seulement des donnees a 00h (ce sont les compteurs qui ont des donnees rassemblees par jours)
  eco_heure <- subset(eco_heure, compteur %in% eco_heure$compteur[eco_heure$Heure=="01"])

  # plot
  if(nrow(eco_heure)>0){
    G_Heure <- ggplot(eco_heure)+
      geom_point(aes(x=as.numeric(Heure), y=compteur, size=Passages))+ # Bidouille pour que la legende de la couleur ne s'affiche pas
      geom_point(aes(x=as.numeric(Heure), y=compteur, size=Passages, color=Passages), show.legend=F)+
      ylab("")+
      scale_size_continuous(name=ifelse(COUNTRY=="FR", "Passages par heure", "Passaggi all'ora"))+
      scale_color_gradient(low="white", high="darkred", trans="sqrt")+
      xlab(ifelse(COUNTRY=="FR", "Heure", "Ora"))+
      facet_wrap(~Mois, ncol=3)+
      theme_minimal()+theme(legend.position="bottom")
  } else {G_Heure=NULL}

  ### PLOT SAISON
  eco_saison <- ecocompteurs_sub %>%
    subset(., is.na(donnees)==F) %>%
    dplyr::group_by(compteur, Week, Julian, Year) %>%
    dplyr::summarise(Passages=sum(as.numeric(donnees), na.rm=T)) %>% # Somme des comptages par journee
    dplyr::group_by(compteur, Week) %>%
    dplyr::summarise(Passages=mean(Passages, na.rm=T)) %>% # Moyenne des comptages journaliers en moyennant sur les differentes annees et les semaines
    subset(., Passages>0.5)

  # Empecher ecriture scientifique en y
  plain_fun <- function(x,...) {
    format(x, ..., scientific = FALSE, drop0trailing = TRUE)
  }

  G_Saison <- ggplot(eco_saison)+
    geom_line(aes(x=as.numeric(Week)*7, y=Passages, group=compteur))+
    ylab(ifelse(COUNTRY=="FR", "Passages par jour", "Passaggi quotidiani"))+
    xlab(ifelse(COUNTRY=="FR", "Jour", "Giorno"))+
    facet_wrap(~compteur)+
    scale_y_continuous(trans="log10", labels=plain_fun)+
    theme_minimal()

  return(list(
    G_Heure=G_Heure,
    G_Saison=G_Saison
    ))
}



BTA_CarteComparaison <- function(Comparison_results_raw, ColName, COUNTRY){

  Comparison_results_raw$Col_toplot <- as.numeric(as.data.frame(Comparison_results_raw)[,ColName])
  Comparison_results <- subset(Comparison_results_raw, is.na(Comparison_results_raw$Col_toplot)==F) # Supprime les parcs italiens pour Outdoor

  # Preparer les labels
  Y_nudge <- 14000*ifelse(Comparison_results$Parc %in% c("PNMercantour", "Alpi_Liguri", "Gran_Paradiso"), -1, 1)
  X_nudge <- 14000*(Comparison_results$Parc %in% c("Gran_Paradiso"))
  
  # Preparer le titre
  Titre <- revalue(ColName, c(
    "strava_meanCell"=ifelse(COUNTRY=="FR", "Strava (moyenne)", "Strava (media)"),
    "strava_maxCell"=ifelse(COUNTRY=="FR", "Strava (maximum)", "Strava (massimo)"),
    "strava_Q95Cell"="Strava (Q95%)",
    "outdoor_meanCell"=ifelse(COUNTRY=="FR", "Outdoorvision (moyenne)", "Outdoorvision (media); Solo Francia"),
    "outdoor_maxCell"=ifelse(COUNTRY=="FR", "Outdoorvision (maximum)", "Outdoorvision (massimo); Solo Francia"),
    "outdoor_Q95Cell"=ifelse(COUNTRY=="FR", "Outdoorvision (Q95%)", "Outdoorvision (Q95%); Solo Francia"),
    "iNat_meanCell"=ifelse(COUNTRY=="FR", "iNaturalist (moyenne)", "iNaturalist (media)"),
    "iNat_Q95Cell"="iNaturalist (Q95%)",
    "iNat_maxCell"=ifelse(COUNTRY=="FR", "iNaturalist (maximum)", "iNaturalist (massimo)"),
    "eco_MeanEstival"=ifelse(COUNTRY=="FR", "Eco-compteur (moyenne)", "Contatori (media)"),
    "eco_MaxEstival"=ifelse(COUNTRY=="FR", "Eco-compteur (maximum)", "Contatori (massimo)"),
    "eco_MeanJA"=ifelse(COUNTRY=="FR", "Eco-compteur juillet-aout (moyenne)", "Contatori luglio-agosto (media)"),
    "eco_MaxJA"=ifelse(COUNTRY=="FR", "Eco-compteur juillet-aout (maximum)", "Contatori luglio-agosto (massimo)")
  ), warn_missing = F)

  # Plot
  G <- ggplot(st_centroid(Comparison_results))+
    geom_sf(data=Comparison_results_raw, fill="grey80", colour="grey80")+
    geom_sf(aes(col=Col_toplot), size=10)+
    geom_sf_text(aes(label=Parc), position=position_nudge(x=X_nudge, y=Y_nudge))+
    scale_color_viridis_c(name=ifelse(COUNTRY=="FR", "Fréquentation", "Frequentazione"))+
    ggtitle(Titre)+
    theme_void()+theme(plot.title=element_text(size=18, face="bold"))

return(G)
}


### Renvoie les interpretations preparees pour etre integrees dans le RMD
BTA_Interpretations <- function(PARC_nom){

  Interpretation_parc <- data.frame(Description="En rédaction par Juliette", Frequentation="A REDIGER", Temporel="A REDIGER")


  ### Description Juliette
  if(PARC_nom=="PNEcrins") {Interpretation_parc$Description <- "Le Parc national des Écrins est une aire protégée en France, à cheval sur les régions Provence-Alpes-Côte d’Azur et Auvergne-Rhône-Alpes (Hautes Alpes et Isère). C’est une aire de haute montagne, culminant à 4102 mètres avec la Barre des Écrins. <br><br>Crée en 1973, le Parc national des Écrins est composé de deux zones distinctes : une zone cœur, d’une superficie de 93 000 ha, avec une réglementation particulière, et d’une aire d’adhésion de 16 600 km^2 regroupant 49 communes qui ont adhéré à la charte, un projet de territoire pour 15 ans. Il s’étend sur sept vallées : Oisans, Valbonnais, Valgaudemar, Champsaur, Embrunais, Vallouise et Briançonnais. <br><br>Il abrite la première réserve intégrale de France créée en 1995, la réserve du Lauvitel.<br><br>Le site internet du Parc : https://www.ecrins-parcnational.fr/. <br><br>L'analyse inclut l'ensemble de l'aire optimale d'adhésion du Parc national des Ecrins."}
  if(PARC_nom=="PNMercantour") {Interpretation_parc$Description <- "Le Parc national du Mercantour est une aire protégée en France située dans la région Provence-Alpes-Côte d’Azur (Alpes-Maritime et Alpes-de-Haute-Provence), et adossée à la frontière italienne. A proximité de la Méditerranée, son plus haut sommet, le Gélas, culmine à 3143 mètres d’altitude. <br><br>Crée en 1979, le Parc national du Mercantour est composé de deux zones distinctes : une zone cœur, d’une superficie de 68 000 ha, avec une réglementation particulière, et d’une aire d’adhésion de 1212 km^2 regroupant 24 communes qui ont adhéré à la charte, un projet de territoire pour 15 ans. Il s’étend sur huit vallées : Roya, Vésubie, Bévéra, Tinée, Cians, Var, Verdon et Ubbaye. <br><br>Cette aire est frontalière à l’Aire protégée d’Alpi Marittime, avec laquelle ils forment le premier parc européen du massif alpin depuis 2013 (GECT).<br><br>Le site internet du Parc : https://www.mercantour-parcnational.fr/fr.  <br><br>L'analyse inclut l'ensemble de la zone coeur et de la zone d'adhésion."}
  if(PARC_nom=="PNVanoise") {Interpretation_parc$Description <- "Le Parc national de la Vanoise est une aire protégée en France, située dans la région Auvergne-Rhône-Alpes (Savoie). C’est le premier parc national de France, créé en 1963, dont le sommet culminant, La Grand Casse, est à 3855 mètres. <br><br>Crée en 1963, le Parc national de la Vanoise est composé de deux zones distinctes : une zone cœur, d’une superficie de 53 300 ha, avec une réglementation particulière, et d’une aire d’adhésion de 20 800 ha regroupant 2 communes qui ont adhéré à la charte, un projet de territoire pour 15 ans. Le Parc national s’étend sur deux vallées, la Maurienne et la Tarentaise, est il est également gestionnaire de quatre réserves naturelles nationales, la Grand Sassière, le Plan de Tuéda, Tignes-Champgny, et Bailletaz. <br><br>Le Parc national de la Vanoise jouxte celui du Grand Paradis en Italie, formant un vaste espace protégé en Europe sur 1250 km^2.<br><br>Le site internet du Parc : https://vanoise-parcnational.fr/fr. <br><br>L'analyse inclut la zone coeur du Parc national de la Vanoise ainsi que les réserves naturelles adjacentes."}
  if(PARC_nom=="RNAsters") {Interpretation_parc$Description <- "Les réserves naturelles de Haute-Savoie sont composées de 9 réserves (Vallon de Bérard, Carlaveyron, Aiguilles Rouges, Passy, Sixt-Fer-à-Cheval Passy, Roc de Chère, Delta de la Dranse, Contamines-Montjoie), et de 2 périmètres de protection (Delta de la Dranse, Bout du Lac). <br><br>Ces espaces protégés sont gérés par Asters, le Conservatoire d’espaces naturels de Haute-Savoie. Ils sont d’une superficie d’environ 21 300 ha, avec une réglementation particulière.  <br><br>Le site internet de la réserve du gestionnaire : https://www.cen-haute-savoie.org/les-reserves-naturelles/ "}
  if(PARC_nom=="PNRVercors") {Interpretation_parc$Description <- "Le Parc naturel régional du Vercors est une aire protégée en France située dans la région Auvergne-Rhône-Alpes (Isère et Drôme). Caractérisé par ses hauts plateaux, cette aire de moyenne montagne regroupe 98 communes, avec 6 villes portes (Crest, Grenoble, Mens, Romans-sur-Isère, Saint-Marcellin, Vinay), sur une superficie de 227 621 ha. <br><br>Crée en 1970, le Parc comprend plusieurs secteurs naturels : le Piémont nord, les Quatre-Montagnes, les Coulmes, le Diois, le Vercors Drômois, le Royans, le Trièves, la Gervanne, les Raye et Monts du Matin, le Pays de Saillans. <br><br>Le site internet du Parc : https://www.parc-du-vercors.fr/"}
  if(PARC_nom=="PNRChartreuse") {Interpretation_parc$Description <- "Le Parc naturel régional de Chartreuse est une aire protégée en France située dans la région Auvergne-Rhône-Alpes (Savoie et Isère). Crée en 1995, le Parc est composé de 72 communes avec 3 villes-portes (Chambéry, Voiron, Grenoble) sur une superficie de 86 000 ha. <br><br>C’est une parc de moyenne montagne, dont le point culminant est à 2082 mètres (Chamechaud). Il est également composé de la réserve naturelle des hauts de Chartreuse. <br><br>Le site internet du Parc : https://www.parc-chartreuse.net/ "}
  if(PARC_nom=="PNRQueyras") {Interpretation_parc$Description <- "Le Parc naturel régional du Queyras est une aire protégée située en France dans la région Provence-Alpes-Côte d’Azur (Hautes Alpes). Crée en 1977, c’est une zone de montagne regroupant 10 communes sur une superficie d’environ 57 400 ha. <br><br>Le Parc régional porte avec le Parco del Monviso, la Réserve de Biosphère transfrontière du Mont Viso. Créée en 2013 et reconnue comme réserve transfrontière en 2014, son territoire s’étend sur 109 communes des Alpes Cottiennes piémontaises, dont 88 en Italie et 21 en France. Cette réserve est un territoire reconnu au niveau mondial par l’UNESCO du programme ‘Man and Biosphère’. <br><br>Le site internet du Parc : https://www.pnr-queyras.fr/ et de la réserve de biosphère : https://www.pnr-queyras.fr/monviso-biosphere-unesco/ "}
  if(PARC_nom=="PNRBauges") {Interpretation_parc$Description <- "Le Parc naturel régional du Massif des Bauges est une aire protégée en France située dans la région Auvergne-Rhône-Alpes (Savoie et Haute-Savoie). D’une superficie de 90 000 ha, le parc est composé de 71 communes, avec ses 5 villes portes de Chambéry, Aix-les-Bains, Rumilly, Annecy, Ugine, et Albertville. <br><br>Depuis 2011 le Parc est reconnu par le label Géoparc mondial de l’UNESCO, soulignant sa richesse géologique du territoire. <br><br>Le site internet du Parc : https://parcdesbauges.com/ "}

  if(PARC_nom=="Alpi_Cozie") {Interpretation_parc$Description <- "L’aire protégée d’Alpi Cozie est située en Italie dans le Piémont, dont l’entité de gestion, l’Organisme de gestion d’Alpi Cozie, a été crée en 2012. <br><br>D’une superficie d’environ 35 000 ha, elle est composée de quatre parcs (Laghi di Avigliana, Orsiera Rocciavré, Gran Bosco di Salbertrand, et Val Troncea), de deux réserves (Orrido di Chianocco, et Orrido di Foresto), et de douze sites Natura 2000.<br><br>Le site internet de l’aire protégée : https://www.parchialpicozie.it/it/"}
  if(PARC_nom=="Alpi_Liguri") {Interpretation_parc$Description <- "Le Parc natural régionale Alpi Liguri est l’aire protégée d’Italie la plus occidentale de la Ligurie. Situé dans la province d’Imperia, au nord-ouest de l’Italie, il est au contact de la frontière française, et est composé de trois vallées : le bassin du torrent Nervia, la haute vallée Argentina, et la haute vallée d’Arroscia. <br><br>Créé en 2007, il est d’une superficie d’environ 6 000 ha. C’est une zone de moyenne montagne à proximité de la Méditerranée, et une zone de haute montagne avec son plus grand sommet, le mont Saccarello (2200 mètres).<br><br>Le site internet du Parc : https://parconaturalealpiliguri.it/ "}
  if(PARC_nom=="Alpi_Marittime") {Interpretation_parc$Description <- "L’aire protégée Alpi Marittime est située en Italie dans les Alpes Maritimes, dans le Piémont, dont l’entité de gestion, l’Organisme de gestion des espaces protégés des Alpes maritimes, a été créée en 2015. <br><br>D’une superficie d’environ 100 0000 ha, elle est constituée de deux parcs, l'aire protégée d'Alpi Marittime et le Parc naturel Marguareis, de huit réserves (Grotte del Bandito, Ciciu del Villar, Benevagienna, Grotte di Bossea, Grotte di Aisone, di Crava-Morozzo, Sorgenti del Belbo, et Rocca San Giovanni-Saben), et de vingt site Natura 2000. <br><br>Cette aire est frontalière au Parc national du Mercantour, avec lequel ils forment le premier parc européen du massif alpin depuis 2013 (GECT), et jouxte le parc des Alpes ligures, formant ainsi l’un des plus grand espaces naturels protégés d’Europe.<br><br>Le site internet de l’aire protégée : https://www.areeprotettealpimarittime.it/"}
  if(PARC_nom=="Gran_Paradiso") {Interpretation_parc$Description <- "Le Parc national du Grand Paradis est une aire protégée italienne située dans les Alpes Graies, entre la Vallée d’Aoste et le Piémont, autour du massif du Grand Paradis (avec le seul sommet à dépasser plus de 4 000 mètres en Italie). <br><br>D’environ 70 000 ha, c’est l’un des plus grands parcs italiens et le plus ancien parc national du pays (créé en 1922). Il a joué un rôle majeur dans la sauvegarde du Bouquetin Alpin, symbole du Parc aujourd’hui. <br><br>Le Parc national du Grand Paradis jouxte celui de la Vanoise en France, formant un vaste espace protégé en Europe sur 1250 km^2.<br><br>Le site internet du Parc : https://www.pngp.it/"}
  if(PARC_nom=="Ossola") {Interpretation_parc$Description <- "L’aree protette dell’Ossola est une aire protégée située en Italie dans le Piémont, et plus particulièrement dans la Val d’Ossola, dont la gestion est assurée par l’entité de gestion delle Aree Protette dell’Ossola. <br><br>Cette aire protégée, crée en 2009, et d’une superficie d’environ 16 000 ha, regroupe deux parcs, le Parc Alpe Veglia e Alpe Devero, ainsi que le Parc Alta Valle Antrona.<br><br>Le site internet de l’aire protégée : https://www.areeprotetteossola.it/it/"}
  if(PARC_nom=="Mont-Avic") {Interpretation_parc$Description <- "Le Parc naturel régional du Mont Avic est situé en Italie dans le Val d’Aoste. Crée en 1989, il est composé du haut bassin du Chalamy (commune de Champdepraz), de la vallée de Champorcher, et du Val Clavalité (commune de Fénis). <br><br>D’une superficie d’environ 7 300 ha, avec une altitude allant jusqu’à 3185 mètres, le Parc est en bordure du Parc national du Grand Paradis, et est caractérisé par de nombreuses zones humides et lacs.<br><br>Le site internet du Parc : https://montavic.it/ "}

  ### Frequentation
  if(PARC_nom=="PNEcrins") {Interpretation_parc$Frequentation <- "Pour le Parc national des Ecrins, les données Strava, Outdoorvision et iNaturalist sont fortement corrélées (ce qui n'est pas le cas de l'ensemble des espaces protégés considérés dans cette analyse), donnant une forte confiance dans les résultats. Ces cartes permettent de constater que les zones autour des Deux-Alpes, du Puy Saint Vincent, du Monetier les Bains, d’Orcières et d’Embrun sont particulièrement fréquentées, tout comme les secteurs du Pré de Mme Carle, du Gioberney et du Lauvitel en coeur de parc."}
  if(PARC_nom=="PNMercantour") {Interpretation_parc$Frequentation <- "Les données Strava, Outdoorvision et iNaturalist dessinent de cartographies de fréquentation très similaires, donnant une forte confiance en la carte de fréquentation standardisée. On y voit que les zones de forte fréquentation sont réparties sur l'ensemble du parc. On peut citer par exemple les alentours du Mont Pelat et lac d'Allos, de Valberg, de Barcelonnette, ou les sentiers montant à la crête italienne à l'Est du parc."}
  if(PARC_nom=="PNVanoise") {Interpretation_parc$Frequentation <- "Les données Strava, Outdoorvision, et iNaturalist mettent toutes les trois en avant les mêmes zones comme les plus fréquentées du Parc national de la Vanoise : la réserve naturelle du plan de Tuéda, les environs de l'aiguille de la Vanoise et l'auberge de Bellecombe, et la réserve naturelle de la Grande Sassière. Ces mêmes zones se retrouvent sur la carte par itinéraires de randonnée, où l'on retrouve que les sentiers les plus empruntés sont la 'Balade nature vers la Réserve naturelle nationale de Tueda', la 'Route du sel et des fromages' et le 'Tour de l’Aiguille de la Vanoise par le Col de la Vanoise'. Du fait de la concordance des trois sources de données, les cartes de fréquentation standard (par raster et par chemin) peuvent être utilisées avec une bonne confiance. <br>Les 16 éco-compteurs dont dispose le parc sont bien positionnés sur les zones à forte fréquentation."}
  if(PARC_nom=="RNAsters") {Interpretation_parc$Frequentation <- "Les zones les plus fréquentées des Réserves Naturelles de Haute-Savoie semblent se concentrer au Nord de la réserve de Sixt-Passy (Tour des Dents Blanches), à l'Est de la réserve des Aiguilles rouges (sur l'itinéraire du Tour du Mont-Blanc), et au Sud-Ouest de la réserve des Contamines Montjoie. Ces zones semblent être mises en évidence de façon cohérente par les données Strava, Outdoorvision et iNaturalist, leur conférant une forte pertinence.<br> Il est à noter que l'analyse déployée ici n'est peut-être pas particulièrement adaptée aux petits espaces protégés (comme le Delta de la Dranse, le Roc de Chère ou le Bout du Lac d'Annecy) et qu'il est donc difficile de réellement comparer les différentes réserves entre elles."}
  if(PARC_nom=="PNRVercors") {Interpretation_parc$Frequentation <- "Les secteurs les plus fréquentés, d'après les trois jeux de données utilisés, se concentrent au Nord Est du parc, notamment autour de Fontaine, Lans en Vercors et Villard de Lans. Plus au Sud, les randonnées autour du Grand Veymont concentrent également une forte fréquentation."}
  if(PARC_nom=="PNRChartreuse") {Interpretation_parc$Frequentation <- "La fréquentation du Parc naturel régional de la Chartreuse est fortement influencée par les villes à proximité comme on peut le voir sur la carte de fréquentation standardisée où les zones les plus fréquentées qui se dessinent sont à proximité de Grenoble, Voiron et Chambéry. Cela met en évidence qu'une part importante des visites du parc sont des visites ponctuelles des habitants de la région. Plus au centre du parc, nous pouvons citer les alentours du Monastère de la Grande Chartreuse et les crêtes menant à Chamechaude par l'Ouest comme des zones fortement fréquentées. Il est intéressant de noter que les trois jeux de données considérés ici dessinent des cartographies assez différentes (ce qui n'est pas la cas sur les autres parcs considérés dans cette analyse), avec un profil plutôt péri-urbain pour Strava et mieux réparti sur l'ensemble du parc pour Outdoorvision et iNaturalist; cela apporte une forte incertitude à la carte de fréquentation standardisée."}
  if(PARC_nom=="PNRQueyras") {Interpretation_parc$Frequentation <- "Les données Strava et Outdoorvision pointent vers les mêmes zones et sentiers comme étant les plus fréquentés et qui sont concentrés sur la partie Sud du parc, notamment autour de Ceillac qui concentre les cinq sentiers les plus fréquentés, Saint-Véran et du refuge Agnel. Le secteur de Brunissard semble également fortement fréquenté. La répartition des observateurs naturalistes correspond en partie à cette distribution, mais avec une forte concentration d'observateurs sur le secteur de Molines en Queyras."}
  if(PARC_nom=="PNRBauges") {Interpretation_parc$Frequentation <- "Les données Strava et Outdoorvision identifient deux grandes zones de haute fréquentation : les abords du lac d'Annecy au Nord (notamment sur les itinéraires autour de Duingt) et la zone entre le Molard de la Gaillarde et la Croix du Nivolet à l'Ouest. Les données iNaturalist sont nombreuses dans ces mêmes zones, ainsi que l'ensemble des sentiers du parc."}

  if(PARC_nom=="Alpi_Cozie") {Interpretation_parc$Frequentation <- "Les données Strava et iNaturalist dessinent des cartes de fréquentation assez similaires, ce qui suggère une bonne fiabilité de la carte de fréquentation standardisée (même si certaines zones ressortent comme plus fréquentées d'après Strava). Cette carte montre notamment que la zone autour du Lago Grande et du Lago Piccolo est la plus fréquentée d'Alpi Cozie. Chacun des autres secteurs d'Alpi Cozie présente au moins un secteur à forte fréquentation, par exemple à proximité de Duc, de Sauze d'Oulx, ou sur la bordure du Parco Naturale Orsiera Rocciavré."}
  if(PARC_nom=="Alpi_Marittime") {Interpretation_parc$Frequentation <- "Les données Strava et iNaturalist dessinent des profils assez similaires avec une forte fréquentation autour des lacs du Sud Ouest du parc, notamment autour de Valscura, mais également autour des lacs de la Sella ou autour de l'éco-compteur Palanfre. La localisation des éco-compteurs semble très adaptée à la cartographie de fréquentation, ils sont très bien positionnés."}
  if(PARC_nom=="Gran_Paradiso") {Interpretation_parc$Frequentation <- "Les données Strava et iNaturalist dessinent des cartes de fréquentation assez similaires, montrant une fréquentation importante sur la diagonale entre le Lago Serru et Cogne. A l'inverse, les parties Nord Ouest et Sud Est du parc sont peu fréquentées."}
  if(PARC_nom=="Ossola") {Interpretation_parc$Frequentation <- "Les données Strava et iNaturalist pointent vers des zones de forte fréquentation très similaires, on peut donc leur faire confiance. La montée au Lago di Devero, la montée vers l'Alpe Veglia par la Cappella del Groppallo (sentier F10), et le secteur entre le Lago di Antrona et le Lago di Campiccioli semblent particulièrement fréquentés."}
  if(PARC_nom=="Mont-Avic") {Interpretation_parc$Frequentation <- "Les données Strava et iNaturalist dessinent des cartes de fréquentation relativement similaire, suggérant un niveau de confiance important pour la carte de fréquentation standard. On peut toutefois noter que la zone autour de l'éco-compteur Serva semble plus fréquentée d'après les données iNaturalist que d'après les données Strava. C'est la montée depuis l'éco-compteur Cort et la zone autour de l'éco-compteur Miserin qui semblent les plus fréquentées."}
  
  ### Temporel
  if(PARC_nom=="PNEcrins") {Interpretation_parc$Temporel <- "Comme cela était constaté dans le rapport de la prestation précédente centré sur le Parc national des Ecrins, le pic des données iNaturalist se situe entre début juillet et mi-août, ce qui correspond également au pic mesuré avec les éco-compteurs. En comparant avec les autres parcs, dont certains comme le PNR des Bauges ou de la Chartreuse ont un pic de fréquentation beaucoup plus étendu sur le printemps et l'été, on constate que la fréquentation du Parc national des Ecrins est fortement concentrée sur les deux mois d'été. Le graphique montrant les horaires de fréquentation des éco-compteurs montre des données probablement erronées pour les éco-compteurs 'LacDouche' et 'Danchere' avec des données très abondantes en pleine nuit, notamment au mois de Mai."}
  if(PARC_nom=="PNMercantour") {Interpretation_parc$Temporel <- "Comme les parcs nationaux de la Vanoise et des Ecrins, le Mercantour a un gros pic de fréquentation entre début juillet et mi-août (bien qu'un point haut soit observé le 20 avril). C'est également ce que montrent les éco-compteurs avec une grande cohérence entre les éco-compteurs (ce qui n'est pas le cas pour tous les parcs) qui mettent en avant un pic de fréquentation début août."}
  if(PARC_nom=="PNVanoise") {Interpretation_parc$Temporel <- "En toute logique, le Parc national de la Vanoise est principalement fréquenté lors des mois de juillet et août, avec un pic de fréquentation entre le 5 juillet et le 24 août (ce qui est comparable aux pics des parcs voisins) avec une basse saison très marquée de fin septembre à avril. On note toutefois des pics de données importants à l'éco-compteur de la RNN Tueda autour de décembre-janvier et de mars-avril (il faudrait se pencher sur les données brutes que ce n'est pas une erreur dans les données de ce compteur). Au niveau horaire, la majorité des éco-compteurs comptent du passages sur l'ensemble de la journée, mais une exception semble se faire aux compteurs du Lac Rond et du Lac des Vaches, qui voient une grande concentration des passages entre 11h et 15h, probablement du fait de leur altitude; et possiblement au fait que les données collectées sur ces éco-compteurs l'ont été sur une courte période incluant uniquement 2025."}
  if(PARC_nom=="RNAsters") {Interpretation_parc$Temporel <- "La fréquentation des réserves naturelles de Haute Savoie se concentre principalement en juillet et août mais avec une fréquentation importante de mai à octobre (si l'on compare par exemple aux Parc nationaux de la Vanoise, des Ecrins et du Mercantour qui ont une saison plus recentrée sur l'été). Les données horaires indiquent des erreurs pour l'éco-compteur 'RNBDL_Parking' qui compte de nombreux passages en pleine nuit, voir par exemple les données du 29 mai 2013). Elles montrent également des horaires de passage assez variés entre les éco-compteurs, parfois très larges (BassinGrandPre), parfois très resserrés (DesertBlanc_piste), ou parfois décalés sur l'après-midi (RNBDL_plage)."}
  if(PARC_nom=="PNRVercors") {Interpretation_parc$Temporel <- "La période de haute fréquentation s'étale de mai à août (d'après les données iNaturalist et les données éco-compteurs) et varie fortement d'un éco-compteur à l'autre. En effet, l'éco-compteur 'routiermoliere' montre une forte fréquentation sur l'ensemble de cette période, là où les éco-compteurs 'Moliere-Aginaux' et 'ENS-Urle' présentent des pics estivaux importants, le premier étant plus tôt que le second (voir la carte interactive; les graphiques de cette section n'incluant pas les éco-compteurs pour lesquels les données envoyées étaient résumées mois par mois). Les éco-compteurs sauvegardant les horaires de passage sont un peu trop rares pour en tirer des conclusions."}
  if(PARC_nom=="PNRChartreuse") {Interpretation_parc$Temporel <- "La période de haute fréquenation est très étalée, entre avril et octobre (principalement avril à août) que ce soit d'après les données iNaturalist ou les données éco-compteurs. Un pic en juillet-août est toutefois visible sur les données d'éco-compteurs, notamment au Charmant-som (qui est l'éco-compteur le plus fréquenté de tous les éco-compteurs des 13 espaces protégés considérés dans cette analyse; voir section Comparaison inter-régionale). Les données horaires mettent en avant deux pics horaires pour les compteurs du Col de l'Alpette et de Ruine_Baton (un pic le matin, un pic l'après-midi) tandis que les passages se concentrent autour de midi pour le Sentier Azil et Magda.<br>Il est à noter que les données de 2011 à 2013 de l'éco-compteur du col de l'Alpette ont été supprimées manuellement car elles semblaient posséder de nombreuses erreurs."}
  if(PARC_nom=="PNRQueyras") {Interpretation_parc$Temporel <- "La période de haute fréquentation du Parc naturel régional du Queyras est très recentrée sur les mois de juillet et août (que ce soit d'après les données iNaturalist ou la quasi-totalité des éco-compteurs). Ce profil ressemble plus au profil des Parcs nationaux voisins (Ecrins, Vanoise, Mercantour) qui ont une fréquentation très estivale, qu'au profil des autres PNRs (Bauges, Chartreuse, Vercors) qui ont une fréquentation plus étalée de mai à septembre. Le compteur du Grand Belvédère, ruedesmasques et de Cascade_pisse font exception puisqu'ils présentent des fréquentation relativement importantes en juin et septembre. On observe une forte différenciation des éco-compteurs d'un point de vue horaire, avec certains où les passagent commencent très tôt (ex : Grand_Belvedere), d'autres avec deux pics journaliers (au passage matinal et au passage d'après-midi; ex : Cascade_pisse) et d'autres où les données sont très concentrées en milieu de journée (ex: LacSoulier). On peut également observer des variations saisonnières pour chaque éco-compteur : le Grand-belvedere est fréquenté majoritairement en matinée en juin, toute la journée en été, et l'après-midi en septembre."}
  if(PARC_nom=="PNRBauges") {Interpretation_parc$Temporel <- "La majorité des observateurs iNaturalist visitant le PNR des Bauges se concentrent au printemps (d'avril à août), contrairement aux parcs nationaux voisins (par exemple celui de la Vanoise) où la fréquentation est très concentrée sur les mois de juillet et août. Les données éco-compteurs confirment une fréquentation très étalée sur le printemps et l'été, même si le compteur 'Aillon Spéléo-rando' montre un petit pic au coeur de l'été, et que le compteur montre un pic important autour de février."}

  if(PARC_nom=="Alpi_Cozie") {Interpretation_parc$Temporel <- "Les visites d'Alpi Cozie par des observateurs iNaturalist s'étalent sur une grande partie de l'année, avec un pic notable de mi-avril à août, qui se poursuit par une fréquentation importante jusqu'à fin octobre. Les observations ne sont pas rares non plus en hiver."}
  if(PARC_nom=="Alpi_Marittime") {Interpretation_parc$Temporel <- "Les visites par des observateurs d'iNaturalist sont principalement concentrées entre début juin et début septembre; elles resent cependant courantes entre mars et novembre (par rapport par exemple au Parc national du Mercantour qui a un pic estival bien plus marqué). Elles sont rares en hiver, entre novembre et mars, mais pas inexistantes. Les données d'éco-compteurs semblent indiquer la même chose, bien qu'elles ne couvrent pas la période hivernale pour la plupart, parfois avec des pics très marqués comme celui de Valasco. Les données brutes de l'éco-compteur Carnino semblent indiquer une erreur (des données arbitrairement datées au 1er janvier ?)."}
  if(PARC_nom=="Gran_Paradiso") {Interpretation_parc$Temporel <- "Les visites par des observateurs d'iNaturalist se concentrent principalement entre mi-juin et fin août. Elles restent cependant régulières sur l'ensemble de l'année (tandis qu'elles sont très rares en hiver à Ossola par exemple)."}
  if(PARC_nom=="Ossola") {Interpretation_parc$Temporel <- "Les visites d'Ossola par des observateurs iNaturalist se concentrent principalement entre mi-mai et août, et se poursuivent jsuqu'à mi-octobre. Les observations sont extrêmement rares en hiver, bien plus que dans d'autres parcs voisins."}
  if(PARC_nom=="Mont-Avic") {Interpretation_parc$Temporel <- "Les données iNaturalist sont trop peu nombreuses pour vraiment étudier la dynamique saisonnière des observations. En revanche, les éco-compteurs nous indiquent une fréquentation assez étalée sur le printemps et l'été, bien synchrone entre les quatre éco-compteurs. L'éco-compteur CORT montre clairement deux passages journaliers en été (un en matinée et un dans l'après-midi) tandis qu'on observe plutôt un passage continu à Serva, Crest, ou Miserin (ce dernier présentant de nombreuses données nocturnes en septembre, probablement des erreurs de données)."}
  

  ### Return
  return(Interpretation_parc)

}



