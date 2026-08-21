

### Lire analysed data ----
Analysed_data <- list()

for(PARC_nom in parclist){
  Analysed_data$new <- readRDS(paste0("2.Outputs/2.Analysed_data/Analysed_data_", PARC_nom, ".rds"))
  names(Analysed_data)[which(names(Analysed_data)=="new")] <- PARC_nom
}


### Grouper shp pour carte ----
AllParcs <- Analysed_data[[1]]$PARC_shp

for(i in 2:length(Analysed_data)){
  AllParcs <- rbind.fillSF(AllParcs, Analysed_data[[i]]$PARC_shp)
}

AllParcs <- st_make_valid(AllParcs)

ggplot(AllParcs)+
  geom_sf(aes(fill=Parc), colour=NA)+
  geom_sf_text(aes(label=Parc))+
  theme_minimal()



### Graphique
AllParcs$Freq <- rnorm(n=nrow(AllParcs), mean=50, sd=15)
Y_nudge <- 14000*ifelse(AllParcs$Parc %in% c("PNMercantour", "Alpi_Liguri", "PNVanoise", "Gran_Paradiso"), -1, 1)

ggplot(st_centroid(AllParcs))+
  geom_sf(data=AllParcs, fill="grey90", colour="grey80")+
  geom_sf(aes(col=Freq), size=10)+
  geom_sf_text(aes(label=Parc), position=position_nudge(y=Y_nudge))+
  scale_color_viridis_c()+
  theme_void()

# Verifier que je n'ai qu'une ligne par parc
table(table(AllParcs$Parc)==1)

# Calculer surface
AllParcs$Area <- st_area(AllParcs) %>% units::set_units(., "km2") %>% as.numeric()

# Extraire si outdoor ou pas + longueur sentier
AllParcs$Run_outdoor <- NA
AllParcs$Length_Sentiers <- NA

for(PARC_nom in AllParcs$Parc){
  AllParcs$Run_outdoor[AllParcs$Parc==PARC_nom] <- Analysed_data[[PARC_nom]]$Run_Outdoor
  if(Analysed_data[[PARC_nom]]$Run_Sentiers==T){
    AllParcs$Length_Sentiers[AllParcs$Parc==PARC_nom] <- Analysed_data[[PARC_nom]]$Sentiers %>% st_union() %>% st_length() %>% units::set_units(., "km")
  }
}



### Comparaison ecocompteurs ----
AllParcs$eco_N <- NA
AllParcs$eco_MeanEstival <- NA
AllParcs$eco_MaxEstival <- NA
eco_stock <- data.frame()

for(PARC_nom in AllParcs$Parc[! AllParcs$Parc %in% c("Alpi_Cozie", "Ossola")]){

  ### Tout l'ete
  eco_estival <- Analysed_data[[PARC_nom]]$ecocompteurs_quinz %>%
    subset(., Mois %in% c("06", "07", "08", "09") & (as.numeric(Annee)>2020))

  # Verifier qu'on a a peu pres le meme nombre de jours par mois (sinon la moyenne n'est pas comparable entre un parc avec juin-sept et un parc avec juillet-aout seulement)
  tapply(eco_estival$Nb_jours, eco_estival$Mois_nom, sum)

  # Calculer le nombre de passages par jour par ecocompteur
  eco_cpts <- eco_estival  %>%
    dplyr::group_by(compteur) %>%
    dplyr::summarise(Nb_jours=sum(Nb_jours, na.rm=T), Value=sum(Value, na.rm=T)) %>%
    mutate(Passages_par_jour=Value/Nb_jours)

  # Ajouter les stats dans le tableau final
  AllParcs$eco_N[AllParcs$Parc==PARC_nom] <- length(eco_cpts$Passages_par_jour[is.na(eco_cpts$Passages_par_jour)==F])
  AllParcs$eco_MeanEstival[AllParcs$Parc==PARC_nom] <- mean(eco_cpts$Passages_par_jour, na.rm=T)
  AllParcs$eco_MaxEstival[AllParcs$Parc==PARC_nom] <- max(eco_cpts$Passages_par_jour, na.rm=T)


  ### Seulement juillet et aout
  eco_JA <- Analysed_data[[PARC_nom]]$ecocompteurs_quinz %>%
    subset(., Mois %in% c("07", "08") & (as.numeric(Annee)>2020)) %>%
    dplyr::group_by(compteur) %>%
    dplyr::summarise(Nb_jours=sum(Nb_jours, na.rm=T), Value=sum(Value, na.rm=T)) %>%
    mutate(Passages_par_jour=Value/Nb_jours)
  
  # Calculer jour max
  eco_jours <- Analysed_data[[PARC_nom]]$ecocompteurs_raw %>%
    subset(., substr(mois,1,2) %in% c("06", "07", "08", "09") & as.numeric(substr(mois, 4, 7))>2020) %>%
    mutate(Date=substr(date, 1, 10), donnees=as.numeric(donnees)) %>%
    dplyr::group_by(compteur, Date) %>%
    summarise(Passages=sum(donnees, na.rm=T)) %>%
    dplyr::group_by(compteur) %>%
    summarise(Max=max(Passages, na.rm=T), Jour_max=Date[which.max(Passages)])

  # Sauvegarder les ecocompteurs individuels pour tableau
  eco_cpts$Passages_Juin_Septembre <- round(eco_cpts$Passages_par_jour, 1)
  eco_cpts$Passages_Juillet_Aout <- round(eco_JA$Passages_par_jour[match(eco_cpts$compteur, eco_JA$compteur)],1)
  eco_cpts$Parc <- PARC_nom
  eco_cpts$Max_passages <- eco_jours$Max[match(eco_cpts$compteur, eco_jours$compteur)]
  eco_cpts$Max_jour <- eco_jours$Jour_max[match(eco_cpts$compteur, eco_jours$compteur)]
  eco_stock <- rbind.fill(eco_stock, eco_cpts[, c("Parc", "compteur", "Passages_Juin_Septembre", "Passages_Juillet_Aout", "Max_passages", "Max_jour")])

  # Ajouter les stats dans le tableau final
  AllParcs$eco_MeanJA[AllParcs$Parc==PARC_nom] <- mean(eco_JA$Passages_par_jour, na.rm=T)
  AllParcs$eco_MaxJA[AllParcs$Parc==PARC_nom] <- max(eco_JA$Passages_par_jour, na.rm=T)
}

# Supprimer les maximums pour les ecocompteurs avec seulement des donnees par mois
eco_stock$Max_passages[nchar(eco_stock$Max_jour)<8]<-NA
eco_stock$Max_jour[nchar(eco_stock$Max_jour)<8]<-NA


### Comparaison Strava ----
AllParcs$strava_meanCell <- NA
AllParcs$strava_maxCell <- NA
AllParcs$strava_Q95Cell <- NA
AllParcs$strava_sumCell <- NA

for(PARC_nom in AllParcs$Parc){

  strava_raster <- rast(paste0("2.Outputs/1.Compiled_data/strava_raster_", PARC_nom,".tif"))
  AllParcs$strava_meanCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(strava_raster, mean, na.rm=T))
  AllParcs$strava_maxCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(strava_raster, max, na.rm=T))
  AllParcs$strava_Q95Cell[AllParcs$Parc==PARC_nom] <- as.numeric(global(strava_raster, quantile, probs=c(0.95), na.rm=T))
  AllParcs$strava_sumCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(strava_raster, sum, na.rm=T))

}

### Comparaison Outdoor ----
AllParcs$outdoor_meanCell <- NA
AllParcs$outdoor_maxCell <- NA
AllParcs$outdoor_Q95Cell <- NA
AllParcs$outdoor_sumCell <- NA

for(PARC_nom in AllParcs$Parc[AllParcs$Run_outdoor==T]){

  outdoor_raster <- rast(paste0("2.Outputs/1.Compiled_data/outdoor_raster_", PARC_nom,".tif"))
  AllParcs$outdoor_meanCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(outdoor_raster, mean, na.rm=T))
  AllParcs$outdoor_maxCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(outdoor_raster, max, na.rm=T))
  AllParcs$outdoor_Q95Cell[AllParcs$Parc==PARC_nom] <- as.numeric(global(outdoor_raster, quantile, probs=c(0.95), na.rm=T))
  AllParcs$outdoor_sumCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(outdoor_raster, sum, na.rm=T))

}

### Comparaison iNat ----
AllParcs$iNat_Observers.Day <- NA
AllParcs$iNat_Observers.Tot <- NA
AllParcs$iNat_meanCell <- NA
AllParcs$iNat_Q95Cell <- NA
AllParcs$iNat_maxCell <- NA

for(PARC_nom in AllParcs$Parc){

  # Nombre total d'observateurs et d'observateurs.day
  inat_data <- Analysed_data[[PARC_nom]]$inat_data
  AllParcs$iNat_Observers.Tot[AllParcs$Parc==PARC_nom] <- length(unique(inat_data$user_login))
  AllParcs$iNat_Observers.Day[AllParcs$Parc==PARC_nom] <- length(unique(paste0(inat_data$user_login, as.Date(inat_data$datetime))))

  # Moyenne et max par cellule du raster
  inat_raster <- rast(paste0("2.Outputs/1.Compiled_data/inat_raster_", PARC_nom,".tif"))
  AllParcs$iNat_meanCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(inat_raster, mean, na.rm=T))
  AllParcs$iNat_Q95Cell[AllParcs$Parc==PARC_nom] <- as.numeric(global(inat_raster, quantile, probs=c(0.95), na.rm=T))
  AllParcs$iNat_maxCell[AllParcs$Parc==PARC_nom] <- as.numeric(global(inat_raster, max, na.rm=T))
}


### Enregistrer resultats comparaison ----
saveRDS(AllParcs, "2.Outputs/2.Analysed_data/Comparison_results.rds")
saveRDS(eco_stock, "2.Outputs/2.Analysed_data/All_compteurs.rds")
