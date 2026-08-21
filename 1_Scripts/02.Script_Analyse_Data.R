### Charge les donnees du parc dans le global environment
Data_tosave <- readRDS(paste0("2.Outputs/1.Compiled_data/Compiled_data_", PARC_nom, ".rds"))
list2env(Data_tosave, .GlobalEnv)
Res_PoidsRaster <- readRDS("2.Outputs/2.Analysed_data/Saved_Poids_raster.rds")
strava_raster <- rast(paste0("2.Outputs/1.Compiled_data/strava_raster_", PARC_nom, ".tif"))
inat_raster <- rast(paste0("2.Outputs/1.Compiled_data/inat_raster_", PARC_nom, ".tif"))
if(Run_Outdoor==T){
  outdoor_raster <- rast(paste0("2.Outputs/1.Compiled_data/outdoor_raster_", PARC_nom, ".tif"))
} else {outdoor_raster <- NULL}


## ANALYSES ----

### Obj 1 : frequentation interne ----

#### Par raster ----

### Correlation entre rasters
Rasters_df <- data.frame(Strava=as.vector(strava_raster), iNaturalist=as.vector(inat_raster))
if(Run_Outdoor){Rasters_df$Outdoor=as.vector(outdoor_raster)}
cor(Rasters_df, use="complete.obs")

### Choix du poids
if(COUNTRY=="FR"){
  Res_PoidsRaster$Poids <- Res_PoidsRaster$Poids_FR
} else {
  Res_PoidsRaster$Poids <- Res_PoidsRaster$Poids_IT
}

### Calcul raster pondere
Res_FreqRaster <- BTA_FreqRaster(Res_PoidsRaster$Poids, strava_raster, outdoor_raster, inat_raster)
writeRaster(Res_FreqRaster, paste0("2.Outputs/1.Compiled_data/freqSTD_raster_", PARC_nom, ".tif"), overwrite=T)

# Scale rasters for plotting
strava_wins <- strava_raster ; strava_Q99 <- as.numeric(global(strava_raster, quantile, probs=c(0.99), na.rm=TRUE)) ; strava_wins[strava_wins>strava_Q99]<-strava_Q99
strava_scale <- 100*(scale(strava_wins)-as.numeric(global(scale(strava_wins), "min", na.rm=T)))/(as.numeric(global(scale(strava_wins), "max", na.rm=T))-as.numeric(global(scale(strava_wins), "min", na.rm=T)))
inat_wins <- inat_raster ; inat_Q99 <- as.numeric(global(inat_raster, quantile, probs=c(0.99), na.rm=TRUE)) ; inat_wins[inat_wins>inat_Q99]<-inat_Q99
inat_scale <- 100*(scale(inat_wins)-as.numeric(global(scale(inat_wins), "min", na.rm=T)))/(as.numeric(global(scale(inat_wins), "max", na.rm=T))-as.numeric(global(scale(inat_wins), "min", na.rm=T)))

# Plot
LegName <- ifelse(COUNTRY=="FR", "Fréquentation", "Frequentazione")
Gfreq_strava <- ggplot()+geom_spatraster(data=strava_scale)+scale_fill_viridis_c(na.value=NA, name=LegName)+ggtitle("Strava")+theme_void()
Gfreq_inat <- ggplot()+geom_spatraster(data=inat_scale)+scale_fill_viridis_c(na.value=NA, name=LegName)+ggtitle("iNaturalist")+theme_void()
if(is.null(outdoor_raster)==F){
  outdoor_wins <- outdoor_raster ; outdoor_Q99 <- as.numeric(global(outdoor_raster, quantile, probs=c(0.99), na.rm=TRUE)) ; outdoor_wins[outdoor_wins>outdoor_Q99]<-outdoor_Q99
  outdoor_scale <- 100*(scale(outdoor_wins)-as.numeric(global(scale(outdoor_wins), "min", na.rm=T)))/(as.numeric(global(scale(outdoor_wins), "max", na.rm=T))-as.numeric(global(scale(outdoor_wins), "min", na.rm=T)))
  Gfreq_outdoor <- ggplot()+geom_spatraster(data=outdoor_scale)+scale_fill_viridis_c(na.value=NA, name=LegName)+ggtitle("Outdoorvision")+theme_void()
}
legend <- gtable::gtable_filter(ggplot_gtable(ggplot_build(Gfreq_strava + theme(legend.position="right"))), "guide-box")
if(is.null(outdoor_raster)==F){
  G_rasters <- grid.arrange(arrangeGrob(Gfreq_strava + theme(legend.position="none"), Gfreq_outdoor + theme(legend.position="none"), Gfreq_inat + theme(legend.position="none"), nrow=1), legend, widths=c(1.5,0.2), nrow=1)
} else {
  G_rasters <- grid.arrange(arrangeGrob(Gfreq_strava + theme(legend.position="none"), Gfreq_inat + theme(legend.position="none"), nrow=1), legend, widths=c(1.5,0.2), nrow=1)
}


G_Freq_raw <- ggplot()+
  geom_spatraster(data=Res_FreqRaster)+
  scale_fill_viridis_c(na.value=NA, name=LegName)+
  #geom_sf(data=PARC_shp, fill=NA, col=alpha("white", 0.5), linewidth=1.2)+
  labs(title=ifelse(COUNTRY=="FR", "Fréquentation standard", "Frequentazione standard"))+
  theme_void()
G_Freq <- grid.arrange(G_Freq_raw, nrow=1)




#### Par sentiers ----
if(Run_Sentiers==T){
  Res_FreqSentiers <- BTA_FreqSentiers(Sentiers, st_transform(strava_NOTcut, st_crs(2154)), outdoor_flux, inat_data)

  PARC_sub <- PARC_shp %>% st_cast(., "POLYGON") %>% st_filter(., Sentiers,  .predicate = st_intersects)
  if(PARC_nom=="Alpi_Marittime"){PARC_sub <- subset(PARC_sub, as.numeric(st_area(PARC_sub))>200000)}

  # Graphique
  G_FreqSentiers <- ggplot()+
    geom_sf(data=PARC_sub)+
    geom_sf(data=st_buffer(Res_FreqSentiers, 100), aes(fill=Freq_std), col=NA)+
    scale_fill_viridis_c(na.value=NA, name=LegName)+
    theme_void()
}




### Obj 3 : temporel ----

### iNat
library(zoo)
Res_iNat_temporel <- BTA_iNatTempo(inat_data, COUNTRY, PARC_nom)


### Ecocompteurs
if(Run_EcoTempo==T){
  Res_ecocompteurs_temporel <- BTA_TempoEcocompteurs(ecocompteurs_raw, COUNTRY)
}




## SAUVEGARDE DES DONNEES DU PARC ----
Data_tosave$Res_PoidsRaster <- Res_PoidsRaster
Data_tosave$Res_FreqRaster <- Res_FreqRaster

Data_tosave$G_rasters <- G_rasters
Data_tosave$G_Freq <- G_Freq

Data_tosave$Res_iNat_temporel <- Res_iNat_temporel

if(Run_Sentiers==T){
  Data_tosave$Res_FreqSentiers <- Res_FreqSentiers
  Data_tosave$G_FreqSentiers <- G_FreqSentiers
}

if(Run_EcoTempo==T){
  Data_tosave$Res_ecocompteurs_temporel <- Res_ecocompteurs_temporel
}

saveRDS(Data_tosave, paste0("2.Outputs/2.Analysed_data/Analysed_data_", PARC_nom, ".rds"))

