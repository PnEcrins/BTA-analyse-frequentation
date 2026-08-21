library(tidyterra)


### Charge les données
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps")
source("1.Scripts/0.BTA preparation donnees.R")


### Calcul Frequentation -------


### Identifier chemins avec ecocompteur
strava_ecocpt <- strava_NOTcutPROJ[strava_NOTcutPROJ$edgeUID %in% st_filter(st_buffer(strava_NOTcutPROJ, 500), ecocompteurPROJ, .predicate = st_intersects)$edgeUID,] %>% rename(., Count_Strava = count)
ecocompteurPROJ$edgeUID <- apply(st_distance(strava_ecocpt, ecocompteurPROJ), 2, which.min) %>% strava_ecocpt$edgeUID[.]
strava_ecocpt$Ecocompteur <- ecocompteurPROJ$name[match(strava_ecocpt$edgeUID, ecocompteurPROJ$edgeUID)]
strava_ecocpt <- subset(strava_ecocpt, is.na(Ecocompteur)==F)
strava_ecocpt$Length <- round(as.numeric(st_length(strava_ecocpt)))

ggplot()+
  geom_sf(data=st_buffer(strava_ecocpt, 1000), aes(col=as.factor(edgeUID)))+
  geom_sf(data=ecocompteurPROJ, aes(col=as.factor(edgeUID)))

ggplot()+
  geom_sf(data=PNE_secteursPROJ, fill="gray90", col=NA)+
  geom_sf(data=strava_NOTcutPROJ)+
  geom_sf(data=ecocompteurPROJ, col="coral2", show.legend=F, size=5)+
  theme_minimal()

### Ajout stat ecocompteur
eco_ete <- subset(eco_mois, eco_mois$Mois_nom %in% c("Jun", "Jul", "Aou", "Sep") & as.numeric(Annee)>2020 & !(eco_mois$Nom %in% c("Carrelet", "AdeleRomanche")))
ggplot(eco_ete)+geom_tile(aes(x=paste(Annee, Mois), y=Nom, fill=Nb_jours))

eco_std <- eco_ete %>%
  subset(., Nb_jours>10) %>%
  group_by(Nom_spatial) %>%
  summarise(Freq_std = mean(Value / Nb_jours, na.rm=T))

strava_ecocpt$Cpt_std <- eco_std$Freq_std[match(strava_ecocpt$Ecocompteur, eco_std$Nom_spatial)]
strava_ecocpt <- subset(strava_ecocpt, is.na(strava_ecocpt$Cpt_std)==F)

G_raw <- ggplot()+
  geom_sf(data=PNE_secteursPROJ)+
  geom_sf(data=st_transform(PNE, PROJ_Lambert), fill="white", col="white")+
  geom_sf(data=PNE_secteursPROJ, fill=NA)+
  geom_sf(data=st_centroid(strava_ecocpt), aes(col=Cpt_std), size=5)+
  scale_colour_viridis_c(name="Fréquentation")+
  theme_void()

cowplot::save_plot("2.Outputs/Plots/1b_carte_ecocompteurs_estival.png", G_raw, base_height=7, base_width=7)


### Calculer nombre donnees iNat / PlantNet

## Creer un buffer de 100m autour du chemin qui est dans les 500m de l'ecocompteur
strava_buffer_raw <- ecocompteurPROJ[, "name"] %>% 
  st_buffer(., 500) %>% # Buffer de 500m autour de chaque ecocompteur
  st_intersection(., strava_NOTcutPROJ) %>% # Extraire tous les chemins strava qui intersectent le buffer de 500m
  subset(., ! edgeUID %in% c(146991228, 146991237, 146991230, 146991229, 146991231, 146991232)) %>% # Decouper manuellement Montee de l'Aigle (ne pas prendre tous les chemins qui passent avant le debut de la montee)
  subset(., ! edgeUID %in% c(146842739, 146842784, 146842786, 146842787, 146842791, 146842793, 146842780, 146842794, 146842781, 146842778, 146843202)) %>% # Decouper manuellement Chazelet (ne pas prendre tous les chemins qui passent avant le debut de la montee)
  subset(., ! edgeUID %in% c(65105513, 65105515, 65105516, 65105517)) %>% # Decouper manuellement Lac du Vallon (ne pas prendre tous les chemins qui passent avant le debut de la montee)
  subset(., ! edgeUID %in% c(65140168)) %>% # Decouper manuellement Dormillouse (ne pas prendre tous les chemins qui passent avant le debut de la montee)
  subset(., ! edgeUID %in% c(65106529, 65106528)) %>% # Decouper manuellement Gioberney (ne pas prendre tous les chemins qui passent avant le debut de la montee)
  subset(., ! edgeUID %in% c(146992506, 146992493)) %>% # Decouper manuellement Pont de l'Alpe (ne pas prendre tous les chemins qui passent avant le debut de la montee)
  subset(., ! edgeUID %in% c(146843306, 146843334)) %>% # Decouper manuellement Serre du Coin (ne pas prendre tous les chemins qui passent avant le debut de la montee)
  st_buffer(., 10) # Faire un buffer de 10m (pour que les lignes quasi adjacentes soient considerees comme un chemin continu)


library(igraph)
strava_buffer_raw$Group <- components(graph_from_adj_list(st_overlaps(strava_buffer_raw)))$membership
strava_buffer <- subset(strava_buffer_raw, Group %in% unique(strava_buffer_raw$Group[strava_buffer_raw$edgeUID %in% strava_ecocpt$edgeUID])) %>%
  group_by(name, Group) %>% summarise(N=n()) %>%
  st_buffer(., 90)

# # Visualisation pour verifier
# for(i in 1:nrow(strava_buffer)){
#   ECO_TEST=strava_buffer$name[i]
#   plot(
#     ggplot()+ggtitle(ECO_TEST)+
#       geom_sf(data=strava_buffer[strava_buffer$name==ECO_TEST,], fill="grey40")+
#       geom_sf(data=strava_buffer_raw[strava_buffer_raw$name==ECO_TEST,], aes(fill=as.factor(Group)), col=NA, alpha=0.3)+
#       geom_sf(data=ecocompteurPROJ[ecocompteurPROJ$name==ECO_TEST,], size=5)+
#       geom_sf(data=st_filter(inat_dataPROJ, st_buffer(ecocompteurPROJ[ecocompteurPROJ$name==ECO_TEST,], 600), .predicate=st_intersects), aes(col=paste0(user_login, as.Date(datetime))), show.legend=F)
#   )
#   readline(paste0("Plot ", i))
# }
#ggplot(strava_buffer)+geom_sf()

### Calculer le nombre d'observateurs
inat_obs.date <- st_join(inat_dataPROJ, strava_buffer, join=st_intersects) %>% 
  subset(., is.na(name)==F) %>%
  distinct(user_login, as.Date(datetime), name, .keep_all = T) %>% 
  group_by(name) %>% summarise(N_obs.day=n())
strava_ecocpt$iNat_obs.day <- inat_obs.date$N_obs.day[match(strava_ecocpt$Ecocompteur, inat_obs.date$name)] %>% replace(., is.na(.), 0)

plantnet_obs.date <- st_join(plantnet_dataPROJ, strava_buffer, join=st_intersects) %>% 
  subset(., is.na(name.y)==F) %>%
  distinct(Creator, as.Date(eventDate), name.y, .keep_all = T) %>% 
  group_by(name.y) %>% summarise(N_obs.day=n())
strava_ecocpt$plantNet_obs.day <- plantnet_obs.date$N_obs.day[match(strava_ecocpt$Ecocompteur, plantnet_obs.date$name.y)] %>% replace(., is.na(.), 0)

outdoor_intersect <- st_join(outdoorPROJ, strava_buffer, join=st_intersects) %>%
  group_by(name) %>% summarise(Sum_DistPers=sum(DistPers))
strava_ecocpt$outdoor <- outdoor_intersect$Sum_DistPers[match(strava_ecocpt$Ecocompteur, outdoor_intersect$name)] %>% replace(., is.na(.), 0)

### Plot valeurs brutes
grid.arrange(
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=Count_Strava), size=5)+scale_color_viridis_c(trans="sqrt"),
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=outdoor), size=5)+scale_color_viridis_c(trans="sqrt"),
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=iNat_obs.day), size=5)+scale_color_viridis_c(trans="sqrt"),
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=plantNet_obs.day), size=5)+scale_color_viridis_c(trans="sqrt"),
  nrow=2
)


### Correlation entre rasters
Rasters_df <- data.frame(Strava=as.vector(strava_raster), Outdoor=as.vector(outdoor_raster), iNaturalist=as.vector(inat_raster), PlantNet=as.vector(plantnet_raster))
cor(Rasters_df, use="complete.obs")


### Correlation ecocompteur / donnees externes
CORR_strava <- cor.test(strava_ecocpt$Cpt_std, strava_ecocpt$Count_Strava)$estimate
CORR_outdoor <- cor.test(strava_ecocpt$Cpt_std, strava_ecocpt$outdoor)$estimate
CORR_iNat <- cor.test(strava_ecocpt$Cpt_std, strava_ecocpt$iNat_obs.day)$estimate
CORR_PlantNet <- cor.test(strava_ecocpt$Cpt_std, strava_ecocpt$plantNet_obs.day)$estimate

G_corr <- grid.arrange(
  ggplot(strava_ecocpt)+geom_point(aes(x=Cpt_std, y=iNat_obs.day))+xlab("Eco-compteurs (été)")+ylab("Observateurs.jours")+labs(title="iNaturalist", subtitle=paste0("Correlation : ", 100*round(CORR_iNat,2), "%")),
  ggplot(strava_ecocpt)+geom_point(aes(x=Cpt_std, y=Count_Strava))+xlab("Eco-compteurs (été)")+ylab("Passages")+labs(title="Strava", subtitle=paste0("Correlation : ", 100*round(CORR_strava,2), "%")),
  ggplot(strava_ecocpt)+geom_point(aes(x=Cpt_std, y=outdoor))+xlab("Eco-compteurs (été)")+ylab("Passages")+labs(title="Outdoorvision", subtitle=paste0("Correlation : ", 100*round(CORR_outdoor,2), "%")),
  ggplot(strava_ecocpt)+geom_point(aes(x=Cpt_std, y=plantNet_obs.day))+xlab("Eco-compteurs (été)")+ylab("Observateurs.jours")+labs(title="PlantNet", subtitle=paste0("Correlation : ", 100*round(CORR_PlantNet,2), "%")),
  ncol=4)

cowplot::save_plot("2.Outputs/Plots/1b_correlation.png", G_corr, base_height=4, base_width=13)


### Structuration spatiale
grid.arrange(
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=(Count_Strava/Cpt_std)), size=5)+scale_color_viridis_c()+theme(legend.position = "bottom")+ggtitle("Strava"),
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=(outdoor/Cpt_std)), size=5)+scale_color_viridis_c()+theme(legend.position = "bottom")+ggtitle("Outdoor"),
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=(iNat_obs.day/Cpt_std)), size=5)+scale_color_viridis_c()+theme(legend.position = "bottom")+ggtitle("iNaturalist"),
  ggplot(st_centroid(strava_ecocpt))+geom_sf(aes(col=(plantNet_obs.day/Cpt_std)), size=5)+scale_color_viridis_c()+theme(legend.position = "bottom")+ggtitle("PlantNet"),
  ncol=4
)

plot((strava_ecocpt$Count_Strava/strava_ecocpt$Cpt_std), (strava_ecocpt$iNat_obs.day/strava_ecocpt$Cpt_std)) # La proposition d'AS ferait sens si pattern inverse ici (des endroits avec plus d'iNat en proportion)




### Calcul raster pondere
Poids_strava <- ifelse(CORR_strava>0.5, (1/(1-CORR_strava)), 0)
Poids_outdoor <- ifelse(CORR_outdoor>0.5, (1/(1-CORR_outdoor)), 0)
Poids_inat <- ifelse(CORR_iNat>0.5, (1/(1-CORR_iNat)), 0)
Poids_plantnet <- ifelse(CORR_PlantNet>0.5, (1/(1-CORR_PlantNet)), 0)

freq_std <- 
  Poids_strava * scale(strava_raster) + 
  Poids_outdoor * scale(outdoor_raster) + 
  Poids_inat * scale(inat_raster) + 
  Poids_plantnet * scale(plantnet_raster)

Winso_Max <- as.numeric(quantile(as.vector(freq_std), 0.98, na.rm=T))
freq_std <- replace(freq_std, freq_std>Winso_Max, Winso_Max)
Min_STD <- as.numeric(global(freq_std, "min", na.rm=T))
Max_STD <- as.numeric(global(freq_std, fun="max", na.rm=T))
freq_std <- (100*(freq_std-Min_STD) / (Max_STD-Min_STD))

SumPoids <- (Poids_strava + Poids_outdoor + Poids_inat + Poids_plantnet) / 100
SousTitre <- paste0("Pondération: iNat (", round(Poids_inat/SumPoids), "%), Strava (", round(Poids_strava/SumPoids), "%), Outdoor (", round(Poids_outdoor/SumPoids), "%), PlantNet (", round(Poids_plantnet/SumPoids), "%)")
print(SousTitre)

G_rasters <- grid.arrange(
  ggplot()+geom_spatraster(data=strava_raster)+scale_fill_viridis_c(na.value=NA, trans="sqrt")+ggtitle("Strava")+theme_void(),
  ggplot()+geom_spatraster(data=outdoor_raster)+scale_fill_viridis_c(na.value=NA, trans="sqrt")+ggtitle("Outdoorvision")+theme_void(),
  ggplot()+geom_spatraster(data=inat_raster)+scale_fill_viridis_c(na.value=NA, trans="sqrt")+ggtitle("iNaturalist")+theme_void(),
  ggplot()+geom_spatraster(data=plantnet_raster)+scale_fill_viridis_c(na.value=NA, trans="sqrt")+ggtitle("PlantNet")+theme_void()
)

G_Freq <- ggplot()+
  geom_spatraster(data=freq_std)+
  scale_fill_viridis_c(na.value=NA)+
  geom_sf(data=PNE_secteursPROJ, fill=NA, col=alpha("white", 0.5), linewidth=1.2)+
  geom_sf(data=PNE, fill=NA, col=alpha("white", 0.65))+
  labs(title="Fréquentation standard")+
  theme_void()


G_tot <- cowplot::plot_grid(G_rasters, G_Freq, ncol=2, labels=c("a", "b"))
cowplot::save_plot("2.Outputs/Plots/1b_freq.png", G_tot, base_height=5.5, base_width=12)




### Calcul chemin pondere
cheminsManu100 <- st_buffer(cheminsManu, 50) # passer a un buffer de 50m (deja 50 de base) pour coller avec la methode des rasters

# Strava et Outdoor
Strava_chemin <- st_intersection(strava_NOTcutPROJ, cheminsManu100) %>% mutate(Length=as.numeric(st_length(.))) %>% mutate(DistPers=Length*count) %>% group_by(NOMPNE) %>% summarise(Sum_DistPers=sum(DistPers, na.rm=T))
cheminsManu100$Strava <- Strava_chemin$Sum_DistPers[match(cheminsManu100$NOMPNE, Strava_chemin$NOMPNE)] / cheminsManu100$Area

Outdoor_chemin <- st_intersection(outdoorPROJ, cheminsManu100) 
Outdoor_cheminCOUNT <- data.frame(OutdoorSum=tapply(Outdoor_chemin$DistPers, Outdoor_chemin$NOMPNE, "sum"))
cheminsManu100$Outdoor <- Outdoor_cheminCOUNT$OutdoorSum[match(cheminsManu100$NOMPNE, rownames(Outdoor_cheminCOUNT))] / cheminsManu100$Area

# Calculer le nombre d'observateurs iNat (pas besoin de plantNet car correlation trop faible) et fit d'un modele de species area relationship pour corriger le biais de distance
inat_obs.dateCHEMIN <- st_join(inat_dataPROJ, cheminsManu100, join=st_intersects) %>% 
  subset(., is.na(NOMPNE)==F) %>%
  distinct(user_login, as.Date(datetime), NOMPNE, .keep_all = T) %>% 
  group_by(NOMPNE) %>% summarise(N_obs.day=n())
cheminsManu100$iNat_obs.day <- inat_obs.dateCHEMIN$N_obs.day[match(cheminsManu100$NOMPNE, inat_obs.dateCHEMIN$NOMPNE)] %>% replace(., is.na(.), 0)

library(sars)
fit <- sar_power(as.data.frame(cheminsManu100)[, c("Area", "iNat_obs.day")])
plot(fit)
cheminsManu100$iNat_residuals <- fit$residuals[match(cheminsManu100$Area, fit$data$A)]

# Verifier les patterns
ggplot(st_buffer(cheminsManu100,200))+geom_sf(aes(fill=iNat_residuals), col=NA)+scale_fill_viridis_c()
plot(inat_raster)
ggplot(st_buffer(cheminsManu100,200))+geom_sf(aes(fill=Strava), col=NA)+scale_fill_viridis_c()
plot(strava_raster)
ggplot(st_buffer(cheminsManu100,200))+geom_sf(aes(fill=Outdoor), col=NA)+scale_fill_viridis_c()
plot(outdoor_raster)

# Creer la carte ponderee
cheminsManu100$Freq_std <- 
  Poids_strava * scale(cheminsManu100$Strava) +
  Poids_outdoor * scale(cheminsManu100$Outdoor) + 
  Poids_inat * scale(cheminsManu100$iNat_residuals)

Winso_Max <- as.numeric(quantile(cheminsManu100$Freq_std, 0.98, na.rm=T))
cheminsManu100$Freq_std <- replace(cheminsManu100$Freq_std, cheminsManu100$Freq_std>Winso_Max, Winso_Max)
Min_STD <- min(cheminsManu100$Freq_std, na.rm=T)
Max_STD <- max(cheminsManu100$Freq_std, na.rm=T)
cheminsManu100$Freq_std <- as.numeric(100*(cheminsManu100$Freq_std-Min_STD) / (Max_STD-Min_STD))

# Sauvegarder
st_write(cheminsManu100, "2.Outputs/Frequentation_standard_chemins.shp", append=F)
  
  
# Graphique
G_Chemin <- ggplot()+
  geom_sf(data=PNE_secteursPROJ)+
  geom_sf(data=st_transform(PNE, PROJ_Lambert), fill="white", col="white")+
  geom_sf(data=PNE_secteursPROJ, fill=NA)+
  geom_sf(data=st_buffer(cheminsManu100,100), aes(fill=Freq_std), col=NA)+
  scale_fill_viridis_c(na.value=NA, name="Frequentation")+
  theme_void()
cowplot::save_plot("2.Outputs/Plots/1b_freqChemins.png", G_Chemin, base_height=5, base_width=5)




### Comparaison avec covariables -----

### Preparation covariables
# Altitude
alt_raster <- rast("0.Data/Altitude/France_metropolitaine.tif") %>% # Telechargement : https://www.data.gouv.fr/datasets/modele-numerique-de-terrain-mnt-france-metropolitaine-et-drom/
  crop(., ext(st_transform(PNE_secteurs, crs(.)))) %>% 
  replace(., is.na(.), 0) %>%
  project(., freq_std, method="average") %>%
  mask(., PNE_secteursPROJ)
names(alt_raster) <- "alt_raster"

plot(alt_raster, main=paste0("Altitude : ", paste0(dim(alt_raster), collapse=", ")))

# Distance aux routes
routesSF <- st_read("0.Data/ROUTE_500/ROUTE500_3-0__SHP_LAMB93_FXX_2021-11-03/ROUTE500/1_DONNEES_LIVRAISON_2022-01-00175/R500_3-0_SHP_LAMB93_FXX-ED211/RESEAU_ROUTIER/TRONCON_ROUTE.shp") %>% # Telechargement : https://geoservices.ign.fr/route500
  st_filter(., st_buffer(st_transform(PNE_secteurs, st_crs(.)), 20000), .predicate = st_intersects) %>%
  st_transform(., PROJ_Lambert)

routes_raster <- rasterize(routesSF, freq_std)
dist_routes <- routes_raster %>% 
  distance(.) %>%
  mask(., PNE_secteursPROJ)
names(dist_routes) <- "dist_routes"

plot(dist_routes, main=paste0("Distance routes : ", paste0(dim(dist_routes), collapse=", ")))


### Creer un DF avec toutes les donnees
DF_covar <- data.frame(
  Freq_std = as.vector(freq_std),
  Alt= as.vector(alt_raster),
  DistRoutes = as.vector(dist_routes) %>% as.numeric(.)/1000
  ) %>% subset(., is.na(Freq_std)==F)

DF_covar$Lon <- st_coordinates(st_as_sf(as.points(freq_std)))[,1]
DF_covar$Lat <- st_coordinates(st_as_sf(as.points(freq_std)))[,2]
DF_covar$Bassin <- st_as_sf(as.points(freq_std)) %>% st_join(., st_transform(basinModif, st_crs(.)), join=st_intersects) %>% .$Nom

ggplot(DF_covar)+geom_point(aes(x=Lon, y=Lat, col=Bassin))
ggplot(DF_covar)+geom_point(aes(x=Lon, y=Lat, col=Alt))
ggplot(DF_covar)+geom_point(aes(x=Lon, y=Lat, col=DistRoutes))
ggplot(DF_covar)+geom_point(aes(x=Lon, y=Lat, col=Freq_std))


### Graphiques
Bassin_Order <- tapply(DF_covar$Freq_std, DF_covar$Bassin, "median") %>% sort(., decreasing=T) %>% names(.)


G_cov_Bassin <- ggplot(DF_covar[is.na(DF_covar$Bassin)==F,])+
  geom_boxplot(aes(x=factor(Bassin, Bassin_Order), y=Freq_std), outlier.size=0.7)+
  xlab("")+ylab("Fréquentation standardisée")+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

G_cov_Alt <- ggplot(DF_covar)+
  geom_point(aes(x=Alt, y=Freq_std), size=0.8)+
  xlab("Altitude (m)")+ylab("Fréquentation standardisée")+
  theme_minimal()

G_cov_Dist <- ggplot(DF_covar)+
  geom_point(aes(x=DistRoutes, y=Freq_std), size=0.8)+
  xlab("Distance aux routes (km)")+ylab("Fréquentation standardisée")+
  theme_minimal()

G_cov <- cowplot::plot_grid(G_cov_Bassin, grid.arrange(G_cov_Alt, G_cov_Dist, ncol=1), ncol=2)
cowplot::save_plot("2.Outputs/Plots/1b_covariates.png", G_cov, base_height=6, base_width=10)



### Incertitude ------- (tentative mais pas tres convaincante... pas integree au rapport)
library(DescTools)

DF_incertitude <- data.frame(
  Strava=scale(Winsorize(as.vector(strava_raster),val=quantile(as.vector(strava_raster), probs=c(0,0.98), na.rm=T))),
  Outdoor=scale(Winsorize(as.vector(outdoor_raster),val=quantile(as.vector(outdoor_raster), probs=c(0,0.98), na.rm=T))),
  iNat=scale(Winsorize(as.vector(inat_raster),val=quantile(as.vector(inat_raster), probs=c(0,0.98), na.rm=T)))
)

DF_incertitude$SD <- apply(DF_incertitude, 1, sd)

freq_std$incertitude <- DF_incertitude$SD
par(mfrow=c(1,2))
plot(freq_std$Freq)
plot(freq_std$incertitude)
par(mfrow=c(1,1))


writeRaster(freq_std, "2.Outputs/frequentation_standard_PNE.tif", overwrite=T)
