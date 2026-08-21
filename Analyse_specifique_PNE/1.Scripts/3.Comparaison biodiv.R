# Lire les premieres lignes du script de preparation des donnees (pour les packages et les donnees carto de base)
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps")
source(textConnection(readLines("1.Scripts/0.BTA preparation donnees.R")[1:51]))

library(tidyterra)
library(exactextractr)
library(ggpubr)




### Charge les donnees
freqSTD <- rast("2.Outputs/frequentation_standard_PNE.tif")
plot(freqSTD$Freq)


######################
### Donnees biodiv ###
######################

### Zones de quietude tetras
tetras <- st_read("0.Data/Bdv_ZoneQuietudeTetras/zone_tranquillite_PNE.shp") %>%
  st_transform(., st_crs(freqSTD$Freq)) %>%
  mutate(Nom=.$zpt_nom)

ggplot()+
  geom_sf(data=PNE_secteurs)+
  geom_sf(data=tetras)



### Aires d'aigle royal (j'ai remplace tous les caracteres erreur par des e et j'ai enregistre en csv)
aigleSF <- read.csv("0.Data/Bdv_AigleRoyal/aires2025_2025-07-31_134740.csv", sep="\t") %>%
  st_as_sf(., coords = c("x_utm","y_utm"), remove = FALSE, crs=st_crs(4326)) %>%
  mutate(Nom=nom_aire) %>%
  st_transform(., st_crs(freqSTD$Freq)) %>%
  distinct(id_aire, .keep_all = T)

aigle_data <- rbind.fill(
  read.csv("0.Data/Bdv_AigleRoyal/controles2021_2025-07-31_135413.csv", row.names=NULL, sep="\t"),
  read.csv("0.Data/Bdv_AigleRoyal/controles2022_2025-07-31_135406.csv", row.names=NULL, sep="\t"),
  read.csv("0.Data/Bdv_AigleRoyal/controles2023_2025-07-31_135400.csv", row.names=NULL, sep="\t"),
  read.csv("0.Data/Bdv_AigleRoyal/controles2024_2025-07-31_135353.csv", row.names=NULL, sep="\t"),
  read.csv("0.Data/Bdv_AigleRoyal/controles2025_2025-07-31_135345.csv", row.names=NULL, sep="\t")
)

aigle_data$actif <- grepl("couvaison", aigle_data$activite.s.) | grepl("jeune", aigle_data$activite.s.) | grepl("nourrissage", aigle_data$activite.s.)

aigleSF$actif <- aigleSF$num_aire %in% aigle_data$num_aire[aigle_data$actif==T]
table(aigleSF$actif)

aigleSF <- subset(aigleSF, actif==T)

ggplot()+
  geom_sf(data=PNE_secteurs)+
  geom_sf(data=aigleSF)



### Habitats prioritaires
HabPrio <- st_read("0.Data/Bdv_HabitatPrioritaires/Habitat_IC_prioritaire.shp") %>%
  st_transform(., st_crs(freqSTD$Freq)) %>%
  st_make_valid(.) %>%
  mutate(Nom=SSP)

ggplot()+
  geom_sf(data=PNE_secteurs)+
  geom_sf(data=HabPrio)


### Zones humides
ZonesHumides <- st_read("0.Data/Bdv_ZonesHumides/export_Inventaire_ZH_Aclimo.gpkg") %>%
  st_transform(., st_crs(freqSTD$Freq)) %>%
  st_make_valid(.) %>%
  group_by(nom_zh) %>%
  summarise(N_group=n()) %>%
  mutate(Nom=nom_zh) %>%
  subset(., st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

ggplot()+
  geom_sf(data=PNE_secteurs)+
  geom_sf(data=ZonesHumides)


### Chardon bleu
bota_raw <- st_read("0.Data/Bdv_AirePresenceBota/aires_presence_2025-09-11T13_41_25.982Z.geojson")
chardon <- subset(bota_raw, grepl("Eryngium alpinum", bota_raw$taxon) & st_geometry_type(bota_raw) %in% c("POLYGON", "POINT")) %>% 
  subset(., format(as.Date(date_max), "%Y")>=2010) %>%
  st_make_valid(.) %>% 
  st_buffer(., 1)

stations <- st_buffer(chardon, 500, nQuadSegs = 4) %>% 
  st_union(.) %>% 
  st_cast(., "POLYGON") %>% 
  st_as_sf(.) %>% 
  st_make_valid() %>%
  mutate(Nom=paste0("Station", 1:nrow(.)))

stations_intersects <- st_join(stations, chardon, join=st_intersects) %>%
  group_by(Nom) %>% summarise(N=n())
stations$N_chardons <- stations_intersects$N[match(stations$Nom, stations_intersects$Nom)]

stations <- subset(stations, N_chardons>=5) %>% st_transform(., st_crs(freqSTD$Freq))

# Noms manuels
stations$Nom <- c("Ruisseau des Berches", "Saut du Laire", "Lauvitel", "Monetier", "Pont de Narreyroux", "Les Rousses", "Désert en Valjouffrey", "Les Clausas", "Combe Méanne", "Le Fournel", "Torrent du Distroit", "Ruisseau des Gorges")

ggplot() +
  geom_sf(data=stations, aes(fill=Nom)) +
  geom_sf(data = chardon) +
  theme_bw()




#################
### FONCTIONS ###
#################

# Histogramme avec highlight des zones bdv
BAT_AnalyseBdvShp <- function(freq, bdv_shp, TITRE){
  
  # Carte de la frequentation et des zones de quietude
  G_brut <- ggplot()+
    geom_spatraster(data=freq, alpha=0.3)+
    scale_fill_viridis_c(na.value=NA)+
    geom_sf(data=bdv_shp, fill="black", col="black")+
    ggtitle("Carte des zones sensibles")+
    theme_minimal()
  
  # Passer rasters en dataframe
  bdv_rast <- terra::rasterize(bdv_shp, freq, touches=T, background=0)

  Rast_DF <- data.frame(
    freq=as.vector(freq),
    Bdv=as.vector(bdv_rast)
  ) %>% subset(., is.na(.$freq)==F)
  
  # Violin plot
  Rast_DF$Sensibilite <- ifelse(Rast_DF$Bdv>0, "Sensible", "Non-sensible")
  
  G_violin <- ggplot(Rast_DF)+
    geom_violin(aes(x=Sensibilite, y=freq, fill=Sensibilite), draw_quantiles=0.5, show.legend=F)+
    scale_fill_manual(values=c("#b8e186", "#d01c8b"), name="Sensible")+
    xlab("")+ylab("Fréquentation")+
    ggtitle("Différence de fréquentation")+
    theme_minimal()
  
  
  # Comparaison de moyenne
  Wilcox_test <- wilcox.test(freq~Bdv, data=Rast_DF, alternative="two.sided")
  Moy_nonsens <- paste0(round(mean(Rast_DF$freq[Rast_DF$Bdv==0])), " +/- ", round(sd(Rast_DF$freq[Rast_DF$Bdv==0])))
  Moy_sens <- paste0(round(mean(Rast_DF$freq[Rast_DF$Bdv==1])), " +/- ", round(sd(Rast_DF$freq[Rast_DF$Bdv==1])))
  
  St_res <- paste0("La fréquentation dans les cellules sensibles (", Moy_sens, ") ",
                   ifelse(Wilcox_test$p.value<0.05, ifelse((mean(Rast_DF$freq[Rast_DF$Bdv==1]) - mean(Rast_DF$freq[Rast_DF$Bdv==0]))>0, "est en moyenne plus élevée que dans les", "est en moyenne plus basse que dans les"), "ne diffère pas significativement des"),
                   " cellules non sensibles (", Moy_nonsens, ifelse(Wilcox_test$p.value>10^-4, paste0("); P-value=", round(Wilcox_test$p.value,4)), "); P-value < 10-4")
  )
  
  # Table
  if(st_geometry_type(bdv_shp)[1]=="POINT"){bdv_shp <- st_buffer(bdv_shp, 0.00001)} # Buffer pour que ca fonctionne aussi avec des points (aigle royal par exemple)
  bdv_shp$freq <- exact_extract(freq, bdv_shp, "mean", progress=F) %>% round(., 1)
  Tab_to_save <- data.frame(Nom=bdv_shp$Nom, Frequentation=bdv_shp$freq) ; if("CODE_CB" %in% names(bdv_shp)){Tab_to_save$Type_habitat <- bdv_shp$CODE_CB}
  Tab_to_save <- Tab_to_save  %>% .[order(.$Frequentation, decreasing=T),]
  Tab_to_plot <- Tab_to_save[1:min(c(15, nrow(Tab_to_save))),]
  GTab <- ggtexttable(Tab_to_plot[1:min(nrow(Tab_to_plot), 20),], rows=NULL)
  
  # Grouper les graphiques
  Graph <- grid.arrange(G_brut, G_violin, GTab,
                        nrow=1, 
                        top=paste0(TITRE, "\n"),
                        bottom=paste0("\n", St_res, "\n")
                        )
  
  write.csv(Tab_to_save, paste0("2.Outputs/Plots/3_", TITRE, ".csv"), row.names=F)
  cowplot::save_plot(paste0("2.Outputs/Plots/3_", TITRE, ".png"), Graph, base_height=6, base_width=12)
  
  return(Graph)
}




################
### Analyses ###
################

BAT_AnalyseBdvShp(freqSTD$Freq, tetras, "Zones de quietude pour tetras")

BAT_AnalyseBdvShp(freqSTD$Freq, aigleSF, "Aires d'aigle royal")

BAT_AnalyseBdvShp(freqSTD$Freq, HabPrio, "Habitats prioritaires")

BAT_AnalyseBdvShp(freqSTD$Freq, ZonesHumides, "Zones humides")

BAT_AnalyseBdvShp(freqSTD$Freq, stations, "Stations botaniques")



# ### Test : voir si on a des donnees iNat dans des zones interdites (zones de quietude) en hiver
# inat_raw_data <- readRDS("2.Outputs/inat_saved_data.rds") %>% 
#   subset(., Annee>2016 & Annee<2025) # Ne garder que les donnees apres 2016 et avant 2025
# inat_data <- st_filter(inat_raw_data, PNE_secteurs, .predicate = st_intersects)
# inat_dataPROJ <- inat_data %>% st_transform(., st_crs(tetras))
# 
# inat_quietude <- st_filter(inat_dataPROJ, tetras, .predicate = st_intersects)
# table(inat_quietude$observed_on)



