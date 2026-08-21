library(gridExtra)
library(tidyterra)
library(cowplot)
library(ggrepel)

### Charge les donnees
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps")
source("1.Scripts/0.BTA preparation donnees.R")

lauvitel_shp <- st_read("0.Data/Donnees_carto_PNE/Lauvitel.gpkg") %>% st_transform(., PROJ_Lambert)



### Subset les donnees
strava_lauv <- st_filter(strava_NOTcutPROJ, lauvitel_shp, .predicate = st_intersects)
outdoor_lauv <- st_filter(outdoorPROJ, lauvitel_shp, .predicate = st_intersects) %>% st_cast(., "LINESTRING")
inat_lauv <- st_filter(inat_dataPROJ, lauvitel_shp, .predicate = st_intersects) %>% subset(., public_positional_accuracy<20)
plantnet_lauv <- st_filter(plantnet_dataPROJ, lauvitel_shp, .predicate = st_intersects) %>% subset(., coordinateUncertaintyInMeters<20)

ecodata_lauv <- subset(ecocompteurs_data, grepl("Danchere", ecocompteurs_data$Nom))
photodata_lauv <- subset(photo_data, grepl("Lauvitel", photo_data$Nom_spatial))

suiviSF_lauv <- rbind(
  subset(ecocompteurPROJ, grepl("Danchère", ecocompteurPROJ$name))[,"geometry"] %>% mutate(Type="Eco-compteur"),
  subset(photoPROJ, grepl("Lauvitel", photoPROJ$Nom))[,"geometry"] %>% mutate(Type="Photo"),
  subset(enquetePROJ, NOM_LIEU=="La Danchere")[,"geometry"] %>% mutate(Type="Enquete"),
  subset(routierPROJ, name=="Compteur routier R16")[,"geometry"] %>% mutate(Type="Routier")
)


# Carte des donnees accessibles
library(maptiles)
OSM_tile <- get_tiles(st_transform(st_buffer(lauvitel_shp,1000), 3857), provider = "OpenStreetMap", zoom = 13, crop=T)

G_2rawmap <- ggplot()+
  geom_spatraster_rgb(data = OSM_tile, alpha=0.5) +
  geom_sf(data=outdoor_lauv, col="black", alpha=0.8)+
  geom_sf(data=strava_lauv, aes(fill="Strava/Outdoorvision"), alpha=0.8)+
  geom_sf(data=inat_lauv, aes(fill="iNat/PlantNet"), alpha=0.5, size=1.2, col="#784421ff")+
  scale_fill_manual(values=c("black", "white"), name="")+
  geom_sf(data=plantnet_lauv, alpha=0.5, size=1.2, col="#784421ff")+
  geom_sf(data=suiviSF_lauv, aes(col=Type), pch=17, size=ifelse(suiviSF_lauv$Type=="Eco-compteur", 6, 4))+
  scale_color_manual(values=c("#99fb0cff", "#3f77eeff", "#6ca618ff", "#852fa7ff"))+
  theme_void()

cowplot::save_plot("2.Outputs/Plots/2_Carte_raw_data.png", G_2rawmap, base_height=9, base_width=9)





### CARTOGRAPHIE DIFFUSION CHEMINS -------- 
## Ajouter distance au parking + altitude
alt_raster <- rast("0.Data/Altitude/France_metropolitaine.tif") %>%
  crop(., ext(st_transform(lauvitel_shp, crs(.)))) %>%
  replace(., is.na(.), 0)

strava_lauv$alt <- extract(alt_raster, st_centroid(strava_lauv))$`France_metropolitaine`


## Graphique diffusion et covariables
# Subset a partir du parking et calcul pourcentage
strava_lauv$lat <- st_coordinates(st_transform(st_centroid(strava_lauv), st_crs(4326)))[,2]
strava_lauv$lon <- st_coordinates(st_transform(st_centroid(strava_lauv), st_crs(4326)))[,1]
strava_lauv$Longueur <- as.numeric(st_length(strava_lauv))

strava_lauvPARK <- subset(strava_lauv,
                        strava_lauv$alt>990 &
                        strava_lauv$lat<44.99 &
                        (strava_lauv$lon>6.07 | strava_lauv$lat<44.985) &
                        ! strava_lauv$edgeUID %in% c(65105687, 65105685, 65105705, 65105596, 65105598, 65105705, 65105698, 65105690, 65105697, 65105696, 65105688, 65105689)
                      ) %>%
  st_transform(., st_crs(OSM_tile))

strava_lauvPARK$count_perc <- round(100*strava_lauvPARK$count / sum(strava_lauvPARK$count[strava_lauvPARK$edgeUID %in% c(65105677, 65105684)]))
strava_lauvPARK$Longitude <- st_coordinates(st_centroid(strava_lauvPARK))[,1]
strava_lauvPARK$Latitude <- st_coordinates(st_centroid(strava_lauvPARK))[,2]


strava_lauvPARKSUB <- subset(strava_lauvPARK,
                          strava_lauvPARK$Longueur>100 &
                          strava_lauvPARK$count_perc>0
)

### Plot
G_diffusion <- plot_grid(
  
  ggplot()+
    geom_spatraster_rgb(data =  crop(OSM_tile, 1.1*ext(st_transform(strava_lauv, st_crs(OSM_tile)))), alpha=0.3) +
    geom_sf(data=strava_lauv, aes(col=count), linewidth=1.6)+
    scale_color_viridis_c(trans="log10", name="N")+
    theme_void(),
  
  ggplot()+
    geom_spatraster_rgb(data = crop(OSM_tile, 1.1*ext(strava_lauvPARK)), alpha=0.3) +
    geom_sf(data=strava_lauvPARK, aes(col=count_perc), linewidth=2)+
    scale_color_viridis_c(name="%")+
    geom_label_repel(data=strava_lauvPARKSUB, aes(x=Longitude, y=Latitude, label=count_perc))+
    xlab("")+ylab("")+
    theme_void(),
  
  ncol=2,
  labels=c("a","b")
)

cowplot::save_plot("2.Outputs/Plots/2_DiffusionStrava.png", G_diffusion, base_height=6, base_width=12)



### Diffusion hors sentier donnees iNat
# Calcul distance aux chemins et altitude des points
inatplant_lauv <- rbind(inat_lauv[,"geometry"], plantnet_lauv[,"geometry"])
inatplant_lauv$distChemin <- as.numeric(st_distance(inatplant_lauv, st_combine(strava_lauv)))
inatplant_lauv$alt <- extract(alt_raster, inatplant_lauv)$`France_metropolitaine`

# Subset les points avec precision de moins de 20m
inatplant_lauvSUB <- subset(inatplant_lauv, inatplant_lauv$distChemin<500) # Chemin qui n'est pas sur la carte, ce n'est pas de la diffusion

G_diffchemin <- ggplot(inatplant_lauvSUB)+
  geom_point(aes(x=alt, y=distChemin))+
  geom_hline(yintercept=20, linetype="dashed")+
  xlab("Altitude") + ylab("Distance aux chemins")+
  theme_minimal()
cowplot::save_plot("2.Outputs/Plots/2_DiffuChemins.png", G_diffchemin, base_height=4, base_width=6)

prop.table(table(cut(inatplant_lauvSUB$distChemin, c(0,20,50,100,200))))


### HEURES DE QUIETUDE -------- 

# Strava
strava_heures_raw <- read.csv("0.Data/Strava/slauvitel_heure_2023_IS/766253e5f27a63da2e978c0544c40f1ab6bc8d44268271a3ac2f230ef04bfa23-1756308792952.csv")
strava_heures_raw$Heure <- substr(strava_heures_raw$hour, 12, 13)
strava_heures_raw$MoisNum <- strava_heures_raw$hour %>% as.Date() %>% format(., "%m") %>% as.numeric(.)
strava_heures_raw$Mois <- Mois_noms$Nom_complet[match(strava_heures_raw$MoisNum, Mois_noms$Num)] %>% factor(., levels=Mois_noms$Nom_complet)

strava_heures <- strava_heures_raw %>%
  subset(., Mois %in% c("Juin", "Juillet", "Aout", "Septembre")) %>%
  group_by(edge_uid, Mois, Heure) %>%
  summarise(Passages=sum(total_trip_count))
strava_heures$alt <- strava_lauv$alt[match(strava_heures$edge_uid, strava_lauv$edgeUID)]

G_HeureStrava <- ggplot(strava_heures)+
  geom_point(aes(x=Heure, y=alt, size=Passages))+
  ylab("Altitude")+
  facet_wrap(~Mois, ncol=4)+
  theme_minimal()

cowplot::save_plot("2.Outputs/Plots/2_HeureStrava.png", G_HeureStrava, base_height=6, base_width=14)


# Pieges photo
photodata_lauv$Heure <- substr(photodata_lauv$date, 12, 13)
photodata_lauv$MoisNum <- photodata_lauv$mois %>% substr(., 1, 2) %>% as.numeric(.)
photodata_lauv$Mois <- Mois_noms$Nom_complet[match(photodata_lauv$MoisNum, Mois_noms$Num)] %>% factor(., levels=Mois_noms$Nom_complet)
photodata_lauv$Jour <- as.Date(photodata_lauv$date)
photodata_lauv$Nom_spatial[photodata_lauv$Nom_spatial=="Lauvitel/Ecocompteur"] <- "Lauvitel/Eco-compteur"

photo_heure <- photodata_lauv %>%
  subset(., Jour %in% as.Date(as.Date("2023-08-14"):as.Date("2023-09-25"))) %>% # Periode restreinte de bon fonctionnement des 4 pieges photos
  group_by(Nom_spatial, Mois, Heure) %>%
  summarise(Passages=sum(person))

G_HeurePhoto <- ggplot(photo_heure)+
  geom_point(aes(x=Heure, y=sub("Lauvitel/", "", Nom_spatial), size=Passages))+
  ylab("Nom")+
  facet_wrap(~Mois)+
  theme_minimal()

cowplot::save_plot("2.Outputs/Plots/2_HeurePhoto.png", G_HeurePhoto, base_height=4, base_width=10)



### DIFFERENTES ACTIVITES ---- 

## Pieges photo -> conclusion, ce serait interessant d'avoir des compteurs un peu plus haut ?
photoTot_lauv <- subset(photodata_lauv, Mois %in% c("Juin", "Juillet", "Aout", "Septembre")) %>%
  .[,c("Nom_spatial", "Child", "Adult", "Dog", "Jour")] %>%
  reshape2::melt(., id.vars=c("Nom_spatial", "Jour"), variable.name="Type", value.name="Count") %>%
  group_by(Nom_spatial, Type, Jour) %>% summarise(Count=sum(Count, na.rm=T))

ggplot(photoTot_lauv)+
  geom_bar(aes(x=Nom_spatial, y=Count, fill=Type), stat="identity", position="dodge")+
  xlab("")+
  scale_y_continuous(trans="log10")

photoHeure_lauv <- subset(photodata_lauv, Mois %in% c("Juin", "Juillet", "Aout", "Septembre")) %>%
  group_by(Nom_spatial, Heure) %>% summarise(Person=sum(person, na.rm=T), Adult=sum(Adult, na.rm=T), Child=sum(Child, na.rm=T), Dog=sum(Dog, na.rm=T)) %>%
  mutate(Nom=sub("Lauvitel/", "", Nom_spatial))

G_chiens <- ggplot(photoHeure_lauv)+
  geom_point(aes(x=Heure, y=Dog, col=Nom))+
  ylab("Canidés")+
  theme_minimal()

cowplot::save_plot("2.Outputs/Plots/2_Chiens.png", G_chiens, base_height=4, base_width=8)


### ENQUETES -------- 
enquetes_lauv <- subset(enquetes, localisation=="la_danchere")

# Duree de sejour
round(prop.table(table(enquetes_lauv$profil.duree_sejour[enquetes_lauv$profil.duree_sejour != ""])),3)

# Logement
round(prop.table(table(enquetes_lauv$profil.lieu_sejour[enquetes_lauv$profil.lieu_sejour != ""])),3)

# Duree randonnee
round(prop.table(table(enquetes_lauv$profil.activite_sentier[enquetes_lauv$profil.lieu_sejour != ""])),3)

