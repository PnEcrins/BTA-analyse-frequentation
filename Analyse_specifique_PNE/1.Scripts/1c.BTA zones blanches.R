

### Charge les données
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps")
source("1.Scripts/0.BTA preparation donnees.R")


library(ggrepel)
library(cowplot)



### CARTES BASSINS VERSANTS ET CHEMINS
# Basins
basinModif$Nom2 <- revalue(basinModif$PNE, c(
  "Vallée de Réallon et forêt de Boscodon"="Vallée de Réallon et\n forêt de Boscodon",
  "Plateau d'Emparis et glacier de la Girose"="Plateau d'Emparis et\n glacier de la Girose",
  "Col d'Ornon à Bourg d'Oisans"="Col d'Ornon à\n Bourg d'Oisans",
  "Torrent de la Séveraissette et de la Muande"="Torrent de la\n Séveraissette\n et de la Muande",
  "Vallons suspendus du Lauvitel et de la Muzelle"="Vallons suspendus\n du Lauvitel\n et de la Muzelle",
  "Source de la Romanche/La Meije"="Source de la\n Romanche/La Meije",
  "Entrée de vallée du Valgaudemar"="Entrée de vallée\n du Valgaudemar",
  "Fond de vallée du Valgaudemar"="Fond de vallée\n du Valgaudemar",
  "Drac de Champoléon"="Drac de\n Champoléon",
  "Pont-du-Fossé/Ancelle"="Pont-du-Fossé/ \nAncelle",
  "Le Monetier les Bains"="\nLe Monetier\n les Bains"
))

Map_BV <- ggplot(basinModif)+
  geom_sf()+
  geom_sf(data=st_transform(PNE, PROJ_Lambert), fill="white", col="white")+
  geom_sf(fill=NA)+
  geom_sf_text(data=st_centroid(basinModif), label=basinModif$Nom2, size=3.5)+
  theme_void()
cowplot::save_plot(paste0("2.Outputs/Plots/1c_Empty_Bassin.png"), Map_BV, base_height=8, base_width=6)


# Chemins
Chemins_DF <- as.data.frame(st_coordinates(st_centroid(cheminsManu_line))) ; Chemins_DF$X_near<-Chemins_DF$Y_near<-NA

for(i in 1:nrow(Chemins_DF)){ # To get points on line closest to centroid
  site.nearest_line <- st_nearest_points(st_centroid(cheminsManu_line[i,]), cheminsManu_line[i,])
  site_nearest_point <- st_cast(site.nearest_line, "POINT")[2]
  Coords <- st_coordinates(site_nearest_point)
  Chemins_DF$X_near[i]<-Coords[1,1]
  Chemins_DF$Y_near[i]<-Coords[1,2]
}

Chemins_DF$Nom <- sapply(strsplit(cheminsManu_line$Nom, " "), function(x) { # Pour sauter une ligne au quatrieme espace
  g <- seq_along(x)
  g[g < 4] <- 4
  g[g > 5] <- 5
  paste(tapply(x, g, paste, collapse = " "), collapse = "\n")
})

Map_chemins <- ggplot(cheminsManu_line)+
  geom_sf(data=PNE_secteursPROJ)+
  geom_sf(data=st_transform(PNE, PROJ_Lambert), fill="white", col="white")+
  geom_sf(data=PNE_secteursPROJ, fill=NA)+
  geom_sf(col="darkred", alpha=0.8)+
  geom_text_repel(data=Chemins_DF, aes(x=X_near, y=Y_near, label=Nom), size=1.8, max.overlaps = 1000)+
  theme_void()
cowplot::save_plot(paste0("2.Outputs/Plots/1c_Empty_Chemins.png"), Map_chemins, base_height=10, base_width=7)


############################
### CARTE ZONES BLANCHES ###
############################

### Identifier les dispositifs de suivi encore actifs
ecocompteurSF$actif <- ifelse(ecocompteurSF$name %in% unique(eco_mois$Nom_spatial[eco_mois$Annee==2024]), T, F)
refugeSF$actif <- ifelse(refugeSF$nom %in% unique(refuges_nuits$Nom_spatial[refuges_nuits$Annee==2024]), T, F)
routierSF$actif <- ifelse(routierSF$name %in% unique(rout_mois$Nom_spatial[rout_mois$Annee==2024]), T, F)
mdpSF$actif <- ifelse(mdpSF$NOM2 %in% unique(visites_mdp$Nom_spatial[visites_mdp$Annee==2024 & visites_mdp$Value>0]), T, F)
photoSF$actif <- ifelse(photoSF$Nom %in% unique(photo_mois$Nom_spatial[photo_mois$Annee==2024]), T, F)


### Combiner les points de suivi
points_combined <- ecocompteurSF %>% mutate(Type="Eco-compteur") %>%
  rbind.fillSF(., refugeSF %>% mutate(Type="Refuge")) %>%
  rbind.fillSF(., routierSF %>% mutate(Type="Routier")) %>%
  rbind.fillSF(., mdpSF %>% mutate(Type="Maison de parc")) %>%
  rbind.fillSF(., photoSF %>% mutate(Type="Photo"))

points_combinedPROJ <- st_transform(points_combined, PROJ_Lambert)

G_map <- ggplot(points_combined[points_combined$actif==T,])+
  geom_sf(data=basinModif)+
  geom_sf(aes(col=Type))+
  theme_minimal()
cowplot::save_plot(paste0("2.Outputs/Plots/1c_Dispositifs.png"), G_map, base_height=5, base_width=5)


### Charger frequentation
freq_std <- rast("2.Outputs/frequentation_standard_PNE.tif")
freq_chemins <- st_read("2.Outputs/Frequentation_standard_chemins.shp")

### Fonction d'analyse
BTA_Zonesblanches <- function(shp_blanches, Freq_fun, Eco_Only=F){
  
  echelle <- ifelse(nrow(shp_blanches)>50, "Chemins", "Bassins versants")
  
  shp_blanches$ecocompteurs <- lengths(st_intersects(shp_blanches, points_combinedPROJ[points_combinedPROJ$Type=="Eco-compteur" & points_combinedPROJ$actif==T,]))
  shp_blanches$routier <- lengths(st_intersects(shp_blanches, points_combinedPROJ[points_combinedPROJ$Type=="Routier" & points_combinedPROJ$actif==T,]))
  shp_blanches$photo <- lengths(st_intersects(shp_blanches, points_combinedPROJ[points_combinedPROJ$Type=="Photo" & points_combinedPROJ$actif==T,]))
  shp_blanches$refuge <- lengths(st_intersects(shp_blanches, points_combinedPROJ[points_combinedPROJ$Type=="Refuge" & points_combinedPROJ$actif==T,]))
  shp_blanches$mdp <- lengths(st_intersects(shp_blanches, points_combinedPROJ[points_combinedPROJ$Type=="Maison de parc" & points_combinedPROJ$actif==T,]))
  
  ### Cumul des outils de suivi
  shp_blanches$Suivi <- shp_blanches$ecocompteurs + shp_blanches$routier + shp_blanches$photo + shp_blanches$refuge + shp_blanches$mdp
  if(Eco_Only==T){shp_blanches$Suivi <- shp_blanches$ecocompteurs}
  
  ### Cumul des frequentations standardisees
  if(class(Freq_fun)[1]=="SpatRaster"){
  shp_blanches$Freq_mean <- exactextractr::exact_extract(Freq_fun$Freq, shp_blanches, "mean")
  } else {
    shp_blanches$Freq_mean <- Freq_fun$Frq_std[match(shp_blanches$Nom, Freq_fun$Nom)]
  }
  
  ### Graphique
  shp_map <- shp_blanches %>%
    st_buffer(., ifelse(echelle=="Chemins", 150, 0)) %>% # Buffer pour mieux voir les chemins sur la carte
    mutate(Suivi = replace(Suivi, Suivi==0, NA))
  
  G_empty <- ggplot(shp_map)
  if(echelle=="Chemins"){
    G_empty <- G_empty+
      geom_sf(data=PNE_secteursPROJ)+
      geom_sf(data=st_transform(PNE, PROJ_Lambert), fill="white", col="white")+
      geom_sf(data=PNE_secteursPROJ, fill=NA)
  }
  
  G_blanches <- plot_grid(
    
    plot_grid(
      G_empty+
        geom_sf(aes(fill=Suivi), col=NA)+
        scale_fill_viridis_c(na.value="grey70")+
        theme_void()+theme(plot.background=element_rect(fill="white")),
      
      G_empty+
        geom_sf(aes(fill=Freq_mean), col=NA)+
        scale_fill_viridis_c(name="Frequentation")+
        theme_void()+theme(plot.background=element_rect(fill="white")),
      
      ncol=2, labels=c("a","b")),
    
    plot_grid(
      
      ggplot(shp_blanches)+
        geom_point(aes(x=Suivi, y=Freq_mean, size=Area), show.legend=F)+
        geom_text_repel(aes(x=Suivi, y=Freq_mean, label=Nom), size=2.5, col="gray40")+
        ylab("Frequentation moyenne")+
        theme_minimal(),
      
      ncol=1,
      labels="c"
    ), ncol=1
  ) + theme(plot.background = element_rect(fill = "white"))
  
  # Sauvegarder tableau
  shp_blanches<-as.data.frame(shp_blanches[,c("Nom", "Suivi", "ecocompteurs", "routier", "photo", "refuge", "mdp", "Freq_mean")]) ; shp_blanches$geometry<-NULL ; shp_blanches$Frequentation<-round(shp_blanches$Freq_mean,1)
  shp_blanches<-shp_blanches[order(shp_blanches$Suivi, shp_blanches$Frequentation, decreasing=T),]
  if(Eco_Only==F){write.csv(shp_blanches, paste0("2.Outputs/Plots/1c_Table_", echelle, ".csv"), row.names=F)}
  
  cowplot::save_plot(paste0("2.Outputs/Plots/1c_Zones blanches (", echelle, ifelse(Eco_Only, " Ecocompteur_only", ""), ").png"), G_blanches, base_height=6, base_width=8)
}


### Analyses
BTA_Zonesblanches(basinModif, freq_std)
BTA_Zonesblanches(basinModif, freq_std, Eco_Only = T)
BTA_Zonesblanches(cheminsManu, freq_chemins)




