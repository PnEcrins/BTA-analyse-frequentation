
## Choix de mon echelle : basins versants mais avec le Veneon coupe entre les Deux Alpes et St Christophe en Oisans (a valider avec PNE)
veneon <- subset(basinsSF, TopoOH=="Le Vénéon")
christophe <- st_read("0.Data/commune-frmetdrom/Saint_christophe_sur_oisans_manuel.shp")
veneon_coupe1 <- st_intersection(veneon, st_transform(christophe, st_crs(veneon))) %>% subset(., select=names(basinsSF)) %>% mutate(TopoOH="Le Vénéon Est")
veneon_coupe2 <- st_difference(veneon, veneon_coupe1) %>% subset(., select=names(basinsSF)) %>% mutate(TopoOH="Le Vénéon Ouest")
basinModif <- rbind(basinsSF[basinsSF$TopoOH != "Le Vénéon",], veneon_coupe1, veneon_coupe2) %>%
  st_intersection(st_combine(PNE_secteursPROJ)) %>%
  mutate(Area=as.numeric(st_area(.))) %>%
  subset(., Area>(10^7))






# Bassins croisés avec chemins pour carto manuelle par PNE

cheminModif <- chemins %>%
  group_by(nom) %>%
  summarise() %>%
  st_transform(., st_crs(basinModif)) %>%
  mutate(Area=as.numeric(st_length(.))) %>%
  subset(., Area>1000)

cheminsBV <- st_intersection(st_transform(cheminModif, st_crs(basinModif)), basinModif) %>%
  mutate(Area=as.numeric(st_length(.))) %>%
  subset(., Area>1000)

TabDF <- as.data.frame(table(cheminsBV$nom))
cheminsBV$N_chemins <- TabDF$Freq[match(cheminsBV$nom, TabDF$Var1)]
cheminsBV$Nom_cheminBV <- paste0(cheminsBV$nom, ifelse(cheminsBV$N_chemins>1, paste0(" ; ", cheminsBV$Nom), ""))

cheminsBV_uniq <- cheminsBV
cheminsBV_uniq$Uniq <- st_equals(cheminsBV)
cheminsBV_uniq <- distinct(cheminsBV_uniq, Uniq, .keep_all = T)

ggplot()+
  geom_sf(data=basinModif, fill="white")+
  geom_sf(data=cheminsBV_uniq, aes(col=Nom_cheminBV), show.legend=F)+
  theme_void()

cheminsBV_tosave <- cheminsBV_uniq[, "geometry"]
cheminsBV_tosave$Nom_cheminBV = cheminsBV_uniq$Nom_cheminBV
cheminsBV_tosave$Nom_cheminOriginal = cheminsBV_uniq$nom
cheminsBV_tosave$BassinVersant = cheminsBV_uniq$Nom
cheminsBV_tosave$Nom_PNE = NA

st_write(cheminsBV_tosave, "0.Data/Donnees_carto_PNE/Chemins_bis/Chemins_PNE_BTA_manuel.shp")
