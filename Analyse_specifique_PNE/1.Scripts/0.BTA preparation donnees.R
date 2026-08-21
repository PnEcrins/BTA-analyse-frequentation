
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps")

library(sf)
library(ggplot2)
library(dplyr) ; library(plyr)
library(leafpop)
library(readxl) ; library(openxlsx)
library(reshape2)
library(plotly)
library(terra)
library(htmltools)
library(gridExtra)
library(ggtext) # Necessaire pour mettre des mots en italique dans les sous-titres

### Charger fonctions creees pour ce script
source("1.Scripts/0.BTA fonctions preparation donnees.R")


### CRS a utiliser
PROJ_Lambert <- st_crs(2154)
PROJtxt_Lambert <- "+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"


##### Charger donnees cartographiques ------

### Limites PNE
PNE <- st_read("0.Data/Donnees_carto_PNE/PNE_coeur/PNE_coeur.shp") %>% st_transform(., st_crs(4326))
PNE_secteurs <- st_read("0.Data/Donnees_carto_PNE/Secteurs/Secteurs_PNE.shp") %>% st_transform(., st_crs(4326))
PNE_secteursPROJ <- st_transform(PNE_secteurs, PROJ_Lambert)
PNE_completPROJ <- st_union(PNE_secteursPROJ)

### Reserves / N2000
N2000 <- st_read("0.Data/Donnees_carto_PNE/N2000_PNE.shp/") %>% st_transform(., st_crs(4326)) %>% mutate(Popup=paste0("<b>Zone Natura 2000 : ", .$nom, "</b>"))
reserves <- st_read("0.Data/Donnees_carto_PNE/PNE_reserves/reserves.shp") %>% st_transform(., st_crs(4326)) %>% mutate(Popup=paste0("<b>Reserve : ", .$reserve, "</b>"))

### Chemins (nommes par Juliette)
cheminsManu_line <- st_read("0.Data/Donnees_carto_PNE/Chemins/Chemins_PNE_BTA_manuel.shp") %>%
  subset(., grepl("SUPPRIMER", .$NOMPNE)==F) %>%
  dplyr::group_by(NOMPNE) %>% dplyr::summarise(N=n()) %>%
  mutate(Area=as.numeric(st_length(.)), Nom=NOMPNE)
cheminsManu <- st_buffer(cheminsManu_line, 50)
ggplot(cheminsManu)+geom_sf(aes(col=NOMPNE), show.legend = F)

### Bassins versants
basinModif <- st_read("0.Data/BassinsVersants/BassinVersantModifie.shp") %>%
  subset(., Area>5*10^7) %>%
  mutate(Nom=PNE)
ggplot()+geom_sf(data=basinModif, aes(fill=Nom), show.legend=F)



##### Maison du parc -----

### Ajout manuel de certaines maisons de parc
MDPAddManu <- st_as_sf(
  data.frame(NOM2=c("La Grave (SI)", "Freissinières", "La Bérarde centre CAF"),
             Lon=c(6.303629550337713, 6.535778064351305, 6.293496996781966),
             Lat=c(45.04535939676773, 44.752145211554186, 44.93257675714347)
  ), 
  coords = c("Lon","Lat"), crs=st_crs(4326), remove = FALSE)


### Charger fichier spatial des MDP
mdpSF <- st_read("0.Data/Donnees_carto_PNE/Maisons du Parc/maisons_centres_pne.shp") %>% 
  st_transform(., st_crs(4326)) %>%
  rbind.fillSF(., MDPAddManu) %>%
  mutate(Lon=st_coordinates(.)[,1], Lat=st_coordinates(.)[,2]) %>%
  mutate(Popup=NA) %>%
  mutate(NOM2=revalue(.$NOM2, c("Maison de la vallee"="Maison de la vallée", "Pre de Mme Carle"="Pré de Mme Carle", "Musee des mineraux"="Musée des mineraux"), warn_missing=F))

### Visites suivi annuel (en utilisant le 'bilan par annee' pour avoir les totaux depuis 1993)
visites_mdp_raw <- read.csv("0.Data/Frequentation_MDP_issuedebilanparannee.csv", sep=",")
visites_mdp <- visites_mdp_raw %>% 
  group_by(Nom_spatial) %>% # Somme par Nom_spatial parce que certaines MDP ont ete herbergees dans plusieurs lieux et ont donc plusieurs lignes dans le tableau de suivi
  summarise_if(is.numeric, sum, na.rm=T) %>%
  reshape2::melt(., id.vars=c("Nom_spatial"), measure.vars=names(.)[substr(names(.), 1, 1)=="X"], variable.name="Annee", value.name="Value") %>%
  mutate(Annee=as.numeric(sub("X", "", .$Annee))) %>%
  subset(., .$Nom_spatial != "SUPPRIMER")

### Visites saisonnieres sur les 10 dernieres annees (utile pour le spatial et le saisonnier)
mdp_saison <- data.frame()
for(AN in 2016:2024){
  mdp_an <- readODS::read_ods("0.Data/GLOBAL FREQUENTATION_MdP_2024_JF.ods", sheet=as.character(AN), col_names=F)
  ROW_with_names <- which(mdp_an[,2]=="janvier")
  names(mdp_an) <- as.character(mdp_an[ROW_with_names,]) ; names(mdp_an)[1] <- "Nom"
  mdp_an <- mdp_an[(ROW_with_names+1):nrow(mdp_an),1:13]
  mdp_an <- subset(mdp_an, is.na(mdp_an$Nom)==F & mdp_an$Nom!="Total")
  mdp_an_melt <- reshape2::melt(mdp_an, id.vars=c("Nom"), measure.vars=names(mdp_an)[2:13], variable.name="Mois", value.name="Comptage") %>% mutate(Annee=AN)
  mdp_saison <- rbind.fill(mdp_saison, mdp_an_melt) %>% subset(., .$Nom != "Briançon") # Cellules fusionnees qui posent probleme
}

### Somme annuelle des donnees de juillet / aout
mdp_ete <- mdp_saison %>%
  subset(., Mois %in% c("juillet", "août")) %>%
  ddply(., .(Nom, Annee), function(x){data.frame(
    Comptage = sum(as.numeric(x$Comptage), na.rm=T),
    Mois_NA = sum(is.na(x$Comptage))
  )})
mdp_ete$Mois_NA[mdp_ete$Nom=="Prapic" & mdp_ete$Annee==2024]<-0 # la valeur est en fait la valeur pour juillet+aout (cellules fusionnees)

### Ajouter nom spatial
mdp_ete$Nom_spatial <- visites_mdp_raw$Nom_spatial[match(mdp_ete$Nom, replace(visites_mdp_raw$MDP, visites_mdp_raw$MDP=="Pré Mme Carle (1986)", "Pré Mme Carle"))] %>% replace(., .=="Siege ", "Siege")

### Projection
mdpPROJ <- mdpSF %>% st_transform(., PROJ_Lambert)

### Preparer popup pour carte interactive
for(i in 1:nrow(mdpSF)){mdpSF$Popup[i] <- BAT_PopupTempoPlot(visites_mdp, mdpSF$NOM2[i], type="mdp")}






##### Refuges ------
### Charger fichier spatial
refugeSF <- st_read("0.Data/Donnees_carto_PNE/Refuges/refuges.shp") %>% 
  st_transform(., st_crs(4326)) %>%
  mutate(Lon=st_coordinates(.)[,1], Lat=st_coordinates(.)[,2]) %>%
  mutate(Popup=NA)


### Charger donnees nuitees
refuges_nuits <- read.csv("0.Data/Nuitees refuge/Nuitees_refuges_issudocument2024_Juliette.csv", sep=",") %>% 
  subset(., Commentaire.JF=="") %>%
  reshape2::melt(., id.vars=c("Nom_spatial", "capacite_accueil"), measure.vars=names(.)[substr(names(.), 1, 1)=="X"], variable.name="Annee", value.name="Value") %>%
  mutate(Annee=as.numeric(sub("X", "", .$Annee)))

### Supprimer les refuges sans donnees de frequentation (notamment FFCAM), ils ne nous servent pas pour ces analyses et donc n'ont rien a faire sur la carte interactive
refugeSF <- subset(refugeSF, refugeSF$nom %in% refuges_nuits$Nom_spatial)

### Projection
refugePROJ <- refugeSF %>% st_transform(., PROJ_Lambert)

### Ajouter popup
for(i in 1:nrow(refugeSF)){refugeSF$Popup[i] <- BAT_PopupTempoPlot(refuges_nuits, refugeSF$nom[i], type="refuge")}


### Destinations refuges (questionnaire aux utilisateurs de refuge sur leur course du lendemain, spatialise par Juliette)
# Preparer coordonnees : au prealable il faut transformer les espaces (qui sont d'un format bizarre) en vrais espaces avec un CTRL+H dans LibreOffice
refuge_desti <- readODS::read_ods("0.Data/Refuge_destinations.ods") %>% subset(. , .$Total_2022>0 & is.na(.$X_destination)==F)
names(refuge_desti) <- revalue(names(refuge_desti), c("X_destination"="Y_destination", "Y_destination"="X_destination"))
refuge_desti$X_depart <- refugeSF$Lon[match(refuge_desti$Refuge_de_depart, refugeSF$nom)]
refuge_desti$Y_depart <- refugeSF$Lat[match(refuge_desti$Refuge_de_depart, refugeSF$nom)]
refuge_desti$X_depart[refuge_desti$Refuge_de_depart=="Refuge Adèle Planchard"] <- 6.348761959213285 # Adele Planchard n'est pas dans les donnees frequentation du parc, mais on a des infos sur la destination des utilisateurs
refuge_desti$Y_depart[refuge_desti$Refuge_de_depart=="Refuge Adèle Planchard"] <- 44.969637711743886
table(is.na(refuge_desti$Y_depart))

# Transformer en fichier spatial
refuge_destiSF <- data.frame(
  Refuge_de_depart=c(refuge_desti$Refuge_de_depart, refuge_desti$Refuge_de_depart),
  Nom_course=c(refuge_desti$Nom_course, refuge_desti$Nom_course),
  Lon=c(refuge_desti$X_depart, refuge_desti$X_destination),
  Lat=c(refuge_desti$Y_depart, refuge_desti$Y_destination)
) %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = "+proj=longlat +datum=WGS84") %>%
  group_by(Refuge_de_depart, Nom_course) %>%
  dplyr::summarize() %>%
  st_cast("LINESTRING")

# Ajouter une colonne avec le total du refuge pour passer en proportions
desti_group <- refuge_desti %>% group_by(Refuge_de_depart) %>% summarise(Total=sum(Total_2022))
refuge_desti$Total_refuge <- desti_group$Total[match(refuge_desti$Refuge_de_depart, desti_group$Refuge_de_depart)]

# Preparer texte pour popup sur carte interactive + epaisseur des traits
refuge_desti$Pop <- paste0("<b>Relevé destinations été 2022 : <br></b>", "Depart : '", refuge_desti$Refuge_de_depart, "' <br>Course : '", refuge_desti$Nom_course, "'<br>N=", refuge_desti$Total_2022, " (", round(100*(refuge_desti$Total_2022/refuge_desti$Total_refuge)), "% des personnes depuis ce refuge)")
refuge_desti$Epaisseur <- 1.5+refuge_desti$Total_2022/refuge_desti$Total_refuge*20
refuge_destiSF <- left_join(refuge_destiSF, refuge_desti, by=c("Refuge_de_depart", "Nom_course"))




##### Ecocompteurs ------
### Charger fichier spatial
ecocompteurSF <- st_read("0.Data/0.Carto_monitoring/ecocompteur.shp") %>% 
  st_transform(., st_crs(4326)) %>% 
  mutate(Lon=st_coordinates(.)[,1], Lat=st_coordinates(.)[,2]) %>%
  mutate(Popup=.$name)


### Charger donnees compteurs
ecocompteurs_data <- data.frame()

for(FILE in list.files("0.Data/Ecocompteurs/") %>% subset(., grepl(".csv", .)) %>% subset(., grepl("-velo", .)==F)){
  eco_new <- read.csv(paste0("0.Data/Ecocompteurs/", FILE), sep=";")
  if(ncol(eco_new)==1){ # On a deux formats, un avec des points-virgules, un avec des virgules
    eco_new <- read.csv(paste0("0.Data/Ecocompteurs/", FILE), sep=",")
    } else {
    eco_new$person <- eco_new$date ; eco_new$date <- rownames(eco_new)
  }
  eco_new$filename <- sub(".csv", "", FILE)
  
  # Ajouter donnees velos pour compteur Serre du coin ou Barriere
  if(eco_new$filename[1] %in% c("EcocompteurSerreduCoin", "EcocompteurBarriere")){
    if(eco_new$filename[1]=="EcocompteurSerreduCoin"){eco_velo <- read.csv("0.Data/Ecocompteurs/Serre_pieton-velo.csv")}
    if(eco_new$filename[1]=="EcocompteurBarriere"){eco_velo <- read.csv("0.Data/Ecocompteurs/Labarriere_pieton-velo.csv")}
    eco_velo$date <- date_trans(eco_velo$Time)
    eco_new <- left_join(eco_new, eco_velo, "date")
    eco_new$pietons <- eco_new$Piétons.IN+eco_new$Piétons.OUT
    eco_new$velos <- eco_new$Vélos.IN+eco_new$Vélos.OUT
    eco_new <- subset(eco_new, select=c("date", "person", "filename", "pietons", "velos"))
    }
  
  # Ajouter au tableau global
  ecocompteurs_data <- rbind.fill(ecocompteurs_data, eco_new)
}

### Formatter les dates
ecocompteurs_data$mois <- format(as.Date(ecocompteurs_data$date), "%m/%Y")
ecocompteurs_data$quinzaine <- ifelse(as.numeric(format(as.Date(ecocompteurs_data$date), "%d"))<15.5, "_1", "_2")
ecocompteurs_data$Nom <- sub("Ecocompteur", "", ecocompteurs_data$filename)

### Corrections manuelles pour enlever des donnees de troupeaux (choix faits par Juliette)
ecocompteurs_data$Day_Cpt <- paste0(as.Date(ecocompteurs_data$date), ecocompteurs_data$Nom)
ecocompteurs_data <- subset(ecocompteurs_data, ! ecocompteurs_data$Day_Cpt %in% c("2021-08-05Confolens", "2021-08-06Confolens", "2021-08-08Confolens", "2021-08-09Confolens", "2021-08-10Confolens",
                                                                "2021-06-06Danchere", "2021-07-11Danchere", "2022-06-18Danchere",
                                                                "2021-07-14LesGourniers", "2023-07-14LesGourniers", "2024-07-14LesGourniers", "2022-07-15LesGourniers", "2023-07-15LesGourniers", "2022-07-17LesGourniers", "2022-07-19LesGourniers", "2021-08-21LesGourniers",
                                                                "2021-08-17PreMmeCarle",
                                                                "2021-09-12Surette", "2022-07-22Surette", "2024-07-23Surette",
                                                                "2023-09-23Valsenestre", "2022-09-24Valsenestre", "2023-09-26Valsenestre", "2022-09-28Valsenestre", "2022-09-29Valsenestre", "2022-09-30Valsenestre",
                                                                "2019-07-31LacDouche", "2019-09-09LacDouche", "2019-09-10LacDouche", "2019-09-11LacDouche",
                                                                "2015-06-29LacVallon"
                                                                
))


### Agreger les donnees par quinzaine
eco_mois <- ddply(ecocompteurs_data, .(Nom, mois, quinzaine), function(x){data.frame(
  Annee=format(as.Date(x$date[1]), "%Y"), Mois=format(as.Date(x$date[1]), "%m"),
  Jour1=min(as.Date(x$date[is.na(x$person)==F]), na.rm=T),
  Jour2=max(as.Date(x$date[is.na(x$person)==F]), na.rm=T),
  Nb_jours=length(unique(as.Date(x$date))),
  Value=sum(as.numeric(x$person), na.rm=T) %>% replace(., .==0, NA),
  Value_pietons=sum(as.numeric(x$pietons), na.rm=T),
  Value_velos=sum(as.numeric(x$velos), na.rm=T)
)}) %>% 
  mutate(Mois_nom=Mois_noms$Nom[match(as.numeric(.$Mois), Mois_noms$Num)]) %>% 
  mutate(quinzaine=paste0(.$Mois_nom, .$quinzaine)) %>%
  mutate(Value_velos=replace(.$Value_velos, is.na(.$Value) | (! .$Nom %in% c("Barriere", "SerreduCoin")), NA)) %>%
  mutate(Value_pietons=replace(.$Value_pietons, is.na(.$Value) | (! .$Nom %in% c("Barriere", "SerreduCoin")), NA))

### Ajouter le nom spatial
eco_correspondance <- read.table("0.Data/Ecocompteurs/Table_correspondance_Ecocompteurs.txt")
eco_mois$Nom_spatial <- eco_correspondance$Nom_spatial[match(eco_mois$Nom, eco_correspondance$Nom_data)]

### Transformer projection
ecocompteurPROJ <- ecocompteurSF %>% st_transform(., PROJ_Lambert)

### Ajouter popup pour carte interactive
for(i in 1:nrow(ecocompteurSF)){ecocompteurSF$Popup[i] <- BAT_PopupDoublePlotly(eco_mois, ecocompteurSF$name[i], type="ecocompteur")}





##### Pieges photo ------ 
### Charger fichier spatial
photoSF <- read.csv("0.Data/Piege_photo_localisation_Victor.csv", sep=",") %>% 
  st_as_sf(., coords = c("Lon","Lat"), remove = FALSE, crs=st_crs(4326))

### Charger donnees nettoyees de pieges photo
photo_data_raw <- data.frame()

for(FILE in list.files("0.Data/Donnees_piege-photo_07.07.25/", recursive=T) %>% subset(., grepl(".csv", .))){
  photo_new <- read.csv(paste0("0.Data/Donnees_piege-photo_07.07.25/", FILE), sep=",", row.names=NULL)
  photo_new$filename <- sub(".csv", "", FILE)
  photo_new$Nom_spatial <- FILE %>% strsplit(., "/") %>% unlist(.) %>% .[1:min(length(.)-1, 2)] %>% replace(., .=="haut", "refuge") %>% paste0(., collapse="/")
  photo_data_raw <- rbind.fill(photo_data_raw, photo_new)
}
table(photo_data_raw$Nom_spatial %in% photoSF$Nom)

### Supprimer les doublons (certains fichiers contiennent exactement la meme information)
photo_data <- photo_data_raw %>% distinct(.[, names(.)!="filename"], .keep_all=T)

### Ensuite sommer les doublons restant (notamment des cas ou le revele a ete fait au milieu d'une heure donc on a une ligne a la fin d'un tableau et une ligne au debut d'un autre, il faut faire la somme)
photo_data <- photo_data[, names(photo_data) != "filename"] %>% group_by(date, Nom_spatial) %>% summarise_all(sum)

### Formatter les dates
photo_data$mois <- format(as.Date(photo_data$date), "%m/%Y")
photo_data$quinzaine <- ifelse(as.numeric(format(as.Date(photo_data$date), "%d"))<15.5, "_1", "_2")

### Agreger les donnees par quinzaine
photo_mois <- ddply(photo_data, .(Nom_spatial, mois, quinzaine), function(x){data.frame(
  Annee=format(as.Date(x$date[1]), "%Y"), Mois=format(as.Date(x$date[1]), "%m"),
  Jour1=min(as.Date(x$date[is.na(x$person)==F]), na.rm=T),
  Jour2=max(as.Date(x$date[is.na(x$person)==F]), na.rm=T),
  Nb_jours=length(unique(as.Date(x$date))),
  Value=sum(as.numeric(x$person), na.rm=T) %>% replace(., .==0, NA)
)}) %>% mutate(Mois_nom=Mois_noms$Nom[match(as.numeric(.$Mois), Mois_noms$Num)]) %>% mutate(quinzaine=paste0(.$Mois_nom, .$quinzaine))

### rojection
photoPROJ <- photoSF %>% st_transform(., PROJ_Lambert)

### Ajouter popup pour carte interactive
for(i in 1:nrow(photoSF)){photoSF$Popup[i] <- BAT_PopupDoublePlotly(photo_mois, sub("Photo", "", photoSF$Nom[i]), type="photo")}




##### Compteurs routiers ------
### Charger les donnees spatiales
routierSF <- st_read("0.Data/0.Carto_monitoring/Ancien_compteur_routier.shp") %>%
  st_transform(., st_crs(4326)) %>%
  mutate(Lon=st_coordinates(.)[,1], Lat=st_coordinates(.)[,2]) %>%
  mutate(Popup=.$name, Nom_spatial=name)

# Ajouter les compteurs pas inclus dans Ancien_compteur_routier
rout_correspondance <- read.csv("0.Data/Donnees_routier/Table_Correspondance_Nom_spatial_Routier_Juliette.csv")

routier_new <- rout_correspondance %>%
  subset(., is.na(Lon)==F) %>%
  distinct(Nom_spatial, .keep_all=T) %>%
  st_as_sf(., coords = c("Lon","Lat"), remove=F, crs=st_crs(4326))

routierSF <- rbind.fillSF(routierSF, routier_new)

### Charger les donnees compteurs routiers (une fonction pour les nettoyer, a ne lancer que si on veut mettre a jour les donnees, sinon on charge directement le fichier sauvegarde qui s'appelle 'Routes_merged_raw.rds')
#BTA_PreparerRoutier()
rout_tot <- readRDS("2.Outputs/Routes_merged_raw.rds")

### Petite verification du format des dates a faire manuellement
#table(is.na(rout_tot$Date))
#unique(rout_tot$Date)[1000 + 1:1000]

### Ajouter les noms et nom_spatial
rout_tot$Nom <- ifelse(rout_tot$name1!="character(0)", rout_tot$name1, rout_tot$name2) %>% ifelse(.=="H3", rout_tot$name4, .) %>% gsub(">", "", .) %>% sub("SENS MONTANT", "", .) %>% sub("SENS DESCENDANT", "", .) %>% trimws(.)
rout_tot$Nom_complet <- paste0(ifelse(rout_tot$name1=="character(0)", "", paste0(rout_tot$name1, " - ")), rout_tot$name2, " - ", rout_tot$name3)

rout_tot$Nom_uniq <- rout_correspondance$Nom_uniq[match(tolower(rout_tot$Nom), rout_correspondance$Nom_brut)]
rout_tot$Nom_spatial <- rout_correspondance$Nom_spatial[match(tolower(rout_tot$Nom), rout_correspondance$Nom_brut)]

table(rout_tot$Nom_spatial) ; table(is.na(rout_tot$Nom_spatial))

### Creer un fichier propre (avec moins de colonnes) et ajouter les mois et quinzaines
rout <- subset(rout_tot, is.na(Comptage)==F, select=c("Nom_spatial", "Nom_uniq", "Date", "Comptage", "filename")) # 2900 donnees avec NA pour comptage (car pas de suivi en cours ou alors pas toutes les heures : table(is.na(rout_tot$Comptage), is.na(rout_tot$H1)))
rout$mois <- format(as.Date(rout$Date), "%m/%Y")
rout$quinzaine <- ifelse(as.numeric(format(as.Date(rout$Date), "%d"))<15.5, "_1", "_2")

### Agreger par quinzaines
rout_mois <- ddply(rout, .(Nom_spatial, Nom_uniq, mois, quinzaine), function(x){data.frame(
  Annee=format(as.Date(x$Date[1]), "%Y"), Mois=format(as.Date(x$Date[1]), "%m"),
  Jour1=min(as.Date(x$Date[is.na(x$Comptage)==F]), na.rm=T),
  Jour2=max(as.Date(x$Date[is.na(x$Comptage)==F]), na.rm=T),
  Nb_jours=length(unique(x$Date)),
  Value=sum(x$Comptage), # Somme car pour certains compteurs on a les deux sens
  N_files=length(unique(x$filename)),
  Files=paste(unique(x$filename), collapse=" ")
)}) %>% mutate(Mois_nom=Mois_noms$Nom[match(as.numeric(.$Mois), Mois_noms$Num)]) %>% 
  mutate(quinzaine=paste0(.$Mois_nom, .$quinzaine)) %>% 
  subset(., Nom_spatial !="supprimer")

table(rout_mois$N_files) # Devrait etre 1 (quand on a le cumul montee / descente) ou 2 (quand montee et descente sont dans des fichiers separes); pas de 3 (sinon c'est qu'on a montee / descente / cumulee, on ne peut pas en faire la somme)
unique(rout_mois$Files[rout_mois$N_files==2]) # Verifier que les cas ou on a 2 fichiers ce sont bien des cas ou la montee et la descente sont dans des fichiers separes (ca se voit dans le nom grace a des 1 et 2)

### Projection
routierSF <- subset(routierSF, routierSF$Nom_spatial %in% rout_mois$Nom_spatial)
routierPROJ <- routierSF %>% st_transform(., PROJ_Lambert)

### Ajouter popup pour carte interactive
for(i in 1:nrow(routierSF)){routierSF$Popup[i] <- BAT_PopupDoublePlotly(rout_mois, routierSF$Nom_spatial[i], type="routier")}




##### Enquetes ------
### Charger et preparer le fichier spatial
enqueteSF_raw <- st_read("0.Data/0.Carto_monitoring/lieux_quetionnaire.shp") %>% 
  st_transform(., st_crs(4326))

emparis_milieu <- subset(enqueteSF_raw, NOM_LIEU %in% c("Emparis - Lac lerie", "Emparis - Lac noir")) %>% st_union(.) %>% st_centroid(.) %>% st_as_sf() %>% mutate(NOM_LIEU="Emparis_milieu", GESTIONNAI="CCO/Grave", ID=6) ; st_geometry(emparis_milieu)<-"geometry"
tourond <- st_as_sf(data.frame(Lon=6.2220036, Lat=44.7169399, NOM_LIEU="Tourond", GESTIONNAI=NA, ID=NA), coords = c("Lon","Lat"), remove = FALSE) ; tourond$Lon <- tourond$Lat <- NULL ; st_crs(tourond)<-st_crs(4326)

enqueteSF <- rbind(enqueteSF_raw[! enqueteSF_raw$NOM_LIEU %in% c("Emparis - Lac lerie", "Emparis - Lac noir"),], emparis_milieu) %>% 
  rbind(., tourond) %>%
  mutate(Lon=st_coordinates(.)[,1], Lat=st_coordinates(.)[,2])

### Charger les donnees d'enquete
library(readODS)
enquetes22 <- read_ods("0.Data/Enquetes/Enquete_frequentation_2022-2023-2024/2022/Export_BdD_4 bis.ods") ; enquetes22$filename <- "Export_BdD_4 bis.ods" ; names(enquetes22) <- sub("-", ".", names(enquetes22)) ; enquetes22$date_de_debut_saisie <- as.character(enquetes22$date_de_debut_saisie)
enquetes23 <- read.csv("0.Data/Enquetes/Enquete_frequentation_2022-2023-2024/2023/Enquetes 2023.csv") ; enquetes23$filename <- "Enquetes 2023.csv" ; enquetes23$date_de_debut_saisie <- as.character(enquetes23$date_de_debut_saisie)
enquetes24 <- read.csv("0.Data/Enquetes/Enquete_frequentation_2022-2023-2024/2024/ECRINS_FREQUENTATION_2024.csv") ; enquetes24$filename <- "ECRINS_FREQUENTATION_2024.csv" ; enquetes24$date_de_debut_saisie <- as.character(enquetes24$date_de_debut_saisie)
enquetesTourond <- read_ods("0.Data/Enquetes/Enquete_frequentation_2022-2023-2024/2024/ECRINS_FREQUENTATION_TOURROND modif dep.ods", sheet=2) %>% subset(., is.na(SubmissionDate)==F); enquetesTourond$filename <- "ECRINS_FREQUENTATION_TOURROND modif dep.ods" ; enquetesTourond$localisation <- "Tourond" ; names(enquetesTourond) <- sub("-", ".", names(enquetesTourond)) ; enquetesTourond$date_de_debut_saisie <- as.character(enquetesTourond$date_de_debut_saisie)

enquetes <- rbind.fill(enquetes22, enquetes23, enquetes24, enquetesTourond) %>% subset(., is.na(localisation)==F) # Pas mal de differences entre les colonnes
rm(enquetes22, enquetes23, enquetes24, enquetesTourond, tourond, emparis_milieu, enqueteSF_raw)

### Agreger par mois
enquetes$mois <- format(as.Date(enquetes$date_de_debut_saisie), "%m/%Y")

enquetes_mois <- ddply(enquetes, .(localisation, mois), function(x){data.frame(
  Value=nrow(x),
  Nom_spatial=revalue(x$localisation[1], c("autre"="NA", "casset"="Casset", "emparis"="Emparis_milieu", "goleon"="Goleon", "la_danchere"="La Danchere", "pied_du_col"="Pied du col", "sentier_crevasses"="Crevasses", "taillefer"="Taillefer", "valsenestre"="Valsenestre"), warn_missing=F)
)})

### Projection
enquetePROJ <- enqueteSF %>% st_transform(., PROJ_Lambert)

### Ajouter popup pour carte interactive
for(i in 1:nrow(enqueteSF)){enqueteSF$Popup[i] <- BAT_PopupEnquete(enquetes_mois, enqueteSF$NOM_LIEU[i])}



##### Creer un raster vide pour avoir le meme format
EXT <- as.vector(raster::extent(st_transform(PNE_secteurs, st_crs(2154))))
RES_raster <- 500
empty_raster <- rast(xmin=EXT[1], xmax=EXT[2]+RES_raster, ymin=EXT[3], ymax=EXT[4]+RES_raster, resolution=RES_raster, vals=1:50000, crs=PROJ_Lambert)
crs(empty_raster) <- PROJtxt_Lambert
plot(empty_raster)


##### iNaturalist ------

### Telecharger les donnees iNaturalist (une fonction pour les telecharger, a ne lancer que si on veut mettre a jour les donnees, sinon on charge directement le fichier sauvegarde qui s'appelle 'inat_saved_data.rds')
# BTA_Telecharge_iNaturalist(PNE_secteurs)
inat_raw_data <- readRDS("2.Outputs/inat_saved_data.rds") %>% 
  subset(., Annee>2016 & Annee<2025) # Ne garder que les donnees apres 2016 et avant 2025

### Supprime les donnees hors du parc
inat_data <- st_filter(inat_raw_data, PNE_secteurs, .predicate = st_intersects)

### Projection
inat_dataPROJ <- inat_data %>% st_transform(., PROJ_Lambert)

### Creer un raster d'effort (en observateur.jour pour chaque cellule)
inat_dataPROJ$Raster_grid <- terra::extract(empty_raster, inat_dataPROJ)$lyr.1
inat_obs.date <- inat_dataPROJ %>% distinct(user_login, as.Date(datetime), Raster_grid, .keep_all = T)
inat_raster <- terra::rasterize(st_coordinates(inat_obs.date), empty_raster, fun=length)
inat_raster <- replace(inat_raster, is.na(inat_raster), 0) %>% mask(., PNE_secteursPROJ)
plot(inat_raster)


### PlantNet ------
### Telecharger les donnees PlantNET (une fonction pour les telecharger, a ne lancer que si on veut mettre a jour les donnees, sinon on charge directement le fichier sauvegarde qui s'appelle 'plantnet_saved_data.rds')
#BTA_Telecharge_PlantNet(PNE_secteurs)
plantnet_raw <- readRDS("2.Outputs/plantnet_saved_data.rds") %>% subset(., format(as.Date(.$eventDate), "%Y")>2016)

# Supprime les donnees hors du parc
plantnet_data <- st_filter(plantnet_raw, PNE_secteurs, .predicate = st_intersects)

### Projection
plantnet_dataPROJ <- plantnet_data %>% st_transform(., PROJ_Lambert)

# Creer un raster d'effort (en jour.observateur pour chaque cellule)
plantnet_dataPROJ$Raster_grid <- terra::extract(empty_raster, plantnet_dataPROJ)$lyr.1
plantnet_obs.date <- plantnet_dataPROJ %>% distinct(Creator, as.Date(eventDate), Raster_grid, .keep_all = T)
plantnet_raster <- terra::rasterize(st_coordinates(plantnet_obs.date), empty_raster, fun=length)
plantnet_raster <- replace(plantnet_raster, is.na(plantnet_raster), 0) %>% mask(., PNE_secteursPROJ)
plot(plantnet_raster)




##### Outdoor ------
### Charger les donnees
outdoor_flux <- rbind(
  st_read("0.Data/Outdoorvision/PN_Ecrins_AA_Courir_01012019_31032025_running.gpkg"),
  st_read("0.Data/Outdoorvision/PN_Ecrins_AA_Marcher_01012019_31032025_walking.gpkg"),
  st_read("0.Data/Outdoorvision/PN_Ecrins_AA_Pedaler_01012019_31032025_cycling.gpkg"),
  st_read("0.Data/Outdoorvision/PN_Ecrins_AA_Skier_01012019_31032025_skiing.gpkg"),
  st_read("0.Data/Outdoorvision/PN_Ecrins_Courir_01012019_31032025_running.gpkg"),
  st_read("0.Data/Outdoorvision/PN_Ecrins_Marcher_01012019_31032025_walking.gpkg"),
  st_read("0.Data/Outdoorvision/PN_Ecrins_Pedaler_01012019_31032025_cycling.gpkg"),
  st_read("0.Data/Outdoorvision/PN_Ecrins_Skier_01012019_31032025_skiing.gpkg")
  ) %>% st_transform(., st_crs(4326))

# Calculer le produit de distance * count, ce qui me donne un nombre de km parcourus (une course de 5km par une personne = une course de 1 km par 5 personnes); pas exactement ce qu'on veut mais avantage est qu'on peut les ajouter.
outdoor_flux$Length <- as.numeric(st_length(outdoor_flux))/1000 # Je coupe par grille pour avoir la longueur parcourue dans chaque grille
outdoor_flux$DistPers <- round(outdoor_flux$T * outdoor_flux$Length, 3)

### Transformer en raster
outdoorPROJ <- st_make_valid(st_transform(outdoor_flux, PROJ_Lambert))

# Preparer la grille (contrairement a Strava, ici on a des toutes petites sections donc je les assigne a une cellule sans les couper)
StravOut_grid <- empty_raster %>% as.polygons(.) %>% st_as_sf(.) %>% mutate(Cell=paste0("C", 1:nrow(.))) %>% st_transform(., PROJ_Lambert) # C'est la meme projection en realite mais pas le meme code, donc besoin de transformer
outdoorPROJ$Cell <- st_join(st_centroid(outdoorPROJ), StravOut_grid, join=st_intersects)$Cell

# Sommer par cellule
outdoor_sum <- outdoorPROJ %>% group_by(Cell) %>% summarise(Frequentation=sum(DistPers))
StravOut_grid$Freq_Outdoor <- outdoor_sum$Frequentation[match(StravOut_grid$Cell, outdoor_sum$Cell)]

# Transformer en raster
outdoor_raster <- rasterize(StravOut_grid, empty_raster, field = "Freq_Outdoor") %>% replace(., is.na(.), 0) %>% mask(., PNE_secteursPROJ)
plot(outdoor_raster)




##### Strava ------
### Preparer les donnees Strava (une fonction pour les preparer, a ne lancer que si on veut mettre a jour les donnees ou la grille de raster, sinon on charge directement le fichier sauvegarde qui s'appelle 'plantnet_saved_data.rds')
# BTA_PrepareStrava(PNE_secteurs, empty_raster)

strava_grid <- st_read("2.Outputs/prep_strava_grid.shp")
strava_cut <- st_read("2.Outputs/prep_strava_vector.shp")
strava_NOTcut <- st_read("2.Outputs/prep_strava_vectorNOTCUT.shp")

### Projection
strava_cutPROJ <- st_make_valid(st_transform(strava_cut, PROJ_Lambert))
strava_NOTcutPROJ <- st_make_valid(st_transform(strava_NOTcut, PROJ_Lambert))


### Transformer en raster
strava_raster <- rasterize(st_transform(strava_grid, PROJ_Lambert), empty_raster, field = "Freq") %>% replace(., is.na(.), 0) %>% mask(., PNE_secteursPROJ)







### Nettoyage -----
# Supprimer les jeux de donnees non necessaires
rm(
  eco_correspondance, eco_new, eco_velo,
  mdp_an, ROW_with_names, mdp_an_melt, MDPAddManu,
  desti_group, refuge_desti,
  plantnet_raw,
  photo_new,
  rout_correspondance, rout_tot, routier_new,
  EXT, FILE, i
)

