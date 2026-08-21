
####################################################################################
### PREPARATION DE QUELQUES FONCTIONS UTILISEES DANS 0.BTA preparation donnees.R ###
####################################################################################


### Une fonction pour regrouper deux shapefiles avec des colonnes differentes
rbind.fillSF <- function(poly1, poly2){
  
  # If one of them is null, return the other one
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


### Preparation popup MDP et refuges
BAT_PopupTempoPlot <- function(DF, Item_NAME, type){
  
  DF_sub <- subset(DF, Nom_spatial==Item_NAME & is.na(Value)==F)
  
  SOUS_titre <- paste0(
    "Première année de suivi : ", min(DF_sub$Annee, na.rm=T), "<br>",
    "Dernière année de suivi : ", max(DF_sub$Annee, na.rm=T), "<br>",
    "*<span style = 'color: purple;'>",
    ifelse(type=="mdp", "Les données de fréquentation mensuelle sont disponibles", "Les données de fréquentation mensuelle sont disponibles pour 2023 et 2024"),
    "*</span>"
  )
  
  if(nrow(DF_sub)==0){SOUS_titre <- paste0("Donnees de ", revalue(type, c("mdp"="visites", "refuge"="nuitées"), warn_missing = F), " indisponibles")}
  
  Pop <- ggplot(DF_sub)+
    geom_point(aes(x=Annee, y=Value))+
    ylim(c(0,1.1*max(c(0, DF_sub$Value), na.rm=T)))+
    theme_minimal()+
    ggtitle(Item_NAME)+
    labs(subtitle=SOUS_titre)+
    theme(plot.title=element_text(face='bold'),
          plot.subtitle = element_markdown(lineheight = 1.1)
    )
  
  if(type=="refuge"){Pop <- Pop+ylab("Nuits")}
  if(type=="mdp"){Pop <- Pop+ylab("Visites")}
  
  
  return(list(Pop))
}



### Preparation popup ecocompteurs, pieges photos, compteurs routiers
BAT_PopupDoublePlotly <- function(DF, Item_NAME, type){
  
  DF_sub <- subset(DF, Nom_spatial==Item_NAME & is.na(Value)==F)
  
  DF_sub$Txt <- paste0("<b>Nombre de jours de suivi: </b>", DF_sub$Nb_jours, "\n",
                       "<b>Dates: </b>du ", DF_sub$Jour1, " au ", DF_sub$Jour2, "\n"
  )
  
  # Ajout different si ecocompteur avec velo ou pas
  if(Item_NAME %in% c("Eco compteur Serre du Coin (piéton + vélo)", "Eco compteur de la Barrière (piéton + vélo)")){
    DF_sub$Txt <- paste0(DF_sub$Txt, "<b>Piétons: </b>", DF_sub$Value_pietons, " passages \n", "<b>Vélos: </b>", DF_sub$Value_velos, " passages")
  } else {
    DF_sub$Txt <- paste0(DF_sub$Txt, "<b>Fréquentation: </b>", DF_sub$Value, " passages")
  }
  DF_sub$Txt <- paste0(DF_sub$Txt, "\n\n<i>Une personne passant sur le compteur a l'aller\n et au retour compte pour 2 passages")
  
  # Sous titre
  SOUS_titre <- paste0(
    "<span style = 'font-size:14'>Nombre de jours de suivi : ", sum(DF_sub$Nb_jours, na.rm=T), "<br>",
    "Années de suivi : ", length(unique(DF_sub$Annee)), " (", min(DF_sub$Annee, na.rm=T), "-", max(DF_sub$Annee, na.rm=T), ")<br>",
    "Proportion de jours suivis par an: ", round(100* sum(DF_sub$Nb_jours, na.rm=T)/ (365*length(unique(DF_sub$Annee)))), "%<br>",
    "<i><span style = 'color: purple'>Ces données sont également disponibles ", ifelse(type=="routier", "jour par jour", "heure par heure"), "</i></span>"
  )
  
  # Ajout manuel de certains dysfonctionnements pour les ecocompteurs
  if(Item_NAME=="Eco Compteur de Confolens"){SOUS_titre <- paste0(SOUS_titre, "<br><span style = 'color: red'><span>&#9888;</span>", " Dysfonctionnements toute l’année 2016", "</span>")}
  if(Item_NAME=="Eco Compteur du Tourond"){SOUS_titre <- paste0(SOUS_titre, "<br><span style = 'color: red'><span>&#9888;</span>", " Dysfonctionnements toute l’année 2016", "</span>")}
  if(Item_NAME=="Eco Compteur du Pré de Mme carle"){SOUS_titre <- paste0(SOUS_titre, "<br><span style = 'color: red'><span>&#9888;</span>", " Changement pour un compteur plus fiable en 2024", "</span>")}
  if(Item_NAME=="Eco Compteur du lac du Vallon"){SOUS_titre <- paste0(SOUS_titre, "<br><span style = 'color: red'><span>&#9888;</span>", " Dysfonctionnements en 2017, 2023 et 2024, données non fiables", "</span>")}
  if(Item_NAME=="Eco Compteur de la Danchère"){SOUS_titre <- paste0(SOUS_titre, "<br><span style = 'color: red'><span>&#9888;</span>", " Dysfonctionnements toute l’année 2016 + début juin 2017 et 18 juillet 2017 <br>Compteur vandalisé en 2020, données incorrectes<br>Nouveau type de capteur en 2021 <br>Données non fiables en 2022, changement de capteur fin juin <br>Nouveau compteur en 2023", "</span>")}
  SOUS_titre <- paste0(SOUS_titre, "</span>")
  
  # Besoin de creer une grille avec des NA sinon on a un probleme quand on a des annees sans donnees (voir par exemple ici : https://stackoverflow.com/questions/71188611/ggplot-geom-tile-is-distorted-in-ggplotly)
  DF_grid <- tidyr::expand_grid(quinzaine=paste0(rep(Mois_noms$Nom, each=2), rep(c("_1", "_2"), 12)),
                                Annee=as.character(min(DF$Annee):max(DF$Annee)))
  DF_sub_fill <- dplyr::left_join(DF_grid, DF_sub, by=c('quinzaine','Annee'))
  DF_sub_fill$Txt[is.na(DF_sub_fill$Txt)]<-""
  
  # Pour les ecocompteurs ou les velos sont separes
  if(Item_NAME %in% c("Eco compteur Serre du Coin (piéton + vélo)", "Eco compteur de la Barrière (piéton + vélo)")){
    DF_sub_fill <- reshape2::melt(DF_sub_fill, id.vars=c("quinzaine", "Annee", "Txt"), measure.vars=c("Value_pietons", "Value_velos"), value.name="Value") %>% 
      mutate(Type=revalue(variable, c("Value_pietons"="Pietons", "Value_velos"="Velos")))
  }
  
  # plots
  Pop <- ggplot(DF_sub_fill)+
    geom_tile(aes(x=factor(quinzaine, paste0(rep(Mois_noms$Nom, each=2), rep(c("_1", "_2"), 12))), y=factor(Annee, levels=as.character(min(DF$Annee):max(DF$Annee))), fill=Value, text=Txt))+
    scale_x_discrete(drop=F)+scale_y_discrete(drop=F)+
    xlab("Quinzaine")+ylab("Annee")+
    scale_fill_viridis_c(name="Passages \nuniques", na.value=NA)+
    theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.margin=unit(c(0.12,0,0,0), units="npc"))
  
  if("Type" %in% names(DF_sub_fill)){Pop <- Pop+facet_wrap(~Type)}
  
  Pop_ly <- Pop %>%
    ggplotly(., tooltip="text") %>%
    config(displayModeBar = F) %>%
    layout(
      xaxis = list(fixedrange = TRUE), xaxis2 = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE),
      margin = list(t=ifelse("Type" %in% names(DF_sub_fill) | grepl("9888", SOUS_titre), 
                             ifelse(Item_NAME %in% c("Eco Compteur de la Danchère", "Eco Compteur du lac du Vallon"), ifelse(Item_NAME=="Eco Compteur de la Danchère", 230, 160), 150), 
                             110), 
                    b=50, 
                    l=80, 
                    r=80, 
                    pad=0),
      title = list(
        yanchor="top",
        y=0.95,
        font=list(size=16),
        text=HTML(paste0("<b>", ifelse(type=="routier", paste0(Item_NAME, " (", DF_sub$Nom_uniq[1], ")"), Item_NAME),
                         '</b><br>',
                         HTML(SOUS_titre)))
      )
    )
  
  return(list(Pop_ly))
}



### Preparation popup enquetes
BAT_PopupEnquete <- function(DF, Item_NAME){
  
  DF_sub <- subset(DF, Nom_spatial==Item_NAME & is.na(Value)==F)
  
  Pop <- ggplot(DF_sub)+
    geom_bar(aes(x=mois, y=Value), stat="identity")+
    xlab("Mois")+ylab("Nombre d'enquetes")+
    ggtitle(Item_NAME)+
    theme_minimal()
  
  return(list(Pop))
}

Mois_noms <- data.frame(Num=1:12, Nom=c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun", "Jul", "Aou", "Sep", "Oct", "Nov", "Dec"), Nom_complet=c("Janvier", "Fevrier", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout", "Septembre", "Octobre", "Novembre", "Decembre"))


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
  x2 <- x %>% strsplit(., "/") %>% unlist(.)
  rev_date <- paste0(x2[3], "-", x2[2], "-", x2[1])
  return(rev_date)
}



### Fonction pour preparer les donnees des compteurs routiers
BTA_PreparerRoutier <- function(){
  
  ### Lister les fichiers a charger (tous les fichiers dans routiers, sauf quelques exceptions listees ci-dessous)
  List_files <- list.files("0.Data/Donnees_routier/", recursive = T) %>%
    subset(., grepl("Répertoire", .)==F) %>%
    subset(., grepl("_CLongueurs_", .)==F) %>%
    subset(., grepl("MJM Corrigé", .)==F) %>%
    subset(., grepl("Synthèse", .)==F) %>%
    subset(., grepl(".pdf", .)==F) %>%
    subset(., grepl("_Supp", .)==F) %>%
    subset(., grepl("-Supp_", .)==F) %>%
    subset(., grepl(".zip", .)==F) %>%
    subset(., grepl("Lexique_", .)==F) %>%
    subset(., grepl("Table_Correspondance", .)==F)
  
  ### Supprimer les fichiers 11 et 12 quand on a 13 (c'est a dire garder uniquement les fichiers double sens) + supprimer ceux qui different mais pour lesquels j'ai verifie manuellement qu'on avait bien le cumul
  List_files <- List_files[! List_files %in% List_files[grepl("11-", List_files) & sub("11-", "13-", List_files) %in% List_files]]
  List_files <- List_files[! List_files %in% List_files[grepl("12-", List_files) & sub("12-", "13-", List_files) %in% List_files]]
  List_files <- List_files[!List_files %in% c(
    "2018_TV_T6-heure/Comptages routiers Horaires_2018_0050480011-0 Gioberney Montant.xlsx", "2018_TV_T6-heure/Comptages routiers Horaires_2018_0050480012-0 Gioberney Descendant.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0380530011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0380530012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0380214011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0380214012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0052000011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0052000012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0050300011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0050300012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0050423011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0050423012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0057100011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0057100012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0050238011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0050238012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0050207011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0050207012-0.xlsx",
    "2018_TV_T6-heure/rapportT6_01_June_2018_0050204011-0.xlsx", "2018_TV_T6-heure/rapportT6_01_June_2018_0050204012-0.xlsx",
    "2005_TV_T6-heure/P Gioberney 2005.xls", "2005_TV_T6-heure/P La Danchère 2005.xls", "2005_TV_T6-heure/P PréMme Carle 2005.xls", # Fichiers avec parking (mais les montees et descentes sont deja dans un autre fichier)
    "2005_TV_T6-heure/0241013LesGourniers_3_05.XLS", # C'est un subset de Les Gourniers Annuel_3_05 (ete seulement)
    "2019_TV_T6-heure/rapportT6_01_June_2019_0050207011-Piedducol.xlsx" # Erreur de frappe (un D en plus que dans les fichiers 12 et 13) mais on a bien le fichier cumule
  )]
  
  ### Lire et regrouper les differents fichiers que j'ai conserve dans la partie precedente
  rout_tot <- data.frame()
  for(FILEN in List_files){
    
    # Premiere lecture "naive" qui lit l'ensemble du fichier et sert a detecter ou commence le tableau et le nom du tableur (car format difficilement lisible sous R)
    naive <- read_excel(paste0("0.Data/Donnees_routier/", FILEN), col_names=F)
    ROW_start <- min(which(unlist(naive[,1]) %in% c("Jours", "Heures et jour", "Date", "Heures et jours")), na.rm=T)
    ROW_end <- min(which(substr(unlist(naive[ROW_start:nrow(naive),1]),1,3) %in% c("Moy", "MJ ", "Déb")), na.rm=T)-1+ROW_start-1
    COL_end <- min(which(naive[ROW_start,]=="H24"), na.rm=T)+1
    
    NAME1 <- naive[which(grepl("Compteur : ", unlist(naive[,1]))),1] %>% as.character() %>% sub("Compteur : ", "", .)
    NAME2 <- naive[which(as.vector(is.na(unlist(naive[,4])))==F)[1],4] %>% unlist()
    NAME3 <- naive[which(as.vector(is.na(unlist(naive[,4])))==F)[2],4] %>% unlist()
    NAME4 <- naive[which(as.vector(is.na(unlist(naive[,8])))==F)[2],8] %>% unlist()
    NAME5 <- naive[which(as.vector(is.na(unlist(naive[,8])))==F)[3],8] %>% unlist()
    
    # Seconde lecture en prenant uniquement le tableau de donnees (grace a ROW_start, ROW_end, COL_end detectes en lecture naive)
    rout <- read_excel(paste0("0.Data/Donnees_routier/", FILEN), range=cell_limits(ul=c(ROW_start,1), lr=c(ROW_end, COL_end)), col_types = c("date", rep("guess", (COL_end-1))))
    rout$Jour_read_as_text <- read_excel(paste0("0.Data/Donnees_routier/", FILEN), range=cell_limits(ul=c(ROW_start,1), lr=c(ROW_end, 1)), col_types = "text") %>% as.vector(.) %>% .[[1]]
    rout$name1 <- NAME1[1]
    rout$name2 <- NAME2[1]
    rout$name3 <- NAME3[1]
    rout$name4 <- NAME4[1]
    rout$name5 <- NAME5[1]
    rout$filename <- FILEN
    
    # Regroupe les donnees
    rout_tot <- rbind.fill(rout_tot, rout)
    
    # Informe sur l'avancement de la boucle
    print(which(List_files==FILEN))
  }
  
  ### Formatter quelques colonnes dates
  rout_tot$`Heures et jour` <- NULL
  rout_tot$Date <- ifelse(is.na(rout_tot$Jours), as.character(rout_tot$Jour_read_as_text), as.character(rout_tot$Jours))
  
  for(i in 1:nrow(rout_tot)){
    rout_tot$Date[i]<- ifelse(grepl("-", rout_tot$Date[i]), rout_tot$Date[i], Rev_date(rout_tot$Date[i]))
  }
  
  ### Ajouter les colonnes de comptage (pas toujours le meme nom selon les fichiers)
  rout_tot$Comptage <- rowSums(rout_tot[,c("du jour", "TOTAL")], na.rm = T) %>% ifelse(is.na(rout_tot$TOTAL) & is.na(rout_tot$`du jour`), NA, .)
  rout_tot$`du jour` <- NULL
  rout_tot$TOTAL <- NULL
  
  ### Sauvegarder les donnees preparees
  saveRDS(rout_tot, "2.Outputs/Routes_merged_raw.rds")
  
  return("Les donnees de compteurs routiers ont ete nettoyees et sauvegardees dans le fichier '2.Outputs/Routes_merged_raw.rds'")
}



### Fonction pour telecharger les donnees iNaturalist
BTA_Telecharge_iNaturalist <- function(PNE_secteurs){
  
  library(rinat)
  
  # Couper la region en grille pour ne pas atteindre le nombre limites de donnees telechargeables (maximum a 10,000 alors qu'on en a plus)
  Grid_inat <- PNE_secteurs %>% st_as_sfc() %>% st_make_grid(n=c(6,6))
  for(i in 1:length(Grid_inat)){
    print(paste0("Iteration ", i, "/", length(Grid_inat)))
    Sys.sleep(15)
    inat_grid_data <- get_inat_obs(geo=T, bounds=as.vector(raster::extent(st_as_sf(Grid_inat[i,])))[c(3,1,4,2)], maxresults=10000) %>% mutate(Download_batch=i)
    if(i==1){inat_raw_data <- inat_grid_data} else {inat_raw_data <- rbind(inat_raw_data, inat_grid_data)}
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
  
  # Sauvegarder les donnees
  saveRDS(inat_raw_data, "2.Outputs/inat_saved_data.rds")
  
  return("Les donnees iNaturalist ont ete telechargees et sauvegardees dans le fichier '2.Outputs/inat_saved_data.rds'")
}





### Fonction pour telecharger les donnees PlantNet
BTA_Telecharge_PlantNet <- function(PNE_secteurs){
  
  library(rgbif)
  
  # Telecharger via rgbif avec le bon nom de dataset
  EXT <- as.vector(raster::extent(PNE_secteurs))
  plantnet_raw <- rgbif::occ_search(datasetKey="7a3679ef-5582-4aaa-81f0-8c2545cafc81",
                                  hasCoordinate = T,
                                  decimalLongitude=paste(EXT[1], EXT[2], sep=","),
                                  decimalLatitude=paste(EXT[3], EXT[4], sep=","),
                                  limit=100000)
  
  # Ajouter et preparer le nom de l'observateur
  plantnet_raw$data$Creator <- lapply(plantnet_raw$media, function(x){
    unlist(x) %>% .[grepl("creator", names(.))] %>% as.character(.) %>% unique(.)
  }) %>% unlist()
  
  # Transformer en fichier spatial
  plantnet_SF <- st_as_sf(plantnet_raw$data, coords = c("decimalLongitude","decimalLatitude"), remove = FALSE)
  st_crs(plantnet_SF) <- st_crs(4326)
  
  # Sauvegarder
  saveRDS(plantnet_SF, "2.Outputs/plantnet_saved_data.rds")
  
  return("Les donnees PlantNET ont ete telechargees et sauvegardees dans le fichier '2.Outputs/plantnet_saved_data.rds'")
}



### Fonction pour preparer les donnees Strava (parce que c'est un peu long de les couper par la grille de raster)
BTA_PrepareStrava <- function(PNE_secteurs, empty_raster){
  
  ## Charger shapefile et supprimer doublons ou segments hors PNE (c'est le meme shp pour les differentes annees)
  strava_shp <- rbind(
    st_read(paste0("0.Data/Strava/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.shp")),
    st_read(paste0("0.Data/Strava/all_edges_yearly_2023_ped_HA/3efc864349c6eaad9e40509ea36a4da8152f4a188059fed722884a65a4bcde31-1756465891784.shp"))
  ) %>%
    distinct(., osmId, edgeUID, .keep_all = T) %>%
    st_filter(., PNE_secteurs, .predicate = st_intersects)
  
  ## Charger comptages par annee, supprimer les doublons (ce sont les chemins a cheval entre deux departements qui apparaissent dans les deux mais ne doivent etre comptes qu'une fois)
  strava20_counts <- rbind(
    read.csv("0.Data/Strava/all_edges_yearly_2020_ped_IS/e97557510fd922dc7c0d955e7bcdc4f03671f69a676d89079aa3ebb0723beb24-1759844814808.csv"),
    read.csv("0.Data/Strava/all_edges_yearly_2020_ped_HA/abe3f84155bb2a9c52dc9a18149c2882e38c0f0125f8fa0c2e40165279ae15da-1756473991938.csv")
  ) %>%
    distinct(., edge_uid, .keep_all=T) %>%
    subset(., edge_uid %in% strava_shp$edgeUID) %>%
    mutate(Annee=2020)
  
  strava21_counts <- rbind(
    read.csv("0.Data/Strava/all_edges_yearly_2021_ped_IS/42369ff676f3bf88c65915a55c757fc30677701ce572573b5147c7a172ea2031-1759841837849.csv"),
    read.csv("0.Data/Strava/all_edges_yearly_2021_ped_HA/a86a6abfdc546df61d355ed81ff26d3399fce2aa771b9bfde2608f89d1b48fe7-1756471629970.csv")
  ) %>%
    distinct(., edge_uid, .keep_all=T) %>%
    subset(., edge_uid %in% strava_shp$edgeUID) %>%
    mutate(Annee=2021)
  
  strava22_counts <- rbind(
    read.csv("0.Data/Strava/all_edges_yearly_2022_ped_IS/b3bd4045af8bab6c0dfae0270895801a5a54393d0da24c95248918f6742e7cd6-1756371314087.csv"),
    read.csv("0.Data/Strava/all_edges_yearly_2022_ped_HA/d8319123485012fb7af66913914f09eaa0d3b1fa4a7f1cc4cd7e12c9de6e0a31-1756469822776.csv")
  ) %>%
    distinct(., edge_uid, .keep_all=T) %>%
    subset(., edge_uid %in% strava_shp$edgeUID) %>%
    mutate(Annee=2022)
  
  strava23_counts <- rbind(
    read.csv("0.Data/Strava/all_edges_yearly_2023_ped_IS/17cf0cd016377fcc8f8a21ed57fdf88b11990bab0f96b95d68de3f1a52c977ff-1756308272399.csv"),
    read.csv("0.Data/Strava/all_edges_yearly_2023_ped_HA/3efc864349c6eaad9e40509ea36a4da8152f4a188059fed722884a65a4bcde31-1756465891784.csv")
  ) %>%
    distinct(., edge_uid, .keep_all=T) %>%
    subset(., edge_uid %in% strava_shp$edgeUID) %>%
    mutate(Annee=2023)
  
  strava24_counts <- rbind(
    read.csv("0.Data/Strava/all_edges_yearly_2024_ped_IS/4fc1fbc0b0b2f82b183a61c36a7361727bd914cb8df6f598e437553057fce9bc-1755699327570.csv"),
    read.csv("0.Data/Strava/all_edges_yearly_2024_ped_HA/b5c8685287bd143eebc47f180c86efd71573b3a26165fa61ce75e94b8aa71afe-1756456670245.csv")
  ) %>%
    distinct(., edge_uid, .keep_all=T) %>%
    subset(., edge_uid %in% strava_shp$edgeUID) %>%
    mutate(Annee=2024)
  
  ## Sommer les differentes annees et diviser par le nombre d'annee (nombre moyen de passages par an)
  strava_counts <- rbind(strava20_counts, strava21_counts, strava22_counts, strava23_counts, strava24_counts) %>%
    group_by(edge_uid) %>%
    summarise(total_trip_count = round(sum(total_trip_count, na.rm=T)/length(unique(.$year)),1), N_years=n())
  
  strava_shp$count <- strava_counts$total_trip_count[match(strava_shp$edgeUID, strava_counts$edge_uid)]
  
  ## Preparer la grille (coupe en 10 parce que c'etait trop lent en une seule action)
  StravOut_grid <- empty_raster %>% as.polygons(.) %>% st_as_sf(.) %>% mutate(Cell=paste0("C", 1:nrow(.)))
  
  strava_sub <- strava_shp[is.na(strava_shp$count)==F,] %>% st_transform(., st_crs(StravOut_grid)) # Si count==NA c'est qu'on a un comptage de 0 donc pas la peine de le garder
  strava_cut1 <- st_intersection(strava_sub[1:4000,], StravOut_grid)
  strava_cut2 <- st_intersection(strava_sub[4001:8000,], StravOut_grid)
  strava_cut3 <- st_intersection(strava_sub[8001:12000,], StravOut_grid)
  strava_cut4 <- st_intersection(strava_sub[12001:16000,], StravOut_grid)
  strava_cut5 <- st_intersection(strava_sub[16001:20000,], StravOut_grid)
  strava_cut6 <- st_intersection(strava_sub[20001:24000,], StravOut_grid)
  strava_cut7 <- st_intersection(strava_sub[24001:28000,], StravOut_grid)
  strava_cut8 <- st_intersection(strava_sub[28001:32000,], StravOut_grid)
  strava_cut9 <- st_intersection(strava_sub[32001:36000,], StravOut_grid)
  strava_cut10 <- st_intersection(strava_sub[36001:nrow(strava_sub),], StravOut_grid)
  strava_cut <- rbind(strava_cut1, strava_cut2, strava_cut3, strava_cut4, strava_cut5, strava_cut6, strava_cut7, strava_cut8, strava_cut9, strava_cut10) ; rm(strava_cut1, strava_cut2, strava_cut3, strava_cut4, strava_cut5, strava_cut6, strava_cut7, strava_cut8, strava_cut9, strava_cut10)
  
  ## Calculer le produit de distance * count, ce qui me donne un nombre de km parcourus (une course de 5km par une personne = une course de 1 km par 5 personnes); pas exactement ce qu'on veut mais avantage est qu'on peut les ajouter.
  strava_cut$Length <- as.numeric(st_length(strava_cut))/1000 # Je coupe par grille pour avoir la longueur parcourue dans chaque grille
  strava_cut$DistPers <- round(strava_cut$count * strava_cut$Length, 3)
  
  ## Sommer par cellule de la grille
  strava_cut_sum <- strava_cut %>% group_by(Cell) %>% summarise(Frequentation=sum(DistPers, na.rm=T))
  StravOut_grid$Freq <- strava_cut_sum$Frequentation[match(StravOut_grid$Cell, strava_cut_sum$Cell)]
  
  ## Sauvergarder les fichiers
  st_write(StravOut_grid, "2.Outputs/prep_strava_grid.shp", append=F)
  st_write(strava_cut, "2.Outputs/prep_strava_vector.shp", append=F)
  st_write(strava_shp, "2.Outputs/prep_strava_vectorNOTCUT.shp", append=F)
  
  
  return("Les donnees Strava ont ete preparees et sauvegardees")
  
}
