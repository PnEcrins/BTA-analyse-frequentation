
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps")
source("1.Scripts/0.BTA preparation donnees.R")



#########################
### CARTE INTERACTIVE ###
#########################

### Installer leaflet.esri qui a ete archive sur le CRAN
# url <- "https://cran.r-project.org/src/contrib/Archive/leaflet.esri/leaflet.esri_1.0.0.tar.gz"
# pkgFile <- "ESRI_1.0.0.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(pkgs=pkgFile, type="source", repos=NULL)
# unlink(pkgFile)

library(leaflet)
library(leaflet.esri)
library(htmlwidgets)
library(htmltools)


source("1.Scripts/1a.customAddPopUpIFrames.R")


### Vider les popup stockees
unlink("2.Outputs/Carte_interactive/TempPOPUP", recursive=T)
dir.create("2.Outputs/Carte_interactive/TempPOPUP", recursive=T)



### Definir les icones
Red_fact <- 2.5
IconH=128/Red_fact
IconW=96/Red_fact

Icon_refuge <- makeIcon(iconUrl = "0.Data/icons/icone_refuge.png", iconWidth = IconW, iconHeight = IconH, iconAnchorX = IconW/2, iconAnchorY = IconH)
Icon_routier <- makeIcon(iconUrl = "0.Data/icons/icone_routier.png", iconWidth = IconW, iconHeight = IconH, iconAnchorX = IconW/2, iconAnchorY = IconH)
Icon_ecocompteur <- makeIcon(iconUrl = "0.Data/icons/icone_ecocompteur.png", iconWidth = IconW, iconHeight = IconH, iconAnchorX = IconW/2, iconAnchorY = IconH)
Icon_piegephoto <- makeIcon(iconUrl = "0.Data/icons/icone_piegephoto.png", iconWidth = IconW, iconHeight = IconH, iconAnchorX = IconW/2, iconAnchorY = IconH)
Icon_mdp <- makeIcon(iconUrl = "0.Data/icons/icone_mdp.png", iconWidth = IconW, iconHeight = IconH, iconAnchorX = IconW/2, iconAnchorY = IconH)
Icon_enquete <- makeIcon(iconUrl = "0.Data/icons/icone_enquete.png", iconWidth = IconW, iconHeight = IconH, iconAnchorX = IconW/2, iconAnchorY = IconH)

### Palettes couleurs
max_iNat <- terra::global(inat_raster, fun="max", na.rm=T) %>% as.numeric(.)
ColorPal_iNat<-colorNumeric(c("#deebf7", "#08519c", "#08306b"), domain=c(0, 100, max_iNat), na.color = NA)
max_PlantNet <- terra::global(plantnet_raster, fun="max", na.rm=T) %>% as.numeric(.)
ColorPal_PlantNet<-colorNumeric(c("#deebf7", "#08519c", "#08306b"), domain=c(0, 100, max_PlantNet), na.color = NA)
max_Strava <- terra::global(strava_raster, fun="max", na.rm=T) %>% as.numeric(.)
ColorPal_Strava<-colorNumeric(c("#deebf7", "#08519c", "#08306b"), domain=c(0, 100, max_Strava), na.color = NA)
max_Outdoor <- terra::global(outdoor_raster, fun="max", na.rm=T) %>% as.numeric(.)
ColorPal_Outdoor<-colorNumeric(c("#deebf7", "#08519c", "#08306b"), domain=c(0, 100, max_Outdoor), na.color = NA)


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

Render_txt <- "
        function() {
        
      var baseOverlays = {}
      
      var groupedOverlays = {
        'Suivi frequentation PNE': {
          'Eco compteurs': this.layerManager.getLayerGroup('Eco compteurs', true),
          'Pieges photo': this.layerManager.getLayerGroup('Pieges photo', true),
          'Enquetes': this.layerManager.getLayerGroup('Enquetes', true),
          'Compteurs routiers': this.layerManager.getLayerGroup('Compteurs routiers', true),
          'Maisons de parc': this.layerManager.getLayerGroup('Maisons de parc', true),
          'Refuges': this.layerManager.getLayerGroup('Refuges', true),
          'Destinations refuges': this.layerManager.getLayerGroup('Destinations refuges', true)
        },
        'Donnees naturalistes': {
          'iNaturalist': this.layerManager.getLayerGroup('iNaturalist', true),
          'PlantNet': this.layerManager.getLayerGroup('PlantNet', true)
        },
        'Donnees sportives': {
          'Strava': this.layerManager.getLayerGroup('Strava', true),
          'Outdoorvision': this.layerManager.getLayerGroup('Outdoorvision', true)
        },
       'Couches additionnelles': {
          'Natura2000': this.layerManager.getLayerGroup('Natura2000', true),
          'Reserves': this.layerManager.getLayerGroup('Reserves', true),
          'Chemins': this.layerManager.getLayerGroup('Chemins', true),
          'Bassins versants': this.layerManager.getLayerGroup('Bassins versants', true)
        }
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
      
    }" # https://stackoverflow.com/questions/79652329/add-subtitles-in-r-leaflet-layer-control-of-overlay-groups


### Popup plotly
fl_rout = lapply(
  routierSF$Popup,
  function(j) {fl = tempfile(tmpdir=paste0(getwd(), "/2.Outputs/Carte_interactive/TempPOPUP"), fileext = ".html"); saveWidget(j, file = fl) ; return(fl)}
)

fl_eco = lapply(
  ecocompteurSF$Popup,
  function(j) {fl = tempfile(tmpdir=paste0(getwd(), "/2.Outputs/Carte_interactive/TempPOPUP"), fileext = ".html"); saveWidget(j, file = fl) ; return(fl)}
)

fl_photo = lapply(
  photoSF$Popup,
  function(j) {fl = tempfile(tmpdir=paste0(getwd(), "/2.Outputs/Carte_interactive/TempPOPUP"), fileext = ".html"); saveWidget(j, file = fl) ; return(fl)}
)

drs = leafpop:::createTempFolder("iframes")


### Carte
map <- leaflet() %>%
  addTiles(group="OpenStreetMap", options = tileOptions(opacity=0.5)) %>%
  addEsriBasemapLayer(esriBasemapLayers$Imagery, group = "Satellite") %>%
  addPolygons(data=PNE_secteurs, fill=F, stroke=T, weight=1) %>%
  addPolygons(data=PNE, fill=F, stroke=T, weight=5) %>%
  addPolygons(data=N2000, group="Natura2000", fill=T, color="black", fillColor="green", stroke=T, weight=0.5, popup=N2000$Popup) %>%
  addPolygons(data=reserves, group="Reserves", fill=T, color="black", fillColor="green", stroke=T, weight=0.5, popup=reserves$Popup) %>%

  addPolylines(data=st_transform(cheminsManu_line, st_crs(4326)), group="Chemins", popup=paste0("<b>Nom du chemin: </b>", cheminsManu_line$Nom), color="darkred", weight=3) %>%
  addPolylines(data=st_transform(basinModif, st_crs(4326)), group="Bassins versants", popup=paste0("<b>Nom du bassin versant : </b>", basinModif$Nom), color="black", fill=T, fillColor="white", fillOpacity=0.01, weight=1.5) %>%
  
  addMarkers(lng=ecocompteurSF$Lon, lat=ecocompteurSF$Lat, group="Eco compteurs", icon = Icon_ecocompteur) %>%
  customAddPopUpIFrames(fl_eco, group = "Eco compteurs", drs = drs, width = 700, height = 500) %>%

  addMarkers(lng=photoSF$Lon, lat=photoSF$Lat, group="Pieges photo", icon = Icon_piegephoto) %>%
  customAddPopUpIFrames(fl_photo, group = "Pieges photo", drs = drs, width = 600, height = 350) %>%

  addMarkers(lng=enqueteSF$Lon, lat=enqueteSF$Lat, group="Enquetes", icon = Icon_enquete) %>%
  leafpop::addPopupGraphs(enqueteSF$Popup, group = "Enquetes", width = 500, height = 500) %>%

  addMarkers(lng=routierSF$Lon, lat=routierSF$Lat, group="Compteurs routiers", icon = Icon_routier) %>%
  customAddPopUpIFrames(fl_rout, group = "Compteurs routiers", drs = drs, width=600, height=500) %>%
  
  addMarkers(lng=mdpSF$Lon, lat=mdpSF$Lat, group="Maisons de parc", icon = Icon_mdp) %>%
  leafpop::addPopupGraphs(mdpSF$Popup, group = "Maisons de parc", width = 600, height = 300) %>%
  
  addMarkers(lng=refugeSF$Lon, lat=refugeSF$Lat, group="Refuges", icon = Icon_refuge) %>%
  leafpop::addPopupGraphs(refugeSF$Popup, group = "Refuges", width = 600, height = 300) %>%
  addPolylines(data=refuge_destiSF, weight=refuge_destiSF$Epaisseur, popup=refuge_destiSF$Pop, group="Destinations refuges", col="black") %>%
  
  addCircleMarkers(lng=inat_data$longitude, lat=inat_data$latitude, group="iNaturalist", radius=2, opacity=0.1, stroke=F, color="#08306b") %>%
  addRasterImage(inat_raster, method="ngb", group="iNaturalist", opacity=0.7, colors=ColorPal_iNat) %>%
  addLegend(colors=ColorPal_iNat(seq(0,max_iNat,length.out=5)), label=round(seq(0,max_iNat,length.out=5)), opacity=0.7, group="iNaturalist", title="Nb observateurs") %>%
  
  addCircleMarkers(lng=plantnet_data$decimalLongitude, lat=plantnet_data$decimalLatitude, group="PlantNet", radius=2, opacity=0.1, stroke=F, color="#08306b") %>%
  addRasterImage(plantnet_raster, method="ngb", group="PlantNet", opacity=0.7, colors=ColorPal_PlantNet) %>%
  addLegend(colors=ColorPal_PlantNet(seq(0,max_PlantNet,length.out=5)), label=round(seq(0,max_PlantNet,length.out=5)), opacity=0.7, group="PlantNet", title="Nb observateurs") %>%

  addRasterImage(strava_raster, method="ngb", group="Strava", opacity=0.7, colors=ColorPal_Strava) %>%
  addLegend(colors=ColorPal_Strava(seq(0, max_Strava, length.out=5)), label=round(seq(0, max_Strava, length.out=5)), opacity=0.7, group="Strava", title="Km parcourus") %>%

  addRasterImage(outdoor_raster, method="ngb", group="Outdoorvision", opacity=0.7, colors=ColorPal_Outdoor) %>%
  addLegend(colors=ColorPal_Outdoor(seq(0, max_Outdoor, length.out=5)), label=round(seq(0, max_Outdoor, length.out=5)), opacity=0.7, group="Outdoorvision", title="Km parcourus") %>%
  
  addLayersControl(baseGroups=c("OpenStreetMap", "Satellite"), position="topleft", options=layersControlOptions(collapsed=F)) %>%
  hideGroup(c("Natura2000", "Reserves", "iNaturalist", "PlantNet", "Outdoorvision", "Strava", "Destinations refuges", "Bassins versants")) %>%
  
  addScaleBar(position="bottomright") %>%
  registerPlugin(groupedLayerControlPlugin) %>%
  htmlwidgets::onRender(Render_txt)


### Carte 2 : ajouter vecteurs Strava et Outdoorvision
map2 <- map %>%
  addPolylines(data=strava_NOTcut, group="Strava", col="#CD34B5", weight=(0.1+20*sqrt(strava_NOTcut$count/max(strava_NOTcut$count, na.rm=T)))) %>%
  addPolylines(data=outdoor_flux, group="Outdoorvision", col="#CD34B5", weight=(0.1+20*sqrt(outdoor_flux$U/max(outdoor_flux$U, na.rm=T))))
  

### Enregistrer les deux cartes
saveWidget(map, "2.Outputs/Carte_interactive/Carte_interactive_frequentation_PNE_sansStravaOutdoor.html", selfcontained = F)
saveWidget(map2, "2.Outputs/Carte_interactive/Carte_interactive_frequentation_PNE.html", selfcontained = F)



