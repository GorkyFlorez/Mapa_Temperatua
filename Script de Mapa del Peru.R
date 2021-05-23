#  Cargamos las Librerias ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse,rnaturalearth,rnaturalearthdata,
               sf, reticulate,maptools,maps, ggplot2 ,ggspatial, rgeos, ggmap , leaflet, ggrepel,
               RColorBrewer)
# Llamando las librerias de Leaflet
library(png)  
library(broom)
library(tidyverse)
library(leaflet)#Libreria para graficar mapas interactivos
library(leaflet.extras)
library(leaflet.providers)
library(leafem)
library(htmlwidgets)#Para guardar el mapa
library(sp)
library(sf)#Manejo de informacion espacial
library(readxl)#Para leer archivos excel
library(mapview)#Para visualizacion de datos espaciales
library(RColorBrewer) #Paleta de Colores
library(viridis)
library(Rgb)
library(ggplot2)#Para distintos graficos incluso mapas
library(raster)#Para leer archivos raster
library(tidyverse)
library(rmarkdown)
# Cargammos los SHp del Peru ---------------------------------------------------------------
Tempera_Peru     <- st_read("Temperatura del Peru/variacion_2030_tmax.shp")   

Peru         <- getData('GADM', country='Peru', level=1) %>%            #Este comando permite leer el shapefile y 'transformarlo' en un data frame
  st_as_sf()                                 #Extracion de Peru
Peru_D       <- st_centroid(Peru )                                       # Centros de cada pais
Peru_D       <- cbind(Peru, st_coordinates(st_centroid(Peru $geometry))) #Utilizamos la geometria

SurAmerica        <- st_read ("Ecorregiones Black/SurAmerica.shp")                  #Sur America
SurAmerica_utm    <- st_transform(SurAmerica ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))


display.brewer.all() 
colours = RColorBrewer::brewer.pal(n = 5, name = "YlOrRd")
pal_colores <- colorFactor(RColorBrewer::brewer.pal(n = 5, name = "YlOrRd"), domain = Tempera_Peru$RANGO)
#Definiendo el Logo
m="https://images.vexels.com/media/users/3/143561/isolated/preview/afa3aa927b63061e3b0222b7dab9cdbf-ubicaci--n-n--utica-norte-flecha-vintage-by-vexels.png"


M<-leaflet() %>%
  addControl(html = "<p><strong><em>Cambio de la Temperatura Máxima Anual centrado al 2030</em></strong></p>",
             position = "topright")%>%
  addLogo(m,url = "https://images.vexels.com/media/users/3/143561/isolated/preview/afa3aa927b63061e3b0222b7dab9cdbf-ubicaci--n-n--utica-norte-flecha-vintage-by-vexels.png",
          position = "topleft",
          offset.x = 50,
          offset.y = 10,
          width = 100,
          height = 100)%>%
  addPolygons(data= Tempera_Peru,
              color = pal_colores(Tempera_Peru$RANGO),
              fillOpacity = 0.5,
              label = Tempera_Peru$RANGO,
              group = "Temperatura")%>%
  addPolygons(data= Peru  ,
              color = "#444444",
              weight = 2,
              fillOpacity = 0.05,
              fillColor = 1,
              group = "Peru")%>%
  addDrawToolbar(targetGroup = "Graficos",
                 editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
  addMeasure(position = "topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479")%>%
  addMeasurePathToolbar(options = measurePathOptions(showOnHover = F,
                                                     showDistances = F,
                                                     showArea = T,
                                                     minPixelDistance = 400))%>%
  addLegend(position = "bottomright", pal = pal_colores, values = Tempera_Peru$RANGO, title= "Temperatura °C", opacity = 0.5)%>%
  addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 100,
                                                                 metric = TRUE,
                                                                 imperial = TRUE,
                                                                 updateWhenIdle = TRUE))%>%
  addLayersControl(baseGroups = c("Satellite", "OSM", "OTM"),
                   overlayGroups = c("Tempera_Peru","Peru" ),
                   position = "topright",
                   options = layersControlOptions(collapsed = T))%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM")%>%
  addProviderTiles(providers$OpenTopoMap, group = "OTM")%>%
  addMiniMap(tiles = providers$Esri.WorldImagery,
             toggleDisplay = TRUE)%>%
  addSearchFeatures(targetGroups = "Tempera_Peru")


# Gardar el mapa
saveWidget ( M , file = "index.html" )






