#' get management regions map
#'
#' @return leaflet map of the management regions
#' @noRd

build_map=function(){
require(leaflet)
require(rgdal)
library(tidyverse)
library(here)

# get the data ------------------------------------------------------------

shape <- readOGR(dsn = paste0(here(),"/data-raw/spatial"), layer = "gadm36_NOR_1")



# check the names of each region ------------------------------------------

#shape@data$NAME_1


# map each region to a management region ----------------------------------

shape@data$MAN_REGION<-c("R4", "R4", "R2","R2", "R8", "R5", "R1", 
                         "R6", "R6", "R7", "R3", "R4", "R1", "R1",
                         "R6", "R2", "R8", "R1", "R2")

# plotting a leaflet map --------------------------------------------------

# get the colour pallette -------------------------------------------------
n = length(unique(shape@data$MAN_REGION))
factorPal <- colorFactor(viridis::inferno(n), shape@data$MAN_REGION)


# plot leaflet map --------------------------------------------------------

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Harvest Management Regions")
)  

leaflet(shape) %>% 
  addTiles() %>% 
  addPolygons(data=shape, weight = 2, color = "black",  
              fillColor = ~factorPal(shape@data$MAN_REGION),
              fillOpacity = 0.5,
              label = shape@data$MAN_REGION) %>% 
  addControl(title, position = "topleft", className="map-title")%>% 
  addLegend("bottomright", pal = factorPal, values=~shape@data$MAN_REGION,
           title = "Management Regions",
            opacity = 0.55
  )
}

