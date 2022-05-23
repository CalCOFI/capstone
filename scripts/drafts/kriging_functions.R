library(tidyverse)
library(sp)
library(sf)
library(gstat)
library(automap)
library(spdplyr)
library(modelr)
library(lubridate)
library(leaflet)
library(smoothr)
library(rgdal)
load('iterated-kriging-out.RData')



points_df <- get_map_data(2000, 1)
kriging_df <- kriging_out %>% filter(year == 2000, quarter == 1) %>% pull(preds)



pal_fn <- colorNumeric(palette = c("#E74C3C", "#000000", "#059BFF"), NULL, n = 5) # try other colorX(...) leaflet functions


add_kriging <- function(basemap, kriging_df){
basemap %>%
  #st_transform(4326) %>%
  leaflet() %>% 
  setView(lng = -121.33940, 
          lat = 33.94975, 
          zoom = 5) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addPolygons(data = kriging_df, 
              fillColor = ~pal_fn(pred),
              stroke = F,
              fillOpacity = 0.8,
              smoothFactor = 0.1)  %>%
  addLegend("topright", pal = pal_fn, values = ~pred,
            title = "Dissolved Oxygen Concentration",
            opacity = 1
  )
}

make_basemap() %>% update_basemap(get_map_data(input$yr, input$qr))

get_kriging_data <- function(yr, qr){
  kriging_out %>% 
    filter(year == yr, quarter == qr) %>% 
    pull(preds)
}


make_basemap() %>% 
  update_basemap(get_map_data(2000, 1)) %>%
  add_kriging(get_kriging_data(2000, 1))




