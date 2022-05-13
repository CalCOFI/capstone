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

# grab data from a year/quarter and process for kriging

load("data/processed/bottle.RData")
yr <- 2000
qr <- 3

kdata_sf <- bottle %>% 
  filter(year == yr, quarter == qr, depth < 500) %>%
  mutate(day_diff = yday(date) - median(yday(date))) %>%
  filter(abs(day_diff) < 10) %>%
  na.omit() %>%
  mutate(depth_layer = cut(depth, 
                           breaks = c(0, 50, 100, 250, 500),
                           right = F)) %>% # fix so uniform across time?
  group_by(station, line, depth_layer) %>%
  sample_n(size = 5, 
           replace = T) %>%
  summarize(lat = unique(lat),
            lon = unique(lon),
            date = unique(date),
            oxygen = mean(oxygen),
            temperature = mean(temperature),
            salinity = mean(salinity),
            depth = mean(depth),
            .groups = 'drop') %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(4326) %>% 
  st_transform(2770)

# make a prediction grid within the boundary of sampled region

boundary <- kdata_sf %>% 
  distinct(line, station, .keep_all = T) %>%
  st_combine() %>% 
  st_convex_hull()

# plot(boundary)

pred_grid <- boundary %>%
  st_make_grid(n = c(50, 50), 
               what = 'polygons', 
               crs = 2770) %>%
  st_intersection(boundary) 

pred_grid_pt <- pred_grid %>%  st_centroid()

plot(pred_grid)

# fit variogram model & krige for one layer

lyr <- 1

kdata_sf_layer <- kdata_sf %>% 
  filter(depth_layer == levels(depth_layer)[lyr])

fit <- autoKrige(oxygen ~ 1, 
                      as(kdata_sf_layer, 'Spatial'), 
                      as(pred_grid_pt, 'Spatial'))

preds_sf <- fit$krige_output %>%
  st_as_sf() %>%
  rename(pred = var1.pred,
         se = var1.stdev) %>%
  select(pred, se)

st_geometry(preds_sf) <- st_geometry(pred_grid)


preds_sf_smooth <- smooth(preds_sf, method = "ksmooth", smoothness = 0.1)


plot(preds_sf_smooth)

# plot


pal_fn <- colorQuantile(palette = c("red", "black", "blue"), NULL, n = 5) # try other colorX(...) leaflet functions

preds_sf_smooth %>%
  st_transform(4326) %>%
  leaflet() %>% 
  setView(lng = -121.33940, 
          lat = 33.94975, 
          zoom = 5) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addPolygons(fillColor = ~pal_fn(pred),
              stroke = F,
              fillOpacity = 0.8,
              smoothFactor = 0.1)  %>%
  addLegend("topright", colors = c("red", "black", "blue"), labels = c("low", "medium", "high"),
            title = "Dissolved Oxygen Concentration",
            opacity = 1
  )

## comments

# would be nice to have raster smoothing so appearance is less block-like [X]

# continuous or discrete scale? not sure, currently discrete []

# match mallika's color palette? [X]

# add legend []