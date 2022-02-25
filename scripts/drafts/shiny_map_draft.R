library(tidyverse)
library(lubridate)
library(leaflet)
library(htmltools)
load('data/processed/bottle-cast-recent.RData')

## functions

get_map_data <- function(yr){
  ## note: for each date & station, only one set of coordinates recorded
  ## i.e. no lat/long variation across depths, with rare but extant exceptions
  
  # station locations
  station_locations <- bottle %>%
    mutate(date = mdy(Date)) %>%
    # select location info
    select(Lat_Dec, 
           Lon_Dec,
           Sta_ID,
           date) %>%
    # better names
    rename(id = Sta_ID,
           lat = Lat_Dec,
           lon = Lon_Dec) %>%
    # unique sampling locations
    distinct(date, id, lat, lon) %>%
    # find average location and variation for each station
    group_by(id) %>%
    summarize(across(.cols = c(lat, lon), 
                     .fns = list(ctr = mean, 
                                 sd = sd)),
              n_visits_total = n())
  
  out <- bottle %>%
    filter(Year == yr) %>%
    # coerce date to datetime
    mutate(date = mdy(Date)) %>%
    # select spatiotemporal info
    select(Depthm, 
           Sta_ID,
           date) %>%
    # better names
    rename(id = Sta_ID,
           depth = Depthm) %>%
    # find maximum depth measured that year and number of visits
    group_by(id) %>%
    summarize(maxdepth = max(depth), 
              n_visits_yr = n_distinct(date)) %>%
    ungroup() %>%
    # merge station info (center lat and long)
    full_join(station_locations, by = 'id') %>%
    # define indicator for whether a station was sampled
    mutate(sampled_ix = is.na(maxdepth)) %>%
    # split id column into line and station
    separate(id, c('line', 'station'), sep = ' ') %>%
    # if no visits, report zero
    mutate(n_visits_yr = replace_na(n_visits_yr, 0)) %>%
    # create labels to display on hover
    mutate(label_line1 = paste('Line ID', line, sep = ' '),
           label_line2 = paste('Station ID', station, sep = ' '),
           label_line3 = paste('Max depth', maxdepth, sep = ' '),
           label_line4 = paste('Visited', n_visits_yr, 'time(s)', sep = ' ')) %>%
    unite(label,
          contains('label'),
          sep = ' <br/> ') %>%
    select(-contains('label_line'))
  
  return(out)
}

point_color_fn <- colorFactor(c('#B73407', '#393939'), 
                      c(T, F))

# base map layer

basemap <- leaflet() %>% 
  setView(lng = -121.33940, 
          lat = 33.94975, 
          zoom = 5) %>%
  addProviderTiles(providers$Esri.OceanBasemap) 

# add points based on year selection
yr <- 2010

basemap %>%
  addCircleMarkers(
    lat = ~lat_ctr, 
    lng = ~lon_ctr, 
    popup = ~label, 
    color = ~clr_fn(sampled_ix),
    radius = 0.5,
    data = get_map_data(yr))
