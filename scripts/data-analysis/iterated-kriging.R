library(tidyverse)
library(sp)
library(sf)
library(gstat)
library(automap)
# library(spdplyr)
library(lubridate)
library(modelr)
load("data/processed/bottle.RData")


## preprocess bottle data

bottle_nested <- bottle %>%
  # drop depths below 500m and missing oxygen values
  #filter on year because will take forever 
  filter(depth < 500,
         !is.na(oxygen), 
         year == 2000) %>%
  # bin depth into layers (ADJUST BINS??)
  mutate(depth_layer = cut(depth, 
                           breaks = c(0, 50, 100, 250, 500),
                           right = F)) %>%
  # group and nest
  group_by(year, quarter, depth_layer) %>%
  nest()

## function to process data for kriging

kdata_fn <- function(df){
  
  out <- df %>% 
    # drop data sampled more than 20 days from median sampling date
    mutate(day_diff = yday(date) - median(yday(date))) %>%
    filter(abs(day_diff) < 10) %>%
    # for each station, randomly sample 5 observations
    group_by(station, line) %>%
    sample_n(size = 5, 
             replace = T) %>%
    # compute average by station; one data point per station/depth; NOTE some non-unique lat/lon/depth recorded
    summarize(across(.cols = c(lat, lon, date, oxygen, temperature, salinity),
                     .fns = mean),
              .groups = 'drop') %>%
    # coerce to sf object 
    st_as_sf(coords = c('lon', 'lat')) %>%
    # lat/lon crs
    st_set_crs(4326) %>%
    # project to california zone 5
    st_transform(2770)
  
  df %>% pull(date) %>% year() %>% unique() %>% print() 
  
  return(out)
}

# bottle_nested %>%
#   mutate(n = map(data, nrow)) %>%
#   unnest(n) %>% 
#   filter(n < 50) %>%
#   mutate(kriging_data = map(data, kdata_fn))

## function to generate prediction grid

grid_fn <- function(sf, w, h){
  
  # determine boundary (convex hull of sampled locations)
  boundary <- sf %>% 
    distinct(line, station, .keep_all = T) %>%
    st_combine() %>% 
    st_convex_hull()
  
  # partition region within boundary into boxes
  grid_geo <- boundary %>%
    st_make_grid(n = c(w, h),
                 what = 'polygons',
                 crs = 2770) %>%
    st_intersection(boundary)
  
  # add box id (modelr::map fails otherwise due to length attribute)
  out <- st_sf(tibble(.id = 1:length(grid_geo)), grid_geo)
  
  return(out)
}

# bottle_nested %>%
#   mutate(kriging_data = map(data, kdata_fn)) %>%
#   mutate(grid = map(kriging_data, ~grid_fn(.x, 10, 10)))


## function to retrieve predictions

extract_preds <- function(autok_out, grid){

    out <- autok_out$krige_output %>%
      # coerce to sf
      st_as_sf() %>%
      # adjust names
      rename(pred = var1.pred,
             se = var1.stdev) %>%
      select(pred, se) %>%
      # replace point geometry with grid regions
      st_drop_geometry() %>%
      st_sf(st_geometry(grid))
  
  return(out)
}

## iterated kriging

# for reproducibility
set.seed(51022)

# compute predictions
kriging_out <- bottle_nested %>%
  # minimum sample size
  mutate(n = map(data, nrow)) %>%
  unnest(n) %>% 
  filter(n > 50) %>%
  select(-n) %>%
  # process data
  mutate(kriging_data = map(data, kdata_fn)) %>%
  # minimum sample size
  mutate(n = map(kriging_data, nrow)) %>%
  unnest(n) %>% 
  filter(n > 50) %>%
  select(-n) %>%
  # generate prediction grid
  mutate(grid = map(kriging_data, ~grid_fn(.x, 50, 50))) %>%
  # fit model and krige
  mutate(fit = map2(kriging_data, 
                    grid,
                    ~ autoKrige(oxygen ~ 1,
                                as_Spatial(.x),
                                as_Spatial(st_centroid(.y))))) %>%
  # extract predictions
  mutate(preds = map2(fit, grid, extract_preds))

## export

save(kriging_out, file = 'iterated-kriging-out.RData')
