# packages ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  glue, here, htmltools, leaflet, lubridate, sp, sf, tidyverse, scales)

# paths ----
bottle_rda <- here("data/processed/bottle.RData")
save_rda   <- here("scripts/shiny/spatial-page/page_functions.RData")
kriging_rda <- here('results/iterated-kriging-out.RData')

# check paths
stopifnot(file.exists(bottle_rda))
stopifnot(dir.exists(dirname(save_rda)))
stopifnot(file.exists(kriging_rda))

# load data
load(bottle_rda)
load(kriging_rda)

## -----------------------------
## SPATIAL PAGE

# map data for a year
get_map_data <- function(yr, qr){
  ## note: for each date & station, usually only one set of coordinates recorded
  ## i.e. no lat/long variation across depths, with rare but extant exceptions
  
  # station locations
  station_locations <- bottle %>%
    filter(year(date) == yr) %>%
    # select location info
    select(lat, 
           lon, 
           line, 
           station,
           date) %>%
    # unique sampling locations
    distinct(date, line, station, lat, lon) %>%
    # find average location and variation for each station
    group_by(line, station) %>%
    summarize(across(.cols = c(lat, lon), 
                     .fns = list(ctr = mean, 
                                 var = var)),
              .groups = 'drop') %>%
    mutate(
      sta_id = glue("{line} {station}"),
      loc_se = sqrt(lat_var + lon_var))
  
  out <- bottle %>%
    # filter to specified year
    filter(year(date) == yr,
           quarter == qr) %>%
    # select spatiotemporal info
    select(depth, 
           line,
           station,
           date) %>%
    # find maximum depth measured that year and number of visits
    group_by(line, station) %>%
    summarize(maxdepth = max(depth), .groups = 'drop') %>%
    ungroup() %>%
    # merge station info (center lat and long)
    full_join(station_locations, by = c('line', 'station')) %>%
    # define indicator for whether a station was sampled
    mutate(sampled_ix = !is.na(maxdepth)) %>%
    # create labels to display on hover
    mutate(label_line1 = paste('Line ID', line, sep = ' '),
           label_line2 = paste('Station ID', station, sep = ' '),
           label_line3 = paste('Max depth', maxdepth, sep = ' ')) %>%
    unite(label,
          contains('label'),
          sep = ' <br/> ') %>%
    select(-contains('label_line'))
  
  return(out)
}

get_kriging_data <- function(yr, qr, dpth){
  kriging_filtered <- kriging_out %>% 
    filter(year == yr,
           quarter == qr,
           depth_layer == dpth) %>%
    pull(preds)
  
  if(length(kriging_filtered) > 0){
    ## TG UPDATE HERE?? smoothing takes too long
    out <- kriging_filtered[[1]] %>%
      st_transform(4326)
  }else{
    out <- NULL
  }
  
  return(out)
}

# leaflet-specific
point_color_fn <- colorFactor(c('#393939', '#393939'), 
                              c(T, F))
raster_color_fn <- colorNumeric(palette = c("#E74C3C", "#000000", "#059BFF"),
                                NULL, n = 5)

lines <- bottle %>% pull(line) %>% unique()
stations <- bottle %>% pull(station) %>% unique()
depths <- kriging_out %>% pull(depth_layer) %>% unique()

# generate base map layer
make_basemap <- function(){
  leaflet() %>% 
  setView(lng = -121.33940, 
          lat = 33.94975, 
          zoom = 5) %>%
  addProviderTiles(providers$Esri.OceanBasemap)
}

# yr <- 2010
# qr <- 1
# dpth <- depths[1]
# filtered_data <- get_map_data(yr, qr)
# basemap <- make_basemap()
# kriging_data <- get_kriging_data(yr, qr, dpth)

update_basemap <- function(basemap, filtered_data, kriging_data){

  list_data <- filtered_data %>%
    arrange(line, station) %>%
    select(line, lon_ctr, lat_ctr) %>%
    distinct(lon_ctr, lat_ctr, line) %>%
    nest(data = c(lon_ctr, lat_ctr)) 
  lines_df <- list_data %>% 
    pull(data) %>%
    lapply(function(df){st_linestring(as.matrix(df))}) %>%
    st_sfc()
  points_df <- filtered_data %>%
    st_as_sf(coords = c('lon_ctr', 'lat_ctr')) %>%
    st_set_crs(4326)
  
  if(is.null(kriging_data)){
    basemap %>%
      clearMarkers() %>% 
      clearShapes()%>%
      addPolylines(data = lines_df,
                   color = "black", 
                   opacity = 0.3,
                   weight = 1.5) %>%
      addCircleMarkers(data = points_df, 
                       popup = ~label, 
                       color = ~point_color_fn(sampled_ix),
                       radius = ~ if_else(sampled_ix, 4, 1.5),
                       stroke = F,
                       fillOpacity = 0.5)
  }else{
    basemap %>%
      clearMarkers() %>% 
      clearShapes()%>%
      addPolylines(data = lines_df,
                   color = "black", 
                   opacity = 0.3,
                   weight = 1.5) %>%
      addCircleMarkers(data = points_df, 
                       popup = ~ label, 
                       color = ~ point_color_fn(sampled_ix),
                       radius = ~ if_else(sampled_ix, 4, 1.5),
                       stroke = F,
                       fillOpacity = 0.5) %>%
      # TG UPDATE HERE
      addPolygons(data = kriging_data,
                  fillColor = ~ raster_color_fn(pred),
                  stroke = F,
                  fillOpacity = 0.5,
                  smoothFactor = 0.1) 
  }

}

# yr <- 1995
# qr <- 3
# dpth <- depths[2]
# make_basemap() %>%
#   update_basemap(get_map_data(yr, qr),
#                  get_kriging_data(yr, qr, dpth))

# custom transformation for depth profiles
rev_sqrt <- trans_new('revsqrt', 
                      function(x) -sqrt(x),
                      function(x) x^2,
                      breaks = breaks_log(n = 5, base = 10))

# depth profiles - Og function
# make_profile <- function(yr, qr){
#   bottle %>% 
#     filter(year(date) == yr,
#            quarter == qr) %>%
#     select(oxygen, 
#            salinity, 
#            temperature, 
#            depth, 
#            cast) %>%
#     pivot_longer(1:3, 
#                  names_to = "measurement", 
#                  values_to = "value") %>%
#     ggplot(aes(x = value, y = depth,
#                group = interaction(cast, measurement))) +
#     geom_path(alpha = 0.1) +
#     scale_y_continuous(trans = rev_sqrt) +
#     facet_wrap(~ measurement,
#                nrow = 1,
#                scales = 'free_x') +
#     geom_hline(yintercept = 0) +
#     labs(x = '') +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, 
#                                      size = 8, 
#                                      vjust = 0.5),
#           axis.text.y = element_text(size = 8))
# }


make_profile <- function(yr, lin){
  stations_in_line <- bottle %>%
    filter(year(date) == yr,
           depth <= 1000,
           line == lin) %>%
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    subset(line == lin)
  bottle %>% 
    filter(year(date) == yr,
           depth <= 1000,
           line != lin) %>%
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    ggplot(aes(x = oxygen, y = depth,
               group = interaction(cast, quarter))) +
    geom_path(color = "black", alpha = 0.1) +
    geom_path(data = stations_in_line,
              color = "aquamarine4",
              size = 2,
              alpha = 0.1) +
    scale_y_continuous(trans = rev_sqrt) +
    facet_wrap(~ quarter,
               nrow = 1) +
    geom_hline(yintercept = 0) +
    labs(x = 'Oxygen (ml of O_2/L of seawater)') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8, 
                                     vjust = 0.5),
          axis.text.y = element_text(size = 8))
}

# add labels for nearshore to off shore on each side

make_station_line <- function(yr, lin){
  bottle %>%
    # filter to year, quarter, and station of interest
    filter(year == yr, 
           # quarter == qr,
           line == lin) %>%
    # bin depths into roughly even numbers of observations
    mutate(depth_interval = cut_number(depth, 10)) %>%
    # aggregate within depth bins
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    group_by(depth_interval,
             quarter,
             distance) %>%
    summarize(oxygen = median(oxygen, na.rm = T)) %>% # tinker with summary stat
    ggplot(aes(x = distance, y = fct_rev(depth_interval))) +
    facet_wrap(~ quarter, 
               # scales = "free_x",
               nrow = 4) +
    # using geom_tile instead of raster in order to create boxes of different widths
    geom_tile(aes(fill = oxygen, width = distance)) +
    # adjust color scale
    scale_fill_gradient2(low = '#E74C3C',
                         high = '#059BFF',
                         mid = '#000000',
                         midpoint = log10(1.4),
                         # limits = c(0.01, 6),
                         # values = rescale(c(-.01,1.4,6)),
                         na.value = "gray",
                         # space = "Lab", 
                         # guide = "colourbar",
                         # n.breaks = 6, 
                         # oob_squish(range = c(0.01, 6)), default for oor values is NA
                         trans = 'log10'
                         ) +
    # aesthetics
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8,
                                     vjust = 0.5),
          panel.grid = element_blank()) +
    labs(x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
         fill='Oxygen (mL O2/L seawater)') 
}



# Editing function to take parameter of interest - first trying ChlorA
#Go light green to gree

make_station_line_chlor <- function(yr, lin){
  bottle %>%
    # filter to year, quarter, and station of interest
    filter(year == yr, 
           # quarter == qr,
           line == lin,
           depth <= 250,
           chlorophyll >= 0) %>%
    # bin depths into roughly even numbers of observations
    mutate(depth_interval = cut_number(depth, 8)) %>%
    # aggregate within depth bins
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    group_by(depth_interval,
             quarter,
             distance) %>%
    summarize(chlorophyll = median(chlorophyll, na.rm = T)) %>% # tinker with summary stat
    ggplot(aes(x = distance, y = fct_rev(depth_interval))) +
    facet_wrap(~ quarter, 
               # scales = "free_x",
               nrow = 4) +
    # using geom_tile instead of raster in order to create boxes of different widths
    geom_tile(aes(fill = chlorophyll, width = distance)) +
    # adjust color scale
    scale_fill_gradient(low = '#E74C3C',
                        high = '#83C70C',
                        space = 'Lab',
                        na.value = "gray",
                        guide = "colourbar",
                        aesthetics = "fill",
                        trans = 'log10'
    ) +
    # aesthetics
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8,
                                     vjust = 0.5),
          panel.grid = element_blank()) +
    labs(x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
         fill='Chlorophyll (micro grams/L seawater)') 
}

make_station_line_temp <- function(yr, lin){
  bottle %>%
    # filter to year, quarter, and station of interest
    filter(year == yr, 
           # quarter == qr,
           line == lin) %>%
    # bin depths into roughly even numbers of observations
    mutate(depth_interval = cut_number(depth, 10)) %>%
    # aggregate within depth bins
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    group_by(depth_interval,
             quarter,
             distance) %>%
    summarize(temperature = median(temperature, na.rm = T)) %>% # tinker with summary stat
    ggplot(aes(x = distance, y = fct_rev(depth_interval))) +
    facet_wrap(~ quarter, 
               # scales = "free_x",
               nrow = 4) +
    # using geom_tile instead of raster in order to create boxes of different widths
    geom_tile(aes(fill = temperature, width = distance)) +
    # adjust color scale
    scale_fill_gradient(low = '#000000',
                        high = '#059BFF',
                        space = 'Lab',
                        na.value = "gray",
                        guide = "colourbar",
                        aesthetics = "fill",
                        trans = 'log10'
    ) +
    # aesthetics
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8,
                                     vjust = 0.5),
          panel.grid = element_blank()) +
    labs(x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
         fill='Temperature (˚C)') 
}


make_station_line_salinity <- function(yr, lin){
  bottle %>%
    # filter to year, quarter, and station of interest
    filter(year == yr, 
           # quarter == qr,
           line == lin) %>%
    # bin depths into roughly even numbers of observations
    mutate(depth_interval = cut_number(depth, 10)) %>%
    # aggregate within depth bins
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    group_by(depth_interval,
             quarter,
             distance) %>%
    summarize(salinity = median(salinity, na.rm = T)) %>% # tinker with summary stat
    ggplot(aes(x = distance, y = fct_rev(depth_interval))) +
    facet_wrap(~ quarter, 
               # scales = "free_x",
               nrow = 4) +
    # using geom_tile instead of raster in order to create boxes of different widths
    geom_tile(aes(fill = salinity, width = distance)) +
    # adjust color scale
    scale_fill_gradient(low = '#000000',
                        high = '#08D1A2',
                        space = 'Lab',
                        na.value = "gray",
                        guide = "colourbar",
                        aesthetics = "fill",
                        trans = 'log10'
    ) +
    # aesthetics
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8,
                                     vjust = 0.5),
          panel.grid = element_blank()) +
    labs(x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
         fill='Salinity (Practical Salinity Scale)') 
}


save(
  list = ls(),
  file = save_rda
)

## ----------------------
## TESTS

make_basemap() %>%
  update_basemap(get_map_data(1984, 4))

make_profile(2012, "093.3")
make_station_line(2014, "093.3")
make_station_line_chlor(2000, "093.3")
#Do a green/cooler color for the cooler temp to red warm for temperature
make_station_line_temp(2019, "080.0")
# pink scale for pink salt
make_station_line_salinity(2014, "093.3")
