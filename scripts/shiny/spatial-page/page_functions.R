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

### code for fixing color palette

# vector with red to black colors
hypoxic <- colorRampPalette(colors = c("#E74C3C", "#000000"), space = "Lab")(14)

# vector of colors for black to blue (180 colors)
not_hypoxic <- colorRampPalette(colors = c("#000000", "#059BFF"), space = "Lab")(51)

## Combine the two color palettes
rampcols <- c(hypoxic, not_hypoxic)

mypal <- colorNumeric(palette = rampcols, domain =c(0,6.5))


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
                       fillOpacity = 0.5,
                       layerId = ~sta_id)
  }else{
    basemap %>%
      clearMarkers() %>% 
      clearShapes()%>%
      addPolylines(data = lines_df,
                   color = "black", 
                   opacity = 0.3,
                   weight = 1.5)  %>%
      addPolygons(data = kriging_data, 
                  fillColor = ~mypal(kriging_data$preds),
                  stroke = F,
                  fillOpacity = 0.8,
                  smoothFactor = 0.1)  %>%
      #addLegend("topright", pal = ~mypal(preds), values = kriging_data$preds,
               # title = "Dissolved Oxygen Concentration",
                #opacity = 1) %>%
      addPolygons(data = kriging_data,
                  fillColor = ~ raster_color_fn(pred),
                  stroke = F,
                  fillOpacity = 0.5,
                  smoothFactor = 0.1,
                  group = "kriging") %>%
      addCircleMarkers(data = points_df, 
                       popup = ~ label, 
                       color = ~ point_color_fn(sampled_ix),
                       radius = ~ if_else(sampled_ix, 4, 1.5),
                       stroke = F,
                       fillOpacity = 0.5, 
                       layerId = ~sta_id,)
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

# function with plots for all parameters
make_profile <- function(yr, lin, param){
  cleaned_bottle <- bottle %>% # Add labels for Quarters
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall"))
  
  stations_in_line <- cleaned_bottle %>%
    filter(year(date) == yr,
           depth <= 500,
           line == lin)
  
  oxygen_profile <- cleaned_bottle %>%
    filter(year(date) == yr,
           depth <= 500,
           line != lin) %>%
    ggplot(aes(x = oxygen, y = depth,
               group = interaction(cast, quarter))) +
    geom_path(color = "black", alpha = 0.1) +
    geom_path(data = stations_in_line,
              color = "turquoise4",
              size = 2,
              alpha = 0.1) +
    scale_y_continuous(trans = rev_sqrt) +
    facet_wrap(~ quarter,
               nrow = 1) +
    geom_hline(yintercept = 0) +
    labs(x = 'Oxygen (ml of O_2/L of seawater)',
         y = 'Depth') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8, 
                                     vjust = 0.5),
          axis.text.y = element_text(size = 8))
  
  temperature_profile <- cleaned_bottle %>%
    filter(year(date) == yr,
           depth <= 500,
           line != lin) %>%
    ggplot(aes(x = temperature, y = depth,
               group = interaction(cast, quarter))) +
    geom_path(color = "black", alpha = 0.1) +
    geom_path(data = stations_in_line,
              color = "blue3",
              size = 2,
              alpha = 0.1) +
    scale_y_continuous(trans = rev_sqrt) +
    facet_wrap(~ quarter,
               nrow = 1) +
    geom_hline(yintercept = 0) +
    labs(x = 'Temperature (˚C)',
         y = 'Depth') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8, 
                                     vjust = 0.5),
          axis.text.y = element_text(size = 8))
  
  salinity_profile <- cleaned_bottle %>%
    filter(year(date) == yr,
           depth <= 500,
           line != lin) %>%
    ggplot(aes(x = salinity, y = depth,
               group = interaction(cast, quarter))) +
    geom_path(color = "black", alpha = 0.1) +
    geom_path(data = stations_in_line,
              color = "hotpink3",
              size = 2,
              alpha = 0.1) +
    scale_y_continuous(trans = rev_sqrt) +
    facet_wrap(~ quarter,
               nrow = 1) +
    geom_hline(yintercept = 0) +
    labs(x = 'Salinity (Practical Salinity Scale)',
         y = 'Depth') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8, 
                                     vjust = 0.5),
          axis.text.y = element_text(size = 8))

  chlorophyll_profile <- cleaned_bottle %>%
    filter(year(date) == yr,
           depth <= 275,
           line != lin) %>%
    ggplot(aes(x = chlorophyll, y = depth,
               group = interaction(cast, quarter))) +
    geom_path(color = "black", alpha = 0.1) +
    geom_path(data = stations_in_line %>% subset(depth <= 275),
              color = "springgreen4",
              size = 2,
              alpha = 0.1) +
    scale_y_continuous(trans = rev_sqrt) +
    facet_wrap(~ quarter,
               nrow = 1) +
    geom_hline(yintercept = 0) +
    labs(x = 'Chlorophyll (micro grams/L seawater)',
         y = 'Depth') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8, 
                                     vjust = 0.5),
          axis.text.y = element_text(size = 8))
  
  if(param == "oxy"){
    oxygen_profile +
      labs(title =  paste("Depth profile for Oxygen Concentration for Line",
                          lin, "in", yr))
  }else{
    if(param == "temp"){
      temperature_profile +
        labs(title =  paste("Depth profile for Temperature for Line",
                            lin, "in", yr))
    }else{
      if(param == "sal"){
        salinity_profile +
          labs(title =  paste("Depth profile for Salinity for Line",
                              lin, "in", yr))
      }else{
        chlorophyll_profile +
          labs(title =  paste("Depth profile for Chrolophyll for Line",
                              lin, "in", yr))
      }
    }
  }

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
    labs(title = paste("Variation in Dissolved Oxygen for Line",
                       lin, "in", yr), 
         caption = "Near shore on the right, off shore on the left.",
         x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
         fill='Oxygen (mL O2/L seawater)') 
}




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
    labs(title = paste("Variation in Chlorophyll for Line",
                       lin, "in", yr), 
         caption = "Near shore on the right, off shore on the left.",
         x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
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
    scale_fill_gradient(high = 'red2',
                        # mid = 'yellow', # only used by scale_fill_gradient2
                        low = 'blue4',
                        # midpoint = median(bottle$temperature, na.rm = TRUE),
                        # na.value = "gray",
                        n.breaks = 8,
                        trans = "log10",
                        # limits = range(bottle$temperature, na.rm = TRUE)
    ) +
    # aesthetics
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8,
                                     vjust = 0.5),
          panel.grid = element_blank()) +
    labs(title = paste("Variation in Temperature for Line",
                       lin, "in", yr), 
         caption = "Near shore on the right, off shore on the left.",
         x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
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
    scale_fill_gradient(low = '#B00282',
                        high = '#FFA1E6',
                        space = 'Lab',
                        na.value = "gray",
                        guide = "colourbar",
                        aesthetics = "fill",
                        trans = 'log10'
    )+
    # aesthetics
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8,
                                     vjust = 0.5),
          panel.grid = element_blank()) +
    labs(title = paste("Variation in Salinity for Line",
                       lin, "in", yr), 
         caption = "Near shore on the right, off shore on the left.",
         x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
         fill='Salinity (Practical Salinity Scale)') 
}

get_nearest_date <- function(time){
  bottle %>%
    group_by(year, quarter) %>%
    summarize(across(date, .fns = list(min = min, max = max)), .groups = "drop") %>%
    filter(date_max > ymd(time)) %>%
    slice_min(date_min) %>%
    pull(quarter)
}

depth_avg_plot <- function(start_input, end_input, date_input, station_input, line_input){
  # really can only do this for stations sampled below a certain depth
  possible_stations <- bottle %>% 
    filter(depth <= 1000,
           date > start_input,
           date < end_input) %>%
    group_by(line, station) %>%
    summarize(max_depth = max(depth), .groups = 'drop') %>%
    filter(max_depth > 100)
  if(station_input %in% possible_stations$station & line_input %in% possible_stations$line){
    # summary stats for full date range
    range_summary <- bottle %>%
      filter(depth <= 1000,
             date > start_input,
             date < end_input,
             line == line_input,
             station == station_input) %>%
      mutate(depth_layer = cut_width(depth, 50)) %>%
      group_by(depth_layer, quarter) %>%
      summarize(across(.cols = c(oxygen, salinity, temperature, chlorophyll),
                       .fns = list(min = ~ min(.x, na.rm = T), 
                                   max = ~ max(.x, na.rm = T),
                                   med = ~ median(.x, na.rm = T))),
                n = n(),
                .groups = 'drop')
    # summary stats for nearest date to given date
    timepoint_summary <- bottle %>%
      filter(depth <= 1000,
             line == line_input,
             station == station_input) %>%
      mutate(diff = date - date_input) %>%
      filter(diff == min(abs(diff))) %>% # oddly, faster than slice_min
      mutate(depth_layer = cut_width(depth, 50)) %>%
      group_by(depth_layer) %>%
      summarize(across(.cols = c(oxygen, salinity, chlorophyll, temperature),
                       .fns = c(min = ~ min(.x, na.rm = T),
                                max = ~ max(.x, na.rm = T),
                                med = ~ median(.x, na.rm = T))),
                n = n(),
                quarter = unique(quarter))
    
    
    # drop quarter from grouping
    range_summary <- bottle %>%
      filter(depth <= 1000,
             date > start_input,
             date < end_input,
             line == line_input,
             station == station_input) %>%
      mutate(depth_layer = cut_width(depth, 50)) %>%
      group_by(depth_layer) %>%
      summarize(across(.cols = c(oxygen, salinity, temperature, chlorophyll),
                       .fns = list(min = ~ min(.x, na.rm = T), 
                                   max = ~ max(.x, na.rm = T),
                                   med = ~ median(.x, na.rm = T))),
                n = n(),
                .groups = 'drop')
    
    # plot
    ggplot(data = ungroup(range_summary), 
           aes(y = fct_rev(depth_layer))) +
      geom_path(aes(x = oxygen_med,
                    group = 1)) +
      geom_errorbarh(aes(xmin = oxygen_min,
                         xmax = oxygen_max),
                     height = 0.5) +
      geom_point(aes(x = oxygen_med),
                 data = timepoint_summary,
                 color = 'red') +
      scale_x_log10() +
      labs(x = 'median oxygen',
           y = 'depth (m)')
  }
  else{
    print("Looks like that station and line don't work! Try another one.")
  }
}

temp_ts_plot <- function(n_ranges, date_min, date_max){
  if (n_ranges > 1){
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac= rep("[0,500]",length(year)))
  }
  
  x <- aggregate(list(bottle$temperature,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
  colnames(x) <- c('year', 'quarter','depth_fac','temperature','date')
  x <- x[order(x$year, x$quarter),]
  rownames(x)<-1:nrow(x)
  
  x %>% 
    ggplot(aes(x=date, y=temperature, group=depth_fac, color=depth_fac, shape=as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = "Median Temperature Across All Stations Over Time", x = "Date", y = "Temperature (°C)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA),expand = c(0.1, 0.1)) +
    theme_bw() 
}

oxy_ts_plot <- function(n_ranges, date_min, date_max){
  if (n_ranges > 1){
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac= rep("[0,500]",length(year)))
  }
  
  x <- aggregate(list(bottle$oxygen,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
  colnames(x) <- c('year', 'quarter','depth_fac','oxygen','date')
  x <- x[order(x$year, x$quarter),]
  rownames(x)<-1:nrow(x)
  
  x %>% 
    ggplot(aes(x=date, y=oxygen, group=depth_fac, color=depth_fac, shape=as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = title = "Median Oxygen Across All Stations Over Time", x = "Date", y = "Oxygen (mL/L)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), expand = c(0.1, 0.1)) +
    theme_bw() 
  
}

cho_ts_plot <- function(n_ranges, date_min, date_max){
  if (n_ranges > 1){
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac= rep("[0,500]",length(year)))
  }
  
  x <- aggregate(list(bottle$chlorophyll,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
  colnames(x) <- c('year', 'quarter','depth_fac','chlorophyll','date')
  x <- x[order(x$year, x$quarter),]
  rownames(x)<-1:nrow(x)
  
  x %>% 
    ggplot(aes(x=date, y=chlorophyll, group=depth_fac, color=depth_fac, shape=as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = "Median Chlorophyll Across All Stations Over Time", x = "Date", y = "Chlorophyll (mg/L)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(0,0.8)) +
    theme_bw() 
  
}

sal_ts_plot <- function(n_ranges, date_min, date_max){
  if (n_ranges > 1){
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle <- bottle %>% 
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac= rep("[0,500]",length(year)))
  }
  
  x <- aggregate(list(bottle$salinity,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
  colnames(x) <- c('year', 'quarter','depth_fac','salinity','date')
  x <- x[order(x$year, x$quarter),]
  rownames(x)<-1:nrow(x)
  
  x %>% 
    ggplot(aes(x=date, y=salinity, group=depth_fac, color=depth_fac, shape=as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = "Median Salinity Across All Stations Over Time", x = "Date", y = "Salinity (Practical Salinity Scale)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(33,36)) +
    theme_bw() 
}

#oxy_ts_plot(1,'2000-01-15','2010-05-20')

save(
  list = ls(),
  file = save_rda
)

## ----------------------
## TESTS

# make_basemap() %>%
#    update_basemap(get_map_data(1984, 4), get_kriging_data(1984,4,as.factor('[0,50)')))
# 
# make_profile(2012, "093.3")
# make_station_line(2014, "093.3")
# make_station_line_chlor(2000, "093.3")
# #Do a green/cooler color for the cooler temp to red warm for temperature
# make_station_line_temp(2019, "080.0")
# # pink scale for pink salt
# make_station_line_salinity(2014, "093.3")
