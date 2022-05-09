library(tidyverse)
library(sp)
library(sf)
library(gstat)
library(automap)
library(spdplyr)
library(modelr)
library(lubridate)
load("data/processed/bottle.RData")

# filter to a single timepoint
yr <- 2010
qr <- 3

test_data <- bottle %>%
  filter(year == yr, 
         quarter == qr) %>%
  # drop missing observations (but why missing??)
  na.omit() %>%
  # bin depth into intervals
  mutate(depth_layer = cut_number(depth, 5))

## some exploratory plots
##########################


# sample sizes

sampsizes <- test_data %>%
  group_by(depth_layer, line, station) %>%
  summarize(n = n()) 

sampsizes %>%
  ungroup() %>%
  group_by(depth_layer) %>%
  summarize(across(n, .fns = list(min = min, 
                                  med = median, 
                                  mean = mean, 
                                  max = max)))

sampsizes %>%
  ggplot(aes(y = depth_layer, x = n)) +
  geom_jitter(width = 0.01, 
              height = 0.2,
              alpha = 0.5) +
  scale_x_log10()

# ggsave(filename = 'tdr-drafts/figures/depth-bins-2010-q3.png', 
#        width = 4, 
#        height = 4, 
#        units = 'in')

# sampling timeframe

test_data %>%
  distinct(station, line, date) %>% 
  ggplot(aes(x = date, y = line)) + 
  geom_jitter(width = 0, 
              height = 0, 
              alpha = 0.3)

## processing
###############

# (hacky) remove any observations 10 or more days from the median sampling date
test_data_sub <- test_data %>% 
  mutate(day_diff = yday(date) - median(yday(date))) %>%
  filter(abs(day_diff) < 10) %>%
  select(-day_diff)

# recheck sample sizes
test_data_sub %>%
  group_by(depth_layer, line, station) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(depth_layer) %>%
  summarize(across(n, .fns = list(min = min, 
                                  med = median, 
                                  mean = mean, 
                                  max = max)))

# subsample and aggregate (to ensure equal variance)
interpolation_data <- test_data_sub %>%
  ungroup() %>%
  group_by(depth_layer, station, line) %>%
  sample_n(5, replace = T) %>%
  summarize(oxygen = mean(oxygen),
            lat = unique(lat),
            lon = unique(lon))

## spatial interpolation, one depth
####################################

# function to project geo to grid and return sp object
project_fn <- function(df){
  out <- df %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
    st_set_crs(4326) %>% 
    st_transform(2770) %>%
    as('Spatial')
  
  return(out)
  # see https://epsg.io/2770 for EPSG
  # see https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
  }

# sanity check
interpolation_data %>%
  project_fn() %>%
  as_tibble() %>%
  select(starts_with('coords')) %>%
  distinct() %>%
  ggplot(aes(x = coords.x1, y = coords.x2)) +
  geom_point()

# filter to depth
bin_num <- 2
fit_data <- interpolation_data %>%
  filter(depth_layer == levels(depth_layer)[bin_num]) %>%
  project_fn()

# empirical variogram
v_emp <- variogram(object = oxygen ~ 1,
                   data = fit_data,
                   width = 25000)
plot(v_emp)

# fit a variogram model
v_fit <- autofitVariogram(oxygen ~ 1, fit_data)
plot(v_fit)

# prediction grid
pred_grid <- fit_data %>%
  as_tibble() %>%
  distinct(coords.x1, coords.x2) %>%
  data_grid(x1 = seq_range(coords.x1, 100),
            x2 = seq_range(coords.x2, 100)) %>%
  st_as_sf(coords = c('x1', 'x2')) %>%
  st_set_crs(2770) %>%
  as('Spatial')

# interpolation
preds_sp <- autoKrige(oxygen ~ 1, fit_data, pred_grid)

# plot surface
preds_sp$krige_output %>%
  as_tibble() %>%
  rename(oxygen = var1.pred,
         oxygen_se = var1.var) %>%
  ggplot(aes(x = coords.x1, y = coords.x2)) +
  geom_raster(aes(fill = oxygen,
                  alpha = -oxygen_se),
              interpolate = T) +
  scale_fill_gradient2(low = '#E74C3C',
                       mid = '#F1C40F',
                       high = '#3498DB',
                       midpoint = log10(1.4),
                       trans = 'log10') +
  guides(alpha = guide_none()) +
  theme_minimal()

## interpolation, iterated over many depths
############################################

# wrapper around variogram
variogram_fn <- function(sp){
  variogram(oxygen ~ 1, data = sp) %>%
    as_tibble() %>%
    select(np, dist, gamma)
}

# 'nesting' for functional programming
nested_data <- interpolation_data %>%
  ungroup() %>%
  select(depth_layer, lon, lat, oxygen) %>%
  nest(data = c(lon, lat, oxygen))

# compute empirical variograms
variograms <- nested_data %>%
  mutate(sp = map(data, project_fn)) %>%
  mutate(vemp = map(sp, variogram_fn))

variograms %>%
  unnest(vemp) %>%
  ggplot(aes(x = dist/1000, y = gamma)) +
  facet_wrap(~ depth_layer, nrow = 1) +
  geom_point() +
  labs(x = 'distance (km)', y = 'semivariance')

# ggsave(filename = 'tdr-drafts/figures/semivariograms-2010-q3.png', 
#        width = 8, 
#        height = 6, 
#        units = 'in')

# wrapper around krige
autokrige_fn <- function(sp, grid){
  autoKrige(oxygen ~ 1, sp, grid)$krige_output %>%
    as_tibble()
}

# prediction grid (needs refinement)
pred_grid <- interpolation_data %>%
  distinct(lat, lon) %>%
  project_fn() %>%
  as_tibble() %>%
  data_grid(x1 = seq_range(coords.x1, 50),
            x2 = seq_range(coords.x2, 50)) %>%
  st_as_sf(coords = c('x1', 'x2')) %>%
  st_set_crs(2770) %>%
  as('Spatial') 

# ordinary kriging at each depth interval
interpolation_df <- interpolation_data %>%
  ungroup() %>%
  select(depth_layer, lon, lat, oxygen) %>%
  # nest
  nest(data = c(lon, lat, oxygen)) %>%
  # convert to spatialpointsdataframe
  mutate(sp = map(data, project_fn)) %>%
  # generate prediction grid (SHOULD BE SENSITIVE TO DATA??)
  mutate(grid = map(depth_layer, ~ pred_grid)) %>%
  # compute predictions (SHOULD ALSO STORE METADATA??)
  mutate(preds = map2(sp, grid, autokrige_fn))

# plot surfaces
interpolation_df %>%
  unnest(preds) %>%
  rename(x = coords.x1,
         y = coords.x2, 
         oxygen = var1.pred) %>%
  mutate(oxygen_se = sqrt(var1.var)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_raster(aes(fill = oxygen, 
                  alpha = -oxygen_se), 
              interpolate = T) +
  geom_contour(aes(z = oxygen),
               color = 'black',
               alpha = 0.5,
               breaks = 1.4) +
  facet_wrap(~ depth_layer, nrow = 1) +
  scale_fill_gradient2(low = '#E74C3C',
                       mid = '#F1C40F',
                       high = '#3498DB',
                       midpoint = 1.4,
                       trans = 'sqrt') +
  guides(alpha = guide_none()) +
  theme_minimal() +
  labs(x = '', y = '') +
  theme(axis.text = element_blank())

# ggsave(filename = 'tdr-drafts/figures/interpolation-2010-q3.png', 
#        width = 8, 
#        height = 6, 
#        units = 'in')

## notes
############

# 1. functionalize generation of prediction grid
# 2. how to bin depth to layers?? want to balance number of bins with resolution at depths of interest
# 3. iterate also over year, quarter
# 4. probably shouldn't use hard cutoff of 1.4, maybe 1.4 + 1se?