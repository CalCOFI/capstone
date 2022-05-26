## ----setup, include=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo = FALSE-----------------------------------------------------------------------
# library(plotly)
library(lubridate)
library(dplyr)
library(here)
library(ggplot2)
library(tidyverse)
library(scales)


## ---------------------------------------------------------------------------------------
bottle_rda <- here("data/processed/bottle.RData")
load(bottle_rda)


## ---------------------------------------------------------------------------------------
bottle


## ----echo=FALSE-------------------------------------------------------------------------
#Get the average oxygen for a given time period
start_test <- as.Date("03-01-2007", "%m-%d-%Y")
end_test <- as.Date("05-09-2020", "%m-%d-%Y")
value_test <- as.Date("07-02-2009", "%m-%d-%Y")
lineID_test <- '083.3'
stationID_test <- '085.0'

get_nearest_date <- function(time){
  bottle %>%
    group_by(year, quarter) %>%
    summarize(across(date, .fns = list(min = min, max = max)), .groups = "drop") %>%
    filter(date_max > ymd(time)) %>%
    slice_min(date_min) %>%
    pull(quarter)
}
# really can only do this for stations sampled below a certain depth
possible_stations <- bottle %>% 
  filter(depth <= 1000,
         date > start_test,
         date < end_test) %>%
  group_by(line, station) %>%
  summarize(max_depth = max(depth), .groups = 'drop') %>%
  filter(max_depth > 100)

set.seed(052422)
test_station <- possible_stations %>% sample_n(1)

# summary stats for full date range
range_summary <- bottle %>%
  filter(depth <= 1000,
         date > start_test,
         date < end_test,
         line == test_station$line,
         station == test_station$station) %>%
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
         line == test_station$line,
         station == test_station$station) %>%
  mutate(diff = date - value_test) %>%
  filter(diff == min(abs(diff))) %>% # oddly, faster than slice_min
  mutate(depth_layer = cut_width(depth, 50)) %>%
  group_by(depth_layer) %>%
  summarize(across(.cols = c(oxygen, salinity, chlorophyll, temperature),
                   .fns = c(min = ~ min(.x, na.rm = T),
                            max = ~ max(.x, na.rm = T),
                            med = ~ median(.x, na.rm = T))),
            n = n(),
            quarter = unique(quarter))


ggsave('results/depthavg_opt1.png',
       width = 5, height = 4, units = 'in', dpi = 300)

# option 2: drop quarter from grouping
range_summary <- bottle %>%
  filter(depth <= 1000,
         date > start_test,
         date < end_test,
         line == test_station$line,
         station == test_station$station) %>%
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
## ---------------------------------------------------------------------------------------
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
      
      
      ggsave('results/depthavg_opt1.png',
             width = 5, height = 4, units = 'in', dpi = 300)
      
      # option 2: drop quarter from grouping
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
#===================================
# option 1: keep quarter in grouping
ggplot(data = range_summary, 
       aes(y = fct_rev(depth_layer),
           color = factor(quarter))) +
  geom_path(aes(x = oxygen_med,
                group = factor(quarter))) +
  geom_errorbarh(aes(xmin = oxygen_min,
                     xmax = oxygen_max),
                 height = 0.5) +
  geom_point(aes(x = oxygen_med),
             data = timepoint_summary) +
  scale_x_log10() +
  labs(x = 'median oxygen',
       y = 'depth (m)')

ggsave('results/depthavg_opt2.png',
       width = 4, height = 4, units = 'in', dpi = 300)


# option 3: fit a loess with std errors

timepoint_raw <- bottle %>%
  filter(depth <= 1000,
         line == test_station$line,
         station == test_station$station) %>%
  mutate(diff = date - value_test) %>%
  filter(diff == min(abs(diff)))

bottle %>%
  filter(depth <= 1000,
         date > start_test,
         date < end_test,
         line == test_station$line,
         station == test_station$station) %>%
  ggplot(aes(x = oxygen, y = -depth)) +
  geom_point(alpha = 0.1, size = 0.7) +
  geom_smooth(method = 'loess', 
              orientation = 'y',
              se = T,
              span = 0.85, # controls smoothness
              size = 0.7,
              level = 0.99) +
  geom_point(data = timepoint_raw,
             size = 0.8,
             alpha = 0.5,
             color = 'red') +
  geom_path(data = timepoint_raw,
            color = 'red',
            alpha = 0.5)

ggsave('results/depthavg_opt3.png',
       width = 4, height = 4, units = 'in', dpi = 300)
