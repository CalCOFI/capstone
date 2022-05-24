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
start_test <- as.Date("07-02-2000", "%m-%d-%Y")
end_test <- as.Date("07-02-2010", "%m-%d-%Y")
value_test <- as.Date("07-02-2005", "%m-%d-%Y")
lineID_test <- '091.7'
stationID_test <- '026.4'

get_date <- function(time){
  bottle %>%
    group_by(year, quarter) %>%
    summarize(across(date, .fns = list(min = min, max = max)), .groups = "drop") %>%
    filter(date_max > ymd(time)) %>%
    slice_min(date_min) %>%
    pull(quarter)
}


## ---------------------------------------------------------------------------------------
data_test <- bottle %>%
  subset(start_test < date & end_test > date) %>%
  filter(depth <= 1000) %>%
  mutate(depth_interval = cut_number(depth, 20)) %>%
  group_by(station, line, depth_interval, date)  %>%
  summarize(across(oxygen, .fns = list(min = min, 
                                       max = max, 
                                       med = median)))

summarise(min_oxy = min(oxygen, na.rm = TRUE),
          max_oxy = max(oxygen, na.rm = TRUE)) %>%
  #Filter by station and date value
  filter(station == stationID_test & line == lineID_test) %>%
  filter(date == get_date(value_test)$date_min)


## ----echo=FALSE-------------------------------------------------------------------------
#Find all oxygen value in the date range at a specific line, station, depth chunks
get_oxy <- function(start, end, value, stationID, lineID){
  bottle %>%
    subset(start < date & end > date) %>%
    filter(depth <= 1000) %>%
    mutate(depth_interval = cut_number(depth, 20)) %>%
    group_by(station, line, depth_interval, date)  %>%
    summarise(min_oxy = min(oxygen, na.rm = TRUE),
              max_oxy = max(oxygen, na.rm = TRUE)) %>%
    #Filter by station and date value
    filter(station == stationID & line == lineID) %>%
    filter(date == get_date(value)$date_min)
}


## ---------------------------------------------------------------------------------------
data <- get_oxy(start_test, end_test, value_test, lineID_test, stationID_test)


## ---------------------------------------------------------------------------------------
data_test


## ---------------------------------------------------------------------------------------
# custom transformation for depth profiles
rev_sqrt <- trans_new('revsqrt', 
                      function(x) -sqrt(x),
                      function(x) x^2,
                      breaks = breaks_log(n = 5, base = 10))



## ---------------------------------------------------------------------------------------
ggplot(data_test, aes(x=max_oxy, y=depth_interval)) + 
  geom_point(size = 2) + 
  scale_y_discrete(limits = rev) +
  #scale_x_discrete(position = 'top')+
  geom_hline(yintercept = 0) +
  labs(x = 'Oxygen (ml of O_2/L of seawater)') +
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 8, 
                                   vjust = 0.5),
        axis.text.y = element_text(size = 8))

## ---------------------------------------------------------------------------------------

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

# more descriptive name
get_nearest_date <- get_date

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
