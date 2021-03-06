## ----setup, include=FALSE----------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------
#read in data 
library(readr)
#plotting 
library(ggplot2)
library(tidyverse)
#arranging plots in panel
library(ggpubr)
#creating interactive maps in R 
library(leaflet)
#splitting and averaging by unique values
library(data.table)
load("data/processed/bottle-cast-recent.RData")

## ----------------------------------------------------------------
#cast and bottle preprocessed data 
# cast_bottle <- read_csv("data/bottle_and_cast.csv", show_col_types = FALSE)
cast_bottle <- bottle

#descriptions of the variables in the cast and bottle preprocessed data 
# descrip <- read_csv("data/Bottle Field Descriptions (1).csv", show_col_types = FALSE)


## ----------------------------------------------------------------
# cast_bottle$Sta_ID %>% substr(1, 5)=="093.3"  # determining how to select for line number within Station ID
# cast_bottle$Date == "01/07/2000"  # determining how to select for line number within Station ID
#  #Alternative subsetting method
# line_93.3 <- subset(cast_bottle, substr(cast_bottle$Sta_ID, 1, 5)=="093.3", select= c(Date, Depthm, O2ml_L))

# ## Quarter 1
# line_93.3_2000_1 <- cast_bottle[which(substr(cast_bottle$Sta_ID, 1, 5)=="093.3" & cast_bottle$Quarter == 1 & cast_bottle$Year == 2000),] %>%
#   mutate(Station = Sta_ID %>% substr(7, 11)) # Adding separate column with station number
# 
# # Data frame with all observations from line 93.3 in Quarter 1 of 2000 
# line_93.3_2000_1  
# 
# 
# ## Quarter 2
# line_93.3_2000_2 <- cast_bottle[which(substr(cast_bottle$Sta_ID, 1, 5)=="093.3" & cast_bottle$Quarter == 2 & cast_bottle$Year == 2000),] %>%
#   mutate(Station = Sta_ID %>% substr(7, 11)) # Adding separate column with station number
# 
# # Data frame with all observations from line 93.3 in Quarter 1 of 2000 
# line_93.3_2000_2  
# 
# 
# ## Quarter 3
# line_93.3_2000_3 <- cast_bottle[which(substr(cast_bottle$Sta_ID, 1, 5)=="093.3" & cast_bottle$Quarter == 3 & cast_bottle$Year == 2000),] %>%
#   mutate(Station = Sta_ID %>% substr(7, 11)) # Adding separate column with station number
# 
# # Data frame with all observations from line 93.3 in Quarter 1 of 2000 
# line_93.3_2000_3
# 
# 
# ## Quarter 4
# line_93.3_2000_4 <- cast_bottle[which(substr(cast_bottle$Sta_ID, 1, 5)=="093.3" & cast_bottle$Quarter == 4 & cast_bottle$Year == 2000),] %>%
#   mutate(Station = Sta_ID %>% substr(7, 11)) # Adding separate column with station number
# 
# # Data frame with all observations from line 93.3 in Quarter 1 of 2000 
# line_93.3_2000_4
# 
# 
# ## ----------------------------------------------------------------
# # Graphing oxygen concentration at different depths for each station line through different quarters of 2000
# station_93.3_2000_1_slice <- line_93.3_2000_1 %>%
#   ggplot(aes(x = Station, y = -Depthm)) +
#   geom_point(aes(color = O2ml_L), size = 3)
# station_93.3_2000_1_slice 

yr <- 2010
qr <- 3
ln <- "093.3"

cast_bottle %>%
  # add line and station variables
  mutate(line = substr(Sta_ID, 1, 5),
         station = substr(Sta_ID, 7, 11)) %>%
  # filter to year, quarter, and station of interest
  filter(Year == yr, 
         Quarter == qr,
         line == ln) %>%
  # bin depths into roughly even numbers of observations
  mutate(depth_interval = cut_number(Depthm, 10)) %>%
  # aggregate within depth bins
  group_by(depth_interval, 
           station) %>%
  summarize(oxygen = median(O2ml_L, na.rm = T)) %>% # tinker with summary stat
  # make raster plot
  ggplot(aes(x = station, y = fct_rev(depth_interval))) +
  geom_raster(aes(fill = oxygen)) +
  # adjust color scale
  scale_fill_gradient2(low = '#E74C3C',
                       mid = '#F1C40F',
                       high = '#3498DB',
                       midpoint = log10(1.4),
                       trans = 'log10') +
  # aesthetics
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 8,
                                   vjust = 0.5),
        panel.grid = element_blank()) +
  labs(x = 'station id', y = 'depth') 

ggsave(filename = 'tdr-drafts/figures/line-profile-2010-q3.png', 
       width = 4, 
       height = 4, 
       units = 'in')

# 
# station_93.3_2000_2_slice <- line_93.3_2000_2 %>%
#   ggplot(aes(x = Station, y = -Depthm)) +
#   geom_point(aes(color = O2ml_L), size = 3)
# station_93.3_2000_2_slice
# 
# station_93.3_2000_3_slice <- line_93.3_2000_3 %>%
#   ggplot(aes(x = Station, y = -Depthm)) +
#   geom_point(aes(color = O2ml_L), size = 3)
# station_93.3_2000_3_slice
# 
# station_93.3_2000_4_slice <- line_93.3_2000_4 %>%
#   ggplot(aes(x = Station, y = -Depthm)) +
#   geom_point(aes(color = O2ml_L), size = 3)
# station_93.3_2000_4_slice
# 
# 
# ## ---- fig.width = 8, fig.height = 5------------------------------
# # Attempting to use facet_wrap
# line_93.3_2000_slices <- cast_bottle[which(substr(cast_bottle$Sta_ID, 1, 5)=="093.3" & cast_bottle$Year == 2000),] %>%
#   mutate(Station = Sta_ID %>% substr(7, 11)) %>% # Adding separate column with station number 
#   pivot_longer(cols = Quarter) %>%
#   ggplot(aes(x = Station, y = -Depthm)) +
#   geom_point(aes(color = O2ml_L), size = 3) +
#   facet_wrap(~ value, nrow = 2, scales = "free_x") +
#   labs(x = "Station", y = "Depth (m)")
# 
# line_93.3_2000_slices

