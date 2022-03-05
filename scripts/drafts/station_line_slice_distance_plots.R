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
#processing dates
library(lubridate)
load("data/processed/bottle-cast-recent.RData")

## ----------------------------------------------------------------
#cast and bottle preprocessed data 
cast_bottle <- read_csv("/Users/mallika/Documents/GitHub/CalCOFI22/data/processed/bottle_and_cast.csv", show_col_types = FALSE) %>%
  mutate(date = mdy(Date)) %>%
  # direct names
  rename(quarter = Quarter,
         lat = Lat_Dec,
         lon = Lon_Dec,
         depth = Depthm,
         oxygen = O2ml_L,
         temperature = T_degC,
         salinity = Salnty,
         id = Sta_ID,
         cast = Cst_Cnt,
         distance = Distance) %>%
  # drop unused variables
  select(-c(Cast_ID, Btl_Cnt, Depth_ID, R_Depth, Date)) %>%
  # split id into station and line
  separate(id, c('line', 'station'), sep = ' ')


yr <- 2015
qr <- 4
ln <- "076.7"

cast_bottle %>%
  # filter to year, quarter, and station of interest
  filter(Year == yr, 
         # quarter == qr,
         line == ln) %>%
  # bin depths into roughly even numbers of observations
  mutate(depth_interval = cut_number(depth, 10)) %>%
  # aggregate within depth bins
  group_by(depth_interval,
           quarter,
           distance) %>%
  summarize(oxygen = median(oxygen, na.rm = T)) %>% # tinker with summary stat
  ggplot(aes(x = distance, y = fct_rev(depth_interval))) +
  facet_wrap(~ quarter, nrow = 4,
             # labeller = labeller(list(1 = "Winter",
             #                 2= "Spring",
             #                 3 = "Summar",
             #                 4 = "Fall")),
             scales = "free_x") +
  # using geom_tile instead of raster in order to create boxes of different widths
  geom_tile(aes(fill = oxygen, width = distance)) +
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
  labs(x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
       fill='Oxygen (millileters O2/Liter of seawater)') 

# ggsave(filename = 'tdr-drafts/figures/line-profile-2010-q3.png', 
#        width = 4, 
#        height = 4, 
#        units = 'in')

