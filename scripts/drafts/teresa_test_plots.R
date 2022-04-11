## SOURCE FILE
# knitr::purl(input = 'teresa_test_plots.Rmd', 
#             output = 'teresa_test_plots.R')

## ----------------------------------------------------------------
# packages
library(readr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(leaflet)
library(data.table)
library(lubridate)

## DATA ----------------------------------------------------------------
#cast and bottle preprocessed data; note col types avoids parse failures
cast_bottle <- read_csv("data/processed/bottle_and_cast.csv",
                        col_types = cols(pH1 = col_double(),
                                         pH2 = col_double()))

load('data/processed/bottle.RData')

## ----------------------------------------------------------------

#creating a new dataframe from the cast_bottle preprocessed data 
# sal_tem_o2 <- bottle %>% 
# #selecting only temp, salinity, oxygen, depth, bottle id, cast id, year for new dataframe
#   select(T_degC, Salnty, O2ml_L, Depthm, Btl_Cnt, Cast_ID, Year) %>% 
# #making a longer dataframe and putting temp, salinity, oxygen into a column called variable 
# #and the values into a column called values 
#   pivot_longer(1:3, names_to = "Variable", values_to = "Value")
# 
# 
# ## ----------------------------------------------------------------
# #creating a plot from the new dataframe with the salinity, temp, oxygen -> variable column 
# sal_tem_o2_vs_depth <- ggplot(data = sal_tem_o2, aes(x = Value, y = Depthm)) + 
# #putting everything into one graph and color coding by what type of variable 
#   geom_point(aes(color = Variable)) + 
#   scale_color_manual(values = c("darkred", "steelblue", "darkgreen")) + 
# #reversing the y-axis so it resembles a thermocline graph 
#   scale_y_reverse()
#   
# 
# sal_tem_o2_vs_depth

# panel of graphs instead of all on same/ diff scales? 
#time and space? 

bottle %>% 
  rename(depth = Depthm) %>%
  filter(Year == 2010, depth < 500, Quarter == 3) %>%
  select(T_degC, Salnty, O2ml_L,
         depth, Btl_Cnt, Cast_ID,
         Year) %>% 
  pivot_longer(1:3, 
               names_to = "measurement", 
               values_to = "value") %>%
  ggplot(aes(x = value, y = depth,
             group = interaction(Cast_ID, measurement))) +
  geom_path(alpha = 0.1) +
  scale_y_reverse() +
  facet_wrap(~ measurement, 
             nrow = 1, 
             scales = 'free_x')

ggsave(filename = 'tdr-drafts/figures/clines-2010-q3.png', 
       width = 8, 
       height = 6, 
       units = 'in')



## ----------------------------------------------------------------
#salinity plot 
salinity_plot <- ggplot() + 
#putting everything into one graph and color coding by what type of variable 
  geom_line(data = cast_bottle, aes(x = Salnty, y = Depthm)) + 
  scale_y_reverse()

salinity_plot


#temperature plot 
temp_plot <- ggplot() + 
#putting everything into one graph and color coding by what type of variable 
  geom_line(data = cast_bottle, aes(x = T_degC, y = Depthm)) + 
  scale_y_reverse()

temp_plot

#oxygen plot 
o2_plot <- ggplot() + 
#putting everything into one graph and color coding by what type of variable 
  geom_line(data = cast_bottle, aes(x = O2ml_L, y = Depthm)) + 
  scale_y_reverse()

o2_plot

#panel of graphs

sal_temp_o2_grid <- ggarrange(salinity_plot, temp_plot, o2_plot, 
          ncol = 3)



## ----------------------------------------------------------------

#multiple sampling in a year, first make an ave, (use day, month, hour), use just one depth, or location 

year_o2_plot <- ggplot(data = cast_bottle, aes(x = Year, y = O2ml_L, group = )) + 
  geom_line() + 
  scale_color_manual(values = c("darkred", "steelblue", "darkgreen"))
  

year_o2_plot


## ----------------------------------------------------------------

#all Na's for depth with this current data, not much to do with pH right now 
ph1_vs_depth <- ggplot(data = cast_bottle, aes(x = pH1, y = Depthm)) + 
  geom_line()
  
ph1_vs_depth


## ----------------------------------------------------------------
#take the averages of the lat/long for each station 
station_avg <- setDT(cast_bottle)[, .(mean_lat = mean(Lat_Dec), mean_long = mean(Lon_Dec)), by = Sta_ID]

yr <- 2010

station_info <- cast_bottle %>%
  filter(Year == yr) %>%
  # coerce date to datetime
  mutate(date = mdy(Date)) %>%
  # select spatiotemporal info and oxygen columns
  select(Lat_Dec, 
         Lon_Dec, 
         Depthm, 
         Sta_ID,
         date) %>%
  # better names
  rename(id = Sta_ID,
         lat = Lat_Dec,
         lon = Lon_Dec,
         depth = Depthm) %>%
  distinct() %>%
  # split id into line and station
  separate(id, c('line', 'station'), sep = ' ') %>%
  # aggregate by station and line
  group_by(line, station) %>%
  summarize(lat = mean(lat),
            lon = mean(lon),
            maxdepth = max(depth), 
            n = n_distinct(date)) %>%
  ungroup()


bottom_depth <- bottle %>% 
  group_by(station) %>% 
  summarise(bottomd = max(bottomd, na.rm = TRUE))

station_info %>%
  mutate(label_line1 = paste('Line ID', line, sep = ' '),
         label_line2 = paste('Station ID', station, sep = ' '),
         label_line3 = paste('Max depth', maxdepth, sep = ' '),
         label_line4 = paste('Visited', n, 'time(s)', sep = ' ')) %>%
  unite(label,
        contains('label'),
        sep = ' <br/> ') %>%
  dplyr::select(-contains('label_line')) %>%
  leaflet() %>% 
#set the latitude and longitude for California, zoom into desired area 
  setView(lng = mean(station_info$lon), 
          lat = mean(station_info$lat), 
          zoom = 5) %>%
#add markers for each of the stations average lat and long 
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(
    lat = ~lat, 
    lng = ~lon, 
    popup = ~label, 
    color = "red", 
    radius = (log(bottom_depth$bottomd))/10) %>% 
  addPolylines(lat = ~lat, 
               lng = ~lon, 
               color = "blue",
               data = filter(station_info, line == '066.7'))
  

#chooses unique station lines (filters dataset by unique station lines) --> draws line for each station
for () 
 station_info<-station_info %>% 
  addPolylines(lat=c(),
              lng=c())






