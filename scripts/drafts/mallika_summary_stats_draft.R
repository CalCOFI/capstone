make_cross_section <- function(yr){
  output <- bottle %>%
    # filter to year, quarter, and station of interest
    filter(year == yr) %>%
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
    # is na.rm actually removing all NA's? Check!!
    # Might have to split it here. We compute our criterion based on the result of this
    ggplot(aes(x = distance, y = fct_rev(depth_interval))) +
    facet_wrap(~ quarter, 
               # scales = "free_x",
               nrow = 4) +
    # using geom_tile instead of raster in order to create boxes of different widths
    geom_tile(aes(fill = oxygen, width = distance)) +
    # adjust color scale
    scale_fill_gradient2(low = '#E74C3C',
                         mid = '#000000',
                         high = '#1093eb',
                         limits = c(0.01, 6.0), 
                         midpoint = log10(1.4),
                         trans = 'log10', # tinker with scales!
                         # might have to replace values in the data frame 
                         # above our limit with the limie
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
#if criterion is true, return output, else print some message that explains that
# the criterion wasn't met

make_cross_section(1999)
make_cross_section(2019)


# Finding average oxygen at 50m
bottle %>%
  filter(depth <= 50) %>%
  mean(oxygen)

mean(filter(bottle, depth == 50)$oxygen, na.rm=TRUE)

#
stations_below_threshold <- function(yr, deep, threshold){
  bottle_subset <- filter(bottle, depth == deep, year == yr)
  bottle_subset_hypoxic <- filter(bottle_subset, oxygen <= threshold)
  return(100*nrow(bottle_subset_hypoxic)/nrow(bottle_subset))
}

depth_of_interest = 50
# threshold = mean(filter(bottle, depth == depth_of_interest)$oxygen, na.rm=TRUE)
threshold = 3

stations_below_threshold(1970, depth_of_interest, threshold)
stations_below_threshold(2019, depth_of_interest, threshold)


# data frame with each distinct time point. 
df <- bottle %>%
  group_by(year, quarter) %>%
  summarise(stations_below_threshold(year, depth_of_interest, threshold)) %>% 
  mutate(per_hyp_50 = stations_below_threshold(year, depth_of_interest, threshold)) 
  
df %>%
  ggplot(aes(x = year, y = per_hyp_50)) +
  geom_point() +
  geom_smooth()

# See if we can make a plot of the average oxygen value across stations at a given depth over time

bottle %>%
  # filter(depth == 50) %>%
  group_by(year, quarter) %>%
  summarize(oxygen = median(oxygen, na.rm = T)) %>%
  ggplot(aes(x = year, y = oxygen)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Avergage Oxygen concentration recorded over all stations and depths")

# Look at values over time rather than the proportion measure.
# tricky to interpret because sampling varies so much year to year


# When are low oxygen values coming into the depths that they usually don't


# For summary stats - only consider the years in which all the quarters were sampled

bottle_by_year <- bottle %>%
  select(year, quarter, oxygen) %>%
  group_by(year, quarter) %>% 
  summarize(mean(oxygen, na.rm = TRUE))

bottle_by_quarter <- bottle_by_year %>%
  pivot_wider(names_from = quarter, values_from = `mean(oxygen, na.rm = TRUE)`) %>%
  select(year, `1`, `2`, `3`, `4`)

years_with_all_qs <- na.omit(bottle_by_quarter)$year


# Looking at minimum values for a specific near-shore station
bottle %>%
  filter(year == years_with_all_qs,
         year >= 1985,
         depth <= 150,
         line == "093.3",
         station == "028.0") %>%
  group_by(year, quarter, station) %>%
  summarize(oxygen = min(oxygen, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = oxygen, color = quarter)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Minimum oxygen concentration for Station 093.3 028.0 above a depth of 150m", 
       subtitle = "Using only years after 1985 that have all quarters sampled", 
       caption = "Quarter 1 is Winter, 2 is Spring, 3 is Summer, 4 is Fall")


# Looking at minimum value for all near-shore stations
bottle %>%
  filter(year == years_with_all_qs,
         year >= 1985,
         depth <= 150,
         distance >= -50) %>%
  group_by(year, quarter) %>%
  summarize(oxygen = min(oxygen, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = oxygen, color = quarter)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Minimum oxygen for stations within 50m from shore above a depth of 150m", 
       subtitle = "Using only years after 1985 that have all quarters sampled", 
       caption = "Quarter 1 is Winter, 2 is Spring, 3 is Summer, 4 is Fall")




## GG Animate things

lin <- "093.3"

p <- bottle %>%
  # filter to year, quarter, and station of interest
  filter(line == lin) %>%
  # bin depths into roughly even numbers of observations
  mutate(depth_interval = cut_number(depth, 10)) %>%
  # aggregate within depth bins
  mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
         quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
         quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
         quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
  group_by(year,
           depth_interval,
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
  transition_time(year) +
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 8,
                                   vjust = 0.5),
        panel.grid = element_blank()) +
  labs(title = paste("Variation in Temperature for Line",
                     lin, "in {frame_time}"), 
       subtitle = "Plotted by depth with respect to distance from shore", 
       x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
       fill='Temperature (˚C)') 
  
animate(p)
  
  
  