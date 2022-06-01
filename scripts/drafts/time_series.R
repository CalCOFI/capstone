library(scales)
library(htmltools)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(hrbrthemes)
load("data/processed/bottle.RData")

temp_ts_plot <- function(n_ranges, date_min, date_max){
  bottle <- bottle %>% 
    filter(depth>=0 & depth<=500) %>%
    mutate(depth_fac = cut(depth, n_ranges))
  
  x <- aggregate(list(bottle$temperature,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
  colnames(x) <- c('year', 'quarter','depth_fac','temperature','date')
  x <- x[order(x$year, x$quarter),]
  rownames(x)<-1:nrow(x)
  
  x %>% 
    ggplot(aes(x=date, y=temperature, group=depth_fac, color=depth_fac, shape=as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(x = "Date", y = "Temperature (°C)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA),expand = c(0.1, 0.1)) +
    theme_bw()  
}

oxy_ts_plot <- function(n_ranges, date_min, date_max){
  bottle <- bottle %>% 
    filter(depth>=0 & depth<=500) %>%
    mutate(depth_fac = cut(depth, n_ranges))
  
  x <- aggregate(list(bottle$oxygen,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
  colnames(x) <- c('year', 'quarter','depth_fac','oxygen','date')
  x <- x[order(x$year, x$quarter),]
  rownames(x)<-1:nrow(x)
  
  x %>% 
    ggplot(aes(x=date, y=oxygen, group=depth_fac, color=depth_fac, shape=as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = "", x = "Date", y = "Oxygen (mL/L)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), expand = c(0.1, 0.1)) +
    theme_bw() 
  
}

temp_ts_plot(5,'1970-06-14', '2020-01-15')
