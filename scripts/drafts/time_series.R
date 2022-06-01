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
    scale_y_continuous(limits=c(NA,24), expand = c(0, 0)) +
    geom_line(linetype='dashed') +
    labs(title = "Temperature over Time", x = "Date", y = "Temperature (°C)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d") 
    
}

temp_ts_plot(6,'1971-06-14', '2004-01-15')

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
    scale_y_continuous(limits=c(NA,7), expand = c(0, 0)) +
    geom_line(linetype='dashed') +
    labs(title = "Oxygen over Time", x = "Date", y = "Temperature (°C)", color = "Depth Range (m)", shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d") 
  
}

oxy_ts_plot(5,'1970-06-14', '2020-01-15')
