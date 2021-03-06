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

temp_ts_plot(1,'2000-01-15','2020-01-15')

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
    labs(title = "Median Oxygen Across All Stations Over Time", x = "Date", y = "Oxygen (mL/L)", color = "Depth Range (m)", shape = "Quarter") +
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
