library(scales)
library(htmltools)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(hrbrthemes)
load("data/processed/bottle.RData")

bottle <- bottle %>% mutate(depth_fac = cut(depth, c(-.01, 200, 1000, Inf)))


x<-aggregate(list(bottle$temperature,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=mean, na.rm=T)
colnames(x) <- c('year', 'quarter','depth_fac','temperature','date')
x <- x[order(x$year, x$quarter),]
rownames(x)<-1:nrow(x)
#x<-mutate(x,ID = as.numeric(rownames(x)))
x

tfull <- x %>% 
  ggplot(aes(x=date, y=temperature, colour=depth_fac)) +
  geom_point(aes(shape=as.factor(quarter)),na.rm=T) +
  #geom_line() +
  xlab("Date") + 
  ylab("Temperature") +
  scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2004-11-30")), date_labels = "%Y %b %d") 

tfull

y<-aggregate(bottle$oxygen, list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=mean, na.rm=T)
colnames(y) <- c('year','quarter','depth_fac','oxygen')
y <- y[order(y$year, y$quarter),]
rownames(y)<-1:nrow(y)
y<-mutate(y,ID = as.numeric(rownames(y)))


ofull <- y %>% 
  ggplot(aes(x=ID, y=oxygen, colour=depth_fac)) +
  geom_point(na.rm=T) +
  #geom_line() +
  xlab("Date") + 
  ylab("Oxygen") 
  
ofull

#geom_errorbar(aes(ymin=temperature-sd, ymax = temperature+sd, width = .1))