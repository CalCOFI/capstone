library(scales)
library(htmltools)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(hrbrthemes)
load("data/processed/bottle.RData")

bottle <- bottle %>% mutate(depth_fac = cut(depth, c(-.01, 50, 100, 200,500)))


x<-aggregate(list(bottle$temperature,bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
colnames(x) <- c('year', 'quarter','depth_fac','temperature','date')
x <- x[order(x$year, x$quarter),]
rownames(x)<-1:nrow(x)
#x<-mutate(x,ID = as.numeric(rownames(x)))
x

tfull <- x %>% 
  ggplot(aes(x=date, y=temperature, group=depth_fac, color=depth_fac,shape=as.factor(quarter))) +
  geom_point(na.rm=T) +
  geom_line(linetype='dashed') +
  labs(title = "Temperature over Time", x = "Date", y = "Temperature (°C)",color="Depth Range (m)", shape = "Quarter") +
  scale_shape_discrete(name="Quarter",
                        breaks=c("1", "2", "3","4"),
                        labels=c("Winter", "Spring", "Summer","Fall")) +
  scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2004-11-30")), date_labels = "%Y %b %d")+ 
  ylim(NA,18)

tfull

y<-aggregate(list(bottle$oxygen, bottle$date), list(bottle$year,bottle$quarter,bottle$depth_fac), FUN=median, na.rm=T)
colnames(y) <- c('year','quarter','depth_fac','oxygen','date')
y <- y[order(y$year, y$quarter),]
rownames(y)<-1:nrow(y)
y<-mutate(y,ID = as.numeric(rownames(y)))


ofull <- y %>% 
  ggplot(aes(x=date, y=oxygen, group=depth_fac, color=depth_fac, shape=as.factor(quarter))) +
  geom_point(na.rm=T) +
  geom_line(linetype='dashed') +
  labs(title = "Oxygen Concentration over Time", x = "Date", y = "Oxygen mL/L",color="Depth Range (m)", shape = "Quarter") +
  scale_shape_discrete(name="Quarter",
                       breaks=c("1", "2", "3","4"),
                       labels=c("Winter", "Spring", "Summer","Fall")) +
  scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2004-11-30")), date_labels = "%Y %b %d") +
  ylim(1,NA)
   
#scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2004-11-30")), date_labels = "%Y %b %d")
  
ofull

make_ts_plot <- function(date_min, date_max){
  
}
#make_ts_plot('1983-02-01', '2005-06-01')