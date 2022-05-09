library(scales)
library(htmltools)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(hrbrthemes)
load("data/processed/bottle.RData")


sunbot<- subset(bottle, depth>=0 & depth<=200)
twibot <- subset(bottle, depth>200 & depth<=1000)
midbot <- subset(bottle, depth>1000)

bottle <- bottle %>% mutate(depth_fac = cut(depth, c(-.01, 200, 1000, Inf)))

aggregate(temperature ~ depth_fac, bottle, mean)

bottle%>%group_by(depth_fac, year(date), quarter)

full <- bottle %>% 
  ggplot(aes(x=date, y =temperature, color=depth_fac)) +
  geom_point(na.rm = TRUE) +
  #geom_line(aes(x=date,y=mean(temperature), color = "steelblue"), size = 5, na.rm = TRUE) +
  xlab("Date") + 
  ylab("Temperature") +  
  scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-11-30")), date_labels = "%Y %b %d")  

full

#geom_line(aes(y=twibot$temperature),color = "steelblue", na.rm = TRUE)

# data_summary <- function(data, varname, groupnames){
#   require(plyr)
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm=TRUE),
#       sd = sd(x[[col]], na.rm=TRUE))
#   }
#   data_sum<-ddply(data, groupnames, .fun=summary_func,
#                   varname)
#   data_sum <- rename(data_sum, c("mean" = varname))
#   return(data_sum)
# }
# 
# df3 <- data_summary(sunbot, varname="temperature", 
#                     groupnames=c("depth", "date"))

#geom_line(aes(lineteype=depth),na.rm = TRUE) +
  #geom_point(aes(color=depth))
#geom_errorbar(aes(ymin=temperature-sd, ymax = temperature+sd, width = .1))

tps <- ggplot(sunbot, aes(date,temperature, group = depth)) +
  geom_line(na.rm = TRUE) +
  geom_point(aes(color=depth)) +
  xlab("Date") + 
  ylab("Temperature") +  
  scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-11-30")), date_labels = "%Y %b %d") 
  
tps

tpt <- ggplot(twibot, aes(date,temperature, group = depth)) +
  geom_line(na.rm = TRUE) +
  geom_point(aes(color=depth)) +
  xlab("Date") + 
  ylab("Temperature") +  
  scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-11-30")), date_labels = "%Y %b %d") 

tpt

#as.data.frame(aggregate(oxygen~date, data=sunbot, mean))$oxygen

# op <- ggplot(sunbot, aes(x=unique(date), y=as.data.frame(aggregate(oxygen~date, data=sunbot, mean))$oxygen)) +
#   geom_point(na.rm = TRUE)
#   #geom_line(aes(y=bot0$oxygen), color = "blue", na.rm = TRUE) + 
#   xlab("Date") +
#   scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-07-01")),
#                 date_labels = "%Y %b %d")

  
ops <- ggplot(sunbot, aes(date,oxygen, group = depth)) +
    geom_line(na.rm = TRUE) +
    geom_point(aes(color=depth)) +
    xlab("Date") + 
    ylab("Oxygen mL/L") +  
    scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-11-30")), date_labels = "%Y %b %d") 
  
ops

opt <- ggplot(twibot, aes(date,oxygen, group = depth)) +
  geom_line(na.rm = TRUE) +
  geom_point(aes(color=depth)) +
  xlab("Date") + 
  ylab("Oxygen mL/L") +  
  scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-11-30")), date_labels = "%Y %b %d") 

opt