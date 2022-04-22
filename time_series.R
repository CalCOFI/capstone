library(scales)
library(htmltools)
library(tidyverse)
library(lubridate)
library(ggplot2)
load("data/processed/bottle.RData")


ln <- bottle$line
stn <- bottle$station
yr <- bottle$year
dpt <- bottle$depth
tmp <- bottle$temperature
oxy <- bottle$oxygen
sal <- bottle$salinity


bot0<- subset(bottle, depth>=0 & depth <= 15)
dates<-as.data.frame(bot0$date)


tp <- ggplot(dates, aes(x=bot0$date)) +
  geom_line(aes(y=bot0$temperature), color = "darkred", na.rm = TRUE) + 
  xlab("Date")

tp+scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-01-30")),
                date_labels = "%Y %b %d")

op <- ggplot(dates, aes(x=bot0$date)) +
  geom_line(aes(y=bot0$oxygen), color = "blue", na.rm = TRUE) + 
  xlab("Date")

op+scale_x_date(limit=c(as.Date("2000-01-01"),as.Date("2000-07-01")),
                date_labels = "%Y %b %d")
