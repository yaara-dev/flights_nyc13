###20.08

#packages and libraries
#install.packages("nycflights13")
install.packages("dataPreparation")
library(dataPreparation)
library(nycflights13)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

flights <- flights
airlines <- airlines
airports <- airports
planes <- planes
weather <- weather


#merged flights + weather + planes
flights_planes<-merge(flights, planes, by="tailnum")
flights_full<-merge(flights_planes,weather,by=c("origin","time_hour"))
summary(flights_full)



# Make dependent variable as a factor (categorical)
flights_full <- transform(
  flights_full,
  origin=as.factor(origin),
  tailnum=as.factor(tailnum),
  month.x=as.factor(month.x),
  day.x=as.factor(day.x),
  carrier=as.factor(carrier),
  flight= as.factor(flight),
  dest=as.factor(dest),
  type=as.factor(type),
  model=as.factor(model),
  engine=as.factor(engine),
  month.y=as.factor(month.y),
  day.y=as.factor(day.y),
  hour.y=as.numeric(hour.y),
  manufacturer=as.factor(manufacturer)
)

#identical columns
identical(flights_full$hour.x, flights_full$hour.y)
identical(flights_full$month.x, flights_full$month.y)
identical(flights_full$day.x, flights_full$day.y)

#remove identical and constant columns
flights_full <- fast_filter_variables(
  flights_full,
  level = 2,
  keep_cols = NULL,
  verbose = TRUE
)

#remove NA from dep_delay
flights_full<-flights_full%>% drop_na(dep_delay)

#outliers
ggplot(flights) +
  aes(x = "", y = dep_delay) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

boxplot.stats(flights$dep_delay)$out

#remove sd outliers
flights_full <- remove_sd_outlier(flights_full, cols = "dep_delay", n_sigmas = 7, verbose = TRUE)

#display all flights_full
str(flights_full)




