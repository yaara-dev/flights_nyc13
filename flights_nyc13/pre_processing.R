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
flights_full$hour.y<-as.numeric(flights_full$hour.y)

# Make dependent variable as a factor (categorical)
flights_full$origin = as.factor(flights_full$origin)
flights_full$tailnum = as.factor(flights_full$tailnum)
flights_full$carrier = as.factor(flights_full$carrier)
flights_full$dest = as.factor(flights_full$dest)
flights_full$type = as.factor(flights_full$type)
flights_full$manufacturer = as.factor(flights_full$manufacturer)
flights_full$model = as.factor(flights_full$model)
flights_full$engine = as.factor(flights_full$engine)

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




