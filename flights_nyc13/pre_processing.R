###20.08

#packages and libraries
#install.packages("nycflights13")
#install.packages("dataPreparation")
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
str(flights_full)



# Make dependent variable as a factor (categorical)
flights_full <- transform(
  flights_full,
  origin=as.factor(origin),
  tailnum=as.factor(tailnum),
  #month.x=as.factor(month.x),
  #day.x=as.factor(day.x),
  carrier=as.factor(carrier),
  flight= as.factor(flight),
  dest=as.factor(dest),
  type=as.factor(type),
  model=as.factor(model),
  engine=as.factor(engine),
  #month.y=as.factor(month.y),
  #day.y=as.factor(day.y),
  hour.y=as.numeric(hour.y),
  manufacturer=as.factor(manufacturer)
  #year.y= as.factor(year.y)
)

#identical columns
identical(flights_full$hour.x, flights_full$hour.y)
identical(flights_full$month.x, flights_full$month.y)
identical(flights_full$day.x, flights_full$day.y)

#before processing (drop na and outliers)
flights_full_old<-flights_full

#remove identical and constant columns
flights_full <- fast_filter_variables(
  flights_full,
  level = 2,
  keep_cols = NULL,
  verbose = TRUE
)

#remove irrelevant columns
flights_full<-select(flights_full, -c(time_hour, arr_delay, flight, tailnum, arr_time, dep_time))

different_columns<-colnames(flights_full_old)[!(colnames(flights_full_old) %in% colnames(flights_full))]

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

#remove model levels with few flights (remove the flights with  those model levels)
model_table<-table(flights_full$model)
ggplot(flights_full, aes(x = model, y=dep_delay)) + geom_bar(stat="identity")
quantile(model_table)
flights_full$seats<-as.factor(flights_full$seats)
str(flights_full)
ggplot(flights_full, aes(x = seats, y=dep_delay)) + geom_bar(stat="identity")

planes_new <- transform(
  planes,
  tailnum=as.factor(tailnum),
  year=as.factor(year),
  type=as.factor(type),
  manufacturer=as.factor(manufacturer),
  model=as.factor(model),
  engines=as.factor(engines),
  seats=as.factor(seats),
  speed=as.factor(speed),
  engine=as.factor(engine)
)

str(planes_new)
#year.y & dep_delay
year.y_df<-flights_full%>% group_by(year.y)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
year.y_df$normalized_dep<-year.y_df$mean_delay/year.y_df$total_counts
ggplot(year.y_df, aes(x = year.y, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per year.y
#type & dep_delay
type_df<-flights_full%>% group_by(type)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
type_df$normalized_dep<-type_df$mean_delay/type_df$total_counts
ggplot(type_df, aes(x = type, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per type
#manufacturer & dep_delay
manufacturer_df<-flights_full%>% group_by(manufacturer)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
manufacturer_df$normalized_dep<-manufacturer_df$mean_delay/manufacturer_df$total_counts
ggplot(manufacturer_df, aes(x = manufacturer, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per type
#model & dep_delay
model_df<-flights_full%>% group_by(model)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
model_df$normalized_dep<-model_df$mean_delay/model_df$total_counts
ggplot(model_df, aes(x = model, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per type
#engines & dep_delay
engines_df<-flights_full%>% group_by(engines)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
engines_df$normalized_dep<-engines_df$mean_delay/engines_df$total_counts
ggplot(engines_df, aes(x = engines, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per type
#seats & dep_delay
seats_df<-flights_full%>% group_by(seats)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
seats_df$normalized_dep<-seats_df$mean_delay/seats_df$total_counts
ggplot(seats_df, aes(x = seats, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per type
#speed & dep_delay
speed_df<-flights_full%>% group_by(speed)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
speed_df$normalized_dep<-speed_df$mean_delay/speed_df$total_counts
ggplot(speed_df, aes(x = speed, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per type
#engine & dep_delay
engine_df<-flights_full%>% group_by(engine)%>% summarise(total_counts=n(), mean_delay=mean(dep_delay))
engine_df$normalized_dep<-engine_df$mean_delay/engine_df$total_counts
ggplot(engine_df, aes(x = engine, y=normalized_dep)) + geom_bar(stat="identity") #normalized mean dep_delay per type

#transform levels in planes variables
flights_full_new<-flights_full
transform_levels_planes<-function(flights_full_df) {
  
  replace(flights_full_df$engine, flights_full_df$engine %in% c('4 Cycle','Turbo-prop'), 1)
  
  flights_full_df$engine[flights_full_df$engine=='4 Cycle'] <-1
  flights_full_df$engine[flights_full_df$engine=='Turbo-prop'] <-1
  
  
}
