install.packages("nycflights13")
library(nycflights13)
library(dplyr)
library(tidyverse)
flights<-flights
airlines<-airlines
airports<-airports
planes<-planes
weather<-weather






flights_airlines <- merge(flights, airlines, by=c("carrier")) 
flights_planes <- merge(flights, planes, by=c("tailnum")) 
flights_airports <- merge(flights, airports, by.x=c("dest"),by.y=c("faa"))

a <- merge(flights_weather, airlines, by=c("carrier"))
b <-  merge(a, planes, by=c("tailnum"))
all_data <- merge(b, airports, by.x=c("dest"),by.y=c("faa"))
####just checking some stuff####
unique(planes$engine)
unique(planes$engines)
unique(planes$seats)
unique(planes$type)
min(planes$year,na.rm = TRUE)
max(planes$year,na.rm = TRUE)

####weather####

flights_weather <- left_join(flights, weather, by=c("time_hour","origin")) 

#first check if there are a lot of delays that pass the threshold
delay_threshold <- 0.05 #ratio

#arrival delay
flights_weather <- flights_weather %>% mutate(
  is_arr_delay = case_when(
    as.numeric(arr_delay) > as.numeric(air_time)* delay_threshold ~ 1,
    as.numeric(arr_delay) < 0 & abs(as.numeric(arr_delay)) > as.numeric(air_time)* delay_threshold ~ -1,
    TRUE ~ 0
  ))
hist(flights_weather$is_arr_delay)
table(flights_weather$is_arr_delay)

#departure delay
flights_weather <- flights_weather %>% mutate(
  is_dep_delay = case_when(
    as.numeric(dep_delay) >0 ~ 1,
    as.numeric(dep_delay) < 0 ~-1,#& abs(as.numeric(dep_delay)) > 15 ~ -1,
    TRUE ~ 0
  ))
hist(flights_weather$is_dep_delay)
table(flights_weather$is_dep_delay)

ggplot(data = flights_weather)+
  geom_histogram(mapping = aes(dep_delay), binwidth = 10) 

#net delay
flights_weather <- flights_weather %>% mutate(
  net_delay = arr_delay - dep_delay 
    )
hist(flights_weather$net_delay)

#is net delay
flights_weather <- flights_weather %>% mutate(
  is_delay = case_when(
    as.numeric(net_delay) > 0 ~1,#as.numeric(air_time)* delay_threshold ~ 1,
    as.numeric(net_delay) < 0 ~-1,# & abs(as.numeric(net_delay)) > as.numeric(air_time)* delay_threshold ~ -1,
    TRUE ~ 0
  ))
hist(flights_weather$is_delay)
table(flights_weather$is_delay)

####stam####

#convert F degrees to C degrees
F.to.C <- function(F_temp){
  C_temp <- (F_temp-32)/1.8
  return(C_temp)
}
flights_weather$temp_c <- sapply(as.numeric(flights_weather$temp), F.to.C)

####temp####
#temp vs arrival delay
ggplot(data = flights_weather, mapping = aes(x = temp, y = abs(arr_delay))) + 
  geom_point(alpha = 0.2) +
  labs(x = "Temperature (F)", y = "Arrival delay (minutes)",
       title = "flights leaving NYC in 2013")

#temp vs arrival delay boxplot - cant see anything
ggplot(data = flights_weather, mapping = aes(x = factor(temp), y = abs(arr_delay) )) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~ is_arr_delay, ncol = 3)+
  labs(x = "Temperature in origin (F)", y = "Arrival delay (minutes)",
       title = "Arrival Delay (absolute)")

#temp vs departure delay
ggplot(data = flights_weather, mapping = aes(x = temp, y = abs(dep_delay))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ is_dep_delay, ncol = 3)+
  labs(x = "Temperature (F)", y = "Departure delay (minutes)",
       title = "temp vs departure delay")

temp.is_delay <- table(flights_weather$temp, flights_weather$is_delay)
temp.is_delay <- as.data.frame(temp.is_delay)
colnames(temp.is_delay) <- c("temp", "is_delay", "number")
ggplot(data = temp.is_delay, mapping = aes(x = temp, y = number))+#, fill = is_delay)) +
  geom_col() +
  facet_wrap(~ is_delay, ncol = 1) 
  

#temp vs net delay
ggplot(data = flights_weather, mapping = aes(x = temp, y = net_delay)) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ is_delay, ncol = 3)+
  labs(x = "Temperature (F)", y = "Net delay (minutes)",
       title = "temp vs net delay by type of delay (-1,0,1)")




#departure delay vs arrival delay
ggplot(data = flights_weather, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point(alpha = 0.2) +
  labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)",
       title = "departure delay vs arrival delay")
####wind####
#wind gust vs arrival delay
ggplot(data = flights_weather, mapping = aes(x = wind_gust, y = abs(arr_delay))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ is_delay, ncol = 3)+
  labs(x = "Wind gust (mph)", y = "Arrival delay (minutes)",
       title = "wind gust vs arrival delay")

#wind gust vs departure delay
ggplot(data = flights_weather, mapping = aes(x = wind_gust, y = abs(dep_delay))) + 
  geom_point(alpha = 0.2) +
  #facet_wrap(~ is_delay, ncol = 3)+
  labs(x = "Wind gust (mph)", y = "departure delay (minutes)",
       title = "wind gust vs departure delay")



#wind speed vs wind gust
ggplot(data = flights_weather, mapping = aes(x = wind_speed, y = wind_gust)) + 
  geom_point(alpha = 0.2) +
  labs(x = "wind_speed", y = "wind_gust",
       title = "wind speed vs wind gust")

#wind speed vs arrival delay
ggplot(data = flights_weather, mapping = aes(x = wind_speed, y = abs(arr_delay))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ is_delay, ncol = 3)+
  labs(x = "wind_speed", y = "Arrival delay (minutes)",
       title = "wind speed vs arrival delay")

#wind speed vs dep delay
ggplot(data = flights_weather, mapping = aes(x = wind_speed, y = abs(dep_delay))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ is_dep_delay, ncol = 3)+
  labs(x = "wind_speed", y = "departure delay (minutes)",
       title = "wind speed vs departure delay ")

#humidity vs dep delay
ggplot(data = flights_weather, mapping = aes(x = humid, y = dep_delay)) + 
  geom_point(alpha = 0.2) +
  #facet_wrap(~ is_dep_delay, ncol = 3)+
  labs(x = "humidity", y = "departure delay (minutes)",
       title = "humidity vs departure delay")

#wind speed vs dep delay boxplot 
ggplot(data = flights_weather, mapping = aes(x = factor(wind_speed), y = abs(dep_delay) )) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~ is_dep_delay, nrow = 1)+
  labs(x = "wind speed in origin (mph)", y = "dep delay (minutes)",
       title = "Dep Delay (absolute)")

#wind speed vs net delay
ggplot(data = flights_weather, mapping = aes(x = wind_speed, y = abs(net_delay))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ is_delay, ncol = 3)+
  labs(x = "wind_speed", y = "Net delay (minutes)",
       title = "wind speed vs net delay by type of delay (-1,0,1)")

#departure wind speed -  departure delay boxplot
ggplot(data = flights_weather, mapping = aes(x = factor(wind_speed), y = dep_delay)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "wind speed in origin", y = "departure delay",
       title = "wind dirction- departure delay") 


unique(flights_weather$wind_dir)
#departure wind direction -  arrival delay boxplot
ggplot(data = flights_weather, mapping = aes(x = factor(wind_dir), y = abs(arr_delay))) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "wind direction in origin", y = "arrival delay",
       title = "wind dirction- arrival delay") 

#departure wind direction -  arrival delay
ggplot(data = flights_weather, mapping = aes(x = wind_dir, y = abs(arr_delay))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ origin, ncol = 3)+
  labs(x = "wind direction in origin", y = "arrival delay",
       title = "departure wind direction -  arrival delay")

delay.wind_dir <- table(flights_weather$origin,flights_weather$is_delay,flights_weather$wind_dir)

delay.wind_dir<- as.data.frame(delay.wind_dir)
colnames(delay.wind_dir) <- c("origin", "delay","wind_dir","number")
ggplot(data = delay.wind_dir, mapping = aes(x = sort(wind_dir), y = number, fill = delay)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  labs(x = "wind direction", y = "Count delays",
       title = "wind direction- delay (net)") 

#departure wind direction -  departure delay
ggplot(data = flights_weather, mapping = aes(x = wind_dir, y = dep_delay)) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ origin, ncol = 3)+
  labs(x = "wind direction in origin", y = "departure delay",
       title = "departure wind direction -  departure delay")

####time####
#hour (time of day)- wind speed boxplot
ggplot(data = flights_weather, mapping = aes(x = factor(hour.y), y = wind_speed)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Hour", y = "Wind speed (mph)",
       title = "Hourly wind speed from NYC in 2013 by month") 

#hour (time of day)- wind speed
ggplot(data = flights_weather, mapping = aes(x = hour.x, y = wind_speed)) + 
  geom_point(alpha = 0.2) +
  labs(x = "hour", y = "Wind speed (mph)",
       title = "hour vs wind speed")

#hour vs net delay
ggplot(data = flights_weather, mapping = aes(x = hour.x, y = net_delay)) + 
  geom_point(alpha = 0.2) +
  #facet_wrap(~ is_delay, ncol = 3)+
  labs(x = "hour", y = "Net delay (minutes)",
       title = "hour vs net delay")

#hour vs departure delay
ggplot(data = flights_weather, mapping = aes(x = hour.x, y = abs(dep_delay))) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ is_dep_delay, ncol = 3)+
  labs(x = "hour", y = "Dep delay (minutes)",
       title = "hour vs dep delay")

#hour (time of day)- departure delay boxplot
ggplot(data = flights_weather, mapping = aes(x = factor(hour.y), y = dep_delay)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Hour", y = "departure delay (minutes)",
       title = "Hourly delays from NYC in 2013 by month")

hour.dep_delay <- table(flights_weather$origin, flights_weather$hour.x, flights_weather$is_dep_delay)
hour.dep_delay <- as.data.frame(hour.dep_delay)
colnames(hour.dep_delay) <- c( "origin", "hour","delay","number")

ggplot(data = hour.dep_delay, mapping = aes(x = hour, y = number, fill = delay)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  labs(x = "hour", y = "Count",
       title = "Delays per hour out of New York City in 2013") 

#month- temp
ggplot(data = flights_weather, mapping = aes(x = factor(month.y), y = temp)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Month", y = "Temperature (Hourly)",
       title = "Hourly temperatures from NYC in 2013 by month")  +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#month- wind speed
ggplot(data = flights_weather, mapping = aes(x = factor(month.y), y = wind_speed)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Month", y = "Wind speed (mph)",
       title = "Hourly wind speed from NYC in 2013 by month")  +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))





delay.origin <- table(flights_weather$is_delay, flights_weather$origin)
barplot(delay.origin, legend.text = TRUE)

delay.origin.month <- table(flights_weather$month.x, flights_weather$origin, flights_weather$is_dep_delay )


delay.origin.month <- as.data.frame(delay.origin.month)
colnames(delay.origin.month) <- c( "month", "origin","delay", "number")
ggplot(data = delay.origin.month, mapping = aes(x = month, y = number, fill = delay)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  labs(x = "month", y = "Count",
       title = "Delays per month out of New York City in 2013") 

numeric.delay.origin <- table(flights_weather)

weather1 <-weather %>% filter(origin == 'EWR' & month == 1)
plot(weather1$time_hour, weather1$wind_speed)

flights_weather1 <-flights_weather %>% filter(month.x == 1)
#####
ggplot(data = flights_weather1)+
  geom_histogram(mapping = aes(time_hour), binwidth = 30) +
  facet_wrap(~ origin, nrow = 1)+
  labs(x = "hour", y = "Number of scheduled flights",
       title = "Number of scheduled flights per hour")


ggplot(data = flights_weather, mapping = aes(x = time_hour, y = hour.x)) + 
  geom_point(alpha = 0.2) +
  #facet_wrap(~ is_delay, ncol = 3)+
  labs(x = "hour", y = "Net delay (minutes)",
       title = "hour vs net delay")