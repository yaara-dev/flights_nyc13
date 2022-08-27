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


#merged flights + weather + planes + airports(tzone)
flights_planes <- merge(flights, planes, by = "tailnum")
flights_planes_weather <-
  merge(flights_planes, weather, by = c("origin", "time_hour"))
flights_full <-
  merge(flights_planes_weather,
        airports,
        by.x = c("dest"),
        by.y = c("faa"))

#before processing (drop na and outliers)
flights_full_old <- flights_full


summary(flights_full)

#suspected columns to have mostly NAs
length(which(is.na(flights_full$speed)))
length(which(is.na(flights_full$wind_gust)))

##weather features
#find variables that are similar
numeric_weather <- sapply(weather, is.numeric)
numeric_weather_data <- weather[numeric_weather]
numeric_weather_data <- select(numeric_weather_data, -c(year))
corr <-
  round(cor(numeric_weather_data, method = "spearman", use = "complete.obs"),
        2)
corr
heatmap(
  corr,
  Rowv = NA,
  Colv = NA,
  symm = TRUE,
  margins = c(6, 6),
)

#dewp and temp are highly correlated (0.9), and so are wind speed and wind gust (0.87)
#we already removed wind_gust because it was mostly NAs.
#will remove dewp as well to avoid double variables in the model

#### todo- fix function####
#plot dewp vs temp
plot(flights_full$dewp, flights_full$temp)
get_var_df <- function(var_name) {
  var_df <-
    flights_full %>% group_by(get(var_name)) %>% summarise(total_counts = n(),
                                                           mean_delay = mean(dep_delay))
  return(var_df)
}
plot_var_vs_norm_dep_delay <- function(var_df) {
  #jpeg(sprintf('%s vs normalized dep_delay.jpg', var))
  
  var_df$normalized_dep <-
    var_df$mean_delay / var_df$total_counts
  
  ggplot(var_df, aes(x = var_df[[1]], y = normalized_dep)) + geom_bar(stat =
                                                                        "identity") + labs(x = as.name(var))
  
  #dev.off()#normalized mean dep_delay per type
}
for (var in colnames(flights_full)) {
  if (is.numeric(flights_full[, eval(as.name(var))])) {
    print(as.name(var))
    var_df <- get_var_df(c(var))
    plot_var_vs_norm_dep_delay(var_df)
    
  }
}

numeric_data <- sapply(flights_full_old, is.numeric)
numeric_data <- flights_full_old[numeric_data]
sapply(as.vector(colnames(numeric_data)), get_var_df)
p_df1 <- get_var_df(c('pressure'))
plot_var_vs_norm_dep_delay(var_df)
#check dewp variable
dewp_df <-
  flights_full %>% group_by(dewp) %>% summarise(total_counts = n(), mean_delay =
                                                  mean(dep_delay))
dewp_df$normalized_dep <-
  dewp_df$mean_delay / dewp_df$total_counts
ggplot(dewp_df, aes(x = dewp, y = normalized_dep)) + geom_bar(stat =
                                                                "identity") #normalized mean dep_delay per type

hist(flights_full$dewp)

#check pressure variable
pressure_df <-
  flights_full %>% group_by(pressure) %>% summarise(total_counts = n(), mean_delay =
                                                      mean(dep_delay))
pressure_df$normalized_dep <-
  pressure_df$mean_delay / pressure_df$total_counts
ggplot(pressure_df, aes(x = pressure, y = normalized_dep)) + geom_bar(stat =
                                                                        "identity") #normalized mean dep_delay per type

hist(flights_full$pressure)

######
#remove irrelevant columns
flights_full <-
  select(
    flights_full,-c(
      time_hour,
      arr_delay,
      flight,
      tailnum,
      arr_time,
      dep_time,
      dest,
      name,
      lat,
      lon,
      alt,
      tz,
      dst,
      speed,
      wind_gust,
      dewp
    )
  )
## cyclic features
#flights_full$hour_sin = sin(flights_full$hour.x * (2 * pi / 24))
#flights_full$hour_cos = cos(flights_full$hour.x * (2 * pi / 24))
#flights_full$month_sin = sin((flights_full$month.x - 1) * (2 * pi / 12))
#flights_full$month_cos = cos((flights_full$month.x - 1) * (2 * pi / 12))

flights_full <-
  flights_full %>% mutate(sched_dep_time = hour.x * 60 + minute)
#todo - sched_arr_time


#remove NA from dep_delay
flights_full <- flights_full %>% drop_na(dep_delay)
flights_full <- flights_full %>% drop_na(tzone)



# Make dependent variable as a factor (categorical)
flights_full <- transform(
  flights_full,
  origin = as.factor(origin),
  #month.x = as.factor(month.x),
  #day.x = as.factor(day.x),
  carrier = as.factor(carrier),
  tzone = as.factor(tzone),
  type = as.factor(type),
  model = as.factor(model),
  engine = as.factor(engine),
  #month.y = as.factor(month.y),
  #day.y = as.factor(day.y),
  hour.y = as.numeric(hour.y),
  manufacturer = as.factor(manufacturer)
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


different_columns <-
  colnames(flights_full_old)[!(colnames(flights_full_old) %in% colnames(flights_full))]


#outliers
ggplot(flights) +
  aes(x = "", y = dep_delay) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

boxplot.stats(flights$dep_delay)$out

#remove sd outliers
flights_full <-
  remove_sd_outlier(flights_full,
                    cols = "dep_delay",
                    n_sigmas = 7,
                    verbose = TRUE)


#display all flights_full
str(flights_full)
