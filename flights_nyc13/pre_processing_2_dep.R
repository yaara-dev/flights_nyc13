# dep delay 2 categories

#packages and libraries
list.of.packages <-
  c(
    "dataPreparation",
    "nycflights13",
    "dplyr",
    "tidyverse",
    "RColorBrewer",
    "ggplot2",
    "lubridate"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}


library(dataPreparation)
library(nycflights13)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(lubridate)

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


######

## cyclic features
#flights_full$hour_sin = sin(flights_full$hour.x * (2 * pi / 24))
#flights_full$hour_cos = cos(flights_full$hour.x * (2 * pi / 24))
#flights_full$month_sin = sin((flights_full$month.x - 1) * (2 * pi / 12))
#flights_full$month_cos = cos((flights_full$month.x - 1) * (2 * pi / 12))

#convert hours to minutes
flights_full <-
  flights_full %>% mutate(
    sched_dep_time = hour.x * 60 + minute,
    sched_arr_time = floor(sched_arr_time / 100) *
      60 + sched_arr_time %% 100
  )
#convert day to week days
flights_full <- flights_full %>%
  mutate(w_day = wday(time_hour, label = TRUE))

#convert days to weeks
flights_full <- flights_full %>% mutate(week_num = (year(time_hour) - year(min(time_hour))) * 52 +
                                          week(time_hour) - week(min(time_hour)))

# convert wind direction from degrees to 16 compass directions
directions <- read.csv('wind_directions.csv')

flights_full <- flights_full %>%
  mutate(wind_dir = cut(
    as.numeric(wind_dir),
    breaks = c(0, directions$degree_max, 360),
    labels = c(directions$cardinal, 'N')
  ))
#note: when the wind direction is 0 degrees the wd_cardinal is NA.
#Also, the wind speed is 0.
#This is correct because if the wind is not moving then it does not have a direction.


#remove NA from dep_delay
flights_full <- flights_full %>% drop_na(dep_delay)
flights_full <- flights_full %>% drop_na(tzone)


str(flights_full)


# Make dependent variable as a factor (categorical)
flights_full <- transform(
  flights_full,
  origin = as.factor(origin),
  carrier = as.factor(carrier),
  tzone = as.factor(tzone),
  type = as.factor(type),
  model = as.factor(model),
  engine = as.factor(engine),
  hour.y = as.numeric(hour.y),
  hour.x = as.numeric(hour.x),
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
      dewp,
      hour.x,
      minute
    )
  )
different_columns <-
  colnames(flights_full_old)[!(colnames(flights_full_old) %in% colnames(flights_full))]


#remove sd outliers
flights_full <-
  remove_sd_outlier(flights_full,
                    cols = "dep_delay",
                    n_sigmas = 7,
                    verbose = TRUE)


#display all flights_full
str(flights_full)

#divide the flights into 2 groups according to their dep_delay - flights above 20 min delay, and flights above -10 & until 20 min delay
flights_full_new_dep_delay <-
  flights_full[which(flights_full$dep_delay > -10),]
flights_full_arranged <-
  flights_full_new_dep_delay %>% arrange(dep_delay)

# plot histogram of original dep_delay before changing it into 2 categories with the threshold
ggplot(flights_full_arranged, aes(x = dep_delay)) +
  geom_histogram(color = "black", fill = "white", bins = 40) +
  geom_vline(aes(xintercept = 20, color = "delay > 20 min"),
             linetype = "dashed",
             size = 1.3) +
  scale_color_manual(name = "Tresholds delay time", values = c("delay > 20 min" = "red")) +
  labs(
    x = "Departure delay time [min]",
    y = "counts of flights",
    title = paste('Histogram of departure delay time')
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold"))


flights_full_arranged <-
  flights_full_arranged %>% mutate(dep_delay = case_when(dep_delay <= 20 ~ 0,
                                                         dep_delay > 20 ~ 1))

#convert dep_delay to factor column
flights_full_arranged$dep_delay <-
  as.factor(flights_full_arranged$dep_delay)

#plot flights counts per 2 dep_delay categories
ggplot(flights_full_arranged, aes(dep_delay, fill = dep_delay)) + geom_bar(fill =
                                                                             c('#CC6666', '#FFCCCC')) +     #'#660000', '#993333', '#CC6666'"#FFCCCC"
  labs(title = "Flights counts per departure delay category", x = "Departure delay categories") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# merge manufacturer and model columns into one column
manu_model <-
  paste(flights_full_arranged$manufacturer,
        flights_full_arranged$model,
        sep = "_")
flights_full_arranged$manu_model <- manu_model

levels_manu_model <-
  levels(factor(flights_full_arranged$manu_model))

#bar plot of manufacturer_model = "AGUSTA SPA_A109E" for example
manu_model_df <-
  flights_full_arranged %>% filter(manu_model == levels_manu_model[1]) %>% group_by(dep_delay) %>% summarise(total_counts =
                                                                                                               n())
manu_model_df$normalized_counts <-
  manu_model_df$total_counts / sum(manu_model_df$total_counts)
num_categories_dep_delay <- 2
threshold_uniform_hist <-
  sum(manu_model_df$normalized_counts) / num_categories_dep_delay
ggplot(manu_model_df, aes(x = dep_delay, y = normalized_counts)) +
  geom_bar(stat = "identity") + labs(
    x = "Departure delay categories",
    y = "normalized counts of flights",
    title = paste('manu_model =', levels_manu_model[1])
  ) + theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold")) +
  geom_hline(
    yintercept = threshold_uniform_hist,
    linetype = "dashed",
    color = "red",
    size = 1.5
  )
SSE <-
  sum((manu_model_df$normalized_counts - threshold_uniform_hist) ^
        2)
SSE
SSE_vec_manu_model <- sapply(levels_manu_model, function(a_model) {
  manu_model_df <-
    flights_full_arranged %>% filter(manu_model == a_model) %>% group_by(dep_delay) %>% summarise(total_counts =
                                                                                                    n())
  manu_model_df$normalized_counts <-
    manu_model_df$total_counts / sum(manu_model_df$total_counts)
  threshold_uniform_hist <-
    sum(manu_model_df$normalized_counts) / num_categories_dep_delay
  SSE <-
    sum((manu_model_df$normalized_counts - threshold_uniform_hist) ^ 2)
  SSE
})

hist(SSE_vec_manu_model, breaks = 20) #histogram of SSE models vector
# split 'manu_model' into 4 categories
manu_model_SSE_df <-
  data.frame(manu_model = names(SSE_vec_manu_model), SSE = SSE_vec_manu_model)
rownames(manu_model_SSE_df) <-
  1:length(manu_model_SSE_df$manu_model)
flights_counts_manu_model_df <-
  flights_full_arranged %>% group_by(manu_model) %>% summarise(total_counts =
                                                                 n())
manu_model_SSE_df <-
  merge(manu_model_SSE_df, flights_counts_manu_model_df, by = "manu_model") #add flights counts per manu_model to manu_model_SSE_df
manu_model_SSE_df <-
  manu_model_SSE_df %>% arrange(SSE_vec_manu_model) #arrange df in ascending order by SSE value per manu_model
# barplot SSE value per manufacturer
ggplot(manu_model_SSE_df, aes(x = reorder(manu_model, SSE) , y = SSE)) + geom_bar(stat =
                                                                                    "identity") + labs(title = "SSE value per manu_model", x = "manu_model", y =
                                                                                                         "SSE value") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 19, face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 6
    )
  )
ggplot(manu_model_SSE_df, aes(x = reorder(manu_model, SSE) , y = total_counts)) + geom_bar(stat =
                                                                                             "identity") + labs(title = "flights counts per manu_model", x = "manu_model", y =
                                                                                                                  "flights counts") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 19, face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 6
    )
  )

num_categories_manu_model <- 4
ind_vec_manu_model <- as.numeric(rownames(manu_model_SSE_df))
split_ind_manu_model <-
  split(ind_vec_manu_model,
        cut(
          seq_along(ind_vec_manu_model),
          num_categories_manu_model,
          labels = FALSE
        ))
manu_model_SSE_df$new_category <-
  rep(0, length(manu_model_SSE_df$manu_model))
for (i in 1:length(split_ind_manu_model)) {
  manu_model_SSE_df$new_category[unlist(split_ind_manu_model[i])] <-
    as.numeric(names(split_ind_manu_model[i]))
}
# barplot SSE value per model with categories thresholds
ggplot(manu_model_SSE_df, aes(x = reorder(manu_model, SSE) , y = SSE)) + geom_bar(stat =
                                                                                    "identity") + labs(title = "SSE value per manu_model", x = "manu_model", y =
                                                                                                         "SSE value") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 19, face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 6
    )
  ) +
  geom_vline(
    xintercept = c(
      split_ind_manu_model$`2`[1],
      split_ind_manu_model$`3`[1],
      split_ind_manu_model$`4`[1]
    ),
    linetype = "dashed",
    color = "red",
    size = 1.3
  )


#replace flights_full_arranged$manu_model to new model categories (1-4)
flights_full_arranged_old <- flights_full_arranged
flights_full_arranged$manu_model <-
  as.character(flights_full_arranged$manu_model)
for (j in 1:length(manu_model_SSE_df$manu_model)) {
  flights_full_arranged$manu_model <-
    replace(
      flights_full_arranged$manu_model,
      flights_full_arranged$manu_model == manu_model_SSE_df$manu_model[j],
      manu_model_SSE_df$new_category[j]
    )
}
flights_full_arranged$manu_model <-
  as.factor(flights_full_arranged$manu_model)

#plot number of flights per manu_model category
df_manu_model_counts <-
  flights_full_arranged %>% group_by(manu_model) %>% summarise(total_counts =
                                                                 n())
ggplot(df_manu_model_counts,
       aes(x = manu_model, y = total_counts, fill = manu_model)) + geom_bar(stat =
                                                                              "identity") + labs(title = "Flights counts per manu_model category", x =
                                                                                                   "manu_model category", y = "Total counts of flights") +
  theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold")) + scale_fill_manual(values =
                                                                                                c("#CC99FF", '#9966CC', '#663399', '#330066'))


# remove model column and manufacturer column from flights_full_arranged
flights_full_arranged <-
  select(flights_full_arranged,-c(model, manufacturer))


str(flights_full_arranged)

########################################################################################

# plot normalized delay per level in each variable

#year.y & dep_delay
year.y_df <-
  flights_full %>% group_by(year.y) %>% summarise(total_counts = n(), mean_delay =
                                                    mean(dep_delay))
year.y_df$normalized_dep <-
  year.y_df$mean_delay / sqrt(year.y_df$total_counts)
ggplot(year.y_df, aes(x = year.y, y = normalized_dep)) + geom_bar(stat =
                                                                    "identity") #normalized mean dep_delay per year.y
flights_full %>%
  ggplot(aes(x = year.y)) +
  geom_bar()

boxplot(dep_delay ~ year.y, data = flights_full)

#type & dep_delay
type_df <-
  flights_full %>% group_by(type) %>% summarise(total_counts = n(), mean_delay =
                                                  mean(dep_delay))
type_df$normalized_dep <-
  type_df$mean_delay / sqrt(type_df$total_counts)
ggplot(type_df, aes(x = type, y = normalized_dep)) + geom_bar(stat = "identity") #normalized mean dep_delay per type

flights_full %>%
  ggplot(aes(x = type)) +
  geom_bar()

boxplot(dep_delay ~ type, data = flights_full)

#manufacturer & dep_delay
manufacturer_df <-
  flights_full %>% group_by(manufacturer) %>% summarise(total_counts = n(), mean_delay =
                                                          mean(dep_delay))
manufacturer_df$normalized_dep <-
  manufacturer_df$mean_delay / sqrt(manufacturer_df$total_counts)
ggplot(manufacturer_df, aes(x = manufacturer, y = normalized_dep)) + geom_bar(stat =
                                                                                "identity") #normalized mean dep_delay per type
flights_full %>%
  ggplot(aes(x = manufacturer)) +
  geom_bar()

boxplot(dep_delay ~ manufacturer, data = flights_full)

#model & dep_delay
model_df <-
  flights_full %>% group_by(model) %>% summarise(total_counts = n(), mean_delay =
                                                   mean(dep_delay))
model_df$normalized_dep <-
  model_df$mean_delay / sqrt(model_df$total_counts)
ggplot(model_df, aes(x = model, y = normalized_dep)) + geom_bar(stat = "identity") #normalized mean dep_delay per type

flights_full %>%
  ggplot(aes(x = model)) +
  geom_bar()

boxplot(dep_delay ~ model, data = flights_full)

#engines & dep_delay
engines_df <-
  flights_full %>% group_by(engines) %>% summarise(total_counts = n(), mean_delay =
                                                     mean(dep_delay))
engines_df$normalized_dep <-
  engines_df$mean_delay / sqrt(engines_df$total_counts)
ggplot(engines_df, aes(x = engines, y = normalized_dep)) + geom_bar(stat =
                                                                      "identity") #normalized mean dep_delay per type
flights_full %>%
  ggplot(aes(x = engines)) +
  geom_bar()

boxplot(dep_delay ~ engines, data = flights_full)

#seats & dep_delay
seats_df <-
  flights_full %>% group_by(seats) %>% summarise(total_counts = n(), mean_delay =
                                                   mean(dep_delay))
seats_df$normalized_dep <-
  seats_df$mean_delay / sqrt(seats_df$total_counts)
ggplot(seats_df, aes(x = seats, y = normalized_dep)) + geom_bar(stat = "identity") #normalized mean dep_delay per type

flights_full %>%
  ggplot(aes(x = seats)) +
  geom_bar()

boxplot(dep_delay ~ seats, data = flights_full)

#engine & dep_delay
engine_df <-
  flights_full %>% group_by(engine) %>% summarise(total_counts = n(), mean_delay =
                                                    mean(dep_delay))
engine_df$normalized_dep <-
  engine_df$mean_delay / sqrt(engine_df$total_counts)
ggplot(engine_df, aes(x = engine, y = normalized_dep)) + geom_bar(stat =
                                                                    
                                                                    "identity") #normalized mean dep_delay per type
flights_full %>%
  ggplot(aes(x = engine)) +
  geom_bar()

boxplot(dep_delay ~ engine, data = flights_full)

#dest & dep_delay
dest_df <-
  flights_full %>% group_by(dest) %>% summarise(total_counts = n(), mean_delay =
                                                  mean(dep_delay))
dest_df$normalized_dep <-
  dest_df$mean_delay / dest_df$total_counts
ggplot(dest_df, aes(x = dest, y = normalized_dep)) + geom_bar(stat =
                                                                "identity") #normalized mean dep_delay per type

flights_full %>%
  ggplot(aes(x = dest)) +
  geom_bar()

boxplot(dep_delay ~ dest, data = flights_full)

#tzone & dep_delay
flights_full <-
  flights_full[-c(which(tzone == "America/Anchorage"))]#there are only 6 flights in this time zone

tzone_df <-
  flights_full %>% group_by(tzone) %>% summarise(total_counts = n(), mean_delay =
                                                   mean(dep_delay))
tzone_df$normalized_dep <-
  tzone_df$mean_delay / sqrt(tzone_df$total_counts)
ggplot(tzone_df, aes(x = tzone, y = normalized_dep)) + geom_bar(stat =
                                                                  "identity") #normalized mean dep_delay per type
flights_full %>%
  ggplot(aes(x = tzone)) +
  geom_bar()

boxplot(dep_delay ~ tzone, data = flights_full)



#week days and dep_delay
w_day_df <-
  flights_full %>% group_by(w_day) %>% summarise(total_counts = n(), mean_delay =
                                                   mean(dep_delay))
w_day_df$normalized_dep <-
  w_day_df$mean_delay / sqrt(w_day_df$total_counts)
ggplot(w_day_df, aes(x = w_day, y = normalized_dep)) + geom_bar(stat =
                                                                  "identity") #normalized mean dep_delay per type

flights_full %>%
  ggplot(aes(x = w_day)) +
  geom_bar()

boxplot(dep_delay ~ w_day, data = flights_full)


#wind direction and dep_delay
wd_cardinal_df <-
  flights_full %>% group_by(wd_cardinal) %>% summarise(total_counts = n(), mean_delay =
                                                         mean(dep_delay))
wd_cardinal_df$normalized_dep <-
  wd_cardinal_df$mean_delay / sqrt(wd_cardinal_df$total_counts)
ggplot(wd_cardinal_df, aes(x = wd_cardinal, y = normalized_dep)) + geom_bar(stat =
                                                                              "identity") #normalized mean dep_delay per type
flights_full %>%
  ggplot(aes(x = wd_cardinal)) +
  geom_bar()

boxplot(dep_delay ~ wd_cardinal, data = flights_full)

#month and dep_delay
flights_full$month.x <- as.factor(flights_full$month.x)
month_df <-
  flights_full %>% group_by(month.x) %>% summarise(total_counts = n(), mean_delay =
                                                         mean(dep_delay))
month_df$normalized_dep <-
  month_df$mean_delay / sqrt(month_df$total_counts)
ggplot(month_df, aes(x = month.x, y = normalized_dep)) + geom_bar(stat =
                                                                              "identity") #normalized mean dep_delay per type
flights_full %>%
  ggplot(aes(x = month.x)) +
  geom_bar()

boxplot(dep_delay ~ month.x, data = flights_full)

#week and dep_delay
flights_full$week <- as.factor(flights_full$week)
week_df <-
  flights_full %>% group_by(week) %>% summarise(total_counts = n(), mean_delay =
                                                     mean(dep_delay))
week_df$normalized_dep <-
  week_df$mean_delay / sqrt(week_df$total_counts)
ggplot(week_df, aes(x = week, y = normalized_dep)) + geom_bar(stat =
                                                                    "identity") #normalized mean dep_delay per type
flights_full %>%
  ggplot(aes(x = week)) +
  geom_bar()

boxplot(dep_delay ~ week, data = flights_full)
