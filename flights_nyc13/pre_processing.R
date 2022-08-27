#packages and libraries
list.of.packages <-
  c("dataPreparation",
    "nycflights13",
    "dplyr",
    "tidyverse",
    "RColorBrewer",
    "ggplot2")
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
#remove irrelevant columns
flights_full <-
  select(
    flights_full,
    -c(
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

#convert hours to minutes
flights_full <-
  flights_full %>% mutate(
    sched_dep_time = hour.x * 60 + minute,
    sched_arr_time = floor(sched_arr_time / 100) *
      60 + sched_arr_time %% 100
  )



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
boxplot.stats(flights$dep_delay)$out

#remove sd outliers
flights_full <-
  remove_sd_outlier(flights_full,
                    cols = "dep_delay",
                    n_sigmas = 7,
                    verbose = TRUE)


#display all flights_full
str(flights_full)

#divide the flights into 4 groups with equal sizes, according to their dep_delay
quantile(flights_full$dep_delay)
quantiles_vec <- c(0.25, 0.5, 0.75, 1)

#split dep_delay into 4 categories
num_categories_dep_delay <- 4
flights_full_arranged <- flights_full %>% arrange(dep_delay)
ind_vec <- as.numeric(rownames(flights_full_arranged))
split_ind <-
  split(ind_vec,
        cut(seq_along(ind_vec), num_categories_dep_delay, labels = FALSE))
for (i in 1:length(split_ind)) {
  flights_full_arranged$dep_delay[unlist(split_ind[i])] <-
    as.numeric(names(split_ind[i]))
}

#plot dep_delay categories counts
ggplot(flights_full_arranged, aes(dep_delay, fill = dep_delay)) + geom_bar(fill =
                                                                             c('#660000', '#993333', '#CC6666', "#FFCCCC")) +
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
  year.y_df$mean_delay / year.y_df$total_counts
ggplot(year.y_df, aes(x = year.y, y = normalized_dep)) + geom_bar(stat =
                                                                    "identity") #normalized mean dep_delay per year.y
#type & dep_delay
type_df <-
  flights_full %>% group_by(type) %>% summarise(total_counts = n(), mean_delay =
                                                  mean(dep_delay))
type_df$normalized_dep <- type_df$mean_delay / type_df$total_counts
ggplot(type_df, aes(x = type, y = normalized_dep)) + geom_bar(stat = "identity") #normalized mean dep_delay per type
#manufacturer & dep_delay
manufacturer_df <-
  flights_full %>% group_by(manufacturer) %>% summarise(total_counts = n(), mean_delay =
                                                          mean(dep_delay))
manufacturer_df$normalized_dep <-
  manufacturer_df$mean_delay / manufacturer_df$total_counts
ggplot(manufacturer_df, aes(x = manufacturer, y = normalized_dep)) + geom_bar(stat =
                                                                                "identity") #normalized mean dep_delay per type
#model & dep_delay
model_df <-
  flights_full %>% group_by(model) %>% summarise(total_counts = n(), mean_delay =
                                                   mean(dep_delay))
model_df$normalized_dep <-
  model_df$mean_delay / model_df$total_counts
ggplot(model_df, aes(x = model, y = normalized_dep)) + geom_bar(stat = "identity") #normalized mean dep_delay per type
#engines & dep_delay
engines_df <-
  flights_full %>% group_by(engines) %>% summarise(total_counts = n(), mean_delay =
                                                     mean(dep_delay))
engines_df$normalized_dep <-
  engines_df$mean_delay / engines_df$total_counts
ggplot(engines_df, aes(x = engines, y = normalized_dep)) + geom_bar(stat =
                                                                      "identity") #normalized mean dep_delay per type
#seats & dep_delay
seats_df <-
  flights_full %>% group_by(seats) %>% summarise(total_counts = n(), mean_delay =
                                                   mean(dep_delay))
seats_df$normalized_dep <-
  seats_df$mean_delay / seats_df$total_counts
ggplot(seats_df, aes(x = seats, y = normalized_dep)) + geom_bar(stat = "identity") #normalized mean dep_delay per type
#speed & dep_delay
speed_df <-
  flights_full %>% group_by(speed) %>% summarise(total_counts = n(), mean_delay =
                                                   mean(dep_delay))
speed_df$normalized_dep <-
  speed_df$mean_delay / speed_df$total_counts
ggplot(speed_df, aes(x = speed, y = normalized_dep)) + geom_bar(stat = "identity") #normalized mean dep_delay per type
#engine & dep_delay
engine_df <-
  flights_full %>% group_by(engine) %>% summarise(total_counts = n(), mean_delay =
                                                    mean(dep_delay))
engine_df$normalized_dep <-
  engine_df$mean_delay / engine_df$total_counts
ggplot(engine_df, aes(x = engine, y = normalized_dep)) + geom_bar(stat =
                                                                    "identity") #normalized mean dep_delay per type
