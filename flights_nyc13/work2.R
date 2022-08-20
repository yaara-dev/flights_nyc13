install.packages("nycflights13")
library(nycflights13)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(gplots)

flights <- flights
airlines <- airlines
airports <- airports
planes <- planes
weather <- weather

flights_weather <-
  left_join(flights, weather, by = c("time_hour", "origin"))




flights_weather1 <-
  flights_weather %>% filter(dep_delay >= 0) %>% mutate(sched_dep_time_numeric = hour.x + minute /
                                                          60)

#####
#correlation between numeric data
numeric_data <- sapply(flights_weather1, is.numeric)
numeric_flights_weather <-
  flights_weather1[, c(
    'sched_dep_time_numeric',
    'dep_delay',
    'arr_delay',
    'distance',
    'air_time',
    'temp',
    'wind_dir',
    "wind_speed",
    "precip",
    "pressure",
    "visib",
    "humid",
    "dewp"
  )]
correlation_mat <-
  cor(numeric_flights_weather, method = "spearman", use = "complete.obs")

heatmap(
  correlation_mat,
  Rowv = NA,
  Colv = NA,
  symm = TRUE,
  margins = c(6, 6),
)

heatmap.2(
  correlation_mat,
  Rowv = NA,
  Colv = NA,
  symm = TRUE,
  dendrogram = 'none',
  margins = c(8, 8),
  trace = 'none',
  keysize = 0
  
)

neg_delays <-
  flights %>% filter(!is.na(arr_delay),!is.na(dep_delay), dep_delay < 0)
cor(neg_delays$arr_delay, neg_delays$dep_delay)
pos_delays <-
  flights %>% filter(!is.na(arr_delay),!is.na(dep_delay), dep_delay > 0)
cor(pos_delays$arr_delay, pos_delays$dep_delay)

library(reshape2)

melted_corr_mat <- melt(round(correlation_mat, 2))
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
library(ggExtra)
ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2,
                                   fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(20, "Oranges")) +
  coord_fixed()+
  theme(
    plot.title = element_text(face = "bold.italic", hjust = 0.5),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ))+
  labs(title = "Correlation Matrix (spearman)")
  

ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(10, "RdYlGn") ) +
  coord_fixed()

#correlation between numeric data per origin airport
flights_weather_EWR <-
  flights_weather1[which(flights_weather1$origin == 'EWR'), ]
numeric_flights_weather_EWR <-
  flights_weather_EWR[, c(
    'sched_dep_time_numeric',
    'dep_delay',
    'arr_delay',
    'distance',
    'air_time',
    'temp',
    'wind_dir',
    "wind_speed",
    "precip",
    "pressure",
    "visib",
    "humid",
    "dewp"
  )]
correlation_mat_EWR <-
  cor(numeric_flights_weather_EWR,
      method = "pearson",
      use = "complete.obs")
heatmap(correlation_mat_EWR, Rowv = NA, Colv = NA)

flights_weather_LGA <-
  flights_weather1[which(flights_weather1$origin == 'LGA'), ]
flights_weather_JFK <-
  flights_weather1[which(flights_weather1$origin == 'JFK'), ]


####1506####
short_thresh <- 10 #minutes
middle_thresh <- 50

flights_weather <- flights_weather %>% mutate(
  dep_delay_cat = case_when(
    as.numeric(dep_delay) > 0 &
      as.numeric(dep_delay) <= short_thresh ~ 1 ,
    as.numeric(dep_delay) > short_thresh &
      as.numeric(dep_delay) <= middle_thresh ~ 2,
    as.numeric(dep_delay) > middle_thresh ~ 3,
    is.na(dep_delay) ~ -1,
    TRUE ~ 0
  )
)
flights_departured <- filter(flights_weather, dep_delay_cat != -1)
flights_delayed <- filter(flights_departured, dep_delay_cat != 0)
flights_delayed$dep_delay_cat <-
  as.factor(flights_delayed$dep_delay_cat)
cat_delay.info <- table(flights_weather$dep_delay_cat)

by_origin = group_by(flights_delayed, origin, dep_delay_cat)

origin_delay_count = summarize(by_origin,
                               count = n())
by_origin.visib = group_by(flights_delayed, origin, dep_delay_cat, visib)

sum_delay_count = summarize(by_origin.visib,
                            count = n())
sum_delay_count1 <-
  merge(sum_delay_count,
        origin_delay_count,
        by = c('origin', 'dep_delay_cat'))

sum_delay_count1$relative_delay <-
  sum_delay_count1$count.x / sum_delay_count1$count.y


by_visib.flights = group_by(flights_delayed, origin, visib,)

sum_visib.flights = summarize(by_visib.flights, count = n())

sum_delay_count2 <-
  merge(sum_delay_count, sum_visib.flights, by = c('origin', 'visib'))

sum_delay_count2$relative_delay2 <-
  sum_delay_count2$count.x / sum_delay_count2$count.y

visib.table <-
  as.data.frame(table(flights_delayed$origin, flights_delayed$visib))
colnames(visib.table) <- c('origin', 'visib', 'Freq')
sum_delay_count2 <-
  merge(sum_delay_count2, visib.table, by = c('origin', 'visib'))
#not good
ggplot(
  data = sum_delay_count1,
  mapping = aes(x = as.factor(visib), y = relative_delay, fill = dep_delay_cat)
) +
  geom_col(width = 0.3) +
  facet_wrap( ~ origin, ncol = 1) +
  labs(x = "Visibility",
       y = "Count [num of delay/ total num of delay]",
       title = "delay - visibility")

#better normalization i think:
p <- ggplot(
  data = sum_delay_count2,
  mapping = aes(
    x = as.factor(visib),
    y = relative_delay2,
    fill = dep_delay_cat,
  )
) +
  geom_col(mapping = aes(
    x = as.factor(visib),
    y = count.y / 100000,
    fill = NA,
    alpha = 0
  )) +
  labs(alpha = '') +
  scale_alpha(labels = 'Delayed flights') +
  geom_col(width = 0.7,
           position = "dodge") +
  facet_wrap( ~ origin, ncol = 1) +
  #  geom_text(
  #    aes(label = sprintf(
  #      "%0.2f", round(relative_delay2, digits = 2)
  #    )),
  #    position = position_dodge(width = 0.7),
  #    colour = "black",
  #    size = 2.5,
  #  ) +
  labs(x = "Visibility",
       y = "Count [num of delay/ total num of flights per visibility]",
       title = "Prevalence of delay category per visibility and origin") +
  theme(
    plot.title = element_text(face = "bold.italic", hjust = 0.5),
    axis.title.y = element_text(size = 8),
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ),
  ) +
  scale_fill_discrete(labels = c('Short', 'Middle', 'Long')) +
  labs(fill = 'Delay Category')
p





flights_delayed.grouped <-
  flights_delayed %>% group_by(origin, dep_delay_cat) %>%
  mutate(Wind_Mean = mean(wind_speed, na.rm = TRUE))
by_origin.wind = group_by(flights_delayed.grouped, origin, dep_delay_cat, Wind_Mean)
sum_origin.wind = summarize(by_origin.wind)

ggplot(
  data = sum_origin.wind,
  mapping = aes(x = dep_delay_cat,
                y = Wind_Mean,
                fill = dep_delay_cat,)
) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ origin, ncol = 1) +
  geom_text(
    aes(label = sprintf("%0.2f", round(Wind_Mean, digits = 2))),
    position = position_stack(vjust = 1.1),
    colour = "black",
    
    size = 3,
  ) +
  labs(x = "Delay category",
       y = "Average Wind Speed [mph]",
       title = "Average Wind Speed per delay category and origin") +
  theme(
    plot.title = element_text(face = "bold.italic", hjust = 0.5),
    axis.title.y = element_text(size = 8),
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 3
    ),
  )


flights_delayed$dep_delay_cat <-
  factor(
    flights_delayed$dep_delay_cat ,
    levels = c("1", "2", "3"),
    labels = c("Short", "Middle", "Long")
  )
flights_delayed_LGA <-
  flights_delayed[which(flights_delayed$origin == 'LGA'), ]

LGA <-
  ggplot(flights_delayed_LGA, aes(x = wind_dir, fill = as.factor(wind_speed))) +
  geom_histogram(binwidth = 15,
                 boundary = -7.5,
                 position = 'stack') +
  coord_polar(theta = "x", direction = 2) +
  facet_wrap( ~ dep_delay_cat, nrow = 1) +
  labs(x = "Wind Direction",
       title = "LGA Airport - wind direction and speed per delay category",
       fill = 'Wind Speed') +
  theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

LGA


flights_delayed_JFK <-
  flights_delayed[which(flights_delayed$origin == 'JFK'), ]

JFK <-
  ggplot(flights_delayed_JFK, aes(x = wind_dir, fill = as.factor(wind_speed))) +
  geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar(theta = "x", direction = 2) +
  facet_wrap( ~ dep_delay_cat, nrow = 1)
JFK

by_time_hour_airport = group_by(flights_delayed, origin, time_hour)
#Calculating the average time delay per airport per time_hour and
#also calculating the number of flights per airport per time_hour
sum_delay_count = summarize(by_time_hour_airport,
                            totaldelay = mean(dep_delay),
                            count = n())

combine_df = merge(sum_delay_count, weather, by = c("origin", "time_hour"))
combine_df_new = combine_df[-677,]
combine_df_new = combine_df_new[!is.na(combine_df_new$wind_speed),]
#Working on the combined df, grouping by wind_speed to see trends between
#delays and the weather variables
by_wind_speed = group_by(combine_df_new, wind_speed, wind_dir)
avg_delay_ws = summarize(by_wind_speed)
check <- merge(avg_delay_ws, flights_delayed_LGA, by = c(''))
p3 <-
  ggplot(avg_delay_ws, aes(x = wind_dir , y = wind_speed))
p3 + geom_point() + geom_smooth(method = "lm") + labs(x = "Wind Speed (mph)",
                                                      y = "Average Departure Delay Time (minutes)",
                                                      title = "Average Departure Delay vs. Wind Speed")
ggplot(by_wind_dir, aes(x = wind_dir, y = mean_delay, fill = mean_speed)) +
  geom_col(binwidth = 15,
           boundary = -7.5,
           position = 'stack') +
  coord_polar(theta = "x", direction = 2) +
  facet_wrap( ~ origin, nrow = 1) +
  labs(x = "Wind Direction [degrees]",
       y = 'Average Delay [minutes]',
       title = "Average Departure Delay and Wind Speed per Wind Direction and Origin",
       fill = 'Ave. Wind Speed [mph]') +
  theme(plot.title = element_text(face = "bold.italic", hjust = 0.5)) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1)

by_wind_dir <-
  flights_delayed %>% group_by(origin, wind_dir) %>% filter(wind_speed !=
                                                              0) %>% summarise(mean_speed = mean(wind_speed),
                                                                               mean_delay = mean(dep_delay))
ggplot(by_wind_dir, aes(x = wind_dir , y = mean)) + geom_point() + geom_smooth(method = "gam")
check <- flights_weather %>% filter(wind_speed != 0)
