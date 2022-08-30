##weather features
#find variables that are similar
numeric_weather <- sapply(weather, is.numeric)
numeric_weather_data <- weather[numeric_weather]
numeric_weather_data <- select(numeric_weather_data,-c(year))
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
  var_df$normalized_dep <-
    var_df$mean_delay / var_df$total_counts
  
  jpeg(sprintf('%s vs normalized dep_delay.jpg', var))
  
  ggplot(var_df, aes(x = var_df[[1]], y = normalized_dep)) + geom_bar(stat =
                                                                        "identity") + labs(x = sprintf('%s', var))
  
  dev.off()#normalized mean dep_delay per type
}
for (var in colnames(flights_full)) {
  if (is.numeric(flights_full[, eval(as.name(var))])) {
    print(as.name(var))
    vari_df <- get_var_df(c(var))
    plot_var_vs_norm_dep_delay(vari_df)
    
  } else{
    print('nan')
  }
}


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


flights_full$sched_arr_time
plot(flights_full$sched_dep_time, flights_full$dep_delay)

#check sched_dep variable
sched_dep_df <-
  flights_full %>% group_by(sched_dep_time) %>% summarise(total_counts = n(), mean_delay =
                                                            mean(dep_delay))
sched_dep_df$normalized_dep <-
  sched_dep_df$mean_delay / sched_dep_df$total_counts
ggplot(sched_dep_df, aes(x = sched_dep_time, y = normalized_dep)) + geom_bar(stat =
                                                                               "identity") #normalized mean dep_delay per type

hist(flights_full$sched_dep_time)

######
short_thresh <- 10 #minutes
middle_thresh <- 50

flights_full_arranged <- flights_full %>% mutate(
  dep_delay_cat = case_when(
    as.numeric(dep_delay) < 0 ~ 0
    as.numeric(dep_delay) <= short_thresh ~ 1 ,
    as.numeric(dep_delay) > short_thresh &
      as.numeric(dep_delay) <= middle_thresh ~ 2,
    as.numeric(dep_delay) > middle_thresh ~ 3,
    is.na(dep_delay) ~ -1
  )
)

#####
flights_full_new <- flights_full_arranged[which(flights_full$dep_delay>-10),]
flights_full_new <-
  flights_full %>% mutate(
    new_dep_delay = case_when(
      dep_delay < 20 ~ 0,
      dep_delay >=20 ~1
    )
  )

delay <- flights_full_arranged[which(flights_full_arranged$dep_delay==1),]
no_delay <- flights_full_arranged[which(flights_full_arranged$dep_delay==0),]

delay_sample <- sample_n(delay, 50000)
no_delay_sample <- sample_n(no_delay, 50000)

all_sampled_delays <- rbind(delay_sample,no_delay_sample)
