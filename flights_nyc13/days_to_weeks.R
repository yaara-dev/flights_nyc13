library(lubridate)
flights_full <- flights_full %>%
  mutate(w_day = wday(time_hour, label = TRUE))

flights_full <- flights_full %>% mutate(week_num = (year(time_hour) - year(min(time_hour))) * 52 +
                                          week(time_hour) - week(min(time_hour)))
#todo- change to ordered factor
week_date = floor_date(flights_full$time_hour, "week")


flights$week_num <-
  factor(flights_full$week ,
         levels = week_num,
         labels = week_date)
####another option####


make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_full <- flights_full %>%
  mutate(
    dep_time_date = make_datetime_100(year.x, month.x, day.x, dep_time),
    arr_time_date = make_datetime_100(year.x, month.x, day.x, arr_time),
    sched_dep_time_date = make_datetime_100(year.x, month.x, day.x, sched_dep_time),
    sched_arr_time_date = make_datetime_100(year.x, month.x, day.x, sched_arr_time)
  )

flights_full <- flights_full %>%
  mutate(weekn = floor_date(time_hour, "week"))



####some plots####
#number of flights per day all year
flights_full %>%
  ggplot(aes(time_hour)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day


#number of flights per week day
flights_full %>%
  ggplot(aes(x = w_day)) +
  geom_bar()

#number of flights per week
flights_full %>%
  count(week = floor_date(time_hour, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line()



#average delay time by dep_time minute
flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),
            n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()

#average delay time by sched_dep_time minute
sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),
            n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()
