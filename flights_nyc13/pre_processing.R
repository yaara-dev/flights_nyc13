###20.08
ggplot(flights) +
  aes(x = "", y = dep_delay) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

boxplot.stats(flights$dep_delay)$out

#merged flights + weather + planes
combined_df<-merge(flights, planes, by="tailnum")
combined_df<-merge(combined_df,weather,by=c("origin","time_hour"))
identical(combined_df$hour.x, combined_df$hour.y)
identical(combined_df$month.x, combined_df$month.y)
identical(combined_df$day.x, combined_df$day.y)

#remove columns
combined_df <- select(combined_df, -c(time_hour, arr_delay))

#display all full ombined_df
str(combined_df)

# Make dependent variable as a factor (categorical)
combined_df$origin = as.factor(combined_df$origin)
combined_df$tailnum = as.factor(combined_df$tailnum)
combined_df$carrier = as.factor(combined_df$carrier)
combined_df$dest = as.factor(combined_df$dest)
combined_df$type = as.factor(combined_df$type)
combined_df$manufacturer = as.factor(combined_df$manufacturer)
combined_df$model = as.factor(combined_df$model)
combined_df$engine = as.factor(combined_df$engine)

#remove NA from dep_delay
combined_df<-combined_df%>% drop_na(dep_delay)