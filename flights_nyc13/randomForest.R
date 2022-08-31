install.packages('ranger')
install.packages('ROSE')
library(ranger)
library(ROSE)

balanced.data <-
  ovun.sample(
    dep_delay ~ .,
    flights_full_arranged,
    method = "both",
    p = 0.5,
    seed = 26
  )$data

table(balanced.data$dep_delay)
#plot flights counts per 2 dep_delay categories
ggplot(balanced.data, aes(dep_delay, fill = dep_delay)) + geom_bar(fill =
                                                                             c('#CC6666', '#FFCCCC')) +     #'#660000', '#993333', '#CC6666'"#FFCCCC"
  labs(title = "Flights counts per departure delay category", x = "Departure delay categories") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

flights_full_arranged <-
  flights_full_arranged %>% drop_na(air_time,
                                    year.y,
                                    temp,
                                    humid,
                                    wind_dir,
                                    wind_speed,
                                    pressure)

sample = sample.split(balanced.data$dep_delay, SplitRatio = .75)
train = subset(balanced.data, sample == TRUE)
test  = subset(balanced.data, sample == FALSE)
dim(train)
dim(test)

fit <- ranger(
  dep_delay ~ .,
  data = train,
  num.trees = 1000,
 # max.depth = 10,
 # importance = 'permutation',
  #scale.permutation.importance = TRUE,
  verbose = TRUE,
 # min.node.size = 10,
  #splitrule = "extratrees"
)
fit


pred <- predict(fit, test)#$predictions
confusion <- table(test$dep_delay, pred$predictions)
confusion
#todo - fix plot
plot(confusion,
     xlab = 'Predicted',
     ylab = 'True',
  main= sprintf('Predicted vs Obsvered
 OOB prediction error: %s',round(fit$prediction.error, 3))) 
predicted_observed_df <- as.data.frame(cbind(test$dep_delay, pred$predictions))

test %>%
  mutate(predicted = predict(fit, test)$predictions) %>%
  ggplot(aes(predicted, dep_delay)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") +  theme_bw(18)
imps <- data.frame(
  var = names(train)[-5],
  imps = fit$variable.importance / max(fit$variable.importance)
)
imps %>%
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18)
