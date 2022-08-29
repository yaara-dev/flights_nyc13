library(randomForest)
library(OneR)
library(rfviz)
library(tidyverse)
require(caTools)
flights_noNA <- na.omit(flights_full_arranged)
set.seed(101)
delay <-
  flights_noNA[which(flights_noNA$dep_delay == 1),]
no_delay <-
  flights_noNA[which(flights_noNA$dep_delay == 0),]

delay_sample <- delay
no_delay_sample <- sample_n(no_delay, dim(delay_sample)[1])

sample_df <- rbind(delay_sample, no_delay_sample)

sample_df <- sample_n(all_sampled_delays, 50000)

data_x <- sample_df[,-c('dep_delay')]
data_y <- sample_df$dep_delay
rfprep <- rf_prep(data_x, data_y)




set.seed(101)
delay <-
  flights_full_arranged[which(flights_full_arranged$dep_delay == 1),]
no_delay <-
  flights_full_arranged[which(flights_full_arranged$dep_delay == 0),]

delay_sample <- sample_n(delay, 50000)
no_delay_sample <- sample_n(no_delay, 50000)

all_sampled_delays <- rbind(delay_sample, no_delay_sample)

sample_df <- sample_n(all_sampled_delays, 50000)

sample = sample.split(sample_df$dep_delay, SplitRatio = .70)
train = subset(sample_df, sample == TRUE)
test  = subset(sample_df, sample == FALSE)
dim(train)
dim(test)
rf <- randomForest(
  dep_delay ~ .,
  data = train,
  #na.action = na.roughfix,
  mtry = 10,
  ntree = 500
)
rf
# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

##
rf.fit <- randomForest(
  dep_delay ~ .,
  data = train,
  ntree = 50,
  keep.forest = FALSE,
  importance = TRUE,
  na.action = na.roughfix
)

rf.fit


predTrain <- predict(rf, train, type = "class")

table(predTrain, train$dep_delay)
# Predicting on Validation set
predValid <- predict(rf, test, type = "class")
# Checking classification accuracy
mean(predValid == test$dep_delay)
table(predValid, test$dep_delay)


unique(flights_full_arranged$dep_delay)
str(flights_full_arranged)
set.seed(120)  # Setting seed
classifier_RF = randomForest(
  x = train,
  y = train$dep_delay,
  na.action = na.roughfix,
  ntree = 500
)

classifier_RF

data.imputed <- rfImpute(dep_delay ~ ., data = train, iter = 6)
model <-
  randomForest(dep_delay ~ ., data = data.imputed, proximity = TRUE)




install.packages('ranger')
library(ranger)

flights_full_arranged <-
  flights_full_arranged %>% drop_na(air_time, year.y, temp, humid, wind_dir, wind_speed, pressure)

fit <- ranger(
  dep_delay ~ .,
  data = train,
  num.trees = 500,
  max.depth = 8,
  importance = 'impurity',
  verbose = TRUE
)
fit
pred <- predict(fit, test)$predictions

test %>%
  mutate(predicted = predict(fit, test)$predictions) %>%
  ggplot(aes(predicted, dep_delay)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") +  theme_bw(18)
imps <- data.frame(
  var = names(train)[-7],
  imps = fit$variable.importance / max(fit$variable.importance)
)
imps %>%
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18)

## Split in training and test data
train.idx <-
  sample(nrow(flights_full_arranged),
         2 / 3 * nrow(flights_full_arranged))
flight.train <- flights_full_arranged[train.idx, ]
flight.test <- flights_full_arranged[-train.idx, ]
## Run case-specific RF
csrf(
  dep_delay ~ .,
  training_data = flight.train,
  test_data = flight.test,
  params1 = list(num.trees = 50, mtry = 4),
  params2 = list(num.trees = 5)
)
