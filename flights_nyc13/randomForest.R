install.packages('ranger')
install.packages('ROSE')
library(ranger)
library(ROSE)
library(caret) 
  
get_balanced_df <- function(flights_full_arranged) {
  balanced.data <-
    ovun.sample(
      dep_delay ~ .,
      flights_full_arranged,
      method = "both",
      p = 0.5,
      seed = 1996
    )$data
  return(balanced.data)
}

#balance dataset
balanced_data <- get_balanced_df(flights_full_arranged)

#sensitivity check
#15
balanced_data_15 <- get_balanced_df(flights_full_arranged_15)

#25
balanced_data_25 <- get_balanced_df(flights_full_arranged_25)

table(balanced_data$dep_delay)
table(balanced_data_15$dep_delay)
table(balanced_data_25$dep_delay)

train_test_split <- function(balanced_data) {
  sample = sample.split(balanced_data$dep_delay, SplitRatio = .8)
  train = subset(balanced_data, sample == TRUE)
  test  = subset(balanced_data, sample == FALSE)
  train_test <- list(train, test)
  return(train_test)
}

#split to train and test
train <- train_test_split(balanced_data)[[1]]
test <- train_test_split(balanced_data)[[2]]
dim(train)
dim(test)


#sensitivity check
#15
train_15 <- train_test_split(balanced_data_15)[[1]]
test_15 <- train_test_split(balanced_data_15)[[2]]
dim(train_15)
dim(test_15)


#25
train_25 <- train_test_split(balanced_data_25)[[1]]
test_25 <- train_test_split(balanced_data_25)[[2]]
dim(train_25)
dim(test_25)

run_random <- function(train, ntrees=1000, depth = NULL) {
  fit <- ranger(
    dep_delay ~ .,
    data = train,
    num.trees = ntrees,
    importance = 'impurity',
    max.depth = depth,
    verbose = TRUE,
    
  )
  return(fit)
}

#run random forest
rf <- run_random(train, ntrees = 2000)
rf

##sensitivity
#15
rf_15 <- run_random(train_15)
rf_15

#25
rf_25 <- run_random(train_25)
rf_25


##prediction
pred <- predict(rf, test)
confusion <- table(test$dep_delay, pred$predictions)
confusion


confusionMatrix(confusion)

##sensitivity
#15
pred_15 <- predict(rf_15, test_15)
confusion_15 <- table(test_15$dep_delay, pred_15$predictions)
confusion_15
confusionMatrix(confusion_15)

#25
pred_25 <- predict(rf_25, test_25)
confusion_25 <- table(test_25$dep_delay, pred_25$predictions)
confusion_25
confusionMatrix(confusion_25)

par(mfrow = c(1, 3))
# plot confusion matrix
plot(
  confusion,
  xlab = 'Predicted',
  ylab = 'True',
  main = sprintf(
    'Predicted vs Obsvered
 OOB prediction error: %s',
    round(rf$prediction.error, 3)
  )
)

plot(
  confusion_15,
  xlab = 'Predicted',
  ylab = 'True',
  main = sprintf(
    'Predicted vs Obsvered, sensitivy check(15)
 OOB prediction error: %s',
    round(rf_15$prediction.error, 3)
  )
)

plot(
  confusion_25,
  xlab = 'Predicted',
  ylab = 'True',
  main = sprintf(
    'Predicted vs Obsvered, sensitivy check(25)
 OOB prediction error: %s',
    round(rf_15$prediction.error, 3)
  )
)


get_importance <- function(rf) {
  imps <- data.frame(
    var = names(train)[-5],
    imps = rf$variable.importance / max(rf$variable.importance)
  )
  return(imps)
}

imps <- get_importance(rf)
imps_15 <- get_importance(rf_15)
imps_25 <- get_importance(rf_25)

require(gridExtra)


p1 <- imps %>%
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  ggtitle('Importance') +
  theme_bw(18)
p1

p2 <- imps_15 %>%
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  ggtitle('Importance: sensitivity(15)') +
  theme_bw(18)


p3 <- imps_25 %>%
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  ggtitle('Importance: sensitivity(25)') +
  theme_bw(18)




grid.arrange(p1, p2, p3, ncol = 2)
