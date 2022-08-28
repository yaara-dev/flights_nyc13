library(randomForest)
require(caTools)
set.seed(101)
sample_df <- flights_full_arranged[sample(nrow(flights_full_arranged), 50000), ]
sample = sample.split(sample_df$dep_delay, SplitRatio = .75)
train = subset(sample_df, sample == TRUE)
test  = subset(sample_df, sample == FALSE)
dim(train)
dim(test)
rf <- randomForest(
  dep_delay ~.,
  data=train,
  na.action=na.roughfix,
  mtry = 8
)
rf
predTrain <- predict(rf, train, type = "class")

table(predTrain, train$dep_delay)  
# Predicting on Validation set
predValid <- predict(rf, test, type = "class")
# Checking classification accuracy
mean(predValid == train$dep_delay)                    
table(predValid,train$dep_delay)


unique(flights_full_arranged$dep_delay)
str(flights_full_arranged) 
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train,
                             y = train$dep_delay,
                             na.action=na.roughfix,
                             ntree = 500)

classifier_RF

data.imputed <- rfImpute(dep_delay ~ ., data = train, iter=6)
model <- randomForest(dep_delay ~ ., data=train, proximity=TRUE)




install.packages('ranger')
library(ranger)

flights_full_arranged <- flights_full_arranged %>% drop_na(air_time, year.y, temp, humid, wind_dir, wind_speed, pressure)

fit <- ranger(dep_delay ~ ., 
              data = train, 
              num.trees = 500,
              max.depth = 8,
              importance = 'impurity',
              verbose = TRUE)
fit
pred <- predict(fit, test)$predictions

test %>% 
  mutate(predicted = predict(fit, test)$predictions) %>% 
  ggplot(aes(predicted, dep_delay)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") +  theme_bw(18)
imps <- data.frame(var = names(train)[-7],
                   imps = fit$variable.importance/max(fit$variable.importance))
imps %>% 
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18)

## Split in training and test data
train.idx <- sample(nrow(flights_full_arranged), 2/3 * nrow(flights_full_arranged))
flight.train <- flights_full_arranged[train.idx, ]
flight.test <- flights_full_arranged[-train.idx, ]
## Run case-specific RF
csrf(dep_delay ~ ., training_data = flight.train, test_data = flight.test,
     params1 = list(num.trees = 50, mtry = 4),
     params2 = list(num.trees = 5))
