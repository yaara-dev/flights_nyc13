#libraries
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(rattle)
library(Metrics)
library(mlr)
library(plotly)
library(caret)
library(ROSE)

flights_full_arranged_origin<-flights_full_arranged
#flights_full_arranged<-flights_full_arranged_origin
flights_full_arranged<-flights_full_arranged %>% select(-c(manu_model))

set.seed(20)
#set.seed(25)
#set.seed(15)
#oversampling and undersampling
balanced.data <-
  ovun.sample(
    dep_delay ~ .,
    flights_full_arranged,
    method = "both",
    p = 0.5,
    seed = 26
  )$data

table(balanced.data$dep_delay)

# take equal number of observation from no delay and delay
# delay <-
#    flights_full_arranged %>% filter(dep_delay==1)
#  no_delay <-
#    flights_full_arranged %>% filter(dep_delay==0)
#  
#  no_delay_sample <- sample_n(no_delay, nrow(delay))
#  
#  all_sampled_delays <- rbind(delay, no_delay_sample)

#train-test split
#use 80% of dataset as training set and 20% as test set
sample_train <- sample(c(TRUE, FALSE), nrow(balanced.data), replace=TRUE, prob=c(0.8, 0.2))
train <- balanced.data[sample_train, ]
test <- balanced.data[!sample_train, ]

#stratified train test split
#train.index <- createDataPartition(flights_full_arranged$dep_delay, p = .8, list = FALSE)
#train_data <- flights_full_arranged[ train.index,]
#test_data  <- flights_full_arranged[-train.index,]

model <- rpart(dep_delay ~. , data=train, method = 'class',control= rpart.control(cp=0.00001, maxdepth = 26, minsplit = 18))
#new_model <- rpart(dep_delay ~. -manu_model, data=train, method = 'class',control= rpart.control(cp=0.0004, maxdepth = 28, minsplit = 19))
#new_model <- rpart(dep_delay ~. , data=train, method = 'class',control= rpart.control(cp=0.0004, maxdepth = 29, minsplit = 18))
model<-rpart(dep_delay~., data=train, method='class')

#view results
split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 15), collapse = "\n")
  }
  labs
}

rpart.plot(model, split.fun = split.fun)

#fancyRpartPlot(new_model, cex=0.05)
#prp(new_model, split.fun = split.fun)
#fancyRpartPlot(new_model, main="Classification plot", split.fun = split.fun)

#predict
predicted_values<- predict(model, test, type = 'class')
accuracy_num<-accuracy(test$dep_delay, predicted_values)
accuracy_num

#view cp results and identify best cp to use
printcp(model)
plotcp(model)
summary(model)
ptree<- prune(model,
              cp= model$cptable[which.min(model$cptable[,"xerror"]),"CP"])


predicted_values_p<- predict(ptree, test, type = 'class')
accuracy(test$dep_delay, predicted_values_p)

cor(predict(rtree.fit, newdata=testData),testData$medv)^2
#accuracy_num<-accuracy(test$dep_delay, predicted_values_p)

#confusion matrix
confusion <- confusionMatrix(predicted_values, test$dep_delay)
confusionMatrix(predicted_values, test$dep_delay)

# plot confusion matrix
plot(confusion$table,
     xlab = 'Predicted',
     ylab = 'True',
     main= sprintf('Predicted vs Obsvered confusion matrix with\n accuracy score: %s', round(accuracy_num,3)))

#find important variables
model$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Classication")+
  theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold"))

######################

#hyperparameter tuning using MLR

#getParamSet("classif.rpart")

train_df<-as.data.frame(train)
test_df<-as.data.frame(test)

model.mlr <- makeClassifTask(
  data=train_df, 
  target="dep_delay"
)

# Search Parameter for Max Depth
param_grid <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=seq(20,30, by=1)),
  makeDiscreteParam("cp", values=seq(0.0001, 0.0007, by=0.0001)),
  makeDiscreteParam("minsplit", values=seq(15,25, by=1))
)

# Define Grid
control_grid = makeTuneControlGrid()

# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)

# Define Measure
measure = acc

#set.seed(123) 
dt_tuneparam <- tuneParams(learner='classif.rpart', 
                           task=model.mlr, 
                           resampling = resample,
                           measures = measure,
                           par.set=param_grid, 
                           control=control_grid, 
                           show.info = TRUE)

dt_tuneparam
#maxdepth=26; cp=1e-04; minsplit=18

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam, partial.dep = TRUE)

ggplot(
  data = result_hyperparam$data,
  aes(x = cp, y=acc.test.mean)
) + geom_line(color = 'darkblue')


best_parameters_multi = setHyperPars(
  makeLearner("classif.rpart", predict.type = "prob"), 
  par.vals = dt_tuneparam$x
)


model_best_param = rpart(dep_delay~ .,   data=train, method = 'class', control = c(maxdepth = 26, cp=0.000, minsplit=18))
predict_new <- predict(model_best_param, test, type='class')                      
accuracy(test$dep_delay, predict_new)
result_hyperparam.multi <- generateHyperParsEffectData(dt_tuneparam, partial.dep = TRUE)

#result_sample_arranged <-  result_hyperparam.multi$data %>% arrange(desc(acc.test.mean))
full_sample_param<-result_hyperparam.multi$data
result_sample <- result_hyperparam.multi$data %>%
  sample_n(400)
hyperparam.plot <- plot_ly(result_sample, 
                           x = ~cp, 
                           y = ~maxdepth, 
                           z = ~minsplit,
                           marker = list(color = ~acc.test.mean,  colorscale = list(c(0, 1), c("darkred", "darkgreen")), showscale = TRUE))
hyperparam.plot <- hyperparam.plot %>% add_markers()
hyperparam.plot

