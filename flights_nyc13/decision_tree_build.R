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

set.seed(4)
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
#delay <-
#   flights_full_arranged %>% filter(dep_delay==1)
# no_delay <-
#   flights_full_arranged %>% filter(dep_delay==0)
# 
# no_delay_sample <- sample_n(no_delay, nrow(delay))
# 
# all_sampled_delays <- rbind(delay, no_delay_sample)

#train-test split
#use 80% of dataset as training set and 20% as test set
sample_train <- sample(c(TRUE, FALSE), nrow(balanced.data), replace=TRUE, prob=c(0.8, 0.2))
train <- balanced.data[sample_train, ]
test <- balanced.data[!sample_train, ]

#stratified train test split
#train.index <- createDataPartition(flights_full_arranged$dep_delay, p = .8, list = FALSE)
#train_data <- flights_full_arranged[ train.index,]
#test_data  <- flights_full_arranged[-train.index,]

model <- rpart(dep_delay ~. -manu_model, data=train, method = 'class',control= rpart.control(cp=0.0005, maxdepth = 20, minsplit = 20))
new_model <- rpart(dep_delay ~. -manu_model, data=train, method = 'class',control= rpart.control(cp=0.0004, maxdepth = 28, minsplit = 19))

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
accuracy(test$dep_delay, predicted_values)

#view cp results and identify best cp to use
printcp(model)
plotcp(model)
ptree<- prune(model,
              cp= model$cptable[which.min(model$cptable[,"xerror"]),"CP"])


predicted_values_p<- predict(ptree, test, type = 'class')
accuracy<-accuracy(test$dep_delay, predicted_values_p)
accuracy(test$dep_delay, predicted_values_p)

#confusion matrix
confusion <- confusionMatrix(predicted_values, test$dep_delay)
confusionMatrix(predicted_values, test$dep_delay)

# plot confusion matrix
plot(confusion$table,
     xlab = 'Predicted',
     ylab = 'True',
     main= sprintf('Predicted vs Obsvered confusion matrix with\n accuracy score: %s', round(accuracy,3)))

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

getParamSet("classif.rpart")

train_df<-as.data.frame(train)
test_df<-as.data.frame(test)

model.mlr.train <- makeClassifTask(
  data=train_df, 
  target="dep_delay"
)

param_grid <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=seq(1,30, by=3)),
  makeDiscreteParam("cp", values=seq(0.0001, 0.001, by=0.0003)),
  makeDiscreteParam("minsplit", values=seq(1,30, by=3))
)

# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
# Define Measure
measure = acc

set.seed(1278)
dt_tuneparam <- tuneParams(learner="classif.rpart", 
                           task=model.mlr.train, 
                           resampling = resample,
                           measures = measure,
                           par.set=param_grid, 
                           control=control_grid, 
                           show.info = TRUE)

dt_tuneparam

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam, partial.dep = TRUE)

ggplot(
  data = result_hyperparam$data,
  aes(x = minsplit, y=acc.test.mean)
) + geom_line(color = 'darkblue')



best_parameters = setHyperPars(
  makeLearner("classif.rpart", predict.type = "prob"), 
  par.vals = dt_tuneparam$x
)

best_model = train(best_parameters, model.mlr.train)

model.mlr.test <- makeClassifTask(
  data=test_df, 
  target="dep_delay"
)

results <- predict(best_model, task = model.mlr.test)$data
accuracy(results$truth, results$response)


# Sampling just for visualization
result_sample <- result_hyperparam$data %>%
  sample_n(160)


hyperparam.plot <- plot_ly(result_sample, 
                           x = ~cp, 
                           y = ~maxdepth, 
                           z = ~minsplit,
                           marker = list(color = ~acc.test.mean,  colorscale = list(c(0, 1), c("darkred", "darkgreen")), showscale = TRUE))
hyperparam.plot <- hyperparam.plot %>% add_markers()
hyperparam.plot
