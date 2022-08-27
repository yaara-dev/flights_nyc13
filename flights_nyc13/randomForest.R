library(randomForest)
require(caTools)
set.seed(101)
sample = sample.split(flights_full$dep_delay, SplitRatio = .75)
train = subset(flights_full, sample == TRUE)
test  = subset(flights_full, sample == FALSE)
dim(train)
dim(test)
rf <- randomForest(
  dep_delay ~.,
  data=train,
  na.action=na.roughfix
)
rf
unique(flights_full$dep_delay)
str(flights_full) 
