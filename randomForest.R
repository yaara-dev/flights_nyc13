
library(randomForest)
require(caTools)
summary(flights_weather)
sapply(flights_weather, class)
flights_weather <- transform(
  flights_weather,
  month.x=as.factor(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)
sapply(data, class)
set.seed(71)
rf <-randomForest(dep_delay~.,data=flights_weather, ntree=500) 
print(rf)