#libraries
library(rpart)
#install.packages("rpart.plot") 
library(rpart.plot)
library(rattle)
library(Metrics)
library(mlr)

delay <-
  flights_full_arranged[which(flights_full_arranged$dep_delay == 1), ]
no_delay <-
  flights_full_arranged[which(flights_full_arranged$dep_delay == 0), ]

no_delay_sample <- sample_n(no_delay, nrow(delay))

all_sampled_delays <- rbind(delay, no_delay_sample)

#train-test split
set.seed(52)
#use 70% of dataset as training set and 30% as test set
sample_train <- sample(c(TRUE, FALSE), nrow(all_sampled_delays), replace=TRUE, prob=c(0.7,0.3))
train  <- all_sampled_delays[sample_train, ]
test   <- all_sampled_delays[!sample_train, ]


model <- rpart(dep_delay ~ . , data=train, method = 'class',control= rpart.control(cp=.0005))
#view results
rpart.plot(model)
prp(model,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

pdf("tree.pdf")
fancyRpartPlot(model)
dev.off()
system("evince tree.pdf")

#view results
printcp(model)  

#identify best cp value to use
best <- model$cptable[which.min(model$cptable[,"xerror"]),"CP"]
#produce a pruned tree based on the best cp value
pruned_tree <- prune(model, cp=best)

#plot the pruned tree
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

rpart.plot(pruned_tree)
pdf("prunned_tree.pdf")
fancyRpartPlot(pruned_tree)
dev.off()
system("evince tree.pdf")


#use pruned tree to predict salary of this player
predicted_values<-predict(pruned_tree, newdata=test, type= 'class')
predicted_values<- predict(model, test, type = 'class')
accuracy(test$dep_delay, predicted_values)

#hyperparameter tuning using MLR
getParamSet("classif.rpart")
