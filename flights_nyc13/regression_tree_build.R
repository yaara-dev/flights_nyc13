#libraries
library(rpart)
#install.packages("rpart.plot") 
library(rpart.plot)
library(rattle)

#train-test split
#set.seed(42)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(flights_full), replace=TRUE, prob=c(0.7,0.3))
train  <- flights_full[sample, ]
test   <- flights_full[!sample, ]


model <- rpart(dep_delay ~ . , data=train, method = 'anova',control= rpart.control(cp=.0001))
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
predict(pruned_tree, newdata=flights_full_tst)


#####21.08
model_table<-table(flights_full$model)

