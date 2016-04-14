install.packages("rpart")
install.packages("rpart.plot")
install.packages("tree")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("bigrf")
install.packages("reshape2")
install.packages("scales")
install.packages("pROC")
install.packages("ROSE")
install.packages("dummies")
install.packages("adabag")
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(ggplot2)
#library(bigrf)
library(reshape2)
library(scales)
library(pROC)
library(ROSE)
library(dummies)
library(adabag)

setwd("X:/Projects/Mining/NIOSH/analysis/data/4_coded")
memory.limit(size = 4095)
ps_data = read.csv("coded_PS_training_set.csv")
ps_data[,"train"] = runif(nrow(ps_data))
ps_data = ps_data[order(ps_data[,"train"]),]

#DEFUNCT SINCE PRE-PROCESS IN STATA: This block dummies out categorical vars with > 53 categories for RF analysis and using ROSE.
#D = cbind(dummy(ps_data$sourceofinjury), dummy(ps_data$equipmentmodelno))
#omitted for size considerations: dummy(ps_data$operatorname), dummy(ps_data$controllername), dummy(ps_data$fipscountyname),  dummy(ps_data$occupation) , dummy(ps_data$controllerid),  dummy(ps_data$operatorid), dummy(ps_data$fipscountyname) 

#Deleting columns that may lead the algorithms to overfit (specifc equipment #s, operator names, ...) seems to improve accuracy.
#ps_data = ps_data[, c(-grep("equipmentmodelno", names(ps_data)), -grep("operatorid", names(ps_data)), -grep("operatorname", names(ps_data)), -grep("controllerid", names(ps_data)), -grep("controllername", names(ps_data)))]
#ps_data = merge(ps_data, data.frame(apply(D, MARGIN = 2, FUN = function(x) factor(x))))

ps_trainingSet = ps_data[1:800,]
ps_testSet = ps_data[801:1000,]
tabulate(ps_trainingSet$PS)
tabulate(ps_testSet$PS)

#CaRT

cart = rpart(PS ~ ., data = ps_trainingSet, method = "class")
table(ps_testSet$PS, predicted = predict(cart, ps_testSet, type = "class"))

#We need to find an oversampler that's more cognizant of the distributions of each predictor.
ps_trainingSet_rose = ROSE(PS ~ ., data = ps_trainingSet, seed = 123)$data
tabulate(ps_trainingSet_rose$PS)
cart_rose = rpart(PS ~ ., data = ps_trainingSet_rose, method = "class")
table(ps_testSet$PS, predicted = predict(cart_rose, ps_testSet, type = "class"))
#rpart.plot(cart, type = 1, extra = 1, fallen.leaves = T)

#AdaBoost.M1

ps_adaboost = boosting(PS ~ ., data = ps_trainingSet, boos = F, mfinal = 100, coeflearn = 'Freund')
adaboost.pred = predict.boosting(ps_adaboost, newdata = ps_testSet)
adaboost.pred$confusion

#Random Forest

ps_rf = randomForest(PS ~ ., data = ps_trainingSet, mtry = 11,
                  keep.inbag = TRUE, 
                  importance = TRUE, 
                  ntree = 400)
table(ps_testSet$PS, predicted = predict(ps_rf, ps_testSet, type = "class"))

#Kill everything:
#rm(list = ls())
