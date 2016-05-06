# This code performs CaRT, random forest (rf) and boosting analysis on the "training" and "simple"
# datatsets produced in "prepare-MR.R". 
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tree")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("pROC")
install.packages("ROSE")
install.packages("adabag")
install.packages("DMwR")
install.packages("caret")
library(tree)
library(randomForest)
library(ggplot2)
library(reshape2)
library(pROC)
library(ROSE)
library(rpart)
library(rpart.plot)
library(adabag)
library(DMwR)
library(caret)

######################################################################################################
# home computer directory
# setwd("/Users/Sarah/Dropbox (SLS)/R-code")
# work computer directory
setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code")
rm(list = ls())

# LOAD IN DATA WITH 1023 OBSERVATIONS - ALL OF MR TRAINING SET PLUS FATALITIES OBSERVATIONS (FROM MSHA WEBSITE).
trainx = read.csv("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/prepped_MR_training_data.csv", header = T)
simplex = read.csv("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/prepped_MR_simple_data.csv", header = T)

# RANDOMLY SORT DATA (IT WAS ORDERED IN STATA BEFORE THIS)
set.seed(625)
rand <- runif(nrow(trainx))
train <- trainx[order(rand),]
rand2 <- runif(nrow(simplex))
simple <- simplex[order(rand2),]
remove(rand,rand2, simplex)
# just to find out which col # MR is
which( colnames(train)=="MR" )
which( colnames(simple)=="MR" )

######################################################################################################
# CREATE CART FUNCTION WITH RPART AND EXECUTE ON 1ST 800 OBSERVATIONS
cart <- rpart(MR ~ ., data = simple[1:700,], method="class")
cart 

# PREDICT ON REMAINING OBSERVATIONS 
cart.predictions = predict(cart, simple[701:1019,],type="class")
table(simple[701:1019,1], predicted = cart.predictions)

# PLOT RESULTS & DETAILED PLOT OF SPLITS
rpart.plot(cart, type=3, extra = 101, fallen.leaves=T)
printcp(cart) 

######################################################################################################
# DEFINE RANDOM FOREST (ON TRUE PROPORTION OF NO'S AND YES'S)
rf <- randomForest(MR ~ ., data = simple[1:700,], mtry = 8, importance=TRUE, type="class",
                   ntree = 200)
rf

# INSPECT RANKED VARIABLES AND ERROR RATE
plot(rf)
plot(margin(rf))
getTree(rf,1, labelVar=TRUE)

# STORE VARIABLE IMPORTANCE
round(importance(rf),2)
#df.rf_imp <- data.frame(variable = names(rf$importance[,1]), importance = rf$importance[,1])

# PREDICT ON OUT-OF-BAG (OOB) OBSERVATIONS 
rf.oob.predictions = predict(rf, simple[1:700,],type="class")
table(simple[1:700,1], predicted = rf.oob.predictions)

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf.predictions = predict(rf, simple[701:1019,],type="class")
table(simple[701:1019,1], predicted = rf.predictions)

######################################################################################################
# DOWNSAMPLE NEGATIVE OUTCOMES (MR=NO) FOR RANDOM FOREST
nmin = sum(simple$MR == "YES")
nmin

ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)

rf.downsampled = train(MR ~ ., data = simple[1:700,], method = "rf", ntree = 800,
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = simple$MR, sampsize = rep(nmin, 2))

rf.baseline = train(MR ~ ., data = simple[1:700,], method = "rf", ntree = 800,
                       tuneLength = 10, metric = "ROC", trControl = ctrl)

down.prob = predict(rf.downsampled, simple[701:1019,], type = "prob")[,1]
down.ROC = roc(response = simple[701:1019,1], predictor = down.prob, levels = rev(levels(simple[701:1019,1])))

base.prob = predict(rf.baseline, simple[701:1019,], type = "prob")[,1]
base.ROC = roc(response = simple[701:1019,1], predictor = base.prob, levels = rev(levels(simple[701:1019,1])))

plot(down.ROC, col = rgb(1, 0, 0, .5), lwd = 2)
plot(base.ROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)
legend(.4, .4, c("Down-Sampled", "Normal"), lwd = rep(2, 1), col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))
# sensitivity = true-positive rate
# specificity = false-positive rate

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE BALANCED DATA W ROSE
simple.rosex <- ROSE(MR ~ ., data=simple[1:700,])$data

# CHECK IMBALANCE AND SORT RANDOMLY (FOR SHITZNGIGGLES)
table(simple.rosex$MR)
rand3 <- runif(nrow(simple.rosex))
simple.rose <- simple.rosex[order(rand3),]
remove(simple.rosex)

# DEFINE RF ON ROSE OVERSAMPLED DATA
rf.rose <- randomForest(MR ~ ., data = simple.rose, mtry = 10, ntree = 1000)
rf.rose
rf.rose.pred = predict(rf.rose, simple[701:1019,],type="class")
table(simple[701:1019,1], predicted = rf.rose.pred)

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE BALANCED DATA W SMOTE
set.seed(1234)
prop.table(table(simple$MR))
# 0.626104 0.373896 

splitIndex = createDataPartition(simple$MR, p =.50, list = FALSE, times = 1)
smote.trainx = simple[splitIndex,]
smote.test = simple[-splitIndex,]
prop.table(table(smote.train$MR))
# 0.6254902 0.3745098 

# USE SMOTE TO OVERSAMPLE DATA
smote.train <- SMOTE(MR ~ ., smote.trainx, perc.over = 600,perc.under=100)
table(smote.train$MR)

# DEFINE RF ON SMOTE OVERSAMPLED DATA
rf.smote <- randomForest(MR ~ ., data = smote.train, mtry = 10, ntree = 1000)
rf.smote
rf.smote.pred = predict(rf.smote, smote.test, type="class")
table(smote.test$MR, predicted = rf.smote.pred)

######################################################################################################
# USE ADABOOST TO IMPLEMENT BOOSTING ALGORITHM 
mr.adaboost = boosting(MR ~ ., data = simple[1:700,], boos = T, mfinal = 100, coeflearn = 'Freund')
adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[701:1019,])
adaboost.pred$confusion

######################################################################################################
# PRINT ALL PREDICTIONS 

# SMOTE
table(smote.test$MR, predicted = rf.smote.pred)
# ROSE
table(simple[701:1019,1], predicted = rf.rose.pred)
# SIMPLE CART
table(simple[701:1019,1], predicted = cart.predictions)
# RF UNBALANCED 
table(simple[701:1019,1], predicted = rf.predictions)
# BOOSTING
adaboost.pred$confusion

simple$new.col <- ifelse(adaboost.pred$prob > 0.5, 1, 0)

######################################################################################################