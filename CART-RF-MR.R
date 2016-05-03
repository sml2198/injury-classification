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
library(tree)
library(randomForest)
library(ggplot2)
library(reshape2)
library(pROC)
library(ROSE)
library(rpart)
library(rpart.plot)
library(adabag)

######################################################################################################
# home computer directory
# setwd("/Users/Sarah/Dropbox (SLS)/R-code")
# work computer directory
setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code")

# LOAD IN DATA WITH 1023 OBSERVATIONS - ALL OF MR TRAINING SET PLUS FATALITIES OBSERVATIONS (FROM MSHA WEBSITE).
trainx = read.csv("prepped_MR_training_data.csv", header = T)
simplex = read.csv("prepped_MR_simple_data.csv", header = T)

# RANDOMLY SORT DATA (IT WAS ORDERED IN STATA BEFORE THIS)
set.seed(625)
rand <- runif(nrow(trainx))
train <- trainx[order(rand),]
rand2 <- runif(nrow(simplex))
simple <- simplex[order(rand2),]
# just to find out which col # MR is
which( colnames(train)=="MR" )
which( colnames(simple)=="MR" )

######################################################################################################
# CREATE CART FUNCTION WITH RPART AND EXECUTE ON 1ST 800 OBSERVATIONS
cart <- rpart(MR ~ ., data = simple[1:800,], method="class")
cart 

# PREDICT ON REMAINING OBSERVATIONS 
cart.predictions = predict(cart, simple[801:1023,],type="class")
table(simple[801:1019,1], predicted = cart.predictions)

# PLOT RESULTS & DETAILED PLOT OF SPLITS
rpart.plot(cart, type=3, extra = 101, fallen.leaves=T)
printcp(cart) 
#importance(cart)
#summary(cart)

######################################################################################################
# DEFINE RANDOM FOREST (ON TRUE PROPORTION OF NO'S AND YES'S)
rf <- randomForest(MR ~ ., data = simple[1:700,], mtry = 8, importance=TRUE, type="class",
                   ntree = 1000)
rf

# INSPECT RANKED VARIABLES AND ERROR RATE
plot(rf)
plot(margin(rf))
getTree(rf,1, labelVar=TRUE)

# STORE VARIABLE IMPORTANCE
round(importance(rf),2)
df.rf_imp <- data.frame(variable = names(rf$importance[,1]), importance = rf$importance[,1])
df.rf_imp <- df.rfImportance[ order(-df.rfImportance[,2]),]

# PREDICT ON OUT-OF-BAG (OOB) OBSERVATIONS 
rf.oob.predictions = predict(rf, simple[1:700,],type="class")
table(simple[1:700,1], predicted = rf.oob.predictions)

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf.predictions = predict(rf, simple[701:1023,],type="class")
table(simple[701:1019,1], predicted = rf.predictions)

#      NO YES
# NO  202  12
# YES  23  82

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

down.prob = predict(rf.downsampled, simple[701:1023,], type = "prob")[,1]
down.ROC = roc(response = simple[701:1019,1], predictor = down.prob, levels = rev(levels(simple[701:1019,1])))

base.prob = predict(rf.baseline, simple[701:1023,], type = "prob")[,1]
base.ROC = roc(response = simple[701:1019,1], predictor = base.prob, levels = rev(levels(simple[701:1019,1])))

plot(down.ROC, col = rgb(1, 0, 0, .5), lwd = 2)
plot(base.ROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)
legend(.4, .4, c("Down-Sampled", "Normal"), lwd = rep(2, 1), col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))
# sensitivity = true-positive rate
# specificity = false-positive rate
# not a huge improvement here as you can see...

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE BALANCED DATA W ROSE
simple.rosex <- ROSE(MR ~ ., data=simple)$data
# CHECK IMBALANCE AND SORT RANDOMLY (FOR SHITZNGIGGLES)
table(simple.rosex$MR)
rand3 <- runif(nrow(simple.rosex))
simple.rose <- simple.rosex[order(rand3),]

# DEFINE RF ON ROSE OVERSAMPLED DATA
rf.rose <- randomForest(MR ~ ., data = simple.rose[1:600,], mtry = 8, ntree = 600)
rf.rose

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf_rose_predictions = predict(rf.rose, simple.rose[601:1019,],type="class")
table(simple.rose[601:1019,1], predicted = rf_rose_predictions)

simple$MR.prediction = rf_rose_predictions

######################################################################################################
# USE ADABOOST TO IMPLEMENT BOOSTING ALGORITHM 
mr.adaboost = boosting(MR ~ ., data = simple[1:800,], boos = F, mfinal = 100, coeflearn = 'Freund')
adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[801:1019,])
adaboost.pred$confusion

# NO  137  12
# YES  12  58

######################################################################################################