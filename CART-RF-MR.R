# DROPBOX CODE
# install packages
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
install.packages("ROCR")
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(ggplot2)
library(bigrf)
library(reshape2)
library(scales)
library(pROC)
library(ROCR)
library(ROSE)

######################################################################################################
setwd("/Users/Sarah/Dropbox (SLS)/R-code")
setwd("X:/Projects/Mining/NIOSH/analysis/data/4_coded")
validatex = read.csv("coded_MR_validation_set.csv", header = T)
trainx = read.csv("coded_MR_training_set.csv", header = T)

# RANDOMLY SORT DATA (IT WAS ORDERED IN STATA BEFORE THIS)
set.seed(625)
rand <- runif(nrow(trainx))
train <- trainx[order(rand),]
set.seed(508)
rand2 <- runif(nrow(validatex))
validate <- validatex[order(rand2),]
#which( colnames(train)=="MR" )

######################################################################################################
# DEFINE RANDOM FOREST (TRUE PROPORTION OF 0'S AND 1'S)
set.seed(1)
rf <- randomForest(MR ~ ., data = train[1:600,], mtry = 11,
                   keep.inbag = TRUE, 
                   importance = TRUE, 
                   ntree = 400)
rf

# INSPECT RANKED VARIABLES AND ERROR RATE
plot(rf)
plot(margin(rf))
getTree(rf,1, labelVar=TRUE)

# STORE VARIABLE IMPORTANCE
round(importance(rf),2)
df.rf_imp <- data.frame(variable = names(rf$importance[,1]), importance = rf$importance[,1])
df.rf_imp <- df.rfImportance[ order(-df.rfImportance[,2]),]

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf.predictions = predict(rf, train[601:1000,],type="class")
table(train[601:1000,55], predicted = rf.predictions)

# PREDICT ON OUT OF BAG OBSERVATIONS 
rf.oob.predictions = predict(rf, train[1:600,],type="class")
table(train[1:600,55], predicted = rf.oob.predictions)

# PLOT VARIABLE IMPORTANCE
rf.imp.plot <- function(df.rf_imp, title='Variable importance for Random Forest model') {
  ggplot(df.rf_imp, aes(x=reorder(variable, importance), y=importance)) + 
    geom_bar(stat='identity', color=BLUE, fill=BLUE) +
    coord_flip() + 
    xlab('Variable') + 
    ylab('Importance') + 
    labs(title=title) +
    theme_enigma()
}
rf.imp.plot 

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=1)FOR RANDOM FOREST: JUST USING SAMPSIZE
# PREDICT ON REMAINING
rf.predictions = predict(rf.samp, train[601:1000,],type="class")
table(train[601:1000,55], predicted = rf.predictions)

# PREDICT ON OOB
rf.oob.predictions = predict(rf.samp, train[1:600,],type="class")
table(train[1:600,55], predicted = rf.oob.predictions)

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=1)FOR RANDOM FOREST: GENERATE BALANCED DATA W ROSE
train.rosex <- ROSE(MR ~ ., data=train)$data

# CHECK IMBALANCE AND SORT RANDOMLY (FOR SHITZNGIGGLES)
table(train.rosex$MR)
set.seed(836)
rand3 <- runif(nrow(train.rosex))
train.rose <- train.rosex[order(rand3),]

# DEFINE RF ON ROSE OVERSAMPLED DATA
set.seed(2)
rf.rose <- randomForest(MR ~ ., data = train.rose[1:600,], mtry = 11, ntree = 100)
rf.rose

# INSPECT RANKED VARIABLES AND ERROR RATE
plot(margin(rf.rose))
round(importance(rf.rose),2)

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf_rose_predictions = predict(rf.rose, train.rose[601:1000,],type="class")
table(train.rose[601:1000,55], predicted = rf_rose_predictions)

######################################################################################################
# DOWNSAMPLE NEGATIVE OUTCOMES (MR=0) FOR RANDOM FOREST
set.seed(3)
rfDownsampled <- randomForest(MR ~ ., train[1:600,], ntree = 100, mtry = 11, 
                              metric = "ROC", strata = train$MR, sampsize = rep(nmin,2))
rfDownsampled
rf_down_predictions = predict(rfDownsampled, train[601:1000,],type="class")
table(train[601:1000,55], predicted = rf_down_predictions)

######################################################################################################
# CREATE CART FUNCTION WITH RPART AND EXECUTE ON 1ST 600 OBSERVATIONS
cart <- rpart(MR ~ ., data = train[1:600,], method="class")
cart 

# PLOT RESULTS & DETAILED PLOT OF SPLITS
rpart.plot(cart, type=3, extra = 101, fallen.leaves=T)
printcp(cart) 
importance(cart)
summary(cart)

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
cart_predictions = predict(cart, train[601:1000,],type="class")
table(train[601:1000,51], predicted = cart_predictions)

# MAKE ROC PLOT FOR REGULAR CART 
fit.pr = predict(cart,newdata=train[601:1000,],type="prob")[,2]
fit.pred = prediction(fit.pr,train[601:1000,51])
fit.perf = performance(fit.pred,"tpr","fpr")
plot(fit.perf,lwd=2,col="blue",
     main="ROC:  Classification Trees on Training Data")
abline(a=0,b=1)

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=1)FOR RANDOM FOREST: GENERATE BALANCED DATA W ROSE
train.rosex <- ROSE(MR ~ ., data=train)$data

# CHECK IMBALANCE AND SORT RANDOMLY (FOR SHITZNGIGGLES)
table(train.rosex$MR)
set.seed(836)
rand3 <- runif(nrow(train.rosex))
train.rose <- train.rosex[order(rand3),]

# DO CART
cart.rose <- rpart(MR ~ ., data = train.rose[1:600,], method="class")
cart.rose 
rose.predictions = predict(cart.rose, train.rose[601:1000,],type="class")
table(train.rose[601:1000,51], predicted = rose.predictions)
rose.predictions = predict(cart.rose, train[1:1000,],type="class")
table(train.rose[1:1000,51], predicted = rose.predictions)

fit.pr = predict(cart.rose,newdata=train[1:1000,],type="prob")[,2]
fit.pred = prediction(fit.pr,train[1:1000,51])
fit.perf = performance(fit.pred,"tpr","fpr")
plot(fit.perf,lwd=2,col="blue",
     main="ROC:  Classification Trees on ROSE Data")
abline(a=0,b=1)

######################################################################################################

