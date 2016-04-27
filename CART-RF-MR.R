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
library(tree)
library(randomForest)
library(ggplot2)
library(reshape2)
library(pROC)
library(ROSE)
library(rpart)
library(rpart.plot)

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
simple <- simplex[order(rand),]
# just to find out which col # MR is
which( colnames(train)=="MR" )
which( colnames(simple)=="MR" )

######################################################################################################
# DEFINE RANDOM FOREST (TRUE PROPORTION OF 0'S AND 1'S)
rf <- randomForest(MR ~ ., data = simple[1:700,], mtry = 8, importance=TRUE, type="class",
                   ntree = 600)
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
rf.predictions = predict(rf, simple[701:1023,],type="class")
table(simple[701:1023,1], predicted = rf.predictions)

# PREDICT ON OUT OF BAG OBSERVATIONS 
rf.oob.predictions = predict(rf, simple[1:700,],type="class")
table(simple[1:700,1], predicted = rf.oob.predictions)

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=1)FOR RANDOM FOREST: GENERATE BALANCED DATA W ROSE
simple.rosex <- ROSE(MR ~ ., data=simple)$data
# CHECK IMBALANCE AND SORT RANDOMLY (FOR SHITZNGIGGLES)
table(simple.rosex$MR)
rand3 <- runif(nrow(simple.rosex))
simple.rose <- simple.rosex[order(rand3),]

# DEFINE RF ON ROSE OVERSAMPLED DATA
rf.rose <- randomForest(MR ~ ., data = simple.rose[1:200,], mtry = 8, ntree = 600)
rf.rose

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf_rose_predictions = predict(rf.rose, simple.rose[201:1023,],type="class")
table(simple.rose[201:1023,1], predicted = rf_rose_predictions)

plot(rf.rose)
plot(margin(rf.rose))
getTree(rf.rose,1, labelVar=TRUE)

######################################################################################################
# CREATE CART FUNCTION WITH RPART AND EXECUTE ON 1ST 800 OBSERVATIONS
cart <- rpart(MR ~ ., data = simple[1:800,], method="class")
cart 

cart.predictions = predict(cart, simple[801:1023,],type="class")
table(simple[801:1023,1], predicted = cart.predictions)

# PLOT RESULTS & DETAILED PLOT OF SPLITS
rpart.plot(cart, type=3, extra = 101, fallen.leaves=T)
printcp(cart) 
importance(cart)
summary(cart)

######################################################################################################