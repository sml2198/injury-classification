# DROPBOX CODE
# install packages
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)
install.packages("ggplot2")
library(ggplot2)
install.packages("bigrf")
library(bigrf)
install.packages("reshape2")
library(reshape2)
install.packages("scales")
library(scales)
install.packages("pROC")
library(pROC)
install.packages("ROSE")
library(ROSE)
install.packages("ROSE")
library(ROSE)
?randomForest

######################################################################################################
setwd("X:/Projects/Mining/NIOSH/analysis/data/4_coded")
#setwd("/Users/Sarah/Dropbox (Stanford Law School)/R-code")
validatex = read.csv("coded_MR_validation_set.csv", header = T)
trainx = read.csv("coded_MR_training_set.csv", header = T)

# generate random number & reorder data
set.seed(625)
rand <- runif(nrow(trainx))
train <- trainx[order(rand),]
set.seed(508)
rand2 <- runif(nrow(validatex))
validate <- validatex[order(rand2),]

######################################################################################################
# define randomforest function on real data (true proportion of positive/negative outcomes)
set.seed(1)
rf <- randomForest(MR ~ ., data = train[1:600,], mtry = 11, 
                   keep.inbag = TRUE,  # mandatory,
                   importance = TRUE,  # recommended, else ordering by giniImpurity (unstable)
                   ntree = 400)
rf

# inspect ranked variables & error rate
plot(rf)
plot(margin(rf))
getTree(rf,1, labelVar=TRUE)
# store variable importance
round(importance(rf),2)
df.rf_imp <- data.frame(variable = names(rf$importance[,1]), importance = rf$importance[,1])
df.rf_imp <- df.rfImportance[ order(-df.rfImportance[,2]),]

# PREDICT ON REMAINING OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf_predictions = predict(rf, train[601:1000,],type="class")
table(train[601:1000,55], predicted = rf_predictions)
#NO  253   0
#YES 145   2

# PREDICT ON OUT OF BAG OBSERVATIONS & PLOT THE PREDICTIONS (TOP ROW) VS ACTUALS IN TABLE 
rf_oob_predictions = predict(rf, train[1:600,],type="class")
table(train[1:600,55], predicted = rf_oob_predictions)
#NO  382   0
#YES 195  23

# plot variable importance for rf
rf_imp_plot <- function(df.rf_imp, title='Variable importance for Random Forest model') {
  ggplot(df.rf_imp, aes(x=reorder(variable, importance), y=importance)) + 
    geom_bar(stat='identity', color=BLUE, fill=BLUE) +
    coord_flip() + 
    xlab('Variable') + 
    ylab('Importance') + 
    labs(title=title) +
    theme_enigma()
}
rf_imp_plot 

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=1)FOR RANDOM FOREST: generate new balanced data by ROSE
train.rosex <- ROSE(MR ~ ., data=train)$data

# check (im)balance of new data and sort randomly, for kicks
table(train.rosex$MR)
set.seed(836)
rand3 <- runif(nrow(train.rosex))
train.rose <- train.rosex[order(rand3),]

# define randomforest function on ROSE oversampled data
set.seed(2)
rf.rose <- randomForest(MR ~ ., data = train.rose[1:600,], mtry = 11, ntree = 100)
rf.rose

# inspect ranked variables & error rate
plot(margin(rf.rose))
# store variable importance
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
#NO  253   0
#YES 122   25

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
table(train[601:1000,55], predicted = cart_predictions)
#NO  230  23
#YES  28 119

######################################################################################################