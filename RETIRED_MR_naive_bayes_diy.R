# NIOSH Project 2014-N-15776

# RETIRED - MR Naive Bayes
    # THIS CODE IS RETIRED.

# Last edit 7/19/16

# This was going was used to experiment with anive bayes for classifying MR injuries, but was no found
# to perform better than other ML algorithms, and so the code has been retired. 

######################################################################################################

library(pastecs)
options(scipen=100)
options(digits=2)
mr_dataset = read.csv("X:/Projects/Mining/NIOSH/analysis/data/4_coded/coded_MR_validation_set.csv")
#Creating Indicators for various factor variables
equipment_names = levels(mr_dataset$typeofequipment)
n_equipm = length(equipment_names)
for (i in 1:n_equipm){
  mr_dataset[, paste("equipm_", i, sep="")] = ifelse(mr_dataset$typeofequipment == equipment_names[i], 1, 0)
}
accident_names = levels(mr_dataset$accidenttype)
n_acci = length(accident_names)
for (i in 1:length(accident_names)){
  mr_dataset[, paste("acci_", i, sep="")] = ifelse(mr_dataset$accidenttype == accident_names[i], 1, 0)
}
bodypart_names = levels(mr_dataset$bodypart)
n_bodyp = length(bodypart_names)
for (i in 1:length(bodypart_names)){
  mr_dataset[, paste("bodyp_", i, sep="")] = ifelse(mr_dataset$bodypart == bodypart_names[i], 1, 0)
}
mr_dataset[,"train"] = ifelse(runif(nrow(mr_dataset)) < 0.80, 1, 0)
trainingSet = mr_dataset[mr_dataset$train==1, c(grep("equipm_", names(mr_dataset)), grep("acci_", names(mr_dataset)), grep("MR", names(mr_dataset)))]
testSet = mr_dataset[mr_dataset$train==0, c(grep("equipm_", names(mr_dataset)), grep("acci_", names(mr_dataset)), grep("MR", names(mr_dataset)))]
k = sum(n_equipm, n_acci)
y_counts = tabulate(trainingSet$MR)
N = nrow(trainingSet)
prop_n = y_counts[1]/N
prop_p = y_counts[2]/N
x_prop_p = as.numeric(colMeans(trainingSet[trainingSet$MR=="YES", c(grep("equipm_", names(trainingSet)), grep("acci_", names(trainingSet)))]))
x_prop_n = as.numeric(colMeans(trainingSet[trainingSet$MR=="NO", c(grep("equipm_", names(trainingSet)), grep("acci_", names(trainingSet)))]))

#Within-Training Set Probabilities
prob_p_cond_x_mat = matrix(nrow = nrow(trainingSet), ncol = k)
prob_n_cond_x_mat = matrix(nrow = nrow(trainingSet), ncol = k)
j = 0
for (i in 1:k) {
  if (i > n_equipm) {
    j = j + 1
    prob_p_cond_x_mat[, i] = x_prop_p[i]*trainingSet[, match(paste("acci_", j, sep=""), names(trainingSet))] + (1-x_prop_p[i])*(1-trainingSet[,match(paste("acci_", j, sep=""), names(trainingSet))])
    prob_n_cond_x_mat[, i] = x_prop_n[i]*trainingSet[, match(paste("acci_", j, sep=""), names(trainingSet))] + (1-x_prop_n[i])*(1-trainingSet[,match(paste("acci_", j, sep=""), names(trainingSet))])
  } else {
    prob_p_cond_x_mat[, i] = x_prop_p[i]*trainingSet[, match(paste("equipm_", i, sep=""), names(trainingSet))] + (1-x_prop_p[i])*(1-trainingSet[,match(paste("equipm_", i, sep=""), names(trainingSet))])
    prob_n_cond_x_mat[, i] = x_prop_n[i]*trainingSet[, match(paste("equipm_", i, sep=""), names(trainingSet))] + (1-x_prop_n[i])*(1-trainingSet[,match(paste("equipm_", i, sep=""), names(trainingSet))])
  }
}
prob_p_cond_x_vec = apply(prob_p_cond_x_mat, 1, function(x) prod(x))
prob_n_cond_x_vec = apply(prob_n_cond_x_mat, 1, function(x) prod(x))

prob_p_cond_x = prop_p*prob_p_cond_x_vec
prob_n_cond_x = prop_n*prob_n_cond_x_vec
pred_p = prob_p_cond_x/(prob_p_cond_x + prob_n_cond_x)

testSet[, "pred_prob_p"] = ifelse(prob_p_cond_x > 0 & prob_n_cond_x > 0, pred_p, 0)
n_pred_p = sum(trainingSet$pred_prob_p >= 0.5 & trainingSet$MR == "YES")
n_pred_n = sum(trainingSet$pred_prob_p < 0.5 & trainingSet$MR == "NO")
accry_rate = (n_pred_p + n_pred_n) / N

#
#
#

#Test Set Probabilities
prob_p_cond_x_mat = matrix(nrow = nrow(testSet), ncol = k)
prob_n_cond_x_mat = matrix(nrow = nrow(testSet), ncol = k)
j = 0
for (i in 1:k) {
  if (i > n_equipm) {
    j = j + 1
    prob_p_cond_x_mat[, i] = x_prop_p[i]*testSet[, match(paste("acci_", j, sep=""), names(testSet))] + (1-x_prop_p[i])*(1-testSet[,match(paste("acci_", j, sep=""), names(testSet))])
    prob_n_cond_x_mat[, i] = x_prop_n[i]*testSet[, match(paste("acci_", j, sep=""), names(testSet))] + (1-x_prop_n[i])*(1-testSet[,match(paste("acci_", j, sep=""), names(testSet))])
  } else {
    prob_p_cond_x_mat[, i] = x_prop_p[i]*testSet[, match(paste("equipm_", i, sep=""), names(testSet))] + (1-x_prop_p[i])*(1-testSet[,match(paste("equipm_", i, sep=""), names(testSet))])
    prob_n_cond_x_mat[, i] = x_prop_n[i]*testSet[, match(paste("equipm_", i, sep=""), names(testSet))] + (1-x_prop_n[i])*(1-testSet[,match(paste("equipm_", i, sep=""), names(testSet))])
  }
}
prob_p_cond_x_vec = apply(prob_p_cond_x_mat, 1, function(x) prod(x))
prob_n_cond_x_vec = apply(prob_n_cond_x_mat, 1, function(x) prod(x))

prob_p_cond_x = prop_p*prob_p_cond_x_vec
prob_n_cond_x = prop_n*prob_n_cond_x_vec
pred_p = prob_p_cond_x/(prob_p_cond_x + prob_n_cond_x)

testSet[, "pred_prob_p"] = ifelse(prob_p_cond_x > 0 & prob_n_cond_x > 0, pred_p, 0)
n_pred_p = sum(testSet$pred_prob_p >= 0.5 & testSet$MR == "YES")
n_pred_n = sum(testSet$pred_prob_p < 0.5 & testSet$MR == "NO")
accry_rate = (n_pred_p + n_pred_n) / N

