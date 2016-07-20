##NIOSH STUDY##
##Professor Alison Morantz##
##Stanford Law School##

#Coded by Nikhil Saifullah

#Description

#This file performs the model selection stage for our prediction data from prepared_violations_mines.R and runs some preliminary models

library(FactoMineR)
library(stats)
library(glarma)
library(pglm)
library(psych)
library(dplyr)
library(zoo)
library(glmnet)
library(randomForest)
library(MASS)
library(data.table)

#prediction_data = readRDS("X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/prediction_data.rds")
prediction_data = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75.rds")
prediction_data = prediction_data[, c(-grep("minetype", names(prediction_data)), -grep("coalcormetalmmine", names(prediction_data)), -match("daysperweek", names(prediction_data)))]

#Make categorical variables with numeric levels

prediction_data$year = factor(prediction_data$year)

prediction_data$minestatus = ifelse(prediction_data$minestatus == "Abandoned", 1, ifelse(prediction_data$minestatus == "Abandoned and Sealed", 2, 
                                                                                         ifelse(prediction_data$minestatus == "Active", 3, 
                                                                                                ifelse(prediction_data$minestatus == "Intermittent", 4,
                                                                                                       ifelse(prediction_data$minestatus == "New Mine", 5,
                                                                                                              ifelse(prediction_data$minestatus == "NonProducing", 6, 
                                                                                                                     ifelse(prediction_data$minestatus == "Temporarily Idled", 7, NA)))))))

prediction_data$idesc = ifelse(prediction_data$idesc == "Hazard", 1, ifelse(prediction_data$idesc == "Ignition or Explosion", 2, 
                                                                            ifelse(prediction_data$idesc == "Inspect Once Every 10-days", 3, 
                                                                                   ifelse(prediction_data$idesc == "Inspect Once Every 15-days", 4,
                                                                                          ifelse(prediction_data$idesc == "Inspect Once Every 5-days", 5,
                                                                                                 ifelse(prediction_data$idesc == "Never Had 103I Status", 6, 
                                                                                                        ifelse(prediction_data$idesc == "Removed From 103I Status", 7, NA)))))))
#Create mine/quarter specific dummies
datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}
test.data1 <- datdum(x="mineid",data=prediction_data,name="mine")
test.data1 <- test.data1[, c(grep("^mine\\.", names(test.data1)))]
test.data2 <- datdum(x="quarter",data=prediction_data,name="quart")
test.data2 <- test.data2[, c(grep("^quart\\.", names(test.data2)))]
prediction_data = cbind(prediction_data, test.data1, test.data2)
rm(test.data1, test.data2)

######################################################################################################################################
#FILL IN MISSING VALUES OF MINE CHARACTERISTICS BY MINE_ID/QUARTER GROUPS
prediction_data = group_by(prediction_data, mineid, quarter)
prediction_data = prediction_data[order(prediction_data$mineid, prediction_data$quarter, na.last = T),]
prediction_data$minename = na.locf(prediction_data$minename)
prediction_data$minesizepoints = na.locf(prediction_data$minesizepoints)
prediction_data$controllersizepoints = na.locf(prediction_data$controllersizepoints)
prediction_data$contractorsizepoints = na.locf(prediction_data$contractorsizepoints)
prediction_data$hours_qtr = na.locf(prediction_data$hours_qtr)
prediction_data$employment_qtr = na.locf(prediction_data$employment_qtr)
prediction_data$coal_prod_qtr = na.locf(prediction_data$coal_prod_qtr)
prediction_data$productionshiftsperday = na.locf(prediction_data$productionshiftsperday)
prediction_data$idesc = na.locf(prediction_data$idesc)

# FIRST PIPE IN ZEROES TO THE MISSING PART-SPECIFIC VARIABLES (IF NOTHING MERGED ON A MINE QUARTER THAN IT SHOULD BE A ZERO)
number_to_zero = prediction_data[, c(grep("^[0-9][0-9]", names(prediction_data)), match("mineid", names(prediction_data)),
                                  match("quarter", names(prediction_data)), match("terminated", names(prediction_data)),
                                  match("total_violations", names(prediction_data)), match("contractor_repeated_viol_cnt", names(prediction_data)), 
                                  match("totalinjuries", names(prediction_data)), match("MR", names(prediction_data)),
                                  match("insp_hours_per_qtr", names(prediction_data)), match("onsite_insp_hours_per_qtr", names(prediction_data)),
                                  match("num_insp", names(prediction_data)))]
prediction_data = prediction_data[, c(-grep("^[0-9][0-9]", names(prediction_data)), -match("terminated", names(prediction_data)),
                                      -match("total_violations", names(prediction_data)), -match("contractor_repeated_viol_cnt", names(prediction_data)), 
                                      -match("totalinjuries", names(prediction_data)), -match("MR", names(prediction_data)),
                                      -match("insp_hours_per_qtr", names(prediction_data)), -match("onsite_insp_hours_per_qtr", names(prediction_data)),
                                      -match("num_insp", names(prediction_data)))]
number_to_zero[is.na(number_to_zero)] = 0
prediction_data = merge(prediction_data, number_to_zero, by = c("mineid", "quarter"), all = T)
rm(number_to_zero)

#NOW REPLACE ANY MISSINGS IN OTHER NUMERIC VARS BY RANDOMLY SAMPLING FROM THE DISTRIBUTION
var_classes = sapply(prediction_data[,names(prediction_data)], class)
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])
for (i in 1:length(num_vars)) {
   i_rowsmissing = row.names(prediction_data)[is.na(prediction_data[, num_vars[i]])]
   while (sum(!complete.cases(prediction_data[, num_vars[i]])) > 0) {
     replace_rows = sample(setdiff(row.names(prediction_data), i_rowsmissing), length(i_rowsmissing), replace = T)
     prediction_data[i_rowsmissing, num_vars[i]] = prediction_data[replace_rows, num_vars[i]]
   }
}
#####################################################################################################################################
# ADD VARIABLES FOR BINARY AND PROPORTIONAL DEPENDENT VARS, AND RESHAPE A FEW VARS OF INTEREST
prediction_data$MR_indicator = ifelse(prediction_data$MR > 0, 1, 0)
prediction_data$MR_proportion = prediction_data$MR / prediction_data$totalinjuries
prediction_data$no_terminations = ifelse(prediction_data$terminated < prediction_data$total_violations, 1, 0)

#Create lagged variables of the 1st and 2nd order
prediction_data = as.data.table(prediction_data[order(prediction_data$mineid, prediction_data$quarter, na.last = T),])
prediction_data[, c("75.penaltypoints_l1", "75.penaltypoints_l2", "75.penaltypoints_l3", "75.penaltypoints_l4", "75.penaltypoints_l5", "75.penaltypoints_l6",
                    "75.sigandsubdesignation_l1", "75.sigandsubdesignation_l2", "75.sigandsubdesignation_l3", "75.sigandsubdesignation_l4", "75.sigandsubdesignation_l5", "75.sigandsubdesignation_l6",
                    "75_l1", "75_l2", "75_l3", "75_l4", "75_l5", "75_l6",
                    "75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4", "75.1405_l5", "75.1405_l6",
                    "MR_l1", "MR_l2", "MR_l3", "MR_l4", "MR_l5", "MR_l6", 
                    "MR_indicator_l1", "MR_indicator_l2", "MR_indicator_l3", "MR_indicator_l4", "MR_indicator_l5", "MR_indicator_l6",
                    "MR_proportion_l1", "MR_proportion_l2", "MR_proportion_l3", "MR_proportion_l4","MR_proportion_l5","MR_proportion_l6") := shift(.SD, 1:6), 
                 by = mineid, .SDcols = c("75.penaltypoints", "75.sigandsubdesignation", "75", "75.1405", "MR", "MR_indicator", "MR_proportion")]
prediction_data = as.data.frame(prediction_data)

#Pare away variables with zero variation before model selection and prediction stages
var_stats = describe(prediction_data[, c(-match("mineid", names(prediction_data)), -match("quarter", names(prediction_data)), -match("year", names(prediction_data)),
                                         -match("minename", names(prediction_data)), -match("minestatusdate", names(prediction_data)), -match("operatorid", names(prediction_data)),
                                         -match("operatorname", names(prediction_data)), -match("stateabbreviation", names(prediction_data)), -match("idate", names(prediction_data)))])
nontriv_vars = rownames(var_stats[var_stats$sd > 0,])
triv_vars = setdiff(names(prediction_data), nontriv_vars)
#Warning: This excludes all non-numeric variables
prediction_data = prediction_data[, c(nontriv_vars, "mineid", "quarter")]

#Run variable selection over CFR subsection codes
#"terminated" is a count of all citations that have been terminated by MSHA for a mine. This reflects a mine's past citations but also its ability to
#improve its safety conditions. We may form terminated/total_violations by mine-qtr in the future but will remain agnostic as of now.
mine_faults = c("total_violations", "contractor_repeated_viol_cnt", "operator_repeated_viol_pInspDay", "terminated")
inspec_exp = c("insp_hours_per_qtr", "onsite_insp_hours_per_qtr", "num_insp")
inj_exp = c("productionshiftsperday", "coal_prod_qtr", "employment_qtr", "hours_qtr", "minesizepoints")
mine_penpoints = c("contractorsizepoints", "controllersizepoints")

# this line will report number of missings per var - should be zero (except lagged vars which will be missing for first quarter)!
#apply(is.na(prediction_data),2,sum)

######################################################################################################################################
#save prediction data to run models in Stata

# remove observations that are missing the third lagged var (this will also by default remove obs that are missing either the first
# or second lagged var) these are the first three quarters of a mines operation so there is no prior data to lag
prediction_data = prediction_data[!is.na(prediction_data$`75.penaltypoints_l6`),]
prediction_data = prediction_data[!is.na(prediction_data$`75.sigandsubdesignation_l6`),]
prediction_data = prediction_data[!is.na(prediction_data$`75_l6`),]

prediction_data$sum_75_1_2 = rowSums(prediction_data[,c("75_l1", "75_l2")])
prediction_data$sum_75_1_3 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3")])
prediction_data$sum_75_1_4 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3", "75_l4")])
prediction_data$sum_75_1_5 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3", "75_l4", "75_l5")])
prediction_data$sum_75_1_6 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3", "75_l4", "75_l5", "75_l6")])

prediction_data$sum_75.1405_1_2 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2")])
prediction_data$sum_75.1405_1_3 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3")])
prediction_data$sum_75.1405_1_4 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4")])
prediction_data$sum_75.1405_1_5 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4", "75.1405_l5")])
prediction_data$sum_75.1405_1_6 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4", "75.1405_l5", "75.1405_l6")])

varnames = names(prediction_data)
varnames = gsub("\\.", "_", varnames)
varnames = gsub("-", "_", varnames)
varnames = paste("_", varnames, sep ="")
names(prediction_data) = varnames
write.csv(prediction_data, "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75.csv")

######################################################################################################################################

#PCA

pca_results = PCA(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+\\.penaltypoints", names(prediction_data))], graph = F)
pca_inj_exp = PCA(prediction_data[,unlist(lapply(inj_exp, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_inspec_exp = PCA(prediction_data[,unlist(lapply(inspec_exp, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_mine_faults = PCA(prediction_data[,unlist(lapply(mine_faults, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_mine_penpoints = PCA(prediction_data[,unlist(lapply(mine_penpoints, FUN = function(x) match(x, names(prediction_data))))], graph = F)

#ANALYZE PCA RESULTS
#Now use pca_results$var$contrib[,j] j = 1, 2, ..., K to access the jth principal component for the ith CFR part code. Take absolute values before analyzing.
#Use plot.PCA(pca_results, choix = "var"/"ind") to view correlation circle plot/individual factor map and summary.PCA(pca_results) for Kaiser-Guttman test.
#UNDER CONSTRUCTION - Tool for extracting significant variables from PCA analysis
imp_vars = list()
a = list()
k = length(grep("^[0-9][0-9]\\.[0-9]+\\.inspacty", names(prediction_data)))
test = PCA(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+\\.inspacty", names(prediction_data))], graph = F)
for (i in 1:3) {
  imp_vars[[i]] = test$var$contrib[test$var$contrib[,i] >= sqrt(1/k),i]
  if (i > 1) {
    a = intersect(a, names(unlist(imp_vars[[i]])))
  } else {
    a = intersect(names(unlist(imp_vars[[i]])), names(unlist(test$var$contrib[test$var$contrib[,i+1] >= sqrt(1/k),i+1])))
  }
  print(a)
}

#LASSO

lasso_results = glmnet(as.matrix(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+\\.penaltypoints", names(prediction_data))]), 
                       as.vector(prediction_data$MR), family = "gaussian")
print(lasso_results)
plot(lasso_results)
#Argument s passed to "coef" is the lambda value at which LASSO coefficients are obtained
lasso_coefs = coef(lasso_results, s = 0.05)[,1]
survng_vars = names(lasso_coefs)[lasso_coefs > 0]
survng_vars[2:length(survng_vars)]

#RANDOM FOREST
rf_results = randomForest(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+\\.penaltypoints", names(prediction_data))], prediction_data$MR)
sort(rf_results$importance[,1], decreasing = T)

#Exploring MCA - NOT USED
#mca_results = MCA(as.data.frame(sapply(prediction_data[, c(match("minestatus", names(prediction_data)), grep("idesc", names(prediction_data)))], FUN = factor)))
#summary.MCA(mca_results)

#EXPLORING MFA - There is an obscure error thrown with this code. NOT USED
#mfa_results = MFA(data, group = c(81, 2, 13), type = c("c", "n", "c"), name.group = c("quant1", "quali1", "quant2"))

######################################################################################################################################
# EVERYTHING BELOW THIS LINE IS FOR THE ALGORITHM

#WARNING: Fails to converge with these initial values
N = nrow(prediction_data)
K = ncol(prediction_data) - 3
X = as.matrix(prediction_data[, c(-grep("MR", names(prediction_data)), -grep("mineid", names(prediction_data)), -grep("quarter", names(prediction_data)))])
Y = as.vector(prediction_data$MR)

#Training/Test Set Creation
set.seed(625)
prediction_data$rand = runif(nrow(prediction_data))
prediction_data = prediction_data[order(prediction_data$rand),]

#Naive OLS prediction. Used as a check on variable selection
test_pred_naive = lm(formula = MR ~ ., data = prediction_data[1:21965, c(match("MR", names(prediction_data)),
                                                                         grep("^[0-9][0-9]$", names(prediction_data)),
                                                                         #grep("MR_l", names(prediction_data)),
                                                                         grep("mine\\.", names(prediction_data)),
                                                                         #grep("^quart\\.", names(prediction_data)),
                                                                         match("47.penaltypoints", names(prediction_data)),
                                                                         match("48.penaltypoints", names(prediction_data)),
                                                                         match("71.penaltypoints", names(prediction_data)),
                                                                         match("72.penaltypoints", names(prediction_data)),
                                                                         match("75.penaltypoints", names(prediction_data)),
                                                                         match("77.penaltypoints", names(prediction_data)),
                                                                         match("47.sigandsubdesignation", names(prediction_data)),
                                                                         match("48.sigandsubdesignation", names(prediction_data)),
                                                                         match("71.sigandsubdesignation", names(prediction_data)),
                                                                         match("72.sigandsubdesignation", names(prediction_data)),
                                                                         match("75.sigandsubdesignation", names(prediction_data)),
                                                                         match("77.sigandsubdesignation", names(prediction_data)),
                                                                         #match("mineid", names(prediction_data)),
                                                                         #match("quarter", names(prediction_data)),
                                                                         match("no_terminations", names(prediction_data)),  
                                                                         match("total_violations", names(prediction_data)),
                                                                         match("totalinjuries", names(prediction_data)),
                                                                         match("num_insp", names(prediction_data)),
                                                                         #match("employment_qtr", names(prediction_data)),
                                                                         #match("coal_prod_qtr", names(prediction_data)),
                                                                         match("hours_qtr", names(prediction_data)),
                                                                         match("onsite_insp_hours_per_qtr", names(prediction_data)))])

test_df = prediction_data[1:21965, c(match("MR", names(prediction_data)),
                                     grep("^[0-9][0-9]$", names(prediction_data)),
                                     match("47.penaltypoints", names(prediction_data)),
                                     match("48.penaltypoints", names(prediction_data)),
                                     match("71.penaltypoints", names(prediction_data)),
                                     match("72.penaltypoints", names(prediction_data)),
                                     match("75.penaltypoints", names(prediction_data)),
                                     match("77.penaltypoints", names(prediction_data)),
                                     match("47.sigandsubdesignation", names(prediction_data)),
                                     match("48.sigandsubdesignation", names(prediction_data)),
                                     match("71.sigandsubdesignation", names(prediction_data)),
                                     match("72.sigandsubdesignation", names(prediction_data)),
                                     match("75.sigandsubdesignation", names(prediction_data)),
                                     match("77.sigandsubdesignation", names(prediction_data)),
                                     match("mineid", names(prediction_data)),
                                     match("quarter", names(prediction_data)),
                                     match("no_terminations", names(prediction_data)),  
                                     match("total_violations", names(prediction_data)),
                                     match("totalinjuries", names(prediction_data)),
                                     match("num_insp", names(prediction_data)),
                                     #match("employment_qtr", names(prediction_data)),
                                     #match("coal_prod_qtr", names(prediction_data)),
                                     match("hours_qtr", names(prediction_data)),
                                     match("onsite_insp_hours_per_qtr", names(prediction_data)))]
test_df = test_df[order(test_df$mineid, test_df$quarter, na.last = T),]
bgtest_results = bgtest(formula = MR ~ . -mineid -quarter, order = 1, type = "Chisq", data = test_df)
ols_fit = summary.lm(test_pred_naive)
test_df = test_pred_naive$model

#Serial-Correlation Block

#Breusch-Godfrey test commented out since strong tendency to reject time-independence due to our large N
#test_df = test_df[order(test_df$mineid, test_df$quarter, na.last = T),]
#bgtest_results = bgtest(formula = MR ~ . -mineid -quarter, order = 1, type = "Chisq", data = test_df)
#Manually assess degree and order of serial-correlation
test_df$residuals = test_pred_naive$residuals
test_df = as.data.table(test_df[order(test_df$mineid, test_df$quarter, na.last = T),])
test_df[, c("residualsl1", "residualsl2", "residualsl3", "residualsl4") := shift(.SD, 1:4), 
                by = mineid, .SDcols = c("residuals")]
test_df = as.data.frame(test_df)
serialcorr_test = lm(formula = residuals ~ ., data = test_df)

#Divergent estimates of theta assuming a NegBi(r, p) distribution on MR suggest failure of NB assumptions. We turn to Poisson regression
#test_pred_0 = glm.nb(formula = MR ~ total_violations + insp_hours_per_qtr -mineid -quarter, data = prediction_data)
test_pred_0 = glm(formula = MR ~ ., family = "poisson", data = prediction_data[1:21965, c(match("MR", names(prediction_data)),
                                                                                          grep("^[0-9][0-9]$", names(prediction_data)),
                                                                                          grep("mine\\.", names(prediction_data)),
                                                                                          grep("(77|75).penaltypoints", names(prediction_data)),
                                                                                          grep("(77|75).gravitylikelihoodpoints", names(prediction_data)),
                                                                                          grep("(77|75).gravityinjurypoints", names(prediction_data)),
                                                                                          grep("(77|75|72).gravitypersonspoints", names(prediction_data)),
                                                                                          grep("(77|75).operator_violation_pInspDay", names(prediction_data)),
                                                                                          grep("(77|75).contractor_violation_cnt", names(prediction_data)),
                                                                                          grep("(77|75).sigandsubdesignation", names(prediction_data)),
                                                                                          grep("(77|75).negligencepoints", names(prediction_data)),
                                                                                          grep("(77|75).inspacty", names(prediction_data)),
                                                                                          grep("(77|75|72|47).violationtypecode", names(prediction_data)),
                                                                                          grep("(77|75|48|47).assessmenttypecode", names(prediction_data)),
                                                                                          grep("(77|75).likelihood", names(prediction_data)),
                                                                                          grep("(77|75).injuryillness", names(prediction_data)),
                                                                                          #match("mineid", names(prediction_data)),
                                                                                          #match("quarter", names(prediction_data)),
                                                                                          match("no_terminations", names(prediction_data)),  
                                                                                          match("total_violations", names(prediction_data)),
                                                                                          match("totalinjuries", names(prediction_data)),
                                                                                          match("num_insp", names(prediction_data)),
                                                                                          match("hours_qtr", names(prediction_data)),
                                                                                          match("onsite_insp_hours_per_qtr", names(prediction_data)))])

poisson_fit = summary.glm(test_pred_0)

logit_data = prediction_data[, c(grep("MR_indicator", names(prediction_data)), 
                                grep("^[0-9][0-9]$", names(prediction_data)), 
                                grep("^[0-9][0-9]_l[1-3]$", names(prediction_data)), 
                                grep("75.penaltypoints", names(prediction_data)),
                                grep("75.sigandsubdesignation", names(prediction_data)),
                                match("no_terminations", names(prediction_data)),  
                                match("total_violations", names(prediction_data)),
                                match("num_insp", names(prediction_data)), 
                                match("hours_qtr", names(prediction_data)),
                                match("onsite_insp_hours_per_qtr", names(prediction_data)))]
# remove observations that are missing the third lagged var (this will also by default remove obs that are missing either the first
# or second lagged var) these are the first three quarters of a mines operation so there is no prior data to lag
logit_data = logit_data[!is.na(logit_data$MR_indicator_l3),]
logit_data = logit_data[!is.na(logit_data$`75.penaltypoints_l3`),]
logit_data = logit_data[!is.na(logit_data$`75.sigandsubdesignation_l3`),]
logit_data = logit_data[!is.na(logit_data$`75_l3`),]

# logit_train_data = logit_data[1:21965,]
# #Pare away variables with zero variation before model selection and prediction stages
# var_stats = describe(logit_train_data[1:21965, c(-match("mineid", names(logit_train_data)), -match("quarter", names(logit_train_data)))])
# nontriv_vars = rownames(var_stats[var_stats$sd > 0,])
# triv_vars = setdiff(names(train_data), nontriv_vars)
# #Warning: This excludes all non-numeric variables
# logit_train_data = logit_train_data[, nontriv_vars]
# logit_test_data = logit_data[21965:27456,nontriv_vars]

# LOGIT ON BINARY OUTCOME VARIABLES
logit = glm(MR_indicator ~ . , family = "binomial", data = logit_data[1:21965,])

logit_prediction = predict(logit, newdata = logit_data[21965:27456,])

ols_prediction = predict(test_pred_naive, newdata = prediction_data[21965:27456,c(match("MR", names(prediction_data)),
                                                                                  grep("^[0-9][0-9]$", names(prediction_data)),
                                                                                  grep("mine\\.", names(prediction_data)),
                                                                                  match("47.penaltypoints", names(prediction_data)),
                                                                                  match("48.penaltypoints", names(prediction_data)),
                                                                                  match("71.penaltypoints", names(prediction_data)),
                                                                                  match("72.penaltypoints", names(prediction_data)),
                                                                                  match("75.penaltypoints", names(prediction_data)),
                                                                                  match("77.penaltypoints", names(prediction_data)),
                                                                                  match("47.sigandsubdesignation", names(prediction_data)),
                                                                                  match("48.sigandsubdesignation", names(prediction_data)),
                                                                                  match("71.sigandsubdesignation", names(prediction_data)),
                                                                                  match("72.sigandsubdesignation", names(prediction_data)),
                                                                                  match("75.sigandsubdesignation", names(prediction_data)),
                                                                                  match("77.sigandsubdesignation", names(prediction_data)),
                                                                                  match("no_terminations", names(prediction_data)),  
                                                                                  match("total_violations", names(prediction_data)),
                                                                                  match("totalinjuries", names(prediction_data)),
                                                                                  match("num_insp", names(prediction_data)),
                                                                                  #match("employment_qtr", names(prediction_data)),
                                                                                  #match("coal_prod_qtr", names(prediction_data)),
                                                                                  match("hours_qtr", names(prediction_data)),
                                                                                  match("onsite_insp_hours_per_qtr", names(prediction_data)))])

#This produces many negative predictions, which is totally perverse for this model.
poisson_prediction = predict(test_pred_0, newdata = prediction_data[21965:27456,c(match("MR", names(prediction_data)),
                                                                                  grep("^[0-9][0-9]$", names(prediction_data)),
                                                                                  grep("(77|75).penaltypoints", names(prediction_data)),
                                                                                  grep("(77|75).gravitylikelihoodpoints", names(prediction_data)),
                                                                                  grep("(77|75).gravityinjurypoints", names(prediction_data)),
                                                                                  grep("(77|75|72).gravitypersonspoints", names(prediction_data)),
                                                                                  grep("(77|75).operator_violation_pInspDay", names(prediction_data)),
                                                                                  grep("(77|75).contractor_violation_cnt", names(prediction_data)),
                                                                                  grep("(77|75).sigandsubdesignation", names(prediction_data)),
                                                                                  grep("(77|75).negligencepoints", names(prediction_data)),
                                                                                  grep("(77|75).inspacty", names(prediction_data)),
                                                                                  grep("(77|75|72|47).violationtypecode", names(prediction_data)),
                                                                                  grep("(77|75|48|47).assessmenttypecode", names(prediction_data)),
                                                                                  grep("(77|75).likelihood", names(prediction_data)),
                                                                                  grep("(77|75).injuryillness", names(prediction_data)),
                                                                                  match("no_terminations", names(prediction_data)),  
                                                                                  match("total_violations", names(prediction_data)),
                                                                                  match("totalinjuries", names(prediction_data)),
                                                                                  match("num_insp", names(prediction_data)),
                                                                                  match("hours_qtr", names(prediction_data)),
                                                                                  match("onsite_insp_hours_per_qtr", names(prediction_data)))])


logit_prediction_r = round(logit_prediction, 0)
prediction_data$MR_r = round(prediction_data$MR, 0)
sum(logit_prediction_r == prediction_data[21965:27456,]$MR_r)/nrow(prediction_data[21965:27456,])
#OLS prediction accuracy is currently 84.6%

ols_prediction_r = round(ols_prediction, 0)
prediction_data$MR_r = round(prediction_data$MR, 0)
ols_predict_data = prediction_data[21965:27456,]
# get accuracy conditional on positive outcome
sum(ols_prediction_r[ols_prediction_r > 0] == ols_predict_data[ols_predict_data$MR_r > 0,]$MR_r)/nrow(ols_predict_data[ols_predict_data$MR_r > 0,])
sum(ols_prediction_r == prediction_data[21965:27456,]$MR_r)/nrow(prediction_data[21965:27456,])
#OLS prediction accuracy is currently 84.6%

poisson_prediction = ifelse(is.na(poisson_prediction), 0, poisson_prediction)
poisson_prediction_r = round(exp(poisson_prediction), 0)
prediction_data$MR_r = round(prediction_data$MR, 0)
possion_predict_data = prediction_data[21965:27456,]
# get accuracy conditional on positive outcome
sum(poisson_prediction_r[poisson_prediction_r > 0] == possion_predict_data[possion_predict_data$MR_r > 0,]$MR_r) /nrow(possion_predict_data[possion_predict_data$MR_r > 0,])
sum(poisson_prediction_r == prediction_data[21965:27456,]$MR_r)/nrow(prediction_data[21965:27456,])
#Poisson prediction accuracy is currently 83.9%

#Compares the Poisson & OLS predicted distributions with the observed response distribution
test_pred_naive$fitted.values = ifelse(test_pred_naive$fitted.values < 0, 0, test_pred_naive$fitted.values)
ols_pred = as.numeric(unlist(lapply(test_pred_naive$fitted.values, FUN = round)))
poisson_pred = as.numeric(unlist(lapply(test_pred_0$fitted.values, FUN = round)))
table(prediction_data$MR)
table(poisson_pred)
table(ols_pred)

test_pred = glarma(Y, X, type = "NegBin", phiLags = c(1, 2), thetaLags = c(1, 2), phiInit = c(0.5, 0.5), thetaInit = c(0.25, 0.25), beta = rep(1, K), alphaInit = 1)

#For some reason, unable to use usual formula abbreviations in this command
names(prediction_data)[match("47.41", names(prediction_data))] = "subsection_47.41"
test_pred_2 = pglm(MR ~  subsection_47.41 + penaltypoints_47.41 + totalinjuries ,
                   prediction_data, na.action = na.omit, family = "negbin", effect = "time", model = "within")
