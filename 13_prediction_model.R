# NIOSH Project 2014-N-15776

# 13 - Prediction Model
    # Imports prediction data produced in prepare_violations_mines.R 
    # Produces lagged dependent and independent variables 
    # Saves a Stata-friendly prediction-ready dataset
    # Performs various model selection methods (Principle Components Analysis, LASSO, Random Forest) 
    # Performs various frequentist prediction models (OLS, Logit, Poisson)

# Last edit 7/29/16

######################################################################################################

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

# define file names
  # input: prediction data produced in 11_prepare_violations.R
prediction_data.in.file.name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75.rds"
  # output: Stata-friendly prediction-ready datatset
prediction_data.out.file.name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75.csv"

######################################################################################################

# read data
prediction_data = readRDS(prediction_data.in.file.name)

# HEY NIKHIL, WHAT DOES THIS DO? - SARAH

# Run variable selection over CFR subsection codes
# "terminated" is a count of all citations that have been terminated by MSHA for a mine. This reflects a mine's past citations 
# but also its ability to improve its safety conditions. We may form terminated/total_violations by mine-qtr in the future but 
# will remain agnostic as of now.
mine_faults = c("total_violations", "contractor_repeated_viol_cnt", "operator_repeated_viol_pInspDay", "terminated")
inspec_exp = c("insp_hours_per_qtr", "onsite_insp_hours_per_qtr", "num_insp")
inj_exp = c("productionshiftsperday", "coal_prod_qtr", "employment_qtr", "hours_qtr", "minesizepoints")
mine_penpoints = c("contractorsizepoints", "controllersizepoints")

######################################################################################################################################

# SAVE DATA TO RUN IN STATA

# This code renames the variables so they are compatible with Stata (cannot begin with a number) and saves a .csv, so we can run all the
# same analyses in Stata to confirm our results
varnames = names(prediction_data)
varnames = gsub("\\.", "_", varnames)
varnames = gsub("-", "_", varnames)
varnames = paste("_", varnames, sep ="")
names(prediction_data) = varnames
write.csv(prediction_data, prediction_data.out.file.name)

######################################################################################################################################

#  APPLY MODEL SELECTION TECHNIQUES (PCA, LASSO, Random Forest)

# Principle Components Analysis (PCA)
pca_results = PCA(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+\\.penaltypoints", names(prediction_data))], graph = F)
pca_inj_exp = PCA(prediction_data[,unlist(lapply(inj_exp, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_inspec_exp = PCA(prediction_data[,unlist(lapply(inspec_exp, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_mine_faults = PCA(prediction_data[,unlist(lapply(mine_faults, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_mine_penpoints = PCA(prediction_data[,unlist(lapply(mine_penpoints, FUN = function(x) match(x, names(prediction_data))))], graph = F)

# Analyze PCA results
# Now use pca_results$var$contrib[,j] j = 1, 2, ..., K to access the jth principal component for the ith CFR part code. 
# Take absolute values before analyzing. Use plot.PCA(pca_results, choix = "var"/"ind") to view correlation circle 
# plot/individual factor map and summary.PCA(pca_results) for Kaiser-Guttman test.
# UNDER CONSTRUCTION - Tool for extracting significant variables from PCA analysis - Nikhil
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

# Least Absolute Shrinkage and Selection Operator (LASSO)
lasso_results = glmnet(as.matrix(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+\\.penaltypoints", names(prediction_data))]), 
                       as.vector(prediction_data$MR), family = "gaussian")
print(lasso_results)
plot(lasso_results)

# Argument passed to "coef" is the lambda value at which LASSO coefficients are obtained
lasso_coefs = coef(lasso_results, s = 0.05)[,1]
survng_vars = names(lasso_coefs)[lasso_coefs > 0]
survng_vars[2:length(survng_vars)]

# RANDOM FOREST
rf_results = randomForest(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+\\.penaltypoints", names(prediction_data))], prediction_data$MR)
sort(rf_results$importance[,1], decreasing = T)

######################################################################################################################################

# THIS CODE IS RETIRED.

# Exploring MCA - NOT USED
#mca_results = MCA(as.data.frame(sapply(prediction_data[, c(match("minestatus", names(prediction_data)), grep("idesc", names(prediction_data)))], FUN = factor)))
#summary.MCA(mca_results)

# EXPLORING MFA - There is an obscure error thrown with this code. NOT USED
#mfa_results = MFA(data, group = c(81, 2, 13), type = c("c", "n", "c"), name.group = c("quant1", "quali1", "quant2"))

######################################################################################################################################

# EVERYTHING BELOW THIS LINE IS FOR THE PREDICTION ALGORITHMS (AT THIS POINT ALL FREQUENTIST METHODS)

# Here are the mine-level vars we believe we want to include in the algorithm:
# mineid, quarter, no_terminations, total_violations*, totalinjuries, num_insp, hours_qtr**, onsite_insp_hours_per_qtr
# *total_violations: the inclusion of total_violations often leads to nonconvergence because of collinearity
# **hours_qtr: we use this as a sort of exposure term, employment_qtr and employment_qtr are valid alternatives

# WARNING: Fails to converge with these initial values
N = nrow(prediction_data)
K = ncol(prediction_data) - 3
X = as.matrix(prediction_data[, c(-grep("MR", names(prediction_data)), 
                                  -grep("mineid", names(prediction_data)), 
                                  -grep("quarter", names(prediction_data)))])
Y = as.vector(prediction_data$MR)

# Training/Test Set Creation
set.seed(625)
prediction_data$rand = runif(nrow(prediction_data))
prediction_data = prediction_data[order(prediction_data$rand),]

######################################################################################################################################

# NAIVE OLS: Used as a check on variable selection

test_pred_naive = lm(formula = MR ~ ., data = prediction_data[1:21965, c(match("MR", names(prediction_data)),
                                                                         grep("^[0-9][0-9]$", names(prediction_data)),
                                                                         #grep("MR_l", names(prediction_data)),
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
                                                                         match("hours_qtr", names(prediction_data)),
                                                                         match("onsite_insp_hours_per_qtr", names(prediction_data)))])

# Create dataset for prediction.
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
                                     match("hours_qtr", names(prediction_data)),
                                     match("onsite_insp_hours_per_qtr", names(prediction_data)))]
test_df = test_df[order(test_df$mineid, test_df$quarter, na.last = T),]
bgtest_results = bgtest(formula = MR ~ . -mineid -quarter, order = 1, type = "Chisq", data = test_df)
ols_fit = summary.lm(test_pred_naive)
test_df = test_pred_naive$model

######################################################################################################################################

# TEST FOR SERIAL-CORRELATION

# Breusch-Godfrey test commented out since strong tendency to reject time-independence due to our large N
#test_df = test_df[order(test_df$mineid, test_df$quarter, na.last = T),]
#bgtest_results = bgtest(formula = MR ~ . -mineid -quarter, order = 1, type = "Chisq", data = test_df)

# Manually assess degree and order of serial-correlation
test_df$residuals = test_pred_naive$residuals
test_df = as.data.table(test_df[order(test_df$mineid, test_df$quarter, na.last = T),])
test_df[, c("residualsl1", "residualsl2", "residualsl3", "residualsl4") := shift(.SD, 1:4), 
                by = mineid, .SDcols = c("residuals")]
test_df = as.data.frame(test_df)
serialcorr_test = lm(formula = residuals ~ ., data = test_df)

######################################################################################################################################

# NEGATIVE BINOMIAL

# Divergent estimates of theta assuming a NegBi(r, p) distribution on MR suggest failure of NB assumptions. 
test_pred_0 = glm.nb(formula = MR ~ total_violations + insp_hours_per_qtr -mineid -quarter, data = prediction_data)
# We turn to Poisson regression.

######################################################################################################################################

# POISSON

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
                                                                                          match("no_terminations", names(prediction_data)),  
                                                                                          match("total_violations", names(prediction_data)),
                                                                                          match("totalinjuries", names(prediction_data)),
                                                                                          match("num_insp", names(prediction_data)),
                                                                                          match("hours_qtr", names(prediction_data)),
                                                                                          match("onsite_insp_hours_per_qtr", names(prediction_data)))])

poisson_fit = summary.glm(test_pred_0)

######################################################################################################################################

# LOGIT

# Create training set data for logit
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

# Remove observations that are missing the last lagged var (this will also by default remove obs that are missing previous lagged vars).
# These are the first three (or however many) quarters of a mines operation so there is no prior data to lag
logit_data = logit_data[!is.na(logit_data$MR_indicator_l3),]
logit_data = logit_data[!is.na(logit_data$`75.penaltypoints_l3`),]
logit_data = logit_data[!is.na(logit_data$`75.sigandsubdesignation_l3`),]
logit_data = logit_data[!is.na(logit_data$`75_l3`),]

# logit_train_data = logit_data[1:21965,]

# # Pare away variables with zero variation before model selection and prediction stages
# var_stats = describe(logit_train_data[1:21965, c(-match("mineid", names(logit_train_data)), -match("quarter", names(logit_train_data)))])
# nontriv_vars = rownames(var_stats[var_stats$sd > 0,])
# triv_vars = setdiff(names(train_data), nontriv_vars)

# # Warning: This excludes all non-numeric variables
# logit_train_data = logit_train_data[, nontriv_vars]
# logit_test_data = logit_data[21965:27456,nontriv_vars]

# Implement logit
logit = glm(MR_indicator ~ . , family = "binomial", data = logit_data[1:21965,])

# Prediction using logit results
logit_prediction = predict(logit, newdata = logit_data[21965:27456,])

######################################################################################################################################

# REPORT VARIOUS MODEL PREDICTIONS

# Create OLS predictions
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
                                                                                  match("hours_qtr", names(prediction_data)),
                                                                                  match("onsite_insp_hours_per_qtr", names(prediction_data)))])

# Create POISSON predictions
# This produces many negative predictions, which is totally perverse for this model.
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
# Report LOGIT prediction
logit_prediction_r = round(logit_prediction, 0)
prediction_data$MR_r = round(prediction_data$MR, 0)
sum(logit_prediction_r == prediction_data[21965:27456,]$MR_r)/nrow(prediction_data[21965:27456,])
# OLS prediction accuracy is currently 84.6%

# Report OLS prediction
ols_prediction_r = round(ols_prediction, 0)
prediction_data$MR_r = round(prediction_data$MR, 0)
ols_predict_data = prediction_data[21965:27456,]
# Get accuracy conditional on positive outcome
sum(ols_prediction_r[ols_prediction_r > 0] == ols_predict_data[ols_predict_data$MR_r > 0,]$MR_r)/nrow(ols_predict_data[ols_predict_data$MR_r > 0,])
sum(ols_prediction_r == prediction_data[21965:27456,]$MR_r)/nrow(prediction_data[21965:27456,])
# OLS prediction accuracy is currently 84.6%

# Report POISSON prediction
poisson_prediction = ifelse(is.na(poisson_prediction), 0, poisson_prediction)
poisson_prediction_r = round(exp(poisson_prediction), 0)
prediction_data$MR_r = round(prediction_data$MR, 0)
possion_predict_data = prediction_data[21965:27456,]
# Get accuracy conditional on positive outcome
sum(poisson_prediction_r[poisson_prediction_r > 0] == possion_predict_data[possion_predict_data$MR_r > 0,]$MR_r) /nrow(possion_predict_data[possion_predict_data$MR_r > 0,])
sum(poisson_prediction_r == prediction_data[21965:27456,]$MR_r)/nrow(prediction_data[21965:27456,])
# Poisson prediction accuracy is currently 83.9%

# Compares the Poisson & OLS predicted distributions with the observed response distribution
test_pred_naive$fitted.values = ifelse(test_pred_naive$fitted.values < 0, 0, test_pred_naive$fitted.values)
ols_pred = as.numeric(unlist(lapply(test_pred_naive$fitted.values, FUN = round)))
poisson_pred = as.numeric(unlist(lapply(test_pred_0$fitted.values, FUN = round)))
table(prediction_data$MR)
table(poisson_pred)
table(ols_pred)

test_pred = glarma(Y, X, type = "NegBin", phiLags = c(1, 2), thetaLags = c(1, 2), phiInit = c(0.5, 0.5), thetaInit = c(0.25, 0.25), beta = rep(1, K), alphaInit = 1)

# For some reason, unable to use usual formula abbreviations in this command
names(prediction_data)[match("47.41", names(prediction_data))] = "subsection_47.41"
test_pred_2 = pglm(MR ~  subsection_47.41 + penaltypoints_47.41 + totalinjuries ,
                   prediction_data, na.action = na.omit, family = "negbin", effect = "time", model = "within")

######################################################################################################################################
