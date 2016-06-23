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

prediction_data = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_47.rds")
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

# DEAL WITH MINE STATUS DATE
# if for a given minequarter the minestatus date is LESS than the quarter, then the observation should take on that minestatus.
# if for a given minequarter the minestatus date is GREATER than that quarter, and the minestatus is abandoned, then the observation should take on some other minestatus (probably active?).
prediction_data$minestatusdate <- as.Date(prediction_data$minestatusdate, "%m/%d/%Y")
prediction_data$statusquarter = as.yearqtr(prediction_data$minestatusdate)
prediction_data$minestatus = ifelse((prediction_data$statusquarter >= prediction_data$quarter) 
                                    & (prediction_data$minestatus == 1 | prediction_data$minestatus == 2), 8, prediction_data$minestatus)
prediction_data = prediction_data[, c(-grep("statusquarter", names(prediction_data)))]

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

#Run variable selection over CFR subsection codes
#"terminated" is a count of all citations that have been terminated by MSHA for a mine. This reflects a mine's past citations but also its ability to
#improve its safety conditions. We may form terminated/total_violations by mine-qtr in the future but will remain agnostic as of now.
mine_faults = c("total_violations", "contractor_repeated_viol_cnt", "totalinjuries", "operator_repeated_viol_pInspDay", "terminated")
inspec_exp = c("insp_hours_per_qtr", "onsite_insp_hours_per_qtr", "num_insp")
inj_exp = c("productionshiftsperday", "coal_prod_qtr", "employment_qtr", "hours_qtr")
mine_penpoints = c("contractorsizepoints", "controllersizepoints", "minesizepoints")

model_sel_quant = cbind(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+", names(prediction_data))], 
                        prediction_data[, c(-grep("^[0-9][0-9]", names(prediction_data)), -grep("mineid", names(prediction_data)),
                                             -match("quarter", names(prediction_data)), -match("minename", names(prediction_data)),
                                             -match("minestatusdate", names(prediction_data)), -match("operatorid", names(prediction_data)),
                                             -match("operatorname", names(prediction_data)), -match("stateabbreviation", names(prediction_data)),
                                             -match("idate", names(prediction_data)), -match("MR", names(prediction_data)), -match("year", names(prediction_data)),
                                             -match("idesc", names(prediction_data)), -match("minestatus", names(prediction_data)))])
pca_results = PCA(prediction_data[, grep("^[0-9][0-9]\\.[0-9]+", names(prediction_data))], graph = F)
pca_inj_exp = PCA(prediction_data[,unlist(lapply(inj_exp, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_inspec_exp = PCA(prediction_data[,unlist(lapply(inspec_exp, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_mine_faults = PCA(prediction_data[,unlist(lapply(mine_faults, FUN = function(x) match(x, names(prediction_data))))], graph = F)
pca_mine_penpoints = PCA(prediction_data[,unlist(lapply(mine_penpoints, FUN = function(x) match(x, names(prediction_data))))], graph = F)

#UNDER CONSTRUCTION - Tool for extracting significant variables from PCA analysis
imp_vars = list()
a = list()
for (i in 1:2) {
  imp_vars[[i]] = test$var$contrib[test$var$contrib[,i] >= sqrt(1/308),i]
  if (i > 1) {
    a = intersect(a, names(unlist(imp_vars[i])))
  } else {
    a = intersect(names(unlist(imp_vars[i])), names(unlist(imp_vars[i+1])))
  }
  print(a)
}

#ANALYZE PCA RESULTS
#Now use pca_results$var$contrib[,j] j = 1, 2, ..., K to access the jth principal component for the ith CFR part code. Take absolute values before analyzing.
#Use plot.PCA(pca_results, choix = "var"/"ind") to view correlation circle plot/individual factor map and summary.PCA(pca_results) for Kaiser-Guttman test.

#Exploring MCA - NOT USED

#mca_results = MCA(as.data.frame(sapply(prediction_data[, c(match("minestatus", names(prediction_data)), grep("idesc", names(prediction_data)))], FUN = factor)))
#summary.MCA(mca_results)

#INSERT MFA CODE HERE (TESTING); There is an obscure error thrown with this code. NOT USED
#mfa_results = MFA(data, group = c(81, 2, 13), type = c("c", "n", "c"), name.group = c("quant1", "quali1", "quant2"))

######################################################################################################################################
# EVERYTHING BELOW THIS LINE IS FOR THE ALGORITHM

rm(multi_qtr_inspcs, mines_accidents_coded, summed_coded_accidents, summed_violations, summed_inspcs, averaged_violations)
#To provide an intercept for the prediction stage:
prediction_data$constant = 1
#WARNING: Fails to converge with these initial values
N = nrow(prediction_data)
K = ncol(prediction_data) - 3
X = as.matrix(prediction_data[, c(-grep("MR", names(prediction_data)), -grep("mineid", names(prediction_data)), -grep("quarter", names(prediction_data)))])
Y = as.vector(prediction_data$MR)

test_pred = glarma(Y, X, type = "NegBin", phiLags = c(1, 2), thetaLags = c(1, 2), phiInit = c(0.5, 0.5), thetaInit = c(0.25, 0.25), beta = rep(1, K), alphaInit = 1)
#For some reason, unable to use usual formula abbreviations in this command
names(prediction_data)[match("47.41", names(prediction_data))] = "subsection_47.41"
test_pred_2 = pglm(MR ~  subsection_47.41 + penaltypoints_47.41 + totalinjuries ,
                   prediction_data, na.action = na.omit, family = "negbin", effect = "time", model = "within")
