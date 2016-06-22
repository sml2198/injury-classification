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

#prediction_data = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_47.rds")
prediction_data = readRDS("X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/prediction_data.rds")

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

#Run variable selection by CFR part/subsection code

#Run this line only if analyzing by CFR subsection code
cfr_codes = names(prediction_data)[grep("^[0-9][0-9]\\.[0-9]+$", names(prediction_data))]

#FILL IN MISSING VALUES OF MINE CHARACTERISTICS BY MINE_ID/QUARTER GROUPS
prediction_data = group_by(prediction_data, mineid, quarter)
prediction_data = prediction_data[order(prediction_data$mineid, prediction_data$quarter, na.last = T),]
prediction_data$minename = na.locf(prediction_data$minename)
prediction_data$minename = na.locf(prediction_data$minesizepoints)
prediction_data$minename = na.locf(prediction_data$controllersizepoints)
prediction_data$minename = na.locf(prediction_data$contractorsizepoints)
prediction_data$minename = na.locf(prediction_data$hours_qtr)
prediction_data$minename = na.locf(prediction_data$employment_qtr)
prediction_data$minename = na.locf(prediction_data$coal_prod_qtr)

# FIRST PIPE IN ZEROES TO THE MISSING PART-SPECIFIC VARIABLES (IF NOTHING MERGED ON A MINE QUARTER THAN IT SHOULD BE A ZERO)
MR_relevant_partcodes = names(prediction_data)[grep("^[0-9][0-9]\\.", names(prediction_data))]
partcodes_data = prediction_data[, c(grep("^[0-9][0-9]", names(prediction_data)), match("mineid", names(prediction_data)),
                               match("quarter", names(prediction_data)))]
prediction_data = prediction_data[, c(-grep("^[0-9][0-9]", names(prediction_data)))]
partcodes_data[is.na(partcodes_data)] = 0
prediction_data = merge(prediction_data, partcodes_data, by = c("mineid", "quarter"), all = T)
rm(partcodes_data)

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

# BEGIN MODEL SELECTION ALGORITHMS
pca_loadings = list()
for (i in 1:length(cfr_codes)) {
  #Until we determine a satisfactory way to handle qualitative variables with many bins we will omit them as they are not essential to prediction yet  
  model_sel_data = cbind(prediction_data[, grep(paste("^", cfr_codes[i], sep = ""), names(prediction_data))], 
                         prediction_data[, c(-grep("^[0-9][0-9]", names(prediction_data)), -grep("mineid", names(prediction_data)),
                                             -match("quarter", names(prediction_data)), -match("minename", names(prediction_data)),
                                             -match("minestatusdate", names(prediction_data)), -match("operatorid", names(prediction_data)),
                                             -match("operatorname", names(prediction_data)), -match("stateabbreviation", names(prediction_data)),
                                             -match("idate", names(prediction_data)), -match("MR", names(prediction_data)),
                                             -match("idesc", names(prediction_data)), -match("minestatus", names(prediction_data)))])
  #run model selection algorithm using model_sel_data and store output in whichever way is necessary. e.g., PCA
  #model_sel_data must be completely numeric and have no missing values before the next step is executed. Neither is currently true.
  pca_loadings[[i]] = princomp(model_sel_data[complete.cases(model_sel_data),])$loadings
}

#ANALYZE PCA RESULTS
#Now use pca_loadings[[i]][,j] j = 1, 2, ..., K to access the jth principal component for the ith CFR part code. Take absolute values before analyzing.

#Exploring MCA

mca_results = MCA(as.data.frame(sapply(model_sel_data[, c(grep("minestatus", names(model_sel_data)), grep("idesc", names(model_sel_data)),
                                                          grep("stateabbreviation", names(model_sel_data)))], FUN = factor)))
summary.MCA(mca_results)

#INSERT MFA CODE HERE

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

#Model Selection
#PCA
pca_output = prcomp(na.omit(X))
pca_results = pca_output$rotation

test_pred = glarma(Y, X, type = "NegBin", phiLags = c(1, 2), thetaLags = c(1, 2), phiInit = c(0.5, 0.5), thetaInit = c(0.25, 0.25), beta = rep(1, K), alphaInit = 1)
#For some reason, unable to use usual formula abbreviations in this command
names(prediction_data)[match("47.41", names(prediction_data))] = "subsection_47.41"
test_pred_2 = pglm(MR ~  subsection_47.41 + penaltypoints_47.41 + totalinjuries ,
                   prediction_data, na.action = na.omit, family = "negbin", effect = "time", model = "within")
