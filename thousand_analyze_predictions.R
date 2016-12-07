# NIOSH Project 2014-N-15776

# 24 - Analyze Predictions

# Last edit 11/14/16

######################################################################################################

library(foreign)

# set preferences
date = "15-15/"

injtype = "PS"
# injtype = "MR"

# define file names
folder = "X:/Projects/Mining/NIOSH/analysis/results/dta/"
root = paste0(folder, date, collapse = NULL)

for (year in c("2010", "2011", "2012", "2013", "2014")) {
  # load in model predictions - the "x" indicates it's bee saved as Stata 12 for hte "foreign" package will work
  rate_data_in_file_name = paste0(root, injtype, "_with_predictions_", year, ".dta", collapse = NULL)
  nonrate_data_in_file_name = paste0(root, injtype, "_with_predictions_non-rate_", year, ".dta", collapse = NULL)
  
  # KEY
  # null 1 - weak null
  # null 2 - strong null (total violations - 1 lag)
  # null 3 - strong null (total violations/onsite inspection hours - 1 lag)
  
  ######################################################################################################
  
  # LOAD DATA
  
  rate_data = read.dta(rate_data_in_file_name)
  nonrate_data = read.dta(nonrate_data_in_file_name)
  
  ######################################################################################################
  
  rate_data = rate_data[, c(names(rate_data)[grepl(injtype, names(rate_data))], "dv", "dv_indicator")]
  nonrate_data = nonrate_data[, c(names(nonrate_data)[grepl(injtype, names(nonrate_data))], "dv", "dv_indicator")]
  
  rate_bin = rate_data[, grepl("B", names(rate_data))]
  rate_count = rate_data[, grepl("C", names(rate_data))]
  nonrate_bin = nonrate_data[, grepl("B", names(nonrate_data))]
  nonrate_count = nonrate_data[, grepl("C", names(nonrate_data))]
  
  names(rate_bin) = paste(names(rate_bin), "rate", sep = "_")
  names(rate_count) = paste(names(rate_count), "rate", sep = "_")
  names(nonrate_bin) = paste(names(nonrate_bin), "nonrate", sep = "_")
  names(nonrate_count) = paste(names(nonrate_count), "nonrate", sep = "_")
  
  bin_data = cbind(rate_bin, nonrate_bin, rate_data$dv_indicator)
  count_data = cbind(rate_count, nonrate_count, rate_data$dv, rate_data$dv_indicator)
  
  bin_data = data.frame(ifelse(bin_data <= 0.5, 0, 1))
  count_as_bin_data = data.frame(ifelse(count_data <= 0.5, 0, 1))
  count_data = data.frame(round(count_data))
  
  names(bin_data) = c("B_1_VR", 
                      "B_4_VR",
                      "B_W_NULL",
                      "B_S_NULL_VC",
                      "B_S_NULL_VR",
                      "B_1_VC",
                      "B_4_VC",
                      "drop1",
                      "drop2",
                      "drop3",
                      "true")
  
  names(count_data) = c("C_1_VR", 
                        "C_4_VR",
                        "C_W_NULL",
                        "C_S_NULL_VC",
                        "C_S_NULL_VR",
                        "C_1_VC",
                        "C_4_VC",
                        "drop1",
                        "drop2",
                        "drop3",
                        "true_count", 
                        "true_indicator")
  
  names(count_as_bin_data) = names(count_data)
  
  bin_data = bin_data[, !grepl("drop", names(bin_data))]
  count_data = count_data[, !grepl("drop", names(count_data))]
  count_as_bin_data = count_as_bin_data[, !grepl("drop", names(count_as_bin_data))]
  
  bin_models = names(bin_data)[grepl("B", names(bin_data))]
  bin_model_stats = data.frame(bin_models)
  names(bin_model_stats) = "model"
  bin_model_stats$model = as.character(bin_model_stats$model)
  bin_model_stats$TP = NA
  bin_model_stats$TN = NA
  bin_model_stats$FP = NA
  bin_model_stats$FN = NA
  bin_model_stats$CCR = NA
  bin_model_stats$FPR = NA
  bin_model_stats$FNR = NA
  
  for (i in 1:nrow(bin_model_stats)) {
    m = bin_model_stats$model[i]
    temp = bin_data[!is.na(bin_data[, m]), c(m, "true")]
    bin_model_stats$TP[i] = TP = sum(temp[, m] == 1 & temp$true == 1)
    bin_model_stats$TN[i] = TN = sum(temp[, m] == 0 & temp$true == 0)
    bin_model_stats$FP[i] = FP = sum(temp[, m] == 1 & temp$true == 0)
    bin_model_stats$FN[i] = FN = sum(temp[, m] == 0 & temp$true == 1)
    bin_model_stats$CCR[i] = 100 * ((TP + TN) / (TP + TN + FP + FN))
    bin_model_stats$FPR[i] = 100 * (FP / (FP + TN))
    bin_model_stats$FNR[i] = 100 * (FN / (FN + TP))
  }
  
  count_models = names(count_data)[grepl("C", names(count_data))]
  count_model_stats = data.frame(count_models)
  names(count_model_stats) = "model"
  count_model_stats$model = as.character(count_model_stats$model)
  count_model_stats$TP = NA
  count_model_stats$TN = NA
  count_model_stats$FP = NA
  count_model_stats$FN = NA
  count_model_stats$CCR = NA
  count_model_stats$FPR = NA
  count_model_stats$FNR = NA
  count_model_stats$SSD = NA
  count_model_stats$SSPD = NA
  count_model_stats$SSND = NA
  
  for (i in 1:nrow(count_model_stats)) {
    m = count_model_stats$model[i]
    temp_c = count_data[!is.na(count_data[, m]), c(m, "true_count")]
    temp_b = count_as_bin_data[!is.na(count_as_bin_data[, m]), c(m, "true_indicator")]
    
    D = temp_c[, m] - temp_c$true_count
    PD = D[D > 0]
    ND = D[D < 0]
    count_model_stats$SSD[i] = sum(D^2) 
    count_model_stats$SSPD[i] = sum(PD^2)  
    count_model_stats$SSND[i] = sum(ND^2)
    
    count_model_stats$TP[i] = TP = sum(temp_b[, m] == 1 & temp_b$true_indicator == 1)
    count_model_stats$TN[i] = TN = sum(temp_b[, m] == 0 & temp_b$true_indicator == 0)
    count_model_stats$FP[i] = FP = sum(temp_b[, m] == 1 & temp_b$true_indicator == 0)
    count_model_stats$FN[i] = FN = sum(temp_b[, m] == 0 & temp_b$true_indicator == 1)
    count_model_stats$CCR[i] = 100 * ((TP + TN) / (TP + TN + FP + FN))
    count_model_stats$FPR[i] = 100 * (FP / (FP + TN))
    count_model_stats$FNR[i] = 100 * (FN / (FN + TP))
  }
  
  bin_models = names(bin_data)[grepl("B", names(bin_data)) & !grepl("NULL", names(bin_data))]
  bin_null_models = names(bin_data)[grepl("B", names(bin_data)) & grepl("NULL", names(bin_data))]
  bin_models = sort(rep(bin_models, 3))
  
  bin_out = data.frame(bin_models)
  names(bin_out) = "Model"
  bin_out$Model = as.character(bin_out$Model)
  bin_out$Measure = rep(c("CCR", "FPR", "FNR"), 4)
  bin_out[, bin_null_models] = NA
  
  for (j in 3:ncol(bin_out)) {
    null_model = names(bin_out)[j]
    for (i in 1:nrow(bin_out)) {
      model = bin_out$Model[i]
      measure = bin_out$Measure[i]
      pref = bin_model_stats[bin_model_stats$model == model, measure]
      null = bin_model_stats[bin_model_stats$model == null_model, measure]
      bin_out[i, j] = pref - null 
    }
  }
  
  count_models = names(count_data)[grepl("C", names(count_data)) & !grepl("NULL", names(count_data))]
  count_null_models = names(count_data)[grepl("C", names(count_data)) & grepl("NULL", names(count_data))]
  count_models = sort(rep(count_models, 6))
  
  count_out = data.frame(count_models)
  names(count_out) = "Model"
  count_out$Model = as.character(count_out$Model)
  count_out$Measure = rep(c("CCR", "FPR", "FNR", "SSD", "SSPD", "SSND"), 4)
  count_out[, count_null_models] = NA
  
  for (j in 3:ncol(count_out)) {
    null_model = names(count_out)[j]
    for (i in 1:nrow(count_out)) {
      model = count_out$Model[i]
      measure = count_out$Measure[i]
      pref = count_model_stats[count_model_stats$model == model, measure]
      null = count_model_stats[count_model_stats$model == null_model, measure]
      count_out[i, j] = pref - null 
    }
  }
  
  
  bin_out[, 3:ncol(bin_out)] = data.frame(round(bin_out[, 3:ncol(bin_out)], 2))
  count_out[, 3:ncol(count_out)] = data.frame(round(count_out[, 3:ncol(count_out)], 2))
  
  bin_model_stats = bin_model_stats[, c("model", "CCR", "FPR", "FNR")]
  bin_model_stats[, 2:ncol(bin_model_stats)] = data.frame(round(bin_model_stats[, 2:ncol(bin_model_stats)], 2))
  
  count_model_stats = count_model_stats[, c("model", "CCR", "FPR", "FNR", "SSD", "SSPD", "SSND")]
  count_model_stats[, 2:ncol(count_model_stats)] = data.frame(round(count_model_stats[, 2:ncol(count_model_stats)], 2))
  
  write.csv(bin_out, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Binary Diff Table ", injtype, year, ".csv", sep = ""), row.names = F)
  write.csv(count_out, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Count Diff Table ", injtype, year, ".csv", sep = ""), row.names = F)
  write.csv(bin_model_stats, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Binary Appendix Table ", injtype, year, ".csv", sep = ""), row.names = F)
  write.csv(count_model_stats, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Count Appendix Table ", injtype, year, ".csv", sep = ""), row.names = F)
  
}


rm(list = ls())

