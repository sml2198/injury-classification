# NIOSH Project 2014-N-15776

# 14 - Data Spelunking

# Last edit 8/18/16

######################################################################################################

library(plyr)
library(minerva)
library(zoo)
library(reshape)

# input: part-level data
data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"

######################################################################################################

# DATA SET-UP

# read in data 
data = readRDS(data_file_name)

# rename variables
data = rename(data, c(coal_prod_qtr = "coal_prod", 
                      employment_qtr = "employment",
                      hours_qtr = "hours",
                      goodfaithind = "num_good_faith",
                      onsite_insp_hours_per_qtr = "onsite_insp_hours"))

names(data)[grep("^[0-9]", names(data))] = paste("p", names(data)[grep("^[0-9]", names(data))], sep = "")
names(data)[grep("[0-9].[0-9]", names(data))] = paste("s", names(data)[grep("[0-9].[0-9]", names(data))], sep = "")

# reformat variables
data$mineid = as.character(data$mineid)
data$quarter = as.numeric(data$quarter)
data$MR_indicator = as.factor(data$MR_indicator)

# group variables
violation_vars = names(data)[grep("^p+[0-9]|^sp+[0-9]", names(data))]
part_vars = violation_vars[grep("^p", violation_vars)]
subpart_vars = violation_vars[grep("^sp", violation_vars)]

sig_sub_vars = violation_vars[grep("sigandsub$", violation_vars)]
penalty_point_vars = violation_vars[grep("penaltypoints$", violation_vars)]
violation_count_vars = setdiff(violation_vars, union(sig_sub_vars, penalty_point_vars))

part_sig_sub_vars = intersect(part_vars, sig_sub_vars)
subpart_sig_sub_vars = intersect(subpart_vars, sig_sub_vars)
part_penalty_point_vars = intersect(part_vars, penalty_point_vars)
subpart_penalty_point_vars = intersect(subpart_vars, penalty_point_vars)
part_violation_count_vars = intersect(part_vars, violation_count_vars)
subpart_violation_count_vars = intersect(subpart_vars, violation_count_vars)

######################################################################################################

# QUARTERS WITH NO INSPECTIONS

summary(data$num_insp) # should have no NAs

# check if quarters with no inspections have non-NA violation data
no_insp = data[data$num_insp == 0, violation_vars]
no_insp_missing = sapply(no_insp, function(x)all(is.na(x)))
no_insp_missing[no_insp_missing != TRUE] # should have no non-NA violation data

rm(no_insp_missing)

# investigate quarters with no inspections
no_insp = data[data$num_insp == 0, c("mineid", "quarter")]

# how many quarters with no inspections per mine?
barplot(sort(table(no_insp$mineid)),
        xaxt = "n",
        main = "Number of No-Inspection Quarters by Mine", 
        xlab = "Mine ID",
        ylab = "Number of No-Inspection Quarters")

# mines with no inspections for >= 5 and >= 10 quarters of data
mineid_5plus_missing = unique(no_insp[no_insp$mineid %in% names(which(table(no_insp$mineid) >= 5)), "mineid"]) # 90 mines
mineid_10plus_missing = unique(no_insp[no_insp$mineid %in% names(which(table(no_insp$mineid) >= 10)), "mineid"]) # 28 mines

# how many (non-missing) quarters do these mines contribute to data?
nrow(data[(data$mineid %in% mineid_5plus_missing & data$num_insp > 0), ]) # 2228 obs
nrow(data[(data$mineid %in% mineid_10plus_missing & data$num_insp > 0), ]) # 585 obs

# which quarters are missing inspections
barplot(table(no_insp$quarter), 
        main = "Number of Mines with No Inspections by Quarter", 
        xlab = "Quarter", 
        ylab = "Number of Mines with No Inspections")

######################################################################################################

# NON-CONTINUITY OF TIME SERIES

# get missing mine-quarters trusting given bounds
fill_in_ts = function(mine_df) {
  times = data.frame(quarter = seq(min(mine_df$quarter), max(mine_df$quarter), by = 0.25))
  full_ts = merge(times, mine_df, by = c("quarter"), all.x = TRUE)
  full_ts$mineid[is.na(full_ts$mineid)] = unique(full_ts$mineid)[1]
  return(full_ts)
}

data_all_quarters = ddply(data, "mineid", fill_in_ts)

# get orphan quarters
orphans = data_all_quarters[is.na(data_all_quarters$hours), c("mineid", "quarter")]

# how many orphan quarters per mine?
barplot(sort(table(orphans$mineid)),
        xaxt = "n",
        main = "Number of Orphan Quarters by Mine", 
        xlab = "Mine ID",
        ylab = "Number of Orphan Quarters")

# which quarters are orphans?
barplot(table(orphans$quarter), 
        main = "Number of Mines with Orphans by Quarter", 
        xlab = "Quarter", 
        ylab = "Number of Mines with Orphans")

# do these quarters have violations data?

# what if we do not trust the given bounds?

######################################################################################################

# OUTLIERS

# take avgs by mine
mine_avgs = aggregate(data_all_quarters[, -match("quarter", names(data_all_quarters))], 
                       list(data_all_quarters$mineid), 
                       FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

for (var in part_violation_count_vars) { # sub in other groups of variables here
  plot(mine_avgs[, var], mine_avgs$MR, 
       main = paste("Average # of", var, "vs. Average # of MR Injuries", sep = " "),
       xlab = paste("Average # of", var, sep = " "),
       ylab = "Average # of MR Injuries")
}

# bye
rm(var)

# I'm hilarious
# fit = lm(as.numeric(mine_avgs$MR) ~ as.numeric(mine_avgs$mineid))
# summary(fit)
# plot(as.numeric(mine_avgs$mineid), mine_avgs$MR, 
     # main = "Does Mine ID (numeric) Predict Accidents?",
     # xlab = "Mine ID (NOT A FACTOR)", 
     # ylab = "# of Accidents")
# abline(fit, col = "red")
# rm(fit)

######################################################################################################

# TIME TRENDS

# take avgs by quarter
q_avgs = aggregate(data_all_quarters[, -match("mineid", names(data_all_quarters))], 
                   list(as.character(data_all_quarters$quarter)), 
                   FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

for (var in part_violation_count_vars) { # sub in other groups of variables here
  plot(q_avgs$quarter, q_avgs[, var],
       type = "l",
       main = paste("Average # of", var, "per Quarter", sep = " "),
       xlab = "Quarter",
       ylab = paste("Average # of", var, sep = " "))
}

# bye
rm(var)

######################################################################################################

# INFO IN EACH COLUMN

# create normalized dataset
data_norm = data

data_norm$mineid = 
  data_norm$quarter = 
  data_norm$MR = 
  data_norm$MR_indicator =  NULL

for (j in 1:ncol(data_norm)) {
  var = as.numeric(data_norm[, j])
  var = (var - min(var, na.rm = TRUE)) / (max(var, na.rm = TRUE) - min(var, na.rm = TRUE)) # normalize
  data_norm[, j] = var
}

# bye
rm(j, var)

# variance of each variable
var_var = data.frame(variable = character(ncol(data_norm)),
                     variance = numeric(ncol(data_norm)))

var_var$variable = names(data_norm)

for (i in 1:nrow(var_var)) {
  variable = var_var$variable[i]
  variance = var(as.numeric(data_norm[, variable]), na.rm = TRUE)
  var_var$variance[i] = variance
}

plot(var_var$variance, 
     main = "Normalized Variance",
     xlab = "Variable Index",
     ylab = "Normalized Variance")

var_var[order(var_var$variance, decreasing = TRUE)[1:10], ]

# bye
rm(i, variable, variance)

# correlation between variables
cor_violation_count = cor(data[, c(violation_count_vars, "MR")], use = "complete.obs")
cor_sig_sub = cor(data[, c(sig_sub_vars, "MR")], use = "complete.obs")
cor_penalty_point = cor(data[, c(penalty_point_vars, "MR")], use = "complete.obs")

# top correlations with MR
sort(cor_violation_count[, "MR"], decreasing = TRUE)[1:10]
sort(cor_sig_sub[, "MR"], decreasing = TRUE)[1:10]
sort(cor_penalty_point[, "MR"], decreasing = TRUE)[1:10]

# find big correlations
find_big_cor = function(df, cor_mat, r) {
  pairs = which(abs(cor_mat) > r, arr.ind = TRUE)
  
  for (i in 1:nrow(pairs)) {
    var1 = rownames(cor_mat)[pairs[i, ][1]]
    var2 = rownames(cor_mat)[pairs[i, ][2]]
    
    newrow = data.frame(var1, var2, cor_mat[var1, var2])
    df = rbind(df, newrow)
  }
  
  for (j in 1:nrow(df)) {
    df[j, 1:2] = sort(df[j, 1:2])
  }
  df = df[! duplicated(df[1:2]),]
  
  names(df) = c("var1", "var2", "r")
  
  df = df[df$r != 1, ]
  
  return(df)
}

big_cor_violation_count = data.frame(var1 = character(), 
                                     var2 = character(),
                                     r = numeric())
big_cor_sig_sub = big_cor_violation_count
big_cor_penalty_point = big_cor_violation_count

big_cor_violation_count = find_big_cor(big_cor_violation_count, cor_violation_count, 0.5)
big_cor_sig_sub = find_big_cor(big_cor_sig_sub, cor_sig_sub, 0.5)
big_cor_penalty_point = find_big_cor(big_cor_penalty_point, cor_penalty_point, 0.5)

big_cor_violation_count = big_cor_violation_count[order(big_cor_violation_count$r, decreasing = TRUE), ]
big_cor_sig_sub = big_cor_sig_sub[order(big_cor_sig_sub$r, decreasing = TRUE), ]
big_cor_penalty_point = big_cor_penalty_point[order(big_cor_penalty_point$r, decreasing = TRUE), ]

# maximal information coefficient (retired)
# variables = names(data)
# variables = variables[-match(c("quarter", "mineid", "MR_indicator", "MR"), variables)]

# mic = mine(data[, variables], y = data$MR, alpha = 0.7, na.rm = TRUE)

# this code from: https://www.r-bloggers.com/maximal-information-coefficient-part-ii/
# res = data.frame(MIC = c(mic$MIC))
# rownames(res) = rownames(mic$MIC)
# res$MIC_Rank = nrow(res) - rank(res$MIC, ties.method="first") + 1
# res = res[order(res$MIC_Rank),]
# res

######################################################################################################
