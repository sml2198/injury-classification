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

# read in data 
data = readRDS(data_file_name)

# rename variables
data = rename(data, c(coal_prod_qtr = "coal_prod", 
                      employment_qtr = "employment",
                      hours_qtr = "hours",
                      goodfaithind = "num_good_faith",
                      onsite_insp_hours_per_qtr = "onsite_insp_hours"))

names(data)[grep("^[0-9]", names(data))] = paste("p", names(data)[grep("^[0-9]", names(data))], sep = "")

violation_vars = names(data)[grep("^p+[0-9]", names(data))]
subpart_vars = violation_vars[grep("\.{3, }", violation_vars)]
part_vars = setdiff(violation_vars, subpart_vars)


names(data)[grep("^p[0-9](.)[0-9]", names(data))] = paste("s", names(data)[grep("^p[0-9](.)[0-9]", names(data))], sep = "")

# reformat variables
data$mineid = as.character(data$mineid)
data$quarter = as.numeric(data$quarter)
data$MR_indicator = as.factor(data$MR_indicator)

# group variables
violation_vars = names(data)[grep("^p[0-9]", names(data))]
subpart_vars = violation_vars[grep("^p+[0-9](.)[0-9]", violation_vars)]
part_vars = setdiff(violation_vars, subpart_vars)


sig_sub_vars = violation_vars[grep("sigandsub$", violation_vars)]
penalty_point_vars = violation_vars[grep("penaltypoints$", violation_vars)]
violation_count_vars = setdiff(violation_vars, union(sig_sub_vars, penalty_point_vars))

part_sig_sub_vars = intersect(part_vars, sig_sub_vars)
subpart_sig_sub_vars = intersect(subpart_vars, sig_sub_vars)

part_penalty_point_vars = intersect(part_vars, penalty_point_vars)
subpart_penalty_point_vars = intersect(subpart_vars, penalty_point_vars)

part_violation_count_vars = intersect(part_vars, part_violation_count_vars)
subpart_violation_count_vars = intersect(subpart_vars, part_violation_count_vars)


# rename variable because R is a lil bitch



######################################################################################################

# QUARTERS WITH NO INSPECTIONS

# set violation data to NA if there are no inspections
make_NA = function(var, data, data_name) {
  data[, var] = ifelse(data[, "num_insp"] != 0, data[, var], NA)
  assign(data_name, data, .GlobalEnv) 
}

violation_vars = names(data)[grep("^p[0-9][0-9]", names(data))]
violation_vars = c(violation_vars, 
                   "total_violations", 
                   "insp_hours_per_qtr", 
                   "onsite_insp_hours_per_qtr")
for (var in violation_vars) {
  make_NA(var, data, "data")
}

# bye
rm(violation_vars, var)

# investigate quarters with no inspections
no_insp = data[data$num_insp == 0, c("mineid", "quarter")]

# how many quarters with no inspections per mine?
barplot(sort(table(no_insp$mineid)),
        xaxt = "n",
        main = "Number of No-Inspection Quarters by Mine", 
        xlab = "Mine ID",
        ylab = "Number of No-Inspection Quarters")

# mines with no inspections for >= 5 quarters of data
mineid_5plus_missing = unique(no_insp[no_insp$mineid %in% names(which(table(no_insp$mineid) >= 5)), "mineid"]) # 27 mines

# how many (non-missing) quarters do these mines contribute to data?
dim(data[(data$mineid %in% mineid_5plus_missing & data$num_insp > 0), ]) # 2228 obs

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
orphans = data_all_quarters[is.na(data_all_quarters$hours_qtr), c("mineid", "quarter")]

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

# get missing quarters using fixed min/max
# temp = expand.grid(mineid = unique(data$mineid), quarter = unique(data$quarter))
# temp = temp[order(temp$mineid, temp$quarter), ]
# data_all_quarters_bigger = merge(data, temp, all.x = TRUE, all.y = TRUE, by = c("mineid", "quarter"))

# bye
# rm(temp)

######################################################################################################

# OUTLIERS

# take avgs by mine
mine_avgs = aggregate(data_all_quarters[, -match("quarter", names(data_all_quarters))], 
                       list(data_all_quarters$mineid), 
                       FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

variables = c("p47", 
              "p47.likelihood.pts", "p47.likelihood.pts.con",
              "p47.negligence.pts", "p47.negligence.pts.con",
              "p47.injuryillness.pts", "p47.injuryillness.pts.con",
              "p48", 
              "p48.likelihood.pts", "p48.likelihood.pts.con",
              "p48.negligence.pts", "p48.negligence.pts.con",
              "p48.injuryillness.pts", "p48.injuryillness.pts.con",
              "p71", 
              "p71.likelihood.pts", "p71.likelihood.pts.con",
              "p71.negligence.pts", "p71.negligence.pts.con",
              "p71.injuryillness.pts", "p71.injuryillness.pts.con",
              "p72", 
              "p72.likelihood.pts", "p72.likelihood.pts.con",
              "p72.negligence.pts", "p72.negligence.pts.con",
              "p72.injuryillness.pts", "p72.injuryillness.pts.con",
              "p75", 
              "p75.likelihood.pts", "p75.likelihood.pts.con",
              "p75.negligence.pts", "p75.negligence.pts.con",
              "p75.injuryillness.pts", "p75.injuryillness.pts.con",
              "p77", 
              "p77.likelihood.pts", "p77.likelihood.pts.con",
              "p77.negligence.pts", "p77.negligence.pts.con",
              "p77.injuryillness.pts", "p77.injuryillness.pts.con")

for (var in variables) {
  plot(mine_avgs[, var], mine_avgs$MR, 
       main = paste("Average # of", var, "vs. Average # of MR Injuries", sep = " "),
       xlab = paste("Average # of", var, sep = " "),
       ylab = "Average # of MR Injuries")
}

# bye 
rm(var, variables)

# I'm hilarious
fit = lm(as.numeric(mine_avgs$MR) ~ as.numeric(mine_avgs$mineid))
summary(fit)
plot(as.numeric(mine_avgs$mineid), mine_avgs$MR, 
     main = "Does Mine ID (numeric) Predict Accidents?",
     xlab = "Mine ID (NOT A FACTOR)", 
     ylab = "# of Accidents")
abline(fit, col = "red")

# bye
rm(fit)

######################################################################################################

# TIME TRENDS

# take avgs by quarter
q_avgs = aggregate(data_all_quarters[, -match("mineid", names(data_all_quarters))], 
                   list(as.character(data_all_quarters$quarter)), 
                   FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

variables = c("p47", 
              "p47.likelihood.pts", "p47.likelihood.pts.con",
              "p47.negligence.pts", "p47.negligence.pts.con",
              "p47.injuryillness.pts", "p47.injuryillness.pts.con",
              "p48", 
              "p48.likelihood.pts", "p48.likelihood.pts.con",
              "p48.negligence.pts", "p48.negligence.pts.con",
              "p48.injuryillness.pts", "p48.injuryillness.pts.con",
              "p71", 
              "p71.likelihood.pts", "p71.likelihood.pts.con",
              "p71.negligence.pts", "p71.negligence.pts.con",
              "p71.injuryillness.pts", "p71.injuryillness.pts.con",
              "p72", 
              "p72.likelihood.pts", "p72.likelihood.pts.con",
              "p72.negligence.pts", "p72.negligence.pts.con",
              "p72.injuryillness.pts", "p72.injuryillness.pts.con",
              "p75", 
              "p75.likelihood.pts", "p75.likelihood.pts.con",
              "p75.negligence.pts", "p75.negligence.pts.con",
              "p75.injuryillness.pts", "p75.injuryillness.pts.con",
              "p77", 
              "p77.likelihood.pts", "p77.likelihood.pts.con",
              "p77.negligence.pts", "p77.negligence.pts.con",
              "p77.injuryillness.pts", "p77.injuryillness.pts.con",
              "MR")

for (var in variables) {
  plot(q_avgs$quarter, q_avgs[, var],
       type = "l",
       main = paste("Average # of", var, "per Quarter", sep = " "),
       xlab = "Quarter",
       ylab = paste("Average # of", var, sep = " "))
}

# bye 
rm(var, variables)

# mine size differences
small_mines = mine_avgs[which(mine_avgs$hours_qtr < 60000), "mineid"]
big_mines = mine_avgs[which(mine_avgs$hours_qtr >= 60000), "mineid"]

q_avgs_small = aggregate(data_all_quarters[data_all_quarters$mineid %in% small_mines, -match("mineid", names(data_all_quarters))], 
                         list(as.character(data_all_quarters[data_all_quarters$mineid %in% small_mines, "quarter"])), 
                         FUN = function(x) mean(as.numeric(as.character(x)), na.rm = TRUE))

q_avgs_big = aggregate(data_all_quarters[data_all_quarters$mineid %in% big_mines, -match("mineid", names(data_all_quarters))], 
                       list(as.character(data_all_quarters[data_all_quarters$mineid %in% big_mines, "quarter"])), 
                       FUN = function(x) mean(as.numeric(as.character(x)), na.rm = TRUE))

plot(q_avgs_big$quarter, q_avgs_big$total_violations,
     type = "l",
     col = "red",
     main = "Average # of total_violations per Quarter by mine size",
     xlab = "Quarter",
     ylab = "Average # of total_violations",
     ylim = c(0, 110))
lines(q_avgs_big$quarter, q_avgs_small$total_violations, col = "blue")

# individual mines - TBC
# upper big branch mineid = "4608436"
data_all_quarters$quarter = as.yearqtr(data_all_quarters$quarter)
UBB = data_all_quarters[data_all_quarters$mineid == "4608436", c("quarter", "MR", "totalinjuries")]
UBB = ts(UBB$MR, start = c(2000, 1), end = c(2012, 2), frequency = 4)
# plot(UBB)

# clean up yo mess
data_all_quarters$quarter = as.numeric(data_all_quarters$quarter)

######################################################################################################

# INFO IN EACH COLUMN

# plot all variables
variables = names(data_all_quarters)
variables = variables[-match(c("quarter", "mineid", "MR", "MR_indicator"), variables)]

for (var in variables) {
  plot(sort(data_all_quarters[, var]), 
       main = paste("Sorted", var, sep = " "), 
       ylab = var)
}

# bye
rm(var)

# create normalized dataset
data_all_quarters_norm = data_all_quarters

data_all_quarters_norm$mineid = 
  data_all_quarters_norm$quarter = 
  data_all_quarters_norm$MR = 
  data_all_quarters_norm$MR_indicator =  NULL

for (j in 1:ncol(data_all_quarters_norm)) {
  var = as.numeric(data_all_quarters_norm[, j])
  var = (var - min(var, na.rm = TRUE)) / (max(var, na.rm = TRUE) - min(var, na.rm = TRUE)) # normalize
  data_all_quarters_norm[, j] = var
}

# bye
rm(j, var)

# variance of each variable
var_var = data.frame(variable = character(ncol(data_all_quarters_norm)),
                     variance = numeric(ncol(data_all_quarters_norm)))

var_var$variable = names(data_all_quarters_norm)

for (i in 1:nrow(var_var)) {
  variable = var_var$variable[i]
  variance = var(as.numeric(data_all_quarters_norm[, variable]), na.rm = TRUE)
  var_var$variance[i] = variance
}

var_var[order(var_var$variance, decreasing = TRUE)[1:10], ]

plot(var_var$variance, 
     main = "Normalized Variance",
     xlab = "Variable Index",
     ylab = "Normalized Variance")

# bye
rm(i, variable, variance)

# correlation between all variables
data_all_quarters$no_terminations = as.numeric(data_all_quarters$no_terminations)
variables = names(data_all_quarters)
variables = variables[-match(c("quarter", "mineid", "MR_indicator"), variables)]
cor_mat = cor(data_all_quarters[, variables], use = "complete.obs")

# bye
rm(variables)

# top correlations with MR
sort(cor_mat[, "MR"], decreasing = TRUE)[1:10]

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

big_cor = data.frame(var1 = character(), 
                     var2 = character(),
                     r = numeric())

big_cor = find_big_cor(big_cor, cor_mat, 0.5)
big_cor = big_cor[order(big_cor$r, decreasing = TRUE), ]

# maximal information coefficient
variables = names(data_all_quarters)
variables = variables[-match(c("quarter", "mineid", "MR_indicator", "MR"), variables)]

mic = mine(data_all_quarters[, variables], y = data_all_quarters$MR, alpha = 0.7, na.rm = TRUE)

# this code from: https://www.r-bloggers.com/maximal-information-coefficient-part-ii/
res = data.frame(MIC = c(mic$MIC))
rownames(res) = rownames(mic$MIC)
res$MIC_Rank = nrow(res) - rank(res$MIC, ties.method="first") + 1
res = res[order(res$MIC_Rank),]
res

# I'll fix it on Monday
mic_norm = mine(data_all_quarters_norm[, variables], y = data_all_quarters_norm$MR, alpha = 0.7, na.rm = TRUE)

res_norm = data.frame(MIC = c(mic_norm$MIC))
rownames(res_norm) = rownames(mic_norm$MIC)
res_norm$MIC_Rank = nrow(res_norm) - rank(res_norm$MIC, ties.method="first") + 1
res_norm = res_norm[order(res_norm$MIC_Rank),]
res_norm

######################################################################################################
