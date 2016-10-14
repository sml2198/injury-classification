# NIOSH Project 2014-N-15776

# 19 - Big Mines / Bad Mines

# Last edit 10/13/16

######################################################################################################

injury = "MR"
# injury = "PS"

if (injury == "MR") {
  data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"
}

######################################################################################################

# DATA SET-UP

# read in data 
data = readRDS(data_file_name)

# drop lagged variables
keep_vars = names(data)[!grepl("lag", names(data))]
data = data[keep_vars]

# generalize injury variables
data$dv = data[, injury]
data$dv_indicator = data[, paste(injury, "_indicator", sep = "")]
data[, injury] = data[, paste(injury, "_indicator", sep = "")] = NULL

# group variables
violation_vars = names(data)[grep("^p+[0-9]|^sp+[0-9]", names(data))]
part_vars = violation_vars[grep("^p", violation_vars)]
sig_sub_vars = violation_vars[grep("sigandsub$", violation_vars)]
penalty_point_vars = violation_vars[grep("penaltypoints$", violation_vars)]
violation_count_vars = setdiff(violation_vars, union(sig_sub_vars, penalty_point_vars))
part_violation_count_vars = intersect(part_vars, violation_count_vars)

# generate total relevant violations variable
data$total_relevant_violations = rowSums(data[, part_violation_count_vars], na.rm  = TRUE)

# bye
data = data[, c("quarter", "mineid",
                "hours", 
                "total_relevant_violations", "total_mine_act_violations", "total_violations", 
                "total_injuries", "dv", "dv_indicator")]

######################################################################################################

# GRAPHS OF ALL MINES 

plot(data$quarter[data$mineid == "0100851"], data$hours[data$mineid == "0100851"],
     main = "Hours For Each Mine Over Time", 
     xlab = "Quarter", 
     ylab = "Hours",
     type = "l", ylim = c(0, max(data$hours, na.rm = TRUE)))
for (id in unique(data$mineid)) {
  lines(data$quarter[data$mineid == id], data$hours[data$mineid == id], type = "l")
}

plot(data$quarter[data$mineid == "0100851"], data$total_relevant_violations[data$mineid == "0100851"],
     main = "Total Relevant Violations For Each Mine Over Time", 
     xlab = "Quarter", 
     ylab = "Total Relevant Violations",
     type = "l", ylim = c(0, max(data$total_relevant_violations, na.rm = TRUE)))
for (id in unique(data$mineid)) {
  lines(data$quarter[data$mineid == id], data$total_relevant_violations[data$mineid == id], type = "l")
}

plot(data$quarter[data$mineid == "0100851"], data$total_violations[data$mineid == "0100851"],
     main = "Total Violations For Each Mine Over Time", 
     xlab = "Quarter", 
     ylab = "Total Violations",
     type = "l", ylim = c(0, max(data$total_violations, na.rm = TRUE)))
for (id in unique(data$mineid)) {
  lines(data$quarter[data$mineid == id], data$total_violations[data$mineid == id], type = "l")
}

######################################################################################################

# COLLAPSE MINE DIMENSION

# avg by mine
mine_avg = aggregate(data[, -match("quarter", names(data))], 
                      list(data$mineid), 
                      FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

# min by mine
mine_min = aggregate(data[, -match("quarter", names(data))], 
                      list(data$mineid), 
                      FUN = function(x) min(as.numeric(x), na.rm = TRUE))

# max by mine
mine_max = aggregate(data[, -match("quarter", names(data))], 
                      list(data$mineid), 
                      FUN = function(x) max(as.numeric(x), na.rm = TRUE))

# median by mine
mine_median = aggregate(data[, -match("quarter", names(data))], 
                      list(data$mineid), 
                      FUN = function(x) median(as.numeric(x), na.rm = TRUE))

######################################################################################################

# HOURS

plot(sort(mine_avg$hours), main = "Avg Hours", xlab = "Sorted Index", ylab = "Avg Hours")
plot(sort(mine_min$hours), main = "Min Hours", xlab = "Sorted Index", ylab = "Min Hours")
plot(sort(mine_max$hours), main = "Max Hours", xlab = "Sorted Index", ylab = "Max Hours")
plot(sort(mine_median$hours), main = "Median Hours", xlab = "Sorted Index", ylab = "Median Hours")

avg_hours = mine_avg$mineid[mine_avg$hours >= quantile(mine_avg$hours, probs = seq(0, 1, 0.05))[20]]
min_hours = mine_min$mineid[mine_min$hours >= quantile(mine_min$hours, probs = seq(0, 1, 0.05))[20]]
max_hours = mine_max$mineid[mine_max$hours >= quantile(mine_max$hours, probs = seq(0, 1, 0.05))[20]]
median_hours = mine_median$mineid[mine_median$hours >= quantile(mine_median$hours, probs = seq(0, 1, 0.05))[20]]

length(setdiff(avg_hours, min_hours)) # 42
length(setdiff(avg_hours, max_hours)) # 8 
length(setdiff(avg_hours, median_hours)) # 4
length(setdiff(min_hours, max_hours)) # 45
length(setdiff(min_hours, median_hours)) # 43
length(setdiff(max_hours, median_hours)) # 9

######################################################################################################

# RELEVANT VIOLATIONS

plot(sort(mine_avg$total_relevant_violations), main = "Avg Relevant Violations", xlab = "Sorted Index", ylab = "Avg Relevant Violations")
plot(sort(mine_min$total_relevant_violations), main = "Min Relevant Violations", xlab = "Sorted Index", ylab = "Min Relevant Violations")
plot(sort(mine_max$total_relevant_violations), main = "Max Relevant Violations", xlab = "Sorted Index", ylab = "Max Relevant Violations")
plot(sort(mine_median$total_relevant_violations), main = "Median Relevant Violations", xlab = "Sorted Index", ylab = "Median Relevant Violations")

avg_rel_violations = mine_avg$mineid[mine_avg$total_relevant_violations >= quantile(mine_avg$total_relevant_violations, probs = seq(0, 1, 0.05))[20]]
min_rel_violations = mine_min$mineid[mine_min$total_relevant_violations >= quantile(mine_min$total_relevant_violations, probs = seq(0, 1, 0.05))[20]]
max_rel_violations = mine_max$mineid[mine_max$total_relevant_violations >= quantile(mine_max$total_relevant_violations, probs = seq(0, 1, 0.05))[20]]
median_rel_violations = mine_median$mineid[mine_median$total_relevant_violations >= quantile(mine_median$total_relevant_violations, probs = seq(0, 1, 0.05))[20]]

length(setdiff(avg_rel_violations, min_rel_violations)) # 56
length(setdiff(avg_rel_violations, max_rel_violations)) # 16
length(setdiff(avg_rel_violations, median_rel_violations)) # 6
length(setdiff(min_rel_violations, max_rel_violations)) # 70
length(setdiff(min_rel_violations, median_rel_violations)) # 66
length(setdiff(max_rel_violations, median_rel_violations)) # 21

######################################################################################################

# ALL VIOLATIONS

plot(sort(mine_avg$total_violations), main = "Avg Total Violations", xlab = "Sorted Index", ylab = "Avg Total Violations")
plot(sort(mine_min$total_violations), main = "Min Total Violations", xlab = "Sorted Index", ylab = "Min Total Violations")
plot(sort(mine_max$total_violations), main = "Max Total Violations", xlab = "Sorted Index", ylab = "Max Total Violations")
plot(sort(mine_median$total_violations), main = "Median Total Violations", xlab = "Sorted Index", ylab = "Median Total Violations")

avg_all_violations = mine_avg$mineid[mine_avg$total_violations >= quantile(mine_avg$total_violations, probs = seq(0, 1, 0.05), na.rm = TRUE)[20]]
min_all_violations = mine_min$mineid[mine_min$total_violations >= quantile(mine_min$total_violations, probs = seq(0, 1, 0.05), na.rm = TRUE)[20]]
max_all_violations = mine_max$mineid[mine_max$total_violations >= quantile(mine_max$total_violations, probs = seq(0, 1, 0.05), na.rm = TRUE)[20]]
median_all_violations = mine_median$mineid[mine_median$total_violations >= quantile(mine_median$total_violations, probs = seq(0, 1, 0.05), na.rm = TRUE)[20]]

length(setdiff(avg_all_violations, min_all_violations)) # 59
length(setdiff(avg_all_violations, max_all_violations)) # 15
length(setdiff(avg_all_violations, median_all_violations)) # 9
length(setdiff(min_all_violations, max_all_violations)) # 66
length(setdiff(min_all_violations, median_all_violations)) # 60
length(setdiff(max_all_violations, median_all_violations)) # 23

######################################################################################################
