# NIOSH Project 2014-N-15776

# 19 - The Big Bad Mines

# Last edit 10/17/16

######################################################################################################

injury = "MR"
# injury = "PS"

if (injury == "MR") {
  data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"
}

big_check_out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Big and Bad Mine Classification/Big_Check.txt"
bad_check_out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Big and Bad Mine Classification/Bad_Check.txt"

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

# keep only useful variables
data = data[, c("quarter", "mineid", "mine_time",
                "hours", 
                "total_relevant_violations", "total_mine_act_violations", "total_violations", 
                "total_injuries", "dv", "dv_indicator")]

# bye
rm(keep_vars, part_vars, violation_vars, part_violation_count_vars, 
   penalty_point_vars, sig_sub_vars, violation_count_vars)

######################################################################################################

# CREATE SUMMARY DATAFRAME

summary = data.frame(unique(data$mineid))
names(summary) = "mineid"
summary$mineid = as.character(summary$mineid)

######################################################################################################

# HOW BIG?

avg_all = aggregate(data[, -match("quarter", names(data))], 
                      list(data$mineid), 
                      FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
max_all = aggregate(data[, -match("quarter", names(data))], 
                      list(data$mineid), 
                      FUN = function(x) max(as.numeric(x), na.rm = TRUE))
median_all = aggregate(data[, -match("quarter", names(data))], 
                      list(data$mineid), 
                      FUN = function(x) median(as.numeric(x), na.rm = TRUE))

for (dataset in c("avg_all", "max_all", "median_all")) {
  
  d = eval(parse(text = dataset))
  
  temp90 = d$mineid[d$hours >= quantile(d$hours, probs = seq(0, 1, 0.05), na.rm = TRUE)[19]]
  temp95 = d$mineid[d$hours >= quantile(d$hours, probs = seq(0, 1, 0.05), na.rm = TRUE)[20]]
  temp975 = d$mineid[d$hours >= quantile(d$hours, probs = seq(0, 1, 0.025, na.rm = TRUE))[40]]

  assign(paste(paste(paste(dataset, "hours", sep = "_"), 90, sep = "_")), temp90)
  assign(paste(paste(paste(dataset, "hours", sep = "_"), 95, sep = "_")), temp95)
  assign(paste(paste(paste(dataset, "hours", sep = "_"), 975, sep = "_")), temp975)
  
  temp_summary = data.frame(summary$mineid)
  names(temp_summary) = "mineid"
  temp_summary$mineid = as.character(temp_summary$mineid)
  
  temp_summary$temp90 = ifelse(temp_summary$mineid %in% temp90, 1, 0)
  temp_summary$temp95 = ifelse(temp_summary$mineid %in% temp95, 1, 0)
  temp_summary$temp975 = ifelse(temp_summary$mineid %in% temp975, 1, 0)
  
  temp_summary$mineid = NULL
  
  names(temp_summary) = c(paste(paste("hours_", dataset, sep = ""), "_90", sep = ""), 
                          paste(paste("hours_", dataset, sep = ""), "_95", sep = ""), 
                          paste(paste("hours_", dataset, sep = ""), "_975", sep = ""))
  
  summary = data.frame(summary, temp_summary)

}

# bye
rm(d, dataset, temp_summary, temp90, temp95, temp975)

######################################################################################################

# HOW BAD?

for (i in seq(2, 8, 2)) {
  
  data_temp = data[data$mine_time <= i, ]

  assign(paste("avg_", i, sep = ""),
         aggregate(data_temp[, -match("quarter", names(data_temp))], 
                   list(data_temp$mineid),
                   FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
  assign(paste("max_", i, sep = ""),
         aggregate(data_temp[, -match("quarter", names(data_temp))], 
                   list(data_temp$mineid), 
                   FUN = function(x) max(as.numeric(x), na.rm = TRUE)))
  assign(paste("median_", i, sep = ""),
         aggregate(data_temp[, -match("quarter", names(data_temp))], 
                   list(data_temp$mineid), 
                   FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
  
  for (dataset in c(paste("avg_", i, sep = ""), 
                    paste("max_", i, sep = ""),
                    paste("median_", i, sep = ""))) {
    
    d = eval(parse(text = dataset))
    
    for (var in c("total_relevant_violations", "total_violations", "total_injuries", "dv")) {
      
      temp90 = d$mineid[d[, var] >= quantile(d[, var], probs = seq(0, 1, 0.05), na.rm = TRUE)[19]]
      temp95 = d$mineid[d[, var] >= quantile(d[, var], probs = seq(0, 1, 0.05), na.rm = TRUE)[20]]
      temp975 = d$mineid[d[, var] >= quantile(d[, var], probs = seq(0, 1, 0.025), na.rm = TRUE)[40]]

      assign(paste(paste(paste(dataset, var, sep = "_"), 90, sep = "_")), temp90)
      assign(paste(paste(paste(dataset, var, sep = "_"), 95, sep = "_")), temp95)
      assign(paste(paste(paste(dataset, var, sep = "_"), 975, sep = "_")), temp975)
      
      temp_summary = data.frame(summary$mineid)
      names(temp_summary) = "mineid"
      temp_summary$mineid = as.character(temp_summary$mineid)
      
      temp_summary$temp90 = ifelse(temp_summary$mineid %in% temp90, 1, 0)
      temp_summary$temp95 = ifelse(temp_summary$mineid %in% temp95, 1, 0)
      temp_summary$temp975 = ifelse(temp_summary$mineid %in% temp975, 1, 0)
      
      temp_summary$mineid = NULL
      
      names(temp_summary) = c(paste(paste(var, dataset, sep = "_"), "_90", sep = ""), 
                              paste(paste(var, dataset, sep = "_"), "_95", sep = ""), 
                              paste(paste(var, dataset, sep = "_"), "_975", sep = ""))
      
      summary = data.frame(summary, temp_summary)
      
    }
    
  }
  
}

# bye
rm(temp90, temp95, temp975, d, dataset, data_temp, temp_summary, i, var)

######################################################################################################

# DROP MINES THAT ARE NEVER BIG OR BAD

summary = summary[apply(summary[, -1], 1, function(x) !all(x == 0)), ]
summary$sum_all = rowSums(summary[, 2:ncol(summary)])

######################################################################################################

# BIG OR NAH

# evaluate differences between big specifications
write("Big Mines - Evaluating Differences in Specifications\n", file = big_check_out_path)

write("Average vs. Max vs. Median Specifications\n", file = big_check_out_path, append = TRUE)
for (q in c("90", "95", "975")) {
  
  avg = eval(parse(text = paste("avg_all_hours", q, sep = "_")))
  max = eval(parse(text = paste("max_all_hours", q, sep = "_")))
  median = eval(parse(text = paste("median_all_hours", q, sep = "_")))
  
  write(paste(q, "Specifications:\n", sep = " "), file = big_check_out_path, append = TRUE)
  
  write("Number of Mines in Average, Max, and Median Specifications", file = big_check_out_path, append = TRUE)
  write(paste("average -", length(avg), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste("max -", length(max), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste(paste("median -", length(median), sep = " "), "\n", sep = ""), file = big_check_out_path, append = TRUE)
  
  write("Number of Mines Not Captured in Both Specifications (Pairwise Comparison)", file = big_check_out_path, append = TRUE)
  write(paste("average vs. max -", length(setdiff(avg, max)), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste("average vs. median -", length(setdiff(avg, median)), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste(paste("max vs. median -", length(setdiff(max, median)), sep = " "), "\n", sep = ""), file = big_check_out_path, append = TRUE)

}

write("\n90 vs. 95 vs. 97.5\n", file = big_check_out_path, append = TRUE)
for (d in c("avg_all_hours", "max_all_hours", "median_all_hours")) {
  
  q90 = eval(parse(text = paste(d, "90", sep = "_")))
  q95 = eval(parse(text = paste(d, "95", sep = "_")))
  q975 = eval(parse(text = paste(d, "975", sep = "_")))
  
  write(paste(d, "Specifications:\n", sep = " "), file = big_check_out_path, append = TRUE)
  
  write("Number of Mines in 90, 95, and 97.5 Specifications", file = big_check_out_path, append = TRUE)
  write(paste("90 -", length(q90), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste("95 -", length(q95), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste(paste("97.5 -", length(q975), sep = " "), "\n", sep = ""), file = big_check_out_path, append = TRUE)
  
  write("Number of Mines Not Captured in Both Specifications (Pairwise Comparison)", file = big_check_out_path, append = TRUE)
  write(paste("90 vs. 95 -", length(setdiff(q90, q95)), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste("90 vs. 97.5 -", length(setdiff(q90, q975)), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste(paste("95 vs. 97.5 -", length(setdiff(q95, q975)), sep = " "), "\n", sep = ""), file = big_check_out_path, append = TRUE)
  
}

# bye
rm(q, avg, max, median, d, q90, q95, q975)

# choose mines that are big under avg, max, and median specifications
big = summary[, grep("hour", names(summary))]

for (q in c("90", "95", "975")) {
  
  temp = big[, grep(q, names(big))]
  temp$sum = rowSums(temp)
  temp$mineid = summary$mineid
  
  temp = temp[temp$sum == 3, ]
  
  assign(paste("big", q, sep = "_"), temp)
  rm(temp)
  
}

# decide whether 90, 95, or 97.5 is the best specification
  # big 90 includes 122 mines
  # big 95 includes 60 mines
  # big 97.5 includes 27 mines
    # seems like big 90 is most reasonable

# check that we aren't including too many too-small mines
plot(sort(avg_all$hours), 
     main = "Average Hours for Each Mine", 
     xlab = "Mine (Sorted on Average Hours)", 
     ylab = "Average Hours")
abline(a = quantile(avg_all$hours, probs = seq(0, 1, 0.05), na.rm = TRUE)[19], b = 0, col = "red")

plot(sort(max_all$hours), 
     main = "Max Hours for Each Mine", 
     xlab = "Mine (Sorted on Max Hours)", 
     ylab = "Max Hours")
abline(a = quantile(max_all$hours, probs = seq(0, 1, 0.05), na.rm = TRUE)[19], b = 0, col = "red")

plot(sort(median_all$hours), 
     main = "Median Hours for Each Mine", 
     xlab = "Mine (Sorted on Median Hours)", 
     ylab = "Median Hours")
abline(a = quantile(median_all$hours, probs = seq(0, 1, 0.05), na.rm = TRUE)[19], b = 0, col = "red")

# bye
rm(q)

# grab big mines
BIG_MINES = big_90$mineid

# robustness checks - mines at the 90th percentile of each or any specification (avg, max, median)
big$mineid = summary$mineid
BIG_R1_AVG = big[big$hours_avg_all_90 == 1, "mineid"]
BIG_R2_MAX = big[big$hours_max_all_90 == 1, "mineid"]
BIG_R3_MEDIAN = big[big$hours_median_all_90 == 1, "mineid"]
BIG_R4_ALL= union(union(BIG_R1_AVG, BIG_R2_MAX), BIG_R3_MEDIAN)

######################################################################################################

# BAD OR NAH

# evaluate differences between big specifications
write("Bad Mines - Evaluating Differences in Specifications\n", file = bad_check_out_path)

write("2 vs. 4 vs. 6 vs. 8 Specifications\n", file = bad_check_out_path, append = TRUE)
for (dataset in c("avg", "max", "median")) {
  
  for (var in c("total_relevant_violations", "total_violations", "total_injuries", "dv")) {
    
    for (q in c("90", "95", "975")) {
      
      two = eval(parse(text = paste(paste(paste(dataset, "2", sep = "_"), var, sep = "_"), q, sep = "_")))
      four = eval(parse(text =  paste(paste(paste(dataset, "4", sep = "_"), var, sep = "_"), q, sep = "_")))
      six = eval(parse(text = paste(paste(paste(dataset, "6", sep = "_"), var, sep = "_"), q, sep = "_")))
      eight = eval(parse(text = paste(paste(paste(dataset, "8", sep = "_"), var, sep = "_"), q, sep = "_")))
      
      write(paste(paste(paste(var, dataset, sep = "_"), q, sep = "_"), "Specifications:\n", sep = " "), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines in 2, 4, 6, and 8 Specifications", file = bad_check_out_path, append = TRUE)
      write(paste("2 -", length(two), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("4 -", length(four), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("6 -", length(six), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("8 -", length(eight), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines Not Captured in Both Specifications (Pairwise Comparison)", file = bad_check_out_path, append = TRUE)
      write(paste("2 vs. 4 -", length(setdiff(two, four)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("2 vs. 6 -", length(setdiff(two, six)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("2 vs. 8 -", length(setdiff(two, eight)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("4 vs. 6 -", length(setdiff(four, six)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("4 vs. 8 -", length(setdiff(four, eight)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("6 vs. 8 -", length(setdiff(six, eight)), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
    }
    
  }
  
}

write("Average vs. Max vs. Median Specifications\n", file = bad_check_out_path, append = TRUE)
for (i in c("2", "4", "6", "8")) {
  
  for (var in c("total_relevant_violations", "total_violations", "total_injuries", "dv")) {
    
    for (q in c("90", "95", "975")) {
      
      avg = eval(parse(text = paste(paste(paste("avg", i, sep = "_"), var, sep = "_"), q, sep = "_")))
      max = eval(parse(text =  paste(paste(paste("max", i, sep = "_"), var, sep = "_"), q, sep = "_")))
      median = eval(parse(text = paste(paste(paste("median", i, sep = "_"), var, sep = "_"), q, sep = "_")))

      write(paste(paste(paste(var, i, sep = "_"), q, sep = "_"), "Specifications:\n", sep = " "), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines in Average, Max, and Median Specifications", file = bad_check_out_path, append = TRUE)
      write(paste("average -", length(avg), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("max -", length(max), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("median -", length(median), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines Not Captured in Both Specifications (Pairwise Comparison)", file = bad_check_out_path, append = TRUE)
      write(paste("average vs. max -", length(setdiff(avg, max)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("average vs. median -", length(setdiff(avg, median)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("max vs. median -", length(setdiff(max, median)), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
    }
    
  }
  
}



write("90 vs. 95 vs. 97.5 Specifications\n", file = bad_check_out_path, append = TRUE)
for (i in c("2", "4", "6", "8")) {
  
  for (var in c("total_relevant_violations", "total_violations", "total_injuries", "dv")) {
    
    for (dataset in c("avg", "max", "median")) {
      
      q90 = eval(parse(text = paste(paste(paste(dataset, i, sep = "_"), var, sep = "_"), "90", sep = "_")))
      q95 = eval(parse(text =  paste(paste(paste(dataset, i, sep = "_"), var, sep = "_"), "95", sep = "_")))
      q975 = eval(parse(text = paste(paste(paste(dataset, i, sep = "_"), var, sep = "_"), "975", sep = "_")))
      
      write(paste(paste(paste(var, i, sep = "_"), q, sep = "_"), "Specifications:\n", sep = " "), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines in 90, 95, and 97.5 Specifications", file = bad_check_out_path, append = TRUE)
      write(paste("90 -", length(q90), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("95 -", length(q95), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("97.5 -", length(q975), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines Not Captured in Both Specifications (Pairwise Comparison)", file = bad_check_out_path, append = TRUE)
      write(paste("90 vs. 95 -", length(setdiff(q90, q95)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("90 vs. 97.5 -", length(setdiff(q90, q975)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("95 vs. 97.5 -", length(setdiff(q95, q975)), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
    }
    
  }
  
}



write("Total vs. Relevant + Violations vs. Injuries Specifications\n", file = bad_check_out_path, append = TRUE)
for (i in c("2", "4", "6", "8")) {
  
  for (q in c("90", "95", "975")) {
      
    for (dataset in c("avg", "max", "median")) {
      
      rel_viol = eval(parse(text = paste(paste(paste(dataset, i, sep = "_"), "total_relevant_violations", sep = "_"), q, sep = "_")))
      tot_viol = eval(parse(text = paste(paste(paste(dataset, i, sep = "_"), "total_violations", sep = "_"), q, sep = "_")))
      rel_inj = eval(parse(text = paste(paste(paste(dataset, i, sep = "_"), "dv", sep = "_"), q, sep = "_")))
      tot_inj = eval(parse(text =  paste(paste(paste(dataset, i, sep = "_"), "total_injuries", sep = "_"), q, sep = "_")))
      
      write(paste(paste(paste(var, i, sep = "_"), q, sep = "_"), "Specifications:\n", sep = " "), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines in Total vs. Relevant + Violations vs. Injuries Specifications", file = bad_check_out_path, append = TRUE)
      write(paste("Relevant Violations -", length(rel_viol), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("Total Violations -", length(tot_viol), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("Relevant Injuries -", length(rel_inj), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("Total Injuries -", length(tot_inj), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines Not Captured in Both Specifications (Pairwise Comparison)", file = bad_check_out_path, append = TRUE)
      write(paste("Rel Viol vs. Tot Viol -", length(setdiff(rel_viol, tot_viol)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("Rel Viol vs. Rel Inj -", length(setdiff(rel_viol, rel_inj)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("Rel Viol vs. Tot Inj -", length(setdiff(rel_viol, tot_inj)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("Tot Viol vs. Rel Inj -", length(setdiff(tot_viol, rel_inj)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("Tot Viol vs. Tot Inj -", length(setdiff(tot_viol, tot_inj)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("Rel Inj vs. Tot Inj -", length(setdiff(rel_inj, tot_inj)), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
    }
    
  }
  
}

######################################################################################################
