# NIOSH Project 2014-N-15776

# 19 - The Big Bad Mines

# Last edit 10/17/16

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
data = data[, c("quarter", "mineid", "mine_time",
                "hours", 
                "total_relevant_violations", "total_mine_act_violations", "total_violations", 
                "total_injuries", "dv", "dv_indicator")]

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

rm(temp90, temp95, temp975, d, dataset, data_temp, temp_summary, i, var)

######################################################################################################

summary = summary[apply(summary[, -1], 1, function(x) !all(x == 0)), ]
summary$sum_all = rowSums(summary[, 2:ncol(summary)])

######################################################################################################

# BIG OR NAH

big = summary[, grep("hour", names(summary))]

for (q in c("90", "95", "975")) {
  temp = big[, grep(q, names(big))]
  temp$sum = rowSums(temp)
  temp$mineid = summary$mineid
  
  temp = temp[temp$sum == 3, ]
  
  assign(paste("big", q, sep = "_"), temp)
  rm(temp)
}

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

######################################################################################################

# BAD OR NAH

# 2 vs 4 vs 6 vs 8 

for (dataset in c("avg", "max", "median")) {
  for (var in c("total_relevant_violations", "total_violations", "total_injuries", "dv")) {
    for (q in c("90", "95", "975")) {
      
      two = eval(parse(text = paste(paste(paste(dataset, "2", sep = "_"), var, sep = "_"), q, sep = "_")))
      print("two")
      print(length(two))
      
      four = eval(parse(text =  paste(paste(paste(dataset, "4", sep = "_"), var, sep = "_"), q, sep = "_")))
      print("four")
      print(length(four))
      
      six = eval(parse(text = paste(paste(paste(dataset, "6", sep = "_"), var, sep = "_"), q, sep = "_")))
      print("six")
      print(length(six))
      
      eight = eval(parse(text = paste(paste(paste(dataset, "8", sep = "_"), var, sep = "_"), q, sep = "_")))
      print("eight")
      print(length(eight))
      
      
      print(paste(paste(var, dataset, sep = "_"), q, sep = "_"))
      
      print("2 vs. 4")
      print(length(setdiff(two, four)))
      
      print("2 vs. 6")
      print(length(setdiff(two, six)))
      
      print("2 vs. 8")
      print(length(setdiff(two, eight)))
      
      print("4 vs. 6")
      print(length(setdiff(four, six)))
      
      print("4 vs. 8")
      print(length(setdiff(four, eight)))
      
      print("6 vs. 8")
      print(length(setdiff(six, eight)))
      
    }
    
  }
  
}

######################################################################################################
