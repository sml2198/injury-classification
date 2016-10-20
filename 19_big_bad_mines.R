# NIOSH Project 2014-N-15776

# 19 - The Big Bad Mines

# Last edit 10/19/16

######################################################################################################

data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"

big_check_out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Big and Bad Mine Classification/Big_Check.txt"
bad_check_out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Big and Bad Mine Classification/Bad_Check.txt"

######################################################################################################

# DATA SET-UP

# read in data 
data = readRDS(data_file_name)

# drop lagged variables
keep_vars = names(data)[!grepl("lag", names(data))]
data = data[keep_vars]

# keep only useful variables
data = data[, c("quarter", "mineid", "mine_time", "hours", "total_violations", "total_injuries")]

# create summary dataframe
summary = data.frame(unique(data$mineid))
names(summary) = "mineid"
summary$mineid = as.character(summary$mineid)

# bye
rm(keep_vars)

######################################################################################################

# GRAB INFORMATION ON BIG-NESS

# calculate avg, max, and median hours per mine
avg_all = aggregate(data[, -match("quarter", names(data))], 
                    by = list(data$mineid), 
                    FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

# returns warnings - some variables are all NA, and therefore return -Inf
max_all = aggregate(data[, -match("quarter", names(data))], 
                    by = list(data$mineid), 
                    FUN = function(x) max(as.numeric(x), na.rm = TRUE))


median_all = aggregate(data[, -match("quarter", names(data))], 
                       by = list(data$mineid), 
                       FUN = function(x) median(as.numeric(x), na.rm = TRUE))

# for each measure, select mines in 90th, 95th, and 97th percentile
for (dataset in c("avg_all", "max_all", "median_all")) {
  
  d = eval(parse(text = dataset))
  
  for (p in c(90, 95, 97)) {
    
    mines = d$mineid[d$hours >= quantile(d$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[p + 1]]
    assign(paste(paste(dataset, "hours", sep = "_"), p, sep = "_"), mines)
    summary[, paste(paste(dataset, "hours", sep = "_"), p, sep = "_")] = ifelse(summary$mineid %in% mines, 1, 0)
    
  }
  
}

# bye
rm(dataset, d, p, mines)

######################################################################################################

# BIG OR NAH

# decide which percentile cutoff is best
# 90 includes 159 mines
# 95 includes 80 mines
# 97 includes 48 mines
# seems like 90 is most reasonable for sample size goals

# check that we aren't including too-small mines
# could put this code in one loop if I wasn't lazy af
plot(sort(avg_all$hours), 
     main = "Average Hours for Each Mine", 
     xlab = "Mine (Sorted on Average Hours)", 
     ylab = "Average Hours")
abline(a = quantile(avg_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[91], b = 0, col = "red")
abline(a = quantile(avg_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[96], b = 0, col = "blue")
abline(a = quantile(avg_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[98], b = 0, col = "green")

plot(sort(max_all$hours), 
     main = "Max Hours for Each Mine", 
     xlab = "Mine (Sorted on Max Hours)", 
     ylab = "Max Hours")
abline(a = quantile(max_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[91], b = 0, col = "red")
abline(a = quantile(max_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[96], b = 0, col = "blue")
abline(a = quantile(max_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[98], b = 0, col = "green")

plot(sort(median_all$hours), 
     main = "Median Hours for Each Mine", 
     xlab = "Mine (Sorted on Median Hours)", 
     ylab = "Median Hours")
abline(a = quantile(median_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[91], b = 0, col = "red")
abline(a = quantile(median_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[96], b = 0, col = "blue")
abline(a = quantile(median_all$hours, probs = seq(0, 1, 0.01), na.rm = TRUE)[98], b = 0, col = "green")

# these plots suggest that we can use the 90th percentile cutoff

# decide which collapsing measure is best
# evaluate differences between average, maximum, and median specifications
write("Big Mines - Evaluating Differences in Specifications\n", file = big_check_out_path)

write("Average vs. Maximum vs. Median Specifications\n", file = big_check_out_path, append = TRUE)
for (p in c("90", "95", "97")) {
  
  write(paste(p, "Specifications:\n", sep = " "), file = big_check_out_path, append = TRUE)
  
  avg = eval(parse(text = paste("avg_all_hours", p, sep = "_")))
  max = eval(parse(text = paste("max_all_hours", p, sep = "_")))
  median = eval(parse(text = paste("median_all_hours", p, sep = "_")))
  
  write("Number of Mines in Each Specification", file = big_check_out_path, append = TRUE)
  write(paste("average -", length(avg), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste("maximum -", length(max), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste(paste("median -", length(median), sep = " "), "\n", sep = ""), file = big_check_out_path, append = TRUE)
  
  write("Number of Non-Overlapping Mines", file = big_check_out_path, append = TRUE)
  write(paste("average vs. maximum -", length(setdiff(avg, max)), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste("average vs. median -", length(setdiff(avg, median)), sep = " "), file = big_check_out_path, append = TRUE)
  write(paste(paste("maximum vs. median -", length(setdiff(max, median)), sep = " "), "\n", sep = ""), file = big_check_out_path, append = TRUE)
  
}

# output shows that there are only a small number of non-overlapping mines

# grab big mines
BIG_MINES = intersect(intersect(avg_all_hours_90, max_all_hours_90), median_all_hours_90) # 132

# robustness checks
BIG_R1_AVG = avg_all_hours_90 # 159
BIG_R2_MAX = max_all_hours_90 # 159
BIG_R3_MEDIAN = median_all_hours_90 # 159
BIG_R4_ALL= unique(union(union(avg_all_hours_90, max_all_hours_90), median_all_hours_90)) # 186

# check that sample size won't be a problem
nrow(data[data$mineid %in% BIG_MINES, ]) # 5152 observations
nrow(data[data$mineid %in% BIG_R1_AVG, ]) # 6180 observations
nrow(data[data$mineid %in% BIG_R2_MAX, ]) # 6143 observations
nrow(data[data$mineid %in% BIG_R3_MEDIAN, ]) # 6104 observations
nrow(data[data$mineid %in% BIG_R4_ALL, ]) # 7045 observations

# bye
rm(p, avg, max, median)

######################################################################################################

# GRAB INFORMATION ON BAD-NESS

for (i in seq(4, 8, 2)) { # number of quarters to use to choose badness
  
  data_temp = data[data$mine_time <= i, ]
  
  # calculate avg, max, and median hours per mine
  assign(paste("avg_", i, sep = ""),
         aggregate(data_temp[, -match("quarter", names(data_temp))], 
                   by = list(data_temp$mineid),
                   FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
  
  # returns warnings - some variables are all NA, and therefore return -Inf
  assign(paste("max_", i, sep = ""),
         aggregate(data_temp[, -match("quarter", names(data_temp))], 
                   by = list(data_temp$mineid), 
                   FUN = function(x) max(as.numeric(x), na.rm = TRUE)))
  
  assign(paste("median_", i, sep = ""),
         aggregate(data_temp[, -match("quarter", names(data_temp))], 
                   by = list(data_temp$mineid), 
                   FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
  
  for (dataset in c(paste("avg_", i, sep = ""), paste("max_", i, sep = ""), paste("median_", i, sep = ""))) {
    
    d = eval(parse(text = dataset))
    
    for (var in c("total_violations", "total_injuries")) {
      
      for (p in c(90, 95, 97)) {
        
        mines = d$mineid[d[, var] >= quantile(d[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[p + 1]]
        assign(paste(paste(dataset, var, sep = "_"), p, sep = "_"), mines)
        summary[, paste(paste(dataset, var, sep = "_"), p, sep = "_")] = ifelse(summary$mineid %in% mines, 1, 0)
        
      }
      
    }
    
  }
  
}

# bye
rm(d, dataset, data_temp, i, var, p, mines)

######################################################################################################

# BAD OR NAH

# decide how many quarters of data to use to choose badness
# evaluate differences between 4, 6, and 8 specifications
write("Bad Mines - Evaluating Differences in Specifications\n", file = bad_check_out_path)

write("4 vs. 6 vs. 8 Specifications\n", file = bad_check_out_path, append = TRUE)
for (dataset in c("avg", "max", "median")) {
  
  for (var in c("total_violations", "total_injuries")) {
    
    for (p in c("90", "95", "97")) {
      
      four = eval(parse(text =  paste(paste(paste(dataset, "4", sep = "_"), var, sep = "_"), p, sep = "_")))
      six = eval(parse(text = paste(paste(paste(dataset, "6", sep = "_"), var, sep = "_"), p, sep = "_")))
      eight = eval(parse(text = paste(paste(paste(dataset, "8", sep = "_"), var, sep = "_"), p, sep = "_")))
      
      write(paste(paste(paste(var, dataset, sep = "_"), p, sep = "_"), "Specifications:\n", sep = " "), file = bad_check_out_path, append = TRUE)
      
      write("Number of Mines in Each Specification", file = bad_check_out_path, append = TRUE)
      write(paste("4 -", length(four), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("6 -", length(six), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("8 -", length(eight), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
      write("Number of Non-Overlapping Mines", file = bad_check_out_path, append = TRUE)
      write(paste("4 vs. 6 -", length(setdiff(four, six)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste("4 vs. 8 -", length(setdiff(four, eight)), sep = " "), file = bad_check_out_path, append = TRUE)
      write(paste(paste("6 vs. 8 -", length(setdiff(six, eight)), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
      
    }
    
  }
  
}

# there are not too many non-overlapping mines between 6 and 8 specifications 
# there are not too many non-overlapping mines between 4 and 6 specifications 
# there are considerable non-overlapping mines between 4 and 8 specifications 
# we think using 8 specifications is probably the safest
# if we run into sample size problems, we could drop down and use 6
# we could also use 6 to grab a sample of mines for a robustness check

# decide which measure of badness to use
# evaluate differences between injuries and violations
write("Violations vs. Injuries Specifications\n", file = bad_check_out_path, append = TRUE)
for (p in c("90", "95", "97")) {
  
  for (d in c("avg", "max", "median")) {
    
    viol = eval(parse(text = paste(paste(paste(d, "8", sep = "_"), "total_violations", sep = "_"), p, sep = "_")))
    inj = eval(parse(text =  paste(paste(paste(d, "8", sep = "_"), "total_injuries", sep = "_"), p, sep = "_")))
    
    write(paste(paste(paste(d, "8", sep = "_"), p, sep = "_"), "Specifications:\n", sep = " "), file = bad_check_out_path, append = TRUE)
    
    write("Number of Mines in Each Specification", file = bad_check_out_path, append = TRUE)
    write(paste("Violations -", length(viol), sep = " "), file = bad_check_out_path, append = TRUE)
    write(paste(paste("Injuries -", length(inj), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
    
    write("Number of Non_Overlapping Mines", file = bad_check_out_path, append = TRUE)
    write(paste(paste("Violations vs. Injuries -", length(setdiff(viol, inj)), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
    
  }
  
}

# there are considerable non-overlapping mines between injury and violation specifications
# we think we should use both (i.e., not choose, but perform analyses twice)

# decide which collapsing measure is best
# evaluate differences between average, maximum, and median specifications
write("Average vs. Max vs. Median Specifications\n", file = bad_check_out_path, append = TRUE)
for (var in c("total_violations", "total_injuries")) {
  
  for (p in c("90", "95", "97")) {
    
    avg = eval(parse(text = paste(paste(paste("avg", "8", sep = "_"), var, sep = "_"), p, sep = "_")))
    max = eval(parse(text =  paste(paste(paste("max", "8", sep = "_"), var, sep = "_"), p, sep = "_")))
    median = eval(parse(text = paste(paste(paste("median", "8", sep = "_"), var, sep = "_"), p, sep = "_")))
    
    write(paste(paste(paste(var, "8", sep = "_"), p, sep = "_"), "Specifications:\n", sep = " "), file = bad_check_out_path, append = TRUE)
    
    write("Number of Mines in Each Specification", file = bad_check_out_path, append = TRUE)
    write(paste("average -", length(avg), sep = " "), file = bad_check_out_path, append = TRUE)
    write(paste("maximum -", length(max), sep = " "), file = bad_check_out_path, append = TRUE)
    write(paste(paste("median -", length(median), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
    
    write("Number of Non-Overlapping Mines", file = bad_check_out_path, append = TRUE)
    write(paste("average vs. maximum -", length(setdiff(avg, max)), sep = " "), file = bad_check_out_path, append = TRUE)
    write(paste("average vs. median -", length(setdiff(avg, median)), sep = " "), file = bad_check_out_path, append = TRUE)
    write(paste(paste("maximum vs. median -", length(setdiff(max, median)), sep = " "), "\n", sep = ""), file = bad_check_out_path, append = TRUE)
    
  }
  
}

# output shows that there are a considerable number of non-overlapping mines
# nevertheless, we want to keep strict definition of being bad -- if we can

# grab big mines
BAD_MINES_VIOL = intersect(intersect(avg_8_total_violations_90, max_8_total_violations_90), median_8_total_violations_90) # 111
BAD_MINES_INJ = intersect(intersect(avg_8_total_injuries_90, max_8_total_injuries_90), median_8_total_injuries_90) # 131

# robustness checks
BAD_VIOL_R1_AVG = avg_8_total_violations_90 # 199
BAD_VIOL_R2_MAX = max_8_total_violations_90 # 161
BAD_VIOL_R3_MEDIAN = median_8_total_violations_90 # 198
BAD_VIOL_R4_ALL = union(union(avg_8_total_violations_90, max_8_total_violations_90), median_8_total_violations_90) # 206

BAD_INJ_R1_AVG = avg_8_total_injuries_90
BAD_INJ_R2_MAX = max_8_total_injuries_90
BAD_INJ_R3_MEDIAN = median_8_total_injuries_90
BAD_INJ_R4_ALL = union(union(avg_8_total_injuries_90, max_8_total_injuries_90), median_8_total_injuries_90)

# check that sample size won't be a problem
temp = data[data$mineid %in% BAD_MINES_VIOL, ]
nrow(temp[temp$quarter > 8, ]) # 3025 observations
temp = data[data$mineid %in% BAD_MINES_INJ, ]
nrow(temp[temp$quarter > 8, ]) # 4721 observations

temp = data[data$mineid %in% BAD_VIOL_R1_AVG, ]
nrow(temp[temp$quarter > 8, ]) # 4118 observations
temp = data[data$mineid %in% BAD_VIOL_R2_MAX, ]
nrow(temp[temp$quarter > 8, ]) # 4245 observations
temp = data[data$mineid %in% BAD_VIOL_R3_MEDIAN, ]
nrow(temp[temp$quarter > 8, ]) # 3884 observations
temp = data[data$mineid %in% BAD_VIOL_R4_ALL, ]
nrow(temp[temp$quarter > 8, ]) # 5094 observations

temp = data[data$mineid %in% BAD_INJ_R1_AVG, ]
nrow(temp[temp$quarter > 8, ]) # 5516 observations
temp = data[data$mineid %in% BAD_INJ_R2_MAX, ]
nrow(temp[temp$quarter > 8, ]) # 6229 observations
temp = data[data$mineid %in% BAD_INJ_R3_MEDIAN, ]
nrow(temp[temp$quarter > 8, ]) # 5608 observations
temp = data[data$mineid %in% BAD_INJ_R4_ALL, ]
nrow(temp[temp$quarter > 8, ]) # 7164 observations

# bye
rm(d, dataset, p, var, avg, max, median, four, six, eight, inj, viol, temp)

######################################################################################################

# FINISH LATER

# BIG & BAD OR NAH

# grab big & bad mines
BIG_BAD_MINES_VIOL = (intersect(BIG_MINES, BAD_MINES_VIOL_8))
BIG_BAD_MINES_INJ = (intersect(BIG_MINES, BAD_MINES_INJ_8))

# robustness checks

# check that sample size won't be a problem
temp = data[data$mineid %in% BIG_BAD_MINES_VIOL_8, ]
nrow(temp[temp$quarter > 8, ]) # 1947 observations

# bye
rm(temp)

######################################################################################################
