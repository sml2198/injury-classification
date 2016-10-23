# NIOSH Project 2014-N-15776

# 20 - Generate Big + Bad Mines

# Last edit 10/22/16

######################################################################################################

library(plyr)
# library(foreign)

injury = "MR"
# injury = "PS"

if (injury == "MR") {
  data_in_file_path = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"
}
if (injury == "PS") {
  data_in_file_path = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"
}

######################################################################################################

# read in data 
data = readRDS(data_in_file_path)
out = readRDS(data_in_file_path)

# drop lagged variables
keep_vars = names(data)[!grepl("lag", names(data))]
data = data[keep_vars]

# keep only useful variables
data$viol_rate = data$total_violations / data$onsite_insp_hours
data$inj_rate = data$total_injuries / data$hours
data = data[, c("quarter", "mineid", "mine_time", "hours", "viol_rate", "inj_rate")]

# bye
rm(keep_vars)

######################################################################################################

num_quarters_in_selection = 6
cutoff = 2003

test = data[data$quarter >= 2015, ]
not_test = data[data$quarter < 2015, ]

out_not_test = out[out$quarter < 2015, ]
out_train = out_not_test[out_not_test$quarter >= cutoff, ]

get_most_recent_only = function(mine_data) {
  mine_data = mine_data[order(mine_data$quarter, decreasing = TRUE), ]
  mine_data = mine_data[1:num_quarters_in_selection, ]
  mine_data = mine_data[order(mine_data$quarter), ]
  return(mine_data)
}

for (type in c("big", "bad_viol", "bad_inj")) {
  
  out_file_path_v1 = paste("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/Big + Bad Data/", injury, "/", injury, "_prediction_data_", type, "_v1.dta", sep = "", collapse = "TRUE")
  out_file_path_v2 = paste("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/Big + Bad Data/", injury, "/", injury, "_prediction_data_", type, "_v2.dta", sep = "", collapse = "TRUE")
  
  if (type == "big") {
    var = "hours"
    percentile = 85
  }
  
  if (type == "bad_viol") {
    var = "viol_rate"
    percentile = 50
    
  }
  
  if (type == "bad_inj") {
    var = "inj_rate"
    percentile = 50
  }

  # define selection and training sets
  select_temp = not_test[not_test$quarter < cutoff, ]
  select = ddply(select_temp, ~mineid, get_most_recent_only)
  select = select[!is.na(select[, ]), ]
  select = select[select$mineid %in% names(table(select$mineid))[table(select$mineid) >= num_quarters_in_selection], ]
  
  # define training sets
  train = not_test[not_test$quarter >= cutoff, ]
  
  # collapse data to use for our way 
  avg = aggregate(select[, -match("quarter", names(select))], 
                  by = list(select$mineid), 
                  FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
  max = aggregate(select[, -match("quarter", names(select))], 
                  by = list(select$mineid), 
                  FUN = function(x) max(as.numeric(x), na.rm = TRUE))
  median = aggregate(select[, -match("quarter", names(select))], 
                     by = list(select$mineid), 
                     FUN = function(x) median(as.numeric(x), na.rm = TRUE))
  
  # our Way
  mines_avg = avg$mineid[avg[, var] >= quantile(avg[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
  mines_max = max$mineid[max[, var] >= quantile(max[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
  mines_median = median$mineid[median[, var] >= quantile(median[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
  
  mines = intersect(intersect(mines_avg, mines_max), mines_median)
  assign(paste(paste(type, cutoff, sep = "_"), "v1", sep = "_"), as.character(mines))

  # write.dta(out_train[out_train$mineid %in% mines, ], file = out_file_path_v1)
  
  # Alison's Way
  mine_info = data.frame(unique(select$mineid))
  names(mine_info) = "mineid"
  
  for (q in unique(select$quarter)) {
    temp = select[select$quarter == q, ]
    mines = temp$mineid[temp[, var] >= quantile(temp[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
    mine_info[, toString(q)] = ifelse(mine_info$mineid %in% mines, 1, 0)
  }
  
  mine_info$sum = rowSums(mine_info[, -1])
  
  mines = mine_info[mine_info$sum == num_quarters_in_selection, "mineid"]
  assign(paste(paste(type, cutoff, sep = "_"), "v2", sep = "_"), as.character(mines))
  
  # write.dta(out_train[out_train$mineid %in% mines, ], file = out_file_path_v2)
  
}

for (type in c("inj", "viol")) {

  out_file_path_v1 = paste("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/Big + Bad Data/", injury, "/", injury, "_prediction_data_big_bad_", type, "_v1.dta", sep = "", collapse = "TRUE")
  out_file_path_v2 = paste("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/Big + Bad Data/", injury, "/", injury, "_prediction_data_big_bad_", type, "_v2.dta", sep = "", collapse = "TRUE")
  
  
  # our Way
  big_mines = eval(parse(text = paste(paste("big", cutoff, sep = "_"), "v1", sep = "_")))
  bad_mines = eval(parse(text = paste(paste(paste("bad", type, sep = "_"), cutoff, sep = "_"), "v1", sep = "_")))
  
  mines = intersect(big_mines, bad_mines)
  
  # write.dta(out_train[out_train$mineid %in% mines, ], file = out_file_path_v1)
  
  # Alison's Way
  big_mines = eval(parse(text = paste(paste("big", cutoff, sep = "_"), "v2", sep = "_")))
  bad_mines = eval(parse(text = paste(paste(paste("bad", type, sep = "_"), cutoff, sep = "_"), "v2", sep = "_")))
    
  mines = intersect(big_mines, bad_mines)
  
  # write.dta(out_train[out_train$mineid %in% mines, ], file = out_file_path_v1)
  
}

######################################################################################################
