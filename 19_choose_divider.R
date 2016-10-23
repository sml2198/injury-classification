# NIOSH Project 2014-N-15776

# 19 - Choose Selection-Training Divider

# Last edit 10/22/16

######################################################################################################

library(plyr)

data_file_path = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"
out_file_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Mine Classification/Choose Selection-Training Cutoff.txt"

######################################################################################################

# read in data 
data = readRDS(data_file_path)

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

test = data[data$quarter >= 2015, ]
not_test = data[data$quarter < 2015, ]

get_most_recent_only = function(mine_data) {
  mine_data = mine_data[order(mine_data$quarter, decreasing = TRUE), ]
  mine_data = mine_data[1:num_quarters_in_selection, ]
  mine_data = mine_data[order(mine_data$quarter), ]
  return(mine_data)
}

write("Choose Selection-Training Cutoff\n\n", file = out_file_path)

for (type in c("big", "bad_viol", "bad_inj")) {
  write(paste(type, ":\n", sep = ""), file = out_file_path, append = TRUE)
  
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
  
  for (cutoff in c(2002:2005)) {
    
    write(paste(paste("Cutoff:", cutoff, sep = " "), "\n", sep = ""), file = out_file_path, append = TRUE)
  
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
    write("V1 - Our Way:", file = out_file_path, append = TRUE)
    
    mines_avg = avg$mineid[avg[, var] >= quantile(avg[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
    mines_max = max$mineid[max[, var] >= quantile(max[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
    mines_median = median$mineid[median[, var] >= quantile(median[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
    
    mines = intersect(intersect(mines_avg, mines_max), mines_median)
    num_obs = nrow(train[train$mineid %in% mines, ])
    
    assign(paste(paste(type, cutoff, sep = "_"), "v1", sep = "_"), as.character(mines))
    
    write(paste("Number of mines:", length(mines), sep = " "), file = out_file_path, append = TRUE)
    write(paste(paste("Number of observations:", num_obs, sep = " "), "\n", sep = ""), file = out_file_path, append = TRUE)
    
    
    # Alison's Way
    write("V2 - Alison's Way:", file = out_file_path, append = TRUE)
    
    mine_info = data.frame(unique(select$mineid))
    names(mine_info) = "mineid"
    
    for (q in unique(select$quarter)) {
      temp = select[select$quarter == q, ]
      mines = temp$mineid[temp[, var] >= quantile(temp[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[percentile + 1]]
      mine_info[, toString(q)] = ifelse(mine_info$mineid %in% mines, 1, 0)
    }
    
    mine_info$sum = rowSums(mine_info[, -1])
    
    mines = mine_info[mine_info$sum == num_quarters_in_selection, "mineid"]
    num_obs = nrow(train[train$mineid %in% mines, ])
    
    assign(paste(paste(type, cutoff, sep = "_"), "v2", sep = "_"), as.character(mines))
    
    write(paste("Number of mines:", length(mines), sep = " "), file = out_file_path, append = TRUE)
    write(paste(paste("Number of observations:", num_obs, sep = " "), "\n", sep = ""), file = out_file_path, append = TRUE)
    
  }
  
}

for (type in c("inj", "viol")) {
  write(paste(paste("big_bad", type, sep = "_"), ":\n", sep = ""), file = out_file_path, append = TRUE)
  
  for (cutoff in c(2002:2005)) {
    
    write(paste(paste("Cutoff:", cutoff, sep = " "), "\n", sep = ""), file = out_file_path, append = TRUE)
    
    # our Way
    write("V1 - Our Way:", file = out_file_path, append = TRUE)
    
    big_mines = eval(parse(text = paste(paste("big", cutoff, sep = "_"), "v1", sep = "_")))
    bad_mines = eval(parse(text = paste(paste(paste("bad", type, sep = "_"), cutoff, sep = "_"), "v1", sep = "_")))
    
    mines = intersect(big_mines, bad_mines)
    num_obs = nrow(train[train$mineid %in% mines, ])
    
    write(paste("Number of mines:", length(mines), sep = " "), file = out_file_path, append = TRUE)
    write(paste(paste("Number of observations:", num_obs, sep = " "), "\n", sep = ""), file = out_file_path, append = TRUE)
    
    # Alison's Way
    write("V2 - Alison's Way:", file = out_file_path, append = TRUE)
    
    big_mines = eval(parse(text = paste(paste("big", cutoff, sep = "_"), "v2", sep = "_")))
    bad_mines = eval(parse(text = paste(paste(paste("bad", type, sep = "_"), cutoff, sep = "_"), "v2", sep = "_")))
    
    mines = intersect(big_mines, bad_mines)
    num_obs = nrow(train[train$mineid %in% mines, ])
    
    write(paste("Number of mines:", length(mines), sep = " "), file = out_file_path, append = TRUE)
    write(paste(paste("Number of observations:", num_obs, sep = " "), "\n", sep = ""), file = out_file_path, append = TRUE)
  
  }
  
}

######################################################################################################
