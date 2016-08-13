# NIOSH Project 2014-N-15776

# File number - File name
  # File description

# Last edit 8/12/16

######################################################################################################

# input: part-level data
data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_part_level.rds"

######################################################################################################

# read in data 
data = readRDS(data_file_name) # 27456 obs, 185 vars

# drop unnecessary variables
data$minestatus =
  data$idesc = 
  data$daysperweek = 
  data$productionshiftsperday =
  data$minesizepoints = 
  data$terminated = 
  data$totalinjuries = 
  data$MR_proportion = 
  data$num_no_terminations = NULL

# reformat variables because R is a lil bitch
names(data)[grep("^[0-9]", names(data))] = paste("p", names(data)[grep("^[0-9]", names(data))], sep = "")

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

# clear out
rm(violation_vars, var)

# investigate missing times
missing_time = data.frame(mineid = character(), 
                          quarter = numeric())
for (mine in unique(data$mineid)) {
  new_info = data[(data$mineid == mine & data$num_insp == 0), c("mineid", "quarter")]
    missing_time = rbind(missing_time, new_info)
}

# bye
rm(mine)

hist(missing_time$quarter)
