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

data = data[, -c(grep("^MR\\_l", names(data)), 
                 grep("^MR\\_indicator\\_l", names(data)), 
                 grep("^MR\\_proportion\\_l", names(data)))]










# un-dummy variables
un_dum = function(dum_var_names, cat_var_name, data, data_name) {
  data[, cat_var_name] = character()
  for (var in dum_var_names) {
    name = substr(var, nchar(var), nchar(var))
    data[, cat_var_name] = ifelse(data[, var] == 1, name, data[, cat_var_name])
    #data[, var] = NULL
  }
  data[, cat_var_name] = as.factor(data[, cat_var_name])
  assign(data_name, data, .GlobalEnv) 
}

parts = c("47", "48", "71", "72", "75", "77")
vars = c("inspacty", 
         "violationtypecode", 
         "assessmenttypecode", 
         "likelihood", 
         "injuryillness", 
         "negligence")

for (part in parts) {
  for (var in vars) {
    search = paste("^", part, "\\.", var, sep = "")
    dum_vars = names(data)[grep(search, names(data))]
    un_dum(dum_vars, paste(part, var, sep = "."), data, "data")
  }
}