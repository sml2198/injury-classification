# NIOSH Project 2014-N-15776

# File number - File name
  # File description

# Last edit 8/12/16

######################################################################################################

# input: part-level data
data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_part_level.rds"

######################################################################################################

# read in data 
data = readRDS(data_file_name) # 27456 obs, 165 vars

# drop unnecessary variables
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

# investigate quarters with no inspections
no_insp = data[data$num_insp == 0, c("mineid", "quarter")]

# how many quarters with no inspections per mine?
barplot(sort(table(no_insp$mineid)),
        xaxt = "n",
        main = "Number of No-Inspection Quarters per Mine", 
        xlab = "Mine ID",
        ylab = "Number of No-Inspection Quarters")

# mines missing > + >= 10 quarters of data
unique(no_insp[no_insp$mineid %in% names(which(table(no_insp$mineid) > 10)), "mineid"])
unique(no_insp[no_insp$mineid %in% names(which(table(no_insp$mineid) == 10)), "mineid"])

# how many non-missing quarters do these mines contribute to data?
data[data$mineid == "1202423", c("num_insp", "quarter")] # just an ex -- could automate and get info for all of these mines ^^ 

# which quarters are missing inspections
hist(no_insp$quarter)
