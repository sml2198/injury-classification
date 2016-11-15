# NIOSH Project 2014-N-15776

# 22 - Add Misc Variables (2)

# Last edit 11/2/16

######################################################################################################

library(foreign)
library(plyr)
library(zoo)
library(stringr)

# input: clean mine-quarter level data (has observations dropped in preparation for prediction)
mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds"
# input: controller/operator history data downloaded on 9/13/2016 from MSHA open data 
controller_operator_history_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ControllerOperatorHistory.txt"
# input: prediction-ready data containing all relevant and maybe relevant vars  - PS
PS_prediction_data_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/PS_prediction_data.rds"
# input: prediction-ready data containing all relevant and maybe relevant vars  - MR
MR_prediction_data_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"

# input: old data from last project (1983-2000)
old_mines_data_in_file_name = "X:/Projects/Mining/Targeting_Pilot/analysis/data/5_prepped/0_prepped_for_CART_unlagged/coal_unlagged_mine_quarter_CART.csv"

# output: prediction-ready data containing all relevant and maybe relevant vars  - PS (dta)
PS_prediction_data_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/PS_prediction_data.dta"
# output: prediction-ready data containing all relevant and maybe relevant vars  - MR (dta)
MR_prediction_data_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.dta"

# injury.type = "PS"
injury.type = "MR"

######################################################################################################

# FROM MINES DATA, VARIABLES TO ADD: 
  # district dummies (district)
  # safety committee indicator (safetycommitteeindicator)

# import clean mine-quarters data (30551 obs)
mines = readRDS(mines_quarters_file_name)

# keep only necessary vars
mines = mines[, c("mineid", "quarter", "district", "safetycommittee")]

######################################################################################################

# FROM CONTROLLER/OPERATOR HISORY DATA, VARIABLES TO ADD: 
  # operator dummies (operatorid)
  # operator-mine age (to be constructed from mine status and operator history)

# read controller history data - 144,065 obs, 13 vars 
# dataset downloaded on 9/13/2016 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
history = read.table(controller_operator_history_in_file_name, header = T, sep = "|")

# drop data from environments not of interest
history = history[(history$COAL_METAL_IND == "C"), ] # down to 63143 obs

# rename variables
names(history)[names(history) == "MINE_ID"] = "mineid"
names(history)[names(history) == "OPERATOR_ID"] = "operatorid"
names(history)[names(history) == "OPERATOR_START_DT"] = "operatorstartdt"
names(history)[names(history) == "OPERATOR_END_DT"] = "operatorenddt"
history = history[, c("operatorid", "mineid", "operatorstartdt", "operatorenddt")]

# format mineid
history$mineid = as.character(history$mineid)
history$mineid = str_pad(history$mineid, 7, pad = "0")

# convert start/end dates into quarters & replace end quarter with 2016 if missing
enddtvars = "operatorenddt"
for (i in 1:length(enddtvars)) {
  history[, enddtvars[i]] = as.character(history[, enddtvars[i]])
  history[, enddtvars[i]] = ifelse(history[, enddtvars[i]] == "", NA, history[, enddtvars[i]])
  history[, enddtvars[i]] = ifelse(is.na(history[, enddtvars[i]]), "01/01/2016", history[, enddtvars[i]]) # Q1 2016
}
datevars = c("operatorstartdt", "operatorenddt")
for (i in 1:length(datevars)) {
  history[, datevars[i]] = as.Date(as.character(history[, datevars[i]]), "%m/%d/%Y")
  history[, datevars[i]] = as.yearqtr(history[, datevars[i]])
}

######################################################################################################

# MERGE IN DATA FROM OLD PROJECT 

old_data = read.table(old_mines_data_in_file_name, na.strings=c("","NA"), sep = "|", header = T)

names(old_data)[names(old_data) == "mine_id"] = "mineid"
names(old_data)[names(old_data) == "total_hours_worked"] = "old_hours"

# format quarters
old_data$quarter = as.character(old_data$quarter)
old_data$quarter = as.yearqtr(old_data$quarter)

# only keep if it's for a year for which we're missing data
old_data = old_data[old_data$quarter < "2000 Q1", ]

# add empty columns so we can append
old_data$district = NA
old_data$safetycommittee = NA

# drop quarters with no hours (non-active)
old_data$old_hours = as.numeric(old_data$old_hours)
old_data = old_data[which(old_data$old_hours > 0), ] 
old_data = old_data[, c("mineid", "quarter", "district", "safetycommittee")]

# append with mines data
mines = rbind(mines, old_data)
rm(old_data)

######################################################################################################

# FILL IN MINES DATASET WITH OPERATOR HISTORY

history = history[order(history$mineid),]
history$operatorid = as.character(history$operatorid)
history = history[!duplicated(history), ]

mines$operatorid = NA

fill_in_mines = function(t_mines) {
  
  mine = unique(t_mines$mineid)[1]
  
  t_history = history[history$mineid == mine, ]
  if (nrow(t_history) != 0) {
    t_history_o = t_history[, c("operatorid", "operatorstartdt", "operatorenddt")]
    t_history_o = t_history_o[!duplicated(t_history_o), ]
    
    for (i in 1:nrow(t_mines)) {
      id = NA
      for (j in 1:nrow(t_history_o)) {
        if (t_mines$quarter[i] >= t_history_o$operatorstartdt[j] 
               & t_mines$quarter[i] <= t_history_o$operatorenddt[j]) {
          id = t_history_o$operatorid[j]
        }
        t_mines$operatorid[i] = id
      }
    }
  }
  return(t_mines)
}

mines$mineid = as.character(mines$mineid)
mines$mineid = str_pad(mines$mineid, 7, pad = "0")
mines_new = ddply(mines, "mineid", fill_in_mines)

######################################################################################################

# get missing mine-quarters trusting given bounds
fill_in_ts = function(mine_df) {
  times = data.frame(quarter = seq(min(mine_df$quarter), max(mine_df$quarter), by = 0.25))
  full_ts = merge(times, mine_df, by = c("quarter"), all.x = TRUE)
  full_ts$mineid[is.na(full_ts$mineid)] = unique(full_ts$mineid)[1]
  return(full_ts)
}

mines_new_full = ddply(mines_new, "mineid", fill_in_ts)

######################################################################################################

mines_new_full$operator_time = NA
  
make_op_time = function(mine_data) {
  
  for (i in 1:nrow(mine_data)) {

    if (is.na(mine_data$operatorid[i])) {
      mine_data$operator_time[i] = NA
    }
    
    else { 
      
      if (i <= 4) {
        
        temp = mine_data[1:(i - 1), "operator_time"]
        
        if (length(temp[!is.na(temp)]) == 0) {
          mine_data$operator_time[i] = 1
        }
        
        else {
          mine_data$operator_time[i] = temp[!is.na(temp)][length(temp[!is.na(temp)])] + 1
        }
        
      }
      
      else {
        
        if (sum(is.na(mine_data[(i - 4):(i - 1), "operatorid"])) == 4) {
            mine_data$operator_time[i] = NA
        }
        
        else {
          
          temp = mine_data[(i - 4):(i - 1), "operator_time"]
          
          if (length(temp[!is.na(temp)]) == 0) {
            mine_data$operator_time[i] = 1
          }
          
          else {
            mine_data$operator_time[i] = temp[!is.na(temp)][length(temp[!is.na(temp)])] + 1
          }
          
        }
      }
    }
  }
  
  return(mine_data)
  
}

mines_with_ops = ddply(mines_new_full, "mineid", make_op_time)

mines_out = mines_with_ops[mines_with_ops$quarter >= 2000, ]

######################################################################################################

# BRING IN REAL DATA, MERGE ON NEW VARS, OUTPUT

if (injury.type == "MR") {
  # data = readRDS(MR_prediction_data_in_file_name)
  data = read.dta(MR_prediction_data_out_file_name)
  data = data[, c(-grep("controller", names(data)),
                  -grep("operator", names(data)),
                  -grep("safety", names(data)),
                  -grep("district", names(data)))]
}

if (injury.type == "PS") {
  # data = readRDS(PS_prediction_data_in_file_name)
  data = read.dta(PS_prediction_data_out_file_name)
  data = data[, c(-grep("controller", names(data)),
                  -grep("operator", names(data)),
                  -grep("safety", names(data)),
                  -grep("district", names(data)))]
}

# merge on new vars from mines and history datasets 
data = merge(data, mines_out, by = c("mineid", "quarter"))

# save new .dtas for Stata analysi 
if (injury.type == "MR") {
  stata.names = names(data)
  stata.names = gsub("\\.", "_", stata.names)
  stata.names = gsub("-", "_", stata.names)
  stata.names = gsub("penaltypoints", "pp", stata.names)
  stata.names = gsub("sigandsub", "ss", stata.names)
  names(data) = stata.names
  write.dta(data, file = MR_prediction_data_out_file_name)
}
if (injury.type == "PS") {
  stata.names = names(data)
  stata.names = gsub("\\.", "_", stata.names)
  stata.names = gsub("-", "_", stata.names)
  stata.names = gsub("penaltypoints", "pp", stata.names)
  stata.names = gsub("sigandsub", "ss", stata.names)
  names(data) = stata.names
  write.dta(data, file = PS_prediction_data_out_file_name)
}

######################################################################################################
