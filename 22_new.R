# NIOSH Project 2014-N-15776

# 21 - Add Misc Variables

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

injury.type = "PS"
#injury.type = "MR"

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
# controller dummies (controllerid)
# operator dummies (operatorid)
# mine age (to be constructed from mine status and operator history)
# controller production (to be generated from production and controller id - logged and unlogged)

# read controller history data - 144,065 obs, 13 vars 
# dataset downloaded on 9/13/2016 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
history = read.table(controller_operator_history_in_file_name, header = T, sep = "|")

# drop data from environments not of interest
history = history[(history$COAL_METAL_IND == "C"), ] # down to 63143 obs

# rename variables
names(history)[names(history) == "CONTROLLER_ID"] = "controllerid"
names(history)[names(history) == "OPERATOR_ID"] = "operatorid"
names(history)[names(history) == "MINE_ID"] = "mineid"
names(history)[names(history) == "CONTROLLER_START_DT"] = "controllerstartdt"
names(history)[names(history) == "OPERATOR_START_DT"] = "operatorstartdt"
names(history)[names(history) == "CONTROLLER_END_DT"] = "controllerenddt"
names(history)[names(history) == "OPERATOR_END_DT"] = "operatorenddt"
history = history[, c("controllerid", "operatorid", "mineid", "controllerstartdt", "operatorstartdt", "controllerenddt", "operatorenddt")]

# Format mineid
history$mineid = as.character(history$mineid)
history$mineid = str_pad(history$mineid, 7, pad = "0")

# convert start/end dates into quarters & replace end quarter with 2016 if missing
enddtvars = c("controllerenddt", "operatorenddt")
for (i in 1:length(enddtvars)) {
  history[, enddtvars[i]] = as.character(history[, enddtvars[i]])
  history[, enddtvars[i]] = ifelse(history[, enddtvars[i]] == "", NA, history[, enddtvars[i]])
  history[, enddtvars[i]] = ifelse(is.na(history[, enddtvars[i]]), "01/01/2016", history[, enddtvars[i]]) # Q1 2016
}
datevars = c("controllerstartdt", "operatorstartdt", "controllerenddt", "operatorenddt")
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

# NOW FILL IN MINES DATASET WITH OPERATOR HISTORY

history = history[order(history$mineid),]
history$controllerid = as.character(history$controllerid)
history$operatorid = as.character(history$operatorid)
history = history[!duplicated(history), ]

# how many controller/operator columns?
check_o = history[, c("mineid", "operatorid", "operatorstartdt", "operatorenddt")]

num_operators = 0
for (mine in unique(mines$mineid)) {
  
  temp_o = check_o[check_o$mineid == mine, ]
  temp_o = temp_o[!duplicated(temp_o), ]
  
  num_operators_temp = nrow(temp_o)
  num_operators = max(num_operators, num_operators_temp)
  
}
num_operators # 21

# make new operator and controller columns in mines
operator = data.frame(matrix(NA, nrow = nrow(mines), ncol = num_operators))  
names(operator) = paste("operatorid", 1:num_operators, sep = "_")
mines = cbind(mines, operator)

# fill in operator and controller ids
fill_in_mines = function(t_mines) {
  mine = unique(t_mines$mineid)[1]
  print(mine)
  
  t_history = history[history$mineid == mine, ]
    if (nrow(t_history) != 0) {
    t_history_o = t_history[, c("operatorid", "operatorstartdt", "operatorenddt")]
    t_history_o = t_history_o[!duplicated(t_history_o), ]
    
    for (i in 1:nrow(t_mines)) {
      for (k in 1:nrow(t_history_o)) {
        t_mines[i, paste("operatorid", k, sep = "_")] = 
          ifelse(t_mines$quarter[i] >= t_history_o$operatorstartdt[k] 
                 & t_mines$quarter[i] < t_history_o$operatorenddt[k], 
                 t_history_o$operatorid[k], NA)
      }
    }
  }
  return(t_mines)
}

mines$mineid = as.character(mines$mineid)
mines$mineid = str_pad(mines$mineid, 7, pad = "0")
mines_new = ddply(mines, "mineid", fill_in_mines)

t_mines = mines[mines$mineid == "3304321", ]
# how many controllers/operators at once?
c = mines_new[, grep("controller", names(mines_new))]
max(rowSums(!is.na(c))) # 13
o = mines_new[, grep("operator", names(mines_new))]
max(rowSums(!is.na(o))) # 1

# collapse non-overlapping controllers/operators into the same column
mines_new2 = mines_new

mines_final = mines[c("mineid", "quarter", "district", "safetycommittee")]

mines_final$operatorid = NA

controller = data.frame(matrix(NA, nrow = nrow(mines_final), ncol = max(rowSums(!is.na(c)))))  
names(controller) = paste("controllerid", 1:max(rowSums(!is.na(c))), sep = "_")
mines_final = cbind(mines_final, controller)

# this takes a while to run
# we are all works in progress
for (i in 1:nrow(mines_final)) {
  print(i) # gives you an idea how far along you are...
  
  mines_final$operatorid[i] = ifelse(sum(!is.na(mines_new[i, grep("operator", names(mines_new))])) == 0, 
                                     NA, mines_new[i, grep("operator", names(mines_new))[!is.na(mines_new[i, grep("operator", names(mines_new))])]])
  
  cont = mines_new[i, grep("controller", names(mines_new))[!is.na(mines_new[i, grep("controller", names(mines_new))])]]
  cont = c(cont, rep(NA, max(rowSums(!is.na(c))) - length(cont)))
  mines_final[i, paste("controllerid", 1:max(rowSums(!is.na(c))), sep = "_")] = cont
  
}

######################################################################################################

# age of current operator
operator_history = history[, c("operatorid", "operatorstartdt")]
operator_history = operator_history[!duplicated(operator_history), ]

get_min = function(op_hist) {
  return(op_hist[op_hist$operatorstartdt == min(op_hist$operatorstartdt), ])
}

operator_first = ddply(operator_history, "operatorid", get_min)

# this will take a while to run
# we are all works in progress
mines_final$operator_age = NA
for (i in 1:nrow(mines_final)) {
  mines_final$operator_age[i] = ifelse(is.na(mines_final$operatorid[i]), NA,
                                       mines_final$quarter[i] - operator_first[operator_first$operatorid == mines_final$operatorid[i], "operatorstartdt"])
}

# number of years operator has been at given mine
operator_mine_history = history[, c("mineid", "operatorid", "operatorstartdt")]
operator_mine_history = operator_mine_history[!duplicated(operator_mine_history), ]

operator_mine_dups = operator_mine_history[, c("mineid", "operatorid")]
operator_mine_dups = operator_mine_dups[duplicated(operator_mine_dups), ]
operator_mine_dups = operator_mine_dups[!duplicated(operator_mine_dups), ]

fill_in_mine_op_history = function(temp) {
  for (i in 1:nrow(temp)) {
    if (is.na(temp$operatorid[i])) {
      temp$operator_mine_age[i] = NA
    }
    else {
      if(temp$mineid[i] %in% operator_mine_dups$mineid & temp$operatorid[i] %in% operator_mine_dups[operator_mine_dups$mineid == temp$mineid[i], "operatorid"]) {
        years = operator_mine_history[operator_mine_history$mineid == temp$mineid[i] & operator_mine_history$operatorid == temp$operatorid[i], "operatorstartdt"]
        years = years[years <= temp$quarter[i]]
        temp$operator_mine_age[i] = min(temp$quarter[i] - years)
      }
      else {
        first = operator_mine_history[operator_mine_history$mineid == temp$mineid[i] & operator_mine_history$operatorid == temp$operatorid[i], "operatorstartdt"]
        temp$operator_mine_age[i] = temp$quarter[i] - first
      }
    }
  }
  return(temp)
}

mines_final$operator_mine_age = NA
mines_final2 = ddply(mines_final, "mineid", fill_in_mine_op_history)

######################################################################################################

# BRING IN REAL DATA, MERGE ON NEW VARS, OUTPUT

if (injury.type == "MR") {
  data = readRDS(MR_prediction_data_in_file_name)
}
if (injury.type == "PS") {
  data = readRDS(PS_prediction_data_in_file_name)
}

# merge on new vars from mines and history datasets 
data = merge(data, mines_final2, by = c("mineid", "quarter"))

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
