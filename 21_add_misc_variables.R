# NIOSH Project 2014-N-15776

# 21 - Add Misc Variables

# Last edit 11/2/16

######################################################################################################

  # input: clean mine-quarter level data (has observations dropped in preparation for prediction)
mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds"
  # input: controller/operator history data downloaded on 9/13/2016 from MSHA open data 
controller_operator_history_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ControllerOperatorHistory.txt"
  # input (and output): prediction-ready data containing all relevant and maybe relevant vars  - PS
PS_prediction_data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/PS_prediction_data.rds"
  # input (and output): prediction-ready data containing all relevant and maybe relevant vars  - MR
MR_prediction_data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"

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
names(history)[names(history) == "CONTROLLER_NAME"] = "controllername"
names(history)[names(history) == "OPERATOR_ID"] = "operatorid"
names(history)[names(history) == "OPERATOR_NAME"] = "operatorname"
names(history)[names(history) == "MINE_ID"] = "mineid"
names(history)[names(history) == "CONTROLLER_START_DT"] = "controllerstartdt"
names(history)[names(history) == "OPERATOR_START_DT"] = "operatorstartdt"
names(history)[names(history) == "CONTROLLER_END_DT"] = "controllerenddt"
names(history)[names(history) == "OPERATOR_END_DT"] = "operatorenddt"

# Format mineid
history$mineid = str_pad(history$mineid, 7, pad = "0")

# convert start/end dates into quarters
datevars = c("controllerstartdt", "operatorstartdt", "controllerenddt", "operatorenddt")
for (i in 1:length(datevars)) {
  history[, datevars[i]] = as.Date(as.character(history[, datevars[i]]), "%m/%d/%Y")
  history[, datevars[i]] = as.yearqtr(history[, datevars[i]])
}

######################################################################################################

# BRING IN REAL DATA, MERGE ON NEW VARS, OUTPUT

if (injury.type == "MR") {
  data = readRDS(MR_prediction_data_file_name)
}
if (injury.type == "PS") {
  data = readRDS(PS_prediction_data_file_name)
}

# merge on new vars from mine dataset
data = merge(data, mines, by = c("mineid", "quarter"))
             
# merge on new vars from history dataset - WHAT'S THE UNIT HERE?
data = merge(data, history, by = c(???????????????????????))

# save new .dtas for Stata analysi 
if (injury.type == "MR") {
    stata.names = names(data)
    stata.names = gsub("\\.", "_", stata.names)
    stata.names = gsub("-", "_", stata.names)
    stata.names = gsub("penaltypoints", "pp", stata.names)
    stata.names = gsub("sigandsub", "ss", stata.names)
    names(data) = stata.names
    write.dta(data, file = MR_prediction_data_file_name)
}
if (injury.type == "PS") {
    stata.names = names(data)
    stata.names = gsub("\\.", "_", stata.names)
    stata.names = gsub("-", "_", stata.names)
    stata.names = gsub("penaltypoints", "pp", stata.names)
    stata.names = gsub("sigandsub", "ss", stata.names)
    names(data) = stata.names
    write.dta(data, file = PS_prediction_data_file_name)
}

######################################################################################################
