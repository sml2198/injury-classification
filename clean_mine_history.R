# NIOSH Project 2014-N-15776

# ? - Clean Controller/Operator History Data
# Loads controller/operator history data

# Last edit 9/13/16

######################################################################################################

library(stringr)

# define file names
  # input: violations data
history_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ControllerOperatorHistory.txt"
  # output: clean violations data
history_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_history.rds"

######################################################################################################

# load data - 144065 obs, 13 vars
history = read.table(history_in_file_name, header = T, sep = "|", na.strings=c("","NA"))

# rename vars to make them prettier
names(history) = tolower(names(history))
names(history)[names(history) == "mine_id"] = "mineid"
names(history)[names(history) == "mine_name"] = "minename"
names(history)[names(history) == "mine_status"] = "minestatus"
names(history)[names(history) == "controller_id"] = "controllerid"
names(history)[names(history) == "operator_id"] = "operatorid"

# format variables
history$controllerid = as.character(history$controllerid)
history$operatorid = as.character(history$operatorid)
history$mineid = as.character(history$mineid)
history$controllerid = str_pad(history$controllerid, 7, pad = "0")
history$operatorid = str_pad(history$operatorid, 7, pad = "0")
history$mineid = str_pad(history$mineid, 7, pad = "0")

# drop obvs from wrong environment - now 63143 obs
history = history[history$coal_metal_ind == "C", ] 

# format date vars
history$controller_start_dt = as.Date(as.character(history$operator_start_dt), "%m/%d/%Y")
history$controller_end_dt = as.Date(as.character(history$operator_start_dt), "%m/%d/%Y")
history$operator_start_dt = as.Date(as.character(history$operator_start_dt), "%m/%d/%Y")
history$operator_end_dt = as.Date(as.character(history$operator_start_dt), "%m/%d/%Y")

history = history[, c(-grep("_dt", names(history)))]

######################################################################################################
