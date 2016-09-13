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

history = read.table(history_in_file_name, header = T, sep = "|", na.strings=c("","NA"))

######################################################################################################
