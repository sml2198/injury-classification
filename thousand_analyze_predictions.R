# NIOSH Project 2014-N-15776

# 24 - Analyze Predictions

# Last edit 11/14/16

######################################################################################################

library(foreign)

# set preferences
date = "11-14/"

injtype = "PS"
# injtype = "MR"

subpart.form = "rate"
# subpart.form = "not-a-rate"

# define file names
folder = "X:/Projects/Mining/NIOSH/analysis/results/dta/"
root = paste0(folder, date, collapse = NULL)

# load in model predictions - the "x" indicates it's bee saved as Stata 12 for hte "foreign" package will work
rate_data_in_file_name = paste0(root, injtype, "_with_predictionsx.dta", collapse = NULL)
nonrate_data_in_file_name = paste0(root, injtype, "_with_predictions_non-ratex.dta", collapse = NULL)

# KEY
# null 1 - weak null
# null 2 - strong null (total violations - 1 lag)
# null 3 - strong null (total violations/onsite inspection hours - 1 lag)

######################################################################################################

# LOAD DATA

rate_data = read.dta(rate_data_in_file_name)
nonrate_data = read.dta(nonrate_data_in_file_name)

######################################################################################################
