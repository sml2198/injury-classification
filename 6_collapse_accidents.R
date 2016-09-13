# NIOSH Project 2014-N-15776

# 6 - Collapse Accidents Data
# Loads in coded accidents data from 5_analyze_MR_R.R and formats variables for merge
# Loads and merges minetype data and drops observations not relevant to the study environment
# Collapses to the mine-quarter level, the outputs

# Last edit 7/29/16

#####################################################################################################

library(plyr)
library(stringr)
library(zoo)

# define file names
  # input: cleaned mine-types key produced in produced in 1_clean_mines.R
mine_types_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds"
  # input: coded MR accidents data produced in 5_analyze_MR.R
MR_accidents_coded_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_coded/MR_accidents_with_predictions.csv"
  # input: coded PS accidents data produced in 4_analyze_PS.R
PS_accidents_coded_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_coded/PS_accidents_with_predictions.csv"

  # output: collapsed coded accidents data 
MR_accidents_coded_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_MR_accidents.rds"
# output: collapsed coded accidents data 
PS_accidents_coded_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_PS_accidents.rds"

######################################################################################################################################

# Set preferences 

# injury.type = "PS"
 injury.type = "MR"

######################################################################################################################################

# LOAD IN CODED ACCIDENTS DATA AND FORMAT VARIABLES FOR MERGE

# Read in data
mine_types = readRDS(mine_types_file_name)

if (injury.type == "PS"){
  mines_accidents_coded = read.csv(PS_accidents_coded_in_file_name)
  
  # Remove unecessary vars
  mines_accidents_coded = mines_accidents_coded[, c(match("mineid", names(mines_accidents_coded)),
                                                    match("accidentdate", names(mines_accidents_coded)),
                                                    match("PS", names(mines_accidents_coded)))]
}
if (injury.type == "MR"){
  mines_accidents_coded = read.csv(MR_accidents_coded_in_file_name)
  
  # Remove unecessary vars
  mines_accidents_coded = mines_accidents_coded[, c(match("mineid", names(mines_accidents_coded)),
                                                    match("accidentdate", names(mines_accidents_coded)),
                                                    match("MR", names(mines_accidents_coded)))]
}

# Format mineid by padding it with zeroes to make it 7 digits, so we have a clean merge
mines_accidents_coded$mineid = str_pad(mines_accidents_coded$mineid, 7, pad = "0")
mines_accidents_coded$mineid = withr::with_options(c(scipen = 999), str_pad(mines_accidents_coded$mineid, 7, pad = "0"))

# Format date vars
mines_accidents_coded$accidentdate = as.Date(as.character(mines_accidents_coded$accidentdate), "%m/%d/%Y")
mines_accidents_coded$quarter = as.yearqtr(mines_accidents_coded$accidentdate)

######################################################################################################################################

# COLLAPSE TO THE MINE-QUARTER LEVEL, THEN OUTPUT

# Drop accidents from too early 
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$quarter > "1999 Q4" & 
                                                mines_accidents_coded$quarter < "2016 Q2",]

# Create injury indicator so that we can collapse & sum total injuries per mine quarter
mines_accidents_coded$total_injuries = 1

# Merge on minetypes to drop non-coal and non-underground observations before saving
mines_accidents_coded = merge(mines_accidents_coded, mine_types, by = c("mineid"), all = T)

# Drop non-merging observations
if (injury.type == "PS"){
  mines_accidents_coded = mines_accidents_coded[!is.na(mines_accidents_coded$PS),]
}
if (injury.type == "MR"){
  mines_accidents_coded = mines_accidents_coded[!is.na(mines_accidents_coded$MR),]
}
rm(mine_types)

# Only keep observations from environment we care about
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$minetype == "Underground",]
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$coalcormetalmmine == "C",]

######################################################################################################################################

# Collapse mines_accidents data here.
if (injury.type == "PS"){
  summed_coded_accidents = ddply(mines_accidents_coded[, c(grep("total_injuries", names(mines_accidents_coded)), 
                                                           grep("PS", names(mines_accidents_coded)),
                                                           match("mineid", names(mines_accidents_coded)), 
                                                           match("quarter", names(mines_accidents_coded)))], c("mineid", "quarter"), 
                                 function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("PS", names(x)))], na.rm = T))
}
if (injury.type == "MR"){
  summed_coded_accidents = ddply(mines_accidents_coded[, c(grep("total_injuries", names(mines_accidents_coded)), 
                                                           grep("MR", names(mines_accidents_coded)),
                                                           match("mineid", names(mines_accidents_coded)), 
                                                           match("quarter", names(mines_accidents_coded)))], c("mineid", "quarter"), 
                                 function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("MR", names(x)))], na.rm = T))
}

######################################################################################################################################

# Save and clear workspace
if (injury.type == "PS"){
  saveRDS(summed_coded_accidents, PS_accidents_coded_out_file_name)
}
if (injury.type == "MR"){
  saveRDS(summed_coded_accidents, MR_accidents_coded_out_file_name)
}

rm(list = ls())
gc()

######################################################################################################################################
