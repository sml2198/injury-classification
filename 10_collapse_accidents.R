# NIOSH Project 2014-N-15776

# 10 - Collapse Accidents 
# Loads in coded accidents data from 5_analyze_MR_R.R and formats variables for merge
# Loads and merges minetype data and drops observations not relevant to the study environment
# Collapses to the mine-quarter level, the outputs

# Last edit 7/29/16

#####################################################################################################

# define file names
# input: coded accidents data (contains MR indicator - no PS indicator yet) produced in 5_analyze_MR.R
mines_accidents_coded.in.file.name = "X:/Projects/Mining/NIOSH/analysis/data/4_coded/accidents_with_predictions.csv"
# output: collapsed coded accidents data 
mines_accidents_coded.out.file.name = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_accidents.rds"
# input: cleaned mine-types key produced in produced in 1_clean_mines.R
mine.types.file.name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"

######################################################################################################################################

# LOAD IN CODED ACCIDENTS DATA AND FORMAT VARIABLES FOR MERGE

# Read in data
mine_types = readRDS(mine.types.file.name)
mines_accidents_coded = read.csv(mines_accidents_coded.in.file.name)

# Format mineid (for merges), date and quarter vars for merges
mines_accidents_coded$mineid = str_pad(mines_accidents_coded$mineid, 7, pad = "0")

# Format mineid and pad it with zeroes to make it 7 digits, so we have a clean merge
mines_accidents_coded$mineid = withr::with_options(c(scipen = 999), str_pad(mines_accidents_coded$mineid, 7, pad = "0"))
mines_accidents_coded$accidentdate = as.Date(as.character(mines_accidents_coded$accidentdate), "%m/%d/%Y")
mines_accidents_coded$quarter = as.yearqtr(mines_accidents_coded$accidentdate)

######################################################################################################################################

# READ MINE TYPE DATA, MERGE ONTO ACCIDENTS, DROP OBSERVATIONS NOT RELEVANT TO THE STUDY ENVIRONMENT

# Drop observations before our study period
mines_accidents_coded = mines_accidents_coded[(mines_accidents_coded$quarter > "1999 Q4"),]

# Merge on minetypes to drop non-coal and non-underground observations before saving
mines_accidents_coded = merge(mines_accidents_coded, mine_types, by = c("mineid"), all = T)

# Drop non-merging observations
mines_accidents_coded = mines_accidents_coded[!is.na(mines_accidents_coded$MR),]
rm(mine_types)

# Only keep observations from environment we care about
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$minetype == "Underground",]
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$subunit == "UNDERGROUND",]
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$coalcormetalmmine.x == "C",]

######################################################################################################################################

# COLLAPSE TO THE MINE-QUARTER LEVEL, THEN OUTPOUT

# Create injury indicator so that we can collapse & sum total injuries per mine quarter
mines_accidents_coded$totalinjuries = 1

# Collapse mines_accidents data here.
summed_coded_accidents = ddply(mines_accidents_coded[, c(grep("totalinjuries", names(mines_accidents_coded)), 
                                                         grep("MR", names(mines_accidents_coded)),
                                                         match("mineid", names(mines_accidents_coded)), 
                                                         match("quarter", names(mines_accidents_coded)))], c("mineid", "quarter"), 
                               function(x) colSums(x[, c(grep("totalinjuries", names(x)), grep("MR", names(x)))], na.rm = T))

# Save and clear workspace
saveRDS(summed_coded_accidents, mines_accidents_coded.out.file.name)
rm(list = ls())
gc()

######################################################################################################################################
