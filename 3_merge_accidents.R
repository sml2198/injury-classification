# NIOSH Project 2014-N-15776

# 3 - Merge Accidents and Mines Data
  # Reads, merges, then outputs cleaned mines (1_clean_mines) and accidents data (2_clean_accidents)

# Last edit 9/13/16

######################################################################################################

library(plyr)

# define file names
  # input: clean and merged accidents data (2_clean_accidents)
accidents_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_accidents.rds" 
  # input: clean and merged mines data (1_clean_mines) - ALL mines
mines_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/all_clean_mines.rds" 
  # output: merged mines and accidents data
mines_accidents_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"

######################################################################################################

# MERGE MINES AND ACCIDENTS DATA, THEN OUTPUT

# read data files
accidents = readRDS(accidents_file_name) # 75,016 obs, 56 vars, 1,469 unique mineids
mines_quarters = readRDS(mines_file_name) # 38,295 obs, 66 vars, 1895 unique mineids

# collapse mines data to the mine level (no quarters required) - these are already mine-level vars
temp = mines_quarters[, c("mineid", 
                          "year",
                          "quarter",
                          "minetype",
                          "minename",
                          "minestatus",
                          "minestatusdate",
                          "operatorid",
                          "operatorname",
                          "coalcormetalmmine",
                          "stateabbreviation",
                          "idate",
                          "idesc",
                          "daysperweek",
                          "productionshiftsperday")]

# average vars that are quarter-specific - wind up with 14,503 observations (equal to number of unique mineids, as it should be)
vars_to_avrg = ddply(mines_quarters[, c("hours_qtr",
                                          "employment_qtr", 
                                          "coal_prod_qtr", 
                                          "mineid")], c("mineid"), 
                       function(x) colMeans(x[, c(match("hours_qtr", names(x)), 
                                                  match("employment_qtr", names(x)), 
                                                  match("coal_prod_qtr", names(x)))], na.rm = TRUE))

# rename quarter specific vars so we don't get confused 
names(vars_to_avrg)[names(vars_to_avrg) == "hours_qtr"] = "avg_hours_qtr"
names(vars_to_avrg)[names(vars_to_avrg) == "employment_qtr"] = "avg_employment_qtr"
names(vars_to_avrg)[names(vars_to_avrg) == "coal_prod_qtr"] = "avg_coal_prod_qtr"

# merge the collapsed data with mine-level data
mines = merge(vars_to_avrg, temp, by = "mineid") # should have 38,295 observations, 18 vars, 1,895 unique mineids

# keep unique mine info for 1895 unique mines - get rid of quarterly info
mines = mines[!duplicated(mines$mineid), ]

# now remove useless datasets
rm(mines_quarters, temp, vars_to_avrg)

# merge mines and accidents data
mines_accidents = merge(accidents, mines, by = "mineid", all = T) # should have 75,531 obs, 1984 unique mineids

# remove obs only from mines data # now 75,016 obs unique on docno, 1469 unique mines
mines_accidents = mines_accidents[!is.na(mines_accidents$documentno),]

# keep mine-level information from mines data (.y)
mines_accidents = mines_accidents[, c(-grep("\\.x", names(mines_accidents)))]
names(mines_accidents) = gsub("\\.[x|y]", "", names(mines_accidents))

# output merged mines and accidents data - 75,016 unique accidents, 1469 unique mineids, 71 vars
saveRDS(mines_accidents, file = mines_accidents_file_name)
rm(list = ls())
gc()

######################################################################################################
