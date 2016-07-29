# NIOSH Project 2014-N-15776

# 3 - Merge Accidents and Mines Data
  # Reads, merges, then outputs cleaned mines and accidents data

# Last edit 7/19/16

######################################################################################################

setwd("X:/Projects/Mining/NIOSH/analysis/")

# define file names
  # input: clean and merged accidents data
accidents.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_accidents.rds" 
  # input: clean and merged mines data
mines.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds" 
  # output: merged mines and accidents data
mines.accidents.file.name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"

######################################################################################################

# MERGE MINES AND ACCIDENTS DATA, THEN OUTPUT

# read data files
accidents = readRDS(accidents.file.name)
mines_quarters = readRDS(mines.file.name)

# merge mines and accidents data
mines.accidents = merge(accidents, mines_quarters,by = "mineid", all = TRUE) # should have 675887 observations

# keep mine-level information from mines data (.y)
mines.accidents = mines.accidents[, c(-grep("\\.x", names(mines.accidents)))]
names(mines.accidents) = gsub("\\.[x|y]", "", names(mines.accidents))

# drop mines that don't merge with any accidents (from 739004 to 675902)
mines.accidents = mines.accidents[!(is.na(mines.accidents$documentno) | mines.accidents$documentno == ""), ]
mines.accidents = mines.accidents[!(is.na(mines.accidents$mineid) | mines.accidents$mineid == ""), ]

# drop problematic observations
mines.accidents = mines.accidents[mines.accidents$problem != 1, ]
mines.accidents = mines.accidents[, c(-match("problem", names(mines.accidents)))]

# output merged mines and accidents data
saveRDS(mines.accidents, file = mines.accidents.file.name)

######################################################################################################
