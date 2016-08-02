# NIOSH Project 2014-N-15776

# 3 - Merge Accidents and Mines Data
  # Reads, merges, then outputs cleaned mines (1_clean_mines) and accidents data (2_clean_accidents)

# Last edit 8/1/16

######################################################################################################

# define file names
  # input: clean and merged accidents data
accidents_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_accidents_TEST.rds" 
  # input: clean and merged mines data
mines_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines_TEST.rds" 
  # output: merged mines and accidents data
mines_accidents_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents_TEST.rds"

######################################################################################################

# MERGE MINES AND ACCIDENTS DATA, THEN OUTPUT

# read data files
accidents = readRDS(accidents_file_name)
mines_quarters = readRDS(mines_file_name)

# merge mines and accidents data
mines_accidents = merge(accidents, mines_quarters,by = "mineid", all = TRUE) # should have 4659780 observations

# keep mine-level information from mines data (.y)
mines_accidents = mines_accidents[, c(-grep("\\.x", names(mines_accidents)))]
names(mines_accidents) = gsub("\\.[x|y]", "", names(mines_accidents))

# drop mines that don't merge with any accidents (from 739004 to 675902)
mines_accidents = mines_accidents[!(is.na(mines_accidents$documentno) | mines_accidents$documentno == ""), ]
mines_accidents = mines_accidents[!(is.na(mines_accidents$mineid) | mines_accidents$mineid == ""), ]

# drop problematic observations
mines_accidents = mines_accidents[mines_accidents$problem != 1, ]
mines_accidents = mines_accidents[, c(-match("problem", names(mines_accidents)))]

# output merged mines and accidents data
saveRDS(mines_accidents, file = mines_accidents_file_name)

######################################################################################################
