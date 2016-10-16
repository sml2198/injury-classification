# NIOSH Project 2014-N-15776

# 1b_clean_eia_data.R

# Last edit 10/16/16

######################################################################################################

# This file converts the EIA data. The EIA data is from 1983-2009 
# and is in four separate .csv files. This file merges them together and creates a .dta file.
# We will use the append function to append the files together. First we will insheet them 
# and save them as temp files.

# inputs
  # eia 1
eia_1_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A Data I.csv"
  # eia 2
eia_2_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A Data II.csv"
  # eia 3
eia_3_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A Data III.csv"
  # eia 4
eia_4_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A_IV(2010_data_included).csv"

######################################################################################################

# Insheet EIA_1
eia.1 = read.csv(eia_1_in_file_name, header = T, na.strings = c("", "NA"))

# Insheet EIA_2
eia.2 = read.csv(eia_2_in_file_name, header = T, na.strings = c("", "NA"))

# Insheet EIA_3
eia.3 = read.csv(eia_3_in_file_name, header = T, na.strings = c("", "NA"))

# Insheet EIA_4
# EIA7A_IV(2010_data_included).csv contains data from 2010 and was received from EIA on December 7th, 2011.
eia.4 = read.csv(eia_4_in_file_name, header = T, na.strings = c("", "NA"))


# Rename MSHA ID mine ID: When we import the data, we have a field for MINE_ID and a field for MSHA_ID. 
# Mine id is empty at every observation, whereas msha id is missing in about 2% of cases. Looking at 
# MSHA id, we can see it is the same as mine id for our other files, so we drop the mine id and rename 
# msha id to mine id.
eia = c("eia.1", "eia.2", "eia.3", "eia.4")
for (i in 1:length(eia)) {
  names(eia[i])[names(eia[i]) == "MSHA_ID"] = "mine_id"
  names(eia[i]) = tolower(names(eia[i]))
}

# append
merge(eia.1, eia.2, eia.3, eia.4, by = "")

# save
save "$PROJECT_ROOT/data/1_converted/eia_data.dta", replace

######################################################################################################

