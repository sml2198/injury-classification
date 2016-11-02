# NIOSH Project 2014-N-15776

# 1b_clean_eia_data.R

# Last edit 10/16/16

######################################################################################################

# This file converts the EIA data. The EIA data is from 1983-2009 
# and is in four separate .csv files. This file merges them together and creates a .dta file.
# We will use the append function to append the files together. First we will insheet them 
# and save them as temp files.

library(stringr)

# inputs
  # eia data 5 (1999-2013)
eia_5_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/2015-05-29 EIA7a data update/Book1.csv"
  # longwall data 1: 1992-2008 (from Chris Mark at NIOSH to Nate Atkinson and Brian Karfunkel on 6/20/11)
longwall_1_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Chris Mark/longwallID1992_2008.csv"
  # longwall data 2: 2009 (from Deno Pappas at NIOSH to Nate Atkinson and Brian Karfunkel on 12/13/11)
longwall_2_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Deno Pappas/LW2009.csv"
  # longwall data 3: 2010 (from Deno Pappas at NIOSH to Nate Atkinson and Brian Karfunkel on 12/13/11)
longwall_3_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Deno Pappas/LW2010.csv"
  # longwall data 4: 2011 (from Deno Pappas at NIOSH to Kristen Altenburger and Ted Westling on 10/26/12)
longwall_4_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Deno Pappas/LW2011.csv"
  # longwall data 5: 2012 (from Deno Pappas at NIOSH to Ted Westling on 9/8/13)
longwall_5_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Deno Pappas/LW2012.csv"
  # longwall data 6: 2013 (from  Linda McWilliams on  10/17/16)
longwall_6_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Linda McWilliams/LW2013.csv"
  # longwall data 7: 2014 (from  Linda McWilliams on  10/17/16)
longwall_7_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Linda McWilliams/LW2014.csv"
  # longwall data 8: 2015 (from  Linda McWilliams on  10/17/16)
longwall_8_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/NIOSH/Data from Linda McWilliams/LW2015.csv"


# output
  # key with mine-year specific union information
eia_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_eia.rds"

######################################################################################################

# BRING IN EIA DATA USED FOR UNDERREPORTING (LATEST DATA PULL FROM JOE CONKLIN)

# Import
eia = read.csv(eia_5_in_file_name, header = T, na.strings = c("","NA"))

# Clean var names
names(eia)[names(eia) == "MSHA_ID"] = "mineid"
names(eia) = tolower(names(eia))
eia = eia[, c(match("mineid", names(eia)),
              match("year", names(eia)),
              match("mine_type_code", names(eia)),
              match("company_type", names(eia)),
              match("operation_type", names(eia)),
              match("longwall_pct", names(eia)),
              match("union_id", names(eia)))]

# Format mineid
eia$mineid = str_pad(eia$mineid, 7, pad = "0")

# Drop years before study period
eia = eia[eia$year > 1999, ]

# Rename company-type categories
eia$company_type = ifelse(eia$mine_type == 1, "independent", eia$mine_type)
eia$company_type = ifelse(eia$mine_type == 2, "subsidiary", eia$mine_type)
eia$company_type = ifelse(eia$mine_type == 3, "contractor", eia$mine_type)

# Rename mine_type categories
eia$mine_type = ifelse(eia$mine_type == 0, "prep plant", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 1, "underground", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 2, "strip", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 4, "auger", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 6, "strip/auger combo ", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 8, "refuse", eia$mine_type)
  
# Rename operation_type categories
eia$operation_type = ifelse(eia$operation_type == 1, "mine", eia$operation_type)
eia$operation_type = ifelse(eia$operation_type == 2, "prep plant", eia$operation_type)
eia$operation_type = ifelse(eia$operation_type == 3, "mine and prep plant", eia$operation_type)

# Replace union ID with "None" with missing & create union indicator
eia$union_id = ifelse(is.na(eia$union_id), 0, eia$union_id)
eia$union = ifelse(eia$union_id != 0, 1, 0)

# Collapse to the mine_year level - longwall_pct is just because ddply needs two arguments
eia = ddply(eia[, c(match("union", names(eia)),
                    match("longwall_pct", names(eia)),
                    match("year", names(eia)),
                    match("mineid", names(eia)))], c("mineid", "year"), 
            function(x) colMeans(x[, c(match("union", names(x)),
                                       match("longwall_pct", names(x)))], na.rm = T))
eia = eia[order(eia$mineid, eia$year), c(-grep("longwall_pct", names(eia)))]

######################################################################################################

# BRING IN LONGWALL DATA
  
# Import, clean & keep only mineid, year, and longwall indicator for all three longwall sheets
longwall.1 = read.csv(longwall_1_in_file_name, header = T, na.strings = c("", "NA"))
names(longwall.1) = tolower(names(longwall.1))
# keep only years after 2000
longwall.1 = longwall.1[longwall.1$year > 1999, ]
names(longwall.1)[names(longwall.1) == "lw.1"] = "longwall"
longwall.1 = longwall.1[, c("mineid", "year", "longwall")]

# two
longwall.2 = read.csv(longwall_2_in_file_name, header = T, na.strings = c("", "NA"))
names(longwall.2) = tolower(names(longwall.2))
names(longwall.2)[names(longwall.2) == "lw"] = "longwall"
longwall.2 = longwall.2[which(!is.na(longwall.2$year)), c("mineid", "year", "longwall")]

# three
longwall.3 = read.csv(longwall_3_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.3) = tolower(names(longwall.3))
# create longwall indicator - everything in this sheet is a longwall mine
longwall.3$longwall = 1
longwall.3 = longwall.3[which(!is.na(longwall.3$year)), c("mineid", "year", "longwall")]

# four
longwall.4 = read.csv(longwall_4_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.4) = tolower(names(longwall.4))
names(longwall.4)[names(longwall.4) == "lw"] = "longwall"
longwall.4 = longwall.4[, c("mineid", "year", "longwall")]

# five
longwall.5 = read.csv(longwall_5_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.5) = tolower(names(longwall.5))
names(longwall.5)[names(longwall.5) == "lw"] = "longwall"
longwall.5 = longwall.5[, c("mineid", "year", "longwall")]

# six
longwall.6 = read.csv(longwall_6_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.6) = tolower(names(longwall.6))
names(longwall.6)[names(longwall.6) == "lw"] = "longwall"
longwall.6 = longwall.6[, c("mineid", "year", "longwall")]
longwall.6 = longwall.6[which(!is.na(longwall.6$mineid) & !(is.na(longwall.6$year))), ]

# seven
longwall.7 = read.csv(longwall_7_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.7) = tolower(names(longwall.7))
names(longwall.7)[names(longwall.7) == "lw"] = "longwall"
longwall.7 = longwall.7[, c("mineid", "year", "longwall")]
longwall.7 = longwall.7[which(!is.na(longwall.7$mineid) & !(is.na(longwall.7$year))), ]

# eight
longwall.8 = read.csv(longwall_8_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.8) = tolower(names(longwall.8))
names(longwall.8)[names(longwall.8) == "lw"] = "longwall"
longwall.8 = longwall.8[, c("mineid", "year", "longwall")]
longwall.8 = longwall.8[which(!is.na(longwall.8$mineid)), ]

# append longwall datasets
longwall = rbind(longwall.1, longwall.2, longwall.3, longwall.4, longwall.5, longwall.6, longwall.7, longwall.8)
longwall = longwall[which(!is.na(longwall$mineid) & !is.na(longwall$year)), ] # should be 0
rm(longwall.1, longwall.2, longwall.3, longwall.4, longwall.5, longwall.6, longwall.7, longwall.8)

# format mineid
longwall$mineid = str_pad(longwall$mineid, 7, pad = "0")

# append eia and longwall data
eia = merge(eia, longwall, by = c("mineid", "year"), all = T)

# replace longwall with zero if it's not a 1 and the year is one for which we have data (2000-2015)
eia$longwall = ifelse(is.na(eia$longwall) & eia$year < 2016, 0, eia$longwall)

# replace union with zero if it's not a 1 and the year is one for which we have data (2000-2013)
eia$union = ifelse(is.na(eia$union) & eia$year < 2014, 0, eia$union)

# save
saveRDS(eia, eia_out_file_name)

######################################################################################################

# inputs
#   # eia 1
# eia_1_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A Data I.csv"
#   # eia 2
# eia_2_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A Data II.csv"
#   # eia 3
# eia_3_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A Data III.csv"
#   # eia 4
# eia_4_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/EIA/EIA/EIA7A_IV(2010_data_included).csv"

# # BRING IN ALL EIA DATA USED IN PREVIOUS PILOT PROJECT
# 
# # Insheet EIA_1: 22647 obs
# eia.1 = read.csv(eia_1_in_file_name, header = T, na.strings = c("", "NA"))
# 
# # Insheet EIA_2: 20934 obs
# eia.2 = read.csv(eia_2_in_file_name, header = T, na.strings = c("", "NA"))
# 
# # Insheet EIA_3: 17868 obs
# eia.3 = read.csv(eia_3_in_file_name, header = T, na.strings = c("", "NA"))
# 
# # Insheet EIA_4: 15492 obs
# # EIA7A_IV(2010_data_included).csv contains data from 2010 and was received from EIA on December 7th, 2011.
# eia.4 = read.csv(eia_4_in_file_name, header = T, na.strings = c("", "NA"))
# 
# # Order vars to make comparison easier
# eia.1 = eia.1[, order(names(eia.1))]
# eia.2 = eia.2[, order(names(eia.2))]
# eia.3 = eia.3[, order(names(eia.3))]
# eia.4 = eia.4[, order(names(eia.4))]
# 
# # Rename MSHA ID mine ID: When we import the data, we have a field for MINE_ID and a field for MSHA_ID. 
# # Mine id is empty at every observation, whereas msha id is missing in about 2% of cases. Looking at 
# # MSHA id, we can see it is the same as mine id for our other files, so we drop the mine id and rename 
# # msha id to mine id.
# names(eia.1)[names(eia.1) == "MSHA_ID"] = "mineid"
# names(eia.2)[names(eia.2) == "MSHA_ID"] = "mineid"
# names(eia.3)[names(eia.3) == "MSHA_ID"] = "mineid"
# names(eia.4)[names(eia.4) == "MSHA_ID"] = "mineid"
# names(eia.1) = tolower(names(eia.1))
# names(eia.2) = tolower(names(eia.2))
# names(eia.3) = tolower(names(eia.3))
# names(eia.4) = tolower(names(eia.4))
# 
# # The last dataset has one column different from the others. Replace & rename it here to facilitate append.
# # one = names(eia.1)
# # two = names(eia.2)
# # three = names(eia.3)
# # four = names(eia.4)
# # test = setdiff(one, four) # status_date/ state
# eia.4$state = NA
# names(eia.4)[names(eia.4) == "state"] = "status_date"
# 
# # Append: 22647 + 20934 + 17868 + 15492 = 76941 obvs (good!)
# eia = rbind(eia.1, eia.2, eia.3, eia.4)
# rm(eia.1, eia.2, eia.3, eia.4)
# eia = eia[, c(-grep("^bed[0-9]", names(eia)),
#               -grep("^captive", names(eia)))]

######################################################################################################
