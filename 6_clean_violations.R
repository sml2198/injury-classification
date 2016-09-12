# NIOSH Project 2014-N-15776

# 6 - Clean Violations Data
  # Reads, cleans, then outputs violations data

# Last edit 8/3/16

######################################################################################################

library(stringr)

# define file names
  # input: violations data
open_data_viols_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Violations.txt"
  # output: clean violations data
    # uniquely identified by eventno (inspection number) * violationno (violation number)
open_data_viols_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds"

######################################################################################################

# READ AND CLEAN VIOLATIONS DATA, THEN OUTPUT

# read violations data - 2193591 obs, 61 vars
  # dataset downloaded on 8/16/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp [MSHA open data platform (Violations)]
open_data_viols = read.table(open_data_viols_in_file_name, header = T, sep = "|", na.strings=c("","NA"))

# drop data from environments not of interest
  # facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16
open_data_viols = open_data_viols[open_data_viols$COAL_METAL_IND == "C", ] # down to 1,193,181 obs (4865 mines)
open_data_viols = open_data_viols[open_data_viols$MINE_TYPE == "Underground", ] # drop 868,757 obs (1685 mines)

# rename variables
names(open_data_viols)[names(open_data_viols) == "VIOLATION_NO"] = "violationno"
names(open_data_viols)[names(open_data_viols) == "VIOLATION_ID"] = "violationid"
names(open_data_viols)[names(open_data_viols) == "EVENT_NO"] = "eventno"
names(open_data_viols)[names(open_data_viols) == "CONTRACTOR_ID"] = "contractorid"
names(open_data_viols)[names(open_data_viols) == "CONTROLLER_NAME"] = "controllername"
names(open_data_viols)[names(open_data_viols) == "FISCAL_YR"] = "fiscalyear"
names(open_data_viols)[names(open_data_viols) == "CAL_QTR"] = "calendarquarter"
names(open_data_viols)[names(open_data_viols) == "FISCAL_QTR"] = "fiscalquarter"
names(open_data_viols)[names(open_data_viols) == "CAL_YR"] = "calendaryear"
names(open_data_viols)[names(open_data_viols) == "COAL_METAL_IND"] = "coalcormetalm"
names(open_data_viols)[names(open_data_viols) == "INJ_ILLNESS"] = "injuryillness"
names(open_data_viols)[names(open_data_viols) == "SIG_SUB"] = "sigandsubdesignation"
names(open_data_viols)[names(open_data_viols) == "PRIMARY_OR_MILL"] = "primarymill"
names(open_data_viols)[names(open_data_viols) == "NO_AFFECTED"] = "numberaffected"
names(open_data_viols)[names(open_data_viols) == "SPECIAL_ASSESS"] = "specialassessment"
names(open_data_viols)[names(open_data_viols) == "MINE_ID"] = "mineid"
names(open_data_viols)[names(open_data_viols) == "RIGHT_TO_CONF_DT"] = "righttoconferencedate"
names(open_data_viols)[names(open_data_viols) == "VIOLATOR_TYPE_CD"] = "violatortypecode"
names(open_data_viols)[names(open_data_viols) == "TERMINATION_TYPE"] = "typeoftermination"
names(open_data_viols)[names(open_data_viols) == "TERMINATION_DT"] = "dateterminated"
names(open_data_viols)[names(open_data_viols) == "TERMINATION_TIME"] = "timeterminated"
names(open_data_viols)[names(open_data_viols) == "VACATE_TIME"] = "timevacated"
names(open_data_viols)[names(open_data_viols) == "WRITTEN_NOTICE"] = "writtennotice"
names(open_data_viols)[names(open_data_viols) == "ASMT_GENERATED_IND"] = "generatedbyassessmt"
names(open_data_viols)[names(open_data_viols) == "SECTION_OF_ACT_1"] = "typeaction1"
names(open_data_viols)[names(open_data_viols) == "SECTION_OF_ACT_2"] = "typeaction2"
names(open_data_viols)[names(open_data_viols) == "LATEST_TERM_DUE_DT"] = "latesttermduedate"
names(open_data_viols)[names(open_data_viols) == "VACATE_DT"] = "datevacated"
names(open_data_viols)[names(open_data_viols) == "VIOLATION_ISSUE_DT"] = "dateissued"
names(open_data_viols)[names(open_data_viols) == "CONTESTED_IND"] = "contestedindicator"
names(open_data_viols)[names(open_data_viols) == "MINE_NAME"] = "minename"
names(open_data_viols)[names(open_data_viols) == "MINE_TYPE"] = "minetype"
names(open_data_viols) = tolower(names(open_data_viols))

# format variables
open_data_viols$violationno = as.character(open_data_viols$violationno)
open_data_viols$eventno = as.character(open_data_viols$eventno)
open_data_viols$mineid = str_pad(open_data_viols$mineid, 7, pad = "0")
open_data_viols$eventno = str_pad(open_data_viols$eventno, 7, pad = "0")
open_data_viols$violationno = str_pad(open_data_viols$violationno, 7, pad = "0")

# flag and drop duplicates on violationno
open_data_viols[, "dup"] = duplicated(open_data_viols$violationno) # at this point we have 868,757 obs with 35 dups
open_data_viols = open_data_viols[open_data_viols$dup == F, ] # drop these 35 - dow to 868,711 obs (still 1685 mines)

# output clean violations data - 868,711 obs, 62 vars, 1685 unique mines
saveRDS(open_data_viols, file = open_data_viols_out_file_name)

######################################################################################################
