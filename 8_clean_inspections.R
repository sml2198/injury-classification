# NIOSH Project 2014-N-15776

# 8 - Clean Inspections
  # Reads and cleans inspections data 
  # Reads clean mine type data (1_clean_mines)
  # Merges and cleans inspections data and mine type data
  # Reads and cleans inspection hours data 
  # Merges inspections data with inspection hours data
  # Outputs merged and clean inspections data

# Last edit 8/3/16

######################################################################################################

library(stringr)

# define file names
  # input: inspections data
open_data_inspecs_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Inspections.txt"
  # input: clean mine type data (1_clean_mines)
mine_types_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds"
  # output: clean and merged inspections data, uniquely identified by mineid *eventno
open_data_inspecs_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_inspections.rds"

######################################################################################################

# READ AND CLEAN INSPECTIONS DATA

# read inspections data - 790234 obs, 45 vars
  # dataset downloaded on 8/16/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp [Inspections]
open_data_inspecs = read.table(open_data_inspecs_in_file_name, header = T, sep = "|")

# drop observations for environments not of interest
open_data_inspecs = open_data_inspecs[open_data_inspecs$COAL_METAL_IND != "M", ] # now we have 327,777 obs

# rename variables
names(open_data_inspecs)[names(open_data_inspecs) == "EVENT_NO"] = "eventno"
names(open_data_inspecs)[names(open_data_inspecs) == "MINE_ID"] = "mineid"
names(open_data_inspecs)[names(open_data_inspecs) == "INSPECTION_BEGIN_DT"] = "beginningdate"
names(open_data_inspecs)[names(open_data_inspecs) == "INSPECTION_END_DT"] = "endingdate"
names(open_data_inspecs)[names(open_data_inspecs) == "CAL_YR"] = "calendaryear"
names(open_data_inspecs)[names(open_data_inspecs) == "CAL_QTR"] = "calendarquarter"
names(open_data_inspecs)[names(open_data_inspecs) == "FISCAL_YR"] = "fiscalyear"
names(open_data_inspecs)[names(open_data_inspecs) == "FISCAL_QTR"] = "fiscalquarter"
names(open_data_inspecs)[names(open_data_inspecs) == "INSPECT_OFFICE_CD"] = "inspectingofficecode"
names(open_data_inspecs)[names(open_data_inspecs) == "ACTIVITY_CODE"] = "inspactycode"
names(open_data_inspecs)[names(open_data_inspecs) == "ACTIVITY"] = "inspacty"
names(open_data_inspecs)[names(open_data_inspecs) == "ACTIVE_SECTIONS"] = "activesectionsinspected"
names(open_data_inspecs)[names(open_data_inspecs) == "IDLE_SECTIONS"] = "idlesectionsinspected"
names(open_data_inspecs)[names(open_data_inspecs) == "SHAFT_SLOPE_SINK"] = "shaftslopesinkingconstinspected"
names(open_data_inspecs)[names(open_data_inspecs) == "IMPOUND_CONSTR"] = "impoundconstinspected"
names(open_data_inspecs)[names(open_data_inspecs) == "BLDG_CONSTR_SITES"] = "buildingconstinspected"
names(open_data_inspecs)[names(open_data_inspecs) == "DRAGLINES"] = "draglineconstinspected"
names(open_data_inspecs)[names(open_data_inspecs) == "UNCLASSIFIED_CONSTR"] = "otherconstsitesinspected"
names(open_data_inspecs)[names(open_data_inspecs) == "CO_RECORDS"] = "companyrecords"
names(open_data_inspecs)[names(open_data_inspecs) == "SURF_UG_MINE"] = "surfaceareasugmines"
names(open_data_inspecs)[names(open_data_inspecs) == "SURF_FACILITY_MINE"] = "surfaceworkings"
names(open_data_inspecs)[names(open_data_inspecs) == "REFUSE_PILES"] = "refusepiles"
names(open_data_inspecs)[names(open_data_inspecs) == "EXPLOSIVE_STORAGE"] = "explosivestorage"
names(open_data_inspecs)[names(open_data_inspecs) == "OUTBY_AREAS"] = "outbyareas"
names(open_data_inspecs)[names(open_data_inspecs) == "MAJOR_CONSTR"] = "majorconstruction"
names(open_data_inspecs)[names(open_data_inspecs) == "SHAFTS_SLOPES"] = "shafts"
names(open_data_inspecs)[names(open_data_inspecs) == "MISC_AREA"] = "miscellaneous"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.SAMPLE_CNT_AIR."] = "airsamples"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.SAMPLE_CNT_DUSTSPOT."] = "spotdustsamples"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.SAMPLE_CNT_DUSTSURVEY."] = "surveydustsamples"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.SAMPLE_CNT_RESPDUST."] = "respdustsamples"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.SAMPLE_CNT_NOISE."] = "noisesamples"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.SAMPLE_CNT_OTHER."] = "othersamples"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.TOTAL_INSP_HOURS."] = "sumtotal_insp_hours"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.TOTAL_ON_SITE_HOURS."] = "sumtotal_on_site_hours"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.TOTAL_INSP_HRS_SPVR_TRAINEE."] = "sumtotal_insp_hrs_spvr_trainee"
names(open_data_inspecs)[names(open_data_inspecs) == "SUM.TOTAL_ON_SITE_HRS_SPVR_TRAINEE."] = "sumtotal_on_site_hrs_spvr_traine"
names(open_data_inspecs)[names(open_data_inspecs) == "CONTROLLER_ID"] = "controllerid"
names(open_data_inspecs)[names(open_data_inspecs) == "OPERATOR_ID"] = "operatorid"
names(open_data_inspecs)[names(open_data_inspecs) == "CONTROLLER_NAME"] = "controllername"
names(open_data_inspecs)[names(open_data_inspecs) == "OPERATOR_NAME"] = "operatorname"
names(open_data_inspecs) = tolower(names(open_data_inspecs))

# format variables
open_data_inspecs$eventno = as.character(open_data_inspecs$eventno)
open_data_inspecs$beginningdate = as.character(open_data_inspecs$beginningdate)
open_data_inspecs$endingdate = as.character(open_data_inspecs$endingdate)
open_data_inspecs$mineid = str_pad(open_data_inspecs$mineid, 7, pad = "0")
open_data_inspecs$eventno = str_pad(open_data_inspecs$eventno, 7, pad = "0")

# one obs is not unique on eventno - remove these two rows (with the same eventno)
#open_data_inspecs[, "dup"] = duplicated(open_data_inspecs$eventno)
open_data_inspecs = open_data_inspecs[open_data_inspecs$eventno != 4165469,]
open_data_inspecs = open_data_inspecs[,-grep("dup", names(open_data_inspecs))]

# memory
clean_inspecs = open_data_inspecs
rm(open_data_inspecs)

######################################################################################################

# READ CLEAN MINE TYPE DATA, MERGE WITH INSPECTIONS DATA, THEN CLEAN RESULTING DATA

# read clean mine type data (1_clean_mines) - 86,362 obs, 3 vars
mine_types = readRDS(mine_types_file_name)

# merge open source inspections data with mine type data - 403,701 obs, 47 vars
clean_inspecs = merge(clean_inspecs, mine_types, by = c("mineid"), all = T)

# drop problematic merge observations
clean_inspecs = clean_inspecs[!is.na(clean_inspecs$eventno), ] # back to 327775 obs

# drop observations from environments not of interest
  # facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16
clean_inspecs = clean_inspecs[clean_inspecs$minetype == "Underground", ] # now we have 195,719 obs

######################################################################################################

clean_inspecs$too_new = ifelse(clean_inspecs$calendaryear == 2016 & clean_inspecs$calendarquarter > 1, 1, 0)
clean_inspecs = clean_inspecs[clean_inspecs$too_new == 0,] # wind up with 192,639 obs, 1930 mines
clean_inspecs = clean_inspecs[, c(-match("too_new", names(clean_inspecs)))] # now we have 47 vars 

# output merged and clean inspections data - 192,639 obs, 1930 mines, 47 vars
saveRDS(clean_inspecs, file = open_data_inspecs_out_file_name)

######################################################################################################
