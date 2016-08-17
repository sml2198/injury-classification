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
  # input: inspection hours data
early_inspecs_hours_file_name = "X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/inspection_hours_fromText.csv"
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

# HONESTLY I HAVE NO IDEA WHY WE EVEN BROUGHT IN THIS INSPECTION HOURS DATA. WE HAVE NO MISSING INSPECTION
# HOURS VARS IN THIS DATA SET FROM THE OPEN DATA PORTAL. I'M GOING TO TAKE OUT ALL OF THIS NONSENSE NOW.

# READ AND CLEAN INSPECTION HOURS DATA 

# read inspection hours data - 4634874 obs, 10 vars
# early_inspecs_hours = read.csv(early_inspecs_hours_file_name)
# 
# # drop observations from envrionments not of interest
# early_inspecs_hours = early_inspecs_hours[early_inspecs_hours$coalcormetalmmine == "C", ] # drop 1744657 obs
# 
# # rename variables
# names(early_inspecs_hours)[names(early_inspecs_hours) == "sumtotalminingareatime"] = "sumtotal_on_site_hours"
# names(early_inspecs_hours)[names(early_inspecs_hours) == "sumcalctotalinspectionrelatedtim"] = "sumtotal_insp_hours"
# 
# # format variables
# early_inspecs_hours$eventno = as.character(early_inspecs_hours$eventno)
# early_inspecs_hours[, "sumtotal_insp_hours"] = as.numeric(gsub(",", "", as.character(early_inspecs_hours$sumtotal_insp_hours), fixed = T))
# early_inspecs_hours = aggregate(cbind(sumtotal_on_site_hours, sumtotal_insp_hours) ~ eventno + mineid + coalcormetalmmine, 
#                                 FUN = "sum", data = early_inspecs_hours)
# early_inspecs_hours$mineid = str_pad(early_inspecs_hours$mineid, 7, pad = "0")
# early_inspecs_hours$eventno = str_pad(early_inspecs_hours$eventno, 7, pad = "0")

# MERGE INSPECTION HOURS DATA WITH INSPECTIONS DATA, THEN OUTPUT

# # create variables to check the merge results
# clean_inspecs$mergecheck.inspec = 1
# early_inspecs_hours$mergecheck.hrs = 1
# 
# # merge inspections data with open source inspection hours data - 1087895 obs, 52 vars
# clean_inspecs = merge(clean_inspecs, early_inspecs_hours, by = c("mineid", "eventno"), all = T)
# 
# # check for conflicts in the merge
# insp_conflcts = sum(!is.na(clean_inspecs[, "sumtotal_insp_hours.x"]) & (clean_inspecs[, "sumtotal_insp_hours.x"] != clean_inspecs[, "sumtotal_insp_hours.y"])) # 6 observations
# on_site_conflcts = sum(!is.na(clean_inspecs[, "sumtotal_on_site_hours.x"]) & (clean_inspecs[, "sumtotal_on_site_hours.x"] != clean_inspecs[, "sumtotal_on_site_hours.y"])) # 2 observations
# nonNA_conflicts = max(insp_conflcts, on_site_conflcts)
# 
# clean_inspecs = clean_inspecs[complete.cases(clean_inspecs$calendaryear),] # 194574 obs, 1934 mines, 52 vars
# #clean_inspecs = clean_inspecs[(!is.na(clean_inspecs$mergecheck.hrs) & !is.na(clean_inspecs$mergecheck.inspec)), ] # drop 916126 obs
# 
# # rename duplicate variables
# clean_inspecs = clean_inspecs[, -grep(".y", names(clean_inspecs), fixed = T)]
# names(clean_inspecs)[c(grep(".x", names(clean_inspecs), fixed = TRUE))] = sub(".x", "", names(clean_inspecs)[c(grep(".x", names(clean_inspecs), fixed = TRUE))])
# clean_inspecs = clean_inspecs[!(((!is.na(clean_inspecs$controllerid) & clean_inspecs$controllerid == "C11088") | is.na(clean_inspecs$controllerid)) & clean_inspecs$eventno == "4165469"),
#                               c(-grep("coal_metal_ind", names(clean_inspecs)), -grep("merge", names(clean_inspecs)))]

######################################################################################################

clean_inspecs$too_new = ifelse(clean_inspecs$calendaryear == 2016 & clean_inspecs$calendarquarter > 1, 1, 0)
clean_inspecs = clean_inspecs[clean_inspecs$too_new == 0,] # wind up with 192,639 obs, 1930 mines
clean_inspecs = clean_inspecs[, c(-match("too_new", names(clean_inspecs)))] # now we have 47 vars 

# output merged and clean inspections data - 192,639 obs, 1930 mines, 47 vars
saveRDS(clean_inspecs, file = open_data_inspecs_out_file_name)

######################################################################################################
