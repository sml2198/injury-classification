# NIOSH Project 2014-N-15776

# 8 - Clean Inspections
    # This file cleans the inspections data we downloaded from MSHA's open data portal. The portions commented out 
    # are to load and clean data from Carolyn Stasik's (MSHA) data pull from May 20th, 2015. 

# Last edit 7/19/16

######################################################################################################

# define file names
  # input: raw inspections data from MSHA open data platform (Inspections): http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
  # downloaded on 4/20/2016 @ 4:35 PM
open_data_inspecs.in.file.name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Inspections.txt"
  # output: cleaned inspections data, uniquely identified by mineid *eventno
open_data_inspecs.out.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_Inspections.rds"
# input: converted inspection hours file from MSHA data download on 5/15/2015 @ 4:17 PM
early_inspecs_hours.file.name = "X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/inspection_hours_fromText.csv"
  # input: cleaned mine-types key produced in produced in 1_clean_mines.R
mine.types.file.name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"

######################################################################################################

# Read data files
open_data_inspecs = read.table(open_data_inspecs.in.file.name, header = T, sep = "|")
early_inspecs_hours.file.name = read.csv(early_inspecs_hours.file.name)
mine_types = readRDS(mine.types.file.name)

# Rename variables (we did this originally so var names would be consistent with our existing data pull - this is mostly cosmetic)
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

# Make all varnames lowercase because Sarah hates capitalization
names(open_data_inspecs) = tolower(names(open_data_inspecs))

# A few more renames
names(early_inspecs_hours)[names(early_inspecs_hours) == "sumtotalminingareatime"] = "sumtotal_on_site_hours"
names(early_inspecs_hours)[names(early_inspecs_hours) == "sumcalctotalinspectionrelatedtim"] = "sumtotal_insp_hours"

# Format eventno as a 7 digit character var padded with zeroes
open_data_inspecs[, "eventno"] = as.character(open_data_inspecs[, "eventno"])
early_inspecs_hours[, "eventno"] = as.character(early_inspecs_hours[, "eventno"])
open_data_inspecs[, "eventno"] = ifelse(nchar(open_data_inspecs[, "eventno"], type = "chars", allowNA = F, keepNA = T) < 7, paste("0", open_data_inspecs[, "eventno"], sep = ""), open_data_inspecs[, "eventno"])

# Format inspection hours
early_inspecs_hours[, "sumtotal_insp_hours"] = as.numeric(gsub(",", "", as.character(early_inspecs_hours$sumtotal_insp_hours), fixed = T))
early_inspecs_hours = aggregate(cbind(sumtotal_on_site_hours, sumtotal_insp_hours) ~ eventno + mineid + coalcormetalmmine, FUN = "sum", data = early_inspecs_hours)

# Format date vars 
open_data_inspecs$beginningdate = as.character(open_data_inspecs$beginningdate)
open_data_inspecs$endingdate = as.character(open_data_inspecs$endingdate)

clean_inspecs = open_data_inspecs
rm(open_data_inspecs)

######################################################################################################

# Format mineid and eventno as 7 digit numbers padded with zeroes
clean_inspecs$mineid = str_pad(clean_inspecs$mineid, 7, pad = "0")
clean_inspecs$mineid = withr::with_options(c(scipen = 999), str_pad(clean_inspecs$mineid, 7, pad = "0"))
clean_inspecs$eventno = str_pad(clean_inspecs$eventno, 7, pad = "0")
clean_inspecs$eventno = withr::with_options(c(scipen = 999), str_pad(clean_inspecs$eventno, 7, pad = "0"))

# Merge on minetypes to drop non-coal and non-underground observations before saving
clean_inspecs = clean_inspecs[clean_inspecs$coal_metal_ind != "M",]
clean_inspecs = merge(clean_inspecs, mine_types, by = c("mineid"), all = T)
clean_inspecs = clean_inspecs[!is.na(clean_inspecs$eventno),]

######################################################################################################

# THIS CODE IS RETIRED.
#clean_inspecs = merge(open_data_inspecs, early_inspecs, by = "eventno", all = T)
#clean_inspecs[, "merge"] = ifelse(!is.na(clean_inspecs$beginningdate.y) & !is.na(clean_inspecs$beginningdate.x), 3, 0)
#clean_inspecs[, "merge"] = ifelse(is.na(clean_inspecs$beginningdate.x) & !is.na(clean_inspecs$beginningdate.y), 2, clean_inspecs[, "merge"])
#clean_inspecs[, "merge"] = ifelse(is.na(clean_inspecs$beginningdate.y) & !is.na(clean_inspecs$beginningdate.x), 1, clean_inspecs[, "merge"])
#table(clean_inspecs$merge)
#1       2 
#775245 1415166

#common_varstbs = sub(".x", "", names(clean_inspecs)[grep(".x", names(clean_inspecs), fixed = T)], fixed = T)
#for (i in 1:length(common_varstbs)) {
#  clean_inspecs[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(clean_inspecs[, "merge"] == 2, clean_inspecs[, paste(common_varstbs[i], ".y", sep = "")], clean_inspecs[, paste(common_varstbs[i], ".x", sep = "")])
#}
#clean_inspecs = clean_inspecs[, -grep(".y", names(clean_inspecs), fixed = T)]
#names(clean_inspecs)[grep(".x", names(clean_inspecs), fixed = T)] = common_varstbs

######################################################################################################

# Format mineid and eventno from the hours data, clean up other hours vars
early_inspecs_hours$mineid = str_pad(early_inspecs_hours$mineid, 7, pad = "0")
early_inspecs_hours$mineid = withr::with_options(c(scipen = 999), str_pad(early_inspecs_hours$mineid, 7, pad = "0"))
early_inspecs_hours$eventno = str_pad(early_inspecs_hours$eventno, 7, pad = "0")
early_inspecs_hours$eventno = withr::with_options(c(scipen = 999), str_pad(early_inspecs_hours$eventno, 7, pad = "0"))
early_inspecs_hours = early_inspecs_hours[early_inspecs_hours$coalcormetalmmine == "C",]

# Create vars to check the merge and merge on hours data
clean_inspecs$mergecheck.inspec = 1
early_inspecs_hours$mergecheck.hrs = 1
clean_inspecs = merge(clean_inspecs, early_inspecs_hours, by = c("mineid", "eventno"), all = T)
rm(early_inspecs_hours)

# Remove problematic merge observations, drop all non-underground observations
clean_inspecs = clean_inspecs[!is.na(clean_inspecs$calendaryear) & !is.na(clean_inspecs$calendarquarter) & !is.na(clean_inspecs$program_area),]
# (facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16)
clean_inspecs = clean_inspecs[clean_inspecs$minetype == "Underground",]
clean_inspecs = clean_inspecs[!is.na(clean_inspecs$mergecheck.hrs) & !is.na(clean_inspecs$mergecheck.inspec),]

######################################################################################################

# THIS CODE IS RETIRED.
# code from before we merged on mineid AND eventno
#clean_inspecs[, "mergehrs"] = ifelse(!is.na(clean_inspecs$mineid.y) & !is.na(clean_inspecs$mineid.x), 3, 0)
#clean_inspecs[, "mergehrs"] = ifelse(is.na(clean_inspecs$mineid.x) & !is.na(clean_inspecs$mineid.y), 2, clean_inspecs[, "mergehrs"])
#clean_inspecs[, "mergehrs"] = ifelse(is.na(clean_inspecs$mineid.y) & !is.na(clean_inspecs$mineid.x), 1, clean_inspecs[, "mergehrs"])
#table(clean_inspecs$mergehrs)
#1       2       3 
#198270   71243 1992141
#Only Open Data:
#1       2       3 
#103014 1391153  672231 

#common_varstbs = sub(".x", "", names(clean_inspecs)[grep(".x", names(clean_inspecs), fixed = T)], fixed = T)
#for (i in 1:length(common_varstbs)) {
#  mstr_copy = paste(common_varstbs[i], ".x", sep = "")
#  usng_copy = paste(common_varstbs[i], ".y", sep = "")
#  clean_inspecs[, mstr_copy] = ifelse(clean_inspecs[, "mergehrs"] == 3 & is.na(clean_inspecs[, mstr_copy]), clean_inspecs[, usng_copy], clean_inspecs[, mstr_copy])
#  clean_inspecs[, mstr_copy] = ifelse(clean_inspecs[, "mergehrs"] == 2, clean_inspecs[, usng_copy], clean_inspecs[, mstr_copy])
#}

# insp_conflcts = sum(clean_inspecs[, "mergehrs"] == 3 & !is.na(clean_inspecs[, "sumtotal_insp_hours.x"]) & (clean_inspecs[, "sumtotal_insp_hours.x"] != clean_inspecs[, "sumtotal_insp_hours.y"]))
# on_site_conflcts = sum(clean_inspecs[, "mergehrs"] == 3 & !is.na(clean_inspecs[, "sumtotal_on_site_hours.x"]) & (clean_inspecs[, "sumtotal_on_site_hours.x"] != clean_inspecs[, "sumtotal_on_site_hours.y"]))
# nonNA_conflicts = max(insp_conflcts, on_site_conflcts)
# clean_inspecs = clean_inspecs[, -grep(".y", names(clean_inspecs), fixed = T)]
# names(clean_inspecs)[grep(".x", names(clean_inspecs), fixed = T)] = common_varstbs

######################################################################################################

# Check for conflicts in the inspections merge
insp_conflcts = sum(!is.na(clean_inspecs[, "sumtotal_insp_hours.x"]) & (clean_inspecs[, "sumtotal_insp_hours.x"] != clean_inspecs[, "sumtotal_insp_hours.y"])) #There are 6 of these
on_site_conflcts = sum(!is.na(clean_inspecs[, "sumtotal_on_site_hours.x"]) & (clean_inspecs[, "sumtotal_on_site_hours.x"] != clean_inspecs[, "sumtotal_on_site_hours.y"])) #There are 2 of these
nonNA_conflicts = max(insp_conflcts, on_site_conflcts)

# Rename duplicate vars from the merge
clean_inspecs = clean_inspecs[, -grep(".y", names(clean_inspecs), fixed = T)]
names(clean_inspecs)[grep(".x", names(clean_inspecs), fixed = T)] = common_varstbs

clean_inspecs = clean_inspecs[!(((!is.na(clean_inspecs$controllerid) & clean_inspecs$controllerid == "C11088") | is.na(clean_inspecs$controllerid)) & clean_inspecs$eventno == "4165469"),c(-grep("coal_metal_ind", names(clean_inspecs)), -grep("merge", names(clean_inspecs)))]

saveRDS(clean_inspecs, file = open_data_inspecs.out.file.name)

######################################################################################################

