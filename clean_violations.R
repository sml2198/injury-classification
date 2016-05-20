##HEADER##

#Coded by Nikhil Saifullah

#Load in CSV from STATA conversion
early_viols = read.csv("X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/violations_fromText.csv")
#This works
open_data_viols = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Violations.txt", header = T, sep = "|")

#Renaming for MSHA Open Data
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

#Flag and drop duplicates on violation number
open_data_viols[, "dup"] = duplicated(open_data_viols$violationno)
table(open_data_viols$dup) #Same duplicates as in the STATA version of the code so no testing here.
#FALSE    TRUE 
#2161636      51 
open_data_viols = open_data_viols[open_data_viols$dup == F,]

open_data_viols[, "violationno"] = as.character(open_data_viols[, "violationno"])
open_data_viols[, "eventno"] = as.character(open_data_viols[, "eventno"])

#Keep only observations from Carolyn Stasik's data in years prior to the Open Data's time range
early_viols = early_viols[early_viols$calendaryear <= 1999 & early_viols$calendaryear >= 1983,]

clean_violations = merge(early_viols, open_data_viols, by = "violationno", all = T)
clean_violations[, "merge"] = ifelse(!is.na(clean_violations$eventno.y) & !is.na(clean_violations$eventno.x), 3, 0)
clean_violations[, "merge"] = ifelse(is.na(clean_violations$eventno.x) & !is.na(clean_violations$eventno.y), 2, clean_violations[, "merge"])
clean_violations[, "merge"] = ifelse(is.na(clean_violations$eventno.y) & !is.na(clean_violations$eventno.x), 1, clean_violations[, "merge"])
table(clean_violations$merge) #1 observation in STATA's open data as compared with open_data_assessments. Nikhil 5/13/16
#1       2       3 
#2276578 2161593      43

#For the 43 pre-2000 observations in both datasets, use the open data
common_varstbs = sub(".x", "", names(clean_violations)[grep(".x", names(clean_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  mstr_copy = paste(common_varstbs[i], ".x", sep = "")
  usng_copy = paste(common_varstbs[i], ".y", sep = "")
  clean_violations[, mstr_copy] = ifelse(clean_violations[, "merge"] == 3, clean_violations[, usng_copy], clean_violations[, mstr_copy])
  clean_violations[, mstr_copy] = ifelse(clean_violations[, "merge"] == 2, clean_violations[, usng_copy], clean_violations[, mstr_copy])
}
clean_violations = clean_violations[, -grep(".y", names(clean_violations), fixed = T)]
names(clean_violations)[grep(".x", names(clean_violations), fixed = T)] = common_varstbs
clean_violations = clean_violations[, -grep("merge", names(clean_violations))]

saveRDS(clean_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds")
