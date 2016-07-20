# NIOSH Project 2014-N-15776

# 7 - Clean Assessments
    # This file cleans the assessments data we downloaded from MSHA's open data portal. The portions 
    # commented out are to load and clean data from Carolyn Stasik's (MSHA) data pull from March 3rd, 2015.

# Last edit 7/19/16

######################################################################################################

# define file names
  # input: raw assessments data from MSHA open data platform (Assessed Violations): http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
  # downloaded on 4/21/2016 @ 9:37 AM
open_data_assessments = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/AssessedViolations.txt", header = T, sep = "|")
  # input: cleaned mine-types key produced in produced in 1_clean_mines.R
mine_types = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds")

######################################################################################################

# THIS CODE IS RETIRED.
#early_assessments = read.csv("X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/assessments_fromText.csv")
#early_assessments[, "dup"] = duplicated(early_assessments) #Checks out with total number of duplicates implied by STATA's "duplicates" command
#table(early_assessments$dup)
#FALSE    TRUE 
#2639669    3238
#early_assessments = early_assessments[early_assessments$dup == F,]
#names(early_assessments)[names(early_assessments) == "Ã.Ã.violationno"] = "violationno"

######################################################################################################

# Rename variables (we did this originally so var names would be consistent with our existing data pull - this is mostly cosmetic)
names(open_data_assessments)[names(open_data_assessments) == "VIOLATION_NO"] = "violationno"
names(open_data_assessments)[names(open_data_assessments) == "VIOLATION_ID"] = "violationid"
names(open_data_assessments)[names(open_data_assessments) == "VIOLATOR_NAME"] = "violatorname"
names(open_data_assessments)[names(open_data_assessments) == "EVENT_NO"] = "eventno"
names(open_data_assessments)[names(open_data_assessments) == "MINE_ID"] = "mineid"
names(open_data_assessments)[names(open_data_assessments) == "COAL_METAL_IND"] = "coalcormetalm"
names(open_data_assessments)[names(open_data_assessments) == "VIOLATOR_TYPE_CD"] = "violatortypecode"
names(open_data_assessments)[names(open_data_assessments) == "OFFICE_CD"] = "officecode"
names(open_data_assessments)[names(open_data_assessments) == "ASSESS_CASE_NO"] = "assesscaseno"
names(open_data_assessments)[names(open_data_assessments) == "PRIMARY_ACTION_CD"] = "primaryactioncode"
names(open_data_assessments)[names(open_data_assessments) == "SIG_SUB_IND"] = "sigandsubindicator"
names(open_data_assessments)[names(open_data_assessments) == "MINE_ACT_SECTION_CD"] = "mineactsectioncode"
names(open_data_assessments)[names(open_data_assessments) == "CFR_STANDARD_CD"] = "cfrstandardcode"
names(open_data_assessments)[names(open_data_assessments) == "CITATION_TYPE_CD"] = "violationtypecode"
names(open_data_assessments)[names(open_data_assessments) == "ASSESS_TYPE_CD"] = "assessmenttypecode"
names(open_data_assessments)[names(open_data_assessments) == "ASSESS_CASE_STATUS_CD"] = "assesscasestatuscode"
names(open_data_assessments)[names(open_data_assessments) == "ASSESS_CASE_STATUS_DT"] = "assesscasestatusdate"
names(open_data_assessments)[names(open_data_assessments) == "OCCURRENCE_DT"] = "occurrencedate"
names(open_data_assessments)[names(open_data_assessments) == "ISSUE_DT"] = "issuedate"
names(open_data_assessments)[names(open_data_assessments) == "FINAL_ORDER_DT"] = "finalorderdate"
names(open_data_assessments)[names(open_data_assessments) == "BILL_PRINT_DT"] = "billprintdate"
names(open_data_assessments)[names(open_data_assessments) == "PROPOSED_PENALTY_AMT"] = "proposedpenaltyamount"
names(open_data_assessments)[names(open_data_assessments) == "CURRENT_ASSESSMENT_AMT"] = "currentassessmentamount"
names(open_data_assessments)[names(open_data_assessments) == "PENALTY_POINTS"] = "penaltypoints"
names(open_data_assessments)[names(open_data_assessments) == "GRAVITY_PERSONS_POINTS"] = "gravitypersonspoints"
names(open_data_assessments)[names(open_data_assessments) == "GRAVITY_INJURY_POINTS"] = "gravityinjurypoints"
names(open_data_assessments)[names(open_data_assessments) == "GRAVITY_LIKELIHOOD_POINTS"] = "gravitylikelihoodpoints"
names(open_data_assessments)[names(open_data_assessments) == "NEGLIGENCE_POINTS"] = "negligencepoints"
names(open_data_assessments)[names(open_data_assessments) == "CONTRACTOR_SIZE_POINTS"] = "contractorsizepoints"
names(open_data_assessments)[names(open_data_assessments) == "GOOD_FAITH_POINTS"] = "goodfaithpoints"
names(open_data_assessments)[names(open_data_assessments) == "SIZE_OF_MINE"] = "minesize"
names(open_data_assessments)[names(open_data_assessments) == "MINE_SIZE_POINTS"] = "minesizepoints"
names(open_data_assessments)[names(open_data_assessments) == "CONTROLLER_SIZE_POINTS"] = "controllersizepoints"

# Make all varnames lowercase because Sarah hates capitalization
names(open_data_assessments) = tolower(names(open_data_assessments))

# Drop non-coal observations
open_data_assessments = open_data_assessments[open_data_assessments$coalcormetalm == "C",]
open_data_assessments = open_data_assessments[!is.na(open_data_assessments$coalcormetalm),]

# Format mineid, eventno, and violationno as 7 digit stringvars padded with zeroes
open_data_assessments[, "violationno"] = as.character(open_data_assessments[, "violationno"])
open_data_assessments$mineid = str_pad(open_data_assessments$mineid, 7, pad = "0")
open_data_assessments$mineid = withr::with_options(c(scipen = 999), str_pad(open_data_assessments$mineid, 7, pad = "0"))
open_data_assessments$eventno = str_pad(open_data_assessments$eventno, 7, pad = "0")
open_data_assessments$eventno = withr::with_options(c(scipen = 999), str_pad(open_data_assessments$eventno, 7, pad = "0"))
open_data_assessments$violationno = str_pad(open_data_assessments$violationno, 7, pad = "0")
open_data_assessments$violationno = withr::with_options(c(scipen = 999), str_pad(open_data_assessments$violationno, 7, pad = "0"))

# Only keep observations from environment we care about
# Merge on minetypes to drop non-coal and non-underground observations before saving
open_data_assessments = merge(open_data_assessments, mine_types, by = c("mineid"), all = T)
open_data_assessments = open_data_assessments[!is.na(open_data_assessments$eventno),]
# (Facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16)
open_data_assessments = open_data_assessments[open_data_assessments$minetype == "Underground",]
open_data_assessments = open_data_assessments[, c(-match("coalcormetalmmine", names(open_data_assessments)))]

clean_assessments = open_data_assessments
rm(open_data_assessments)

saveRDS(clean_assessments, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds")

######################################################################################################

# THIS CODE IS RETIRED.
#early_assessments$src = "early"
#open_data_assessments$src = "open_data"

#clean_assessments = merge(open_data_assessments, early_assessments, by = "violationno", all = T)
#clean_assessments[, "merge"] = ifelse(!is.na(clean_assessments$issuedate.y) & !is.na(clean_assessments$issuedate.x), 3, 0)
#clean_assessments[, "merge"] = ifelse(is.na(clean_assessments$issuedate.x) & !is.na(clean_assessments$issuedate.y), 2, clean_assessments[, "merge"])
#clean_assessments[, "merge"] = ifelse(is.na(clean_assessments$issuedate.y) & !is.na(clean_assessments$issuedate.x), 1, clean_assessments[, "merge"])
#table(clean_assessments$merge) #1 observation in STATA's open data as compared with open_data_assessments. Nikhil 5/13/16
#1       2       3 
#237358  769535 1870134 

#Next two lines to ensure R doesn't automatically convert occurrencedate to an integer (a likely quirk with factor variables)
#clean_assessments$occurrencedate.x = as.character(clean_assessments$occurrencedate.x)
#clean_assessments$occurrencedate.y = as.character(clean_assessments$occurrencedate.y)
#common_varstbs = sub(".x", "", names(clean_assessments)[grep(".x", names(clean_assessments), fixed = T)], fixed = T)
#for (i in 1:length(common_varstbs)) {
#  clean_assessments[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(clean_assessments[, "merge"] == 2, clean_assessments[, paste(common_varstbs[i], ".y", sep = "")], clean_assessments[, paste(common_varstbs[i], ".x", sep = "")])
#}
#clean_assessments = clean_assessments[, -grep(".y", names(clean_assessments), fixed = T)]
#names(clean_assessments)[grep(".x", names(clean_assessments), fixed = T)] = common_varstbs
#clean_assessments = clean_assessments[, -grep("merge", names(clean_assessments))]

######################################################################################################
