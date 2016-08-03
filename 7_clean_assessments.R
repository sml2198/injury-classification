# NIOSH Project 2014-N-15776

# 7 - Clean Assessments
  # Reads and cleans assessments data
  # Reads clean mine types data (1_clean_mines)
  # Merges assessment data and mine types data to drop environments not of interest
  # Outputs clean assessments data

# Last edit 8/3/16

######################################################################################################

library(stringr)

# define file names
  # input: assessments data
open_data_assessments_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/AssessedViolations.txt"
  # input: mine-type data (1_clean_mines)
mine_types_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds"
  # output: clean assessments data
    # uniquely identified by eventno (inspection number) * violationno (violation number)
open_data_assessments_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds"

######################################################################################################

# READ, CLEAN, AND MERGE ASSESSMENTS DATA AND MINE TYPE DATA, THEN OUTPUT

# read assessments data - 2107492 obs, 58 vars
  # dataset downloaded on 4/21/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp [MSHA open data platform (Assessed Violations)]
open_data_assessments = read.table(open_data_assessments_in_file_name, header = T, sep = "|")

# drop data from environments not of interest
open_data_assessments = open_data_assessments[open_data_assessments$COAL_METAL_IND == "C", ] # drop 959176 obs

# rename variables
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
names(open_data_assessments) = tolower(names(open_data_assessments))

# format variables
open_data_assessments$violationno = as.character(open_data_assessments$violationno)
open_data_assessments$mineid = str_pad(open_data_assessments$mineid, 7, pad = "0")
open_data_assessments$eventno = str_pad(open_data_assessments$eventno, 7, pad = "0")
open_data_assessments$violationno = str_pad(open_data_assessments$violationno, 7, pad = "0")

# read mine types data (1_clean_mines) - 86135 obs, 3 vars
mine_types = readRDS(mine_types_file_name)

# merge assessments with mine types - 1229627 obs, 60 vars
open_data_assessments = merge(open_data_assessments, mine_types, by = c("mineid"), all = T)
open_data_assessments = open_data_assessments[!is.na(open_data_assessments$eventno), ] # drop 81311 obs

# drop data from environments not of interest
  # facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16
open_data_assessments = open_data_assessments[open_data_assessments$minetype == "Underground", ] # drop 306643 obs

# drop unnecessary variables
open_data_assessments$coalcormetalmmine = NULL

# output clean assessments data - 841673 obs, 59 vars, 1673 unique mines
saveRDS(open_data_assessments, file = open_data_assessments_out_file_name)

######################################################################################################
