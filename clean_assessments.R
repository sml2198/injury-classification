##HEADER##

#Coded by Nikhil Saifullah

early_assessments = read.csv("X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/assessments_fromText.csv")
open_data_assessments = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/AssessedViolations.txt", header = T, sep = "|")

early_assessments[, "dup"] = duplicated(early_assessments) #Checks out with total number of duplicates implied by STATA's "duplicates" command
early_assessments = early_assessments[early_assessments$dup == F,]

names(early_assessments)[names(early_assessments) == "Ã.Ã.violationno"] = "violationno"

names(open_data_assessments)[names(open_data_assessments) == "VIOLATION_NO"] = "violationno"
names(open_data_assessments)[names(open_data_assessments) == "VIOLATION_ID"] = "violationid"
names(open_data_assessments)[names(open_data_assessments) == "EVENT_NO"] = "eventno"
names(open_data_inspecs)[names(open_data_inspecs) == "MINE_ID"] = "mineid"
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
open_data_assessments[, "violationno"] = as.character(open_data_assessments[, "violationno"])

clean_assessments = merge(open_data_assessments, early_assessments, by = "violationno", all = T)
clean_assessments[, "merge"] = ifelse(!is.na(clean_assessments$issuedate.y) & !is.na(clean_assessments$issuedate.x), 3, 0)
clean_assessments[, "merge"] = ifelse(is.na(clean_assessments$issuedate.x) & !is.na(clean_assessments$issuedate.y), 2, clean_assessments[, "merge"])
clean_assessments[, "merge"] = ifelse(is.na(clean_assessments$issuedate.y) & !is.na(clean_assessments$issuedate.x), 1, clean_assessments[, "merge"])
table(clean_assessments$merge) #Not matching up with STATA. 1,432 more observations match that in STATA were master only and 1 using only. 32 more using only

clean_assessments = clean_assessments[, -grep("merge", names(clean_assessments))]

save(clean_assessments, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.RData")
