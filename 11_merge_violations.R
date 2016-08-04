# NIOSH Project 2014-N-15776

# 11 - Merge Violations
    # Load violations, assessments, and inspections from 6_clean_violations.R, 7_clean_assessments.R, 8_clean_inspections.R, 9_clean_employment.R
    # Merge violations and assessments 
    # Clean up the merged violations
    # Merge violations and inspections
    # Load and merge in contractor data
    # Clean merged dataset 
    # Load and merge in cfr-key data
    # Clean and output

# Last edit 8/3/16

######################################################################################################

library(stringr)

# define file names
  # input: clean violations data (6_clean_violations)
clean_violations_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds"
  # input: clean assessments data (7_clean_assessments.R
clean_assessments_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds" 
  # output: merged violations and assessments data 
clean_violations_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations_TEST.rds"



  # input: clean inspections data (8_clean_inspections)
clean_inspections_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_inspections.rds"
  # input: cfr key data (10_clean_cfr_key)
clean_cfr_key_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_cfr_key.rds"
  # input: quarterly contractor employment, produced in 9_clean_employment.R
contractor_quarterly_employment_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds"
  # input: annual contractor employment, produced in 9_clean_employment.R
contractor_yearly_employment_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds"

######################################################################################################

# READ AND MERGE ASSESSMENTS DATA AND VIOLATIONS DATA, THEN CLEAN

# read assessments data - 841673 obs, 59 vars
clean_assessments = readRDS(clean_assessments_file_name) 

# read violations data - 866357 obs, 62 vars
clean_violations = readRDS(clean_violations_in_file_name)

# merge assessments data with violations data - 866390 obs, 119 vars
assessments_violations = merge(clean_assessments, clean_violations, by = c("mineid","violationno"), all = T)

# memory
rm(clean_violations, clean_assessments)

# fill in eventno where missing
assessments_violations$eventno.x = ifelse((is.na(assessments_violations$eventno.x) & !is.na(assessments_violations$eventno.y)), 
                                          assessments_violations$eventno.y, assessments_violations$eventno.x)

# drop duplicate variables from merge
common_varstbs = sub(".x", "", names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)], fixed = T)
assessments_violations = assessments_violations[, -grep(".y", names(assessments_violations), fixed = T)]
names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)] = common_varstbs

# drop unnecessary variables
assessments_violations$issuedate =
  assessments_violations$final_order_issue_dt =
  assessments_violations$last_action_dt =
  assessments_violations$datevacated =
  assessments_violations$righttoconferencedate =
  assessments_violations$bill_print_dt =
  assessments_violations$number =
  assessments_violations$violdup = NULL

# format/rename variables
assessments_violations[, "violatorname"] = tolower(str_trim(assessments_violations[, "violatorname"], side = c("both")))
assessments_violations[, "violator_name"] = tolower(str_trim(assessments_violations[, "violator_name"], side = c("both")))
names(assessments_violations)[names(assessments_violations) == "bill_print_fiscal_yr"] = "billprintfiscalyr"
names(assessments_violations)[names(assessments_violations) == "good_faith_ind"] = "goodfaithind"

# clean violator name field
string_fun_temp = function(var) {
  assessments_violations[, var] = gsub(",", "", assessments_violations[, var])
  assessments_violations[, var] = gsub("\\.", "", assessments_violations[, var])
  assessments_violations[, var] = gsub("company", "co", assessments_violations[, var])
  assessments_violations[, var] = gsub("comp", "co", assessments_violations[, var])
  assessments_violations[, var] = gsub("incorporated", "inc", assessments_violations[, var])
  assessments_violations[, var] = gsub("and", "&", assessments_violations[, var])
  assessments_violations[, var] = gsub("limited", "ltd", assessments_violations[, var])
}
assessments_violations$violatorname = string_fun_temp("violatorname")
assessments_violations$violator_name = string_fun_temp("violator_name")

assessments_violations[((assessments_violations$violator_name != assessments_violations$violatorname) & 
                         !is.na(assessments_violations$violator_name) & 
                         !is.na(assessments_violations$violatorname)), "violator_name"] = 
  gsub(" llc", "", assessments_violations[((assessments_violations$violator_name != assessments_violations$violatorname) & 
                                            !is.na(assessments_violations$violator_name) & 
                                            !is.na(assessments_violations$violatorname)), "violator_name"])

assessments_violations[((assessments_violations$violator_name != assessments_violations$violatorname) & 
                         !is.na(assessments_violations$violator_name) & 
                         !is.na(assessments_violations$violatorname)), "violatorname"] = 
  gsub(" llc", "", assessments_violations[((assessments_violations$violator_name != assessments_violations$violatorname) & 
                                            !is.na(assessments_violations$violator_name) & 
                                            !is.na(assessments_violations$violatorname)), "violatorname"])

# clean occurrencedate field
assessments_violations$occurrencedate = as.character(assessments_violations$occurrencedate)
assessments_violations$violation_occur_dt = as.character(assessments_violations$violation_occur_dt)
sum(assessments_violations$occurrencedate != assessments_violations$violation_occur_dt, na.rm = T) # 114
assessments_violations = assessments_violations[((assessments_violations$occurrencedate == assessments_violations$violation_occur_dt) | 
                                                  is.na(assessments_violations$occurrencedate) | 
                                                  is.na(assessments_violations$violation_occur_dt)), ]
assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt), ]$occurrencedate = 
  assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt), ]$violation_occur_dt
assessments_violations$violation_occur_dt = NULL
















# output - 866276 obs, 105 vars
saveRDS(assessments_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/assessments_violations.rds")


clean_inspections = readRDS(clean_inspections_file_name)

assessments_violations$inspecid = paste("L", assessments_violations$eventno, sep = "")
clean_inspections$inspecid = paste("L", clean_inspections$eventno, sep = "")

######################################################################################################

# MERGE VIOLATIONS AND INSPECTIONS

merged_violations = merge(assessments_violations, clean_inspections, by = c("mineid", "eventno"), all = T)

# Flag which observations merged from each dataset - this is only useful for comparing the merge to Stata output
merged_violations[, "inspecmerge"] = ifelse(!is.na(merged_violations$inspecid.y) 
    & !is.na(merged_violations$inspecid.x), 3, 0)
merged_violations[, "inspecmerge"] = ifelse(is.na(merged_violations$inspecid.x) 
    & !is.na(merged_violations$inspecid.y), 2, merged_violations[, "inspecmerge"])
merged_violations[, "inspecmerge"] = ifelse(is.na(merged_violations$inspecid.y) 
    & !is.na(merged_violations$inspecid.x), 1, merged_violations[, "inspecmerge"])

# Clean up redundant varnames from the merge
common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "inspecmerge"] == 2, merged_violations[, paste(common_varstbs[i], ".y", sep = "")], merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = T)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = T)] = common_varstbs

# Remove unnecessary vars
merged_violations = merged_violations[!is.na(merged_violations$violationno), c(-grep("nooftailingponds", names(merged_violations)), 
                                                                               -grep("timevacated", names(merged_violations)), 
                                                                               -grep("minegascategorycode", names(merged_violations)), 
                                                                               -grep("merge", names(merged_violations)), 
                                                                               -grep("coalcormetalmmine", names(merged_violations)), 
                                                                               -grep("coalcormetalm", names(merged_violations)))]

######################################################################################################

# FINAL VARIABLE CLEAN UP

# Create variables for mine-quarter level prediction dataset (format date vars), and for merging contractor data
datevars = names(merged_violations)[grep("date", names(merged_violations))]
for (i in 1:length(datevars)) {
  merged_violations[, datevars[i]] = as.Date(as.character(merged_violations[, datevars[i]]), "%m/%d/%Y")
}
merged_violations$quarter = as.yearqtr(merged_violations$dateissued)
merged_violations$year = as.yearqtr(merged_violations$dateissued)
merged_violations$year = as.numeric(format(merged_violations$quarter,'%Y'))

# Remove violations that have no dateissued and therefore no quarter data (only 44 observations)
merged_violations = merged_violations[complete.cases(merged_violations$quarter),]

# Drop observations before our study period (only 27 observations)
merged_violations = merged_violations[(merged_violations$quarter > "1999 Q4"),]
merged_violations = merged_violations[!is.na(merged_violations$quarter),]

# Turn blank cells to missing
merged_violations$contractorid = sub("^$", NA, merged_violations$contractorid)

# Two observations are missing contractorid but the violatortype is contractor and the violatorid is not missing 
merged_violations$contractorid = ifelse(merged_violations$violatortypecode == "Contractor" & is.na(merged_violations$contractorid), merged_violations$violator_id, merged_violations$contractorid)

######################################################################################################

# MERGE ON CONTRACTOR INFORMATIO

# Load data
contractor_quarterly_employment = readRDS(contractor_quarterly_employment_file_name)
contractor_yearly_employment = readRDS(contractor_yearly_employment_file_name)

# Merge it on
merged_violations = merge(merged_violations, contractor_quarterly_employment, by = c("contractorid", "quarter"), all = T)
merged_violations = merge(merged_violations, contractor_yearly_employment, by = c("contractorid", "year"), all = T)

# Divide annual vars by 4 if missing quarterly info, sub this into quarterly vars
merged_violations$con_avg_employee_cnt_qtr = ifelse(is.na(merged_violations$con_avg_employee_cnt_qtr) &
                                          !is.na(merged_violations$con_avg_employee_yr), (merged_violations$con_avg_employee_yr/4), merged_violations$con_avg_employee_cnt_qtr)
merged_violations$con_employee_hours_qtr = ifelse(is.na(merged_violations$con_employee_hours_qtr) &
                                                      !is.na(merged_violations$con_employee_hours_yr), (merged_violations$con_employee_hours_yr/4), merged_violations$con_employee_hours_qtr)
merged_violations$con_coal_prod_qtr = ifelse(is.na(merged_violations$con_coal_prod_qtr) &
                                                    !is.na(merged_violations$con_coal_prod_yr), (merged_violations$con_coal_prod_yr/4), merged_violations$con_coal_prod_qtr)

# Remove observations from contractor data that didn't merge onto our violations data 
merged_violations = merged_violations[complete.cases(merged_violations$violationno),]

######################################################################################################

# ELIMINATE VARS AND THEN OUTPUT

# Remove unnecessary variables and save
merged_violations = merged_violations[, c(-grep("datevacated", names(merged_violations)), 
                                          -grep("primarymill", names(merged_violations)), 
                                          -grep("generatedbyassessmt", names(merged_violations)), 
                                          -grep("source", names(merged_violations)),
                                          -grep("sigandsubindicator", names(merged_violations)), 
                                          -grep("dup", names(merged_violations)), 
                                          -grep("merge", names(merged_violations)), 
                                          -grep("con_avg_employee_yr", names(merged_violations)), 
                                          -grep("con_employee_hours_yr", names(merged_violations)), 
                                          -grep("samples", names(merged_violations)), 
                                          -grep("sic", names(merged_violations)), 
                                          -grep("con_coal_prod_yr", names(merged_violations)),
                                          -grep("(latitude|longitude|idate|idesc)", names(merged_violations)), 
                                          -match("outbyareas", names(merged_violations)), 
                                          -match("shafts", names(merged_violations)), 
                                          -match("refusepiles", names(merged_violations)), 
                                          -match("shaftslopesinkingconstinspected", names(merged_violations)), 
                                          -match("surfaceareasugmines", names(merged_violations)), 
                                          -match("surfaceworkings", names(merged_violations)), 
                                          -match("majorconstruction", names(merged_violations)), 
                                          -match("nbr_inspector", names(merged_violations)), 
                                          -match("inspection_begin_dt", names(merged_violations)),
                                          -match("inspection_end_dt", names(merged_violations)), 
                                          -match("last_action_cd", names(merged_violations)),
                                          -match("latest_term_due_time", names(merged_violations)), 
                                          -match("latesttermduedate", names(merged_violations)),
                                          -match("explosivestorage", names(merged_violations)), 
                                          -match("history_end_dt", names(merged_violations)),
                                          -match("history_start_dt", names(merged_violations)), 
                                          -match("docket_no", names(merged_violations)),
                                          -match("docket_status_cd", names(merged_violations)), 
                                          -match("orig_term_due_dt", names(merged_violations)),
                                          -match("orig_term_due_time", names(merged_violations)), 
                                          -match("enforcement_area", names(merged_violations)),
                                          -match("fiscalquarter", names(merged_violations)), 
                                          -match("fiscalyear", names(merged_violations)),
                                          -match("beginningdate", names(merged_violations)), 
                                          -match("billprintdate", names(merged_violations)),
                                          -match("billprintfiscalyr", names(merged_violations)), 
                                          -match("delinquent_dt", names(merged_violations)),
                                          -match("contestedindicator", names(merged_violations)), 
                                          -match("companyrecords", names(merged_violations)),
                                          -match("buildingconstinspected", names(merged_violations)), 
                                          -match("writtennotice", names(merged_violations)),
                                          -match("draglineconstinspected", names(merged_violations)), 
                                          -match("miscellaneous", names(merged_violations)))]

######################################################################################################

# MERGE CFR CODES ONTO VIOLATIONS AND MAKE VARIABLES FOR COLLAPSING ON

# Read data files
merged_cfr_key = readRDS(clean_cfr_key_file_name)

# Format cfr code
merged_violations$cfrstandardcode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", merged_violations$cfrstandardcode)
merged_violations$cfrstandardcode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "", merged_violations$cfrstandardcode)
names(merged_violations)[names(merged_violations) == "cfrstandardcode"] = "subsection_code"
merged_violations$subsection_code_marker = paste("S", merged_violations$subsection_code, sep = "")
merged_cfr_key$subsection_code_marker = paste("S", merged_cfr_key$subsection_code, sep = "")

# In some cases where subsection is missing, part_section is not and can be subbed in 
merged_violations$part_section2 = merged_violations$part_section
merged_violations$part_section2 = gsub("\\([a-z]+\\)", "", merged_violations$part_section2)
merged_violations$part_section2 = gsub("\\([0-9]+\\)", "", merged_violations$part_section2)
merged_violations$subsection_code = ifelse((is.na(merged_violations$subsection_code) 
                                            & !is.na(merged_violations$part_section2)), merged_violations$part_section2, 
                                           merged_violations$subsection_code)

# Merge violations and cfr key
merged_violations = merge(merged_violations, merged_cfr_key, by = "subsection_code", all = T)

# Flag which observations merged from each dataset - this is only useful for comparing the merge to Stata output
merged_violations[, "merge"] = ifelse(!is.na(merged_violations$subsection_code_marker.y) 
                                      & !is.na(merged_violations$subsection_code_marker.x), 3, 0)
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.x) 
                                      & !is.na(merged_violations$subsection_code_marker.y), 2, merged_violations[, "merge"])
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.y) 
                                      & !is.na(merged_violations$subsection_code_marker.x), 1, merged_violations[, "merge"])

# Clean up redundant varnames from the merge
common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "merge"] == 2, 
                                                                         merged_violations[, paste(common_varstbs[i], ".y", sep = "")], 
                                                                         merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = T)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = T)] = common_varstbs
rm(merged_cfr_key, common_varstbs, i)

# Format date vars
datevars = names(merged_violations)[grep("date", names(merged_violations))]
for (i in 1:length(datevars)) {
  merged_violations[, datevars[i]] = as.Date(as.character(merged_violations[, datevars[i]]), "%m/%d/%Y")
}

# Remove observations from cfr data that didn't merge onto our violations data 
merged_violations = merged_violations[complete.cases(merged_violations$violationno),]

# Condition the per-day vars on positive denominator. (There are 256 cases of zero inspection days and positive violation counts). 6/6/16
merged_violations$contractor_violation_cnt = ifelse(merged_violations$violatortypecode == "Contractor", merged_violations$violator_violation_cnt, NA)
merged_violations$operator_violation_pInspDay = ifelse(merged_violations$violatortypecode == "Operator" & merged_violations$violator_inspection_day_cnt > 0, 
                                                       merged_violations$violator_violation_cnt/merged_violations$violator_inspection_day_cnt, NA)
merged_violations$contractor_repeated_viol_cnt = ifelse(merged_violations$violatortypecode == "Contractor", merged_violations$violator_repeated_viol_cnt, NA)
merged_violations$operator_repeated_viol_pInspDay = ifelse(merged_violations$violatortypecode == "Operator" & merged_violations$violator_inspection_day_cnt > 0, 
                                                           merged_violations$violator_repeated_viol_cnt/merged_violations$violator_inspection_day_cnt, NA)

######################################################################################################

# REMOVE UNNECESSARY VARS AND OUTPUT 

merged_violations = merged_violations[, c(-grep("merge", names(merged_violations)))]

saveRDS(merged_violations, file = clean_violations_out_file_name)
rm(assessments_violations, clean_inspections, common_varstbs, violnames, i)

######################################################################################################
