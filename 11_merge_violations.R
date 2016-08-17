# NIOSH Project 2014-N-15776

# 11 - Merge Violations
  # Reads, merges, cleans, then outputs cleaned 
    # violations (6_clean_violations), 
    # assessments (7_clean_assessments), 
    # inspections (8_clean_inspections), 
    # quarterly and annual contractor employment (9_clean_employment), and 
    # cfr code (10_clean_cfr_key) data

# Last edit 8/4/16

######################################################################################################

library(stringr)
library(zoo)

# define file names
  # input: clean violations data (6_clean_violations.R)
clean_violations_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds"
  # input: clean assessments data (7_clean_assessments.R)
clean_assessments_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds" 
  # input: clean inspections data (8_clean_inspections.R)
clean_inspections_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_inspections.rds"
  # input: quarterly contractor employment data (9_clean_employment.R)
contractor_quarterly_employment_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds"
  # input: annual contractor employment data (9_clean_employment.R)
contractor_yearly_employment_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds"
  # input: cfr key data (10_clean_cfr_key.R)
clean_cfr_key_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_cfr_key.rds"
  # output: merged violations data 
clean_violations_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds"

######################################################################################################

# READ AND MERGE ASSESSMENTS DATA AND VIOLATIONS DATA, THEN CLEAN

# read assessments data - 843,818 obs, 59 vars
clean_assessments = readRDS(clean_assessments_file_name) 

# read violations data - 868,722 obs, 62 vars
clean_violations = readRDS(clean_violations_in_file_name)

# merge assessments data with violations data - 843,787 obs, 119 vars
assessments_violations = merge(clean_violations, clean_assessments, by = c("mineid","violationno"))

# memory
rm(clean_violations, clean_assessments)

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

# clean violator name field - this is extremely trivial being that we never use this field!
string_fun_temp = function(var) {
  assessments_violations[, var] = gsub(",", "", assessments_violations[, var])
  assessments_violations[, var] = gsub("\\.", "", assessments_violations[, var])
  assessments_violations[, var] = gsub("company", "co", assessments_violations[, var])
  assessments_violations[, var] = gsub("comp", "co", assessments_violations[, var])
  assessments_violations[, var] = gsub("incorporated", "inc", assessments_violations[, var])
  assessments_violations[, var] = gsub(" and", " &", assessments_violations[, var])
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
sum((assessments_violations$occurrencedate != assessments_violations$violation_occur_dt), na.rm = TRUE) # 115

# remove these cases - wind up with 843,672 obs
assessments_violations = assessments_violations[((assessments_violations$occurrencedate == assessments_violations$violation_occur_dt) | 
                                                  is.na(assessments_violations$occurrencedate) | 
                                                  is.na(assessments_violations$violation_occur_dt)), ]

# if missing occurrencedate, fill in violation_occur_dt, then drop so we have just one var
assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt), ]$occurrencedate = 
  assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt), ]$violation_occur_dt
assessments_violations$violation_occur_dt = NULL

######################################################################################################

# READ INSPECTIONS DATA, MERGE WITH VIOLATIONS DATA, THEN CLEAN

# read inspections data - 192,639 obs, 47 vars
clean_inspections = readRDS(clean_inspections_file_name)

# merge inspections data and merged assessments and violations data
clean_inspections$inspecid = paste("L", clean_inspections$eventno, sep = "")
assessments_violations$inspecid = paste("L", assessments_violations$eventno, sep = "")
merged_violations = merge(assessments_violations, clean_inspections, by = c("mineid", "eventno")) # 836,639 obs, 152 vars

# memory
rm(clean_inspections, assessments_violations)

# flag observations merged from each dataset - this is only useful for comparing the merge to Stata output
merged_violations$inspecmerge = ifelse((!is.na(merged_violations$inspecid.y) & !is.na(merged_violations$inspecid.x)), 3, 0)
merged_violations$inspecmerge = ifelse((is.na(merged_violations$inspecid.x) & !is.na(merged_violations$inspecid.y)), 2, merged_violations$inspecmerge)
merged_violations$inspecmerge = ifelse((is.na(merged_violations$inspecid.y) & !is.na(merged_violations$inspecid.x)), 1, merged_violations$inspecmerge)

# remove duplicate variables - get down to 146
common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = TRUE)], fixed = TRUE)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "inspecmerge"] == 2, merged_violations[, paste(common_varstbs[i], ".y", sep = "")], merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = T)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = T)] = common_varstbs

# drop unnecessary variables - down to 141 vars
merged_violations$nooftailingponds =
  merged_violations$timevacated =
  merged_violations$timeterminated =
  merged_violations$minegascategorycode =
  merged_violations$merge =
  merged_violations$coal_metal_ind = NULL

# format date variables
datevars = names(merged_violations)[grep("date", names(merged_violations))]
for (i in 1:length(datevars)) {
  merged_violations[, datevars[i]] = as.Date(as.character(merged_violations[, datevars[i]]), "%m/%d/%Y")
}
merged_violations$quarter = as.yearqtr(merged_violations$dateissued)
merged_violations$year = as.yearqtr(merged_violations$dateissued)
merged_violations$year = as.numeric(format(merged_violations$quarter, "%Y"))

# drop data before study period
merged_violations = merged_violations[(merged_violations$quarter > "1999 Q4"), ] # now 836,612 obs

# format contractorid
merged_violations$contractorid = sub("^$", NA, merged_violations$contractorid)
merged_violations$contractorid = ifelse((merged_violations$violatortypecode == "Contractor" & 
                                           is.na(merged_violations$contractorid)), merged_violations$violator_id, merged_violations$contractorid) # 2 obs

######################################################################################################

# READ QUARTERLY AND ANNUAL CONTRACTOR EMPLOYMENT DATA, THEN MERGE WITH VIOLATIONS DATA AND CLEAN

# read quarterly contractor employment data # 28487 obs, 5 vars
contractor_quarterly_employment = readRDS(contractor_quarterly_employment_file_name)

# read annual contractor employment data # 5404 obs, 6 vars
contractor_yearly_employment = readRDS(contractor_yearly_employment_file_name)

# merge contractor employment data an violations data
merged_violations = merge(merged_violations, contractor_quarterly_employment, by = c("contractorid", "quarter"), all = TRUE) # 863384 obs, 146 vars
merged_violations = merge(merged_violations, contractor_yearly_employment, by = c("contractorid", "year"), all = TRUE) # 867,896 obs, 151 vars

# memory
rm(contractor_quarterly_employment, contractor_yearly_employment)

# drop contractor data that didn't merge onto violations
merged_violations = merged_violations[complete.cases(merged_violations$violationno), ] # now 836,612 obs, 76,296 unique eventno, 1669 mines

# fill in quarterly variables with annual information if missing  
merged_violations$con_avg_employee_cnt_qtr = ifelse((is.na(merged_violations$con_avg_employee_cnt_qtr) &
                                                       !is.na(merged_violations$con_avg_employee_yr)), (merged_violations$con_avg_employee_yr / 4), merged_violations$con_avg_employee_cnt_qtr)
merged_violations$con_employee_hours_qtr = ifelse((is.na(merged_violations$con_employee_hours_qtr) & 
                                                     !is.na(merged_violations$con_employee_hours_yr)), (merged_violations$con_employee_hours_yr / 4), merged_violations$con_employee_hours_qtr)
merged_violations$con_coal_prod_qtr = ifelse((is.na(merged_violations$con_coal_prod_qtr) & 
                                                !is.na(merged_violations$con_coal_prod_yr)), (merged_violations$con_coal_prod_yr / 4), merged_violations$con_coal_prod_qtr)

# drop unnecessary variables - wind up with 104
merged_violations = merged_violations[, -grep("samples", names(merged_violations))]
merged_violations$primarymill =
  merged_violations$generatedbyassessmt =
  merged_violations$sigandsubindicator =
  merged_violations$dup =
  merged_violations$con_avg_employee_yr =
  merged_violations$con_employee_hours_yr =
  merged_violations$con_coal_prod_yr =
  merged_violations$outbyareas =
  merged_violations$shafts =
  merged_violations$refusepiles =
  merged_violations$shaftslopesinkingconstinspected =
  merged_violations$surfaceareasugmines =
  merged_violations$surfaceworkings =
  merged_violations$majorconstruction =
  merged_violations$nbr_inspector =
  merged_violations$inspection_begin_dt =
  merged_violations$inspection_end_dt =
  merged_violations$last_action_cd =
  merged_violations$latest_term_due_time =
  merged_violations$latesttermduedate =
  merged_violations$explosivestorage =
  merged_violations$history_end_dt =
  merged_violations$history_start_dt =
  merged_violations$docket_no =
  merged_violations$docket_status_cd =
  merged_violations$orig_term_due_dt =
  merged_violations$orig_term_due_time =
  merged_violations$enforcement_area =
  merged_violations$fiscalquarter =
  merged_violations$fiscalyear =
  merged_violations$beginningdate =
  merged_violations$billprintdate =
  merged_violations$billprintfiscalyr =
  merged_violations$delinquent_dt =
  merged_violations$contestedindicator =
  merged_violations$companyrecords =
  merged_violations$buildingconstinspected =
  merged_violations$writtennotice =
  merged_violations$draglineconstinspected =
  merged_violations$miscellaneous = NULL

######################################################################################################

# READ CFR CODE DATA AND MERGE WITH VIOLATIONS DATA, THEN CLEAN AND OUTPUT

# read cfr code data - 2026 obs, 11 vars
merged_cfr_key = readRDS(clean_cfr_key_file_name)

# format cfr code
merged_violations$cfrstandardcode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", merged_violations$cfrstandardcode)
merged_violations$cfrstandardcode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "",  merged_violations$cfrstandardcode)
names(merged_violations)[names(merged_violations) == "cfrstandardcode"] = "subsection_code"
merged_violations$subsection_code_marker = paste("S", merged_violations$subsection_code, sep = "")
merged_cfr_key$subsection_code_marker = paste("S", merged_cfr_key$subsection_code, sep = "")

# where subsection is missing, part_section can be subbed in 
merged_violations$part_section2 = merged_violations$part_section
merged_violations$part_section2 = gsub("\\([a-z]+\\)", "", merged_violations$part_section2)
merged_violations$part_section2 = gsub("\\([0-9]+\\)", "", merged_violations$part_section2)
merged_violations$subsection_code = ifelse((is.na(merged_violations$subsection_code) & !is.na(merged_violations$part_section2)), 
                                           merged_violations$part_section2, merged_violations$subsection_code)

# merge cfr key data and violations data - 837,847 obs, 117 vars
merged_violations = merge(merged_violations, merged_cfr_key, by = "subsection_code", all = TRUE)

# flag observations merged from each dataset - this is only useful for comparing the merge to Stata output
merged_violations[, "merge"] = ifelse(!is.na(merged_violations$subsection_code_marker.y) 
                                      & !is.na(merged_violations$subsection_code_marker.x), 3, 0)
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.x) 
                                      & !is.na(merged_violations$subsection_code_marker.y), 2, merged_violations[, "merge"])
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.y) 
                                      & !is.na(merged_violations$subsection_code_marker.x), 1, merged_violations[, "merge"])

# remove duplicate variables
common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = TRUE)], fixed = TRUE)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "merge"] == 2, 
                                                                         merged_violations[, paste(common_varstbs[i], ".y", sep = "")], 
                                                                         merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = TRUE)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = TRUE)] = common_varstbs

# memory
rm(merged_cfr_key)

# drop data that didn't merge onto violations
merged_violations = merged_violations[complete.cases(merged_violations$violationno), ] # now 836,612 obs, 76,296 eventnos, 1669 mines

# condition the per-day vars on positive denominator
  # there are 256 cases of zero inspection days and positive violation counts 6/6/16
merged_violations$contractor_violation_cnt = ifelse(merged_violations$violatortypecode == "Contractor", merged_violations$violator_violation_cnt, NA)
merged_violations$operator_violation_pInspDay = ifelse((merged_violations$violatortypecode == "Operator" & merged_violations$violator_inspection_day_cnt > 0), 
                                                       merged_violations$violator_violation_cnt/merged_violations$violator_inspection_day_cnt, NA)
merged_violations$contractor_repeated_viol_cnt = ifelse(merged_violations$violatortypecode == "Contractor", merged_violations$violator_repeated_viol_cnt, NA)
merged_violations$operator_repeated_viol_pInspDay = ifelse((merged_violations$violatortypecode == "Operator" & merged_violations$violator_inspection_day_cnt > 0), 
                                                           merged_violations$violator_repeated_viol_cnt/merged_violations$violator_inspection_day_cnt, NA)

# drop unnecessary variables - wind up with 119 vars
merged_violations = merged_violations[, c(-grep("merge", names(merged_violations)))]

# output merged violations data - 836,612 obs, 76,296 eventnos, 1669 mines, 119 vars
saveRDS(merged_violations, file = clean_violations_out_file_name)

######################################################################################################
