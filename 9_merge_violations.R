# NIOSH Project 2014-N-15776

# 9 - Merge Violations
    # In this file, we merge inspections and assessments to each violation.

# Last edit 7/19/16

######################################################################################################

library(stringr)

# define file names
  # input: cleaned violations produced in 6_clean_violations.R
clean_violations = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds")
  # input: cleaned assessments produced in 7_clean_assessments.R
clean_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds") #Note: Carolyn data doesn't have eventno!
  # input: cleaned inspections produced in 8_clean_inspections.R
clean_inspections = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_inspections.rds")

######################################################################################################

# MERGE VIOLATIONS AND ASSESSMENTS

assessments_violations = merge(clean_assessments, clean_violations, by = c("mineid","violationno"), all = T)

# Pipe in eventno where it might be missing from either dataset
assessments_violations$eventno.x = ifelse((is.na(assessments_violations$eventno.x) & !is.na(assessments_violations$eventno.y)), assessments_violations$eventno.y, assessments_violations$eventno.x)

# Remove duplicate vars as a result of merge
common_varstbs = sub(".x", "", names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)], fixed = T)
assessments_violations = assessments_violations[, -grep(".y", names(assessments_violations), fixed = T)]
names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)] = common_varstbs

# Clean up violator name fields
assessments_violations[, "violatorname"] = tolower(str_trim(assessments_violations[, "violatorname"], side = c("both")))
assessments_violations[, "violator_name"] = tolower(str_trim(assessments_violations[, "violator_name"], side = c("both")))
violnames = c(grep("violatorname", names(assessments_violations)), grep("violator_name", names(assessments_violations)))

# Do some string replacements to clean up violator name fields and reduce disagreements
# This actually isn't important for anything down the line, but was useful for us comparing the violations and inspections datasets
for (i in 1:length(violnames)) {
  assessments_violations[, violnames[i]] = gsub(",", "", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("\\.", "", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("company", "co", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("comp", "co", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("incorporated", "inc", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("and", "&", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("limited", "ltd", assessments_violations[, violnames[i]])
}

# More work to make violator names agree. This is actually not important for any of our modeling, but is useful as a sanity check on our merge.
assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violator_name"] = gsub(" llc", "", assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violator_name"])
assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violatorname"] = gsub(" llc", "", assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violatorname"])

# Clean up occurrencedate field (we use this field later on to create the quarter variables)
assessments_violations$occurrencedate = as.character(assessments_violations$occurrencedate)
assessments_violations$violation_occur_dt = as.character(assessments_violations$violation_occur_dt)
sum(assessments_violations$occurrencedate != assessments_violations$violation_occur_dt, na.rm = T) #214
assessments_violations = assessments_violations[(assessments_violations$occurrencedate == assessments_violations$violation_occur_dt) | is.na(assessments_violations$occurrencedate) | is.na(assessments_violations$violation_occur_dt),]
assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt),]$occurrencedate = assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt),]$violation_occur_dt

# Drop uninformative variables and perform final renaming before the save
assessments_violations = assessments_violations[, c(-grep("issuedate", names(assessments_violations)), -grep("final_order_issue_dt", names(assessments_violations)), -grep("last_action_dt", names(assessments_violations)), 
                                                    -grep("violation_occur_dt", names(assessments_violations)), -grep("datevacated", names(assessments_violations)), -grep("righttoconferencedate", names(assessments_violations)), 
                                                    -grep("bill_print_dt", names(assessments_violations)), -grep("number", names(assessments_violations)), -grep("violdup", names(assessments_violations)))]
names(assessments_violations)[names(assessments_violations) == "bill_print_fiscal_yr"] = "billprintfiscalyr"
names(assessments_violations)[names(assessments_violations) == "good_faith_ind"] = "goodfaithind"

saveRDS(assessments_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/assessments_violations.rds") #should we name to align w/ STATA version?
rm(clean_violations, clean_assessments)

assessments_violations$inspecid = paste("L", assessments_violations$eventno, sep = "")
clean_inspections$inspecid = paste("L", clean_inspections$eventno, sep = "")

######################################################################################################

# THIS CODE IS RETIRED.

# Uses the "calendaryear" field from violations data to purge post-1999 data from Carolyn's pull
# This line causes assessments_violations to have all NA's
# assessments_violations = assessments_violations[(assessments_violations$src == "early" & assessments_violations$calendaryear <= 1999 &
#                                                   !is.na(assessments_violations$src) & !is.na(assessments_violations$calendaryear)) |
#                                                  (assessments_violations$src == "open_data") & !is.na(assessments_violations$src) | 
#                                                  assessments_violations$merge == 1 | assessments_violations$merge == 2,]
# assessments_violations = assessments_violations[, -grep("merge", names(assessments_violations))]

# Now that data will be partially merged on eventno we do some basic cleaning
# clean_inspections$eventno = ifelse(nchar(clean_inspections$eventno) < 7, paste("0", clean_inspections$eventno, sep = ""), clean_inspections$eventno)
# assessments_violations$eventno = ifelse(nchar(assessments_violations$eventno) < 7, paste("0", assessments_violations$eventno, sep = ""), assessments_violations$eventno)

######################################################################################################
# MERGE VIOLATIONS AND INSPECTIONS

merged_violations = merge(assessments_violations, clean_inspections, by = c("mineid", "eventno"), all = T)

# Flag which observations merged from each dataset - this is only useful for comparing the merge to Stata output
merged_violations[, "inspecmerge"] = ifelse(!is.na(merged_violations$inspecid.y) & !is.na(merged_violations$inspecid.x), 3, 0)
merged_violations[, "inspecmerge"] = ifelse(is.na(merged_violations$inspecid.x) & !is.na(merged_violations$inspecid.y), 2, merged_violations[, "inspecmerge"])
merged_violations[, "inspecmerge"] = ifelse(is.na(merged_violations$inspecid.y) & !is.na(merged_violations$inspecid.x), 1, merged_violations[, "inspecmerge"])

# Clean up redundant varnames from the merge
common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "inspecmerge"] == 2, merged_violations[, paste(common_varstbs[i], ".y", sep = "")], merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = T)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = T)] = common_varstbs

# Remove unnecessary vars
merged_violations = merged_violations[!is.na(merged_violations$violationno), c(-grep("nooftailingponds", names(merged_violations)), -grep("timevacated", names(merged_violations)), 
                                                                                  -grep("minegascategorycode", names(merged_violations)), -grep("merge", names(merged_violations)), 
                                                                                  -grep("coalcormetalmmine", names(merged_violations)), -grep("coalcormetalm", names(merged_violations)))]

######################################################################################################
# BRING IN CONTRACTOR DATA - WILL BE MERGED ON CONTRACTOR ID 

# OPEN DATA - QUARTERLY CONTRACTOR EMPLOYMENT/PRODUCTION
# THIS FILE DOES NOT CONTAIN MINEID'S - ONLY CONTRACTORID'S. CAN BE MERGED INTO VIOLATIONS DATA BY CONTRACTORID IF AND ONLY IF THE CONTRACTOR WAS CITED AT A MINE.
contractor_quarterly_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdQuarterly.txt", header = T, sep = "|")
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_YR"] = "year"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_QTR"] = "quarter"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_cnt_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "COAL_PRODUCTION"] = "con_coal_prod_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "HOURS_WORKED"] = "con_employee_hours_qtr"
contractor_quarterly_employment = contractor_quarterly_employment[contractor_quarterly_employment$SUBUNIT=="UNDERGROUND" & contractor_quarterly_employment$COAL_METAL_IND=="C",]
contractor_quarterly_employment = contractor_quarterly_employment[, c(-grep("COAL_METAL_IND", names(contractor_quarterly_employment)), -match("SUBUNIT", names(contractor_quarterly_employment)), 
                                                                      -grep("FISCAL_YR", names(contractor_quarterly_employment)), -match("FISCAL_QTR", names(contractor_quarterly_employment)), 
                                                                      -match("SUBUNIT_CD", names(contractor_quarterly_employment)), -match("CONTRACTOR_NAME", names(contractor_quarterly_employment)))]

contractor_quarterly_employment = ddply(contractor_quarterly_employment[, c(match("con_avg_employee_cnt_qtr", names(contractor_quarterly_employment)), 
                                                                            match("con_coal_prod_qtr", names(contractor_quarterly_employment)), 
                                                                            match("con_employee_hours_qtr", names(contractor_quarterly_employment)),
                                                                            match("contractorid", names(contractor_quarterly_employment)), 
                                                                            match("year", names(contractor_quarterly_employment)), 
                                                                            match("quarter", names(contractor_quarterly_employment)))], c("contractorid", "year", "quarter"), 
                                        function(x) colMeans(x[, c(match("con_avg_employee_cnt_qtr", names(x)), match("con_employee_hours_qtr", names(x)), 
                                                                   match("con_coal_prod_qtr", names(x)))], na.rm = T))
# Format date vars so that "quarter" contains date-formatted year AND quarter info
contractor_quarterly_employment$quarter = paste(contractor_quarterly_employment$year, contractor_quarterly_employment$quarter, sep= "-")
contractor_quarterly_employment$quarter = ifelse(contractor_quarterly_employment$quarter == "NA-NA", NA, contractor_quarterly_employment$quarter)
contractor_quarterly_employment$quarter = as.yearqtr(contractor_quarterly_employment$quarter)
contractor_quarterly_employment = contractor_quarterly_employment[, c(-grep("year", names(contractor_quarterly_employment)))]

saveRDS(contractor_quarterly_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds")

# OPEN DATA - ANNUAL CONTRACTOR EMPLOYMENT/PRODUCTION
contractor_yearly_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdYearly.txt", header = T, sep = "|")
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CAL_YR"] = "year"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "con_employee_hours_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_COAL_PRODUCTION"] = "con_coal_prod_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_HOURS"] = "con_hours_yr"
contractor_yearly_employment = contractor_yearly_employment[contractor_yearly_employment$SUBUNIT=="UNDERGROUND" & contractor_yearly_employment$COAL_METAL_IND=="C",]
contractor_yearly_employment = contractor_yearly_employment[, c(-grep("COAL_METAL_IND", names(contractor_yearly_employment)), 
                                                                -match("SUBUNIT", names(contractor_yearly_employment)), -match("CONTRACTOR_NAME", names(contractor_yearly_employment)), 
                                                                -match("SUBUNIT_CD", names(contractor_yearly_employment)))]
saveRDS(contractor_yearly_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds")

######################################################################################################
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
# MERGE ON CONTRACTOR INFORMATION

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
# Remove unnecessary variables and save
merged_violations = merged_violations[, c(-grep("datevacated", names(merged_violations)), -grep("primarymill", names(merged_violations)), 
                                          -grep("generatedbyassessmt", names(merged_violations)), -grep("source", names(merged_violations)),
                                          -grep("sigandsubindicator", names(merged_violations)), -grep("dup", names(merged_violations)), 
                                          -grep("merge", names(merged_violations)), -grep("con_avg_employee_yr", names(merged_violations)), 
                                          -grep("con_employee_hours_yr", names(merged_violations)), -grep("samples", names(merged_violations)), 
                                          -grep("sic", names(merged_violations)), -grep("con_coal_prod_yr", names(merged_violations)),
                                          -grep("(latitude|longitude|idate|idesc)", names(merged_violations)), -match("outbyareas", names(merged_violations)), 
                                          -match("shafts", names(merged_violations)), -match("refusepiles", names(merged_violations)), 
                                          -match("shaftslopesinkingconstinspected", names(merged_violations)), -match("surfaceareasugmines", names(merged_violations)), 
                                          -match("surfaceworkings", names(merged_violations)), -match("majorconstruction", names(merged_violations)), 
                                          -match("nbr_inspector", names(merged_violations)), -match("inspection_begin_dt", names(merged_violations)),
                                          -match("inspection_end_dt", names(merged_violations)), -match("last_action_cd", names(merged_violations)),
                                          -match("latest_term_due_time", names(merged_violations)), -match("latesttermduedate", names(merged_violations)),
                                          -match("explosivestorage", names(merged_violations)), -match("history_end_dt", names(merged_violations)),
                                          -match("history_start_dt", names(merged_violations)), -match("docket_no", names(merged_violations)),
                                          -match("docket_status_cd", names(merged_violations)), -match("orig_term_due_dt", names(merged_violations)),
                                          -match("orig_term_due_time", names(merged_violations)), -match("enforcement_area", names(merged_violations)),
                                          -match("fiscalquarter", names(merged_violations)), -match("fiscalyear", names(merged_violations)),
                                          -match("beginningdate", names(merged_violations)), -match("billprintdate", names(merged_violations)),
                                          -match("billprintfiscalyr", names(merged_violations)), -match("delinquent_dt", names(merged_violations)),
                                          -match("contestedindicator", names(merged_violations)), -match("companyrecords", names(merged_violations)),
                                          -match("buildingconstinspected", names(merged_violations)), -match("writtennotice", names(merged_violations)),
                                          -match("draglineconstinspected", names(merged_violations)), -match("miscellaneous", names(merged_violations)))]

saveRDS(merged_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds")
rm(assessments_violations, clean_inspections, common_varstbs, violnames, i)

######################################################################################################
