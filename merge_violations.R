##NIOSH STUDY##
##Professor Alison Morantz##
##Stanford Law School##

#Coded by Nikhil Saifullah

#Description

#In this file, we merge inspections and assessments to each 
#violation.

library(stringr)

clean_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds") #Note: Carolyn data doesn't have eventno!
clean_violations = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds")
clean_inspections = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_inspections.rds")

######################################################################################################
# MERGE VIOLATIONS AND ASSESSMENTS
assessments_violations = merge(clean_assessments, clean_violations, by = c("mineid","violationno"), all = T)

assessments_violations$eventno.x = ifelse((is.na(assessments_violations$eventno.x) & !is.na(assessments_violations$eventno.y)), assessments_violations$eventno.y, assessments_violations$eventno.x)

common_varstbs = sub(".x", "", names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)], fixed = T)
assessments_violations = assessments_violations[, -grep(".y", names(assessments_violations), fixed = T)]
names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)] = common_varstbs

# Clean up violator name fields

assessments_violations[, "violatorname"] = tolower(str_trim(assessments_violations[, "violatorname"], side = c("both")))
assessments_violations[, "violator_name"] = tolower(str_trim(assessments_violations[, "violator_name"], side = c("both")))

violnames = c(grep("violatorname", names(assessments_violations)), grep("violator_name", names(assessments_violations)))

#UNDER CONSTRUCTION PAST THIS POINT; just need to examine why we have less non-missing disagreements between violator_name and violatorname than STATA

for (i in 1:length(violnames)) {
  assessments_violations[, violnames[i]] = gsub(",", "", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("\\.", "", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("company", "co", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("comp", "co", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("incorporated", "inc", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("and", "&", assessments_violations[, violnames[i]])
  assessments_violations[, violnames[i]] = gsub("limited", "ltd", assessments_violations[, violnames[i]])
}

assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violator_name"] = gsub(" llc", "", assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violator_name"])
assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violatorname"] = gsub(" llc", "", assessments_violations[(assessments_violations$violator_name != assessments_violations$violatorname) & !is.na(assessments_violations$violator_name) & !is.na(assessments_violations$violatorname), "violatorname"])

#Clean up occurrencedate field
assessments_violations$occurrencedate = as.character(assessments_violations$occurrencedate)
assessments_violations$violation_occur_dt = as.character(assessments_violations$violation_occur_dt)
sum(assessments_violations$occurrencedate != assessments_violations$violation_occur_dt, na.rm = T) #214
assessments_violations = assessments_violations[(assessments_violations$occurrencedate == assessments_violations$violation_occur_dt) | is.na(assessments_violations$occurrencedate) | is.na(assessments_violations$violation_occur_dt),]
assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt),]$occurrencedate = assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt),]$violation_occur_dt

#Drop uninformative variables and perform final renaming before the save
assessments_violations = assessments_violations[, c(-grep("issuedate", names(assessments_violations)), -grep("final_order_issue_dt", names(assessments_violations)), -grep("last_action_dt", names(assessments_violations)), 
                                                    -grep("violation_occur_dt", names(assessments_violations)), -grep("datevacated", names(assessments_violations)), -grep("righttoconferencedate", names(assessments_violations)), 
                                                    -grep("bill_print_dt", names(assessments_violations)), -grep("number", names(assessments_violations)), -grep("violdup", names(assessments_violations)))]
names(assessments_violations)[names(assessments_violations) == "bill_print_fiscal_yr"] = "billprintfiscalyr"
names(assessments_violations)[names(assessments_violations) == "good_faith_ind"] = "goodfaithind"

#Uses the "calendaryear" field from violations data to purge post-1999 data from Carolyn's pull
#This line causes assessments_violations to have all NA's
#assessments_violations = assessments_violations[(assessments_violations$src == "early" & assessments_violations$calendaryear <= 1999 &
#                                                   !is.na(assessments_violations$src) & !is.na(assessments_violations$calendaryear)) |
#                                                  (assessments_violations$src == "open_data") & !is.na(assessments_violations$src) | 
#                                                  assessments_violations$merge == 1 | assessments_violations$merge == 2,]
# assessments_violations = assessments_violations[, -grep("merge", names(assessments_violations))]

saveRDS(assessments_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/assessments_violations.rds") #should we name to align w/ STATA version?
rm(clean_violations, clean_assessments)

#Now that data will be partially merged on eventno we do some basic cleaning
#clean_inspections$eventno = ifelse(nchar(clean_inspections$eventno) < 7, paste("0", clean_inspections$eventno, sep = ""), clean_inspections$eventno)
#assessments_violations$eventno = ifelse(nchar(assessments_violations$eventno) < 7, paste("0", assessments_violations$eventno, sep = ""), assessments_violations$eventno)

assessments_violations$inspecid = paste("L", assessments_violations$eventno, sep = "")
clean_inspections$inspecid = paste("L", clean_inspections$eventno, sep = "")

######################################################################################################
# MERGE VIOLATIONS AND INSPECTIONS
merged_violations = merge(assessments_violations, clean_inspections, by = c("mineid", "eventno"), all = T)

merged_violations[, "inspecmerge"] = ifelse(!is.na(merged_violations$inspecid.y) & !is.na(merged_violations$inspecid.x), 3, 0)
merged_violations[, "inspecmerge"] = ifelse(is.na(merged_violations$inspecid.x) & !is.na(merged_violations$inspecid.y), 2, merged_violations[, "inspecmerge"])
merged_violations[, "inspecmerge"] = ifelse(is.na(merged_violations$inspecid.y) & !is.na(merged_violations$inspecid.x), 1, merged_violations[, "inspecmerge"])
table(merged_violations$inspecmerge)
#Open Data Only: 
#1       2       3 
#134 1812672 2161288

common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "inspecmerge"] == 2, merged_violations[, paste(common_varstbs[i], ".y", sep = "")], merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = T)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = T)] = common_varstbs

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

saveRDS(merged_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds")
rm(assessments_violations, clean_inspections, common_varstbs, violnames, i)

######################################################################################################
