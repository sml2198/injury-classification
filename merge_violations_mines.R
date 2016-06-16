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

assessments_violations = merge(clean_assessments, clean_violations, by = c("mineid","violationno"), all = T)
# EVERYTHING BETWEEN THESE LINES IS RETIRED - WE MUST MERGE BY VIOLNO AND MINEID #####################################
#assessments_violations[, "merge"] = ifelse(!is.na(assessments_violations$mineid.y) & !is.na(assessments_violations$mineid.x), 3, 0)
#assessments_violations[, "merge"] = ifelse(is.na(assessments_violations$mineid.x) & !is.na(assessments_violations$mineid.y), 2, assessments_violations[, "merge"])
#assessments_violations[, "merge"] = ifelse(is.na(assessments_violations$mineid.y) & !is.na(assessments_violations$mineid.x), 1, assessments_violations[, "merge"])
#table(assessments_violations[assessments_violations$calendaryear >= 2000,]$merge) aligns with the STATA version except for the 43 pre-2000 open data 
#observations noted at the end of "clean_violations.do"
#1       2       3 
#55668 1616855 2821359
#Open Data Only:
#2       3 
#54144 2107492 
#for (i in 1:length(common_varstbs)) {
#  assessments_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(assessments_violations[, "merge"] == 2, assessments_violations[, paste(common_varstbs[i], ".y", sep = "")], assessments_violations[, paste(common_varstbs[i], ".x", sep = "")])
#}

########################################################################################################################

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

saveRDS(merged_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds")
rm(assessments_violations, clean_inspections, common_varstbs, violnames, i)
