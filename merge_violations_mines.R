#HEADER
library(stringr)

clean_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds") #Note: Carolyn data doesn't have eventno!
clean_violations = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds")
clean_mines = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds")
clean_Inspections = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_Inspections.rds")

assessments_violations = merge(clean_assessments, clean_violations, by = "violationno", all = T)
assessments_violations[, "merge"] = ifelse(!is.na(assessments_violations$mineid.y) & !is.na(assessments_violations$mineid.x), 3, 0)
assessments_violations[, "merge"] = ifelse(is.na(assessments_violations$mineid.x) & !is.na(assessments_violations$mineid.y), 2, assessments_violations[, "merge"])
assessments_violations[, "merge"] = ifelse(is.na(assessments_violations$mineid.y) & !is.na(assessments_violations$mineid.x), 1, assessments_violations[, "merge"])
table(assessments_violations$merge) 
#table(assessments_violations[assessments_violations$calendaryear >= 2000,]$merge) aligns with the STATA version except for the 43 pre-2000 open data 
#observations noted at the end of "clean_violations.do"
#1       2       3 
#55668 1616855 2821359
#Open Data Only:
#2       3 
#54144 2107492 

common_varstbs = sub(".x", "", names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  assessments_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(assessments_violations[, "merge"] == 2, assessments_violations[, paste(common_varstbs[i], ".y", sep = "")], assessments_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
assessments_violations = assessments_violations[, -grep(".y", names(assessments_violations), fixed = T)]
names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)] = common_varstbs

#Clean up violator name fields

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
assessments_violations = assessments_violations[, -grep("merge", names(assessments_violations))]
saveRDS(assessments_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/assessments_violations.rds") #should we name to align w/ STATA version?
rm(clean_violations, clean_assessments)

#Merge in mines data; still need an identifying variable @ mine-level
assessments_violations$minemarker = paste("L", as.character(assessments_violations$mineid), sep = "")
clean_mines$minemarker = paste("L", as.character(clean_mines$mineid), sep = "")
assessments_violations_mines = merge(assessments_violations, clean_mines, by = "mineid", all = T)
assessments_violations_mines[, "minesmerge"] = ifelse(!is.na(assessments_violations_mines$minemarker.y) & !is.na(assessments_violations_mines$minemarker.x), 3, 0)
assessments_violations_mines[, "minesmerge"] = ifelse(is.na(assessments_violations_mines$minemarker.x) & !is.na(assessments_violations_mines$minemarker.y), 2, assessments_violations_mines[, "minesmerge"])
assessments_violations_mines[, "minesmerge"] = ifelse(is.na(assessments_violations_mines$minemarker.y) & !is.na(assessments_violations_mines$minemarker.x), 1, assessments_violations_mines[, "minesmerge"])
table(assessments_violations_mines$minesmerge)
#1       2       3 
#5   39288 4493661
#Open Data Only:
#2       3 
#59710 2161422 

#Drop mines without inspections/assessments and 5 obs from contractor with US DOE at a Waste Isolation Plant in NM (MSHA Data Retrieval System) without mine data (mineid = 2901857). Nikhil 5/1/16
assessments_violations_mines = assessments_violations_mines[assessments_violations_mines$minesmerge == 3 & assessments_violations_mines$coalcormetalm == 1,]
common_varstbs = sub(".x", "", names(assessments_violations_mines)[grep(".x", names(assessments_violations_mines), fixed = T)], fixed = T)
assessments_violations_mines = assessments_violations_mines[, -grep(".y", names(assessments_violations_mines), fixed = T)]
names(assessments_violations_mines)[grep(".x", names(assessments_violations_mines), fixed = T)] = common_varstbs

##Clean violator names code##
assessments_violations_mines$operatorname = tolower(str_trim(assessments_violations_mines$operatorname, side = c("both")))
violnames = c(grep("violatorname", names(assessments_violations_mines)), grep("violator_name", names(assessments_violations_mines)), grep("operatorname", names(assessments_violations_mines)))
for (i in 1:length(violnames)) {
  assessments_violations_mines[, violnames[i]] = gsub(",", "", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("\\.", "", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("company", "co", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("comp", "co", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("incorporated", "inc", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("and", "&", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("limited", "ltd", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("corporation", "corp", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("limited", "ltd", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("services", "srv", assessments_violations_mines[, violnames[i]])
  
  assessments_violations_mines[, violnames[i]] = gsub("llc", "", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("inc", "", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("corp", "", assessments_violations_mines[, violnames[i]])
  assessments_violations_mines[, violnames[i]] = gsub("ltd", "", assessments_violations_mines[, violnames[i]])
}
assessments_violations_mines$violator_name = ifelse(assessments_violations_mines$violatortype == "operator" & !is.na(assessments_violations_mines$operatorname), assessments_violations_mines$operatorname, 
                                                    assessments_violations_mines$violator_name)
names(assessments_violations_mines)[names(assessments_violations_mines) == "violatorname"] = "oldviolatorname"
names(assessments_violations_mines)[names(assessments_violations_mines) == "violator_name"] = "violatorname"

saveRDS(assessments_violations_mines, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/assessments_violations_mines.rds")
rm(clean_mines, assessments_violations)

#Now that data will be partially merged on eventno we do some basic cleaning
clean_Inspections$eventno = ifelse(nchar(clean_Inspections$eventno) < 7, paste("0", clean_Inspections$eventno, sep = ""), clean_Inspections$eventno)
assessments_violations_mines$eventno = ifelse(nchar(assessments_violations_mines$eventno) < 7, paste("0", assessments_violations_mines$eventno, sep = ""), assessments_violations_mines$eventno)

assessments_violations_mines$inspecid = paste("L", assessments_violations_mines$eventno, sep = "")
clean_Inspections$inspecid = paste("L", clean_Inspections$eventno, sep = "")
merged_assessments = merge(assessments_violations_mines, clean_Inspections, by = c("mineid", "eventno"), all = T)
merged_assessments[, "inspecmerge"] = ifelse(!is.na(merged_assessments$inspecid.y) & !is.na(merged_assessments$inspecid.x), 3, 0)
merged_assessments[, "inspecmerge"] = ifelse(is.na(merged_assessments$inspecid.x) & !is.na(merged_assessments$inspecid.y), 2, merged_assessments[, "inspecmerge"])
merged_assessments[, "inspecmerge"] = ifelse(is.na(merged_assessments$inspecid.y) & !is.na(merged_assessments$inspecid.x), 1, merged_assessments[, "inspecmerge"])
table(merged_assessments$inspecmerge)
#Open Data Only: 
#1       2       3 
#71 2037701 1180363

common_varstbs = sub(".x", "", names(merged_assessments)[grep(".x", names(merged_assessments), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_assessments[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_assessments[, "inspecmerge"] == 2, merged_assessments[, paste(common_varstbs[i], ".y", sep = "")], merged_assessments[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_assessments = merged_assessments[, -grep(".y", names(merged_assessments), fixed = T)]
names(merged_assessments)[grep(".x", names(merged_assessments), fixed = T)] = common_varstbs

merged_assessments = merged_assessments[!is.na(merged_assessments$violationno), c(-grep("nooftailingponds", names(merged_assessments)), -grep("timevacated", names(merged_assessments)), 
                                                                                  -grep("minegascategorycode", names(merged_assessments)), -grep("merge", names(merged_assessments)), 
                                                                                  -grep("coalcormetalmmine", names(merged_assessments)), -grep("coalcormetalm", names(merged_assessments)))]

saveRDS(merged_assessments, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_assessments.rds")
rm(assessments_violations_mines, clean_Inspections, common_varstbs, violnames, i)
