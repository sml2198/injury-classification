#HEADER

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

common_varstbs = sub(".x", "", names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  assessments_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(assessments_violations[, "merge"] == 2, assessments_violations[, paste(common_varstbs[i], ".y", sep = "")], assessments_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
assessments_violations = assessments_violations[, -grep(".y", names(assessments_violations), fixed = T)]
names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)] = common_varstbs

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
sum(assessments_violations$occurrencedate != assessments_violations$violation_occur_dt, na.rm = T) #214
assessments_violations = assessments_violations[(assessments_violations$occurrencedate == assessments_violations$violation_occur_dt) | is.na(assessments_violations$occurrencedate) | is.na(assessments_violations$violation_occur_dt),]
assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt),]$occurrencedate = assessments_violations[is.na(assessments_violations$occurrencedate) & !is.na(assessments_violations$violation_occur_dt),]$violation_occur_dt

