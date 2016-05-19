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
common_varstbs = sub(".x", "", names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  assessments_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(assessments_violations[, "merge"] == 2, assessments_violations[, paste(common_varstbs[i], ".y", sep = "")], assessments_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
assessments_violations = assessments_violations[, -grep(".y", names(assessments_violations), fixed = T)]
names(assessments_violations)[grep(".x", names(assessments_violations), fixed = T)] = common_varstbs

assessments_violations[, "violatorname"] = tolower(str_trim(assessments_violations[, "violatorname"], side = c("both")))
assessments_violations[, "violator_name"] = tolower(str_trim(assessments_violations[, "violator_name"], side = c("both")))

violnames = c(grep("violatorname", names(assessments_violations)), grep("violator_name", names(assessments_violations)))

for (i in 1:length(violnames)) {
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], ",", "")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], ".", "")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "Company", "Co")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "Comp", "Co")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "company", "co")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "comp", "co")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "Incorporated", "Inc")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "incorporated", "inc")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "And", "&")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "and", "&")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "Llc", "LLC")
  assessments_violations[, violnames[i]] = str_replace(assessments_violations[, violnames[i]], "Limited", "Ltd")
}

assessments_violations[assessments_violations$violator_name != assessments_violations$violatorname, "violator_name"] = str_replace(assessments_violations[assessments_violations$violator_name != assessments_violations$violatorname, "violator_name"], " LLC", "")
assessments_violations[assessments_violations$violator_name != assessments_violations$violatorname, "violatorname"] = str_replace(assessments_violations[assessments_violations$violator_name != assessments_violations$violatorname, "violatorname"], " LLC", "")

