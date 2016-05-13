library(stringr)

clean_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_assessments.rds") #Note: Carolyn data doesn't have eventno!
clean_violations = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_violations.rds")
clean_mines = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds")
clean_Inspections = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_Inspections.rds")

#is beginningdate defined for all rows in each?
assessments_violations = merge(clean_assessments, clean_violations, by = "violationno", all = T)
assessments_violations[, "merge"] = ifelse(!is.na(assessments_violations$beginningdate.y) & !is.na(assessments_violations$beginningdate.x), 3, 0)
assessments_violations[, "merge"] = ifelse(is.na(assessments_violations$beginningdate.x) & !is.na(assessments_violations$beginningdate.y), 2, assessments_violations[, "merge"])
assessments_violations[, "merge"] = ifelse(is.na(assessments_violations$beginningdate.y) & !is.na(assessments_violations$beginningdate.x), 1, assessments_violations[, "merge"])
table(assessments_violations$merge)

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

