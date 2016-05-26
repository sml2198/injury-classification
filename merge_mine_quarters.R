##HEADER##

merged_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_assessments.rds")
merged_cfr_key = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_cfr_key.rds")

merged_assessments = merged_assessments[, c(-grep("datevacated", names(merged_assessments)), -grep("primarymill", names(merged_assessments)), -grep("generatedbyassessmt", names(merged_assessments)),
                                            -grep("sigandsubindicator", names(merged_assessments)), -grep("dup", names(merged_assessments)), -grep("source", names(merged_assessments)),
                                            -grep("merge", names(merged_assessments)))]

merged_assessments$cfrstandardcode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", merged_assessments$cfrstandardcode)
merged_assessments$cfrstandardcode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "", merged_assessments$cfrstandardcode)
names(merged_assessments)[names(merged_assessments) == "cfrstandardcode"] = "subsection_code"

merged_assessments$subsection_code_marker = paste("S", merged_assessments$subsection_code, sep = "")
merged_cfr_key$subsection_code_marker = paste("S", merged_cfr_key$subsection_code, sep = "")
merge_mine_quarters = merge(merged_assessments, merged_cfr_key, by = "subsection_code", all = T)
merge_mine_quarters[, "merge"] = ifelse(!is.na(merge_mine_quarters$subsection_code_marker.y) & !is.na(merge_mine_quarters$subsection_code_marker.x), 3, 0)
merge_mine_quarters[, "merge"] = ifelse(is.na(merge_mine_quarters$subsection_code_marker.x) & !is.na(merge_mine_quarters$subsection_code_marker.y), 2, merge_mine_quarters[, "merge"])
merge_mine_quarters[, "merge"] = ifelse(is.na(merge_mine_quarters$subsection_code_marker.y) & !is.na(merge_mine_quarters$subsection_code_marker.x), 1, merge_mine_quarters[, "merge"])
table(merge_mine_quarters$merge) 
#Open Data Only:
#1       2       3 
#41047    1188 1139387 

common_varstbs = sub(".x", "", names(merge_mine_quarters)[grep(".x", names(merge_mine_quarters), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merge_mine_quarters[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merge_mine_quarters[, "merge"] == 2, merge_mine_quarters[, paste(common_varstbs[i], ".y", sep = "")], merge_mine_quarters[, paste(common_varstbs[i], ".x", sep = "")])
}
merge_mine_quarters = merge_mine_quarters[, -grep(".y", names(merge_mine_quarters), fixed = T)]
names(merge_mine_quarters)[grep(".x", names(merge_mine_quarters), fixed = T)] = common_varstbs

datevars = names(merge_mine_quarters)[grep("date", names(merge_mine_quarters))]
for (i in 1:length(datevars)) {
  merge_mine_quarters[, datevars[i]] = as.Date(as.character(merge_mine_quarters[, datevars[i]]), "%m/%d/%Y")
}

#Create variables for mine-quarter level prediction dataset

merge_mine_quarters$quarter = as.yearqtr(merge_mine_quarters$dateissued)
merge_mine_quarters$total_violations = 1
temp = aggregate.data.frame(merge_mine_quarters$total_violations, list(merge_mine_quarters$mineid), sum)
names(temp)[names(temp) == "Group.1"] = "mineid"
names(temp)[names(temp) == "x"] = "total_violations"
merge_mine_quarters = merge_mine_quarters[, -grep("total_violations", names(merge_mine_quarters))]
merge_mine_quarters = merge(merge_mine_quarters, temp, by = "mineid", all = T)

##But how to create category-specific total_viols?

merge_mine_quarters$inspect_length = merge_mine_quarters$endingdate - merge_mine_quarters$beginningdate
merge_mine_quarters$maint_qtrly_hrs = merge_mine_quarters$numberofemployees * merge_mine_quarters$maintenanceshiftsperday * merge_mine_quarters$hourspershift * merge_mine_quarters$daysperweek * 13
merge_mine_quarters$prod_qtrly_hrs = merge_mine_quarters$numberofemployees * merge_mine_quarters$productionshiftsperday * merge_mine_quarters$hourspershift * merge_mine_quarters$daysperweek * 13

##But how to aggregate using different FUNs at once?
