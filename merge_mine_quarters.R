##HEADER##

library(plyr)
library(zoo)

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
relevancevars = names(merge_mine_quarters)[grep("_relevant", names(merge_mine_quarters))]
for (i in 1:length(relevancevars)) {
  merge_mine_quarters[, paste(relevancevars[i], "_viols", sep = "")] = ifelse(merge_mine_quarters[, relevancevars[i]] == 1, 1, 0)
}
temp = aggregate.data.frame(merge_mine_quarters[, c(grep("_viols", names(merge_mine_quarters)), grep("total_violations", names(merge_mine_quarters)))], list(merge_mine_quarters$mineid), sum)
names(temp)[names(temp) == "Group.1"] = "mineid"
merge_mine_quarters = merge_mine_quarters[, c(-grep("total_violations", names(merge_mine_quarters)), -grep("_viols", names(merge_mine_quarters)))]
merge_mine_quarters = merge(merge_mine_quarters, temp, by = "mineid", all = T)
rm(temp)
merge_mine_quarters$inspect_length = merge_mine_quarters$endingdate - merge_mine_quarters$beginningdate
merge_mine_quarters$maint_qtrly_hrs = merge_mine_quarters$numberofemployees * merge_mine_quarters$maintenanceshiftsperday * merge_mine_quarters$hourspershift * merge_mine_quarters$daysperweek * 13
merge_mine_quarters$prod_qtrly_hrs = merge_mine_quarters$numberofemployees * merge_mine_quarters$productionshiftsperday * merge_mine_quarters$hourspershift * merge_mine_quarters$daysperweek * 13

summed = ddply(merge_mine_quarters[, c(grep("_viols", names(merge_mine_quarters)), grep("total_violations", names(merge_mine_quarters)), 
                                       grep("mineid", names(merge_mine_quarters)), match("quarter", names(merge_mine_quarters)))], c("mineid", "quarter"), 
                                       function(x) sapply(x[, c(grep("_viols", names(x)), grep("total_violations", names(x)))], sum))
averaged = ddply(merge_mine_quarters[, c(grep("numberofemployees", names(merge_mine_quarters)), grep("proposed_penalty", names(merge_mine_quarters)),
                                         grep("total_inspections", names(merge_mine_quarters)), grep("inspect_length", names(merge_mine_quarters)),
                                         grep("maint_qtrly_hrs", names(merge_mine_quarters)), grep("prod_qtrly_hrs", names(merge_mine_quarters)),
                                         grep("mineid", names(merge_mine_quarters)), match("quarter", names(merge_mine_quarters)))], c("mineid", "quarter"), 
                                         function(x) sapply(x[, c(grep("numberofemployees", names(x)), grep("proposed_penalty", names(x)),
                                                                  grep("total_inspections", names(x)), grep("inspect_length", names(x)),
                                                                  grep("maint_qtrly_hrs", names(x)), grep("prod_qtrly_hrs", names(x)))], mean))

merge_mine_quarters = merge(summed, averaged, by = c("mineid", "quarter"), all = T)

saveRDS(merge_mine_quarters, file = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_mine_quarters.rds")

