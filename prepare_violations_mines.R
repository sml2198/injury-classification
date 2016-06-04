##HEADER##

library(plyr)
library(zoo)
library(glarma)

merged_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_assessments.rds")
merged_cfr_key = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_cfr_key.rds")

merged_assessments = merged_assessments[, c(-grep("datevacated", names(merged_assessments)), -grep("primarymill", names(merged_assessments)), -grep("generatedbyassessmt", names(merged_assessments)),
                                            -grep("sigandsubindicator", names(merged_assessments)), -grep("dup", names(merged_assessments)), -grep("source", names(merged_assessments)),
                                            -grep("merge", names(merged_assessments)), -grep("samples", names(merged_assessments)), -grep("sic", names(merged_assessments)), 
                                            -grep("(latitude|longitude|idate|idesc)", names(merged_assessments)))]

merged_assessments$cfrstandardcode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", merged_assessments$cfrstandardcode)
merged_assessments$cfrstandardcode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "", merged_assessments$cfrstandardcode)
names(merged_assessments)[names(merged_assessments) == "cfrstandardcode"] = "subsection_code"

merged_assessments$subsection_code_marker = paste("S", merged_assessments$subsection_code, sep = "")
merged_cfr_key$subsection_code_marker = paste("S", merged_cfr_key$subsection_code, sep = "")
merged_assessments_cfrkey = merge(merged_assessments, merged_cfr_key, by = "subsection_code", all = T)
merged_assessments_cfrkey[, "merge"] = ifelse(!is.na(merged_assessments_cfrkey$subsection_code_marker.y) & !is.na(merged_assessments_cfrkey$subsection_code_marker.x), 3, 0)
merged_assessments_cfrkey[, "merge"] = ifelse(is.na(merged_assessments_cfrkey$subsection_code_marker.x) & !is.na(merged_assessments_cfrkey$subsection_code_marker.y), 2, merged_assessments_cfrkey[, "merge"])
merged_assessments_cfrkey[, "merge"] = ifelse(is.na(merged_assessments_cfrkey$subsection_code_marker.y) & !is.na(merged_assessments_cfrkey$subsection_code_marker.x), 1, merged_assessments_cfrkey[, "merge"])
table(merged_assessments_cfrkey$merge) 
#Open Data Only:
#1       2       3 
#41047    1188 1139387 

common_varstbs = sub(".x", "", names(merged_assessments_cfrkey)[grep(".x", names(merged_assessments_cfrkey), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_assessments_cfrkey[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_assessments_cfrkey[, "merge"] == 2, merged_assessments_cfrkey[, paste(common_varstbs[i], ".y", sep = "")], merged_assessments_cfrkey[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_assessments_cfrkey = merged_assessments_cfrkey[, -grep(".y", names(merged_assessments_cfrkey), fixed = T)]
names(merged_assessments_cfrkey)[grep(".x", names(merged_assessments_cfrkey), fixed = T)] = common_varstbs
rm(merged_assessments, merged_cfr_key, common_varstbs, i)

datevars = names(merged_assessments_cfrkey)[grep("date", names(merged_assessments_cfrkey))]
for (i in 1:length(datevars)) {
  merged_assessments_cfrkey[, datevars[i]] = as.Date(as.character(merged_assessments_cfrkey[, datevars[i]]), "%m/%d/%Y")
}

#Create variables for mine-quarter level prediction dataset
merged_assessments_cfrkey$quarter = as.yearqtr(merged_assessments_cfrkey$dateissued)

merged_assessments_cfrkey$contractor_violation_cnt = ifelse(merged_assessments_cfrkey$violatortypecode == 1, merged_assessments_cfrkey$violator_violation_cnt, NA)
merged_assessments_cfrkey$operator_violation_cnt = ifelse(merged_assessments_cfrkey$violatortypecode == 2, merged_assessments_cfrkey$violator_violation_cnt/merged_assessments_cfrkey$violator_inspection_day_cnt, NA)
merged_assessments_cfrkey$contractor_repeated_viol_cnt = ifelse(merged_assessments_cfrkey$violatortypecode == 1, merged_assessments_cfrkey$violator_repeated_viol_cnt, NA)
merged_assessments_cfrkey$operator_repeated_viol_cnt = ifelse(merged_assessments_cfrkey$violatortypecode == 2, merged_assessments_cfrkey$violator_repeated_viol_cnt/merged_assessments_cfrkey$violator_inspection_day_cnt, NA)

##VIOLATIONS & ASSESSMENTS##

#Dummy out CFR codes (at the subpart and subsection levels) only for *relevant types and mark all non-relevant CFR codes

MR_relevant_subsectcodes = levels(factor(merged_assessments_cfrkey[merged_assessments_cfrkey$MR_relevant == 1 | merged_assessments_cfrkey$MR_maybe_relevant == 1,]$subsection_code))
#For preliminary testing only. Comment out when done. 6/3/2016
MR_relevant_subsectcodes = c("47.41")
for (i in 1:length(MR_relevant_subsectcodes)) {
  merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]] = ifelse(merged_assessments_cfrkey$subsection_code == MR_relevant_subsectcodes[i], 1, 0)
  merged_assessments_cfrkey[, paste("penaltypoints", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "penaltypoints"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  #There is also a factor var likelihood which marks the severity of negligence e.g., reasonably, unlikely, ...
  merged_assessments_cfrkey[, paste("gravitylikelihoodpoints", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "gravitylikelihoodpoints"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("gravityinjurypoints", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "gravityinjurypoints"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("gravitypersonspoints", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "gravitypersonspoints"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  #There is also a factor var negligence which marks the severity of negligence e.g., low, high, ...
  merged_assessments_cfrkey[, paste("negligencepoints", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "negligencepoints"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("sigandsubdesignation", MR_relevant_subsectcodes[i], sep = "_")] = ifelse(merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]] == 1, merged_assessments_cfrkey[, "sigandsubdesignation"], 0)
  merged_assessments_cfrkey[, paste("contractor_violation_cnt", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "contractor_violation_cnt"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("operator_violation_cnt", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "operator_violation_cnt"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("contractor_repeated_viol_cnt", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "contractor_repeated_viol_cnt"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("operator_repeated_viol_cnt", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "operator_repeated_viol_cnt"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
}

#Create CFR-code (only for *relevant) specific for penalty variables:
#penalty pts, good faith, gravity and its components as well & sig. and sub., negligence, # of repeat violations for operators and ind. contractors,
## of overall violations for operators and ind. contractors, create our own previous violations var and compare to the given version for verification,
#(NOT by *relevant CFR-code) appropriateness: size of mine in tonnage, size of controlling entity in tonnage, size of indep. contractor in annual hrs. worked

#Testing aggregation to mine-quarter level

summed_assessments_cfrkey = ddply(merged_assessments_cfrkey[, c(grep("_*[0-9][0-9]\\..+", names(merged_assessments_cfrkey)), grep("minesizepoints", names(merged_assessments_cfrkey)),
                                             grep("controllersizepoints", names(merged_assessments_cfrkey)), grep("contractorsizepoints", names(merged_assessments_cfrkey)),
                                       match("mineid", names(merged_assessments_cfrkey)), match("quarter", names(merged_assessments_cfrkey)))], c("mineid", "quarter"), 
               function(x) colSums(x[, c(grep("_*[0-9][0-9]\\..+", names(x)), grep("minesizepoints", names(x)),
                                        grep("controllersizepoints", names(x)), grep("contractorsizepoints", names(x)))], na.rm = T))

#Question: Check if operator variables vary by mine? Are indep. contractors the only operators @ a mine or are they only a part of the operation?

##INSPECTIONS##

#Hours, # of regular inspections, # of special inspections (work out issue of over-counting inspection hours over violations data)

