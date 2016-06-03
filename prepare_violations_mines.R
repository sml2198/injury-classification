##HEADER##

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

##VIOLATIONS & ASSESSMENTS##

#Dummy out CFR codes (at the subpart and subsection levels) only for *relevant types and mark all non-relevant CFR codes

MR_relevant_subsectcodes = levels(factor(merged_assessments_cfrkey[merged_assessments_cfrkey$MR_relevant == 1 | merged_assessments_cfrkey$MR_maybe_relevant == 1,]$subsection_code))

for (i in 1:length(MR_relevant_subsectcodes)) {
  merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]] = ifelse(merged_assessments_cfrkey$subsection_code == MR_relevant_subsectcodes[i], 1, 0)
}

#Create CFR-code (only for *relevant) specific for penalty variables:
#penalty pts, good faith, gravity and its components as well & sig. and sub., negligence, # of repeat violations for operators and ind. contractors,
## of overall violations for operators and ind. contractors, create our own previous violations var and compare to the given version for verification,
#(NOT by *relevant CFR-code) appropriateness: size of mine in tonnage, size of controlling entity in tonnage, size of indep. contractor in annual hrs. worked

#Question: Check if operator variables vary by mine? Are indep. contractors the only operators @ a mine or are they only a part of the operation?

##INSPECTIONS##

#Hours, # of regular inspections, # of special inspections (work out issue of over-counting inspection hours over violations data)

