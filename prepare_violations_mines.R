##HEADER##

library(plyr)
library(zoo)
library(glarma)

merged_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_assessments.rds")
merged_cfr_key = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_cfr_key.rds")
mines.accidents.coded = read.csv("X:/Projects/Mining/NIOSH/analysis/data/4_coded/accidents_with_predictions.csv")

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

#Condition the per-day vars on positive denominator. (There are 256 cases of zero inspection days and positive violation counts). 6/6/16
merged_assessments_cfrkey$contractor_violation_cnt = ifelse(merged_assessments_cfrkey$violatortypecode == 1, merged_assessments_cfrkey$violator_violation_cnt, NA)
merged_assessments_cfrkey$operator_violation_pInspDay = ifelse(merged_assessments_cfrkey$violatortypecode == 2 & merged_assessments_cfrkey$violator_inspection_day_cnt > 0, merged_assessments_cfrkey$violator_violation_cnt/merged_assessments_cfrkey$violator_inspection_day_cnt, NA)
merged_assessments_cfrkey$contractor_repeated_viol_cnt = ifelse(merged_assessments_cfrkey$violatortypecode == 1, merged_assessments_cfrkey$violator_repeated_viol_cnt, NA)
merged_assessments_cfrkey$operator_repeated_viol_pInspDay = ifelse(merged_assessments_cfrkey$violatortypecode == 2 & merged_assessments_cfrkey$violator_inspection_day_cnt > 0, merged_assessments_cfrkey$violator_repeated_viol_cnt/merged_assessments_cfrkey$violator_inspection_day_cnt, NA)

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
  merged_assessments_cfrkey[, paste("operator_violation_pInspDay", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "operator_violation_pInspDay"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("contractor_repeated_viol_cnt", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "contractor_repeated_viol_cnt"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
  merged_assessments_cfrkey[, paste("operator_repeated_viol_pInspDay", MR_relevant_subsectcodes[i], sep = "_")] = apply(cbind(merged_assessments_cfrkey[, "operator_repeated_viol_pInspDay"], merged_assessments_cfrkey[, MR_relevant_subsectcodes[i]]), 1, prod)
}

#Create CFR-code (only for *relevant) specific for penalty variables:
#penalty pts, good faith, gravity and its components as well & sig. and sub., negligence, # of repeat violations for operators and ind. contractors,
## of overall violations for operators and ind. contractors, create our own previous violations var and compare to the given version for verification,
#(NOT by *relevant CFR-code) appropriateness: size of mine in tonnage, size of controlling entity in tonnage, size of indep. contractor in annual hrs. worked

#Testing aggregation to mine-quarter level
assessments_to_sum = merged_assessments_cfrkey[, c(grep("_*[0-9][0-9]\\..+", names(merged_assessments_cfrkey)), 
                                                   match("mineid", names(merged_assessments_cfrkey)), match("quarter", names(merged_assessments_cfrkey)))]
assessments_to_sum = assessments_to_sum[, c(-grep("operator_repeated_viol_pInspDay", names(assessments_to_sum)), -grep("contractor_repeated_viol_cnt", names(assessments_to_sum)))]
summed_assessments_cfrkey = ddply(assessments_to_sum, c("mineid", "quarter"), function(x) colSums(x[, c(grep("_*[0-9][0-9]\\..+", names(x)))], na.rm = T))

averaged_assessments_cfrkey = ddply(merged_assessments_cfrkey[, c(grep("operator_repeated_viol_pInspDay", names(merged_assessments_cfrkey)), grep("minesizepoints", names(merged_assessments_cfrkey)), grep("controllersizepoints", names(merged_assessments_cfrkey)),
                                                                  grep("contractorsizepoints", names(merged_assessments_cfrkey)), grep("contractor_repeated_viol_cnt", names(merged_assessments_cfrkey)),
                                                                  match("mineid", names(merged_assessments_cfrkey)), match("quarter", names(merged_assessments_cfrkey)))], c("mineid", "quarter"), 
                                    function(x) colMeans(x[, c(grep("operator_repeated_viol_pInspDay", names(x)), grep("minesizepoints", names(x)), grep("controllersizepoints", names(x)),
                                                               grep("contractorsizepoints", names(x)), grep("contractor_repeated_viol_cnt", names(x)))], na.rm = T))
rm(assessments_to_sum)
#Question: Check if operator variables vary by mine? Are indep. contractors the only operators @ a mine or are they only a part of the operation? A: Inspections generate
#both contractor and operator violations. 6/6/16

##INSPECTIONS##

#Hours, # of regular inspections, # of special inspections (work out issue of over-counting inspection hours over violations data)
merged_assessments_cfrkey$inspacty = tolower(merged_assessments_cfrkey$inspacty)
merged_assessments_cfrkey[, "inspacty"] = ifelse(merged_assessments_cfrkey[, "inspacty"] == "mine idle activity", "mine idle", merged_assessments_cfrkey[, "inspacty"])
merged_assessments_cfrkey[, "inspacty"] = ifelse(merged_assessments_cfrkey[, "inspacty"] == "na", "n", merged_assessments_cfrkey[, "inspacty"])
merged_assessments_cfrkey[, "inspacty"] = ifelse(merged_assessments_cfrkey[, "inspacty"] == "part 50 audit", "part 50 audits", merged_assessments_cfrkey[, "inspacty"])
merged_assessments_cfrkey[, "inspacty"] = ifelse(merged_assessments_cfrkey[, "inspacty"] == "resp dust tech insp - surface mines", "resp dust tech insp - u. g. mines", merged_assessments_cfrkey[, "inspacty"])
merged_assessments_cfrkey[, "inspacty"] = ifelse(merged_assessments_cfrkey[, "inspacty"] == "roof control technical investigation", "roof control technical investigtion", merged_assessments_cfrkey[, "inspacty"])
merged_assessments_cfrkey[, "inspacty"] = ifelse(merged_assessments_cfrkey[, "inspacty"] == "non-fatal accident investigation", "nonfatal injury accident inv", merged_assessments_cfrkey[, "inspacty"])
merged_assessments_cfrkey[, "inspacty"] = ifelse(merged_assessments_cfrkey[, "inspacty"] == "shaft, slope or major construction spot inspection", "shft, slpe, or maj constr spot insp", merged_assessments_cfrkey[, "inspacty"])

multi_qtr_inspcs = ddply(merged_assessments_cfrkey[, c(grep("sumtotal_insp_hours", names(merged_assessments_cfrkey)), grep("sumtotal_on_site_hours", names(merged_assessments_cfrkey)), grep("eventno", names(merged_assessments_cfrkey)),
                                                       match("mineid", names(merged_assessments_cfrkey)), match("quarter", names(merged_assessments_cfrkey)))], c("mineid", "quarter", "eventno"), 
                         function(x) colSums(x[, c(grep("sumtotal_insp_hours", names(x)), grep("sumtotal_on_site_hours", names(x)))], na.rm = T))
multi_qtr_inspcs = multi_qtr_inspcs[duplicated(multi_qtr_inspcs$eventno) == F,]
multi_qtr_inspcs$totalinspecs = 1
summed_inspcs = ddply(multi_qtr_inspcs[, c(grep("sumtotal_insp_hours", names(multi_qtr_inspcs)), grep("sumtotal_on_site_hours", names(multi_qtr_inspcs)),
                                           match("totalinspecs", names(multi_qtr_inspcs)), match("mineid", names(multi_qtr_inspcs)), match("quarter", names(multi_qtr_inspcs)))], c("mineid", "quarter"), 
                         function(x) colSums(x[, c(grep("sumtotal_insp_hours", names(x)), grep("sumtotal_on_site_hours", names(x)), match("totalinspecs", names(x)))], na.rm = T))

mines.accidents.coded$accidentdate = as.Date(as.character(mines.accidents.coded$accidentdate), "%m/%d/%Y")
mines.accidents.coded$quarter = as.yearqtr(mines.accidents.coded$accidentdate)
mines.accidents.coded$totalinjuries = 1

#Collapse mines_accidents data here.
summed_coded_accidents = ddply(mines.accidents.coded[, c(grep("totalinjuries", names(mines.accidents.coded)), grep("MR", names(mines.accidents.coded)),
                                                                match("mineid", names(mines.accidents.coded)), match("quarter", names(mines.accidents.coded)))], c("mineid", "quarter"), 
                                  function(x) colSums(x[, c(grep("totalinjuries", names(x)), grep("MR", names(x)))], na.rm = T))

mines_assessments_accidents = merge(summed_assessments_cfrkey, summed_coded_accidents, by = c("mineid", "quarter"), all = T)
prediction_data = merge(mines_assessments_accidents, summed_inspcs, by = c("mineid", "quarter"), all = T)

#To provide an intercept for the prediction stage:
prediction_data$constant = 1
#UNDER CONSTRUCTION
test_pred = glarma(as.vector(prediction_data$MR), as.matrix(prediction_data[, -grep("MR", names(prediction_data))]), type = "NegBin", 
                   phiLags = c(1, 2), thetaLags = c(1, 2), )
