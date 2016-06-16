##NIOSH STUDY##
##Professor Alison Morantz##
##Stanford Law School##

#Coded by Nikhil Saifullah and Sarah Levine

#Description

#This file merges our merged violations data from "merge_violations_mines.R" with our prepared CFR subsection codes from "prepare_cfr_key.R",
#accidents data, and employment/production data. We then collapse the merged data to the mine-quarter level for use in the prediction stage
#and run some candidate models.

library(plyr)
library(zoo)
library(glarma)
library(pglm)
library(stats)
library(stringr)
library(withr)

merged_violations = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds")
merged_cfr_key = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_cfr_key.rds")
mines_quarters = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds")
mine_types = readRDS("X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds")
mines_accidents_coded = read.csv("X:/Projects/Mining/NIOSH/analysis/data/4_coded/accidents_with_predictions.csv")

######################################################################################################################################
# MERGE CFR CODES ONTO VIOLATIONS AND MAKE VARIABLES FOR COLLAPSING ON

merged_violations = merged_violations[, c(-grep("datevacated", names(merged_violations)), -grep("primarymill", names(merged_violations)), -grep("generatedbyassessmt", names(merged_violations)),
                                            -grep("sigandsubindicator", names(merged_violations)), -grep("dup", names(merged_violations)), -grep("source", names(merged_violations)),
                                            -grep("merge", names(merged_violations)), -grep("samples", names(merged_violations)), -grep("sic", names(merged_violations)), 
                                            -grep("(latitude|longitude|idate|idesc)", names(merged_violations)))]

merged_violations$cfrstandardcode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", merged_violations$cfrstandardcode)
merged_violations$cfrstandardcode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "", merged_violations$cfrstandardcode)
names(merged_violations)[names(merged_violations) == "cfrstandardcode"] = "subsection_code"

merged_violations$subsection_code_marker = paste("S", merged_violations$subsection_code, sep = "")
merged_cfr_key$subsection_code_marker = paste("S", merged_cfr_key$subsection_code, sep = "")

# in some cases where subsection is missing, part_section is not and can be subbed in.
merged_violations$part_section2 = merged_violations$part_section
merged_violations$part_section2 = gsub("\\([a-z]+\\)", "", merged_violations$part_section2)
merged_violations$part_section2 = gsub("\\([0-9]+\\)", "", merged_violations$part_section2)
merged_violations$subsection_code = ifelse((is.na(merged_violations$subsection_code) & !is.na(merged_violations$part_section2)), merged_violations$part_section2, merged_violations$subsection_code)

merged_violations = merge(merged_violations, merged_cfr_key, by = "subsection_code", all = T)
#unmerged_violations = merged_violations[(merged_violations$subsection_code == ""),]
#write.csv(unmerged_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/unmerged_violations.csv", row.names = TRUE)

merged_violations[, "merge"] = ifelse(!is.na(merged_violations$subsection_code_marker.y) & !is.na(merged_violations$subsection_code_marker.x), 3, 0)
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.x) & !is.na(merged_violations$subsection_code_marker.y), 2, merged_violations[, "merge"])
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.y) & !is.na(merged_violations$subsection_code_marker.x), 1, merged_violations[, "merge"])
table(merged_violations$merge) 
#Open Data Only:
#1       2       3 
#41047    1188 1139387 

common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "merge"] == 2, merged_violations[, paste(common_varstbs[i], ".y", sep = "")], merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = T)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = T)] = common_varstbs
rm(merged_cfr_key, common_varstbs, i)

datevars = names(merged_violations)[grep("date", names(merged_violations))]
for (i in 1:length(datevars)) {
  merged_violations[, datevars[i]] = as.Date(as.character(merged_violations[, datevars[i]]), "%m/%d/%Y")
}

# Create variables for mine-quarter level prediction dataset
merged_violations$quarter = as.yearqtr(merged_violations$dateissued)

# Condition the per-day vars on positive denominator. (There are 256 cases of zero inspection days and positive violation counts). 6/6/16
merged_violations$contractor_violation_cnt = ifelse(merged_violations$violatortypecode == 1, merged_violations$violator_violation_cnt, NA)
merged_violations$operator_violation_pInspDay = ifelse(merged_violations$violatortypecode == 2 & merged_violations$violator_inspection_day_cnt > 0, merged_violations$violator_violation_cnt/merged_violations$violator_inspection_day_cnt, NA)
merged_violations$contractor_repeated_viol_cnt = ifelse(merged_violations$violatortypecode == 1, merged_violations$violator_repeated_viol_cnt, NA)
merged_violations$operator_repeated_viol_pInspDay = ifelse(merged_violations$violatortypecode == 2 & merged_violations$violator_inspection_day_cnt > 0, merged_violations$violator_repeated_viol_cnt/merged_violations$violator_inspection_day_cnt, NA)

# remove observations from cfr data that didn't merge onto our violations data 
merged_violations = merged_violations[complete.cases(merged_violations$violationno),]
# remove violations that we have no dateissued and therefore no quarter data (only 44 observations)
merged_violations = merged_violations[complete.cases(merged_violations$quarter),]
# drop observations before our study period (only 27 observations)
merged_violations = merged_violations[(merged_violations$quarter > "1999 Q4"),]

######################################################################################################################################
#DUMMY OUT FACTOR VARIABLES

# FIRST - CLEAN UP THE FIELD THAT REPORTS THE TYPE OF INSPECTION
merged_violations$inspacty = tolower(merged_violations$inspacty)
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "mine idle activity", "mine idle", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "na", "n", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "part 50 audit", "part 50 audits", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "non-fatal accident investigation", "nonfatal injury accident inv", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "shaft, slope or major construction spot inspection", "shft, slpe, or maj constr spot insp", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "103(g)(1) spot umwa inspection", "103(g)(1) spot inspection", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "103(i) spot (ign or expl) insp", "103(i) spot inspections", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "103(i) spot (hazard) inspection", "103(i) spot inspections", merged_violations[, "inspacty"])
# can't have any missing values or our dummy method won't work
merged_violations[, "inspacty"] = ifelse(is.na(merged_violations$inspacty), "unknown", merged_violations[, "inspacty"])
merged_violations[, "violationtypecode"] = ifelse(is.na(merged_violations$violationtypecode), "unknown", merged_violations[, "violationtypecode"])
# these make up a grand total of 4 in our observations - not worth making dummies for 4 violation types
merged_violations = merged_violations[(merged_violations$violationtypecode != "Notice" & merged_violations$violationtypecode != "Safeguard"),]
merged_violations[, "assessmenttypecode"] = ifelse(is.na(merged_violations$assessmenttypecode), "unknown", merged_violations[, "assessmenttypecode"])

datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}

test.data1 <- datdum(x="inspacty",data=merged_violations,name="inspacty")
test.data1 <- test.data1 [, c(grep("inspacty", names(test.data1)))]
test.data2 <- datdum(x="assessmenttypecode",data=merged_violations,name="assessmenttypecode")
test.data2 <- test.data2 [, c(grep("assessmenttypecode", names(test.data2)))]
test.data3 <- datdum(x="violationtypecode",data=merged_violations,name="violationtypecode")
test.data3 <- test.data3 [, c(grep("violationtypecode", names(test.data3)))]

merged_violations = cbind(merged_violations, test.data1, test.data2, test.data3)
rm(test.data1, test.data2, test.data3)

######################################################################################################################################
# CLEAN & COLLAPSE ASSESSMENTS/VIOLATIONS

#Dummy out CFR codes (at the subpart and subsection levels) only for *relevant types and mark all non-relevant CFR codes

MR_relevant_subsectcodes = levels(factor(merged_violations[merged_violations$MR_relevant == 1 | merged_violations$MR_maybe_relevant == 1,]$subsection_code))
#For preliminary testing only. Comment out when done. 6/3/2016
MR_relevant_subsectcodes = c("47.41", "77.404")
violationtypecodes = c("1", "2", "3", "4", "5")
assessmenttypecodes = c("1", "2", "3", "4", "5")
inspactycodes = seq(1, 56)
#For CFR part-specific variable creation
MR_relevant_partcodes = levels(factor(merged_violations[merged_violations$MR_relevant == 1 | merged_violations$MR_maybe_relevant == 1,]$cfr_part_code))

#Sets the level of CFR code for which to create code-specific variables
cfr_codes = MR_relevant_partcodes
for (i in 1:length(cfr_codes)) {
  merged_violations[, cfr_codes[i]] = ifelse(merged_violations$subsection_code == cfr_codes[i], 1, 0)
  merged_violations[, paste(cfr_codes[i], "penaltypoints", sep = ".")] = apply(cbind(merged_violations[, "penaltypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  #There is also a factor var likelihood which marks the severity of negligence e.g., reasonably, unlikely, ...
  merged_violations[, paste(cfr_codes[i], "gravitylikelihoodpoints", sep = ".")] = apply(cbind(merged_violations[, "gravitylikelihoodpoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "gravityinjurypoints", sep = ".")] = apply(cbind(merged_violations[, "gravityinjurypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "gravitypersonspoints", sep = ".")] = apply(cbind(merged_violations[, "gravitypersonspoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  #There is also a factor var negligence which marks the severity of negligence e.g., low, high, ...
  merged_violations[, paste(cfr_codes[i], "negligencepoints", sep = ".")] = apply(cbind(merged_violations[, "negligencepoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "sigandsubdesignation", sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1, merged_violations[, "sigandsubdesignation"], 0)
  merged_violations[, paste(cfr_codes[i], "contractor_violation_cnt", sep = ".")] = apply(cbind(merged_violations[, "contractor_violation_cnt"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "operator_violation_pInspDay", sep = ".")] = apply(cbind(merged_violations[, "operator_violation_pInspDay"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "contractor_repeated_viol_cnt", sep = ".")] = apply(cbind(merged_violations[, "contractor_repeated_viol_cnt"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "operator_repeated_viol_pInspDay", sep = ".")] = apply(cbind(merged_violations[, "operator_repeated_viol_pInspDay"], merged_violations[, cfr_codes[i]]), 1, prod)
  # dummied out categorical vars
  for (j in 1:length(violationtypecodes)) {
    merged_violations[, paste(cfr_codes[i], "violationtypecode", violationtypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("violationtypecode", violationtypecodes[j], sep = ".")] == 1, 1, 0)
  }
  for (j in 1:length(assessmenttypecodes)) {
    merged_violations[, paste(cfr_codes[i], "assessmenttypecode", assessmenttypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("assessmenttypecode", assessmenttypecodes[j], sep = ".")] == 1, 1, 0)
  }
  for (j in 1:length(inspactycodes)) {
    merged_violations[, paste(cfr_codes[i], "inspacty", inspactycodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("inspacty", inspactycodes[j], sep = ".")] == 1, 1, 0)
  }
}

######################################################################################################################################
#Create CFR-code (only for *relevant) specific for penalty variables:
#penalty pts, good faith, gravity and its components as well & sig. and sub., negligence, # of repeat violations for operators and ind. contractors,
## of overall violations for operators and ind. contractors, create our own previous violations var and compare to the given version for verification,
#(NOT by *relevant CFR-code) appropriateness: size of mine in tonnage, size of controlling entity in tonnage, size of indep. contractor in annual hrs. worked

#Aggregation to mine-quarter level - create variable to sum for violation #, and for if terminated
merged_violations$total_violations = 1
merged_violations$terminated = ifelse(merged_violations$typeoftermination == "Terminated", 1, 0)

# select all variables to sum when we collapse to the mine-quarter lever (the first regex will grab all vars created above)
# add indicator for if a mine q was terminated because of a violation (sum this for now - maybe we'll just make it an indicator later)
violations_to_sum = merged_violations[, c(grep("^[0-9][0-9]", names(merged_violations)), 
                                          match("total_violations", names(merged_violations)), 
                                          match("terminated", names(merged_violations)),
                                          match("mineid", names(merged_violations)), match("quarter", names(merged_violations)))]

# these were grabbed by the regular expression above, but we want to average (not sum) these, so we remove them  
violations_to_sum = violations_to_sum[, c(-grep("operator_repeated_viol_pInspDay", names(violations_to_sum)), -grep("contractor_repeated_viol_cnt", names(violations_to_sum)))]
summed_violations = ddply(violations_to_sum, c("mineid", "quarter"), function(x) colSums(x[, c(grep("^[0-9][0-9]", names(x)), 
                                                                                               match("total_violations", names(x)),
                                                                                               match("terminated", names(x)))], na.rm = T))

averaged_violations = ddply(merged_violations[, c(grep("operator_repeated_viol_pInspDay", names(merged_violations)), grep("minesizepoints", names(merged_violations)), grep("controllersizepoints", names(merged_violations)),
                                                                  grep("contractorsizepoints", names(merged_violations)), grep("contractor_repeated_viol_cnt", names(merged_violations)),
                                                                  match("mineid", names(merged_violations)), match("quarter", names(merged_violations)))], c("mineid", "quarter"), 
                                    function(x) colMeans(x[, c(grep("operator_repeated_viol_pInspDay", names(x)), grep("minesizepoints", names(x)), grep("controllersizepoints", names(x)),
                                                               grep("contractorsizepoints", names(x)), grep("contractor_repeated_viol_cnt", names(x)))], na.rm = T))
rm(violations_to_sum)
#Question: Check if operator variables vary by mine? Are indep. contractors the only operators @ a mine or are they only a part of the operation? A: Inspections generate
#both contractor and operator violations. 6/6/16

######################################################################################################################################
# COUNT TOTAL # QUARTERS PER INSPECTION AND TOTAL # INSPECTIONS PER QUARTER 

# COLLAPSE TO MINE-QUARTER-EVENT LEVEL TO FLAG EACH INSPECTIONS PER MINE QUARTER WITH A "1"
num_inspecs_per_qtr = ddply(merged_violations[, c(match("sumtotal_insp_hours", names(merged_violations)), match("sumtotal_on_site_hours", names(merged_violations)),
                                                       grep("mineid", names(merged_violations)), match("quarter", names(merged_violations)), 
                                                       grep("eventno", names(merged_violations)))], c("mineid", "quarter", "eventno"), 
                            function(x) colSums(x[, c(match("sumtotal_insp_hours", names(x)), match("sumtotal_on_site_hours", names(x)))], na.rm = T))
num_inspecs_per_qtr$num_insp = 1

# COLLAPSE TO MINE-QUARTER LEVEL AND CREATE VARIABLE COUNTING THE TOTAL # OF INSPECTIONS PER MINE QUARTER
# "sumtotal_insp_hours" IS ONLY IN THE LIST BECAUSE OTHER YOU GET AN ERROR THAT "X" MUST BE AN ARRAY OF TWO COLUMNS... 
num_inspecs_per_qtr = ddply(num_inspecs_per_qtr[, c(match("num_insp", names(num_inspecs_per_qtr)), match("sumtotal_insp_hours", names(num_inspecs_per_qtr)), 
                                                    match("mineid", names(num_inspecs_per_qtr)), 
                                              match("quarter", names(num_inspecs_per_qtr)))], c("mineid", "quarter"), 
                      function(x) colSums(x[, c(match("num_insp", names(x)), match("sumtotal_insp_hours", names(x)))], na.rm = T))
# just get rid of it for now so it doesn't confuse us
num_inspecs_per_qtr = num_inspecs_per_qtr[, c(-grep("sumtotal_insp_hours", names(num_inspecs_per_qtr)))]

# COLLAPSE TO MINE-INSPECTION-QUARTER LEVEL TO FLAG EACH QUARTER PER INSPECTION WITH A "1"
num_qtrs_per_inspec = ddply(merged_violations[, c(match("sumtotal_insp_hours", names(merged_violations)), match("sumtotal_on_site_hours", names(merged_violations)), 
                                                          match("mineid", names(merged_violations)), grep("eventno", names(merged_violations)),
                                                          match("quarter", names(merged_violations)))], c("mineid", "eventno", "quarter"), 
                            function(x) colSums(x[, c(match("sumtotal_insp_hours", names(x)), match("sumtotal_on_site_hours", names(x)))], na.rm = T))
num_qtrs_per_inspec$num_qtrs = 1

# COLLAPSE TO MINE-INSPECTION LEVEL AND CREATE VARIABLE COUNTING THE TOTAL # OF QUARTERS PER INSPECTION (TO DIVIDE HOURS PER INSPECTION INTO QUARTERLY VARS)
num_qtrs_per_inspec = ddply(num_qtrs_per_inspec[, c(match("num_qtrs", names(num_qtrs_per_inspec)), match("sumtotal_insp_hours", names(num_qtrs_per_inspec)), match("mineid", names(num_qtrs_per_inspec)), 
                                                    match("eventno", names(num_qtrs_per_inspec)))], c("mineid", "eventno"), 
                            function(x) colSums(x[, c(match("num_qtrs", names(x)), match("sumtotal_insp_hours", names(x)))], na.rm = T))
# same - just get rid of it for now so it doesn't confuse us
num_qtrs_per_inspec = num_qtrs_per_inspec[, c(-grep("sumtotal_insp_hours", names(num_qtrs_per_inspec)))]
num_qtrs_per_inspec = num_qtrs_per_inspec[complete.cases(num_qtrs_per_inspec$mineid),]
num_qtrs_per_inspec = num_qtrs_per_inspec[complete.cases(num_qtrs_per_inspec$eventno),]

# MERGE NUMBER OF QUARTERS PER INSPECTION INTO VIOLATIONS/ASSESSMENTS/MINES
merged_violations = merge(num_qtrs_per_inspec, merged_violations, by = c("mineid", "eventno"), all = T)

# DIVIDE INSPECTION HOURS INTO QUARTERLY VARS (TOTAL HOURS PER INSPECTION/NUMBER OF QUARTERS PER INSPECTION)
merged_violations$insp_hours_per_qtr = (merged_violations$sumtotal_insp_hours / merged_violations$num_qtrs)
merged_violations$onsite_insp_hours_per_qtr = (merged_violations$sumtotal_on_site_hours / merged_violations$num_qtrs)

# COLLAPSE ALL INSPECTIONS DATA TO THE MINE-QUARTER LEVEL - VARS TO AVERAGE
summed_inspcs = ddply(merged_violations[, c(match("insp_hours_per_qtr", names(merged_violations)), match("onsite_insp_hours_per_qtr", names(merged_violations)),
                                                    match("mineid", names(merged_violations)), match("quarter", names(merged_violations)))], c("mineid", "quarter"), 
                      function(x) colMeans(x[, c(match("insp_hours_per_qtr", names(x)), match("onsite_insp_hours_per_qtr", names(x)))], na.rm = T))

# MERGE NUMBER OF INSPECTIONS PER QUARTER & NUMBER OF QUARTERS PER INSPECTION ONTO INSPECTIONS DATA
summed_inspcs = merge(summed_inspcs, num_inspecs_per_qtr, by = c("mineid", "quarter"), all = T)

######################################################################################################################################
# COLLAPSE ACCIDENTS DATA

# format date and quarter vars for merges
mines_accidents_coded$accidentdate = as.Date(as.character(mines_accidents_coded$accidentdate), "%m/%d/%Y")
mines_accidents_coded$quarter = as.yearqtr(mines_accidents_coded$accidentdate)

# drop observations before our study period
mines_accidents_coded = mines_accidents_coded[(mines_accidents_coded$quarter > "1999 Q4"),]

# merge on minetypes to drop non-coal and non-underground observations before saving
mines_accidents_coded = merge(mines_accidents_coded, mine_types, by = c("mineid"), all = T)

# only keep observations from environment we care about
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$minetype != "Surface",]
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$minetype != "",]
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$coalcormetalmmine.x == "C",]

# format mineid so we can properly merge onto minequarters
mines_accidents_coded$mineid = str_pad(mines_accidents_coded$mineid, 7, pad = "0")
mines_accidents_coded$mineid = withr::with_options(c(scipen = 999), str_pad(mines_accidents_coded$mineid, 7, pad = "0"))

# create injury indicator so that we can collapse & sum total injuries per mine quarter
mines_accidents_coded$totalinjuries = 1

# collapse mines_accidents data here.
summed_coded_accidents = ddply(mines_accidents_coded[, c(grep("totalinjuries", names(mines_accidents_coded)), grep("MR", names(mines_accidents_coded)),
                                                                match("mineid", names(mines_accidents_coded)), match("quarter", names(mines_accidents_coded)))], c("mineid", "quarter"), 
                                  function(x) colSums(x[, c(grep("totalinjuries", names(x)), grep("MR", names(x)))], na.rm = T))

######################################################################################################################################
# FINISH COLLAPSING AND CLEANING VIOLATIONS

summed_violations$row_id = seq.int(nrow(summed_violations))
averaged_violations$row_id = seq.int(nrow(averaged_violations))
collapsed_violations = merge(summed_violations, averaged_violations, by = c("mineid", "quarter"), all = T)
collapsed_violations[, "merge1"] = ifelse(!is.na(collapsed_violations$row_id.y) & !is.na(collapsed_violations$row_id.x), 3, 0)
collapsed_violations[, "merge1"] = ifelse(is.na(collapsed_violations$row_id.x) & !is.na(collapsed_violations$row_id.y), 2, collapsed_violations[, "merge1"])
collapsed_violations[, "merge1"] = ifelse(is.na(collapsed_violations$row_id.y) & !is.na(collapsed_violations$row_id.x), 1, collapsed_violations[, "merge1"])
table(collapsed_violations$merge1)
#Open Data only:
#3 
#46539 

collapsed_violations = collapsed_violations[, -grep("row_id", names(collapsed_violations))]
collapsed_violations$row_id = seq.int(nrow(collapsed_violations))
summed_coded_accidents$row_id = seq.int(nrow(summed_coded_accidents))

######################################################################################################################################
# MERGE VIOLATIONS DATA ONTO MINES
merged_mines_violations = merge(mines_quarters, collapsed_violations, by = c("mineid", "quarter"), all = T)
#To save time we save/load all constituent datasets in the prediction data after each of the following merges
merged_mines_violations = merged_mines_violations[, -grep("row_id", names(merged_mines_violations))]
merged_mines_violations$row_id = seq.int(nrow(merged_mines_violations))
saveRDS(collapsed_violations, file = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_violations.rds")

######################################################################################################################################
# MERGE ACCIDENTS DATA ONTO VIOLATIONS/PER QUARTER

merged_mines_violations_accidents = merge(merged_mines_violations, summed_coded_accidents, by = c("mineid", "quarter"), all = T)

merged_mines_violations_accidents[, "merge2"] = ifelse(!is.na(merged_mines_violations_accidents$row_id.y) & !is.na(merged_mines_violations_accidents$row_id.x), 3, 0)
merged_mines_violations_accidents[, "merge2"] = ifelse(is.na(merged_mines_violations_accidents$row_id.x) & !is.na(merged_mines_violations_accidents$row_id.y), 2, merged_mines_violations_accidents[, "merge2"])
merged_mines_violations_accidents[, "merge2"] = ifelse(is.na(merged_mines_violations_accidents$row_id.y) & !is.na(merged_mines_violations_accidents$row_id.x), 1, merged_mines_violations_accidents[, "merge2"])
table(merged_mines_violations_accidents$merge2) 
#Open Data Only:
#1     2     3 
#43353  3568  9274

merged_mines_violations_accidents$totalinjuries = ifelse(is.na(merged_mines_violations_accidents$totalinjuries), 0, merged_mines_violations_accidents$totalinjuries)
merged_mines_violations_accidents$total_violations = ifelse(is.na(merged_mines_violations_accidents$total_violations), 0, merged_mines_violations_accidents$total_violations)

merged_mines_violations_accidents = merged_mines_violations_accidents[, -grep("row_id", names(merged_mines_violations_accidents))]
merged_mines_violations_accidents$row_id = seq.int(nrow(merged_mines_violations_accidents))
summed_inspcs$row_id = seq.int(nrow(summed_inspcs))

######################################################################################################################################
# MERGE ON FINAL INSPECTION CHARACTERISTICS AND CLEAN UP FINAL PREDICTION DATASET

prediction_data = merge(merged_mines_violations_accidents, summed_inspcs, by = c("mineid", "quarter"), all = T)
prediction_data[, "merge3"] = ifelse(!is.na(prediction_data$row_id.y) & !is.na(prediction_data$row_id.x), 3, 0)
prediction_data[, "merge3"] = ifelse(is.na(prediction_data$row_id.x) & !is.na(prediction_data$row_id.y), 2, prediction_data[, "merge3"])
prediction_data[, "merge3"] = ifelse(is.na(prediction_data$row_id.y) & !is.na(prediction_data$row_id.x), 1, prediction_data[, "merge3"])
table(prediction_data$merge3)
#Open Data Only:
#1     3 
#24634 31561

#remove observations that are missing key variables or are from the wrong environment
prediction_data = prediction_data[complete.cases(prediction_data$quarter),]
prediction_data = prediction_data[complete.cases(prediction_data$minetype),]
prediction_data = prediction_data[prediction_data$minetype == "Underground",]
prediction_data = prediction_data[prediction_data$coalcormetalmmine == "C",]

prediction_data = prediction_data[, c(-grep("merge", names(prediction_data)), -grep("row_id", names(prediction_data)))]

saveRDS(prediction_data, file = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/prediction_data.rds")

#GOAL HERE IS TO MAKE A LOOP TO FILL IN MISSING VLAUES (OF, SAY, MINENAME) BY MINE_ID GROUPS
# CURRENT ISSUE IS A KNOWN BUG WITH DPLYR - https://github.com/hadley/dplyr/issues/859
#library(dplyr)
#library(zoo)
#prediction_data$mineid = as.character(prediction_data$mineid)
#prediction_data %>% group_by(prediction_data$mineid) %>% do(na.locf(prediction_data$minename))

######################################################################################################################################
# EVERYTHING BELOW THIS LINE IS FOR THE ALGORITHM

rm(multi_qtr_inspcs, merged_violations, mines_accidents_coded, summed_coded_accidents, summed_violations, summed_inspcs, averaged_violations)
#To provide an intercept for the prediction stage:
prediction_data$constant = 1
#WARNING: Fails to converge with these initial values
N = nrow(prediction_data)
K = ncol(prediction_data) - 3
X = as.matrix(prediction_data[, c(-grep("MR", names(prediction_data)), -grep("mineid", names(prediction_data)), -grep("quarter", names(prediction_data)))])
Y = as.vector(prediction_data$MR)

#Model Selection
#PCA
pca_output = prcomp(na.omit(X))
pca_results = pca_output$rotation

test_pred = glarma(Y, X, type = "NegBin", phiLags = c(1, 2), thetaLags = c(1, 2), phiInit = c(0.5, 0.5), thetaInit = c(0.25, 0.25), beta = rep(1, K), alphaInit = 1)
#For some reason, unable to use usual formula abbreviations in this command
names(prediction_data)[match("47.41", names(prediction_data))] = "subsection_47.41"
test_pred_2 = pglm(MR ~  subsection_47.41 + penaltypoints_47.41 + totalinjuries ,
                   prediction_data, na.action = na.omit, family = "negbin", effect = "time", model = "within")
