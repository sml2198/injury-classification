# NIOSH Project 2014-N-15776

# 13 - Prepare Violations Data
    # Loads merged violations data from "11_merge_violations.R" 
    # Merges violations with prepared CFR subsection codes from "10_clean_cfr_key.R", 
    # Creates subsection specific variables and lagged variables
    # Collapses violations to the mine-quarter level
    # Loads inspections data from "8_clean_inspections.R" and collapses to the mine-quarter level
    # Loads accidents data from "6_collapse_accidents.R"
    # Merges mine-quarter level violations with inspections and accidents

# Last edit 10/10/16

######################################################################################################

library(plyr)
library(stats)
library(stringr)
library(withr)
library(psych)
library(reshape)
library(foreign)

# define file names
  # input: merged violations data produced in 11_merge_violations.R
merged_violations_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds"
  # input: collapsed coded MR accidents data produced in 6_collapse_accidents.R
MR_accidents_coded_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_MR_accidents.rds"
  # input: collapsed coded PS accidents data produced in 6_collapse_accidents.R - DOES NOT EXIST YET
PS_accidents_coded_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_PS_accidents.rds"
  # input: cleaned mine-quarters as produced in 1_clean_mines.R
mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds"
  # input: cleaned mine-types key produced in produced in 1_clean_mines.R
mine_types_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds"
  # input: cleaned union and production category variables (at the mine-year level) from 1b_clean_union_data.R
eia_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_eia.rds"
  
  # output: prediction-ready data containing all relevant and maybe relevant vars - MR
MR_prediction_data_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"
  # output: prediction-ready csv containing all relevant and maybe relevant vars (for Stata analysis) - MR
MR_prediction_data_out_csv_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.csv"
  # output: prediction-ready dta containing all relevant and maybe relevant vars (for Stata analysis) - MR
MR_prediction_data_out_dta_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.dta"
  # output: prediction-ready data containing only relevant vars - MR
MR_relevant_prediction_data_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data_relevant.rds"

  # output: prediction-ready data containing all relevant and maybe relevant vars  - PS
PS_prediction_data_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/PS_prediction_data.rds"
  # output: prediction-ready csv containing all relevant and maybe relevant vars (for Stata analysis) - PS
PS_prediction_data_out_csv_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/PS_prediction_data.csv"
  # output: prediction-ready dta containing all relevant and maybe relevant vars (for Stata analysis) - PS
PS_prediction_data_out_dta_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/PS_prediction_data.dta"
  # output: prediction-ready data containing only relevant vars - PS
PS_relevant_prediction_data_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/PS_prediction_data_relevant.rds"

# When relevant-only option is set to "on" (not commented out) only CFR subsections marked as "relevant" will be used for creating
# vars. Otherwise, "relevant" and "maybe relevant subsections" will be used.
#relevant.only.option = "on"
relevant.only.option = "off"

# Specify whether you want to create data for maintenance and repair (MR) or pinning and striking (PS) injuries
#injury.type = "MR"
injury.type = "PS"

# Specify whether or not you want this file to also produce a Stata-friendly .csv 
stata.friendly = T

# Specify whether or not you also want to produce data collapsed to the mine-year level
make.year.data = T

######################################################################################################################################

# Define log file name
#sink(file="prepare_violations_log.txt")

######################################################################################################################################

# Read in merged violations-assessments: 836,612 obs, 1669 unique mineids, 118 vars
merged_violations = readRDS(merged_violations_in_file_name)

# Format inspection type
merged_violations$inspacty = tolower(merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "mine idle activity", "mine idle", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "na", "n", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "part 50 audit", "part 50 audits", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "non-fatal accident investigation", "nonfatal injury accident inv", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "shaft, slope or major construction spot inspection", "shft, slpe, or maj constr spot insp", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "103(g)(1) spot umwa inspection", "103(g)(1) spot inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "103(i) spot (ign or expl) insp", "103(i) spot inspections", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "103(i) spot (hazard) inspection", "103(i) spot inspections", merged_violations$inspacty)

# Collapse inspection type categories to reasonable amount
merged_violations$inspacty = ifelse((merged_violations$inspacty == "regular inspection" |
                                       merged_violations$inspacty == "regular safety and health inspection"), "regular inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(grepl("complaint", merged_violations$inspacty), "complaint inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "fatal accident investigation", "fatality inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(grepl("103", merged_violations$inspacty), "103", merged_violations$inspacty)
merged_violations$inspacty = ifelse(grepl("(103|fatality|regular|complaint)", merged_violations$inspacty), merged_violations$inspacty, "other")

# Format violation and assessment type
merged_violations$violationtypecode = as.character(merged_violations$violationtypecode)
merged_violations$assessmenttypecode = as.character(merged_violations$assessmenttypecode)

# Specify function for making the pts vars into numeric vars (from their current factor state)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Remove the 3 observations that are missing for likelihood, injuryillness & negligence (now 836,609 obs)
merged_violations = merged_violations[(!is.na(merged_violations$likelihood) & 
                                       !is.na(merged_violations$injuryillness) & 
                                       !is.na(merged_violations$negligence)),]

# Rename likelihood categories and deal with missing values
merged_violations$likelihood.pts.con = merged_violations$likelihood
levels(merged_violations$likelihood.pts.con) = c(3, 0, 4, 2, 1)
merged_violations$likelihood.pts.con = as.numeric.factor(merged_violations$likelihood.pts.con)
merged_violations$likelihood = as.character(merged_violations$likelihood)

# Rename injuryillness categories and deal with missing values
merged_violations$injuryillness.pts.con = merged_violations$injuryillness
levels(merged_violations$injuryillness.pts.con) = c(3, 1, 0, 2)
merged_violations$injuryillness.pts.con = as.numeric.factor(merged_violations$injuryillness.pts.con)
merged_violations$injuryillness = as.character(merged_violations$injuryillness)

# Rename negligence categories and deal with missing values
merged_violations$negligence.pts.con = merged_violations$negligence
levels(merged_violations$negligence.pts.con) = c(3, 1, 2, 0, 4)
merged_violations$negligence.pts.con = as.numeric.factor(merged_violations$negligence.pts.con)
merged_violations$negligence = as.character(merged_violations$negligence)

# Sarah emailed April Ramirez (DOL) @ 2:02PM on 8/18/16 to ask why our personsaffected var (from the violations data) disagrees 
# with the gravitypersonpoints var (from the assessments data) if we compare how they should map into each other based on the CFR 
# code, from here: http://www.ecfr.gov/cgi-bin/text-idx?SID=f563151d4c4ee003f464fc78296bc3a8&node=pt30.1.100&rgn=div
# She explained that in 2000 the point values (for personsaffected) were different than they are today; she also confirmed this with 
# Melanie Garris, Chief, Civil Penalty and Compliance Office. We use the personsaffected var from the violations data without concern.
# She also shared that a decrease in the expected number of gravity person points could be the result of a court decision.
names(merged_violations)[names(merged_violations) == "numberaffected"] = "personsaffected"

# But, I'm still super skeptical of this variable, so I'm producing another one that is capped at 10 people
# (which is the limit of categories in the CFR code)
merged_violations$personsaffected.pts.con = merged_violations$personsaffected
merged_violations$personsaffected.pts.con = ifelse(merged_violations$personsaffected.pts.con > 10, 
                                                   10, merged_violations$personsaffected.pts.con)

# Rename points vars 
names(merged_violations)[names(merged_violations) == "gravitypersonspoints"] = "personsaffected.pts"
names(merged_violations)[names(merged_violations) == "negligencepoints"] = "negligence.pts"
names(merged_violations)[names(merged_violations) == "gravityinjurypoints"] = "injuryillness.pts"
names(merged_violations)[names(merged_violations) == "gravitylikelihoodpoints"] = "likelihood.pts"

# Format sig and sub var
names(merged_violations)[names(merged_violations) == "sigandsubdesignation"] = "sigandsub"
levels(merged_violations$sigandsub) = c(0,1)
merged_violations$sigandsub = as.numeric.factor(merged_violations$sigandsub)

######################################################################################################################################

# # Function to dummy out variables
# datdum = function(var_name, data, data_name) {
#   data$rv = rnorm(nrow(data), 1, 1)
#   mm = data.frame(model.matrix(lm(data$rv ~ -1 + factor(data[, var_name]))))
#   data$rv = NULL
#   data[, var_name] = NULL
#   names(mm) = paste(var, 1:ncol(mm), sep = ".")
#   data = cbind(data, mm)
#   assign(data_name, data, .GlobalEnv) 
# }
# 
# # Dummy out categorical variables
# dum_vars = c("inspacty", 
#              "assessmenttypecode", 
#              "violationtypecode", 
#              "likelihood", 
#              "injuryillness", 
#              "negligence")
# 
# for (var in dum_vars) {
#   datdum(var, merged_violations, "merged_violations")
# }
# 
# # Create lists of number of dummies for violation, assessment, and inspection types 
# violationtypecodes = seq(1, 4)
# assessmenttypecodes = seq(1, 3)
# inspactycodes = seq(1, 5)
# 
# # Create lists of number of dummies for likelihood, injuryillness (num persons affected) and negligence codes 
# likelihoodcodes = seq(1, 5)
# injuryillnesscodes = seq(1, 4)
# negligencecodes = seq(1, 5)
#
# # Memory
# rm(dum_vars, var)

######################################################################################################################################

# PREPARE TO GENERATE PART AND SUBSECTION SPECIFIC VARIABLES

# For part-specific variable creation
if (injury.type == "MR") {
  if (relevant.only.option != "on") {
    relevant_partcodes = as.list(levels(factor(merged_violations[merged_violations$MR_relevant == 1 | 
                                                                 merged_violations$MR_maybe_relevant == 1, ]$cfr_part_code)))
    relevant_subsectcodes = as.list(levels(factor(merged_violations[merged_violations$MR_relevant == 1 | 
                                                                    merged_violations$MR_maybe_relevant == 1, ]$subsection_code)))
  }
  if (relevant.only.option == "on") {
    relevant_partcodes = as.list(levels(factor(merged_violations[merged_violations$MR_relevant == 1, ]$cfr_part_code)))
    relevant_subsectcodes = as.list(levels(factor(merged_violations[merged_violations$MR_relevant == 1, ]$subsection_code)))
  }
}
if (injury.type == "PS") {
  if (relevant.only.option != "on") {
    relevant_partcodes = as.list(levels(factor(merged_violations[merged_violations$PS_relevant == 1 | 
                                                                 merged_violations$PS_maybe_relevant == 1, ]$cfr_part_code)))
    relevant_subsectcodes = as.list(levels(factor(merged_violations[merged_violations$PS_relevant == 1 | 
                                                                    merged_violations$PS_maybe_relevant == 1, ]$subsection_code)))
  }
  if (relevant.only.option == "on") {
    relevant_partcodes = as.list(levels(factor(merged_violations[merged_violations$PS_relevant == 1, ]$cfr_part_code)))
    relevant_subsectcodes = as.list(levels(factor(merged_violations[merged_violations$PS_relevant == 1, ]$subsection_code)))
  }
}

# If the subsectioncode is (maybe-)relevant, AND there are < 15 violations from that subsectioncode, remove
# that subsectioncode from the relevant lists 
remove_subcodes = list()
for (code in relevant_subsectcodes) {
  if (nrow(merged_violations[merged_violations$subsection_code == code, ]) < 15) {
    remove_subcodes = c(remove_subcodes, code)
  }
}
relevant_subsectcodes = setdiff(relevant_subsectcodes, remove_subcodes)
rm(remove_subcodes)

######################################################################################################################################

# LOOPS TO GENERATE PART AND SUBSECTION SPECIFIC VARIABLES

# Set the list of cfr parts for which to produce dummy vars
cfr_codes = unlist(relevant_partcodes)
    
# This loop creates the part-specific variable-specific vars/dummies (which will later be collapsed to the mine-quarter level).
for (i in 1:length(cfr_codes)) {
    merged_violations[, cfr_codes[i]] = ifelse(merged_violations$cfr_part_code == cfr_codes[i], 1, 0)
    merged_violations[, paste(cfr_codes[i], "penaltypoints", sep = ".")] = apply(cbind(merged_violations[, "penaltypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
    merged_violations[, paste(cfr_codes[i], "sigandsub", sep = ".")] = apply(cbind(merged_violations[, "sigandsub"], merged_violations[, cfr_codes[i]]), 1, prod)
# 
#     # PTS VARS FOR ASSESSMENT CHARACTERISTICS & CONSTRUCTED PTS VARS FOR ASSESSMENT CHARACTERISTICS  
#     merged_violations[, paste(cfr_codes[i], "personsaffected", sep = ".")] = apply(cbind(merged_violations[, "personsaffected"], merged_violations[, cfr_codes[i]]), 1, prod)
#       for (j in 1:length(inspactycodes)) {
#         merged_violations[, paste(cfr_codes[i], "inspacty", inspactycodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("inspacty", inspactycodes[j], sep = ".")] == 1, 1, 0)
#       }
#       for (j in 1:length(violationtypecodes)) {
#         merged_violations[, paste(cfr_codes[i], "violationtypecode", violationtypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("violationtypecode", violationtypecodes[j], sep = ".")] == 1, 1, 0)
#       }
#       for (j in 1:length(assessmenttypecodes)) {
#         merged_violations[, paste(cfr_codes[i], "assessmenttypecode", assessmenttypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("assessmenttypecode", assessmenttypecodes[j], sep = ".")] == 1, 1, 0)
#       }
#       for (j in 1:length(likelihoodcodes)) {
#         merged_violations[, paste(cfr_codes[i], "likelihood", likelihoodcodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("likelihood", likelihoodcodes[j], sep = ".")] == 1, 1, 0)
#       }
#       for (j in 1:length(injuryillnesscodes)) {
#         merged_violations[, paste(cfr_codes[i], "injuryillness", injuryillnesscodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("injuryillness", injuryillnesscodes[j], sep = ".")] == 1, 1, 0)
#       }
#       for (j in 1:length(negligencecodes)) {
#         merged_violations[, paste(cfr_codes[i], "negligence", negligencecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("negligence", negligencecodes[j], sep = ".")] == 1, 1, 0)
#       }
}

# NOW IT'S TIME TO PRODUCE THE SUBSECTION-LEVEL VARIABLES 
cfr_codes = unlist(relevant_subsectcodes)
for (i in 1:length(cfr_codes)) {
  merged_violations[, cfr_codes[i]] = ifelse(merged_violations$subsection_code == cfr_codes[i], 1, 0)
  merged_violations[, paste(cfr_codes[i], "penaltypoints", sep = ".")] = apply(cbind(merged_violations[, "penaltypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "sigandsub", sep = ".")] = apply(cbind(merged_violations[, "sigandsub"], merged_violations[, cfr_codes[i]]), 1, prod)
}
  
# Remove things we won't use again.
rm(relevant_subsectcodes, relevant_partcodes, cfr_codes)
# violationtypecodes, assessmenttypecodes, inspactycodes, likelihoodcodes, injuryillnesscodes, negligencecodes)

######################################################################################################################################

# MAKE PART-LEVEL VARS ONLY CONTAIN DATA FOR OBVS THAT ARE RELEVANT OR MAYBE-RELEVANT

# Currently, part-level vars contain information for all observations in any part with any relevant
# subsections. We only want our part-level vars to reflect information about observations that are
# relevant or maybe relevant (at the subsection level), before we collapse to the mine-quarter level.

varlist = names(merged_violations[, grep("^[0-9][0-9]((.[a-z])|($))", names(merged_violations))])

if (injury.type == "MR" & relevant.only.option == "off") {
  merged_violations$relevant = ifelse((merged_violations$MR_relevant == 1 |
                                       merged_violations$MR_maybe_relevant == 1), 1, 0)
}

if (injury.type == "PS" & relevant.only.option == "off") {
  merged_violations$relevant = ifelse((merged_violations$PS_relevant == 1 |
                                       merged_violations$PS_maybe_relevant == 1), 1, 0)
}

# This problem only affects the part-level data preparation. Subsection-level data is already (obviously) subsection specific.
  for (j in 1:length(varlist)) {
      merged_violations[, varlist[j]] = ifelse(merged_violations$relevant == 1, 0, merged_violations[, varlist[j]])
  }

rm(i, j, code, varlist)

######################################################################################################################################

# COLLAPSE VIOLATIONS DATA TO THE MINE-QUARTER LEVEL

# Create variables to sum for total violation counts (always 1) and number of Mine Act violations (only 1 if not missing Mine Act var)
merged_violations$total_violations = 1
merged_violations$total_mine_act_violations = ifelse(!is.na(merged_violations$section_of_act), 1, 0) # 4873 positive obs

# Create indicator for if violation was terminated (all should be terminated)
merged_violations$terminated = ifelse(merged_violations$typeoftermination == "Terminated", 1, 0)

# Reformat vars to be summed so they are all numeric (required for ddply)
merged_violations$excessive_history_ind = as.numeric(merged_violations$excessive_history_ind)
merged_violations$excessive_history_ind = ifelse(merged_violations$excessive_history_ind == 2, 1, 0)
merged_violations$goodfaithind = as.numeric(merged_violations$goodfaithind)
merged_violations$goodfaithind = ifelse(merged_violations$goodfaithind == 2, 1, 0)

# Select all variables to sum when we collapse to the mine-quarter lever (the first regex will grab all vars created above)
violations_to_sum = merged_violations[, c(grep("^[0-9][0-9]", names(merged_violations)), 
                                          match("total_violations", names(merged_violations)),
                                          match("total_mine_act_violations", names(merged_violations)),
                                          match("excessive_history_ind", names(merged_violations)), 
                                          match("goodfaithind", names(merged_violations)), 
                                          match("terminated", names(merged_violations)),
                                          match("mineid", names(merged_violations)), 
                                          match("quarter", names(merged_violations)))]

# Collapse the variables that we need to sum
summed_violations = ddply(violations_to_sum, c("mineid", "quarter"), function(x) colSums(x[, c(grep("^[0-9][0-9]", names(x)), 
                                                                                               match("total_violations", names(x)),
                                                                                               match("total_mine_act_violations", names(x)),
                                                                                               match("excessive_history_ind", names(x)), 
                                                                                               match("goodfaithind", names(x)), 
                                                                                               match("terminated", names(x)))], na.rm = T))

# Collapse the variables that we need to average.
averaged_violations = ddply(merged_violations[, c(grep("minesizepoints", names(merged_violations)), 
                                                  match("exlate_interest_amt", names(merged_violations)), 
                                                  match("mineid", names(merged_violations)), 
                                                  match("quarter", names(merged_violations)))], c("mineid", "quarter"), 
                                    function(x) colMeans(x[, c(grep("minesizepoints", names(x)), 
                                                               match("exlate_interest_amt", names(x)))], na.rm = T))

rm(violations_to_sum)
# Inspections generate both contractor and operator violations. - April Ramirez @ DOL, 6/6/16

######################################################################################################################################

# GRAB CONTRACTOR VARS AND COLLAPSE TO MINE-QUARTER LEVEL

contractor_vars = merged_violations[, c(match("mineid", names(merged_violations)),
                                        match("quarter", names(merged_violations)),
                                        match("contractorid", names(merged_violations)),
                                        match("con_avg_employee_cnt_qtr", names(merged_violations)),
                                        match("con_employee_hours_qtr", names(merged_violations)),
                                        match("con_coal_prod_qtr", names(merged_violations)))]
# Remove obs without contractorid
contractor_vars = contractor_vars[!is.na(contractor_vars$contractorid),]

# Remove obs that never merged with production/employment info
contractor_vars = contractor_vars[!is.na(contractor_vars$con_avg_employee_cnt_qtr),]

# Collapse contractor vars to mine quarter level 
contractor_vars = ddply(contractor_vars[, c(grep("mineid", names(contractor_vars)), 
                                            grep("quarter", names(contractor_vars)), 
                                            grep("con_avg_employee_cnt_qtr", names(contractor_vars)), 
                                            grep("con_employee_hours_qtr", names(contractor_vars)),
                                            match("con_coal_prod_qtr", names(contractor_vars)))], c("mineid", "quarter"),
                            function(x) colMeans(x[, c(grep("con_avg_employee_cnt_qtr", names(x)), 
                                                       grep("con_employee_hours_qtr", names(x)),
                                                       grep("con_coal_prod_qtr", names(x)))], na.rm = T))

######################################################################################################################################

# COUNT TOTAL NUMBER OF QUARTERS PER INSPECTION AND TOTAL NUMBER OF INSPECTIONS PER QUARTER 

# While in theory inspections are quarterly, they do not always happen once per quarter or even four times per year. Some inspections run
# long (can last an entire year), sometimes special or spot inspections are triggered, or sometimes mines simply are not inspected as
# frequently as they should be. Assuming that an inspection beginning in a certain quarter has all inspection hours contained within 
# that quarter would be foolish. Therefore we not only count the total number of inspections per quarter as a sort of exposure for
# the opportunity for a violation to occur, but we also count the number of quarters for inspection (ie if an inspection last a year,
# there will be four quarters for that inspection). This allows us to the take the total inspection hours variable and divide it by the 
# number of quarters spanned by that inspection. This helps prevent double-counting inspection hours in several quarters, once we collapse
# to the mine-quarter level.

# Collapse to mine-quarter-event level to flag each inspections per mine quarter with a "1"
num_inspecs_per_qtr = ddply(merged_violations[, c(match("sumtotal_insp_hours", names(merged_violations)), 
                                                  match("sumtotal_on_site_hours", names(merged_violations)),
                                                  grep("mineid", names(merged_violations)), match("quarter", names(merged_violations)), 
                                                  grep("eventno", names(merged_violations)))], c("mineid", "quarter", "eventno"), 
                            function(x) colSums(x[, c(match("sumtotal_insp_hours", names(x)), match("sumtotal_on_site_hours", names(x)))], na.rm = T))

# Create indicator for inspections, so we can sum inspections per quarter. This works because the data is currently @ the inspections-level
num_inspecs_per_qtr$num_insp = 1

# Collapse to mine-quarter level and create variable counting the total # of inspections per mine quarter
# "sumtotal_insp_hours" is only in the list because other you get an error that "x" must be an array of two columns... 
num_inspecs_per_qtr = ddply(num_inspecs_per_qtr[, c(match("num_insp", names(num_inspecs_per_qtr)), 
                                                    match("sumtotal_insp_hours", names(num_inspecs_per_qtr)), 
                                                    match("mineid", names(num_inspecs_per_qtr)), 
                                                    match("quarter", names(num_inspecs_per_qtr)))], c("mineid", "quarter"), 
                      function(x) colSums(x[, c(match("num_insp", names(x)), match("sumtotal_insp_hours", names(x)))], na.rm = T))

# Just get rid of sumtotal_insp_hours so it doesn't confuse us and is only a placeholder
num_inspecs_per_qtr = num_inspecs_per_qtr[, c(-grep("sumtotal_insp_hours", names(num_inspecs_per_qtr)))]

# Collapse to mine-inspection-quarter level to flag each quarter per inspection with a "1"
num_qtrs_per_inspec = ddply(merged_violations[, c(match("sumtotal_insp_hours", names(merged_violations)), 
                                                  match("sumtotal_on_site_hours", names(merged_violations)), 
                                                  match("mineid", names(merged_violations)), grep("eventno", names(merged_violations)),
                                                  match("quarter", names(merged_violations)))], c("mineid", "eventno", "quarter"), 
                            function(x) colSums(x[, c(match("sumtotal_insp_hours", names(x)), match("sumtotal_on_site_hours", names(x)))], na.rm = T))
num_qtrs_per_inspec$num_qtrs = 1

# Collapse to mine-inspection level and create variable counting the total # of quarters per inspection (to divide hours per inspection into quarterly vars)
num_qtrs_per_inspec = ddply(num_qtrs_per_inspec[, c(match("num_qtrs", names(num_qtrs_per_inspec)), 
                                                    match("sumtotal_insp_hours", names(num_qtrs_per_inspec)), 
                                                    match("mineid", names(num_qtrs_per_inspec)), 
                                                    match("eventno", names(num_qtrs_per_inspec)))], c("mineid", "eventno"), 
                            function(x) colSums(x[, c(match("num_qtrs", names(x)), match("sumtotal_insp_hours", names(x)))], na.rm = T))

# Same as above - just get rid of sumtotal_insp_hours so it doesn't confuse us
num_qtrs_per_inspec = num_qtrs_per_inspec[, c(-grep("sumtotal_insp_hours", names(num_qtrs_per_inspec)))]

# Drop cases missing mineid or eventno
num_qtrs_per_inspec = num_qtrs_per_inspec[complete.cases(num_qtrs_per_inspec$mineid),]
num_qtrs_per_inspec = num_qtrs_per_inspec[complete.cases(num_qtrs_per_inspec$eventno),]

# Merge number of quarters per inspection into violations/assessments/mines
merged_violations = merge(num_qtrs_per_inspec, merged_violations, by = c("mineid", "eventno"), all = T)

# Divide inspection hours into quarterly vars (total hours per inspection/number of quarters per inspection)
merged_violations$insp_hours_per_qtr = (merged_violations$sumtotal_insp_hours / merged_violations$num_qtrs)
merged_violations$onsite_insp_hours_per_qtr = (merged_violations$sumtotal_on_site_hours / merged_violations$num_qtrs)

# Collapse all inspections data to the mine-quarter level - vars to average
summed_inspcs = ddply(merged_violations[, c(match("insp_hours_per_qtr", names(merged_violations)), 
                                            match("onsite_insp_hours_per_qtr", names(merged_violations)),
                                            match("mineid", names(merged_violations)), 
                                            match("quarter", names(merged_violations)))], c("mineid", "quarter"), 
                      function(x) colMeans(x[, c(match("insp_hours_per_qtr", names(x)), 
                                                 match("onsite_insp_hours_per_qtr", names(x)))], na.rm = T))

# Merge number of inspections per quarter & number of quarters per inspection onto inspections data
summed_inspcs = merge(summed_inspcs, num_inspecs_per_qtr, by = c("mineid", "quarter"), all = T)

rm(merged_violations, num_inspecs_per_qtr, num_qtrs_per_inspec)
gc()

######################################################################################################################################

# FINISH MERGING VIOLATIONS

# Merge the summed and averaged components of violations (both now at the mine-quarter level)
summed_violations$row_id = seq.int(nrow(summed_violations))
averaged_violations$row_id = seq.int(nrow(averaged_violations))
collapsed_violations = merge(summed_violations, averaged_violations, by = c("mineid", "quarter"), all = T)
rm(summed_violations, averaged_violations)

# Flag observations that do and don't merge - this was used to tab the merge & compare to Stata as a sanity check
collapsed_violations[, "merge1"] = ifelse(!is.na(collapsed_violations$row_id.y) & !is.na(collapsed_violations$row_id.x), 3, 0)
collapsed_violations[, "merge1"] = ifelse(is.na(collapsed_violations$row_id.x) & !is.na(collapsed_violations$row_id.y), 2, collapsed_violations[, "merge1"])
collapsed_violations[, "merge1"] = ifelse(is.na(collapsed_violations$row_id.y) & !is.na(collapsed_violations$row_id.x), 1, collapsed_violations[, "merge1"])
collapsed_violations = collapsed_violations[, -grep("row_id", names(collapsed_violations))]
collapsed_violations$row_id = seq.int(nrow(collapsed_violations))

######################################################################################################################################

# MERGE VIOLATIONS DATA ONTO MINES

# Read in data and drop obs not in study period
mines_quarters = readRDS(mines_quarters_file_name) # 30551 obs 21 vars
mines_quarters = mines_quarters[mines_quarters$quarter < "2016 Q2",] # 30289 obs

# Merge mine-specific contractor info onto mine quarters & drop 140 obs that were added but not merged (there were no nonmissing hours until this)
merged_quarters_contractors = merge(mines_quarters, contractor_vars, by = c("mineid", "quarter"), all = T)
merged_quarters_contractors = merged_quarters_contractors[!is.na(merged_quarters_contractors$hours_qtr),] 

# Merge all violation vars onto mine quarters & drop obs that were added but not merged from violations data 
merged_mines_violations = merge(merged_quarters_contractors, collapsed_violations, by = c("mineid", "quarter"), all = T)
merged_mines_violations = merged_mines_violations[!is.na(merged_mines_violations$hours_qtr),] 

# To save time we save/load all constituent datasets in the prediction data after each of the following merges
merged_mines_violations = merged_mines_violations[, -grep("row_id", names(merged_mines_violations))]
merged_mines_violations$row_id = seq.int(nrow(merged_mines_violations))

rm(contractor_vars, collapsed_violations, mines_quarters, merged_quarters_contractors)
gc()

######################################################################################################################################

# MERGE ACCIDENTS DATA ONTO VIOLATIONS/PER QUARTER

# read in data (either PS or MR) and drop obs not in study period

if (injury.type == "MR"){
  summed_coded_accidents = readRDS(MR_accidents_coded_file_name)
}
if (injury.type == "PS"){
  summed_coded_accidents = readRDS(PS_accidents_coded_file_name)
}

# Merge violations (now with contractor and mine info) and accidents
merged_mines_violations_accidents = merge(merged_mines_violations, summed_coded_accidents, by = c("mineid", "quarter"), all = T)
merged_mines_violations_accidents = merged_mines_violations_accidents[!is.na(merged_mines_violations_accidents$hours_qtr),]

# Replace missings (mine quarters without accidents data) with zeroes (see next section for zero violations)
merged_mines_violations_accidents$total_injuries = ifelse(is.na(merged_mines_violations_accidents$total_injuries), 
                                                         0, merged_mines_violations_accidents$total_injuries)

# Replace MR/PS with zero where missing (these all occur in quarters that had zero total accidents)
if (injury.type == "MR") {
  merged_mines_violations_accidents$MR = ifelse(is.na(merged_mines_violations_accidents$MR), 
                                                0, merged_mines_violations_accidents$MR)
}
if (injury.type == "PS") {
merged_mines_violations_accidents$PS = ifelse(is.na(merged_mines_violations_accidents$PS), 
                                              0, merged_mines_violations_accidents$PS)
}

merged_mines_violations_accidents = merged_mines_violations_accidents[, -grep("row_id", names(merged_mines_violations_accidents))]
merged_mines_violations_accidents$row_id = seq.int(nrow(merged_mines_violations_accidents))
summed_inspcs$row_id = seq.int(nrow(summed_inspcs))

rm(merged_mines_violations, summed_coded_accidents)
gc()

######################################################################################################################################

# MERGE ON FINAL INSPECTION CHARACTERISTICS AND CLEAN UP FINAL PREDICTION DATASET

# Merge violations & accidents with remaining inspections data
prediction_data = merge(merged_mines_violations_accidents, summed_inspcs, by = c("mineid", "quarter"), all = T)

# replace NAs in inspections with zeroes (mine quarters that had no inspections)
prediction_data$num_insp = ifelse(is.na(prediction_data$num_insp), 0, prediction_data$num_insp)

# Replace missings (mine quarters without violations) with zeroes - BUT only when there were no inspections!
prediction_data$total_violations = ifelse((is.na(prediction_data$total_violations) &
                                             prediction_data$num_insp > 0), 0, prediction_data$total_violations)

rm(merged_mines_violations_accidents,summed_inspcs)
gc()

# Remove observations that are missing key variables or are from the wrong environment (there shouldn't be any at this point, but just in case)
prediction_data = prediction_data[complete.cases(prediction_data$minetype),]
prediction_data = prediction_data[prediction_data$minetype == "Underground",]
prediction_data = prediction_data[prediction_data$coalcormetalmmine == "C",]

######################################################################################################################################

# FINAL VARIABLE CLEANING AND PREP

# Group data by mines and order the data by mine-quarter
prediction_data = prediction_data[order(prediction_data[,"mineid"], prediction_data[,"quarter"]),]

# Add variables for binary and proportional dependent vars
if (injury.type == "MR"){
  prediction_data$MR_indicator = ifelse(prediction_data$MR > 0, 1, 0)
}
if (injury.type == "PS"){
  prediction_data$PS_indicator = ifelse(prediction_data$PS > 0, 1, 0)
}

# All violations should be terminated, meaning the issue has been abated by the violator. A violation that has not been terminated is
# therefore an indication of mine/operator negligence. Rather than include the number of terminations as a variable in our models
# (which would yield collinearity because the # of terminations closely tracks the # of violations), we create a variable that is 
# indicates the # of non-terminated violations in a mine quarter. 
prediction_data$num_no_terminations = ifelse(prediction_data$terminated < prediction_data$total_violations, 
                                             prediction_data$total_violations-prediction_data$terminated, 0)

# Rename and format state var
prediction_data$stateabbreviation = as.factor(as.character(prediction_data$stateabbreviation))

# Drop unnecessary vars - at this point should have 30,289 obs & 1046 vars
prediction_data = prediction_data[, c(-grep("coalcormetalmmine", names(prediction_data)),
                                      -grep("con_", names(prediction_data)),
                                      -grep("daysperweek", names(prediction_data)), 
                                      -grep("excessive_history_ind", names(prediction_data)),
                                      -grep("exlate_interest_amt", names(prediction_data)),
                                      -grep("idate", names(prediction_data)), 
                                      -grep("idesc", names(prediction_data)), 
                                      -match("insp_hours_per_qtr", names(prediction_data)),
                                      -grep("merge", names(prediction_data)), 
                                      -grep("minetype", names(prediction_data)),
                                      -match("minename", names(prediction_data)),
                                      -grep("minesizepoints", names(prediction_data)), 
                                      -match("minestatus", names(prediction_data)), 
                                      -match("minestatusdate", names(prediction_data)), 
                                      -grep("productionshiftsperday", names(prediction_data)),
                                      -grep("row_id", names(prediction_data)),
                                      -match("terminated", names(prediction_data)),
                                      -grep("year", names(prediction_data)))]

######################################################################################################################################

# CLEAN UP OPERATORNAME AND OPERATORID

# Remove any leading or traling whitespace on operator names & make lowercase
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
prediction_data$operatorname = trim(prediction_data$operatorname)
prediction_data$operatorname = tolower(prediction_data$operatorname)

clean_names = function(var) {
  prediction_data[, var] = gsub(",", "", prediction_data[, var])
  prediction_data[, var] = gsub("\\.", "", prediction_data[, var])
  prediction_data[, var] = gsub("-", " ", prediction_data[, var])
  prediction_data[, var] = gsub("\\s\\s", " ", prediction_data[, var])
  prediction_data[, var] = gsub("company", "co", prediction_data[, var])
  prediction_data[, var] = gsub("corporation", "co", prediction_data[, var])
  prediction_data[, var] = gsub("comp", "co", prediction_data[, var])
  prediction_data[, var] = gsub("corp$", "co", prediction_data[, var])
  prediction_data[, var] = gsub("incorporated", "inc", prediction_data[, var])
  prediction_data[, var] = gsub(" and ", " & ", prediction_data[, var])
  prediction_data[, var] = gsub("limited", "ltd", prediction_data[, var])
  prediction_data[, var] = gsub("l.l.c.", "llc", prediction_data[, var])
  prediction_data[, var] = gsub("l l c", "llc", prediction_data[, var])
}
prediction_data$operatorname = clean_names("operatorname")

strip_names = function(var) {
  prediction_data[, var] = gsub(" inc( )*$", "", prediction_data[, var])
  prediction_data[, var] = gsub(" co( )*$", "", prediction_data[, var])
  prediction_data[, var] = gsub(" co( )*inc( )*$", "", prediction_data[, var])
  prediction_data[, var] = gsub(" co( )*llc( )*$", "", prediction_data[, var])
  prediction_data[, var] = gsub(" llc( )*$", "", prediction_data[, var])
  prediction_data[, var] = gsub(" ltd( )*$", "", prediction_data[, var])
}
prediction_data$operatorname = strip_names("operatorname")

# Special clean-up treatment
special_names = function(var) {
  prediction_data[, var] = gsub("little buck coal co # 2", "little buck coal co #2", prediction_data[, var])
  prediction_data[, var] = gsub("gemco energies", "gemco energy", prediction_data[, var])
  prediction_data[, var] = gsub("goodin creek contracting", "goodin creek mining", prediction_data[, var])
  prediction_data[, var] = gsub("j&r", "j & r", prediction_data[, var])
  prediction_data[, var] = gsub("d&d", "d & d", prediction_data[, var])
  prediction_data[, var] = gsub("nu fac mining", "nufac mining", prediction_data[, var])
  prediction_data[, var] = gsub("omega mining  beehive coal", "omega mining", prediction_data[, var])
  prediction_data[, var] = gsub("s e l a h", "selah", prediction_data[, var])
  prediction_data[, var] = gsub("williams bros coal", "williams brothers coal", prediction_data[, var])
  prediction_data[, var] = gsub("rock 'n' roll coal", "rock n roll coal", prediction_data[, var])
  prediction_data[, var] = gsub("road fork development", "roadfork development", prediction_data[, var])
}
prediction_data$operatorname = special_names("operatorname")

# Generate factor identifier for unique oeprators (ignore names from here on out)
prediction_data$operatorid = as.factor(as.numeric(as.factor(prediction_data$operatorname)))
prediction_data = prediction_data[, c(-grep("operatorname", names(prediction_data)))]

######################################################################################################################################

# PRUNE VARIABLE SET 

# Pare away variables with zero variation before model selection and prediction stages
var_stats = describe(prediction_data[, c(-match("mineid", names(prediction_data)), 
                                         -match("stateabbreviation", names(prediction_data)), 
                                         -match("operatorid", names(prediction_data)), 
                                         -match("quarter", names(prediction_data)))])

# Variables are nontrivial (worth keeping) if their standard deviation is greater than zero 
nontriv_vars = rownames(var_stats[var_stats$sd > 0,])
triv_vars = setdiff(names(prediction_data), nontriv_vars)
rm(var_stats)

# Keeps only nontrivial vars. Warning: This excludes all non-numeric variables - wind up with 30,289 obs
prediction_data = prediction_data[, c(nontriv_vars, "mineid", "quarter", "stateabbreviation", "operatorid")]

######################################################################################################################################

# SET UP THE DATA - RENAME AND GROUP VARS TO MAKE ANALYSIS EASIER

# Rename variables
prediction_data = rename(prediction_data, c(coal_prod_qtr = "coal_prod", 
                                            stateabbreviation = "state", 
                                            employment_qtr = "employment",
                                            hours_qtr = "hours",
                                            goodfaithind = "num_good_faith",
                                            onsite_insp_hours_per_qtr = "onsite_insp_hours"))

names(prediction_data)[grep("^[0-9]", names(prediction_data))] = paste("p", names(prediction_data)[grep("^[0-9]", names(prediction_data))], sep = "")
names(prediction_data)[grep("[0-9].[0-9]", names(prediction_data))] = paste("s", names(prediction_data)[grep("[0-9].[0-9]", names(prediction_data))], sep = "")

# Reformat variables
prediction_data$mineid = as.character(prediction_data$mineid)
prediction_data$quarter = as.numeric(prediction_data$quarter)

if (injury.type == "MR"){
  prediction_data$MR_indicator = as.factor(prediction_data$MR_indicator)
}
if (injury.type == "PS"){
  prediction_data$PS_indicator = as.factor(prediction_data$PS_indicator)
}

# Group variables
violation_vars = names(prediction_data)[grep("^p+[0-9]|^sp+[0-9]", names(prediction_data))]
part_vars = violation_vars[grep("^p", violation_vars)]
subpart_vars = violation_vars[grep("^sp", violation_vars)]
sig_sub_vars = violation_vars[grep("sigandsub", violation_vars)]
penalty_point_vars = violation_vars[grep("penaltypoints", violation_vars)]
violation_count_vars = setdiff(violation_vars, union(sig_sub_vars, penalty_point_vars))

part_sig_sub_vars = intersect(part_vars, sig_sub_vars)
subpart_sig_sub_vars = intersect(subpart_vars, sig_sub_vars)
part_penalty_point_vars = intersect(part_vars, penalty_point_vars)
subpart_penalty_point_vars = intersect(subpart_vars, penalty_point_vars)
part_violation_count_vars = intersect(part_vars, violation_count_vars)
subpart_violation_count_vars = intersect(subpart_vars, violation_count_vars)

######################################################################################################################################

# CREATE MINE-TIME VARIABLE (accounts for age of mine)

make_mine_time = function (mine_data) {
  for (i in 1:nrow(mine_data)) {
    if (is.na(mine_data$hours[i])) {
      mine_data$mine_time = NA
    }
    else {
      mine_data$mine_time[i] = i
    }
  }
  return(mine_data)
}
prediction_data$mine_time = integer(nrow(prediction_data))
prediction_data = ddply(prediction_data, "mineid", make_mine_time)

######################################################################################################################################

# DECLARE FUNCTION TO CREATE VARIOUS LEAD/LAGGED VARIABLE CONSTRUCTIONS

# Thanks, R-bloggers! https://www.r-bloggers.com/generating-a-laglead-variables/
shift_helper = function(x, shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  if (length(shift_by) > 1)
    return(sapply(shift_by, shift_helper, x = x))
  out = NULL
  abs_shift_by = abs(shift_by)
  if (shift_by > 0)
    out = c(tail(x, -abs_shift_by), rep(NA, abs_shift_by))
  else if (shift_by < 0)
    out = c(rep(NA, abs_shift_by), head(x, -abs_shift_by))
  else
    out = x
  out
}
shift = function(data, var_to_shift, shifted_var, shift_by) {
  data[, shifted_var] = shift_helper(data[, var_to_shift], shift_by)
  return(data)
}

######################################################################################################################################

# CREATE NON-CUMULATIVE LAGGED VARIABLES (LOOKING BACK ONE QUARTER)

# Group data by mines and order the data by mine-quarters (descending)
prediction_data = prediction_data[order(prediction_data[,"mineid"], -prediction_data[,"quarter"]),]

violation_vars = names(prediction_data)[grep("^p+[0-9][0-9](\\.penaltypoints|\\.sigandsub)*$|^sp+[0-9].+[0-9](\\.penaltypoints|\\.sigandsub)*$", names(prediction_data))]
part_vars = violation_vars[grep("^p[0-9][0-9](\\.penaltypoints|\\.sigandsub)*$", violation_vars)]
subpart_vars = violation_vars[grep("^sp[0-9].+[0-9](\\.penaltypoints|\\.sigandsub)*$", violation_vars)]

# CREATE LAGGED (1) VARS FOR PARTS AND SUBPARTS
for (i in 1:length(violation_vars)) {
  prediction_data[, paste(violation_vars[i], "1lag", sep = ".")] = 0 
  prediction_data = ddply(prediction_data, "mineid", shift, 
                          var_to_shift = violation_vars[i], 
                          shifted_var = paste(violation_vars[i], "1lag", sep = "."), 
                          shift_by = 1)
}

######################################################################################################################################

# CREATE CUMULATIVE LAGGED VARIABLES 

# Generate cumulative violation lag variables (we must lag violations here, because we want to evaluate whether or not 
# patterns of violations predict injuries, not whether incidents of injuries predict patterns of injuries).

# Group data by mines and order the data by mine-quarters (ascending)
prediction_data = prediction_data[order(prediction_data[,"mineid"], prediction_data[,"quarter"]),]

# CREATE FUNCTION TO FIND CUMULATIVE NUMBER OF VIOLATIONS PER MINE SINCE THE FIRST QUARTER IN WHICH THAT MINE WAS OPERATIONAL
# However, this var should be quarter-specific (looking backwards).
viols_so_far = function(data, var_to_sum, sum_viols) {
  data[, sum_viols] =  cumsum(data[, var_to_sum])
  return(data)
}

# CREATE FUNCTION TO GET CUMULATIVE VIOLATIONS LAGS (LOOKING BACK 4 QUARTERS) USING CUMSUM
cs_wrapper = function(data, var_to_cum, cum_var, cum_shift) {
  data[, cum_var] = 0
  
  for (i in 1:nrow(data)) {
    if (i < cum_shift) {
      data[, cum_var][i] = NA
    }
    else {
      data_to_cum = data[which(data$quarter >= data$quarter[i - cum_shift + 1] & data$quarter <= data$quarter[i]), var_to_cum]
      val = tail(cumsum(data_to_cum), n = 1)
      data[, cum_var][i] = val
    }
  }
  return(data)
}
# Now apply the two functions written above to variables of interest.
for (i in 1:length(violation_vars)) {
  #cs_wrapper
  prediction_data = ddply(prediction_data, "mineid", cs_wrapper, 
                          var_to_cum = part_vars[i], 
                          cum_var = paste(part_vars[i], "c.4lag", sep = "."), 
                          cum_shift = 4)
  # viols_so_far
  prediction_data = ddply(prediction_data, "mineid", viols_so_far, 
                          var_to_sum = part_vars[i], 
                          sum_viols = paste(part_vars[i], "c.lag.all", sep = "."))
}
# VIOLATION LAG = 1lag
# VIOLATION CUMULATIVE LAG OVER LAST 4 QUARTERS = c.4lag
# VIOLATION CUMULATIVE LAG SINCE BEGINNING OF MINE-TIME = c.lag.all

######################################################################################################################################

# merge EIA and longwall info
prediction_data = readRDS(PS_prediction_data_out_file_name) # 30289
eia = readRDS(eia_file_name)

prediction_data$quarter = as.yearqtr(prediction_data$quarter, format = "%Y-%q")
prediction_data$year = format(prediction_data$quarter, "%Y")

# merge on mine-year specific union & longwall data
prediction_data = merge(prediction_data, eia, by = c("mineid", "year"), all = T)
prediction_data = prediction_data[!is.na(prediction_data$quarter),]

# replace longwall with zero if it's not a 1 and the year is one for which we have data (2000-2015)
prediction_data$longwall = ifelse(is.na(prediction_data$longwall) & 
                                    prediction_data$year < 2016, 0, prediction_data$longwall)

# replace union with zero if it's not a 1 and the year is one for which we have data (2000-2013)
prediction_data$union = ifelse(is.na(prediction_data$union) & 
                                 prediction_data$year < 2014, 0, prediction_data$union)

######################################################################################################################################

# Set the file name (MR/PS, relevant or all vars, Stata-friendly or not.)
if (relevant.only.option == "on" & injury.type == "MR") {
  saveRDS(prediction_data, file = MR_relevant_prediction_data_out_file_name)
}

if (relevant.only.option == "off" & injury.type == "MR") {
  saveRDS(prediction_data, file = MR_prediction_data_out_file_name)
    if (stata.friendly) {
      # Remove special characters from data names so it's stata-friendly, if option is TRUE
      stata.names = names(prediction_data)
      stata.names = gsub("\\.", "_", stata.names)
      stata.names = gsub("-", "_", stata.names)
      stata.names = gsub("penaltypoints", "pp", stata.names)
      stata.names = gsub("sigandsub", "ss", stata.names)
      stata.data = prediction_data
      names(stata.data) = stata.names
      # Also save a csv for this so that we can do prelimary analysis in Stata (clustering is easier in Stata)
      write.csv(stata.data, file = MR_prediction_data_out_csv_name)
      write.dta(stata.data, file = MR_prediction_data_out_dta_name)
      rm(stata.names, stata.data)
    }
}

if (relevant.only.option == "on" & injury.type == "PS") {
  saveRDS(prediction_data, file = PS_relevant_prediction_data_out_file_name)
}

if (relevant.only.option == "off" & injury.type == "PS") {
  saveRDS(prediction_data, file = PS_prediction_data_out_file_name)
    if (stata.friendly) {
      # Remove special characters from data names so it's stata-friendly, if option is TRUE
      stata.names = names(prediction_data)
      stata.names = gsub("\\.", "_", stata.names)
      stata.names = gsub("-", "_", stata.names)
      stata.names = gsub("penaltypoints", "pp", stata.names)
      stata.names = gsub("sigandsub", "ss", stata.names)
      stata.data = prediction_data
      names(stata.data) = stata.names
      # Also save a csv for this so that we can do prelimary analysis in Stata (clustering is easier in Stata)
      write.csv(stata.data, file = PS_prediction_data_out_csv_name)
      write.dta(stata.data, file = PS_prediction_data_out_dta_name)
      rm(stata.names, stata.data)
    }
}

######################################################################################################################################

# MAKE YEAR DATA

if (make.year.data) {
  # Create year var
  prediction_data$quarter = as.yearqtr(prediction_data$quarter)
  prediction_data$year = as.numeric(format(prediction_data$quarter, "%Y"))
  
  # Create var that will count number of quarters per year
  prediction_data$num_qtrs_year = 1
  
  # Collapse to the mine-year (sums) - only non-lagged vars
  if (injury.type == "PS") {
      prediction_data$injofinterest = prediction_data$PS
      prediction_data$injofinterestindicator = prediction_data$PS_indicator
  }
  if (injury.type == "MR") {
      prediction_data$injofinterest = prediction_data$MR
      prediction_data$injofinterestindicator = prediction_data$MR_indicator
  }
  prediction_year_data = ddply(prediction_data[, c(grep("p", names(prediction_data)),
                                              grep("sp", names(prediction_data)),
                                              grep("hours", names(prediction_data)),
                                              grep("num_qtrs_year", names(prediction_data)),
                                              grep("injofinterest", names(prediction_data)),
                                              match("mineid", names(prediction_data)), 
                                              match("year", names(prediction_data)))], c("mineid", "year"), 
                              function(x) colSums(x[, c(grep("p", names(x)),
                                                        grep("sp", names(x)),
                                                        grep("num_qtrs_year", names(x)),
                                                        grep("hours", names(x)),
                                                        grep("injofinterest", names(x)))], na.rm = T))
  # Merge states back on
  prediction_year_data = merge(prediction_data, prediction_year_data[, c("mineid", "state")], by = c("mineid"))
  
  # Calculate mine-time
  prediction_data = ddply(prediction_data, "mineid", make_mine_time)
  
  # Create 1 lags, lag4, and lag all vars 
  violation_vars = names(prediction_data)[grep("^p+[0-9][0-9](\\.penaltypoints|\\.sigandsub)*$|^sp+[0-9].+[0-9](\\.penaltypoints|\\.sigandsub)*$", names(prediction_data))]
  
      # lag 1
      prediction_data = prediction_data[order(prediction_data[,"mineid"], -prediction_data[,"year"]),]
      violation_vars = names(prediction_data)[grep("^p+[0-9][0-9](\\.penaltypoints|\\.sigandsub)*$|^sp+[0-9].+[0-9](\\.penaltypoints|\\.sigandsub)*$", names(prediction_data))]
      for (i in 1:length(violation_vars)) {
          # shift (lag 1)
          prediction_data[, paste(violation_vars[i], "1lag", sep = ".")] = 0 
          prediction_data = ddply(prediction_data, "mineid", shift, 
                                  var_to_shift = violation_vars[i], 
                                  shifted_var = paste(violation_vars[i], "1lag", sep = "."), 
                                  shift_by = 1)
      }
      prediction_data = prediction_data[order(prediction_data[,"mineid"], prediction_data[,"year"]),]
      for (i in 1:length(violation_vars)) {
           # cs_wrapper (4 lag)
          prediction_data = ddply(prediction_data, "mineid", cs_wrapper, 
                                  var_to_cum = part_vars[i], 
                                  cum_var = paste(part_vars[i], "c.4lag", sep = "."), 
                                  cum_shift = 4)
          # viols_so_far (lag all)
          prediction_data = ddply(prediction_data, "mineid", viols_so_far, 
                                  var_to_sum = part_vars[i], 
                                  sum_viols = paste(part_vars[i], "c.lag.all", sep = "."))
      }
      
  # Save
  if (injury.type == "PS") {
    
    write.csv(prediction_data, file = PS_year_prediction_data_out_csv_name)
  }
  if (injury.type == "MR") {
    write.csv(prediction_data, file = MR_year_prediction_data_out_csv_name)
  }
}

#sink()
######################################################################################################################################

