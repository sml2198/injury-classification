# NIOSH Project 2014-N-15776

# 12 - Prepare Violations
    # Loads merged violations data from "11_merge_violations.R" 
    # Merges violations with prepared CFR subsection codes from "10_clean_cfr_key.R", 
    # Creates subsection specific variables and lagged variables
    # Collapses violations to the mine-quarter level
    # Loads inspections data from "8_clean_inspections.R" and collapses to the mine-quarter level
    # Loads accidents data from "6_collapse_accidents.R"
    # Merges mine-quarter level violations with inspections and accidents

# Last edit 8/10/16

######################################################################################################

library(plyr)
library(zoo)
library(stats)
library(stringr)
library(withr)
library(psych)
library(dplyr)

# define file names
  # input: merged violations data produced in 11_merge_violations.R
merged_violations_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds"
  # input: collapsed coded accidents data (contains MR indicator - no PS indicator yet) produced in 6_collapse_accidents.R
mines_accidents_coded_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/collapsed_accidents.rds"
  # input: cleaned mine-quarters as produced in 1_clean_mines.R
mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds"
  # input: cleaned mine-types key produced in produced in 1_clean_mines.R
mine_types_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"

# When relevant-only option is set to "on" (not commented out) only CFR subsections marked as "relevant" will be used for creating
# vars. Otherwise, "relevant" and "maybe relevant subsections" will be used.
#relevant.only.option = "on"
relevant.only.option = "off"

######################################################################################################################################

merged_violations = readRDS(merged_violations_in_file_name)

# format inspection type
merged_violations$inspacty = tolower(merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "mine idle activity", "mine idle", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "na", "n", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "part 50 audit", "part 50 audits", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "non-fatal accident investigation", "nonfatal injury accident inv", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "shaft, slope or major construction spot inspection", "shft, slpe, or maj constr spot insp", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "103(g)(1) spot umwa inspection", "103(g)(1) spot inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "103(i) spot (ign or expl) insp", "103(i) spot inspections", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "103(i) spot (hazard) inspection", "103(i) spot inspections", merged_violations$inspacty)
merged_violations$inspacty = ifelse(is.na(merged_violations$inspacty), "unknown", merged_violations$inspacty)

# collapse inspection type categories to resonable amount
merged_violations$inspacty = ifelse((merged_violations$inspacty == "regular inspection" |
                                       merged_violations$inspacty == "regular safety and health inspection"), "regular inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(grepl("complaint", merged_violations$inspacty), "complaint inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(merged_violations$inspacty == "fatal accident investigation", "fatality inspection", merged_violations$inspacty)
merged_violations$inspacty = ifelse(grepl("103", merged_violations$inspacty), "103", merged_violations$inspacty)
merged_violations$inspacty = ifelse(grepl("(103|fatality|unknown|regular|complaint)", merged_violations$inspacty), merged_violations$inspacty, "other")

# format violation type
merged_violations$violationtypecode = as.character(merged_violations$violationtypecode)
merged_violations$violationtypecode = ifelse(is.na(merged_violations$violationtypecode), "Unknown", merged_violations[, "violationtypecode"])

# remove violation types with 4 observations
merged_violations = merged_violations[(merged_violations$violationtypecode != "Notice" & merged_violations$violationtypecode != "Safeguard"), ]
merged_violations$assessmenttypecode = as.character(merged_violations$assessmenttypecode)
merged_violations$assessmenttypecode = ifelse(is.na(merged_violations$assessmenttypecode), "Unknown", merged_violations$assessmenttypecode)

# rename likelihood categories and deal with missing values
is.na(merged_violations$likelihood) = merged_violations$likelihood == ""
levels(merged_violations$likelihood) = c("Unknown", "Highly", "NoLikelihood", "Occurred", "Reasonably", "Unlikely")
merged_violations$likelihood = as.character(merged_violations$likelihood)
merged_violations$likelihood = ifelse(is.na(merged_violations$likelihood), "Unknown", merged_violations$likelihood)

# rename injuryillness categories and deal with missing values
merged_violations$injuryillness = as.character(merged_violations$injuryillness)
is.na(merged_violations$injuryillness) = merged_violations$injuryillness == ""
levels(merged_violations$injuryillness) = c("Unknown", "Fatal", "LostDays", "NoLostDays", "Permanent")
merged_violations$injuryillness = ifelse(is.na(merged_violations$injuryillness), "Unknown", merged_violations$injuryillness)

# rename negligence categories and deal with missing values
is.na(merged_violations$negligence) = merged_violations$negligence == ""
levels(merged_violations$negligence) = c("Unknown", "HighNegligence", "LowNegligence", "ModNegligence", "NoNegligence", "Reckless")
merged_violations$negligence = as.character(merged_violations$negligence)
merged_violations$negligence = ifelse(is.na(merged_violations$negligence), "Unknown", merged_violations$negligence)

######################################################################################################################################

# function to dummy out vairables
datdum = function(var_name, data, data_name) {
  data$rv = rnorm(nrow(data), 1, 1)
  mm = data.frame(model.matrix(lm(data$rv ~ -1 + factor(data[, var_name]))))
  data$rv = NULL
  data[, var_name] = NULL
  names(mm) = paste(var, 1:ncol(mm), sep = ".")
  data = cbind(data, mm)
  assign(data_name, data, .GlobalEnv) 
}

# dummy out categorical variables
dum_vars = c("inspacty", 
             "assessmenttypecode", 
             "violationtypecode", 
             "likelihood", 
             "injuryillness", 
             "negligence")

for (var in dum_vars) {
  datdum(var, merged_violations, "merged_violations")
}

# memory
rm(dum_vars, var)

######################################################################################################################################

# PREPARE TO GENERATE PART AND SUBSECTION SPECIFIC VARIABLES

# Dummy out CFR codes (at the subpart and subsection levels) only for *relevant types and mark all non-relevant CFR codes
if (relevant.only.option == "on") {
    MR_relevant_subsectcodes = levels(factor(merged_violations[merged_violations$MR_relevant == 1, ]$subsection_code))
}
if (relevant.only.option != "on") {
    MR_relevant_subsectcodes = levels(factor(merged_violations[merged_violations$MR_relevant == 1 | merged_violations$MR_maybe_relevant == 1,]$subsection_code))
}

#  Eventually we can create loop to go through each part (of cfr codes) and create lists of relevent/maybe relevant 
# subsections within that part (instead of this mess) 
if (relevant.only.option != "on") {
    MR_relevant_subsectcodes_47 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("47\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_48 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("48\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_71 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("71\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_72 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("72\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75a = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.1[0-3]", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75b = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.14", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75c = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.1[5-9]", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75d = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.[2-4]", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75e = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.5", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75f = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.[6-7]", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75g = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.8", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75h = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.9", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("75\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_77 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 | 
                merged_violations$MR_maybe_relevant == 1) & (grepl("77\\.", merged_violations[,"subsection_code"])),]$subsection_code))
}
if (relevant.only.option == "on") {
    MR_relevant_subsectcodes_47 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 ) 
            & (grepl("47\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_48 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 ) 
            & (grepl("48\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_71 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 ) 
            & (grepl("71\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_72 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 ) 
            & (grepl("72\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_75 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 ) 
            & (grepl("75\\.", merged_violations[,"subsection_code"])),]$subsection_code))
    MR_relevant_subsectcodes_77 = levels(factor(merged_violations[(merged_violations$MR_relevant == 1 ) 
            & (grepl("77\\.", merged_violations[,"subsection_code"])),]$subsection_code))
}

# Added by Sarah 7/5/2016 - these violations have so few observations (< 15) they are not worth analyzing.
# Removing them saves memory.
remove_subcodes = c("75.1431", "75.1436", "75.1438", "75.151", "75.153", "75.155", "75.156", "75.160", "75.1721", "75.1727", "75.1728", 
                    "75.341", "75.402-2", "75.500-1", "75.502", "75.503-1", "75.505", "75.508-1", "75.510", "75.510-1", "75.511-1", "75.512-1", 
                    "75.516-1", "75.517-1", "75.517-2", "75.519", "75.522", "75.524", "75.600", "75.601-2", "75.601-3","75.700-1", "75.701-4", 
                    "75.702", "75.702-1", "75.703-1", "75.703-2", "75.703-4", "75.704", "75.705", "75.705-1", "75.705-2", "75.705-3", "75.705-8", 
                    "75.800-2", "75.801", "75.803-2", "75.805", "75.806", "75.812", "75.812-2", "75.814", "75.815", "75.817", "75.818", "75.819", 
                    "75.820", "75.825", "75.827", "75.830", "75.831", "75.832", "75.833", "75.834", "75.900-2", "75.902-1", "75.905", "75.906")
if (relevant.only.option == "on") {
  MR_relevant_subsectcodes_75a = setdiff(MR_relevant_subsectcodes_75a, remove_subcodes)
  MR_relevant_subsectcodes_75b = setdiff(MR_relevant_subsectcodes_75b, remove_subcodes)
  MR_relevant_subsectcodes_75c = setdiff(MR_relevant_subsectcodes_75c, remove_subcodes)
  MR_relevant_subsectcodes_75d = setdiff(MR_relevant_subsectcodes_75d, remove_subcodes)
  MR_relevant_subsectcodes_75e = setdiff(MR_relevant_subsectcodes_75e, remove_subcodes)
  MR_relevant_subsectcodes_75f = setdiff(MR_relevant_subsectcodes_75f, remove_subcodes)
  MR_relevant_subsectcodes_75g = setdiff(MR_relevant_subsectcodes_75g, remove_subcodes)
  MR_relevant_subsectcodes_75h = setdiff(MR_relevant_subsectcodes_75h, remove_subcodes)
}
MR_relevant_subsectcodes_75 = setdiff(MR_relevant_subsectcodes_75, remove_subcodes)
MR_relevant_subsectcodes = setdiff(MR_relevant_subsectcodes, remove_subcodes)

# Create lists of number of dummies for violation, assessment, and inspection types 
likelihoodcodes = seq(1, 6)
injuryillnesscodes = seq(1, 5)
negligencecodes = seq(1, 6)
violationtypecodes = seq(1, 2)
assessmenttypecodes = seq(1, 3)
inspactycodes = seq(1, 5)

######################################################################################################################################

# LOOPS TO GENERATE PART AND SUBSECTION SPECIFIC VARIABLES

# For CFR part-specific variable creation
if (relevant.only.option != "on") {
    MR_relevant_partcodes = levels(factor(merged_violations[merged_violations$MR_relevant == 1 | merged_violations$MR_maybe_relevant == 1,]$cfr_part_code))
}
if (relevant.only.option == "on") {
    MR_relevant_partcodes = levels(factor(merged_violations[merged_violations$MR_relevant == 1,]$cfr_part_code))
}

# Sets the level of CFR code for which to create code-specific variables
cfr_codes = MR_relevant_partcodes

# This loop creates the part-specific variable-specific dummies (which will later be collapsed to the mine-quarter level).
for (i in 1:length(cfr_codes)) {
  merged_violations[, cfr_codes[i]] = ifelse(merged_violations$cfr_part_code == cfr_codes[i], 1, 0)
  merged_violations[, paste(cfr_codes[i], "penaltypoints", sep = ".")] = apply(cbind(merged_violations[, "penaltypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
    # There is also a factor var likelihood which marks the severity of negligence e.g., reasonably, unlikely, ...
  merged_violations[, paste(cfr_codes[i], "gravitylikelihoodpoints", sep = ".")] = apply(cbind(merged_violations[, "gravitylikelihoodpoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "gravityinjurypoints", sep = ".")] = apply(cbind(merged_violations[, "gravityinjurypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "gravitypersonspoints", sep = ".")] = apply(cbind(merged_violations[, "gravitypersonspoints"], merged_violations[, cfr_codes[i]]), 1, prod)
    # There is also a factor var negligence which marks the severity of negligence e.g., low, high, ...
  merged_violations[, paste(cfr_codes[i], "negligencepoints", sep = ".")] = apply(cbind(merged_violations[, "negligencepoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "sigandsubdesignation", sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1, merged_violations[, "sigandsubdesignation"], 0)
  merged_violations[, paste(cfr_codes[i], "contractor_violation_cnt", sep = ".")] = apply(cbind(merged_violations[, "contractor_violation_cnt"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "operator_violation_pInspDay", sep = ".")] = apply(cbind(merged_violations[, "operator_violation_pInspDay"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "contractor_repeated_viol_cnt", sep = ".")] = apply(cbind(merged_violations[, "contractor_repeated_viol_cnt"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "operator_repeated_viol_pInspDay", sep = ".")] = apply(cbind(merged_violations[, "operator_repeated_viol_pInspDay"], merged_violations[, cfr_codes[i]]), 1, prod)
    # Dummied out categorical vars:
    for (j in 1:length(inspactycodes)) {
      merged_violations[, paste(cfr_codes[i], "inspacty", inspactycodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("inspacty", inspactycodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(violationtypecodes)) {
      merged_violations[, paste(cfr_codes[i], "violationtypecode", violationtypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("violationtypecode", violationtypecodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(assessmenttypecodes)) {
      merged_violations[, paste(cfr_codes[i], "assessmenttypecode", assessmenttypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("assessmenttypecode", assessmenttypecodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(likelihoodcodes)) {
      merged_violations[, paste(cfr_codes[i], "likelihood", likelihoodcodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("likelihood", likelihoodcodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(injuryillnesscodes)) {
      merged_violations[, paste(cfr_codes[i], "injuryillness", injuryillnesscodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("injuryillness", injuryillnesscodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(negligencecodes)) {
      merged_violations[, paste(cfr_codes[i], "negligence", negligencecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("negligence", negligencecodes[j], sep = ".")] == 1, 1, 0)
    }
}

# Set this line below to be the subsection code group you want. Cannot do all at once because of memory issues.
cfr_codes = MR_relevant_subsectcodes

# This loop creates the subsection-specific variable-specific dummies (which will later be collapsed to the mine-quarter level).
for (i in 1:length(cfr_codes)) {
  merged_violations[, cfr_codes[i]] = ifelse(merged_violations$subsection_code == cfr_codes[i], 1, 0)
  merged_violations[, paste(cfr_codes[i], "penaltypoints", sep = ".")] = apply(cbind(merged_violations[, "penaltypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
    # There is also a factor var likelihood which marks the severity of likelihood e.g., reasonably, unlikely, ...
  merged_violations[, paste(cfr_codes[i], "gravitylikelihoodpoints", sep = ".")] = apply(cbind(merged_violations[, "gravitylikelihoodpoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "gravityinjurypoints", sep = ".")] = apply(cbind(merged_violations[, "gravityinjurypoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "gravitypersonspoints", sep = ".")] = apply(cbind(merged_violations[, "gravitypersonspoints"], merged_violations[, cfr_codes[i]]), 1, prod)
    # There is also a factor var negligence which marks the severity of negligence e.g., low, high, ...
  merged_violations[, paste(cfr_codes[i], "negligencepoints", sep = ".")] = apply(cbind(merged_violations[, "negligencepoints"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "sigandsubdesignation", sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1, merged_violations[, "sigandsubdesignation"], 0)
  merged_violations[, paste(cfr_codes[i], "contractor_violation_cnt", sep = ".")] = apply(cbind(merged_violations[, "contractor_violation_cnt"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "operator_violation_pInspDay", sep = ".")] = apply(cbind(merged_violations[, "operator_violation_pInspDay"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "contractor_repeated_viol_cnt", sep = ".")] = apply(cbind(merged_violations[, "contractor_repeated_viol_cnt"], merged_violations[, cfr_codes[i]]), 1, prod)
  merged_violations[, paste(cfr_codes[i], "operator_repeated_viol_pInspDay", sep = ".")] = apply(cbind(merged_violations[, "operator_repeated_viol_pInspDay"], merged_violations[, cfr_codes[i]]), 1, prod)
    # Dummied out categorical vars
    for (j in 1:length(inspactycodes)) {
      merged_violations[, paste(cfr_codes[i], "inspacty", inspactycodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("inspacty", inspactycodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(violationtypecodes)) {
      merged_violations[, paste(cfr_codes[i], "violationtypecode", violationtypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("violationtypecode", violationtypecodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(assessmenttypecodes)) {
      merged_violations[, paste(cfr_codes[i], "assessmenttypecode", assessmenttypecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("assessmenttypecode", assessmenttypecodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(likelihoodcodes)) {
      merged_violations[, paste(cfr_codes[i], "likelihood", likelihoodcodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("likelihood", likelihoodcodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(injuryillnesscodes)) {
      merged_violations[, paste(cfr_codes[i], "injuryillness", injuryillnesscodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("injuryillness", injuryillnesscodes[j], sep = ".")] == 1, 1, 0)
    }
    for (j in 1:length(negligencecodes)) {
      merged_violations[, paste(cfr_codes[i], "negligence", negligencecodes[j], sep = ".")] = ifelse(merged_violations[, cfr_codes[i]] == 1 & merged_violations[, paste("negligence", negligencecodes[j], sep = ".")] == 1, 1, 0)
    }
}

# Remove things we won't use again.
rm(MR_relevant_subsectcodes, MR_relevant_subsectcodes_47, MR_relevant_subsectcodes_48,
   MR_relevant_subsectcodes_71, MR_relevant_subsectcodes_72, MR_relevant_subsectcodes_75a,
   MR_relevant_subsectcodes_75b, MR_relevant_subsectcodes_75c, MR_relevant_subsectcodes_75d,
   MR_relevant_subsectcodes_75e, MR_relevant_subsectcodes_75, MR_relevant_subsectcodes_77) 

######################################################################################################################################

# COLLAPSE VIOLATIONS DATA TO THE MINE-QUARTER LEVEL

# Create variables to sum for violation counts, and create indicator for if violation was terminated (all should be terminated)
merged_violations$total_violations = 1
merged_violations$terminated = ifelse(merged_violations$typeoftermination == "Terminated", 1, 0)

# Select all variables to sum when we collapse to the mine-quarter lever (the first regex will grab all vars created above)
violations_to_sum = merged_violations[, c(grep("^[0-9][0-9]", names(merged_violations)), 
                                          match("total_violations", names(merged_violations)),
                                          match("excessive_history_ind", names(merged_violations)), 
                                          match("goodfaithind", names(merged_violations)), 
                                          match("terminated", names(merged_violations)),
                                          match("mineid", names(merged_violations)), 
                                          match("quarter", names(merged_violations)))]

# These were grabbed by the regular expression above, but we want to average (not sum) these, so we remove them  
violations_to_sum = violations_to_sum[, c(-grep("operator_repeated_viol_pInspDay", names(violations_to_sum)), 
                                          -grep("contractor_repeated_viol_cnt", names(violations_to_sum)))]

# Collapse the variables that we need to sum.
summed_violations = ddply(violations_to_sum, c("mineid", "quarter"), function(x) colSums(x[, c(grep("^[0-9][0-9]", names(x)), 
                                                                                               match("total_violations", names(x)),
                                                                                               match("excessive_history_ind", names(merged_violations)), 
                                                                                               match("goodfaithind", names(merged_violations)), 
                                                                                               match("terminated", names(x)))], na.rm = T))
# Collapse the variables that we need to average.
averaged_violations = ddply(merged_violations[, c(grep("operator_repeated_viol_pInspDay", names(merged_violations)), 
                                                  grep("minesizepoints", names(merged_violations)), 
                                                  grep("controllersizepoints", names(merged_violations)),
                                                  grep("contractorsizepoints", names(merged_violations)), 
                                                  grep("contractor_repeated_viol_cnt", names(merged_violations)),
                                                  match("exlate_interest_amt", names(merged_violations)), 
                                                  match("mineid", names(merged_violations)), 
                                                  match("quarter", names(merged_violations)))], c("mineid", "quarter"), 
                                    function(x) colMeans(x[, c(grep("operator_repeated_viol_pInspDay", names(x)), 
                                                               grep("minesizepoints", names(x)), 
                                                               grep("controllersizepoints", names(x)),
                                                               grep("contractorsizepoints", names(x)), 
                                                               grep("contractor_repeated_viol_cnt", names(x)),
                                                               match("exlate_interest_amt", names(x)))], na.rm = T))

rm(violations_to_sum)
# Fun fact! Inspections generate both contractor and operator violations. - April Ramirez @ DOL, 6/6/16

######################################################################################################################################

# GRAB CONTRACTOR VARS AND COLLAPSE TO MINE-QUARTER LEVEL

contractor_vars = merged_violations[, c(match("mineid", names(merged_violations)),
                                        match("quarter", names(merged_violations)),
                                        match("contractorid", names(merged_violations)),
                                        match("con_avg_employee_cnt_qtr", names(merged_violations)),
                                        match("con_employee_hours_qtr", names(merged_violations)),
                                        match("con_coal_prod_qtr", names(merged_violations)))]
# Remove obs without contractor ID
contractor_vars = contractor_vars[!is.na(contractor_vars$contractorid),]

# Remove obs that never merged with production/employment info
contractor_vars = contractor_vars[!is.na(contractor_vars$con_avg_employee_cnt_qtr),]

# Collapse contractor vars to mine quarter level 
contractor_vars = ddply(contractor_vars[, c(grep("mineid", names(contractor_vars)), grep("quarter", names(contractor_vars)), 
                                            grep("contractorid", names(contractor_vars)),
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

# Flag observations that do and don't merge - this was used to tab the merge & compare to Stata as a sanity check
collapsed_violations[, "merge1"] = ifelse(!is.na(collapsed_violations$row_id.y) & !is.na(collapsed_violations$row_id.x), 3, 0)
collapsed_violations[, "merge1"] = ifelse(is.na(collapsed_violations$row_id.x) & !is.na(collapsed_violations$row_id.y), 2, collapsed_violations[, "merge1"])
collapsed_violations[, "merge1"] = ifelse(is.na(collapsed_violations$row_id.y) & !is.na(collapsed_violations$row_id.x), 1, collapsed_violations[, "merge1"])
collapsed_violations = collapsed_violations[, -grep("row_id", names(collapsed_violations))]
collapsed_violations$row_id = seq.int(nrow(collapsed_violations))

######################################################################################################################################

# MERGE VIOLATIONS DATA ONTO MINES

# Read in data
mines_quarters = readRDS(mines_quarters_file_name)

# Merge mine-specific contractor info onto mine quarters
merged_mines_violations = merge(mines_quarters, contractor_vars, by = c("mineid", "quarter"), all = T)

# Merge all violation vars onto mine quarters
merged_mines_violations = merge(mines_quarters, collapsed_violations, by = c("mineid", "quarter"), all = T)

# To save time we save/load all constituent datasets in the prediction data after each of the following merges
merged_mines_violations = merged_mines_violations[, -grep("row_id", names(merged_mines_violations))]
merged_mines_violations$row_id = seq.int(nrow(merged_mines_violations))

rm(contractor_vars, collapsed_violations)
gc()

######################################################################################################################################

# MERGE ACCIDENTS DATA ONTO VIOLATIONS/PER QUARTER

summed_coded_accidents = readRDS(mines_accidents_coded_file_name)

# Merge violations (now with contractor and mine info) and accidents
merged_mines_violations_accidents = merge(merged_mines_violations, summed_coded_accidents, by = c("mineid", "quarter"), all = T)

# Replace missings (mine quarters without violations or accidents data) with zeroes
merged_mines_violations_accidents$totalinjuries = ifelse(is.na(merged_mines_violations_accidents$totalinjuries), 0, merged_mines_violations_accidents$totalinjuries)
merged_mines_violations_accidents$total_violations = ifelse(is.na(merged_mines_violations_accidents$total_violations), 0, merged_mines_violations_accidents$total_violations)

merged_mines_violations_accidents = merged_mines_violations_accidents[, -grep("row_id", names(merged_mines_violations_accidents))]
merged_mines_violations_accidents$row_id = seq.int(nrow(merged_mines_violations_accidents))
summed_inspcs$row_id = seq.int(nrow(summed_inspcs))

rm(merged_mines_violations, summed_coded_accidents)
gc()

######################################################################################################################################

# MERGE ON FINAL INSPECTION CHARACTERISTICS AND CLEAN UP FINAL PREDICTION DATASET

# Merge violations & accidents with remaining inspections data
prediction_data = merge(merged_mines_violations_accidents, summed_inspcs, by = c("mineid", "quarter"), all = T)

rm(merged_mines_violations_accidents,summed_inspcs)
gc()

# Same drill here - just a sanity check 
prediction_data[, "merge3"] = ifelse(!is.na(prediction_data$row_id.y) & !is.na(prediction_data$row_id.x), 3, 0)
prediction_data[, "merge3"] = ifelse(is.na(prediction_data$row_id.x) & !is.na(prediction_data$row_id.y), 2, prediction_data[, "merge3"])
prediction_data[, "merge3"] = ifelse(is.na(prediction_data$row_id.y) & !is.na(prediction_data$row_id.x), 1, prediction_data[, "merge3"])

# Remove observations that are missing key variables or are from the wrong environment (there shouldn't be any at this point, but just in case)
prediction_data = prediction_data[complete.cases(prediction_data$quarter),]
prediction_data = prediction_data[complete.cases(prediction_data$minetype),]
prediction_data = prediction_data[prediction_data$minetype == "Underground",]
prediction_data = prediction_data[prediction_data$coalcormetalmmine == "C",]
prediction_data = prediction_data[prediction_data$coal_prod_qtr != 0,]

# Drop unnecessary vars
prediction_data = prediction_data[, c(-grep("merge", names(prediction_data)), -grep("row_id", names(prediction_data)), 
                                      -grep("coalcormetalmmine", names(prediction_data)), -grep("minetype", names(prediction_data)))]

######################################################################################################################################

# FINAL VARIABLE CLEANING AND PREP

# Replace categorical variables with numeric levels
prediction_data$year = factor(prediction_data$year)
prediction_data$minestatus = ifelse(prediction_data$minestatus == "Abandoned", 1, 
                                    ifelse(prediction_data$minestatus == "Abandoned and Sealed", 2, 
                                           ifelse(prediction_data$minestatus == "Active", 3, 
                                                  ifelse(prediction_data$minestatus == "Intermittent", 4,
                                                         ifelse(prediction_data$minestatus == "New Mine", 5,
                                                                ifelse(prediction_data$minestatus == "NonProducing", 6, 
                                                                       ifelse(prediction_data$minestatus == "Temporarily Idled", 7, NA)))))))

prediction_data$idesc = ifelse(prediction_data$idesc == "Hazard", 1, 
                               ifelse(prediction_data$idesc == "Ignition or Explosion", 2, 
                                      ifelse(prediction_data$idesc == "Inspect Once Every 10-days", 3, 
                                             ifelse(prediction_data$idesc == "Inspect Once Every 15-days", 4,
                                                    ifelse(prediction_data$idesc == "Inspect Once Every 5-days", 5,
                                                           ifelse(prediction_data$idesc == "Never Had 103I Status", 6, 
                                                                  ifelse(prediction_data$idesc == "Removed From 103I Status", 7, NA)))))))

######################################################################################################################################

# MAKE MINE AND QUARTER DUMMIES

# # Create mine/quarter specific dummies: function
# datdum <- function(x, data, name){
#   data$rv <- rnorm(dim(data)[1],1,1)
#   mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
#   names(mm) <- paste(name,1:dim(mm)[2],sep=".")
#   data$rv <- NULL
#   data <- cbind(data,mm)
#   return(data)
# }

# dummy out categorical variables
# dum_vars = c("mineid", 
#              "quarter")
# 
# for (var in dum_vars) {
#   datdum(var, prediction_data, "prediction_data")
# }
# 
# rm(dum_vars, var)

# # Create mine/quarter specific dummies: apply function and merge dummies datasets (then remove them)
# test.data1 <- datdum(x="mineid",data=prediction_data,name="mine")
# test.data1 <- test.data1[, c(grep("^mine\\.", names(test.data1)))]
# test.data2 <- datdum(x="quarter",data=prediction_data,name="quart")
# test.data2 <- test.data2[, c(grep("^quart\\.", names(test.data2)))]
# prediction_data = cbind(prediction_data, test.data1, test.data2)
# rm(test.data1, test.data2)

######################################################################################################################################

# FILL IN MISSING VALUES OF MINE CHARACTERISTICS BY MINE_ID/QUARTER GROUPS 

# Group and order the data by mine-quarter
prediction_data = group_by(prediction_data, mineid, quarter)
prediction_data = prediction_data[order(prediction_data$mineid, prediction_data$quarter, na.last = T),]

# na.locf pipes the first non-missing value into NA's by mine-quarter group
prediction_data$minename = na.locf(prediction_data$minename)
prediction_data$minesizepoints = na.locf(prediction_data$minesizepoints)
prediction_data$controllersizepoints = na.locf(prediction_data$controllersizepoints)
prediction_data$contractorsizepoints = na.locf(prediction_data$contractorsizepoints)
prediction_data$hours_qtr = na.locf(prediction_data$hours_qtr)
prediction_data$employment_qtr = na.locf(prediction_data$employment_qtr)
prediction_data$coal_prod_qtr = na.locf(prediction_data$coal_prod_qtr)
prediction_data$productionshiftsperday = na.locf(prediction_data$productionshiftsperday)
prediction_data$idesc = na.locf(prediction_data$idesc)

# Pipe in zeroes to the missing part-specific variables (if nothing merged on a mine-quarter then it should be a zero)
number_to_zero = prediction_data[, c(grep("^[0-9][0-9]", names(prediction_data)), 
                                     match("mineid", names(prediction_data)),
                                     match("quarter", names(prediction_data)), 
                                     match("terminated", names(prediction_data)),
                                     match("total_violations", names(prediction_data)), 
                                     match("contractor_repeated_viol_cnt", names(prediction_data)), 
                                     match("totalinjuries", names(prediction_data)), 
                                     match("MR", names(prediction_data)),
                                     match("insp_hours_per_qtr", names(prediction_data)), 
                                     match("onsite_insp_hours_per_qtr", names(prediction_data)),
                                     match("num_insp", names(prediction_data)))]

# Remove these from the prediction dataset (they'll be merged back in soon once zeroes are inserted)
prediction_data = prediction_data[, c(-grep("^[0-9][0-9]", names(prediction_data)), 
                                      -match("terminated", names(prediction_data)),
                                      -match("total_violations", names(prediction_data)), 
                                      -match("contractor_repeated_viol_cnt", names(prediction_data)), 
                                      -match("totalinjuries", names(prediction_data)), 
                                      -match("MR", names(prediction_data)),
                                      -match("insp_hours_per_qtr", names(prediction_data)), 
                                      -match("onsite_insp_hours_per_qtr", names(prediction_data)),
                                      -match("num_insp", names(prediction_data)))]

# Replace missings in the number-to-zero group with zeroes.
number_to_zero[is.na(number_to_zero)] = 0

# Merge all vars back together now.
prediction_data = merge(prediction_data, number_to_zero, by = c("mineid", "quarter"), all = T)
rm(number_to_zero)

# Now replace any NA's in remaining numeric vars by randomly sampling from the distribution of the column
var_classes = sapply(prediction_data[,names(prediction_data)], class)
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])
for (i in 1:length(num_vars)) {
  i_rowsmissing = row.names(prediction_data)[is.na(prediction_data[, num_vars[i]])]
  while (sum(!complete.cases(prediction_data[, num_vars[i]])) > 0) {
    replace_rows = sample(setdiff(row.names(prediction_data), i_rowsmissing), length(i_rowsmissing), replace = T)
    prediction_data[i_rowsmissing, num_vars[i]] = prediction_data[replace_rows, num_vars[i]]
  }
}

#####################################################################################################################################

# FINAL VARIABLE CLEANING AND PREP

# Add variables for binary and proportional dependent vars
prediction_data$MR_indicator = ifelse(prediction_data$MR > 0, 1, 0)
prediction_data$MR_proportion = prediction_data$MR / prediction_data$totalinjuries

# All violations should be terminated, meaning the issue has been abated by the violator. A violation that has not been terminated is
# therefore an indication of mine/operator negligence. Rather than include the number of terminations as a variable in our models
# (which would yield collinearity because the # of terminations closely tracks the # of violations), we create an indicator for mine
# quarters that have any non-terminated violations.  
prediction_data$no_terminations = ifelse(prediction_data$terminated < prediction_data$total_violations, 1, 0)

# This var is the number of non-terminated violations in a mine quarter. Probably can't be used due to collinearity, but not sure. 
prediction_data$num_no_terminations = ifelse(prediction_data$terminated < prediction_data$total_violations, 
                                             prediction_data$total_violations-prediction_data$terminated, 0)

#####################################################################################################################################

# CREATE LAGGED VARS

# Create lagged variables of various orders (currently code exists for 6 orders)
# Currently we're doing this for part 75 only (the biggest part) and for vars that we believe to be strong predictors, based on model
# selection. This includes penalty points, sig and sub designation, violation counts, MR counts, and one subsection (just as a test - randomly selected)
prediction_data = as.data.table(prediction_data[order(prediction_data$mineid, prediction_data$quarter, na.last = T),])
prediction_data[, c("75.penaltypoints_l1", "75.penaltypoints_l2", "75.penaltypoints_l3", "75.penaltypoints_l4", "75.penaltypoints_l5", "75.penaltypoints_l6",
                    "75.sigandsubdesignation_l1", "75.sigandsubdesignation_l2", "75.sigandsubdesignation_l3", "75.sigandsubdesignation_l4", "75.sigandsubdesignation_l5", "75.sigandsubdesignation_l6",
                    "75_l1", "75_l2", "75_l3", "75_l4", "75_l5", "75_l6",
                    "75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4", "75.1405_l5", "75.1405_l6",
                    "MR_l1", "MR_l2", "MR_l3", "MR_l4", "MR_l5", "MR_l6", 
                    "MR_indicator_l1", "MR_indicator_l2", "MR_indicator_l3", "MR_indicator_l4", "MR_indicator_l5", "MR_indicator_l6",
                    "MR_proportion_l1", "MR_proportion_l2", "MR_proportion_l3", "MR_proportion_l4","MR_proportion_l5","MR_proportion_l6") := shift(.SD, 1:6), 
                by = mineid, .SDcols = c("75.penaltypoints", "75.sigandsubdesignation", "75", "75.1405", "MR", "MR_indicator", "MR_proportion")]
prediction_data = as.data.frame(prediction_data)

######################################################################################################################################

# CREATE COMPOUND LAGGED VARS

# Create compound lagged vars: in this formulation, order 6 will be the sum of all violations (or other lagged var) over 
# the last 6 quarters, instead of simply the value of that var 6 quarters ago.
prediction_data$sum_75_1_2 = rowSums(prediction_data[,c("75_l1", "75_l2")])
prediction_data$sum_75_1_3 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3")])
prediction_data$sum_75_1_4 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3", "75_l4")])
prediction_data$sum_75_1_5 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3", "75_l4", "75_l5")])
prediction_data$sum_75_1_6 = rowSums(prediction_data[,c("75_l1", "75_l2", "75_l3", "75_l4", "75_l5", "75_l6")])

# Same process for the subsection var that we're working with right now.
prediction_data$sum_75.1405_1_2 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2")])
prediction_data$sum_75.1405_1_3 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3")])
prediction_data$sum_75.1405_1_4 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4")])
prediction_data$sum_75.1405_1_5 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4", "75.1405_l5")])
prediction_data$sum_75.1405_1_6 = rowSums(prediction_data[,c("75.1405_l1", "75.1405_l2", "75.1405_l3", "75.1405_l4", "75.1405_l5", "75.1405_l6")])

######################################################################################################################################

# PRUNE VARIABLE SET 

# Pare away variables with zero variation before model selection and prediction stages
var_stats = describe(prediction_data[, c(-match("mineid", names(prediction_data)), 
                                         -match("quarter", names(prediction_data)), 
                                         -match("year", names(prediction_data)),
                                         -match("minename", names(prediction_data)), 
                                         -match("minestatusdate", names(prediction_data)), 
                                         -match("operatorid", names(prediction_data)),
                                         -match("operatorname", names(prediction_data)), 
                                         -match("stateabbreviation", names(prediction_data)), 
                                         -match("idate", names(prediction_data)))])

# Variables are nontrivial (worth keeping) if their standard deviation is greater than zero 
nontriv_vars = rownames(var_stats[var_stats$sd > 0,])
triv_vars = setdiff(names(prediction_data), nontriv_vars)

# Keeps only nontrivial vars. Warning: This excludes all non-numeric variables
prediction_data = prediction_data[, c(nontriv_vars, "mineid", "quarter")]

# This line will report number of missings per var - should be zero (except lagged vars which will be missing for first quarter)!
apply(is.na(prediction_data),2,sum)

######################################################################################################################################

# Set the file name (if part-specific).
if (relevant.only.option == "on") {
  saveRDS(prediction_data, file = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_relevant.rds")
}
if (relevant.only.option != "on") {
  #saveRDS(prediction_data, file = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/prediction_data.rds")
  saveRDS(prediction_data, file = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_part_level.rds")
}
  
######################################################################################################################################

# REFERENCE INFO

# Here we've tabbed our categorical vars, so we know which value will become which dummy.
#table(merged_violations$inspacty)
#103           complaint inspection  fatality inspection        other         regular inspection         unknown 
#35583                 4519                  732                69825               657741                97816

#table(merged_violations$violationtypecode)
#Citation    Order  Unknown 
#830340    11155    24717 

#table(merged_violations$assessmenttypecode)
#Regular  Single Special Unknown 
#640735  187678   13082   24717 

#table(merged_violations$likelihood)
#Highly NoLikelihood     Occurred   Reasonably      Unknown     Unlikely 
#5472        23386         1475       271986        18984       544909 

#table(merged_violations$injuryillness)
#Fatal   LostDays NoLostDays  Permanent    Unknown 
#57634     584733     120395      84464      18986 

#table(merged_violations$negligence)
#HighNegligence  LowNegligence  ModNegligence   NoNegligence       Reckless        Unknown 
#36421          97549         711082           1221            959          18980 

######################################################################################################################################

