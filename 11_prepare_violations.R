# NIOSH Project 2014-N-15776

# 11 - Prepare Violations
    # Loads merged violations data from "9_merge_violations.R" 
    # Merges violations with prepared CFR subsection codes from "prepare_cfr_key.R", 
    # Creates subsection specific variables and lagged variables
    # Collapses violations to the mine-quarter level
    # Loads inspections data from "8_clean_inspections.R" and collapses to the mine-quarter
    # Loads accidents data from 5_analyze_MR_R.R and collapses to the mine-quarter level
    # Merges mine-quarter level violations with inspections and accidents

# Last edit 7/20/16

######################################################################################################

library(plyr)
library(zoo)
library(stats)
library(stringr)
library(withr)
library(psych)

# define file names
  # input: merged violations data produced in 9_merge_violations.R
merged_violations.in.file.name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_violations.rds"
  # input: merged cfr key data produced in 10_prepare_cfr_key.R
merged_cfr_key.file.name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_cfr_key.rds"
 # input: coded accidents data (contains MR indicator - no PS indicator yet) produced in 5_analyze_MR.R
mines_accidents_coded.file.name = "X:/Projects/Mining/NIOSH/analysis/data/4_coded/accidents_with_predictions.csv"
  # input: cleaned mine-quarters as produced in 1_clean_mines.R
mines_quarters.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds"
  # input: cleaned mine-types key produced in produced in 1_clean_mines.R
mine.types.file.name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"

# When relevant-only option is set to "on" (not commented out) only CFR subsections marked as "relevant" will be used for creating
# vars. Otherwise, "relevant" and "maybe relevant subsections" will be used.
#relevant.only.option = "on"
relevant.only.option = "off"

######################################################################################################################################

# MERGE CFR CODES ONTO VIOLATIONS AND MAKE VARIABLES FOR COLLAPSING ON

# Read data files
merged_violations = readRDS(merged_violations.in.file.name)
merged_cfr_key = readRDS(merged_cfr_key.file.name)

# Format cfr code
merged_violations$cfrstandardcode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", merged_violations$cfrstandardcode)
merged_violations$cfrstandardcode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "", merged_violations$cfrstandardcode)
names(merged_violations)[names(merged_violations) == "cfrstandardcode"] = "subsection_code"
merged_violations$subsection_code_marker = paste("S", merged_violations$subsection_code, sep = "")
merged_cfr_key$subsection_code_marker = paste("S", merged_cfr_key$subsection_code, sep = "")

# In some cases where subsection is missing, part_section is not and can be subbed in 
merged_violations$part_section2 = merged_violations$part_section
merged_violations$part_section2 = gsub("\\([a-z]+\\)", "", merged_violations$part_section2)
merged_violations$part_section2 = gsub("\\([0-9]+\\)", "", merged_violations$part_section2)
merged_violations$subsection_code = ifelse((is.na(merged_violations$subsection_code) 
                                            & !is.na(merged_violations$part_section2)), merged_violations$part_section2, 
                                            merged_violations$subsection_code)

# Merge violations and cfr key
merged_violations = merge(merged_violations, merged_cfr_key, by = "subsection_code", all = T)

# Flag which observations merged from each dataset - this is only useful for comparing the merge to Stata output
merged_violations[, "merge"] = ifelse(!is.na(merged_violations$subsection_code_marker.y) 
                                      & !is.na(merged_violations$subsection_code_marker.x), 3, 0)
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.x) 
                                      & !is.na(merged_violations$subsection_code_marker.y), 2, merged_violations[, "merge"])
merged_violations[, "merge"] = ifelse(is.na(merged_violations$subsection_code_marker.y) 
                                      & !is.na(merged_violations$subsection_code_marker.x), 1, merged_violations[, "merge"])

# Clean up redundant varnames from the merge
common_varstbs = sub(".x", "", names(merged_violations)[grep(".x", names(merged_violations), fixed = T)], fixed = T)
for (i in 1:length(common_varstbs)) {
  merged_violations[, paste(common_varstbs[i], ".x", sep = "")] = ifelse(merged_violations[, "merge"] == 2, 
                                                                         merged_violations[, paste(common_varstbs[i], ".y", sep = "")], 
                                                                         merged_violations[, paste(common_varstbs[i], ".x", sep = "")])
}
merged_violations = merged_violations[, -grep(".y", names(merged_violations), fixed = T)]
names(merged_violations)[grep(".x", names(merged_violations), fixed = T)] = common_varstbs
rm(merged_cfr_key, common_varstbs, i)

# Format date vars
datevars = names(merged_violations)[grep("date", names(merged_violations))]
for (i in 1:length(datevars)) {
  merged_violations[, datevars[i]] = as.Date(as.character(merged_violations[, datevars[i]]), "%m/%d/%Y")
}

# Remove observations from cfr data that didn't merge onto our violations data 
merged_violations = merged_violations[complete.cases(merged_violations$violationno),]

# Condition the per-day vars on positive denominator. (There are 256 cases of zero inspection days and positive violation counts). 6/6/16
merged_violations$contractor_violation_cnt = ifelse(merged_violations$violatortypecode == "Contractor", merged_violations$violator_violation_cnt, NA)
merged_violations$operator_violation_pInspDay = ifelse(merged_violations$violatortypecode == "Operator" & merged_violations$violator_inspection_day_cnt > 0, 
                                                       merged_violations$violator_violation_cnt/merged_violations$violator_inspection_day_cnt, NA)
merged_violations$contractor_repeated_viol_cnt = ifelse(merged_violations$violatortypecode == "Contractor", merged_violations$violator_repeated_viol_cnt, NA)
merged_violations$operator_repeated_viol_pInspDay = ifelse(merged_violations$violatortypecode == "Operator" & merged_violations$violator_inspection_day_cnt > 0, 
                                                           merged_violations$violator_repeated_viol_cnt/merged_violations$violator_inspection_day_cnt, NA)

# Remove unnecessary vars
merged_violations = merged_violations[, c(-grep("merge", names(merged_violations)))]

######################################################################################################################################

# DUMMY-OUT FACTOR VARIABLES

# Clean up the field that reports the type of inspection (some of this is unnecessary now that we merge codes but oh well)
merged_violations$inspacty = tolower(merged_violations$inspacty)
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "mine idle activity", "mine idle", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "na", "n", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "part 50 audit", "part 50 audits", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "non-fatal accident investigation", "nonfatal injury accident inv", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "shaft, slope or major construction spot inspection", "shft, slpe, or maj constr spot insp", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "103(g)(1) spot umwa inspection", "103(g)(1) spot inspection", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "103(i) spot (ign or expl) insp", "103(i) spot inspections", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "103(i) spot (hazard) inspection", "103(i) spot inspections", merged_violations[, "inspacty"])

# We can't have any missing values or our dummy method won't work
merged_violations[, "inspacty"] = ifelse(is.na(merged_violations$inspacty), "unknown", merged_violations[, "inspacty"])

# Deal with too many categories here (so we don't have hundreds of vars later - group inspections into categories)
merged_violations[, "inspacty"] = ifelse((merged_violations[, "inspacty"] == "regular inspection" |
                                            merged_violations[, "inspacty"] == "regular safety and health inspection"), "regular inspection", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(grepl("complaint", merged_violations[,"inspacty"]), "complaint inspection", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(merged_violations[, "inspacty"] == "fatal accident investigation", "fatality inspection", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(grepl("103", merged_violations[,"inspacty"]), "103", merged_violations[, "inspacty"])
merged_violations[, "inspacty"] = ifelse(grepl("(103|fatality|unknown|regular|complaint)", merged_violations[,"inspacty"]), merged_violations[, "inspacty"], "other")

# Can't have any missing values or our dummy method won't work
merged_violations$violationtypecode = as.character(merged_violations$violationtypecode)
merged_violations[, "violationtypecode"] = ifelse(is.na(merged_violations$violationtypecode), "Unknown", merged_violations[, "violationtypecode"])

# These make up a grand total of 4 in our observations - not worth making dummies for 4 violation types
merged_violations = merged_violations[(merged_violations$violationtypecode != "Notice" & merged_violations$violationtypecode != "Safeguard"),]
merged_violations$assessmenttypecode = as.character(merged_violations$assessmenttypecode)
merged_violations[, "assessmenttypecode"] = ifelse(is.na(merged_violations$assessmenttypecode), "Unknown", merged_violations[, "assessmenttypecode"])

# For each of the categorical vars we replace missings with NA, format as character vars, and name all NA "unknown"
is.na(merged_violations$likelihood) = merged_violations$likelihood == ""
levels(merged_violations$likelihood) = c("Unknown", "Highly", "NoLikelihood", "Occurred", "Reasonably", "Unlikely")
merged_violations$likelihood = as.character(merged_violations$likelihood)
merged_violations[, "likelihood"] = ifelse(is.na(merged_violations$likelihood), "Unknown", merged_violations[, "likelihood"])

# For each of the categorical vars we replace missings with NA, format as character vars, and name all NA "unknown"
merged_violations$injuryillness = as.character(merged_violations$injuryillness)
is.na(merged_violations$injuryillness) = merged_violations$injuryillness==""
levels(merged_violations$injuryillness) = c("Unknown", "Fatal", "LostDays", "NoLostDays", "Permanent")
merged_violations[, "injuryillness"] = ifelse(is.na(merged_violations$injuryillness), "Unknown", merged_violations[, "injuryillness"])

# For each of the categorical vars we replace missings with NA, format as character vars, and name all NA "unknown"
is.na(merged_violations$negligence) = merged_violations$negligence==""
levels(merged_violations$injuryillness) = c("Unknown", "HighNegligence", "LowNegligence", "ModNegligence", "NoNegligence", "Reckless")
merged_violations$negligence = as.character(merged_violations$negligence)
merged_violations[, "negligence"] = ifelse(is.na(merged_violations$negligence), "Unknown", merged_violations[, "negligence"])

######################################################################################################################################

# Here we've tabbed our categorical vars, so we know which value will become which dummy.

table(merged_violations$inspacty)
#inspacty.n variables are numbered from left to right over these values
#103           complaint inspection  fatality inspection        other         regular inspection         unknown 
#35583                 4519                  732                69825               657741                97816

table(merged_violations$violationtypecode)
#violationtypecode.n variables are numbered from left to right over these values
#Citation    Order  Unknown 
#830340    11155    24717 

table(merged_violations$assessmenttypecode)
#assessmenttypecode.n variables are numbered from left to right over these values
#Regular  Single Special Unknown 
#640735  187678   13082   24717 

table(merged_violations$likelihood)
#Highly NoLikelihood     Occurred   Reasonably      Unknown     Unlikely 
#5472        23386         1475       271986        18984       544909 

table(merged_violations$injuryillness)
#Fatal   LostDays NoLostDays  Permanent    Unknown 
#57634     584733     120395      84464      18986 

table(merged_violations$negligence)
#HighNegligence  LowNegligence  ModNegligence   NoNegligence       Reckless        Unknown 
#36421          97549         711082           1221            959          18980 

######################################################################################################################################

# FINISH DUMMYING-OUT CATEGORICAL VARIABLES 

# This is the function that will dummy out the categorical vars.
datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}

# Apply the dummy function to each cat var
test.data1 <- datdum(x="inspacty",data=merged_violations,name="inspacty")
test.data1 <- test.data1 [, c(grep("inspacty", names(test.data1)))]
test.data2 <- datdum(x="assessmenttypecode",data=merged_violations,name="assessmenttypecode")
test.data2 <- test.data2 [, c(grep("assessmenttypecode", names(test.data2)))]
test.data3 <- datdum(x="violationtypecode",data=merged_violations,name="violationtypecode")
test.data3 <- test.data3 [, c(grep("violationtypecode", names(test.data3)))]
test.data4 <- datdum(x="likelihood",data=merged_violations,name="likelihood")
test.data4 <- test.data4 [, c(grep("likelihood", names(test.data4)))]
test.data5 <- datdum(x="injuryillness",data=merged_violations,name="injuryillness")
test.data5 <- test.data5 [, c(grep("injuryillness", names(test.data5)))]
test.data6 <- datdum(x="negligence",data=merged_violations,name="negligence")
test.data6 <- test.data6 [, c(grep("negligence", names(test.data6)))]

# Merge all dummied datasets and remove them when done
merged_violations = cbind(merged_violations, test.data1, test.data2, test.data3, test.data4, test.data5, test.data6)
rm(test.data1, test.data2, test.data3, test.data4, test.data5, test.data6, datdum)

######################################################################################################################################

# PREPARE TO GENERATE PART AND SUBSECTION SPECIFIC VARIABLES

# Dummy out CFR codes (at the subpart and subsection levels) only for *relevant types and mark all non-relevant CFR codes
if (relevant.only.option == "on") {
    MR_relevant_subsectcodes = levels(factor(merged_violations[merged_violations$MR_relevant == 1 ,]$subsection_code))
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

# Create lists of number of dummies for violation, assessment, and inspection types 
likelihoodcodes = seq(1, 6)
injuryillnesscodes = seq(1, 5)
negligencecodes = seq(1, 6)
violationtypecodes = seq(1, 3)
assessmenttypecodes = seq(1, 4)
inspactycodes = seq(1, 6)

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

# Set this line below to be the subsecion code group you want. Cannot do all at once because of memory issues.
cfr_codes = MR_relevant_subsectcodes_75

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

# REMOVE THINGS WE WON'T USE AGAIN.
rm(MR_relevant_subsectcodes, MR_relevant_subsectcodes_47, MR_relevant_subsectcodes_48,
   MR_relevant_subsectcodes_71, MR_relevant_subsectcodes_72, MR_relevant_subsectcodes_75a,
   MR_relevant_subsectcodes_75b, MR_relevant_subsectcodes_75c, MR_relevant_subsectcodes_75d,
   MR_relevant_subsectcodes_75e, MR_relevant_subsectcodes_75, MR_relevant_subsectcodes_77) 

######################################################################################################################################

# COLLAPSE VIOLATIONS DATA TO THE MINE-QUARTER LEVEL

# Aggregation to mine-quarter level - create variables to sum for violation counts, and for if terminated (all violations should be terminated)
merged_violations$total_violations = 1
merged_violations$terminated = ifelse(merged_violations$typeoftermination == "Terminated", 1, 0)

# Select all variables to sum when we collapse to the mine-quarter lever (the first regex will grab all vars created above)
# Add indicator for if a mine q was terminated because of a violation (sum this for now - maybe we'll just make it an indicator later)
violations_to_sum = merged_violations[, c(grep("^[0-9][0-9]", names(merged_violations)), 
                                          match("total_violations", names(merged_violations)),
                                          match("excessive_history_ind", names(merged_violations)), 
                                          match("goodfaithind", names(merged_violations)), 
                                          match("terminated", names(merged_violations)),
                                          match("mineid", names(merged_violations)), 
                                          match("quarter", names(merged_violations)))]

# These were grabbed by the regular expression above, but we want to average (not sum) these, so we remove them  
violations_to_sum = violations_to_sum[, c(-grep("operator_repeated_viol_pInspDay", names(violations_to_sum)), -grep("contractor_repeated_viol_cnt", names(violations_to_sum)))]

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
# Question: Check if operator variables vary by mine? Are indep. contractors the only operators @ a mine or are they only a part of the operation? A: Inspections generate
# both contractor and operator violations. - April Ramirez @ DOL, 6/6/16

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
# number of quarters spanned by that inspection. This helps prevent doubt-counting inspection hours in several quarters, once we collapse
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

# COLLAPSE ACCIDENTS DATA

# Read in data
mine_types = readRDS(mine.types.file.name)
mines_accidents_coded = read.csv(mines_accidents_coded.file.name)

# Format mineid (for merges), date and quarter vars for merges
mines_accidents_coded$mineid = str_pad(mines_accidents_coded$mineid, 7, pad = "0")

# Format mineid and pad it with zeroes to make it 7 digits, so we have a clean merge
mines_accidents_coded$mineid = withr::with_options(c(scipen = 999), str_pad(mines_accidents_coded$mineid, 7, pad = "0"))
mines_accidents_coded$accidentdate = as.Date(as.character(mines_accidents_coded$accidentdate), "%m/%d/%Y")
mines_accidents_coded$quarter = as.yearqtr(mines_accidents_coded$accidentdate)

# Drop observations before our study period
mines_accidents_coded = mines_accidents_coded[(mines_accidents_coded$quarter > "1999 Q4"),]

# Merge on minetypes to drop non-coal and non-underground observations before saving
mines_accidents_coded = merge(mines_accidents_coded, mine_types, by = c("mineid"), all = T)

# Drop non-merging observations
mines_accidents_coded = mines_accidents_coded[!is.na(mines_accidents_coded$MR),]
rm(mine_types)

# Only keep observations from environment we care about
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$minetype == "Underground",]
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$subunit == "UNDERGROUND",]
mines_accidents_coded = mines_accidents_coded[mines_accidents_coded$coalcormetalmmine.x == "C",]

# Create injury indicator so that we can collapse & sum total injuries per mine quarter
mines_accidents_coded$totalinjuries = 1

# Collapse mines_accidents data here.
summed_coded_accidents = ddply(mines_accidents_coded[, c(grep("totalinjuries", names(mines_accidents_coded)), 
                                                         grep("MR", names(mines_accidents_coded)),
                                                         match("mineid", names(mines_accidents_coded)), 
                                                         match("quarter", names(mines_accidents_coded)))], c("mineid", "quarter"), 
                                  function(x) colSums(x[, c(grep("totalinjuries", names(x)), grep("MR", names(x)))], na.rm = T))
rm(mines_accidents_coded)
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
summed_coded_accidents$row_id = seq.int(nrow(summed_coded_accidents))

######################################################################################################################################
# MERGE VIOLATIONS DATA ONTO MINES

# Read in data
mines_quarters = readRDS(mines_quarters.file.name)

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

# Merge violations (now with contractor and mine info) and accidents
merged_mines_violations_accidents = merge(merged_mines_violations, summed_coded_accidents, by = c("mineid", "quarter"), all = T)

# Same as above - flag observations by merge status so we can compare to Stata output and make sure nothing weird happened in the merge 
merged_mines_violations_accidents[, "merge2"] = ifelse(!is.na(merged_mines_violations_accidents$row_id.y) 
                                                       & !is.na(merged_mines_violations_accidents$row_id.x), 3, 0)
merged_mines_violations_accidents[, "merge2"] = ifelse(is.na(merged_mines_violations_accidents$row_id.x) 
                                                       & !is.na(merged_mines_violations_accidents$row_id.y), 2, merged_mines_violations_accidents[, "merge2"])
merged_mines_violations_accidents[, "merge2"] = ifelse(is.na(merged_mines_violations_accidents$row_id.y) 
                                                       & !is.na(merged_mines_violations_accidents$row_id.x), 1, merged_mines_violations_accidents[, "merge2"])

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

# Set the file name (if part-specific).
if (relevant.only.option == "on") {
    saveRDS(prediction_data, file = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_relevant.rds")
}
if (relevant.only.option != "on") {
    #saveRDS(prediction_data, file = "X:/Projects/Mining/NIOSH/analysis/data/4_collapsed/prediction_data.rds")
    saveRDS(prediction_data, file = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75h.rds")
}

######################################################################################################################################

# Part 75 is enormous. If we want one dataset with all subsections included, we have to drop a number of variables first.
# This code will take the various part-75 datasets, keep only those vars that we use for our preliminary prediction algorithm,
# and merges them together.

rm(list=ls())
gc()
#part 75 csv
data_75a = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75a.rds")
data_75a  = data_75a[,c(match("MR", names(data_75a)),
                        match("year", names(data_75a)),
                        match("minename", names(data_75a)),
                        match("minesizepoints", names(data_75a)),
                        match("controllersizepoints", names(data_75a)),
                        match("contractorsizepoints", names(data_75a)),
                        match("minestatusdate", names(data_75a)),
                        match("operatorid", names(data_75a)),
                        match("operatorname", names(data_75a)),
                        match("stateabbreviation", names(data_75a)),
                        match("idate", names(data_75a)),
                        match("hours_qtr", names(data_75a)),
                        match("employment_qtr", names(data_75a)),
                        match("coal_prod_qtr", names(data_75a)),
                        match("productionshiftsperday", names(data_75a)),
                        match("terminated", names(data_75a)),
                        match("contractor_repeated_viol_cnt", names(data_75a)),
                        match("insp_hours_per_qtr", names(data_75a)),
                        match("terminated", names(data_75a)),
                        match("idesc", names(data_75a)),
                        match("minestatus", names(data_75a)),
                        match("75", names(data_75a)),
                        grep("75.inspacty", names(data_75a)),
                        grep("^75\\.[0-9]+(-[0-9]+)*$", names(data_75a)),
                        grep("penaltypoints", names(data_75a)),
                        grep("sigandsubdesignation", names(data_75a)),
                        match("total_violations", names(data_75a)),
                        match("totalinjuries", names(data_75a)),
                        match("num_insp", names(data_75a)), 
                        match("hours_qtr", names(data_75a)),
                        match("mineid", names(data_75a)), 
                        match("quarter", names(data_75a)),
                        match("onsite_insp_hours_per_qtr", names(data_75a)))]

data_75b = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75b.rds")
data_75b  = data_75b[, c(match("mineid", names(data_75b)), 
                        match("quarter", names(data_75b)),
                        grep("^75\\.14[0-9]+(-[0-9]+)*$", names(data_75b)),
                        grep("penaltypoints", names(data_75b)),
                        grep("sigandsubdesignation", names(data_75b)))]

data_75c = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75c.rds")
data_75c  = data_75c[, c(match("mineid", names(data_75c)), 
                        match("quarter", names(data_75c)),
                        grep("^75\\.1[5-9][0-9](-[0-9]+)*$", names(data_75c)),
                        grep("penaltypoints", names(data_75c)),
                        grep("sigandsubdesignation", names(data_75c)))]

data_75d = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75d.rds")
data_75d  = data_75d[, c(match("mineid", names(data_75d)), 
                         match("quarter", names(data_75d)),
                         grep("^75\\.[2-4][0-9]+(-[0-9]+)*$", names(data_75d)),
                         grep("penaltypoints", names(data_75d)),
                         grep("sigandsubdesignation", names(data_75d)))]

data_75e = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75e.rds")
data_75e  = data_75e[, c(match("mineid", names(data_75e)), 
                         match("quarter", names(data_75e)),
                         grep("^75\\.5[0-9]+(-[0-9]+)*$", names(data_75e)),
                         grep("penaltypoints", names(data_75e)),
                         grep("sigandsubdesignation", names(data_75e)))]

data_75f = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75f.rds")
data_75f  = data_75f[, c(match("mineid", names(data_75f)), 
                         match("quarter", names(data_75f)),
                         grep("^75\\.[6-7][0-9]+(-[0-9]+)*$", names(data_75f)),
                         grep("penaltypoints", names(data_75f)),
                         grep("sigandsubdesignation", names(data_75f)))]

data_75g = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75g.rds")
data_75g  = data_75g[, c(match("mineid", names(data_75g)), 
                         match("quarter", names(data_75g)),
                         grep("^75\\.[8-9][0-9]+(-[0-9]+)*$", names(data_75g)),
                         grep("penaltypoints", names(data_75g)),
                         grep("sigandsubdesignation", names(data_75g)))]

data_75h = readRDS("X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75h.rds")
data_75h  = data_75h[, c(match("mineid", names(data_75h)), 
                         match("quarter", names(data_75h)),
                         grep("^75\\.[8-9][0-9]+(-[0-9]+)*$", names(data_75h)),
                         grep("penaltypoints", names(data_75h)),
                         grep("sigandsubdesignation", names(data_75h)))]

part75_select_vars = merge(data_75a, data_75b, by = c("mineid", "quarter"), all=T)
part75_select_vars = part75_select_vars[,c(-grep("\\.y$", names(part75_select_vars)))]
names(part75_select_vars)[names(part75_select_vars) == '75.penaltypoints.x'] = '75.penaltypoints'
names(part75_select_vars)[names(part75_select_vars) == '75.sigandsubdesignation.x'] = '75.sigandsubdesignation'

part75_select_vars = merge(part75_select_vars, data_75c, by = c("mineid", "quarter"), all=T)
part75_select_vars = part75_select_vars[,c(-grep("\\.y$", names(part75_select_vars)))]
names(part75_select_vars)[names(part75_select_vars) == '75.penaltypoints.x'] = '75.penaltypoints'
names(part75_select_vars)[names(part75_select_vars) == '75.sigandsubdesignation.x'] = '75.sigandsubdesignation'

part75_select_vars = merge(part75_select_vars, data_75d, by = c("mineid", "quarter"), all=T)
part75_select_vars = part75_select_vars[,c(-grep("\\.y$", names(part75_select_vars)))]
names(part75_select_vars)[names(part75_select_vars) == '75.penaltypoints.x'] = '75.penaltypoints'
names(part75_select_vars)[names(part75_select_vars) == '75.sigandsubdesignation.x'] = '75.sigandsubdesignation'

part75_select_vars = merge(part75_select_vars, data_75e, by = c("mineid", "quarter"), all=T)
part75_select_vars = part75_select_vars[,c(-grep("\\.y$", names(part75_select_vars)))]
names(part75_select_vars)[names(part75_select_vars) == '75.penaltypoints.x'] = '75.penaltypoints'
names(part75_select_vars)[names(part75_select_vars) == '75.sigandsubdesignation.x'] = '75.sigandsubdesignation'
part75_select_vars = part75_select_vars[,c(-grep("\\.x$", names(part75_select_vars)))]

part75_select_vars = merge(part75_select_vars, data_75f, by = c("mineid", "quarter"), all=T)
part75_select_vars = part75_select_vars[,c(-grep("\\.y$", names(part75_select_vars)))]
names(part75_select_vars)[names(part75_select_vars) == '75.penaltypoints.x'] = '75.penaltypoints'
names(part75_select_vars)[names(part75_select_vars) == '75.sigandsubdesignation.x'] = '75.sigandsubdesignation'

part75_select_vars = merge(part75_select_vars, data_75g, by = c("mineid", "quarter"), all=T)
part75_select_vars = part75_select_vars[,c(-grep("\\.y$", names(part75_select_vars)))]
names(part75_select_vars)[names(part75_select_vars) == '75.penaltypoints.x'] = '75.penaltypoints'
names(part75_select_vars)[names(part75_select_vars) == '75.sigandsubdesignation.x'] = '75.sigandsubdesignation'
part75_select_vars = part75_select_vars[,c(-grep("\\.x$", names(part75_select_vars)))]

part75_select_vars = merge(part75_select_vars, data_75h, by = c("mineid", "quarter"), all=T)
part75_select_vars = part75_select_vars[,c(-grep("\\.y$", names(part75_select_vars)))]
names(part75_select_vars)[names(part75_select_vars) == '75.penaltypoints.x'] = '75.penaltypoints'
names(part75_select_vars)[names(part75_select_vars) == '75.sigandsubdesignation.x'] = '75.sigandsubdesignation'

saveRDS(part75_select_vars, "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75.rds")

varnames = names(part75_select_vars)
varnames = gsub("\\.", "_", varnames)
varnames = gsub("-", "_", varnames)
varnames = paste("_", varnames, sep ="")
names(part75_select_vars) = varnames

write.csv(part75_select_vars, "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/prediction_data_75.csv")
  
######################################################################################################################################
