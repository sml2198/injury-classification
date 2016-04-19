install.packages("eeptools")
library(eeptools)
install.packages("zoo")
library(zoo)
install.packages("dummies")
library(dummies)

setwd("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/")
mr.data = read.csv("Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = TRUE)
imputation_method = 3

# CLEAN NARRATIVE FIELDS: DROP REDUNDANT VARS AND KEEP LOWERCASE VERSION
drops <- c("narrativemodified", "degreeofinjury", "accidentclassification", "accidenttype", "natureofinjury", "mineractivity")
mr.data = mr.data[, !(names(mr.data) %in% drops)]
names(mr.data)[names(mr.data) == 'narrativemodified.1'] = 'narrative'
mr.data$narrative = tolower(mr.data$narrative)
names(mr.data)[names(mr.data) == 'degreeofinjury.1'] = 'degreeofinjury'
mr.data$narrative = tolower(mr.data$narrative)
names(mr.data)[names(mr.data) == 'accidentclassification.1'] = 'accidentclassification'
mr.data$narrative = tolower(mr.data$narrative)
names(mr.data)[names(mr.data) == 'accidenttype.1'] = 'accidenttype'
mr.data$narrative = tolower(mr.data$narrative)
names(mr.data)[names(mr.data) == 'natureofinjury.1'] = 'natureofinjury'
mr.data$narrative = tolower(mr.data$narrative)
names(mr.data)[names(mr.data) == 'mineractivity.1'] = 'mineractivity'
mr.data$narrative = tolower(mr.data$narrative)
mr.data$MR = as.factor(mr.data$MR)

mr.data[,grep("numberofemployees", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("numberofemployees", names(mr.data))])
mr.data[,grep("numberofemployees", names(mr.data))] = as.numeric(mr.data[,grep("numberofemployees", names(mr.data))])
mr.data[,grep("methaneliberation", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("methaneliberation", names(mr.data))])
mr.data[,grep("methaneliberation", names(mr.data))] = as.numeric(mr.data[,grep("methaneliberation", names(mr.data))])
mr.data[,grep("averagemineheight", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("averagemineheight", names(mr.data))])
mr.data[,grep("averagemineheight", names(mr.data))] = as.numeric(mr.data[,grep("averagemineheight", names(mr.data))])

# Merge redundant 'not-found' values within variables. Note: Values like "Unknown" or "Other" are not funneled into "No Value Found"
mr.data[, "uglocation"] = ifelse(mr.data[, "uglocation"] == "NOT MARKED", "NO VALUE FOUND", mr.data[, "uglocation"])
mr.data[, "immediatenotificationclass"] = ifelse(mr.data[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", mr.data[, "immediatenotificationclass"])
mr.data[, "natureofinjury"] = ifelse(mr.data[, "natureofinjury"] == "UNCLASSIFIED,NOT DETERMED", "NO VALUE FOUND", mr.data[, "natureofinjury"])
mr.data[, "equipmanufacturer"] = ifelse(mr.data[, "equipmanufacturer"] == "Not Reported", "NO VALUE FOUND", mr.data[, "equipmanufacturer"])

# CONVERT DATES
indices_with_date = grep("date", names(mr.data))
for (i in indices_with_date) {
  mr.data[,i] = as.Date(mr.data[,i], "%m/%d/%Y")
}

# MERGE IN OTHER VARIABLES FROM PROTO-ALGORITHM (LIKE KEYWORD FLAGS) CHANGE TO KEEP ANY OTHER ADDTL VARS
mr.pre.coding = read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/training_set_2_internal.csv")
mr.data = merge(mr.data, mr.pre.coding, c("mineid", "documentno"))
remove(mr.pre.coding)
mr.data = mr.data[, c(-grep("\\.y", names(mr.data)))]
names(mr.data) = gsub("\\.[x]", "", names(mr.data))

# DROP 36 OF 144 VARS - THESE ARE VARIABLES WE CREATED 
mr.data = mr.data[, c(-grep("repair", names(mr.data)), -grep("rplace", names(mr.data)),
                      -grep("service", names(mr.data)), -grep("fix", names(mr.data)), 
                      -grep("upgrade", names(mr.data)), -grep("updte", names(mr.data)), 
                      -grep("servicing", names(mr.data)), -grep("keyword", names(mr.data)), 
                      -grep("maintain", names(mr.data)), -grep("roof.bolt", names(mr.data)), 
                      -grep("rib.hole", names(mr.data)), -grep("instal", names(mr.data)), 
                      -grep("pain", names(mr.data)), -grep("hoist", names(mr.data)),
                      -grep("insample", names(mr.data)), -grep("group", names(mr.data)),
                      -grep("changed", names(mr.data)), -grep("dup", names(mr.data)),
                      -grep("narrativemodified", names(mr.data)), -grep("narrative_old", names(mr.data)),
                      -grep("surgery", names(mr.data)), -grep("random", names(mr.data)), 
                      -grep("v5", names(mr.data)))]

# ADD NEW KEYWORD VARS: 11 NEW VARS (ADDED TO 108)
mr.data[, "repair"] = ifelse(grepl("(^| )r(e|a)pa(i*)r[a-z]*", mr.data[,"narrative"]), 1, 0)
mr.data[, "rplace"] = ifelse(grepl("(^| )replac(e|i)[a-z]*", mr.data[,"narrative"]), 1, 0)
mr.data[, "service"] = ifelse(grepl("serviced", mr.data[,"narrative"]) | grepl("servicing", mr.data[,"narrative"]), 1, 0)
mr.data[, "fix"] = ifelse(grepl("(^| )fix[a-z]*", mr.data[,"narrative"]) & !grepl("(^| )fixture", mr.data[,"narrative"]), 1, 0) 
mr.data[, "maintain"] = ifelse(grepl("(^| )maintain[a-z]*", mr.data[,"narrative"]), 1, 0)

mr.data[, "roof.bolt"] = ifelse(grepl("(roof|rib)*( )*(bolt)(er|ing| |$|.|,).?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", 
  mr.data[,"narrative"]) | grepl("(roof|rib)*( )*(bolt)(er|ing| |$|.|,).?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", 
  mr.data[,"narrative"]), 1, 0)  
mr.data[, "rib.hole"] = ifelse(grepl("(rib)( )*(hole).?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", 
  mr.data[,"narrative"]) | 
    grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?.?(rib)( )*(hole)", mr.data[,"narrative"]), 1, 0)  
mr.data[, "install"] = ifelse(grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) & (mr.data[, "rib.hole"] != 1 & mr.data[, "roof.bolt"] != 1), 1, 0) 

mr.data[, "pain"] = ifelse(grepl("(^| )pain( |$|.|,|:|)", mr.data[,"narrative"]), 1, 0)
mr.data[, "hoist"] = ifelse(((grepl("(^| )hoist[a-z]*", mr.data[,"narrative"]) | grepl("(^| )el(e|a|i)vat(o|e)r*", mr.data[,"narrative"])) & mr.data[, "pain"] == 0), 1, 0) 
mr.data[, "surgery"] = ifelse((grepl("surger[a-z]*", mr.data[,"narrative"]) | grepl("surgic[a-z]*", mr.data[,"narrative"])) & mr.data[, "pain"] == 0, 1, 0)

# CREATE/PREP VARIOUS TIME AND DATE VARIABLES - YEAR AND QUARTER
date <- strptime(mr.data$calendaryear, "%Y")
format(date, "%Y")
mr.data[, "year"] = format(date, "%Y")
require(zoo)
mr.data[, "quarter"] = as.yearqtr(mr.data$accidentdate,"%Y-%m-%d")
# now drop calendar year and quarter and accidentdate, all not needed anymore 
mr.data = mr.data[, c(-grep("calendar", names(mr.data)), -grep("accidentdate", names(mr.data)))]
# do we want to treat accidenttime a certain way?


# REMOVE IRRELEVANT VARS: 24 OF 119 VARS 
mr.data = mr.data[, c(-grep("directionstominemodified", names(mr.data)), -grep("minegascategorycode", names(mr.data)),
                      -grep("nooftailingponds", names(mr.data)), -grep("noofproducingpits", names(mr.data)),
                      -grep("longitude", names(mr.data)), -grep("latitude", names(mr.data)),
                      -grep("nearesttown", names(mr.data)), -grep("minestatus", names(mr.data)),
                      -grep("secondary", names(mr.data)), -grep("minetype", names(mr.data)), 
                      -grep("milesfromoffice", names(mr.data)), -grep("ai_type_desc_old", names(mr.data)),
                      -grep("portableoperationindicator", names(mr.data)), -grep("roomandpillarindicator", names(mr.data)),
                      -grep("highwallminerindicator", names(mr.data)), -grep("multiplepitsindicator", names(mr.data)),
                      -grep("minersrepindicator", names(mr.data)), -grep("coalcormetalmmine", names(mr.data)),
                      -grep("primarycanvasscodedesc", names(mr.data)), -grep("primarysiccodesuffix", names(mr.data)),
                      -grep("transferredorterminated", names(mr.data)), -grep("primarysiccodesuffix", names(mr.data)))]

# REMOVE VARS UNIQUE AT OBS. LEVEL - 8 OF 95
mr.data = mr.data[, c(-grep("documentno", names(mr.data)), -grep("narrative", names(mr.data)),
                      -grep("accidenttime", names(mr.data)), -grep("date", names(mr.data)))]

# SHOULD NOW HAVE 87 VARS, NOW REMOVE REDUNDANT VARS (CODES/IDS WITH CORRESPONDING CLASSES - 26 VARS)
mr.data = mr.data[, c(-grep("operatorid", names(mr.data)), -grep("controllerid", names(mr.data)), 
                      -grep("contractorid", names(mr.data)), -grep("primarycanvasscode", names(mr.data)),
                      -grep("subunitcode", names(mr.data)), -grep("inj_degr_cd_old", names(mr.data)),
                      -grep("fipsstatecode", names(mr.data)), -grep("activitycode", names(mr.data)), 
                      -grep("injurysourcecode", names(mr.data)), -grep("natureofinjurycode", names(mr.data)),
                      -grep("bodypartcode", names(mr.data)), -grep("degreeofinjurycode", names(mr.data)),
                      -grep("uglocationcode", names(mr.data)), -grep("ugminingmethodcode", names(mr.data)),
                      -grep("classificationcode", names(mr.data)), -grep("accidenttypecode", names(mr.data)),
                      -grep("equiptypecode", names(mr.data)), -grep("equipmanufacturercode", names(mr.data)),
                      -grep("immediatenotificationcode", names(mr.data)), -grep("occupcode3digit", names(mr.data)),
                      -grep("fipscountycode", names(mr.data)), -grep("officecode", names(mr.data)), 
                      -grep("bomstatecode", names(mr.data)), -grep("primarysiccode", names(mr.data)), 
                      -grep("primarysiccodegroup", names(mr.data)), -grep("oldoccupationcode", names(mr.data)),
                      -grep("portablefipsstatecode", names(mr.data)))]

# SHOULD NOW HAVE 61 VARS, NOW REMOVE 5 OLD/UNKNOWN VARS 
mr.data = mr.data[, c(-grep("ai_dt_old", names(mr.data)), -grep("idesc", names(mr.data)), 
                      -grep("ai_time_old", names(mr.data)), -grep("ai_acty_cd_old", names(mr.data)),
                      -grep("portablefipsstatecode", names(mr.data)))]

#We don't use date vars as of yet so no need to store a list of their names, "logical" class vars are missing all obsvtns
var_classes = sapply(mr.data[,names(mr.data)], class)
charac_vars = names(var_classes[c(grep("character", var_classes), grep("factor", var_classes))])
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])
mr.data = mr.data[, -grep("logical", var_classes)]

for (i in 1:length(charac_vars)) {
  mr.data[, charac_vars[i]] = ifelse((mr.data[,charac_vars[i]] == "NO VALUE FOUND" | mr.data[,charac_vars[i]] == "UNKNOWN" | 
                                        mr.data[,charac_vars[i]] == "?" | mr.data[,charac_vars[i]] == ""), NA_character_, as.character(mr.data[,charac_vars[i]]))
  mr.data[, charac_vars[i]] = factor(mr.data[, charac_vars[i]])
}

# DEFINE FUNCTION FOR MODES FOR IMPUTATION
modus = function(x) {
  uniqv = unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

# IMPUTE MISSING VARS
if (imputation_method == 1 | imputation_method == 2) {
  for (i in 1:length(num_vars)) {
    mr.data[, num_vars[i]] = ifelse(is.na(mr.data[, num_vars[i]]), mean(mr.data[, num_vars[i]]), mr.data[, num_vars[i]])
  }
  if (imputation_method == 2) {
    for (i in 1:length(num_vars)) {
      mr.data[, num_vars[i]] = ifelse(is.na(mr.data[, num_vars[i]]), median(mr.data[, num_vars[i]]), mr.data[, num_vars[i]])
    }
  }
  for (i in 1:length(charac_vars)) {
    mr.data[, charac_vars[i]] = ifelse(is.na(mr.data[, charac_vars[i]]), modus(mr.data[, charac_vars[i]]), mr.data[, charac_vars[i]])
  }
} else if (imputation_method == 3) {
  for (i in 1:length(num_vars)) {
    i_rowsmissing = row.names(mr.data)[is.na(mr.data[, num_vars[i]])]
    while (sum(!complete.cases(mr.data[, num_vars[i]])) > 0) {
      replace_rows = sample(setdiff(row.names(mr.data), i_rowsmissing), length(i_rowsmissing), replace = T)
      mr.data[i_rowsmissing, num_vars[i]] = mr.data[replace_rows, num_vars[i]]
    }
  }
  for (i in 1:length(charac_vars)) {
    i_rowsmissing = row.names(mr.data)[is.na(mr.data[, charac_vars[i]])]
    while (sum(!complete.cases(mr.data[, charac_vars[i]])) > 0) {
      replace_rows = sample(setdiff(row.names(mr.data), i_rowsmissing), length(i_rowsmissing), replace = T)
      mr.data[i_rowsmissing, charac_vars[i]] = mr.data[replace_rows, charac_vars[i]]
    }
  }
} 

# DUMMY-OUT FACTOR VARS WITH TOO MANY VALUES
require(dummies)
new_dummies = apply(cbind(dummy(mr.data$sourceofinjury), dummy(mr.data$occupation), dummy(mr.data$equipmentmodelno), dummy(mr.data$fipscountyname),
                          dummy(mr.data$controllername), dummy(mr.data$operatorname), MARGIN = 2, FUN = function(x) factor(x)))

