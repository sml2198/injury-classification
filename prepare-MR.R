install.packages("eeptools")
library(eeptools)
install.packages("zoo")
library(zoo)
install.packages("dummies")
library(dummies)

# SET PREFERENCES - IMPUTATION METHOD - METHOD 3 IS RANDOM DRAWS FROM DISTRIBUTION (OUR BEST METHOD)
imputation_method = 3

# LOAD IN CODED TRAINING SET (1000 OBSERVATIONS, CODED FOR "MR")
setwd("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/")
mr.data = read.csv("Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)

# LOAD IN DATASET OF ADDITIONAL FATALITIES (FROM OPEN DATA) TO APPEND TO OUR TRAINING SET - ALL "MR"
#setwd("X:/Projects/Mining/NIOSH/analysis/data/4_coded/")
mr.fatalities = read.csv("X:/Projects/Mining/NIOSH/analysis/data/4_coded/coded_MR_fatalities.csv", header = TRUE, sep = ",", nrows = 24, stringsAsFactors = FALSE)

# MAKE SURE TRAINING SET AND FATALITIES DATASETS HAVE ALL THE SAME VAR NAMES BEFORE APPENDING
mr.data$MR = as.factor(mr.data$M.R.)
mr.data = mr.data[, c(-match("M.R.", names(mr.data)))]
mr.data[, "death"] = ifelse(grepl("fatality", mr.data[,"degreeofinjury"]), 1, 0)

# CLEAN UP FATALITIES VARIABLES - DROP VARS NOT PRESENT IN TRAINING SET BEFORE APPENDING
mr.fatalities$MR = as.factor(mr.fatalities$MR_fatality)
mr.fatalities = mr.fatalities[, c(-grep("MR_fatality", names(mr.fatalities)), -grep("v56", names(mr.fatalities)),
                                  -grep("v57", names(mr.fatalities)), -grep("v58", names(mr.fatalities)), 
                                  -grep("v59", names(mr.fatalities)))]

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

# APPEND DATASET OF ADDITIONAL FATALITY OBSERVATIONS FOR TRAINING SET
mr.data <- rbind(mr.data, mr.fatalities) 

# DESTRING VARIABLES
mr.data[,grep("numberofemployees", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("numberofemployees", names(mr.data))])
mr.data[,grep("numberofemployees", names(mr.data))] = as.numeric(mr.data[,grep("numberofemployees", names(mr.data))])
mr.data[,grep("methaneliberation", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("methaneliberation", names(mr.data))])
mr.data[,grep("methaneliberation", names(mr.data))] = as.numeric(mr.data[,grep("methaneliberation", names(mr.data))])
mr.data[,grep("averagemineheight", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("averagemineheight", names(mr.data))])
mr.data[,grep("averagemineheight", names(mr.data))] = as.numeric(mr.data[,grep("averagemineheight", names(mr.data))])

# MERGE REDUNDANT "NO VALUE FOUND" FIELDS IN FACTOR VARIABLES
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
#mr.pre.coding = read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/training_set_2_internal.csv")
#mr.data = merge(mr.data, mr.pre.coding, c("mineid", "documentno"))
#remove(mr.pre.coding)
#mr.data = mr.data[, c(-grep("\\.y", names(mr.data)))]
#names(mr.data) = gsub("\\.[x]", "", names(mr.data))
# THERE'S NO NEED TO DO THIS ANYMORE, WE'RE REMAKING ALL THE VARS ANYWAY. - SARAH ON 4/25/2016

# ADD NEW KEYWORD VARS: 11 NEW VARS (ADDED TO 106)
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

# REMOVE IRRELEVANT VARS: 25 OF 116 VARS 
mr.data = mr.data[, c(-match("directionstominemodified", names(mr.data)), -match("minegascategorycode", names(mr.data)),
                      -match("nooftailingponds", names(mr.data)), -match("noofproducingpits", names(mr.data)),
                      -match("longitude", names(mr.data)), -match("latitude", names(mr.data)),
                      -match("nearesttown", names(mr.data)), -match("minestatus", names(mr.data)),
                      -grep("secondary", names(mr.data)), -match("minetype", names(mr.data)), 
                      -match("milesfromoffice", names(mr.data)), -match("primarysiccodesuffix", names(mr.data)),
                      -match("portableoperationindicator", names(mr.data)), -match("roomandpillarindicator", names(mr.data)),
                      -match("highwallminerindicator", names(mr.data)), -match("multiplepitsindicator", names(mr.data)),
                      -match("minersrepindicator", names(mr.data)), -match("coalcormetalmmine", names(mr.data)),
                      -match("primarycanvasscodedesc", names(mr.data)), -match("primarysiccodesuffix", names(mr.data)),
                      -match("transferredorterminated", names(mr.data)))]

# REMOVE VARS UNIQUE AT OBS. LEVEL - 4 OF 89
mr.data = mr.data[, c(-match("documentno", names(mr.data)), -match("narrative", names(mr.data)),
                      -match("accidenttime", names(mr.data)), -grep("date", names(mr.data)))]

# SHOULD NOW HAVE 89 VARS, NOW REMOVE REDUNDANT VARS (CODES/IDS WITH CORRESPONDING CLASSES - 30 VARS)
mr.data = mr.data[, c(-match("operatorid", names(mr.data)), -match("controllerid", names(mr.data)), 
                      -match("primarycanvasscode", names(mr.data)), -match("portablefipsstatecode", names(mr.data)),
                      -match("subunitcode", names(mr.data)), -match("death", names(mr.data)),
                      -match("fipsstatecode", names(mr.data)), -match("activitycode", names(mr.data)), 
                      -match("injurysourcecode", names(mr.data)), -match("natureofinjurycode", names(mr.data)),
                      -match("bodypartcode", names(mr.data)), -match("degreeofinjurycode", names(mr.data)),
                      -match("uglocationcode", names(mr.data)), -match("ugminingmethodcode", names(mr.data)),
                      -match("classificationcode", names(mr.data)), -match("accidenttypecode", names(mr.data)),
                      -match("equiptypecode", names(mr.data)), -match("equipmanufacturercode", names(mr.data)),
                      -match("immediatenotificationcode", names(mr.data)), -match("occupcode3digit", names(mr.data)),
                      -match("fipscountycode", names(mr.data)), -match("officecode", names(mr.data)), 
                      -match("bomstatecode", names(mr.data)), -match("primarysiccode", names(mr.data)), 
                      -match("primarysiccodegroup", names(mr.data)), -match("oldoccupationcode", names(mr.data)))]

# SHOULD NOW HAVE 59 VARS, NOW REMOVE 2 OLD/UNKNOWN VARS 
mr.data = mr.data[, c(-grep("idesc", names(mr.data)), -match("i", names(mr.data)))]

# STORE THE TYPE OF ALL VARIABLES AND CREATE LISTS OF VARS BY TYPE
var_classes = sapply(mr.data[,names(mr.data)], class)
# "character" vars are factor variables
charac_vars = names(var_classes[c(grep("character", var_classes), grep("factor", var_classes))])
# "logical" vars are missing all observations - these variables will be dropped (0 actually dropped here)
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])

for (i in 1:length(charac_vars)) {
  print(i)
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
datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}
test.data1 = datdum(x="sourceofinjury",data=mr.data,name="sourceofinjury")
test.data2 = datdum(x="equipmentmodelno",data=test.data1,name="equipmentmodelno")
test.data3 = datdum(x="minename",data=test.data2,name="minename")
test.data4 = datdum(x="operatorname",data=test.data3,name="operatorname")
test.data5 = datdum(x="fipscountyname",data=test.data4,name="fipscountyname")
test.data6 = datdum(x="controllername",data=test.data5,name="controllername")
mr.data = datdum(x="mineractivity",data=test.data5,name="mineractivity")

# home computer directory
# setwd("/Users/Sarah/Dropbox (SLS)/R-code")
# office computer directory
setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code")
write.csv(mr.data, file = "prepped_MR_training_data.csv", row.names = FALSE)
