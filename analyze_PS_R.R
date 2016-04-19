#analyze_PS.R by Nikhil Saifullah 4/14/16

#install.packages("dummies")
library(dummies)

#Specify imputation method here: 1 - means & modes, 2 - medians & modes, 3 - random sampling from distribution - best we can do for now
#4 - multiple imputation through regression

imputation_method = 3

ps_data = read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv")

##Cleaning##

ps_data = ps_data[!is.na(ps_data$mineid),]
names(ps_data)[names(ps_data) == 'narrativemodified'] = 'narrative'
ps_data[, "X"] = factor(ifelse(ps_data[, "X"] == 1, "YES", "NO"))
names(ps_data)[names(ps_data) == "X"] = "PS"

#How to destring a variable
ps_data[,grep("numberofemployees", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("numberofemployees", names(ps_data))] = as.numeric(ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = as.numeric(ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("averagemineheight", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = as.numeric(ps_data[,grep("averagemineheight", names(ps_data))])

#Merge redundant 'not-found' values within variables. Note: Values like "Unknown" or "Other" are not funneled into "No Value Found"
ps_data[, "uglocation"] = ifelse(ps_data[, "uglocation"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "uglocation"])
ps_data[, "accidenttype"] = ifelse(ps_data[, "accidenttype"] == "NOT ELSEWHERE CLASSIFIED", "NO VALUE FOUND", ps_data[, "accidenttype"])
ps_data[, "immediatenotificationclass"] = ifelse(ps_data[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "immediatenotificationclass"])
ps_data[, "natureofinjury"] = ifelse(ps_data[, "natureofinjury"] == "UNCLASSIFIED,NOT DETERMED", "NO VALUE FOUND", ps_data[, "natureofinjury"])
ps_data[, "equipmanufacturer"] = ifelse(ps_data[, "equipmanufacturer"] == "Not Reported", "NO VALUE FOUND", ps_data[, "equipmanufacturer"])

#Convert date variables. We drop date variables now, but eventually will make use of them. Nikhil 4/18/16
indices_with_date = grep("date", names(ps_data))
for (i in indices_with_date) {
  ps_data[,i] = as.Date(ps_data[,i], "%m/%d/%Y")
}

# MERGE IN OTHER VARIABLES FROM PROTO-ALGORITHM (LIKE KEYWORD FLAGS) CHANGE TO KEEP ANY OTHER ADDTL VARS
ps_data = merge(ps_data, read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/training_set_1_internal.csv"), c("mineid", "documentno"))
ps_data = ps_data[, c(-grep("\\.y", names(ps_data)))]
names(ps_data) = gsub("\\.[x|y]", "", names(ps_data))

ps_data[, "pinion"] = ifelse(grepl("(^| )pinion", ps_data[,"narrative"]), 1, 0)
ps_data[, "pinner"] = ifelse(grepl("(^| )pinner", ps_data[,"narrative"]), 1, 0)
ps_data[, "pin"] = ifelse(grepl("(^| )pin(n*)(e|i)[a-z]+", ps_data[,"narrative"]) & ps_data[, "pinion"] != 1 & ps_data[, "pinner"] != 1, 1, 0)
ps_data[, "strike"] = ifelse(grepl("str(i|u)(.*)k[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "trap"] = ifelse(grepl("( )trap[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "keyword"] = ifelse((ps_data[, "trap"] == 1 | ps_data[, "pin"] == 1 | ps_data[, "strike"] == 1), 1, 0)
ps_data = ps_data[, c(-grep("pinner", names(ps_data)), -grep("pinion", names(ps_data)))]

#Drop variables with redundant or no information
ps_data = ps_data[, c(-match("primarycanvasscodedesc", names(ps_data)), -match("primarycanvasscode", names(ps_data)), -match("primarysicdesc", names(ps_data))
                      , -match("primarysiccode", names(ps_data)), -match("minetype", names(ps_data)), -match("coalcormetalmmine", names(ps_data))
                      , -match("primarysiccodegroup", names(ps_data)), -match("primarysiccodesuffix", names(ps_data)), -match("equiptypecode", names(ps_data))
                      , -match("immediatenotificationcode", names(ps_data)), -match("secondarysiccode", names(ps_data)), -match("secondarysicdesc", names(ps_data))
                      , -match("secondarysiccodegroup", names(ps_data)), -match("secondarysiccodesuffix", names(ps_data)), -match("secondarycanvasscode", names(ps_data))
                      , -match("secondarycanvasscodedesc", names(ps_data)), -match("minegascategorycode", names(ps_data)), -match("noofproducingpits", names(ps_data))
                      , -match("nooftailingponds", names(ps_data)), -match("roomandpillarindicator", names(ps_data)), -match("highwallminerindicator", names(ps_data))
                      , -match("multiplepitsindicator", names(ps_data)), -match("minersrepindicator", names(ps_data)), -match("activitycode", names(ps_data))
                      , -match("injurysourcecode", names(ps_data)), -match("natureofinjurycode", names(ps_data)), -match("bodypartcode", names(ps_data)), -match("subunitcode", names(ps_data))
                      , -match("degreeofinjurycode", names(ps_data)), -match("uglocationcode", names(ps_data)), -match("ugminingmethodcode", names(ps_data)), -match("classificationcode", names(ps_data))
                      , -match("accidenttypecode", names(ps_data)), -match("inj_degr_cd_old", names(ps_data)), -match("dup", names(ps_data)), -match("changed", names(ps_data))
                      , -match("narrativemodified", names(ps_data)), -match("narrative_old", names(ps_data)), -match("equipment", names(ps_data)), -match("date_old", names(ps_data))
                      , -match("date_new", names(ps_data)), -match("directionstominemodified", names(ps_data)), -grep("insample", names(ps_data))
                      , -grep("group", names(ps_data)), -grep("random", names(ps_data)))]

#Drop totally irrelevant variables
ps_data = ps_data[, c(-match("documentno", names(ps_data)), -match("accidenttime", names(ps_data)), -grep("date", names(ps_data))
                      , -match("longitude", names(ps_data)), -match("latitude", names(ps_data)), -match("bomstatecode", names(ps_data))
                      , -match("nearesttown", names(ps_data)), -match("narrative", names(ps_data)), -match("minestatus", names(ps_data))
                      , -match("minename", names(ps_data)))]

#Drop variables we are confused about
ps_data = ps_data[, c(-match("i", names(ps_data)), -match("idesc", names(ps_data)), -match("oldoccupationcode", names(ps_data))
                      , -match("equip", names(ps_data)), -match("ai_acty_cd_old", names(ps_data)), -match("ai_dt_old", names(ps_data))
                      , -match("ai_time_old", names(ps_data)))]

##Missing Value Imputation##

#We don't use date vars as of yet so no need to store a list of their names, "logical" class vars are missing all obsvtns
var_classes = sapply(ps_data[,names(ps_data)], class)
charac_vars = names(var_classes[c(grep("character", var_classes), grep("factor", var_classes))])
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])
ps_data = ps_data[, -grep("logical", var_classes)]

for (i in 1:length(charac_vars)) {
  ps_data[, charac_vars[i]] = ifelse((ps_data[,charac_vars[i]] == "NO VALUE FOUND" | ps_data[,charac_vars[i]] == "UNKNOWN" | 
                                        ps_data[,charac_vars[i]] == "?" | ps_data[,charac_vars[i]] == ""), NA_character_, as.character(ps_data[,charac_vars[i]]))
  ps_data[, charac_vars[i]] = factor(ps_data[, charac_vars[i]])
}

#Must define function to calculate mode for imputation methods 1 & 2
modus = function(x) {
  uniqv = unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

if (imputation_method == 1 | imputation_method == 2) {
  for (i in 1:length(num_vars)) {
    ps_data[, num_vars[i]] = ifelse(is.na(ps_data[, num_vars[i]]), mean(ps_data[, num_vars[i]]), ps_data[, num_vars[i]])
  }
  if (imputation_method == 2) {
    for (i in 1:length(num_vars)) {
      ps_data[, num_vars[i]] = ifelse(is.na(ps_data[, num_vars[i]]), median(ps_data[, num_vars[i]]), ps_data[, num_vars[i]])
    }
  }
  for (i in 1:length(charac_vars)) {
    ps_data[, charac_vars[i]] = ifelse(is.na(ps_data[, charac_vars[i]]), modus(ps_data[, charac_vars[i]]), ps_data[, charac_vars[i]])
  }
} else if (imputation_method == 3) {
  for (i in 1:length(num_vars)) {
    i_rowsmissing = row.names(ps_data)[is.na(ps_data[, num_vars[i]])]
    while (sum(!complete.cases(ps_data[, num_vars[i]])) > 0) {
      replace_rows = sample(setdiff(row.names(ps_data), i_rowsmissing), length(i_rowsmissing), replace = T)
      ps_data[i_rowsmissing, num_vars[i]] = ps_data[replace_rows, num_vars[i]]
    }
  }
  for (i in 1:length(charac_vars)) {
    i_rowsmissing = row.names(ps_data)[is.na(ps_data[, charac_vars[i]])]
    while (sum(!complete.cases(ps_data[, charac_vars[i]])) > 0) {
      replace_rows = sample(setdiff(row.names(ps_data), i_rowsmissing), length(i_rowsmissing), replace = T)
      ps_data[i_rowsmissing, charac_vars[i]] = ps_data[replace_rows, charac_vars[i]]
    }
  }
} else {
  #Not to be implemented as of now. 4/15/16
}

##Dummying-Out Factors with more than 53 categories (An issue for RF analysis)

new_dummies = apply(cbind(dummy(ps_data$sourceofinjury), dummy(ps_data$occupation), dummy(ps_data$equipmentmodelno), dummy(ps_data$fipscountyname),
                          dummy(ps_data$controllername), dummy(ps_data$operatorname), dummy(ps_data$controllerid), dummy(ps_data$operatorid)),
                    MARGIN = 2, FUN = function(x) factor(x))
#Memory issues with the next line so, for now, this is mostly to outline what ideally should happen at this stage. 4/16/16
#ps_data = merge(ps_data, data.frame(new_dummies))
