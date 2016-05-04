# This code pulls in the merged accidents-mines data (created in clean-accidents.R), cleans it, imputes missing values,
# appends the training set (created in prepare-MR.R) and runs a random forest and boosting. 

install.packages("zoo")
library(zoo)

setwd("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/")
mines.accidents = read.csv("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/prepped_mines_accidents.csv", header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)

imputation_method = 3

######################################################################################################
# CLEAN NARRATIVE FIELDS: DROP REDUNDANT VARS AND KEEP LOWERCASE VERSION
mines.accidents$narrative = tolower(mines.accidents$narrative)
mines.accidents$narrative = tolower(mines.accidents$narrative)
mines.accidents$narrative = tolower(mines.accidents$narrative)
mines.accidents$narrative = tolower(mines.accidents$narrative)
mines.accidents$narrative = tolower(mines.accidents$narrative)
mines.accidents$narrative = tolower(mines.accidents$narrative)

# DESTRING VARIABLES
mines.accidents[,grep("numberofemployees", names(mines.accidents))] = gsub(pattern = ",",replacement =  "", mines.accidents[,grep("numberofemployees", names(mines.accidents))])
mines.accidents[,grep("numberofemployees", names(mines.accidents))] = as.numeric(mines.accidents[,grep("numberofemployees", names(mines.accidents))])
mines.accidents[,grep("methaneliberation", names(mines.accidents))] = gsub(pattern = ",",replacement =  "", mines.accidents[,grep("methaneliberation", names(mines.accidents))])
mines.accidents[,grep("methaneliberation", names(mines.accidents))] = as.numeric(mines.accidents[,grep("methaneliberation", names(mines.accidents))])
mines.accidents[,grep("averagemineheight", names(mines.accidents))] = gsub(pattern = ",",replacement =  "", mines.accidents[,grep("averagemineheight", names(mines.accidents))])
mines.accidents[,grep("averagemineheight", names(mines.accidents))] = as.numeric(mines.accidents[,grep("averagemineheight", names(mines.accidents))])

# MERGE REDUNDANT "NO VALUE FOUND" FIELDS IN FACTOR VARIABLES
mines.accidents[, "uglocation"] = ifelse(mines.accidents[, "uglocation"] == "NOT MARKED", "NO VALUE FOUND", mines.accidents[, "uglocation"])
mines.accidents[, "immediatenotificationclass"] = ifelse(mines.accidents[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", mines.accidents[, "immediatenotificationclass"])
mines.accidents[, "natureofinjury"] = ifelse(mines.accidents[, "natureofinjury"] == "UNCLASSIFIED,NOT DETERMED", "NO VALUE FOUND", mines.accidents[, "natureofinjury"])
mines.accidents[, "equipmanufacturer"] = ifelse(mines.accidents[, "equipmanufacturer"] == "Not Reported", "NO VALUE FOUND", mines.accidents[, "equipmanufacturer"])

######################################################################################################
# 60 NARRATIVE FIELDS ARE POLLUTED WITH OTHER COLUMNS - SPLIT AND REPLACE THESE 
mines.accidents[, "messy"] = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", mines.accidents[,"narrative"]), 1, 0)
narrative_split = strsplit(mines.accidents[mines.accidents$messy == 1, "narrative"], "|", fixed = T)
messy_rows = row.names(mines.accidents[mines.accidents$messy == 1, ])
for (i in 1:length(messy_rows)) {
  mines.accidents[messy_rows[i], "narrative"] = unlist(narrative_split[i])[1]
  mines.accidents[messy_rows[i], "occupcode3digit"] = unlist(narrative_split[i])[2]
  mines.accidents[messy_rows[i], "occupation"] = unlist(narrative_split[i])[3]
  mines.accidents[messy_rows[i], "returntoworkdate"] = unlist(narrative_split[i])[4]
}
mines.accidents = mines.accidents[, c(-match("messy", names(mines.accidents)))]

# CONVERT DATES - THIS NEEDS TO HAPPEN AFTER REPLACING RETURNTOWORKDATE WITH EXTRACTS FROM NARRATIVE FIELDS
indices_with_date = grep("date", names(mines.accidents))
for (i in indices_with_date) {
  mines.accidents[,i] = as.Date(mines.accidents[,i], "%m/%d/%Y")
}

######################################################################################################
# ADD NEW KEYWORD VARS

# GENERATE LIKELY POSITIVE KEYWORDS (LIKELY TO INDICATE POSITIVE OUTCOMES) 

# *REPAIR* 
mines.accidents[, "repair"] = ifelse(grepl("(^| )r(e|a)(p|[0-9])a(i*)r[a-z]*", mines.accidents[,"narrative"]) &
                               !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,20}hernia", mines.accidents[,"narrative"]) &
                               !grepl("hernia.{1,10}r(e|a)(p|[0-9])a(i*)r[a-z]*", mines.accidents[,"narrative"]) &
                               !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,10}wound", mines.accidents[,"narrative"]) &
                               !grepl("wound.{1,20}r(e|a)(p|[0-9])a(i*)r[a-z]*", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "rplace"] = ifelse(grepl("(^| )replac(e|i)[a-z]*", mines.accidents[,"narrative"]), 1, 0)
# We don't want to see the noun "service" because that often refers to hoist service, but "serviced" and "servicing" are good indicators
mines.accidents[, "service"] = ifelse(grepl("serviced", mines.accidents[,"narrative"]) | grepl("servicing", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "fix"] = ifelse(grepl("(^| )fix[a-z]*", mines.accidents[,"narrative"]) & !grepl("(^| )fixture", mines.accidents[,"narrative"]), 1, 0) 
mines.accidents[, "changing"] = ifelse(grepl("chang(e|ing|ed)( )*out", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "retrack"] = ifelse(grepl("re(rail|track|trakc)(ed|ing)", mines.accidents[,"narrative"]) |
                                grepl("pull(ing|ed)*.{1,5}track", mines.accidents[,"narrative"]), 1, 0)

mines.accidents[, "pullbelt"] = ifelse(grepl("pull( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mines.accidents[,"narrative"]) |
                                 grepl("(belt|rope|spool|tube|tubing).{1,15}pull( |ing|ed|s)*", mines.accidents[,"narrative"]) |
                                 grepl("(belt|rope|spool|tube|tubing).{1,15}load( |ing|ed|s)*", mines.accidents[,"narrative"]) |
                                 grepl("load( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "reposition"] = ifelse(grepl("re( )*pos(i|t)(i|t)(i|o)(i|o)n", mines.accidents[,"narrative"]), 1, 0) 
# sometimes even when the occupation field isn't MR, the narrative field refers to the job of the injured, or to the injured ee helping an MR worker 
mines.accidents[, "mrworker"] = ifelse(grepl("(mechanic|electrician|repairm(a|e)n)", mines.accidents[,"narrative"]), 1, 0) 
mines.accidents[, "cover"] = ifelse(grepl("(replac|lift).{1,20}(panel|cover| lid)", mines.accidents[,"narrative"]) |
                              grepl("(panel|cover| lid){1,5}fell", mines.accidents[,"narrative"]) &
                              !grepl("eye.{1,5}lid)", mines.accidents[,"narrative"]), 1, 0) 

# *MAINTENANCE* 

mines.accidents[, "cleaning"] = ifelse(grepl("clean", mines.accidents[,"narrative"]), 1, 0) 
mines.accidents[, "maintain"] = ifelse(grepl("(^| )maintain[a-z]*", mines.accidents[,"narrative"]), 1, 0)
#Try to avoid inspection/inspector
mines.accidents[, "inspect"] = ifelse(grepl("inspect( |ed|s|ing|\\.|,|$)", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "shovel"] = ifelse(grepl("shovel(ing|ed).{1,5}coal)", mines.accidents[,"narrative"]) |
                               grepl("coal.{1,15}shovel(ing|ed)", mines.accidents[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}belt", mines.accidents[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}convey(e|o)r", mines.accidents[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}tail( )*p(e|i)(e|i)ce", mines.accidents[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}(head|drive|guide|bend|lagged|tail)*( )*pull(y|ey|ies|ys)", mines.accidents[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}(roller|idler)", mines.accidents[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}(west|header|drive)", mines.accidents[,"narrative"]), 1, 0)
# We don't want the noun "hose" (or "whose") just the verb - also dont want "bullhose" or any of that nonsense
mines.accidents[, "washingdown"] = ifelse(grepl("( |^|\\.|,)(wash|hose)(d|ed|ing| )", mines.accidents[,"narrative"]), 1, 0) 
mines.accidents[, "grease"] = ifelse(grepl("greas(ed|ing|e|er)", mines.accidents[,"narrative"]), 1, 0) 
# Try to avoid "doctor checked out injury..."
mines.accidents[, "check"] = ifelse(grepl("che(c|k)(c|k)", mines.accidents[,"narrative"]) &
                              !grepl("doctor", mines.accidents[,"narrative"]) &
                              !grepl("clinic", mines.accidents[,"narrative"]), 1, 0) 
# Oil in mention of can/drum/barrel often means something is being greased. otherwise it usually apears in some other context (being slipped on, lit, etc...)
mines.accidents[, "oil"] = ifelse(grepl("(^| )oil.{1,25}( can|drum|barrel)", mines.accidents[,"narrative"]) |
                            grepl("(can|drum|barrel).{1,25} oil", mines.accidents[,"narrative"]), 1, 0) 

# GENERATE POTENTIALLY POSITIVE KEYWORDS (MAYBE INDICATE POSITIVE OUTCOMES?) 

# *REPAIR* 

mines.accidents[, "dismantl"] = ifelse(grepl("dismant(el|le|al|il|l)", mines.accidents[,"narrative"]), 1, 0) 
mines.accidents[, "rethread"] = ifelse(grepl("re( )*thr(ea|e)d", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "remove"] = ifelse(grepl("remov(e|ed|ing)", mines.accidents[,"narrative"]), 1, 0) 
#                             & grepl("broken", mines.accidents[,"narrative"]) & grepl("flanged", mines.accidents[,"narrative"])
#                             & grepl("old", mines.accidents[,"narrative"]) & grepl("bent", mines.accidents[,"narrative"]) 
#                            & grepl("busted", mines.accidents[,"narrative"]) , 1, 0) 

# *MAINTENANCE* 

mines.accidents[, "bits"] = ifelse(grepl("set(t)*(ing)*( )*bits", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "conveyor"] = ifelse(grepl("convey(o|e)r", mines.accidents[,"narrative"]), 1, 0)
# We don't want "help" 
mines.accidents[, "helping"] = ifelse(grepl("help(ed|ing)", mines.accidents[,"narrative"]) |
                                grepl("assis(s)*t(ed|ing)*", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "belt"] = ifelse(grepl("belt|spool|tube|tubing", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "tighten"] = ifelse(grepl("tighten", mines.accidents[,"narrative"]), 1, 0)

# DEAL WITH THE WORD "INSTALLATION". INSTALLS ARE NOT M&R IF RELEVANT TO CONSTRUCTION/PRODUCTION, LIKE WITH ROOF BOLTING

mines.accidents[, "roof.bolt"] = ifelse(grepl("(roof|rib)*( )*(bolt)(er|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mines.accidents[,"narrative"]) | 
                                  grepl("(roof|rib)*( )*(bolt)(er|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mines.accidents[,"narrative"]), 1, 0)  
mines.accidents[, "rib.hole"] = ifelse(grepl("(rib)( )*(hole).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mines.accidents[,"narrative"]) | 
                                 grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(rib)( )*(hole)", mines.accidents[,"narrative"]), 1, 0)  
# Accounts for install, reinstall, uninstall (but not "an installed cable" or something like that)
mines.accidents[, "install"] = ifelse(grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mines.accidents[,"narrative"]) &
                                !grepl("(^| )an( |e|n)i(s|n|t)(s|n|t)(s|n|t)alled", mines.accidents[,"narrative"]) 
                              & (mines.accidents[, "rib.hole"] != 1 & mines.accidents[, "roof.bolt"] != 1), 1, 0)

# NEGATIVE KEYWORDS (LIKELY TO INDICATE NEGATIVE OUTCOMES) 

mines.accidents[, "pain"] = ifelse(grepl("(^| )pain( |$|\\.|,|:|)", mines.accidents[,"narrative"]), 1, 0)
# Make sure "hoisting" or "hoisted" aren't grabbed
mines.accidents[, "hoist"] = ifelse(((grepl("(^| )hoist(s| |$|\\.|,|:)", mines.accidents[,"narrative"]) |
                                grepl("(^| )el(e|a|i)vat(o|e)r*", mines.accidents[,"narrative"])) & mines.accidents[, "pain"] == 0), 1, 0) 
mines.accidents[, "surgery"] = ifelse((grepl("surger[a-z]*", mines.accidents[,"narrative"]) | 
                                 grepl("surgic[a-z]*", mines.accidents[,"narrative"])) & mines.accidents[, "pain"] == 0, 1, 0)

######################################################################################################
# GENERATE ADDITIONAL KEYWORDS WE HAVE NO PRIORS ABOUT TO FEED INTO RANDOM FOREST 

mines.accidents[, "trash"] = ifelse(grepl("(trash|garbage)", mines.accidents[,"narrative"]), 1, 0)
mines.accidents[, "roller"] = ifelse(grepl("roller", mines.accidents[,"narrative"]), 1, 0)

######################################################################################################
# CREATE SIMPLE DATA CONTAINING JUST THE VARS USED FOR SIMPLE ALGORITHM - DO THIS BEFORE IMPUTATION HAPPENS 
prepped.MR = mines.accidents[, c(match("repair", names(mines.accidents)), match("narrative", names(mines.accidents)),
                          match("rplace", names(mines.accidents)), match("service", names(mines.accidents)),
                          match("fix", names(mines.accidents)),  match("changing", names(mines.accidents)),
                          match("retrack", names(mines.accidents)), match("pullbelt", names(mines.accidents)),
                          match("reposition", names(mines.accidents)), match("mrworker", names(mines.accidents)),
                          match("cover", names(mines.accidents)), match("cleaning", names(mines.accidents)),
                          match("maintain", names(mines.accidents)), match("inspect", names(mines.accidents)),
                          match("shovel", names(mines.accidents)), match("washingdown", names(mines.accidents)),
                          match("grease", names(mines.accidents)), match("check", names(mines.accidents)),
                          match("oil", names(mines.accidents)), match("dismantl", names(mines.accidents)),
                          match("rethread", names(mines.accidents)), match("remove", names(mines.accidents)),
                          match("bits", names(mines.accidents)), match("conveyor", names(mines.accidents)),
                          match("helping", names(mines.accidents)), match("belt", names(mines.accidents)),
                          match("tighten", names(mines.accidents)), match("install", names(mines.accidents)),
                          match("hoist", names(mines.accidents)), match("surgery", names(mines.accidents)),                          
                          match("pain", names(mines.accidents)), match("mineractivity", names(mines.accidents)),
                          match("occupation", names(mines.accidents)), match("degreeofinjury", names(mines.accidents)),
                          match("accidentclassification", names(mines.accidents)), match("accidenttype", names(mines.accidents)))]

prepped.MR[, "likely.occup"] = ifelse(grepl("maintenance", prepped.MR[,"occupation"]), 1, 0)
prepped.MR[, "maybe.occup"] = ifelse(grepl("electrician", prepped.MR[,"occupation"]) , 1, 0)
prepped.MR[, "likely.activy"] = ifelse(grepl("maintenance", prepped.MR[,"mineractivity"]) | 
                                          grepl("wet down working place", prepped.MR[,"mineractivity"]), 1, 0)
prepped.MR[, "maybe.activy"] = ifelse(match("handling supplies/materials", prepped.MR[,"mineractivity"]) |
                                         match("hand tools (not powered)", prepped.MR[,"mineractivity"]) |
                                         match("no value found", prepped.MR[,"mineractivity"]) |
                                         match("unknown", prepped.MR[,"mineractivity"]) | 
                                         match("clean up", prepped.MR[,"mineractivity"]) | 
                                         match("inspect equipment", prepped.MR[,"mineractivity"]), 1, 0)
prepped.MR[, "likely.class"] = ifelse(match("handtools (nonpowered)", prepped.MR[,"accidentclassification"]) |
                                         match("machinery", prepped.MR[,"accidentclassification"]) |
                                         match("electrical", prepped.MR[,"accidentclassification"]), 1, 0)
prepped.MR[, "unlikely.class"] = ifelse(match("fall of roof or back", prepped.MR[,"accidentclassification"]) |
                                           match("struck by falling object", prepped.MR[,"accidenttype"]), 1, 0)

prepped.MR$false.keyword = ifelse(prepped.MR$hoist == 1 | prepped.MR$surgery == 1, 1, 0)
prepped.MR$likely.keyword = ifelse((prepped.MR$repair == 1 | prepped.MR$fix == 1 | 
                                       prepped.MR$maintain == 1 | prepped.MR$rplace == 1 |
                                       prepped.MR$install == 1 | prepped.MR$service == 1 |
                                       prepped.MR$cleaning == 1 | prepped.MR$changing == 1 |
                                       prepped.MR$retrack == 1 | prepped.MR$inspect == 1 |
                                       prepped.MR$shovel == 1 | prepped.MR$reposition == 1 | 
                                       prepped.MR$pullbelt == 1 | prepped.MR$grease == 1 |
                                       prepped.MR$washingdown == 1 | prepped.MR$check == 1 |
                                       prepped.MR$oil == 1 | prepped.MR$mrworker == 1 |                                      
                                       prepped.MR$cover == 1 ) & prepped.MR$false.keyword == 0, 1, 0)

prepped.MR$maybe.keyword = ifelse( (prepped.MR$remove == 1 | prepped.MR$dismantl == 1 | prepped.MR$rethread == 1 | 
                                       prepped.MR$bits == 1 | prepped.MR$helping == 1 |
                                       prepped.MR$conveyor == 1 | prepped.MR$belt == 1 |
                                       prepped.MR$tighten == 1 ) & prepped.MR$false.keyword == 0, 1, 0)

# GENERATE OTHER USEFUL FLAGS ABOUT ACCIDENT 
prepped.MR$falling.class = ifelse(prepped.MR$accidentclassification == "fall of roof or back", 1, 0)
prepped.MR[, "falling.word"] = ifelse(grepl("rock( )*fell", prepped.MR[,"narrative"]) |
                                         !grepl("fell.{1,20}roof", prepped.MR[,"narrative"]) |
                                         !grepl("roof( )*f(a|e)ll", prepped.MR[,"narrative"]), 1, 0)
prepped.MR$falling.accident = ifelse(prepped.MR$falling.class == 1 | prepped.MR$falling.word == 1, 1, 0)

prepped.MR$accident.only = ifelse( (prepped.MR$degreeofinjury == "accident only" | prepped.MR$accidenttype == "acc type, without injuries"), 1, 0)


prepped.MR = prepped.MR[, c(-match("degreeofinjury", names(prepped.MR)), -match("occupation", names(prepped.MR)),
                              -match("falling.class", names(prepped.MR)), -match("falling.word", names(prepped.MR)),
                              -match("accidentclassification", names(prepped.MR)), -match("mineractivity", names(prepped.MR)),
                              -match("accidenttype", names(prepped.MR)), -match("narrative", names(prepped.MR)))]

###################################################################################################################################
# PREP FINAL VARS FOR ANALYSIS

# STORE THE TYPE OF ALL VARIABLES AND CREATE LISTS OF VARS BY TYPE
var_classes = sapply(mines.accidents[,names(mines.accidents)], class)
# "character" vars are factor variables
charac_vars = names(var_classes[c(grep("character", var_classes), grep("factor", var_classes))])
# "logical" vars are missing all observations - these variables will be dropped (0 actually dropped here)
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])

for (i in 1:length(charac_vars)) {
  print(i)
  mines.accidents[, charac_vars[i]] = ifelse((mines.accidents[,charac_vars[i]] == "NO VALUE FOUND" | mines.accidents[,charac_vars[i]] == "UNKNOWN" | 
                                        mines.accidents[,charac_vars[i]] == "?" | mines.accidents[,charac_vars[i]] == ""), NA_character_, as.character(mines.accidents[,charac_vars[i]]))
  mines.accidents[, charac_vars[i]] = factor(mines.accidents[, charac_vars[i]])
}

# DEFINE FUNCTION FOR MODES FOR IMPUTATION
modus = function(x) {
  uniqv = unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

######################################################################################################
# IMPUTE MISSING VARS
if (imputation_method == 1 | imputation_method == 2) {
  for (i in 1:length(num_vars)) {
    mines.accidents[, num_vars[i]] = ifelse(is.na(mines.accidents[, num_vars[i]]), mean(mines.accidents[, num_vars[i]]), mines.accidents[, num_vars[i]])
  }
  if (imputation_method == 2) {
    for (i in 1:length(num_vars)) {
      mines.accidents[, num_vars[i]] = ifelse(is.na(mines.accidents[, num_vars[i]]), median(mines.accidents[, num_vars[i]]), mines.accidents[, num_vars[i]])
    }
  }
  for (i in 1:length(charac_vars)) {
    mines.accidents[, charac_vars[i]] = ifelse(is.na(mines.accidents[, charac_vars[i]]), modus(mines.accidents[, charac_vars[i]]), mines.accidents[, charac_vars[i]])
  }
} else if (imputation_method == 3) {
  for (i in 1:length(num_vars)) {
    i_rowsmissing = row.names(mines.accidents)[is.na(mines.accidents[, num_vars[i]])]
    while (sum(!complete.cases(mines.accidents[, num_vars[i]])) > 0) {
      replace_rows = sample(setdiff(row.names(mines.accidents), i_rowsmissing), length(i_rowsmissing), replace = T)
      mines.accidents[i_rowsmissing, num_vars[i]] = mines.accidents[replace_rows, num_vars[i]]
    }
  }
  for (i in 1:length(charac_vars)) {
    i_rowsmissing = row.names(mines.accidents)[is.na(mines.accidents[, charac_vars[i]])]
    while (sum(!complete.cases(mines.accidents[, charac_vars[i]])) > 0) {
      replace_rows = sample(setdiff(row.names(mines.accidents), i_rowsmissing), length(i_rowsmissing), replace = T)
      mines.accidents[i_rowsmissing, charac_vars[i]] = mines.accidents[replace_rows, charac_vars[i]]
    }
  }
} 

######################################################################################################
# OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE BALANCED DATA W ROSE

prepped.MR$MR = ""
prepped.MR$train = 0
simple.data$train = 1
accidents = rbind(prepped.MR, simple.data)
accidents = accidents[order(-accidents$train),]

mr.adaboost = boosting(MR ~ ., data = simple[1:800,], boos = F, mfinal = 100, coeflearn = 'Freund')
adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[801:1019,])
adaboost.pred$confusion

######################################################################################################