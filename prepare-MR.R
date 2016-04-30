# This code pulls in 1000 observations from the manually coded training set for maintenance and repair (MR)
# injuries. It then appends 23 MR fatalities that were scraped from MSHA: http://arlweb.msha.gov/fatals/coal/2014/
# It then prepares all variables for analyses in CART-RF-MR.R by formatting, imputing missing values, 
# dropping extraneous parameters, and dummying out large factor variables. We produce two datasets: A "training"
# set contianing all potentially relevant variables, and a "simple" dataset, containing only those variables
# used in the "simple-algorithm."

install.packages("zoo")
library(zoo)

######################################################################################################
# SET PREFERENCES - IMPUTATION METHOD - METHOD 3 IS RANDOM DRAWS FROM DISTRIBUTION (OUR BEST METHOD)
setwd("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/")

imputation_method = 3

# LOAD IN CODED TRAINING SET (1000 OBSERVATIONS, CODED FOR "MR")
mr.data = read.csv("Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)

# LOAD IN DATASET OF ADDITIONAL FATALITIES (FROM OPEN DATA) TO APPEND TO OUR TRAINING SET - ALL "MR"
mr.fatalities = read.csv("X:/Projects/Mining/NIOSH/analysis/data/4_coded/coded_MR_fatalities.csv", header = TRUE, sep = ",", nrows = 24, stringsAsFactors = FALSE)

######################################################################################################
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

# MAKE MR A FACTOR VARIABLE
mr.data[, "MR"] = factor(ifelse(mr.data[, "MR"] == 1, "YES", "NO"))
names(mr.data)[names(mr.data) == "MR"] = "MR"

######################################################################################################
# CLEAN UP ALL VARS 

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

# We decided to recode three observations in our data that were coded as MR, but are apparently non-injury accidents. 
# It's apparent that whoever did the coding didn't look at this field. We don't ever want our algorithm to classify 
# an acident-only observation as positive for MR, so we enforce this change.  
mr.data$accident.only = ifelse(mr.data$degreeofinjury == "accident only" | mr.data$accidenttype == "acc type, without injuries", 1, 0)
mr.data$MR = ifelse(mr.data$MR == "YES" & mr.data$accident.only == 0, 1, 0)
mr.data$MR[mr.data$MR == "YES" & mr.data$accident.only == 1] = 0

# MAKE SURE MR IS STILL A FACTOR VARIABLE
mr.data[, "MR"] = factor(ifelse(mr.data[, "MR"] == 1, "YES", "NO"))
names(mr.data)[names(mr.data) == "MR"] = "MR"

######################################################################################################
# 60 NARRATIVE FIELDS ARE POLLUTED WITH OTHER COLUMNS - SPLIT AND REPLACE THESE 
mr.data[, "messy"] = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", mr.data[,"narrative"]), 1, 0)
narrative_split = strsplit(mr.data[mr.data$messy == 1, "narrative"], "|", fixed = T)
messy_rows = row.names(mr.data[mr.data$messy == 1, ])
for (i in 1:length(messy_rows)) {
  mr.data[messy_rows[i], "narrative"] = unlist(narrative_split[i])[1]
  mr.data[messy_rows[i], "occupcode3digit"] = unlist(narrative_split[i])[2]
  mr.data[messy_rows[i], "occupation"] = unlist(narrative_split[i])[3]
  mr.data[messy_rows[i], "returntoworkdate"] = unlist(narrative_split[i])[4]
}
mr.data = mr.data[, c(-match("messy", names(mr.data)))]

# CONVERT DATES - THIS NEEDS TO HAPPEN AFTER REPLACING RETURNTOWORKDATE WITH EXTRACTS FROM NARRATIVE FIELDS
indices_with_date = grep("date", names(mr.data))
for (i in indices_with_date) {
  mr.data[,i] = as.Date(mr.data[,i], "%m/%d/%Y")
}

######################################################################################################
# ADD NEW KEYWORD VARS

# GENERATE LIKELY POSITIVE KEYWORDS (LIKELY TO INDICATE POSITIVE OUTCOMES) 

# *REPAIR* 
mr.data[, "repair"] = ifelse(grepl("(^| )r(e|a)pa(i*)r[a-z]*", mr.data[,"narrative"]) &
                             !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,20}hernia", mr.data[,"narrative"]) &
                             !grepl("hernia.{1,10}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]) &
                             !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,10}wound", mr.data[,"narrative"]) &
                             !grepl("wound.{1,20}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]), 1, 0)
mr.data[, "rplace"] = ifelse(grepl("(^| )replac(e|i)[a-z]*", mr.data[,"narrative"]), 1, 0)
# We don't want to see the noun "service" because that often refers to hoist service, but "serviced" and "servicing" are good indicators
mr.data[, "service"] = ifelse(grepl("serviced", mr.data[,"narrative"]) | grepl("servicing", mr.data[,"narrative"]), 1, 0)
mr.data[, "fix"] = ifelse(grepl("(^| )fix[a-z]*", mr.data[,"narrative"]) & !grepl("(^| )fixture", mr.data[,"narrative"]), 1, 0) 
mr.data[, "changing"] = ifelse(grepl("chang(e|ing|ed)( )*out", mr.data[,"narrative"]), 1, 0)
mr.data[, "retrack"] = ifelse(grepl("re(rail|track|trakc)(ed|ing)", mr.data[,"narrative"]), 1, 0)
mr.data[, "pullbelt"] = ifelse(grepl("pull( |ing|ed|s)*.{1,20}(belt|rope|tube|tubing)", mr.data[,"narrative"]) |
                               grepl("(belt|rope|spool|tube|tubing).{1,20}pull( |ing|ed|s)*", mr.data[,"narrative"]) |
                               grepl("(belt|rope|spool|tube|tubing).{1,20}load( |ing|ed|s)*", mr.data[,"narrative"]) |
                               grepl("load( |ing|ed|s)*.{1,20}(belt|rope|tube|tubing)", mr.data[,"narrative"]), 1, 0)
mr.data[, "reposition"] = ifelse(grepl("sre( )*pos(i|t)(i|t)(i|o)(i|o)n", mr.data[,"narrative"]), 1, 0) 
# sometimes even when the occupation field isn't MR, the narrative field refers to the job of the injured, or to the injured ee helping an MR worker 
mr.data[, "mrworker"] = ifelse(grepl("(mechanic|electrician|repairm(a|e)n)", mr.data[,"narrative"]), 1, 0) 
mr.data[, "cover"] = ifelse(grepl("(replac|lift).{1,20}(panel|cover| lid)", mr.data[,"narrative"]) |
                            grepl("(panel|cover| lid){1,5}fell", mr.data[,"narrative"]) &
                            !grepl("eye.{1,5}lid)", mr.data[,"narrative"]), 1, 0) 
                            
# *MAINTENANCE* 

mr.data[, "cleaning"] = ifelse(grepl("clean", mr.data[,"narrative"]), 1, 0) 
mr.data[, "maintain"] = ifelse(grepl("(^| )maintain[a-z]*", mr.data[,"narrative"]), 1, 0)
#Try to avoid inspection/inspector
mr.data[, "inspect"] = ifelse(grepl("inspect( |ed|s|ing|\\.|,|$)", mr.data[,"narrative"]), 1, 0)
mr.data[, "shovel"] = ifelse(grepl("shovel(ing|ed).{1,5}coal)", mr.data[,"narrative"]) |
                             grepl("coal.{1,15}shovel(ing|ed)", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}belt", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}convey(e|o)r", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}tail( )*p(e|i)(e|i)ce", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}(head|drive|guide|bend|lagged|tail)*( )*pull(y|ey|ies|ys)", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}(roller|idler)", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}(west|header|drive)", mr.data[,"narrative"]), 1, 0)
# We don't want the noun "hose" (or "whose") just the verb - also dont want "bullhose" or any of that nonsense
mr.data[, "washingdown"] = ifelse(grepl("( |^|\\.|,)(wash|hose)(d|ed|ing| )", mr.data[,"narrative"]), 1, 0) 
mr.data[, "grease"] = ifelse(grepl("greas(ed|ing|e|er)", mr.data[,"narrative"]), 1, 0) 
# Try to avoid "doctor checked out injury..."
mr.data[, "check"] = ifelse(grepl("che(c|k)(c|k)", mr.data[,"narrative"]) &
                            !grepl("doctor", mr.data[,"narrative"]) &
                            !grepl("clinic", mr.data[,"narrative"]), 1, 0) 
# Oil in mention of can/drum/barrel often means something is being greased. otherwise it usually apears in some other context (being slipped on, lit, etc...)
mr.data[, "oil"] = ifelse(grepl("(^| )oil.{1,25}( can|drum|barrel)", mr.data[,"narrative"]) |
                          grepl("(can|drum|barrel).{1,25} oil", mr.data[,"narrative"]), 1, 0) 

# GENERATE POTENTIALLY POSITIVE KEYWORDS (MAYBE INDICATE POSITIVE OUTCOMES?) 

# *REPAIR* 

mr.data[, "dismantl"] = ifelse(grepl("dismant(el|le|al|il|l)", mr.data[,"narrative"]), 1, 0) 
mr.data[, "rethread"] = ifelse(grepl("re( )*thr(ea|e)d", mr.data[,"narrative"]), 1, 0)
mr.data[, "remove"] = ifelse(grepl("remov(e|ed|ing)", mr.data[,"narrative"]), 1, 0) 
#                             & grepl("broken", mr.data[,"narrative"]) & grepl("flanged", mr.data[,"narrative"])
#                             & grepl("old", mr.data[,"narrative"]) & grepl("bent", mr.data[,"narrative"]) 
#                            & grepl("busted", mr.data[,"narrative"]) , 1, 0) 

# *MAINTENANCE* 

mr.data[, "bits"] = ifelse(grepl("set(t)*(ing)*( )*bits", mr.data[,"narrative"]), 1, 0)
mr.data[, "conveyor"] = ifelse(grepl("convey(o|e)r", mr.data[,"narrative"]), 1, 0)
# We don't want "help" 
mr.data[, "helping"] = ifelse(grepl("help(ed|ing)", mr.data[,"narrative"]) |
                               grepl("assis(s)*t(ed|ing)*", mr.data[,"narrative"]), 1, 0)
mr.data[, "belt"] = ifelse(grepl("belt|spool|tube|tubing", mr.data[,"narrative"]), 1, 0)
mr.data[, "tighten"] = ifelse(grepl("tighten", mr.data[,"narrative"]), 1, 0)

# DEAL WITH THE WORD "INSTALLATION". INSTALLS ARE NOT M&R IF RELEVANT TO CONSTRUCTION/PRODUCTION, LIKE WITH ROOF BOLTING

mr.data[, "roof.bolt"] = ifelse(grepl("(roof|rib)*( )*(bolt)(er|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) | 
                                grepl("(roof|rib)*( )*(bolt)(er|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]), 1, 0)  
mr.data[, "rib.hole"] = ifelse(grepl("(rib)( )*(hole).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) | 
                               grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(rib)( )*(hole)", mr.data[,"narrative"]), 1, 0)  
mr.data[, "install"] = ifelse(grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) 
                              & (mr.data[, "rib.hole"] != 1 & mr.data[, "roof.bolt"] != 1), 1, 0)

# NEGATIVE KEYWORDS (LIKELY TO INDICATE NEGATIVE OUTCOMES) 

mr.data[, "pain"] = ifelse(grepl("(^| )pain( |$|\\.|,|:|)", mr.data[,"narrative"]), 1, 0)
# Make sure "hoisting" or "hoisted" aren't grabbed
mr.data[, "hoist"] = ifelse(((grepl("(^| )hoist(s| |$|\\.|,|:)", mr.data[,"narrative"]) |
                              grepl("(^| )el(e|a|i)vat(o|e)r*", mr.data[,"narrative"])) & mr.data[, "pain"] == 0), 1, 0) 
mr.data[, "surgery"] = ifelse((grepl("surger[a-z]*", mr.data[,"narrative"]) | 
                              grepl("surgic[a-z]*", mr.data[,"narrative"])) & mr.data[, "pain"] == 0, 1, 0)

######################################################################################################
# GENERATE ADDITIONAL KEYWORDS WE HAVE NO PRIORS ABOUT TO FEED INTO RANDOM FOREST 

# belt chute, belt structure, belt line, conveyor chain, conveyor belt, tubing rope, stacker belt, slope belt
# roller, tail roller, bottom roller
# speed reducer (never comes off scoop motor if not beign repair)
# tightening / unlugging
# miner sprays 
# add "testing' to checking
mr.data[, "trash"] = ifelse(grepl("(trash|garbage)", mr.data[,"narrative"]), 1, 0)
mr.data[, "roller"] = ifelse(grepl("roller", mr.data[,"narrative"]), 1, 0)

# tightening bolts, tightening conveyor chain, tightening cables 
# batteries 
# repair a wound, repair a hernia, repair an injury, repair 

######################################################################################################
# CREATE SIMPLE DATA CONTAINING JUST THE VARS USED FOR SIMPLE ALGORITHM - DO THIS BEFORE IMPUTATION HAPPENS 
simple.data = mr.data[, c(match("MR", names(mr.data)), match("repair", names(mr.data)),
                          match("rplace", names(mr.data)), match("service", names(mr.data)),
                          match("fix", names(mr.data)), match("maintain", names(mr.data)),
                          match("install", names(mr.data)), match("pain", names(mr.data)),
                          match("changing", names(mr.data)), match("cleaning", names(mr.data)),
                          match("retrack", names(mr.data)), match("pullbelt", names(mr.data)),
                          match("remove", names(mr.data)), match("reposition", names(mr.data)),
                          match("grease", names(mr.data)), match("rethread", names(mr.data)),
                          match("dismantl", names(mr.data)), match("mineractivity", names(mr.data)),
                          match("hoist", names(mr.data)), match("surgery", names(mr.data)),
                          match("inspect", names(mr.data)), match("check", names(mr.data)),
                          match("occupation", names(mr.data)), match("degreeofinjury", names(mr.data)),
                          match("accidentclassification", names(mr.data)), match("oil", names(mr.data)),
                          match("shovel", names(mr.data)), match("bits", names(mr.data)),
                          match("accidenttype", names(mr.data)))]

simple.data[, "likely.occup"] = ifelse(grepl("maintenance", simple.data[,"occupation"]), 1, 0)
simple.data[, "maybe.occup"] = ifelse(grepl("electrician", simple.data[,"occupation"]) , 1, 0)
simple.data[, "likely.activy"] = ifelse(grepl("maintenance", simple.data[,"mineractivity"]), 1, 0)
simple.data[, "maybe.activy"] = ifelse(match("handling supplies/materials", simple.data[,"mineractivity"]) |
                                         match("hand tools (not powered)", simple.data[,"mineractivity"]) |
                                         match("no value found", simple.data[,"mineractivity"]) |
                                         match("unknown", simple.data[,"mineractivity"]) | match("clean up", simple.data[,"mineractivity"]) | 
                                         match("wet down working place", simple.data[,"mineractivity"]) |
                                         match("inspect equipment", simple.data[,"mineractivity"]), 1, 0)
simple.data[, "likely.class"] = ifelse(match("handtools (nonpowered)", simple.data[,"accidentclassification"]) |
                                         match("machinery", simple.data[,"accidentclassification"]) |
                                         match("electrical", simple.data[,"accidentclassification"]), 1, 0)
simple.data[, "unlikely.class"] = ifelse(match("fall of roof or back", simple.data[,"accidentclassification"]) |
                                         match("struck by falling object", simple.data[,"accidenttype"]), 1, 0)

simple.data$false.keyword = ifelse(simple.data$hoist == 1 | simple.data$surgery == 1, 1, 0)
simple.data$likely.keyword = ifelse((simple.data$repair == 1 | simple.data$fix == 1 | 
                                     simple.data$maintain == 1 | simple.data$rplace == 1 |
                                     simple.data$shovel == 1 | simple.data$washingdown == 1 |
                                     simple.data$check == 1 | simple.data$oil == 1 | 
                                     simple.data$mrworker == 1 | simple.data$cover == 1 |
                                     simple.data$instal == 1 | simple.data$service == 1 |
                                     simple.data$cleaning == 1 | simple.data$retrack == 1 |
                                     simple.data$changing == 1 | simple.data$reposition == 1 |
                                     simple.data$pullbelt == 1 | simple.data$grease == 1 |                                      
                                     simple.data$inspect == 1 ) & simple.data$false.keyword == 0, 1, 0)
simple.data$maybe.keyword = ifelse( (simple.data$remove == 1 | simple.data$pullbelt == 1 | simple.data$bits == 1 | 
                                     simple.data$reposition == 1 | simple.data$grease == 1 |
                                     simple.data$rethread == 1 | simple.data$dismantl == 1 |
                                     simple.data$oil == 1 | simple.data$check == 1 ) & simple.data$false.keyword == 0, 1, 0)

# GENERATE OTHER USEFUL FLAGS ABOUT ACCIDENT 
simple.data$falling.accident = ifelse(simple.data$accidentclassification == "fall of roof or back", 1, 0)
#replace falling_accident = 1 if ustrregexm(narrative, "rock( )*fell") | ustrregexm(narrative, "fell.{1,20}roof") | ustrregexm(narrative, "roof( )*f(a|e)ll") 

simple.data = simple.data[, c(-match("degreeofinjury", names(simple.data)), -match("occupation", names(simple.data)),
                          -match("accidentclassification", names(simple.data)), -match("mineractivity", names(simple.data)),
                          -match("accidenttype", names(simple.data)))]

# office computer directory
setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code")
write.csv(simple.data, file = "prepped_MR_simple_data.csv", row.names = FALSE)

######################################################################################################
# CREATE/PREP VARIOUS TIME AND DATE VARIABLES - YEAR AND QUARTER
date <- strptime(mr.data$calendaryear, "%Y")
format(date, "%Y")
mr.data[, "year"] = format(date, "%Y")
mr.data[, "quarter"] = as.yearqtr(mr.data$accidentdate,"%Y-%m-%d")
mr.data = mr.data[, c(-grep("calendar", names(mr.data)), -grep("accidentdate", names(mr.data)))]

# REMOVE IRRELEVANT VARS: 25 OF 116 VARS 
mr.data = mr.data[, c(-match("directionstominemodified", names(mr.data)), -match("minegascategorycode", names(mr.data)),
                      -match("nooftailingponds", names(mr.data)), -match("noofproducingpits", names(mr.data)),
                      -match("longitude", names(mr.data)), -match("latitude", names(mr.data)),
                      -match("nearesttown", names(mr.data)), -match("minestatus", names(mr.data)),
                      -grep("secondary", names(mr.data)), -match("minetype", names(mr.data)), 
                      -match("roof.bolt", names(mr.data)), -match("rib.hole", names(mr.data)), 
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

######################################################################################################
# PREP FINAL VARS FOR ANALYSIS

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

######################################################################################################
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

######################################################################################################
# DUMMY-OUT FACTOR VARS WITH TOO MANY VALUES
datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}
test.data1 <- datdum(x="sourceofinjury",data=mr.data,name="sourceofinjury")
test.data1 <- test.data1 [, c(grep("sourceofinjury", names(test.data1)))]

test.data2 <- datdum(x="equipmentmodelno",data=mr.data,name="equipmentmodelno")
test.data2 <- test.data2 [, c(grep("equipmentmodelno", names(test.data2)))]

test.data3 <- datdum(x="minename",data=mr.data,name="minename")
test.data3 <- test.data3 [, c(grep("minename", names(test.data3)))]

test.data4 <- datdum(x="operatorname",data=mr.data,name="operatorname")
test.data4 <- test.data4 [, c(grep("operatorname", names(test.data4)))]

test.data5 <- datdum(x="fipscountyname",data=mr.data,name="fipscountyname")
test.data5 <- test.data5 [, c(grep("fipscountyname", names(test.data5)))]

test.data6 <- datdum(x="controllername",data=mr.data,name="controllername")
test.data6 <- test.data6 [, c(grep("controllername", names(test.data6)))]

test.data7 <- datdum(x="mineractivity",data=mr.data,name="mineractivity")
test.data7 <- test.data7 [, c(grep("mineractivity", names(test.data7)))]

test.data8 <- datdum(x="quarter",data=mr.data,name="quarter")
test.data8 <- test.data8 [, c(grep("quarter", names(test.data8)))]

mr.data = cbind(mr.data, test.data1, test.data2, test.data3, test.data4, test.data5, test.data6, test.data7, test.data8)

######################################################################################################
# SAVE DATA FOR CART AND RF ANALYSIS
drops <- c("sourceofinjury", "equipmentmodelno", "fipscountyname", "controllername", "mineractivity", "minename", "operatorname", "quarter")
mr.data = mr.data[, !(names(mr.data) %in% drops)]

#setwd("/Users/Sarah/Dropbox (SLS)/R-code")
#office computer directory
setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code")
write.csv(mr.data, file = "prepped_MR_training_data.csv", row.names = FALSE)

######################################################################################################
