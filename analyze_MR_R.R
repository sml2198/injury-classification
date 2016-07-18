# This code pulls in 1000 observations from the manually coded training set for maintenance and repair (MR)
# injuries. It then appends 23 MR fatalities that were scraped from MSHA: http://arlweb.msha.gov/fatals/coal/2014/
# It then prepares all variables for analyses in CART-RF-MR.R by formatting, imputing missing values, 
# dropping extraneous parameters, and dummying out large factor variables. It then performs a number of modeling
# strategies and prints their accuracy. If the data.type is set to "real accidents data", this file will read in
# and classify all accidents by the algorithm with the highest accuracy.

* JULIAWUZHERE

install.packages("zoo")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tree")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("pROC")
install.packages("ROSE")
install.packages("adabag")
install.packages("DMwR")
install.packages("caret")
library(zoo)
library(tree)
library(randomForest)
library(ggplot2)
library(reshape2)
library(pROC)
library(ROSE)
library(rpart)
library(rpart.plot)
library(adabag)
library(DMwR)
library(caret)

######################################################################################################
rm(list = ls())

# SET PREFERENCES - IMPUTATION METHOD - METHOD 3 IS RANDOM DRAWS FROM DISTRIBUTION (OUR BEST METHOD)
imputation.method = 0

# Different people at NIOSH told us different things about whether or not to include accidents that occur during MR
# but are not related in nature to MR activities, for example falling rock/metal accidents, or an employee walking
# into a piece of equipment. If "excluded", this code will replace MR with 0 for observations I've identified as
# these types of accidents. UPDATE: We're moving forward with the "included" version.
#falling.accidents = "excluded"
falling.accidents = "included"

# SET PREFERENCES - DATA TYPE - EITHER TRAINING DATA FOR MODEL SELECTION, OR REAL ACCIDENTS DATA FOR CLASSIFICATION
#data.type = "training data"
data.type = "real accidents data"

dummies.option = "off"

# LOAD IN CODED TRAINING SET (1000 OBSERVATIONS, CODED FOR "MR")
mr.data = read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)
# LOAD IN DATASET OF ADDITIONAL FATALITIES (FROM OPEN DATA) TO APPEND TO OUR TRAINING SET - ALL "MR"
mr.fatalities = read.csv("X:/Projects/Mining/NIOSH/analysis/data/4_coded/coded_MR_fatalities.csv", header = TRUE, sep = ",", nrows = 24, stringsAsFactors = FALSE)

if (data.type == "real accidents data") {
  # LOAD IN REAL ACCIDENTS DATA FOR CLASSIFICATION
  accidents.data = read.csv("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/prepped_mines_accidents.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
}

######################################################################################################
# MAKE SURE TRAINING SET AND FATALITIES DATASETS HAVE ALL THE SAME VAR NAMES BEFORE APPENDING - IF USING TRAINING DATA

mr.data$MR = as.factor(mr.data$M.R.)
mr.data = mr.data[, c(-match("M.R.", names(mr.data)))]
mr.data[, "death"] = ifelse(grepl("fatality", mr.data[,"degreeofinjury"]), 1, 0)
    
# CLEAN UP FATALITIES VARIABLES - DROP VARIABLES NOT PRESENT IN TRAINING SET BEFORE APPENDING
mr.fatalities$MR = as.factor(mr.fatalities$MR_fatality)
mr.fatalities = mr.fatalities[, c(-grep("MR_fatality", names(mr.fatalities)), -grep("v56", names(mr.fatalities)),
                                      -grep("v57", names(mr.fatalities)), -grep("v58", names(mr.fatalities)), 
                                      -grep("v59", names(mr.fatalities)))]
    
# THESE FOUR FATALGRAMS ARE CONSIDERED M&R AND WERE INCLUDED IN A STUDY BY JOHN H. FROM NIOSH AS M&R. 
# HOWEVER, IT'S REALLY ONLY EVIDENT FROM THE FATALGRAMS (SEE OPEN DATA FOLDER) THAT THESE WERE SUSTAINED
# DURING LARGER GROUP M&R ACTIVITIES. NOTHING FROM THE NARRATIVE FIELD/OCCUPATION INDICATES THAT M&R WAS THE
# ACTIVITY AT THE TIME. ESSENTIALLY, TRAINING ON THESE OBSERVATIONS WILL STACK THE DECK AGAINST US.
# LET'S DELETE THEM FOR NOW.
mr.fatalities = mr.fatalities[!(mr.fatalities$documentno=="220030290001") & !(mr.fatalities$documentno=="220030290002") &
                                  !(mr.fatalities$documentno=="220030290003") & !(mr.fatalities$documentno=="220030130149"),]
    
# CLEAN NARRATIVE FIELDS: DROP REDUNDANT VARS AND KEEP LOWERCASE VERSION
drops <- c("narrativemodified", "degreeofinjury", "accidentclassification", "accidenttype", "natureofinjury", "mineractivity")
mr.data = mr.data[, !(names(mr.data) %in% drops)]
names(mr.data)[names(mr.data) == 'narrativemodified.1'] = 'narrative'
mr.data$narrative = tolower(mr.data$narrative)
names(mr.data)[names(mr.data) == 'degreeofinjury.1'] = 'degreeofinjury'
mr.data$degreeofinjury = tolower(mr.data$degreeofinjury)
names(mr.data)[names(mr.data) == 'accidentclassification.1'] = 'accidentclassification'
mr.data$accidentclassification = tolower(mr.data$accidentclassification)
names(mr.data)[names(mr.data) == 'accidenttype.1'] = 'accidenttype'
mr.data$accidenttype = tolower(mr.data$accidenttype)
names(mr.data)[names(mr.data) == 'natureofinjury.1'] = 'natureofinjury'
mr.data$natureofinjury = tolower(mr.data$natureofinjury)
names(mr.data)[names(mr.data) == 'mineractivity.1'] = 'mineractivity'
mr.data$mineractivity = tolower(mr.data$mineractivity)
mr.data$occupation = tolower(mr.data$occupation)
mr.data$typeofequipment = tolower(mr.data$typeofequipment)
mr.data$sourceofinjury = tolower(mr.data$sourceofinjury)
mr.data$bodypart = tolower(mr.data$bodypart)
mr.data$equipmanufacturer = tolower(mr.data$equipmanufacturer)
mr.data$immediatenotificationclass = tolower(mr.data$immediatenotificationclass)
mr.data$uglocation = tolower(mr.data$uglocation)
    
# APPEND DATASET OF ADDITIONAL FATALITY OBSERVATIONS FOR TRAINING SET
mr.data <- rbind(mr.data, mr.fatalities) 
    
# MAKE MR A FACTOR VARIABLE
mr.data[, "MR"] = factor(ifelse(mr.data[, "MR"] == 1, "YES", "NO"))
names(mr.data)[names(mr.data) == "MR"] = "MR"
    
# RECODE MISCODED INJURIES AS NON-M&R. See email with J. Heberger from NIOSH on May 2, 2016. About the following injury,
# he explains "even though mine worker activity is M&R, installing roof bolts is not considered M&R.  Should be coded  2."
# Here we manually recode this one observation. 
mr.data$MR[mr.data$documentno=="219932950056"] = "NO"

######################################################################################################
# DO THIS CODE IF YOU'RE RUNNING ON THE REAL ACCIDENTS DATA (NOT THE TRAINING SET)
if (data.type == "real accidents data") {
    # FIRST MAKE A FLAG FOR TRAINING OBSERVATIONS
    mr.data[, "type"] = "training"  
    mr.data[, "datasource"] = "training"  
    mr.data[, "investigationbegindate"] = "" 
    accidents.data[, "type"] = "unclassified" 
    accidents.data[, "contractor_accident"] = "" 
    accidents.data[, "MR"] = "" 
    
    # CLEAN NARRATIVE FIELDS: DROP REDUNDANT VARS AND KEEP LOWERCASE VERSION
    accidents.data$narrative = tolower(accidents.data$narrative)
    accidents.data$degreeofinjury = tolower(accidents.data$degreeofinjury)
    accidents.data$accidentclassification = tolower(accidents.data$accidentclassification)
    accidents.data$accidenttype = tolower(accidents.data$accidenttype)
    accidents.data$natureofinjury = tolower(accidents.data$natureofinjury)
    accidents.data$mineractivity = tolower(accidents.data$mineractivity)
    accidents.data$occupation = tolower(accidents.data$occupation)
    accidents.data$typeofequipment = tolower(accidents.data$typeofequipment)
    accidents.data$sourceofinjury = tolower(accidents.data$sourceofinjury)
    accidents.data$bodypart = tolower(accidents.data$bodypart)
    accidents.data$equipmanufacturer = tolower(accidents.data$equipmanufacturer)
    accidents.data$immediatenotificationclass = tolower(accidents.data$immediatenotificationclass)
    accidents.data$uglocation = tolower(accidents.data$uglocation)
  
    drops <- c("death", "i" )
    mr.data = mr.data[, !(names(mr.data) %in% drops)]  
    drops <- c("assesscontrolno", "part48training", "controllerbegindate", "fiscalquarter", "fiscalyear" )
    accidents.data = accidents.data[, !(names(accidents.data) %in% drops)]
    
    # APPEND DATASET OF TRAINING OBSERVATIONS AND REAL ACCIDENTS FOR CLASSIFICATION
    mr.data <- rbind(mr.data, accidents.data)
}

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
mr.data[, "uglocation"] = ifelse(mr.data[, "uglocation"] == "not marked", "no value found", mr.data[, "uglocation"])
mr.data[, "immediatenotificationclass"] = ifelse(mr.data[, "immediatenotificationclass"] == "not marked", "no value found", mr.data[, "immediatenotificationclass"])
mr.data[, "natureofinjury"] = ifelse(mr.data[, "natureofinjury"] == "unclassified,not determed", "no value found", mr.data[, "natureofinjury"])
mr.data[, "equipmanufacturer"] = ifelse(mr.data[, "equipmanufacturer"] == "not reported", "no value found", mr.data[, "equipmanufacturer"])

# We decided to recode three observations in our data that were coded as MR, but are apparently non-injury accidents. 
# It's apparent that whoever did the coding didn't look at this field. We don't ever want our algorithm to classify 
# an accident-only observation as positive for MR, so we enforce this change.  
mr.data$accident.only = ifelse(mr.data$degreeofinjury == "accident only" | mr.data$accidenttype == "acc type, without injuries", 1, 0)
mr.data$MR = ifelse(mr.data$MR == "YES" & mr.data$accident.only == 0, 1, 0)
mr.data$MR[mr.data$MR == "YES" & mr.data$accident.only == 1] = 0

# MAKE SURE MR IS STILL A FACTOR VARIABLE
mr.data[, "MR"] = factor(ifelse(mr.data[, "MR"] == 1, "YES", "NO"))
names(mr.data)[names(mr.data) == "MR"] = "MR"

######################################################################################################
# 60 NARRATIVE FIELDS ARE POLLUTED WITH OTHER COLUMNS - SPLIT AND REPLACE THESE 

mr.data[, "messy"] = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", mr.data[,"narrative"]), 1, 0)
narrative.split = strsplit(mr.data[mr.data$messy == 1, "narrative"], "|", fixed = T)
messy.rows = row.names(mr.data[mr.data$messy == 1, ])
for (i in 1:length(messy.rows)) {
  mr.data[messy.rows[i], "narrative"] = unlist(narrative.split[i])[1]
  mr.data[messy.rows[i], "occupcode3digit"] = unlist(narrative.split[i])[2]
  mr.data[messy.rows[i], "occupation"] = unlist(narrative.split[i])[3]
  mr.data[messy.rows[i], "returntoworkdate"] = unlist(narrative.split[i])[4]
}
mr.data = mr.data[, c(-match("messy", names(mr.data)))]

# DEAL WITH MESSY NUMBER TYPOS - RANDOM NUMBERS THAT HAVE BEEN DROPPED INTO NARRATIVES 
mr.data[, "numbertypo"] = ifelse(grepl("[a-z][0-9][a-z]", mr.data[,"narrative"]), 1, 0)
for (i in 0:9) {
  mr.data[mr.data$numbertypo == 1,]$narrative <- gsub(i, "", mr.data[mr.data$numbertypo == 1,]$narrative)
}

# CONVERT DATES - THIS NEEDS TO HAPPEN AFTER REPLACING RETURNTOWORKDATE WITH EXTRACTS FROM NARRATIVE FIELDS
indices_with_date = grep("date", names(mr.data))
for (i in indices_with_date) {
  mr.data[,i] = as.Date(mr.data[,i], "%m/%d/%Y")
}

######################################################################################################
# ADD NEW KEYWORD VARS

# GENERATE LIKELY POSITIVE KEYWORDS (LIKELY TO INDICATE POSITIVE OUTCOMES) 

# *REPAIR* 
mr.data[, "repair"] = ifelse(grepl("(^| )r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]) &
                             !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,20}hernia", mr.data[,"narrative"]) &
                             !grepl("hernia.{1,10}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]) &
                             !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,10}wound", mr.data[,"narrative"]) &
                             !grepl("wound.{1,20}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]), 1, 0)
mr.data[, "rplace"] = ifelse(grepl("(^| )replac(e|i)[a-z]*", mr.data[,"narrative"]), 1, 0)
# We don't want to see the noun "service" because that often refers to hoist service, but "serviced" and "servicing" are good indicators.
mr.data[, "service"] = ifelse(grepl("serviced", mr.data[,"narrative"]) | grepl("servicing", mr.data[,"narrative"]), 1, 0)
mr.data[, "fix"] = ifelse(grepl("(^| )fix[a-z]*", mr.data[,"narrative"]) & !grepl("(^| )fixture", mr.data[,"narrative"]), 1, 0) 
mr.data[, "changing"] = ifelse(grepl("chang(e|ing|ed)( |-)*out", mr.data[,"narrative"]) |
                               (grepl("chang(e|ing|ed)", mr.data[,"narrative"]) & !grepl("chang(e|ing|ed).{1,10}(shift|place|positi)", mr.data[,"narrative"])), 1, 0)
mr.data[, "retrack"] = ifelse(grepl("re(rail|track|trakc)(ed|ing)", mr.data[,"narrative"]) |
                              grepl("pull(ing|ed)*.{1,5}track", mr.data[,"narrative"]), 1, 0)
mr.data[, "pullbelt"] = ifelse(grepl("pull( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mr.data[,"narrative"]) |
                               grepl("(belt|rope|spool|tube|tubing).{1,15}pull( |ing|ed|s)*", mr.data[,"narrative"]) |
                               grepl("(belt|rope|spool|tube|tubing).{1,15}load( |ing|ed|s)*", mr.data[,"narrative"]) |
                               grepl("load( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mr.data[,"narrative"]), 1, 0)
mr.data[, "reposition"] = ifelse(grepl("re( |-)*pos(i|t)(i|t)(i|o)(i|o)n", mr.data[,"narrative"]), 1, 0) 
# Sometimes even when the occupation field isn't MR, the narrative field refers to the job of the injured, or to the injured ee helping an MR worker. 
mr.data[, "mrworker"] = ifelse(grepl("(mechanic|electrician|repairm(a|e)n)", mr.data[,"narrative"]), 1, 0) 
mr.data[, "cover"] = ifelse((grepl("(replac|l(i|e)ft).{1,20}(panel|cover| lid|hood)", mr.data[,"narrative"]) |
                            grepl("(panel|cover| lid|hood){1,5}fell", mr.data[,"narrative"]) | 
                            grepl("drop.{1,10}(panel|cover| lid|hood)", mr.data[,"narrative"])) &
                            !grepl("eye.{1,5}lid", mr.data[,"narrative"]), 1, 0) 
mr.data[, "toolbox"] = ifelse(grepl("( |^)tool", mr.data[,"narrative"]), 1, 0)
                            
# *MAINTENANCE* 

# "Cleaning the rib" refers to using a scoop to grab extra coal, but this is in fact a production activity.
mr.data[, "cleaning"] = ifelse(grepl("cl(ean|(e)*aning)", mr.data[,"narrative"]) & !grepl("clean.{1,10} rib", mr.data[,"narrative"]), 1, 0) 
mr.data[, "maintain"] = ifelse(grepl("(^| )maint(ain|en|ean)[a-z]*", mr.data[,"narrative"]) | grepl("maint.{1,9}work", mr.data[,"narrative"]), 1, 0)
# Try to avoid inspection/inspector
mr.data[, "inspect"] = ifelse(grepl("inspect( |ed|s|ing|\\.|,|$)", mr.data[,"narrative"]), 1, 0)
mr.data[, "shovel"] = ifelse(grepl("shovel(ing|ed).{1,5}coal)", mr.data[,"narrative"]) |
                             grepl("coal.{1,15}shovel(ing|ed)", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}belt", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}convey(e|o)r", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}tail( |-)*p(e|i)(e|i)ce", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}(head|drive|guide|bend|lagged|tail)*( |-)*pull(y|ey|ies|ys)", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}(roller|idler)", mr.data[,"narrative"]) |
                             grepl("shovel(ing|ed).{1,20}(west|header|drive)", mr.data[,"narrative"]), 1, 0)
# We don't want the noun "hose" (or "whose") just the verb - also dont want "bullhose" or any of that nonsense
mr.data[, "washingdown"] = ifelse(grepl("( |^|\\.|,)(wash|hose)(d|ed|ing| )", mr.data[,"narrative"]), 1, 0) 
mr.data[, "grease"] = ifelse(grepl("greas(ed|ing|e|er)", mr.data[,"narrative"]), 1, 0) 
# Try to avoid "doctor checked out injury..."
mr.data[, "check"] = ifelse(grepl("che(c|k)(c|k)", mr.data[,"narrative"]) &
                           !grepl("doctor", mr.data[,"narrative"]) &
                           !grepl("hospital", mr.data[,"narrative"]) &
                           !grepl("emergency", mr.data[,"narrative"]) &
                           !grepl("clinic", mr.data[,"narrative"]), 1, 0) 
# "Tests" usually means doctors test, and we also want to avoid "testicles."
mr.data[, "tests"] = ifelse(grepl("test(ing|ed)", mr.data[,"narrative"]) &
                          !grepl("doctor", mr.data[,"narrative"]) &
                          !grepl("hospital", mr.data[,"narrative"]) &
                          !grepl("emergency", mr.data[,"narrative"]) &
                          !grepl("clinic", mr.data[,"narrative"]), 1, 0) 
# Oil in mention of can/drum/barrel often means something is being greased. Otherwise it usually apears in some other context (being slipped on, lit, etc...)
mr.data[, "oil"] = ifelse(grepl("(^| )(oil).{1,25}(can|drum|barrel|container)", mr.data[,"narrative"]) |
                          grepl("(can|drum|barrel|container).{1,25}oil", mr.data[,"narrative"]) | 
                          grepl("(chang|add)(e|ing).{1,6}(oil|fuel|equipment)", mr.data[,"narrative"]) |
                          grepl("(( |^)oil|fuel)ing", mr.data[,"narrative"]) | grepl("to (((change|add) (oil|fuel))|fuel)", mr.data[,"narrative"]), 1, 0) 

# GENERATE POTENTIALLY POSITIVE KEYWORDS (MAYBE INDICATE POSITIVE OUTCOMES?) 

# *REPAIR* 

mr.data[, "dismantl"] = ifelse(grepl("dismant(el|le|al|il|l)", mr.data[,"narrative"]), 1, 0) 
mr.data[, "rethread"] = ifelse(grepl("re( |-)*thr(ea|e)d", mr.data[,"narrative"]), 1, 0)
mr.data[, "remove"] = ifelse(grepl("re(m)*ov(e|ed|ing|al)", mr.data[,"narrative"]) | grepl("rem(o)*v(e|ed|ing|al)", mr.data[,"narrative"]), 1, 0) 

# *MAINTENANCE* 

mr.data[, "bits"] = ifelse(grepl("set(t)*(ing)*( |-)*bits", mr.data[,"narrative"]), 1, 0)
mr.data[, "conveyor"] = ifelse(grepl("convey(o|e)r", mr.data[,"narrative"]), 1, 0)
# We don't want "help" 
mr.data[, "helping"] = ifelse(grepl("help(ed|in(g)*|er)", mr.data[,"narrative"]) |
                               grepl("assis(s)*t(ed|in(g))*", mr.data[,"narrative"]), 1, 0)
mr.data[, "belt"] = ifelse(grepl("belt|spool|tube|tubing", mr.data[,"narrative"]), 1, 0)
mr.data[, "tighten"] = ifelse(grepl("tighten", mr.data[,"narrative"]), 1, 0)
mr.data[, "loosen"] = ifelse(grepl("loos(en|ing)", mr.data[,"narrative"]), 1, 0)
# Most cases of changing batteries are M&R, but sometimes someone might just trip on a charger, or
# be operating a "battery personnel carrier", and we want to avoid these.
mr.data[, "battery"] = ifelse(grepl("bat(t)*(e)*r(y|ies)", mr.data[,"narrative"]) &
                             !grepl("bat(t)*(e)*r(y|ies).{1,6}charg(er|ing)", mr.data[,"narrative"]) &
                             !grepl("bat(t)*(e)*r(y|ies).{1,8}person(n)*(el|le)", mr.data[,"narrative"]) &
                             !grepl("bat(t)*(e)*r(y|ies).{1,5}car(r)*i(e|o)r", mr.data[,"narrative"]), 1, 0)

# DEAL WITH THE WORD "INSTALLATION". INSTALLS ARE NOT M&R IF RELEVANT TO CONSTRUCTION/PRODUCTION, LIKE WITH ROOF BOLTING

mr.data[, "roof.bolt"] = ifelse(grepl("(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) | 
                                grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,)", mr.data[,"narrative"]), 1, 0)  
mr.data[, "rib.hole"] = ifelse(grepl("(rib)( |-)*(hole).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) | 
                               grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(rib)( |-)*(hole)", mr.data[,"narrative"]), 1, 0)  
# Accounts for install, reinstall, uninstall (but not "an installed cable" or something like that)
mr.data[, "install"] = ifelse(grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) &
                             !grepl("(^| )an( |e|n)i(s|n|t)(s|n|t)(s|n|t)alled", mr.data[,"narrative"]) 
                              & (mr.data[, "rib.hole"] != 1 & mr.data[, "roof.bolt"] != 1), 1, 0)

# NEGATIVE KEYWORDS (LIKELY TO INDICATE NEGATIVE OUTCOMES) 

mr.data[, "pain"] = ifelse(grepl("(^| )(pain|hurt)(s)*( |$|\\.|,|:)", mr.data[,"narrative"]), 1, 0)
mr.data[, "injured"] = ifelse(grepl("injur", mr.data[,"narrative"]), 1, 0)
# Make sure "hoisting" or "hoisted" aren't grabbed
mr.data[, "hoist"] = ifelse(((grepl("(^| )hoist(s| |$|\\.|,|:)", mr.data[,"narrative"]) |
                              grepl("(^| )el(e|a|i)vat(o|e)r", mr.data[,"narrative"]))
                              & !grepl("(elevat(o|e)r|hoist).{1,10}(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame)", mr.data[,"narrative"])
                              & !grepl("(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame).{1,10}(elevat(o|e)r|hoist)", mr.data[,"narrative"])
                              & !grepl("(^| )us(e|ing).{1,10}(elevat(o|e)r|hoist)", mr.data[,"narrative"])
                              & mr.data[, "pain"] == 0 & mr.data[, "injured"] == 0), 1, 0) 
mr.data[, "surgery"] = ifelse((grepl("surger[a-z]*", mr.data[,"narrative"]) | 
                              grepl("surgic[a-z]*", mr.data[,"narrative"])) & mr.data[, "pain"] == 0 & mr.data[, "injured"] == 0, 1, 0)

######################################################################################################
# GENERATE ADDITIONAL KEYWORDS WE HAVE NO PRIORS ABOUT TO FEED INTO RANDOM FOREST 

mr.data[, "power"] = ifelse(grepl("pow(e)*r", mr.data[,"narrative"]), 1, 0)
#These don't add much
mr.data[, "splice"] = ifelse(grepl("splice", mr.data[,"narrative"]) & (mr.data$occupcode3digit %in% c("004", "418")), 1, 0)
mr.data[, "lug"] = ifelse(grepl("( |^)lug(g)*", mr.data[,"narrative"]) & (mr.data$occupcode3digit %in% c("004", "418")), 1, 0)
# We only want the noun, not the verb.
mr.data[, "wrench"] = ifelse(grepl("wrench", mr.data[,"narrative"]), 1, 0)
mr.data[, "trash"] = ifelse(grepl("(trash|garbage|dumpster)", mr.data[,"narrative"]), 1, 0)
mr.data[, "roller"] = ifelse(grepl("roller", mr.data[,"narrative"]), 1, 0)
mr.data[, "moretools"] = ifelse(grepl("(pry|crow|jack)( |-)*bar", mr.data[,"narrative"]) | 
                                grepl("(hammer|screw( |-)*driver|shovel( |\\.|$|,|:)|ratchet)", mr.data[,"narrative"]) | 
                                grepl("com(e)*(-)*(a)*(-)*long", mr.data[,"narrative"])
                                , 1, 0)
mr.data[, "welding"] = ifelse((grepl("(( |^)tank|ac(c)*etyle(ne|en)|weld)", mr.data[,"narrative"]) | grepl("(oxygen|o2)( )*(bottle|cylinder)", mr.data[,"narrative"])) &
                              !grepl("chemic.{1,10}tank)", mr.data[,"narrative"]), 1, 0)
mr.data[, "tire"] = ifelse(grepl("(chang|pump)(e|ed|ing).{1,5}tire", mr.data[,"narrative"]), 1, 0)

# GENERATE OTHER USEFUL FLAGS ABOUT ACCIDENT 
mr.data$falling.class = ifelse(mr.data$accidentclassification == "fall of roof or back", 1, 0)
mr.data[, "falling.word"] = ifelse(grepl("rock( )*fell", mr.data[,"narrative"]) |
                                         grepl("fell.{1,20}roof", mr.data[,"narrative"]) |
                                         grepl("roof( )*f(a|e)ll", mr.data[,"narrative"]), 1, 0)
mr.data$falling.accident = ifelse(mr.data$falling.class == 1 | mr.data$falling.word == 1, 1, 0)
mr.data = mr.data[, c(-match("falling.class", names(mr.data)), -match("falling.word", names(mr.data)))]

mr.data$accident.only = ifelse( (mr.data$degreeofinjury == "accident only" | mr.data$accidenttype == "acc type, without injuries"), 1, 0)

######################################################################################################
# DEAL WITH FALLING ACCIDENTS 

# If excluded, replace MR equal to 0 for falling accidents
if (falling.accidents == "excluded") {
  mr.data$MR[mr.data$documentno == "220011760163" ] = "NO"
  mr.data$MR[mr.data$documentno == "220120940039" ] = "NO"
  mr.data$MR[mr.data$documentno == "219982750027" ] = "NO"
  mr.data$MR[mr.data$documentno == "219910290016" ] = "NO"
  mr.data$MR[mr.data$documentno == "219840090187" ] = "NO"
  mr.data$MR[mr.data$documentno == "219951000128" ] = "NO"
  mr.data$MR[mr.data$documentno == "219902760023" ] = "NO"
  mr.data$MR[mr.data$documentno == "219950190042" ] = "NO"
  mr.data$MR[mr.data$documentno == "219900990332" ] = "NO"
  mr.data$MR[mr.data$documentno == "219990350006" ] = "NO"
  mr.data$MR[mr.data$documentno == "219873020102" ] = "NO"
  mr.data$MR[mr.data$documentno == "219993010007" ] = "NO"
  mr.data$MR[mr.data$documentno == "219892770045" ] = "NO"
  mr.data$MR[mr.data$documentno == "220002840084" ] = "NO"
  mr.data$MR[mr.data$documentno == "219861670105" ] = "NO"
  mr.data$MR[mr.data$documentno == "220002840160" ] = "NO"
  mr.data$MR[mr.data$documentno == "219900440203" ] = "NO"
  mr.data$MR[mr.data$documentno == "220110140025" ] = "NO"
  mr.data$MR[mr.data$documentno == "220020350113" ] = "NO"
  mr.data$MR[mr.data$documentno == "220072410056" ] = "NO"
  mr.data$MR[mr.data$documentno == "220062280014" ] = "NO"
  mr.data$MR[mr.data$documentno == "220033090003" ] = "NO"  
} 

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
mr.data = mr.data[, c(-match("accidenttime", names(mr.data)), -grep("date", names(mr.data)))]

# SHOULD NOW HAVE 89 VARS, NOW REMOVE REDUNDANT VARS (CODES/IDS WITH CORRESPONDING CLASSES - 30 VARS)
mr.data = mr.data[, c(-match("operatorid", names(mr.data)), -match("controllerid", names(mr.data)), 
                      -match("primarycanvasscode", names(mr.data)), -match("portablefipsstatecode", names(mr.data)),
                      -match("subunitcode", names(mr.data)), -match("oldoccupationcode", names(mr.data)),
                      -match("fipsstatecode", names(mr.data)), -match("activitycode", names(mr.data)), 
                      -match("injurysourcecode", names(mr.data)), -match("natureofinjurycode", names(mr.data)),
                      -match("bodypartcode", names(mr.data)), -match("degreeofinjurycode", names(mr.data)),
                      -match("uglocationcode", names(mr.data)), -match("ugminingmethodcode", names(mr.data)),
                      -match("classificationcode", names(mr.data)), -match("accidenttypecode", names(mr.data)),
                      -match("equiptypecode", names(mr.data)), -match("equipmanufacturercode", names(mr.data)),
                      -match("immediatenotificationcode", names(mr.data)), -match("occupcode3digit", names(mr.data)),
                      -match("fipscountycode", names(mr.data)), -match("officecode", names(mr.data)), 
                      -match("bomstatecode", names(mr.data)), -match("primarysiccode", names(mr.data)), 
                      -match("primarysiccodegroup", names(mr.data)))]

# SHOULD NOW HAVE 59 VARS, NOW REMOVE 1 OLD/UNKNOWN VAR 
mr.data = mr.data[, c(-grep("idesc", names(mr.data)))]

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
  mr.data[, charac_vars[i]] = ifelse((mr.data[,charac_vars[i]] == "no value found" | mr.data[,charac_vars[i]] == "unknown" | 
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

if (imputation.method == 1 | imputation.method == 2) {
  for (i in 1:length(num_vars)) {
    mr.data[, num_vars[i]] = ifelse(is.na(mr.data[, num_vars[i]]), mean(mr.data[, num_vars[i]]), mr.data[, num_vars[i]])
  }
  if (imputation.method == 2) {
    for (i in 1:length(num_vars)) {
      mr.data[, num_vars[i]] = ifelse(is.na(mr.data[, num_vars[i]]), median(mr.data[, num_vars[i]]), mr.data[, num_vars[i]])
    }
  }
  for (i in 1:length(charac_vars)) {
    mr.data[, charac_vars[i]] = ifelse(is.na(mr.data[, charac_vars[i]]), modus(mr.data[, charac_vars[i]]), mr.data[, charac_vars[i]])
  }
} else if (imputation.method == 3) {
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
# DUMMY-OUT FACTOR VARS WITH TOO MANY VALUES - NOT REQUIRED ANYMORE
if (dummies.option == "on") {
      datdum <- function(x, data, name){
        data$rv <- rnorm(dim(data)[1],1,1)
        mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
        names(mm) <- paste(name,1:dim(mm)[2],sep=".")
        data$rv <- NULL
        data <- cbind(data,mm)
        return(data)
      }
      test.data1 <- datdum(x="sourceofinjury",data=mr.data,name="sourceofinjury")
      test.data2 <- datdum(x="equipmentmodelno",data=mr.data,name="equipmentmodelno")
      test.data3 <- datdum(x="minename",data=mr.data,name="minename")
      test.data4 <- datdum(x="operatorname",data=mr.data,name="operatorname")
      test.data5 <- datdum(x="fipscountyname",data=mr.data,name="fipscountyname")
      test.data6 <- datdum(x="controllername",data=mr.data,name="controllername")
      test.data7 <- datdum(x="mineractivity",data=mr.data,name="mineractivity")
      test.data8 <- datdum(x="quarter",data=mr.data,name="quarter")
      test.data9 <- datdum(x="occupation",data=mr.data,name="occupation")
      
      # REMOVE CATEGORICAL VARS (JUST KEEPING DUMMIES) AND BIND ALL TOGETHER
      test.data1 <- test.data1 [, c(grep("sourceofinjury", names(test.data1)))]
      test.data2 <- test.data2 [, c(grep("equipmentmodelno", names(test.data2)))]
      test.data3 <- test.data3 [, c(grep("minename", names(test.data3)))]
      test.data4 <- test.data4 [, c(grep("operatorname", names(test.data4)))]
      test.data5 <- test.data5 [, c(grep("fipscountyname", names(test.data5)))]
      test.data6 <- test.data6 [, c(grep("controllername", names(test.data6)))]
      test.data7 <- test.data7 [, c(grep("mineractivity", names(test.data7)))]
      test.data8 <- test.data8 [, c(grep("quarter", names(test.data8)))]
      test.data9 <- test.data9 [, c(grep("occupation", names(test.data9)))]
      
      mr.data = cbind(mr.data, test.data1, test.data2, test.data3, test.data4, test.data5, test.data6, test.data7, test.data8, test.data9)
      remove(test.data1,test.data2,test.data3,test.data4,test.data5,test.data6,test.data7,test.data8,test.data9)
}

######################################################################################################
# CREATE SIMPLE DATA CONTAINING JUST THE VARS USED FOR SIMPLE ALGORITHM 
simple.data = mr.data[, c(match("MR", names(mr.data)), match("repair", names(mr.data)),
                          match("rplace", names(mr.data)), match("service", names(mr.data)),
                          match("fix", names(mr.data)),  match("changing", names(mr.data)),
                          match("retrack", names(mr.data)), match("pullbelt", names(mr.data)),
                          match("reposition", names(mr.data)), match("mrworker", names(mr.data)),
                          match("cover", names(mr.data)), match("toolbox", names(mr.data)),
                          match("cleaning", names(mr.data)), match("power", names(mr.data)),
                          match("maintain", names(mr.data)), match("inspect", names(mr.data)),
                          match("shovel", names(mr.data)), match("washingdown", names(mr.data)),
                          match("grease", names(mr.data)), match("check", names(mr.data)),
                          match("tests", names(mr.data)), match("splice", names(mr.data)), match("lug", names(mr.data)),
                          match("moretools", names(mr.data)), match("welding", names(mr.data)), match("tire", names(mr.data)),
                          match("oil", names(mr.data)), match("dismantl", names(mr.data)),
                          match("rethread", names(mr.data)), match("remove", names(mr.data)),
                          match("bits", names(mr.data)), match("conveyor", names(mr.data)),
                          match("helping", names(mr.data)), match("belt", names(mr.data)),
                          match("tighten", names(mr.data)), match("battery", names(mr.data)),
                          match("install", names(mr.data)), match("wrench", names(mr.data)), 
                          match("hoist", names(mr.data)), match("surgery", names(mr.data)),                          
                          match("pain", names(mr.data)), match("trash", names(mr.data)), match("mineid", names(mr.data)), 
                          match("roller", names(mr.data)), match("loosen", names(mr.data)), match("type", names(mr.data)), 
                          match("mineractivity", names(mr.data)), match("sourceofinjury", names(mr.data)), 
                          match("occupation", names(mr.data)), match("degreeofinjury", names(mr.data)),
                          match("accidentclassification", names(mr.data)), match("accidenttype", names(mr.data)),
                          match("falling.accident", names(mr.data)), match("accident.only", names(mr.data)),
                          match("narrative", names(mr.data)), match("documentno", names(mr.data)))]

if (falling.accidents == "excluded") {
simple.data[, "likely.occup"] = ifelse(grepl("maintenance", simple.data[,"occupation"]) & simple.data$accident.only == 0  & simple.data$accident.only == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data[, "likely.occup"] = ifelse(grepl("maintenance", simple.data[,"occupation"]) & simple.data$accident.only == 0, 1, 0)
}

if (falling.accidents == "excluded") {
simple.data[, "maybe.occup"] = ifelse(grepl("electrician", simple.data[,"occupation"]) & simple.data$falling.accident == 0 & simple.data$accident.only == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data[, "maybe.occup"] = ifelse(grepl("electrician", simple.data[,"occupation"]) & simple.data$accident.only == 0, 1, 0)
}

if (falling.accidents == "excluded") {
simple.data[, "likely.activy"] = ifelse(grepl("maintenance", simple.data[,"mineractivity"]) | grepl("wet down working place", simple.data[,"mineractivity"])
                                        & simple.data$falling.accident == 0 & simple.data$accident.only == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data[, "likely.activy"] = ifelse(grepl("maintenance", simple.data[,"mineractivity"]) | grepl("wet down working place", simple.data[,"mineractivity"])
                                        & simple.data$accident.only == 0, 1, 0)
}

if (falling.accidents == "excluded") {
simple.data[, "maybe.activy"] = ifelse(match("handling supplies/materials", simple.data[,"mineractivity"]) |
                                         match("hand tools (not powered)", simple.data[,"mineractivity"]) |
                                         match("no value found", simple.data[,"mineractivity"]) |
                                         match("unknown", simple.data[,"mineractivity"]) | 
                                         match("clean up", simple.data[,"mineractivity"]) | 
                                         match("inspect equipment", simple.data[,"mineractivity"])
                                         & simple.data$falling.accident == 0 & simple.data$accident.only == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data[, "maybe.activy"] = ifelse(match("handling supplies/materials", simple.data[,"mineractivity"]) |
                                           match("hand tools (not powered)", simple.data[,"mineractivity"]) |
                                           match("no value found", simple.data[,"mineractivity"]) |
                                           match("unknown", simple.data[,"mineractivity"]) | 
                                           match("clean up", simple.data[,"mineractivity"]) | 
                                           match("inspect equipment", simple.data[,"mineractivity"])
                                          & simple.data$accident.only == 0, 1, 0)
}

if (falling.accidents == "excluded") {
simple.data[, "likely.class"] = ifelse(match("handtools (nonpowered)", simple.data[,"accidentclassification"]) |
                                         match("machinery", simple.data[,"accidentclassification"]) |
                                         match("electrical", simple.data[,"accidentclassification"])
                                         & simple.data$falling.accident == 0 & simple.data$accident.only == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data[, "likely.class"] = ifelse(match("handtools (nonpowered)", simple.data[,"accidentclassification"]) |
                                           match("machinery", simple.data[,"accidentclassification"]) |
                                           match("electrical", simple.data[,"accidentclassification"])
                                           & simple.data$accident.only == 0, 1, 0)
}

if (falling.accidents == "excluded") {
  simple.data[, "unlikely.class"] = ifelse(match("fall of roof or back", simple.data[,"accidentclassification"]), 1, 0)
}

if (falling.accidents == "excluded") {
simple.data[, "likely.source"] = ifelse((simple.data$sourceofinjury == "wrench" | simple.data$sourceofinjury == "knife" |
                                         simple.data$sourceofinjury == "power saw" | simple.data$sourceofinjury == "hand tools,nonpowered,nec" |
                                         simple.data$sourceofinjury == "crowbar,pry bar" | simple.data$sourceofinjury == "axe,hammer,sledge")
                                         & simple.data$falling.accident == 0 & simple.data$accident.only == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data[, "likely.source"] = ifelse((simple.data$sourceofinjury == "wrench" | simple.data$sourceofinjury == "knife" |
                                           simple.data$sourceofinjury == "power saw" | simple.data$sourceofinjury == "hand tools,nonpowered,nec" |
                                           simple.data$sourceofinjury == "crowbar,pry bar" | simple.data$sourceofinjury == "axe,hammer,sledge")
                                           & simple.data$accident.only == 0, 1, 0)
}

# ALL "SURGERIES" ARE FALSE KEYWORDS, BUT ONLY "HOIST/ELEVATOR" IN COMBO WITH WORDS THAT REFER TO ELEVATOR SERVICE ARE FALSE KEYWORDS
simple.data$false.keyword = ifelse((simple.data$repair & simple.data$surgery == 1 ) |
                                  (simple.data$fix & simple.data$surgery == 1 ) |
                                  (simple.data$rplace & simple.data$surgery == 1 ) |
                                  (simple.data$repair & simple.data$hoist == 1 ) |
                                  (simple.data$maintain & simple.data$hoist == 1 ) |
                                  (simple.data$service & simple.data$hoist == 1 ) |
                                  (simple.data$fix & simple.data$hoist == 1 ), 1, 0)

if (falling.accidents == "excluded") {
simple.data$likely.keyword = ifelse((simple.data$repair == 1 | simple.data$fix == 1 | 
                                       simple.data$maintain == 1 | simple.data$rplace == 1 |
                                       simple.data$install == 1 | simple.data$service == 1 |
                                       simple.data$cleaning == 1 | simple.data$changing == 1 |
                                       simple.data$retrack == 1 | simple.data$inspect == 1 |
                                       simple.data$shovel == 1 | simple.data$reposition == 1 | 
                                       simple.data$pullbelt == 1 | simple.data$grease == 1 |
                                       simple.data$washingdown == 1 | simple.data$check == 1 |
                                       simple.data$oil == 1 | simple.data$mrworker == 1 |                                      
                                       simple.data$cover == 1 | simple.data$tests == 1 |
                                       simple.data$toolbox == 1 ) &
                                       simple.data$falling.accident == 0 & simple.data$accident.only == 0 &
                                       simple.data$false.keyword == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data$likely.keyword = ifelse((simple.data$repair == 1 | simple.data$fix == 1 | 
                                         simple.data$maintain == 1 | simple.data$rplace == 1 |
                                         simple.data$install == 1 | simple.data$service == 1 |
                                         simple.data$cleaning == 1 | simple.data$changing == 1 |
                                         simple.data$retrack == 1 | simple.data$inspect == 1 |
                                         simple.data$shovel == 1 | simple.data$reposition == 1 | 
                                         simple.data$pullbelt == 1 | simple.data$grease == 1 |
                                         simple.data$washingdown == 1 | simple.data$check == 1 |
                                         simple.data$oil == 1 | simple.data$mrworker == 1 |                                      
                                         simple.data$cover == 1 | simple.data$tests == 1 |
                                         simple.data$toolbox == 1 ) & simple.data$accident.only == 0 &
                                         simple.data$false.keyword == 0, 1, 0)
}
if (falling.accidents == "excluded") {
simple.data$maybe.keyword = ifelse( (simple.data$remove == 1 | simple.data$dismantl == 1 | 
                                       simple.data$rethread == 1 | simple.data$welding == 1 | 
                                       simple.data$bits == 1 | simple.data$helping == 1 |
                                       simple.data$conveyor == 1 | simple.data$belt == 1 |
                                       simple.data$tighten == 1 | simple.data$battery == 1 ) &
                                       simple.data$falling.accident == 0 & simple.data$accident.only == 0 &
                                       simple.data$false.keyword == 0, 1, 0)
}
if (falling.accidents == "included") {
  simple.data$maybe.keyword = ifelse( (simple.data$remove == 1 | simple.data$dismantl == 1 | 
                                         simple.data$rethread == 1 | simple.data$welding == 1 | 
                                         simple.data$bits == 1 | simple.data$helping == 1 |
                                         simple.data$conveyor == 1 | simple.data$belt == 1 |
                                         simple.data$tighten == 1 | simple.data$battery == 1 ) & simple.data$accident.only == 0 &
                                         simple.data$false.keyword == 0, 1, 0)
}

# Remove all categorical variables (and their dummies) - keep narratives and documentno for model training
simple.data = simple.data[, c(-grep("degreeofinjury", names(simple.data)), -grep("occupation", names(simple.data)),
                              -grep("accidentclassification", names(simple.data)), -grep("mineractivity", names(simple.data)),
                              -grep("accidenttype", names(simple.data)), -grep("sourceofinjury", names(simple.data)))]

######################################################################################################
# DROP RAW CATEGORICAL VARS (KEEP DUMMIES) FROM MR DATA
drops <- c("sourceofinjury", "equipmentmodelno", "fipscountyname", "controllername", "mineractivity", "minename", "operatorname", "quarter", "occupation")
mr.data = mr.data[, !(names(mr.data) %in% drops)]

######################################################################################################
# BEGIN ALGORITHM - RANDOMLY SORT DATA (IT WAS ORDERED IN STATA BEFORE THIS)
set.seed(626)
rand <- runif(nrow(mr.data))
train <- mr.data[order(rand),]
rand2 <- runif(nrow(simple.data))
simple <- simple.data[order(rand2),]
remove(rand,rand2)
# just to find out which col # MR is
which( colnames(train)=="MR" )
which( colnames(simple)=="MR" )

if (data.type == "training" ) {
      ######################################################################################################
      # CREATE CART FUNCTION WITH RPART AND EXECUTE ON 1ST 600 OBSERVATIONS
      cart <- rpart(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], method="class")
      cart 
      
      # PLOT RESULTS & DETAILED PLOT OF SPLITS
      rpart.plot(cart, type=3, extra = 101, fallen.leaves=T)
      printcp(cart) 
      
      ######################################################################################################
      # DEFINE RANDOM FOREST (ON TRUE PROPORTION OF NO'S AND YES'S)
      rf <- randomForest(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], mtry = 15, importance=TRUE, type="class",
                         ntree = 1000)
      rf
      
      # PREDICT ON OUT-OF-BAG (OOB) OBSERVATIONS 
      rf.oob.predictions = predict(rf, simple[1:700,!(names(simple) %in% c('documentno','narrative'))],type="class")
      table(simple[1:700,1], predicted = rf.oob.predictions)
      
      ######################################################################################################
      # DOWNSAMPLE NEGATIVE OUTCOMES (MR=NO) FOR RANDOM FOREST
      nmin = sum(simple$MR == "YES")
      nmin
      
      ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
      
      rf.downsampled = train(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], method = "rf", ntree = 800,
                             tuneLength = 10, metric = "ROC", trControl = ctrl, 
                             strata = simple$MR, sampsize = rep(nmin, 2))
      
      rf.baseline = train(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], method = "rf", ntree = 800,
                          tuneLength = 10, metric = "ROC", trControl = ctrl)
      
      down.prob = predict(rf.downsampled, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))], type = "prob")[,1]
      down.ROC = roc(response = simple[701:1019,1], predictor = down.prob, levels = rev(levels(simple[701:1019,1])))
      
      base.prob = predict(rf.baseline, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))], type = "prob")[,1]
      base.ROC = roc(response = simple[701:1019,1], predictor = base.prob, levels = rev(levels(simple[701:1019,1])))
      
      plot(down.ROC, col = rgb(1, 0, 0, .5), lwd = 2)
      plot(base.ROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)
      legend(.4, .4, c("Down-Sampled", "Normal"), lwd = rep(2, 1), col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))
      # sensitivity = true-positive rate
      # specificity = false-positive rate
      
      ######################################################################################################
      # OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE BALANCED DATA W ROSE
      simple.rosex <- ROSE(MR ~ ., data=simple[1:700,!(names(simple) %in% c('documentno','narrative'))])$data
      
      # CHECK IMBALANCE AND SORT RANDOMLY (FOR SHITZNGIGGLES)
      table(simple.rosex$MR)
      rand3 <- runif(nrow(simple.rosex))
      simple.rose <- simple.rosex[order(rand3),]
      remove(simple.rosex)
      
      # DEFINE RF ON ROSE OVERSAMPLED DATA
      rf.rose <- randomForest(MR ~ ., data = simple.rose, mtry = 15, ntree = 1000)
      rf.rose
      
      ######################################################################################################
      # OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE BALANCED DATA W SMOTE
      
      prop.table(table(simple$MR))
      #0.6470588 0.3529412 
      
      set.seed(625)
      splitIndex = createDataPartition(simple$MR, p =.50, list = FALSE, times = 1)
      smote.trainx = simple[splitIndex,]
      smote.test = simple[-splitIndex,]
      prop.table(table(smote.trainx$MR))
      #0.6470588 0.3529412 
      
      # USE SMOTE TO OVERSAMPLE DATA
      smote.train <- SMOTE(MR ~ ., smote.trainx[,!(names(smote.trainx) %in% c('documentno','narrative'))], perc.over = 500,perc.under=100)
      table(smote.train$MR)
      
      # DEFINE RF ON SMOTE OVERSAMPLED DATA
      rf.smote <- randomForest(MR ~ ., data = smote.train, mtry = 15, ntree = 1000)
      rf.smote
      
      ######################################################################################################
      # USE ADABOOST TO IMPLEMENT BOOSTING ALGORITHM 
      
      set.seed(625)
      mr.adaboost = boosting(MR ~ . , data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], boos = T, mfinal = 300, coeflearn = 'Freund')
      adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[701:1019,!(names(simple) %in% c('documentno','narrative'))])
      
      ######################################################################################################
      # PRINT ALL PREDICTIONS 
      
      # SMOTE
      rf.smote.pred = predict(rf.smote, smote.test, type="class")
      table(smote.test$MR, predicted = rf.smote.pred)
      
      # ROSE
      rf.rose.pred = predict(rf.rose, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))],type="class")
      table(simple[701:1019,1], predicted = rf.rose.pred)
      
      # SIMPLE CART
      cart.predictions = predict(cart, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))],type="class")
      table(simple[701:1019,1], predicted = cart.predictions)
      
      # RF UNBALANCED 
      rf.predictions = predict(rf, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))],type="class")
      table(simple[701:1019,1], predicted = rf.predictions)
      
      # BOOSTING
      adaboost.pred$confusion
      adaboost_test = cbind(simple[701:1019,], adaboost.pred$class)
      names(adaboost_test)[names(adaboost_test) == 'adaboost.pred$class'] = 'adaboost'
      #View(adaboost_test[adaboost_test$MR == "YES" & adaboost_test$adaboost == "NO",]$documentno)
}

if (data.type == "real accidents data") {
    # USE BOOSTING TO CLASSIFY REAL ACCIDENTS DATA WITH UNKNOWN "MR" STATUS
    set.seed(625)
    mr.adaboost = boosting(MR ~ . , data = simple[simple$type!="unclassified",!(names(simple) %in% c('documentno','narrative','type','mineid'))], boos = T, mfinal = 300, coeflearn = 'Freund')
    adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[simple$type=="unclassified",!(names(simple) %in% c('documentno','narrative','type','mineid'))])
    accidents.data = cbind(simple[simple$type=="unclassified",], adaboost.pred$class)
    names(accidents.data)[names(accidents.data) == 'adaboost.pred$class'] = 'adaboost'
    
    ######################################################################################################
    # POST-PROCESSING
    
    # merge in the rest of the variables from the original accidents-mines data #     
    accidents.mines = read.csv("C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/prepped_mines_accidents.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
    accidents.data = merge(accidents.data, accidents.mines,by="documentno", all = TRUE)
    accidents.data = accidents.data[, c(-grep("\\.x", names(accidents.data)))]
    names(accidents.data) = gsub("\\.[x|y]", "", names(accidents.data))
    
    # now manually weed out false positives and negatives that could not have been foreseen in the training data # 
    accidents.data$manual.predict = ifelse(((accidents.data$likely.activy == 1 & accidents.data$likely.class == 1 & accidents.data$false.keyword == 0) |
                                           (accidents.data$likely.occup == 1 & 
                                                (accidents.data$maybe.activy == 1 | accidents.data$likely.activy == 1 | accidents.data$likely.class == 1 | accidents.data$maybe.keyword == 1)) |
                                           (accidents.data$likely.activy == 1 & 
                                                (accidents.data$maybe.occup == 1 | accidents.data$likely.class == 1 | accidents.data$maybe.keyword == 1)) |
                                           (accidents.data$maybe.occup == 1 & 
                                                ((accidents.data$maybe.activy == 1 & accidents.data$likely.class == 1) | 
                                                 (accidents.data$likely.class == 1 & accidents.data$maybe.keyword == 1) | 
                                                 (accidents.data$maybe.activy == 1 & accidents.data$maybe.keyword == 1))) |
                                           accidents.data$likely.keyword == 1) & accidents.data$accident.only == 0, 1, 0)
    
    # false negatives #
    accidents.data[, "flashburn"] = ifelse(grepl("weld.{1,40}flash( |-)*burn", accidents.data[,"narrative"]) | 
                                           grepl("flash( |-)*burn.{1,40}weld", accidents.data[,"narrative"]), 1, 0)
    
    accidents.data$false.neg = ifelse(accidents.data$flashburn == 1 & accidents.data$adaboost == "NO", 1, 0)
    
    # false positives #  
    accidents.data[, "carpal.tunnel"] = ifelse((grepl("carp(a|u|e)l( |-)*tun(n)*(e|l)(e|l)", accidents.data[,"narrative"]) | 
                                                grepl("bursitis", accidents.data[,"narrative"])) &
                                                !grepl("fracture", accidents.data[,"narrative"]), 1, 0)
    accidents.data[, "cumulative"] = ifelse(grepl("rep(e|i)t(e|a|i)ti(v|n)e", accidents.data[,"narrative"]) | 
                                            grepl("(cumulative|degenerativ)", accidents.data[,"narrative"]) |
                                            grepl("repeated(ed)*.{1,10}(mo(tion|vement)|trauma|irritation)", accidents.data[,"narrative"]) |
                                            grepl("long( |-)*term", accidents.data[,"narrative"]) | grepl("slow( |-)*on( |-)*set", accidents.data[,"narrative"]), 1, 0)
    accidents.data[, "hearingloss"] = ifelse(grepl("hearing.los", accidents.data[,"narrative"]) | grepl("los.{1,10}hearing", accidents.data[,"narrative"]) |
                                             grepl("n(o|i)(o|i)se exposur", accidents.data[,"narrative"]) |
                                             grepl("dimini.{1,10}hearing", accidents.data[,"narrative"]) |
                                             grepl("thr(e|i)s(h)*( |-)*hold( |-)*shift", accidents.data[,"narrative"]) |
                                             grepl("shift.{1,4}change.{1,30}hear", accidents.data[,"narrative"]) |
                                             grepl("exposur(e)*.{1,20}noise", accidents.data[,"narrative"]), 1, 0)
    accidents.data[, "exposure"] = ifelse(grepl("((prolon|occupation|long( |-)*term).{1,8}exposur)|(exposur.{1,20}(noise|we(a)*ther))|p(n|h)e(n)*umo(n)*co(nio)*sis", accidents.data[,"narrative"]) | 
                                          accidents.data$natureofinjury == "pneumoconiosis,black lung", 1, 0)
    accidents.data[, "heartattack"] = ifelse(grepl("heart( |-)*at(t)*ac(k|h)", accidents.data[,"narrative"]) | accidents.data$natureofinjury == "heart attack", 1, 0)
    accidents.data[, "unrelated"] = ifelse(grepl("not work relat", accidents.data[,"narrative"]) | 
                                           grepl("no.{1,20}(specific|single).{1,5}(accident|injury|indicent|exposure)", accidents.data[,"narrative"]) |  
                                          (grepl("no (accident|incident|injury)", accidents.data[,"narrative"]) & 
                                          !grepl("no (accident|incident|injury).{1,5}report", accidents.data[,"narrative"])), 1, 0)
    
    # last ditch attempt to find likely verbs and nouns before dropping false positives # 
    accidents.data[, "working.on"] = ifelse(grepl("(to work|workin(g)*)( |-)*(on|in|under|out|at)", accidents.data[,"narrative"]), 1, 0)
    accidents.data[, "barring"] = ifelse(grepl("barr(ed|ing).{1,10}(rock|motor)", accidents.data[,"narrative"]), 1, 0)
    accidents.data[, "otherverb"] = ifelse(grepl("( |^)patch", accidents.data[,"narrative"]) | 
                                           grepl("re(-)*(build|pack|fuel|assembl|lin)", accidents.data[,"narrative"]) | 
                                           grepl("(re)*adjust", accidents.data[,"narrative"]) | 
                                           grepl("(secure|unplug)", accidents.data[,"narrative"]) | 
                                           grepl("trouble( |-)*shoot", accidents.data[,"narrative"]) | 
                                           grepl("to drain", accidents.data[,"narrative"]) | 
                                           grepl("mod(i|y)f(y|ication)", accidents.data[,"narrative"]) | 
                                           grepl("(mount|splic|bolt|adjust|digg|drill|cutt|unload|dislodg|pump|lift|jack|lay|haul|spread|position|tap(p)*|air|oil|fuel|drain|hook)(ing|ign|ed)", accidents.data[,"narrative"]) |
                                           grepl("(mov|hang|chan|putt|load|pry|assembl|push|pul(l)*(l)*|swing|trim(m)*|carry|strip(p)*|torqu(e)*|shovel(l)*|plac|pick|dispos)(ing|ign|ed)", accidents.data[,"narrative"]) |
                                           grepl("(grind|tension|clip(p)*|notch|straighten|band|guid(e)*|throw|rotat|saw|apply|align|tear|(un)*screw|attach|latch|goug|clear|restor)(ing|ign|ed)", accidents.data[,"narrative"]) |
                                           grepl("(set(t)*|put(t)*|pump|tak|prim)(ing).{1,10}(tire|wheel|pump|oil|links|fuel)", accidents.data[,"narrative"]) | 
                                           grepl("tr(ied|ying|yign).{1,3}to.{1,3}(free|take( |-)*out|shut( |-)*down|position|start|lift|pry|press|rotate|roll|sep(e|a)rat|releas)", accidents.data[,"narrative"]) | 
                                           grepl("attempt(ed|ing)*.{1,3}to.{1,3}(free|take( |-)*out|position|shut( |-)*down|pry|lift|put|get|unplug|unstop|lay|clear|start|press|rotate|roll|sep(e|a)rat)", accidents.data[,"narrative"]), 1, 0)
    accidents.data[, "othernoun"] = ifelse(grepl("anti( |-)*freeze", accidents.data[,"narrative"]) | 
                                           grepl("sweeper", accidents.data[,"narrative"]), 1, 0)
    
    accidents.data$other.keyword = ifelse((accidents.data$otherverb == 1 | accidents.data$othernoun == 1 |
                                           accidents.data$trash == 1 | accidents.data$wrench == 1 |
                                           accidents.data$moretools == 1 | accidents.data$loosen == 1 |
                                           accidents.data$tire == 1 | accidents.data$splice == 1 |
                                           accidents.data$working.on == 1 | accidents.data$barring == 1) &
                                           accidents.data$accident.only == 0, 1, 0)
  
    # flag definitely and likely false positives # 
    accidents.data$false.pos = ifelse((accidents.data$carpal.tunnel == 1 | accidents.data$cumulative == 1 | accidents.data$heartattack == 1 |
                                       accidents.data$hearingloss == 1 | accidents.data$exposure == 1 |
                                       accidents.data$unrelated == 1 | accidents.data$accident.only == 1) & accidents.data$adaboos == "YES", 1, 0)
    accidents.data$false.pos = ifelse(accidents.data$adaboos == "YES" & accidents.data$likely.keyword == 0 &
                                      accidents.data$maybe.keyword == 0 & accidents.data$other.keyword == 0, 1, accidents.data$false.pos)  
    
    # SAVE FINAL MR PREDICTION AND DATASET WITH JUST DOC NO'S  & PREDICTIONS
    accidents.data$MR  = ifelse((accidents.data$adaboost == "YES" & accidents.data$false.pos == 0) | accidents.data$false.neg == 1, 1, 0)
    accidents.data = accidents.data[, c(-grep("accident.only", names(accidents.data)), -grep("adaboost", names(accidents.data)),
                                      -grep("barring", names(accidents.data)), -grep("battery", names(accidents.data)),
                                      -grep("bits", names(accidents.data)), -grep("belt", names(accidents.data)),
                                      -grep("carpal.tunnel", names(accidents.data)), -grep("belt", names(accidents.data)) 
                                      -grep("changing", names(accidents.data)), -grep("check", names(accidents.data)),
                                      -grep("cleaning", names(accidents.data)), -grep("cover", names(accidents.data)),
                                      -grep("conveyor", names(accidents.data)), -grep("cumulative", names(accidents.data)),
                                      -grep("datasource", names(accidents.data)), -grep("dismantl", names(accidents.data)),
                                      -grep("district", names(accidents.data)), -grep("exposure", names(accidents.data)),
                                      -match("falling.accident", names(accidents.data)), -grep("false.", names(accidents.data)),
                                      -grep("fix", names(accidents.data)), -grep("flashburn", names(accidents.data)),
                                      -grep("grease", names(accidents.data)), -grep("hearingloss", names(accidents.data)),
                                      -grep("heartattack", names(accidents.data)), -grep("helping", names(accidents.data)),
                                      -grep("hoist", names(accidents.data)), -grep("inspect", names(accidents.data)),
                                      -grep("install", names(accidents.data)), -grep("latitude", names(accidents.data)),
                                      -grep("longitude", names(accidents.data)), -grep("likely", names(accidents.data)),
                                      -grep("like", names(accidents.data)), -grep("loosen", names(accidents.data)),
                                      -grep("lug", names(accidents.data)), -grep("maintain", names(accidents.data)),
                                      -grep("manual.predict", names(accidents.data)), -grep("maybe", names(accidents.data)),
                                      -grep("moretools", names(accidents.data)), -grep("mrworker", names(accidents.data)),  
                                      -grep("oil", names(accidents.data)), -grep("other", names(accidents.data)),
                                      -grep("pain", names(accidents.data)), -grep("power", names(accidents.data)), -grep("pullbelt", names(accidents.data)),
                                      -grep("repair", names(accidents.data)), -grep("reposition", names(accidents.data)),
                                      -grep("rethread", names(accidents.data)), -grep("retrack", names(accidents.data)),
                                      -grep("rethread", names(accidents.data)), -grep("roller", names(accidents.data)), -grep("remove", names(accidents.data)),
                                      -grep("rplace", names(accidents.data)), -grep("shovel", names(accidents.data)), -grep("splice", names(accidents.data)),
                                      -grep("surgery", names(accidents.data)), -grep("tests", names(accidents.data)), -grep("service", names(accidents.data)),
                                      -grep("tighten", names(accidents.data)), -grep("tire", names(accidents.data)),
                                      -grep("toolbox", names(accidents.data)), -grep("trash", names(accidents.data)),
                                      -grep("type", names(accidents.data)), -grep("unrelated", names(accidents.data)),
                                      -grep("washingdown", names(accidents.data)), -grep("welding", names(accidents.data)),
                                      -grep("working.on", names(accidents.data)), -grep("wrench", names(accidents.data)))]
    
write.csv(accidents.data, file = "C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/accidents_with_predictions.csv", row.names = FALSE)
}

######################################################################################################

