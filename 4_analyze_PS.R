# NIOSH Project 2014-N-15776

# 4 - Analyze PS (Pinning and Striking Injuries)
  # Loads accidents training set, creates keywords and various predictors, implements various classification algorithms.

# Last edit 9/13/16

# This file loads the training set sent to us by Miguel Reyes on January 29th, 2016 for use in constructing our 
# Pinning & Striking injury classification algorithm. We clean and format the variables, conduct narrative analysis
# on the injury description fields, and group existing categorical variables by how likely they are to predict "PS"
# injuries. We then employ various machine learning algorithms including CaRT, random forest, and boosting. We also 
# create a compound-algorithm using several models and pre- and post-processing for maximum accuracy.

######################################################################################################

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
library(dummies)
library(stringr)

# define file names
  # input: coded PS training set - sent to us by NIOSH on 8/28/2015
coded_training_set_file_name = "X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv"
  # input: all accidents data, unclassified, cleaned in 2_clean_accidents.R and merged on mines in 3_merge_accidents.R
accidents_data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"
  # output: all accidents, now classified as PS after algorithm
classified_accidents_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_coded/PS_accidents_with_predictions.rds"

# FOR TESTING
false_neg_file_name = "C:/Users/slevine2/Desktop/ps_false_negatives.csv"
false_pos_file_name = "C:/Users/slevine2/Desktop/ps_false_positives.csv"

# SARAH'S COMPUTER
#coded_training_set_file_name = "C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/Injury-Classification/PS/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv"
#accidents_data_file_name = "C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/Injury-Classification/PS/merged_mines_accidents.rds"

######################################################################################################

# SET PREFERENCES 

# Set preferences - data type - either training data for model selection, or real accidents data for classification
data.type = "training data"
#data.type = "real accidents data"

# Specify imputation method
imputation_method = 3

# WHAT DOES THIS MEAN? -.-
strict = T

##################################################################################################

# LOAD DATA
ps_data = read.csv(coded_training_set_file_name)

# load in real accidents data for classification
if (data.type == "real accidents data") {
  accidents.data = readRDS(accidents_data_file_name)
}

##################################################################################################

# RENAME AND FORMAT VARS

# Drop anything missing mineid - 2 obvs
ps_data = ps_data[!is.na(ps_data$mineid),]

# Make all string variables lowercase
names(ps_data)[names(ps_data) == 'narrativemodified'] = 'narrative'
ps_data[, "narrative"] = tolower(ps_data[, "narrative"])
ps_data$mineractivity = tolower(ps_data$mineractivity)
ps_data$natureofinjury = tolower(ps_data$natureofinjury)
ps_data$degreeofinjury = tolower(ps_data$degreeofinjury)
ps_data$sourceofinjury = tolower(ps_data$sourceofinjury)
ps_data$accidenttype = tolower(ps_data$accidenttype)
ps_data$accidentclassification = tolower(ps_data$accidentclassification)
ps_data$accidenttype = tolower(ps_data$accidenttype)
ps_data$bodypart = tolower(ps_data$bodypart)
ps_data$typeofequipment = tolower(ps_data$typeofequipment)
ps_data$occupation = tolower(ps_data$occupation)
ps_data$mineractivity = tolower(ps_data$mineractivity)

# Format fields as characters
ps_data[, "narrative"] = as.character(ps_data[, "narrative"])
ps_data[, "occupcode3digit"] = as.character(ps_data[, "occupcode3digit"])
ps_data[, "occupation"] = as.character(ps_data[, "occupation"])
ps_data[, "returntoworkdate"] = as.character(ps_data[, "returntoworkdate"])

# This is used for identifying training from testing observations, or training from real observations 
ps_data[, "type"] = "training" 

# Format PS indicator as a binary factor
ps_data[, "X"] = factor(ifelse(ps_data[, "X"] == 1, "YES", "NO"))
names(ps_data)[names(ps_data) == "X"] = "PS"

##################################################################################################

# DO THIS CODE IF YOU'RE RUNNING ON THE REAL ACCIDENTS DATA (NOT THE TRAINING SET)

if (data.type == "real accidents data") {
  
  # Remove uncommon vars from ps_data 
  drops <- c("bomstatecode", "fipscountycode", "fipscountyname", 
             "primarysiccode", "primarysicdesc", "primarycanvasscode", 
             "primarycanvasscodedesc", "averagemineheight", "district", 
             "officecode", "officename", "primarysiccodegroup", 
             "primarysiccodesuffix", "secondarysiccode", "secondarysicdesc",
             "secondarysiccodegroup", "secondarysiccodesuffix", "secondarycanvasscode",
             "secondarycanvasscodedesc", "i", "portableoperationindicator", 
             "portablefipsstatecode", "hourspershift", "maintenanceshiftsperday",
             "numberofemployees", "longitude", "latitude",
             "minegascategorycode", "methaneliberation", "noofproducingpits", 
             "noofnonproducingpits", "nooftailingponds", "roomandpillarindicator",
             "highwallminerindicator", "multiplepitsindicator", "minersrepindicator",
             "safetycommitteeindicator", "milesfromoffice", "directionstominemodified",
             "nearesttown")
  ps_data = ps_data[, !(names(ps_data) %in% drops)]
  
  # Remove uncommon vars from ps_data   
  drops <- c("fiscalyear", "fiscalquarter", "investigationbegindate", 
             "avg_hours_qtr", "avg_employment_qtr", "avg_coal_prod_qtr",
             "year", "quarter")
  accidents.data = accidents.data[, !(names(accidents.data) %in% drops)] 
  accidents.data[, "PS"] = ""
  accidents.data[, "type"] = "unclassified" 
  
  # Create lists of documentnos from each dataset 
  ps.docnos = ps_data$documentno
  ps.docnos = as.character(ps.docnos)
  accident.docnos = accidents.data$documentno
  
  # Identify common documentnos
  keep.docnos = setdiff(accident.docnos, ps.docnos) 
  
  # Remove observations from accidents data present in the ps_data/training set - 74,743 obs
  accidents.data = accidents.data[which(accidents.data$documentno %in% keep.docnos),]
  
  # Append dataset of training observations and real accidents for classification
  all.accidents <- rbind(ps_data, accidents.data) # keep this for when we do prediction
  ps_data <- rbind(ps_data, accidents.data) # 75,743 obs
}

##################################################################################################

# CLEAN UP THE NARRATIVE FIELDS 

# 23 narrative fields are polluted with other columns - split and replace these  
ps_data[, "messy"] = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", ps_data[,"narrative"]), 1, 0)
narrative_split = strsplit(ps_data[ps_data$messy == 1, "narrative"], "|", fixed = T)
messy_rows = row.names(ps_data[ps_data$messy == 1, ])
for (i in 1:length(messy_rows)) {
  ps_data[messy_rows[i], "narrative"] = unlist(narrative_split[i])[1]
  ps_data[messy_rows[i], "occupcode3digit"] = unlist(narrative_split[i])[2]
  ps_data[messy_rows[i], "occupation"] = unlist(narrative_split[i])[3]
  ps_data[messy_rows[i], "returntoworkdate"] = unlist(narrative_split[i])[4]
}
ps_data = ps_data[, c(-match("messy", names(ps_data)))]

# Deal with messy number typos in narrative fields
ps_data[, "numbertypo"] = ifelse(grepl("[a-z][0-9][a-z]", ps_data[,"narrative"]), 1, 0)
for (i in 0:9) {
  ps_data[ps_data$numbertypo == 1,]$narrative <- gsub(i, "", ps_data[ps_data$numbertypo == 1,]$narrative)
}

# Clean up common typos that may affect our keyword searches
ps_data$narrative <- gsub("ag(a)*( )*(in)*st", "against", ps_data$narrative)

##################################################################################################

# CLEAN UP OTHER VARIABLES & RECODE MISCLASSIFIED OBSERVATIONS FROM THE TRAINING SET

# Recoded in light of Miguel's 5/27/16 response to our questions
ps_data$PS[ps_data$documentno=="219891280164"] = "YES"
ps_data$PS[ps_data$documentno=="219852170075"] = "YES"
ps_data$PS[ps_data$documentno=="219901620109"] = "YES"
ps_data$PS[ps_data$documentno=="220011070020"] = "NO"
ps_data$PS[ps_data$documentno=="219892570061"] = "NO"
ps_data$PS[ps_data$documentno=="219893100251"] = "NO"
ps_data$PS[ps_data$documentno=="219872990054"] = "NO"
ps_data$PS[ps_data$documentno=="219983280016"] = "NO"
ps_data$PS[ps_data$documentno=="220082800043"] = "NO"
ps_data$PS[ps_data$documentno=="219830320021"] = "NO"
ps_data$PS[ps_data$documentno=="219912970040"] = "NO"
ps_data$PS[ps_data$documentno=="219942900032"] = "NO"
ps_data$PS[ps_data$documentno=="219982380025"] = "NO"

# Recoded in light of Miguel's 6/7/16 response to our questions
ps_data$PS[ps_data$documentno=="219912970040"] = "YES"
ps_data$PS[ps_data$documentno=="219871460076"] = "NO"
ps_data$PS[ps_data$documentno=="219861280065"] = "NO"
ps_data$PS[ps_data$documentno=="220000310115"] = "NO"
ps_data$PS[ps_data$documentno=="220001180052"] = "NO"
ps_data$PS[ps_data$documentno=="219831430047"] = "NO"
ps_data$PS[ps_data$documentno=="219943180016"] = "NO"
ps_data$PS[ps_data$documentno=="220112090013"] = "NO"

# Recoded on 9/13/2016 in light of Miguel's lack of response to our questions
ps_data$PS[ps_data$documentno=="220090630033"] = "YES"
ps_data$PS[ps_data$documentno=="220050800006"] = "YES"
ps_data$PS[ps_data$documentno=="219892210062"] = "YES"
ps_data$PS[ps_data$documentno=="219950870035"] = "YES"
ps_data$PS[ps_data$documentno=="219972890025"] = "YES"
ps_data$PS[ps_data$documentno=="219930390025"] = "NO"
ps_data$PS[ps_data$documentno=="219992320012"] = "NO"
ps_data$PS[ps_data$documentno=="219853190080"] = "NO"
ps_data$PS[ps_data$documentno=="219973490121"] = "NO"
ps_data$PS[ps_data$documentno=="219852050003"] = "NO"
ps_data$PS[ps_data$documentno=="219891140147"] = "NO"
ps_data$PS[ps_data$documentno=="220020100051"] = "NO"

# Destring these numeric variables
ps_data[,grep("numberofemployees", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("numberofemployees", names(ps_data))] = as.numeric(ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = as.numeric(ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("averagemineheight", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = as.numeric(ps_data[,grep("averagemineheight", names(ps_data))])

# Merge redundant 'not-found' values within variables. Note: Values like "Unknown" or "Other" are not funneled into "No Value Found"
ps_data[, "uglocation"] = ifelse(ps_data[, "uglocation"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "uglocation"])
ps_data[, "accidenttype"] = ifelse(ps_data[, "accidenttype"] == "not elsewhereclassified", "no value found", ps_data[, "accidenttype"])
ps_data[, "immediatenotificationclass"] = ifelse(ps_data[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", 
                                                 ps_data[, "immediatenotificationclass"])
ps_data[, "natureofinjury"] = ifelse(ps_data[, "natureofinjury"] == "unclassified,not determed", "no value found", ps_data[, "natureofinjury"])
ps_data[, "equipmanufacturer"] = ifelse(ps_data[, "equipmanufacturer"] == "Not Reported", "NO VALUE FOUND", ps_data[, "equipmanufacturer"])

# Convert date variables. We drop date variables now, but eventually will make use of them. Nikhil 4/18/16
indices_with_date = grep("date", names(ps_data))
for (i in indices_with_date) {
  ps_data[,i] = as.Date(ps_data[,i], "%m/%d/%Y")
}

# Convert accident type codes to factor to make this code usable with accidents data
ps_data$accidenttypecode = as.factor(ps_data$accidenttypecode)

##################################################################################################

# GENERATE LIKELY KEYWORDS

# Pin but not pinion, pinner, pinning top, or pinned himself
ps_data[, "pin"] = ifelse(grepl("(^| )pin(n*)(e|i)[a-z]+", ps_data[,"narrative"]) &
                         !grepl("(^| )pinion", ps_data[,"narrative"]) &
                         !grepl("(^| )pinner", ps_data[,"narrative"]) &
                         !grepl("pinn(ing|ed)( ).{1,5}top", ps_data[,"narrative"]) &
                         !grepl("pinn(ing|ed).{1,5}(him|his|her)self", ps_data[,"narrative"]), 1, 0)
ps_data[, "strike"] = ifelse(grepl("str(i|u)(.*)k[a-z]*", ps_data[,"narrative"]) &
                            !grepl("str(i|u)(.*)k[a-z]*.{1,6}head", ps_data[,"narrative"]) &
                            !grepl("head.{1,6}str(i|u)(.*)k[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "trap"] = ifelse(grepl("( )trap[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "collided"] = ifelse(grepl("col(l)*i(de|ded|sion|ssion)", ps_data[,"narrative"]), 1, 0)

# Generate maybe likely keywords
ps_data[, "ranover"] = ifelse(grepl("( |^)r(a|u)n( )*(over|into)", ps_data[,"narrative"]), 1, 0)
ps_data[, "rolled"] = ifelse(grepl("rolled( )*(over|into|onto|on|down)", ps_data[,"narrative"]), 1, 0)
ps_data[, "between"] = ifelse(grepl("between", ps_data[,"narrative"]) | 
                              grepl("btwn", ps_data[,"narrative"]), 1, 0)
ps_data[, "wheel"] = ifelse(grepl("wheel", ps_data[,"narrative"]) & 
                           !grepl("wheeler", ps_data[,"narrative"]), 1, 0)
ps_data[, "by"] = ifelse(grepl("by", ps_data[,"narrative"]), 1, 0)

# Generate negative keywords

# Commented out as per Miguel's 6/7/16 response to our questions
# jarred, jolted, jostled
ps_data[, "jarring"] = ifelse(grepl("jar(r)*(ed|ing)", ps_data[,"narrative"]) |
                              grepl("jo(lt|stl)(ed|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "bounced"] = ifelse(grepl("boun(c)*( )*(e|ing)", ps_data[,"narrative"]), 1, 0)
# avoid sprocket, rockduster, etc
ps_data[, "rock"] = ifelse((grepl("rock( |$|\\.|s|,)", ps_data[,"narrative"]) & 
                           !grepl("rock( )*dust", ps_data[,"narrative"])), 1, 0)
ps_data[, "digit"] = ifelse(grepl("(finger(s)*|pinky|hand(s)*|thumb|hand( |\\.|,|$))", ps_data[,"narrative"]), 1, 0)
ps_data[, "derail"] = ifelse((grepl("(left|off|jumped).{1,15}track", ps_data[,"narrative"]) & 
                             !grepl("(left|off|jumped).{1,15}track.{1,3}switch", ps_data[,"narrative"])) | 
                              grepl("derai", ps_data[,"narrative"]), 1, 0)
ps_data[, "steering"] = ifelse(grepl("ste(e|a)ring( )*wheel.{1,15}sp(u|i)n", ps_data[,"narrative"]), 1, 0)

# Generate less good negative keywords
#ps_data[, "passenger"] = ifelse(grepl("passenger", ps_data[,"narrative"]), 1, 0)

ps_data[, "wrench"] = ifelse(grepl("wrench", ps_data[,"narrative"]), 1, 0)
ps_data[, "controls"] = ifelse(grepl("(lever|stick)", ps_data[,"narrative"]), 1, 0)
ps_data[, "resin"] = ifelse(grepl("resin", ps_data[,"narrative"]), 1, 0)
ps_data[, "atrs"] = ifelse(grepl("a(\\.)*t(\\.)*r(\\.)*s(\\.)*", ps_data[,"narrative"]), 1, 0)
ps_data[, "flew"] = ifelse(grepl("fl(ew|y|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "loose"] = ifelse(grepl("loose", ps_data[,"narrative"]), 1, 0)
ps_data[, "broke"] = ifelse(grepl("br(oke|eak)", ps_data[,"narrative"]), 1, 0)
ps_data[, "bent"] = ifelse(grepl("bent", ps_data[,"narrative"]) & 
                          !grepl("bent( )*over", ps_data[,"narrative"]), 1, 0)
ps_data[, "canopy"] = ifelse(grepl("canopy", ps_data[,"narrative"]), 1, 0)

##################################################################################################

# GENERATE KEYWORDS TO IDENTIFY FALSE POSITIVE ACCIDENTS BY CIRUMSTANCES

# Use body/seat to remove false positive accidents of someone being jostled against the seat
ps_data[, "bodyseat"] = ifelse(grepl("(back|head|neck|shoulder|elbo).{1,10}seat", ps_data[,"narrative"]) &
                              !grepl("backward.{1,10}seat", ps_data[,"narrative"]) &
                              !grepl("(bolt|over|drill)( )*head.{1,10}seat", ps_data[,"narrative"]), 1, 0) 
# Hitting head against canopy
ps_data[, "headcanopy"] = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(canopy)", ps_data[,"narrative"]) |
                                  grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(canopy)", ps_data[,"narrative"])) &
                                 !grepl("drill( )*head.{1,10}canopy", ps_data[,"narrative"]) &
                                 !grepl("over( )*head.{1,10}canopy", ps_data[,"narrative"]) &
                                 !grepl("head(ing|er|ed).{1,10}canopy", ps_data[,"narrative"]), 1, 0) 
# Going over a bump and operator hitting head 
ps_data[, "hole"] = ifelse(grepl("(hit|str(ike|uck)|r(a|u)n( )*over|(went|go)( )*over).{1,10}(rock|hole|(h|b)ump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|patch|place)))", 
                           ps_data[,"narrative"]), 1, 0)
ps_data[, "unevenbottom"] = ifelse(grepl("(hole|bump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|place|patch)))", 
                                   ps_data[,"narrative"]) & 
                                  !grepl("(bolt|drill|steel|cable|test|pin).{1,15}hole", ps_data[,"narrative"]), 1, 0)

# GENERATE KEYWORDS TO IDENTIFY ROOFBOLTING ACCIDENTS AND THEIR FALSE POSITIVES

# Roof bolting/drilling steel injuries
ps_data[, "drillsteel"] = ifelse(grepl("drill.{1,5}steel", ps_data[,"narrative"]) & 
                                 grepl("(between|btwn).{1,17}steel.{1,25}(drill|head|roof|guide|canopy|ring)", ps_data[,"narrative"]), 1, 0)
# Drill steel beraking/bending during roofbolting and caught an injury is not considered PS
ps_data[, "brokensteel"] = ifelse(grepl("drill.{1,5}steel", ps_data[,"narrative"]) & 
                                 (grepl("(drill|roof).{1,5}(steel|bolt).{1,15}(burst|ben(t|d)|br(eak|oke)|loose|drop(ped|ping)*|c(a|o)*me( )*( )*out|f(a|e)ll|stuck|clog)", ps_data[,"narrative"]) |
                                  grepl("wrench.{1,5}(slip|c(a|o)me).{1,5}off.{1,15}(bolt|drill head)", ps_data[,"narrative"]) |  
                                  grepl("wrench.{1,15}broke", ps_data[,"narrative"])), 1, 0)
ps_data[, "roofbolt"] = ifelse(grepl("(roof|( |^)rib).{1,10}bolt", ps_data[,"narrative"]) | 
                               grepl("(roof|rib).{1,25}bolting", ps_data[,"narrative"]) |
                               grepl("roof.{1,35}bolting", ps_data[,"narrative"]) |
                               grepl("bolt.{1,10}instal", ps_data[,"narrative"]) |
                               grepl("instal.{1,20}bolt", ps_data[,"narrative"]), 1, 0)
ps_data[, "bolting"] = ifelse(grepl("bolting", ps_data[,"narrative"]) |
                              grepl("put(t)*(ing)*( )*bolt.{1,10}top", ps_data[,"narrative"]), 1, 0)

# Gloves getting caught during roof bolting count as "entrapment" and should not be marked as PS
ps_data[, "entrapment"] = ifelse((grepl("drill.{1,5}steel", ps_data[,"narrative"]) | 
                                  ps_data$roofbolt == 1 |
                                  ps_data$bolting == 1 ) & 
                                 (grepl("(caught|catching|snagg(ed|ing)|grab).{1,10}(glove|shirt|sleeve)", ps_data[,"narrative"]) | 
                                  grepl("(glove|shi(r)*t|sle(e)*ve).{1,10}(entangl|cau( )*ght|catching|snagg(ed|ing))", ps_data[,"narrative"])), 1, 0)
#ps_data[, "glove"] = ifelse(grepl("glove", ps_data[,"narrative"]), 1, 0)
ps_data[ps_data$glove==1 & ps_data$entrapment==0, c("narrative","drillsteel", "roofbolt")]

##################################################################################################

# CREATE DUPLICATE NARRATIVE FIELDS AND THEN REPLACE ALL MENTIONS OF VEHICLES WITH "VEHICLE", BODY PARTS WITH "BODY", ETC.

# rename narrative variable
ps_data$old_narrative <- ps_data$narrative

# VEHICLE
ps_data$narrative <- gsub("(man|ram|s(ch|h)uttle|scoop)( |-|- |v)*(trip|car)( car)*", "VEHICLE1", ps_data$narrative)
ps_data$narrative <- gsub("( |^)car( |-|s|\\.|,|$)", " VEHICLE2 ", ps_data$narrative)
ps_data$narrative <- gsub("(m|a)(m|a)n( |-|- )*bus", "VEHICLE3", ps_data$narrative)
ps_data$narrative <- gsub("vehic(l|e)(l|e)", "VEHICLE4", ps_data$narrative)
ps_data$narrative <- gsub("person(n)*(e|a)l carrier", "VEHICLE5", ps_data$narrative)
ps_data$narrative <- gsub("wheeler", "VEHICLE6", ps_data$narrative)
ps_data$narrative <- gsub("scooter", "VEHICLE7", ps_data$narrative)
ps_data$narrative <- gsub("s(ch|h)uttle", "VEHICLE8", ps_data$narrative)
ps_data$narrative <- gsub("cricket ", "VEHICLE9", ps_data$narrative)
ps_data$narrative <- gsub("(rock|roof)( |-)*bolter", "VEHICLE10", ps_data$narrative)
ps_data$narrative <- gsub("( |^)truck", " VEHICLE11", ps_data$narrative)
ps_data$narrative <- gsub("buggy", "VEHICLE12", ps_data$narrative)
ps_data$narrative <- gsub("stam(m)*ler", "VEHICLE13", ps_data$narrative)
ps_data$narrative <- gsub("mac( |-)*(8|eight)", "VEHICLE14", ps_data$narrative)
ps_data$narrative <- gsub("(3|three)( |-)*wh(ee)*l(e)*r", "VEHICLE15", ps_data$narrative)
ps_data$narrative <- gsub("(c)*ont.{1,10}mi(e)*n(r|er|ing)", "VEHICLE16", ps_data$narrative)
ps_data$narrative <- gsub("long( |-)*wall", "VEHICLE17", ps_data$narrative)
ps_data$narrative <- gsub("load( |-)*haul(-| )*dump", "VEHICLE18", ps_data$narrative)
ps_data$narrative <- gsub("(mining|miner|minr|loading|(roof)*( )*bolt(ing)*)( )*machine", "VEHICLE19", ps_data$narrative)
ps_data$narrative <- gsub("tunnel( |-)*borer", "VEHICLE20", ps_data$narrative)
ps_data$narrative <- gsub("fork( |-)*lift", "VEHICLE21", ps_data$narrative)
ps_data$narrative <- gsub("(front( |-)*end|scraper)( )*loader", "VEHICLE22", ps_data$narrative)
ps_data$narrative <- gsub("locomotiv(e)*", "VEHICLE23", ps_data$narrative)
ps_data$narrative <- gsub("(road|motor)( |-)*grader", "VEHICLE24", ps_data$narrative)
ps_data$narrative <- gsub("motor", "VEHICLE25", ps_data$narrative)
ps_data$narrative <- gsub("tractor", "VEHICLE26", ps_data$narrative)
ps_data$narrative <- gsub("jeep", "VEHICLE27", ps_data$narrative)
ps_data$narrative <- gsub("(ore)*( |-)haul(er|age)", "VEHICLE28", ps_data$narrative)
ps_data$narrative <- gsub("rail( |-)*runner", "VEHICLE29", ps_data$narrative)
ps_data$narrative <- gsub("feeder", "VEHICLE30", ps_data$narrative)
ps_data$narrative <- gsub("s/c", "VEHICLE31", ps_data$narrative)
ps_data$narrative <- gsub("shearer", "VEHICLE32", ps_data$narrative)
ps_data$narrative <- gsub("mucker", "VEHICLE33", ps_data$narrative)
ps_data$narrative <- gsub("eimco", "VEHICLE34", ps_data$narrative)
ps_data$narrative <- gsub("jitney", "VEHICLE35", ps_data$narrative)
ps_data$narrative <- gsub("bolter", "VEHICLE36", ps_data$narrative)
ps_data$narrative <- gsub("rail( |-)*runner", "VEHICLE37", ps_data$narrative)
ps_data$narrative <- gsub("mobile", "VEHICLE38", ps_data$narrative)
ps_data$narrative <- gsub("porta(l)*( |-)*bus", "VEHICLE39", ps_data$narrative)
ps_data$narrative <- gsub("( |^|-)bus(es| |\\.|,|$)", " VEHICLE40 ", ps_data$narrative)
ps_data[!grepl("troll(e)*y( )*pol(e|l)", ps_data[,"narrative"]),]$narrative <- gsub("trol(l)*(e)*y", " VEHICLE41 ", ps_data[!grepl("troll(e)*y( )*pol(e|l)", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("to trip", ps_data[,"narrative"]),]$narrative <- gsub("( |^)trip( |$|,|\\.)", " VEHICLE42 ", ps_data[!grepl("to trip", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("scoop(er|ing)", ps_data[,"narrative"]),]$narrative <- gsub("scoop", " VEHICLE43 ", ps_data[!grepl("scoop(er|ing)", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("to tram", ps_data[,"narrative"]) & 
          !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", ps_data[,"narrative"]),]$narrative <- gsub("tram( |$|\\.|,)", " VEHICLE44 ", ps_data[!grepl("to tram", ps_data[,"narrative"]) & 
                                                                                                !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", ps_data[,"narrative"]),]$narrative)
ps_data$narrative <- gsub("mucker", "VEHICLE45", ps_data$narrative)

ps_data[, "shuttlecar_or_rbolter"] = ifelse((grepl("VEHICLE(8|10|36)", ps_data$narrative) | 
                                             grepl("(s(ch|h)uttle).{1,30}( |-|- |v)*(trip|car)( car)*", ps_data$old_narrative)), 1, 0)

# BODY PARTS
ps_data$narrative <- gsub("hand(s| |\\.|,|$)", "BODY ", ps_data$narrative)
ps_data$narrative <- gsub("finger(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("thumb", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("ankle(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("shoulder(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("knee( |s|\\.|,|$)", "BODY ", ps_data$narrative) 		# AVOID "KNEEL"
ps_data$narrative <- gsub("wrist(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("cal(f|ve|ves)", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("( |^)leg(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("eye(lid|brow|s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("cheek(s|bones)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("bone(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("( |^)lip(s)*", " BODY", ps_data$narrative)
ps_data$narrative <- gsub("( |^)ear(s)*", " BODY", ps_data$narrative)
ps_data$narrative <- gsub("chin( |$|\\.|,)", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("neck", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("(^| )(fore|for)*arm", " BODY", ps_data$narrative)
ps_data$narrative <- gsub("mouth", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("nose( |s|\\.|,|$)", "BODY ", ps_data$narrative)
ps_data$narrative <- gsub("pelvis", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("chest", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("groin", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("(t|f)ibia", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("ulna", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("radia", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("rib( |-)*cage", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("buttock(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("spine", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("elbow(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("testicle(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("t(ee|oo)th", "BODY", ps_data$narrative) # Check to make sure no equipment has teeth/tooth
ps_data$narrative <- gsub("(top|bottom) of|(r(igh)*t|l(e)*ft|his|her|both|onto|r\\.)( )*(foot|feet)", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("( |^)hip(s)*", " BODY", ps_data$narrative)
ps_data[!grepl("backward", ps_data[,"narrative"]),]$narrative <- gsub("(lowe(r)*|upper|PERSON|the|strain).{1,8}back", " BODY", ps_data[!grepl("backward", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("drill.{1,5}head", ps_data[,"narrative"]) & 
        !grepl("(over|cutter)( )*head", ps_data[,"narrative"]),]$narrative <- gsub("(^| )head( |$|\\.|,)", " BODY ", ps_data[!grepl("drill.{1,5}head", ps_data[,"narrative"]) & 
                                                                                                                             !grepl("(over|cutter)( )*head", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("(coal|the).{1,5}face", ps_data[,"narrative"]) & 
        !grepl("surface", ps_data[,"narrative"]),]$narrative <- gsub("face", "BODY", ps_data[!grepl("(coal|the).{1,5}face", ps_data[,"narrative"]) & 
                                                                                             !grepl("surface", ps_data[,"narrative"]),]$narrative)

# Generate some positive keywords using the body parts, before subbing in the pin/strike/trap masks
ps_data[, "bumped"] = ifelse((grepl("bump(ed|ing)( )*(over|into|onto)", ps_data[,"narrative"]) | 
                              grepl("bump(ed|ing).{1,10}BODY", ps_data[,"narrative"])) &
                             !grepl("bump(ed|ing).{1,10}head", ps_data[,"old_narrative"]), 1, 0)
ps_data[, "caught"] = ifelse(grepl("caught.{1,15}(between| in )", ps_data[,"old_narrative"]) |
                             grepl("caught.{1,10}BODY", ps_data[,"narrative"]) |
                             grepl("BODY.{1,6}caught", ps_data[,"narrative"]), 1, 0)
ps_data[, "hit"] = ifelse(grepl("( |^)hit.{1,5}(by|him|his|her|employee|ee)", ps_data[,"old_narrative"]) |
                          grepl("( |^)hit.{1,10}BODY", ps_data[,"narrative"]), 1, 0)
ps_data[, "dropped"] = ifelse(grepl("(lowe(r)*(ing|ed)*|drop(p)*(ing|ed)*).{1,15}(bucket|drill( |-)*head|drill( |-)*pod|pinner( |-)*head).{1,15}BODY", ps_data[,"narrative"]), 1, 0)
ps_data[, "neg_wrench"] = ifelse(ps_data$wrench == 1 & 
                                (grepl("(burst|ben(t|d)|br(eak|oke)|loose|dislodge|shifted|drop(ped|ping)*|c(a|o)*me( )*( )*(out|off)|f(a|e)ll|stuck|clog|slipped)+", ps_data[, "old_narrative"]) | 
                                                                                                                                                                      ps_data$flew == 1 | 
                                                                                                                                                                      ps_data$caught == 1), 1, 0)
    
# PIN/STRIKE/TRAP
ps_data$narrative <- gsub("( |^)pin(n)*(ed|ing)", " PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("(s)*tr(u|i)(c)*k(e|ing)*", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("r(a|u)n( )*(into|over)", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("col(l)*ided( w| with)*", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("( |^)trap(p)*(ed|ing)", " PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("rolled (into|onto|over)", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("c(a|u)(a|u)ght", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("catching|to catch", "PINNED/STRUCK", ps_data$narrative)
ps_data[ps_data$hole == 0,]$narrative <- gsub("( |^)hit(t)*(ing)*( |$|\\.|,|s)", "PINNED/STRUCK", 
                                         ps_data[ps_data$hole == 0,]$narrative)
ps_data[grepl("VEHICLE.{1,5}got on{1,5}BODY", ps_data[,"narrative"]),]$narrative <- gsub("got on", "PINNED/STRUCK", 
                                              ps_data[grepl("VEHICLE.{1,5}got on{1,5}BODY", ps_data[,"narrative"]),]$narrative)

# PERSON FLAGS
ps_data$narrative <- gsub("( |^)e(e|mp|mpl|mploye)(e)*( |$|,|\\.)", " PERSON ", ps_data$narrative)
ps_data$narrative <- gsub("( |^)i(,|\\.| )*name", " PERSON", ps_data$narrative)
ps_data$narrative <- gsub("injured person", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("(him|her)self", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("vi(c)*tim", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("repairm(a|e)n", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("maintenance( )*m(a|e)n", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("( |^)m(a|e)n( |$|,|\\.)", " PERSON ", ps_data$narrative)
ps_data$narrative <- gsub("fore(m|a)(a|e|m)n", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("ind(i)*v(idual)*(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("helper(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("person(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("worker(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("^inj(\\.)*(ured)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("the.{1,6}injured", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("injured was", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("( |^)(s)*he(r|rs)*( |$|,|\\.)", " PERSON ", ps_data$narrative)
ps_data$narrative <- gsub("( |^)hi(s|m)( |$|,|\\.)", " PERSON ", ps_data$narrative)
ps_data$narrative <- gsub("(wo)*m(a|e)n( |$|,|\\.)", "PERSON ", ps_data$narrative)

# These are less likely
ps_data$narrative <- gsub("operat(o|e)r", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("passenger", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("driver", "PERSON", ps_data$narrative)

##################################################################################################

# GENERATE VARS TO COUNT # OF UPPERCASE WORDS AND DISTANCES

# Count the number of capital words in each string
ps_data$num.vehicles <- str_count(ps_data$narrative, "VEHICLE")
ps_data$num.pinstrike <- str_count(ps_data$narrative, "PINNED/STRUCK")
ps_data$num.person <- str_count(ps_data$narrative, "PERSON")
ps_data$num.body <- str_count(ps_data$narrative, "BODY")

# Create var counting the number of unique vehicles mentioned in a narrative
uniq_vehcls = function(x) {
  return(length(unique(substr(unlist(regmatches(x, gregexpr("VEHICLE[0-9][0-9]*", x))), 8, 9))))
}
ps_data$num_unique_vehcl = sapply(ps_data$narrative, uniq_vehcls)
ps_data$mult_vehcl = ifelse(ps_data$num_unique_vehcl > 1, 1, 0)

# If two different types of vehicles are mentioned, it's much more likely to be a V-to-V striking accident
ps_data[, "dif_vehicle"] = ifelse(grepl("(second|another|different).{1,5}VEHICLE", ps_data[,"narrative"]), 1, 0)
ps_data[, "loose_rbolting"] = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps_data[,"narrative"]), 1, 0)
ps_data[, "drill_action"] = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps_data[,"narrative"]), 1, 0)

##################################################################################################

# CREATE A FEW MORE KEYWORDS ON THE NEW NARRATIVE FIELDS
                                
# Define a few narratives using the vehicle flags - before replacing body parts 
ps_data[, "operating"] = ifelse((grepl("( |^|was|while|had)(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|backin.{1,10}VEHICLE|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv)", ps_data$narrative) &
                                   (!grepl("PERSON.{1,20}(splic(e|ing)|crawl(ing)*|repair|fix)", ps_data$narrative) & 
                                      grepl("(splic(e|ing)|crawl(ing)*|repair|fix).{1,20}PERSON", ps_data$narrative)) &
                                   (grepl("operat", ps_data$mineractivity) | 
                                      (grepl("roof bolt", ps_data$mineractivity) & 
                                         !grepl("help(ing|er|)", ps_data$old_narrative))) &
                                   (!grepl("(side of|right|left|beside).{1,10}VEHICLE", ps_data$narrative) | 
                                      grepl("remote.{1,5}control", ps_data$narrative))), 1, 0)o

# Use head/roof to remove driver hitting head against vehicle roof - REQUIRES OPEATING
ps_data[, "headroof"] = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(roof|top)", ps_data[,"old_narrative"]) |
                                  grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(roof|top)", ps_data[,"old_narrative"]) | 
                                  (grepl("whip( )*lash", ps_data[,"old_narrative"]) & 
                                     ps_data$operating == 1) | 
                                  grepl("jerked.{1,10}(head|neck)", ps_data[,"old_narrative"])) &
                                 !grepl("drill( )*head.{1,10}roof", ps_data[,"old_narrative"]) &
                                 !grepl("over( )*head.{1,10}roof", ps_data[,"old_narrative"]) &
                                 !grepl("head(ing|er|ed).{1,10}roof", ps_data[,"old_narrative"]) &
                                 !grepl("head.{1,10}roof.{1,5}bolt", ps_data[,"old_narrative"]), 1, 0) 
# ROOF BOLTING

# Now create positive and negative roof bolting flags 
ps_data[, "pos_roofbolt"] = ifelse(ps_data$roofbolt == 1 & 
                                   grepl("PINNED/STRUCK.{1,15}between.{1,15}(roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|head)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead|top).{1,30}(top|roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|(h|l)ead)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead)", ps_data[,"narrative"]), 1, 0)
ps_data[, "neg_roofbolt"] = ifelse(ps_data$roofbolt == 1 & 
                                  (ps_data$entrapment == 1 | 
                                   ps_data$brokensteel == 1), 1, 0)

# Identify accidents involving a person who was inside (or hanging outside) of the vehicle
ps_data[, "in_vehicle"] = ifelse(grepl("riding.{1,10}(passenger|driver|operat(o|e)r)", ps_data[,"old_narrative"]) | 
                                 grepl("PERSON.{1,8}riding", ps_data[,"narrative"]) |
                                 grepl("riding.{1,5}outside", ps_data[,"narrative"]) |
                                !grepl("riding.{1,15}VEHICLE", ps_data[,"narrative"]), 1, 0)
# Operator arm of hand trailing outside vehicle
ps_data[, "outsidevehicle"] = ifelse(((grepl("BODY.{1,15}(resting| hanging).{1,5}(over|out|on)", ps_data[,"narrative"]) & 
                                       grepl("VEHICLE", ps_data[,"narrative"])) |
                                       grepl("BODY.{1,15}out( )*side.{1,30}VEHICLE", ps_data[,"narrative"])) &
                                      !grepl("overhang", ps_data[,"narrative"]), 1, 0)

# In several injuries the miner is struck by a cable which is never PS. However, getting hit by the boom while replacing the cable is common and is PS
ps_data[, "cable"] = ifelse((grepl("cable.{1,30}PINNED/STRUCK", ps_data[,"narrative"]) | 
                             grepl("PINNED/STRUCK.{1,30}cable", ps_data[,"narrative"])) & 
                           (!grepl("boom", ps_data[,"narrative"]) &
                            !grepl("cable.{1,30}PINNED/STRUCK.{1,15}(against|between)", ps_data[,"narrative"]) &
                            !grepl("cable( )*bolt", ps_data[,"narrative"])), 1, 0)
ps_data[, "strap"] = ifelse(grepl("strap.{1,20}PINNED/STRUCK", ps_data[,"narrative"]) | 
                            grepl("PINNED/STRUCK.{1,20}strap", ps_data[,"narrative"]), 1, 0)
ps_data[, "trolleypole"] = ifelse(grepl("PINNED/STRUCK.{1,20}troll(e)*y( )*pol(e|l)", ps_data[,"narrative"]) | 
                                  grepl("troll(e)*y( )*pol(e|l).{1,20}PINNED/STRUCK", ps_data[,"narrative"]) |
                                 (grepl(" pol(e|l).{1,20}PINNED/STRUCK", ps_data[,"narrative"]) & 
                                  grepl("troll(e)*y( )*pol(e|l)", ps_data[,"narrative"])) |                                    
                                 (grepl("PINNED/STRUCK.{1,20} pol(e|l)", ps_data[,"narrative"]) & 
                                  grepl("troll(e)*y( )*pol(e|l)", ps_data[,"narrative"])), 1, 0)
ps_data[, "tool_break"] = ifelse(grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t))", ps_data[,"old_narrative"]) & 
                                !grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t)).{1,20}PINNED/STRUCK.{1,15}between", ps_data[,"narrative"]), 1, 0)

ps_data[, "vcomp_test"] = ifelse(grepl("(seat|rail|canopy|battery|drill|steel|chain|cable)+.{1,20}VEHICLE", ps_data[,"narrative"]) | 
                                 grepl("VEHICLE.{1,20}(seat|rail|canopy|battery|drill|steel|chain|cable)+", ps_data[,"narrative"]), 1, 0)
ps_data[, "psobject_test"] = ifelse(grepl("(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+.{1,20}PINNED/STRUCK", ps_data[,"narrative"]) | 
                                    grepl("PINNED/STRUCK.{1,20}(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+", ps_data[,"narrative"]), 1, 0)
ps_data[, "strikerib"] = ifelse((grepl("PINNED/STRUCK.{0,20}( )rib", ps_data[, "narrative"]) | 
                                 grepl("( )rib.{1,20}PINNED/STRUCK", ps_data[, "narrative"])) &
                               (!grepl("(lower|upper|side|cage|right|left| in |fractured|bruised).{0,10}( )rib", ps_data[, "old_narrative"]) & 
                                !grepl("PERSON.{0,10}( )rib", ps_data[, "old_narrative"]) & 
                                !grepl(" ribs", ps_data[, "old_narrative"])), 1, 0)

##################################################################################################

# GENERATE LIKELY CIRCUMSTANCES

# Remove accidents involving falling rock 
ps_data$falling.class = ifelse(ps_data$accidentclassification == "fall of roof or back", 1, 0)

ps_data[, "falling.word"] = ifelse(grepl("rock( )*fell", ps_data[,"narrative"]) |
                                   grepl("fell.{1,20}roof", ps_data[,"narrative"]) |
                                   grepl("roof( )*f(a|e)ll", ps_data[,"narrative"]) |
                                   grepl("(rolled|fell) (from|.ff|out).{0,}( )rib", ps_data[,"narrative"]) |
                                   grepl("( )rib.{0,15}(rolled|fell) (from|.ff|out)", ps_data[,"narrative"]), 1, 0)

ps_data$falling.accident = ifelse(ps_data$falling.class == 1 | 
                                  ps_data$falling.word == 1, 1, 0)

ps_data = ps_data[, c(-match("falling.class", names(ps_data)), 
                      -match("falling.word", names(ps_data)))]

# The last non-missing is key, because otherwise we have one obs that is NA for accident.only
ps_data$accident.only = ifelse((ps_data$degreeofinjury == "accident only" | 
                                ps_data$accidenttypecode == 44) & 
                               !is.na(ps_data$accidenttypecode), 1, 0)

# GENERATE KEYWORD FLAGS

ps_data$keyword = ifelse((ps_data$pin == 1 | 
                          ps_data$strike == 1 | 
                          ps_data$pos_roofbolt == 1 | 
                          ps_data$trap == 1 | 
                          ps_data$collided == 1 | 
                          ps_data$hit == 1 | 
                          ps_data$dropped == 1 | 
                          ps_data$ranover == 1 | 
                          ps_data$bumped == 1 | 
                          ps_data$caught == 1 |
                          ps_data$rolled == 1 | 
                          ps_data$between == 1 | 
                          ps_data$wheel == 1) &
                         (ps_data$falling.accident == 0), 1, 0)

ps_data$false_keyword = ifelse((ps_data$jarring == 1 | 
                                ps_data$outsidevehicle == 1 | 
                                ps_data$steering == 1 | 
                                ps_data$neg_roofbolt == 1 |
                                ps_data$bounced == 1 | 
                                ps_data$rock == 1 | 
                                ps_data$derail == 1 | 
                                ps_data$cable == 1 | 
                                ps_data$tool_break == 1 | 
                                ps_data$bodyseat == 1 | 
                                ps_data$headroof == 1 | 
                                ps_data$strap == 1 | 
                                ps_data$trolleypole == 1 | 
                                ps_data$entrapment == 1 |
                                ps_data$hole == 1), 1, 0)

ps_data$maybe_false_keyword = ifelse((ps_data$digit == 1 | 
                                      ps_data$operating == 1 |
                                      ps_data$bent == 1 | 
                                      ps_data$strikerib == 1 |
                                      ps_data$wrench == 1 |
                                      ps_data$controls == 1 | 
                                      ps_data$resin == 1 |
                                      ps_data$loose == 1 | 
                                      ps_data$broke == 1 | 
                                      ps_data$canopy == 1 | 
                                      ps_data$flew == 1), 1, 0)

##################################################################################################

# GENERATE LIKELY CLASSES

ps_data$likely_class = ifelse((ps_data$accidentclassification == "powered haulage" | 
                               ps_data$accidentclassification == "machinery" ), 1, 0)

ps_data$unlikely_class = ifelse((ps_data$accidentclassification == "disorders (repeated trauma)" | 
                                 ps_data$accidentclassification == "electrical" |
                                 ps_data$accidentclassification == "explosives and breaking agents" | 
                                 ps_data$accidentclassification == "stepping or kneeling on object" | 
                                 ps_data$accidentclassification == "ignition or explosion of gas or dust"), 1, 0)

ps_data$uncertain_class = ifelse((ps_data$accidentclassification == "fall of roof or back" |
                                  ps_data$accidentclassification == "handling of materials" |
                                  ps_data$accidentclassification == "slip or fall of person" |
                                  ps_data$accidentclassification == "fall of face/rib/pillar/side/highwall" |
                                  ps_data$accidentclassification == "handtools (nonpowered)" |
                                  ps_data$accidentclassification == "no value found" |
                                  ps_data$accidentclassification == "other" |
                                  ps_data$accidentclassification == "striking or bumping"), 1, 0)

# GENERATE LIKELY TYPES

ps_data$likely_type = ifelse((ps_data$accidenttype == "struck by, nec" | 
                              ps_data$accidenttype == "struck by powered moving obj" |
                              ps_data$accidenttype == "struck by rollng or slidng obj" |
                              ps_data$accidenttype == "struck against moving object" |
                              ps_data$accidenttype == "cght i, u, b, rnng, mshng objs" |
                              ps_data$accidenttype == "cght i, u, b, mvng & sttn objs" |
                              ps_data$accidenttype == "caught i, u, b, moving objects" |
                              ps_data$accidenttype == "cght in, under, or btween, nec" |
                              ps_data$accidenttype == "struck against moving object"), 1, 0)

ps_data$unlikely_type = ifelse((ps_data$accidenttype == "fall from ladders" | 
                                ps_data$accidenttype == "fall to lower level, nec" |
                                ps_data$accidenttype == "fall to wlkway or wrkng surfc" |
                                ps_data$accidenttype == "fall onto or against objects" |
                                ps_data$accidenttype == "rubbed or abraded, nec" |
                                ps_data$accidenttype == "bodily reaction, nec" |
                                ps_data$accidenttype == "over-exertion in lifting objs" |
                                ps_data$accidenttype == "ovr-exrtn in pllng, pshng objs" |
                                ps_data$accidenttype == "ovrexrtn in wldng, thrwng objs" |
                                ps_data$accidenttype == "contact with elctric current" |
                                ps_data$accidenttype == "acc type, without injuries" |
                                ps_data$accidenttype == "contct w/ hot objs or substanc" |
                                ps_data$accidenttype == "absrtn rad caust txc & nox sbs" |
                                ps_data$accidenttype == "flash burns (electric)" |
                                ps_data$accidenttype == "over-exertion, nec"), 1, 0)

ps_data$uncertain_type = ifelse((ps_data$accidenttype == "struck against stationary obj" |
                                 ps_data$accidenttype == "fall frm mach, vehicle, equip" |
                                 ps_data$accidenttype == "struck by falling object" |
                                 ps_data$accidenttype == "struck by flying object" |
                                 ps_data$accidenttype == "no value found" |
                                 ps_data$accidenttype == "not elsewhere classified"), 1, 0)

ps_data$maybe_type = ifelse((ps_data$accidenttype == "acc type, without injuries" |
                             ps_data$accidenttype == "struck against stationary obj" |
                             ps_data$accidenttype == "fall frm mach, vehicle, equip" |
                             ps_data$accidenttype == "struck by falling object" |
                             ps_data$accidenttype == "struck by flying object" |
                             ps_data$accidenttype == "no value found" |
                             ps_data$accidenttype == "not elsewhere classified") & 
                             ps_data$false_keyword == 0, 1, 0)

# GENERATE LIKELY EQUIPMENT

vehcl_equip_codes = c("06", "13", "28", "53", "?")
ps_data[, "moving_vehcl"] = ifelse(!(ps_data$equiptypecode %in% vehcl_equip_codes), 1, 0)

ps_data$likely_equip = ifelse((ps_data$equiptypecode == "12" |  
                               ps_data$equiptypecode == "23" |
                               ps_data$equiptypecode == "33" | 
                               ps_data$equiptypecode == "34" |
                               ps_data$equiptypecode == "35" | 
                               ps_data$equiptypecode == "37" |
                               ps_data$equiptypecode == "41" | 
                               ps_data$equiptypecode == "61" |
                               ps_data$equiptypecode == "67") & 
                               ps_data$false_keyword == 0, 1, 0)

ps_data$unlikely_equip = ifelse((ps_data$equiptypecode == "06" |  
                                 ps_data$equiptypecode == "09" |
                                 ps_data$equiptypecode == "15" |  
                                 ps_data$equiptypecode == "16" |
                                 ps_data$equiptypecode == "20" |
                                 ps_data$equiptypecode == "28" |
                                 ps_data$equiptypecode == "29" |
                                 ps_data$equiptypecode == "53" |
                                 ps_data$equiptypecode == "55" | 
                                 ps_data$equiptypecode == "?" ), 1, 0)

ps_data$uncertain_equip = ifelse((ps_data$equiptypecode == "54" |
                                  ps_data$equiptypecode == "71" |  
                                  ps_data$equiptypecode == "25" |
                                  ps_data$equiptypecode == "13" | 
                                  ps_data$equiptypecode == "14"), 1, 0)

# GENERATE LIKELY SOURCES

ps_data$likely_source = ifelse((ps_data$injurysourcecode == "074" | 
                                ps_data$injurysourcecode == "077" | 
                                ps_data$injurysourcecode == "081" | 
                                ps_data$injurysourcecode == "087" | 
                                ps_data$injurysourcecode == "104" | 
                                ps_data$injurysourcecode == "105" |
                                ps_data$injurysourcecode == "106" | 
                                ps_data$injurysourcecode == "107" | 
                                ps_data$injurysourcecode == "108" | 
                                ps_data$injurysourcecode == "110") & 
                                ps_data$false_keyword == 0, 1, 0)

ps_data$unlikely_source = ifelse((ps_data$injurysourcecode == "003" | 
                                  ps_data$injurysourcecode == "004" | 
                                  ps_data$injurysourcecode == "006" | 
                                  ps_data$injurysourcecode == "007" | 
                                  ps_data$injurysourcecode == "008" | 
                                  ps_data$injurysourcecode == "009" |
                                  ps_data$injurysourcecode == "012" | 
                                  ps_data$injurysourcecode == "051" | 
                                  ps_data$injurysourcecode == "089" |
                                  ps_data$injurysourcecode == "067" | 
                                  ps_data$injurysourcecode == "068" |
                                  ps_data$injurysourcecode == "078" | 
                                  ps_data$injurysourcecode == "079" | 
                                  ps_data$injurysourcecode == "080" | 
                                  ps_data$injurysourcecode == "083" |
                                  ps_data$injurysourcecode == "090" |
                                  ps_data$injurysourcecode == "092" | 
                                  ps_data$injurysourcecode == "093" | 
                                  ps_data$injurysourcecode == "096" | 
                                  ps_data$injurysourcecode == "098" | 
                                  ps_data$injurysourcecode == "112" |
                                  ps_data$injurysourcecode == "116" | 
                                  ps_data$injurysourcecode == "125"), 1, 0)

ps_data$uncertain_source = ifelse((ps_data$likely_source == 0 & 
                                   ps_data$unlikely_source == 0), 1, 0)

# GENERATE LIKELY NATURES

ps_data$likely_nature = ifelse(ps_data$natureofinjury == "crushing", 1, 0)
ps_data$unlikely_nature = ifelse((ps_data$natureofinjury == "burn or scald (heat)" |
                                  ps_data$natureofinjury == "burn,chemicl-fume,compoun" |
                                  ps_data$natureofinjury == "elect shock,electrocution" |
                                  ps_data$natureofinjury == "hearing loss or impairmnt" |
                                  ps_data$natureofinjury == "dust in eyes" |
                                  ps_data$natureofinjury == "elect.arc burn-not contac"), 1, 0)

ps_data$uncertain_nature = ifelse((ps_data$natureofinjury == "no value found" |
                                   ps_data$natureofinjury == "sprain,strain rupt disc" |
                                   ps_data$natureofinjury == "cut,lacer,punct-opn wound" |
                                   ps_data$natureofinjury == "contusn,bruise,intac skin" |
                                   ps_data$natureofinjury == "fracture,chip" |
                                   ps_data$natureofinjury == "multiple injuries" |
                                   ps_data$natureofinjury == "amputation or enucleation" |
                                   ps_data$natureofinjury == "dislocation" |
                                   ps_data$natureofinjury == "other injury,nec" |
                                   ps_data$natureofinjury == "scratch,abrasion,superfcl" |
                                   ps_data$natureofinjury == "concussion-brain,cerebral" |
                                   ps_data$natureofinjury == "joint,tendon,muscl inflam"), 1, 0)

# GENERATE LIKELY ACTIVITIES

ps_data[, "likely_actvty"] = ifelse((grepl("operate", ps_data$mineractivity) | 
                                     grepl("roof", ps_data$mineractivity)), 1, 0)

ps_data[, "maybe_likely_actvty"] = ifelse(grepl("move/reel", ps_data$mineractivity) | 
                                          grepl("handling supplies/materials", ps_data$mineractivity), 1, 0)

ps_data$unlikely_activity = ifelse((ps_data$activitycode == "009" | 
                                    ps_data$activitycode == "016" | 
                                    ps_data$activitycode == "020" | 
                                    ps_data$activitycode == "022" | 
                                    ps_data$activitycode == "025" | 
                                    ps_data$activitycode == "026" |
                                    ps_data$activitycode == "027" | 
                                    ps_data$activitycode == "029" | 
                                    ps_data$activitycode == "030" | 
                                    ps_data$activitycode == "032" | 
                                    ps_data$activitycode == "034" | 
                                    ps_data$activitycode == "036" |
                                    ps_data$activitycode == "075" | 
                                    ps_data$activitycode == "066" | 
                                    ps_data$activitycode == "065" | 
                                    ps_data$activitycode == "056"), 1, 0)

# Not sure this is mutually exclusive AND exhaustive
ps_data$uncertain_activity = ifelse((ps_data$likely_actvty == 0 & 
                                     ps_data$maybe_likely_actvty == 0 & 
                                     ps_data$unlikely_activity == 0), 1, 0)

# GENERATE LIKELY OCCUPATIONS

ps_data$likely_occup = ifelse((ps_data$occupcode3digit == "050" | 
                               ps_data$occupcode3digit == "046" | 
                               ps_data$occupcode3digit == "028" | 
                               ps_data$occupcode3digit == "016" | 
                               ps_data$occupcode3digit == "036"), 1, 0)

# GENERATE LIKELY BODY PARTS

ps_data$unlikely_body = ifelse((ps_data$bodypartcode == "200" | 
                                ps_data$bodypartcode == "340" | 
                                ps_data$bodypartcode == "420"), 1, 0)

##################################################################################################

# SUM UP LIKELY AND UNLIKELY INDICATORS

ps_data$keyword_pts = rowSums(ps_data[,c('pin', 'strike', 'drillsteel', 
                                         'trap', 'collided', 'hit', 
                                         'dropped', 'ranover', 'bumped', 
                                         'caught', 'rolled', 'between', 
                                         'wheel')], na.rm=TRUE)

ps_data$neg_keyword_pts = rowSums(ps_data[,c('jarring', 'outsidevehicle', 'steering', 
                                             'bounced', 'rock', 'derail', 
                                             'cable', 'strap', 'trolleypole', 
                                             'tool_break', 'bodyseat', 'headroof', 
                                             'hole')], na.rm=TRUE)

ps_data$pos_pts = rowSums(ps_data[,c('likely_class', 'likely_equip', 'likely_nature', 
                                     'likely_source', 'likely_type')], na.rm=TRUE)

ps_data$neg_pts = rowSums(ps_data[,c('unlikely_class', 'unlikely_equip', 'unlikely_source', 
                                     'unlikely_nature', 'unlikely_type', 'uncertain_activity')], na.rm=TRUE)

##################################################################################################

# A FEW MORE KEYWORDS, USING EXISTING KEYWORD FLAGS 

ps_data[, "no_vehcl"] = ifelse(!grepl("VEHICLE", ps_data[, "narrative"]), 1, 0)

ps_data[, "v_to_v"] = ifelse((grepl("(VEHICLE|drill|steel|bolter|shear|cutter|tire).{1,35}PINNED/STRUCK.{1,35}VEHICLE", ps_data[, "narrative"]) |
                              grepl("VEHICLE.{1,35}PINNED/STRUCK.{1,35}(VEHICLE|drill|steel|bolter|shear|cutter|tire)", ps_data[, "narrative"])) & 
                              ps_data$hole == 0, 1, 0)

ps_data[, "v_to_p"] = ifelse((grepl("(VEHICLE).{1,20}PINNED/STRUCK.{1,20}(PERSON|BODY)", ps_data[, "narrative"]) |
                              grepl("(PERSON|BODY).{1,20}PINNED/STRUCK.{1,20}(VEHICLE)", ps_data[, "narrative"])) & 
                              ps_data$false_keyword == 0, 1, 0)

ps_data[, "int_obj_strike"] = ifelse((grepl("( )(block|rock|cho(c)*k|chunk|rail|i-beam)( )", ps_data[, "old_narrative"]) & 
                                      grepl("VEHICLE", ps_data[, "narrative"]) & 
                                      grepl("PINNED/STRUCK", ps_data[, "narrative"]) &
                                      grepl("(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|back(in|ed).{1,10}VEHICLE|VEHICLE.{1,10}back(in|ed)|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv|pull)", ps_data[, "narrative"]) &
                                      grepl("(steering |(hand )*knob).{1,20}(PINNED/STRUCK).{1,20}(BODY|PERSON)", ps_data[, "narrative"]) &
                                     (ps_data$accidenttypecode %in% c(8, 5)) & 
                                      ps_data$falling.accident == 0 & 
                                      ps_data$operating == 0), 1, 0)

##################################################################################################

# VARIOUS SIMPLE ALGORITHMS

ps_data[, "holistic"] = ifelse((((ps_data$likely_type == 1) | 
                                 (ps_data$maybe_type == 1)) & 
                                 (ps_data$likely_actvty == 1 | 
                                  ps_data$maybe_likely_actvty == 1) & 
                                 (ps_data$likely_class == 1) & 
                                  ps_data$moving_vehcl == 1), 1, 0)

# Generate likely pinning and striking accident flag
ps_data$potential_ps = ifelse(ps_data$keyword == 1 | 
                              ps_data$likely_class == 1 | 
                              ps_data$v_to_v == 1 | 
                              ps_data$v_to_p == 1, 1, 0)

# Generate our best simple algorithm pinning and striking accident flag
ps_data$likely_ps = ifelse((ps_data$keyword == 1 | 
                            ps_data$likely_class == 1 | 
                            ps_data$v_to_v == 1 | 
                            ps_data$v_to_p == 1) &
                           (ps_data$falling.accident == 0) &
                           (ps_data$bodyseat == 0 & 
                            ps_data$headroof == 0 & 
                            ps_data$hole == 0 &
                            ps_data$cable == 0 & 
                            ps_data$strap == 0 & 
                            ps_data$tool_break == 0 &
                            ps_data$outsidevehicle == 0 & 
                            ps_data$derail == 0 & 
                            ps_data$bounced == 0  & 
                            ps_data$trolleypole == 0 & 
                            ps_data$neg_roofbolt == 0 & 
                            ps_data$unlikely_nature == 0 & 
                            ps_data$unlikely_source == 0) &
                           (ps_data$neg_keyword_pts < 2 & 
                            ps_data$pos_pts > 1 & 
                            ps_data$neg_pts < 3), 1, 0)

##################################################################################################

# Drop variables with redundant or no information while keeping codes used in the algorithms below

all_vars = ps_data
ps_data = ps_data[, c(-match("accidenttime", names(ps_data)), 
                      -match("accidenttypecode", names(ps_data)),
                      -match("bodypartcode", names(ps_data)),
                      -match("classificationcode", names(ps_data)),
                      -match("coalcormetalmmine", names(ps_data)),
                      -match("contractorid", names(ps_data)),
                      -match("daysperweek", names(ps_data)), 
                      -match("daysrestrictedduty", names(ps_data)), 
                      -match("degreeofinjurycode", names(ps_data)), 
                      -match("equipmanufacturercode", names(ps_data)),
                      -match("idesc", names(ps_data)), 
                      -match("immediatenotificationcode", names(ps_data)), 
                      -match("immediatenotificationclass", names(ps_data)), 
                      -match("injurysourcecode", names(ps_data)),
                      -match("mineexperience", names(ps_data)), 
                      -match("minename", names(ps_data)),
                      -match("minestatus", names(ps_data)),
                      -match("minetype", names(ps_data)), 
                      -match("narrative", names(ps_data)), 
                      -match("natureofinjurycode", names(ps_data)),
                      -match("numbertypo", names(ps_data)),
                      -match("occupation", names(ps_data)),
                      -match("old_narrative", names(ps_data)),
                      -match("oldoccupationcode", names(ps_data)),
                      -match("operatorid", names(ps_data)),
                      -match("operatorname", names(ps_data)),
                      -match("productionshiftsperday", names(ps_data)),
                      -match("schedulechargedays", names(ps_data)),  
                      -match("shiftbeginningtime", names(ps_data)),
                      -match("stateabbreviation", names(ps_data)),
                      -match("subunitcode", names(ps_data)),
                      -match("transferredorterminated", names(ps_data)),
                      -grep("^ug(l|m)", names(ps_data)))]

# Drop date vars (now irrelevant)
ps_data = ps_data[, c(-grep("date", names(ps_data)))]

##################################################################################################
# 
# # MISSING VALUE IMPUTATION
# 
# # Make everything a factor var
# varlist = names(ps_data)
# for (i in 1:length(varlist)) {
#   ps_data[, varlist[i]] = as.factor(ps_data[, varlist[i]])
# }
# 
# # Now pick out the character vars and make sure they're the right type
# charac_vars = (c("subunit", "degreeofinjury", "accidentclassification", 
#                  "accidenttype", "documentno", "mineid", 
#                  "mineractivity", "sourceofinjury", "controllername",
#                  "natureofinjury", "bodypart", "controllerid", 
#                  "typeofequipment", "equipmanufacturer"))
# for (i in 1:length(charac_vars)) {
#   ps_data[, charac_vars[i]] = as.character(ps_data[, charac_vars[i]])
# }
# 
# # Same thing for vars that should be numeric
# num_vars = (c("numberofinjuries", "totalexperience", "jobexperience", 
#               "dayslost", "num.vehicles", "num.pinstrike", 
#               "num.person", "num.body", "num_unique_vehcl", 
#               "keyword_pts", "neg_keyword_pts", "pos_pts", 
#               "neg_pts"))
# for (i in 1:length(num_vars)) {
#   ps_data[, num_vars[i]] = as.numeric(ps_data[, num_vars[i]])
# }
# 
# # We don't use date vars as of yet so no need to store a list of their names, "logical" class vars are missing all obsvtns
# var_classes = sapply(ps_data[,names(ps_data)], class)
# charac_vars = names(var_classes[c(grep("character", var_classes), 
#                                   grep("factor", var_classes))])
# 
# # Make sure PS isn't imputed
# PS = c("PS")
# charac_vars = setdiff(charac_vars, PS)
# 
# # Format all missing/unknown vars as NAs
# for (i in 1:length(charac_vars)) {
#   ps_data[, charac_vars[i]] = ifelse((ps_data[,charac_vars[i]] == "NO VALUE FOUND" | 
#                                       ps_data[,charac_vars[i]] == "UNKNOWN" | 
#                                       ps_data[,charac_vars[i]] == "no value found" | 
#                                       ps_data[,charac_vars[i]] == "unknown" | 
#                                       ps_data[,charac_vars[i]] == "?" | 
#                                       ps_data[,charac_vars[i]] == ""), NA_character_, as.character(ps_data[,charac_vars[i]]))
#   ps_data[, charac_vars[i]] = factor(ps_data[, charac_vars[i]])
# }
# 
# # This line will report number of missings per var 
# # apply(is.na(ps_data),2,sum)
# 
# # Must define function to calculate mode for imputation methods 1 & 2
# modus = function(x) {
#   uniqv = unique(x)
#   uniqv[which.max(tabulate(match(x, uniqv)))]
# }
# 
# if (imputation_method == 1 | imputation_method == 2) {
#   # METHOD 1 IMPUTES THE MEAN (this is a weak method, was just used for testing)
#   for (i in 1:length(num_vars)) {
#     ps_data[, num_vars[i]] = ifelse(is.na(ps_data[, num_vars[i]]), 
#                                     mean(ps_data[, num_vars[i]]), ps_data[, num_vars[i]])
#   }
#   # METHOD 2 IMPUTES BY MEAN/MODE DEPEDENDING ON VAR TYPE (this is a weak method, was just used for testing)
#   if (imputation_method == 2) {
#     for (i in 1:length(num_vars)) {
#       ps_data[, num_vars[i]] = ifelse(is.na(ps_data[, num_vars[i]]), 
#                                       median(ps_data[, num_vars[i]]), ps_data[, num_vars[i]])
#     }
#   }
#   for (i in 1:length(charac_vars)) {
#     ps_data[, charac_vars[i]] = ifelse(is.na(ps_data[, charac_vars[i]]), 
#                                        modus(ps_data[, charac_vars[i]]), ps_data[, charac_vars[i]])
#   }
#   # METHOD 3 RANDOMLY SAMPLE FROM THE DISTRIBUTION - THIS IS THE PREFERRED METHOD
# } else if (imputation_method == 3) {
#   for (i in 1:length(num_vars)) {
#     i_rowsmissing = row.names(ps_data)[is.na(ps_data[, num_vars[i]])]
#     while (sum(!complete.cases(ps_data[, num_vars[i]])) > 0) {
#       replace_rows = sample(setdiff(row.names(ps_data), i_rowsmissing), length(i_rowsmissing), replace = T)
#       ps_data[i_rowsmissing, num_vars[i]] = ps_data[replace_rows, num_vars[i]]
#     }
#   }
#   for (i in 1:length(charac_vars)) {
#     i_rowsmissing = row.names(ps_data)[is.na(ps_data[, charac_vars[i]])]
#     while (sum(!complete.cases(ps_data[, charac_vars[i]])) > 0) {
#       replace_rows = sample(setdiff(row.names(ps_data), i_rowsmissing), length(i_rowsmissing), replace = T)
#       ps_data[i_rowsmissing, charac_vars[i]] = ps_data[replace_rows, charac_vars[i]]
#     }
#   }
# } 

##################################################################################################

# PRODUCE DATASETS WITH ONLY VARS OF INTEREST 

simple.data = ps_data[, c(grep("likely_", names(ps_data)),
                          grep("keyword", names(ps_data)),
                          grep("uncertain", names(ps_data)),
                          match("accident.only", names(ps_data)),
                          match("atrs", names(ps_data)),
                          match("bent", names(ps_data)),
                          match("between", names(ps_data)), 
                          match("bodyseat", names(ps_data)), 
                          match("bounced", names(ps_data)), 
                          match("broke", names(ps_data)), 
                          match("brokensteel", names(ps_data)), 
                          match("bumped", names(ps_data)),
                          match("by", names(ps_data)), 
                          match("cable", names(ps_data)),
                          match("canopy", names(ps_data)), 
                          match("caught", names(ps_data)), 
                          match("collided", names(ps_data)),
                          match("controls", names(ps_data)), 
                          match("derail", names(ps_data)),
                          match("dif_vehicle", names(ps_data)),
                          match("digit", names(ps_data)), 
                          match("documentno", names(ps_data)), 
                          match("drill_action", names(ps_data)),
                          match("drillsteel", names(ps_data)), 
                          match("dropped", names(ps_data)), 
                          match("entrapment", names(ps_data)),
                          match("falling.accident", names(ps_data)),
                          match("flew", names(ps_data)), 
                          match("headcanopy", names(ps_data)),
                          match("headroof", names(ps_data)), 
                          match("hit", names(ps_data)), 
                          match("hole", names(ps_data)), 
                          match("in_vehicle", names(ps_data)),
                          match("int_obj_strike", names(ps_data)),
                          match("jarring", names(ps_data)), 
                          match("loose", names(ps_data)),
                          match("loose_rbolting", names(ps_data)), 
                          match("maybe_type", names(ps_data)),
                          match("moving_vehcl", names(ps_data)),
                          match("mult_vehcl", names(ps_data)),
                          match("neg_pts", names(ps_data)),
                          match("neg_roofbolt", names(ps_data)),
                          match("neg_wrench", names(ps_data)),
                          match("no_vehcl", names(ps_data)),
                          match("num.pinstrike", names(ps_data)), 
                          match("num.person", names(ps_data)), 
                          match("num.body", names(ps_data)), 
                          match("num_unique_vehcl", names(ps_data)),
                          match("num.vehicles", names(ps_data)),
                          match("operating", names(ps_data)),
                          match("outsidevehicle", names(ps_data)), 
                          match("pin", names(ps_data)),
                          match("pos_pts", names(ps_data)), 
                          match("pos_roofbolt", names(ps_data)), 
                          match("potential_ps", names(ps_data)), 
                          match("PS", names(ps_data)),
                          match("psobject_test", names(ps_data)), 
                          match("ranover", names(ps_data)), 
                          match("resin", names(ps_data)), 
                          match("rock", names(ps_data)),                                    
                          match("rolled", names(ps_data)), 
                          match("roofbolt", names(ps_data)), 
                          match("shuttlecar_or_rbolter", names(ps_data)),
                          match("steering", names(ps_data)), 
                          match("strap", names(ps_data)), 
                          match("strike", names(ps_data)), 
                          match("strikerib", names(ps_data)), 
                          match("tool_break", names(ps_data)), 
                          match("trap", names(ps_data)),
                          match("trolleypole", names(ps_data)), 
                          match("type", names(ps_data)),
                          match("unevenbottom", names(ps_data)),
                          match("vcomp_test", names(ps_data)), 
                          match("v_to_v", names(ps_data)), 
                          match("v_to_p", names(ps_data)),                                    
                          match("wheel", names(ps_data)),
                          match("wrench", names(ps_data)))]

# Enforce factor storage
if (strict) {
  vars = names(simple.data)
  for (i in 1:length(vars)) {
    simple.data[, vars[i]] = factor(simple.data[, vars[i]])
  }
}

##################################################################################################

# Randomly sort data (it was ordered in stata before this)
set.seed(625)
rand <- runif(nrow(simple.data))
simple.ps <- simple.data[order(rand),]
remove(rand)
# which(colnames(simple.ps)=="PS") # prints PS column number - 81

######################################################################################################

# TO TEST VARIOUS MODELS

if (data.type == "training data" ) {

  simple.ps = simple.ps[,c(-grep("type", names(simple.ps)))]
  
  # CART
  cart <- rpart(PS ~ ., data = simple.ps[1:600, !(names(simple.ps) %in% c('documentno'))], method = "class")
  cart.predictions = predict(cart, simple.ps[601:1000,], type = "class")
  table(simple.ps[601:1000,81], predicted = cart.predictions)
  
  # RANDOM FOREST
  rf <- randomForest(PS ~ . -documentno, data = simple.ps[1:600,], mtry = 11, importance = TRUE, type = "class", ntree = 800)
  rf.predictions = predict(rf, simple.ps[601:1000,], type="class")
  table(simple.ps[601:1000,81], predicted = rf.predictions)
  
  # RANDOM FOREST WITH SMOTE
  smote.trainx = simple.ps[1:600,]
  smote.test = simple.ps[601:1000,]
  smote <- SMOTE(PS ~ ., smote.trainx, perc.over = 100, perc.under = 100)
  rf.smo <- randomForest(PS ~ . -documentno, data = smote, mtry = 10, ntree = 800)
  rf.smo.pred = predict(rf.smo, smote.test, type = "class")
  table(simple.ps[601:1000,81], predicted = rf.smo.pred)
  
  # BOOSTING
  ps.adaboost = boosting(PS ~ ., data = simple.ps[1:600, !(names(simple.ps) %in% c('documentno'))], boos = T, mfinal = 300, coeflearn = 'Freund')
  simple.adaboost.pred = predict.boosting(ps.adaboost, newdata = simple.ps[601:1000,])
  simple.adaboost.pred$confusion
  # # Predicted Class  NO YES
  #  NO  282  20
  #  YES  21  77
  
  # Generate variable with boosting predictions
  simple.adaboost.pred$class = as.factor(simple.adaboost.pred$class)
  predictions = simple.ps[601:1000,]
  predictions = cbind(predictions, simple.adaboost.pred$class)
  names(predictions)[names(predictions) == 'simple.adaboost.pred$class'] = 'prediction'
  
  # Print variable importance
  pdf("C:/Users/slevine2/Desktop/plots.pdf", width=40, height=30)
  importanceplot(ps.adaboost)
  dev.off()
  
  # Retrieve narratives of misclassified obs
  predictions = merge(predictions, all_vars[, c("narrative", "old_narrative", "documentno", "mineid")], by = "documentno")
  
  # POST-PROCESSING: MANUALLY RECODE COMMON FALSE POSITIVES # 1 = no, 2 = yes
  predictions$prediction = ifelse(predictions$entrapment == 1, 1, predictions$prediction) 
  predictions$prediction = ifelse(predictions$falling.accident == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$brokensteel == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$accident.only == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$headroof == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$headcanopy == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$hole == 1, 1, predictions$prediction)
  predictions$prediction = as.factor(predictions$prediction)
  
  # Save simple data with predictions
  predictions = predictions[,c(match("prediction", names(predictions)),
                           match("mineid", names(predictions)),
                           match("PS", names(predictions)),
                           match("old_narrative", names(predictions)),
                           match("documentno", names(predictions)))]
  
  # Inspect false negatives
  View(predictions[predictions$PS == "YES" & predictions$prediction == 1, c("old_narrative", "documentno")], "false negatives")
  write.csv(predictions[predictions$PS == "YES" & predictions$prediction == 1, c("old_narrative", "documentno")], 
            file = false_neg_file_name)
  # Inspect false positives
  View(predictions[predictions$PS == "NO" & predictions$prediction == 2, c("old_narrative", "documentno")], "false positives")
  write.csv(predictions[predictions$PS == "NO" & predictions$prediction == 2, c("old_narrative", "documentno")], 
            file = false_pos_file_name)
}

######################################################################################################

# TO CLASSIFY REAL ACCIDENTS DATA

if (data.type == "real accidents data") {
  
#   # COMPOSITE ALGORITHM
#   #splitIndex = createDataPartition(simple.ps$PS, p =.50, list = FALSE, times = 1)
#   #smote.trainx = simple.ps[splitIndex,]
#   #smote.test = simple.ps[-splitIndex,]
#   set.seed(625)
#   smote.trainx = simple.ps[1:600,]
#   smote.test = simple.ps[601:1000,]
#   
#   ######################################################################################################
#   
#   # PRE-PROCESSING: OVER SAMPLE DATA & WEED OUT FALLING ROCK ACCIDENTS
#   
#   # Use smote to oversample data
#   smote.ps <- SMOTE(PS ~ ., smote.trainx, perc.over = 600,perc.under=100)
#   table(smote.ps$PS)
#   
#   # Weed out obs that are definitely not ps
#   smote.test[, "predict"] = ifelse((smote.test$falling.accident == 0), 1, 0)
#   
#   ######################################################################################################
#   
#   # RUN A RANDOM FOREST
# 
#   # Now do a random forest on the smoted data
#   rf.smote <- randomForest(PS ~ . -documentno, data = smote.ps, mtry = 15, ntree = 1000)
#   rf.smote
#   
#   # Predict
#   rf.smote.pred = predict(rf.smote, smote.test[smote.test$predict == 1,], type="class")
#   table(smote.test[smote.test$predict == 1,]$PS, predicted = rf.smote.pred)
#   
#   # Merge on predictions
#   smote.test.aux = cbind(smote.test[smote.test$predict == 1,], rf.smote.pred)
#   post.smote.test = merge(smote.test, smote.test.aux, by = "documentno", all = T)
#   post.smote.test = post.smote.test[, c(-grep("\\.y", names(post.smote.test)))]
#   names(post.smote.test) = gsub("\\.[x|y]", "", names(post.smote.test))
#   
#   post.smote.test[, "smote_pred"] = ifelse(is.na(post.smote.test$rf.smote.pred), 1, post.smote.test$rf.smote.pred)
  
  ######################################################################################################
  
  # STEP THREE: RUN ANOTHER MODEL TO TRY AND CLASSIFY MORE FALSE NEGATIVES
  
  # Randomize
  set.seed(625)
  
  # Run boosting on training observations
  ps.adaboost = boosting(PS ~ ., data = simple.ps[simple.ps$type=="training", 
                                                !(names(simple.ps) %in% c('documentno', 'type'))], boos = T, mfinal = 1000, coeflearn = 'Freund')
  
  # Predict PS for unclassified observations
  adaboost.pred = predict.boosting(ps.adaboost, newdata = simple.ps[simple.ps$type=="unclassified",
                                                                  !(names(simple.ps) %in% c('documentno', 'type'))])

  # Generate variable with boosting predictions
  adaboost.pred$class = as.factor(adaboost.pred$class)
  accidents = cbind(simple.ps[simple.ps$type=="unclassified",], adaboost.pred$class)
  accidents = accidents[,c(-match("PS", names(accidents)))]
  names(accidents)[names(accidents) == 'adaboost.pred$class'] = 'prediction'
  
  # Merge predictions back onto real data (we just need mineid and accidentdate for the next stage)
  accidents = accidents[,c(match("prediction", names(accidents)),
                           match("documentno", names(accidents)))]
  accidents = merge(accidents, all.accidents, by="documentno")
  accidents$PS = ifelse(!is.na(accidents$prediction), accidents$prediction, accidents$PS)
  accidents = accidents[,c(match("prediction", names(accidents)),
                           match("mineid", names(accidents)),
                           match("documentno", names(accidents)))]
  
  # Save
  saveRDS(accidents, file = classified_accidents_file_name)
}

rm(list = ls())
gc()

##################################################################################################
