#analyze_PS.R by Nikhil Saifullah 4/14/16

#install.packages("dummies")
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
install.packages("stringr")
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

rm(list = ls())
#Specify imputation method here: 1 - means & modes, 2 - medians & modes, 3 - random sampling from distribution - best we can do for now
#4 - multiple imputation through regression
imputation_method = 3

ps_data = read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv")

##################################################################################################
# MAKE ALL STRING VARIABLES LOWERCASE

ps_data = ps_data[!is.na(ps_data$mineid),]
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

##################################################################################################
# CLEAN UP THE NARRATIVE FIELDS 

# 23 NARRATIVE FIELDS ARE POLLUTED WITH OTHER COLUMNS - SPLIT AND REPLACE THESE 
ps_data[, "narrative"] = as.character(ps_data[, "narrative"])
ps_data[, "occupcode3digit"] = as.character(ps_data[, "occupcode3digit"])
ps_data[, "occupation"] = as.character(ps_data[, "occupation"])
ps_data[, "returntoworkdate"] = as.character(ps_data[, "returntoworkdate"])
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

#DEAL WITH MESSY NUMBER TYPOS IN NARRATIVE FIELDS
ps_data[, "numbertypo"] = ifelse(grepl("[a-z][0-9][a-z]", ps_data[,"narrative"]), 1, 0)
for (i in 0:9) {
  ps_data$narrative[ps_data$numbertypo == 1,] <- gsub(i, "", ps_data$narrative[ps_data$numbertypo == 1,])
}
# CLEAN UP COMMON TYPOS THAT MAY AFFECT OUR KEYWORD SEARCHES
ps_data$narrative <- gsub("ag(a)*( )*(in)*st", "against", ps_data$narrative)

##################################################################################################
# CLEAN UP OTHER VARIABLES 

ps_data[, "X"] = factor(ifelse(ps_data[, "X"] == 1, "YES", "NO"))
names(ps_data)[names(ps_data) == "X"] = "PS"

# How to destring a variable
ps_data[,grep("numberofemployees", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("numberofemployees", names(ps_data))] = as.numeric(ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = as.numeric(ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("averagemineheight", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = as.numeric(ps_data[,grep("averagemineheight", names(ps_data))])

# Merge redundant 'not-found' values within variables. Note: Values like "Unknown" or "Other" are not funneled into "No Value Found"
ps_data[, "uglocation"] = ifelse(ps_data[, "uglocation"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "uglocation"])
ps_data[, "accidenttype"] = ifelse(ps_data[, "accidenttype"] == "not elsewhereclassified", "no value found", ps_data[, "accidenttype"])
ps_data[, "immediatenotificationclass"] = ifelse(ps_data[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "immediatenotificationclass"])
ps_data[, "natureofinjury"] = ifelse(ps_data[, "natureofinjury"] == "unclassified,not determed", "no value found", ps_data[, "natureofinjury"])
ps_data[, "equipmanufacturer"] = ifelse(ps_data[, "equipmanufacturer"] == "Not Reported", "NO VALUE FOUND", ps_data[, "equipmanufacturer"])

# Convert date variables. We drop date variables now, but eventually will make use of them. Nikhil 4/18/16
indices_with_date = grep("date", names(ps_data))
for (i in indices_with_date) {
  ps_data[,i] = as.Date(ps_data[,i], "%m/%d/%Y")
}

##################################################################################################
# MERGE IN OTHER VARIABLES FROM PROTO-ALGORITHM (LIKE KEYWORD FLAGS) CHANGE TO KEEP ANY OTHER ADDTL VARS

ps_data = merge(ps_data, read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/training_set_1_internal.csv"), c("mineid", "documentno"))
ps_data = ps_data[, c(-grep("\\.y", names(ps_data)))]
names(ps_data) = gsub("\\.[x|y]", "", names(ps_data))

##################################################################################################
# GENERATE LIKELY KEYWORDS

# Pin but not pinion, pinner, pinning top, or pinned himself
ps_data[, "pin"] = ifelse(grepl("(^| )pin(n*)(e|i)[a-z]+", ps_data[,"narrative"]) &
                         !grepl("(^| )pinion", ps_data[,"narrative"]) &
                         !grepl("(^| )pinner", ps_data[,"narrative"]) &
                         !grepl("pinn(ing|ed)( ).{1,5}top", ps_data[,"narrative"]) &
                         !grepl("pinn(ing|ed).{1,5}(him|her)self", ps_data[,"narrative"]), 1, 0)
ps_data[, "strike"] = ifelse(grepl("str(i|u)(.*)k[a-z]*", ps_data[,"narrative"]) &
                            !grepl("str(i|u)(.*)k[a-z]*.{1,6}head", ps_data[,"narrative"]) &
                            !grepl("head.{1,6}str(i|u)(.*)k[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "strikerib"] = ifelse(grepl("str(i|u)(.*)k[a-z]*.{1,15} rib", ps_data[,"narrative"]), 1, 0)
ps_data[, "trap"] = ifelse(grepl("( )trap[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "collided"] = ifelse(grepl("collided", ps_data[,"narrative"]), 1, 0)
# DRILLING STEEL AND FINGER BEING PINNED: drill steel breaks = NOT PS & drill steel entangles gloves = NOT PS		
ps_data[, "drillsteel"] = ifelse((grepl("drill.{1,5}steel", ps_data[,"narrative"]) & 
                                     grepl("(between|btwn).{1,17}steel.{1,25}(drill|head|roof|guide|canopy|ring)+", ps_data[,"narrative"])) &
                                    !grepl("drill.{1,5}steel.{1,15}(broke|burst|bent|bend|break|loose|drop out|dropped out|fell|stuck|clogged)+", ps_data[,"narrative"]) &
                                    !grepl("drill.{1,5}steel.{1,10}caught.{1,10}glove", ps_data[,"narrative"]) &
                                    !grepl("glove.{1,10}caught.{1,10}drill.{1,5}steel", ps_data[,"narrative"]), 1, 0)

# GENERATE MAYBE LIKELY KEYWORDS

ps_data[, "ranover"] = ifelse(grepl("( |^)r(a|u)n( )*(over|into)", ps_data[,"narrative"]), 1, 0)
ps_data[, "rolled"] = ifelse(grepl("rolled( )*(over|into|onto)", ps_data[,"narrative"]), 1, 0)
ps_data[, "between"] = ifelse(grepl("between", ps_data[,"narrative"]) | grepl("btwn", ps_data[,"narrative"]), 1, 0)
ps_data[, "wheel"] = ifelse(grepl("wheel", ps_data[,"narrative"]) & !grepl("wheeler", ps_data[,"narrative"]), 1, 0)
ps_data[, "by"] = ifelse(grepl("by", ps_data[,"narrative"]), 1, 0)

#ps_data[, "bump"] = ifelse(grepl("bump(ing|ed)", ps_data[,"narrative"]), 1, 0)

# GENERATE NEGATIVE KEYWORDS

ps_data[, "brakes"] = ifelse(grepl("brakes.{1,10}(off|lost|not engage|did not|were not|fail)", ps_data[,"narrative"]) |
                               grepl("lost.{1,10}brakes", ps_data[,"narrative"]), 1, 0)
# jarred, jolted, jostled
ps_data[, "jarring"] = ifelse(grepl("jar(r)*(ed|ing)", ps_data[,"narrative"]) |
                                grepl("jo(lt|stl)(ed|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "bounced"] = ifelse(grepl("boun(c)*( )*(e|ing)", ps_data[,"narrative"]), 1, 0)
# avoid sprocket, rockduster, etc
ps_data[, "rock"] = ifelse(grepl("rock( |$|\\.|s|,)", ps_data[,"narrative"]), 1, 0)
ps_data[, "digit"] = ifelse(grepl("(finger(s)*|pinky|hand(s)*|thumb|hand( |\\.|,|$))", ps_data[,"narrative"]), 1, 0)
ps_data[, "derail"] = ifelse(grepl("(left|off|jumped).{1,15}track", ps_data[,"narrative"]) | grepl("derai", ps_data[,"narrative"]), 1, 0)

# GENERATE LESS GOOD NEGATIVE KEYWORDS
ps_data[, "roofbolt"] = ifelse(grepl("(roof|rib)( )*bolt", ps_data[,"narrative"]), 1, 0)
ps_data[, "driving"] = ifelse(grepl("was.{1,5}driv(e|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "operating"] = ifelse(grepl("operating", ps_data[,"narrative"]), 1, 0)
ps_data[, "riding"] = ifelse(grepl("was.{1,5}rid(e|ing)( )*in(side)*", ps_data[,"narrative"]), 1, 0)
ps_data[, "passenger"] = ifelse(grepl("passenger", ps_data[,"narrative"]), 1, 0)
ps_data[, "wrench"] = ifelse(grepl("wrench", ps_data[,"narrative"]), 1, 0)
ps_data[, "controls"] = ifelse(grepl("(lever|stick)", ps_data[,"narrative"]), 1, 0)
ps_data[, "resin"] = ifelse(grepl("resin", ps_data[,"narrative"]), 1, 0)
ps_data[, "atrs"] = ifelse(grepl("a(\\.)*t(\\.)*r(\\.)*s(\\.)*", ps_data[,"narrative"]), 1, 0)
ps_data[, "flew"] = ifelse(grepl("fl(ew|y|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "loose"] = ifelse(grepl("loose", ps_data[,"narrative"]), 1, 0)
ps_data[, "broke"] = ifelse(grepl("broke", ps_data[,"narrative"]), 1, 0)
ps_data[, "canopy"] = ifelse(grepl("canopy", ps_data[,"narrative"]), 1, 0)
ps_data[, "drill_steel_simple"] = ifelse(grepl("drill.{1,5}steel", ps_data[,"narrative"]), 1, 0)

##################################################################################################
# GENERATE KEYWORDS TO IDENTIFY FALSE POSITIVE ACCIDENTS BY CIRUMSTANCES

# USE BODY/SEAT TO REMOVE FALSE POSITIVE ACCIDENTS OF SOMEONE BEING JOSTLED AGAINST THE SEAT
ps_data[, "bodyseat"] = ifelse(grepl("(back|head|neck).{1,10}seat", ps_data[,"narrative"]) &
                                 !grepl("backward.{1,10}seat", ps_data[,"narrative"]) &
                                 !grepl("(bolt|over|drill)( )*head.{1,10}seat", ps_data[,"narrative"]), 1, 0) 
# USE HEAD/ROOF TO REMOVE DRIVER HITTING HEAD AGAINST VEHICLE ROOF
ps_data[, "headroof"] = ifelse((grepl("(head|neck).{1,5}(on|struck|hit|against)+.{1,5}roof", ps_data[,"narrative"]) |
                                grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(roof|top)", ps_data[,"narrative"])) &
                                 !grepl("drill( )*head.{1,10}roof", ps_data[,"narrative"]) &
                                 !grepl("over( )*head.{1,10}roof", ps_data[,"narrative"]) &
                                 !grepl("head(ing|er|ed).{1,10}roof", ps_data[,"narrative"]) &
                                 !grepl("head.{1,10}roof.{1,5}bolt", ps_data[,"narrative"]), 1, 0) 
# GOING OVER A BUMP AND OPERATOR HITTING HEAD 
ps_data[, "hole"] = ifelse(grepl("(hit|strike|ran over|struck|went over).{1,10}(rock|hole|bump(s| |$|\\.|,)|dip|depression|low spot)", ps_data[,"narrative"]), 1, 0)

##################################################################################################
# CREATE DUPLICATE NARRATIVE FIELDS AND THEN REPLACE ALL MENTIONS OF VEHICLES WITH "VEHICLE", BODY PARTS WITH "BODY", ETC.

ps_data$old_narrative <- ps_data$narrative

# VEHICLE
ps_data$narrative <- gsub("(man|ram|shuttle)( |-|- |v)*(trip|car)", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("( |^)car( |\\.|,|$)", " VEHICLE ", ps_data$narrative)
ps_data$narrative <- gsub("(m|a)(m|a)n( |-|- )*bus", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("( |^)bus( |\\.|,|$)", " VEHICLE ", ps_data$narrative)
ps_data$narrative <- gsub("vehic(l|e)(l|e)", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("person(n)*(e|a)l carrier", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("wheeler", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("trolley", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("motor", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("scooter", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("cricket ", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("(rock|roof)*( |-)*bolt(er|ing)", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("( |^)truck", " VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("buggy", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("mac( |-)*(8|eight)", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("(3|three)( |-)*wh(ee)*l(e)*r", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("cont.{1,10}min(r|er|ing)", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("long( |-)*wall", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("load( |-)haul(-| )dump( |-)", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("(mining|miner|minr|loading) machine", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("tunnel( |-)*borer", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("fork( |-)*lift", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("(front( |-)*end|scraper) loader", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("locotive", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("(road|motor)( |-)*grader", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("tractor", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("jeep", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("(ore)*( |-)haul(er|age)", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("rail( |-)*runner", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("feeder", "VEHICLE", ps_data$narrative)
ps_data$narrative <- gsub("porta( |-)*bus", "VEHICLE", ps_data$narrative)
ps_data[!grepl("to trip", ps_data[,"narrative"]),]$narrative <- gsub("( |^)trip( |$|,|\\.)", " VEHICLE ", ps_data[!grepl("to trip", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("scoop(er|ing)", ps_data[,"narrative"]),]$narrative <- gsub("scoop", " VEHICLE ", ps_data[!grepl("scoop(er|ing)", ps_data[,"narrative"]),]$narrative)

# BODY PARTS
ps_data$narrative <- gsub("hand(s| |\\.|,|$)", "BODY ", ps_data$narrative)
ps_data$narrative <- gsub("finger(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("thumb", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("feet", "BODY", ps_data$narrative) # MAYBE DO SOMETHING FOR UNIT OF MEASUREMENT
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
ps_data$narrative <- gsub("(^| |fore|for)arm", " BODY", ps_data$narrative)
ps_data$narrative <- gsub("mouth", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("pelvis", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("chest", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("tibia", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("fibia", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("spine", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("elbow(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("testicle(s)*", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("t(ee|oo)th", "BODY", ps_data$narrative) # CHECK TO MAKE SURE NO EQUIPMENT HAS TEETH/TOOTH
ps_data$narrative <- gsub("(right|left|his|her|both|onto)( )*(foot|feet)", "BODY", ps_data$narrative)
ps_data$narrative <- gsub("( |^)hip(s)*", " BODY", ps_data$narrative)
ps_data[!grepl("backward", ps_data[,"narrative"]),]$narrative <- gsub("(lower|PERSON|the)( )*back", " BODY", ps_data[!grepl("backward", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("drill.{1,5}head", ps_data[,"narrative"]) & !grepl("head(ing|er|ed)", ps_data[,"narrative"]) 
        & !grepl("over( )*head", ps_data[,"narrative"]),]$narrative <- gsub("(^| )head( |$|\\.|,)", " BODY ",
                                                                            ps_data[!grepl("drill.{1,5}head", ps_data[,"narrative"]) & !grepl("head(ing|er|ed)", ps_data[,"narrative"]) & !grepl("over( )*head", ps_data[,"narrative"]),]$narrative)
ps_data[!grepl("(coal|the).{1,5}face", ps_data[,"narrative"]),]$narrative <- gsub("face", "BODY", ps_data[!grepl("(coal|the).{1,5}face", ps_data[,"narrative"]),]$narrative)

#GENERATE SOME POSITIEV KEYWORDS USING THE BODY PARTS, BEFORE SUBBING IN THE PIN/STRIKE/TRAP 
ps_data[, "bumped"] = ifelse((grepl("bump(ed|ing)( )*(over|into|onto)", ps_data[,"narrative"]) | 
                                grepl("bump(ed|ing).{1,10}BODY", ps_data[,"narrative"])) &
                               !grepl("bump(ed|ing).{1,10}head", ps_data[,"old_narrative"]), 1, 0)
ps_data[, "caught"] = ifelse(grepl("caught.{1,15}(between| in )", ps_data[,"old_narrative"]) |
                               grepl("caught.{1,10}BODY", ps_data[,"narrative"]) |
                               grepl("BODY.{1,6}caught", ps_data[,"narrative"]), 1, 0)
ps_data[, "hit"] = ifelse(grepl("( |^)hit.{1,5}(by|him|his|her|employee|ee)", ps_data[,"old_narrative"]) |
                            grepl("( |^)hit.{1,10}BODY", ps_data[,"narrative"]), 1, 0)
ps_data[, "dropped"] = ifelse(grepl("(lower(ing|ed)*|drop(p)*(ing|ed)*).{1,15}(bucket|drill( |-)*head|drill( |-)*pod|pinner( |-)*head).{1,15}BODY", ps_data[,"old_narrative"]), 1, 0)

# PIN/STRIKE/TRAP

ps_data$narrative <- gsub("( |^)pin(n)*(ed|ing)", " PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("(s)*tr(u|i)(c)*k(e|ing)", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("r(a|u)n( )*(into|over)", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("col(l)*ided( w| with)*", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("( |^)trap(p)*(ed|ing)", " PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("col(l)*ided( w| with)*", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("rolled (into|onto|over)", "PINNED/STRUCK", ps_data$narrative)
ps_data$narrative <- gsub("c(a|u)(a|u)ght", "PINNED/STRUCK", ps_data$narrative)
ps_data[ps_data$hole == 0,]$narrative <- gsub("( |^)hit(t)*(ing)*( |$|\\.|,|s)", "PINNED/STRUCK", 
          ps_data[ps_data$hole == 0,]$narrative)

# PERSON FLAGS
ps_data$narrative <- gsub("( |^)e(e|mp|mpl|mployee)( |$|,|\\.)", " PERSON ", ps_data$narrative)
ps_data$narrative <- gsub("( |^)i(,|\\.| )*name", " PERSON", ps_data$narrative)
ps_data$narrative <- gsub("injured person", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("(him|her)self", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("vi(c)*tim", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("repairm(a|e)n", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("maintenance( )*m(a|e)n", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("( |^)m(a|e)n( |$|,|\\.)", " PERSON ", ps_data$narrative)
ps_data$narrative <- gsub("forem(a|e)n", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("ind(i)*v(idual)*(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("helper(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("person(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("worker(s)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("^inj(\\.)*(ured)*", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("the.{1,6}injured", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("injured was", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("( |^)(s)*he(r|rs)*( |$|,|\\.)", " PERSON ", ps_data$narrative)
ps_data$narrative <- gsub("( |^)hi(s|m)( |$|,|\\.)", " PERSON ", ps_data$narrative)

# these are less likely
ps_data$narrative <- gsub("operat(o|e)r", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("passenger", "PERSON", ps_data$narrative)
ps_data$narrative <- gsub("driver", "PERSON", ps_data$narrative)

##################################################################################################
# CREATE A FEW MORE KEYWORDS ON THE NEW NARRATIVE FIELDS, AND GENERATE VARS TO COUNT # OF UPPERCASE WORDS AND DISTANCES

# Count the number of capital words in each string
ps_data$num.vehicles <- str_count(ps_data$narrative, "VEHICLE")
ps_data$num.pinstrike <- str_count(ps_data$narrative, "PINNED/STRUCK")
ps_data$num.person <- str_count(ps_data$narrative, "PERSON")
ps_data$num.body <- str_count(ps_data$narrative, "BODY")

ps_data[, "no_vehcl"] = ifelse(!grepl("VEHICLE", ps_data[, "narrative"]), 1, 0)
ps_data[, "v_to_v"] = ifelse(!grepl("(VEHICLE|drill|steel|bolter|shear|cutter|tire).{1,20}PINNED/STRUCK.{1,20}(VEHICLE|drill|steel|bolter|shear|cutter|tire)", ps_data[, "narrative"]), 1, 0)
ps_data[, "v_to_p"] = ifelse(!grepl("VEHICLE.{1,35}PINNED/STRUCK.{1,35}(PERSON|BODY)", ps_data[, "narrative"]), 1, 0)
ps_data[, "loose_rbolting"] = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps_data[,"narrative"]), 1, 0)
ps_data[, "drill_action"] = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps_data[,"narrative"]), 1, 0)

# OPERATOR ARM OF HAND TRAILING OUTSIDE VEHICLE
ps_data[, "outsidevehicle"] = ifelse(((grepl("BODY.{1,15}(resting| hanging).{1,5}(over|out|on)", ps_data[,"narrative"]) & grepl("VEHICLE", ps_data[,"narrative"])) |
                                     grepl("BODY.{1,15}out( )*side.{1,30}VEHICLE", ps_data[,"narrative"])) &
                                    !grepl("overhang", ps_data[,"narrative"]), 1, 0)

ps_data[, "vcomp_test"] = ifelse(grepl("(seat|rail|canopy|battery|drill|steel|chain|cable)+.{1,20}VEHICLE", ps_data[,"narrative"]) | grepl("VEHICLE.{1,20}(seat|rail|canopy|battery|drill|steel|chain|cable)+", ps_data[,"narrative"]), 1, 0)
ps_data[, "psobject_test"] = ifelse(grepl("(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+.{1,20}PINNED/STRUCK", ps_data[,"narrative"]) | grepl("PINNED/STRUCK.{1,20}(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+", ps_data[,"narrative"]), 1, 0)


##################################################################################################
# GENERATE KEYWORD FLAGS

ps_data$keyword = ifelse((ps_data$pin == 1 | ps_data$strike == 1 |  ps_data$strikerib == 1 | ps_data$drillsteel == 1 |
                            ps_data$trap == 1 | ps_data$collided == 1 | ps_data$hit == 1 | ps_data$dropped = 1 |
                            ps_data$ranover == 1 | ps_data$bumped == 1 | ps_data$caught == 1 |
                            ps_data$rolled == 1 | ps_data$between == 1 | ps_data$wheel == 1), 1, 0)
ps_data$false_keyword = ifelse((ps_data$brakes == 1 | ps_data$jarring == 1 | ps_data$outsidevehicle == 1 |
                                  ps_data$bounced == 1 | ps_data$rock == 1 |
                                  ps_data$digit == 1 | ps_data$derail == 1 |
                                  ps_data$bodyseat == 1 | ps_data$headroof == 1 |
                                  ps_data$hole == 1), 1, 0)
ps_data$maybe_false_keyword = ifelse((ps_data$roofbolt == 1 | ps_data$driving == 1 |
                                        ps_data$operating == 1 | ps_data$riding == 1 |
                                        ps_data$passenger == 1 | ps_data$wrench == 1 |
                                        ps_data$controls == 1 | ps_data$resin == 1 |
                                        ps_data$atrs == 1 | ps_data$flew == 1 |
                                        ps_data$loose == 1 | ps_data$broke == 1 | 
                                        ps_data$canopy == 1), 1, 0)

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
ps_data$maybs_type = ifelse((ps_data$accidenttype == "acc type, without injuries" |
                                   ps_data$accidenttype == "struck against stationary obj" |
                                   ps_data$accidenttype == "fall frm mach, vehicle, equip" |
                                   ps_data$accidenttype == "struck by falling object" |
                                   ps_data$accidenttype == "struck by flying object" |
                                   ps_data$accidenttype == "no value found" |
                                   ps_data$accidenttype == "not elsewhere classified") & ps_data$false_keyword == 0, 1, 0)
# GENERATE LIKELY EQUIPMENT

ps_data[, "moving_vehcl"] = ifelse(!(ps_data$equiptypecode %in% vehcl_equip_codes), 1, 0)

ps_data$likely_equip = ifelse((ps_data$equiptypecode == "12" |  ps_data$equiptypecode == "23" |
                                 ps_data$equiptypecode == "33" |  ps_data$equiptypecode == "34" |
                                 ps_data$equiptypecode == "35" |  ps_data$equiptypecode == "37" |
                                 ps_data$equiptypecode == "41" |  ps_data$equiptypecode == "44" |
                                 ps_data$equiptypecode == "60" |  ps_data$equiptypecode == "61" |
                                 ps_data$equiptypecode == "66" |  ps_data$equiptypecode == "67") & ps_data$false_keyword == 0, 1, 0)
ps_data$unlikely_equip = ifelse((ps_data$equiptypecode == "06" |  ps_data$equiptypecode == "09" |
                                   ps_data$equiptypecode == "15" |  ps_data$equiptypecode == "16" |
                                   ps_data$equiptypecode == "20" |  ps_data$equiptypecode == "28" |
                                   ps_data$equiptypecode == "29" |  ps_data$equiptypecode == "53" |
                                   ps_data$equiptypecode == "55"), 1, 0)
ps_data$uncertain_equip = ifelse((ps_data$equiptypecode == "?" |  ps_data$equiptypecode == "54" |
                                  ps_data$equiptypecode == "71" |  ps_data$equiptypecode == "25" |
                                  ps_data$equiptypecode == "13" |  ps_data$equiptypecode == "14"), 1, 0)
# GENERATE LIKELY SOURCES

ps_data$likely_source = ifelse((ps_data$injurysourcecode == "074" | ps_data$injurysourcecode == "077" | 
                               ps_data$injurysourcecode == "081" | ps_data$injurysourcecode == "087" | 
                               ps_data$injurysourcecode == "104" | ps_data$injurysourcecode == "105" |
                               ps_data$injurysourcecode == "106" | ps_data$injurysourcecode == "107" | 
                               ps_data$injurysourcecode == "108" | ps_data$injurysourcecode == "110") & ps_data$false_keyword == 0, 1, 0)
ps_data$unlikely_source = ifelse((ps_data$injurysourcecode == "003" | ps_data$injurysourcecode == "004" | 
                                 ps_data$injurysourcecode == "006" | ps_data$injurysourcecode == "007" | 
                                 ps_data$injurysourcecode == "008" | ps_data$injurysourcecode == "009" |
                                 ps_data$injurysourcecode == "012" | ps_data$injurysourcecode == "051" | 
                                 ps_data$injurysourcecode == "057" | ps_data$injurysourcecode == "063" | 
                                 ps_data$injurysourcecode == "067" | ps_data$injurysourcecode == "068" |
                                 ps_data$injurysourcecode == "078" | ps_data$injurysourcecode == "079" | 
                                 ps_data$injurysourcecode == "080" | ps_data$injurysourcecode == "083" | 
                                 ps_data$injurysourcecode == "084" | ps_data$injurysourcecode == "089" |
                                 ps_data$injurysourcecode == "090" | ps_data$injurysourcecode == "092" | 
                                 ps_data$injurysourcecode == "093" | ps_data$injurysourcecode == "096" | 
                                 ps_data$injurysourcecode == "098" | ps_data$injurysourcecode == "112" |
                                 ps_data$injurysourcecode == "116" | ps_data$injurysourcecode == "125"), 1, 0)
ps_data$uncertain_source = ifelse((ps_data$likely_source == 0 & ps_data$unlikely_source == 0), 1, 0)

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

ps_data[, "likely_actvty"] = ifelse((grepl("operate", ps_data$mineractivity) | grepl("roof", ps_data$mineractivity)), 1, 0)
ps_data[, "maybe_likely_actvty"] = ifelse(grepl("move/reel", ps_data$mineractivity) | grepl("handling supplies/materials", ps_data$mineractivity), 1, 0)

ps_data$unlikely_activity = ifelse((ps_data$activitycode == "009" | ps_data$activitycode == "016" | 
                                    ps_data$activitycode == "020" | ps_data$activitycode == "022" | 
                                    ps_data$activitycode == "025" | ps_data$activitycode == "026" |
                                    ps_data$activitycode == "027" | ps_data$activitycode == "029" | 
                                    ps_data$activitycode == "030" | ps_data$activitycode == "032" | 
                                    ps_data$activitycode == "034" | ps_data$activitycode == "036" |
                                    ps_data$activitycode == "075" | ps_data$activitycode == "066" | 
                                    ps_data$activitycode == "065" | ps_data$activitycode == "056"), 1, 0)
#Not sure this is mutually exclusive AND exhaustive
ps_data$uncertain_activity = ifelse((ps_data$likely_actvty == 0 & ps_data$maybe_likely_actvty == 0 & ps_data$unlikely_activity == 0), 1, 0)

# GENERATE LIKELY OCCUPATIONS

ps_data$likely_occup = ifelse((ps_data$occupcode3digit == "050" | ps_data$occupcode3digit == "046" | 
                                 ps_data$occupcode3digit == "028" | ps_data$occupcode3digit == "016" | 
                                 ps_data$occupcode3digit == "036"), 1, 0)

# GENERATE LIKELY BODY PARTS

ps_data$unlikely_body = ifelse((ps_data$bodypartcode == "200" | ps_data$bodypartcode == "340" | 
                                ps_data$bodypartcode == "420"), 1, 0)

# GENERATE LIKELY PINNING AND STRIKING ACCIDENT FLAG
ps_data$potential_ps = ifelse((ps_data$keyword == 1 | ps_data$likely_class == 1 | ps_data$accidenttype == 1), 1, 0)

# GENERATE LIKELY CIRCUMSTANCES

ps_data$falling.class = ifelse(ps_data$accidentclassification == "fall of roof or back", 1, 0)
ps_data[, "falling.word"] = ifelse(grepl("rock( )*fell", ps_data[,"narrative"]) |
                                   grepl("fell.{1,20}roof", ps_data[,"narrative"]) |
                                   grepl("roof( )*f(a|e)ll", ps_data[,"narrative"]), 1, 0)
ps_data$falling.accident = ifelse(ps_data$falling.class == 1 | ps_data$falling.word == 1, 1, 0)
ps_data = ps_data[, c(-match("falling.class", names(ps_data)), -match("falling.word", names(ps_data)))]

ps_data$accident.only = ifelse( (ps_data$degreeofinjury == "accident only" | ps_data$accidenttypecode == 44), 1, 0)

##################################################################################################
# SIMPLE ALGORITHM
ps_data[, "holistic"] = ifelse((((ps_data$accidenttypecode %in% likely_acc_type) | (ps_data$accidenttypecode %in% maybe_likely_acc_type)) 
                                  & (ps_data$likely_actvty == 1 | ps_data$maybe_likely_actvty == 1) & (ps_data$accidentclassification %in% likely_classfctn) 
                                  & ps_data$moving_vehcl == 1), 1, 0)

# DUMMY-OUT FACTOR VARS WITH TOO MANY VALUES FOR RANDOM FOREST - MAYBE BETTER THAN THE ABOVE
datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}
test.data1 <- datdum(x="injurysourcecode",data=ps_data,name="injurysourcecode")
test.data1 <- test.data1 [, c(grep("injurysourcecode", names(test.data1)))]
test.data2 <- datdum(x="accidentclassification",data=ps_data,name="accidentclassification")
test.data2 <- test.data2 [, c(grep("accidentclassification", names(test.data2)))]
test.data3 <- datdum(x="accidenttype",data=ps_data,name="accidenttype")
test.data3 <- test.data3 [, c(grep("accidenttype", names(test.data3)))]
test.data4 <- datdum(x="equiptypecode",data=ps_data,name="equiptypecode")
test.data4 <- test.data4 [, c(grep("equiptypecode", names(test.data4)))]
test.data5 <- datdum(x="natureofinjury",data=ps_data,name="natureofinjury")
test.data5 <- test.data5 [, c(grep("natureofinjury", names(test.data5)))]
test.data6 <- datdum(x="activitycode",data=ps_data,name="activitycode")
test.data6 <- test.data6 [, c(grep("activitycode", names(test.data6)))]
test.data7 <- datdum(x="occupcode3digit",data=ps_data,name="occupcode3digit")
test.data7 <- test.data7 [, c(grep("occupcode3digit", names(test.data7)))]
test.data8 <- datdum(x="bodypartcode",data=ps_data,name="bodypartcode")
test.data8 <- test.data8 [, c(grep("bodypartcode", names(test.data8)))]
ps_data = cbind(ps_data, test.data1, test.data2, test.data3, test.data4, test.data5, test.data6, test.data7, test.data8)
rm(test.data1, test.data2, test.data3, test.data4, test.data5, test.data6, test.data7, test.data8)

##################################################################################################
#Drop variables with redundant or no information while keeping codes used in the algorithms below
ps_data = ps_data[, c(-match("primarycanvasscodedesc", names(ps_data)), -match("primarycanvasscode", names(ps_data)), -match("primarysicdesc", names(ps_data))
                      , -match("primarysiccode", names(ps_data)), -match("minetype", names(ps_data)), -match("coalcormetalmmine", names(ps_data))
                      , -match("primarysiccodegroup", names(ps_data)), -match("primarysiccodesuffix", names(ps_data))
                      , -match("immediatenotificationcode", names(ps_data)), -match("secondarysiccode", names(ps_data)), -match("secondarysicdesc", names(ps_data))
                      , -match("secondarysiccodegroup", names(ps_data)), -match("secondarysiccodesuffix", names(ps_data)), -match("secondarycanvasscode", names(ps_data))
                      , -match("secondarycanvasscodedesc", names(ps_data)), -match("minegascategorycode", names(ps_data)), -match("noofproducingpits", names(ps_data))
                      , -match("nooftailingponds", names(ps_data)), -match("roomandpillarindicator", names(ps_data)), -match("highwallminerindicator", names(ps_data))
                      , -match("multiplepitsindicator", names(ps_data)), -match("minersrepindicator", names(ps_data))
                      , -match("injurysourcecode", names(ps_data)), -match("natureofinjurycode", names(ps_data)), -match("subunitcode", names(ps_data))
                      , -match("degreeofinjurycode", names(ps_data)), -match("uglocationcode", names(ps_data)), -match("ugminingmethodcode", names(ps_data)), -match("classificationcode", names(ps_data))
                      , -match("accidenttypecode", names(ps_data)), -match("inj_degr_cd_old", names(ps_data)), -match("dup", names(ps_data)), -match("changed", names(ps_data))
                      , -match("narrativemodified", names(ps_data)), -match("narrative_old", names(ps_data)), -match("equipment", names(ps_data)), -match("date_old", names(ps_data))
                      , -match("date_new", names(ps_data)), -match("directionstominemodified", names(ps_data)), -grep("insample", names(ps_data))
                      , -grep("group", names(ps_data)), -grep("random", names(ps_data)))]

#Drop totally irrelevant variables, but keeping document number to reference prediction results with narrative fields downstream
ps_data = ps_data[, c( -match("accidenttime", names(ps_data)), -grep("date", names(ps_data))
                      , -match("longitude", names(ps_data)), -match("latitude", names(ps_data)), -match("bomstatecode", names(ps_data))
                      , -match("nearesttown", names(ps_data)), -match("narrative", names(ps_data)), -match("minestatus", names(ps_data))
                      , -match("minename", names(ps_data)))]

#Drop variables we are confused about
ps_data = ps_data[, c(-match("i", names(ps_data)), -match("idesc", names(ps_data)), -match("oldoccupationcode", names(ps_data))
                      , -match("equip", names(ps_data)), -match("ai_acty_cd_old", names(ps_data)), -match("ai_dt_old", names(ps_data))
                      , -match("ai_time_old", names(ps_data)))]

##################################################################################################
##Missing Value Imputation##

#We don't use date vars as of yet so no need to store a list of their names, "logical" class vars are missing all obsvtns
var_classes = sapply(ps_data[,names(ps_data)], class)
charac_vars = names(var_classes[c(grep("character", var_classes), grep("factor", var_classes))])
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])
ps_data = ps_data[, -grep("logical", var_classes)]

for (i in 1:length(charac_vars)) {
  ps_data[, charac_vars[i]] = ifelse((ps_data[,charac_vars[i]] == "NO VALUE FOUND" | ps_data[,charac_vars[i]] == "UNKNOWN" | 
                                        ps_data[,charac_vars[i]] == "no value found" | ps_data[,charac_vars[i]] == "unknown" | 
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

##################################################################################################
# PRODUCE DATASETS WITH ONLY VARS OF INTEREST FOR RF/BOOSTING ANALYSIS
simple.data.grouped = ps_data[, c(match("documentno", names(ps_data)), match("PS", names(ps_data)), match("pin", names(ps_data)),
                               match("strike", names(ps_data)), match("strikerib", names(ps_data)), grep("maybs_", names(ps_data)),
                               match("trap", names(ps_data)),  match("collided", names(ps_data)),
                               match("hit", names(ps_data)), match("ranover", names(ps_data)),
                               match("rolled", names(ps_data)), match("caught", names(ps_data)),
                               match("between", names(ps_data)), match("by", names(ps_data)),
                               match("brakes", names(ps_data)), match("jarring", names(ps_data)), 
                               match("bounced", names(ps_data)), match("rock", names(ps_data)),                                    
                               match("digit", names(ps_data)), match("derail", names(ps_data)),
                               match("roofbolt", names(ps_data)), match("driving", names(ps_data)),
                               match("operating", names(ps_data)), match("riding", names(ps_data)), match("drill_steel_simple", names(ps_data)),
                               match("passenger", names(ps_data)), match("wrench", names(ps_data)), 
                               match("controls", names(ps_data)), match("resin", names(ps_data)), 
                               match("atrs", names(ps_data)), match("flew", names(ps_data)),
                               match("loose", names(ps_data)), match("broke", names(ps_data)), 
                               match("canopy", names(ps_data)), match("bodyseat", names(ps_data)), 
                               match("headroof", names(ps_data)), match("hole", names(ps_data)), 
                               match("drillsteel", names(ps_data)), match("outsidevehicle", names(ps_data)), 
                               match("keyword", names(ps_data)), match("false_keyword", names(ps_data)), 
                               match("maybe_false_keyword", names(ps_data)), match("dropped", names(ps_data)), 
                               match("v_to_v", names(ps_data)), match("v_to_p", names(ps_data)),                                    
                               match("no_vehcl", names(ps_data)), match("bumped", names(ps_data)),
                               match("num.vehicles", names(ps_data)), 
                               match("num.pinstrike", names(ps_data)), 
                               match("num.person", names(ps_data)), 
                               match("num.body", names(ps_data)), 
                               match("vcomp_test", names(ps_data)), match("psobject_test", names(ps_data)), 
                               match("loose_rbolting", names(ps_data)), match("drill_action", names(ps_data)),
                                   match("likely_equip", names(ps_data)), match("unlikely_equip", names(ps_data)),
                                   match("likely_class", names(ps_data)), match("unlikely_class", names(ps_data)),
                                   match("likely_type", names(ps_data)), match("unlikely_type", names(ps_data)),
                                   match("unlikely_nature", names(ps_data)), match("likely_source", names(ps_data)),                          
                                   match("unlikely_source", names(ps_data)), match("likely_actvty", names(ps_data)), 
                                   match("unlikely_activity", names(ps_data)), match("uncertain_activity", names(ps_data)),
                                   match("uncertain_class", names(ps_data)),
                                   match("unlikely_body", names(ps_data)), match("uncertain_type", names(ps_data)),
                                   match("uncertain_equip", names(ps_data)), match("uncertain_source", names(ps_data)),
                                   match("uncertain_nature", names(ps_data)),
                                   match("falling.accident", names(ps_data)), match("accident.only", names(ps_data)))]

#write.csv(simple.data.grouped, file = "C:/Users/nsaifull/Dropbox/R-Code/prepped_PS_training_data_grouped.csv", row.names = FALSE)

##################################################################################################
# ALGORITHM

# RANDOMLY SORT DATA (IT WAS ORDERED IN STATA BEFORE THIS)
#Best one so far is "grouped2" dataset
simple.data = simple.data.grouped
set.seed(625)
rand <- runif(nrow(simple.data))
simple.ps <- simple.data[order(rand),]
remove(rand)
# just to find out which col # PS is
which( colnames(simple.ps)=="PS" )

# CART
cart <- rpart(PS ~ . -documentno, data = simple.ps[1:600,], method="class")
cart 

# RANDOM FOREST
rf <- randomForest(PS ~ . -documentno, data = simple.ps[1:600,], mtry = 8, importance=TRUE, type="class",
                   ntree = 200)
rf

# BOOSTING
ps.adaboost = boosting(PS ~ . -documentno, data = simple.ps[1:600,], boos = T, mfinal = 100, coeflearn = 'Freund')
adaboost.pred = predict.boosting(ps.adaboost, newdata = simple.ps[601:1000,])

##################################################################################################
# MODEL PERFORMANCE

cart.predictions = predict(cart, simple.ps[601:1000,],type="class")
table(simple.ps[601:1000,2], predicted = cart.predictions)

rf.predictions = predict(rf, simple.ps[601:1000,],type="class")
table(simple.ps[601:1000,2], predicted = rf.predictions)

adaboost.pred$confusion


##################################################################################################
# COMPOSITE ALGORITHM

#splitIndex = createDataPartition(simple.ps$PS, p =.50, list = FALSE, times = 1)
#smote.trainx = simple.ps[splitIndex,]
#smote.test = simple.ps[-splitIndex,]
#prop.table(table(smote.trainx$PS))
set.seed(625)
# CREATE UNIQUE ID FOR FUTURE MERGES, SEPARATE TRAINING AND TEST DATA
#simple.ps[, "unique_id"] = row(as.matrix(simple.ps[,1]))
smote.trainx = simple.ps[1:600,]
smote.test = simple.ps[601:1000,]

# STEP ONE: PRE-PROCESSING
# USE SMOTE TO OVERSAMPLE DATA
smote.ps <- SMOTE(PS ~ ., smote.trainx, perc.over = 600,perc.under=100)
table(smote.ps$PS)

#  WEED OUT OBS THAT ARE DEFINITELY NOT PS
smote.test[, "predict"] = ifelse((smote.test$accident.only == 0 & smote.test$falling.accident == 0), 1, 0)

# STEP TWO: MODEL
# NOW DO A RANDOM FOREST ON THE SMOTED DATA
rf.smote <- randomForest(PS ~ . -documentno, data = smote.ps, mtry = 15, ntree = 1000)
rf.smote

#PREDICT
rf.smote.pred = predict(rf.smote, smote.test[smote.test$predict == 1,], type="class")
table(smote.test[smote.test$predict == 1,]$PS, predicted = rf.smote.pred)

# MERGE ON PREDICTIONS
smote.test.aux = cbind(smote.test[smote.test$predict == 1,], rf.smote.pred)
post.smote.test = merge(smote.test, smote.test.aux, by = "documentno", all = T)
post.smote.test = post.smote.test[, c(-grep("\\.y", names(post.smote.test)))]
names(post.smote.test) = gsub("\\.[x|y]", "", names(post.smote.test))

post.smote.test[, "smote_pred"] = ifelse(is.na(post.smote.test$rf.smote.pred), 1, post.smote.test$rf.smote.pred)

# RUN BOOSTING ON OBSERVATIONS CLASSIFIED "NO" BY THE RANDOM FOREST
ps.adaboost = boosting(PS ~ . -documentno, data = smote.trainx, boos = T, mfinal = 1000, coeflearn = 'Freund')

adaboost.pred = predict.boosting(ps.adaboost, newdata = post.smote.test[post.smote.test$predict==1 & post.smote.test$rf.smote.pred=="NO",
                  c(-grep("rf.smote.pred",names(post.smote.test)), -grep("predict",names(post.smote.test)))])
adaboost.pred$confusion

# GENERATE VARIABLE WITH FINAL PREDICTIONS
boost.test.aux = cbind(post.smote.test[post.smote.test$predict == 1 & post.smote.test$rf.smote.pred == "NO",], adaboost.pred$class)
post.smote.test = merge(post.smote.test, boost.test.aux, by = "documentno", all = T)
post.smote.test = post.smote.test[, c(-grep("\\.y", names(post.smote.test)))]
names(post.smote.test) = gsub("\\.[x|y]", "", names(post.smote.test))

# LET'S TRACK DOWN WHY THERE ARE MISSING DOC NO'S 

post.smote.test[, "smote_pred"] = ifelse(post.smote.test$`adaboost.pred$class` == "YES" | post.smote.test$rf.smote.pred == "YES", "YES", "NO")
table(post.smote.test$smote_pred, post.smote.test$PS)

# STEP THREE - POST-PROCESSING

# THIS IS WHERE WE FILTER OUT FALSE POSITIVES


# BEST PREDICTION SO FAR
# 5/12/16 (mtry = 15)
#predicted
#NO YES
#NO  316  21
#YES  49  86

