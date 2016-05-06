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
ps_data[, "accidenttype"] = ifelse(ps_data[, "accidenttype"] == "not elsewhereclassified", "no value found", ps_data[, "accidenttype"])
ps_data[, "immediatenotificationclass"] = ifelse(ps_data[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "immediatenotificationclass"])
ps_data[, "natureofinjury"] = ifelse(ps_data[, "natureofinjury"] == "unclassified,not determed", "no value found", ps_data[, "natureofinjury"])
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

# GENERATE MAYBE LIKELY KEYWORDS

ps_data[, "hit"] = ifelse(grepl("( |^)hit( )*(by|him|his|her|employee|ee)", ps_data[,"narrative"]), 1, 0)
ps_data[, "between"] = ifelse(grepl("between", ps_data[,"narrative"]), 1, 0)
ps_data[, "ranover"] = ifelse(grepl("( |^)r(a|u)n( )*(over|into)", ps_data[,"narrative"]), 1, 0)
ps_data[, "rolled"] = ifelse(grepl("rolled( )*(over|into|onto)", ps_data[,"narrative"]), 1, 0)
ps_data[, "caught"] = ifelse(grepl("caught", ps_data[,"narrative"]), 1, 0)
ps_data[, "by"] = ifelse(grepl("by", ps_data[,"narrative"]), 1, 0)

# GENERATE NEGATIVE KEYWORDS

ps_data[, "brakes"] = ifelse(grepl("brakes.{1,10}(off|lost|not engage|did not|were not)", ps_data[,"narrative"]) |
                               grepl("lost.{1,10}brakes", ps_data[,"narrative"]), 1, 0)
ps_data[, "jarring"] = ifelse(grepl("jar(r)*(ed|ing)", ps_data[,"narrative"]) |
                                grepl("jolt(ed|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "bounced"] = ifelse(grepl("bounc(e|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "hole"] = ifelse(grepl("hit.{1,6}head", ps_data[,"narrative"]) |
                             grepl("head.{1,6}hit", ps_data[,"narrative"]) |
                             grepl("hit.{1,6}(rock|hole|bump)", ps_data[,"narrative"]), 1, 0)
ps_data[, "roofbolt"] = ifelse(grepl("(roof|rib)( )*bolt", ps_data[,"narrative"]), 1, 0)
# avoid sprocket, rockduster, etc
ps_data[, "rock"] = ifelse(grepl("rock( |$|\\.|s|,)", ps_data[,"narrative"]), 1, 0)

ps_data[, "driving"] = ifelse(grepl("was.{1,5}driv(e|ing)", ps_data[,"narrative"]), 1, 0)
ps_data[, "operating"] = ifelse(grepl("operating", ps_data[,"narrative"]), 1, 0)
ps_data[, "riding"] = ifelse(grepl("was.{1,5}rid(e|ing)( )*in(side)*", ps_data[,"narrative"]), 1, 0)
ps_data[, "passenger"] = ifelse(grepl("passenger", ps_data[,"narrative"]), 1, 0)

# GENERATE KEYWORD FLAGS

ps_data$keyword = ifelse( ps_data$pin == 1 | ps_data$strike == 1 |
                            ps_data$trap == 1 | ps_data$collided == 1 |
                            ps_data$hit == 1 | ps_data$ranover == 1 |
                            ps_data$rolled == 1 | ps_data$between == 1, 1, 0)
ps_data$false_keyword = ifelse( ps_data$jarring == 1 | ps_data$hole == 1 |
                                  ps_data$roofbolt == 1 | ps_data$rock == 1 |
                                  ps_data$driving == 1 | ps_data$riding == 1 |
                                  ps_data$passenger == 1 | ps_data$brakes == 1 |
                                  ps_data$bounce == 1, 1, 0)

# GENERATE LIKELY CLASSES

likely_classfctn = c("machinery", "powered haulage")

ps_data$likely_class = ifelse( ps_data$accidentclassification == "powered haulage" | 
                                 ps_data$accidentclassification == "machinery" |
                                 ps_data$accidentclassification == "striking or bumping", 1, 0)
ps_data$unlikely_class = ifelse( ps_data$accidentclassification == "disorders (repeated trauma)" | 
                                   ps_data$accidentclassification == "electrical" |
                                   ps_data$accidentclassification == "explosives and breaking agents" | 
                                   ps_data$accidentclassification == "stepping or kneeling on object" | 
                                   ps_data$accidentclassification == "ignition or explosion of gas or dust", 1, 0)
# GENERATE LIKELY TYPES

likely_acc_type = c(7, 6, 22, 2) 
maybe_likely_acc_type = c(1, 12, 20, 21, 24) 

ps_data$likely_type = ifelse( ps_data$accidenttype == "struck by, nec" | 
                                ps_data$accidenttype == "struck by powered moving obj" |
                                ps_data$accidenttype == "struck by rollng or slidng obj" |
                                ps_data$accidenttype == "handling of materials" |
                                ps_data$accidenttype == "struck against moving object" |
                                ps_data$accidenttype == "cght i, u, b, rnng, mshng objs" |
                                ps_data$accidenttype == "cght i, u, b, mvng & sttn objs" |
                                ps_data$accidenttype == "caught i, u, b, moving objects" |
                                ps_data$accidenttype == "cght in, under, or btween, nec" |
                                ps_data$accidenttype == "struck against moving object", 1, 0)
ps_data$unlikely_type = ifelse( ps_data$accidenttype == "fall from ladders" | 
                                  ps_data$accidenttype == "fall to lower level, nec" |
                                  ps_data$accidenttype == "fall to wlkway or wrkng surfc" |
                                  ps_data$accidenttype == "fall onto or against objects" |
                                  ps_data$accidenttype == "rubbed or abraded, nec" |
                                  ps_data$accidenttype == "bodily reaction, nec" |
                                  ps_data$accidenttype == "over-exertion in lifting objs" |
                                  ps_data$accidenttype == "ovr-exrtn in pllng, pshng objs" |
                                  ps_data$accidenttype == "ovrexrtn in wldng, thrwng objs" |
                                  ps_data$accidenttype == "contact with elctric current" |
                                  ps_data$accidenttype == "contct w/ hot objs or substanc" |
                                  ps_data$accidenttype == "absrtn rad caust txc & nox sbs" |
                                  ps_data$accidenttype == "flash burns (electric)" |
                                  ps_data$accidenttype == "over-exertion, nec", 1, 0)

# GENERATE LIKELY EQUIPMENT

vehcl_equip_codes = c("06", "13", "28", "53", "?")
ps_data[, "moving_vehcl"] = ifelse(!(ps_data$equiptypecode %in% vehcl_equip_codes), 1, 0)

ps_data$likely_equip = ifelse((ps_data$accidenttype == "12" |  ps_data$accidenttype == "23" |
                                 ps_data$accidenttype == "33" |  ps_data$accidenttype == "34" |
                                 ps_data$accidenttype == "35" |  ps_data$accidenttype == "37" |
                                 ps_data$accidenttype == "41" |  ps_data$accidenttype == "44" |
                                 ps_data$accidenttype == "60" |  ps_data$accidenttype == "61" |
                                 ps_data$accidenttype == "66" |  ps_data$accidenttype == "67"), 1, 0)
ps_data$unlikely_equip = ifelse((ps_data$accidenttype == "06" |  ps_data$accidenttype == "09" |
                                   ps_data$accidenttype == "15" |  ps_data$accidenttype == "16" |
                                   ps_data$accidenttype == "20" |  ps_data$accidenttype == "28" |
                                   ps_data$accidenttype == "29" |  ps_data$accidenttype == "53" |
                                   ps_data$accidenttype == "55"), 1, 0)

# GENERATE LIKELY SOURCES

ps_data$likely_source = ifelse(ps_data$injurysourcecode == "074" | 
                                 ps_data$injurysourcecode == "077" | 
                                 ps_data$injurysourcecode == "081" | 
                                 ps_data$injurysourcecode == "087" | 
                                 ps_data$injurysourcecode == "104" |
                                 ps_data$injurysourcecode == "105" |
                                 ps_data$injurysourcecode == "106" | 
                                 ps_data$injurysourcecode == "107" | 
                                 ps_data$injurysourcecode == "108" | 
                                 ps_data$injurysourcecode == "110", 1, 0)
ps_data$unlikely_source = ifelse(ps_data$injurysourcecode == "003" | 
                                   ps_data$injurysourcecode == "004" | 
                                   ps_data$injurysourcecode == "006" | 
                                   ps_data$injurysourcecode == "007" | 
                                   ps_data$injurysourcecode == "008" |
                                   ps_data$injurysourcecode == "009" |
                                   ps_data$injurysourcecode == "012" | 
                                   ps_data$injurysourcecode == "051" | 
                                   ps_data$injurysourcecode == "057" | 
                                   ps_data$injurysourcecode == "063" | 
                                   ps_data$injurysourcecode == "067" |
                                   ps_data$injurysourcecode == "068" |
                                   ps_data$injurysourcecode == "078" | 
                                   ps_data$injurysourcecode == "079" | 
                                   ps_data$injurysourcecode == "080" |   
                                   ps_data$injurysourcecode == "083" | 
                                   ps_data$injurysourcecode == "084" |
                                   ps_data$injurysourcecode == "089" |
                                   ps_data$injurysourcecode == "090" | 
                                   ps_data$injurysourcecode == "092" | 
                                   ps_data$injurysourcecode == "093" | 
                                   ps_data$injurysourcecode == "096" | 
                                   ps_data$injurysourcecode == "098" |
                                   ps_data$injurysourcecode == "112" |
                                   ps_data$injurysourcecode == "116" | 
                                   ps_data$injurysourcecode == "125", 1, 0)

# GENERATE LIKELY NATURES

ps_data$likely_nature = ifelse(ps_data$natureofinjury == "crushing", 1, 0)
ps_data$unlikely_nature = ifelse((ps_data$natureofinjury == "burn or scald (heat)" |
                                    ps_data$natureofinjury == "burn,chemicl-fume,compoun" |
                                    ps_data$natureofinjury == "elect shock,electrocution" |
                                    ps_data$natureofinjury == "hearing loss or impairmnt" |
                                    ps_data$natureofinjury == "dust in eyes" |
                                    ps_data$natureofinjury == "elect.arc burn-not contac"), 1, 0)

# GENERATE LIKELY ACTIVITIES

ps_data[, "likely_actvty"] = ifelse(grepl("operate", ps_data$mineractivity) | grepl("roof", ps_data$mineractivity), 1, 0)
ps_data[, "maybe_likely_actvty"] = ifelse(grepl("move/reel", ps_data$mineractivity) | grepl("handling supplies/materials", ps_data$mineractivity), 1, 0)

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

# GENERATE LIKELY OCCUPATIONS

ps_data$unlikely_occup = ifelse((ps_data$occupcode3digit == "050" | 
                                   ps_data$occupcode3digit == "046" | 
                                   ps_data$occupcode3digit == "028" | 
                                   ps_data$occupcode3digit == "016" | 
                                   ps_data$occupcode3digit == "036"), 1, 0)

# GENERATE LIKELY BODY PARTS

ps_data$unlikely_body = ifelse((ps_data$bodypartcode == "200" | 
                                  ps_data$bodypartcode == "340" | 
                                  ps_data$bodypartcode == "420"), 1, 0)

# GENERATE LIKELY CIRCUMSTANCES

ps_data$falling.class = ifelse(ps_data$accidentclassification == "fall of roof or back", 1, 0)
ps_data[, "falling.word"] = ifelse(grepl("rock( )*fell", ps_data[,"narrative"]) |
                                     grepl("fell.{1,20}roof", ps_data[,"narrative"]) |
                                     grepl("roof( )*f(a|e)ll", ps_data[,"narrative"]), 1, 0)
ps_data$falling.accident = ifelse(ps_data$falling.class == 1 | ps_data$falling.word == 1, 1, 0)
ps_data = ps_data[, c(-match("falling.class", names(ps_data)), -match("falling.word", names(ps_data)))]

ps_data$accident.only = ifelse( (ps_data$degreeofinjury == "accident only" | ps_data$accidenttypecode == 44), 1, 0)

# nikhil's simple algorithm
ps_data[, "holistic"] = ifelse((((ps_data$accidenttypecode %in% likely_acc_type) | (ps_data$accidenttypecode %in% maybe_likely_acc_type)) & (ps_data$likely_actvty == 1 | ps_data$maybe_likely_actvty == 1) & (ps_data$accidentclassification %in% likely_classfctn) & ps_data$moving_vehcl == 1), 1, 0)


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

##Dummying-Out Factors with more than 53 categories (An issue for RF analysis)

new_dummies = apply(cbind(dummy(ps_data$sourceofinjury), dummy(ps_data$occupation), dummy(ps_data$equipmentmodelno), dummy(ps_data$fipscountyname),
                          dummy(ps_data$controllername), dummy(ps_data$operatorname), dummy(ps_data$controllerid), dummy(ps_data$operatorid)),
                    MARGIN = 2, FUN = function(x) factor(x))
#Memory issues with the next line so, for now, this is mostly to outline what ideally should happen at this stage. 4/16/16
#ps_data = merge(ps_data, data.frame(new_dummies))

# PRODUCE DATASET WITH ONLY VARS OF INTEREST FOR RF/BOOSTING ANALYSIS
simple.data = ps_data[, c(match("PS", names(ps_data)), match("pin", names(ps_data)),
                          match("strike", names(ps_data)), match("strikerib", names(ps_data)),
                          match("trap", names(ps_data)),  match("collided", names(ps_data)),
                          match("hit", names(ps_data)), match("ranover", names(ps_data)),
                          match("rolled", names(ps_data)), match("caught", names(ps_data)),
                          match("between", names(ps_data)), match("by", names(ps_data)),
                          match("brakes", names(ps_data)),
                          match("jarring", names(ps_data)), match("bounced", names(ps_data)),
                          match("hole", names(ps_data)), match("roofbolt", names(ps_data)),
                          match("rock", names(ps_data)), match("driving", names(ps_data)),
                          match("operating", names(ps_data)),
                          match("riding", names(ps_data)), match("passenger", names(ps_data)),
                          match("keyword", names(ps_data)), match("false_keyword", names(ps_data)),
                          match("likely_equip", names(ps_data)), match("unlikely_equip", names(ps_data)),
                          match("likely_class", names(ps_data)), match("unlikely_class", names(ps_data)),
                          match("likely_type", names(ps_data)), match("unlikely_type", names(ps_data)),
                          match("unlikely_nature", names(ps_data)), match("likely_source", names(ps_data)),                          
                          match("unlikely_source", names(ps_data)), match("likely_actvty", names(ps_data)), 
                          match("unlikely_activity", names(ps_data)), 
                          match("unlikely_occup", names(ps_data)), 
                          match("unlikely_body", names(ps_data)),
                          match("falling.accident", names(ps_data)), match("accident.only", names(ps_data)))]

write.csv(simple.data, file = "C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/prepped_PS_training_data.csv", row.names = FALSE)