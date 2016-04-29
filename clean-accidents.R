# This code pulls in and cleans accidents data and merges accidents with mines.

######################################################################################################
setwd("X:/Projects/Mining/NIOSH/analysis/")
acc.2000.16 = read.table("data/0_originals/MSHA/open_data/Accidents.txt",  header = TRUE, sep = "|")

names(acc.2000.16)[names(acc.2000.16) == 'MINE_ID'] <- 'mineid'
names(acc.2000.16)[names(acc.2000.16) == 'ACCIDENT_DT'] <- 'accidentdate'
names(acc.2000.16)[names(acc.2000.16) == 'ACCIDENT_TIME'] <- 'accidenttime'
names(acc.2000.16)[names(acc.2000.16) == 'ACCIDENT_TYPE'] <- 'accidenttype'
names(acc.2000.16)[names(acc.2000.16) == 'ACCIDENT_TYPE_CD'] <- 'accidenttypecode'
names(acc.2000.16)[names(acc.2000.16) == 'ACTIVITY'] <- 'mineractivity'
names(acc.2000.16)[names(acc.2000.16) == 'ACTIVITY_CD'] <- 'activitycode'
names(acc.2000.16)[names(acc.2000.16) == 'CAL_QTR'] <- 'calendarquarter'
names(acc.2000.16)[names(acc.2000.16) == 'CAL_YR'] <- 'calendaryear'
names(acc.2000.16)[names(acc.2000.16) == 'CLASSIFICATION'] <- 'accidentclassification'
names(acc.2000.16)[names(acc.2000.16) == 'CLASSIFICATION_CD'] <- 'classificationcode'
names(acc.2000.16)[names(acc.2000.16) == 'CLOSED_DOC_NO'] <- 'closeddocumentno'
names(acc.2000.16)[names(acc.2000.16) == 'COAL_METAL_IND'] <- 'coalcormetalm'
names(acc.2000.16)[names(acc.2000.16) == 'CONTRACTOR_ID'] <- 'contractorid'
names(acc.2000.16)[names(acc.2000.16) == 'CONTROLLER_ID'] <- 'controllerid'
names(acc.2000.16)[names(acc.2000.16) == 'CONTROLLER_NAME'] <- 'controllername'
names(acc.2000.16)[names(acc.2000.16) == 'DAYS_LOST'] <- 'dayslost'
names(acc.2000.16)[names(acc.2000.16) == 'DAYS_RESTRICT'] <- 'daysrestrictedduty'
names(acc.2000.16)[names(acc.2000.16) == 'DEGREE_INJURY'] <- 'degreeofinjury'
names(acc.2000.16)[names(acc.2000.16) == 'DEGREE_INJURY_CD'] <- 'degreeofinjurycode'
names(acc.2000.16)[names(acc.2000.16) == 'DOCUMENT_NO'] <- 'documentno'
names(acc.2000.16)[names(acc.2000.16) == 'EQUIP_MFR_CD'] <- 'equipmanufacturercode'
names(acc.2000.16)[names(acc.2000.16) == 'EQUIP_MFR_NAME'] <- 'equipmanufacturer'
names(acc.2000.16)[names(acc.2000.16) == 'EQUIP_MODEL_NO'] <- 'equipmentmodelno'
names(acc.2000.16)[names(acc.2000.16) == 'FISCAL_QTR'] <- 'fiscalquarter'
names(acc.2000.16)[names(acc.2000.16) == 'FISCAL_YR'] <- 'fiscalyear'
names(acc.2000.16)[names(acc.2000.16) == 'FIPS_STATE_CD'] <- 'fipsstatecode'
names(acc.2000.16)[names(acc.2000.16) == 'IMMED_NOTIFY'] <- 'immediatenotificationclass'
names(acc.2000.16)[names(acc.2000.16) == 'IMMED_NOTIFY_CD'] <- 'immediatenotificationcode'
names(acc.2000.16)[names(acc.2000.16) == 'INJ_BODY_PART'] <- 'bodypart'
names(acc.2000.16)[names(acc.2000.16) == 'INJ_BODY_PART_CD'] <- 'bodypartcode'
names(acc.2000.16)[names(acc.2000.16) == 'INJURY_SOURCE'] <- 'sourceofinjury'
names(acc.2000.16)[names(acc.2000.16) == 'INJURY_SOURCE_CD'] <- 'injurysourcecode'
names(acc.2000.16)[names(acc.2000.16) == 'INVEST_BEGIN_DT'] <- 'investigationbegindate'
names(acc.2000.16)[names(acc.2000.16) == 'JOB_EXPER'] <- 'jobexperience'
names(acc.2000.16)[names(acc.2000.16) == 'MINE_EXPER'] <- 'mineexperience'
names(acc.2000.16)[names(acc.2000.16) == 'MINING_EQUIP'] <- 'typeofequipment'
names(acc.2000.16)[names(acc.2000.16) == 'MINING_EQUIP_CD'] <- 'equiptypecode'
names(acc.2000.16)[names(acc.2000.16) == 'NARRATIVE'] <- 'narrativemodified'
names(acc.2000.16)[names(acc.2000.16) == 'NATURE_INJURY'] <- 'natureofinjury'
names(acc.2000.16)[names(acc.2000.16) == 'NATURE_INJURY_CD'] <- 'natureofinjurycode'
names(acc.2000.16)[names(acc.2000.16) == 'NO_INJURIES'] <- 'numberofinjuries'
names(acc.2000.16)[names(acc.2000.16) == 'OCCUPATION'] <- 'occupation'
names(acc.2000.16)[names(acc.2000.16) == 'OCCUPATION_CD'] <- 'occupcode3digit'
names(acc.2000.16)[names(acc.2000.16) == 'OPERATOR_ID'] <- 'operatorid'
names(acc.2000.16)[names(acc.2000.16) == 'OPERATOR_NAME'] <- 'operatorname'
names(acc.2000.16)[names(acc.2000.16) == 'RETURN_TO_WORK_DT'] <- 'returntoworkdate'
names(acc.2000.16)[names(acc.2000.16) == 'SCHEDULE_CHARGE'] <- 'schedulechargedays'
names(acc.2000.16)[names(acc.2000.16) == 'SHIFT_BEGIN_TIME'] <- 'shiftbeginningtime'
names(acc.2000.16)[names(acc.2000.16) == 'SUBUNIT'] <- 'subunit'
names(acc.2000.16)[names(acc.2000.16) == 'SUBUNIT_CD'] <- 'subunitcode'
names(acc.2000.16)[names(acc.2000.16) == 'TOT_EXPER'] <- 'totalexperience'
names(acc.2000.16)[names(acc.2000.16) == 'TRANS_TERM'] <- 'transferredorterminated'
names(acc.2000.16)[names(acc.2000.16) == 'UG_LOCATION'] <- 'uglocation'
names(acc.2000.16)[names(acc.2000.16) == 'UG_LOCATION_CD'] <- 'uglocationcode'
names(acc.2000.16)[names(acc.2000.16) == 'UG_MINING_METHOD'] <- 'ugminingmethod'
names(acc.2000.16)[names(acc.2000.16) == 'UG_MINING_METHOD_CD'] <- 'ugminingmethodcode'

acc.2000.16$mineid = sprintf("%07d", acc.2000.16$mineid)
acc.2000.16$documentno = as.character(acc.2000.16$documentno)
acc.2000.16$documentno = sprintf("%12s", acc.2000.16$documentno)
acc.2000.16$datasource = "opendata"
  
######################################################################################################
# IMPORT 1983-2013 ACCIDENTS DATA FROM MSHA FOR DATA PRE-2000
# I wish we could import the raw .txt file but I've tried all the encodings and none work, so for now I converted the .txt into a .csv in Stata and I read that. 
#acc.83.13 = read.table("data/0_originals/MSHA/rec_2015_06_02/Accidents_1983_2013/Accidents_1983_2013.txt",  header = TRUE, sep = "|",  fileEncoding = "ASCII")
acc.83.13 = read.csv("data/0_originals/MSHA/rec_2015_06_02/Accidents_1983_2013/Accidents_1983_2013.csv",  header = TRUE, sep = ",",  stringsAsFactors = FALSE)

acc.83.13$mineid = sprintf("%07d", acc.83.16$mineid)
acc.83.13$documentno = as.character(acc.83.16$documentno)
acc.83.13$documentno = sprintf("%12s", acc.83.16$documentno)
acc.83.13$datasource = "msha"

# MERGE ACCIDENTS
accidents = merge(acc.83.13,acc.2000.16,by=c("documentno","mineid"))
accidents = accidents[, c(-grep("\\.y", names(accidents)))]
names(accidents) = gsub("\\.[x|y]", "", names(accidents))

accidents$mineid = sprintf("%07s", accidents$mineid)
accidents$documentno = sprintf("%12s", accidents$documentno)
# We've already done this merge in Stata so we know that obsverations not merged from master data (msha) are all 1983-1999 observations (good) 
# except for 2 from 2013 and 1 from 2005, and all obserbaions not merged from using data (opendata) are 2013+ (good) except for 1 from 2005, 2008, 
# 2010, 2011, each, and 9 from 2012.
# mystery doc no's with _merge == 1: (2005): 220052270088 (2013): 220133400002 220140070002 
# mystery doc no's with _merge == 2: (2005): 220052270088 (2008):  220153560029 (2010): 220153240004 (2011): 220152580041  		
# note that 220052270088 appears in both lists (because mineid is different)
# cont'd (2012): 220151480037 220151520023 220151910032 220151970035 220152260215 220152260218 220153080004 220160040025 220160680001
accidents$problem = ifelse((accidents$documentno == "220052270088" | accidents$documentno == "220133400002" |
                            accidents$documentno == "220140070002" | accidents$documentno == "220153560029" |
                            accidents$documentno == "220153240004" | accidents$documentno == "220152580041" |
                            accidents$documentno == "220151480037" | accidents$documentno == "220151520023" |
                            accidents$documentno == "220151910032" | accidents$documentno == "220151970035" |
                            accidents$documentno == "220152260215" | accidents$documentno == "220152260218" |
                            accidents$documentno == "220153080004" | accidents$documentno == "220160040025" |
                            accidents$documentno == "220160680001"), 1, 0)
names(accidents)[names(accidents) == 'narrativemodified'] <- 'narrative'
names(accidents)[names(accidents) == 'coalcormetalm'] <- 'coalcormetalmmine'

# MAKE SURE NARRATIVE & CHARACTER VARS ARE ALL LOWERCASE
accidents$mineractivity = tolower(accidents$mineractivity)
accidents$narrative = tolower(accidents$narrative)
accidents$natureofinjury = tolower(accidents$natureofinjury)
accidents$degreeofinjury = tolower(accidents$degreeofinjury)
accidents$sourceofinjury = tolower(accidents$sourceofinjury)
accidents$accidenttype = tolower(accidents$accidenttype)
accidents$bodypart = tolower(accidents$bodypart)
accidents$typeofequipment = tolower(accidents$typeofequipment)
accidents$immediatenotificationclass = tolower(accidents$immediatenotificationclass)
accidents$equipmanufacturer = tolower(accidents$equipmanufacturer)
accidents$accidentclassification = tolower(accidents$accidentclassification)
accidents$occupation = tolower(accidents$occupation)

######################################################################################################