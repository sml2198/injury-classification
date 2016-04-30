# This code pulls in and cleans accidents data and mines data and merges accidents with mines.

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

# FORMAT VARS AND MAKE SURE HAS ALL THE SAME VARS AS OPEN DATA
acc.2000.16$mineid = sprintf("%07s", acc.2000.16$mineid)
acc.2000.16$documentno = as.character(acc.2000.16$documentno)
acc.2000.16$documentno = sprintf("%12s", acc.2000.16$documentno)
acc.2000.16$datasource = "opendata"
acc.2000.16$oldoccupationcode = ""
acc.2000.16 = acc.2000.16[, c(-match("closeddocumentno", names(acc.2000.16)), -match("coalcormetalm", names(acc.2000.16)))]
acc.2000.16 = acc.2000.16[acc.2000.16$calendaryear>1999, ]

######################################################################################################
# IMPORT 1983-2013 ACCIDENTS DATA FROM MSHA FOR DATA PRE-2000
# I wish we could import the raw .txt file but I've tried all the encodings and none work, so for now I converted the .txt into a .csv in Stata and I read that. 
#acc.83.13 = read.table("data/0_originals/MSHA/rec_2015_06_02/Accidents_1983_2013/Accidents_1983_2013.txt",  header = TRUE, sep = "|",  fileEncoding = "ASCII")
acc.83.13 = read.csv("data/0_originals/MSHA/rec_2015_06_02/Accidents_1983_2013/Accidents_1983_2013.csv",  header = TRUE, sep = ",",  stringsAsFactors = FALSE)

# FORMAT VARS AND MAKE SURE HAS ALL THE SAME VARS AS OPEN DATA
acc.83.13$mineid = sprintf("%07s", acc.83.13$mineid)
acc.83.13$documentno = as.character(acc.83.13$documentno)
acc.83.13$documentno = sprintf("%12s", acc.83.13$documentno)
acc.83.13$datasource = "msha"
acc.83.13$controllername = ""
acc.83.13$operatorname = ""
acc.83.13$fiscalquarter = ""
acc.83.13$fiscalyear = ""
acc.83.13$investigationbegindate = ""

# DROP OBSERVATIONS AFTER 2000 (WE'LL GET THESE FROM THE OPEN DATA)
acc.83.13 = acc.83.13[acc.83.13$calendaryear<2000, ]

# MERGE ACCIDENTS ( SHOULD HAVE 675902 OBS) AND FORMAT DOC/MINE IDS
accidents = rbind(acc.83.13, acc.2000.16)
accidents$mineid = sprintf("%07s", accidents$mineid)
accidents$documentno = sprintf("%12s", accidents$documentno)

# We've already done this as a merge in Stata so we know that observations not merged from master data (msha) are all 1983-1999 observations (good) 
# except for 2 from 2013 and 1 from 2005, and all observations not merged from using data (opendata) are 2013+ (good) except for 1 from 2005, 2008, 
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
# IMPORT MINES DATA - 86135 OBSERVATIONS
mines = read.table("data/0_originals/MSHA/open_data/Mines.txt",  header = TRUE, sep = "|")

names(mines)[names(mines) == 'ASSESS_CTRL_NO'] <- 'assesscontrolno'
names(mines)[names(mines) == 'AVG_MINE_HEIGHT'] <- 'averagemineheight'
names(mines)[names(mines) == 'BOM_STATE_CD'] <- 'bomstatecode'
names(mines)[names(mines) == 'COAL_METAL_IND'] <- 'coalcormetalmmine'
names(mines)[names(mines) == 'COMPANY_TYPE'] <- 'companytype'
names(mines)[names(mines) == 'CONG_DIST_CD'] <- 'congdistcode'
names(mines)[names(mines) == 'CURRENT_103I'] <- 'idesc'
names(mines)[names(mines) == 'CURRENT_103I_DT'] <- 'idate'
names(mines)[names(mines) == 'CURRENT_CONTROLLER_BEGIN_DT'] <- 'controllerbegindate'
names(mines)[names(mines) == 'CURRENT_CONTROLLER_ID'] <- 'controllerid'
names(mines)[names(mines) == 'CURRENT_CONTROLLER_NAME'] <- 'controllername'
names(mines)[names(mines) == 'CURRENT_MINE_NAME'] <- 'minename'
names(mines)[names(mines) == 'CURRENT_MINE_STATUS'] <- 'minestatus'
names(mines)[names(mines) == 'CURRENT_MINE_TYPE'] <- 'minetype'
names(mines)[names(mines) == 'CURRENT_OPERATOR_ID'] <- 'operatorid'
names(mines)[names(mines) == 'CURRENT_OPERATOR_NAME'] <- 'operatorname'
names(mines)[names(mines) == 'CURRENT_STATUS_DT'] <- 'minestatusdate'
names(mines)[names(mines) == 'DAYS_PER_WEEK'] <- 'daysperweek'
names(mines)[names(mines) == 'DIRECTIONS_TO_MINE'] <- 'directionstominemodified'
names(mines)[names(mines) == 'DISTRICT'] <- 'district'
names(mines)[names(mines) == 'FIPS_CNTY_CD'] <- 'fipscountycode'
names(mines)[names(mines) == 'FIPS_CNTY_NM'] <- 'fipscountyname'
names(mines)[names(mines) == 'HIGHWALL_MINER_USED'] <- 'highwallminerindicator'
names(mines)[names(mines) == 'HOURS_PER_SHIFT'] <- 'hourspershift'
names(mines)[names(mines) == 'LATITUDE'] <- 'latitude'
names(mines)[names(mines) == 'LONGITUDE'] <- 'longitude'
names(mines)[names(mines) == 'MAINT_SHIFTS_PER_DAY'] <- 'maintenanceshiftsperday'
names(mines)[names(mines) == 'METHANE_LIBERATION'] <- 'methaneliberation'
names(mines)[names(mines) == 'MILES_FROM_OFFICE'] <- 'milesfromoffice'
names(mines)[names(mines) == 'MINE_GAS_CATEGORY_CD'] <- 'minegascategorycode'
names(mines)[names(mines) == 'MINE_ID'] <- 'mineid'
names(mines)[names(mines) == 'MINERS_REP_IND'] <- 'minersrepindicator'
names(mines)[names(mines) == 'MULTIPLE_PITS'] <- 'multiplepitsindicator'
names(mines)[names(mines) == 'NEAREST_TOWN'] <- 'nearesttown'
names(mines)[names(mines) == 'NO_EMPLOYEES'] <- 'numberofemployees'
names(mines)[names(mines) == 'NO_NONPRODUCING_PITS'] <- 'noofnonproducingpits'
names(mines)[names(mines) == 'NO_PRODUCING_PITS'] <- 'noofproducingpits'
names(mines)[names(mines) == 'NO_TAILING_PONDS'] <- 'nooftailingponds'
names(mines)[names(mines) == 'OFFICE_CD'] <- 'officecode'
names(mines)[names(mines) == 'OFFICE_NAME'] <- 'officename'
names(mines)[names(mines) == 'PART48_TRAINING'] <- 'part48training'
names(mines)[names(mines) == 'PILLAR_RECOVERY_USED'] <- 'roomandpillarindicator'
names(mines)[names(mines) == 'PORTABLE_FIPS_ST_CD'] <- 'portablefipsstatecode'
names(mines)[names(mines) == 'PORTABLE_OPERATION'] <- 'portableoperationindicator'
names(mines)[names(mines) == 'PRIMARY_CANVASS'] <- 'primarycanvasscodedesc'
names(mines)[names(mines) == 'PRIMARY_CANVASS_CD'] <- 'primarycanvasscode'
names(mines)[names(mines) == 'PRIMARY_SIC_CD'] <- 'primarysiccode'
names(mines)[names(mines) == 'PRIMARY_SIC'] <- 'primarysicdesc'
names(mines)[names(mines) == 'PRIMARY_SIC_CD_1'] <- 'primarysiccodegroup'
names(mines)[names(mines) == 'PRIMARY_SIC_CD_SFX'] <- 'primarysiccodesuffix'
names(mines)[names(mines) == 'PROD_SHIFTS_PER_DAY'] <- 'productionshiftsperday'
names(mines)[names(mines) == 'SAFETY_COMMITTEE_IND'] <- 'safetycommitteeindicator'
names(mines)[names(mines) == 'SECONDARY_CANVASS'] <- 'secondarycanvasscodedesc'
names(mines)[names(mines) == 'SECONDARY_CANVASS_CD'] <- 'secondarycanvasscode'
names(mines)[names(mines) == 'SECONDARY_SIC'] <- 'secondarysicdesc'
names(mines)[names(mines) == 'SECONDARY_SIC_CD'] <- 'secondarysiccode'
names(mines)[names(mines) == 'SECONDARY_SIC_CD_1'] <- 'secondarysiccodegroup'
names(mines)[names(mines) == 'SECONDARY_SIC_CD_SFX'] <- 'secondarysiccodesuffix'
names(mines)[names(mines) == 'STATE'] <- 'stateabbreviation'

#GENERATE SOURCE FIELD, FORMAT ID VARS, REMOVE VARS THAT ARE MISSING FOR MOST MINES
mines = mines[, c(-match("companytype", names(mines)), -match("congdistcode", names(mines)))]
mines$mineid = sprintf("%07s", mines$mineid)

######################################################################################################
# MERGE MINES AND ACCIDENTS DATA 
# SHOULD WIND UP WITH 675887 MINES.ACCIDENTS
mines.accidents = merge(accidents, mines,by="mineid", all = TRUE)

# KEEP MINES LEVEL DATA FROM MINES DATASET (.y)
mines.accidents = mines.accidents[, c(-grep("\\.x", names(mines.accidents)))]
names(mines.accidents) = gsub("\\.[x|y]", "", names(mines.accidents))

# DROP MINES THAT DIDN'T MERGE WITH ANY ACCIDENTS (FROM 739004 TO 675902)
mines.accidents = mines.accidents[!(is.na(mines.accidents$documentno) | mines.accidents$documentno==""), ]
mines.accidents = mines.accidents[!(is.na(mines.accidents$mineid) | mines.accidents$mineid==""), ]

# DROP PROBLEM OBSERVATIONS
mines.accidents = mines.accidents[mines.accidents$problem!=1, ]
mines.accidents = mines.accidents[, c(-match("problem", names(mines.accidents)))]

######################################################################################################