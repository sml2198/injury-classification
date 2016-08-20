# NIOSH Project 2014-N-15776

# 2 - Clean Accidents Data
  # Reads and cleans open source accidents data (post-2000)
  # Reads and cleans non-open source accidents data (1983-2013)
  # Merges open source accidents data (post-2000) and non-open source accidents data (1983-2013)
  # Outputs clean and merged accidents data

# Last edit 8/1/16

######################################################################################################

library(stringr)

# define file names
  # input: open source accidents data (post-2000)
acc_2000_16_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Accidents.txt" 
  # input: non-open source accidents data (1983-2013)
#acc_83_13_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/rec_2015_06_02/Accidents_1983_2013/Accidents_1983_2013.csv" 
  # output: clean and merged accidents data
accidents_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_accidents.rds" 

######################################################################################################

# READ AND CLEAN OPEN SOURCE ACCIDENTS DATA (post-2000)

# read open source accidents data (post-2000)
  # dataset downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp 
acc_2000_16 = read.table(acc_2000_16_file_name, header = TRUE, sep = "|", na.strings=c("","NA")) # 212,611 obs unique on documentno, 11,763 unique mines

# drop data from times and environments not of interest - wind up with 75,672 obs unique on documentno, 1469 unique mines 
acc_2000_16 = acc_2000_16[acc_2000_16$COAL_METAL_IND == "C", ]
acc_2000_16 = acc_2000_16[acc_2000_16$SUBUNIT == "UNDERGROUND", ]

# drop unnecessary variables
acc_2000_16$CLOSED_DOC_NO = 
  acc_2000_16$COAL_METAL_IND = NULL

# re-name variables
names(acc_2000_16)[names(acc_2000_16) == "MINE_ID"] = "mineid"
names(acc_2000_16)[names(acc_2000_16) == "ACCIDENT_DT"] = "accidentdate"
names(acc_2000_16)[names(acc_2000_16) == "ACCIDENT_TIME"] = "accidenttime"
names(acc_2000_16)[names(acc_2000_16) == "ACCIDENT_TYPE"] = "accidenttype"
names(acc_2000_16)[names(acc_2000_16) == "ACCIDENT_TYPE_CD"] = "accidenttypecode"
names(acc_2000_16)[names(acc_2000_16) == "ACTIVITY"] = "mineractivity"
names(acc_2000_16)[names(acc_2000_16) == "ACTIVITY_CD"] = "activitycode"
names(acc_2000_16)[names(acc_2000_16) == "CAL_QTR"] = "calendarquarter"
names(acc_2000_16)[names(acc_2000_16) == "CAL_YR"] = "calendaryear"
names(acc_2000_16)[names(acc_2000_16) == "CLASSIFICATION"] = "accidentclassification"
names(acc_2000_16)[names(acc_2000_16) == "CLASSIFICATION_CD"] = "classificationcode"
names(acc_2000_16)[names(acc_2000_16) == "CONTRACTOR_ID"] = "contractorid"
names(acc_2000_16)[names(acc_2000_16) == "CONTROLLER_ID"] = "controllerid"
names(acc_2000_16)[names(acc_2000_16) == "CONTROLLER_NAME"] = "controllername"
names(acc_2000_16)[names(acc_2000_16) == "DAYS_LOST"] = "dayslost"
names(acc_2000_16)[names(acc_2000_16) == "DAYS_RESTRICT"] = "daysrestrictedduty"
names(acc_2000_16)[names(acc_2000_16) == "DEGREE_INJURY"] = "degreeofinjury"
names(acc_2000_16)[names(acc_2000_16) == "DEGREE_INJURY_CD"] = "degreeofinjurycode"
names(acc_2000_16)[names(acc_2000_16) == "DOCUMENT_NO"] = "documentno"
names(acc_2000_16)[names(acc_2000_16) == "EQUIP_MFR_CD"] = "equipmanufacturercode"
names(acc_2000_16)[names(acc_2000_16) == "EQUIP_MFR_NAME"] = "equipmanufacturer"
names(acc_2000_16)[names(acc_2000_16) == "EQUIP_MODEL_NO"] = "equipmentmodelno"
names(acc_2000_16)[names(acc_2000_16) == "FISCAL_QTR"] = "fiscalquarter"
names(acc_2000_16)[names(acc_2000_16) == "FISCAL_YR"] = "fiscalyear"
names(acc_2000_16)[names(acc_2000_16) == "FIPS_STATE_CD"] = "fipsstatecode"
names(acc_2000_16)[names(acc_2000_16) == "IMMED_NOTIFY"] = "immediatenotificationclass"
names(acc_2000_16)[names(acc_2000_16) == "IMMED_NOTIFY_CD"] = "immediatenotificationcode"
names(acc_2000_16)[names(acc_2000_16) == "INJ_BODY_PART"] = "bodypart"
names(acc_2000_16)[names(acc_2000_16) == "INJ_BODY_PART_CD"] = "bodypartcode"
names(acc_2000_16)[names(acc_2000_16) == "INJURY_SOURCE"] = "sourceofinjury"
names(acc_2000_16)[names(acc_2000_16) == "INJURY_SOURCE_CD"] = "injurysourcecode"
names(acc_2000_16)[names(acc_2000_16) == "INVEST_BEGIN_DT"] = "investigationbegindate"
names(acc_2000_16)[names(acc_2000_16) == "JOB_EXPER"] = "jobexperience"
names(acc_2000_16)[names(acc_2000_16) == "MINE_EXPER"] = "mineexperience"
names(acc_2000_16)[names(acc_2000_16) == "MINING_EQUIP"] = "typeofequipment"
names(acc_2000_16)[names(acc_2000_16) == "MINING_EQUIP_CD"] = "equiptypecode"
names(acc_2000_16)[names(acc_2000_16) == "NARRATIVE"] = "narrativemodified"
names(acc_2000_16)[names(acc_2000_16) == "NATURE_INJURY"] = "natureofinjury"
names(acc_2000_16)[names(acc_2000_16) == "NATURE_INJURY_CD"] = "natureofinjurycode"
names(acc_2000_16)[names(acc_2000_16) == "NO_INJURIES"] = "numberofinjuries"
names(acc_2000_16)[names(acc_2000_16) == "OCCUPATION"] = "occupation"
names(acc_2000_16)[names(acc_2000_16) == "OCCUPATION_CD"] = "occupcode3digit"
names(acc_2000_16)[names(acc_2000_16) == "OPERATOR_ID"] = "operatorid"
names(acc_2000_16)[names(acc_2000_16) == "OPERATOR_NAME"] = "operatorname"
names(acc_2000_16)[names(acc_2000_16) == "RETURN_TO_WORK_DT"] = "returntoworkdate"
names(acc_2000_16)[names(acc_2000_16) == "SCHEDULE_CHARGE"] = "schedulechargedays"
names(acc_2000_16)[names(acc_2000_16) == "SHIFT_BEGIN_TIME"] = "shiftbeginningtime"
names(acc_2000_16)[names(acc_2000_16) == "SUBUNIT"] = "subunit"
names(acc_2000_16)[names(acc_2000_16) == "SUBUNIT_CD"] = "subunitcode"
names(acc_2000_16)[names(acc_2000_16) == "TOT_EXPER"] = "totalexperience"
names(acc_2000_16)[names(acc_2000_16) == "TRANS_TERM"] = "transferredorterminated"
names(acc_2000_16)[names(acc_2000_16) == "UG_LOCATION"] = "uglocation"
names(acc_2000_16)[names(acc_2000_16) == "UG_LOCATION_CD"] = "uglocationcode"
names(acc_2000_16)[names(acc_2000_16) == "UG_MINING_METHOD"] = "ugminingmethod"
names(acc_2000_16)[names(acc_2000_16) == "UG_MINING_METHOD_CD"] = "ugminingmethodcode"

# create new variable to track data source
#acc_2000_16$datasource = "opendata" 

# format variables to facilitate merging
acc_2000_16$oldoccupationcode = ""
acc_2000_16$subunit = tolower(acc_2000_16$subunit)

# remove observations after Q1 2016
acc_2000_16$drop = ifelse((acc_2000_16$calendaryear == 2016 & acc_2000_16$calendarquarter > 1), 1, 0)
acc_2000_16 = acc_2000_16[acc_2000_16$drop == 0,] # now 75,016 obs, 1469 unique mineids
acc_2000_16 = acc_2000_16[,-match("drop", names(acc_2000_16))]

######################################################################################################

# # READ AND CLEAN NON-OPEN SOURCE ACCIDENTS DATA (1983-2013)
# 
# # read non-open source accidents data (1983-2013)
# # originally a .txt file, converted to .csv format in Stata
# acc_83_13 = read.csv(acc_83_13_file_name, header = TRUE, sep = ",",  stringsAsFactors = FALSE) # 656,373 obs unique on documentno, 22,593 unique mines
# 
# # drop data from times and environments not of interest, wind up with 209,095 obs unique on documentno, 4738 unique mines
# acc_83_13 = acc_83_13[acc_83_13$calendaryear >= 2000, ]
# acc_83_13 = acc_83_13[acc_83_13$subunit == "UNDERGROUND", ]
# 
# # track data source
# acc_83_13$datasource = "msha"
# 
# # format variables to facilitate merging
# acc_83_13$mineid = sprintf("%07s", acc_83_13$mineid)
# acc_83_13$documentno = sprintf("%12s", acc_83_13$documentno)
# acc_83_13$documentno = as.character(acc_83_13$documentno)
# acc_83_13$subunit = tolower(acc_83_13$subunit)
# acc_83_13$controllername = ""
# acc_83_13$operatorname = ""
# acc_83_13$fiscalquarter = ""
# acc_83_13$fiscalyear = ""
# acc_83_13$investigationbegindate = ""
#
# # MERGE OPEN SOURCE ACCIDENTS DATA (post-2000) AND NON-OPEN SOURCE ACCIDENTS DATA (1983-2013), THEN OUTPUT
# accidents = rbind(acc_83_13, acc_2000_16) # 284,111 obs unique on documentno, 5,593 unique mines
#
# # remove problematic observations identified from previous analysis in Stata:
# accidents$problem = ifelse((accidents$documentno == "220052270088" | accidents$documentno == "220133400002" |
#                               accidents$documentno == "220140070002" | accidents$documentno == "220153560029" |
#                               accidents$documentno == "220153240004" | accidents$documentno == "220152580041" |
#                               accidents$documentno == "220151480037" | accidents$documentno == "220151520023" |
#                               accidents$documentno == "220151910032" | accidents$documentno == "220151970035" |
#                               accidents$documentno == "220152260215" | accidents$documentno == "220152260218" |
#                               accidents$documentno == "220153080004" | accidents$documentno == "220160040025" |
#                               accidents$documentno == "220160680001"), 1, 0)
# accidents = accidents[accidents$problem == 0,] # now 284,103 obs

######################################################################################################

# LET'S JUST USE OPEN DATA FOR NOW. 
accidents = acc_2000_16 # this leaves us with 75,016 obs 
rm(acc_2000_16)

# format variables
accidents$mineid = as.character(as.numeric(accidents$mineid))
accidents$documentno = as.character(as.numeric(accidents$documentno))
accidents$mineid = str_pad(accidents$mineid, 7, pad = "0")
accidents$documentno = str_pad(accidents$documentno, 12, pad = "0")

# format variables
names(accidents)[names(accidents) == "narrativemodified"] = "narrative"
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

# output clean and merged accidents data - 75,016 obs unique on documentno, 1,469 unique mines
saveRDS(accidents, file = accidents_file_name)
rm(list = ls())
gc()

######################################################################################################
