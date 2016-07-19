# NIOSH Project 2014-N-15776

# 1 - Clean Accidents Data
  # Reads and cleans open source accidents data (post-2000)
  # Reads and cleans non-open source accidents data (1983-2013)
  # Merges open source accidents data (post-2000) and non-open source accidents data (1983-2013)
  # Outputs clean and merged accidents data

# Last edit 7/19/16

######################################################################################################

setwd("X:/Projects/Mining/NIOSH/analysis/")

# define file names
  # input: open source accidents data (post-2000)
acc.2000.16.file.name = "data/0_originals/MSHA/open_data/Accidents.txt" 
  # input: non-open source accidents data (1983-2013)
acc.83.13.file.name = "data/0_originals/MSHA/rec_2015_06_02/Accidents_1983_2013/Accidents_1983_2013.csv" 
  # output: clean and merged accidents data
accidents.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_accidents.rds" 

######################################################################################################

# READ AND CLEAN OPEN SOURCE ACCIDENTS DATA (post-2000)

# read open source accidents data (post-2000)
  # dataset downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp 
acc.2000.16 = read.table(acc.2000.16.file.name, header = TRUE, sep = "|")

# re-name variables in open source data (post-2000)
names(acc.2000.16)[names(acc.2000.16) == "MINE_ID"] = "mineid"
names(acc.2000.16)[names(acc.2000.16) == "ACCIDENT_DT"] = "accidentdate"
names(acc.2000.16)[names(acc.2000.16) == "ACCIDENT_TIME"] = "accidenttime"
names(acc.2000.16)[names(acc.2000.16) == "ACCIDENT_TYPE"] = "accidenttype"
names(acc.2000.16)[names(acc.2000.16) == "ACCIDENT_TYPE_CD"] = "accidenttypecode"
names(acc.2000.16)[names(acc.2000.16) == "ACTIVITY"] = "mineractivity"
names(acc.2000.16)[names(acc.2000.16) == "ACTIVITY_CD"] = "activitycode"
names(acc.2000.16)[names(acc.2000.16) == "CAL_QTR"] = "calendarquarter"
names(acc.2000.16)[names(acc.2000.16) == "CAL_YR"] = "calendaryear"
names(acc.2000.16)[names(acc.2000.16) == "CLASSIFICATION"] = "accidentclassification"
names(acc.2000.16)[names(acc.2000.16) == "CLASSIFICATION_CD"] = "classificationcode"
names(acc.2000.16)[names(acc.2000.16) == "CLOSED_DOC_NO"] = "closeddocumentno"
names(acc.2000.16)[names(acc.2000.16) == "COAL_METAL_IND"] = "coalcormetalm"
names(acc.2000.16)[names(acc.2000.16) == "CONTRACTOR_ID"] = "contractorid"
names(acc.2000.16)[names(acc.2000.16) == "CONTROLLER_ID"] = "controllerid"
names(acc.2000.16)[names(acc.2000.16) == "CONTROLLER_NAME"] = "controllername"
names(acc.2000.16)[names(acc.2000.16) == "DAYS_LOST"] = "dayslost"
names(acc.2000.16)[names(acc.2000.16) == "DAYS_RESTRICT"] = "daysrestrictedduty"
names(acc.2000.16)[names(acc.2000.16) == "DEGREE_INJURY"] = "degreeofinjury"
names(acc.2000.16)[names(acc.2000.16) == "DEGREE_INJURY_CD"] = "degreeofinjurycode"
names(acc.2000.16)[names(acc.2000.16) == "DOCUMENT_NO"] = "documentno"
names(acc.2000.16)[names(acc.2000.16) == "EQUIP_MFR_CD"] = "equipmanufacturercode"
names(acc.2000.16)[names(acc.2000.16) == "EQUIP_MFR_NAME"] = "equipmanufacturer"
names(acc.2000.16)[names(acc.2000.16) == "EQUIP_MODEL_NO"] = "equipmentmodelno"
names(acc.2000.16)[names(acc.2000.16) == "FISCAL_QTR"] = "fiscalquarter"
names(acc.2000.16)[names(acc.2000.16) == "FISCAL_YR"] = "fiscalyear"
names(acc.2000.16)[names(acc.2000.16) == "FIPS_STATE_CD"] = "fipsstatecode"
names(acc.2000.16)[names(acc.2000.16) == "IMMED_NOTIFY"] = "immediatenotificationclass"
names(acc.2000.16)[names(acc.2000.16) == "IMMED_NOTIFY_CD"] = "immediatenotificationcode"
names(acc.2000.16)[names(acc.2000.16) == "INJ_BODY_PART"] = "bodypart"
names(acc.2000.16)[names(acc.2000.16) == "INJ_BODY_PART_CD"] = "bodypartcode"
names(acc.2000.16)[names(acc.2000.16) == "INJURY_SOURCE"] = "sourceofinjury"
names(acc.2000.16)[names(acc.2000.16) == "INJURY_SOURCE_CD"] = "injurysourcecode"
names(acc.2000.16)[names(acc.2000.16) == "INVEST_BEGIN_DT"] = "investigationbegindate"
names(acc.2000.16)[names(acc.2000.16) == "JOB_EXPER"] = "jobexperience"
names(acc.2000.16)[names(acc.2000.16) == "MINE_EXPER"] = "mineexperience"
names(acc.2000.16)[names(acc.2000.16) == "MINING_EQUIP"] = "typeofequipment"
names(acc.2000.16)[names(acc.2000.16) == "MINING_EQUIP_CD"] = "equiptypecode"
names(acc.2000.16)[names(acc.2000.16) == "NARRATIVE"] = "narrativemodified"
names(acc.2000.16)[names(acc.2000.16) == "NATURE_INJURY"] = "natureofinjury"
names(acc.2000.16)[names(acc.2000.16) == "NATURE_INJURY_CD"] = "natureofinjurycode"
names(acc.2000.16)[names(acc.2000.16) == "NO_INJURIES"] = "numberofinjuries"
names(acc.2000.16)[names(acc.2000.16) == "OCCUPATION"] = "occupation"
names(acc.2000.16)[names(acc.2000.16) == "OCCUPATION_CD"] = "occupcode3digit"
names(acc.2000.16)[names(acc.2000.16) == "OPERATOR_ID"] = "operatorid"
names(acc.2000.16)[names(acc.2000.16) == "OPERATOR_NAME"] = "operatorname"
names(acc.2000.16)[names(acc.2000.16) == "RETURN_TO_WORK_DT"] = "returntoworkdate"
names(acc.2000.16)[names(acc.2000.16) == "SCHEDULE_CHARGE"] = "schedulechargedays"
names(acc.2000.16)[names(acc.2000.16) == "SHIFT_BEGIN_TIME"] = "shiftbeginningtime"
names(acc.2000.16)[names(acc.2000.16) == "SUBUNIT"] = "subunit"
names(acc.2000.16)[names(acc.2000.16) == "SUBUNIT_CD"] = "subunitcode"
names(acc.2000.16)[names(acc.2000.16) == "TOT_EXPER"] = "totalexperience"
names(acc.2000.16)[names(acc.2000.16) == "TRANS_TERM"] = "transferredorterminated"
names(acc.2000.16)[names(acc.2000.16) == "UG_LOCATION"] = "uglocation"
names(acc.2000.16)[names(acc.2000.16) == "UG_LOCATION_CD"] = "uglocationcode"
names(acc.2000.16)[names(acc.2000.16) == "UG_MINING_METHOD"] = "ugminingmethod"
names(acc.2000.16)[names(acc.2000.16) == "UG_MINING_METHOD_CD"] = "ugminingmethodcode"

# create new variable to track data source
acc.2000.16$datasource = "opendata" 

# format variables in open source accidents data (post-2000) to match non-open source accidents data (1983-2013) to facilitate merging
acc.2000.16$mineid = sprintf("%07s", acc.2000.16$mineid)
acc.2000.16$documentno = sprintf("%12s", acc.2000.16$documentno)
acc.2000.16$documentno = as.character(acc.2000.16$documentno)
acc.2000.16$oldoccupationcode = ""
acc.2000.16 = acc.2000.16[, c(-match("closeddocumentno", names(acc.2000.16)), -match("coalcormetalm", names(acc.2000.16)))]

# drop data from times and environments not of interest
acc.2000.16 = acc.2000.16[acc.2000.16$calendaryear > 1999, ]
acc.2000.16 = acc.2000.16[acc.2000.16$coalcormetalm == "C", ]
acc.2000.16 = acc.2000.16[!is.na(acc.2000.16$coalcormetalm), ]
acc.2000.16$subunit = tolower(acc.2000.16$subunit)
acc.2000.16 = acc.2000.16[acc.2000.16$subunit == "underground", ]

######################################################################################################

# READ AND CLEAN NON-OPEN SOURCE ACCIDENTS DATA (1983-2013)

# read non-open source accidents data (1983-2013)
  # originally a .txt file, converted to .csv format in Stata
acc.83.13 = read.csv(acc.83.13.file.name, header = TRUE, sep = ",",  stringsAsFactors = FALSE)

# track data source
acc.83.13$datasource = "msha"

# format variables in non-open source accidents data (1983-2013) to match open source accidents data (post-2000) to facilitate merging
acc.83.13$mineid = sprintf("%07s", acc.83.13$mineid)
acc.83.13$documentno = sprintf("%12s", acc.83.13$documentno)
acc.83.13$documentno = as.character(acc.83.13$documentno)
acc.83.13$controllername = ""
acc.83.13$operatorname = ""
acc.83.13$fiscalquarter = ""
acc.83.13$fiscalyear = ""
acc.83.13$investigationbegindate = ""
acc.83.13$subunit = tolower(acc.83.13$subunit)
acc.83.13 = acc.83.13[acc.83.13$subunit == "underground", ]
acc.83.13 = acc.83.13[acc.83.13$calendaryear < 2000, ]

######################################################################################################

# MERGE OPEN SOURCE ACCIDENTS DATA (post-2000) AND NON-OPEN SOURCE ACCIDENTS DATA (1983-2013), THEN OUTPUT

accidents = rbind(acc.83.13, acc.2000.16) # should have 675902 observations

# format variables to facilitate merging
accidents$mineid = sprintf("%07s", accidents$mineid)
accidents$documentno = sprintf("%12s", accidents$documentno)

# remove problematic observations identified from previous analysis in Stata:
  # mystery doc nos with _merge == 1:
    # (2005): 220052270088 
    # (2013): 220133400002 220140070002 
  # mystery doc nos with _merge == 2: 
    # (2005): 220052270088
    # (2008):  220153560029 
    # (2010): 220153240004 
    # (2011): 220152580041  		
    # (2012): 220151480037 220151520023 220151910032 220151970035 220152260215 
    # (2012, ctd): 220152260218 220153080004 220160040025 220160680001
      # 220052270088 appears in both lists because mineid is different
accidents$problem = ifelse((accidents$documentno == "220052270088" | accidents$documentno == "220133400002" |
                            accidents$documentno == "220140070002" | accidents$documentno == "220153560029" |
                            accidents$documentno == "220153240004" | accidents$documentno == "220152580041" |
                            accidents$documentno == "220151480037" | accidents$documentno == "220151520023" |
                            accidents$documentno == "220151910032" | accidents$documentno == "220151970035" |
                            accidents$documentno == "220152260215" | accidents$documentno == "220152260218" |
                            accidents$documentno == "220153080004" | accidents$documentno == "220160040025" |
                            accidents$documentno == "220160680001"), 1, 0)

# formate variables in merged data
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

# output clean and merged accidents data
saveRDS(accidents, file = accidents.file.name)

######################################################################################################


### HELP! I AM A LOST DATA PUP... PLEASE MOVE ME IN A NEW FILE WHERE I BELONG! ### 
### I HAVE COMMENTED THIS CODE, SO IT REALLY ONLY NEEDS TO BE MOVED, NO EDITS NECESSARY ###

# NIOSH Project 2014-N-15776

# [[INSERT NUMBER]] - Merge Accidents and Mines Data
  # Merges complete accidents data and mines data on mineid
  # Outputs clean and merged mines and accidents data

# Last edit 7/19/16

######################################################################################################

setwd("X:/Projects/Mining/NIOSH/analysis/")

# define file names
  # input: clean and merged accidents data
accidents.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_accidents.rds" 
  # input: clean and merged mines data
mines.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds" 
  # output: merged mines and accidents data
mines.accidents.file.name = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_mines_accidents.rds"

######################################################################################################

# MERGE MINES AND ACCIDENTS DATA, THEN OUTPUT

# read data files
accidents = readRDS(accidents.file.name)
mines_quarters = readRDS(mines.file.name)

# merge mines and accidents data
mines.accidents = merge(accidents, mines_quarters,by = "mineid", all = TRUE) # should have 675887 observations

# keep mine-level information from mines data (.y)
mines.accidents = mines.accidents[, c(-grep("\\.x", names(mines.accidents)))]
names(mines.accidents) = gsub("\\.[x|y]", "", names(mines.accidents))

# drop mines that don't merge with any accidents (from 739004 to 675902)
mines.accidents = mines.accidents[!(is.na(mines.accidents$documentno) | mines.accidents$documentno == ""), ]
mines.accidents = mines.accidents[!(is.na(mines.accidents$mineid) | mines.accidents$mineid == ""), ]

# drop problematic observations
mines.accidents = mines.accidents[mines.accidents$problem != 1, ]
mines.accidents = mines.accidents[, c(-match("problem", names(mines.accidents)))]

# output merged mines and accidents data
saveRDS(mines.accidents, file = mines.accidents.file.name)

######################################################################################################
