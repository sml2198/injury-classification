##HEADER##

#Coded by Nikhil Saifullah

early_accidents = read.csv("X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/accidents_fromText.csv")
open_data_accidents = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Accidents.txt", header = T, sep = "|")

#renaming here
names(open_data_accidents)[names(open_data_accidents) == "CONTROLLER_NAME"] = "controllername"
names(open_data_accidents)[names(open_data_accidents) == "CONTROLLER_ID"] = "controllerid"
names(open_data_accidents)[names(open_data_accidents) == "OPERATOR_ID"] = "operatorid"
names(open_data_accidents)[names(open_data_accidents) == "OPERATOR_NAME"] = "operatorname"
names(open_data_accidents)[names(open_data_accidents) == "CONTRACTOR_ID"] = "contractorid"
names(open_data_accidents)[names(open_data_accidents) == "FISCAL_YR"] = "fiscalyear"
names(open_data_accidents)[names(open_data_accidents) == "CAL_QTR"] = "calendarquarter"
names(open_data_accidents)[names(open_data_accidents) == "FISCAL_QTR"] = "fiscalquarter"
names(open_data_accidents)[names(open_data_accidents) == "CAL_YR"] = "calendaryear"
names(open_data_accidents)[names(open_data_accidents) == "COAL_METAL_IND"] = "coalcormetalmmine"
names(open_data_accidents)[names(open_data_accidents) == "MINE_ID"] = "mineid"
names(open_data_accidents)[names(open_data_accidents) == "DOCUMENT_NO"] = "documentno"
names(open_data_accidents)[names(open_data_accidents) == "SUBUNIT_CD"] = "subunitcode"
names(open_data_accidents)[names(open_data_accidents) == "ACCIDENT_DT"] = "accidentdate"
names(open_data_accidents)[names(open_data_accidents) == "ACCIDENT_TIME"] = "accidenttime"
names(open_data_accidents)[names(open_data_accidents) == "DEGREE_INJURY_CD"] = "degreeofinjurycode"
names(open_data_accidents)[names(open_data_accidents) == "DEGREE_INJURY"] = "degreeofinjury"
names(open_data_accidents)[names(open_data_accidents) == "FIPS_STATE_CD"] = "fipsstatecode"
names(open_data_accidents)[names(open_data_accidents) == "UG_LOCATION_CD"] = "uglocationcode"
names(open_data_accidents)[names(open_data_accidents) == "UG_LOCATION"] = "uglocation"
names(open_data_accidents)[names(open_data_accidents) == "UG_MINING_METHOD_CD"] = "ugminingmethodcode"
names(open_data_accidents)[names(open_data_accidents) == "UG_MINING_METHOD"] = "ugminingmethod"
names(open_data_accidents)[names(open_data_accidents) == "MINING_EQUIP_CD"] = "equiptypecode"
names(open_data_accidents)[names(open_data_accidents) == "MINING_EQUIP"] = "typeofequipment"
names(open_data_accidents)[names(open_data_accidents) == "EQUIP_MFR_CD"] = "equipmanufacturercode"
names(open_data_accidents)[names(open_data_accidents) == "EQUIP_MFR_NAME"] = "equipmanufacturer"
names(open_data_accidents)[names(open_data_accidents) == "EQUIP_MODEL_NO"] = "equipmentmodelno"
names(open_data_accidents)[names(open_data_accidents) == "SHIFT_BEGIN_TIME"] = "shiftbeginningtime"
names(open_data_accidents)[names(open_data_accidents) == "CLASSIFICATION_CD"] = "classificationcode"
names(open_data_accidents)[names(open_data_accidents) == "CLASSIFICATION"] = "accidentclassification"
names(open_data_accidents)[names(open_data_accidents) == "ACCIDENT_TYPE_CD"] = "accidenttypecode"
names(open_data_accidents)[names(open_data_accidents) == "ACCIDENT_TYPE"] = "accidenttype"
names(open_data_accidents)[names(open_data_accidents) == "NO_INJURIES"] = "numberofinjuries"
names(open_data_accidents)[names(open_data_accidents) == "TOT_EXPER"] = "totalexperience"
names(open_data_accidents)[names(open_data_accidents) == "JOB_EXPER"] = "jobexperience"
names(open_data_accidents)[names(open_data_accidents) == "MINE_EXPER"] = "mineexperience"
names(open_data_accidents)[names(open_data_accidents) == "OCCUPATION_CD"] = "occupcode3digit"
names(open_data_accidents)[names(open_data_accidents) == "ACTIVITY_CD"] = "activitycode"
names(open_data_accidents)[names(open_data_accidents) == "ACTIVITY"] = "mineractivity"
names(open_data_accidents)[names(open_data_accidents) == "INJURY_SOURCE_CD"] = "injurysourcecode"
names(open_data_accidents)[names(open_data_accidents) == "INJURY_SOURCE"] = "sourceofinjury"
names(open_data_accidents)[names(open_data_accidents) == "NATURE_INJURY_CD"] = "natureofinjurycode"
names(open_data_accidents)[names(open_data_accidents) == "NATURE_INJURY"] = "natureofinjury"
names(open_data_accidents)[names(open_data_accidents) == "INJ_BODY_PART_CD"] = "bodypartcode"
names(open_data_accidents)[names(open_data_accidents) == "INJ_BODY_PART"] = "bodypart"
names(open_data_accidents)[names(open_data_accidents) == "SCHEDULE_CHARGE"] = "schedulechargedays"
names(open_data_accidents)[names(open_data_accidents) == "DAYS_RESTRICT"] = "daysrestrictedduty"
names(open_data_accidents)[names(open_data_accidents) == "DAYS_LOST"] = "dayslost"
names(open_data_accidents)[names(open_data_accidents) == "TRANS_TERM"] = "transferredorterminated"
names(open_data_accidents)[names(open_data_accidents) == "RETURN_TO_WORK_DT"] = "returntoworkdate"
names(open_data_accidents)[names(open_data_accidents) == "IMMED_NOTIFY_CD"] = "immediatenotificationcode"
names(open_data_accidents)[names(open_data_accidents) == "IMMED_NOTIFY"] = "immediatenotificationclass"
names(open_data_accidents)[names(open_data_accidents) == "INVEST_BEGIN_DT"] = "investigationbegindate"
names(open_data_accidents)[names(open_data_accidents) == "NARRATIVE"] = "narrative"
names(open_data_accidents)[names(open_data_accidents) == "CLOSED_DOC_NO"] = "closeddocumentno"

names(early_accidents)[names(early_accidents) == "narrativemodified"] = "narrative"
names(early_accidents)[names(early_accidents) == "coalcormetalm"] = "coalcormetalmmine"

names(open_data_accidents) = tolower(names(open_data_accidents))

open_data_accidents = open_data_accidents[open_data_accidents$calendaryear > 1999,]
early_accidents = early_accidents[early_accidents$calendaryear <= 1999,]

#Testing rbind method
open_data_accidents[,"oldoccupationcode"] = NA
early_accidents[,"controllername"] = NA
early_accidents[,"operatorname"] = NA
early_accidents[,"fiscalyear"] = NA
early_accidents[,"fiscalquarter"] = NA
early_accidents[,"investigationbegindate"] = NA
early_accidents[,"closeddocumentno"] = NA
early_accidents[,"coalcormetalmmine"] = NA
clean_accidents = rbind.data.frame(open_data_accidents, early_accidents)

problem_doc_nums = c(220052270088, 220133400002, 220140070002, 220153560029, 220153240004, 220152580041, 220151480037, 220151520023, 220151910032, 220151970035, 220152260215, 220152260218, 220153080004, 220160040025, 220160680001)
clean_accidents[, "problem_flag"] = as.numeric(clean_accidents$documentno %in% problem_doc_nums)

lowercase_vars = c("mineractivity", "narrative", "natureofinjury", "degreeofinjury", "sourceofinjury", "accidenttype", "bodypart", "typeofequipment", "immediatenotificationclass", "equipmanufacturer", "accidentclassification", "occupation")
for (i in 1:length(lowercase_vars)) {
  clean_accidents[, lowercase_vars[i]] = tolower(as.character(clean_accidents[, lowercase_vars[i]]))
}

#narrative cleaning apparently unecessary? Nikhil 5/4/16

save(clean_accidents, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_accidents.RData")
