##HEADER##

#Coded by Nikhil Saifullah

#early_mines = read.csv("X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/mines_fromText.csv") #only 25 mines added and all are non-coal
open_data_mines = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Mines.txt", header = T, sep = "|")

names(open_data_mines)[names(open_data_mines) == "MINE_ID"] = "mineid"
names(open_data_mines)[names(open_data_mines) == "MINE_NAME"] = "minename"
names(open_data_mines)[names(open_data_mines) == "MINE_TYPE"] = "minetype"
names(open_data_mines)[names(open_data_mines) == "COAL_METAL_IND"] = "coalcormetalmmine"
names(open_data_mines)[names(open_data_mines) == "CURRENT_MINE_TYPE"] = "minetype"
names(open_data_mines)[names(open_data_mines) == "CURRENT_MINE_STATUS"] = "minestatus"
names(open_data_mines)[names(open_data_mines) == "CURRENT_STATUS_DT"] = "minestatusdate"
names(open_data_mines)[names(open_data_mines) == "CURRENT_CONTROLLER_ID"] = "controllerid"
names(open_data_mines)[names(open_data_mines) == "CURRENT_CONTROLLER_NAME"] = "controllername"
names(open_data_mines)[names(open_data_mines) == "CURRENT_OPERATOR_ID"] = "operatorid"
names(open_data_mines)[names(open_data_mines) == "CURRENT_OPERATOR_NAME"] = "operatorname"
names(open_data_mines)[names(open_data_mines) == "STATE"] = "stateabbreviation"
names(open_data_mines)[names(open_data_mines) == "BOM_STATE_CD"] = "bomstatecode"
names(open_data_mines)[names(open_data_mines) == "FIPS_CNTY_CD"] = "fipscountycode"
names(open_data_mines)[names(open_data_mines) == "FIPS_CNTY_NM"] = "fipscountyname"
names(open_data_mines)[names(open_data_mines) == "CURRENT_CONTROLLER_BEGIN_DT"] = "controllerbegindate"
names(open_data_mines)[names(open_data_mines) == "OFFICE_CD"] = "officecode"
names(open_data_mines)[names(open_data_mines) == "OFFICE_NAME"] = "officename"
names(open_data_mines)[names(open_data_mines) == "PRIMARY_SIC"] = "primarysicdesc"
names(open_data_mines)[names(open_data_mines) == "PRIMARY_SIC_CD"] = "primarysiccode"
names(open_data_mines)[names(open_data_mines) == "PRIMARY_SIC_CD_1"] = "primarysiccodegroup"
names(open_data_mines)[names(open_data_mines) == "PRIMARY_SIC_CD_SFX"] = "primarysiccodesuffix"
names(open_data_mines)[names(open_data_mines) == "PRIMARY_CANVASS_CD"] = "primarycanvasscode"
names(open_data_mines)[names(open_data_mines) == "PRIMARY_CANVASS"] = "primarycanvasscodedesc"
names(open_data_mines)[names(open_data_mines) == "SECONDARY_CANVASS_CD"] = "secondarycanvasscode"
names(open_data_mines)[names(open_data_mines) == "SECONDARY_CANVASS"] = "secondarycanvasscodedesc"
names(open_data_mines)[names(open_data_mines) == "CURRENT_103I"] = "idesc"
names(open_data_mines)[names(open_data_mines) == "CURRENT_103I_DT"] = "idate"
names(open_data_mines)[names(open_data_mines) == "PORTABLE_OPERATION"] = "portableoperationindicator"
names(open_data_mines)[names(open_data_mines) == "PORTABLE_FIPS_ST_CD"] = "portablefipsstatecode"
names(open_data_mines)[names(open_data_mines) == "DAYS_PER_WEEK"] = "daysperweek"
names(open_data_mines)[names(open_data_mines) == "HOURS_PER_SHIFT"] = "hourspershift"
names(open_data_mines)[names(open_data_mines) == "PART48_TRAINING"] = "part48training"
names(open_data_mines)[names(open_data_mines) == "PROD_SHIFTS_PER_DAY"] = "productionshiftsperday"
names(open_data_mines)[names(open_data_mines) == "MAINT_SHIFTS_PER_DAY"] = "maintenanceshiftsperday"
names(open_data_mines)[names(open_data_mines) == "NO_EMPLOYEES"] = "numberofemployees"
names(open_data_mines)[names(open_data_mines) == "AVG_MINE_HEIGHT"] = "averagemineheight"
names(open_data_mines)[names(open_data_mines) == "MINE_GAS_CATEGORY_CD"] = "minegascategorycode"
names(open_data_mines)[names(open_data_mines) == "METHANE_LIBERATION"] = "methaneliberation"
names(open_data_mines)[names(open_data_mines) == "NO_PRODUCING_PITS"] = "noofproducingpits"
names(open_data_mines)[names(open_data_mines) == "NO_NONPRODUCING_PITS"] = "noofnonproducingpits"
names(open_data_mines)[names(open_data_mines) == "NO_TAILING_PONDS"] = "nooftailingponds"
names(open_data_mines)[names(open_data_mines) == "PILLAR_RECOVERY_USED"] = "roomandpillarindicator"
names(open_data_mines)[names(open_data_mines) == "HIGHWALL_MINER_USED"] = "highwallminerindicator"
names(open_data_mines)[names(open_data_mines) == "MULTIPLE_PITS"] = "multiplepitsindicator"
names(open_data_mines)[names(open_data_mines) == "MINERS_REP_IND"] = "minersrepindicator"
names(open_data_mines)[names(open_data_mines) == "SAFETY_COMMITTEE_IND"] = "safetycommitteeindicator"
names(open_data_mines)[names(open_data_mines) == "MILES_FROM_OFFICE"] = "milesfromoffice"
names(open_data_mines)[names(open_data_mines) == "DIRECTIONS_TO_MINE"] = "directionstominemodified"
names(open_data_mines)[names(open_data_mines) == "NEAREST_TOWN"] = "nearesttown"
names(open_data_mines) = tolower(names(open_data_mines))

open_data_mines = open_data_mines[, c(-grep("company_type", names(open_data_mines)), -grep("cong_dist_cd", names(open_data_mines)))]
clean_mines = open_data_mines

save(clean_mines, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.RData")
