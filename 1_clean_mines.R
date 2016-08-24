# NIOSH Project 2014-N-15776

# 1 - Clean Mines Data
  # Reads, cleans, then outputs quarterly under-reporting employment/production data
  # Reads, cleans, then outputs quarterly employment/production data
  # Reads, cleans, then outputs annual employment/production data
  # Reads and cleans mines data, then outputs mine type data
  # Merges mines and employment/production data on mineid, year, and quarter
  # Collapses merged data to the mine-quarter level, then outputs

# Last edit 8/1/16

######################################################################################################

library(plyr)
library(devtools)
library(stringr)
library(zoo)

# define file names
  # input: quarterly employment/production data
quarterly_employment_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdQuarterly.txt"
  # output: clean quarterly employment/production data
quarterly_employment_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment.rds"
  # input: mines data
open_data_mines_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Mines.txt"
  # output: clean mine type data
mine_types_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds"
  # output: clean mine-quarter level data (has observations dropped in preparation for prediction)
mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds"
  # output: all clean mine-quarter level data
all_mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/all_clean_mines.rds"

######################################################################################################

# READ AND CLEAN QUARTERLY EMPLOYMENT/PRODUCTION DATA, THEN OUTPUT

# read quarterly employment/production data
  # dataset downloaded on 8/16/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp - 1,803,767 obs, 30,993 unique mineids, 13 vars
quarterly_employment = read.table(quarterly_employment_in_file_name, header = T, sep = "|", na.strings=c("","NA"))

# drop data from environments not of interest - 57,957 obs unique on mine-year-quarter, 2626 unique mineids
quarterly_employment = quarterly_employment[(quarterly_employment$SUBUNIT == "UNDERGROUND"), ]

# drop unnecessary variables
quarterly_employment$COAL_METAL_IND = 
  quarterly_employment$STATE = 
  quarterly_employment$CURR_MINE_NM = 
  quarterly_employment$FISCAL_YR = 
  quarterly_employment$FISCAL_QTR = 
  quarterly_employment$SUBUNIT = 
  quarterly_employment$SUBUNIT_CD = NULL

# re-name variables
names(quarterly_employment)[names(quarterly_employment) == "CAL_YR"] = "year"
names(quarterly_employment)[names(quarterly_employment) == "CAL_QTR"] = "quarter"
names(quarterly_employment)[names(quarterly_employment) == "MINE_ID"] = "mineid"
names(quarterly_employment)[names(quarterly_employment) == "HOURS_WORKED"] = "hours_qtr"
names(quarterly_employment)[names(quarterly_employment) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_qtr"
names(quarterly_employment)[names(quarterly_employment) == "COAL_PRODUCTION"] = "coal_prod_qtr"

# format mine id
quarterly_employment$mineid = str_pad(quarterly_employment$mineid, 7, pad = "0")

# output clean quarterly employment/production data - 57,957 obs, 2,626 unique mineids, 6 vars
saveRDS(quarterly_employment, file = quarterly_employment_out_file_name)

######################################################################################################

# READ AND CLEAN MINES DATA, THEN OUTPUT MINE TYPE DATA

# read mines data
  # dataset downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp - 86,362 obs unique on mineid, 59 vars
open_data_mines = read.table(open_data_mines_file_name, header = T, sep = "|", na.strings=c("","NA"))

# re-name variables
names(open_data_mines)[names(open_data_mines) == "MINE_ID"] = "mineid"
names(open_data_mines)[names(open_data_mines) == "MINE_NAME"] = "minename"
names(open_data_mines)[names(open_data_mines) == "MINE_TYPE"] = "minetype"
names(open_data_mines)[names(open_data_mines) == "COAL_METAL_IND"] = "coalcormetalmmine"
names(open_data_mines)[names(open_data_mines) == "CURRENT_MINE_TYPE"] = "minetype"
names(open_data_mines)[names(open_data_mines) == "CURRENT_MINE_NAME"] = "minename"
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

# format mineid
open_data_mines$mineid = str_pad(open_data_mines$mineid, 7, pad = "0")

# save mine type info
mine_types = open_data_mines[, c(match("mineid", names(open_data_mines)), 
                                     match("minetype", names(open_data_mines)),
                                     match("coalcormetalmmine", names(open_data_mines)))]
saveRDS(mine_types, file = mine_types_file_name) #86,362 obs unique on mineid, 3 vars
rm(mine_types)

######################################################################################################

# MERGE MINES AND EMPLOYMENT/PRODUCTION DATA, THEN CLEAN

# merge mines and employment/production data on mineid, year, and quarter - wind up with 141,693 obs, 86,362 mines, 64 vars
# drop observations missing for coalcormetalmmine
mines_quarters = merge(quarterly_employment, open_data_mines, by = c("mineid"), all = T)
rm(open_data_mines, quarterly_employment)

# Drop data from environments not of interest - wind up with 50,993 obs, 14,503 mines, 64 vars
# (facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16)
mines_quarters = subset(mines_quarters, minetype == "Underground")
mines_quarters = subset(mines_quarters, coalcormetalmmine == "C")
mines_quarters = mines_quarters[order(mines_quarters$mineid, mines_quarters$year, mines_quarters$quarter),]

# Format date issued so that we can drop mine that were abandoned before our study period
datevars = names(mines_quarters)[grep("date", names(mines_quarters))]
for (i in 1:length(datevars)) {
  mines_quarters[, datevars[i]] = as.Date(as.character(mines_quarters[, datevars[i]]), "%m/%d/%Y")
}
mines_quarters$statusyear = as.yearqtr(mines_quarters$minestatusdate)
mines_quarters$statusyear = as.numeric(format(mines_quarters$statusyear, "%Y"))
mines_quarters$statusquarter = as.yearqtr(mines_quarters$minestatusdate)

# Drop mines that were abandoned before our study period - wind up with 38,653 obs, 2253 mines, 66 vars
mines_quarters$too_early = ifelse((mines_quarters$minestatus == "Abandoned" | 
                                    mines_quarters$minestatus == "Abandoned and Sealed" |
                                    mines_quarters$minestatus == "NonProducing") & 
                                    mines_quarters$statusyear < 2000, 1, 0)
mines_quarters = mines_quarters[mines_quarters$too_early == 0,]
mines_quarters$too_early = NULL

# There are 358 observations (mines) that didn't merge on any hours/employment data. I confirmed using the Mine Data Retrieval System
# that there is really no employment information on these mines. I drop them here. - Sarah L. 8/22/2016 @ 8:01 PM.
mines_quarters = mines_quarters[!is.na(mines_quarters$hours_qtr),]

######################################################################################################

# format date variables so quarter contains date-formatted year and quarter info (now obs will be unique on mineid-quarter)
mines_quarters$quarter = paste(mines_quarters$year, mines_quarters$quarter, sep = "-")
mines_quarters$quarter = ifelse(mines_quarters$quarter == "NA-NA", NA, mines_quarters$quarter)
mines_quarters$quarter = as.yearqtr(mines_quarters$quarter)

# rename employment for ease of use
names(mines_quarters)[names(mines_quarters) == "avg_employee_cnt_qtr"] = "employment_qtr"

# output mines data BEFORE dropping any observations based on missing data 
# this data is for merging on accidents - will be used in 3_merge_accidents.R
saveRDS(mines_quarters, file = all_mines_quarters_file_name)

######################################################################################################

# HERE WE DROP OBSERVATIONS THAT DON'T BELONG 

# now have 38,295 obs unique on mineid-quarter, 1895 unique mines, 66 vars

# drop observations for mines that are abandoned or sealed when their status date comes before the current quarter - now 37,297 obs, 1855 unique mineids
mines_quarters$minestatus = as.character(mines_quarters$minestatus)
mines_quarters$drop = ifelse((mines_quarters$minestatus == "Abandoned" | mines_quarters$minestatus == "Abandoned and Sealed") &
                               (mines_quarters$statusquarter <= mines_quarters$quarter), 1, 0)
mines_quarters = mines_quarters[mines_quarters$drop == 0, ]

# edit mine status date:
  # if for a given mine-quarter the minestatus date is LESS than the quarter then the observation should take on that minestatus
  # if for a given mine-quarter the minestatus date is GREATER than that quarter AND the minestatus is abandoned, then the observation should take on some other minestatus
mines_quarters$minestatus = ifelse((mines_quarters$statusquarter >= mines_quarters$quarter) 
                                   & (mines_quarters$minestatus == "Abandoned" | mines_quarters$minestatus == "Abandoned and Sealed" | 
                                        mines_quarters$minestatus == "Temporarily Idled" | mines_quarters$minestatus == "NonProducing" ), "Unknown", mines_quarters$minestatus)

# remove mine-quarters with minestatus "Temporarily Idled" and zero hours/employment/production (these are nonproducing) - wind up with 36,916 obs, 67 vars, 1850 unique mineids
mines_quarters = mines_quarters[(mines_quarters$minestatus != "Temporarily Idled" | mines_quarters$hours_qtr != 0), ]

# drop unnecessary variables - wind up with 36,916 obs, 64 vars
mines_quarters$statusyear = 
  mines_quarters$statusquarter = 
  mines_quarters$drop = NULL

######################################################################################################

# COLLAPSE MERGED DATA TO THE MINE-QUARTER LEVEL, THEN OUTPUT

# collapse data to mine-quarter level to make sure nothing weird has happened
# we wind up with 36,916 obs which is the same as before, as it should be, wahoo!
temp = mines_quarters[, c("mineid", 
                          "year",
                          "quarter",
                          "minetype",
                          "minename",
                          "minestatus",
                          "minestatusdate",
                          "operatorid",
                          "operatorname",
                          "coalcormetalmmine",
                          "stateabbreviation",
                          "idate",
                          "idesc",
                          "daysperweek",
                          "productionshiftsperday")]

mines_quarters = ddply(mines_quarters[, c("hours_qtr",
                                          "employment_qtr", 
                                          "coal_prod_qtr", 
                                          "mineid", 
                                          "year", 
                                          "quarter")], c("mineid", "quarter"), 
                       function(x) colMeans(x[, c(match("hours_qtr", names(x)), 
                                                  match("employment_qtr", names(x)), 
                                                  match("coal_prod_qtr", names(x)))], na.rm = TRUE))
mines_quarters = merge(mines_quarters, temp, by = c("mineid", "quarter"), all = T)

# drop observations with no hours - wind up with 30,551 obs unique on mineid-quarter, 18 vars, 1583 unique mines
mines_quarters = mines_quarters[(mines_quarters$hours_qtr != 0), ]

# output mine-level data
saveRDS(mines_quarters, file = mines_quarters_file_name)
rm(list = ls())
gc()

######################################################################################################
