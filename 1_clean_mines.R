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
  # input: quarterly under-reporting employment/production data
underreporting_employment_in_file_name = "X:/Projects/Mining/MSHA_OSHA_Underreporting/analysis/data/0_originals/MSHA/rec_2015_03_03/Operator_Quarterly_Emp_Production/Operator_Quarterly_Emp_Production.txt"
  # output: clean quarterly under-reporting employment/production data
underreporting_employment_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_underreporting_employment.rds"
  # input: quarterly employment/production data
quarterly_employment_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdQuarterly.txt"
  # output: clean quarterly employment/production data
quarterly_employment_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment.rds"
  # input: annual employment/production data
yearly_employment_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdYearly.txt"
  # output: clean annual employment/production data
yearly_employment_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment_yrly.rds"
  # input: mines data
open_data_mines_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Mines.txt"
  # output: clean mine type data
mine_types_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/mine_types.rds"
  # output: clean mine-quarter level data (has observations dropped in preparation for prediction)
mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds"
  # output: all clean mine-quarter level data
all_mines_quarters_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/all_clean_mines.rds"

######################################################################################################

# READ AND CLEAN QUARTERLY UNDER-REPORTING EMPLOYMENT/PRODUCTION DATA, THEN OUTPUT

# read quarterly under-reporting employment/production data
underreporting_employment = read.table(underreporting_employment_in_file_name, fileEncoding = "UCS-2LE", header = T, sep = "|")

# drop data from environments not of interest
underreporting_employment = underreporting_employment[underreporting_employment$Subunit == "UNDERGROUND", ]

# drop unnecessary variables
underreporting_employment$Subunit = 
  underreporting_employment$Subunit.Code = NULL

# re-name variables
names(underreporting_employment)[names(underreporting_employment) == "Production.Year"] = "year"
names(underreporting_employment)[names(underreporting_employment) == "Production.Quarter"] = "quarter"
names(underreporting_employment)[names(underreporting_employment) == "Mine.Id"] = "mineid"
names(underreporting_employment)[names(underreporting_employment) == "Employees"] = "under_avg_employee_cnt_qtr"
names(underreporting_employment)[names(underreporting_employment) == "Hours.Worked"] = "under_employee_hours_qtr"
names(underreporting_employment)[names(underreporting_employment) == "Coal.Production"] = "under_coal_prod_qtr"

# format mine id
underreporting_employment$mineid = str_pad(underreporting_employment$mineid, 7, pad = "0")

# output clean quarterly under-reporting employment/production data - 187370 obs, 6 vars
saveRDS(underreporting_employment, file = underreporting_employment_out_file_name)

######################################################################################################

# READ AND CLEAN QUARTERLY EMPLOYMENT/PRODUCTION DATA, THEN OUTPUT

# read quarterly employment/production data
  # dataset downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp 
quarterly_employment = read.table(quarterly_employment_in_file_name, header = T, sep = "|")

# drop data from environments not of interest
quarterly_employment = quarterly_employment[(quarterly_employment$SUBUNIT == "UNDERGROUND" & quarterly_employment$COAL_METAL_IND == "C"), ]

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

# output clean quarterly employment/production data - 41623 obs, 6 vars
saveRDS(quarterly_employment, file = quarterly_employment_out_file_name)

######################################################################################################

# READ AND CLEAN ANNUAL EMPLOYMENT/PRODUCTION DATA, THEN OUTPUT

# read annual employment/production data
  # dataset downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp - 427741 obs, 11 vars
yearly_employment = read.table(yearly_employment_in_file_name, header = T, sep = "|")

# drop data from environments not of interest - 9762 obs, 11 vars
yearly_employment = yearly_employment[(yearly_employment$SUBUNIT == "UNDERGROUND" & yearly_employment$COAL_METAL_IND == "C"), ]

# drop unnecessary variables
yearly_employment$COAL_METAL_IND =
  yearly_employment$STATE =
  yearly_employment$SUBUNIT =
  yearly_employment$SUBUNIT_CD = NULL

# re-name variables
names(yearly_employment)[names(yearly_employment) == "CAL_YR"] = "year"
names(yearly_employment)[names(yearly_employment) == "MINE_ID"] = "mineid"
names(yearly_employment)[names(yearly_employment) == "MINE_NAME"] = "minename"
names(yearly_employment)[names(yearly_employment) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_yr"
names(yearly_employment)[names(yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "employee_hours_yr"
names(yearly_employment)[names(yearly_employment) == "ANNUAL_COAL_PRODUCTION"] = "coal_prod_yr"
names(yearly_employment)[names(yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "employee_hours_yr"
names(yearly_employment)[names(yearly_employment) == "ANNUAL_HOURS"] = "hours_yr"

# format mine id
yearly_employment$mineid = str_pad(yearly_employment$mineid, 7, pad = "0")

# output clean annual employment/production data - 9762 obs, 7 vars
saveRDS(yearly_employment, file = yearly_employment_out_file_name)

######################################################################################################

# READ AND CLEAN MINES DATA, THEN OUTPUT MINE TYPE DATA

# data from Carolyn Stasik's old data pull - very messy and turns out being unecessary (only 25 mines added and all are non-coal)
# early_mines = read.csv("X:/Projects/Mining/NIOSH/analysis/data/1_converted/MSHA/mines_fromText.csv")

# read mines data
  # dataset downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp - 86135 obs, 59 vars
open_data_mines = read.table(open_data_mines_file_name, header = T, sep = "|")

# re-name variables
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

# create new variable to track data source
open_data_mines$datasource = "mines data"

# format mineid
open_data_mines$mineid = str_pad(open_data_mines$mineid, 7, pad = "0")

# save mine type info
mine_types = open_data_mines[, c(match("mineid", names(open_data_mines)), 
                                     match("minetype", names(open_data_mines)),
                                     match("coalcormetalmmine", names(open_data_mines)))]
saveRDS(mine_types, file = mine_types_file_name) 

######################################################################################################

# MERGE MINES AND EMPLOYMENT/PRODUCTION DATA, THEN CLEAN

# merge mines and employment/production data on mineid, year, and quarter - wind up with 125692 obs, 73 vars, 86135 unique mines
# drop observations missing for coalcormetalmmine
mines_quarters = merge(quarterly_employment, open_data_mines, by = c("mineid"), all = T)
mines_quarters = mines_quarters[!is.na(mines_quarters$coalcormetalmmine), ]
mines_quarters = merge(mines_quarters, yearly_employment, by = c("mineid", "year"), all = T)
mines_quarters = mines_quarters[!is.na(mines_quarters$coalcormetalmmine), ]
mines_quarters = merge(mines_quarters, underreporting_employment, by = c("mineid", "year", "quarter"), all = T)
mines_quarters = mines_quarters[!is.na(mines_quarters$coalcormetalmmine), ]

# drop data from environments not of interest - now we have 50890 obs, 73 vars, 14508 unique mines
  # (facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16)
mines_quarters = mines_quarters[mines_quarters$coalcormetalmmine == "C", ]
mines_quarters = mines_quarters[mines_quarters$minetype == "Underground", ]

# drop production variables that are not quarterly coal production - now we have 50890 obs, 71 vars
  # coal_prod_qtr is most reliable (spot-checked against MSHA mine retrieval system)
mines_quarters$coal_prod_yr = 
  mines_quarters$under_coal_prod_qtr = NULL

# track data source
mines_quarters$datasource = ifelse(is.na(mines_quarters$datasource), "emp/prod data", mines_quarters$datasource)

# create new variable: final quarterly employment
mines_quarters$final_employment_qtr = ifelse((mines_quarters$avg_employee_cnt_qtr == mines_quarters$under_avg_employee_cnt_qtr &
                                                mines_quarters$avg_employee_cnt_qtr != 0 & 
                                                !is.na(mines_quarters$under_avg_employee_cnt_qtr) &
                                                !is.na(mines_quarters$avg_employee_cnt_qtr)), mines_quarters$avg_employee_cnt_qt, NA)
mines_quarters$final_employment_qtr = ifelse((mines_quarters$avg_employee_cnt_qtr != 0 & 
                                                is.na(mines_quarters$under_avg_employee_cnt_qtr) &
                                                !is.na(mines_quarters$avg_employee_cnt_qtr)), mines_quarters$avg_employee_cnt_qt, mines_quarters$final_employment_qtr)
mines_quarters$final_employment_qtr = ifelse((mines_quarters$under_avg_employee_cnt_qtr != 0 & 
                                                !is.na(mines_quarters$under_avg_employee_cnt_qtr) &
                                                is.na(mines_quarters$avg_employee_cnt_qtr)), mines_quarters$under_avg_employee_cnt_qtr, mines_quarters$final_employment_qtr)

# if no quarterly data is available, use annual average employment
mines_quarters$final_employment_qtr = ifelse((mines_quarters$avg_employee_cnt_yr != 0 & 
                                                is.na(mines_quarters$final_employment_qtr) &
                                                !is.na(mines_quarters$avg_employee_cnt_yr)), mines_quarters$avg_employee_cnt_yr, mines_quarters$final_employment_qtr)

# last resort: use # of employees from the mines dataset (this is # employees as of minestatus date (not relevant to quarters))
mines_quarters$final_employment_qtr = ifelse(is.na(mines_quarters$final_employment_qtr), mines_quarters$numberofemployees, mines_quarters$final_employment_qtr)

# create new variable: quarterly hours
mines_quarters$final_hours_qtr = ifelse(!is.na(mines_quarters$hours_qtr), mines_quarters$hours_qtr, NA)
mines_quarters$final_hours_qtr = ifelse((is.na(mines_quarters$hours_qtr) &
                                           !is.na(mines_quarters$under_employee_hours_qtr)), mines_quarters$under_employee_hours_qtr, mines_quarters$final_hours_qtr)
mines_quarters$final_hours_qtr = ifelse((is.na(mines_quarters$final_hours_qtr) &
                                           !is.na(mines_quarters$hours_yr)), (mines_quarters$hours_yr / 4), mines_quarters$final_hours_qtr)

# drop variables other than "final" count - we still have 50890 obs, 66 vars, 14508 unique mines
mines_quarters$avg_employee_cnt_yr =
  mines_quarters$avg_employee_cnt_qtr =
  mines_quarters$numberofemployees =
  mines_quarters$under_avg_employee_cnt_qtr =
  mines_quarters$hours_qtr =
  mines_quarters$under_employee_hours_qtr =
  mines_quarters$hours_yr = NULL

# set employment/production/hours to NA for mine-observations for which there is no year/quarter data
mines_quarters$final_hours_qtr = ifelse((is.na(mines_quarters$year) & is.na(mines_quarters$quarter)), NA, mines_quarters$final_hours_qtr)
mines_quarters$coal_prod_qtr = ifelse((is.na(mines_quarters$year) & is.na(mines_quarters$quarter)), NA, mines_quarters$coal_prod_qtr)
mines_quarters$final_employment_qtr = ifelse((is.na(mines_quarters$year) & is.na(mines_quarters$quarter)), NA, mines_quarters$final_employment_qtr)

# rename variables (dropping "finals")
names(mines_quarters)[names(mines_quarters) == "final_hours_qtr"] = "hours_qtr"
names(mines_quarters)[names(mines_quarters) == "final_employment_qtr"] = "employment_qtr"

# format quarter and year variables for mine status date
mines_quarters$minestatusdate = as.Date(mines_quarters$minestatusdate, "%m/%d/%Y")
mines_quarters$statusquarter = as.yearqtr(mines_quarters$minestatusdate)
mines_quarters$statusyear = as.numeric(format(mines_quarters$minestatusdate, "%Y"))

# format date variables so quarter contains date-formatted year and quarter info
mines_quarters$quarter = paste(mines_quarters$year, mines_quarters$quarter, sep = "-")
mines_quarters$quarter = ifelse(mines_quarters$quarter == "NA-NA", NA, mines_quarters$quarter)
mines_quarters$quarter = as.yearqtr(mines_quarters$quarter)

# output mines data BEFORE dropping any observations base don missing data.
# this data is for merging on accidents - will be used in 3_merge_accidents.R
saveRDS(mines_quarters, file = all_mines_quarters_file_name)

######################################################################################################

# HERE WE DROP OBSERVATIONS THAT DON'T BELONG 

# drop data for which:
  # the hours/production file didn't merge with a mine
  # AND the minestatus is abandoned/nonproducing
  # AND the minestatus date is before 2000
mines_quarters$missing = ifelse((mines_quarters$minestatus == "Abandoned" | mines_quarters$minestatus == "Abandoned and Sealed" | 
                                   mines_quarters$minestatus == "NonProducing") & (mines_quarters$statusyear < 2000), 1, 0)
mines_quarters = mines_quarters[mines_quarters$missing == 0, ] # 12340 observations dropped, we now have 2257 unique mines

# drop observations without year/quarter data - we now have 1898 unique mines
mines_quarters = mines_quarters[!is.na(mines_quarters$year), ] # 359 observations dropped

# drop observations for mines that are abandoned or sealed when their status date comes before the current quarter - wind up with 37189 obs, 70 vars
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

# drop the data that is wacky in the mine data retrieval system
  # sometimes the employment looks like it should be zero, but sometimes the employment is positive (and the hours are wrong)
mines_quarters$missing = is.na(mines_quarters$employment_qtr)
mines_quarters = mines_quarters[mines_quarters$missing == 0, ] # 563 observations dropped, now we have 1801 unique mines

# remove mine-quarters with minestatus "Temporarily Idled" and zero hours/employment/production (these are nonproducing) - wind up with 36270 obs, 70 vars, 1796 unique mines
mines_quarters = mines_quarters[(mines_quarters$minestatus != "Temporarily Idled" | mines_quarters$hours_qtr != 0), ]
mines_quarters = mines_quarters[(mines_quarters$minestatus != "Temporarily Idled" | mines_quarters$employment_qtr != 0), ]
mines_quarters = mines_quarters[(mines_quarters$minestatus != "Temporarily Idled" | mines_quarters$coal_prod_qtr != 0), ]

# drop unnecessary variables - wind up with 36270 obs, 66 vars
mines_quarters$statusyear = 
  mines_quarters$statusquarter = 
  mines_quarters$drop = 
  mines_quarters$missing = NULL

######################################################################################################

# COLLAPSE MERGED DATA TO THE MINE-QUARTER LEVEL, THEN OUTPUT

# collapse data to mine-quarter level - wind up with 36270 obs, 18 vars
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

# drop observations with no hours - wind up with 30466 obs, 18 vars, 1587 unique mines
mines_quarters = mines_quarters[(mines_quarters$hours_qtr != 0), ]

# output mine-level data
saveRDS(mines_quarters, file = mines_quarters_file_name)
rm(list = ls())
gc()

######################################################################################################
