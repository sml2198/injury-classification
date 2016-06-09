##HEADER##

# THIS FILE READS IN THE MINES DATA, MERGES IN EMPLOYMENT/PRODUCTION/HOURS DATA, AND COLLAPSES TO THE MINE-QUARTER LEVEL. 

library(plyr)

# THIS FILE BRINGS IN AND CLEANS UP EMPLOYMENT/PRODUCTION DATA, TO BE MERGED ONTO FINAL VIOLATIONS/ACCIDENTS/MINES DATASET.
# IT THEN BRINGS IN THE MINES DATA AND COLLAPSES TO THE MINE-QUARTER LEVEL.S

######################################################################################################

# DATA FROM UNDERREPORTING - QUARTERLY MINE EMPLOYMENT/PRODUCTION DATA
underreporting_employment = read.table("X:/Projects/Mining/MSHA_OSHA_Underreporting/analysis/data/0_originals/MSHA/rec_2015_03_03/Operator_Quarterly_Emp_Production/Operator_Quarterly_Emp_Production.txt", fileEncoding="UCS-2LE", header = T, sep = "|")
names(underreporting_employment)[names(underreporting_employment) == "Production.Year"] = "year"
names(underreporting_employment)[names(underreporting_employment) == "Production.Quarter"] = "quarter"
names(underreporting_employment)[names(underreporting_employment) == "Mine.Id"] = "mineid"
names(underreporting_employment)[names(underreporting_employment) == "Employees"] = "under_avg_employee_cnt_qtr"
names(underreporting_employment)[names(underreporting_employment) == "Hours.Worked"] = "under_employee_hours_qtr"
names(underreporting_employment)[names(underreporting_employment) == "Coal.Production"] = "under_coal_prod_qtr"
underreporting_employment = underreporting_employment[underreporting_employment$Subunit=="UNDERGROUND",]
underreporting_employment = underreporting_employment[, c(-match("Subunit", names(underreporting_employment)), -match("Subunit.Code", names(underreporting_employment)))]
saveRDS(underreporting_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_underreporting_employment.rds")

# OPEN DATA - QUARTERLY MINE EMPLOYMENT/PRODUCTION
quarterly_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdQuarterly.txt", header = T, sep = "|")
names(quarterly_employment)[names(quarterly_employment) == "CAL_YR"] = "year"
names(quarterly_employment)[names(quarterly_employment) == "CAL_QTR"] = "quarter"
names(quarterly_employment)[names(quarterly_employment) == "MINE_ID"] = "mineid"
names(quarterly_employment)[names(quarterly_employment) == "HOURS_WORKED"] = "hours_qtr"
names(quarterly_employment)[names(quarterly_employment) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_qtr"
names(quarterly_employment)[names(quarterly_employment) == "COAL_PRODUCTION"] = "coal_prod_qtr"
quarterly_employment = quarterly_employment[quarterly_employment$SUBUNIT=="UNDERGROUND" & quarterly_employment$COAL_METAL_IND=="C",]
quarterly_employment = quarterly_employment[, c(-grep("COAL_METAL_IND", names(quarterly_employment)), -match("STATE", names(quarterly_employment)), -match("CURR_MINE_NM", names(quarterly_employment)), 
                            -match("FISCAL_YR", names(quarterly_employment)), -match("FISCAL_QTR", names(quarterly_employment)), 
                            -match("SUBUNIT", names(quarterly_employment)), -match("SUBUNIT_CD", names(quarterly_employment)))]
saveRDS(quarterly_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment.rds")

# OPEN DATA - ANNUAL MINE EMPLOYMENT/PRODUCTION
yearly_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdYearly.txt", header = T, sep = "|")
names(yearly_employment)[names(yearly_employment) == "CAL_YR"] = "year"
names(yearly_employment)[names(yearly_employment) == "MINE_ID"] = "mineid"
names(yearly_employment)[names(yearly_employment) == "MINE_NAME"] = "minename"
names(yearly_employment)[names(yearly_employment) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_yr"
names(yearly_employment)[names(yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "employee_hours_yr"
names(yearly_employment)[names(yearly_employment) == "ANNUAL_COAL_PRODUCTION"] = "coal_prod_yr"
names(yearly_employment)[names(yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "employee_hours_yr"
names(yearly_employment)[names(yearly_employment) == "ANNUAL_HOURS"] = "hours_yr"
yearly_employment = yearly_employment[yearly_employment$SUBUNIT=="UNDERGROUND" & yearly_employment$COAL_METAL_IND=="C",]
yearly_employment = yearly_employment[, c(-grep("COAL_METAL_IND", names(yearly_employment)), -match("STATE", names(yearly_employment)), 
                                          -match("SUBUNIT", names(yearly_employment)), -match("SUBUNIT_CD", names(yearly_employment)))]
saveRDS(yearly_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment_yrly.rds")

# OPEN DATA - QUARTERLY CONTRACTOR EMPLOYMENT/PRODUCTION
contractor_quarterly_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdQuarterly.txt", header = T, sep = "|")
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_YR"] = "year"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_QTR"] = "quarter"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_cnt_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "COAL_PRODUCTION"] = "con_coal_prod_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "HOURS_WORKED"] = "con_employee_hours_qtr"
contractor_quarterly_employment = contractor_quarterly_employment[contractor_quarterly_employment$SUBUNIT=="UNDERGROUND" & contractor_quarterly_employment$COAL_METAL_IND=="C",]
contractor_quarterly_employment = contractor_quarterly_employment[, c(-grep("COAL_METAL_IND", names(contractor_quarterly_employment)), -match("SUBUNIT", names(contractor_quarterly_employment)), 
                                                  -grep("FISCAL_YR", names(contractor_quarterly_employment)), -match("FISCAL_QTR", names(contractor_quarterly_employment)), 
                                                  -match("SUBUNIT_CD", names(contractor_quarterly_employment)), -match("CONTRACTOR_NAME", names(contractor_quarterly_employment)))]

contractor_quarterly_employment = ddply(contractor_quarterly_employment[, c(match("con_avg_employee_cnt_qtr", names(contractor_quarterly_employment)), 
                                                        match("con_coal_prod_qtr", names(contractor_quarterly_employment)), 
                                                        match("con_employee_hours_qtr", names(contractor_quarterly_employment)),
                                                        match("contractorid", names(contractor_quarterly_employment)), 
                                                        match("year", names(contractor_quarterly_employment)), 
                                                        match("quarter", names(contractor_quarterly_employment)))], c("contractorid", "year", "quarter"), 
                              function(x) colMeans(x[, c(match("con_avg_employee_cnt_qtr", names(x)), match("con_employee_hours_qtr", names(x)), 
                                                         match("con_coal_prod_qtr", names(x)))], na.rm = T))
saveRDS(contractor_quarterly_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds")

# OPEN DATA - ANNUAL CONTRACTOR EMPLOYMENT/PRODUCTION
contractor_yearly_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdYearly.txt", header = T, sep = "|")
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CAL_YR"] = "year"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "con_employee_hours_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_COAL_PRODUCTION"] = "con_coal_prod_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_HOURS"] = "con_hours_yr"
contractor_yearly_employment = contractor_yearly_employment[contractor_yearly_employment$SUBUNIT=="UNDERGROUND" & contractor_yearly_employment$COAL_METAL_IND=="C",]
contractor_yearly_employment = contractor_yearly_employment[, c(-grep("COAL_METAL_IND", names(contractor_yearly_employment)), 
                                                                -match("SUBUNIT", names(contractor_yearly_employment)), -match("CONTRACTOR_NAME", names(contractor_yearly_employment)), 
                                                                -match("SUBUNIT_CD", names(contractor_yearly_employment)))]
saveRDS(contractor_yearly_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds")

######################################################################################################

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
open_data_mines$datasource = "mines data"

######################################################################################################

# KEEP ONLY OBSERVATIONS FROM ENVIRONMENT OF INTEREST
mines_quarters = mines_quarters[!is.na(mines_quarters$coalcormetalmmine),]
mines_quarters = mines_quarters[mines_quarters$coalcormetalmmine == "C",]
mines_quarters = mines_quarters[mines_quarters$minetype != "Surface",]

# MERGE IN ALL EMPLOYMENT/PRODUCTION/HOURS DATATSETS
# observations that are missing for coalcormetalmmine need to be dropped anyway
mines_quarters = merge(quarterly_employment, open_data_mines, by = c("mineid"), all = T)
mines_quarters = mines_quarters[!is.na(mines_quarters$coalcormetalmmine),]

mines_quarters = merge(mines_quarters, yearly_employment, by = c("mineid", "year"), all = T)
mines_quarters = mines_quarters[!is.na(mines_quarters$coalcormetalmmine),]

mines_quarters = merge(mines_quarters, underreporting_employment, by = c("mineid", "year", "quarter"), all = T)
mines_quarters = mines_quarters[!is.na(mines_quarters$coalcormetalmmine),]

# make sure we have only kept obsevrations from our environment of interest
mines_quarters = mines_quarters[mines_quarters$coalcormetalmmine=="C",]
mines_quarters = mines_quarters[!(mines_quarters$minetype=="Surface"),]

mines_quarters$datasource = ifelse(is.na(mines_quarters$datasource), "emp/prod data", mines_quarters$datasource)

######################################################################################################

# QUARTERLY EMPLOYMENT
# construct final quarterly employment from three data sources
mines_quarters$final_employment_qtr = ifelse(mines_quarters$avg_employee_cnt_qtr == mines_quarters$under_avg_employee_cnt_qtr &
                                             mines_quarters$avg_employee_cnt_qtr != 0 & 
                                            !is.na(mines_quarters$under_avg_employee_cnt_qtr) &
                                            !is.na(mines_quarters$avg_employee_cnt_qtr), mines_quarters$avg_employee_cnt_qt, NA)
mines_quarters$final_employment_qtr = ifelse(mines_quarters$avg_employee_cnt_qtr != 0 & 
                                             is.na(mines_quarters$under_avg_employee_cnt_qtr) &
                                             !is.na(mines_quarters$avg_employee_cnt_qtr), mines_quarters$avg_employee_cnt_qt, mines_quarters$final_employment_qtr)
mines_quarters$final_employment_qtr = ifelse(mines_quarters$under_avg_employee_cnt_qtr != 0 & 
                                               !is.na(mines_quarters$under_avg_employee_cnt_qtr) &
                                               is.na(mines_quarters$avg_employee_cnt_qtr), mines_quarters$under_avg_employee_cnt_qtr, mines_quarters$final_employment_qtr)
# if no quarterly data is available, use annual average employment
mines_quarters$final_employment_qtr = ifelse(mines_quarters$avg_employee_cnt_yr != 0 & 
                                             is.na(mines_quarters$final_employment_qtr) &
                                             !is.na(mines_quarters$avg_employee_cnt_yr), mines_quarters$avg_employee_cnt_yr, mines_quarters$final_employment_qtr)
# only use # of employees from the mines dataset as a last resort - this is actually # employees as of minestatus date (not relevant to quarters)
mines_quarters$final_employment_qtr = ifelse(is.na(mines_quarters$final_employment_qtr), mines_quarters$numberofemployees, mines_quarters$final_employment_qtr)

######################################################################################################

# QUARTERLY COAL PRODUCTION
# make sure this var has no missings - missings should be zeros
mines_quarters$coal_prod_qtr = ifelse(is.na(mines_quarters$coal_prod_qtr), 0, mines_quarters$coal_prod_qtr)
# get rid of other prod vars - coal_prod_qtr is definitely the most reliable (spot-checked against MSHA mine retrieval system)
mines_quarters = mines_quarters[, c(-match("coal_prod_yr", names(mines_quarters)), 
                                    -match("under_coal_prod_qtr", names(mines_quarters)))]

######################################################################################################

# QUARTERLY HOURS
mines_quarters$final_hours_qtr = ifelse(!is.na(mines_quarters$hours_qtr), mines_quarters$hours_qtr, NA)
mines_quarters$final_hours_qtr = ifelse(is.na(mines_quarters$hours_qtr) &
                                       !is.na(mines_quarters$under_employee_hours_qtr), mines_quarters$under_employee_hours_qtr, mines_quarters$final_hours_qtr)
mines_quarters$final_hours_qtr = ifelse(is.na(mines_quarters$final_hours_qtr) &
                                       !is.na(mines_quarters$hours_yr), (mines_quarters$hours_yr/4), mines_quarters$final_hours_qtr)

# drop vars other than "final" count - will just confuse us 
mines_quarters = mines_quarters[, c(-match("avg_employee_cnt_yr", names(mines_quarters)), 
                                    -match("avg_employee_cnt_qtr", names(mines_quarters)), 
                                    -match("numberofemployees", names(mines_quarters)),
                                    -match("under_avg_employee_cnt_qtr", names(mines_quarters)),
                                    -match("hours_qtr", names(mines_quarters)),
                                    -match("under_employee_hours_qtr", names(mines_quarters)),
                                    -match("hours_yr", names(mines_quarters)))]

# There should be no employment/production/hours data for mine-observations for which we have no year/quarter data. Enforce this.
mines_quarters$final_hours_qtr = ifelse(is.na(mines_quarters$year) & is.na(mines_quarters$quarter), NA, mines_quarters$final_hours_qtr)
mines_quarters$coal_prod_qtr = ifelse(is.na(mines_quarters$year) & is.na(mines_quarters$quarter), NA, mines_quarters$coal_prod_qtr)
mines_quarters$final_employment_qtr = ifelse(is.na(mines_quarters$year) & is.na(mines_quarters$quarter), NA, mines_quarters$final_employment_qtr)

# Rename to get rid of "finals"
names(mines_quarters)[names(mines_quarters) == "final_hours_qtr"] = "hours_qtr"
names(mines_quarters)[names(mines_quarters) == "final_employment_qtr"] = "employment_qtr"

######################################################################################################
# collapse to mine-quarter level
temp = mines_quarters[, c(match("mineid", names(mines_quarters)), 
                          match("year", names(mines_quarters)), 
                          match("quarter", names(mines_quarters)),
                          match("minetype", names(mines_quarters)),
                          match("minename", names(mines_quarters)),
                          match("minestatus", names(mines_quarters)),
                          match("minestatusdate", names(mines_quarters)),                          
                          match("operatorid", names(mines_quarters)),
                          match("operatorname", names(mines_quarters)),         
                          match("stateabbreviation", names(mines_quarters)),
                          match("idate", names(mines_quarters)),  
                          match("idesc", names(mines_quarters)),  
                          match("daysperweek", names(mines_quarters)),  
                          match("productionshiftsperday", names(mines_quarters)))]

mines_quarters = ddply(mines_quarters[, c(match("hours_qtr", names(mines_quarters)), match("employment_qtr", names(mines_quarters)), match("coal_prod_qtr", names(mines_quarters)), 
                                            match("mineid", names(mines_quarters)), match("year", names(mines_quarters)), match("quarter", names(mines_quarters)))], c("mineid", "year", "quarter"), 
                      function(x) colMeans(x[, c(match("hours_qtr", names(x)), match("employment_qtr", names(x)), match("coal_prod_qtr", names(x)))], na.rm = T))
mines_quarters = merge(mines_quarters, temp, by = c("mineid", "year", "quarter"), all = T)
rm(temp)
rm(open_data_mines)

# save
saveRDS(mines_quarters, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_mines.rds")

######################################################################################################
