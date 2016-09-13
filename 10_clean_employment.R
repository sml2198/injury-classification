# NIOSH Project 2014-N-15776

# 9 - Clean Employment Data
  # Read, clean, and ouput quarterly contractor employment data
  # Read, clean, and ouput annual contractor employment data

# Last edit 8/3/16

######################################################################################################

library(plyr)
library(zoo)

# define file names
  # input: quarterly contractor employment data
contractor_quarterly_employment_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdQuarterly.txt"
  # output: cleaned quarterly contractor employment data, collapsed to mine-quarter level
contractor_quarterly_employment_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds"
  # input: annual contractor employment data
contractor_yearly_employment_in_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdYearly.txt"
  # output: cleaned annual contractor employment data
contractor_yearly_employment_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds"

######################################################################################################

# READ AND CLEAN QUARTERLY CONTRACTOR EMPLOYMENT DATA, THEN OUTPUT

# read quarterly contractor employment data - 890,744 obs, 12 vars
  # dataset downloaded on 6/24/2016 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
  # this does not contain mineid's; can be merged into violations data by contractorid if and only if the contractor was cited at a mine
contractor_quarterly_employment = read.table(contractor_quarterly_employment_in_file_name, header = T, sep = "|")

# drop data from environments not of interest
contractor_quarterly_employment = contractor_quarterly_employment[(contractor_quarterly_employment$SUBUNIT == "UNDERGROUND" & 
                                                                     contractor_quarterly_employment$COAL_METAL_IND == "C"), ] # 28,818 obs

# drop unnecessary variables
contractor_quarterly_employment$COAL_METAL_IND = 
  contractor_quarterly_employment$SUBUNIT = 
  contractor_quarterly_employment$FISCAL_YR =
  contractor_quarterly_employment$FISCAL_QTR =
  contractor_quarterly_employment$SUBUNIT_CD =
  contractor_quarterly_employment$CONTRACTOR_NAME = NULL

# rename variables
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_YR"] = "year"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_QTR"] = "quarter"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_cnt_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "COAL_PRODUCTION"] = "con_coal_prod_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "HOURS_WORKED"] = "con_employee_hours_qtr"

# collapse to the mine-quarter level
contractor_quarterly_employment = ddply(contractor_quarterly_employment[, c(match("con_avg_employee_cnt_qtr", names(contractor_quarterly_employment)), 
                                                                            match("con_coal_prod_qtr", names(contractor_quarterly_employment)), 
                                                                            match("con_employee_hours_qtr", names(contractor_quarterly_employment)),
                                                                            match("contractorid", names(contractor_quarterly_employment)), 
                                                                            match("year", names(contractor_quarterly_employment)), 
                                                                            match("quarter", names(contractor_quarterly_employment)))], c("contractorid", "year", "quarter"), 
                                        function(x) colMeans(x[, c(match("con_avg_employee_cnt_qtr", names(x)), match("con_employee_hours_qtr", names(x)), 
                                                                   match("con_coal_prod_qtr", names(x)))], na.rm = T))

# format date variables
contractor_quarterly_employment$quarter = paste(contractor_quarterly_employment$year, contractor_quarterly_employment$quarter, sep = "-")
contractor_quarterly_employment$quarter = ifelse(contractor_quarterly_employment$quarter == "NA-NA", NA, contractor_quarterly_employment$quarter)
contractor_quarterly_employment$quarter = as.yearqtr(contractor_quarterly_employment$quarter)
contractor_quarterly_employment = contractor_quarterly_employment[, c(-grep("year", names(contractor_quarterly_employment)))]

# remove observations after our study period
contractor_quarterly_employment = contractor_quarterly_employment[contractor_quarterly_employment$quarter < "2016 Q2",]

# output clean quarterly contractor employment data - 28,487 obs, 5 vars
saveRDS(contractor_quarterly_employment, file = contractor_quarterly_employment_out_file_name)
rm(contractor_quarterly_employment)

######################################################################################################

# READ AND CLEAN ANNUAL CONTRACTOR EMPLOYMENT DATA, THEN OUTPUT

# read annual contractor employment data - 187,801 obs, 10 vars
  # dataset downloaded on 6/7/2016 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
contractor_yearly_employment = read.table(contractor_yearly_employment_in_file_name, header = T, sep = "|")

# drop data from environments not of interest
contractor_yearly_employment = contractor_yearly_employment[(contractor_yearly_employment$SUBUNIT == "UNDERGROUND" & 
                                                               contractor_yearly_employment$COAL_METAL_IND == "C"), ] # 5404 obs

# drop unnecessary variables
contractor_yearly_employment$COAL_METAL_IND = 
  contractor_yearly_employment$SUBUNIT =
  contractor_yearly_employment$CONTRACTOR_NAME =
  contractor_yearly_employment$SUBUNIT_CD = NULL

# rename variables
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CAL_YR"] = "year"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "con_employee_hours_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_COAL_PRODUCTION"] = "con_coal_prod_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_HOURS"] = "con_hours_yr"

# output clean annual contractor employment data - 5404 obs, 6 vars
saveRDS(contractor_yearly_employment, file = contractor_yearly_employment_out_file_name)
rm(contractor_yearly_employment)

######################################################################################################
