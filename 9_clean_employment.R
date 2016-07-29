# NIOSH Project 2014-N-15776

# 9 - Clean Employment
# Bring in contractor data from MSHA open data
# Clean up variable names, remove unnecessary vars
# Collapse to mine-quarter level and output

# Last edit 7/29/16

######################################################################################################

# define file names
  # input: quarterly contractor employment, from MSHA open data on 6/24/2016 @ 9:28 AM
contractor_quarterly_employment.in.file.name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdQuarterly.txt"
# output: cleaned contractor employment by quarter, collapsed to mine-quarter level
  contractor_quarterly_employment.out.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds"
  # input: annual contractor employment, from MSHA open data on 6/7/2016 @ 4:05 PM
contractor_yearly_employment.in.file.name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdYearly.txt"
  # output: cleaned contractor employment by year
contractor_yearly_employment.out.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds"

######################################################################################################

# BRING IN CONTRACTOR DATA - WILL BE MERGED ON CONTRACTOR ID 

# Open data - quarterly contractor employment/production
# This file does not contain mineid's - only contractorid's. Can be merged into violations data by contractorid 
# if and only if the contractor was cited at a mine.
contractor_quarterly_employment = read.table(contractor_quarterly_employment.in.file.name, header = T, sep = "|")

# Rename and clean up variables
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_YR"] = "year"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CAL_QTR"] = "quarter"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_cnt_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "COAL_PRODUCTION"] = "con_coal_prod_qtr"
names(contractor_quarterly_employment)[names(contractor_quarterly_employment) == "HOURS_WORKED"] = "con_employee_hours_qtr"
contractor_quarterly_employment = contractor_quarterly_employment[contractor_quarterly_employment$SUBUNIT=="UNDERGROUND" 
                                                                  & contractor_quarterly_employment$COAL_METAL_IND=="C",]
contractor_quarterly_employment = contractor_quarterly_employment[, c(-grep("COAL_METAL_IND", names(contractor_quarterly_employment)), 
                                                                      -match("SUBUNIT", names(contractor_quarterly_employment)), 
                                                                      -grep("FISCAL_YR", names(contractor_quarterly_employment)), 
                                                                      -match("FISCAL_QTR", names(contractor_quarterly_employment)), 
                                                                      -match("SUBUNIT_CD", names(contractor_quarterly_employment)), 
                                                                      -match("CONTRACTOR_NAME", names(contractor_quarterly_employment)))]

# Collapse to the mine-quarter level
contractor_quarterly_employment = ddply(contractor_quarterly_employment[, c(match("con_avg_employee_cnt_qtr", names(contractor_quarterly_employment)), 
                                                                            match("con_coal_prod_qtr", names(contractor_quarterly_employment)), 
                                                                            match("con_employee_hours_qtr", names(contractor_quarterly_employment)),
                                                                            match("contractorid", names(contractor_quarterly_employment)), 
                                                                            match("year", names(contractor_quarterly_employment)), 
                                                                            match("quarter", names(contractor_quarterly_employment)))], c("contractorid", "year", "quarter"), 
                                        function(x) colMeans(x[, c(match("con_avg_employee_cnt_qtr", names(x)), match("con_employee_hours_qtr", names(x)), 
                                                                   match("con_coal_prod_qtr", names(x)))], na.rm = T))

# Format date vars so that "quarter" contains date-formatted year AND quarter info
contractor_quarterly_employment$quarter = paste(contractor_quarterly_employment$year, contractor_quarterly_employment$quarter, sep= "-")
contractor_quarterly_employment$quarter = ifelse(contractor_quarterly_employment$quarter == "NA-NA", NA, contractor_quarterly_employment$quarter)
contractor_quarterly_employment$quarter = as.yearqtr(contractor_quarterly_employment$quarter)
contractor_quarterly_employment = contractor_quarterly_employment[, c(-grep("year", names(contractor_quarterly_employment)))]

saveRDS(contractor_quarterly_employment, file = contractor_quarterly_employment.out.file.name)

# Open data - annual contractor employment/production
contractor_yearly_employment = read.table(contractor_yearly_employment.in.file.name, header = T, sep = "|")

# Rename and clean up variables
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CAL_YR"] = "year"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "AVG_EMPLOYEE_HOURS"] = "con_employee_hours_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_COAL_PRODUCTION"] = "con_coal_prod_yr"
names(contractor_yearly_employment)[names(contractor_yearly_employment) == "ANNUAL_HOURS"] = "con_hours_yr"
contractor_yearly_employment = contractor_yearly_employment[contractor_yearly_employment$SUBUNIT=="UNDERGROUND" 
                                                            & contractor_yearly_employment$COAL_METAL_IND=="C",]
contractor_yearly_employment = contractor_yearly_employment[, c(-grep("COAL_METAL_IND", names(contractor_yearly_employment)), 
                                                                -match("SUBUNIT", names(contractor_yearly_employment)), 
                                                                -match("CONTRACTOR_NAME", names(contractor_yearly_employment)), 
                                                                -match("SUBUNIT_CD", names(contractor_yearly_employment)))]

# Save
saveRDS(contractor_yearly_employment, file = contractor_yearly_employment.out.file.name)

######################################################################################################
