library(plyr)

# THIS FILE BRINGS IN AND CLEANS UP EMPLOYMENT/PRODUCTION DATA, TO BE MERGED ONTO FINAL VIOLATIONS/ACCIDENTS/MINES DATASET.

# DATA FROM UNDERREPORTING - QUARTERLY MINE EMPLOYMENT/PRODUCTION DATA
under_employment = read.table("X:/Projects/Mining/MSHA_OSHA_Underreporting/analysis/data/0_originals/MSHA/rec_2015_03_03/Operator_Quarterly_Emp_Production/Operator_Quarterly_Emp_Production.txt", fileEncoding="UCS-2LE", header = T, sep = "|")
names(under_employment)[names(under_employment) == "Production.Year"] = "year"
names(under_employment)[names(under_employment) == "Production.Quarter"] = "quarter"
names(under_employment)[names(under_employment) == "Mine.Id"] = "mineid"
names(under_employment)[names(under_employment) == "Employees"] = "under_avg_employee_cnt_qtr"
names(under_employment)[names(under_employment) == "Hours.Worked"] = "under_employee_hours_qtr"
names(under_employment)[names(under_employment) == "Coal.Production"] = "under_coal_prod_qtr"
under_employment = under_employment[under_employment$Subunit=="UNDERGROUND",]
under_employment = under_employment[, c(-match("Subunit", names(under_employment)), -match("Subunit.Code", names(under_employment)))]
saveRDS(under_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_underreporting_employment.rds")

# OPEN DATA - QUARTERLY MINE EMPLOYMENT/PRODUCTION
employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdQuarterly.txt", header = T, sep = "|")
names(employment)[names(employment) == "CAL_YR"] = "year"
names(employment)[names(employment) == "CAL_QTR"] = "quarter"
names(employment)[names(employment) == "MINE_ID"] = "mineid"
names(employment)[names(employment) == "HOURS_WORKED"] = "hours_qtr"
names(employment)[names(employment) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_qtr"
names(employment)[names(employment) == "COAL_PRODUCTION"] = "coal_prod_qtr"
employment = employment[employment$SUBUNIT=="UNDERGROUND" & employment$COAL_METAL_IND=="C",]
employment = employment[, c(-grep("COAL_METAL_IND", names(employment)), -match("STATE", names(employment)), -match("CURR_MINE_NM", names(employment)), 
                            -match("FISCAL_YR", names(employment)), -match("FISCAL_QTR", names(employment)), 
                            -match("SUBUNIT", names(employment)), -match("SUBUNIT_CD", names(employment)))]
saveRDS(employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment.rds")

# OPEN DATA - ANNUAL MINE EMPLOYMENT/PRODUCTION
employment_yearly = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdYearly.txt", header = T, sep = "|")
names(employment_yearly)[names(employment_yearly) == "CAL_YR"] = "year"
names(employment_yearly)[names(employment_yearly) == "MINE_ID"] = "mineid"
names(employment_yearly)[names(employment_yearly) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_yr"
names(employment_yearly)[names(employment_yearly) == "AVG_EMPLOYEE_HOURS"] = "employee_hours_yr"
names(employment_yearly)[names(employment_yearly) == "ANNUAL_COAL_PRODUCTION"] = "coal_prod_yr"
employment_yearly = employment_yearly[employment_yearly$SUBUNIT=="UNDERGROUND" & employment_yearly$COAL_METAL_IND=="C",]
employment_yearly = employment_yearly[, c(-grep("COAL_METAL_IND", names(employment_yearly)), -match("STATE", names(employment_yearly)), 
                                          -match("SUBUNIT", names(employment_yearly)), -match("SUBUNIT_CD", names(employment_yearly)))]
saveRDS(employment_yearly, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment_yrly.rds")

# OPEN DATA - QUARTERLY CONTRACTOR EMPLOYMENT/PRODUCTION
contractor_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdQuarterly.txt", header = T, sep = "|")
names(contractor_employment)[names(contractor_employment) == "CAL_YR"] = "year"
names(contractor_employment)[names(contractor_employment) == "CAL_QTR"] = "quarter"
names(contractor_employment)[names(contractor_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_employment)[names(contractor_employment) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_cnt_qtr"
names(contractor_employment)[names(contractor_employment) == "COAL_PRODUCTION"] = "con_coal_prod_qtr"
names(contractor_employment)[names(contractor_employment) == "HOURS_WORKED"] = "con_employee_hours_qtr"
contractor_employment = contractor_employment[contractor_employment$SUBUNIT=="UNDERGROUND" & contractor_employment$COAL_METAL_IND=="C",]
contractor_employment = contractor_employment[, c(-grep("COAL_METAL_IND", names(contractor_employment)), -match("SUBUNIT", names(contractor_employment)), 
                                                  -grep("FISCAL_YR", names(contractor_employment)), -match("FISCAL_QTR", names(contractor_employment)), 
                                                  -match("SUBUNIT_CD", names(contractor_employment)), -match("CONTRACTOR_NAME", names(contractor_employment)))]

contractor_employment = ddply(contractor_employment[, c(match("con_avg_employee_cnt_qtr", names(contractor_employment)), 
                                                        match("con_coal_prod_qtr", names(contractor_employment)), 
                                                        match("con_employee_hours_qtr", names(contractor_employment)),
                                                        match("contractorid", names(contractor_employment)), 
                                                        match("year", names(contractor_employment)), 
                                                        match("quarter", names(contractor_employment)))], c("contractorid", "year", "quarter"), 
                      function(x) colMeans(x[, c(match("con_avg_employee_cnt_qtr", names(x)), match("con_employee_hours_qtr", names(x)), 
                                                 match("con_coal_prod_qtr", names(x)))], na.rm = T))
saveRDS(contractor_employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds")

# OPEN DATA - ANNUAL CONTRACTOR EMPLOYMENT/PRODUCTION
contractor_employment_yearly = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdYearly.txt", header = T, sep = "|")
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "CAL_YR"] = "year"
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "AVG_EMPLOYEE_CNT"] = "con_avg_employee_yr"
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "AVG_EMPLOYEE_HOURS"] = "con_employee_hours_yr"
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "ANNUAL_COAL_PRODUCTION"] = "con_coal_prod_yr"
contractor_employment_yearly = contractor_employment_yearly[contractor_employment_yearly$SUBUNIT=="UNDERGROUND" & contractor_employment_yearly$COAL_METAL_IND=="C",]
contractor_employment_yearly = contractor_employment_yearly[, c(-grep("COAL_METAL_IND", names(contractor_employment_yearly)), 
                                                                -match("SUBUNIT", names(contractor_employment_yearly)), -match("CONTRACTOR_NAME", names(contractor_employment_yearly)), 
                                                                -match("SUBUNIT_CD", names(contractor_employment_yearly)))]
saveRDS(contractor_employment_yearly, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds")






