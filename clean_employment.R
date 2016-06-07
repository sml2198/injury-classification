library(plyr)

# THIS FILE BRINGS IN AND CLEANS UP EMPLOYMENT/PRODUCTION DATA, TO BE MERGED ONTO FINAL VIOLATIONS/ACCIDENTS/MINES DATASET.

employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdQuarterly.txt", header = T, sep = "|")
names(employment)[names(employment) == "CAL_YR"] = "calendaryear"
names(employment)[names(employment) == "CAL_QTR"] = "calendarquarter"
names(employment)[names(employment) == "MINE_ID"] = "mineid"
employment = employment[!(employment$SUBUNIT=="UNDERGROUND" & employment$COAL_METAL_IND=="C"),]
employment = employment[, c(-grep("COAL_METAL_IND", names(employment)), -match("STATE", names(employment)), 
                            -match("SUBUNIT", names(employment)), -match("SUBUNIT_CD", names(employment)))]
saveRDS(employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment.rds")

employment_yearly = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/MinesProdYearly.txt", header = T, sep = "|")
names(employment_yearly)[names(employment_yearly) == "CAL_YR"] = "calendaryear"
names(employment_yearly)[names(employment_yearly) == "MINE_ID"] = "mineid"
names(employment_yearly)[names(employment_yearly) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_yrly"
names(employment_yearly)[names(employment_yearly) == "AVG_EMPLOYEE_HOURS"] = "avg_employee_hours_yrly"
employment_yearly = employment_yearly[!(employment_yearly$SUBUNIT=="UNDERGROUND" & employment_yearly$COAL_METAL_IND=="C"),]
employment_yearly = employment_yearly[, c(-grep("COAL_METAL_IND", names(employment_yearly)), -match("STATE", names(employment_yearly)), 
                                          -match("SUBUNIT", names(employment_yearly)), -match("SUBUNIT_CD", names(employment_yearly)))]
saveRDS(employment_yearly, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment_yrly.rds")

contractor_employment = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdQuarterly.txt", header = T, sep = "|")
names(contractor_employment)[names(contractor_employment) == "CAL_YR"] = "calendaryear"
names(contractor_employment)[names(contractor_employment) == "CAL_QTR"] = "calendarquarter"
names(contractor_employment)[names(contractor_employment) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_employment)[names(contractor_employment) == "AVG_EMPLOYEE_CNT"] = "cntct_avg_employee_cnt"
names(contractor_employment)[names(contractor_employment) == "COAL_PRODUCTION"] = "cntct_coal_production"
contractor_employment = contractor_employment[!(contractor_employment$SUBUNIT=="UNDERGROUND" & contractor_employment$COAL_METAL_IND=="C"),]
contractor_employment = contractor_employment[, c(-grep("COAL_METAL_IND", names(contractor_employment)), -match("SUBUNIT", names(contractor_employment)), -match("SUBUNIT_CD", names(contractor_employment)))]
contractor_employment = ddply(contractor_employment[, c(match("insp_hours_per_qtr", names(contractor_employment)), match("onsite_insp_hours_per_qtr", names(contractor_employment)),
                                                    match("mineid", names(contractor_employment)), match("quarter", names(contractor_employment)))], c("mineid", "quarter"), 
                      function(x) colMeans(x[, c(match("insp_hours_per_qtr", names(x)), match("onsite_insp_hours_per_qtr", names(x)))], na.rm = T))
saveRDS(employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment.rds")

contractor_employment_yearly = read.table("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/ContractorProdYearly.txt", header = T, sep = "|")
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "CAL_YR"] = "calendaryear"
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "CONTRACTOR_ID"] = "contractorid"
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "AVG_EMPLOYEE_CNT"] = "cntct_avg_employee_cnt_yrly"
names(contractor_employment_yearly)[names(contractor_employment_yearly) == "ANNUAL_COAL_PRODUCTION"] = "cntct_annual_coal_production"
contractor_employment_yearly = contractor_employment_yearly[!(contractor_employment_yearly$SUBUNIT=="UNDERGROUND" & contractor_employment_yearly$COAL_METAL_IND=="C"),]
contractor_employment_yearly = contractor_employment_yearly[, c(-grep("COAL_METAL_IND", names(contractor_employment_yearly)), -match("SUBUNIT", names(contractor_employment_yearly)), 
                                                                -match("SUBUNIT_CD", names(contractor_employment_yearly)))]
saveRDS(contractor_employment_yearly, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_contractor_employment_yearly.rds")