
# THIS FILE BRINGS IN AND CLEANS UP EMPLOYMENT/PRODUCTION DATA, TO BE MERGED ONTO FINAL VIOLATIONS/ACCIDENTS/MINES DATASET.

employment = read.table("X:\Projects\Mining\NIOSH\analysis\data\0_originals\MSHA\open_data\MinesProdQuarterly.txt", header = T, sep = "|")
names(employment)[names(employment) == "cal_yr"] = "calendaryear"
names(employment)[names(employment) == "cal_qtr"] = "calendarquarter"
names(employment)[names(employment) == "mine_id"] = "mineid"

employment = employment[!(employment$subunit=="UNDERGROUND" & employment$coal_metal_ind=="C"),]
employment = employment[, c(-grep("coal_metal_ind", names(employment)), -match("state", names(employment)), -match("subunit", names(employment)), -match("subunit_cd", names(employment)))]

saveRDS(employment, file = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_employment.rds")
