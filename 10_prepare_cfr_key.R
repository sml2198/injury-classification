##NIOSH STUDY##
##Professor Alison Morantz##
##Stanford Law School##

#Coded by Nikhil Saifullah

#Description

#Coded by Nikhil Saifullah

#In this file we code all violations as either "relevant" or "maybe relevant" to the cause of
#pinning and striking (PS) and maintenance and repair (MR) injuries, respectively. Violation
#codes not flagged as "relevant" or "maybe relevant" can be regarded as irrelevant. These codings
#came from a meeting between Alison Morantz, Nikhil Saifullah, and Sarah Levine at NIOSH in
#Pittsburgh in February of 2016.


library(stringr)

cfr_key = read.csv("X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/cfr_key/cfr_key.csv")

cfr_key$cfr_section_code_desc = as.character(cfr_key$cfr_section_code_desc)
subsection_code_length = attr(regexpr("[0-9]+\\.[0-9]+(-)*[0-9]*", cfr_key$cfr_section_code_desc), "match.length")
cfr_key$subsection_code = substr(cfr_key$cfr_section_code_desc, 2, 1+subsection_code_length)

##PINNING & STRIKING##

cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "48" & (cfr_key$cfr_subpart_code %in% c("Subpart A", "Subpart B")) & 
                               !(cfr_key$subsection_code %in% c("48.10", "48.3", "48.9", "48.23", "48.29", "48.30", "48.31", "48.32")), 1, 0)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart B")) & 
                               (cfr_key$subsection_code %in% c("75.100", "75.153", "75.155", "75.156")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart C")) & 
                               (cfr_key$subsection_code %in% c("75.203", "75.204", "75.205", "75.207", "75.208", "75.209", "75.212", "75.213", "75.215")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart D")) & 
                               (cfr_key$subsection_code %in% c("75.332", "75.334", "75.340")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart F")) & 
                               (cfr_key$subsection_code %in% c("75.500", "75.500-1", "75.501", "75.501-2", "75.502", "75.503", "75.505", "75.506-1", "75.507")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart F")) & 
                               (cfr_key$subsection_code %in% c("75.507-1", "75.508-1", "75.509", "75.510", "75.512-1", "75.523", "75.523-3", "75.524")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart G")) & 
                               (cfr_key$subsection_code %in% c("75.602", "75.603", "75.604", "75.605", "75.606", "75.607")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart H")) & 
                               (cfr_key$subsection_code %in% c("75.703-3", "75.703-4")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart I")) & 
                               (cfr_key$subsection_code %in% c("75.807", "75.810", "75.811", "75.812")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart I")) & 
                               (cfr_key$subsection_code %in% c("75.816", "75.817")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart J")) & 
                               (cfr_key$subsection_code %in% c("75.906")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart K")) & 
                               (cfr_key$subsection_code %in% c("75.1002", "75.1003", "75.1003-2")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart N")) & 
                               (cfr_key$subsection_code %in% c("75.1311")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart O")) & 
                               (cfr_key$subsection_code %in% c("75.1400", "75.1400-1", "75.1403-10", "75.1403-6", "75.1403-7", "75.1403-8", "75.1404", "75.1404-1", "75.1405")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart O")) & 
                               (cfr_key$subsection_code %in% c("75.1405-1")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart R")) & 
                               (cfr_key$subsection_code %in% c("75.1719-2", "75.1719-4", "75.1720", "75.1725")), 1, cfr_key$PS_relevant)
cfr_key$PS_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart T")) & 
                               (cfr_key$subsection_code %in% c("75.1906", "75.1916")), 1, cfr_key$PS_relevant)

cfr_key$PS_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart B")) & 
                                     (cfr_key$subsection_code %in% c("75.160")), 1, 0)
cfr_key$PS_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart D")) & 
                                     (cfr_key$subsection_code %in% c("75.327", "75.337", "75.343", "75.373", "75.385", "75.388", "75.389")), 1, cfr_key$PS_maybe_relevant)
cfr_key$PS_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart N")) & 
                                     (cfr_key$subsection_code %in% c("75.1315", "75.1316", "75.1318", "75.1322")), 1, cfr_key$PS_maybe_relevant)
cfr_key$PS_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart O")) & 
                                     (cfr_key$subsection_code %in% c("75.1403-5")), 1, cfr_key$PS_maybe_relevant)

##MAINTENANCE & REPAIR##

cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "48" & (cfr_key$cfr_subpart_code %in% c("Subpart A", "Subpart B")) & 
                               !(cfr_key$subsection_code %in% c("48.10", "48.3", "48.9", "48.23", "48.29", "48.30", "48.31", "48.32")), 1, 0)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "71" & (cfr_key$cfr_subpart_code %in% c("Subpart H")) & 
                               (cfr_key$subsection_code %in% c("71.701")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "72" & (cfr_key$cfr_subpart_code %in% c("Subpart E")) & 
                               (cfr_key$subsection_code %in% c("72.610", "72.620", "72.630")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart B")) & 
                               (cfr_key$subsection_code %in% c("75.100", "75.150", "75.151", "75.153", "75.155", "75.156")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart C")) & 
                               (cfr_key$subsection_code %in% c("75.202", "75.208", "75.211", "75.212")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart D")) & 
                               (cfr_key$subsection_code %in% c("75.312", "75.320", "75.324", "75.340", "75.341", "75.342", "75.344", "75.352", "75.382")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart F")) & 
                               (cfr_key$subsection_code %in% c("75.503", "75.504", "75.505", "75.506", "75.506-1", "75.507")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart F")) & 
                               (cfr_key$subsection_code %in% c("75.510-1", "75.511", "75.511-1", "75.512", "75.512-1", "75.512-2", "75.513", "75.513-1", "75.514")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart F")) & 
                               (cfr_key$subsection_code %in% c("75.515", "75.516", "75.516-1", "75.516-2", "75.517", "75.517-1", "75.518", "75.518-1")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart F")) & 
                               (cfr_key$subsection_code %in% c("75.519", "75.519-1", "75.520", "75.523", "75.523-1", "75.523-2")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart G")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart H")) & 
                               (cfr_key$subsection_code %in% c("75.700", "75.700-1", "75.701", "75.701-1", "75.701-2", "75.701-3", "75.701-4", "75.701-5")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart H")) & 
                               (cfr_key$subsection_code %in% c("75.702", "75.702-1", "75.703", "75.703-1", "75.703-2", "75.703-3", "75.703-4", "75.704", "75.704-1")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart H")) & 
                               (cfr_key$subsection_code %in% c("75.705", "75.705-1", "75.705-2", "75.705-3", "75.705-6", "75.705-8", "75.705-9", "75.706")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart I")) & 
                               (cfr_key$subsection_code %in% c("75.800", "75.800-2", "75.800-3", "75.800-4", "75.801", "75.802", "75.803", "75.803-2", "75.804")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart I")) & 
                               (cfr_key$subsection_code %in% c("75.805", "75.806", "75.807", "75.808", "75.809", "75.810", "75.811", "75.812")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart I")) & 
                               (cfr_key$subsection_code %in% c("75.814", "75.815", "75.816", "75.818", "75.819", "75.820", "75.821", "75.825", "75.827")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart I")) & 
                               (cfr_key$subsection_code %in% c("75.831", "75.832", "75.834", "75.828", "75.829")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart J")) & 
                               (cfr_key$subsection_code %in% c("75.900", "75.900-2", "75.900-3", "75.900-4", "75.901", "75.902", "75.902-1", "75.902-2", "75.902-4")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart J")) & 
                               (cfr_key$subsection_code %in% c("75.903", "75.904", "75.905", "75.907")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart K")) & 
                               (cfr_key$subsection_code %in% c("75.1001", "75.1001-1", "75.1003-1")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart L")) & 
                               (cfr_key$subsection_code %in% c("75.1100-2", "75.1103-4", "75.1104", "75.1106", "75.1106-2", "75.1106-3", "75.1106-4", "75.1106-5", "75.1106-6")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart L")) & 
                               (cfr_key$subsection_code %in% c("75.1107-14")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart O")) & 
                               (cfr_key$subsection_code %in% c("75.1400", "75.1400-1", "75.1400-2", "75.1400-3", "75.1400-4", "75.1401", "75.1401-1", "75.1402-2", "75.1403-10",
                                                               "75.1403-11", "75.1403-2", "75.1403-3", "75.1403-4", "75.1403-5", "75.1403-6", "75.1403-7", "75.1403-8", "75.1403-9",
                                                               "75.1404", "75.1404-1", "75.1405", "75.1405-1", "75.1431", "75.1432", "75.1433", "75.1434", "75.1435",
                                                               "75.1436", "75.1437", "75.1438")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart Q")) & 
                               (cfr_key$subsection_code %in% c("75.1600-2")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart R")) & 
                               (cfr_key$subsection_code %in% c("75.1712-10", "75.1712-6", "75.1720", "75.1721", "75.1725", "75.1726", "75.1727", "75.1728", "75.1729",
                                                               "75.1730", "75.1731")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart T")) & 
                               (cfr_key$subsection_code %in% c("75.1903", "75.1909", "75.1910", "75.1913", "75.1914", "75.1915")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart B")) & 
                               (cfr_key$subsection_code %in% c("77.103", "77.104")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart C")) & 
                               (cfr_key$subsection_code %in% c("77.200", "77.202", "77.203", "77.204", "77.205", "77.206", "77.207", "77.208", "77.210",
                                                               "77.216")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart D")) & 
                               (cfr_key$subsection_code %in% c( "77.305", "77.309", "77.311", "77.314", "77.315")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart E", "Subpart F", "Subpart G", "Subpart H", "Subpart I", "Subpart J")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart L")) & 
                               (cfr_key$subsection_code %in% c("77.1103", "77.1104", "77.1111", "77.1112")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart O")) & 
                               (cfr_key$subsection_code %in% c("77.1403", "77.1405", "77.1431", "77.1432", "77.1433", "77.1434", "77.1435", "77.1436", "77.1437",
                                                               "77.1438")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart Q")) & 
                               (cfr_key$subsection_code %in% c("77.1605", "77.1606")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart R")) & 
                               (cfr_key$subsection_code %in% c("77.1710")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart S")) & 
                               (cfr_key$subsection_code %in% c("77.1800", "77.1801", "77.1802")), 1, cfr_key$MR_relevant)
cfr_key$MR_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart T")) & 
                               (cfr_key$subsection_code %in% c("77.1906", "77.1915", "77.1916")), 1, cfr_key$MR_relevant)

cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "47" & (cfr_key$cfr_subpart_code %in% c("Subpart E")), 1, 0)
cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "72" & (cfr_key$cfr_subpart_code %in% c("Subpart D")) &
                                     (cfr_key$subsection_code %in% c("72.503")), 1, cfr_key$MR_maybe_relevant)
cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart B")) &
                                     (cfr_key$subsection_code %in% c("75.160")), 1, cfr_key$MR_maybe_relevant)
cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart C")) &
                                     (cfr_key$subsection_code %in% c("75.214")), 1, cfr_key$MR_maybe_relevant)
cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart D")) &
                                     (cfr_key$subsection_code %in% c("75.337")), 1, cfr_key$MR_maybe_relevant)
cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart L")) &
                                     (cfr_key$subsection_code %in% c("75.1101-20", "75.1102")), 1, cfr_key$MR_maybe_relevant)
cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "75" & (cfr_key$cfr_subpart_code %in% c("Subpart T")) &
                                     (cfr_key$subsection_code %in% c("75.1911", "75.1912")), 1, cfr_key$MR_maybe_relevant)
cfr_key$MR_maybe_relevant = ifelse(cfr_key$cfr_part_code == "77" & (cfr_key$cfr_subpart_code %in% c("Subpart L")) &
                                     (cfr_key$subsection_code %in% c("77.1106")), 1, cfr_key$MR_maybe_relevant)

#cfr_key = cfr_key[, c(grep("relevant", names(cfr_key)), grep("subsection_code", names(cfr_key)))]

saveRDS(cfr_key, file = "X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_cfr_key.rds")
rm(subsection_code_length, cfr_key)
