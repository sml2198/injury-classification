#analyze_PS.R by Nikhil Saifullah 4/14/16

ps_data = read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/coded_sets/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv")
ps_data = ps_data[!is.na(ps_data$mineid),]
names(ps_data)[names(ps_data) == 'X'] = 'PS'
names(ps_data)[names(ps_data) == 'narrativemodified'] = 'narrative'
ps_data$PS = as.factor(ps_data$PS)

#How to destring a variable
ps_data[,grep("numberofemployees", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("numberofemployees", names(ps_data))] = as.numeric(ps_data[,grep("numberofemployees", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("methaneliberation", names(ps_data))] = as.numeric(ps_data[,grep("methaneliberation", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = gsub(pattern = ",",replacement =  "", ps_data[,grep("averagemineheight", names(ps_data))])
ps_data[,grep("averagemineheight", names(ps_data))] = as.numeric(ps_data[,grep("averagemineheight", names(ps_data))])

#Merge redundant 'not-found' values within variables. Note: Values like "Unknown" or "Other" are not funneled into "No Value Found"
ps_data[, "uglocation"] = ifelse(ps_data[, "uglocation"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "uglocation"])
ps_data[, "immediatenotificationclass"] = ifelse(ps_data[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", ps_data[, "immediatenotificationclass"])
ps_data[, "natureofinjury"] = ifelse(ps_data[, "natureofinjury"] == "UNCLASSIFIED,NOT DETERMED", "NO VALUE FOUND", ps_data[, "natureofinjury"])
ps_data[, "equipmanufacturer"] = ifelse(ps_data[, "equipmanufacturer"] == "Not Reported", "NO VALUE FOUND", ps_data[, "equipmanufacturer"])

#Convert date variables
indices_with_date = grep("date", names(ps_data))
for (i in indices_with_date) {
  ps_data[,i] = as.Date(ps_data[,i], "%m/%d/%Y")
}

# MERGE IN OTHER VARIABLES FROM PROTO-ALGORITHM (LIKE KEYWORD FLAGS) CHANGE TO KEEP ANY OTHER ADDTL VARS
ps_tngSet_intnl = read.csv("X:/Projects/Mining/NIOSH/analysis/data/training/training_set_1_internal.csv")
ps_data = merge(ps_data, ps_tngSet_intnl, c("mineid", "documentno"))
remove(ps_tngSet_intnl)
ps_data = ps_data[, c(-grep("\\.y", names(ps_data)))]
names(ps_data) = gsub("\\.[x|y]", "", names(ps_data))

ps_data[, "pinion"] = ifelse(grepl("(^| )pinion", ps_data[,"narrative"]), 1, 0)
ps_data[, "pinner"] = ifelse(grepl("(^| )pinner", ps_data[,"narrative"]), 1, 0)
ps_data[, "pin"] = ifelse(grepl("(^| )pin(n*)(e|i)[a-z]+", ps_data[,"narrative"]) & (ps_data[, "pinion"] == 1 | ps_data[, "pinner"] == 1), 1, 0)
ps_data[, "strike"] = ifelse(grepl("str(i|u)(.*)k[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "trap"] = ifelse(grepl("( )trap[a-z]*", ps_data[,"narrative"]), 1, 0)
ps_data[, "keyword"] = ifelse((ps_data[, "trap"] == 1 | ps_data[, "pin"] == 1 | ps_data[, "trap"] == 1), 1, 0)
ps_data = ps_data[, c(-grep("pinner", names(ps_data)), -grep("pinion", names(ps_data)))]

var_classes = sapply(ps_data[,names(ps_data)], class)
factor_vars = names(var_classes[grep("factor", var_classes)])
charac_vars = names(var_classes[grep("character", var_classes)])
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])
#We don't use date vars as of yet so no need to store a list of their names, "logical" class vars are missing all obsvtns
ps_data = ps_data[, -grep("logical", var_classes)]

for (i in length(charac_vars)) {
  ps_data[, charac_vars[i]] = ifelse((ps_data[,charac_vars[i]] == "NO VALUE FOUND" | ps_data[,charac_vars[i]] == "UNKNOWN" | ps_data[,charac_vars[i]] == "?"), NA_character_, ps_data[,charac_vars[i]])
}