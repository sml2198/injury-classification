# NIOSH Project 2014-N-15776

# analyze_RI_method_1.R
# Inspect Randomization Inference (Method 1) Output

# Last edit 12/07/16 - SML

######################################################################################################

library(foreign)

# set preferences
date = "12-6/"

# WHICH MODELS DO YOU WANT TO RUN?

  injtype = "PS"
  # injtype = "MR"
  
  # subpart.form = "rate"
  subpart.form = "not-a-rate"
  
  # specification.test = "on" # analyze results from models with union & longwall indicators
  specification.test = "off"

  lag_3 = "on" # cannot be on at same time as ulw specification test. will also run lag 5.
  # lag_3 = "off"
  
  lag_5 = "on" # cannot be on at same time as ulw specification test. will also run lag 3.
  # lag_5 = "off"
  
# WHAT DO YOU WANT TO DO WITH THIS SAMPLE?
  
  # analyze.method.1 = "on" # analyze method 1, spit out csvs of robustly significant subparts (p < 0.05)
  analyze.method.1 = "off"
  
  # append.models = "on" # make a csv of each model set appended - cannot be done if analyzing method 1 
  if (analyze.method.1 == "on") {
    append.models = "off"
  }
  if (analyze.method.1 == "off") {
    append.models = "on"
  }
  
######################################################################################################

# DEFINE DTA AND CSV ROOTS
  
if (specification.test == "on") {
  file_ext = "_ulw"
}
if (specification.test != "on") {
  file_ext = ""
}
  
dtaroot = paste0("X:/Projects/Mining/NIOSH/analysis/results/dta/", date, collapse = NULL)
csvroot = paste0("X:/Projects/Mining/NIOSH/analysis/results/csv/", date, collapse = NULL)
  
if (specification.test == "on") {
  dtaroot = paste0("X:/Projects/Mining/NIOSH/analysis/results/dta/", date, "ulw/", collapse = NULL)
  csvroot = paste0("X:/Projects/Mining/NIOSH/analysis/results/csv/", date, "ulw/", collapse = NULL)
}
if (lag_3 == "on") {
  dtaroot3 = paste0("X:/Projects/Mining/NIOSH/analysis/results/dta/", date, "lag_3/", collapse = NULL)
  csvroot3 = paste0("X:/Projects/Mining/NIOSH/analysis/results/csv/", date, "lag_3/", collapse = NULL)
}
if (lag_5 == "on") {
  dtaroot5 = paste0("X:/Projects/Mining/NIOSH/analysis/results/dta/", date, "lag_5/", collapse = NULL)
  csvroot5 = paste0("X:/Projects/Mining/NIOSH/analysis/results/csv/", date, "lag_5/", collapse = NULL)
}

######################################################################################################

# NAME CSVS WITH LISTS OF SIGNIFICANT VARIABLES FROM PREFERRED MODELS

if (subpart.form == "rate" & lag_3 == "off" & lag_5 == "off") {
  B_1_sig = paste0(csvroot, injtype, "_B_sp_1_sig", file_ext, ".csv", collapse = NULL)
  B_4_sig = paste0(csvroot, injtype, "_B_sp_4_sig", file_ext, ".csv", collapse = NULL)
  C_1_sig = paste0(csvroot, injtype, "_C_sp_1_sig", file_ext, ".csv", collapse = NULL)
  C_4_sig = paste0(csvroot, injtype, "_C_sp_4_sig", file_ext, ".csv", collapse = NULL)
}
if (subpart.form == "not-a-rate" & lag_3 == "off" & lag_5 == "off") {
  B_1_sig = paste0(csvroot, injtype, "_B_sp_1_non-rate_sig", file_ext, ".csv", collapse = NULL)
  B_4_sig = paste0(csvroot, injtype, "_B_sp_4_non-rate_sig", file_ext, ".csv", collapse = NULL)
  C_1_sig = paste0(csvroot, injtype, "_C_sp_1_non-rate_sig", file_ext, ".csv", collapse = NULL)
  C_4_sig = paste0(csvroot, injtype, "_C_sp_4_non-rate_sig", file_ext, ".csv", collapse = NULL)
}
if (subpart.form == "rate" & (lag_3 == "on" | lag_5 == "on")) {
  B_1_sig = paste0(csvroot3, injtype, "_B_sp_3_sig", file_ext, ".csv", collapse = NULL)
  B_4_sig = paste0(csvroot5, injtype, "_B_sp_5_sig", file_ext, ".csv", collapse = NULL)
  C_1_sig = paste0(csvroot3, injtype, "_C_sp_3_sig", file_ext, ".csv", collapse = NULL)
  C_4_sig = paste0(csvroot5, injtype, "_C_sp_5_sig", file_ext, ".csv", collapse = NULL)
}
if (subpart.form == "not-a-rate" & (lag_3 == "on" | lag_5 == "on")) {
  B_1_sig = paste0(csvroot3, injtype, "_B_sp_3_non-rate_sig", file_ext, ".csv", collapse = NULL)
  B_4_sig = paste0(csvroot5, injtype, "_B_sp_5_non-rate_sig", file_ext, ".csv", collapse = NULL)
  C_1_sig = paste0(csvroot3, injtype, "_C_sp_3_non-rate_sig", file_ext, ".csv", collapse = NULL)
  C_4_sig = paste0(csvroot5, injtype, "_C_sp_5_non-rate_sig", file_ext, ".csv", collapse = NULL)
}

######################################################################################################

# NAME RESULTS OF RANDOMIZATION PROCEDURE METHOD 1 (must be Stata 12 .dtas - use oops.do to convert if necessary)

if (subpart.form == "rate" & lag_3 == "off" & lag_5 == "off") {
  B_1 = paste0(dtaroot, injtype, "_B_1_ri.dta", collapse = NULL)
  B_4 = paste0(dtaroot, injtype, "_B_4_ri.dta", collapse = NULL)
  C_1 = paste0(dtaroot, injtype, "_C_1_ri.dta", collapse = NULL)
  C_4 = paste0(dtaroot, injtype, "_C_4_ri.dta", collapse = NULL)
}
if (subpart.form == "not-a-rate" & lag_3 == "off" & lag_5 == "off") {
  B_1 = paste0(dtaroot, injtype, "_B_1_non-rate_ri.dta", collapse = NULL)
  B_4 = paste0(dtaroot, injtype, "_B_4_non-rate_ri.dta", collapse = NULL)
  C_1 = paste0(dtaroot, injtype, "_C_1_non-rate_ri.dta", collapse = NULL)
  C_4 = paste0(dtaroot, injtype, "_C_4_non-rate_ri.dta", collapse = NULL)
}
if (subpart.form == "rate" & (lag_3 == "on" | lag_5 == "on")) {
  B_1 = paste0(dtaroot3, injtype, "_B_3_ri.dta", collapse = NULL)
  B_4 = paste0(dtaroot5, injtype, "_B_5_ri.dta", collapse = NULL)
  C_1 = paste0(dtaroot3, injtype, "_C_3_ri.dta", collapse = NULL)
  C_4 = paste0(dtaroot5, injtype, "_C_5_ri.dta", collapse = NULL)
}
if (subpart.form == "not-a-rate" & (lag_3 == "on" | lag_5 == "on")) {
  B_1 = paste0(dtaroot3, injtype, "_B_3_non-rate_ri.dta", collapse = NULL)
  B_4 = paste0(dtaroot5, injtype, "_B_5_non-rate_ri.dta", collapse = NULL)
  C_1 = paste0(dtaroot3, injtype, "_C_3_non-rate_ri.dta", collapse = NULL)
  C_4 = paste0(dtaroot5, injtype, "_C_5_non-rate_ri.dta", collapse = NULL)
}

######################################################################################################

# NAME OUTPUT FILES: CSVS OF SUBPARTS THAT ARE ROBUSTLY SIGNIFICANT AFTER RI METHOD 1

if (subpart.form == "rate" & lag_3 == "off" & lag_5 == "off") {
  B_1_out_file = paste0(csvroot, injtype, "_B_1_method_2_input", file_ext, ".csv", collapse = NULL)
  B_4_out_file = paste0(csvroot, injtype, "_B_4_method_2_input", file_ext, ".csv", collapse = NULL)
  C_1_out_file = paste0(csvroot, injtype, "_C_1_method_2_input", file_ext, ".csv", collapse = NULL)
  C_4_out_file = paste0(csvroot, injtype, "_C_4_method_2_input", file_ext, ".csv", collapse = NULL)
  
  # final csv with results from all 4 models  
  all_out_file = paste0(csvroot, injtype, "_rate", file_ext, ".csv",  collapse = NULL)
}
if (subpart.form == "not-a-rate" & lag_3 == "off" & lag_5 == "off") {
  B_1_out_file = paste0(csvroot, injtype, "_B_1_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  B_4_out_file = paste0(csvroot, injtype, "_B_4_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  C_1_out_file = paste0(csvroot, injtype, "_C_1_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  C_4_out_file = paste0(csvroot, injtype, "_C_4_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  
  # final csv with results from all 4 models  
  all_out_file = paste0(csvroot, injtype, "_non-rate", file_ext, ".csv", collapse = NULL)
}
if (subpart.form == "rate" & (lag_3 == "on" | lag_5 == "on")) {
  B_1_out_file = paste0(csvroot3, injtype, "_B_3_method_2_input", file_ext, ".csv", collapse = NULL)
  B_4_out_file = paste0(csvroot5, injtype, "_B_5_method_2_input", file_ext, ".csv", collapse = NULL)
  C_1_out_file = paste0(csvroot3, injtype, "_C_3_method_2_input", file_ext, ".csv", collapse = NULL)
  C_4_out_file = paste0(csvroot5, injtype, "_C_5_method_2_input", file_ext, ".csv", collapse = NULL)
  
  # final csv with results from all 4 models  
  all_out_file = paste0(csvroot, injtype, "_rate_3-5", file_ext, ".csv",  collapse = NULL)
}
if (subpart.form == "not-a-rate" & (lag_3 == "on" | lag_5 == "on")) {
  B_1_out_file = paste0(csvroot3, injtype, "_B_3_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  B_4_out_file = paste0(csvroot5, injtype, "_B_5_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  C_1_out_file = paste0(csvroot3, injtype, "_C_3_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  C_4_out_file = paste0(csvroot5, injtype, "_C_5_non-rate_method_2_input", file_ext, ".csv", collapse = NULL)
  
  # final csv with results from all 4 models  
  all_out_file = paste0(csvroot, injtype, "_non-rate_3-5", file_ext, ".csv", collapse = NULL)
}

######################################################################################################

# LOAD DATA FROM PREFERRED MODELS

# load in preferred model results (significant subparts)
B_1_sig = read.table(B_1_sig, sep = ",")
names = c("subpart", "coefficient", "pvalue")
names(B_1_sig) = names
B_1_sig = B_1_sig[c(4: nrow(B_1_sig)),]
B_1_sig = B_1_sig[which(B_1_sig$pvalue != "."),]
B_1_sig$pvalue = as.numeric(as.character(B_1_sig$pvalue))
B_1_sig$coefficient = gsub("\\*", "", B_1_sig$coefficient)

B_4_sig = read.table(B_4_sig, sep = ",")
names(B_4_sig) = names
B_4_sig = B_4_sig[c(4: nrow(B_4_sig)),]
B_4_sig = B_4_sig[which(B_4_sig$pvalue != "."),]
B_4_sig$pvalue = as.numeric(as.character(B_4_sig$pvalue))
B_4_sig$coefficient = gsub("\\*", "", B_4_sig$coefficient)

C_1_sig = read.table(C_1_sig, sep = ",")
names(C_1_sig) = names
C_1_sig = C_1_sig[c(4: nrow(C_1_sig)),]
C_1_sig = C_1_sig[which(C_1_sig$pvalue != "."),]
C_1_sig$pvalue = as.numeric(as.character(C_1_sig$pvalue))
C_1_sig$coefficient = gsub("\\*", "", C_1_sig$coefficient)

C_4_sig = read.table(C_4_sig, sep = ",")
names(C_4_sig) = names
C_4_sig = C_4_sig[c(4: nrow(C_4_sig)),]
C_4_sig = C_4_sig[which(C_4_sig$pvalue != "."),]
C_4_sig$pvalue = as.numeric(as.character(C_4_sig$pvalue))
C_4_sig$coefficient = gsub("\\*", "", C_4_sig$coefficient)

######################################################################################################

# FORMAT DATA FROM PREFERRED MODELS

for (d in c("B_1", "B_4", "C_1", "C_4")) {
  data = eval(parse(text = paste(d, "sig", sep = "_")))
  data$subpart = as.character(data$subpart)
  data$coefficient = as.numeric(as.character(data$coefficient))
  assign(paste(d, "sig", sep = "_"), data)
  
  temp = data.frame(data$subpart)
  names(temp) = "subpart"
  temp$subpart = as.character(temp$subpart)
  temp$p = numeric(nrow(temp))
  assign(paste(d, "ri", sep = "_"), temp)
}

######################################################################################################

# APPEND RESULTS FROM PREFERRED MODELS - FOR MAKING LATEX TABLES

if (append.models == "on") {
  
  for (d in c("B_1", "B_4", "C_1", "C_4")) {
    if ((d == "B_1" | d == "C_1") & (lag_3 != "on" & lag_5 != "on")) {
      strip = 5
    }
    if ((d == "B_4" | d == "C_4") | ((lag_3 == "on" | lag_5 != "on") & (d == "B_1" | d == "C_1"))) {
      strip = 6
    }
    data = eval(parse(text = paste(d, "sig", sep = "_")))
    data$subpart = substr(data$subpart, 1, nchar(data$subpart) - strip)
    assign(paste(d, "sig", sep = "_"), data)
  }
  
  all_subparts = c(B_1_sig$subpart, B_4_sig$subpart, C_1_sig$subpart, C_4_sig$subpart)
  all_subparts = unique(all_subparts)
  
  for (d in c("B_1", "B_4", "C_1", "C_4")) {
    data = eval(parse(text = paste(d, "sig", sep = "_")))
    add = setdiff(all_subparts, data$subpart)
    
    add = data.frame(add)
    names(add) = "subpart"
    add$coefficient = NA
    add$pvalue = NA
    
    data = rbind(data, add)
    
    assign(paste(d, "sig", sep = "_"), data)
  }
  
  # MERGE & CLEAN - SAVE CSV WITH RESULTS OF ALL 4 MODELS
  
  data = merge(B_1_sig, B_4_sig, by = "subpart", all = T)
  names(data)[names(data) == "coefficient.x"] = "c.B_1_sig"
  names(data)[names(data) == "pvalue.x"] = "p.B_1_sig"
  names(data)[names(data) == "coefficient.y"] = "c.B_4_sig"
  names(data)[names(data) == "pvalue.y"] = "p.B_4_sig"
  
  data = merge(data, C_1_sig, by = "subpart", all = T)
  names(data)[names(data) == "coefficient"] = "c.C_1_sig"
  names(data)[names(data) == "pvalue"] = "p.C_1_sig"
  
  data = merge(data, C_4_sig, by = "subpart", all = T)
  names(data)[names(data) == "coefficient"] = "c.C_4_sig"
  names(data)[names(data) == "pvalue"] = "p.C_4_sig"
  
  # format coefficients (c) and p-values (p)
  data$c.B_1_sig = as.numeric(as.character(data$c.B_1_sig))
  data$p.B_1_sig = as.numeric(as.character(data$p.B_1_sig))
  data$c.B_1_sig = round(data$c.B_1_sig, digits = 3)
  data$p.B_1_sig = round(data$p.B_1_sig, digits = 3)
  
  data$c.B_4_sig = as.numeric(as.character(data$c.B_4_sig))
  data$p.B_4_sig = as.numeric(as.character(data$p.B_4_sig))
  data$c.B_4_sig = round(data$c.B_4_sig, digits = 3)
  data$p.B_4_sig = round(data$p.B_4_sig, digits = 3)
  
  data$c.C_1_sig = as.numeric(as.character(data$c.C_1_sig))
  data$p.C_1_sig = as.numeric(as.character(data$p.C_1_sig))
  data$c.C_1_sig = round(data$c.C_1_sig, digits = 3)
  data$p.C_1_sig = round(data$p.C_1_sig, digits = 3)
  
  data$c.C_4_sig = as.numeric(as.character(data$c.C_4_sig))
  data$p.C_4_sig = as.numeric(as.character(data$p.C_4_sig))
  data$c.C_4_sig = round(data$c.C_4_sig, digits = 3)
  data$p.C_4_sig = round(data$p.C_4_sig, digits = 3)
  
  # add significance stars back in based on p-values
  data$c.B_1_sig = as.character(data$c.B_1_sig)
  data$c.B_1_sig = ifelse(data$p.B_1_sig < 0.001, paste0(data$c.B_1_sig, "***"), data$c.B_1_sig)
  data$c.B_1_sig = ifelse(data$p.B_1_sig < 0.01 & data$p.B_1_sig >= 0.001, paste0(data$c.B_1_sig, "**"), data$c.B_1_sig)
  data$c.B_1_sig = ifelse(data$p.B_1_sig <= 0.05 & data$p.B_1_sig >= 0.01, paste0(data$c.B_1_sig, "*"), data$c.B_1_sig)
  
  data$c.B_4_sig = as.character(data$c.B_4_sig)
  data$c.B_4_sig = ifelse(data$p.B_4_sig < 0.001, paste0(data$c.B_4_sig, "***"), data$c.B_4_sig)
  data$c.B_4_sig = ifelse(data$p.B_4_sig < 0.01 & data$p.B_4_sig >= 0.001, paste0(data$c.B_4_sig, "**"), data$c.B_4_sig)
  data$c.B_4_sig = ifelse(data$p.B_4_sig <= 0.05 & data$p.B_4_sig >= 0.01, paste0(data$c.B_4_sig, "*"), data$c.B_4_sig)
  
  data$c.C_1_sig = as.character(data$c.C_1_sig)
  data$c.C_1_sig = ifelse(data$p.C_1_sig < 0.001, paste0(data$c.C_1_sig, "***"), data$c.C_1_sig)
  data$c.C_1_sig = ifelse(data$p.C_1_sig < 0.01 & data$p.C_1_sig >= 0.001, paste0(data$c.C_1_sig, "**"), data$c.C_1_sig)
  data$c.C_1_sig = ifelse(data$p.C_1_sig <= 0.05 & data$p.C_1_sig >= 0.01, paste0(data$c.C_1_sig, "*"), data$c.C_1_sig)
  
  data$c.C_4_sig = as.character(data$c.C_4_sig)
  data$c.C_4_sig = ifelse(data$p.C_4_sig < 0.001, paste0(data$c.C_4_sig, "***"), data$c.C_4_sig)
  data$c.C_4_sig = ifelse(data$p.C_4_sig < 0.01 & data$p.C_4_sig >= 0.001, paste0(data$c.C_4_sig, "**"), data$c.C_4_sig)
  data$c.C_4_sig = ifelse(data$p.C_4_sig <= 0.05 & data$p.C_4_sig >= 0.01, paste0(data$c.C_4_sig, "*"), data$c.C_4_sig)
  
  # remove unnecssary variables and prep data so that it can be popped into a pretty latex table
  data = data[, c(-grep("^p", names(data)))]
  data$subpart = substr(data$subpart, 3, nchar(data$subpart) - 0)
  data$subpart = gsub("_", ".", data$subpart)
  
  # if lags 3 and 5 are being run, we imported "3" as "1" and "5" as "4" (because this file we coded before 3 and 5 were
  # being used as a robustness test), so rename the variables appropriately now before outputting
  if (lag_3 == "on" | lag_5 == "on") {
    names_3_and_5 = c("subpart", "c.B_3_sig", "c.B_5_sig", "c.C_3_sig", "c.C_5_sig")
    names(data) = names_3_and_5
  }
  
  write.csv(data, file = all_out_file, row.names = FALSE, na = "")
}

######################################################################################################
  
# PROCESS RESULTS OF THE FIRST RANDOMIZATION INFERENCE (RI METHOD 1)

# load in RI results
B_1 = read.dta(B_1)
B_4 = read.dta(B_4)
C_1 = read.dta(C_1)
C_4 = read.dta(C_4)

# calculate p values for each subpart
for (d in c("B_1", "B_4", "C_1", "C_4")) {
  true = eval(parse(text = paste(d, "sig", sep = "_")))
  fake = eval(parse(text = d))
  ri = eval(parse(text = paste(d, "ri", sep = "_")))
  for (i in 1:nrow(true)) {
    sp = true$subpart[i]
    true_coef = true[true$subpart == sp, "coefficient"]
    fake_coefs = fake[, grepl(sp, names(fake))]
    p = sum(fake_coefs >= true_coef, na.rm = TRUE) / sum(!is.na(fake_coefs))
    ri[ri$subpart == sp, "p"] = p
    assign(paste(d, "ri", sep = "_"), ri)
  }
}

######################################################################################################

# SAVE CSVS WITH NAMES OF ROBUSTLY SIGNIFICANT SUBPARTS  (AFTER RI METHOD 1)

B_1_ri = B_1_ri[which(B_1_ri$p < 0.05) ,]
B_4_ri = B_4_ri[which(B_4_ri$p < 0.05) ,]
C_1_ri = C_1_ri[which(C_1_ri$p < 0.05) ,]
C_4_ri = C_4_ri[which(C_4_ri$p < 0.05) ,]

if (nrow(B_1_ri) != 0) {
  write.csv(B_1_ri, file = B_1_out_file, row.names = FALSE)
}
if (nrow(B_4_ri) != 0) {
  write.csv(B_4_ri, file = B_4_out_file, row.names = FALSE)
}
if (nrow(C_1_ri) != 0) {
  write.csv(C_1_ri, file = C_1_out_file, row.names = FALSE)
}
if (nrow(C_4_ri) != 0) {
  write.csv(C_4_ri, file = C_4_out_file, row.names = FALSE)
}

######################################################################################################

# not sure what this does - can I delete it? Hmm...

if (subpart.form == "rate" & injtype == "MR") {
  B_1a = unique(B_1_ri$subpart)
  B_4a = unique(B_4_ri$subpart)
  C_1a = unique(C_1_ri$subpart)
  C_4a = unique(C_4_ri$subpart)
}
if (subpart.form == "not-a-rate" & injtype == "MR") {
  B_1b = unique(B_1_ri$subpart)
  B_4b = unique(B_4_ri$subpart)
  C_1b = unique(C_1_ri$subpart)
  C_4b = unique(C_4_ri$subpart)
}
if (subpart.form == "rate" & injtype == "PS") {
  B_1c = unique(B_1_ri$subpart)
  B_4c = unique(B_4_ri$subpart)
  C_1c = unique(C_1_ri$subpart)
  C_4c = unique(C_4_ri$subpart)
}
if (subpart.form == "not-a-rate" & injtype == "PS") {
  B_1d = unique(B_1_ri$subpart)
  B_4d = unique(B_4_ri$subpart)
  C_1d = unique(C_1_ri$subpart)
  C_4d = unique(C_4_ri$subpart)
}

######################################################################################################
