# NIOSH Project 2014-N-15776

# 24 - Inspect Randomization Inference Output

# Last edit 11/14/16

######################################################################################################

library(foreign)

# set preferences
date = "11-11/"
# injtype = "PS"
injtype = "MR"
subpart.form = "rate"
# subpart.form = "not-a-rate"

# define file names

dtafolder = "X:/Projects/Mining/NIOSH/analysis/results/dta/"
csvfolder = "X:/Projects/Mining/NIOSH/analysis/results/csv/"
dtaroot = paste0(dtafolder, date, collapse = NULL)
csvroot = paste0(csvfolder, date, collapse = NULL)

# significant variable lists
  # load in 4 csvs
if (subpart.form == "rate") {
  B_1_sig = paste0(csvroot, injtype, "_B_sp_1_sig.csv", collapse = NULL)
  B_4_sig = paste0(csvroot, injtype, "_B_sp_4_sig.csv", collapse = NULL)
  C_1_sig = paste0(csvroot, injtype, "_C_sp_1_sig.csv", collapse = NULL)
  C_4_sig = paste0(csvroot, injtype, "_C_sp_4_sig.csv", collapse = NULL)
}
if (subpart.form ==  "not-a-rate") {
  B_1_sig = paste0(csvroot, injtype, "_B_sp_1_non-rate_sig.csv", collapse = NULL)
  B_4_sig = paste0(csvroot, injtype, "_B_sp_4_non-rate_sig.csv", collapse = NULL)
  C_1_sig = paste0(csvroot, injtype, "_C_sp_1_non-rate_sig.csv", collapse = NULL)
  C_4_sig = paste0(csvroot, injtype, "_C_sp_4_non-rate_sig.csv", collapse = NULL)
}

# randomization results (method 1)
  # load in 4 dtas - these must be Stata 12 or older. use oops.do to convert.
if (subpart.form == "rate") {
  B_1 = paste0(dtaroot, injtype, "_B_1_ri12.dta", collapse = NULL)
  B_4 = paste0(dtaroot, injtype, "_B_4_ri12.dta", collapse = NULL)
  C_1 = paste0(dtaroot, injtype, "_C_1_ri12.dta", collapse = NULL)
  C_4 = paste0(dtaroot, injtype, "_C_4_ri12.dta", collapse = NULL)
}
if (subpart.form == "not-a-rate") {
  B_1 = paste0(dtaroot, injtype, "_B_1_non-rate_ri.dta", collapse = NULL)
  B_4 = paste0(dtaroot, injtype, "_B_4_non-rate_ri.dta", collapse = NULL)
  C_1 = paste0(dtaroot, injtype, "_C_1_non-rate_ri.dta", collapse = NULL)
  C_4 = paste0(dtaroot, injtype, "_C_4_non-rate_ri.dta", collapse = NULL)
}

# output - lists of robustly significant subparts
if (subpart.form == "rate") {
  B_1_out_file = paste0(csvroot, injtype, "_B_1_method_2_input.csv", collapse = NULL)
  B_4_out_file = paste0(csvroot, injtype, "_B_4_method_2_input.csv", collapse = NULL)
  C_1_out_file = paste0(csvroot, injtype, "_C_1_method_2_input.csv", collapse = NULL)
  C_4_out_file = paste0(csvroot, injtype, "_C_4_method_2_input.csv", collapse = NULL)
}
if (subpart.form == "not-a-rate") {
  B_1_out_file = paste0(csvroot, injtype, "_B_1_non-rate_method_2_input.csv", collapse = NULL)
  B_4_out_file = paste0(csvroot, injtype, "_B_4_non-rate_method_2_input.csv", collapse = NULL)
  C_1_out_file = paste0(csvroot, injtype, "_C_1_non-rate_method_2_input.csv", collapse = NULL)
  C_4_out_file = paste0(csvroot, injtype, "_C_4_non-rate_method_2_input.csv", collapse = NULL)
}

######################################################################################################

# LOAD DATA

# load in RI results
B_1 = read.dta(B_1)
B_4 = read.dta(B_4)
C_1 = read.dta(C_1)
C_4 = read.dta(C_4)

# load in preferred model results (significant subparts)
B_1_sig = read.table(B_1_sig, sep = ",")
names = c("subpart", "coefficient", "pvalue")
names(B_1_sig) = names
B_1_sig = B_1_sig[c(4: nrow(B_1_sig)),]

B_4_sig = read.table(B_4_sig, sep = ",")
names(B_4_sig) = names
B_4_sig = B_4_sig[c(4: nrow(B_4_sig)),]

C_1_sig = read.table(C_1_sig, sep = ",")
names(C_1_sig) = names
C_1_sig = C_1_sig[c(4: nrow(C_1_sig)),]

C_4_sig = read.table(C_4_sig, sep = ",")
names(C_4_sig) = names
C_4_sig = C_4_sig[c(4: nrow(C_4_sig)),]

######################################################################################################

# GRAB ALL SUBPARTS 
# 
# B_1 = B_1_sig[, c("subpart","coefficient")]
# B_4 = B_4_sig[, c("subpart","coefficient")]
# C_1 = C_1_sig[, c("subpart","coefficient")]
# C_4 = C_4_sig[, c("subpart","coefficient")]
# 
# B_1$subpart = as.character(B_1$subpart)
# B_4$subpart = as.character(B_4$subpart)
# C_1$subpart = as.character(C_1$subpart)
# C_4$subpart = as.character(C_4$subpart)
# 
# B_1$subpart = substr(B_1$subpart, 3, nchar(B_1$subpart) - 5)
# B_4$subpart = substr(B_4$subpart, 3, nchar(B_4$subpart) - 6)
# C_1$subpart = substr(C_1$subpart, 3, nchar(C_1$subpart) - 5)
# C_4$subpart = substr(C_4$subpart, 3, nchar(C_4$subpart) - 6)
# 
# B_1$subpart = gsub("_", ".", B_1$subpart)
# B_4$subpart = gsub("_", ".", B_4$subpart)
# C_1$subpart = gsub("_", ".", C_1$subpart)
# C_4$subpart = gsub("_", ".", C_4$subpart)
# 
# B_1 = B_1[order(B_1$subpart),]
# B_4 = B_4[order(B_4$subpart),]
# C_1 = C_1[order(C_1$subpart),]
# C_4 = C_4[order(C_4$subpart),]
# 
# write.csv(B_1, file = "X:/Projects/Mining/NIOSH/analysis/results/csv/test/model_1.csv", row.names = FALSE)
# write.csv(B_4, file = "X:/Projects/Mining/NIOSH/analysis/results/csv/test/model_2.csv", row.names = FALSE)
# write.csv(C_1, file = "X:/Projects/Mining/NIOSH/analysis/results/csv/test/model_3.csv", row.names = FALSE)
# write.csv(C_4, file = "X:/Projects/Mining/NIOSH/analysis/results/csv/test/model_4.csv", row.names = FALSE)

class(B_1_sig$subpart)
all_subparts = B_1_sig$subpart

######################################################################################################

# FORMAT DATA

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

for (d in c("B_1", "B_4", "C_1", "C_4")) {
  if (d == "B_1" | d == "C_1") {
    strip = 5
  }
  if (d == "B_4" | d == "C_4") {
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

data = data[, c(-grep("^p", names(data)))]
data$subpart = substr(data$subpart, 3, nchar(data$subpart) - 0)
data$subpart = gsub("_", ".", data$subpart)

write.csv(data, file = "X:/Projects/Mining/NIOSH/analysis/results/csv/test/MR_rate.csv", row.names = FALSE, na = "")

######################################################################################################
  
# CALCULATE P VALUES FOR EACH SUBPART

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

# SAVE CSVS WITH NAMES OF ROBUSTLY SIGNIFICANT SUBPARTS

B_1_ri = B_1_ri[which(B_1_ri$p < 0.05) ,]
B_4_ri = B_4_ri[which(B_4_ri$p < 0.05) ,]
C_1_ri = C_1_ri[which(C_1_ri$p < 0.05) ,]
C_4_ri = C_4_ri[which(C_4_ri$p < 0.05) ,]

write.csv(B_1_ri, file = B_1_out_file, row.names = FALSE)
write.csv(B_4_ri, file = B_4_out_file, row.names = FALSE)
write.csv(C_1_ri, file = C_1_out_file, row.names = FALSE)
write.csv(C_4_ri, file = C_4_out_file, row.names = FALSE)

######################################################################################################

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

B_1a 
B_4a 
C_1a 
C_4a
B_1b 
B_4b 
C_1b 
C_4b
B_1c 
B_4c 
C_1c 
C_4c
B_1d 
B_4d 
C_1d 
C_4d

######################################################################################################
