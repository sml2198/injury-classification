# NIOSH Project 2014-N-15776

# 24 - Inspect Randomization Inference Output

# Last edit 11/14/16

######################################################################################################

library(foreign)

# set preferences
date = "11-11/"
injtype = "PS"
#injtype = "MR"
subpart.form = "rate"
#subpart.form = "not-a-rate"

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
