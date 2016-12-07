# NIOSH Project 2014-N-15776

# analyze_RI_method_2.R
# Inspect Randomization Inference (Method 2) Output

# Last edit 12/07/16 - SML

######################################################################################################

library(foreign)

# set preferences
date = "12-6/"

# WHICH MODELS DO YOU WANT TO RUN?

  # injtype = "PS"
  injtype = "MR"
  
  subpart.form = "rate"
  # subpart.form = "not-a-rate"
  
  # specification.test = "on" # analyze results from models with union & longwall indicators
  specification.test = "off"
  
  lag_3 = "on" # cannot be on at same time as ulw specification test. will also run lag 5.
  #lag_3 = "off"
  
  lag_5 = "on" # cannot be on at same time as ulw specification test. will also run lag 3.
  #lag_5 = "off"

######################################################################################################

# define file names
dtaroot = paste0("X:/Projects/Mining/NIOSH/analysis/results/dta/", date, "method_2/", collapse = NULL)
csvroot = paste0("X:/Projects/Mining/NIOSH/analysis/results/csv/", date, collapse = NULL)

# significant variable lists
  # load in 4 csvs - preferred model results
if (subpart.form == "rate") {
  B_1 = paste0(csvroot, injtype, "_B_sp_1_sig.csv", collapse = NULL)
  B_4 = paste0(csvroot, injtype, "_B_sp_4_sig.csv", collapse = NULL)
  C_1 = paste0(csvroot, injtype, "_C_sp_1_sig.csv", collapse = NULL)
  C_4 = paste0(csvroot, injtype, "_C_sp_4_sig.csv", collapse = NULL)
}
if (subpart.form == "not-a-rate") {
  B_1 = paste0(csvroot, injtype, "_B_sp_1_non-rate_sig.csv", collapse = NULL)
  B_4 = paste0(csvroot, injtype, "_B_sp_4_non-rate_sig.csv", collapse = NULL)
  C_1 = paste0(csvroot, injtype, "_C_sp_1_non-rate_sig.csv", collapse = NULL)
  C_4 = paste0(csvroot, injtype, "_C_sp_4_non-rate_sig.csv", collapse = NULL)
}
# method 2 inputs
  # load in 4 csvs - method 1 RI results
if (subpart.form == "rate") {
  B_1_sig = paste0(csvroot, injtype, "_B_1_method_2_input.csv", collapse = NULL)
  B_4_sig = paste0(csvroot, injtype, "_B_4_method_2_input.csv", collapse = NULL)
  C_1_sig = paste0(csvroot, injtype, "_C_1_method_2_input.csv", collapse = NULL)
  C_4_sig = paste0(csvroot, injtype, "_C_4_method_2_input.csv", collapse = NULL)
}
if (subpart.form == "not-a-rate") {
  B_1_sig = paste0(csvroot, injtype, "_B_1_non-rate_method_2_input.csv", collapse = NULL)
  B_4_sig = paste0(csvroot, injtype, "_B_4_non-rate_method_2_input.csv", collapse = NULL)
  C_1_sig = paste0(csvroot, injtype, "_C_1_non-rate_method_2_input.csv", collapse = NULL)
  C_4_sig = paste0(csvroot, injtype, "_C_4_non-rate_method_2_input.csv", collapse = NULL)
}

if (subpart.form == "rate") {
  ext = ""
}
if (subpart.form == "not-a-rate") {
  ext = "non-rate_"
}

######################################################################################################

if (subpart.form == "rate") {
  B_1_out_file = paste0(csvroot, injtype, "_B_1_method_2_output.csv", collapse = NULL)
  B_4_out_file = paste0(csvroot, injtype, "_B_4_method_2_output.csv", collapse = NULL)
  C_1_out_file = paste0(csvroot, injtype, "_C_1_method_2_output.csv", collapse = NULL)
  C_4_out_file = paste0(csvroot, injtype, "_C_4_method_2_output.csv", collapse = NULL)
}
if (subpart.form == "not-a-rate") {
  B_1_out_file = paste0(csvroot, injtype, "_B_1_non-rate_method_2_output.csv", collapse = NULL)
  B_4_out_file = paste0(csvroot, injtype, "_B_4_non-rate_method_2_output.csv", collapse = NULL)
  C_1_out_file = paste0(csvroot, injtype, "_C_1_non-rate_method_2_output.csv", collapse = NULL)
  C_4_out_file = paste0(csvroot, injtype, "_C_4_non-rate_method_2_output.csv", collapse = NULL)
}

######################################################################################################

# LOAD DATA

# load in preferred model results
B_1 = read.table(B_1, sep = ",", header = T)
B_4 = read.table(B_4, sep = ",", header = T)
C_1 = read.table(C_1, sep = ",", header = T)
C_4 = read.table(C_4, sep = ",", header = T)

# load in method 2 inputs 
B_1_sig = read.table(B_1_sig, sep = ",", header = T)
B_4_sig = read.table(B_4_sig, sep = ",", header = T)
C_1_sig = read.table(C_1_sig, sep = ",", header = T)
C_4_sig = read.table(C_4_sig, sep = ",", header = T)

# create lists of the subparts that were tested in method 2 (and their file paths)
B_1_sig_list = B_1_sig$subpart
B_1_sig_files = paste0(dtaroot, injtype, "_B_1_", ext, B_1_sig_list, ".dta", collapse = NULL)
B_4_sig_list = B_4_sig$subpart
B_4_sig_files = paste0(dtaroot, injtype, "_B_4_", ext, B_4_sig_list, ".dta", collapse = NULL)
C_1_sig_list = C_1_sig$subpart
C_1_sig_files = paste0(dtaroot, injtype, "_C_1_", ext, C_1_sig_list, ".dta", collapse = NULL)
C_4_sig_list = C_4_sig$subpart
C_4_sig_files = paste0(dtaroot, injtype, "_C_4_", ext, C_4_sig_list, ".dta", collapse = NULL)

######################################################################################################

# format coefficients and subparts

names = c("subpart", "b", "p")
names(B_1) = names
names(B_4) = names
names(C_1) = names
names(C_4) = names
B_1$b = as.numeric(as.character(B_1$b))
B_4$b = as.numeric(as.character(B_4$b))
C_1$b = as.numeric(as.character(C_1$b))
C_4$b = as.numeric(as.character(C_4$b))

B_1 = B_1[which(B_1$p != "."), ]
B_1 = B_1[-1, ]
B_1 = B_1[-1, ]

B_4 = B_4[which(B_4$p != "."), ]
B_4 = B_4[-1, ]
B_4 = B_4[-1, ]

C_1 = C_1[which(C_1$p != "."), ]
C_1 = C_1[-1, ]
C_1 = C_1[-1, ]

C_4 = C_4[which(C_4$p != "."), ]
C_4 = C_4[-1, ]
C_4 = C_4[-1, ]

######################################################################################################

# LOAD IN RI METHOD 2 RESULTS & CALCULATE P VALUES FOR EACH SUBPART

names = "fake_coef"
B_1_sig$new_p = NA
for (a in B_1_sig_list) {
  data = read.dta(paste0(dtaroot, injtype, "_B_1_", ext, a, ".dta", collapse = NULL))
  names(data) = names
  data$fake_coef = as.numeric(as.character(data$fake_coef))
  fake_coefs = data$fake_coef
  true_coef = B_1[B_1$subpart == a, "b"]
  p = sum(fake_coefs >= true_coef, na.rm = TRUE) / sum(!is.na(fake_coefs))
  B_1_sig[B_1_sig$subpart == a, "new_p"] = p
}

B_4_sig$new_p = NA
for (a in B_4_sig_list) {
  data = read.dta(paste0(dtaroot, injtype, "_B_4_", ext, a, ".dta", collapse = NULL))
  names(data) = names
  data$fake_coef = as.numeric(as.character(data$fake_coef))
  fake_coefs = data$fake_coef
  true_coef = B_4[B_4$subpart == a, "b"]
  p = sum(fake_coefs >= true_coef, na.rm = TRUE) / sum(!is.na(fake_coefs))
  B_4_sig[B_4_sig$subpart == a, "new_p"] = p
}

C_1_sig$new_p = NA
for (a in C_1_sig_list) {
  data = read.dta(paste0(dtaroot, injtype, "_C_1_", ext, a, ".dta", collapse = NULL))
  names(data) = names
  data$fake_coef = as.numeric(as.character(data$fake_coef))
  fake_coefs = data$fake_coef
  true_coef = C_1[C_1$subpart == a, "b"]
  p = sum(fake_coefs >= true_coef, na.rm = TRUE) / sum(!is.na(fake_coefs))
  C_1_sig[C_1_sig$subpart == a, "new_p"] = p
}

C_4_sig$new_p = NA
for (a in C_4_sig_list) {
  data = read.dta(paste0(dtaroot, injtype, "_C_4_", ext, a, ".dta", collapse = NULL))
  names(data) = names
  data$fake_coef = as.numeric(as.character(data$fake_coef))
  fake_coefs = data$fake_coef
  true_coef = C_4[C_4$subpart == a, "b"]
  p = sum(fake_coefs >= true_coef, na.rm = TRUE) / sum(!is.na(fake_coefs))
  C_4_sig[C_4_sig$subpart == a, "new_p"] = p
}

######################################################################################################

# SAVE CSVS WITH ROBUSTLY SIGNIFICANT SUBPARTS (AFTER METHOD 2)

B_1_sig = B_1_sig[which(B_1_sig$new_p < 0.05) ,]
B_4_sig = B_4_sig[which(B_4_sig$new_p < 0.05) ,]
C_1_sig = C_1_sig[which(C_1_sig$new_p < 0.05) ,]
C_4_sig = C_4_sig[which(C_4_sig$new_p < 0.05) ,]

if (nrow(B_1_sig) != 0) {
  write.csv(B_1_sig, file = B_1_out_file, row.names = FALSE)
}
if (nrow(B_4_sig) != 0) {
  write.csv(B_4_sig, file = B_4_out_file, row.names = FALSE)
}
if (nrow(C_1_sig) != 0) {
  write.csv(C_1_sig, file = C_1_out_file, row.names = FALSE)
}
if (nrow(C_4_sig) != 0) {
  write.csv(C_4_sig, file = C_4_out_file, row.names = FALSE)
}

######################################################################################################
