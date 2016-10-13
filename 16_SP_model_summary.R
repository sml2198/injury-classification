# NIOSH Project 2014-N-15776

# 16 - Subpart Model Summary

# Last edit 10/13/16

#####################################################################################################

# injury = "MR"
injury = "PS"

# trusted_MR = "on"
trusted_MR = "off"

# trusted_PS = "on"
trusted_PS = "off"

#####################################################################################################

if (injury == "MR") {
  directory_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Model Summaries 10-12/MR/Estout/"
  
  if (trusted_MR == "on") {
    out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Model Summaries 10-12/MR/Summaries/Subpart Summary (models we trust).csv"
  }
  
  if (trusted_MR == "off") {
    out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Model Summaries 10-12/MR/Summaries/Subpart Summary (all models).csv"
  }
  
}

if (injury == "PS") {
  directory_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Model Summaries 10-12/PS/Estout/"
  
  if (trusted_PS == "on") {
    out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Model Summaries 10-12/PS/Summaries/Subpart Summary (models we trust).csv"
  }
  
  if (trusted_PS == "off") {
    out_path = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Injury-Classification/Model Summaries 10-12/PS/Summaries/Subpart Summary (all models).csv"
  }
  
}

#####################################################################################################

# gather model information
clean_model_output = function(model_name) {
  
  file_name = paste(directory_path, model_name, sep = "", collapse = "")
  data = read.csv(file_name, header = FALSE)
  
  names(data) = c("var", "coef", "p")
  data = data[-c(1, 2, nrow(data)), ]
  
  data$var = as.character(data$var)
  data$coef = as.numeric(as.character(data$coef))
  data$p = as.numeric(as.character(data$p))
  
  data$p = ifelse(is.na(data$coef), NA, data$p)
  
  data$coef = ifelse(data$coef == 1, NA, data$coef)
  data$coef_0.05 = ifelse(is.na(data$p), NA, 
                          ifelse(data$p >= 0.05, NA, data$coef))
  data$coef_0.01 = ifelse(is.na(data$p), NA, 
                          ifelse(data$p >= 0.01, NA, data$coef))
  
  data$var = ifelse(grepl("_1lag$", data$var), substr(data$var, 1, (nchar(data$var) - 5)), 
                    ifelse(grepl("_c_4lag$", data$var), substr(data$var, 1, (nchar(data$var) - 7)), 
                           ifelse(grepl("_c_lag_all$", data$var), substr(data$var, 1, (nchar(data$var) - 10)), data$var)))
  data$var = ifelse(grepl("pp$", data$var), substr(data$var, 1, (nchar(data$var) - 3)), 
                    ifelse(grepl("ss$", data$var), substr(data$var, 1, (nchar(data$var) - 3)), data$var))
  
  data = data[grepl("[0-9]$", data$var), ]
  
  names(data) = c("var", 
                  paste(substr(model_name, 1, nchar(model_name) - 4), "coef", sep = "."),
                  paste(substr(model_name, 1, nchar(model_name) - 4), "p", sep = "."), 
                  paste(substr(model_name, 1, nchar(model_name) - 4), "coef.05", sep = "."),
                  paste(substr(model_name, 1, nchar(model_name) - 4), "coef.01", sep = "."))
  
  return(data)

}

#####################################################################################################

models = list.files(directory_path)
models = models[grepl("^Model", models)]
models = models[grepl("\\.SP", models)]

if (trusted_MR == "on") {
  models = models[!grepl("\\.Q", models)]
  models = models[!grepl("4", models)]
}

if (trusted_PS == "on") {
  # fill in later
}

model_info = clean_model_output(models[1])

for (i in 2:length(models)) {
  data = clean_model_output(models[i])
  model_info = merge(data, model_info, all = TRUE)
}

rm(data, i)

#####################################################################################################

all_models = substr(models, 7, (nchar(models) - 4))

Y_models = all_models[grepl("\\.Y", all_models)]
Q_models = all_models[grepl("\\.Q", all_models)]

B_models = all_models[grepl("\\.B", all_models)]
C_models = all_models[grepl("\\.C", all_models)]

V_models = all_models[grepl("\\.V", all_models)]
SSV_models = all_models[grepl("\\.SSV", all_models)]
PP_models = all_models[grepl("\\.PP", all_models)]

l1_models = all_models[grepl("1$", all_models)]
l2_models = all_models[grepl("2$", all_models)]
l3_models = all_models[grepl("3$", all_models)]
l4_models = all_models[grepl("4$", all_models)]

model_summary = data.frame(model_info$var)
names(model_summary) = "var"

p_data = model_info[, grepl("p$", names(model_info))]
coef_data = model_info[, grepl("coef", names(model_info))]

model_types = c("all_models", 
               "Y_models", 
               "Q_models",
               "B_models", "C_models", 
               "V_models", "SSV_models", "PP_models", 
               "l1_models", "l2_models", "l3_models", "l4_models")

significance_levels = c(0.05, 0.01)

for (group in model_types) {

  for (sig in significance_levels) {

    p = rowSums(p_data[paste("Model", paste(eval(parse(text = group)), "p", sep = "."), sep = ".")] <= sig, na.rm = TRUE)
    coef_data_temp = coef_data[paste("Model", paste(paste(eval(parse(text = group)), "coef", sep = "."), substr(toString(sig), 3, nchar(toString(sig))), sep = "."), sep = ".")]
    
    coef_pos = rowSums(coef_data_temp > 1, na.rm = TRUE)
    coef_neg = rowSums(coef_data_temp <= 1, na.rm = TRUE)
    coef_min = apply(coef_data_temp, 1, min, na.rm = TRUE)
    coef_min = ifelse(coef_min == Inf, NA, 
                      ifelse(coef_min == -Inf, NA, coef_min))
    coef_max = apply(coef_data_temp, 1, max, na.rm = TRUE)
    coef_max = ifelse(coef_max == Inf, NA, 
                      ifelse(coef_max == -Inf, NA, coef_max))
    
    data_temp = data.frame(p, coef_pos, coef_neg, coef_min, coef_max)
    names(data_temp) = c(paste(paste(toString(group), "p", sep = "_"), substr(toString(sig), 3, nchar(toString(sig))), sep = "_", collapse = ""), 
                         paste(paste(toString(group), "coef_pos", sep = "_"), substr(toString(sig), 3, nchar(toString(sig))), sep = "_", collapse = ""), 
                         paste(paste(toString(group), "coef_neg", sep = "_"), substr(toString(sig), 3, nchar(toString(sig))), sep = "_", collapse = ""), 
                         paste(paste(toString(group), "coef_min", sep = "_"), substr(toString(sig), 3, nchar(toString(sig))), sep = "_", collapse = ""), 
                         paste(paste(toString(group), "coef_max", sep = "_"), substr(toString(sig), 3, nchar(toString(sig))), sep = "_", collapse = ""))
    
    model_summary = data.frame(model_summary, data_temp)
  
    }
}

rm(coef_data_temp, data_temp, p, coef_pos, coef_neg, coef_min, coef_max, group, sig)

#####################################################################################################

write.csv(model_summary, file = out_path, quote = FALSE, row.names = FALSE)

#####################################################################################################
