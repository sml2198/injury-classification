# NIOSH Project 2014-N-15776

# 23 - Descriptive Analyses

# Last edit 11/11/16

######################################################################################################

# SETTINGS

library(plyr)

injury = "MR"
# injury = "PS"

if (injury == "MR") {
  data_file_name = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/data/MR-data.csv"
}
if (injury == "PS") {
  data_file_name ="C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/data/PS-data.csv"
}

out_directory = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/Plots/Plot"

plot_num = 1

######################################################################################################

# READ DATA

data = read.csv(data_file_name)

data = data[, c("mineid", "year", "dv", "total_injuries",
                "total_violations", "onsite_insp_hours", 
                "hours", "coal_prod", "employment", "operator_time", 
                "district", "apalachia", "safetycommittee")]

# make extra variables
data$dv_exp = data$dv / data$hours
data$dv_rel = data$dv / data$total_injuries
data$dv_rel_exp = data$dv_rel / data$hours
data[is.na(data)] = NA

rm(data_file_name)

######################################################################################################

# PLOTS WITH ALL MINES

for (dv in c("dv", "dv_rel", "dv_exp", "dv_rel_exp")) {
  
  if (dv == "dv") {
    y_lab = paste("Number of", injury, "Injuries", sep = " ")
  }
  if (dv == "dv_rel") {
    y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
  }
  if (dv == "dv_exp") {
    y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
  }
  if (dv == "dv_rel_exp") {
    y_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
  }
  
  out_file = paste(out_directory, plot_num, ".png", sep = "")
  jpeg(out_file)
  
  plot(data[data$mineid == data$mineid[1], "year"], data[data$mineid == data$mineid[1], dv], 
       xlim = c(min(data$year), max(data$year)),
       ylim = c(min(data[, dv], na.rm = TRUE), max(data[, dv], na.rm = TRUE)), 
       main = paste(y_lab, "\n(each line represents one mine)", sep = ""), 
       xlab = "Year", ylab = y_lab, cex.main = 0.75, cex.lab = 0.75,
       type = "l")
  
  for (i in 2:length(unique(data$mineid))) {
    lines(data[data$mineid == data$mineid[i], "year"], data[data$mineid == data$mineid[i], dv])
  }
  
  dev.off()
  plot_num = plot_num + 1
  
}

rm(dv, i, out_file, y_lab)

######################################################################################################

# TAKE YEAR AVG/MED/MAX

# year avg
data_year_avg = aggregate(data, list(data$year),
                          FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

# year median
data_year_med = aggregate(data, list(data$year),
                          FUN = function(x) median(as.numeric(x), na.rm = TRUE))

# year max
data_year_max = aggregate(data, list(data$year),
                          FUN = function(x) max(as.numeric(x), na.rm = TRUE))

######################################################################################################

# PLOTS OF YEAR AVG/MED/MAX

for (dv in c("dv", "dv_rel", "dv_exp", "dv_rel_exp")) {
  
  for (method in c("avg", "med", "max")) {
    
    if (dv == "dv") {
      y_lab = paste("Number of", injury, "Injuries", sep = " ")
    }
    if (dv == "dv_rel") {
      y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
    }
    if (dv == "dv_exp") {
      y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
    }
    if (dv == "dv_rel_exp") {
      y_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
    }
    
    if (method == "avg") {
      y_lab = paste("Average", y_lab, sep = " ")
      m = "average"
    }
    if (method == "med") {
      y_lab = paste("Median", y_lab, sep = " ")
      m = "median"
    }
    if (method == "max") {
      y_lab = paste("Max", y_lab, sep = " ")
      m = "max"
    }
    
    d = eval(parse(text = paste("data_year", method, sep = "_")))
    
    out_file = paste(out_directory, plot_num, ".png", sep = "")
    jpeg(out_file)
    
    plot(d$year, d[, dv], 
         main = paste(y_lab, "\n(line represents ", m, " across all mines)", sep = ""), 
         xlab = "Year", ylab = y_lab, cex.main = 0.75, cex.lab = 0.75,
         type = "l")
    
    dev.off()
    plot_num = plot_num + 1
    
  }
}

rm(d, dv, m, method, out_file, y_lab, data_year_avg, data_year_max, data_year_med)

######################################################################################################

# GROUP MINES BY (SOME) COVARIATES OF INTEREST

# groups of mines
# get percentile cutoffs for variables of interest
year_info = data.frame(unique(data$year))
names(year_info) = "year"
for (var in c("hours", "employment", "coal_prod")) {
  year_info[, c(paste(var, seq(5, 100, 5), sep = "_"))] = NA
}

for (var in c("hours", "employment", "coal_prod")) {
  for (p in seq(5, 100, 5)) {
    x = paste(var, p, sep = "_")
    for (i in 1:nrow(year_info)) {
      year = year_info$year[i]
      temp = data[data$year == year, ]
      q = quantile(temp[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[p + 1]
      year_info[i, x] = q
    }
  }
}

rm(temp, i, p, q, var, x, year)

# dynamic groups
cutoff_high = 80
cutoff_low = 50
for (var in c("hours", "employment", "coal_prod")) {
  new_var = paste(var, "dynamic", sep = "_")
  data[, new_var] = NA
  for (i in 1:nrow(data)) {
    year = data[i, "year"]
    data[i, new_var] = ifelse(data[i, var] >= year_info[year_info$year == year, paste(var, toString(cutoff_high), sep = "_")], 2, 
                              ifelse(data[i, var] < year_info[year_info$year == year, paste(var, toString(cutoff_low), sep = "_")], 0, 1))
  }
}

rm(cutoff_high, cutoff_low, i, new_var, var, year)

# static groups
make_static_var = function(mine_data, var) {
  static_var = paste(var, "static", sep = "_")
  dynamic_var = paste(var, "dynamic", sep = "_")
  mine_data[, static_var] = ifelse(nrow(mine_data) == sum(mine_data[, dynamic_var] == 2), 1, 0)
  return(mine_data)
}

data$hours_static = NA
data = ddply(data, "mineid", make_static_var, var = "hours")

data$employment_static = NA
data = ddply(data, "mineid", make_static_var, var = "employment")

data$coal_prod_static = NA
data = ddply(data, "mineid", make_static_var, var = "coal_prod")

rm(make_static_var, year_info)

######################################################################################################

# PLOTS OF YEAR AVG/MED/MAX BY GROUP OF (SOME) COVARIATES OF INTEREST

# collapse datasets by groups
for (var in c("district", "apalachia", "safetycommittee", 
              "hours_dynamic", "hours_static", 
              "employment_dynamic", "employment_static", 
              "coal_prod_dynamic", "coal_prod_static")) {
  for (level in unique(data[, var])) {
    assign(paste("data_year_avg", var, level, sep = "_"), 
           aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                     FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
    assign(paste("data_year_med", var, level, sep = "_"), 
           aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                     FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
    assign(paste("data_year_max", var, level, sep = "_"), 
           aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                     FUN = function(x) max(as.numeric(x), na.rm = TRUE)))
  }
}

rm(level, var)

# make plots
for (var in c("district", "apalachia", "safetycommittee", 
              "hours_dynamic", "hours_static", 
              "employment_dynamic", "employment_static", 
              "coal_prod_dynamic", "coal_prod_static")) {
  
  if (var == "district") {
    v = "District"
  }
  if (var == "apalachia") {
    v = "Whether Mine is in Appalachia"
  }
  if (var == "safetycommittee") {
    v = "Whether Mine has a Safety Committee"
  }
  if (var == "hours_dynamic") {
    v = "Size, determined by hours dynamically"
  }
  if (var == "hours_static") {
    v = "Size, determined by hours statically"
  }
  if (var == "employment_dynamic") {
    v = "Size, determined by employment dynamically"
  }
  if (var == "employment_static") {
    v = "Size, determined by employment statically"
  }
  if (var == "coal_prod_dynamic") {
    v = "Size, determined by production dynamically"
  }
  if (var == "coal_prod_static") {
    v = "Size, determined by production statically"
  }
  
  for (dv in c("dv", "dv_rel", "dv_exp", "dv_rel_exp")) {
    
    for (method in c("avg", "med", "max")) {
      
      if (dv == "dv") {
        y_lab = paste("Number of", injury, "Injuries", sep = " ")
      }
      if (dv == "dv_rel") {
        y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
      }
      if (dv == "dv_exp") {
        y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
      }
      if (dv == "dv_rel_exp") {
        y_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
      }
      
      if (method == "avg") {
        y_lab = paste("Average", y_lab, sep = " ")
        m = "average"
      }
      if (method == "med") {
        y_lab = paste("Median", y_lab, sep = " ")
        m = "median"
      }
      if (method == "max") {
        y_lab = paste("Max", y_lab, sep = " ")
        m = "max"
      }
      
      x = ls()[grepl(paste("data_year", method, var, sep = "_"), ls())]
      n = eval(parse(text = x[1]))
      for (j in 2:length(x)) {
        n2 = eval(parse(text = x[j]))
        n = rbind(n, n2)
      }
      
      lb = min(n[, dv])
      ub = max(n[, dv])
      c = palette(rainbow(length(unique(data[, var]))))
      
      out_file = paste(out_directory, plot_num, ".png", sep = "")
      jpeg(out_file)
      
      i = 1
      for (level in sort(unique(data[, var]))) {
        
        d = eval(parse(text = paste("data_year", method, var, level, sep = "_")))
        
        if (i == 1) {
          plot(d[, "year"], d[, dv], 
               main = paste(y_lab, "\nBy ", v, "\n(each line represents ", m, " across each group of mines)", sep = ""), 
               xlab = "Year", ylab = y_lab,
               ylim = c(lb, ub),
               type = "l", col = c[i], 
               cex.main = 0.75, cex.lab = 0.75)
        }
        else {
          lines(d[, "year"], d[, dv], col = c[i])
        }
        
        i = i + 1
        
      }
      
      legend("topright", 
             legend = sort(unique(data[, var])), 
             lty = c(rep(1, length(unique(data[, var])))), col = c, 
             cex = 0.5)
      
      dev.off()
      plot_num = plot_num + 1
      
    }
  }
}

rm(list = ls()[grepl("data_year", ls())])
rm(c, dv, i, j, lb, level, m, method, out_file, ub, v, var, x, y_lab, d, n, n2) 

######################################################################################################

# CREATE EXTRA OUTCOME VARIABLES FOR MINES

# correlation
get_cor = function(mine_data, var1, var2, var3) {
  if (!is.na(sd(mine_data[, var1])) & !is.na(sd(mine_data[, var2]))) {
    if (sd(mine_data[, var1]) != 0 & sd(mine_data[, var2]) != 0) {
      mine_data[, var3] = cor(mine_data[, var1], mine_data[, var2])
    }
    else {
      mine_data[, var3] = NA
    }
  }
  else {
    mine_data[, var3] = NA
  }
  return(mine_data)
}

# regression
get_reg = function(mine_data, var1, var2, var3) {
  if(nrow(mine_data[!is.na(mine_data[, var2]), ]) > 1) { 
    m = lm(mine_data[, var2] ~ mine_data[, var1])
    c = m$coefficients[2]
    mine_data[, var3] = c
  }
  else {
    mine_data[, var3] = NA
  }
  return(mine_data)
}

# fill in
for (dv in c("dv", "dv_exp", "dv_rel", "dv_rel_exp")) {
  for (m in c("cor", "reg")) {
    data[, paste(dv, "year", m, sep = "_")] = NA
    data = ddply(data, "mineid", 
                 eval(parse(text = paste("get", m, sep = "_"))), var1 = "year", var2 = dv, var3 = paste(dv, "year", m, sep = "_"))
  }
}

rm(dv, m, get_cor, get_reg)

######################################################################################################

# TAKE MINE AVG/MED/MAX

# mine avg
data_mine_avg = aggregate(data, list(data$mineid),
                          FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

# mine median
data_mine_med = aggregate(data, list(data$mineid),
                          FUN = function(x) median(as.numeric(x), na.rm = TRUE))

# mine max
data_mine_max = aggregate(data, list(data$mineid),
                          FUN = function(x) max(as.numeric(x), na.rm = TRUE))

######################################################################################################

# PLOTS OF MINE AVG/MED/MAX

for (dv in c("dv", "dv_rel", "dv_exp", "dv_rel_exp")) {
  
  for (method in c("avg", "med", "max")) {
    
    if (dv == "dv") {
      y_lab = paste("Number of", injury, "Injuries", sep = " ")
    }
    if (dv == "dv_rel") {
      y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
    }
    if (dv == "dv_exp") {
      y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
    }
    if (dv == "dv_rel_exp") {
      y_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
    }
    
    if (method == "avg") {
      y_lab = paste("Average", y_lab, sep = " ")
      m = "average"
    }
    if (method == "med") {
      y_lab = paste("Median", y_lab, sep = " ")
      m = "median"
    }
    if (method == "max") {
      y_lab = paste("Max", y_lab, sep = " ")
      m = "max"
    }
    
    d = eval(parse(text = paste("data_mine", method, sep = "_")))
    
    out_file = paste(out_directory, plot_num, ".png", sep = "")
    jpeg(out_file)
    
    plot(sort(d[, dv]), 
         main = paste(y_lab, "\nBy Mine, Sorted\n(each point represents ", m, " at one mine across all times)", sep = ""), 
         xlab = "Mine Index (Sorted by Outcome)", ylab = y_lab, cex.main = 0.75, cex.lab = 0.75)
    
    dev.off()
    plot_num = plot_num + 1
    
    out_file = paste(out_directory, plot_num, ".png", sep = "")
    jpeg(out_file)
    
    hist(d[, dv], main = paste("Histogram of", y_lab, sep = " "), xlab = y_lab, cex.main = 0.75, cex.lab = 0.75)
    
    dev.off()
    plot_num = plot_num + 1
    
  }
}

rm(d, dv, m, method, out_file, y_lab)

######################################################################################################

# PLOT 

for (dv in c("dv", "dv_exp", "dv_rel", "dv_rel_exp")) {
  for (m in c("cor", "reg")) {
  
  var = paste(dv, "year", m, sep = "_")

  if (dv == "dv") {
    y_lab = paste("Number of", injury, "Injuries", sep = " ")
  }
  if (dv == "dv_rel") {
    y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
  }
  if (dv == "dv_exp") {
    y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
  }
  if (dv == "dv_rel_exp") {
    y_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
  }
  
  if (m == "cor") {
    y_lab = paste("Correlation Between ", y_lab, " and Year", sep = "")
  }
  if (m == "reg") {
    y_lab = paste("Regression Coefficient of ", y_lab, " on Time", sep = "")
  }
  
  out_file = paste(out_directory, plot_num, ".png", sep = "")
  jpeg(out_file)
  
  plot(sort(data_mine_avg[, var]),
       main = paste(y_lab, "\nBy Mine, Sorted\n(each point represents ", m, " at one mine across all times)", sep = ""), 
       xlab = "Mine Index (Sorted by Outcome)", ylab = y_lab, cex.main = 0.75, cex.lab = 0.75)

  dev.off()
  plot_num = plot_num + 1
  
  out_file = paste(out_directory, plot_num, ".png", sep = "")
  jpeg(out_file)
  
  hist(data_mine_avg[, var], main = paste("Histogram of", y_lab, sep = " "), xlab = y_lab, cex.main = 0.75, cex.lab = 0.75)
  
  dev.off()
  plot_num = plot_num + 1
  
  }
}

######################################################################################################