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
  out_directory = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/MR Plots/Plot"
}
if (injury == "PS") {
  data_file_name ="C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/data/PS-data.csv"
  out_directory = "C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/PS Plots/Plot"
}

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

# TAKE YEAR AVG/MED

# year avg
data_year_avg = aggregate(data, list(data$year),
                          FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

# year median
data_year_med = aggregate(data, list(data$year),
                          FUN = function(x) median(as.numeric(x), na.rm = TRUE))

######################################################################################################

# PLOTS OF YEAR AVG/MED/MAX

for (dv in c("dv_exp", "dv_rel_exp")) {
  
  for (method in c("avg", "med")) {
    
    if (dv == "dv_exp") {
      y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
    }
    if (dv == "dv_rel_exp") {
      y_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
    }
    
    if (method == "avg") {
      y_lab = paste("Mean", y_lab, sep = " ")
    }
    if (method == "med") {
      y_lab = paste("Median", y_lab, sep = " ")
    }
    
    d = eval(parse(text = paste("data_year", method, sep = "_")))
    
    out_file = paste(out_directory, plot_num, ".svg", sep = "")
    svg(out_file)
    
    plot(d$year, d[, dv], 
         xlab = "Year", ylab = y_lab, cex.main = 0.75, cex.lab = 0.75,
         type = "l")
    
    dev.off()
    plot_num = plot_num + 1
    
  }
}

rm(d, dv, method, out_file, y_lab, data_year_avg, data_year_med)

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
  mine_data[, static_var] = ifelse(nrow(mine_data) == sum(mine_data[, dynamic_var] == 2), 2, 
                                   ifelse(nrow(mine_data) == sum(mine_data[, dynamic_var] >= 1), 1, 0))
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

data$hours_dynamic = factor(data$hours_dynamic, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
data$hours_static = factor(data$hours_static, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
data$apalachia = factor(data$apalachia, levels = c(0, 1), labels = c("Out", "In"))

# collapse datasets by groups
for (var in c("hours_dynamic", "hours_static", "apalachia")) {
  for (level in unique(data[, var])) {
    assign(paste("data_year_avg", var, level, sep = "_"), 
           aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                     FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
    assign(paste("data_year_med", var, level, sep = "_"), 
           aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                     FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
  }
}

rm(level, var)


# make plots
for (var in c("hours_dynamic", "hours_static", "apalachia")) {
  
  color = palette(rainbow(length(unique(data[, var]))))
  color = palette(rainbow(length(unique(data[, var]))))
  
  for (dv in c("dv_exp", "dv_rel_exp")) {

    for (method in c("avg", "med")) {

      if (dv == "dv_exp") {
        y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
      }
      if (dv == "dv_rel_exp") {
        y_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
      }
      
      if (method == "avg") {
        y_lab = paste("Mean", y_lab, sep = " ")
      }
      if (method == "med") {
        y_lab = paste("Median", y_lab, sep = " ")
      }
      
      x = ls()[grepl(paste("data_year", method, var, sep = "_"), ls())]
      n = eval(parse(text = x[1]))
      for (j in 2:length(x)) {
        n2 = eval(parse(text = x[j]))
        n = rbind(n, n2)
      }
      
      lb = min(n[, dv])
      ub = max(n[, dv])

      out_file = paste(out_directory, plot_num, ".svg", sep = "")
      svg(out_file)

      i = 1
      for (level in sort(unique(data[, var]))) {

        d = eval(parse(text = paste("data_year", method, var, level, sep = "_")))
        
        if (i == 1) {
          plot(d[, "year"], d[, dv], 
               xlab = "Year", ylab = y_lab,
               ylim = c(lb, ub),
               type = "l", col = color[i], 
               cex.main = 0.75, cex.lab = 0.75)
        }
        else {
          lines(d[, "year"], d[, dv], col = color[i])
        }
        
        i = i + 1
        
      }
      
      legend("topright", 
             legend = sort(unique(data[, var])), 
             lty = c(rep(1, length(unique(data[, var])))), col = color, 
             cex = 0.5)
      
      dev.off()
      plot_num = plot_num + 1
      
    }
  }
}

rm(list = ls()[grepl("data_year", ls())])
rm(color, dv, i, j, lb, level, method, out_file, ub, var, x, y_lab, d, n, n2) 

######################################################################################################

# TAKE MINE AVG/MED/MAX

# mine avg
data_mine_avg = aggregate(data, list(data$mineid),
                          FUN = function(x) mean(as.numeric(x), na.rm = TRUE))

# mine median
data_mine_med = aggregate(data, list(data$mineid),
                          FUN = function(x) median(as.numeric(x), na.rm = TRUE))

######################################################################################################

# PLOTS OF MINE AVG/MED/MAX

for (dv in c("dv_exp", "dv_rel_exp")) {
  
  for (method in c("avg", "med")) {
    
    if (dv == "dv_exp") {
      x_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
    }
    if (dv == "dv_rel_exp") {
      x_lab = paste("Share of Total Injuries that are", injury, "Injuries, per Hour Worked", sep = " ")
    }
    
    if (method == "avg") {
      x_lab = paste("Mean", x_lab, sep = " ")
    }
    if (method == "med") {
      x_lab = paste("Median", x_lab, sep = " ")
    }
    
    d = eval(parse(text = paste("data_mine", method, sep = "_")))
    
    out_file = paste(out_directory, plot_num, ".svg", sep = "")
    svg(out_file)
    
    hist(d[, dv], main = NULL, xlab = x_lab, cex.main = 0.75, cex.lab = 0.75)
    
    dev.off()
    plot_num = plot_num + 1
    
  }
}

rm(list = ls())

######################################################################################################