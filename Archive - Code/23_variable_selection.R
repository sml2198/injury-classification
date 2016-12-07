# NIOSH Project 2014-N-15776

# 23 - Variable Selection

# Last edit 11/11/16

######################################################################################################

library(rpart)
library(rpart.plot)
library(randomForest)

# define file names
  # input: MR data
MR_in_file_name = "C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/data/MR-data.csv"
  # input: PS data
PS_in_file_name = "C:/Users/slevine2/Dropbox (Stanford Law School)/R-code/data/PS-data.csv"

  # output: 
MR_out_file_name = ""
  # output: 
PS_out_file_name = ""

# set injury type
injury.type = MR

######################################################################################################

# load data 
if (injury.type == "MR") {
  data = read.csv(MR_in_file_name, header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}
if (injury.type == "PS") {
  data = read.csv(PS_in_file_name, header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

######################################################################################################

# CaRT

cart = rpart(dv ~ ., data[, c(-match("dv_indicator", names(data)),
                              -match("total_injuries", names(data)))], method = "class")
cart 

# plot results & splits
rpart.plot(cart, fallen.leaves = T)
printcp(cart) 

plot(cart, uniform = TRUE, 
     main = "Classification Tree for Injuries")
text(cart, use.n = TRUE, all = TRUE, cex = .5)


######################################################################################################

# RANDOM FOREST

rf = randomForest(dv ~ ., data[, c(-match("dv_indicator", names(data)),
                                  -match("total_injuries", names(data)))], 
                  mtry = 15, importance = TRUE, type = "class", ntree = 500)
rf

######################################################################################################
