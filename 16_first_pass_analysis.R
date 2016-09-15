# NIOSH Project 2014-N-15776

# 16 - Who am I?
  # What do I do?

# Last edit 9/13/16

#####################################################################################################

# Online Resources

# General GLM
  # http://www.utstat.toronto.edu/~brunner/oldclass/2201s11/readings/glmbook.pdf
  # http://www.statmethods.net/advstats/glm.html
  # http://www.uphs.upenn.edu/dgimhsr/documents/ispor15.glmworkshop.glick.2.pdf

# Poisson GLM
  # https://onlinecourses.science.psu.edu/stat504/node/168
  # https://onlinecourses.science.psu.edu/stat504/node/169
  # http://datavoreconsulting.com/programming-tips/count-data-glms-choosing-poisson-negative-binomial-zero-inflated-poisson/
  # http://www.biostat.umn.edu/~dipankar/bmtry711.11/lecture_13.pdf
  # http://stats.stackexchange.com/questions/176918/poisson-vs-quasi-poisson
  # http://stats.stackexchange.com/questions/66586/is-there-a-test-to-determine-whether-glm-overdispersion-is-significant
  # http://data.princeton.edu/wws509/notes/c4.pdf

# Probit/Logit GLM
  # http://www.biostat.umn.edu/~dipankar/bmtry711.11/lecture_12.pdf
  # http://stats.stackexchange.com/questions/20523/difference-between-logit-and-probit-models/30909#30909

# Other Modeling Options
  # https://core.ac.uk/download/pdf/6303254.pdf

######################################################################################################

library(reshape)
library(MASS)
# library(clusterSEs)
library(mfx)

# input: part-level data
data_file_name = "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.rds"

######################################################################################################

# DATA SET-UP

# read in data 
data = readRDS(data_file_name)

# rename variables
data = rename(data, c(coal_prod_qtr = "coal_prod", 
                      employment_qtr = "employment",
                      hours_qtr = "hours",
                      goodfaithind = "num_good_faith",
                      onsite_insp_hours_per_qtr = "onsite_insp_hours"))

names(data)[grep("^[0-9]", names(data))] = paste("p", names(data)[grep("^[0-9]", names(data))], sep = "")
names(data)[grep("[0-9].[0-9]", names(data))] = paste("s", names(data)[grep("[0-9].[0-9]", names(data))], sep = "")

# reformat variables
data$mineid = as.character(data$mineid)
data$quarter = as.numeric(data$quarter)
data$MR_indicator = as.factor(data$MR_indicator)

# group variables
violation_vars = names(data)[grep("^p+[0-9]|^sp+[0-9]", names(data))]
part_vars = violation_vars[grep("^p", violation_vars)]
subpart_vars = violation_vars[grep("^sp", violation_vars)]

sig_sub_vars = violation_vars[grep("sigandsub$", violation_vars)]
penalty_point_vars = violation_vars[grep("penaltypoints$", violation_vars)]
violation_count_vars = setdiff(violation_vars, union(sig_sub_vars, penalty_point_vars))

part_sig_sub_vars = intersect(part_vars, sig_sub_vars)
subpart_sig_sub_vars = intersect(subpart_vars, sig_sub_vars)
part_penalty_point_vars = intersect(part_vars, penalty_point_vars)
subpart_penalty_point_vars = intersect(subpart_vars, penalty_point_vars)
part_violation_count_vars = intersect(part_vars, violation_count_vars)
subpart_violation_count_vars = intersect(subpart_vars, violation_count_vars)

######################################################################################################

# Model Label Key: X.Y.Z.N
  # X
    # P: predictors are part-level violations
    # SP: predictors are subpart-level violations
  # Y
    # C: response variable is count of injuries
    # B: response variable is binary of injuries
  # Z
    # V: predictors are number of violations
    # SSV: predictors are number of significant and substantial violations
    # PP: predictors are number of penalty points for violations
  # N
    # 1: inj_t ~ viol_t
    # 2: inj_t ~ viol_(t_-1)
    # 3: inj_t ~ viol_(annual avg pre_t)
    # 4: inj_t ~ viol_(avg since study period)

######################################################################################################

# Running and Testing Models

# Model P.C.V.1

# Model P.C.V.2

# Model P.C.V.3

# Model P.C.V.4

# Model P.C.SSV.1

# Model P.C.SSV.2

# Model P.C.SSV.3

# Model P.C.SSV.4

# Model P.C.PP.1

# Model P.C.PP.2

# Model P.C.PP.3

# Model P.C.PP.4

# Model P.B.V.1

# Model P.B.V.2

# Model P.B.V.3

# Model P.B.V.4

# Model P.B.SSV.1

# Model P.B.SSV.2

# Model P.B.SSV.3

# Model P.B.SSV.4

# Model P.B.PP.1

# Model P.B.PP.2

# Model P.B.PP.3

# Model P.B.PP.4

# Model SP.C.V.1

# Model SP.C.V.2

# Model SP.C.V.3

# Model SP.C.V.4

# Model SP.C.SSV.1

# Model SP.C.SSV.2

# Model SP.C.SSV.3

# Model SP.C.SSV.4

# Model SP.C.PP.1

# Model SP.C.PP.2

# Model SP.C.PP.3

# Model SP.C.PP.4

# Model SP.B.V.1

# Model SP.B.V.2

# Model SP.B.V.3

# Model SP.B.V.4

# Model SP.B.SSV.1

# Model SP.B.SSV.2

# Model SP.B.SSV.3

# Model SP.B.SSV.4

# Model SP.B.PP.1

# Model SP.B.PP.2

# Model SP.B.PP.3

# Model SP.B.PP.4

######################################################################################################
