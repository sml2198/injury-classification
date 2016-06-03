##HEADER##

merged_assessments = readRDS("X:/Projects/Mining/NIOSH/analysis/data/3_merged/merged_assessments.rds")

merged_assessments = merged_assessments[, c(-grep("samples", names(merged_assessments)), -grep("sic", names(merged_assessments)), 
                                            -grep("(latitude|longitude|idate|idesc)", names(merged_assessments)))]

##VIOLATIONS & ASSESSMENTS##

#Dummy out CFR codes (at the subpart and subsection levels) only for *relevant types and mark all non-relevant CFR codes

#Create CFR-code (only for *relevant) specific for penalty variables:
#penalty pts, good faith, gravity and its components as well & sig. and sub., negligence, # of repeat violations for operators and ind. contractors,
## of overall violations for operators and ind. contractors, create our own previous violations var and compare to the given version for verification,
#(NOT by *relevant CFR-code) appropriateness: size of mine in tonnage, size of controlling entity in tonnage, size of indep. contractor in annual hrs. worked

#Question: Check if operator variables vary by mine? Are indep. contractors the only operators @ a mine or are they only a part of the operation?

##INSPECTIONS##

#Hours, # of regular inspections, # of special inspections (work out issue of over-counting inspection hours over violations data)

