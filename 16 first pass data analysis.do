// NIOSH Project 2014-N-15776

/* 
16 - Who am I?
	What do I do?
*/

// Last edit 9/13/16

********************************************************************************

/*
Online Resources
*/
pause on 

********************************************************************************

// DATA SET-UP

// read in data 
use "X:/Projects/Mining/NIOSH/analysis/data/5_prediction-ready/MR_prediction_data.dta", clear

// make variables the right type
recast int p* sp* total_violations total_mine_act_violations num_good_faith num_no_terminations num_insp total_injuries MR MR_indicator

tostring quarter, replace
encode quarter, gen(quart)
drop quarter
rename quart quarter

********************************************************************************

// group variables
foreach var of varlist p* sp* {
	local violation_vars `violation_vars' `var'
}
// noi di "`violation_vars'"

foreach var of varlist p* {
	local part_vars `part_vars' `var'
}
// noi di "`part_vars'"

foreach var of varlist sp* {
	local subpart_vars `subpart_vars' `var'
}
// noi di "`subpart_vars'"

foreach var of varlist *sigandsub {
	local sig_sub_vars `sig_sub_vars' `var'
}
// noi di "`sig_sub_vars'"

foreach var of varlist *penaltypoints {
	local penalty_point_vars `penalty_point_vars' `var'
}
// noi di "`penalty_point_vars'"

foreach x in 1 2 3 4 5 6 7 8 9 0 {
	foreach var of varlist *`x' {
		local violation_count_vars `violation_count_vars' `var'
	}
}
// noi di "`violation_count_vars'"

foreach var of varlist p*sigandsub {
	local part_sig_sub_vars `part_sig_sub_vars' `var'
}
// noi di "`part_sig_sub_vars'"

foreach var of varlist sp*sigandsub {
	local subpart_sig_sub_vars `subpart_sig_sub_vars' `var'
}
// noi di "`subpart_sig_sub_vars'"

foreach var of varlist p*penaltypoints {
		local part_penalty_point_vars `part_penalty_point_vars' `var'
}
// noi di "`part_penalty_point_vars'"

foreach var of varlist sp*penaltypoints {
	local subpart_penalty_point_vars `subpart_penalty_point_vars' `var'
}
// noi di "`subpart_penalty_point_vars'"

foreach x in 47 48 71 72 75 77 {
	foreach var of varlist p*`x' {
		local part_violation_count_vars `part_violation_count_vars' `var'
	}
}
// noi di "`part_violation_count_vars'"

foreach x in 1 2 3 4 5 6 7 8 9 0 {
	foreach var of varlist sp*`x' {
		local subpart_violation_count_vars `subpart_violation_count_vars' `var'
	}
}
// noi di "`subpart_violation_count_vars'"

local covariates "mine_time onsite_insp_hours"
// noi di "`covariates'"

pause "after locals"

********************************************************************************

/* 
Model Label Key: X.Y.Z.N
  X
    P: predictors are part-level violations
    SP: predictors are subpart-level violations
  Y
    C: response variable is count of injuries
    B: response variable is binary of injuries
  Z
    V: predictors are number of violations
    SSV: predictors are number of significant and substantial violations
    PP: predictors are number of penalty points for violations
  N
    1: inj_t ~ viol_t
    2: inj_t ~ viol_(t_-1)
    3: inj_t ~ viol_(annual avg pre_t)
    4: inj_t ~ viol_(avg since study period) 
*/

********************************************************************************

// RUNNING MODELS

// Model P.C.V.1
glm MR `part_violation_count_vars' `covariates' ib(freq).state ib(freq).quart, cl(mineid) exp(hours) family(poisson) link(log) iter(200) irls eform

pause "after model"

// Model P.C.V.2

// Model P.C.V.3

// Model P.C.V.4

// Model P.C.SSV.1

// Model P.C.SSV.2

// Model P.C.SSV.3

// Model P.C.SSV.4

// Model P.C.PP.1

// Model P.C.PP.2

// Model P.C.PP.3

// Model P.C.PP.4

// Model P.B.V.1

// Model P.B.V.2

// Model P.B.V.3

// Model P.B.V.4

// Model P.B.SSV.1

// Model P.B.SSV.2

// Model P.B.SSV.3

// Model P.B.SSV.4

// Model P.B.PP.1

// Model P.B.PP.2

// Model P.B.PP.3

// Model P.B.PP.4

// Model SP.C.V.1

// Model SP.C.V.2

// Model SP.C.V.3

// Model SP.C.V.4

// Model SP.C.SSV.1

// Model SP.C.SSV.2

// Model SP.C.SSV.3

// Model SP.C.SSV.4

// Model SP.C.PP.1

// Model SP.C.PP.2

// Model SP.C.PP.3

// Model SP.C.PP.4

// Model SP.B.V.1

// Model SP.B.V.2

// Model SP.B.V.3

// Model SP.B.V.4

// Model SP.B.SSV.1

// Model SP.B.SSV.2

// Model SP.B.SSV.3

// Model SP.B.SSV.4

// Model SP.B.PP.1

// Model SP.B.PP.2

// Model SP.B.PP.3

// Model SP.B.PP.4

********************************************************************************
