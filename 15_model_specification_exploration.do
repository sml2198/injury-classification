// NIOSH Project 2014-N-15776

// 15 - Model Specification Exploration

// Last edit 10/12/16

********************************************************************************
********************************************************************************
// SETTINGS

* pause on
pause off

set more off

* local directory "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\MR\Estout\"
local directory "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\PS\Estout\"

* log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\MR\Log Files\MR_log_part_year.txt", text
* log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\MR\Log Files\MR_log_part_quarter.txt", text
* log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\MR\Log Files\MR_log_subpart_year.txt", text
* log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\MR\Log Files\MR_log_subpart_quarter.txt", text

* log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\PS\Log Files\PS_log_part_year.txt", text
* log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\PS\Log Files\PS_log_part_quarter.txt", text
* log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\PS\Log Files\PS_log_subpart_year.txt", text
log using "C:\Users\jbodson\Dropbox (Stanford Law School)\R-code\Injury-Classification\Model Summaries 10-12\PS\Log Files\PS_log_subpart_quarter.txt", text

* local injury_type "MR"
local injury_type "PS"

* local unit_of_analysis "year_sum"
* local unit_of_analysis "year_avg"
local unit_of_analysis "quarter"

* local violation_level "part"
local violation_level "subpart"

********************************************************************************
********************************************************************************
// DATA SET-UP

// read data 
if "`injury_type'" == "MR" {
	use "X:\Projects\Mining\NIOSH\analysis\data\5_prediction-ready\MR_prediction_data.dta", clear
}

if "`injury_type'" == "PS" {
	use "X:\Projects\Mining\NIOSH\analysis\data\5_prediction-ready\PS_prediction_data.dta", clear
}

// format variables
if "`injury_type'" == "MR" {
	rename MR dv
	rename MR_indicator dv_indicator
	local injury_label "MR"
	local relevant_parts "47 48 71 72 75 77"
}

if "`injury_type'" == "PS" {
	rename PS dv
	rename PS_indicator dv_indicator
	local injury_label "PS"
	local relevant_parts "48 75"
}

if "`violation_level'" == "part" {
	local violation_level_label "P"
}

if "`violation_level'" == "subpart" {
	local violation_level_label "SP"
}

* recast int p* sp* total_violations total_mine_act_violations num_good_faith num_no_terminations num_insp total_injuries dv dv_indicator

gen sample_pp = quarter >= 2007 // flag obs after or equal to 2007 to use in penlaty points analyses (assessments changed for the last time in 2007)

tostring quarter, replace
encode quarter, gen(quart)
drop quarter
rename quart time

local time_label "Q" // will be overwritten if unit_of_analysis is year_sum or year_avg

if "`unit_of_analysis'" == "year_sum" { // collapse to mine years - sums 
	local time_label "Y"
	
	// count number of complete quarters per year
	decode time, gen(year)
	replace year = regexs(0) if(regexm(year, "^[0-9][0-9][0-9][0-9]"))
	gen marker = 1
	bysort mineid year: egen num_quarts = sum(marker)
		/*tab num_quarts
			 num_quarts |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  1 |      1,100        3.63        3.63
					  2 |      1,666        5.50        9.13
					  3 |      2,511        8.29       17.42
					  4 |     25,012       82.58      100.00
			------------+-----------------------------------
				  Total |     30,289      100.00 - Sarah 9/21/2016 @ 10:54 PM */
	drop if num_quarts < 4
	
	// collapse to the mine-year 
	collapse (sum) p* sp* onsite_insp_hours hours dv* (firstnm) state sample_pp, by(mineid year)
	rename year time
	encode time, gen(time2)
	drop time
	rename time2 time
	bys mineid: gen mine_time = _n
	
	pause "complete: year_sum collapse"
}

if "`unit_of_analysis'" == "year_avg" { // collapse on mine years - averages
	local time_label "Y"
	decode time, gen(year)
	replace year = regexs(0) if(regexm(year, "^[0-9][0-9][0-9][0-9]"))
	pause "collapsing to mine-year"
	collapse (mean) p* sp* onsite_insp_hours hours dv* (firstnm) state sample_pp, by(mineid year)
	rename year time
	encode time, gen(time2)
	drop time
	rename time2 time
	bys mineid: gen mine_time = _n
	
	pause "complete: year_avg collapse"
}

drop dv_indicator
gen dv_binary = 0 if dv == 0
replace dv_binary = 1 if dv != 0
rename dv_binary dv_indicator
	
gen lnhours = log(hours)

pause "complete: data formatting"

// group variables
local covariates "mine_time onsite_insp_hours"

*** lag 0 ***
if "`violation_level'" == "part" {

	foreach x in `relevant_parts' {
		foreach var of varlist p`x' {
			local count_vars `count_vars' `var'
		}
	}
	noi di "`count_vars'"

	foreach var of varlist p*ss {
		local ss_vars `ss_vars' `var'
	}
	noi di "`ss_vars'"

	foreach var of varlist p*pp {
		local pp_vars `pp_vars' `var'
	}
	noi di "`pp_vars'"
	
	pause "complete: variable groups - part no lag"
}


if "`violation_level'" == "subpart" {

	foreach x in 1 2 3 4 5 6 7 8 9 0 {
		foreach var of varlist sp*`x' {
			local count_vars `count_vars' `var'
		}
	}
	noi di "`count_vars'"

	foreach var of varlist sp*ss {
		local ss_vars `ss_vars' `var'
	}
	noi di "`ss_vars'"

	foreach var of varlist sp*pp {
		local pp_vars `pp_vars' `var'
	}
	noi di "`pp_vars'"
	
	pause "complete: variable groups - subpart no lag"
}


*** lag 1 ***
if "`violation_level'" == "part" {

	foreach x in `relevant_parts' {
		foreach var of varlist p`x'_1lag  {
			local count_lag_1_vars `count_lag_1_vars' `var'
		}
	}
	noi di "`count_lag_1_vars'"

	foreach var of varlist p*ss_1lag  {
		local ss_lag_1_vars `ss_lag_1_vars' `var'
	}
	noi di "`ss_lag_1_vars'"

	foreach var of varlist p*pp_1lag  {
		local pp_lag_1_vars `pp_lag_1_vars' `var'
	}
	noi di "`pp_lag_1_vars'"
	
	pause "complete: variable groups - part lag 1"
}



if "`violation_level'" == "subpart" {

	foreach x in 1 2 3 4 5 6 7 8 9 0 {
		foreach var of varlist sp*`x'_1lag  {
			local count_lag_1_vars `count_lag_1_vars' `var'
		}
	}
	noi di "`count_lag_1_vars'"

	foreach var of varlist sp*ss_1lag  {
		local ss_lag_1_vars `ss_lag_1_vars' `var'
	}
	noi di "`ss_lag_1_vars'"


	foreach var of varlist sp*pp_1lag  {
		local pp_lag_1_vars `pp_lag_1_vars' `var'
	}
	noi di "`pp_lag_1_vars'"

	pause "complete: variable groups - subpart lag 1"
} 

*** lag 4 ***
if "`violation_level'" == "part" {
	
	foreach x in `relevant_parts' {
		foreach var of varlist p`x'_c_4lag  {
			local count_lag_4_vars `count_lag_4_vars' `var'
		}
	}
	noi di "`count_lag_4_vars'"

	foreach var of varlist p*ss_c_4lag {
		local ss_lag_4_vars `ss_lag_4_vars' `var'
	}
	noi di "`ss_lag_4_vars'"

	foreach var of varlist p*pp_c_4lag {
		local pp_lag_4_vars `pp_lag_4_vars' `var'
	}
	noi di "`pp_lag_4_vars'"

	pause "complete: variable groups - part lag 4"
}

if "`violation_level'" == "subpart" {

	foreach x in 1 2 3 4 5 6 7 8 9 0 {
		foreach var of varlist sp*`x'_c_4lag  {
			local count_lag_4_vars `count_lag_4_vars' `var'
		}
	}
	noi di "`count_lag_4_vars'"

	foreach var of varlist sp*ss_c_4lag {
		local ss_lag_4_vars `ss_lag_4_vars' `var'
	}
	noi di "`ss_lag_4_vars'"

	foreach var of varlist sp*pp_c_4lag {
		local pp_lag_4_vars `pp_lag_4_vars' `var'
	}
	noi di "`pp_lag_4_vars'"

	pause "complete: variable groups - subpart lag 4"
}

*** lag all ***
if "`violation_level'" == "part" {

	foreach x in `relevant_parts' {
		foreach var of varlist p`x'_c_lag_all  {
			local count_lag_all_vars `count_lag_all_vars' `var'
		}
	}
	noi di "`count_lag_all_vars'"

	foreach var of varlist p*ss_c_lag_all {
		local ss_lag_all_vars `ss_lag_all_vars' `var'
	}
	noi di "`ss_lag_all_vars'"

	foreach var of varlist p*pp_c_lag_all {
		local pp_lag_all_vars `pp_lag_all_vars' `var'
	}
	noi di "`pp_lag_all_vars'"

	pause "complete: variable groups - part lag all"
}

*** subpart lag all ***
if "`violation_level'" == "subpart" {

	foreach x in 1 2 3 4 5 6 7 8 9 0 {
		foreach var of varlist sp*`x'_c_lag_all  {
			local count_lag_all_vars `count_lag_all_vars' `var'
		}
	}
	noi di "`count_lag_all_vars'"
	
	foreach var of varlist sp*ss_c_lag_all {
		local ss_lag_all_vars `ss_lag_all_vars' `var'
	}
	noi di "`ss_lag_all_vars'"
	
	foreach var of varlist sp*pp_c_lag_all {
		local pp_lag_all_vars `pp_lag_all_vars' `var'
	}
	noi di "`pp_lag_all_vars'"

	pause "complete: variable groups - subpart lag all"
}

********************************************************************************
*******************************************************************************
// DATA OVERVIEW

// outcome variables
summ dv

pause "next"

tab dv_indicator
	  
pause "next"

if "`violation_level'" == "part" {
	summ `count_vars' `ss_vars' `pp_vars'
	
	pause "next"
	
	summ `count_lag_1_vars' `ss_lag_1_vars' `pp_lag_1_vars'
	
	pause "next"
	
	summ `count_lag_4_vars' `ss_lag_4_vars' `pp_lag_4_vars'
	
	pause "next"
	
	summ `count_lag_all_vars' `ss_lag_all_vars' `pp_lag_all_vars'
}

pause "complete: data overview"

*******************************************************************************/
/*******************************************************************************
// MODEL LABEL KEY

Model X.Y.Z
  X
    C: response variable is count of injuries
    B: response variable is binary of injuries
  Y
    V: predictors are number of violations
    SSV: predictors are number of significant and substantial violations
    PP: predictors are number of penalty points
  Z
    1: inj_t ~ viol_t
    2: inj_t ~ viol_(t_-1)
    3: inj_t ~ viol_(annual sum pre_t)
    4: inj_t ~ viol_(sum since study period) 

*******************************************************************************/
********************************************************************************
// NULL MODELS (V & SSV)

// count outcome (V & SSV)
glm dv, family(poisson) link(log) vce(cl mineid) exposure(hours) eform
	
pause "next"	
	
quietly poisson dv, vce(cl mineid) exposure(hours) irr
estat gof

pause "next"
	
glm dv, family(nbinomial) link(log) vce(cl mineid) exposure(hours) eform

pause "next"

nbreg dv, vce(cl mineid) exposure(hours) irr

pause "next"

predict c_null_yhat 
summ dv_indicator c_null_yhat

pause "complete: null model - count outcome"

// binary outcome (V & SSV)
logit dv_indicator, vce(cl mineid) offset(lnhours) or

pause "next"

lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict b_null_yhat 
summ dv_indicator b_null_yhat

pause "complete: null model - binary outcome"

*******************************************************************************/
********************************************************************************
// NULL MODELS (PP)

// count outcome (PP)
glm dv if sample_pp == 1, family(poisson) link(log) vce(cl mineid) exposure(hours) eform
	
pause "next"	
	
quietly poisson dv if sample_pp == 1, vce(cl mineid) exposure(hours) irr
estat gof

pause "next"
	
glm dv if sample_pp == 1, family(nbinomial) link(log) vce(cl mineid) exposure(hours) eform

pause "next"

nbreg dv if sample_pp == 1, vce(cl mineid) exposure(hours) irr

pause "next"

predict c_pp_null_yhat 
summ dv_indicator c_pp_null_yhat

pause "complete: null model - count outcome"

// binary outcome (PP)
logit dv_indicator if sample_pp == 1, vce(cl mineid) offset(lnhours) or

pause "next"

lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict b_pp_null_yhat 
summ dv_indicator b_pp_null_yhat

pause "complete: null model - binary outcome"

********************************************************************************
********************************************************************************

if "`violation_level'" == "subpart" {
	set matsize 11000, perm
	set emptycells drop
}

********************************************************************************
********************************************************************************

// Model C.V.1

// poisson model
glm dv `count_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `count_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `count_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `count_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.V.1.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `count_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cv1_yhat 
gen cv1_res = dv - cv1_yhat

summ dv cv1_yhat
/*
pause "next"

scatter dv cv1_yhat

pause "next"

scatter cv1_res dv

pause "next"

scatter cv1_res cv1_yhat
*/
pause "complete: C.V.1"

********************************************************************************
********************************************************************************

// Model C.V.2

// poisson model
glm dv `count_lag_1_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `count_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `count_lag_1_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `count_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.V.2.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `count_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cv2_yhat 
gen cv2_res = dv - cv2_yhat

summ dv cv2_yhat
/*
pause "next"

scatter dv cv2_yhat

pause "next"

scatter cv2_res dv

pause "next"

scatter cv2_res cv2_yhat
*/
pause "complete: C.V.2"

********************************************************************************
********************************************************************************

// Model C.V.3

// poisson model
glm dv `count_lag_4_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `count_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `count_lag_4_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `count_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.V.3.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `count_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cv3_yhat 
gen cv3_res = dv - cv3_yhat

summ dv cv3_yhat

pause "next"
/*
scatter dv cv3_yhat

pause "next"

scatter cv3_res dv

pause "next"

scatter cv3_res cv3_yhat
*/
pause "complete: C.V.3"

********************************************************************************
********************************************************************************

// Model C.V.4

// poisson model
glm dv `count_lag_all_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `count_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `count_lag_all_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `count_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.V.4.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `count_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cv4_yhat 
gen cv4_res = dv - cv4_yhat

summ dv cv4_yhat
/*
pause "next"

scatter dv cv4_yhat

pause "next"

scatter cv4_res dv

pause "next"

scatter cv4_res cv4_yhat
*/
pause "complete: C.V.4"

********************************************************************************
********************************************************************************

// Model C.SSV.1

// poisson model
glm dv `ss_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `count_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `ss_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `ss_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.SSV.1.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `ss_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cssv1_yhat 
gen cssv1_res = dv - cssv1_yhat

summ dv cssv1_yhat
/*
pause "next"

scatter dv cssv1_yhat

pause "next"

scatter cssv1_res dv

pause "next"

scatter cssv1_res cssv1_yhat
*/
pause "complete: C.SSV.1"

********************************************************************************
********************************************************************************

// Model C.SSV.2

// poisson model
glm dv `ss_lag_1_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `ss_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `ss_lag_1_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `ss_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.SSV.2.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `ss_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cssv2_yhat 
gen cssv2_res = dv - cssv2_yhat

summ dv cssv2_yhat
/*
pause "next"

scatter dv cssv2_yhat

pause "next"

scatter cssv2_res dv

pause "next"

scatter cssv2_res cssv2_yhat
*/
pause "complete: C.SSV.2"

********************************************************************************
********************************************************************************

// Model C.SSV.3

// poisson model
glm dv `ss_lag_4_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `ss_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `ss_lag_4_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `ss_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.SSV.3.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `ss_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cssv3_yhat 
gen cssv3_res = dv - cssv3_yhat

summ dv cssv3_yhat
/*
pause "next"

scatter dv cssv3_yhat

pause "next"

scatter cssv3_res dv

pause "next"

scatter cssv3_res cssv3_yhat
*/
pause "complete: C.SSV.3"

********************************************************************************
********************************************************************************

// Model C.SSV.4

// poisson model
glm dv `ss_lag_all_vars' `covariates' ib(freq).state ib(freq).time, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `ss_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `ss_lag_all_vars' `covariates' ib(freq).state ib(freq).time, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `ss_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.SSV.4.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `ss_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) exposure(hours) iter(50) irr
predict cssv4_yhat 
gen cssv4_res = dv - cssv4_yhat

summ dv cssv4_yhat
/*
pause "next"

scatter dv cssv4_yhat

pause "next"

scatter cssv4_res dv

pause "next"

scatter cssv4_res cssv4_yhat
*/
pause "complete: C.SSV.4"

********************************************************************************
********************************************************************************

// Model C.PP.1

// poisson model
glm dv `pp_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `pp_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `pp_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `pp_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.PP.1.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `pp_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
predict cpp1_yhat 
gen cpp1_res = dv - cpp1_yhat

summ dv cpp1_yhat
/*
pause "next"

scatter dv cpp1_yhat

pause "next"

scatter cpp1_res dv

pause "next"

scatter cpp1_res cpp1_yhat
*/
pause "complete: C.PP.1"

********************************************************************************
********************************************************************************

// Model C.PP.2

// poisson model
glm dv `pp_lag_1_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `pp_lag_1_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `pp_lag_1_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `pp_lag_1_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.PP.2.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `pp_lag_1_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
predict cpp2_yhat 
gen cpp2_res = dv - cpp2_yhat

summ dv cpp2_yhat
/*
pause "next"

scatter dv cpp2_yhat

pause "next"

scatter cpp2_res dv

pause "next"

scatter cpp2_res cpp2_yhat
*/
pause "complete: C.PP.2"

********************************************************************************
********************************************************************************

// Model C.PP.3

// poisson model
glm dv `pp_lag_4_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `pp_lag_4_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `pp_lag_4_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `pp_lag_4_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.PP.3.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `pp_lag_4_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
predict cpp3_yhat 
gen cpp3_res = dv - cpp3_yhat

summ dv cpp3_yhat
/*
pause "next"

scatter dv cpp3_yhat

pause "next"

scatter cpp3_res dv

pause "next"

scatter cpp3_res cpp3_yhat
*/
pause "complete: C.PP.3"

********************************************************************************
********************************************************************************

// Model C.PP.4

// poisson model
glm dv `pp_lag_all_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(poisson) link(log) vce(cl mineid) exposure(hours) iter(50) eform
	
quietly poisson dv `pp_lag_all_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
est store pois
estat gof
	
pause "next"

// negative binomial model
glm dv `pp_lag_all_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, family(nbinomial) link(log) vce(cl mineid) exposure(hours) iter(50) eform

pause "next"

eststo clear
eststo: nbreg dv `pp_lag_all_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.C.PP.4.csv"', replace plain wide p eform
est store nbin

pause "next"

// test for over-dispersion
lrtest pois nbin, stats force 

pause "next" 

// final model + diagnostics/assessment
quietly nbreg dv `pp_lag_all_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) exposure(hours) iter(50) irr
predict cpp4_yhat 
gen cpp4_res = dv - cpp4_yhat

summ dv cpp4_yhat
/*
pause "next"

scatter dv cpp4_yhat

pause "next"

scatter cpp4_res dv

pause "next"

scatter cpp4_res cpp4_yhat
*/
pause "complete: C.PP.4"

********************************************************************************
********************************************************************************

// Model B.V.1

eststo clear
eststo: logit dv_indicator `count_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.V.1.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bv1_yhat 
gen bv1_res = dv_indicator - bv1_yhat

summ dv_indicator bv1_yhat
/*
pause "next"

scatter dv_indicator bv1_yhat

pause "next"

scatter bv1_res dv_indicator

pause "next"

scatter bv1_res bv1_yhat
*/
pause "complete: B.V.1"

********************************************************************************
********************************************************************************

// Model B.V.2

eststo clear
eststo: logit dv_indicator `count_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.V.2.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bv2_yhat 
gen bv2_res = dv_indicator - bv2_yhat

summ dv_indicator bv2_yhat
/*
pause "next"

scatter dv_indicator bv2_yhat

pause "next"

scatter bv2_res dv_indicator

pause "next"

scatter bv2_res bv2_yhat
*/
pause "complete: B.V.2"

********************************************************************************
********************************************************************************

// Model B.V.3

eststo clear
eststo: logit dv_indicator `count_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.V.3.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bv3_yhat 
gen bv3_res = dv_indicator - bv3_yhat

summ dv_indicator bv3_yhat
/*
pause "next"

scatter dv_indicator bv3_yhat

pause "next"

scatter bv3_res dv_indicator

pause "next"

scatter bv3_res bv3_yhat
*/
pause "complete: B.V.3"

********************************************************************************
********************************************************************************

// Model B.V.4

eststo clear
eststo: logit dv_indicator `count_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.V.4.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bv4_yhat 
gen bv4_res = dv_indicator - bv4_yhat

summ dv_indicator bv4_yhat
/*
pause "next"

scatter dv_indicator bv4_yhat

pause "next"

scatter bv4_res dv_indicator

pause "next"

scatter bv4_res bv4_yhat
*/
pause "complete: B.V.4"

********************************************************************************
********************************************************************************

// Model B.SSV.1

eststo clear
eststo: logit dv_indicator `ss_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.SSV.1.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bssv1_yhat 
gen bssv1_res = dv_indicator - bssv1_yhat

summ dv_indicator bssv1_yhat
/*
pause "next"

scatter dv_indicator bssv1_yhat

pause "next"

scatter bssv1_res dv_indicator

pause "next"

scatter bssv1_res bssv1_yhat
*/
pause "complete: B.SSV.1"

********************************************************************************
********************************************************************************

// Model B.SSV.2

eststo clear
eststo: logit dv_indicator `ss_lag_1_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.SSV.2.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bssv2_yhat 
gen bssv2_res = dv_indicator - bssv2_yhat

summ dv_indicator bssv2_yhat
/*
pause "next"

scatter dv_indicator bssv2_yhat

pause "next"

scatter bssv2_res dv_indicator

pause "next"

scatter bssv2_res bssv2_yhat
*/
pause "complete: B.SSV.2"

********************************************************************************
********************************************************************************

// Model B.SSV.3

eststo clear
eststo: logit dv_indicator `ss_lag_4_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.SSV.3.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bssv3_yhat 
gen bssv3_res = dv_indicator - bssv3_yhat

summ dv_indicator bssv3_yhat
/*
pause "next"

scatter dv_indicator bssv3_yhat

pause "next"

scatter bssv3_res dv_indicator

pause "next"

scatter bssv3_res bssv3_yhat
*/
pause "complete: B.SSV.3"

********************************************************************************
********************************************************************************

// Model B.SSV.4

eststo clear
eststo: logit dv_indicator `ss_lag_all_vars' `covariates' ib(freq).state ib(freq).time, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.SSV.4.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bssv4_yhat 
gen bssv4_res = dv_indicator - bssv4_yhat

summ dv_indicator bssv4_yhat
/*
pause "next"

scatter dv_indicator bssv4_yhat

pause "next"

scatter bssv4_res dv_indicator

pause "next"

scatter bssv4_res bssv4_yhat
*/
pause "complete: B.SSV.4"

********************************************************************************
********************************************************************************

// Model B.PP.1

eststo clear
eststo: logit dv_indicator `pp_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.PP.1.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bpp1_yhat 
gen bpp1_res = dv_indicator - bpp1_yhat

summ dv_indicator bpp1_yhat
/*
pause "next"

scatter dv_indicator bpp1_yhat

pause "next"

scatter bpp1_res dv_indicator

pause "next"

scatter bpp1_res bpp1_yhat
*/
pause "complete: B.PP.1"

********************************************************************************
********************************************************************************

// Model B.PP.2

eststo clear
eststo: logit dv_indicator `pp_lag_1_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.PP.2.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bpp2_yhat 
gen bpp2_res = dv_indicator - bpp2_yhat

summ dv_indicator bpp2_yhat
/*
pause "next"

scatter dv_indicator bpp2_yhat

pause "next"

scatter bpp2_res dv_indicator

pause "next"

scatter bpp2_res bpp2_yhat
*/
pause "complete: B.PP.2"

********************************************************************************
********************************************************************************

// Model B.PP.3

eststo clear
eststo: logit dv_indicator `pp_lag_4_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.PP.3.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bpp3_yhat 
gen bpp3_res = dv_indicator - bpp3_yhat

summ dv_indicator bpp3_yhat
/*
pause "next"

scatter dv_indicator bpp3_yhat

pause "next"

scatter bpp3_res dv_indicator

pause "next"

scatter bpp3_res bpp3_yhat
*/
pause "complete: B.PP.3"

********************************************************************************
********************************************************************************

// Model B.PP.4

eststo clear
eststo: logit dv_indicator `pp_lag_all_vars' `covariates' ib(freq).state ib(freq).time if sample_pp == 1, vce(cl mineid) offset(lnhours) iter(50) or
esttab using `"`directory'Model.`injury_label'.`time_label'.`violation_level_label'.B.PP.4.csv"', replace plain wide p eform

pause "next"

// diagnostics/assessment
lfit

pause "next"

linktest

pause "next"

estat classification

pause "next"

predict bpp4_yhat 
gen bpp4_res = dv_indicator - bpp4_yhat

summ dv_indicator bpp4_yhat
/*
pause "next"

scatter dv_indicator bpp4_yhat

pause "next"

scatter bpp4_res dv_indicator

pause "next"

scatter bpp4_res bpp4_yhat
*/
pause "complete: B.PP.4"

********************************************************************************
********************************************************************************

log close

********************************************************************************
********************************************************************************
