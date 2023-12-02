***************************************************************
* File name: zearn_analysis.do								  *
* Author: Ramon Silvera Zumaran								  *
* Purpose: Zearn Main and Per Protocol Analyses				  *
* Date: 13 Jul 2023										   	  *
***************************************************************

clear all
set more off
set type double
set scheme plotplain

cd "" // Edit to run file

*************************
**# Defining Programs #**
*************************

* Data Transformation and BH Adjustment Program
/* This program performs a Benjamini-Hochberg Adjustment for multiple hypothesis
   testing and summarizes the original results alongside the adjusted p-values */

cap drop bhadjustment

program define bhadjustment
	args cond_num
	
	matrix results = r(table)
	local colnames : colfullnames results
	local colnum : colsof results
	local cond_num = `cond_num'
	
	gen category_num = .
	gen category = ""
	gen coef = .
	gen standard_error = .
	gen p_val = .
	gen p_val_BH = .
	
	forvalues j = 1/`cond_num' {
		levelsof condition_cat if condition == `j', local(level) clean
		replace category_num = `j' in `j'
		replace category = "`level'" in `j'
		replace coef = results[1,`j'] in `j'
		replace standard_error = results[2,`j'] in `j'
		replace p_val = results[4,`j'] in `j'
	}
	
	keep category_num-p_val
	
	drop if mi(category)
	
	sort category_num
	drop category_num
	
	sort p_val
	gen rank = _n

	gen p_val_BH = p_val*(`cond_num'/rank)

	forvalues i = 1/`cond_num' {
		local num = `cond_num'+1
		local ind = `num'-`i'
		if p_val_BH[`ind'+1] < p_val_BH[`ind'] {
			replace p_val_BH = p_val_BH[`ind'+1] in `ind'
		}
	}
	
	replace p_val_BH = round(p_val_BH, 0.001)

	drop rank
	
	list

end

***************************************
**# Defining globals for covariates #**
***************************************
/* These are our pre-registered covariates for both our primary and secondary
   DV regressions. */

* Main Analysis: Badges
global badges_covs perc_G1 perc_G2 perc_G4 perc_G5 perc_G6 perc_G7 perc_G8 is_free teacher_badges_pre pre_intv_logins total_associated_students n_classes acct_age missing_classroom_grade opened_weekneg1 opened_week0

* Main Analysis: Minutes
global minutes_covs perc_G1 perc_G2 perc_G4 perc_G5 perc_G6 perc_G7 perc_G8 is_free teacher_mins_pre pre_intv_logins total_associated_students n_classes acct_age missing_classroom_grade opened_weekneg1 opened_week0

* Main Analysis: Logins
global logins_covs perc_G1 perc_G2 perc_G4 perc_G5 perc_G6 perc_G7 perc_G8 is_free pre_intv_logins total_associated_students n_classes acct_age missing_classroom_grade opened_weekneg1 opened_week0

********************************************
**# Primary and Secondary DV Regressions #**
********************************************

* Student Math Performance
use "megastudy_clean.dta", clear
// Pre-registered regression
areg teacher_badges_during ib16.condition $badges_covs [aweight=total_associated_students], absorb(combined_school_id)
// Wald test for checking whether friday and weekly treatment are postive 
test 7.condition 8.condition
test 8.condition-7.condition=0
local sign_car = sign(_b[8.condition]-_b[7.condition])
display "H_1: Wednesday >= Friday p-value = " normal(`sign_car'*sqrt(r(F)))

