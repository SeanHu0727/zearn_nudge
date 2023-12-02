clear
cd ""

import delimited "raw_data/BCFG - Email Opens and Clicks by Date 2022-04-08T1211.csv", stringcols(2) encoding(ISO-8859-1)
drop v1
drop if emailname == "BCFG_StudyWideMessage 09.01.2021" | emailname == "BCFG_StudyWideMessage 09.08.2021" | emailname == "BCFG_StudyWideMessage 10.13.2021"

gen click = .
replace click = 1 if clickedemailyesno == "Yes"
replace click = 0 if clickedemailyesno == "No"

gen open = .
replace open = 1 if openedemailyesno == "Yes"
replace open = 0 if openedemailyesno == "No"

gen deliver_dow = date(emaildelivereddate, "YMD")
replace deliver_dow = dow(deliver_dow)
tab deliver_dow, gen(deliver_dow)
// 1 for Sunday, 2 for Monday, and so on.

gen open_dow = date(emailopeneddate, "YMD")
replace open_dow = dow(open_dow)
tab open_dow, gen(open_dow)
// 1 for Sunday, 2 for Monday, and so on.

collapse (mean) click open deliver_dow1 deliver_dow2 deliver_dow3 deliver_dow4 deliver_dow5 deliver_dow6 deliver_dow7 open_dow1 open_dow2 open_dow3 open_dow4 open_dow5 open_dow6 open_dow7, by(useridpseudonymized)
rename useridpseudonymized teacher_id
save "BCFG-Email-Opens-summary_wo_first_email.dta"

clear
use "raw_data/megastudy_clean.dta"
drop _merge 

merge 1:1 teacher_id using "raw_data/BCFG-Email-Opens-summary_wo_first_email.dta"

drop if _merge == 2

ttest click if condition_cat == "Nudging Friday" | condition_cat == "Nudging Weekly", by(condition_cat)

// mean deliver_dow6 if condition_cat == "Nudging Friday"


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


tab condition, gen(effect)

replace effect9 = 1 if effect8 == 1

areg teacher_badges_during effect1 effect2 effect3 effect4 effect5 effect6 effect7 effect8 effect9 effect10 effect11 effect12 effect13 effect14 effect15 $badges_covs [aweight=total_associated_students], absorb(combined_school_id)
