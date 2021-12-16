
* 11-dta-reg-data-ur13.do

* Change project root:
glo projroot "/Users/ioi/Documents/telehealth-v2"

clear
set more off
set rmsg on
cd "${projroot}"

* Omitted from the analysis: "CA" (97), "LA" (95), "OK" (97), "TX" (97), "HI" (99), "KY" (01)
u "dta/09_analysis_data_ur13.dta" if !inlist(usps, "CA", "LA", "OK", "TX", "HI", "KY"), clear

* 1. Add indicator for treated states with at least 3 post periods ------------

tab usps if !missing(policy_year_private_parity_law) & (policy_year_private_parity_law <= 2014)
di r(r)   // 14
gen s3    = !missing(policy_year_private_parity_law) & (policy_year_private_parity_law <= 2014)

* 2. Add indicator for never treated states -----------------------------------

tab usps if missing(policy_year_private_parity_law) & !inlist(usps, "AK", "AR", "CO", "CT", "DE", "MN", "NY")
di r(r)   // 21
gen c     = missing(policy_year_private_parity_law) & !inlist(usps, "AK", "AR", "CO", "CT", "DE", "MN", "NY") 

* 2. Prepare variables --------------------------------------------------------

egen area  = group(usps ur13_code)
egen state = group(usps)

gen treat = (policy_year_private_parity_law <= 2016)
tab treat
tab usps treat
gen post  = (policy_year_private_parity_law <= year) // missing(policy_year_private_parity_law) will be 0
tab post
tab usps post
gen treat_post = treat * post
tab treat_post
tab usps treat_post

foreach out in dr_diabetes_mellitus dr_ischemic_heart_disease dr_suicide {
	gen ln_`out' = ln(`out')
}

la var ln_dr_diabetes_mellitus "DM"
la var ln_dr_ischemic_heart_disease "IHD" 
la var ln_dr_suicide "Suicide"

gen u1 = (ur13_code == 1) // Large central metro
gen u2 = (ur13_code == 2) // Large fringe metro
gen u3 = inlist(ur13_code, 3, 4) // Medium metro and Small metro
gen u4 = inlist(ur13_code, 5, 6) // Nonmetropolitan: Micropolitan and Noncore

save "dta/11_reg_data_ur13.dta", replace
