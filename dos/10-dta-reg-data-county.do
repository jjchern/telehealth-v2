
* 10-dta-reg-data-county.R

glo projroot "/Volumes/Drive/Github/jjchern/telehealth-v2"

clear
set more off
set rmsg on
cd "${projroot}"

* Omitted from the analysis: "CA" (97), "LA" (95), "OK" (97), "TX" (97), "HI" (99), "KY" (01)
u "dta/09_analysis_data_county.dta" if !inlist(usps, "CA", "LA", "OK", "TX", "HI", "KY"), clear

* Add under65
merge 1:1 usps county_code year using "dta/07_cnty_yr_death_rate_under_65.dta"
keep if _m == 3
drop _m

* 1. Add indicator for treated states with at least 3 post periods ------------

tab usps if !missing(policy_year_private_parity_law) & (policy_year_private_parity_law <= 2014)
di r(r)   // 14
gen s3    = !missing(policy_year_private_parity_law) & (policy_year_private_parity_law <= 2014)

* 2. Add indicator for never treated states -----------------------------------

tab usps if missing(policy_year_private_parity_law) & !inlist(usps, "AK", "AR", "CO", "CT", "DE", "MN", "NY")
di r(r)   // 21
gen c     = missing(policy_year_private_parity_law) & !inlist(usps, "AK", "AR", "CO", "CT", "DE", "MN", "NY") 

* 3. Prepare variables --------------------------------------------------------

egen county = group(county_code)
egen state  = group(usps)

gen treat = (policy_year_private_parity_law <= 2016)
gen post  = (policy_year_private_parity_law <= year)
gen treat_post = treat * post

ren dr_cerebrovascular_diseases_u65 dr_cvd_u65
ren dr_influenza_and_pneumonia_u65 dr_i_and_p_u65

foreach out in dr_all_causes dr_cerebrovascular_diseases dr_diabetes_mellitus ///
				dr_influenza_and_pneumonia dr_ischemic_heart_disease dr_suicide ///
				dr_accidents dr_transport_acc dr_non_trans_acc dr_assualt ///
				dr_all_causes_u65 dr_cvd_u65 dr_diabetes_mellitus_u65 ///
				dr_i_and_p_u65 dr_ischemic_heart_disease_u65 dr_suicide_u65 ///
				dr_combined_u65 {
	gen ln_`out' = ln(`out')
}

la var treat_post "Parity Law"
la var ln_dr_all_causes "All Causes" 
la var ln_dr_cerebrovascular_diseases "CVD"
la var ln_dr_diabetes_mellitus "DM"
la var ln_dr_influenza_and_pneumonia "I&P"
la var ln_dr_ischemic_heart_disease "IHD" 
la var ln_dr_suicide "Suicide"
la var ln_dr_accidents "Accidents (All)" 
la var ln_dr_transport_acc "Accidents (Transport)" 
la var ln_dr_non_trans_acc "Accidents (Non-transport)" 
la var ln_dr_assualt "Homicide"

la var ln_dr_all_causes_u65 "All Causes" 
la var ln_dr_cvd_u65 "CVD"
la var ln_dr_diabetes_mellitus_u65 "DM"
la var ln_dr_i_and_p_u65 "I&P"
la var ln_dr_ischemic_heart_disease_u65 "IHD" 
la var ln_dr_suicide_u65 "Suicide"
la var ln_dr_combined_u65 "Combined"

gen s3_es_year_l3p = (year - policy_year_private_parity_law + 1 <= -3)
foreach y in -2 -1 0 1 2 {
	if (`y' <= -1) { // lag, omit l0 -> the year prior to the policy year
		local lag = substr("`y'", 2, .)
		gen s3_es_year_l`lag' = (year - policy_year_private_parity_law + 1 == `y')
	} 
	else if (`y' > 0) { // lead
		gen s3_es_year_f`y'   = (year - policy_year_private_parity_law + 1 == `y')
	} 
	else {
		di "`y' is omited"
	}
}
gen s3_es_year_f3p = (year - policy_year_private_parity_law + 1 >= 3) if c != 1
replace s3_es_year_f3p = 0 if c == 1

la var s3_es_year_l3p "Parity Law Effect, {\i t} - 3{\super +}"
la var s3_es_year_l2  "Parity Law Effect, {\i t} - 2"
la var s3_es_year_l1  "Parity Law Effect, {\i t} - 1"
la var s3_es_year_f1  "Parity Law Effect, {\i t} + 1"
la var s3_es_year_f2  "Parity Law Effect, {\i t} + 2"
la var s3_es_year_f3p "Parity Law Effect, {\i t} + 3{\super +}"

* 4. Save file ----------------------------------------------------------------

save "dta/10_reg_data_county.dta", replace
