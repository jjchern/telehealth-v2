
* app-tab-03-es-w-nt.do

glo projroot "/Volumes/Drive/Github/jjchern/telehealth-v2"

clear
set more off
set rmsg on
cd "${projroot}"

u "dta/10_reg_data_county.dta", clear

local ctrls tot_active_md_p100k_appx cty_unem_rate median_hh_inc pov_rates /// 
    median_age_appx pct_black_appx pct_white_appx pct_female_appx pct_ba_degree_appx pct_hs_or_less_appx
	   
* Panel B

eststo clear
foreach out in dr_all_causes dr_cerebrovascular_diseases dr_diabetes_mellitus ///
			   dr_influenza_and_pneumonia dr_ischemic_heart_disease dr_suicide { 
				
		eststo: qui areg ln_`out' `ctrls' i.year##i.treat s3_es_year_l* s3_es_year_f* [aw = pop] if (s3 == 1 | c == 1), ab(county) vce(cl state)	
		qui su ln_`out' [aw = pop] if e(sample) == 1 & (missing(policy_year_private_parity_law) | (year - policy_year_private_parity_law + 1 <= 0))
		qui estadd scalar bl_mean = r(mean)		
		qui estadd scalar n_units = e(N_clust)
		qui su `out' [aw = pop] if e(sample) == 1 & (missing(policy_year_private_parity_law) | (year - policy_year_private_parity_law + 1 <= 0))
		qui estadd scalar bl_mean_lvl = r(mean)		
		qui testparm s3_es_year_l*
		qui estadd scalar pv = r(p)
		}
		
esttab, b(3) se(3) keep(s3_es_year_l* s3_es_year_f*) label star(** 0.05) varwidth(32) ///
	   s(pv bl_mean bl_mean_lvl n_units N, fmt(2 2 2 0 %9.0fc) l("{\i p}-values (Pre-Trend)" "Mean of DV (Period 0)" "Mean of DV (Period 0) in Level" "Number of States" "Number of Observations")) ti("1. WLS, s3")
esttab using "out/app-tab-03-es-w-nt.rtf", replace b(3) se(3) keep(s3_es_year_l* s3_es_year_f*) label star(** 0.05) varwidth(32) ///
	   s(pv bl_mean bl_mean_lvl n_units N, fmt(2 2 2 0 %9.0fc) l("{\i p}-values (Pre-Trend)" "Mean of DV (Period 0)" "Mean of DV (Period 0) in Level" "Number of States" "Number of Observations")) ti("1. WLS, s3")

	   
