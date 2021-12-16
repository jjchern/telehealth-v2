
* tab-07-subarea.do

* Change project root:
glo projroot "/Users/ioi/Documents/telehealth-v2"

clear
set more off
set rmsg on
cd "${projroot}"

u "dta/11_reg_data_ur13.dta", clear

local ctrls tot_active_md_p100k_appx cty_unem_rate median_hh_inc pov_rates /// 
    median_age_appx pct_black_appx pct_white_appx pct_female_appx pct_ba_degree_appx pct_hs_or_less_appx ///
	himcaid himcare		
	
eststo clear
eststo: reg dr_all_causes
esttab using "out/tab-07-subarea.rtf", replace

eststo clear
foreach u in 1 2 3 4 {
	foreach out in dr_diabetes_mellitus dr_ischemic_heart_disease dr_suicide {
	
		eststo: qui areg ln_`out' `ctrls' i.year treat_post [aw = tot_pop] if s3 == 1 & u`u' == 1, ab(area) vce(cl state)	
		* count if (e(sample) == 1) & (post == 0)
		qui su ln_`out' [aw = tot_pop] if (e(sample) == 1) & (post == 0)
		qui estadd scalar bl_mean = r(mean)	
		qui estadd scalar n_units = e(N_clust)
		qui su `out' [aw = tot_pop] if (e(sample) == 1) & (post == 0)
		qui estadd scalar bl_mean_lvl = r(mean)	
		
		eststo: qui areg ln_`out' `ctrls' i.year##i.treat treat_post [aw = tot_pop] if (s3 == 1 | c == 1) & u`u' == 1, ab(area) vce(cl state)
		* count if (e(sample) == 1) & (post == 0)
		qui su ln_`out' [aw = tot_pop] if (e(sample) == 1) & (post == 0)
		qui estadd scalar bl_mean = r(mean)	
		qui estadd scalar n_units = e(N_clust)
		qui su `out' [aw = tot_pop] if (e(sample) == 1) & (post == 0)
		qui estadd scalar bl_mean_lvl = r(mean)	
		
		eststo: qui areg ln_`out' `ctrls' i.year##i.treat c.year#i.state treat_post [aw = tot_pop] if (s3 == 1 | c == 1) & u`u' == 1, ab(area) vce(cl state)	
		qui su ln_`out' [aw = tot_pop] if (e(sample) == 1) & (post == 0)
		qui estadd scalar bl_mean = r(mean)	
		qui estadd scalar n_units = e(N_clust)
		qui su `out' [aw = tot_pop] if (e(sample) == 1) & (post == 0)
		qui estadd scalar bl_mean_lvl = r(mean)			
	}
	
	esttab, b(3) se(3) keep(treat_post) label star(** 0.05) varwidth(32) ///
			s(bl_mean bl_mean_lvl n_units N, ///
				fmt(2 2 0 %9.0fc) ///
				l("Mean of DV (Pre-Event)" "Mean of DV (Pre-Event) in Level" ///
				  "Number of States" "Number of Observations")) ///
			ti("1. WLS, s3, u`u'")
			
	esttab using "out/tab-07-subarea.rtf", append nonotes ///
			b(3) se(3) keep(treat_post) label star(** 0.05) varwidth(32) ///
			s(bl_mean bl_mean_lvl n_units N, ///
				fmt(2 2 0 %9.0fc) ///
				l("Mean of DV (Pre-Event)" "Mean of DV (Pre-Event) in Level" ///
				  "Number of States" "Number of Observations")) ///
			ti("1. WLS, s3, u`u'")
			
	eststo clear
}
