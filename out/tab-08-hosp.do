
* tab-08-hosp.do

* Change project root:
glo projroot "/Users/muriel/Documents/GitHub/telehealth-v2"

clear
set more off
set rmsg on
cd "${projroot}"

u "dta/12_reg_data_state.dta", clear

local ctrls tot_active_md_p100k_appx cty_unem_rate median_hh_inc pov_rates /// 
    median_age_appx pct_black_appx pct_white_appx pct_female_appx pct_ba_degree_appx pct_hs_or_less_appx

* Panel A

eststo clear
foreach out in hosp_ad_p1k hosp_em_vis_p1k hosp_ip_days_p1k hosp_op_vis_p1k { 
				
		eststo: qui areg ln_`out' `ctrls' i.year treat_post [aw = tot_pop] if (s3 == 1) & !inlist(usps, "MO", "MT") & (year <= 2015), ab(state) vce(cl state)	
		qui su ln_`out' [aw = pop] if e(sample) == 1 & (missing(policy_year_private_parity_law) | (year - policy_year_private_parity_law + 1 <= 0))
		qui estadd scalar bl_mean = r(mean)		
		qui estadd scalar n_units = e(N_clust)
		qui su `out' [aw = pop] if e(sample) == 1 & (missing(policy_year_private_parity_law) | (year - policy_year_private_parity_law + 1 <= 0))
		qui estadd scalar bl_mean_lvl = r(mean)		
		
		}
		
esttab, b(3) se(3) keep(treat_post) label star(** 0.05) varwidth(32) ///
	   s(bl_mean bl_mean_lvl n_units N, fmt(2 2 0 %9.0fc) l("Mean of DV (Pre-Event)" "Mean of DV (Pre-Event) in Level" "Number of States" "Number of Observations")) ti("1. WLS, s3")
esttab using "out/tab-08-hosp.rtf", replace b(3) se(3) keep(treat_post) label star(** 0.05) varwidth(32) ///
	   s(bl_mean bl_mean_lvl n_units N, fmt(2 2 0 %9.0fc) l("Mean of DV (Pre-Event)" "Mean of DV (Pre-Event) in Level" "Number of States" "Number of Observations")) ti("1. WLS, s3")

* Panel B

eststo clear
foreach out in hosp_ad_p1k hosp_em_vis_p1k hosp_ip_days_p1k hosp_op_vis_p1k { 

		eststo: qui areg ln_`out' `ctrls' i.year##i.treat treat_post [aw = tot_pop] if (s3 == 1 | c == 1) & !inlist(usps, "MO", "MT") & (year <= 2015), ab(state) vce(cl state)	
		qui su ln_`out' [aw = pop] if e(sample) == 1 & (missing(policy_year_private_parity_law) | (year - policy_year_private_parity_law + 1 <= 0))
		qui estadd scalar bl_mean = r(mean)		
		qui estadd scalar n_units = e(N_clust)
		qui su `out' [aw = tot_pop] if e(sample) == 1 & (missing(policy_year_private_parity_law) | (year - policy_year_private_parity_law + 1 <= 0))
		qui estadd scalar bl_mean_lvl = r(mean)		
		
		}
		
esttab, b(3) se(3) keep(treat_post) label star(** 0.05) varwidth(32) ///
	   s(bl_mean bl_mean_lvl n_units N, fmt(2 2 0 %9.0fc) l("Mean of DV (Pre-Event)" "Mean of DV (Pre-Event) in Level" "Number of States" "Number of Observations")) ti("1. WLS, s3")
esttab using "out/tab-08-hosp.rtf", append b(3) se(3) keep(treat_post) label star(** 0.05) varwidth(32) ///
	   s(bl_mean bl_mean_lvl n_units N, fmt(2 2 0 %9.0fc) l("Mean of DV (Pre-Event)" "Mean of DV (Pre-Event) in Level" "Number of States" "Number of Observations")) ti("1. WLS, s3")

	   
