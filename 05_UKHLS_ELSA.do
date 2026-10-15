********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Robustness: Pooling ELSA and UKHLS data
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
**	Description: This file pools ELSA data with UKHLS data, harmonising the 
**					CESD and GHQ mental healh measures through three methods. It
**					then runs the RDD on the pooled data. The UKHLS is already 
**					pre-cleaned. 
**
** 	Table of contents:
**		(1) Pooling UKHLS and ELSA data and harmonising mental health varibales
**		(2) Descriptive Statistics (Table H.1)
**		(3) RDD results
**			(a) Approach 1 (Table H2)
**			(b) Approach 2 (Table H2)
**			(c) Approach 3 (Table 6)
**		(4) Graphs (H1 and H2)
**
********************************************************************************

********************************************************************************
** (1) Pooling UKHLS and ELSA data 
********************************************************************************
** ELSA data
	use "${data_dir}cleaned_data_work_status.dta", clear 
** Append UKHLS (Understanding Society) data
	append using "${data_dir}clean_ukhls1_workstatus_noeng.dta"
** Harmonise person ID: ELSA uses idauniq; UKHLS uses pid
replace idauniq=pid if missing(idauniq)
** Flag ELSA data
gen elsa = 1 if missing(pid)
replace elsa = 0 if missing(elsa)
** Keep only indidivudals from the GPS in UKHLS (first sampled in 2009) 
bysort pid (wave_number): gen first_wave = wave_number[1]
drop if first_wave < 19 & elsa == 0
** Gen sex variable
gen female = (sex == 2)

** Harmonise Mental health variables
	** (a) CESD predicted from dummies for each GHQ-12 component score
		** This uses data that has already been predicted.
		merge m:1 pid year month running_variable ///
			using "Z:\Data\UKHLS\stata\stata14_se\ukhls\UKHLS_predicted_CESD.dta"
		drop if _merge == 2
		drop _merge

		gen CESD_v1 = CESD
		replace CESD_v1 = predicted_cesd if missing(CESD_v1)

		gen high_CESD_v1 = (CESD_v1 > 3) if !missing(CESD_v1)

	** (b) CESD predicted from simple regression of CESD on GHQ-12 caseness
		** This uses data that has already been predicted.
		merge m:1 pid year month running_variable ///
			using "Z:\Data\UKHLS\stata\stata14_se\ukhls\UKHLS_predicted_CESD1.dta"
		drop if _merge == 2
		drop _merge

		gen CESD_v2 = CESD
		replace CESD_v2 = simple_predicted_cesd if missing(CESD_v2)

		gen high_CESD_v2 = (CESD_v2 > 3) if !missing(CESD_v2)

	** (c) Standardise Mental Health Scores (Z-score)
	egen cesd_z = std(CESD)
	egen ghq_z  = std(ghq12_caseness)

	gen mental_health_z = cesd_z 
	replace mental_health_z = ghq_z if missing(mental_health_z)

	summarize mental_health_z, detail
	gen high_mental_health_z = (mental_health_z >= r(p75)) if !missing(mental_health_z)

** Create pooled weight
gen pooled_weight = ukhls_bhps_weight
replace pooled_weight = 1 if missing(ukhls_bhps_weight)

** Save Pooled Dataset
save "${data_dir}elsa_ukhls.dta", replace

********************************************************************************
* (2) Descriptive Statistics
********************************************************************************
** Data
use "${data_dir}elsa_ukhls.dta", clear
** Identify individuals who were DLA claimants in their last observation before 
** April 2013.
** Replace yob to most common yob
bysort idauniq: egen yob1 = mode(yob)
gen pre_apr2013 = (year < 2013 | (year == 2013 & month < 4))

preserve
    keep if pre_apr2013
    * order by interview date within person
    bysort idauniq (year month wave_number): keep if _n == _N
    * recode negative DLA codes to missing
    replace dla = . if dla < 0
	* Drop individuals whose last pre-Apr 2013 DLA is missing
    drop if missing(dla)
    * keep if received DLA
    keep if dla > 0
    * keep only ID
    keep idauniq
    duplicates drop idauniq, force
    tempfile dla_claimants
    save "`dla_claimants'"
restore

*check baseline size (R equivalent: length(dla.claimants))
merge m:1 idauniq using "`dla_claimants'", keep(match) nogen
egen tagid = tag(idauniq)
*drop if born April 2018, as we can't determine which side of the cutoff you fall.
drop if yob==1948 & mob==4
count if tagid & dla>=0
display "Baseline DLA claimants (pre-Apr2013): " r(N)
*drop if born April 2018, as we can't determine which side of the cutoff you fall.
drop if mob==4 & yob==1948 
*apply restrictions as in R-file
keep if yob>0
keep if year<2011
*count observations
count
display "Total pre-2011 observations (pooled): " r(N)
egen tag2011 = tag(idauniq)
count if tag2011
display "Unique individuals with post-Apr2018 observations: " r(N)
* keep last observation before 2011 within individual
bysort idauniq (year month wave_number): keep if _n == _N
count
display "Total individuals (last pre-2011 obs only): " r(N)

** Descriptive statistics 
		** ELSA
		sum dla_positive total_dla_pip_payments good_health ///
			CESD_v1 high_CESD_v1 ///
			CESD_v2 high_CESD_v2 ///
			mental_health_z high_mental_health_z ///
			female my_age retired working sick other ///
			running_variable ///
			if running_variable>=-60 & running_variable<=60 & elsa==1	
		** UKHLS (non weighted)
		sum dla_positive total_dla_pip_payments good_health ///
			CESD_v1 high_CESD_v1 ///
			CESD_v2 high_CESD_v2 ///
			mental_health_z high_mental_health_z ///
			female my_age retired working sick other ///
			running_variable ///
			if running_variable>=-60 & running_variable<=60 & elsa==0
		** UKHLS (weighted)
		sum dla_positive total_dla_pip_payments good_health ///
			CESD_v1 high_CESD_v1 ///
			CESD_v2 high_CESD_v2 ///
			mental_health_z high_mental_health_z ///
			female my_age retired working sick other ///
			running_variable ///
			[aw=pooled_weight] ///
			if running_variable>=-60 & running_variable<=60 & elsa==0

********************************************************************************
* (3) RDD Results
********************************************************************************
** (a) Approach 1 (Table H2)
	** Data
	use "${data_dir}elsa_ukhls.dta", clear
	** Identify individuals who were DLA claimants in their last observation before 
	** April 2013.
	gen pre_apr2013 = (year < 2013 | (year == 2013 & month < 4))

	preserve
		keep if pre_apr2013
		* Retain each individual's last pre-reform obs.
		bysort idauniq (year month wave_number): keep if _n == _N
		* Clean DLA variable and keep claimants only
		replace dla = . if dla < 0
		drop if missing(dla)
		keep if dla > 0
		* keep unique individuals
		keep idauniq
		duplicates drop idauniq, force
		tempfile dla_claimants
		save "Z:\Data\UKHLS\stata\stata14_se\ukhls\dla_claimants_US_ELSA", replace
	restore

	** Restrict main dataset to baseline DLA claimants
	merge m:1 idauniq using "Z:\Data\UKHLS\stata\stata14_se\ukhls\dla_claimants_US_ELSA", keep(match) nogen
	** Count baseline claimants
	egen tagid = tag(idauniq)
	count if tagid & dla >= 0
	display "Baseline DLA claimants (pre-Apr 2013): " r(N)
	** Drop individuals born in April 1948
	drop if yob==1948 & mob==4
	** Drop invalid year-of-birth values and post-2010 observations
	keep if yob>0
	keep if year>2011
	** Restrict to post-April 2018 observations
	gen post_apr2018 = (year > 2018 | (year == 2018 & month >= 4))
	keep if post_apr2018
	** Count observations and individuals
	count
	display "Total post-Apr2018 observations (pooled): " r(N)
	egen tag2018 = tag(idauniq)
	count if tag2018
	display "Unique individuals with post-Apr2018 observations: " r(N)

	** RDD (pooled)
	local h = 60
	local outcomes ///
		dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health ///
		CESD_v1 high_CESD_v1 ///
		CESD_v2 high_CESD_v2 ///
		mental_health_z high_mental_health_z
	
	foreach y of local outcomes {
		display as text "------------------------------------------------------------"
		display as text "RDD for `y' among pre-2013 DLA claimants (pooled post-2018)"
		* Run rdrobust
		rdrobust `y' running_variable if post_apr2018, ///
			h(`h') kernel(triangular) p(1) weights(pooled_weight) vce(cluster id) all
		* Access e-class results
		matrix b = e(b)
		matrix V = e(V)
		* Extract treatment effect (first element) and SE
		local est = b[1,1]
		local se  = sqrt(V[1,1])
		* Display results
		display as result "Estimate (τ̂): " `est'
		display as result "Clustered SE: " `se'
		* Optional 95% CI
		local ci_low  = `est' - 1.96*`se'
		local ci_high = `est' + 1.96*`se'
		display as result "95% CI: [" %9.3f `ci_low' ", " %9.3f `ci_high' "]"
	}

	** RDDs (by sex sample)
	local h = 60
	local outcomes ///
		dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health  ///
		CESD_v1 high_CESD_v1 ///
		CESD_v2 high_CESD_v2 ///
		mental_health_z high_mental_health_z
		
	foreach s in 1 2 {
		display as text "============================================================"
		display as text "SEX == `s'"
		
		foreach y of local outcomes {
			rdrobust `y' running_variable if post_apr2018 & sex==`s', ///
				h(`h') kernel(triangular) p(1) vce(cluster idauniq) all

			matrix b = e(b)
			matrix V = e(V)
			local est = b[1,1]
			local se  = sqrt(V[1,1])
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}
	}
	
	** (b) Approach 2
	** Data
	use "${data_dir}elsa_ukhls.dta", clear
	** Create date of SPA
	gen dob_months = yob*12 + mob
	gen interview_months = year*12 + month
	* 3. Define state pension (retirement) eligibility rules
	gen earliest_retirement_month = .
		*---WOMEN born April 1950 -> SPA 60
		replace earliest_retirement_month=12*(yob+60) + mob if sex==2 & (yob < 1950 | (yob==1950 & mob < 4))
		*---WOMEN born April 1950 - April 1953 -> gradual rise 60 to 65
		replace earliest_retirement_month = 12*(yob + 60) + mob + ///
			12*(yob-1950)+(mob-3) if sex==2 & dob_months >= (1950*12 + 4) & dob_months <= (1953*12 + 4)
		*--- WOMEN born Apr 1953 – Dec 1953 → more rapid raise
		replace earliest_retirement_month = 12*(yob + 60) + mob + ///
			12*(yob-1950)+3*(mob-3) if sex==2 & dob_months >= (1953*12 + 4) & dob_months <= (1953*12 + 12)
		*--- WOMEN born Jan 1954 - Sep 1954 -> gradual rise
		replace earliest_retirement_month = 12*(yob + 60) + mob + ///
			12*(yob-1950)+18+(mob-3) if sex==2 & dob_months >= (1954*12 + 1) & dob_months <= (1954*12 + 9)
		*--- ALL SEXES born Oct 1954 - March 1960 ->
		replace earliest_retirement_month = 12*(yob + 66) + mob if dob_months >= (1954*12 + 10) & dob_months <= (1960*12 + 3)
		*--- ALL SEXES born April 1960 - February 1961 ->
		replace earliest_retirement_month = 12*(yob-1960) + (2*mob-3) if dob_months >= (1960*12 + 4) & dob_months <= (1960*12 + 3) 
		*---MEN born before December 1953 -> SPA at 65
		replace earliest_retirement_month=12*(yob+65) + mob if sex==1 & (yob < 1953 | (yob==1953 & mob < 12))
		*---Born after April 1960 -> SPA 66
		replace earliest_retirement_month=12*(yob+66)+mob if dob_months>(1960*12+4)
		* --- Everyone born after April 1961 → retire at 67 
		replace earliest_retirement_month = 12*(yob + 67) + mob -12 ///
			if dob_months > (1961*12 + 4)
	gen latest_retirement_month = earliest_retirement_month
	gen mean_retirement_month = (earliest_retirement_month + latest_retirement_month)/2
	gen time_to_retirement = mean_retirement_month - interview_months
	gen eligible_to_retire = (interview_months >= mean_retirement_month)
	
	** Keep all post-April 2013 observations
		gen pre_apr2013 = (year < 2013 | (year == 2013 & month < 4))
	*keep if pre_apr2013 == 0
	** Identify baseline pre-2013 DLA claimants
	preserve
		keep if pre_apr2013
		bysort idauniq (year month wave_number): keep if _n == _N
		replace dla = . if dla < 0
		drop if missing(dla)
		keep if dla > 0
		keep idauniq
		duplicates drop idauniq, force
		tempfile dla_claimants
		save "${data_dir}dla_claimants_US_ELSA", replace
	restore
	merge m:1 idauniq using "${data_dir}dla_claimants_US_ELSA", keep(match) nogen
	display "Unique pre-2013 DLA claimants retained: " _N
	** Drop individuals who were born in April 1948
	drop if yob==1948 & mob==4
	gen time_to_retirement1=1 if time_to_retirement>0
	replace time_to_retirement1=0 if missing(time_to_retirement1)
	gen time_to_retirement2=time_to_retirement*time_to_retirement1 
	
	** RDD, Approach 2 (pooled)
	local h = 64
	local outcomes ///
		dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health ///
		CESD_v1 high_CESD_v1 ///
		CESD_v2 high_CESD_v2 ///
		mental_health_z high_mental_health_z
	
	foreach y of local outcomes {
		di "------------------------------------------------------------"
		di "RDD for `y' among pre-2013 DLA claimants (post-2013, dropping those not reached SPA)"

		rdrobust `y' running_variable if pre_apr2013==0 & eligible_to_retire==1, ///
			 ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
			
			matrix b = e(b)
			matrix V = e(V)
			local est = b[1,1]
			local se  = sqrt(V[1,1])
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}
	
	** RDD, Approach 2 (by sex)
	local h = 64
	local outcomes ///
		dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health ///
		CESD_v1 high_CESD_v1 ///
		CESD_v2 high_CESD_v2 ///
		mental_health_z high_mental_health_z

	foreach s in 1 2 {
		display as text "============================================================"
		display as text "SEX == `s'"
		
		foreach y of local outcomes {
			rdrobust `y' running_variable if pre_apr2013==0 & eligible_to_retire==1 & sex==`s', ///
				h(`h') kernel(triangular) p(1) vce(cluster idauniq) all

			matrix b = e(b)
			matrix V = e(V)
			local est = b[1,1]
			local se  = sqrt(V[1,1])
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}
	}
	
	** RDD, Approach 3 (pooled)
	local h = 64
	local outcomes ///
		dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health ///
		CESD_v1 high_CESD_v1 ///
		CESD_v2 high_CESD_v2 ///
		mental_health_z high_mental_health_z

	foreach y of local outcomes {
		di "------------------------------------------------------------"
		di "RDD for `y' among pre-2013 DLA claimants (post-2013, controlling for retirement eligibility)"

		rdrobust `y' running_variable if pre_apr2013==0 & sex==2, ///
		   covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
		h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
			
			matrix b = e(b)
			matrix V = e(V)
			local est = b[1,1]
			local se  = sqrt(V[1,1])
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}

	* RDD, Approach 3 (by sex)
	local h = 64
	local outcomes ///
		dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health ///
		CESD_v1 high_CESD_v1 ///
		CESD_v2 high_CESD_v2 ///
		mental_health_z high_mental_health_z
	
	foreach s in 1 2 {
		display as text "============================================================"
		display as text "SEX == `s'"
		
		foreach y of local outcomes {
			rdrobust `y' running_variable if pre_apr2013==0 & sex==`s', ///
			covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
				h(`h') kernel(triangular) p(1) vce(cluster idauniq) all

			matrix b = e(b)
			matrix V = e(V)
			local est = b[1,1]
			local se  = sqrt(V[1,1])
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}
	}

********************************************************************************
** (4) Graphs (H1 and H2)
********************************************************************************
** Data
use "${data_dir}elsa_ukhls.dta", clear
** Create date of SPA
gen dob_months = yob*12 + mob
gen interview_months = year*12 + month
	* 3. Define state pension (retirement) eligibility rules
	gen earliest_retirement_month = .
		*---WOMEN born April 1950 -> SPA 60
		replace earliest_retirement_month=12*(yob+60) + mob if sex==2 & (yob < 1950 | (yob==1950 & mob < 4))
		*---WOMEN born April 1950 - April 1953 -> gradual rise 60 to 65
		replace earliest_retirement_month = 12*(yob + 60) + mob + ///
			12*(yob-1950)+(mob-3) if sex==2 & dob_months >= (1950*12 + 4) & dob_months <= (1953*12 + 4)
		*--- WOMEN born Apr 1953 – Dec 1953 → more rapid raise
		replace earliest_retirement_month = 12*(yob + 60) + mob + ///
			12*(yob-1950)+3*(mob-3) if sex==2 & dob_months >= (1953*12 + 4) & dob_months <= (1953*12 + 12)
		*--- WOMEN born Jan 1954 - Sep 1954 -> gradual rise
		replace earliest_retirement_month = 12*(yob + 60) + mob + ///
			12*(yob-1950)+18+(mob-3) if sex==2 & dob_months >= (1954*12 + 1) & dob_months <= (1954*12 + 9)
		*--- ALL SEXES born Oct 1954 - March 1960 ->
		replace earliest_retirement_month = 12*(yob + 66) + mob if dob_months >= (1954*12 + 10) & dob_months <= (1960*12 + 3)
		*--- ALL SEXES born April 1960 - February 1961 ->
		replace earliest_retirement_month = 12*(yob-1960) + (2*mob-3) if dob_months >= (1960*12 + 4) & dob_months <= (1960*12 + 3) 
		*---MEN born before December 1953 -> SPA at 65
		replace earliest_retirement_month=12*(yob+65) + mob if sex==1 & (yob < 1953 | (yob==1953 & mob < 12))
		*---Born after April 1960 -> SPA 66
		replace earliest_retirement_month=12*(yob+66)+mob if dob_months>(1960*12+4)
		* --- Everyone born after April 1961 → retire at 67 
		replace earliest_retirement_month = 12*(yob + 67) + mob -12 ///
			if dob_months > (1961*12 + 4)
	gen latest_retirement_month = earliest_retirement_month
	gen mean_retirement_month = (earliest_retirement_month + latest_retirement_month)/2
	gen time_to_retirement = mean_retirement_month - interview_months
	gen eligible_to_retire = (interview_months >= mean_retirement_month)
	
	gen time_to_retirement1=1 if time_to_retirement>0
	replace time_to_retirement1=0 if missing(time_to_retirement1)
	gen time_to_retirement2=time_to_retirement*time_to_retirement1 
	
** Keep all post-April 2013 observations
gen pre_apr2013 = (year < 2013 | (year == 2013 & month < 4))

preserve
    keep if pre_apr2013
    bysort idauniq (year month wave_number): keep if _n == _N
    replace dla = . if dla < 0
    drop if missing(dla)
    keep if dla > 0
    keep idauniq
    duplicates drop idauniq, force
    save "Z:\Data\UKHLS\stata\stata14_se\ukhls\dla_claimants_US_ELSA", replace
restore

merge m:1 idauniq using "Z:\Data\UKHLS\stata\stata14_se\ukhls\dla_claimants_US_ELSA", ///
    keep(match) nogen

drop if yob==1948 & mob==4

gen post_apr2018 = (year > 2018 | (year == 2018 & month >= 4))

** RDD and Outcomes
local h = 64
local outcomes ///
    dla_positive pip_positive dla_or_pip_positive ///
    total_dla_pip_payments total_benefit_payments ///
    good_health ///
    CESD_v1 high_CESD_v1 ///
    CESD_v2 high_CESD_v2 ///
    mental_health_z high_mental_health_z
local nout : word count `outcomes'

forval ap = 1/3 {
    foreach sub in all women men {
        matrix R`ap'_`sub' = J(`nout', 3, .)
        matrix rownames R`ap'_`sub' = `outcomes'
        matrix colnames R`ap'_`sub' = b ci_lo ci_hi
    }
}

capture program drop store_conv
program define store_conv
    * args: matname row
    args matname row
    local b   = e(tau_cl)
    local se  = e(se_tau_cl)
    matrix `matname'[`row', 1] = `b'
    matrix `matname'[`row', 2] = `b' - 1.96 * `se'
    matrix `matname'[`row', 3] = `b' + 1.96 * `se'
end

	**Approach 1
	di as text "============================================================"
	di as text "APPROACH 1: Post-April 2018, full DLA claimant sample"
	di as text "============================================================"

	local row = 0
	foreach y of local outcomes {
		local ++row

		rdrobust `y' running_variable if post_apr2018, ///
			h(60) kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R1_all `row'

		rdrobust `y' running_variable if post_apr2018 & sex==2, ///
			h(60) kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R1_women `row'

		rdrobust `y' running_variable if post_apr2018 & sex==1, ///
			h(60) kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R1_men `row'

		foreach sub in all women men {
			di as result "Approach 1 | `y' (`sub'): " ///
				"τ̂=" %6.3f R1_`sub'[`row',1] ///
				"  95% CI [" %6.3f R1_`sub'[`row',2] ", " %6.3f R1_`sub'[`row',3] "]"
		}
	}

	** Approach 2
	di as text "============================================================"
	di as text "APPROACH 2: Post-2013, SPA-eligible subsample"
	di as text "============================================================"

	local row = 0
	foreach y of local outcomes {
		local ++row

		rdrobust `y' running_variable if pre_apr2013==0 & eligible_to_retire==1, ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R2_all `row'

		rdrobust `y' running_variable if pre_apr2013==0 & eligible_to_retire==1 & sex==2, ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R2_women `row'

		rdrobust `y' running_variable if pre_apr2013==0 & eligible_to_retire==1 & sex==1, ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R2_men `row'

		foreach sub in all women men {
			di as result "Approach 2 | `y' (`sub'): " ///
				"τ̂=" %6.3f R2_`sub'[`row',1] ///
				"  95% CI [" %6.3f R2_`sub'[`row',2] ", " %6.3f R2_`sub'[`row',3] "]"
		}
	}

	** Approach 3
	di as text "============================================================"
	di as text "APPROACH 3: Post-2013, full sample, retirement spline controls"
	di as text "============================================================"

	local row = 0
	foreach y of local outcomes {
		local ++row

		rdrobust `y' running_variable if pre_apr2013==0, ///
			covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R3_all `row'

		rdrobust `y' running_variable if pre_apr2013==0 & sex==2, ///
			covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R3_women `row'

		rdrobust `y' running_variable if pre_apr2013==0 & sex==1, ///
			covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
		store_conv R3_men `row'

		foreach sub in all women men {
			di as result "Approach 3 | `y' (`sub'): " ///
				"τ̂=" %6.3f R3_`sub'[`row',1] ///
				"  95% CI [" %6.3f R3_`sub'[`row',2] ", " %6.3f R3_`sub'[`row',3] "]"
		}
	}

	** Plot
	preserve
	clear
	set obs `= `nout' * 3 * 3'

	gen str30 outcome  = ""
	gen int   approach = .
	gen str8  subgroup = ""
	gen float b        = .
	gen float ci_lo    = .
	gen float ci_hi    = .

	local obs = 0
	forval ap = 1/3 {
		local row = 0
		foreach y of local outcomes {
			local ++row
			foreach sub in all women men {
				local ++obs
				quietly replace outcome  = "`y'"   in `obs'
				quietly replace approach = `ap'    in `obs'
				quietly replace subgroup = "`sub'" in `obs'
				quietly replace b        = R`ap'_`sub'[`row', 1] in `obs'
				quietly replace ci_lo    = R`ap'_`sub'[`row', 2] in `obs'
				quietly replace ci_hi    = R`ap'_`sub'[`row', 3] in `obs'
			}
		}
	}

	gen xpos = (subgroup=="all")*1 + (subgroup=="women")*2 + (subgroup=="men")*3

	label define applbl 1 "Approach 1" 2 "Approach 2" 3 "Approach 3"
	label values approach applbl
	*foreach y of local outcomes {

	 *   twoway ///
			(rcap ci_hi ci_lo xpos if outcome=="`y'", ///
				lcolor("59 110 143") lwidth(medthin)) ///
			(scatter b xpos if outcome=="`y'", ///
				mcolor("210 43 97") msymbol(circle) msize(medsmall)) ///
			, by(approach, ///
				 note("") cols(3) title("`y'") ///
				 graphregion(color(white)) imargin(medsmall)) ///
			  yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
			  ylabel(, angle(0) glcolor(gs14) glwidth(vthin)) ///
			  ytitle("Estimated RDD estimate") ///
			  xlabel(1 "All" 2 "Women" 3 "Men", noticks) ///
			  xtitle("") legend(off) ///
			  graphregion(color(white)) plotregion(color(white)) ///
			  name(`y', replace)

	 *   graph display `y'
	*}

	foreach y of local outcomes {

		twoway ///
			(rcap ci_hi ci_lo xpos if outcome=="`y'", ///
				lcolor("59 110 143") lwidth(medthin)) ///
			(scatter b xpos if outcome=="`y'", ///
				mcolor("210 43 97") msymbol(circle) msize(medsmall)) ///
			, by(approach, ///
				 note("") cols(3) title("`y'") ///
				 graphregion(color(white)) imargin(medsmall)) ///
			  yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
			  yscale(range(-0.4 1)) ///
			  ylabel(-0.4 -0.2 0 0.2 0.4 0.60 0.8 1.0, angle(0) glcolor(gs14) glwidth(vthin)) ///
			  ytitle("Estimated RDD estimate") ///
			  xlabel(1 "All" 2 "Women" 3 "Men", noticks) ///
			  xtitle("") legend(off) ///
			  graphregion(color(white)) plotregion(color(white)) ///
			  name(`y', replace)

		graph display `y'
	}

	restore