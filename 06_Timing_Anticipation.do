********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Robustness: Timing of the Effects and evidence of anticipation
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
**	Description: This file explores the timing of the estimated effects. It runs
**					an event study and controls for pre-PIP outcomes 
**					observations. 
**
** 	Table of contents:
**		(1) Event Study
**		(2) Controlling for pre-PIP outcomes
**		(3) RDD with all individuals (regardless of DLA status)
**
********************************************************************************

********************************************************************************
** (1) Event Study (use approach 3)
********************************************************************************
** Data
	use "${data_dir}cleaned_data.dta", clear 
	** Create date of SPA
	gen dob_months = yob*12 + mob
	gen interview_months = year*12 + month
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
		* Keep all post-April 2013 observations
		gen pre_apr2013 = (year < 2013 | (year == 2013 & month < 4))
		*keep if pre_apr2013 == 0
		* Identify baseline pre-2013 DLA claimants
		preserve
			keep if pre_apr2013
			bysort idauniq (year month wave_number): keep if _n == _N
			replace dla = . if dla < 0
			drop if missing(dla)
			keep if dla > 0
			keep idauniq
			duplicates drop idauniq, force
			tempfile dla_claimants
			save "`dla_claimants'"
		restore
	merge m:1 idauniq using "`dla_claimants'", keep(match) nogen
	display "Unique pre-2013 DLA claimants retained: " _N
	
	gen time_to_retirement1=1 if time_to_retirement>0
	replace time_to_retirement1=0 if missing(time_to_retirement1)
	gen time_to_retirement2=time_to_retirement*time_to_retirement1 
	gen female=0 if sex==1
	replace female=1 if sex==2
	gen econactive=1 if work_status==2 | work_status==3 | work_status==4
	replace econactive=0 if missing(econactive)
	gen sick=1 if work_status==5
	replace sick=0 if missing(sick)
	gen other=1 if work_status==6 | work_status==95 | work_status==96
	replace other=0 if missing(other)
	
	* Drop individuals who were born in April 1948
	drop if yob==1948 & mob==4

		** Estimate RDD separately for each wave
		local h = 64
		local y "CESD"
		tempname memhold
		tempfile results
		postfile `memhold' wave tau se ci_low ci_high n_h_l n_h_r using "`results'", replace

		di "=========================================================="
		di "RD event study by wave for `y', waves"
		di "=========================================================="
		levelsof wave_number if wave_number >= 1 & !missing(`y'), local(waves)
		foreach w of local waves {
			quietly count if wave_number==`w' & !missing(`y', running_variable)
			di "--- `y', wave `w': `r(N)' obs available ---"
			capture rdrobust `y' running_variable if wave_number==`w', ///
				covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
				h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
			if _rc == 0 {
				matrix b = e(b)
				matrix V = e(V)
				local est     = b[1,1]
				local se      = sqrt(V[1,1])
				local ci_low  = `est' - 1.96*`se'
				local ci_high = `est' + 1.96*`se'
				local nl = e(N_h_l)
				local nr = e(N_h_r)
				display as result "`y' (wave `w'): tau=" %6.3f `est' ", SE=" %6.3f `se' ///
					", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "], N_h=" `nl'+`nr'
				post `memhold' (`w') (`est') (`se') (`ci_low') (`ci_high') (`nl') (`nr')
			}
			else {
				di as error "rdrobust failed: `y', wave `w' (rc=`=_rc'), likely too few obs in bandwidth"
			}
		}

		postclose `memhold'
		** Plot the RDD per wave
	local policy_wave 6
	
	preserve
	use "`results'", clear
	quietly count
	if r(N) == 0 {
		di as error "`y': no successful rdrobust estimates across any wave - nothing to plot"
	}
	else {
		gen thin_flag = (n_h_l + n_h_r) < 30

		graph close _all

		capture noisily twoway (rcap ci_low ci_high wave, lcolor(gs8)) ///
			   (scatter tau wave if thin_flag==0, mcolor(navy) msymbol(O) connect(l) lcolor(navy)) ///
			   (scatter tau wave if thin_flag==1, mcolor(red) msymbol(O)), ///
			yline(0, lcolor(red) lpattern(dash)) ///
			xline(`policy_wave', lcolor(black) lpattern(dash)) ///
			xtitle("Wave") ytitle("RD estimate (tau)") ///
			title("`y': RD effect by wave") ///
			note("Black dashed line = policy introduction (April 2013)." ) ///
			legend(off)

		if _rc != 0 {
			di as error "`y': twoway command failed (rc=`=_rc')"
		}
		else {
			capture noisily graph export "`outdir'\rd_event_study_`y'_wave4plus.png", replace width(1600)

			if _rc != 0 {
				di as error "`y': graph export failed (rc=`=_rc'). Common causes: the PNG is currently open in another program, the folder is read-only, or the path is invalid."
			}
			else {
				di as result "`y': graph exported to `outdir'\rd_event_study_`y'_wave4plus.png"
			}
		}
	}

	restore



********************************************************************************
** (2) Controls for pre-PIP outcomes
********************************************************************************
** Data
	use "${data_dir}cleaned_data.dta", clear 
	** Create date of SPA
	gen dob_months = yob*12 + mob
	gen interview_months = year*12 + month
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
	* Indicators for each year 
	gen pre_apr2013 = (year < 2013 | (year == 2013 & month < 4))
	gen pre_apr2011 = (year < 2011 | (year == 2011 & month < 4))
	gen pre_apr2010 = (year < 2010 | (year == 2010 & month < 4))
	gen pre_dec2010 = (year < 2010 | (year == 2010 & month < 12))
	gen pre_apr2008 = (year < 2008 | (year == 2008 & month < 4))
	gen pre_apr2003 = (year < 2003 | (year == 2003 & month < 4))
	gen pre_july2008 = (year < 2008 | (year == 2008 & month < 7))
	gen pre_wave4 = wave_number<4
	*keep if pre_apr2013 == 0
	* Identify baseline pre-2013 DLA claimants
		preserve
			keep if pre_apr2013
			bysort idauniq (year month wave_number): keep if _n == _N
			replace dla = . if dla < 0
			drop if missing(dla)
			keep if dla > 0
			keep idauniq
			duplicates drop idauniq, force
			tempfile dla_claimants
			save "`dla_claimants'"
		restore
	merge m:1 idauniq using "`dla_claimants'", keep(match) nogen
	display "Unique pre-2013 DLA claimants retained: " _N
	* Drop individuals who were born in April 1948
	drop if yob==1948 & mob==4
	** RDDs - change which pre-PIP observation you control for 
	preserve
		keep if pre_apr2008 == 1
		* Keep last pre-treatment observation per person
		sort idauniq year month
		by idauniq: keep if _n == _N
		* Keep only variables you need
		keep idauniq dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health CESD high_CESD
		* Rename to indicate pre-treatment
		foreach var of varlist * {
			if "`var'" != "idauniq" {
				rename `var' pre_`var'
			}
		}
		tempfile pre_outcomes
		save "${data_dir}pre_outcomes_elsa", replace
	restore

	* Merge pre-treatment outcomes back onto the main panel
	merge m:1 idauniq using "${data_dir}pre_outcomes_elsa"

	tab _merge

	drop if _merge == 2
	drop _merge

	local h = 64

	local outcomes ///
		dla_positive pip_positive dla_or_pip_positive ///
		total_dla_pip_payments total_benefit_payments ///
		good_health CESD high_CESD

	foreach y of local outcomes {
		
		local pre_y pre_`y'

		di "------------------------------------------------------------"
		di "RDD for `y' with pre-treatment control"

		rdrobust `y' running_variable if pre_apr2013==0, ///
			covs(time_to_retirement time_to_retirement1 time_to_retirement2 `pre_y') ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all

		matrix b = e(b)
		matrix V = e(V)
		local est = b[1,1]
		local se  = sqrt(V[1,1])
		local ci_low  = `est' - 1.96*`se'
		local ci_high = `est' + 1.96*`se'

		display as result "`y': τ̂=" %6.3f `est' ///
			", SE=" %6.3f `se' ///
			", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
	}


********************************************************************************
** (3) RDD with all individuals (regardless of DLA status)
********************************************************************************
** Data
	use "${data_dir}cleaned_data.dta", clear 
	**Drop individuals born in April 1948
	drop if yob == 1948 & mob == 4
	** Apply restrictions
	keep if yob > 0
	keep if year > 2011
	** Keep all post-April 2018 observations (pooled)
	gen post_apr2018 = (year > 2018 | (year == 2018 & month >= 4))
	keep if post_apr2018
	** Count observations and individuals
	count
	display "Total post-Apr2018 observations (pooled): " r(N)
	egen tag2018 = tag(idauniq)
	count if tag2018
	display "Unique individuals with post-Apr2018 observations: " r(N)

** RDD (pooled sample)
local h = 60
local outcomes dla_positive pip_positive dla_or_pip_positive ///
    total_dla_pip_payments total_benefit_payments good_health CESD high_CESD

foreach y of local outcomes {

    display as text "------------------------------------------------------------"
    display as text "RDD for `y' among all individuals (pooled post-2018)"

    * Run rdrobust
    rdrobust `y' running_variable if post_apr2018, ///
        h(`h') kernel(triangular) p(1) vce(cluster idauniq) all

    * Access e-class results
    matrix b = e(b)
    matrix V = e(V)

    * Extract treatment effect and SE
    local est = b[1,1]
    local se  = sqrt(V[1,1])

    * Mean outcome within RDD bandwidth
    quietly summarize `y' if post_apr2018 & ///
        abs(running_variable) <= `h', meanonly
    local mean_y = r(mean)

    * Display results
    display as result "Estimate (τ̂): " `est'
    display as result "Clustered SE: " `se'
    display as result "Mean outcome: " `mean_y'

    * 95% CI
    local ci_low  = `est' - 1.96*`se'
    local ci_high = `est' + 1.96*`se'

    display as result "95% CI: [" %9.3f `ci_low' ///
        ", " %9.3f `ci_high' "]"
}

** RDD plot
local outcomes CESD
foreach y of local outcomes {
    rdplot `y' running_variable if post_apr2018 & running_variable <= 60 & running_variable >= -60, ///
        p(1) h(60) ///
        nbins(10 10) ///
        kernel(triangular) ///
        title("Effect of Disability Reform on `y'") ///
        xtitle("Months from Eligibility Cutoff") ///
        ytitle("Probability of claiming DLA") ///
        graphregion(color(white)) ///
        name(`y'_rdplot)   // 👈 no "replace" here!
    * Optionally save each graph to a file
    graph export "`y'_rdplot.png", replace width(1600) height(1200)
}