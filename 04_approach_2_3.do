********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Main Results: Approach 2 and 3 - Addressing the SPA confounder
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
**	Description: This file runs the RDD for approach 2 and 3,  when we use all 
**					observations from individuals from April 2013 onwards, but 
**					restrict to those who have reached SPA (approach 2) and 
**					control flexibly for months to/from SPA (approach 3). 
**
** 	Table of contents:
**		(1) RDD results 
**			(a) Approach 2 (Table 4)
**			(b) Approach 3 (Table 5)
**		(2) Discontinuity Plots (Figure 5a-d and G.2a-d)
**
********************************************************************************


********************************************************************************
** (1) RDD results 
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
	
** (a) Approach 2 (Table 4)
	** Pooled
		local h = 64
		local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD
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
				
							* Mean outcome within RDD bandwidth
			quietly summarize `y' if pre_apr2013==0 & abs(running_variable) <= `h' & eligible_to_retire==1, meanonly
			local mean_y = r(mean)

			* Display results
			display as result "Estimate (τ̂): " `est'
			display as result "Clustered SE: " `se'
			display as result "Mean outcome: " `mean_y'

			* 95% CI
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			display as result "95% CI: [" %9.3f `ci_low' ", " %9.3f `ci_high' "]"
				
				display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
			}
			
	** Power analysis 
	*rdrobust CESD running_variable if pre_apr2013==0 & eligible_to_retire==1, h(64) kernel(triangular) ///
	*	p(1) vce(cluster idauniq)
	*	matrix b = e(b)
	*	local tau = b[1,1] 
	*	rdpower CESD running_variable if post_apr2018, h(60) tau(`tau') all

	** By sex
		local h = 64
		local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD
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
				
							* Mean outcome within RDD bandwidth
			quietly summarize `y' if pre_apr2013==0 & abs(running_variable) <= `h' & eligible_to_retire==1 & sex==`s', meanonly
			local mean_y = r(mean)

			* Display results
			display as result "Estimate (τ̂): " `est'
			display as result "Clustered SE: " `se'
			display as result "Mean outcome: " `mean_y'

			* 95% CI
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			display as result "95% CI: [" %9.3f `ci_low' ", " %9.3f `ci_high' "]"
				display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
			}
		}

** (b) Approach 3 (Table 5)
	** Pooled
	local h = 64
	local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD

	foreach y of local outcomes {
		di "------------------------------------------------------------"
		di "RDD for `y' among pre-2013 DLA claimants (post-2013, controlling for retirement eligibility)"

		rdrobust `y' running_variable if pre_apr2013==0, ///
		   covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
		h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
			
			matrix b = e(b)
			matrix V = e(V)
			local est = b[1,1]
			local se  = sqrt(V[1,1])
			local ci_low  = `est' - 1.96*`se'
			local ci_high = `est' + 1.96*`se'
			
		* Mean outcome within RDD bandwidth
		quietly summarize `y' if pre_apr2013==0 & abs(running_variable) <= `h', meanonly
		local mean_y = r(mean)

		* Display results
		display as result "Estimate (τ̂): " `est'
		display as result "Clustered SE: " `se'
		display as result "Mean outcome: " `mean_y'

		* 95% CI
		local ci_low  = `est' - 1.96*`se'
		local ci_high = `est' + 1.96*`se'
		display as result "95% CI: [" %9.3f `ci_low' ", " %9.3f `ci_high' "]"
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}
		
	** Power analysis
	*rdrobust high_CESD running_variable if pre_apr2013==0, ///
	*covs(time_to_retirement time_to_retirement1 time_to_retirement2) h(64) 
	*kernel(triangular) p(1) vce(cluster idauniq) ///
	*matrix b = e(b)
	*local tau = b[1,1] 
	*rdpower high_CESD  running_variable if pre_apr2013==0, ///
	*covs(time_to_retirement time_to_retirement1 time_to_retirement2) h(64) tau(`tau') all
	
	** By sex
	local h = 64
	local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD
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
			
		* Mean outcome within RDD bandwidth
		quietly summarize `y' if pre_apr2013==0 & abs(running_variable) <= `h' & sex==`s', meanonly
		local mean_y = r(mean)

		* Display results
		display as result "Estimate (τ̂): " `est'
		display as result "Clustered SE: " `se'
		display as result "Mean outcome: " `mean_y'

		* 95% CI
		local ci_low  = `est' - 1.96*`se'
		local ci_high = `est' + 1.96*`se'
		display as result "95% CI: [" %9.3f `ci_low' ", " %9.3f `ci_high' "]"
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}
	}	
		
********************************************************************************
** (2) RDD plots (Figure 5a-d and G.2a-d)
********************************************************************************
local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD
foreach y of local outcomes {
    rdplot `y' running_variable if pre_apr2013==0 & eligible_to_retire==1 & running_variable <= 64 & running_variable >= -64, ///
        p(1) h(64) ///
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