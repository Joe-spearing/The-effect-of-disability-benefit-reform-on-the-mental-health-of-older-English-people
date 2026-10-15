********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Main Results: Approach 1 - post April 2018
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
**	Description: This file runs the RDD for approach 1,  when we restrict the
**					sample to observations from April 2018 onwards. By this 	
**					point in time, all individuals within a 60-month bandwidth 
**					around teh cutoff have reached State Pension Age, regardless
**					of sex.
**
** 	Table of contents:
**		(1) RDD results (Table 3)
**		(2) Discontinuity Plots (Figure 4a-d and G.1a-d)
**
********************************************************************************

********************************************************************************
** (1) RDD results
********************************************************************************
** Data
use "${data_dir}cleaned_data.dta", clear 

** Identify individuals who were DLA claimants in their last observation before 
** April 2013.
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

* check baseline size
merge m:1 idauniq using "`dla_claimants'", keep(match) nogen
egen tagid = tag(idauniq)
* drop if born April 1948, as can't determine which side of the cutoff an individual falls
drop if yob==1948 & mob==4
count if tagid & dla>=0
display "Baseline DLA claimants (pre-Apr2013): " r(N)
* apply restrictions as in R-file
keep if yob>0
keep if year>2011
* keep all post-April 2018 observations (pooled)
gen post_apr2018 = (year > 2018 | (year == 2018 & month >= 4))
keep if post_apr2018
* count observations
count
display "Total pre-2011 observations (pooled): " r(N)
egen tag2018 = tag(idauniq)
count if tag2018
display "Unique individuals with post-Apr2018 observations: " r(N)

** RDD results (pooled)
local h = 60
	local outcomes dla_positive pip_positive dla_or_pip_positive ///
	total_dla_pip_payments total_benefit_payments good_health CESD high_CESD
		foreach y of local outcomes {
		display as text "------------------------------------------------------------"
		display as text "RDD for `y' among pre-2013 DLA claimants (pooled post-2018)"
		* Run rdrobust
		rdrobust `y' running_variable if post_apr2018, ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
		* Access e-class results
		matrix b = e(b)
		matrix V = e(V)
		* Extract treatment effect (first element) and SE
		local est = b[1,1]
		local se  = sqrt(V[1,1])
		* Mean outcome within RDD bandwidth
		quietly summarize `y' if post_apr2018 & abs(running_variable) <= `h', meanonly
		local mean_y = r(mean)

		* Display results
		display as result "Estimate (τ̂): " `est'
		display as result "Clustered SE: " `se'
		display as result "Mean outcome: " `mean_y'

		* 95% CI
		local ci_low  = `est' - 1.96*`se'
		local ci_high = `est' + 1.96*`se'
		display as result "95% CI: [" %9.3f `ci_low' ", " %9.3f `ci_high' "]"
	}
	
** Power analysis 
*rdrobust CESD running_variable if post_apr2018, h(60) kernel(triangular) ///
*	p(1) vce(cluster idauniq)
*	matrix b = e(b)
*	local tau = b[1,1] 
*	rdpower CESD running_variable if post_apr2018, h(60) tau(`tau') all

** RDDs results (by sex sample)
local h = 60
	local outcomes dla_positive pip_positive dla_or_pip_positive ///
	total_dla_pip_payments total_benefit_payments good_health CESD high_CESD
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
				* Mean outcome within RDD bandwidth
		quietly summarize `y' if post_apr2018 & abs(running_variable) <= `h' & sex==`s', meanonly
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
** (2) RDD plots (Figrues 4a-d and G1a-d)
********************************************************************************
local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD
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
		* JS note: feels like "no replace here" needs explaining to me!
                * Optionally save each graph to a file
		graph export "`y'_rdplot.png", replace width(1600) height(1200)
	}		
