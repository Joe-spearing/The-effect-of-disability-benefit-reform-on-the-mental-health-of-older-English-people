********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Robustness: Alternative specification and estimation choices
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
**	Description: This file runs robustness of the main findings to several 
**					alternative specifications and estimation choices. It also
**					runs the RDD with attrition as an outcome (defined as not 
**					being in the data after April 2018) 
**
** 	Table of contents:
**		(1)  Sensitivity analysis (Figure I1-I7)
**		(2)  Attrition (Table I2 and I8)
**
********************************************************************************

********************************************************************************
** (1) Sensitvity Analysis (Figures I1-I7)
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

	** Approach 1 (change the outcomes)
	eststo clear
	local outcomes "pip_positive"
	local bandwidths 48 60 72
	local kernels "triangular uniform"
	local polys 1 2
	*Approach 1
	foreach h of local bandwidths {
		foreach k of local kernels {
			foreach p of local polys {
				rdrobust `outcomes' running_variable, h(`h') kernel(`k') p(`p') vce(cluster idauniq)
				eststo rdd_h`h'_k`k'_p`p'
			}
		}
	}
	estimates dir
	esttab, se

	coefplot rdd_h48_ktriangular_p1 rdd_h48_kuniform_p1 ///
			 rdd_h60_ktriangular_p1 rdd_h60_kuniform_p1 ///
			 rdd_h72_ktriangular_p1 rdd_h72_kuniform_p1 ///
			 rdd_h48_ktriangular_p2 rdd_h48_kuniform_p2 ///
			 rdd_h60_ktriangular_p2 rdd_h60_kuniform_p2 ///
			 rdd_h72_ktriangular_p2 rdd_h72_kuniform_p2, ///
		vertical ///	
		xlabel(none) ///
		xline(0, lpattern(solid) lcolor(black)) /// reference line
		yline(0, lpattern(dash) lcolor(black)) /// horizontal zero line
		msymbol(O) mcolor(black) ciopts(recast(rcap) lcolor(black)) ///
		lcolor(black) lpattern(solid) ///
		xlabel(, valuelabel angle(45) labsize(small)) ///
		ytitle("Estimated ITT Effect") ///
		legend(off) ///
		scheme(s1mono)
	
	** Approach 2
	eststo clear
	local outcomes "pip_positive"
	local bandwidths 52 64 76
	local kernels "triangular uniform"
	local polys 1 2
	*Approach 2
	foreach h of local bandwidths {
		foreach k of local kernels {
			foreach p of local polys {
				rdrobust `outcomes' running_variable if pre_apr2013==0 & eligible_to_retire==1, h(`h') kernel(`k') p(`p') vce(cluster idauniq)
				eststo rdd_h`h'_k`k'_p`p'
			}
		}
	}
	estimates dir
	esttab, se

	coefplot rdd_h52_ktriangular_p1 rdd_h52_kuniform_p1 ///
			 rdd_h64_ktriangular_p1 rdd_h64_kuniform_p1 ///
			 rdd_h76_ktriangular_p1 rdd_h76_kuniform_p1 ///
			 rdd_h52_ktriangular_p2 rdd_h52_kuniform_p2 ///
			 rdd_h64_ktriangular_p2 rdd_h64_kuniform_p2 ///
			 rdd_h76_ktriangular_p2 rdd_h76_kuniform_p2, ///
		vertical ///	
		xlabel(none) ///
		xline(0, lpattern(solid) lcolor(black)) /// reference line
		yline(0, lpattern(dash) lcolor(black)) /// horizontal zero line
		msymbol(O) mcolor(black) ciopts(recast(rcap) lcolor(black)) ///
		lcolor(black) lpattern(solid) ///
		xlabel(, valuelabel angle(45) labsize(small)) ///
		ytitle("Estimated ITT Effect") ///
		legend(off) ///
		scheme(s1mono)
	
	** Approach 3
	eststo clear
	local outcomes "pip_positive"
	local bandwidths 52 64 76
	local kernels "triangular uniform"
	local polys 1 2
	foreach h of local bandwidths {
		foreach k of local kernels {
			foreach p of local polys {
				rdrobust `outcomes' running_variable if pre_apr2013==0, covs(time_to_retirement time_to_retirement1 time_to_retirement2) h(`h') kernel(`k') p(`p') vce(cluster idauniq)
				eststo rdd_h`h'_k`k'_p`p'
			}
		}
	}
	estimates dir
	esttab, se

	coefplot rdd_h52_ktriangular_p1 rdd_h52_kuniform_p1 ///
			 rdd_h64_ktriangular_p1 rdd_h64_kuniform_p1 ///
			 rdd_h76_ktriangular_p1 rdd_h76_kuniform_p1 ///
			 rdd_h52_ktriangular_p2 rdd_h52_kuniform_p2 ///
			 rdd_h64_ktriangular_p2 rdd_h64_kuniform_p2 ///
			 rdd_h76_ktriangular_p2 rdd_h76_kuniform_p2, ///
		vertical ///	
		xlabel(none) ///
		xline(0, lpattern(solid) lcolor(black)) /// reference line
		yline(0, lpattern(dash) lcolor(black)) /// horizontal zero line
		msymbol(O) mcolor(black) ciopts(recast(rcap) lcolor(black)) ///
		lcolor(black) lpattern(solid) ///
		xlabel(, valuelabel angle(45) labsize(small)) ///
		ytitle("Estimated ITT Effect") ///
		legend(off) ///
		scheme(s1mono)
		
********************************************************************************
** (2) Attrition (Table I2 and Figure I8)
********************************************************************************
**Data 
use "${data_dir}cleaned_data.dta", clear 
*replace yob to most common yob
bysort idauniq: egen yob1 = mode(yob)
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
*keep if year>2011
** Keep all post-April 2018 observations (pooled)
gen post_apr2018 = (year > 2018 | (year == 2018 & month >= 4))
bysort idauniq: egen post_apr2018_max = max(post_apr2018)
bysort idauniq: egen max_year = max(year) if year<2013 & post_apr2018_max==0

keep if post_apr2018_max==0 | year_pair==2018
replace year_pair=2018 if year==max_year

replace year_pair=2018 if year_pair==2012 & max_year==2012

keep if running_variable<=64 & running_variable>=-64
keep if year_pair==2018
rdrobust post_apr2018_max running_variable, h(60) kernel(triangular) p(1)

gen attrition=1 if post_apr2018_max==0
replace attrition=0 if post_apr2018_max==1

local h=60
local outcomes post_apr2018_max attrition
foreach y of local outcomes {
    di "========== Covariate balance test for `x' =========="
    rdrobust `y' running_variable, ///
        h(`h') kernel(triangular) p(1) vce(cluster idauniq) all
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

local outcomes attrition
foreach y of local outcomes {
    rdplot `y' running_variable if running_variable <= 60 & running_variable >= -60, ///
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
