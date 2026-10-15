********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Falsification tests  
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
**	Description: This file runs a number of tests exploring whether the key
**				assumptions of the RDD holds. In particular, it tests for any 
**				discontinuieis in pre-policy variables, runs five falsifcation 
**				tests to assess whether groups that should experience no 
**				treatment effect display any discontinuities in the outcome 
**				varibales, and also runs a McCrary Density test. 
**
** 	Table of contents:
**		(1) Discontinuities in pre-policy variables (Table F1 and Figure F1)
**		(2) Five falsification tests (Table 2)
**			(a) pre-state pension sample (pre-2008, men only)
**			(b) pre-state pension sample (pre-2003)
**			(c) pre-announcement sample (controlling for SPA)
**			(d) non-claimant sample (post-implementation, reached SPA age)
**			(e) non-claimant sample (post-implementation, controlling for SPA)
**		(3) McCrary Density Tests (Figure 3, F2)
**
********************************************************************************

********************************************************************************
** (1) Discontinuities in pre-policy variables
********************************************************************************
** (a) Post-April 2018
	** Data 
	use "${data_dir}cleaned_data.dta", clear 
	** Identify individuals who were DLA claimants in their last observation 
	** before April 2013.
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
	
	*  baseline size (R equivalent: length(dla.claimants))
	merge m:1 idauniq using "`dla_claimants'", keep(match) nogen
	egen tagid = tag(idauniq)
	* drop if born April 2018, as we can't determine which side of the cutoff you fall.
	drop if yob==1948 & mob==4
	count if tagid & dla>=0
	display "Baseline DLA claimants (pre-Apr2013): " r(N)
	* drop if born April 2018, as we can't determine which side of the cutoff you fall.
	drop if mob==4 & yob==1948 
	* apply restrictions as in R-file
	keep if yob>0
	keep if year>2011
        *Keep all post-April 2018 observations (pooled)
	gen post_apr2018 = (year > 2018 | (year == 2018 & month >= 4))
	keep if post_apr2018
	* Count observations and individuals
	count
	display "Total post-Apr2018 observations (pooled): " r(N)
	egen tag2018 = tag(idauniq)
	count if tag2018
	display "Unique individuals with post-Apr2018 observations: " r(N)

	gen female=0 if sex==1
	replace female=1 if sex==2
	gen econactive=1 if work_status==2 | work_status==3 | work_status==4
	replace econactive=0 if missing(econactive)
	gen sick=1 if work_status==5
	replace sick=0 if missing(sick)
	gen other=1 if work_status==6 | work_status==95 | work_status==96
	replace other=0 if missing(other)

	** Covariate balance
	local h=60
	local outcomes female econactive retired sick other
	foreach y of local outcomes {
		di "========== Covariate balance test for `x' =========="
		rdrobust `y' running_variable if post_apr2018, ///
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

	* Replace 'retired' for 'female' (Figure F1)	
	binscatter retired running_variable if post_apr2018==1 & running_variable<=60 & running_variable>=-60, ///
		line(lfit) rd(0) n(20) ///
		xlabel(-60(20)60) ///
		mcolor(black) msymbol(o) ///
		lcolor(black) ///
		xtitle("Months from Eligibility Cutoff") ///
		ytitle("% Female")
	graph export "binscatter_`y'.png", replace
	
** (b) Post-April 2013, reached SPA
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
	* Sanity check
	tab eligible_to_retire
	summ time_to_retirement if eligible_to_retire==1
	summ time_to_retirement if eligible_to_retire==0
	histogram time_to_retirement if time_to_retirement>-500, width(12) ///
		title("Distribution of Time to Retirement") ///
		xtitle("Months from retirement (negative = after retirement)") ///
		ytitle("Number of observations") ///
		normal
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
	* Drop individuals who were born in April 1948
	drop if yob==1948 & mob==4
	
	*gen time_to_retirement1=1 if time_to_retirement>0
	*replace time_to_retirement1=0 if missing(time_to_retirement1)
	*gen time_to_retirement2=time_to_retirement*time_to_retirement1 
	gen female=0 if sex==1
	replace female=1 if sex==2
	gen econactive=1 if work_status==2 | work_status==3 | work_status==4
	replace econactive=0 if missing(econactive)
	gen sick=1 if work_status==5
	replace sick=0 if missing(sick)
	gen other=1 if work_status==6 | work_status==95 | work_status==96
	replace other=0 if missing(other)

local h = 64
local outcomes female econactive retired sick other
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

** (c) Post-April 2013, controlling for SPA 
	local h = 64
	local outcomes female econactive retired sick other
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
			
			display as result "`y' (sex=`s'): τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
		}


********************************************************************************
** (2) Five falsification tests
********************************************************************************
** (a) Pre-state pension sample (pre-2008, men only)
	** Data
	use "${data_dir}cleaned_data.dta", clear 
	** Identify individuals who were DLA claimants in their last observation 
	** before April 2013.
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
	keep if year<2011
	* count observations
	count
	display "Total pre-2011 observations (pooled): " r(N)
	egen tag2011 = tag(idauniq)
	count if tag2011
	display "Unique individuals with post-Apr2018 observations: " r(N)
	
	gen female=0 if sex==1
	replace female=1 if sex==2
	gen econactive=1 if work_status==2 | work_status==3 | work_status==4
	replace econactive=0 if missing(econactive)
	gen sick=1 if work_status==5
	replace sick=0 if missing(sick)
	gen other=1 if work_status==6 | work_status==95 | work_status==96
	replace other=0 if missing(other)
	
	** Pre-2008 
	keep if year<2008
	keep if sex==1
	local h = 60
	local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD female econactive retired sick other
	foreach y of local outcomes {
		display as text "------------------------------------------------------------"
		display as text "RDD for `y' among pre-2013 DLA claimants (pooled post-2018)"
		* Run rdrobust
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

** (b) Pre-state pension sample (pre-2003)
	** Data
	use "${data_dir}cleaned_data.dta", clear 
	** Identify individuals who were DLA claimants in their last observation 
	** before April 2013.
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
	keep if year<2011
	* count observations
	count
	display "Total pre-2011 observations (pooled): " r(N)
	egen tag2011 = tag(idauniq)
	count if tag2011
	display "Unique individuals with post-Apr2018 observations: " r(N)
	
	gen female=0 if sex==1
	replace female=1 if sex==2
	gen econactive=1 if work_status==2 | work_status==3 | work_status==4
	replace econactive=0 if missing(econactive)
	gen sick=1 if work_status==5
	replace sick=0 if missing(sick)
	gen other=1 if work_status==6 | work_status==95 | work_status==96
	replace other=0 if missing(other)
	
	** Pre-2003 
	keep if year<2003
	local h = 60
	local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD female econactive retired sick other
	foreach y of local outcomes {
		display as text "------------------------------------------------------------"
		display as text "RDD for `y' among pre-2013 DLA claimants (pooled post-2018)"
		* Run rdrobust
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
	
** (c) pre-announcement sample (controlling for SPA)
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
	
	**pre-announcement
	local h = 64
	local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD female econactive retired sick other
	foreach y of local outcomes {
		di "------------------------------------------------------------"
		di "RDD for `y' among pre-2013 DLA claimants (post-2013, controlling for retirement eligibility)"

		rdrobust `y' running_variable if pre_apr2013==1, ///
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

** (d) non-claimant sample (post-implementation, reached SPA age)
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
	* Drop individuals who were born in April 1948
	drop if yob==1948 & mob==4
	* Restrict to post-April 2013 observations
	gen post_apr2013 = (year > 2013 | (year == 2013 & month >= 4))
	drop if post_apr2013 == 0
	* Create time-to-retirement
	drop if year==2013 & month==4
*	gen time_to_retirement1=1 if time_to_retirement>0
*	replace time_to_retirement1=0 if missing(time_to_retirement1)
*	gen time_to_retirement2=time_to_retirement*time_to_retirement1 
	gen female=0 if sex==1
	replace female=1 if sex==2
	gen econactive=1 if work_status==2 | work_status==3 | work_status==4
	replace econactive=0 if missing(econactive)
	gen sick=1 if work_status==5
	replace sick=0 if missing(sick)
	gen other=1 if work_status==6 | work_status==95 | work_status==96
	replace other=0 if missing(other)


	** Non-claimant sample
	local h = 64
	local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD female econactive retired sick other

	foreach y of local outcomes {
		di "------------------------------------------------------------"
		di "RDD for `y' among never-treated (post-2013, eligible for retirement)"

		rdrobust `y' running_variable if post_apr2013==1 & eligible_to_retire==1 & sex==1 & age>=65 | post_apr2013==1 & eligible_to_retire==1 & sex==2 & time_to_retirement!=0, ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all

		matrix b = e(b)
		matrix V = e(V)
		local est = b[1,1]
		local se  = sqrt(V[1,1])
		local ci_low  = `est' - 1.96*`se'
		local ci_high = `est' + 1.96*`se'

		display as result "`y': τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
	}

** (e) non-claimant sample (post-implementation, controlling for SPA)
	** Non-claimant sample
	local h = 64
	local outcomes dla_positive pip_positive dla_or_pip_positive total_dla_pip_payments total_benefit_payments good_health CESD high_CESD female econactive retired sick other

	foreach y of local outcomes {
		di "------------------------------------------------------------"
		di "RDD for `y' among never-treated (post-2013, eligible for retirement)"

		rdrobust `y' running_variable post_apr2013==1 & eligible_to_retire==1 & sex==1 & age>=65 | post_apr2013==1 & eligible_to_retire==1 & sex==2 & time_to_retirement!=0, ///
		covs(time_to_retirement time_to_retirement1 time_to_retirement2) ///
			h(`h') kernel(triangular) p(1) vce(cluster idauniq) all

		matrix b = e(b)
		matrix V = e(V)
		local est = b[1,1]
		local se  = sqrt(V[1,1])
		local ci_low  = `est' - 1.96*`se'
		local ci_high = `est' + 1.96*`se'

		display as result "`y': τ̂=" %6.3f `est' ", SE=" %6.3f `se' ", 95% CI [" %6.3f `ci_low' ", " %6.3f `ci_high' "]"
	}

********************************************************************************
** (3) McCrary Density Tests
********************************************************************************
	** Data
	use "${data_dir}cleaned_data.dta", clear 
	** Identify individuals who were DLA claimants in their last observation 
	** before April 2013.
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
	keep if year<2011
	* count observations
	count
	display "Total pre-2011 observations (pooled): " r(N)
	egen tag2011 = tag(idauniq)
	count if tag2011
	display "Unique individuals with post-Apr2018 observations: " r(N)
	
	* keep one observation of an individual
	bysort idauniq (year month wave_number): keep if _n == _N

	* McCrary Density Test (non-bias corrected) (Figure 2)
	rddensity running_variable, c(0) p(1) q(1) h(64 64) hist_n(8 8) plot_n(64 64) plot
	* McCrary Density Test (bias corrected) (Figure F2)
	rddensity running_variable, c(0) p(1) h(64 64) hist_n(8 8) plot_n(64 64) plot