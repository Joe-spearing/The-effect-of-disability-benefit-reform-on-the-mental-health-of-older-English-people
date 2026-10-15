********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Descriptive Statistics  
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
**	Description: This file creates the descriptive statistics table (Table D1)
**				and the share of people claiming DLA and PIP by date of birth 
**				(Figure 1).
**
** 	Table of contents:
**		(1) Descriptive statistics (Table D1)
**		(2) DLA or PIP claiming by date of birth (Figure 1)
**
********************************************************************************

********************************************************************************
** (1) Descriptive Statistics
********************************************************************************
** Data
use "${data_dir}cleaned_data.dta", clear 

** Identify individuals who were DLA claimants in their last observation before 
** April 2013.
** Replace yob to most common yob
bysort idauniq: egen yob1 = mode(yob)
gen pre_apr2013 = (year < 2013 | (year == 2013 & month < 4))

preserve
    keep if pre_apr2013
    * order by interview date within person.
    * JS note: also keep only most recent observation?
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
*JS note: matches the main data set to "dla claimants" and only keeps if there is a match
egen tagid = tag(idauniq)
*JS note: generates a new unique identified for each person
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

** Table D1
gen female=1 if sex==2
replace female=0 if sex==1

sum dla_positive total_dla_pip_payments good_health CESD high_CESD retired working female my_age
sum dla_positive total_dla_pip_payments good_health CESD high_CESD retired working female my_age if running_variable>=-60 & running_variable<=60

gen above_cutoff = 1 if (running_variable>=1 & running_variable<=60)
replace above_cutoff = 0 if running_variable>=-60 & running_variable<=0
*Check for each variable
ttest my_age, by(above_cutoff)

********************************************************************************
** (2) DLA or PIP claiming in date of birth (Figure 1)
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

* check baseline size (R equivalent: length(dla.claimants))
merge m:1 idauniq using "`dla_claimants'", keep(match) nogen
egen tagid = tag(idauniq)
* drop if born April 1948, as we can't determine which side of the cutoff you fall.
drop if yob==1948 & mob==4
count if tagid & dla>=0
display "Baseline DLA claimants (pre-Apr2013): " r(N)
* apply restrictions as in R-file
keep if yob>0
keep if year>2011

** Share of people claiming DLA
gen dla_claim = dla > 0
by year, sort: egen share_all = mean(dla_claim)
by year, sort: egen share_before = mean(dla_claim) ///
    if yob < 1948 | (yob == 1948 & mob < 4)
by year, sort: egen share_after = mean(dla_claim) ///
    if yob > 1948 | (yob == 1948 & mob >= 4)
twoway ///
    (line share_before year, lpattern(dash)    lcolor(black)     msymbol(triangle) mcolor(gs8)) ///
    (line share_after  year, lpattern(solid)     lcolor(gs8)    msymbol(circle)  mcolor(gs8)), ///
    ytitle("Share of people claiming DLA") ///
    xtitle("Year") ///
    xlabel(2012(2)2023) ///
    legend(order(1 "Born before April 1948" 2 "Born after April 1948") ///
           position(1) ring(0)) ///
    graphregion(color(white)) ///
    plotregion(color(white))
	
** Share of people claiming PIP
gen pip_claim = pip > 0
by year, sort: egen share_all_pip = mean(pip_claim)
by year, sort: egen share_before_pip = mean(pip_claim) ///
    if yob < 1948 | (yob == 1948 & mob < 4)
by year, sort: egen share_after_pip = mean(pip_claim) ///
    if yob > 1948 | (yob == 1948 & mob >= 4)
twoway ///
    (line share_before_pip year, lpattern(dash)    lcolor(black)     msymbol(triangle) mcolor(gs8)) ///
    (line share_after_pip  year, lpattern(solid)     lcolor(gs8)    msymbol(circle)  mcolor(gs8)), ///
    ytitle("Share of people claiming PIP") ///
    xtitle("Year") ///
    xlabel(2012(2)2023) ///
    legend(order(1 "Born before April 1948" 2 "Born after April 1948") ///
           position(10) ring(0)) ///
    graphregion(color(white)) ///
    plotregion(color(white))