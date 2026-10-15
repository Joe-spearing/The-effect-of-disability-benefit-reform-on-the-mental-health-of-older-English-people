********************************************************************************
** 	The Effect of Disaibility Reform on the Health of Older People in England 
**
**	Purpose: Master file for generating the paper  
** 	Authors: Andrew M Jones, James Lomas, Suzanna Nesom*, Joe Spearing         
**	Last Updated: September 2026
**
** Notes: This do file performs statistical analysis on data from the English 
**		Longitudinal Study of Ageing (ELSA), accessed via the UK Data Archive.
**
**		The do-file does not append the raw data each time. Instead, it uses a 
**		pre-cleaned dataset that has already been complied (corresponding to 
**		lines 1-451 of the original R-code. This code includes a running 
**		variable measures months before/after the cutoff data (8 April 1948), 
** 		with a half-month correction placing births at the midpoint of each 
**		month.
**
** Location of all codes: 
** https://github.com/Joe-spearing/The-effect-of-disability-benefit-reform-on-the-mental-health-of-older-English-people
********************************************************************************

// SET UP
* stata 19.0
clear all
set mem 1300m
set more off
cap log close
eststo clear

* Directory globals
global data_dir   "Z:\Data\UKDA-5050-stata\stata\stata13_se\"
global charts_dir "Z:\Charts"
global dofile_dir "Z:\do_file\"

* Uncomment to install rdrobust if needed:
*net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace


**********
* (1) Descriptive Statistics
**********
do "${dofile_dir}01_descriptive_statistics"

**********
* (2) Falsification Tests
**********
do "${dofile_dir}02_falsification_tests"

**********
* (3) Main Results: Approach 1 - post April 2018
**********
do "${dofile_dir}03_approach_1"

**********
* (4) Main Results: Approach 2 - post April 2013, dropping individuals not yet reached SPA
**********
do "${dofile_dir}04_approach_2_3"

**********
* (5) Robustness: Pooling UKHLS and ELSA
**********
do "${dofile_dir}05_UKHLS_ELSA"

**********
* (6) Robustness: Timing of the effects and evidence of anticipation
**********
do "${dofile_dir}06_Timing_Anticipation"

**********
* (7) Robustness: Alternative specification and estimation choices
**********
do "${dofile_dir}07_Robustness"