************************ MARIE PARENT SAMPLE .do FILE *************************
/* About this file: This file is an edited version of a similar file that was
part of a group project for a course at the Goldman School of Public Policy at
UC Berkeley. We conducted a difference-in-differences analysis to examine any
effect of the Affordable Care Act on insurance rates for young adults. */

* Data source: IPUMS Current Population Survey
*******************************************************************************

clear
cls
set more off
global path="https://github.com/marieparent/ACA_DiffInDiff/raw/master"
use "$path/aca_data_march_cps.dta"


*********** DATA PREP ************
* About this section: create new variables, summarize by race etc., create summary data & save as new data file

/* Drop variables that will not be used */
drop any_insurance emp_prov_ins medicaid
label variable pvt_insurance "Insurance Rate"

/* Change year variable: original "year" variable reflects year of Current Population Survey.
Because survey asks about insurance status in the previous year,
the new "year" variable is the origianl "year" minus 1 to reflect the year for which we observe the insurance status. */
gen year_adj = year-1
label variable year_adj "year for which insurance status is observed (adjusted from year of survey)"
drop year
rename year_adj year

/* Post-Policy binary variable.  Year 2010 omitted. */
gen postyr = year
replace postyr = 0 if year<=2009
replace postyr = 1 if year>=2011
replace postyr = . if postyr>1
label variable postyr "dummy variable, =1 if 2011 or 2012, =0 otherwise (omits 2010)"

/* Restrict observations by age, narrowing band around age 26 cutoff */
drop if age<24 | age>27

/* Eligibility binary variable */
gen elig = 1 if age<=25
replace elig = 0 if age>=26
label variable elig "dummy variable, =1 if under 26, =0 otherwise"

/* Race Variables */
gen white = 1 if race_ethnic==1
replace white = 0 if race_ethnic!=1

gen black = 1 if race_ethnic==2
replace black = 0 if race_ethnic!=2

gen other = 1 if race_ethnic==3
replace other = 0 if race_ethnic!=3

gen hispanic = 1 if race_ethnic==4
replace hispanic = 0 if race_ethnic!=4

drop race_ethnic

/* Interaction Terms */
gen post_elig = postyr*elig
label variable post_elig "interaction term for postyr and elig"

gen post_elig_other = postyr*elig*other
gen post_elig_black = postyr*elig*black
gen post_elig_hispanic = postyr*elig*hispanic
gen post_elig_white = postyr*elig*white

gen year_elig = year*elig

/* Ordering the variables for display in the summary statistics table */
order pvt_insurance, first

summarize

*save CleanedAcaData, replace

*********** PARALLEL TRENDS ************
* Establish parallel trends by running regressions on insurance and demographics
* And by using the summarized data to create line graph visuals

/* Regression */
logit pvt_insurance year elig year_elig if year<2010
/* Theoretically, the coefficient on year*elig should be statistically insignificantly different from 0 */

collapse age female white black other hispanic pvt_insurance, by(year elig)

gen eventyear = year-2011
label variable eventyear "Year Since Implementation of Parents' Provision"
label variable pvt_insurance "Insurance Rate"

/* Line Graph */
twoway (line pvt_insurance eventyear if elig==0) (line pvt_insurance eventyear if elig==1), xline(0)  title("Insurance Rate by Age Group, 2004-2012") legend(label(1 "Ineligible") label(2 "Eligible"))

/* Table */
table eventyear elig, contents(mean pvt_insurance)

reshape wide age female white black other hispanic pvt_insurance eventyear, i(year) j(elig)

*save CollapsedData, replace


*********** REGRESSIONS ************
* Run regressions

clear
use "$path/CleanedAcaData.dta"

/* Model 1 - Unrestricted */
logit pvt_insurance postyr elig post_elig
estimates store Unrestricted

/* Models 2 - Models for Each Race Group */
logit pvt_insurance postyr elig post_elig if white==1
estimates store White
logit pvt_insurance postyr elig post_elig if other==1
estimates store Other
logit pvt_insurance postyr elig post_elig if black==1
estimates store Black
logit pvt_insurance postyr elig post_elig if hispanic==1
estimates store Hispanic

/* Model 3 - Restricted1 */
logit pvt_insurance postyr elig black hispanic other post_elig
test black hispanic other
estimates store Restricted1

/* Models 3 - Restricted2 */
logit pvt_insurance postyr elig black hispanic other post_elig post_elig_black post_elig_hispanic post_elig_other

estimates store Restricted2

/* Are our race interaction terms significant as a group? */
test post_elig_black post_elig_hispanic post_elig_other

/* Are there significant differences between each of the race interaction terms? */
test post_elig_black = post_elig_other
test post_elig_black = post_elig_hispanic
test post_elig_other = post_elig_hispanic

est tab Unrestricted Restricted1 Restricted2, newpanel varwidth(20) modelwidth(10) title(Comparison of Unrestricted and Restricted Models) style(columns) star
