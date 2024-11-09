* THIS DOFILE IS WRITTEN AS A SOLUTION TO PART 2 OF EMPIRICAL PROJECT 2 OF IBUS 6101
* Create a folder on Apporto Desktop called HW2 where you will store all relevant files associated with this project
* This includes the data file atlas_new.dta uploaded on BB

* Change the working directory to this folder. It should read something like below except you will use your username in place of "ayyagari"*/
cd Desktop\empricial2\

* Set up a log file which will store all the commands and output of your work today*
capture log close
log using hw2.log, replace

/*Q1: Nothing to do in Stata*/

/*Q2: How does average upward mobility, pooling races and genders, for children with parents at the 25th percentile (variable kfr pooled_p25) in this Census tract compare to population-weighted mean upward mobility in the state and in the U.S. overall? Do kids where you grew up have better or worse chances of climbing the income ladder than the average child in America? */ 

* Load the data
use atlas_new.dta, clear

* Average upward mobility in Census Tract 42101003702 in Philadelphia
sum kfr_pooled_p25 if state == "42" & county == "101" & tract == "003702"
local tract_mobility_25 = r(mean)

* Average upward mobility in Pennsylvania State
sum kfr_pooled_p25 if state == "42" [aw=count_pooled]
local state_mobility_25 = r(mean)

* Average upward mobility in US
sum kfr_pooled_p25 [aw=count_pooled]
local us_mobility_25 = r(mean)

di "Tract avg mobility: `tract_mobility_25'"
di "State avg mobility: `state_mobility_25'"
di "US average mobility: `us_mobility_25'"


/*Q3: What is the standard deviation of upward mobility in your county? Is it larger or smaller than the standard deviation across tracts in the state? Across tracts in the country? What do you learn from these comparisons? Note that you are not asked to report weighted values here */

* Standard deviation for Philadelphia County where tract is in
sum kfr_pooled_p25 if state == "42" & county == "101", detail
local philly_sd_25 = r(sd)

* Standard deviation for Pennsylvania State
sum kfr_pooled_p25 if state == "42", detail
local state_sd_25 = r(sd)

* Standard deviation for U.S.
sum kfr_pooled_p25, detail
local us_sd_25 = r(sd)

di "Philadelphia County SD: `philly_sd_25'"
di "Pennsylvania state SD: `state_sd_25'"
di "US SD: `us_sd_25'"

/*Q4: Now let's turn to downward mobility: 
repeat questions (2) and (3) looking at children who start with parents at the 75th and 100th percentiles.  
How do the patterns differ?
*/
* Downward mobility 
foreach pct in 75 100 {
    * Average mobility Census Tract 42101003702
    sum kfr_pooled_p`pct' if state == "42" & county == "101" & tract == "003702"
    local tract_mobility_`pct' = r(mean)

    * Weighted Average mobility for Pennsylvania State
    sum kfr_pooled_p`pct' if state == "42" [aw=count_pooled]
    local state_mobility_`pct' = r(mean)

    * Weighted average mobility for the U.S
    sum kfr_pooled_p`pct' [aw=count_pooled]
    local us_mobility_`pct' = r(mean)

    * Standard deviation of mobility for Philadelphia County where tract is in
    sum kfr_pooled_p`pct' if state == "42" & county == "101", detail
    local philly_sd_`pct' = r(sd)

    * Standard deviation of mobility for Pennsylvania
    sum kfr_pooled_p`pct' if state == "42", detail
    local state_sd_`pct' = r(sd)

    * Standard deviation of mobility for the US
    sum kfr_pooled_p`pct', detail
    local us_sd_`pct' = r(sd)

    di "`pct'th Percentile:"
    di "Tract 42101003702 mobility: `tract_mobility_`pct''"
    di "Pennsylvania state average mobility: `state_mobility_`pct''"
    di "US average mobility: `us_mobility_`pct''"
    di "Philadelphia County SD: `philly_sd_`pct''"
    di "Pennsylvania state SD: `state_sd_`pct''"
    di "US SD: `us_sd_`pct''"
}
****************************************************************

// Part II: Regression Analysis

*Q1: Using a linear regression, estimate the relationship between outcomes of children at the 25th and 75th percentile for the Census tracts in your home county.  Generate a scatter plot to visualize this regression.  Do areas where children from low-income families do well generally have better outcomes for those from high-income families, too?
*/

* Regression for Philadelphia County
reg kfr_pooled_p75 kfr_pooled_p25 if state == "42" & county == "101"

* Scatter plots of mobility outcomes
twoway (scatter kfr_pooled_p75 kfr_pooled_p25 if state == "42" & county == "101", legend(label(1 "Household Income ($)"))) (lfit kfr_pooled_p75 kfr_pooled_p25 if state == "42" & county == "101"), title("25th vs 75th Percentile Outcomes in Philadelphia County") xtitle("25th Percentile Outcome")  ytitle("75th Percentile Outcome", margin(up=15))

graph export "philadelphia_mobility_scatter.png", replace

/*Q2: Next, examine whether the patterns you have looked at above are similar by race. 
Draw a scatter plot for each race.
*/

* Scatter plots by race
foreach race in white black asian hisp {
    twoway (scatter kfr_`race'_p75 kfr_`race'_p25 if state == "42" & county == "101", legend(label(1 "Household Income ($)"))) (lfit kfr_`race'_p75 kfr_`race'_p25 if state == "42" & county == "101"), title("Mobility Outcomes by Race") xtitle("P25 for `race'", size(vlarge)) ytitle("P75 for `race'", margin(up=5) size(vlarge)) name(g_`race', replace)
	graph export "philadelphia_mobility_scatter_`race'.png", replace
}

* Combine scatter plots
graph combine g_white g_black g_asian g_hisp, title("Mobility Outcomes by Race") rows(2) cols(2) iscale(*0.4)
graph export "philadelphia_mobility_scatter_combined.png", replace width(1600) height(1300)

****************************************************************

// Part III: Regression with tract characteristics

/*Q4: Using a linear regression, estimate the relationship between outcomes of children at the 25th and 75th percentile for the Census tracts in your home county.  Generate a scatter plot to visualize this regression.  Do areas where children from low-income families do well generally have better outcomes for those from high-income families, too?
*/

* Definition of tract, county, and state codes
*local tract_code "42101003702"
*local county_code "101"
*local state_code "42"

* Import the tract characteristics dataset
import delimited "tract_allCharacteristics.csv", stringcols(1) clear 
describe

* Rename tract to tract_id for consistency
rename tract tract_id

save "tract_characteristics.dta", replace

* Assuming that the atlas data is still loaded
* Load atlas data
use "atlas_new.dta", clear

describe state county tract

* Create tract ID to the 11-digit format by concatenating strings
gen tract_id = state + county + tract

order tract_id, first

describe tract_id

* merge datasets
merge 1:1 tract_id using "tract_characteristics.dta", keep(master match)

* Check the merge results
tab _merge

* Keep only the matched observations
keep if _merge == 3
drop _merge

* Save the merged dataset
save "atlas_ALL.dta", replace

describe

*******  Correlation and Distribution *******

* Distribution plot for singleparent_share2016
histogram singleparent_share2016, kdensity title("Distribution of Single Parent Households") xtitle("Percentage of Single Parent Households") ytitle("Density")

* Distribution plot for poor_share2016
histogram poor_share2016, kdensity title("Distribution of Poverty Share") xtitle("Percentage of Households in Poverty") ytitle("Density")

* Distribution plot for frac_coll_plus2016
histogram frac_coll_plus2016, kdensity title("Distribution of College Education") xtitle("Percentage of Adults with College Degree") ytitle("Density")

* Twoway scatter plots
twoway (scatter med_hhinc2016_real singleparent_share2016) (lfit med_hhinc2016_real singleparent_share2016), title("Single Parent Households vs Median Income") xtitle("Percentage of Single Parent Households") ytitle("Median Household Income (2016)")
twoway (scatter med_hhinc2016_real poor_share2016) (lfit med_hhinc2016_real poor_share2016), title("Poverty Share vs Median Income") xtitle("Percentage of Households in Poverty") ytitle("Median Household Income (2016)")
twoway (scatter med_hhinc2016_real frac_coll_plus2016) (lfit med_hhinc2016_real frac_coll_plus2016), title("College Education vs Median Income") xtitle("Percentage of Adults with College Degree") ytitle("Median Household Income (2016)")


* install package to store results in a nice way 
ssc install estout, replace

* Regressions, adding one variable at a time

reg kfr_pooled_p25 med_hhinc2016_real if state == "42" & county == "101"
reg kfr_pooled_p25 frac_coll_plus2016 if state == "42" & county == "101"
reg kfr_pooled_p25 med_hhinc2016_real if state == "42" & county == "101"
reg kfr_pooled_p25 med_hhinc2016_real frac_coll_plus2016 if state == "42" & county == "101"
reg kfr_pooled_p25 frac_coll_plus2016 singleparent_share2016 if state == "42" & county == "101"
reg kfr_pooled_p25 med_hhinc2016 singleparent_share2016 if state == "42" & county == "101"
reg kfr_pooled_p25 med_hhinc2016_real frac_coll_plus2016 singleparent_share2016 if state == "42" & county == "101"

* Display results in a table
esttab, se r2 ar2 star(* 0.10 ** 0.05 *** 0.01)



* Install the estout package for formatting results
ssc install estout, replace

* Run and store regressions with different variable combinations
eststo clear

* Each regression stores results with eststo
eststo model1: reg kfr_pooled_p25 med_hhinc2016_real if state == "42" & county == "101"
eststo model2: reg kfr_pooled_p25 frac_coll_plus2016 if state == "42" & county == "101"
eststo model3: reg kfr_pooled_p25 singleparent_share2016 if state == "42" & county == "101"
eststo model4: reg kfr_pooled_p25 med_hhinc2016_real frac_coll_plus2016 if state == "42" & county == "101"
eststo model5: reg kfr_pooled_p25 frac_coll_plus2016 singleparent_share2016 if state == "42" & county == "101"
eststo model6: reg kfr_pooled_p25 med_hhinc2016_real singleparent_share2016 if state == "42" & county == "101"
eststo model7: reg kfr_pooled_p25 med_hhinc2016_real frac_coll_plus2016 singleparent_share2016 if state == "42" & county == "101"

* Display results in a formatted table
esttab, se r2 ar2 star(* 0.10 ** 0.05 *** 0.01) title("Regression Results: Upward Mobility and Key Covariates") label

* Estimated regression coefficients with 95% confidence intervals for key variables

* Regression with all variables as predictors of upward mobility (kfr_pooled_p25)
* This regression will estimate how changes in the independent variables (household income, college attainment, and single-parent share) are associated with changes in upward mobility (kfr_pooled_p25).
regress kfr_pooled_p25 med_hhinc2016_real frac_coll_plus2016 singleparent_share2016

* the coefficient and confidence interval for how each independent variable affects upward mobility (kfr_pooled_p25)
foreach var of varlist med_hhinc2016_real frac_coll_plus2016 singleparent_share2016 {
  lincom `var', level(95)
}

* Correlation matrix for the independent variables
pwcorr med_hhinc2016_real frac_coll_plus2016 singleparent_share2016, sig


log close
