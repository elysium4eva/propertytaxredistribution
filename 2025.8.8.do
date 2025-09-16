*****summarize statisticis****

estpost summarize  totalpopulation employees wage socialeconomicscore arnona_rate_res arnona_rate_bus residentialstarts businessstarts balance accumulated_balance independence immi emmi, detail
esttab . using "summary.csv", ///
    cells("count mean sd min max") ///
    noobs nonumber nomtitle label replace



* Step 0 generate treatment variable*****
xtset ID year

* Generate post-treatment indicator
gen post=1 if year>2011
replace post = 0 if missing(post)

* Generate treatment indicator
gen treated = 1
replace treated = 0 if inlist(ID, 70, 2650, 4000, 5000, 6300, 6400, 6500, 6600, 6900, 7100, 7400, 7900, 8300, 8400, 8600, 8700, 9000)
gen did = treated * post




* Table 1*
* Baseline DID results model 1
eststo model1: xtreg log_start_res did  i.year, fe vce(robust)
* alternative regression
xtdidregress (log_start_res) (did), group(ID) time(year)

* Baseline DID results model 2
eststo model2: xtreg log_start_res did log_land_reser log_start_bus log_pop_dens log_employ independence em_rate i.year, fe vce(robust)
* alternative regression
xtdidregress (log_start_res log_land_reser log_start_bus log_pop_dens log_employ independence em_rate) (did), group(ID) time(year)

* Baseline DID results model 3
eststo model3: xtreg log_start_res did log_arnona_rate_res log_land_reser log_start_bus log_pop_dens log_employ independence em_rate i.year, fe vce(robust)
* alternative regression
xtdidregress (log_start_res log_arnona_rate_res log_land_reser log_start_bus log_pop_dens log_employ independence em_rate) (did), group(ID) time(year)
estat ptrends

**Graphical diagnostics**
estat trendplot


* Baseline DID results model 4
eststo model4: xtreg log_start_bus did log_arnona_rate_bus log_land_reser log_start_res log_pop_dens log_employ independence em_rate i.year, fe vce(robust)
* alternative regression
xtdidregress (log_start_bus log_arnona_rate_bus log_land_reser log_start_res log_pop_dens log_employ independence em_rate) (did), group(ID) time(year)

* export the regression results
esttab model1 model2 model3 model4 using "regression_results.csv", ///
      csv replace label b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
      stats(r2 N, fmt(3 0) labels("R-squared")) ///
      title("Regression Results") ///
      mtitle("Model 1" "Model 2" "Model 3" "Model 4") ///
      addnotes("Standard errors in parentheses; * p<0.05, ** p<0.05, *** p<0.01")

* PSM procedure
* Step 2: Estimate Propensity Scores
psmatch2 treated log_arnona_rate_res log_wage jew log_land_reser log_start_bus log_pop_dens log_employ independence em_rate if post == 0 , ///
         logit out(log_start_res) common ties ///
         n(1) caliper(0.05) // Nearest neighbor (1:1) matching with 0.05 caliper
		 
* Evaluate match graphically
psgraph

* Step 3: Balance check (FIXED)
pstest  log_arnona_rate_res log_wage jew log_land_reser log_start_bus log_pop_dens log_employ independence em_rate, treated(treated) both graph


* Step 4: Drop unsupported samples*
gen common=_support
eststo model5: xtreg log_start_res did log_start_bus log_employ independence em_rate i.year if common != 0, fe vce(robust)

* alternative regression
xtdidregress (log_start_res log_start_bus log_employ independence em_rate) (did) if common != 0, group(ID) time(year)

estat granger
estat grangerplot
estat ptrends
estat trendplots

eststo model6: xtreg log_start_res did log_start_bus log_pop_dens log_land_reser log_employ independence em_rate i.year if common != 0, fe vce(robust)

* alternative regression
xtdidregress (log_start_res log_start_bus log_pop_dens log_land_reser log_employ independence em_rate) (did) if common != 0, group(ID) time(year)


* Step 2: Estimate Propensity Scores
drop common
drop _support
psmatch2 treated log_arnona_rate_res log_wage jew log_land_reser log_start_bus log_pop_dens log_employ independence em_rate if post == 0 , ///
         kernel out(log_start_res) common ties ///
         n(5) caliper(0.05) // Nearest neighbor (1:1) matching with 0.05 caliper
		 

* Step 4: Drop unsupported samples*

gen common=_support
eststo model7: xtreg log_start_res did log_start_bus log_employ independence em_rate i.year if common != 0, fe vce(robust)

* alternative regression
xtdidregress (log_start_res log_start_bus log_employ independence em_rate) (did) if common != 0, group(ID) time(year)


* export the regression results
esttab model3 model5 model6 model7 using "regression_results1.csv", ///
      csv replace label b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
      stats(r2 N, fmt(3 0) labels("R-squared")) ///
      title("Regression Results") ///
      mtitle("Model 5" "Model 6" "Model 7" "Model 8") ///
      addnotes("Standard errors in parentheses; * p<0.05, ** p<0.05, *** p<0.01")


*******perallal trend test*********
* Create relative time indicators
gen rel_time = year - 2011
tab rel_time, gen(rt)

* Create shifted relative time
sum rel_time, meanonly
gen rel_time_s = rel_time - r(min)


* Alternative with fixed effects:
* Estimate dynamic specification
xtreg log_start_res ibn.rel_time_s#i.treat log_arnona_rate_res log_land_reser log_start_bus log_pop_dens log_employ independence em_rate i.year, fe 

* Store results for plotting
estimates store event_study


* Test pre-treatment coefficients (periods -3 to -1)
test rt_12#treat rt_13#treat rt_14#treat

* Alternatively using factor variables
testparm i(-3).rel_time#treat i(-2).rel_time#treat


* Plot coefficients with confidence intervals
coefplot event_study, ///
    keep(*treat*) ///
    vertical ///
    recast(connected) ///
    ciopts(recast(rline) lpattern(dash)) ///
    xline(0, lpattern(dash)) ///
    yline(0, lcolor(black)) ///
    xtitle("Relative time to treatment") ///
    ytitle("Coefficient") ///
    title("Event Study Plot") ///
    graphregion(color(white))
	
	
****************

* Step 0 generate treatment variable*****
gen post=1 if year>2012
replace post = 0 if missing(post)
gen treated = 1
replace treated = 0 if inlist(ID, 70, 2650, 4000, 5000, 6300, 6400, 6500, 6600, 6900, 7100, 7400, 7900, 8300, 8400, 8600, 8700, 9000)
gen did = treated * post


* Step 2: Estimate Propensity Scores
psmatch2 treated log_wage log_land_reser log_arnona_rate_bus log_employ log_pop_dens log_balance_accu balance_ratio independence if post == 0 , ///
         logit out(log_start_bus) common ties ///
         n(1) caliper(0.05) // Nearest neighbor (1:1) matching with 0.05 caliper
		 
* Evaluate match graphically
psgraph

* Step 3: Balance check (FIXED)
pstest  log_wage log_land_reser log_arnona_rate_bus log_employ log_pop_dens log_balance_accu balance_ratio independence, treated(treated) both graph		 
		 
* Step 4: Drop unsupported samples*
gen common=_support
drop if common == 0
xtreg log_start_bus did log_wage log_land_reser log_arnona_rate_bus log_employ log_pop_dens log_balance_accu balance_ratio independence i.year, fe