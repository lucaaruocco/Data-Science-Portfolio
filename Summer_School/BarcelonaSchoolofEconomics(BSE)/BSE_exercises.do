******************************************************
* Barcelona School of Economics – Summer School 2024 *
* Quantitative Methods for Public Policy Evaluation  *
* Exercises (replications)                          *
******************************************************

*-----------------------------------------------------
* PS1 – Randomized Controlled Trials
*-----------------------------------------------------

***********************************************************
*** set directory
*** load dataset
*** change for your working directory
***********************************************************

clear all
version 17
cap log close
***********************************************************
*** install packages
***********************************************************

cap ssc install estout.ado
cap ssc install esttab.ado
cap ssc install eststo.ado
cap ssc install estadd.ado
cap ssc install estpost.ado
cap ssc install cibar 
cap net install grc1leg.pkg
cap ssc install outreg2
cap ssc install ritest


global dir "your directory"

log using "$dir/log_PS1.log", replace

use "$dir/altfacts.dta", clear


***********************************************************
*** Randomization
***********************************************************

local tabnom "BalanceCheck"

global pre_treatment_vars_t1 gender educ born_france father_born_france mother_born_france age city married income children 

eststo control: quietly estpost summarize $pre_treatment_vars_t1 if treatment==1

eststo imposed: quietly estpost summarize $pre_treatment_vars_t1 if treatment==2

eststo voluntary: quietly estpost summarize $pre_treatment_vars_t1 if treatment==3

eststo diff: quietly estpost ttest $pre_treatment_vars_t1 if treatment<3, by(treatment) unequal

eststo diff2: quietly estpost ttest $pre_treatment_vars_t1 if treatment==1 | treatment==3, by(treatment) unequal

eststo diff3: quietly estpost ttest $pre_treatment_vars_t1 if treatment==2 | treatment==3, by(treatment) unequal

esttab control imposed voluntary diff diff2 diff3, cells("mean(pattern(1 1 1 0 0 0) fmt(2)) sd(pattern(1 1 1 0 0 0)) b(pattern(0 0 0 1 1 1) fmt(2)) p(pattern(0 0 0 1 1 1) par fmt(2))") label

esttab control imposed voluntary diff diff2 diff3 using `tabnom'.tex, replace cells("mean(pattern(1 1 1 0 0 0) fmt(2)) sd(pattern(1 1 1 0 0 0)) b(pattern(0 0 0 1 1 1) fmt(2)) p(pattern(0 0 0 1 1 1) par fmt(2))") label

***********************************************************
*** Average Treatment Effect
***********************************************************

reg int_share_alt i.treatment, r // this is the percentage point effect on the intention to share

sum int_share_alt if e(sample)&treatment==1
scalar y_contr = r(mean)

test 2.treatment - 3.treatment = 0
scalar p_2_3 = r(p)

outreg2 using "$dir/panelA.xls", replace label tex excel bdec(3) tdec(3) sdec(3) nocon nonotes adds(Mean DV alt-facts treatment, y_contr, pvalue, p_2_3)

reg act_share_alt i.treatment, r // this is the percentage point effect on the action to share
sum act_share_alt if e(sample)&treatment==1
scalar y_contr = r(mean)
test 2.treatment - 3.treatment = 0
scalar p_2_3 = r(p)

outreg2 using "$dir/panelA.xls", append label tex excel bdec(3) tdec(3) sdec(3) nocon nonotes adds(Mean DV alt-facts treatment, y_contr, pvalue, p_2_3)

reg int_share_alt i.treatment i.gender i.educ i.mother_born_france i.father_born_france, r
sum int_share_alt if e(sample)&treatment==1
scalar y_contr = r(mean)
test 2.treatment - 3.treatment = 0
scalar p_2_3 = r(p)

outreg2 using "$dir/panelA1.xls", replace label tex excel bdec(3) tdec(3) sdec(3) nocon nonotes adds(Mean DV alt-facts treatment, y_contr, pvalue, p_2_3)

reg act_share_alt i.treatment i.gender educ, r
sum act_share_alt if e(sample)&treatment==1
scalar y_contr = r(mean)
test 2.treatment - 3.treatment = 0
scalar p_2_3 = r(p)

outreg2 using "$dir/panelA1.xls", append label tex excel bdec(3) tdec(3) sdec(3) nocon nonotes adds(Mean DV alt-facts treatment, y_contr, pvalue, p_2_3)

***********************************************************
*** Graphical Representation of ATE
***********************************************************

label define treatment1 1 "Alt-facts" 2 "Imposed fact-check" 3 "Voluntary fact-check"
label values treatment treatment1

cibar int_share_alt, over1(treatment) barcol(ltblue emidblue dknavy) graphopts(title("Intent of Sharing Alt-Facts") graphregion(color(white)) ytitle("Intent of Sharing Alt-Facts on FB"  " ") ysc(range(0 0.2)) ylabel(0 (0.05) 0.2) legend(cols(1))) bargap(20)
graph save "$dir/ATE_altfacts1", replace

cibar act_share_alt, over1(treatment) barcol(ltblue emidblue dknavy) graphopts(title("Action of Sharing Alt-Facts") graphregion(color(white)) ytitle("Action of Sharing Alt-Facts on FB" " ") ysc(range(0 0.06)) ylabel(0 (0.02) 0.06) legend(cols(1))) bargap(20) 
graph save "$dir/ATE_altfacts2", replace

grc1leg "$dir/ATE_altfacts1" "$dir/ATE_altfacts2", graphregion(color(white)) legendfrom("$dir/ATE_altfacts1")
graph export "$dir/Figure_4_Panel_A.pdf", replace

***********************************************************
*** Randomization Inference
***********************************************************
set seed 1001
ritest treatment _b[2.treatment], kdensityplot: reg int_share_alt i.treatment if treatment != 3
graph export ritest2.pdf, replace
ritest treatment _b[3.treatment], kdensityplot: reg act_share_alt i.treatment if treatment != 2
graph export ritest3.pdf, replace

***********************************************************
*** Conditional Average Treatment Effect (CATE)
***********************************************************

reg int_share_alt i.treatment##c.gender, r // on average, men intend to share more than women (coeff on gender .04). treatment effect on men= 
lincom 2.treatment+2.treatment#c.gender // CATE of treatment 2 on men 
lincom 3.treatment+3.treatment#c.gender // CATE of treatment 3 on men
* imposed fact checking is effective on men, but not on women (check the uninteracted coefficient), the other way around for voluntary fact check

sum int_share_alt if e(sample)&treatment==1
scalar y_contr = r(mean)
test 2.treatment - 3.treatment = 0
scalar p_2_3 = r(p)

outreg2 using "$dir/cate.xls", replace label tex excel bdec(3) tdec(3) sdec(3) nocon nonotes adds(Mean DV alt-facts treatment, y_contr, pvalue, p_2_3)

reg act_share_alt i.treatment##c.gender, r // on average, men share more than women (coeff on gender .022)
lincom 2.treatment+2.treatment#c.gender // CATE of treatment 2 on men: imposed fact check is effective on men, but not on women
lincom 3.treatment+3.treatment#c.gender // the other way around for voluntary fact check

log close


*-----------------------------------------------------
* PS2 – Regression Discontinuity Designs
*-----------------------------------------------------

clear all
version 17
capture log close

***********************************************************
*** install packages
***********************************************************

cap ssc install estout.ado
cap ssc install esttab.ado
cap ssc install eststo.ado
cap ssc install estpost.ado
cap ssc install outreg2.ado
cap ssc install rdrobust
cap ssc install rd

* rdd packages for McCrary density plot
cap ssc install rddensity
net install lpdensity, replace from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata)

*************************************
**** 
**** Directory
****
*************************************

* set your working directory
global dir = "your directory" 

cd "$dir"

log using "log_PS2.log", replace

use "mental.dta", clear

do "rd_an.do"

global contr="month_* province_n_* i.ethnicity i.education female i.survey_taker_id"
global se "cluster modate"	
global slvl "starlevels(* 0.10 ** 0.05 *** 0.01)"

global depression_vars "z_depression z_somatic z_nonsomatic sum_srq"
global mobility_vars "under_curfew outside_week never_out"

*************************************
**** 
**** Figure 2: RD TREATMENT EFFECTS ON MOBILITY
****
*************************************

/* loop over mobility variables */
foreach v in $mobility_vars {
	
  /* get the variable label for this var */
  local title: var label `v'
  local xtitle xtitle("Born before December 1955 (in months)", size(2.5) color(black))  
}

	/* set optimal bandwidth */
	local bw = 44

rd under_curfew dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	msize(small) `xtitle' title("(a) Under curfew", size(small) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/Curfew.gph", replace

rd outside_week dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	msize(small) `xtitle' title("(b) Days outside last week", size(small) ///
	color(black)) ylabel(0(2)6,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/Outside_week.gph", replace
  
rd never_out dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	msize(small) `xtitle' title("(c) Never goes out", size(small) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/Never_out.gph", replace
	
graph combine "graphs/Curfew.gph" "graphs/Outside_week.gph" "graphs/Never_out.gph", row(3) ///
	imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ysize(6) xsize(5.5) ///
	title("", size(3.5) color(black)) saving("graphs/Figure2.gph", replace)

graph export "Figure2.pdf", replace font($grfont)

*************************************
**** 
**** Figure 3: RD TREATMENT EFFECTS ON MENTAL HEALTH OUTCOMES
****
*************************************

/* loop over all mental health variables */
foreach v in $depression_vars {

  cap drop yhat
  cap drop `v'_resid

  *local exclude `v'
  local bw = 44

  /* get the variable label for this var */
  local title: var label `v'
  local xtitle xtitle("Born before December 1955 (in months)", size(2.5) color(black))

  /* residualize output variable on fixed effects */
  reg `v'   $controls if inrange(dif,-`bw',`bw'), vce($se)
  predict yhat
  gen `v'_resid = `v' - yhat
  
}

rd z_depression_resid dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(a) Mental distress index", size(small) ///
	color(black)) ylabel(-1(0.5)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/Depression.gph", replace

rd z_somatic_resid dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(b) Somatic symptoms of distress index", size(small) ///
	color(black)) ylabel(-1(0.5)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/Somatic.gph", replace
  
rd z_nonsomatic_resid dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(c) Nonsomatic symptoms of distress index", size(small) ///
	color(black)) ylabel(-1(0.5)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/Nonsomatic.gph", replace
	
rd sum_srq_resid dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(d) Sum of yes answers in SRQ-20", size(small) ///
	color(black)) ylabel(-4(2)4,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/Sum_srq.gph", replace

graph combine "graphs/Depression.gph" "graphs/Somatic.gph" "graphs/Nonsomatic.gph" "graphs/Sum_srq.gph", col(2) ///
	imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ysize(6) xsize(5.5) ///
	title("", size(3.5) color(black)) saving("graphs/Figure3.gph", replace)

graph export "Figure3.pdf", replace font($grfont)

*************************************
**** 
**** Table 4: Treatment Effects
****
*************************************
eststo clear

*Linear, bandwith of +/- 17
local bw = 17
foreach x in z_depression z_somatic z_nonsomatic sum_srq{
	xi: reg `x' before1955 di1* $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l1_`x'
}

*Linear, bandwith of +/- 30
local bw = 30
foreach x in z_depression z_somatic z_nonsomatic sum_srq{
	xi: reg `x' before1955 di1 di1_i $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l2_`x'
}

*Linear, bandwith of +/- 45
local bw = 45
foreach x in z_depression z_somatic z_nonsomatic sum_srq{
	xi: reg `x' before1955 di1* $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l3_`x'
}

*Linear, bandwith of +/- 60
local bw = 60
foreach x in z_depression z_somatic z_nonsomatic sum_srq{
	xi: reg `x' before1955 di1* $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l4_`x'
}

foreach x in z_depression z_somatic z_nonsomatic sum_srq{
	local title_`x': var label `x'
	local title_`x' `title_`x'' \\
}
	estout l1_z_depression l2_z_depression l3_z_depression l4_z_depression using "table4.tex", ///
			replace style(tex) collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par)) title("`title_z_depression'")  ///
			keep(before1955) mlabels(, none) stats(N outcome_mean, fmt(%9.0g %9.2f) ///
			labels("\hspace{0.5 cm} Observations" "\hspace{0.5 cm} Control group mean" ) ///
			) $slvl 

foreach x in z_somatic z_nonsomatic {
	estout l1_`x' l2_`x' l3_`x' l4_`x' using "table4.tex", ///
			append style(tex) collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par)) title("`title_`x''") ///
			keep(before1955) mlabels(, none) stats(N outcome_mean, fmt(%9.0g %9.2f) ///
			labels("\hspace{0.5 cm} Observations" "\hspace{0.5 cm} Control group mean" ) ///
			) $slvl 
}

foreach x in sum_srq{
	estout l1_`x' l2_`x' l3_`x' l4_`x'  using "table4.tex", ///
			append style(tex) collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par)) title("`title_`x''") ///
			keep(before1955) mlabels(, none) stats(N outcome_mean, fmt(%9.0g %9.2f) ///
			labels("\hspace{0.5 cm} Observations" "\hspace{0.5 cm} Control group mean") ///
			layout(@ @)) $slvl 
}

	estout l1_z_depression l2_z_depression l3_z_depression l4_z_depression , ///
			replace style(tex) collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par))   ///
			keep(before1955) mlabels(, none) stats(N outcome_mean, fmt(%9.0g) ///
			labels("\hspace{0.5 cm} Observations" "\hspace{0.5 cm} Control group mean" ) ///
			) $slvl 
			
*************************************
**** 
**** Figure 1: Covariate Check
****
*************************************

local bw = 44
local xtitle xtitle("Born before December 1955 (in months)", size(3) color(black))

rd highschool dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(a) Completed high school", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/highschool.gph", replace


rd illiterate dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(b) Illiterate", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/illiterate.gph", replace
	
rd female dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(c) Female", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/female.gph", replace
	
rd married dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(d) Married", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/married.gph", replace
	
rd widowed_separated dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(e) Widowed or separated", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/widowed_separated.gph", replace
	
rd non_turk dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(f) Non-Turkish", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/non_turk.gph", replace
	
rd pre_covid_hhsize dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(g) Pre-Covid-19 household size", size(medsmall) ///
	color(black)) ylabel(0(2)6,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/pre_covid_hhsize.gph", replace
	
rd psych_support dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(h) Ever received psychological support", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/psych_support.gph", replace
	
rd chronic_disease dif if inrange(dif,-`bw',`bw'), bins(44) bw start(-`bw') end(`bw') ///
	name(`v') msize(small) `xtitle' title("(i) Has a chronic disease", size(medsmall) ///
	color(black)) ylabel(0(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) degree(1) 
	graph save "graphs/chronic_disease.gph", replace
	
graph combine "graphs/highschool.gph" "graphs/illiterate.gph" "graphs/female.gph" "graphs/married.gph"  ///
	"graphs/widowed_separated.gph" "graphs/non_turk.gph" "graphs/pre_covid_hhsize.gph" "graphs/psych_support.gph" "graphs/chronic_disease.gph", col(3) ///
	imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ysize(4) xsize(5.5) saving("graphs/Figure1.gph", replace)

graph export "Figure1.pdf", replace

*************************************
**** 
**** Density Check
****
*************************************
rddensity dif, c(0) plot

rddensity dif, c(0) p(2) h(44 44) plot graph_opt(graphr(c(white)) legend(off) title("Manipulation Testing Plot"))
graph export density_test_stata.pdf, replace font($grfont)

rddensity dif, c(0) p(1) plot graph_opt(graphr(c(white)) legend(off) title("Manipulation Testing Plot"))
rddensity dif, c(0) p(2) plot graph_opt(graphr(c(white)) legend(off) title("Manipulation Testing Plot")) // Different results depending on the order of the polynomial

*************************************
**** 
**** BONUS Alternative commands based on the more recent rdrobust package which is the recommended package at the moment
****
*************************************

***** polynomial of degree 1 with bandwidth of 44 ******
rdrobust z_depression_resid dif if inrange(dif,-44,44), c(0) p(1) h(44 44)
rdplot z_depression_resid dif if inrange(dif,-44,44), c(0) p(1) h(44 44) graph_options(title("Impact on Mental Distress", size(medsmall) color(black)) legend(off) ylabel(-1(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) graphr(c(white))) name(mc2,replace)

***** polynomial of degree 1 with optimal bandwidth ******
rdplot z_depression_resid dif if inrange(dif,-44,44), c(0) p(1) h("`e(h_l)'" "`e(h_r)'") graph_options(title("Impact on Mental Distress", size(medsmall) color(black)) legend(off)  ylabel(-1(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) graphr(c(white))) name(mc3,replace)

***** polynomial of degree 2 with bandwidth of 44 ******
rdrobust z_depression_resid dif if inrange(dif,-44,44), c(0) p(2) h(44 44) all
rdplot z_depression_resid dif if inrange(dif,-44,44), c(0) p(2) h(44 44) graph_options(title("Impact on Mental Distress", size(medsmall) color(black)) legend(off)  ylabel(-1(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) graphr(c(white))) name(mc4,replace)

***** polynomial of degree 2 with optimal bandwidth ******
rdrobust z_depression_resid dif if inrange(dif,-44,44), c(0) p(2) all
rdplot z_depression_resid dif if inrange(dif,-44,44), c(0) p(2) h("`e(h_l)'" "`e(h_r)'") graph_options(title("Impact on Mental Distress", size(medsmall) color(black)) legend(off)  ylabel(-1(0.2)1,labsize(small)) xlabel(-44(22)44,labsize(small)) graphr(c(white))) name(mc5,replace)


*************************************
**** 
**** BONUS Placebo Treatment: effects on other variables
****
*************************************

local bw = 17
eststo clear
foreach x in highschool illiterate female married {
	xi: reg `x' before1955 di1* $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l1_`x'
}

*Linear 30 bandwith
local bw = 30
foreach x in highschool illiterate female married {
	xi: reg `x' before1955 di1* $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l2_`x'
}

*Linear 45 bandwidth
local bw = 45
foreach x in highschool illiterate female married {
	xi: reg `x' before1955 di1* $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l3_`x'
}

*Linear 60 bandwidth
local bw = 60
foreach x in highschool illiterate female married {
	xi: reg `x' before1955 di1* $contr  if inrange(dif,-`bw',`bw'), vce($se)
	sum `x' if e(sample) & before1955 == 0
    estadd scalar outcome_mean = r(mean)
	eststo l4_`x'
}

	estout l1_highschool l2_highschool l3_highschool l4_highschool, ///
			replace collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par))  ///
			keep(before1955) mlabels(, none) stats(N , fmt(%9.0g ) ///
			labels("Observations" ) ///
			) $slvl 

	foreach x in illiterate female married {
		estout l1_`x' l2_`x' l3_`x' l4_`x', ///
				append collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par))  ///
				keep(before1955) mlabels(, none) stats(N , fmt(%9.0g ) ///
				labels("Observations" ) ///
				) $slvl 
	}


	estout l1_highschool l2_highschool l3_highschool l4_highschool using "table4_placebo.tex", ///
			replace style(tex) collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par))  ///
			keep(before1955) mlabels(, none) stats(N , fmt(%9.0g ) ///
			labels("\hspace{0.5 cm} Observations" ) ///
			) $slvl 

foreach x in illiterate female married {
	estout l1_`x' l2_`x' l3_`x' l4_`x' using "table4_placebo.tex", ///
			append style(tex) collabels(, none) label varlabels(before1955 "Born before 1955") cells(b(star fmt(%9.3f)) se(par))  ///
			keep(before1955) mlabels(, none) stats(N , fmt(%9.0g ) ///
			labels("\hspace{0.5 cm} Observations" ) ///
			) $slvl 
}

log close


*-----------------------------------------------------
* PS3 – Selection on Observables (Regression, Matching)
*-----------------------------------------------------

clear all
version 17
capture log close

***********************************************************
*** install packages
***********************************************************

cap ssc install nnmatch
cap ssc install ebalance
cap ssc install psmatch2
cap ssc install teffects
net install sgmediation2, from("https://tdmize.github.io/data/sgmediation2")


*************************************
**** 
**** Directory
****
*************************************

* set your working directory
global dir = "your directory" 

cd "$dir"

log using "log_PS3.log", replace

use "bowling.dta", clear


****************************************
*********
********* Variables
*********
****************************************

*Log entry rates:
gen lnNSentry_total=ln(1+NSentry_total)
label var lnNSentry_total "log total (Falter) Party entries 1925-01/33"
gen lnNSentry_FU_total=ln(1+NSentry_FU_total)
label var lnNSentry_FU_total "log total (Falter-corrected) Party entries 1925-01/33"
gen lnclubs_all = ln(clubs_all)

*Population:
gen pop2 = (pop25/1000)^2
gen pop3 = (pop25/1000)^3
gen lnpop25=ln(pop25)
label var pop2 "(pop'25)^2"
label var pop3 "(pop'25)^3"
label var lnpop25 "ln(pop) in 1925"

*Dummies for size quintiles
xtile pop25_quintiles = pop25, nq(5)
tab  pop25_quintiles, gen(d_pop_quintile)

****************************************
*********
********* Baseline (Table 3)
*********
****************************************

* Panel A
eststo clear

* Baseline controls
eststo: reg lnNSentry_total lnclubs_all lnpop25 share_cath25 bcollar25, r beta

* different dv
eststo: reg lnNSentry_FU_total lnclubs_all lnpop25 share_cath25 bcollar25, r beta

* with standardized entry
eststo: reg pcNSentry_std clubs_all_pc, r

* + controls
eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r beta

* + reduced sample (civic associations)
eststo: reg pcNSentry_std clubs_civic_pc share_cath25 lnpop25 bcollar25, r beta

* + reduced sample (military associations)
eststo: reg pcNSentry_std clubs_nonCivic_pc share_cath25 lnpop25 bcollar25, r beta

* some fine-tuning with latex manually (add tex option)
esttab using "output/table3a.tex", replace style(tex) se b star compress keep(lnclubs_all clubs_all_pc clubs_civic_pc clubs_nonCivic_pc lnpop25 share_cath25 bcollar25) title("Table 3 Panel A in Stata") mtitles(All All All All Civic Military) coeflabels(lnclubs_all ln(ASSOC_total) clubs_all_pc ASSOC clubs_civic_pc ASSOC clubs_nonCivic_pc ASSOC lnpop25 ln(population) share_cath25 "Share Catholics" bcollar25 "Share-blue collar") ar2 nonotes // tex


* Panel B:		

* additional controls
local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg"	
	
eststo clear

* Baseline controls
eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, cl(landweimar)

* + socioeconomic
eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon', cl(landweimar)

* + political
eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political', cl(landweimar)

* + state fixed effects 
eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' dummy_land*, cl(landweimar)

* + reduced sample (civic associations)
eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' dummy_land*, cl(landweimar)

* + reduced sample (military associations)
eststo: reg pcNSentry_std clubs_nonCivic_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' dummy_land*, cl(landweimar)

esttab , se beta star compress keep(clubs_all_pc clubs_civic_pc clubs_nonCivic_pc)

* some fine-tuning with latex manually (add tex option)
esttab using "output/table3b.tex", replace style(tex) se b star compress keep(clubs_all_pc clubs_civic_pc clubs_nonCivic_pc) title("Table 3 Panel B in Stata") mtitles(All All All All Civic Military) coeflabels(lnclubs_all ln(ASSOC_total) clubs_all_pc ASSOC clubs_civic_pc ASSOC clubs_nonCivic_pc ASSOC lnpop25 ln(population) share_cath25 "Share Catholics" bcollar25 "Share-blue collar") ar2 nonotes // tex

* to reproduce the exact tables like in the paper some manuel latex work is needed

****************************************
*********
********* Election Results and Mediation (Table 4)
*********
****************************************

* Panel A
eststo clear

* Reduced form (DV regressed on IV)
* 1928 elections
eststo: reg pcNSDAP285 clubs_all_pc lnpop25 share_cath25 bcollar25, r

* 1930 elections
eststo: reg pcNSDAP309 clubs_all_pc lnpop25 share_cath25 bcollar25, r

* 1933 elections
eststo: reg pcNSDAP333 clubs_all_pc lnpop25 share_cath25 bcollar25, r

* First stage (Mediator regressed on IV)
* 1925-28
eststo: reg pcNSentry_early_std clubs_all_pc lnpop25 share_cath25 bcollar25, r

* 1925-30
eststo: reg pcNSentry_pre1930_std clubs_all_pc lnpop25 share_cath25 bcollar25, r

* 1925-33
eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r

esttab , se beta star compress keep(clubs_all_pc) //view beta coefficients

* some fine-tuning with latex manually (add tex option)
esttab using "output/table4a.text", replace style(tex) se b star compress keep(clubs_all_pc) title("Table 4 Panel A in Stata") mtitles("May 1928" "September 1930" "March 1933" 1925–28 1925-30 1925-1/33) coeflabels(clubs_all_pc ASSOC_all) ar2 nonotes // tex

* Panel B
eststo clear

* DV regressed on Mediator and IV
* 1925-28
eststo: reg pcNSDAP285 pcNSentry_early_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
sum clubs_all_pc pcNSDAP285 if e(sample)

* 1925-30
eststo: reg pcNSDAP309 pcNSentry_pre1930_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
sum clubs_all_pc pcNSDAP309 if e(sample)

* 1925-33
eststo: reg pcNSDAP333 pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
sum clubs_all_pc pcNSDAP333 if e(sample)
esttab , se beta star compress keep(clubs_all_pc pcNSentry_early_std pcNSentry_pre1930_std pcNSentry_std)

* Mediation Analysis
* 1928 elections
sgmediation2 pcNSDAP285, iv(clubs_all_pc) mv(pcNSentry_early_std) cv(lnpop25 share_cath25 bcollar25)
	disp(1.571497*.48343/4.758155)

* 1930 elections
sgmediation2 pcNSDAP309, iv(clubs_all_pc) mv(pcNSentry_pre1930_std) cv(lnpop25 share_cath25 bcollar25)
	disp(1.571497*.729729/8.694133)

* 1933 elections
sgmediation2 pcNSDAP333, iv(clubs_all_pc) mv(pcNSentry_std) cv(lnpop25 share_cath25 bcollar25)
	disp(1.571497*.422989/9.829142)
	
* some fine-tuning with latex manually (add tex option)
esttab using "output/table4b.tex", replace style(tex) se b star compress keep(clubs_all_pc pcNSentry_early_std pcNSentry_pre1930_std pcNSentry_std) title("Table 4 Panel B in Stata") mtitles(1925-28 1925-30 1925-1/33) coeflabels(clubs_all_pc ASSOC_all pcNSentry_early_std "NSDAP Entry" pcNSentry_pre1930_std "NSDAP Entry" pcNSentry_std "NSDAP Entry") ar2 nonotes // tex

* results of mediation analysis have to be added manually to latex table

****************************************
*********
********* Matching (Table 10)
*********
****************************************

eststo clear
***** Note that, contrary to what the authors state in the footnote of Table 10, they are NOT actually doing propensity score matching but rather direct covariate matching! *****

* covariate matching (one neighbor)
eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25, m(1) robust(1) tc(att)

* covariate matching (nearest three neighbors)
eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25, m(3) robust(3) tc(att)

* covariate matching (nearest three neighbors)
eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25 share_cath25 bcollar25, m(3) robust(3) tc(att)

* covariate matching (nearest three neighbors)
eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude, m(3) robust(3) tc(att)

* (partial) exact matching
eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude, m(3) robust(3) tc(att) exact(pop25_quintiles landweimar_num)

* entropy reweighting  
ebalance clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude, tar(2)

eststo: reg pcNSentry_std clubs_pc_AM [pweight=_webal],r

* some fine-tuning with latex manually (add tex option)
esttab using "output/table10.tex", replace style(tex) se b star compress keep(SATT clubs_pc_AM) title("Table 10 in Stata") mtitles("Only One Neighbor" "Nearest Three Neighbors" "Nearest Three Neighbors" "Nearest Three Neighbors" "Exact Matching" "Entropy Reweighting") coeflabels(SATT "I(ASSOC_all > median)" clubs_pc_AM "I(ASSOC_all > median)") nonotes // tex



***** The same covariate matching results as above but now using the more-flexible built-in Stata teffects package *****

* covariate matching (one neighbor)
teffects nnmatch (pcNSentry_std lnpop25) (clubs_pc_AM), nn(1) vce(robust) atet 

* covariate matching (nearest three neighbors)
teffects nnmatch (pcNSentry_std lnpop25) (clubs_pc_AM), nn(3) vce(robust,nn(3)) atet 

* covariate matching (nearest three neighbors)
teffects nnmatch (pcNSentry_std lnpop25 share_cath25 bcollar25) (clubs_pc_AM), nn(3) vce(robust,nn(3)) atet 

* covariate matching (nearest three neighbors)
teffects nnmatch (pcNSentry_std lnpop25 share_cath25 bcollar25 latitude longitude) (clubs_pc_AM), nn(3) vce(robust,nn(3)) atet 


***** Propensity score matching results *****

* propensity score matching (one neighbor)
teffects psmatch (pcNSentry_std) (clubs_pc_AM lnpop25, logit), nn(1) vce(robust) atet

* propensity score matching (nearest three neighbors)
teffects psmatch (pcNSentry_std) (clubs_pc_AM lnpop25, logit), nn(3) vce(robust,nn(3)) atet

* propensity score matching (nearest three neighbors)
teffects psmatch (pcNSentry_std) (clubs_pc_AM lnpop25 share_cath25 bcollar25, logit), nn(3) vce(robust,nn(3)) atet

* propensity score matching (nearest three neighbors)
teffects psmatch (pcNSentry_std) (clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude, logit), nn(3) vce(robust,nn(3)) atet




****************************************
*********
********* Common Support
*********
****************************************

logit clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude
predict pscore

su pscore if clubs_pc_AM==1, detail
su pscore if clubs_pc_AM==0, detail

histogram pscore, by(clubs_pc_AM) binrescale xtitle("Probability of Treatment")
graph export "output/common_sup_stata.pdf",replace 
****************************************
*********
********* Balance in Covariates
*********
****************************************

teffects nnmatch (pcNSentry_std lnpop25 share_cath25 bcollar25 latitude longitude) (clubs_pc_AM), atet nn(3) metric(ivariance)

tebalance summarize
tebalance density lnpop25
tebalance density share_cath25, name(cath_density,replace)
tebalance box share_cath25, name(cath_box,replace)

graph combine cath_density cath_box
graph export "output/covbal1.pdf",replace


*-----------------------------------------------------
* PS4 – Difference-in-Differences
*-----------------------------------------------------

clear all
version 17
capture log close

****************************************************************************************************
********** Data & Packages
**********
******************************************************************************************

* install packages
ssc install bacondecomp
ssc install csdid
ssc install did_imputation
ssc install event_plot
ssc install drdid
ssc install ftools
ssc install reghdfe
ssc install coefplot

set maxvar 10000
set matsize 11000

* import dataset
global dir = "your path"

cd "$dir"

log using "log_PS4.log", replace

use "exile.dta", clear

gen log_tweets = log(num_tweets)

****************************************************************************************************
********** Figure 2: Discussion of Service Provision
**********
******************************************************************************************

* event study regression
xtset actor_id month
xtreg perc_service i.lead_lags i.month log_tweets, fe vce(cluster actor_id)

coefplot, vertical keep(*.lead_lags) yline(0, lpattern(dash)) xline(7.5, lpattern(dash)) baselevels order(2.lead_lags 3.lead_lags 4.lead_lags 5.lead_lags 6.lead_lags 7.lead_lags 1b.lead_lags) xtitle("Months Since Exile") ytitle("Change in % Tweets on Service Provision") recast(connected) graphregion(color(white)) ciopts(color(black)) lcolor(black) color(black) name(event,replace)
graph export "figure2_left_stata.pdf",replace


* panel model, within
xtset actor_id month
xtreg perc_service tweeted_exile i.month log_tweets, fe vce(cluster actor_id)

estimates store m1

* pooled
reg perc_service tweeted_exile i.month log_tweets, vce(cluster actor_id)

estimates store m2

* time trend
xtset actor_id month
xtreg perc_service tweeted_exile i.month log_tweets i.actor_id#c.trend, fe vce(cluster actor_id)

estimates store m3

coefplot (m2, label(Months FEs) pstyle(p1) msymbol(S)) (m1, label(Two-Way FEs) pstyle(p2) msymbol(T)) (m3, label(Time Trend) pstyle(p3) msymbol(O)), horizontal keep(tweeted_exile) xline(0, lpattern(dash)) xtitle("Change in % of Tweets") coeflabels(tweeted_exile = "Service Provision") graphregion(color(white)) ciopts(color(black)) lcolor(black) color(black) name(coef,replace)
graph export "figure2_right_stata.pdf",replace

****************************************************************************************************
********** Figure 4: Harsh Anti-Regime Criticism
**********
******************************************************************************************

* event study regression
xtset actor_id month
xtreg perc_harsh_criticism i.lead_lags i.month log_tweets, fe vce(cluster actor_id)

coefplot, vertical keep(*.lead_lags) yline(0, lpattern(dash)) xline(7.5, lpattern(dash)) baselevels order(2.lead_lags 3.lead_lags 4.lead_lags 5.lead_lags 6.lead_lags 7.lead_lags 1b.lead_lags) xtitle("Months Since Exile") ytitle("Change in % Tweets on Harsh Criticism") recast(connected) graphregion(color(white)) ciopts(color(black)) lcolor(black) color(black)
graph export "figure4_left_stata.pdf",replace

* panel model, within
xtset actor_id month
xtreg perc_harsh_criticism tweeted_exile i.month log_tweets, fe vce(cluster actor_id)

estimates store m4

* pooled
reg perc_harsh_criticism tweeted_exile i.month log_tweets, vce(cluster actor_id)

estimates store m5

* time trend
xtset actor_id month
xtreg perc_harsh_criticism tweeted_exile i.month log_tweets i.actor_id#c.trend, fe vce(cluster actor_id)

estimates store m6

* panel model, within
xtset actor_id month
xtreg perc_narco tweeted_exile i.month log_tweets, fe vce(cluster actor_id)

estimates store m41

* pooled
reg perc_narco tweeted_exile i.month log_tweets, vce(cluster actor_id)

estimates store m51

* time trend
xtset actor_id month
xtreg perc_narco tweeted_exile i.month log_tweets i.actor_id#c.trend, fe vce(cluster actor_id)

estimates store m61

* panel model, within
xtset actor_id month
xtreg perc_dictator tweeted_exile i.month log_tweets, fe vce(cluster actor_id)

estimates store m42

* pooled
reg perc_dictator tweeted_exile i.month log_tweets, vce(cluster actor_id)

estimates store m52

* time trend
xtset actor_id month
xtreg perc_dictator tweeted_exile i.month log_tweets i.actor_id#c.trend, fe vce(cluster actor_id)

estimates store m62

* panel model, within
xtset actor_id month
xtreg perc_cuba tweeted_exile i.month log_tweets, fe vce(cluster actor_id)

estimates store m43

* pooled
reg perc_cuba tweeted_exile i.month log_tweets, vce(cluster actor_id)

estimates store m53

* time trend
xtset actor_id month
xtreg perc_cuba tweeted_exile i.month log_tweets i.actor_id#c.trend, fe vce(cluster actor_id)

estimates store m63

coefplot (m5, label(Months FEs) pstyle(p1) msymbol(S)) (m4, label(Two-Way FEs) pstyle(p2) msymbol(T)) (m6, label(Time Trend) pstyle(p3) msymbol(O)), bylabel(Harsh Criticism) || (m51, label(Months FEs) pstyle(p1) msymbol(S)) (m41, label(Two-Way FEs) pstyle(p2) msymbol(T)) (m61, label(Time Trend) pstyle(p3) msymbol(O)), bylabel(Narco-State) || (m52, label(Months FEs) pstyle(p1) msymbol(S)) (m42, label(Two-Way FEs) pstyle(p2) msymbol(T)) (m62, label(Time Trend) pstyle(p3) msymbol(O)), bylabel(Dictator) || (m53, label(Months FEs) pstyle(p1) msymbol(S)) (m43, label(Two-Way FEs) pstyle(p2) msymbol(T)) (m63, label(Time Trend) pstyle(p3) msymbol(O)), bylabel(Cuban/Russian Influence) ||, horizontal keep(tweeted_exile) xline(0, lpattern(dash)) xtitle("Change in % of Tweets") coeflabels(tweeted_exile = " ") graphregion(color(white)) ciopts(color(black)) lcolor(black) color(black)
graph export "figure4_right_stata.pdf",replace

****************************************************************************************************
********** Callaway and Sant'Anna (2021)
**********
******************************************************************************************

gen start_date = date("2013-01-01", "YMD")
format start_date %td

generate first_treat = datediff(start_date, month_of_exile, "month")
replace first_treat = 0 if missing(first_treat)

generate month_num = datediff(start_date, month_format, "month")

csdid perc_service, ivar(actor_id) time(month_num) gvar(first_treat) long2 // this will take a while to run... if you want it to run faster, reduce the number of groups; results are going to change but at least you will be able to see what the output looks like 
eststo cs_service

* retrieve the ATT and event study coefficients
estat all
estat event, estore(cs_service_event)
estat simple, estore(cs_service_att)

* plot CS event study
event_plot cs_service_event, default_look graph_opt(xtitle("Event Time") ytitle("Average Effect") title("Callaway & Sant'Anna")) stub_lag(Tp#) stub_lead(Tm#)
graph export "callsant_event_stata.pdf",replace

csdid perc_harsh_criticism, ivar(actor_id) time(month_num) gvar(first_treat) long2
eststo cs_harsh

* retrieve the ATT and event study coefficients
estat all
estat event, estore(cs_harsh_event)
estat simple, estore(cs_harsh_att)
* plot CS event study
event_plot cs_harsh, default_look graph_opt(xtitle("Event Time") ytitle("Average Effect") title("Callaway & Sant'Anna")) stub_lag(Tp#) stub_lead(Tm#)


coefplot (cs_service_att, label(Service Provision) msymbol(S)) (cs_harsh_att, label(Harsh Criticism)), horizontal xline(0, lpattern(dash)) xtitle("Change in % of Tweets") coeflabels(tweeted_exile = " ") graphregion(color(white)) ciopts(color(black)) lcolor(black) color(black)
graph export "callsant_coef_stata.pdf",replace

****************************************************************************************************
********** Borusyak, Jaravel, Spiess (2021)
**********
******************************************************************************************

replace first_treat = . if first_treat == 0

did_imputation perc_service actor_id month_num first_treat , delta(1) autosample horizons(0/10) pretrends(6) 
estimates store didimp

event_plot didimp, plottype("scatter") ciplottype("rcap") together  graph_opt(xtitle("Event Time") ytitle("Borusyak, Jaravel, and Spiess (2021) Estimator") xline(-0.5, lcolor(gs8) lpattern(dash)) yline(0, lcolor(gs8)) graphregion(color(white)) bgcolor(white) ylabel(, angle(horizontal))) lag_opt(color(navy)) lead_opt(color(maroon) msymbol(S)) lag_ci_opt(color(navy%45 navy%45)) lead_ci_opt(color(maroon%45 maroon%45)) legend_opt(region(lstyle(none)))
 
graph export "bjs_stata.pdf",replace
****************************************************************************************************
********** Goodman-Bacon Decomposition
**********
******************************************************************************************

* balanced panel needed for decomposition
use "balanced_panel.dta", clear

* we want the data in a format that shows units treated or not at different points in time --> group by actor, year, and treatment
collapse (mean) perc_service, by(actor_id year tweeted_exile)

* only uniqe observations should be kept
bysort year actor_id: keep if _n == 1

* exclude people for which 2013 is post treatment
gen zero2013 = (year == 2013 & tweeted_exile == 1)
tab zero2013
egen actor_drop1 = max(zero2013), by(actor_id)
drop if actor_drop1 == 1

* run bacondecomp
xtset actor_id year
bacondecomp perc_service tweeted_exile,ddetail

graph export "goodman_bacon_service_stata.pdf",replace


*-----------------------------------------------------
* PS5 – Synthetic Control Methods
*-----------------------------------------------------

****************************************
*********
********* Dataset & Packages
*********
****************************************

clear all
global dir = "your path"

cd "$dir"

log using "log_PS5.log", replace

use "smoking.dta", clear

* synthetic control packages
net install synth_runner, from(https://raw.github.com/bquistorff/synth_runner/master/) replace
net from "https://web.stanford.edu/~jhain/Synth"
net install synth, all replace force

****************************************
*********
********* Figure 1
*********
****************************************

decode state, generate(state_str)

gen cali = cond(state_str == "California", "California", "Rest of the U.S.")

bysort cali year: egen cig_avg = mean(cigsale)

twoway  line cig_avg year if cali == "California", lpattern(full) ||line cig_avg year if cali == "Rest of the U.S.", lpattern(dash) ||, xtitle("Year") ytitle("Per capita cigarette sale (in packs)") legend(label(1 "California") label(2 "Rest of the U.S.")) xline(1988) scheme(s1mono)

graph export "figures/figure1_stata.pdf", replace 

****************************************
*********
********* Generate weights for table
*********
****************************************

tsset state year

synth cigsale cigsale(1988) cigsale(1980) cigsale(1975) lnincome(1972(1)1988) retprice age15to24 beer(1984(1)1988), trunit(3) trperiod(1989) margin(0.005) sigf(5) bound(10) nested

****************************************
*********
********* Figure 2 & 3
*********
****************************************

tsset state year

synth_runner cigsale beer(1984(1)1988) lnincome(1972(1)1988) retprice age15to24 cigsale(1988) cigsale(1980) cigsale(1975), trunit(3) trperiod(1989) gen_vars

effect_graphs, trlinediff(-1)

graph export "figures/figure2_stata.pdf",replace name(tc)
graph export "figures/figure3_stata.pdf",replace name(effect)
pval_graphs

****************************************
*********
********* Figure 4, 5, 6, and 7
*********
****************************************

tsset state year
forval i = 1/39 {
		di "Running sc for state `i'"
       qui synth cigsale beer(1984(1)1988) lnincome(1972(1)1988) retprice age15to24 cigsale(1988) cigsale(1980) cigsale(1975), trunit(`i') trperiod(1989) keep(data/synth`i'.dta) replace
}

forval i = 1/39 {

use data/synth`i', clear

rename _time years
rename _Co_Number state

gen tr_effect_`i' = _Y_treated - _Y_synthetic
egen mspe_`i' = mean(tr_effect^2) if years < 1989
gen rmspe_`i' = sqrt(mspe)

egen mspe1988_`i'=mean(tr_effect^2) if years==1988
gen rmspe1988_`i'=sqrt(mspe1988_`i')
egen aux_`i'=mean(rmspe1988_`i')

egen pmspe_`i' = mean(tr_effect^2) if years > 1988 & years <= 2000
gen prmspe_`i' = sqrt(pmspe)

gen pmspe_corr`i'=pmspe_`i'/aux_`i'

keep state years tr_effect_`i' mspe_`i' rmspe_`i' pmspe_`i' prmspe_`i' pmspe_corr`i'

save data/synth`i', replace

}

use data/synth1, clear

forval i = 2/39 {

qui merge 1:1 state using data/synth`i', nogenerate

}

gen p=. // compute (non standardized) p-value by hand
sum prmspe_3
local target=`r(mean)'
local units
local p=0
forval i = 1/39 {
   capture qui summarize prmspe_`i', meanonly
   if r(mean) > `target' {
      local units `units', `i'
	  local p=`p'+1/39
   }
   qui: replace p=`p'
}
sum p


*** for all observations 

local lp

forval i = 1/39 {
   local lp `lp' line tr_effect_`i' years, lcolor(gs12) ||
}

twoway `lp' || line tr_effect_3 years, lcolor(black)  legend(off) xline(1989, lpattern(dash)) xtitle("Year") ytitle("Gap in per capita cigarette sale (in packs)") title("Full Placebo Test", size(small))

graph export "figures/figure4_stata.pdf", replace 


*** for observations pre-RMSPE < x20 Californias pre-RMSPE 

local lp

local threshold = 20*rmspe_3

forval i = 1/39 {
   capture qui summarize rmspe_`i', meanonly
   if r(mean) < `threshold' {
      local lp `lp' line tr_effect_`i' years, lcolor(gs12) ||
   }
}

twoway `lp' || line tr_effect_3 years, lcolor(black) lw(thick) legend(off) xline(1989, lpattern(dash)) xtitle("Year") ytitle("Gap in per capita cigarette sale (in packs)") title("Discards states with pre-RMSPE >= x20 California)", size(small))

graph export "figures/figure5_stata.pdf", replace 


*** for observations pre-RMSPE < x5 Californias pre-RMSPE

local lp

local threshold = 5*rmspe_3

forval i = 1/39 {
   capture qui summarize rmspe_`i', meanonly
   if r(mean) < `threshold' {
      local lp `lp' line tr_effect_`i' years, lcolor(gs12) ||
   }
}

twoway `lp' || line tr_effect_3 years, lcolor(black) lw(thick) legend(off) xline(1989, lpattern(dash)) xtitle("Year") ytitle("Gap in per capita cigarette sale (in packs)") title("Discards states with pre-RMSPE >= x5 California)", size(small))

graph export "figures/figure6_stata.pdf", replace 

*** for observations pre-RMSPE < x2 Californias pre-RMSPE

local lp

local threshold = 2*rmspe_3

forval i = 1/39 {
   capture qui summarize rmspe_`i', meanonly
   if r(mean) < `threshold' {
      local lp `lp' line tr_effect_`i' years, lcolor(gs12) ||
   }
}

twoway `lp' || line tr_effect_3 years, lcolor(black) lw(thick) legend(off) xline(1989, lpattern(dash)) xtitle("Year") ytitle("Gap in per capita cigarette sale (in packs)") title("Discards states with pre-RMSPE >= x2 California)", size(small))

graph export "figures/figure7_stata.pdf", replace 

****************************************
*********
********* PRE/POST MSPE Ratio
*********
****************************************

forval i = 1/39 {
	summarize mspe_`i', meanonly
    local var1_mean = r(mean)
    
    summarize pmspe_`i', meanonly
    local var2_mean = r(mean)
    gen ratio_`i' = `var2_mean' / `var1_mean'
}

gen rank = .

forval i = 1/39 {
    replace rank = ratio_`i'[1] if _n == `i'
}

gen p_std=1/39 // calculating standardized p-value by hand

graph hbar rank, over(state, sort(1) descending label(labsize(tiny))) legend(off) title("Ratio of post-Proposition 99 MSPE and pre-Proposition 99", size(small)) scheme(s1mono) ytitle("Post vs. Pre-MSPE", size(small))

graph export "figures/ratio_stata.pdf", replace 

*** THE END
log close


