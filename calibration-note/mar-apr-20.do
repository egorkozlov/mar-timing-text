clear
set more off
pause on

cap log close

//log using log_all, replace
// load things
cd /projects/p30190/data
local rawdata "usa_00019.dta"

if 0 {
// compute ACS labor income paths
use `rawdata'
//keep if sex == 1
gen income_ok = (incwage >= 2000  & incwage ~= . & empstat == 1)
gen income_h_ok = (incwage >= 2000  & incwage ~= . & empstat == 1 & uhrswork >= 10 & (wkswork2 == 6 | wkswork2 == 5) )

gen income_ok_sp = (incwage_sp >= 2000  & incwage_sp ~= . & empstat_sp == 1 & marst==1)
gen income_h_ok_sp = (incwage_sp >= 2000  & incwage_sp ~= . & empstat_sp == 1 & uhrswork_sp >= 10 & wrklstwk_sp == 2 & marst==1)


keep if age > 20
gen T = age - 25
gen T2 = T^2
gen T3 = T^3


//gen weeks_mid_year = 0.5*(1+13)*(wkswork2==1) + 0.5*(4+26)*(wkswork2==2) + 0.5*(27+39)*(wkswork2==3) +0.5*(40+47)*(wkswork2==4) + 0.5*(48+49)*(wkswork2==5) + 0.5*(50+52)*(wkswork2==6) if wkswork2 ~= 0
gen weeks_mid_year = 50

gen hrs_year = uhrswork*weeks_mid_year if uhrswork >= 10 
gen hrs_year_sp = uhrswork_sp*weeks_mid_year if uhrswork_sp>=10

gen low_educ = (educ<=6)
gen med_educ = (educ>6 & educ<10)
gen high_educ = (educ>=10)

gen educ_group = 1*low_educ + 2*med_educ + 3*high_educ



gen lincome = log(incwage) if income_ok
gen lincome_ph = log(incwage) - log(hrs_year) if income_h_ok

gen lincome_sp = log(incwage_sp) if income_ok_sp
gen lincome_ph_sp = log(incwage_sp) - log(hrs_year_sp) if income_h_ok_sp

gen has_own = (lincome_ph ~= .)
gen has_sp = (lincome_ph_sp ~= .)

tab has_own has_sp if age == 25 & high_educ & sex == 2


//winsor2 lincome_ph, trim s(_trim) cut(5 95) by(sex)
//winsor2 lincome_ph_sp, trim s(_trim) cut(5 95) by(sex_sp)


gen lincome_ph_trim = .
gen lincome_ph_sp_trim = .

foreach cond in "sex == 1" "sex == 2" {
qui sum lincome_ph [aw=perwt] if `cond', de
replace lincome_ph_trim = lincome_ph if `cond' & lincome_ph >= r(p5) & lincome_ph <= r(p95)
qui sum lincome_ph_sp [aw=perwt] if `cond', de
replace lincome_ph_sp_trim = lincome_ph_sp if `cond' & lincome_ph_sp >= r(p5) & lincome_ph_sp <= r(p95)
}


reg lincome_ph_trim lincome_ph_sp_trim [pw=perwt] if high_educ==1 & sex==2 & age==25
reg lincome_ph_trim lincome_ph_sp_trim [pw=perwt] if high_educ==1 & sex==2 & age==40


gen log_sp_diff = lincome_ph_sp_trim - lincome_ph_trim if lincome_ph_sp_trim ~= . & lincome_ph_trim ~= .


gen yr_1m = yrmarr  if ((marst == 1 | marst == 4) & marrno == 1)
gen yaftmar = year - yr_1m if yr_1m ~=.
sum log_sp_diff [aw=perwt] if sex==2 & high_educ==1 & yaftmar==1


local incvar "lincome_ph_sp"
qui sum `incvar' [aw=perwt] if age==25 & sex==1 & high_educ==1, de
scalar g_25 = r(p50)
gen above_median_25 = (`incvar'>=g_25) if `incvar' ~= .
qui sum `incvar' [aw=perwt] if age==30 & sex==1 & high_educ==1, de
scalar g_30 = r(p50)
gen above_median_30 = (`incvar'>=g_30) if `incvar' ~= .

cap gen any_child = (nchild>0) if nchild~=.
cap gen any_mar = (marrno > 0) if marrno~=.

reg any_child above_median_25 [pw=perwt] if age==25 & sex==1 & high_educ==1
reg any_child above_median_30 [pw=perwt] if age==30 & sex==1 & high_educ==1

gen inlf = (labforce==2)
reg inlf above_median_30 [pw=perwt] if age==30 & sex==2 & high_educ==1 & any_child==1
exit
reg uhrswork_sp above_median_30 [pw=perwt] if age==30 & sex==1 & high_educ==1 & any_child==1

exit


mean uhrswork [aw=perwt] if sex==2 & high_educ & marst==1 & empstat==1 & any_child==0
mean uhrswork [aw=perwt] if sex==2 & high_educ & marst==1 & empstat==1 & any_child==1


mean uhrswork [aw=perwt] if sex==2 & low_educ & marst==1 & empstat==1 & any_child==0
mean uhrswork [aw=perwt] if sex==2 & low_educ & marst==1 & empstat==1 & any_child==1


gen outlf = (empstat==3)
mean outlf [aw=perwt] if any_child==1 & age==30 & sex==2 & high_educ==1 & marst==1
reg outlf above_median_30 [pw=perwt] if any_child==1 & age==30 & sex==2 & high_educ==1 & marst==1

// constants by education

keep if low_educ | high_educ
gen male = (sex == 1)

gen h_male = high_educ*male
gen h_male_T = h_male*T
gen h_male_T2 = h_male*T2
gen h_male_T3 = h_male*T3
gen h_female = high_educ*(1-male)
gen h_female_T = h_female*T
gen h_female_T2 = h_female*T2
gen h_female_T3 = h_female*T3
gen l_male = low_educ*male
gen l_male_T = l_male*T
gen l_male_T2 = l_male*T2
gen l_male_T3 = l_male*T3
gen l_female = low_educ*(1-male)
gen l_female_T = l_female*T
gen l_female_T2 = l_female*T2
gen l_female_T3 = l_female*T3

//variances


foreach cond in "h_male==1" "h_female==1" "l_male==1" "l_female==1" {
di "Condition is `cond'"

local a1 = 24
local a2 = 30
qui reghdfe lincome_ph_trim [pw=perwt] if age==`a1' & `cond', a(statefip year)
qui scalar s_`a1' = e(rmse)
di "Age `a1', `cond': " s_`a1'
qui reghdfe lincome_ph_trim [pw=perwt] if age==`a2' & `cond', a(statefip year)
qui scalar s_`a2' = e(rmse)
di "Age `a2', `cond': " s_`a2'
scalar se_imp = ( (s_`a2'^2 - s_`a1'^2)/(`a2'-`a1') )^0.5
di "Implied standard deviation if `cond' is " se_imp
scalar se_init = (s_`a1'^2 - (`a1'-21)*se_imp^2)^0.5
di "Implied initial if `cond' is " se_init
}

exit

drop h_female
//reghdfe lincome_ph h_male l_male l_female [pw=perwt] if income_ok & age == 25, a(statefip year)
reghdfe lincome_ph_trim h_fem* h_mal* l_fem* l_mal* [pw=perwt] if income_h_ok & (low_educ==1 | high_educ==1) , a(statefip year)
reghdfe lincome_ph_sp_trim T T2 T3 [pw=perwt] if income_h_ok_sp & (sex==2 & high_educ==1)
exit

//keep if educ <= 6
//keep if high_educ


reghdfe lincome T T2 T3 [pw=perwt] if income_ok & sex == 1, a(statefip year)
exit
reghdfe lincome T T2 T3 [pw=perwt] if income_ok & sex == 2, a(statefip year)



reghdfe lincome_ph T T2 T3 [pw=perwt] if income_h_ok & sex == 1, a(statefip year)
reghdfe lincome_ph T T2 T3 [pw=perwt] if income_h_ok & sex == 2, a(statefip year)


// constant
reghdfe lincome_ph male [pw=perwt] if income_ok & age == 25, a(statefip year)


// variances

reghdfe lincome_ph [pw=perwt] if income_ok & male & age==25, a(statefip year)
reghdfe lincome_ph [pw=perwt] if income_ok & male & age==40, a(statefip year)
reghdfe lincome_ph [pw=perwt] if income_ok & ~male & age==25, a(statefip year)
reghdfe lincome_ph [pw=perwt] if income_ok & ~male & age==40, a(statefip year)



exit

}


// this generates income groupings, see a separate file
if 0 {
clear
do mar-may-generate.do
}

if 1 {
use all_plus_quatniles_multi.dta, clear

gen inc_group = inc_group_base

replace eldch = . if eldch == 99 | (age - eldch < 14)
gen yr_1m = yrmarr  if ((marst == 1 | marst == 4) & marrno == 1)
gen yr_1k = year - eldch

gen dT = yr_1k - yr_1m
replace dT = . if dT > 10
replace dT = . if dT < -5
gen k_m = (dT<=0) if dT ~=.

//keep if marrno==1

gen div_mar1 = (marst==4) if ((marst == 1 | marst == 4) & marrno == 1)


gen educ_gr = 1*(educ<=6) + 2*(educ>6 & educ<10) + 3*(educ>=10)
/*
gen div_mar1_col = div_mar1 if educ_gr==3
gen div_mar1_hs = div_mar1 if educ_gr==1
gen div_mar1_y5 = div_mar1 if year - yr_1m==5
gen div_mar1_past25 = div_mar1 if age - (year-yr_1k) >= 25


keep if dT<=5
collapse (mean) div_mar* (sum) c = perwt [aw=perwt], by(dT)
twoway (scatter div_mar1_y5 dT) if dT<=5
exit



exit
*/

gen age_diff = age_sp - age if age_sp ~= .
gen age_1k = yr_1k - birthyr
gen age_1m = yr_1m - birthyr
gen yaftmar = year - yr_1m

gen col = (educ>= 10) if educ~=.
gen col_h = (educ_sp>=10) if educ_sp~=.
gen inlf = (labforce==2)
gen emp = (empstat==1)

gen income_emp = incwage if emp
gen hours_emp = uhrswork if emp

gen white = (race==1)
gen black = (race==2)

//gen age2 = age^2
gen age3 = age^3
gen age4 = age^4

gen never_mar = (marrno==0)
gen no_kids = (nchild==0)
gen one_mar = (marrno==1)
gen more_than_one_mar = (marrno>1)
gen divsep = (marst == 3 | marst == 4) if marst ~= 6 & marst != .
gen div = (marst == 4) if marst ~= 6 & marst != .
gen div_1mar = (marst == 4) if marst ~= 6 & marst != . & (marrno==1)
gen no_kids_mar = (nchild==0) if marst==1

gen just_mar = (marrinyr == 2) & (marrno==1)
gen just_mar_kid = (nchild>=1) & (marrinyr == 2) & (marrno==1)
gen just_mar_kid_t0 = (fertyr==2) & (marrinyr==2) & (marrno==1)


gen div_kids = (div==1) & (nchild>=1) if div~=. & nchild~=.
gen div_nokids = (div==1) & (nchild==0) if div~=. & nchild~=.
gen never_mar_kids = (never_mar==1) & (nchild>=1) if nchild ~= .
gen mar_remar_kids = (nchild>=1) if (marrno>=1)

gen fertyr_mar = (fertyr == 2) & (marrinyr ==2) & (marrno==1)
gen just_sec_mar = (marrinyr==2) & (marrno==2)
gen just_sec_mar_kid = (marrinyr==2) & (marrno==2) & (nchild>=1)

gen just_div_kids = (divinyr==2) & (nchild>0)
gen just_div = (divinyr==2)


//foreach cond in "1" "educ>=10" "(educ<=6)" {
//preserve
//keep if educ>=10

cap gen educ_gr = 1*(educ<=6) + 2*(educ>6 & educ<10) + 3*(educ>=10)
/*
keep age never_mar just_mar just_mar_kid fertyr_mar perwt educ_gr
collapse (mean) just_mar_kid just_mar never_mar fertyr_mar [aw=perwt], by(age educ_gr)

xtset educ_gr age
gen haz_km = just_mar_kid/L.never_mar
gen haz_jkm = fertyr_mar/L.never_mar
gen share_km = just_mar_kid/just_mar
gen share_jkm = fertyr_mar/just_mar
drop if educ_gr == 2
drop if age>35

//twoway (connected haz_km age if educ_gr==3) (connected haz_km age if educ_gr==1) if age<=35, ytitle(Hazard) xtitle(Age (female)) title(Hazards of Marriage + Baby) subtitle(at the same year of before) legend(on order(1 "College" 2 "High School"))
twoway (connected haz_jkm age if educ_gr==3) (connected haz_jkm age if educ_gr==1) if age<=35, ytitle(Hazard) xtitle(Age (female)) title(Hazards of KF-Marriage) subtitle(baby at the same year) legend(on order(1 "College" 2 "High School"))
//twoway (connected share_km age if educ_gr==3) (connected share_km age if educ_gr==1) if age<=35, ytitle(Hazard) xtitle(Age (female)) title(Share of New KF-Marriages )  subtitle(kids at the same year or before) legend(on order(1 "College" 2 "High School"))
twoway (connected share_jkm age if educ_gr==3) (connected share_jkm age if educ_gr==1) if age<=35, ytitle(Hazard) xtitle(Age (female)) title(Share of New KF-Marriages ) subtitle(baby at the same year) legend(on order(1 "College" 2 "High School"))
*/

/*
di "Condition is `cond'"
forval age = 22/35 { 
local age_prev = `age' - 1
qui sum never_mar [aw=perwt] if age==`age_prev' & `cond'
local unm = r(mean)
qui sum just_mar_kid [aw=perwt] if age==`age' & `cond'
local shr = r(mean) / `unm'
di "at age = `age' the share is `shr'"
}
*/
//restore
//}

gen age_m30 = age - 30
gen age_m30_sq = (age-30)^2
sum age
gen ever_kids = (no_kids==0) if no_kids~=.
gen yaftmar_sq = yaftmar^2
gen yaftmar_km = yaftmar*k_m

exit
if 1 {
foreach  cond in "(educ>=10)" { //"(educ<=6)"  {
di "`cond'"
reg never_mar age_m30 age_m30_sq [pw=perwt] if `cond'
reg ever_kids age_m30 age_m30_sq [pw=perwt] if `cond'
reg div_1mar age_m30 age_m30_sq [pw=perwt] if `cond'
reg k_m age_m30 age_m30_sq [pw=perwt] if `cond'
reg ever_kids yaftmar yaftmar_sq [pw=perwt] if `cond' & yaftmar>=1 & yaftmar<=10 & marrno==1 & marst == 1
mean ever_kids [aw=perwt] if `cond' & yaftmar==1 & marrno==1 & marst==1
mean ever_kids [aw=perwt] if `cond' & yaftmar==2 & marrno==1 & marst==1
mean ever_kids [aw=perwt] if `cond' & yaftmar==3 & marrno==1 & marst==1
mean ever_kids [aw=perwt] if `cond' & yaftmar==4 & marrno==1 & marst==1
mean ever_kids [aw=perwt] if `cond' & yaftmar==5 & marrno==1 & marst==1
mean ever_kids [aw=perwt] if `cond' & yaftmar==6 & marrno==1 & marst==1
reg div_1mar yaftmar yaftmar_sq [pw=perwt] if `cond' & yaftmar>=1 & yaftmar<=10
mean div_1mar [aw=perwt] if `cond' & yaftmar==1 
mean div_1mar [aw=perwt] if `cond' & yaftmar==2 
mean div_1mar [aw=perwt] if `cond' & yaftmar==3 
mean div_1mar [aw=perwt] if `cond' & yaftmar==4 
mean div_1mar [aw=perwt] if `cond' & yaftmar==5 
mean div_1mar [aw=perwt] if `cond' & yaftmar==6 
reg div_1mar k_m [pw=perwt] if `cond'
nlcom (_b[_cons] + _b[k_m])
reg uhrswork ever_kids [pw=perwt] if `cond' & age == 30 & marst==1 & marrno==1
nlcom ((_b[_cons] + _b[ever_kids])/(_b[_cons]))
mean div_kids [aw=perwt] if `cond' & age==30
mean div_nokids [aw=perwt] if `cond' & age==30
reg just_mar_kid_t0 age_m30 [aw=perwt] if `cond' & just_mar==1
mean just_mar_kids_t0 [aw=perwt] if `cond' & just_mar==1 & age==25
mean just_mar_kids_t0 [aw=perwt] if `cond' & just_mar==1 & age==30
mean just_mar_kids_t0 [aw=perwt] if `cond' & just_mar==1 & age==35
mean never_mar_kids [aw=perwt] if `cond' & age==30
mean more_than_one_mar [aw=perwt] if `cond' & age==40
mean just_div_kids [aw=perwt] if `cond' & just_div==1 & age>=25 & age<=35
}
}

 

if 0 {
// educ >= 10

local ages = "(age==25 | age==30 | age==35 | age==40)"
foreach  cond in "(educ<=6)" "(educ>=10)"  {
di "`cond'"
tabstat never_mar [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat one_mar [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat divsep [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat div [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat div_1mar [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat no_kids [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat no_kids_mar [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat no_kids_mar [aw = perwt] if `cond' & marrno ==1 & (yaftmar<=4), statistics( mean ) by(yaftmar)
tabstat k_m [aw = perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat just_mar [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat just_mar_kid [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat just_mar [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat just_mar_kid [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat never_mar_kids [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat div_nokids [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat div_kids [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)
tabstat more_than_one_mar [aw=perwt] if `cond' & `ages', statistics( mean ) by(age)

}
}


if 0 {
// moments old


mean k_m [aw=perwt] if age == 25
mean k_m [aw=perwt] if age == 35



exit
if 1 {
foreach vname in "never_mar" "no_kids" "one_mar" "div_mar1" {
di "At 25:"
mean `vname' [aw=perwt] if age == 25
di "At 30, low:"
mean `vname' [aw=perwt] if inc_group == 1 & age == 30
di "At 30, high:"
mean `vname' [aw=perwt] if inc_group == 3 & age == 30
di "At 35"
mean `vname' [aw=perwt] if age == 35
di "At 40"
mean `vname' [aw=perwt] if age == 40
}
}


mean div_mar1 [aw=perwt] if (k_m==1) & (age==40)
mean div_mar1 [aw=perwt] if (k_m==0) & (age==40)
mean div_mar1 [aw=perwt] if (k_m==1) & (age==40) & (inc_group==1)
mean div_mar1 [aw=perwt] if (k_m==0) & (age==40) & (inc_group==1)
mean div_mar1 [aw=perwt] if (k_m==1) & (age==40) & (inc_group==3)
mean div_mar1 [aw=perwt] if (k_m==0) & (age==40) & (inc_group==3)


mean div_mar1 [aw=perwt] if (k_m==1) 
mean div_mar1 [aw=perwt] if (k_m==0) 
mean div_mar1 [aw=perwt] if (k_m==1) & (inc_group==1)
mean div_mar1 [aw=perwt] if (k_m==0) & (inc_group==1)
mean div_mar1 [aw=perwt] if (k_m==1) & (inc_group==3)
mean div_mar1 [aw=perwt] if (k_m==0) & (inc_group==3)

gen never_married_with_kids = (nchild > 0 & marrno == 0)
mean never_married_with_kids if age == 30

gen divorced_with_kids = (nchild > 0 & marst == 4)
mean divorced_with_kids if age == 30


exit

}



// this takes a while
if 1 { 

disp "Hi I am ready"
exit
foreach cond_text in "1" "age>=35 & age<=40" "educ_group==1" "educ_group==3 | educ_group == 4" "inc_group==1" "inc_group==2" "inc_group==3"  {

drop income_ok_fem
drop income_fem_base
gen income_ok_fem = (incwage >= 1000 & uhrswork >= 5 & empstat == 1 & wkswork2 >= 3)
gen income_fem_base = log(incwage) - log(hrs_year) if income_ok_fem


gen linc = income_fem_base

gen linc_p1 = linc
gen linc_p2 = linc^2
gen linc_p3 = linc^3
gen linc_p4 = linc^4

di "`cond_text'"

gen cond_i = (`cond_text')

mean k_m [aw=perwt] if cond_i

table k_m [aw=perwt] if cond_i, contents(mean div_mar1)


mean(div_mar1) [aw=perwt] if cond_i



reg div_mar1 k_m [pw=perwt] if cond_i, robust
reghdfe div_mar1 k_m age age2 age3 age4 [pw=perwt] if cond_i, a(year statefip nchild yaftmar eldch race educ_group year)
reghdfe div_mar1 k_m age age2 age3 age4 linc_p* [pw=perwt] if cond_i, a(year statefip nchild yaftmar eldch race educ_group year)

drop cond_i


}



cap log close
}





// composition comparison
if 0 {
foreach vname in "age" "age_1m" "age_1k" "age_sp" "age_diff" "eldch" "nchild" "col" "col_h" "inlf" "income_emp" "hours_emp" "white" "black" {

qui sum `vname'
local g = r(mean)

if (`g' < 1) {
qui gen t_tmp_`vname' = 100*`vname'
local vname "t_tmp_`vname'"
}

local fmt "%5.1f"

quietly {
gen t_u = "`vname'"
gen t_all_`vname'  = `vname'
gen  t_sg_`vname'  = `vname' if (k_m ~= .)
gen  t_km_`vname'  = `vname' if (k_m == 1)
gen  t_mk_`vname'  = `vname' if (k_m == 0)
}

quietly mean t_all_`vname'


table t_u [aw=perwt], contents(mean t_all mean t_sg mean t_km mean t_mk) format(`fmt')

drop t_*

cap log close
}

}
//table inc_group k_m [aw=perwt], contents(mean div_mar1)

//table educ_group k_m [aw=perwt], contents(mean div_mar1)

}

