****************************************
*Model preparation (using individual-person data, so most of this cannot be repeated off the sercure server hosting individual-person data). 
****************************************

{

*NDSS
{
use "G:/NDSS data/NDSS cleaned.dta", clear
keep if diabetes_type == 2
keep aihw sex diabetes_type_ndss regdate-censdate90 diabetes_type
save "G:/Jed/Equity model/Data/NDSS.dta", replace
}

*Diabetes prevalence in 2020 by age, sex, and SEIFA. 
{
use "G:/Jed/Equity model/Data/NDSS.dta", clear
keep if dod > td(31,12,2019)

merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge

drop if exitry2!=. | (exitry!=. & entry2==.)
drop if exitry!=. & entry2==.

merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
*N=5,863 for whom no SEIFA information
keep if _merge == 3
drop _merge ARIA

gen age = (td(31,12,2019)-dob)/365.25
replace age = round(age,1)

gen N = 1
replace age = 100 if age > 100

collapse (sum) N, by(age sex SEIFA)

*Convert to total Aus
replace N = N*1.25

drop if age >= 120

save "G:/Jed/Equity model/Data/DMprevpop2020.dta", replace

*Sanity checks
su(N), detail
di r(sum)
bysort SEIFA : egen A = sum(N)
ta A SEIFA
bysort SEIFA (age sex) : egen Aa = sum(N) if age < 40
bysort SEIFA (age sex) : egen Ba = sum(N) if age > 90
}

*Genpop prevalence in 2020 by age, sex, and SEIFA. 
{
use "G:/Jed/Modelling/Data/AUSpop.dta", clear
gen prev2020 = (yr2019+yr2020)/10
drop if sex == 0
keep age sex prev2020
expand 5
bysort age sex : gen SEIFA = _n
*Again, assuming age-specific SEIFA equivalent. 
save "G:/Jed/Equity model/Data/genpop 2020.dta", replace
}

*Total prevalence in 2020 by diabetes status, age, sex, and SEIFA
{
use "G:/Jed/Equity model/Data/genpop 2020.dta", clear
merge 1:1 age sex SEIFA using "G:/Jed/Equity model/Data/DMprevpop2020.dta"
drop _merge
replace N = 0 if N==.
gen noDM = prev2020-N
rename N DMN

expand 2
bysort age sex SEIFA : gen DM = _n-1
gen N = noDM if DM == 0
replace N = DMN if DM == 1
keep age sex SEIFA DM N

save "G:/Jed/Equity model/Data/prevpop2020.dta", replace
}

*Diabetes incidence rate in 2019 by age, sex, and SEIFA.
{

forval i = 2015/2019 {
*Denominator
*DM prevalence
use "G:/Jed/Modelling/Data/NDSS.dta", clear
merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge
drop if (exitry < td(30,6,`i') & entry2 > td(30,6,`i')) | (exitry2 < td(30,6,`i'))
keep if dod > td(30,6,`i') & regdate <= td(30,6,`i')
merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
keep if _merge == 3
drop _merge ARIA
gen age = (td(30,6,`i')-dob)/365.25
replace age = round(age,1)
replace age = 100 if age > 100
gen N = 1
collapse (sum) N, by(age sex SEIFA)
save "G:/Jed/Equity model/Data/DM prevalence `i'.dta", replace


use "G:/Jed/Modelling/Data/4STpop.dta", clear
drop if sex == 0
keep age sex yr`i'

*assume evenly spread across age - not necessarily safe assumption. Come back to. 

replace yr`i' = yr`i'/5
expand 5
bysort age sex : gen SEIFA = _n

merge 1:1 age sex SEIFA using "G:/Jed/Equity model/Data/DM prevalence `i'.dta"
drop _merge
drop if age < 10
replace N = 0 if N==.

gen denom = yr`i'-N

keep age sex SEIFA denom

save "G:/Jed/Equity model/Data/Denominator `i'.dta", replace



*Numerator
use "G:/Jed/Equity model/Data/NDSS.dta", clear
merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge
keep if inrange(regdate, entry-91.3, exitry+91.3) | (inrange(regdate,entry2-91.3, exitry2+91.3) & entry2 !=.)
keep if year(regdate)==`i'

merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
*N=298 for whom no SEIFA information (<1%, no effect)
keep if _merge == 3
drop _merge ARIA

gen age = (td(30,6,`i')-dob)/365.25
replace age = round(age,1)
replace age = 100 if age > 100

gen N = 1

collapse (sum) N, by(age sex SEIFA)

save "G:/Jed/Equity model/Data/Numerator `i'.dta", replace
}


clear
gen year =.
forval i = 2015/2019 {
append using "G:/Jed/Equity model/Data/Denominator `i'.dta"
recode year .=`i'
}
save "G:/Jed/Equity model/Data/Denominator.dta", replace

clear
gen year =.
forval i = 2015/2019 {
append using "G:/Jed/Equity model/Data/Numerator `i'.dta"
recode year .=`i'
}
save "G:/Jed/Equity model/Data/Numerator.dta", replace


*Predicted incidence
use "G:/Jed/Equity model/Data/Denominator.dta", clear
merge 1:1 age sex SEIFA year using "G:/Jed/Equity model/Data/Numerator.dta"
drop _merge
replace N = 0 if missing(N)
replace age = age + 0.5
mkspline agesp = age, cubic knots(10(10)90)

forval i = 1/5 {
forval ii = 1/2 {
poisson N agesp* year if SEIFA == `i' & sex == `ii', exposure(denom)
predict rate_`i'_`ii' if SEIFA == `i' & sex == `ii', ir
predict errr_`i'_`ii' if SEIFA == `i' & sex == `ii', stdp
}
}

gen rate =.
gen errr=.
forval i = 1/5 {
forval ii = 1/2 {
replace rate = rate_`i'_`ii' if SEIFA == `i' & sex == `ii'
replace errr = errr_`i'_`ii' if SEIFA == `i' & sex == `ii'

}
}

drop rate_1_1-errr_5_2

replace age = age - 0.5 

keep if year == 2019

keep age sex SEIFA rate errr year

gen DM = 0

save "G:/Jed/Equity model/Data/DMINC.dta", replace


*Figure
{
use "G:/Jed/Equity model/Data/DMINC.dta", clear
replace rate = rate*1000
gen lb = exp(ln(rate)-1.96*errr)
gen ub = exp(ln(rate)+1.96*errr)
drop errr


twoway ///
(line rate lb ub age if sex == 1 & SEIFA == 1, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 2, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 3, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 4, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 5, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off)

twoway ///
(line rate lb ub age if sex == 2 & SEIFA == 1, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 2, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 3, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 4, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 5, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off)


}


}

*Diabetes mortality rate in 2019 by age, sex, and SEIFA.
{
forval i = 2015/2019 {
use "G:/Jed/Modelling/Data/NDSS.dta", clear
merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge
drop if (exitry < td(30,6,`i') & entry2 > td(30,6,`i')) | (exitry2 < td(30,6,`i'))
keep if inrange(dod,td(1,1,`i'),td(31,12,`i'))
merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
keep if _merge == 3
drop _merge ARIA
gen age = (td(30,6,`i')-dob)/365.25
replace age = round(age,1)
replace age = 100 if age > 100
gen N = 1
collapse (sum) N, by(age sex SEIFA)
rename N deaths
gen year = `i'
save "G:/Jed/Equity model/Data/DM deaths `i'.dta", replace
}

clear 
gen year =.
forval i = 2015/2019 {
append using "G:/Jed/Equity model/Data/DM prevalence `i'.dta"
recode year .=`i'
}
save "G:/Jed/Equity model/Data/DM prevalence.dta", replace

clear 
gen year =.
forval i = 2015/2019 {
append using "G:/Jed/Equity model/Data/DM deaths `i'.dta"
recode year .=`i'
}
save "G:/Jed/Equity model/Data/DM deaths.dta", replace



clear
set obs 91
gen age = _n+9
expand 2
bysort age : gen sex = _n
expand 5
bysort age sex : gen SEIFA = _n
expand 5
bysort age sex SEIFA : gen year = _n+2014
merge 1:1 age sex SEIFA year using "G:/Jed/Equity model/Data/DM prevalence.dta"
replace N = 0.1 if missing(N)
drop _merge
merge 1:1 age sex SEIFA year using "G:/Jed/Equity model/Data/DM deaths.dta"
replace deaths = 0 if missing(deaths)
drop _merge

replace age = age + 0.5
mkspline agesp = age, cubic knots(0 30(10)90)

forval i = 1/5 {
forval ii = 1/2 {
poisson deaths agesp* year if SEIFA == `i' & sex == `ii', exposure(N)
predict rate_`i'_`ii' if SEIFA == `i' & sex == `ii', ir
predict errr_`i'_`ii' if SEIFA == `i' & sex == `ii', stdp
}
}

gen rate =.
gen errr=.
forval i = 1/5 {
forval ii = 1/2 {
replace rate = rate_`i'_`ii' if SEIFA == `i' & sex == `ii'
replace errr = errr_`i'_`ii' if SEIFA == `i' & sex == `ii'

}
}

drop rate_1_1-errr_5_2
replace age = age - 0.5 
keep if year == 2019
keep age sex SEIFA rate errr
gen DM = 1
save "G:/Jed/Equity model/Data/DMMORT.dta", replace

*Figure
{
use "G:/Jed/Equity model/Data/DMMORT.dta", clear
replace rate = rate*1000
gen lb = exp(ln(rate)-1.96*errr)
gen ub = exp(ln(rate)+1.96*errr)
drop errr


twoway ///
(line rate lb ub age if sex == 1 & SEIFA == 1, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 2, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 3, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 4, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 5, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off)

twoway ///
(line rate lb ub age if sex == 2 & SEIFA == 1, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 2, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 3, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 4, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 5, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off)



twoway ///
(line rate lb ub age if sex == 1 & SEIFA == 1 & age >=60, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 2 & age >=60, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 3 & age >=60, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 4 & age >=60, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 5 & age >=60, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off) yscale(log)

twoway ///
(line rate lb ub age if sex == 2 & SEIFA == 1 & age >=60, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 2 & age >=60, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 3 & age >=60, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 4 & age >=60, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 5 & age >=60, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off) yscale(log)


}


}

*Non-diabetes mortality in 2019, by age, sex, and SEIFA. 
{

use "G:/Jed/Equity model/Data/DM prevalence 2019.dta", clear

merge 1:1 age sex SEIFA using "G:/Jed/Equity model/Data/DM deaths 2019.dta"
drop _merge
replace deaths = 0 if deaths==.
tostring age, gen(A)
replace A = substr(A,2,1)
replace age = age-1 if A == "1" | A == "6"
replace age = age-2 if A == "2" | A == "7"
replace age = age-3 if A == "3" | A == "8"
replace age = age-4 if A == "4" | A == "9"
replace age = 85 if age > 85

collapse (sum) N deaths, by(age sex SEIFA)

*Convert to total pop size
replace N = N*1.25
replace deaths = deaths*1.25

rename N DMN
rename deaths DMdeaths

save "G:/Jed/Equity model/Data/DM deaths N 2019.dta", replace

*Sourced form AIHW GRIMBOOK ACM
use "G:/Jed/Equity model/Data/genpopdeaths2019.dta", clear
expand 5
bysort age sex : gen SEIFA = _n

replace deaths = deaths/5
replace N = N/5
*Assuming even SEIFA across age groups

gen rate = deaths/N
*Assuming applies evenly across age groups
gen adjrate =.
replace adjrate = rate*0.796614 if SEIFA == 1
replace adjrate = rate*0.888758 if SEIFA == 2
replace adjrate = rate*0.973026 if SEIFA == 3
replace adjrate = rate*1.090569 if SEIFA == 4
replace adjrate = rate*1.210475 if SEIFA == 5

gen adjdeaths = N*adjrate



merge 1:1 age sex SEIFA using "G:/Jed/Equity model/Data/DM deaths N 2019.dta"
drop _merge
replace DMN = 0 if DMN==.
replace DMdeaths=0 if DMdeaths==.

gen noDMN = N-DMN
gen noDMdeaths = adjdeaths-DMdeaths

replace age = age + 7 if age == 85
replace age = age + 0.5


forval i = 1/5 {
forval ii = 1/2 {
preserve 
poisson noDMdeaths age if SEIFA == `i' & sex == `ii', exposure(noDMN)
clear 
set obs 101
gen age = _n-0.5
gen sex = `ii'
gen SEIFA = `i'
gen noDMN = 1
predict rate, ir
predict errr, stdp

drop noDMN
replace age = age - 0.5 



save "G:/Jed/Equity model/Data/noDMMR019_`i'_`ii'.dta", replace
restore
}
}


clear
forval i = 1/5 {
forval ii = 1/2 {
append using "G:/Jed/Equity model/Data/noDMMR019_`i'_`ii'.dta"
}
}

gen DM = 0
save "G:/Jed/Equity model/Data/NODMMORT.dta", replace


*Figure
{
use "G:/Jed/Equity model/Data/NODMMORT.dta", clear
replace rate = rate*1000
gen lb = exp(ln(rate)-1.96*errr)
gen ub = exp(ln(rate)+1.96*errr)
drop errr


twoway ///
(line rate lb ub age if sex == 1 & SEIFA == 1, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 2, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 3, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 4, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 1 & SEIFA == 5, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off)

twoway ///
(line rate lb ub age if sex == 2 & SEIFA == 1, color(gs0 gs0 gs0) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 2, color(gs3 gs3 gs3) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 3, color(gs6 gs6 gs6) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 4, color(gs9 gs9 gs9) lpattern(solid dot dot)) ///
(line rate lb ub age if sex == 2 & SEIFA == 5, color(gs12 gs12 gs12) lpattern(solid dot dot)) ///
, legend(off)

}


*Check against DM
use "G:/Jed/Equity model/Data/NODMMORT.dta", clear
rename rate nodmrate
drop errr
merge 1:1 age sex SEIFA using "G:/Jed/Equity model/Data/DMMORT.dta"
drop _merge errr

gen ratioo = rate/nodmrate
gen A = 1

twoway ///
(line ratioo age if sex == 2 & SEIFA == 1, color(gs0)) ///
(line ratioo age if sex == 2 & SEIFA == 2, color(gs3)) ///
(line ratioo age if sex == 2 & SEIFA == 3, color(gs6)) ///
(line ratioo age if sex == 2 & SEIFA == 4, color(gs9)) ///
(line ratioo age if sex == 2 & SEIFA == 5, color(gs12)) ///
(line A age, lpattern(dot)) ///
, legend(off) yscale(log) ylabel(1 2 5 10 20 50)


*Total for easy merge
use "G:/Jed/Equity model/Data/NODMMORT.dta", clear
append using "G:/Jed/Equity model/Data/DMMORT.dta"
save "G:/Jed/Equity model/Data/AllMORT.dta", replace


}

*Births
{
use "G:/Jed/Modelling/Data/Series B projections.dta", clear
keep if age == 0
drop if sex == 0
save "G:/Jed/Equity model/Data/Births.dta", replace

forval i = 2020/2040 {
use "G:/Jed/Equity model/Data/Births.dta", clear
keep age sex yr`i'
rename yr`i' N
expand 5
replace N = N/5
bysort age sex : gen SEIFA = _n
expand 2
bysort age sex SEIFA : gen DM = _n-1
replace N = 0 if DM == 1
save "G:/Jed/Equity model/Data/Births `i'.dta", replace
}

}

*Diabetes prevalence for costing
{
use "G:/Jed/Equity model/Data/DM prevalence 2019.dta", clear

tostring age, gen(A)
replace A = substr(A,2,1)
replace age = age-1 if A == "1" | A == "6"
replace age = age-2 if A == "2" | A == "7"
replace age = age-3 if A == "3" | A == "8"
replace age = age-4 if A == "4" | A == "9"
replace age = 85 if age > 85

collapse (sum) N, by(age sex)
*Convert to total pop size
replace N = N*1.25
rename N DMN

merge 1:1 age sex using "G:/Jed/Equity model/Data/genpopdeaths2019.dta"
drop _merge deaths
sort sex age
replace DMN = 0 if missing(DMN)

gen prev = DMN/N

format DMN %9.0f

su DMN
di r(sum)

}

*Utilities matrix
{

clear
set obs 101
gen age = _n-1
expand 2
bysort age : gen sex = _n
expand 5 
bysort age sex : gen SEIFA = _n
expand 2
bysort age sex SEIFA : gen DM = _n-1

gen UT=.
replace UT = 0.96 if inrange(age,0,24) & sex == 1
replace UT = 0.95 if inrange(age,25,34) & sex == 1
replace UT = 0.93 if inrange(age,35,44) & sex == 1
replace UT = 0.9 if inrange(age,45,54) & sex == 1
replace UT = 0.9 if inrange(age,55,64) & sex == 1
replace UT = 0.87 if inrange(age,65,74) & sex == 1
replace UT = 0.85 if inrange(age,75,100) & sex == 1

replace UT = 0.95 if inrange(age,0,24) & sex == 2
replace UT = 0.95 if inrange(age,25,34) & sex == 2
replace UT = 0.91 if inrange(age,35,44) & sex == 2
replace UT = 0.87 if inrange(age,45,54) & sex == 2
replace UT = 0.88 if inrange(age,55,64) & sex == 2
replace UT = 0.87 if inrange(age,65,74) & sex == 2
replace UT = 0.82 if inrange(age,75,100) & sex == 2

replace UT=UT*0.785 if DM == 1

save "G:/Jed/Equity model/Data/Utilities.dta", replace
*Also replace UT=UT*0.785 if DM == 1 as alternative
*Alternative
*replace UT = 0.785 if DM == 1
*replace UT=UT*1.103 if inrange(age,0,24) & DM == 1
*replace UT=UT*1.092 if inrange(age,25,34) & DM == 1
*replace UT=UT*1.069 if inrange(age,35,44) & DM == 1
*replace UT=UT*1.034 if inrange(age,45,54) & DM == 1
*replace UT=UT*1.034 if inrange(age,55,64) & DM == 1
*replace UT=UT*0.977 if inrange(age,75,100) & DM == 1



}

*Cost matrix
{
clear
set obs 101
gen age = _n-1
expand 2
bysort age : gen sex = _n
expand 5 
bysort age sex : gen SEIFA = _n
expand 2
bysort age sex SEIFA : gen DM = _n-1

***THESE COSTS COME FROM PUBLICLY AVAILABLE DATA FROM THE AIHW

gen hccost=.
replace hccost = 3942 if inrange(age,0,4) & sex == 1 & DM == 0
replace hccost = 1438 if inrange(age,5,9) & sex == 1 & DM == 0
replace hccost = 1473 if inrange(age,10,14) & sex == 1 & DM == 0
replace hccost = 1760 if inrange(age,15,19) & sex == 1 & DM == 0
replace hccost = 1755 if inrange(age,20,24) & sex == 1 & DM == 0
replace hccost = 1853 if inrange(age,25,29) & sex == 1 & DM == 0
replace hccost = 2137 if inrange(age,30,34) & sex == 1 & DM == 0
replace hccost = 2589 if inrange(age,35,39) & sex == 1 & DM == 0
replace hccost = 3064 if inrange(age,40,44) & sex == 1 & DM == 0
replace hccost = 3649 if inrange(age,45,49) & sex == 1 & DM == 0
replace hccost = 4505 if inrange(age,50,54) & sex == 1 & DM == 0
replace hccost = 5514 if inrange(age,55,59) & sex == 1 & DM == 0
replace hccost = 7027 if inrange(age,60,64) & sex == 1 & DM == 0
replace hccost = 8853 if inrange(age,65,69) & sex == 1 & DM == 0
replace hccost = 10821 if inrange(age,70,74) & sex == 1 & DM == 0
replace hccost = 13111 if inrange(age,75,79) & sex == 1 & DM == 0
replace hccost = 15199 if inrange(age,80,84) & sex == 1 & DM == 0
replace hccost = 16481 if inrange(age,85,100) & sex == 1 & DM == 0

replace hccost = 3358 if inrange(age,0,4) & sex == 2 & DM == 0
replace hccost = 1168 if inrange(age,5,9) & sex == 2 & DM == 0
replace hccost = 1365 if inrange(age,10,14) & sex == 2 & DM == 0
replace hccost = 2421 if inrange(age,15,19) & sex == 2 & DM == 0
replace hccost = 3014 if inrange(age,20,24) & sex == 2 & DM == 0
replace hccost = 3937 if inrange(age,25,29) & sex == 2 & DM == 0
replace hccost = 4989 if inrange(age,30,34) & sex == 2 & DM == 0
replace hccost = 4687 if inrange(age,35,39) & sex == 2 & DM == 0
replace hccost = 4218 if inrange(age,40,44) & sex == 2 & DM == 0
replace hccost = 4292 if inrange(age,45,49) & sex == 2 & DM == 0
replace hccost = 4719 if inrange(age,50,54) & sex == 2 & DM == 0
replace hccost = 5337 if inrange(age,55,59) & sex == 2 & DM == 0
replace hccost = 6271 if inrange(age,60,64) & sex == 2 & DM == 0
replace hccost = 7692 if inrange(age,65,69) & sex == 2 & DM == 0
replace hccost = 9407 if inrange(age,70,74) & sex == 2 & DM == 0
replace hccost = 11225 if inrange(age,75,79) & sex == 2 & DM == 0
replace hccost = 12779 if inrange(age,80,84) & sex == 2 & DM == 0
replace hccost = 13745 if inrange(age,85,100) & sex == 2 & DM == 0

bysort age sex SEIFA (DM) : replace hccost = hccost[_n-1]*1.6 if DM[_n-1]==0

save "G:/Jed/Equity model/Data/hccost.dta", replace




}

*Workforce participation matrix
{
clear
set obs 101
gen age = _n-1
expand 2
bysort age : gen sex = _n
expand 5 
bysort age sex : gen SEIFA = _n
expand 2
bysort age sex SEIFA : gen DM = _n-1

***THESE VALUES ARE THE WORKFORCE PARTICIPATION AND UNEMPLOYMENT PROPORTIONS FROM PUBLICLY AVAILABLE DATA FROM THE AUSTRALIAN BUREAU OF STATISTICS


gen WP=.
replace WP = 0.536474579 if sex == 1 & inrange(age,15,19)
replace WP = 0.827875563 if sex == 1 & inrange(age,20,24)
replace WP = 0.900484309 if sex == 1 & inrange(age,25,29)
replace WP = 0.913761702 if sex == 1 & inrange(age,30,34)
replace WP = 0.926136852 if sex == 1 & inrange(age,35,39)
replace WP = 0.907449022 if sex == 1 & inrange(age,40,44)
replace WP = 0.897113171 if sex == 1 & inrange(age,45,49)
replace WP = 0.873837147 if sex == 1 & inrange(age,50,54)
replace WP = 0.789899179 if sex == 1 & inrange(age,55,59)
replace WP = 0.644344635 if sex == 1 & inrange(age,60,67)
replace WP = 0.600615970 if sex == 2 & inrange(age,15,19)
replace WP = 0.807503198 if sex == 2 & inrange(age,20,24)
replace WP = 0.818268178 if sex == 2 & inrange(age,25,29)
replace WP = 0.773345517 if sex == 2 & inrange(age,30,34)
replace WP = 0.766066221 if sex == 2 & inrange(age,35,39)
replace WP = 0.793035416 if sex == 2 & inrange(age,40,44)
replace WP = 0.803592841 if sex == 2 & inrange(age,45,49)
replace WP = 0.761692756 if sex == 2 & inrange(age,50,54)
replace WP = 0.695354177 if sex == 2 & inrange(age,55,59)
replace WP = 0.507805357 if sex == 2 & inrange(age,60,67)

gen UM=.
replace UM = 0.229813081 if sex == 1 & inrange(age,15,19)
replace UM = 0.100285878 if sex == 1 & inrange(age,20,24)
replace UM = 0.059074220 if sex == 1 & inrange(age,25,29)
replace UM = 0.036598822 if sex == 1 & inrange(age,30,34)
replace UM = 0.033976955 if sex == 1 & inrange(age,35,39)
replace UM = 0.040268199 if sex == 1 & inrange(age,40,44)
replace UM = 0.035587286 if sex == 1 & inrange(age,45,49)
replace UM = 0.041954338 if sex == 1 & inrange(age,50,54)
replace UM = 0.047681851 if sex == 1 & inrange(age,55,59)
replace UM = 0.042147636 if sex == 1 & inrange(age,60,67)
replace UM = 0.170246396 if sex == 2 & inrange(age,15,19)
replace UM = 0.090742394 if sex == 2 & inrange(age,20,24)
replace UM = 0.054268546 if sex == 2 & inrange(age,25,29)
replace UM = 0.053758484 if sex == 2 & inrange(age,30,34)
replace UM = 0.039200976 if sex == 2 & inrange(age,35,39)
replace UM = 0.037183168 if sex == 2 & inrange(age,40,44)
replace UM = 0.034615628 if sex == 2 & inrange(age,45,49)
replace UM = 0.041812272 if sex == 2 & inrange(age,50,54)
replace UM = 0.039326261 if sex == 2 & inrange(age,55,59)
replace UM = 0.043420309 if sex == 2 & inrange(age,60,67)

gen EM=WP*(1-UM)

gen EMD=.
replace EMD=(WP-0.0391)*(1-UM) if sex == 1 & inrange(age,15,49)
replace EMD=(WP-0.0370)*(1-UM) if sex == 2 & inrange(age,15,49)
replace EMD=(WP-0.1147)*(1-UM) if sex == 1 & inrange(age,50,67)
replace EMD=(WP-0.0020)*(1-UM) if sex == 2 & inrange(age,50,67)

drop WP UM

save "G:/Jed/Equity model/Data/WAP.dta", replace

}

*Training population table
{

*Prevalent 
{
use "G:/Jed/Equity model/Data/NDSS.dta", clear
merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge

keep if regdate <= td(31,12,2014)
keep if dod > td(31,12,2014)
keep if (entry <= td(31,12,2014) & exitry>td(31,12,2014)) | (exitry!=. & entry2 <= td(31,12,2014))

merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
*N=5,307 for whom no SEIFA information
keep if _merge == 3
drop _merge ARIA

gen age = (td(31,12,2014)-dob)/365.25
gen agec = 90
replace agec = 80 if age < 90
replace agec = 70 if age < 80
replace agec = 60 if age < 70
replace agec = 50 if age < 60
replace agec = 40 if age < 50
replace agec = 30 if age < 40
replace agec = 20 if age < 30
replace agec = 10 if age < 20

ta SEIFA, matcell(A)
matrix A = A'
count
matrix A = (A,r(N))
ta sex SEIFA, matcell(B)
ta sex, matcell(C)
matrix B = (B,C)
matrix A = (A\B)
ta agec SEIFA, matcell(B)
ta agec, matcell(C)
matrix B = (B,C)
matrix A = (A\B)

clear
svmat double A
gen njm = _n
drop if njm == 3
replace njm = _n

forval i = 1/6 {
gen ref`i' = A`i'[1]
gen perc`i' = 100*A`i'/ref`i'
tostring A`i', force format(%9.0f) replace
tostring perc`i', force format(%9.1f) replace
replace A`i' = A`i' + " (" + perc`i' + "%)" if njm > 1
}
keep A1-A6
save "G:/Jed/Equity model/Data/Prevtable1.dta", replace
}

*Incident
{
use "G:/Jed/Equity model/Data/NDSS.dta", clear
merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge
keep if inrange(regdate, entry-91.3, exitry+91.3) | (inrange(regdate,entry2-91.3, exitry2+91.3) & entry2 !=.)
keep if regdate > td(31,12,2014)

merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
*N=1,365 for whom no SEIFA information
keep if _merge == 3
drop _merge ARIA

gen age = (regdate-dob)/365.25
gen agec = 90
replace agec = 80 if age < 90
replace agec = 70 if age < 80
replace agec = 60 if age < 70
replace agec = 50 if age < 60
replace agec = 40 if age < 50
replace agec = 30 if age < 40
replace agec = 20 if age < 30
replace agec = 10 if age < 20

ta SEIFA, matcell(A)
matrix A = A'
count
matrix A = (A,r(N))
ta sex SEIFA, matcell(B)
ta sex, matcell(C)
matrix B = (B,C)
matrix A = (A\B)
ta agec SEIFA, matcell(B)
ta agec, matcell(C)
matrix B = (B,C)
matrix A = (A\B)

clear
svmat double A
gen njm = _n
drop if njm == 3
replace njm = _n

forval i = 1/6 {
gen ref`i' = A`i'[1]
gen perc`i' = 100*A`i'/ref`i'
tostring A`i', force format(%9.0f) replace
tostring perc`i', force format(%9.1f) replace
replace A`i' = A`i' + " (" + perc`i' + "%)" if njm > 1
}
keep A1-A6
save "G:/Jed/Equity model/Data/Inctable1.dta", replace
}
clear 
append using "G:/Jed/Equity model/Data/Prevtable1.dta"
append using "G:/Jed/Equity model/Data/Inctable1.dta"
br



*Text report
{

use "G:/Jed/Equity model/Data/NDSS.dta", clear
merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge

keep if regdate <= td(31,12,2014)
keep if dod > td(31,12,2014)
keep if (entry <= td(31,12,2014) & exitry>td(31,12,2014)) | (exitry!=. & entry2 <= td(31,12,2014))

merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
*N=5,307 for whom no SEIFA information
keep if _merge == 3
drop _merge ARIA

gen age = (td(31,12,2014)-dob)/365.25

keep aihw age sex
save "G:/Jed/Equity model/Data/Sensient.dta", replace


use "G:/Jed/Equity model/Data/NDSS.dta", clear
merge 1:1 aihw using "G:/Jed/Modelling/Data/Exitry.dta"
keep if _merge == 3
drop _merge
keep if inrange(regdate, entry-91.3, exitry+91.3) | (inrange(regdate,entry2-91.3, exitry2+91.3) & entry2 !=.)
keep if regdate > td(31,12,2014)

merge 1:1 aihw using "G:/Jed/Trends in GLD use/Datasets/PPN IRSD PC.dta"
*N=1,365 for whom no SEIFA information
keep if _merge == 3
drop _merge ARIA

gen age = (regdate-dob)/365.25
keep aihw age sex
append using "G:/Jed/Equity model/Data/Sensient.dta"
su(age), detail

ta sex



}



}

*Baseline population table
{
use "G:/Jed/Equity model/Data/prevpop2020.dta", clear

forval q = 0/1 {

forval i = 1/5 {
su(N) if SEIFA == `i' & DM == `q'
matrix A`i' = r(sum)
su(N) if sex == 1 & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,0,9.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,10,19.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,20,29.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,30,39.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,40,49.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,50,59.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,60,69.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,70,79.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,80,89.9) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
su(N) if inrange(age,90,1000) & SEIFA == `i' & DM == `q'
matrix A`i' = (A`i'\r(sum))
}

su(N) if DM == `q'
matrix A6 = r(sum)
su(N) if sex == 1 & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,0,9.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,10,19.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,20,29.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,30,39.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,40,49.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,50,59.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,60,69.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,70,79.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,80,89.9) & DM == `q'
matrix A6 = (A6\r(sum))
su(N) if inrange(age,90,1000) & DM == `q'
matrix A6 = (A6\r(sum))

matrix A`q' = (A1,A2,A3,A4,A5,A6)
}
matrix A = (A0\A1)
clear
svmat double A
tostring A1-A6, force format(%9.0f) replace
}

*Rates by IRSD Figures
{
use "G:/Jed/Equity model/Data/DMINC.dta", clear
replace rate = rate*1000
gen lb = exp(ln(rate)-1.96*errr)
gen ub = exp(ln(rate)+1.96*errr)
drop errr

twoway ///
(rarea ub lb age if sex == 1 & SEIFA == 1, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 1, color("0 0 90") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 2, color("0 60 120%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 2, color("0 60 120") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 3, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 3, color("0 90 140") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 4, color("0 120 160%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 4, color("0 120 160") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 5, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 5, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(10 "1 (most disadvantaged)" 8 "2" 6 "3" 4 "4" 2 "5 (least disadvantaged)") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
ytitle("Incidence rate per 1,000 persons") ///
xtitle("Age") ///
ylab(0 2 4 6 8 10 12 14) xlab(10(10)100) ///
text(15 3 "Type 2 diabetes incidence - Males", color(gs0) placement(east))
graph save "Graph" "G:/Jed/Equity model/Data/MINCFIG.gph", replace


twoway ///
(rarea ub lb age if sex == 2 & SEIFA == 1, color("90 0 20%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 1, color("90 0 20") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 2, color("130 0 40%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 2, color("130 0 40") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 3, color("170 0 60%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 3, color("170 0 60") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 4, color("210 0 80%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 4, color("210 0 80") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 5, color("250 0 100%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 5, color("250 0 100") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(10 "1 (most disadvantaged)" 8 "2" 6 "3" 4 "4" 2 "5 (least disadvantaged)") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
ytitle("Incidence rate per 1,000 persons") ///
xtitle("Age") ///
ylab(0 2 4 6 8 10 12 14) xlab(10(10)100) ///
text(15 3 "Type 2 diabetes incidence - Females", color(gs0) placement(east))
graph save "Graph" "G:/Jed/Equity model/Data/FINCFIG.gph", replace


use "G:/Jed/Equity model/Data/DMMORT.dta", clear
keep if age >=40
replace rate = rate*1000
gen lb = exp(ln(rate)-1.96*errr)
gen ub = exp(ln(rate)+1.96*errr)
drop errr


twoway ///
(rarea ub lb age if sex == 1 & SEIFA == 1, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 1, color("0 0 90") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 2, color("0 60 120%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 2, color("0 60 120") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 3, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 3, color("0 90 140") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 4, color("0 120 160%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 4, color("0 120 160") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 5, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 5, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(10 "1 (most disadvantaged)" 8 "2" 6 "3" 4 "4" 2 "5 (least disadvantaged)") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
ytitle("Mortality rate per 1,000 persons") ///
xtitle("Age") ///
xlab(40(10)100) yscale(log) ylab(1 2 5 10 20 50 100 200 500) ///
text(750 35 "Mortality among males with type 2 diabetes", color(gs0) placement(east))
graph save "Graph" "G:/Jed/Equity model/Data/MMORFIG.gph", replace

twoway ///
(rarea ub lb age if sex == 2 & SEIFA == 1, color("90 0 20%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 1, color("90 0 20") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 2, color("130 0 40%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 2, color("130 0 40") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 3, color("170 0 60%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 3, color("170 0 60") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 4, color("210 0 80%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 4, color("210 0 80") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 5, color("250 0 100%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 5, color("250 0 100") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(10 "1 (most disadvantaged)" 8 "2" 6 "3" 4 "4" 2 "5 (least disadvantaged)") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
ytitle("Mortality rate per 1,000 persons") ///
xtitle("Age") ///
xlab(40(10)100) yscale(log) ylab(1 2 5 10 20 50 100 200 500) ///
text(750 35 "Mortality among females with type 2 diabetes", color(gs0) placement(east))
graph save "Graph" "G:/Jed/Equity model/Data/FMORFIG.gph", replace


graph combine "G:/Jed/Equity model/Data/MINCFIG.gph" ///
"G:/Jed/Equity model/Data/FINCFIG.gph" ///
"G:/Jed/Equity model/Data/MMORFIG.gph" ///
"G:/Jed/Equity model/Data/FMORFIG.gph" ///
, iscale(0.4) rows(2) xsize(5) graphregion(color(white))
graph export "G:/Jed/Equity model/Results/Diabetes incidence and mortality.pdf", as(pdf) name("Graph") replace


use "G:/Jed/Equity model/Data/NODMMORT.dta", clear
keep if age >=40
replace rate = rate*1000
gen lb = exp(ln(rate)-1.96*errr)
gen ub = exp(ln(rate)+1.96*errr)
drop errr

twoway ///
(rarea ub lb age if sex == 1 & SEIFA == 1, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 1, color("0 0 90") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 2, color("0 60 120%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 2, color("0 60 120") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 3, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 3, color("0 90 140") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 4, color("0 120 160%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 4, color("0 120 160") lpattern(solid)) ///
(rarea lb ub age if sex == 1 & SEIFA == 5, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 1 & SEIFA == 5, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(10 "1 (most disadvantaged)" 8 "2" 6 "3" 4 "4" 2 "5 (least disadvantaged)") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
ytitle("Mortality rate per 1,000 persons") ///
xtitle("Age") ///
xlab(40(10)100) yscale(log range(0.5 500)) ylab(0.5 1 2 5 10 20 50 100 200 500) ///
text(750 35 "Mortality among males without type 2 diabetes", color(gs0) placement(east))
graph save "Graph" "G:/Jed/Equity model/Data/MMORnoDMFIG.gph", replace

twoway ///
(rarea ub lb age if sex == 2 & SEIFA == 1, color("90 0 20%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 1, color("90 0 20") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 2, color("130 0 40%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 2, color("130 0 40") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 3, color("170 0 60%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 3, color("170 0 60") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 4, color("210 0 80%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 4, color("210 0 80") lpattern(solid)) ///
(rarea lb ub age if sex == 2 & SEIFA == 5, color("250 0 100%30") fintensity(inten80) lwidth(none)) ///
(line rate age if sex == 2 & SEIFA == 5, color("250 0 100") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(10 "1 (most disadvantaged)" 8 "2" 6 "3" 4 "4" 2 "5 (least disadvantaged)") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
ytitle("Mortality rate per 1,000 persons") ///
xtitle("Age") ///
xlab(40(10)100) yscale(log range(0.5 500)) ylab(0.5 1 2 5 10 20 50 100 200 500) ///
text(750 35 "Mortality among females without type 2 diabetes", color(gs0) placement(east))
graph save "Graph" "G:/Jed/Equity model/Data/FMORnoDMFIG.gph", replace


graph combine ///
"G:/Jed/Equity model/Data/MMORnoDMFIG.gph" ///
"G:/Jed/Equity model/Data/FMORnoDMFIG.gph" ///
, iscale(0.9) rows(1) xsize(10) graphregion(color(white))
graph export "G:/Jed/Equity model/Results/Non-diabetes mortality.pdf", as(pdf) name("Graph") replace



}

}



****************************************
*Everything before this was done on a secure server with individual-person data. What follows is the model using only 4 final datasets generated above. 
****************************************

cd "/Users/jed/Documents/Equity model"

set seed 28980

*The model
{




*Due to privacy, counts <6 cannot be shown, we fill them in randomly. 
use prevpop2020, clear

replace N = runiformint(1,6) if N==.

save prevpop2020, replace

*The Falconist
{

set rmsg on

tempname atuin

postfile `atuin' double(sim RR year SEIFA INC DTN DTD DTH YLN YLD YOL QLY HCN HCD HCC NPA1 NPD1 ABS1 NPA2 NPD2 ABS2) using THEBLACKPLOT, replace

quietly {


forval qq = 1/1000 {
noisily di `qq'
local sim = `qq'


local a1 = rnormal()
local a2 = rnormal()
local a3 = rnormal()
local a4 = rbeta(1789.58,74.57)
local a5 = rbeta(1014.36,53.39)
local a6 = rbeta(999.71,75.25)
local a7 = rbeta(780.62,86.74)
local a8 = rbeta(895.89,99.54)
local a9 = rbeta(587.21,87.74)
local a10 = rbeta(439.42,77.55)
local a11 = rbeta(1592.53,83.82)
local a12 = rbeta(834.42,43.92)
local a13 = rbeta(1061.90,105.02)
local a14 = rbeta(971.57,145.18)
local a15 = rbeta(932.53,127.16)
local a16 = rbeta(740.95,110.72)
local a17 = rbeta(655.44,143.88)
local a18 = rbeta(46.27,12.67)
local a19 = rgamma(61.47,85.35)

*If uncertainty too small, move up
forval q = 0/99 {
local RR=`q'


use prevpop2020, clear

expand 2
bysort sex age SEIFA DM : gen nj = _n
replace N = 0 if nj == 2

forval i = 2020/2029 {
local year = `i'

*Migration (just assume migration evenly distributed by age for now; and that DM prevalence for migrants matches AUSPOP)
su(N) if nj == 1
replace N = N*(1+(225000/r(sum))) if nj == 1

*Incident diabetes
*Incidence occurs before death - no one is at risk for death twice in cycle. 
merge m:1 age sex SEIFA DM using DMINC
replace rate = exp(ln(rate)+`a1'*errr)
if `q' < 10 {
replace rate = rate*(1-(0.0`q'))
}
else {
replace rate = rate*(1-(0.`q'))
}
drop errr _merge
gen TP = 1-exp(-rate)
replace TP = 0 if nj == 2
gen DMINC = TP*N
gen YLL = N
bysort nj age sex SEIFA (DM) : replace N = N+DMINC[_n-1] if DM==1 & DMINC[_n-1]!=. & nj == 1
bysort nj age sex SEIFA (DM) : replace YLL = YLL + 0.5*DMINC[_n-1] if DM==1 & DMINC[_n-1]!=. & nj == 1
replace N = N-DMINC if DM==0 & DMINC!=.
replace YLL = YLL - 0.5*DMINC if DM==0 & DMINC!=.
drop rate TP

*Death
merge m:1 age sex SEIFA DM using AllMORT
replace rate = exp(ln(rate)+`a2'*errr) if DM == 0
replace rate = exp(ln(rate)+`a3'*errr) if DM == 1
drop errr _merge
gen TP = 1-exp(-rate)
replace TP = 0 if nj == 2
gen Death = TP*N
replace N = N-Death if Death!=.
replace YLL = YLL - 0.5*Death if Death !=.
drop rate TP

*Non-participation due to death
bysort sex age SEIFA DM (nj) : replace YLL = N+0.5*Death[_n-1] if nj == 2
bysort sex age SEIFA DM (nj) : replace N = N+Death[_n-1] if nj == 2

*Attach utilities
gen UT=.
replace UT = `a4' if inrange(age,0,24) & sex == 1
replace UT = `a5' if inrange(age,25,34) & sex == 1
replace UT = `a6' if inrange(age,35,44) & sex == 1
replace UT = `a7' if inrange(age,45,54) & sex == 1
replace UT = `a8' if inrange(age,55,64) & sex == 1
replace UT = `a9' if inrange(age,65,74) & sex == 1
replace UT = `a10' if inrange(age,75,100) & sex == 1

replace UT = `a11' if inrange(age,0,24) & sex == 2
replace UT = `a12' if inrange(age,25,34) & sex == 2
replace UT = `a13' if inrange(age,35,44) & sex == 2
replace UT = `a14' if inrange(age,45,54) & sex == 2
replace UT = `a15' if inrange(age,55,64) & sex == 2
replace UT = `a16' if inrange(age,65,74) & sex == 2
replace UT = `a17' if inrange(age,75,100) & sex == 2

replace UT = UT*`a18' if DM == 1

replace UT = 0 if nj == 2

gen QALY=UT*YLL

*Attach costs (Needs to be fixed, for now just placeholders)
*Public only
gen hccost=.
replace hccost = `a19' if DM == 0
replace hccost = `a19'*1.6 if DM == 1
replace hccost=0 if nj == 2

gen double Healthcost = hccost*YLL

*Societal costs
merge m:1 age sex SEIFA DM using WAP
drop _merge

*Non-participation costs for alive are difference in employment (proportion not employed due to DM times salary and YLL)
gen double NPC1=.
replace NPC1=YLL*(EM-EMD)*80235 if sex == 1 & nj == 1 & DM == 1
replace NPC1=YLL*(EM-EMD)*56494 if sex == 2 & nj == 1 & DM == 1

*Non-participation death costs
replace NPC1=YLL*EM*80235 if sex == 1 & nj == 2
replace NPC1=YLL*EM*56494 if sex == 2 & nj == 2


*Absenteeism costs
gen double ABC1 = .
replace ABC1 = YLL*EMD*(3.0/240)*80235 if sex == 1 & nj == 1 & DM == 1
replace ABC1 = YLL*EMD*(3.0/240)*56494 if sex == 2 & nj == 1 & DM == 1





*Non-participation costs for alive are difference in employment (proportion not employed due to DM times salary and YLL)
gen double NPC2=.
replace NPC2=YLL*(EM-EMD)*50910 if nj == 1 & DM == 1 & SEIFA == 5
replace NPC2=YLL*(EM-EMD)*56329 if nj == 1 & DM == 1 & SEIFA == 4
replace NPC2=YLL*(EM-EMD)*61739 if nj == 1 & DM == 1 & SEIFA == 3
replace NPC2=YLL*(EM-EMD)*67726 if nj == 1 & DM == 1 & SEIFA == 2
replace NPC2=YLL*(EM-EMD)*88329 if nj == 1 & DM == 1 & SEIFA == 1

*Non-participation death costs
replace NPC2=YLL*EM*50910 if nj == 2 & SEIFA == 5
replace NPC2=YLL*EM*56329 if nj == 2 & SEIFA == 4
replace NPC2=YLL*EM*61739 if nj == 2 & SEIFA == 3
replace NPC2=YLL*EM*67726 if nj == 2 & SEIFA == 2
replace NPC2=YLL*EM*88329 if nj == 2 & SEIFA == 1


*Absenteeism costs
gen double ABC2 = .
replace ABC2 = YLL*EMD*(3.0/240)*50910 if nj == 1 & DM == 1 & SEIFA == 5
replace ABC2 = YLL*EMD*(3.0/240)*56329 if nj == 1 & DM == 1 & SEIFA == 4
replace ABC2 = YLL*EMD*(3.0/240)*61739 if nj == 1 & DM == 1 & SEIFA == 3
replace ABC2 = YLL*EMD*(3.0/240)*67726 if nj == 1 & DM == 1 & SEIFA == 2
replace ABC2 = YLL*EMD*(3.0/240)*88329 if nj == 1 & DM == 1 & SEIFA == 1






*Do all the tracking
forval ii = 1/5 {
local SEIFA = `ii'
su(DMINC) if SEIFA == `ii'
local INC = r(sum)
su(Death) if SEIFA == `ii' & DM == 0
local DTN = r(sum)
su(Death) if SEIFA == `ii' & DM == 1
local DTD = r(sum)
su(Death) if SEIFA == `ii'
local DTH = r(sum)
su(YLL) if SEIFA == `ii' & DM == 0 & nj == 1
local YLN = r(sum)
su(YLL) if SEIFA == `ii' & DM == 1 & nj == 1
local YLD = r(sum)
su(YLL) if SEIFA == `ii' & nj == 1
local YOL = r(sum)
su(QALY) if SEIFA == `ii'
local QLY = r(sum)
su(Healthcost) if SEIFA == `ii' & DM == 0
local HCN = r(sum)
su(Healthcost) if SEIFA == `ii' & DM == 1
local HCD = r(sum)
su(Healthcost) if SEIFA == `ii'
local HCC = r(sum)
su(NPC1) if SEIFA == `ii' & nj == 1
local NPA1 = r(sum)
su(NPC1) if SEIFA == `ii' & nj == 2
local NPD1 = r(sum)
su(ABC1) if SEIFA == `ii'
local ABS1 = r(sum)
su(NPC2) if SEIFA == `ii' & nj == 1
local NPA2 = r(sum)
su(NPC2) if SEIFA == `ii' & nj == 2
local NPD2 = r(sum)
su(ABC2) if SEIFA == `ii'
local ABS2 = r(sum)

post `atuin' (`sim') (`RR') (`year') (`SEIFA') (`INC') (`DTN') (`DTD') (`DTH') (`YLN') (`YLD') (`YOL') (`QLY') (`HCN') (`HCD') (`HCC') (`NPA1') (`NPD1') (`ABS1') (`NPA2') (`NPD2') (`ABS2')
}

*Age a year
drop DMINC Death
replace age = age+1 if age < 100
collapse (sum) N, by(age sex SEIFA DM nj)

*Add births
append using Births_`i'

*append using "G:/Jed/Equity model/Data/Births 2020.dta"

expand 2 if age == 0
bysort sex age SEIFA DM : replace nj = _n if age == 0
replace N = 0 if nj == 2 & age == 0

}
}
}
postclose `atuin'
}


}

}


*Presentation of model results
{

*Base-case results table
{
use THEBLACKPLOT, clear
keep if RR == 0
drop RR
gen double SCC1 = NPA1+NPD1+ABS1
gen double SCC2 = NPA2+NPD2+ABS2
gen double TTC1 = HCC+SCC1
gen double TTC2 = HCC+SCC2


*Discount
bysort sim SEIFA (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double HCNDC = HCN*DC
gen double HCDDC = HCD*DC
gen double HCCDC = HCC*DC
gen double NPADC1 = NPA1*DC
gen double NPDDC1 = NPD1*DC
gen double ABSDC1 = ABS1*DC
gen double SCCDC1 = SCC1*DC
gen double TTCDC1 = TTC1*DC
gen double NPADC2 = NPA2*DC
gen double NPDDC2 = NPD2*DC
gen double ABSDC2 = ABS2*DC
gen double SCCDC2 = SCC2*DC
gen double TTCDC2 = TTC2*DC

collapse (sum) INC-TTCDC2, by(sim SEIFA)

*Totals
foreach i in INC DTN DTD DTH YLN YLD YOL QLYDC HCNDC HCDDC HCCDC ABSDC1 NPADC1 NPDDC1 SCCDC1 TTCDC1 ABSDC2 NPADC2 NPDDC2 SCCDC2 TTCDC2 {
bysort sim (SEIFA) : egen double `i'TOT = sum(`i')
bysort sim (SEIFA) : gen `i'EXC = `i'-`i'[_n-4] if SEIFA == 5
bysort sim (SEIFA) : gen `i'ETT = (`i'-`i'[_n-4])+(`i'[_n-1]-`i'[_n-4])+(`i'[_n-2]-`i'[_n-4])+(`i'[_n-3]-`i'[_n-4]) if SEIFA == 5

centile `i'TOT if SEIFA == 5, centile(50 2.5 97.5)
matrix `i'MAT = (r(c_1), r(c_2), r(c_3))
centile `i' if SEIFA == 1, centile(50 2.5 97.5)
matrix `i'MAT = (`i'MAT,r(c_1), r(c_2), r(c_3))
centile `i' if SEIFA == 2, centile(50 2.5 97.5)
matrix `i'MAT = (`i'MAT,r(c_1), r(c_2), r(c_3))
centile `i' if SEIFA == 3, centile(50 2.5 97.5)
matrix `i'MAT = (`i'MAT,r(c_1), r(c_2), r(c_3))
centile `i' if SEIFA == 4, centile(50 2.5 97.5)
matrix `i'MAT = (`i'MAT,r(c_1), r(c_2), r(c_3))
centile `i' if SEIFA == 5, centile(50 2.5 97.5)
matrix `i'MAT = (`i'MAT,r(c_1), r(c_2), r(c_3))
centile `i'EXC if SEIFA == 5, centile(50 2.5 97.5)
matrix `i'MAT = (`i'MAT,r(c_1), r(c_2), r(c_3))
centile `i'ETT if SEIFA == 5, centile(50 2.5 97.5)
matrix `i'MAT = (`i'MAT,r(c_1), r(c_2), r(c_3))
}

matrix BIGMAT = (INCMAT\DTNMAT\DTDMAT\DTHMAT\YLNMAT\YLDMAT\YOLMAT\QLYDCMAT\HCNDCMAT\HCDDCMAT\HCCDCMAT\ABSDC1MAT\NPADC1MAT\NPDDC1MAT\SCCDC1MAT\TTCDC1MAT\ABSDC2MAT\NPADC2MAT\NPDDC2MAT\SCCDC2MAT\TTCDC2MAT)
clear
svmat double BIGMAT

tostring BIGMAT1-BIGMAT24, force format(%18.0fc) replace

gen njm = _n

foreach i in 1 4 7 10 13 16 19 22 {
local j = `i'+1
local k = `i'+2
replace BIGMAT`i' = BIGMAT`i'+ " (" + BIGMAT`j' + ", " + BIGMAT`k' + ")"
replace BIGMAT`i' = "\$" + BIGMAT`i' if njm >=9
replace BIGMAT`i' = subinstr(BIGMAT`i',"\$-","-\$",.)
}
drop BIGMAT2 BIGMAT3 BIGMAT5 BIGMAT6 BIGMAT8 BIGMAT9 BIGMAT11 BIGMAT12 BIGMAT14 BIGMAT15 BIGMAT17 BIGMAT18 BIGMAT20 BIGMAT21 BIGMAT23 BIGMAT24 njm
save Angles_of_Destruction, replace

}

*Intervention figures
{

*Popsize
{
use prevpop2020, clear
su(N)
di r(sum)
di r(sum)/5
}

*ICER vs. cost data
{

*Overall
{
use THEBLACKPLOT, clear
keep if RR == 0 | RR == 10 | RR == 25 | RR == 40
collapse (sum) INC-ABS2, by(sim RR year)
bysort sim RR (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double HCCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC
collapse (sum) QLYDC-ABS2DC, by(sim RR)
expand 201 if RR!=0
bysort sim RR : gen cost = 2*(_n-1)
bysort sim RR : gen ITCDC = 25529544*cost
replace ITCDC=0 if RR == 0
gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC
bysort sim (RR cost) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim (RR cost) : gen double INCTHC = THCDC-THCDC[1]
bysort sim (RR cost) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim (RR cost) : gen double INCTTC2 = TTC2DC-TTC2DC[1]
*Check no negative QALYs before proceed
count if INCQLY < 0
*STOP
gen double ICERTHC = INCTHC/INCQLY
gen double ICERTTC1 = INCTTC1/INCQLY
gen double ICERTTC2 = INCTTC2/INCQLY

keep sim RR cost ITCDC ICERTHC-ICERTTC2
quietly {
matrix ICERMAT = (0,0,0)
matrix SICER1MAT = (0,0,0)
matrix SICER2MAT = (0,0,0)
foreach i in 10 25 40 {
noisily di `i'
forval ii = 0(2)400 {
preserve
keep if RR == `i' & cost == `ii'
centile ICERTHC, centile(50 2.5 97.5)
matrix ICERMAT = (ICERMAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC1, centile(50 2.5 97.5)
matrix SICER1MAT = (SICER1MAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC2, centile(50 2.5 97.5)
matrix SICER2MAT = (SICER2MAT\r(c_1), r(c_2), r(c_3))
restore
}
}
}
svmat double ICERMAT
svmat double SICER1MAT
svmat double SICER2MAT
keep if sim == 1
keep RR cost ITCDC ICERMAT1-SICER2MAT3
save Buddy_Guy, replace
}

*By SES
{
use THEBLACKPLOT, clear
keep if RR == 0 | RR == 10 | RR == 25 | RR == 40
keep if SEIFA == 1 | SEIFA == 5
bysort sim RR SEIFA (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double HCCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC
collapse (sum) QLYDC-ABS2DC, by(sim RR SEIFA)
expand 201 if RR!=0
bysort sim RR SEIFA : gen cost = 2*(_n-1)
bysort sim RR SEIFA : gen ITCDC = 5105908.7*cost
replace ITCDC=0 if RR == 0
gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC
bysort sim SEIFA (RR cost) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim SEIFA (RR cost) : gen double INCTHC = THCDC-THCDC[1]
bysort sim SEIFA (RR cost) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim SEIFA (RR cost) : gen double INCTTC2 = TTC2DC-TTC2DC[1]
*Check no negative QALYs before proceed
count if INCQLY < 0
*STOP
gen double ICERTHC = INCTHC/INCQLY
gen double ICERTTC1 = INCTTC1/INCQLY
gen double ICERTTC2 = INCTTC2/INCQLY
keep sim RR SEIFA cost ITCDC ICERTHC-ICERTTC2
quietly {
matrix ICERMAT = (0,0,0)
matrix SICER1MAT = (0,0,0)
matrix SICER2MAT = (0,0,0)
foreach i in 10 25 40 {
noisily di `i'
forval ii = 0(2)400 {
preserve
keep if SEIFA == 1 & RR == `i' & cost == `ii'
centile ICERTHC, centile(50 2.5 97.5)
matrix ICERMAT = (ICERMAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC1, centile(50 2.5 97.5)
matrix SICER1MAT = (SICER1MAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC2, centile(50 2.5 97.5)
matrix SICER2MAT = (SICER2MAT\r(c_1), r(c_2), r(c_3))
restore
}
}
matrix ICERMAT = (ICERMAT\0,0,0)
matrix SICER1MAT = (SICER1MAT\0,0,0)
matrix SICER2MAT = (SICER2MAT\0,0,0)
foreach i in 10 25 40 {
noisily di `i'
forval ii = 0(2)400 {
preserve
keep if SEIFA == 5 & RR == `i' & cost == `ii'
centile ICERTHC, centile(50 2.5 97.5)
matrix ICERMAT = (ICERMAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC1, centile(50 2.5 97.5)
matrix SICER1MAT = (SICER1MAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC2, centile(50 2.5 97.5)
matrix SICER2MAT = (SICER2MAT\r(c_1), r(c_2), r(c_3))
restore
}
}
}
svmat double ICERMAT
svmat double SICER1MAT
svmat double SICER2MAT
keep if sim == 1
keep RR SEIFA cost ITCDC ICERMAT1-SICER2MAT3
save The_Falcon, replace
}

}

*ICER vs. effectiveness data
{

*Overall
{
use THEBLACKPLOT, clear
keep if RR == 0 | inrange(RR,10,40)
collapse (sum) INC-ABS2, by(sim RR year)
bysort sim RR (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double HCCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC
collapse (sum) QLYDC-ABS2DC, by(sim RR)
expand 3 if RR!=0
bysort sim RR : gen cost = _n*100
bysort sim RR : gen ITCDC = 25529544*cost
replace ITCDC=0 if RR == 0
gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC
bysort sim (RR cost) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim (RR cost) : gen double INCTHC = THCDC-THCDC[1]
bysort sim (RR cost) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim (RR cost) : gen double INCTTC2 = TTC2DC-TTC2DC[1]
*Check no negative QALYs before proceed
count if INCQLY < 0
*STOP
gen double ICERTHC = INCTHC/INCQLY
gen double ICERTTC1 = INCTTC1/INCQLY
gen double ICERTTC2 = INCTTC2/INCQLY
keep sim RR cost ITCDC ICERTHC-ICERTTC2
quietly {
matrix ICERMAT = (0,0,0)
matrix SICER1MAT = (0,0,0)
matrix SICER2MAT = (0,0,0)
forval i = 10/40 {
noisily di `i'
forval ii = 100(100)300 {
preserve
keep if RR == `i' & cost == `ii'
centile ICERTHC, centile(50 2.5 97.5)
matrix ICERMAT = (ICERMAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC1, centile(50 2.5 97.5)
matrix SICER1MAT = (SICER1MAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC2, centile(50 2.5 97.5)
matrix SICER2MAT = (SICER2MAT\r(c_1), r(c_2), r(c_3))
restore
}
}
}
svmat double ICERMAT
svmat double SICER1MAT
svmat double SICER2MAT
keep if sim == 1
keep RR cost cost ICERMAT1-SICER2MAT3
save Ocean, replace
}

*By SES 
{
use THEBLACKPLOT, clear
keep if RR == 0 | inrange(RR,10,40)
keep if SEIFA == 1 | SEIFA == 5
bysort sim RR SEIFA (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double HCCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC
collapse (sum) QLYDC-ABS2DC, by(sim RR SEIFA)
expand 3 if RR!=0
bysort sim RR SEIFA : gen cost = _n*100
bysort sim RR SEIFA : gen ITCDC = 5105908.7*cost
replace ITCDC=0 if RR == 0
gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC
bysort sim SEIFA (RR cost) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim SEIFA (RR cost) : gen double INCTHC = THCDC-THCDC[1]
bysort sim SEIFA (RR cost) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim SEIFA (RR cost) : gen double INCTTC2 = TTC2DC-TTC2DC[1]
*Check no negative QALYs before proceed
count if INCQLY < 0
*STOP
gen double ICERTHC = INCTHC/INCQLY
gen double ICERTTC1 = INCTTC1/INCQLY
gen double ICERTTC2 = INCTTC2/INCQLY
keep sim RR SEIFA cost ITCDC ICERTHC-ICERTTC2
quietly {
matrix ICERMAT = (0,0,0)
matrix SICER1MAT = (0,0,0)
matrix SICER2MAT = (0,0,0)
forval i = 10/40 {
noisily di `i'
forval ii = 100(100)300 {
preserve
keep if SEIFA == 1 & RR == `i' & cost == `ii'
centile ICERTHC, centile(50 2.5 97.5)
matrix ICERMAT = (ICERMAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC1, centile(50 2.5 97.5)
matrix SICER1MAT = (SICER1MAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC2, centile(50 2.5 97.5)
matrix SICER2MAT = (SICER2MAT\r(c_1), r(c_2), r(c_3))
restore
}
}
matrix ICERMAT = (ICERMAT\0,0,0)
matrix SICER1MAT = (SICER1MAT\0,0,0)
matrix SICER2MAT = (SICER2MAT\0,0,0)
forval i = 10/40 {
noisily di `i'
forval ii = 100(100)300 {
preserve
keep if SEIFA == 5 & RR == `i' & cost == `ii'
centile ICERTHC, centile(50 2.5 97.5)
matrix ICERMAT = (ICERMAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC1, centile(50 2.5 97.5)
matrix SICER1MAT = (SICER1MAT\r(c_1), r(c_2), r(c_3))
centile ICERTTC2, centile(50 2.5 97.5)
matrix SICER2MAT = (SICER2MAT\r(c_1), r(c_2), r(c_3))
restore
}
}
}
svmat double ICERMAT
svmat double SICER1MAT
svmat double SICER2MAT
keep if sim == 1
keep RR SEIFA cost cost ICERMAT1-SICER2MAT3
save The_Falconeer, replace

}

}


*Healthcare
{

*Overall
{

use Buddy_Guy, clear

drop if cost < 10

twoway ///
(rarea ICERMAT2 ICERMAT3 cost if RR == 10, color("50 0 95%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if RR == 10, color("50 0 95") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 cost if RR == 25, color("110 0 175%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if RR == 25, color("110 0 175") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 cost if RR == 40, color("170 0 255%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if RR == 40, color("170 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("ICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 400000)) ///
title("ICER vs. cost", color(gs0) placement(west) size(medium))
graph save "Graph" WoFat1, replace


use Ocean, clear

drop if ICERMAT1==0

twoway ///
(rarea ICERMAT2 ICERMAT3 RR if cost == 100, color("50 0 95%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if cost == 100, color("50 0 95") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 RR if cost == 200, color("110 0 175%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if cost == 200, color("110 0 175") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 RR if cost == 300, color("170 0 255%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if cost == 300, color("170 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("ICER") ///
ylabel(0(50000)250000, format(%9.0fc) nogrid) yscale(range(-10000 270000)) ///
title("ICER vs. efficacy", color(gs0) placement(west) size(medium))
graph save "Graph" Wofat2, replace



graph combine Wofat1.gph ///
Wofat2.gph ///
, iscale(0.8) rows(1) xsize(9) graphregion(color(white))
graph export Overall_Intervention.pdf, as(pdf) name("Graph") replace
}

*By SES
{
use The_Falcon, clear

drop if cost < 10

twoway ///
(rarea ICERMAT2 ICERMAT3 cost if SEIFA == 1 & RR == 10, color("90 0 90%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if SEIFA == 1 & RR == 10, color("90 0 90") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 cost if SEIFA == 1 & RR == 25, color("170 0 170%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if SEIFA == 1 & RR == 25, color("170 0 170") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 cost if SEIFA == 1 & RR == 40, color("255 0 255%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if SEIFA == 1 & RR == 40, color("255 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("ICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 485000)) ///
text(520000 7 "ICER vs. cost - IRSD 5 (least disadvantaged)", color(gs0) placement(east))
graph save "Graph" Falcon1A, replace

twoway ///
(rarea ICERMAT2 ICERMAT3 cost if SEIFA == 5 & RR == 10, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if SEIFA == 5 & RR == 10, color("0 0 90") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 cost if SEIFA == 5 & RR == 25, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if SEIFA == 5 & RR == 25, color("0 90 140") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 cost if SEIFA == 5 & RR == 40, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 cost if SEIFA == 5 & RR == 40, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("ICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 485000)) ///
text(520000 7 "ICER vs. cost - IRSD 1 (most disadvantaged)", color(gs0) placement(east))
graph save "Graph" Falcon1B, replace



use The_Falconeer, clear

drop if ICERMAT1==0

twoway ///
(rarea ICERMAT2 ICERMAT3 RR if SEIFA == 1 & cost == 100, color("90 0 90%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if SEIFA == 1 & cost == 100, color("90 0 90") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 RR if SEIFA == 1 & cost == 200, color("170 0 170%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if SEIFA == 1 & cost == 200, color("170 0 170") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 RR if SEIFA == 1 & cost == 300, color("255 0 255%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if SEIFA == 1 & cost == 300, color("255 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("ICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 450000)) ///
text(480000 7.5 "ICER vs. efficacy - IRSD 5 (least disadvantaged)", color(gs0) placement(east))
graph save "Graph" Falcon1C, replace


twoway ///
(rarea ICERMAT2 ICERMAT3 RR if SEIFA == 5 & cost == 100, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if SEIFA == 5 & cost == 100, color("0 0 90") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 RR if SEIFA == 5 & cost == 200, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if SEIFA == 5 & cost == 200, color("0 90 140") lpattern(solid)) ///
(rarea ICERMAT2 ICERMAT3 RR if SEIFA == 5 & cost == 300, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line ICERMAT1 RR if SEIFA == 5 & cost == 300, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("ICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 450000)) ///
text(480000 7.5 "ICER vs. efficacy - IRSD 1 (most disadvantaged)", color(gs0) placement(east))
graph save "Graph" Falcon1D, replace

graph combine Falcon1A.gph ///
Falcon1B.gph ///
Falcon1C.gph ///
Falcon1D.gph ///
, iscale(0.4) rows(2) xsize(5) graphregion(color(white))
graph export Intervention_by_SES.pdf, as(pdf) name("Graph") replace
}

}


*Societal
{

*Overall
{

use Buddy_Guy, clear

drop if cost < 10

twoway ///
(rarea SICER1MAT2 SICER1MAT3 cost if RR == 10, color("50 0 95%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if RR == 10, color("50 0 95") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 cost if RR == 25, color("110 0 175%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if RR == 25, color("110 0 175") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 cost if RR == 40, color("170 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if RR == 40, color("170 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 400000)) ///
text(430000 7 "SICER vs. cost - Income", color(gs0) placement(east))
graph save "Graph" Particle_Flux1, replace

twoway ///
(rarea SICER2MAT2 SICER2MAT3 cost if RR == 10, color("50 0 95%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if RR == 10, color("50 0 95") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 cost if RR == 25, color("110 0 175%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if RR == 25, color("110 0 175") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 cost if RR == 40, color("170 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if RR == 40, color("170 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 400000)) ///
text(430000 7 "SICER vs. cost - Income inequality", color(gs0) placement(east))
graph save "Graph" Particle_Flux3, replace


use Ocean, clear

drop if ICERMAT1==0

twoway ///
(rarea SICER1MAT2 SICER1MAT3 RR if cost == 100, color("50 0 95%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if cost == 100, color("50 0 95") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 RR if cost == 200, color("110 0 175%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if cost == 200, color("110 0 175") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 RR if cost == 300, color("170 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if cost == 300, color("170 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)250000, format(%9.0fc) nogrid) yscale(range(-50000 270000)) ///
text(290000 7.5 "SICER vs. efficacy - Income", color(gs0) placement(east))
graph save "Graph" Particle_Flux2, replace

twoway ///
(rarea SICER2MAT2 SICER2MAT3 RR if cost == 100, color("50 0 95%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if cost == 100, color("50 0 95") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 RR if cost == 200, color("110 0 175%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if cost == 200, color("110 0 175") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 RR if cost == 300, color("170 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if cost == 300, color("170 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)250000, format(%9.0fc) nogrid) yscale(range(-50000 270000)) ///
text(290000 7.5 "SICER vs. efficacy - Income inequality", color(gs0) placement(east))
graph save "Graph" Particle_Flux4, replace


graph combine ///
Particle_Flux1.gph ///
Particle_Flux2.gph ///
Particle_Flux3.gph ///
Particle_Flux4.gph ///
, iscale(0.4) rows(2) xsize(5) graphregion(color(white))
graph export Overall_societal.pdf, as(pdf) name("Graph") replace
}

*By SES
{

*SICER vs. cost
{

use The_Falcon, clear

drop if cost < 10

twoway ///
(rarea SICER1MAT2 SICER1MAT3 cost if SEIFA == 1 & RR == 10, color("90 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if SEIFA == 1 & RR == 10, color("90 0 90") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 cost if SEIFA == 1 & RR == 25, color("170 0 170%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if SEIFA == 1 & RR == 25, color("170 0 170") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 cost if SEIFA == 1 & RR == 40, color("255 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if SEIFA == 1 & RR == 40, color("255 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 450000)) ///
text(480000 7 "IRSD 5 (least disadvantaged) - Income", color(gs0) placement(east))
graph save "Graph" ATW1, replace

twoway ///
(rarea SICER2MAT2 SICER2MAT3 cost if SEIFA == 1 & RR == 10, color("90 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if SEIFA == 1 & RR == 10, color("90 0 90") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 cost if SEIFA == 1 & RR == 25, color("170 0 170%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if SEIFA == 1 & RR == 25, color("170 0 170") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 cost if SEIFA == 1 & RR == 40, color("255 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if SEIFA == 1 & RR == 40, color("255 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 450000)) ///
text(480000 7 "IRSD 5 (least disadvantaged) - Income inequality", color(gs0) placement(east))
graph save "Graph" ATW3, replace

twoway ///
(rarea SICER1MAT2 SICER1MAT3 cost if SEIFA == 5 & RR == 10, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if SEIFA == 5 & RR == 10, color("0 0 90") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 cost if SEIFA == 5 & RR == 25, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if SEIFA == 5 & RR == 25, color("0 90 140") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 cost if SEIFA == 5 & RR == 40, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 cost if SEIFA == 5 & RR == 40, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 450000)) ///
text(480000 7 "IRSD 1 (most disadvantaged) - Income", color(gs0) placement(east))
graph save "Graph" ATW2, replace

twoway ///
(rarea SICER2MAT2 SICER2MAT3 cost if SEIFA == 5 & RR == 10, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if SEIFA == 5 & RR == 10, color("0 0 90") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 cost if SEIFA == 5 & RR == 25, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if SEIFA == 5 & RR == 25, color("0 90 140") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 cost if SEIFA == 5 & RR == 40, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 cost if SEIFA == 5 & RR == 40, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(11) region(lcolor(white) color(none)) ///
order(2 "10% RRR" 4 "25% RRR" 6 "40% RRR") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Cost of intervention (AU$ per person)") ///
xscale(log) ///
xlabel(10 20 30 40 50 100 200 300 400, grid) xmtick(10(10)400, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)400000, format(%9.0fc) nogrid) yscale(range(-50000 450000)) ///
text(480000 7 "IRSD 1 (most disadvantaged) - Income inequality", color(gs0) placement(east))
graph save "Graph" ATW4, replace

graph combine ///
ATW1.gph ///
ATW2.gph ///
ATW3.gph ///
ATW4.gph ///
, iscale(0.4) rows(2) xsize(5) graphregion(color(white))
graph export SICERvscost_SES.pdf, as(pdf) name("Graph") replace

}


*SICER vs. efficacy
{


use The_Falconeer, clear

drop if ICERMAT1==0

twoway ///
(rarea SICER1MAT2 SICER1MAT3 RR if SEIFA == 1 & cost == 100, color("90 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if SEIFA == 1 & cost == 100, color("90 0 90") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 RR if SEIFA == 1 & cost == 200, color("170 0 170%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if SEIFA == 1 & cost == 200, color("170 0 170") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 RR if SEIFA == 1 & cost == 300, color("255 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if SEIFA == 1 & cost == 300, color("255 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)250000, format(%9.0fc) nogrid) yscale(range(-50000 350000)) ///
text(380000 7.5 "IRSD 5 (least disadvantaged) - Income", color(gs0) placement(east))
graph save "Graph" Will_He1, replace

twoway ///
(rarea SICER2MAT2 SICER2MAT3 RR if SEIFA == 1 & cost == 100, color("90 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if SEIFA == 1 & cost == 100, color("90 0 90") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 RR if SEIFA == 1 & cost == 200, color("170 0 170%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if SEIFA == 1 & cost == 200, color("170 0 170") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 RR if SEIFA == 1 & cost == 300, color("255 0 255%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if SEIFA == 1 & cost == 300, color("255 0 255") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)250000, format(%9.0fc) nogrid) yscale(range(-50000 350000)) ///
text(380000 7.5 "IRSD 5 (least disadvantaged) - Income inequality", color(gs0) placement(east))
graph save "Graph" Will_He3, replace

twoway ///
(rarea SICER1MAT2 SICER1MAT3 RR if SEIFA == 5 & cost == 100, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if SEIFA == 5 & cost == 100, color("0 0 90") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 RR if SEIFA == 5 & cost == 200, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if SEIFA == 5 & cost == 200, color("0 90 140") lpattern(solid)) ///
(rarea SICER1MAT2 SICER1MAT3 RR if SEIFA == 5 & cost == 300, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line SICER1MAT1 RR if SEIFA == 5 & cost == 300, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)250000, format(%9.0fc) nogrid) yscale(range(-50000 350000)) ///
text(380000 7.5 "IRSD 1 (most disadvantaged) - Income", color(gs0) placement(east))
graph save "Graph" Will_He2, replace

twoway ///
(rarea SICER2MAT2 SICER2MAT3 RR if SEIFA == 5 & cost == 100, color("0 0 90%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if SEIFA == 5 & cost == 100, color("0 0 90") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 RR if SEIFA == 5 & cost == 200, color("0 90 140%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if SEIFA == 5 & cost == 200, color("0 90 140") lpattern(solid)) ///
(rarea SICER2MAT2 SICER2MAT3 RR if SEIFA == 5 & cost == 300, color("0 150 180%30") fintensity(inten80) lwidth(none)) ///
(line SICER2MAT1 RR if SEIFA == 5 & cost == 300, color("0 150 180") lpattern(solid)) ///
, legend(ring(0) symxsize(0.13cm) position(2) region(lcolor(white) color(none)) ///
order(2 "$100 per person" 4 "$200 per person" 6 "$300 per person") cols(1)) ///
bgcolor(white) graphregion(color(white)) ///
xtitle("Relative risk reduction (%)") ///
xlabel(10(5)40, grid) ///
yline(0, lcolor(gs0)) yline(28000, lcolor(gs0) lpattern(dash))  ///
ytitle("SICER") ///
ylabel(-50000(100000)250000, format(%9.0fc) nogrid) yscale(range(-50000 350000)) ///
text(380000 7.5 "IRSD 1 (most disadvantaged) - Income inequality", color(gs0) placement(east))
graph save "Graph" Will_He4, replace

graph combine ///
Will_He1.gph ///
Will_He2.gph ///
Will_He3.gph ///
Will_He4.gph ///
, iscale(0.4) rows(2) xsize(5) graphregion(color(white))
graph export SICERvsefficacy_SES.pdf, as(pdf) name("Graph") replace

}

}

}


}

*Intervention tables
{

*Threshold cost at RR
{

use THEBLACKPLOT, clear
keep if RR == 0 | RR == 10 | RR == 25 | RR == 40
collapse (sum) INC-ABS2, by(sim RR year)
bysort sim RR (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double THCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC
collapse (sum) QLYDC-ABS2DC, by(sim RR)
gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC
bysort sim (RR) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim (RR) : gen double INCTHC = THCDC-THCDC[1]
bysort sim (RR) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim (RR) : gen double INCTTC2 = TTC2DC-TTC2DC[1]
*Check no negative QALYs before proceed
count if INCQLY < 0
*STOP
gen double CETHC = (28000*INCQLY - INCTHC)/25529544
gen double CSTHC = -INCTHC/25529544
gen double CETTC1 = (28000*INCQLY - INCTTC1)/25529544
gen double CSTTC1 = -INCTTC1/25529544
gen double CETTC2 = (28000*INCQLY - INCTTC2)/25529544
gen double CSTTC2 = -INCTTC2/25529544
keep sim RR CETHC-CSTTC2
matrix CEHC = (.,.,.,.,.)
matrix CSHC = (.,.,.)
matrix CESC1 = (.,.,.)
matrix CSSC1 = (.,.,.)
matrix CESC2 = (.,.,.)
matrix CSSC2 = (.,.,.)
foreach ii in 10 25 40 {
centile CETHC if RR == `ii', centile(50 2.5 97.5)
matrix CEHC = (CEHC\0,`ii',r(c_1), r(c_2), r(c_3))
centile CSTHC if RR == `ii', centile(50 2.5 97.5)
matrix CSHC = (CSHC\r(c_1), r(c_2), r(c_3))
centile CETTC1 if RR == `ii', centile(50 2.5 97.5)
matrix CESC1 = (CESC1\r(c_1), r(c_2), r(c_3))
centile CSTTC1 if RR == `ii', centile(50 2.5 97.5)
matrix CSSC1 = (CSSC1\r(c_1), r(c_2), r(c_3))
centile CETTC2 if RR == `ii', centile(50 2.5 97.5)
matrix CESC2 = (CESC2\r(c_1), r(c_2), r(c_3))
centile CSTTC2 if RR == `ii', centile(50 2.5 97.5)
matrix CSSC2 = (CSSC2\r(c_1), r(c_2), r(c_3))
}
clear
svmat double CEHC
svmat double CSHC
svmat double CESC1
svmat double CSSC1
svmat double CESC2
svmat double CSSC2
drop if _n==1
save CircusFreaks, replace

use THEBLACKPLOT, clear
keep if RR == 0 | RR == 10 | RR == 25 | RR == 40

bysort sim RR SEIFA (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double THCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC
collapse (sum) QLYDC-ABS2DC, by(sim RR SEIFA)

gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC

bysort sim SEIFA (RR) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim SEIFA (RR) : gen double INCTHC = THCDC-THCDC[1]
bysort sim SEIFA (RR) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim SEIFA (RR) : gen double INCTTC2 = TTC2DC-TTC2DC[1]


*Check no negative QALYs before proceed

count if INCQLY < 0
*STOP


gen double CETHC = (28000*INCQLY - INCTHC)/5105908.7
gen double CSTHC = -INCTHC/5105908.7
gen double CETTC1 = (28000*INCQLY - INCTTC1)/5105908.7
gen double CSTTC1 = -INCTTC1/5105908.7
gen double CETTC2 = (28000*INCQLY - INCTTC2)/5105908.7
gen double CSTTC2 = -INCTTC2/5105908.7


keep sim RR SEIFA CETHC-CSTTC2

matrix CEHC = (.,.,.,.,.)
matrix CSHC = (.,.,.)
matrix CESC1 = (.,.,.)
matrix CSSC1 = (.,.,.)
matrix CESC2 = (.,.,.)
matrix CSSC2 = (.,.,.)
forval i = 1/5 {
foreach ii in 10 25 40 {
centile CETHC if SEIFA == `i' & RR == `ii', centile(50 2.5 97.5)
matrix CEHC = (CEHC\0`i',`ii',r(c_1), r(c_2), r(c_3))
centile CSTHC if SEIFA == `i' & RR == `ii', centile(50 2.5 97.5)
matrix CSHC = (CSHC\r(c_1), r(c_2), r(c_3))
centile CETTC1 if SEIFA == `i' & RR == `ii', centile(50 2.5 97.5)
matrix CESC1 = (CESC1\r(c_1), r(c_2), r(c_3))
centile CSTTC1 if SEIFA == `i' & RR == `ii', centile(50 2.5 97.5)
matrix CSSC1 = (CSSC1\r(c_1), r(c_2), r(c_3))
centile CETTC2 if SEIFA == `i' & RR == `ii', centile(50 2.5 97.5)
matrix CESC2 = (CESC2\r(c_1), r(c_2), r(c_3))
centile CSTTC2 if SEIFA == `i' & RR == `ii', centile(50 2.5 97.5)
matrix CSSC2 = (CSSC2\r(c_1), r(c_2), r(c_3))
}
}
clear
svmat double CEHC
svmat double CSHC
svmat double CESC1
svmat double CSSC1
svmat double CESC2
svmat double CSSC2

drop if _n==1

append using CircusFreaks

tostring CEHC3-CSSC23, force format(%18.0fc) replace
gen CEHC = "\$" + CEHC3 + " (" + CEHC4 + ", " + CEHC5 + ")"
gen CSHC = "\$" + CSHC1 + " (" + CSHC2 + ", " + CSHC3 + ")"
gen CESC1 = "\$" + CESC11 + " (" + CESC12 + ", " + CESC13 + ")"
gen CSSC1 = "\$" + CSSC11 + " (" + CSSC12 + ", " + CSSC13 + ")"
gen CESC2 = "\$" + CESC21 + " (" + CESC22 + ", " + CESC23 + ")"
gen CSSC2 = "\$" + CSSC21 + " (" + CSSC22 + ", " + CSSC23 + ")"
drop CEHC3-CSSC23
sort CEHC2 CEHC1
br

}

*Threshold RR at cost
{


use THEBLACKPLOT, clear
collapse (sum) INC-ABS2, by(sim RR year)

bysort sim RR (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double HCCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC

collapse (sum) QLYDC-ABS2DC, by(sim RR)

expand 3 if RR!=0

bysort sim RR : gen cost = _n*100
gen ITCDC = 25529544*cost
replace ITCDC=0 if RR == 0

gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC

bysort sim (RR cost) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim (RR cost) : gen double INCTHC = THCDC-THCDC[1]
bysort sim (RR cost) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim (RR cost) : gen double INCTTC2 = TTC2DC-TTC2DC[1]


*Check no negative QALYs before proceed

count if INCQLY < 0
*STOP
gen double ICERTHC = INCTHC/INCQLY
gen double ICERTTC1 = INCTTC1/INCQLY
gen double ICERTTC2 = INCTTC2/INCQLY

keep sim RR cost ITCDC ICERTHC-ICERTTC2

gen A = 1 if ICERTHC < 28000
bysort sim cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim cost (RR) : egen THCRRCE = min(B)
drop A B

gen A = 1 if ICERTHC < 0
bysort sim cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim cost (RR) : egen THCRRCS = min(B)
drop A B

gen A = 1 if ICERTTC1 < 28000
bysort sim cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim cost (RR) : egen TTCRRCE1 = min(B)
drop A B

gen A = 1 if ICERTTC1 < 0
bysort sim cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim cost (RR) : egen TTCRRCS1 = min(B)
drop A B

gen A = 1 if ICERTTC2 < 28000
bysort sim cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim cost (RR) : egen TTCRRCE2 = min(B)
drop A B

gen A = 1 if ICERTTC2 < 0
bysort sim cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim cost (RR) : egen TTCRRCS2 = min(B)
drop A B

bysort sim cost (RR) : keep if _n == 1

recode THCRRCE .=100
recode THCRRCS .=100
recode TTCRRCE1 .=100
recode TTCRRCS1 .=100
recode TTCRRCE2 .=100
recode TTCRRCS2 .=100

keep sim cost THCRRCE-TTCRRCS2

matrix CEHC = (.,.,.,.,.)
matrix CSHC = (.,.,.)
matrix CESC1 = (.,.,.)
matrix CSSC1 = (.,.,.)
matrix CESC2 = (.,.,.)
matrix CSSC2 = (.,.,.)

forval ii = 100(100)300 {
centile THCRRCE if cost == `ii', centile(50 2.5 97.5)
matrix CEHC = (CEHC\0`i',`ii',r(c_1), r(c_2), r(c_3))
centile THCRRCS if cost == `ii', centile(50 2.5 97.5)
matrix CSHC = (CSHC\r(c_1), r(c_2), r(c_3))
centile TTCRRCE1 if cost == `ii', centile(50 2.5 97.5)
matrix CESC1 = (CESC1\r(c_1), r(c_2), r(c_3))
centile TTCRRCS1 if cost == `ii', centile(50 2.5 97.5)
matrix CSSC1 = (CSSC1\r(c_1), r(c_2), r(c_3))
centile TTCRRCE2 if cost == `ii', centile(50 2.5 97.5)
matrix CESC2 = (CESC2\r(c_1), r(c_2), r(c_3))
centile TTCRRCS2 if cost == `ii', centile(50 2.5 97.5)
matrix CSSC2 = (CSSC2\r(c_1), r(c_2), r(c_3))
}



clear
svmat double CEHC
svmat double CSHC
svmat double CESC1
svmat double CSSC1
svmat double CESC2
svmat double CSSC2

drop if _n==1

save PNEUMA, replace




use THEBLACKPLOT, clear

bysort sim RR SEIFA (year) : gen DC = 1/((1.05)^(_n-1))
gen double QLYDC = QLY*DC
gen double HCCDC = HCC*DC
gen double NPA1DC = NPA1*DC
gen double NPD1DC = NPD1*DC
gen double ABS1DC = ABS1*DC
gen double NPA2DC = NPA2*DC
gen double NPD2DC = NPD2*DC
gen double ABS2DC = ABS2*DC

collapse (sum) QLYDC-ABS2DC, by(sim RR SEIFA)

expand 3 if RR!=0

bysort sim RR SEIFA : gen cost = _n*100
gen ITCDC = 5105908.7*cost
replace ITCDC=0 if RR == 0

gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = NPA1DC+NPD1DC+ABS1DC+THCDC
gen double TTC2DC = NPA2DC+NPD2DC+ABS2DC+THCDC

bysort sim SEIFA (RR cost) : gen double INCQLY = QLYDC-QLYDC[1]
bysort sim SEIFA (RR cost) : gen double INCTHC = THCDC-THCDC[1]
bysort sim SEIFA (RR cost) : gen double INCTTC1 = TTC1DC-TTC1DC[1]
bysort sim SEIFA (RR cost) : gen double INCTTC2 = TTC2DC-TTC2DC[1]


*Check no negative QALYs before proceed

count if INCQLY < 0
*STOP
gen double ICERTHC = INCTHC/INCQLY
gen double ICERTTC1 = INCTTC1/INCQLY
gen double ICERTTC2 = INCTTC2/INCQLY

keep sim RR SEIFA cost ITCDC ICERTHC-ICERTTC2

gen A = 1 if ICERTHC < 28000
bysort sim SEIFA cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim SEIFA cost (RR) : egen THCRRCE = min(B)
drop A B

gen A = 1 if ICERTHC < 0
bysort sim SEIFA cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim SEIFA cost (RR) : egen THCRRCS = min(B)
drop A B

gen A = 1 if ICERTTC1 < 28000
bysort sim SEIFA cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim SEIFA cost (RR) : egen TTCRRCE1 = min(B)
drop A B

gen A = 1 if ICERTTC1 < 0
bysort sim SEIFA cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim SEIFA cost (RR) : egen TTCRRCS1 = min(B)
drop A B

gen A = 1 if ICERTTC2 < 28000
bysort sim SEIFA cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim SEIFA cost (RR) : egen TTCRRCE2 = min(B)
drop A B

gen A = 1 if ICERTTC2 < 0
bysort sim SEIFA cost A (RR) : replace A =. if _n!=1
gen B = RR if A == 1
bysort sim SEIFA cost (RR) : egen TTCRRCS2 = min(B)
drop A B


bysort sim SEIFA cost (RR) : keep if _n == 1

recode THCRRCE .=100
recode THCRRCS .=100
recode TTCRRCE1 .=100
recode TTCRRCS1 .=100
recode TTCRRCE2 .=100
recode TTCRRCS2 .=100

keep sim SEIFA cost THCRRCE-TTCRRCS2

matrix CEHC = (.,.,.,.,.)
matrix CSHC = (.,.,.)
matrix CESC1 = (.,.,.)
matrix CSSC1 = (.,.,.)
matrix CESC2 = (.,.,.)
matrix CSSC2 = (.,.,.)

forval i = 1/5 {
forval ii = 100(100)300 {
centile THCRRCE if SEIFA == `i' & cost == `ii', centile(50 2.5 97.5)
matrix CEHC = (CEHC\0`i',`ii',r(c_1), r(c_2), r(c_3))
centile THCRRCS if SEIFA == `i' & cost == `ii', centile(50 2.5 97.5)
matrix CSHC = (CSHC\r(c_1), r(c_2), r(c_3))
centile TTCRRCE1 if SEIFA == `i' & cost == `ii', centile(50 2.5 97.5)
matrix CESC1 = (CESC1\r(c_1), r(c_2), r(c_3))
centile TTCRRCS1 if SEIFA == `i' & cost == `ii', centile(50 2.5 97.5)
matrix CSSC1 = (CSSC1\r(c_1), r(c_2), r(c_3))
centile TTCRRCE2 if SEIFA == `i' & cost == `ii', centile(50 2.5 97.5)
matrix CESC2 = (CESC2\r(c_1), r(c_2), r(c_3))
centile TTCRRCS2 if SEIFA == `i' & cost == `ii', centile(50 2.5 97.5)
matrix CSSC2 = (CSSC2\r(c_1), r(c_2), r(c_3))
}
}


clear
svmat double CEHC
svmat double CSHC
svmat double CESC1
svmat double CSSC1
svmat double CESC2
svmat double CSSC2

drop if _n==1

append using PNEUMA

tostring CEHC3-CSSC23, force format(%18.0fc) replace
replace CSHC1 = "Nil" if CSHC1 == "100"
replace CSHC2 = "Nil" if CSHC2 == "100"

gen CEHC = CEHC3 + " (" + CEHC4 + ", " + CEHC5 + ")"
gen CSHC = CSHC1 + " (" + CSHC2 + ", " + CSHC3 + ")"
gen CESC1 = CESC11 + " (" + CESC12 + ", " + CESC13 + ")"
gen CSSC1 = CSSC11 + " (" + CSSC12 + ", " + CSSC13 + ")"
gen CESC2 = CESC21 + " (" + CESC22 + ", " + CESC23 + ")"
gen CSSC2 = CSSC21 + " (" + CSSC22 + ", " + CSSC23 + ")"
drop CEHC3-CSSC23
sort CEHC2 CEHC1

}

*Detailed outcomes tables
{

*Overall
{

use THEBLACKPLOT, clear

keep if RR == 0 | RR == 10 | RR == 25 | RR == 40

collapse (sum) INC-ABS2, by(sim RR year)

gen double SCC1 = NPA1+NPD1+ABS1
gen double SCC2 = NPA2+NPD2+ABS2
gen double TTC1 = HCC+SCC1
gen double TTC2 = HCC+SCC2

bysort sim RR (year) : gen DC = 1/((1.05)^(_n-1))

gen double QLYDC = QLY*DC
gen double HCNDC = HCN*DC
gen double HCDDC = HCD*DC
gen double HCCDC = HCC*DC
gen double NPADC1 = NPA1*DC
gen double NPDDC1 = NPD1*DC
gen double ABSDC1 = ABS1*DC
gen double SCCDC1 = SCC1*DC
gen double TTCDC1 = TTC1*DC
gen double NPADC2 = NPA2*DC
gen double NPDDC2 = NPD2*DC
gen double ABSDC2 = ABS2*DC
gen double SCCDC2 = SCC2*DC
gen double TTCDC2 = TTC2*DC

collapse (sum) INC-TTCDC2, by(sim RR)

expand 3 if RR!=0

bysort sim RR : gen cost = _n*100
gen ITCDC = 25529544*cost
replace cost = 0 if RR == 0
replace ITCDC = 0 if RR == 0

gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = SCCDC1+THCDC
gen double TTC2DC = SCCDC2+THCDC

foreach var of varlist INC-YOL QLYDC-TTC2DC {
bysort sim (RR cost) : gen double INC_`var' = `var'-`var'[1]
}

*Check no negative QALYs before proceed
count if INC_QLY < 0
*STOP
gen double ICERTHC = INC_THCDC/INC_QLYDC
gen double ICERTTC1 = INC_TTC1DC/INC_QLYDC
gen double ICERTTC2 = INC_TTC2DC/INC_QLYDC

*Actual values
foreach var of varlist INC-ICERTTC2 {

centile `var' if RR == 0, centile(50 2.5 97.5)
matrix `var'MAT = (0,0,0,r(c_1), r(c_2), r(c_3))

matrix MAT`var'_0_0 = `var'MAT

foreach i in 10 25 40 {
foreach ii in 100 200 300 {
centile `var' if RR == `i' & cost == `ii', centile(50 2.5 97.5)
matrix `var'MAT = (`i',`ii',0,r(c_1), r(c_2), r(c_3))

matrix MAT`var'_`i'_`ii' = `var'MAT

}
}
}

preserve
foreach var of varlist INC-ICERTTC2 {

matrix A = MAT`var'_0_0

foreach i in 10 25 40 {
foreach ii in 100 200 300 {
matrix A = (A\MAT`var'_`i'_`ii')
}
}

clear
svmat A
gen VAL = "`var'"
save ONC1_`var'.dta", replace

}
restore

foreach var of varlist INC-ICERTTC2 {
if "`var'" == "INC"  {
clear
append using ONC1_`var'
}
else {
append using ONC1_`var'
}
}

save ONC, replace


}

*SEIFA
{

use THEBLACKPLOT, clear

keep if RR == 0 | RR == 10 | RR == 25 | RR == 40

gen double SCC1 = NPA1+NPD1+ABS1
gen double SCC2 = NPA2+NPD2+ABS2
gen double TTC1 = HCC+SCC1
gen double TTC2 = HCC+SCC2

bysort sim RR SEIFA (year) : gen DC = 1/((1.05)^(_n-1))

gen double QLYDC = QLY*DC
gen double HCNDC = HCN*DC
gen double HCDDC = HCD*DC
gen double HCCDC = HCC*DC
gen double NPADC1 = NPA1*DC
gen double NPDDC1 = NPD1*DC
gen double ABSDC1 = ABS1*DC
gen double SCCDC1 = SCC1*DC
gen double TTCDC1 = TTC1*DC
gen double NPADC2 = NPA2*DC
gen double NPDDC2 = NPD2*DC
gen double ABSDC2 = ABS2*DC
gen double SCCDC2 = SCC2*DC
gen double TTCDC2 = TTC2*DC

collapse (sum) INC-TTCDC2, by(sim RR SEIFA)

expand 3 if RR!=0

bysort sim RR SEIFA : gen cost = _n*100
gen ITCDC = 5105908.7*cost
replace cost = 0 if RR == 0
replace ITCDC = 0 if RR == 0

gen double THCDC = HCCDC+ITCDC
gen double TTC1DC = SCCDC1+THCDC
gen double TTC2DC = SCCDC2+THCDC

foreach var of varlist INC-YOL QLYDC-TTC2DC {
bysort sim SEIFA (RR cost) : gen double INC_`var' = `var'-`var'[1]
}

*Check no negative QALYs before proceed
count if INC_QLY < 0
*STOP
gen double ICERTHC = INC_THCDC/INC_QLYDC
gen double ICERTTC1 = INC_TTC1DC/INC_QLYDC
gen double ICERTTC2 = INC_TTC2DC/INC_QLYDC

*Actual values
foreach var of varlist INC-ICERTTC2 {

centile `var' if SEIFA == 1 & RR == 0, centile(50 2.5 97.5)
matrix `var'MAT = (0,0,1,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 2 & RR == 0, centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0,0,2,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 3 & RR == 0, centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0,0,3,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 4 & RR == 0, centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0,0,4,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 5 & RR == 0, centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0,0,5,r(c_1), r(c_2), r(c_3))

matrix MAT`var'_0_0 = `var'MAT

foreach i in 10 25 40 {
foreach ii in 100 200 300 {
centile `var' if SEIFA == 1 & RR == `i' & cost == `ii', centile(50 2.5 97.5)
matrix `var'MAT = (`i',`ii',1,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 2 & RR == `i' & cost == `ii', centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0`i',`ii',2,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 3 & RR == `i' & cost == `ii', centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0`i',`ii',3,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 4 & RR == `i' & cost == `ii', centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0`i',`ii',4,r(c_1), r(c_2), r(c_3))
centile `var' if SEIFA == 5 & RR == `i' & cost == `ii', centile(50 2.5 97.5)
matrix `var'MAT = (`var'MAT\0`i',`ii',5,r(c_1), r(c_2), r(c_3))

matrix MAT`var'_`i'_`ii' = `var'MAT

}
}
}

preserve
foreach var of varlist INC-ICERTTC2 {

matrix A = MAT`var'_0_0

foreach i in 10 25 40 {
foreach ii in 100 200 300 {
matrix A = (A\MAT`var'_`i'_`ii')
}
}

clear
svmat A
gen VAL = "`var'"
save ONC_`var', replace

}
restore

foreach var of varlist INC-ICERTTC2 {
if "`var'" == "INC"  {
clear
append using ONC_`var'
}
else {
append using ONC_`var'
}
}

save ONC_IRSD, replace


}


use ONC, clear
append using ONC_IRSD

gen row =.
replace row = 1 if VAL == "INC" | VAL == "INC_INC"
replace row = 2 if VAL == "DTN" | VAL == "INC_DTN"
replace row = 3 if VAL == "DTD" | VAL == "INC_DTD"
replace row = 4 if VAL == "DTH" | VAL == "INC_DTH"
replace row = 5 if VAL == "YLN" | VAL == "INC_YLN"
replace row = 6 if VAL == "YLD" | VAL == "INC_YLD"
replace row = 7 if VAL == "YOL" | VAL == "INC_YOL"
replace row = 8 if VAL == "QLYDC" | VAL == "INC_QLYDC"
replace row = 9 if VAL == "HCNDC" | VAL == "INC_HCNDC"
replace row = 10 if VAL == "HCDDC" | VAL == "INC_HCDDC"
replace row = 11 if VAL == "ITCDC" | VAL == "INC_ITCDC"
replace row = 12 if VAL == "THCDC" | VAL == "INC_THCDC"
replace row = 13 if VAL == "NPADC1" | VAL == "INC_NPADC1"
replace row = 14 if VAL == "NPDDC1" | VAL == "INC_NPDDC1"
replace row = 15 if VAL == "ABSDC1" | VAL == "INC_ABSDC1"
replace row = 16 if VAL == "SCCDC1" | VAL == "INC_SCCDC1"
replace row = 17 if VAL == "TTC1DC" | VAL == "INC_TTC1DC"
replace row = 18 if VAL == "NPADC2" | VAL == "INC_NPADC2"
replace row = 19 if VAL == "NPDDC2" | VAL == "INC_NPDDC2"
replace row = 20 if VAL == "ABSDC2" | VAL == "INC_ABSDC2"
replace row = 21 if VAL == "SCCDC2" | VAL == "INC_SCCDC2"
replace row = 22 if VAL == "TTC2DC" | VAL == "INC_TTC2DC"
replace row = 23 if VAL == "ICERTHC"
replace row = 24 if VAL == "ICERTTC1"
replace row = 25 if VAL == "ICERTTC2"

drop if row==.
gen inc = 0
replace inc = 1 if substr(VAL,1,4)=="INC_" | row >= 23
drop if A1 == 0 & inc == 1

tostring A4-A6, force format(%18.0fc) replace
replace A4 = A4 + " (" + A5 + ", " + A6 + ")"
replace A4 = "\$" + A4 if inrange(row,9,22)
replace A4 = subinstr(A4,"\$-","-\$",.)

*Full tables

foreach i in 10 25 40 {
foreach ii in 100 200 300 {
preserve
replace A3 = (A3*10) + inc
keep if A1 == `i' & A2 == `ii'
keep A3 A4 row
reshape wide A4, i(row) j(A3)
save The_Pursuit_of_Vikings_`i'_`ii', replace
restore
}
}

*Small table

keep if (row == 1 | row == 8 | row == 12 | row == 23) & inc == 1

replace A1 = (A1*1000)+A2
replace row = 100*row+A3

keep A1 A4 row
reshape wide A4, i(row) j(A1)





}

}


}
