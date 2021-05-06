*data cleaning file

*change working directory
cd "C:\Users\dmf52\OneDrive - Johns Hopkins\Dissertation\Summer 2019\MEPSUM August 2019"

*import data
use "int_surv3", clear

*get rid of prevalent difficulty
keep if end!=0

*ages of entry and exit
sum age_entry, detail
sum age_end, detail /*age_end is always missing if last visit is censored*/

*create censoring variable to aid with coding
gen censor = 0
replace censor = 1 if end==.

*need to get age at last visit for people who are censored
tab entry if age_end==.
/*for censored individuals, entry is last known IADL status
entry+1 will contain missing values (censored at this time interval)
*/
replace end = (entry+1) if censor==1 

/*age at first study visit is == age
age at last study visit (whether censored or not) is ==age_end
*/
gen endage = age+end /*ranges from 66 to 96, 99(2 obs only)*/
sum endage, detail


*create grouping variables for iadls
*note that total() function returns sum of varlist or . if all are missing
egen acta = rowtotal(iadla2_diff_bin-iadlc2_diff_bin), missing /*Cooking*/
replace acta = 1 if (acta>0 & acta!=.)
egen actb = rowtotal(iadld2_diff_bin-iadle2_diff_bin), missing /*Housework*/
replace actb = 1 if (actb>0 & actb!=.)
egen actc = rowtotal(iadlf2_diff_bin-iadli2_diff_bin), missing /*Money*/
replace actc = 1 if (actc>0 & actc!=.)
egen actd = rowtotal(iadlj2_diff_bin-iadlm2_diff_bin), missing	/*Healthcare*/
replace actd = 1 if (actd>0 & actd!=.)
egen acte = rowtotal(iadln2_diff_bin-iadlq2_diff_bin), missing /*Phone*/
replace acte = 1 if (acte>0 & acte!=.)
gen actf = iadlr2_diff_bin /*Shopping*/
gen actg = iadls2_diff_bin /*Travel*/


*create grouping variables for iadls
*Danielle's proposed grouping
*note that total() function returns sum of varlist or . if all are missing
*create variables based on Danielle's proposed grouping
egen dana = rowtotal(iadlf2_diff_bin-iadli2_diff_bin), missing /*Finances*/
replace dana = 1 if (dana>0 & dana!=.)
egen danb = rowtotal(iadlj2_diff_bin-iadlk2_diff_bin), missing /*Managing meds (cognitive)*/
replace danb = 1 if (danb>0 & danb!=.)
egen danc = rowtotal(iadll2_diff_bin-iadlm2_diff_bin), missing /*Managing meds (physical)*/
replace danc = 1 if (danc>0 & danc!=.)
egen dand = rowtotal(iadln2_diff_bin-iadlq2_diff_bin), missing /*Phone use*/
replace dand = 1 if (dand>0 & dand!=.)
egen dane = rowtotal(iadlr2_diff_bin-iadls2_diff_bin), missing /*Outside travel*/
replace dane = 1 if (dane>0 & dane!=.)
egen danf = rowtotal(iadla2_diff_bin iadlc2_diff_bin), missing /*Housework (cognitive)*/
replace danf = 1 if (danf>0 & danf!=.)
egen dang = rowtotal(iadlb2_diff_bin iadld2_diff_bin-iadle2_diff_bin), missing /*Housework (physical)*/
replace dang = 1 if (dang>0 & dang!=.)


*generate IADL variables for each time interval
/*
forvalues v = 66/99	{
	foreach task of varlist acta-actg dana-dang {
		gen `task'j`v' = 999
		}
	}
*/	
	
*grouped time interval (age range in which incident difficulty occurred)
/*	66-70
	71-75
	76-80
	81-85
	86-90
	90+

forvalues t = 70(5)95 {
	foreach task of varlist acta-actg dana-dang {
		gen t`task'j`t' = 999
		}
	}
*/


*grouped time interval - interval in which difficulty occurred (2 years)
/*	66-67
	68-69
	70-71
	72-73
	74-75
	76-77
	78-79
	80-81
	82-83
	84-85
	86-87
	88-89
*/
forvalues t = 67(2)89 {
	foreach task of varlist acta-actg dana-dang {
		gen t`task'j`t' = 999
		}
	}		


*grouped time interval - interval in which difficulty occurred (3 years)
/*	66-68
	69-71
	72-74
	75-77
	78-80
	81-83
	84-86
	87-89
	
forvalues t = 68(3)89 {
	foreach task of varlist acta-actg dana-dang {
		gen t`task'j`t' = 999
		}
	}
*/	

*check entry and exit for all people	
sum age, detail
sum endage, detail	





/* IADLS FOR EACH YEAR OF AGE */

*generate observed IADLs for each person
levelsof id, local(persons)
foreach subject of local persons {
	/*get start and end age for specific person*/
	qui summarize age if id=="`subject'" 
	local min = r(min) /*start age*/
	qui summarize endage if id=="`subject'"
	local max = r(max) /*end age*/
	forvalues v = 66/99 {
		foreach task of varlist acta-actg dana-dang {
			/*uncensored individuals*/
			replace `task'j`v' = 0 if (id[_n]=="`subject'" & `v'>=`min' & `v'<`max' & censor==0)
			replace `task'j`v' = `task' if (id[_n]=="`subject'" & `v'==`max' & censor==0) /*incident IADL difficulty*/
			
			/*censored individuals*/
			replace `task'j`v' = 0 if (id[_n]=="`subject'" & `v'<=(`max'-1) & censor==1) /*last observed visit (no difficulty)*/
			replace `task'j`v' = 999 if (id[_n]=="`subject'" & `v'==`max' & censor==1) /*now censored, so missing data*/
			}
		}
	}
	
*check N
forvalues v = 66/99 {
	foreach task of varlist acta-actg dana-dang {
		tab `task'j`v'
		}
	}

	
	
/* IADLS FOR EACH 5 YEARS OF AGE */	
	
/*	66-70
	71-75
	76-80
	81-85
	86-90
	90+ (need to fix below loop for this)
*/	

*generate observed IADLs in grouped time
levelsof id, local(persons)
foreach subject of local persons {
	/*get start and end age for specific person*/
	qui summarize age if id=="`subject'" 
	local min = r(min) /*start age*/
	qui summarize endage if id=="`subject'"
	local max = r(max) /*end age*/
	forvalues t = 70(5)95 {
		foreach task of varlist acta-actg dana-dang {
			/*uncensored individuals*/
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & `min'>=`t' & `max'<`t' & censor==0)
			replace t`task'j`t' = `task' if (id[_n]=="`subject'" & `min'<`t' & `max'<=`t' & censor==0)
			
			/*censored individuals*/
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & `min'<`t' & (`max'-1)>=`t' & censor==1)
			}
		}
	}
	
*check N
forvalues t = 70(5)95 {
	foreach task of varlist acta-actg dana-dang {
		tab t`task'j`t'
		}
	}
	
/* IADLS FOR EACH 2 YEARS OF AGE */
/*	66-67
	68-69
	70-71
	72-73
	74-75
	76-77
	78-79
	80-81
	82-83
	84-85
	86-87
	88-89
*/

*generate observed IADLs in grouped time *2 years)
levelsof id, local(persons)
foreach subject of local persons {
	/*get start and end age for specific person*/
	qui summarize age if id=="`subject'" 
	local min = r(min) /*start age*/
	qui summarize endage if id=="`subject'"
	local max = r(max) /*end age*/
	forvalues t = 67(2)89 {
		foreach task of varlist acta-actg dana-dang {
			/*uncensored individuals*/
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & inrange(`min',`t'-1,`t')==1 & censor==0)
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & inrange(`t', `min', `max')==1 & censor==0)
			*replace t`task'j`t' = 0 if (id[_n]=="`subject'" & `min'>=`t' & `max'<`t' & censor==0)
			replace t`task'j`t' = `task' if (id[_n]=="`subject'" & inrange(`max',`t'-1,`t')==1 & censor==0) 
			*replace t`task'j`t' = `task' if (id[_n]=="`subject'" & `min'<`t' & `max'<=`t' & censor==0)
			
			/*censored individuals*/
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & `min'<`t' & (`max'-1)>=`t' & censor==1)
			}
		}
	}	
*check N
forvalues t = 67(2)89 {
	foreach task of varlist acta-actg dana-dang {
		tab t`task'j`t'
		}
	}		






/* IADLS FOR EACH 3 YEARS OF AGE */
/*	66-68
	69-71
	72-74
	75-77
	78-80
	81-83
	84-86
	87-89
*/	
*generate observed IADLs in grouped time *3 years)
levelsof id, local(persons)
foreach subject of local persons {
	/*get start and end age for specific person*/
	qui summarize age if id=="`subject'" 
	local min = r(min) /*start age*/
	qui summarize endage if id=="`subject'"
	local max = r(max) /*end age*/
	forvalues t = 68(3)89 {
		foreach task of varlist acta-actg dana-dang {
			/*uncensored individuals*/
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & inrange(`min',`t'-2,`t')==1 & censor==0)
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & inrange(`t', `min', `max')==1 & censor==0)
			*replace t`task'j`t' = 0 if (id[_n]=="`subject'" & `min'>=`t' & `max'<`t' & censor==0)
			replace t`task'j`t' = `task' if (id[_n]=="`subject'" & inrange(`max',`t'-2,`t')==1 & censor==0) 
			*replace t`task'j`t' = `task' if (id[_n]=="`subject'" & `min'<`t' & `max'<=`t' & censor==0)
			
			/*censored individuals*/
			replace t`task'j`t' = 0 if (id[_n]=="`subject'" & `min'<`t' & (`max'-1)>=`t' & censor==1)
			}
		}
	}	
*check N
forvalues t = 68(3)89 {
	foreach task of varlist acta-actg dana-dang {
		tab t`task'j`t'
		}
	}		
			
*handle missing data for mplus
*no missing intgrp
replace bl_sf36h = 999 if bl_sf36h == .
replace site = 999 if site == .
replace female = 999 if female == .
replace race3 = 999 if race3 == .
replace age = 999 if age == .
replace yrseduc = 999 if yrseduc == .
replace marstat = 999 if marstat == .

*change id variable to string
*destring id, replace force
*cap drop tempvar
*generate tempvar = [_n]
*replace id = tempvar
*cap drop tempvar

*create 4 group variables for intervention status
cap drop mem
gen mem = 1 if intgrp==1
replace mem = 0 if intgrp!=1
cap drop reason
gen reason = 1 if intgrp==2
replace reason = 0 if intgrp!=2
cap drop speed
gen speed = 1 if intgrp==3
replace speed = 0 if intgrp!=3
cap drop control
gen control = 1 if intgrp==4
replace control = 0 if intgrp!=4

*for aim 2 analysis
cd "C:\Users\dmf52\OneDrive - Johns Hopkins\Dissertation\Aim 2\Data Files"

*keep only necessary variables
findname *dan*
drop `r(varlist)'
keep id bl_sf36 female race3 age yrseduc marstat mem reason speed control ///
tactaj67-tactgj89
*sort for mplus
order tactaj67-tactgj89, sequential
order id mem reason speed control bl_sf36 female race3 age yrseduc marstat bl_sf36h, first
*replace ID for mplus
destring id, force replace
replace id = _n
export delimited ACTIVE_mepsum_scale2, novarnames nolabel replace


/*************************************************************/

cd "C:\Users\dmf52\OneDrive - Johns Hopkins\Dissertation\Summer 2019\MEPSUM August 2019\ACTIVE Agescale Race Subanalysis"
cap drop _merge
merge id using currdrive, sort update

keep if _merge==3

*save full dataset
save lta_full, replace

*prepare csv for mplus analysis
*grouped iadls based on active coding
use lta_full, clear



save lta_full, replace


*compare between censored and uncensored individuals
tab iadl2_event

tab female iadl2_event, chi2 freq
tab race3 iadl2_event, chi2 freq
tab intgrp iadl2_event, chi2 freq
tab marstat iadl2_event, chi2 freq
ttest age if age!=999, by(iadl2_event)
sdtest age, by(iadl2_event)
sdtest yrseduc, by(iadl2_event)
ttest yrseduc if yrseduc!=999, by(iadl2_event)
ttest bl_sf36 if yrseduc!=999, by(iadl2_event)
sum age, detail
sum yrseduc, detail
sum bl_sf36, detail
*replace bl_sf36 = . if bl_sf36==999
ttest bl_sf36, by(iadl2_event)


*keep needed variables
findname *dan*
drop `r(varlist)'
keep id intgrp bl_sf36 female race3 age yrseduc marstat mem reason speed control ///
actaj66-actgj99
*sort for mplus
order actaj66-actgj99, sequential
order id intgrp mem reason speed control bl_sf36 female race3 age yrseduc marstat bl_sf36h, first

*export
export delimited lta_Aug19_ACTIVE, novarnames nolabel replace
save lta_Aug19_ACTIVE, replace

*prepare file for race analysis
*per the updated SDoH guidelines posted on 2/6/20, analyses using race should:
	*use White, Black, (both including Hispanic) and drop other races because N is small
use lta_full, clear
findname *dan*
drop `r(varlist)'
gen black = 0 /*White*/
replace black = 1 if race==2 /*Black*/
drop if inlist(race, 3, 5, 6, 7, .) /*drop all other race*/
keep id intgrp bl_sf36 female black age yrseduc marstat mem reason speed control ///
actaj66-actgj99
*sort for mplus
order actaj66-actgj99, sequential
order id intgrp mem reason speed control bl_sf36 female black age yrseduc marstat bl_sf36h, first
export delimited lta_Aug19_ACTIVE_race, novarnames nolabel replace
save lta_Aug19_ACTIVE_race, replace


*prepare csv based on active (grouped time)
use lta_full, clear
findname *dan*
drop `r(varlist)'
gen black = 0 /*White*/
replace black = 1 if race==2 /*Black*/
drop if inlist(race, 3, 5, 6, 7, .) /*drop all other race*/
findname tact*
keep id intgrp site bl_sf36 female black age yrseduc marstat mem reason speed control curdrive `r(varlist)'
*sort for mplus
order tactaj70-tactgj95, sequential
order id intgrp site mem reason speed control bl_sf36 female black age yrseduc marstat curdrive, first
export delimited lta_Aug19_ACTIVE_t, novarnames nolabel replace
save lta_Aug19_ACTIVE_t, replace

*prepare csv based on Danielle's coding
use lta_full, clear
findname *act*
drop `r(varlist)'
keep id intgrp bl_sf36 female race3 age yrseduc marstat mem reason speed control ///
danaj66-dangj99
order danaj66-dangj99, sequential
order id intgrp mem reason speed control bl_sf36 female race3 age yrseduc marstat bl_sf36h, first
export delimited lta_Aug19_DAN, novarnames nolabel replace
save lta_Aug19_DAN, replace

*prepare csv based on Danielle's coding (grouped time)
use lta_full, clear
findname *act*
drop `r(varlist)'
findname tdan*
keep id intgrp bl_sf36 female race3 age yrseduc marstat mem reason speed control `r(varlist)'
*sort for mplus
order tdanaj70-tdanaj95, sequential
order id intgrp mem reason speed control bl_sf36 female race3 age yrseduc marstat, first
export delimited lta_Aug19_DAN_t, novarnames nolabel replace
save lta_Aug19_DAN_t, replace






