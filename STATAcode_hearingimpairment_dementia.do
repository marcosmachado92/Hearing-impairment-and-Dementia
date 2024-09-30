********************************************************************************
********************************************************************************
****************************** STATA CODE  *************************************
********************************************************************************
********************************************************************************


********************************************************************************
********************************************************************************
******************** ANALYSES IN WHITEHALL II **********************************
********************************************************************************
********************************************************************************


********************************************************************************
********************************************************************************
/*                               DESCRIPTIVES  - TABLE 1                      */
********************************************************************************
********************************************************************************

******HEARING IMPAIRMENT

*N
tab hearing_impairment

*Age
sum age if hearing_impairment==0
sum age if hearing_impairment==1

ttest age, by(hearing_impairment)

*Sex
tab sex hearing_impairment, col chi

*Ethnicity
tab ethn_ds hearing_impairment, col chi

*Marital status
tab maritalstatus hearing_impairment, col chi nolab

gen livingalone=.
replace livingalone=0 if maritalstatus==1
replace livingalone=1 if maritalstatus==2 | maritalstatus==3

tab livingalone maritalstatus

tab livingalone hearing_impairment, col chi 


*Education
tab edu hearing_impairment, col chi


*BMI
tab bmi_cat hearing_impairment, col chi

*Alcohol
tab alcohol hearing_impairment, col chi

*Smoking
tab smoking hearing_impairment, col chi

*DIET
tab diet hearing_impairment, col chi

*Physical activity
sum pa if hearing_impairment==0
sum pa if hearing_impairment==1

ttest pa, by(hearing_impairment)


*Prevalent CHD
tab chd_prevalent hearing_impairment, col chi

*Prevalent Stroke
tab stroke_prevalent hearing_impairment, col chi

*Prevalent heart failure
tab heartfailure_prevalent hearing_impairment, col chi

*Prevalent HTA
tab hta_prevalent hearing_impairment, col chi

*Prevalent DIABETES
tab diabetes_prevalent hearing_impairment, col chi

*Prevalent CANCER
tab cancer_prevalent hearing_impairment, col chi

*Prevalent CKD
tab ckd_prevalent hearing_impairment, col chi

*Prevalent COPD
tab copd_prevalent hearing_impairment, col chi

*Prevalent LIVER
tab liver_prevalent hearing_impairment, col chi

*Prevalent DEPRESSION
tab depression_prevalent hearing_impairment, col chi

*Prevalent MENTAL
tab mentaldisorders_prevalent hearing_impairment, col chi

*Prevalent LIVER
tab arthritis_prevalent hearing_impairment, col chi

*DEMENTIA
tab demence hearing_impairment, col chi

*FOLLOW-UP
gen time_yr=(date_demence - tdatscrn_i)/365.25

sum time_yr if hearing_impairment==0, detail
sum time_yr if hearing_impairment==1, detail

ttest time_yr, by(hearing_impairment)




*********************
* HEARING AID USE
*********************
*N
tab hearing_aid

*Age
sum age if hearing_aid==0
sum age if hearing_aid==1

ttest age, by(hearing_aid)

*sex
tab sex hearing_aid, col chi

*Ethnicity
tab ethn_ds hearing_aid, col chi

*Marital status
tab livingalone hearing_aid, col chi 


*Education
tab edu hearing_aid, col chi

*BMI
tab bmi_cat hearing_aid, col chi

*Alcohol
tab alcohol hearing_aid, col chi

*Smoking
tab smoking hearing_aid, col chi

*DIET
tab diet hearing_aid, col chi

*Physical activity
sum pa if hearing_aid==0
sum pa if hearing_aid==1

ttest pa, by(hearing_aid)

*Prevalent CHD
tab chd_prevalent hearing_aid, col chi

*Prevalent Stroke
tab stroke_prevalent hearing_aid, col chi

*Prevalent heart failure
tab heartfailure_prevalent hearing_aid, col chi

*Prevalent HTA
tab hta_prevalent hearing_aid, col chi

*Prevalent DIABETES
tab diabetes_prevalent hearing_aid, col chi

*Prevalent CANCER
tab cancer_prevalent hearing_aid, col chi

*Prevalent CKD
tab ckd_prevalent hearing_aid, col chi

*Prevalent COPD
tab copd_prevalent hearing_aid, col chi

*Prevalent LIVER
tab liver_prevalent hearing_aid, col chi

*Prevalent DEPRESSION
tab depression_prevalent hearing_aid, col chi

*Prevalent MENTAL
tab mentaldisorders_prevalent hearing_aid, col chi

*Prevalent LIVER
tab arthritis_prevalent hearing_aid, col chi

*DEMENTIA
tab demence hearing_aid, col chi

*FOLLOW-UP

sum time_yr if hearing_aid==0, detail
sum time_yr if hearing_aid==1, detail

ttest time_yr, by(hearing_aid)



********************************************************************************
********************************************************************************
/*                         TABLE 2
             ASSOCIATION BETWEEN HEARING IMPAIRMENT, HEARING AID USE
			                      AND DEMENTIA                                */
********************************************************************************
********************************************************************************

capture prog drop hearing_dementia
program hearing_dementia
args variable variable2

*follow-up

gen startfu=tdatscrn_i

gen time=date_demence- startfu

gen time_yr=time/365.25



*We declare time and event for Cox Regression analyses

stset date_demence, fail(demence) scale(365.25) entry(startfu) id(stno) origin(dateob_c)



*follow up
sum time_yr,detail


* N cases
tab  `variable' demence 

* Rate of dementia
stptime, by(`variable') per(1000)



*MEN
*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.maritalstatus 
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.maritalstatus i.diet i.smoking i.alcohol pa i.bmi_cat 
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_d i.maritalstatus i.diet i.smoking i.alcohol pa i.bmi_cat number_chronicdiseases 
estat phtest, detail


end

hearing_dementia hearing_impairment 

hearing_dementia hearing_aid 



********************************************************************************
********************************************************************************
/*                         TABLE 3A
             HEARING IMPAIRMENT AND HEARING AID USE - 3 CATEGORIES         */
********************************************************************************
********************************************************************************


*SEX ADJUSTED
capture prog drop hearing_dementia
program hearing_dementia
args variable variable2

*follow-up

gen startfu=tdatscrn_i

gen time=date_demence- startfu

gen time_yr=time/365.25

drop if `variable'==1

replace `variable'=1 if `variable'==2
replace `variable'=2 if `variable'==3


. label define hearing_imp_aid_label 1 "", modify

. label define hearing_imp_aid_label 1 "hearing impairment and No hearing aid", add

. label define hearing_imp_aid_label 2 "", modify

. label define hearing_imp_aid_label 2 "Hearing impairment and hearing aid", add

. label define hearing_imp_aid_label 3 "", modify




*We declare time and event for Cox Regression analyses

stset date_demence, fail(demence) scale(365.25) entry(startfu) id(stno) origin(dateob_c)



*follow up
sum time_yr,detail


* N cases
tab  `variable' demence 

* Rate of dementia
stptime, by(`variable') per(1000)



*MEN
*COX model 1 if sex==1
stcox i.`variable' i.sex i.edu i.ethn_ds i.maritalstatus 
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.maritalstatus i.diet i.smoking i.alcohol pa i.bmi_cat 
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_d i.maritalstatus i.diet i.smoking i.alcohol pa i.bmi_cat number_chronicdiseases 
estat phtest, detail


end

hearing_dementia hearing_imp_aid 
 

********************************************************************************
********************************************************************************
/*                         TABLE 3B
             ASSOCIATION BETWEEN HEARING AID USE AND AND DEMENTIA
             IN THOSE WITH HEARING IMPAIRMENT          */
********************************************************************************
********************************************************************************

capture prog drop hearing_dementia
program hearing_dementia
args variable variable2 variable3


*follow-up

gen startfu=tdatscrn_i

gen time=date_demence- startfu

gen time_yr=time/365.25

keep if `variable2'==1




*We declare time and event for Cox Regression analyses

stset date_demence, fail(demence) scale(365.25) entry(startfu) id(stno) origin(dateob_c)



*follow up
sum time_yr,detail


* N cases
tab  `variable' demence

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.maritalstatus, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.maritalstatus i.diet i.smoking i.alcohol pa i.bmi_cat, nolog
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_ds i.maritalstatus i.diet i.smoking i.alcohol pa i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail




end

hearing_dementia hearing_aid hearing_impairment



********************************************************************************
********************************************************************************
/*                         TABLE S1
              Prevalence of hearing impairment and hearing aid use           */
********************************************************************************
********************************************************************************

gen age_2cat=.
replace age_2cat=0 if age<60
replace age_2cat=1 if age>=60


codebook age_2cat

tab hearing_impairment age_2cat, col chi

tab hearing_aid age_2cat, col chi





********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************** ANALYSES IN UK BIOBANK **********************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************


********************************************************************************
/*                                 TABLE 1                                    */
*******************************************************************************

*********HEARING IMPAIRMENT

replace sex=2 if sex==0

*N
tab hearing_impairment

*Age
sum age if hearing_impairment==0
sum age if hearing_impairment==1

ttest age, by(hearing_impairment)

*SEX
tab sex hearing_impairment, col chi


*Ethnicity
tab ethn_ds hearing_impairment, col chi

*living alone
tab livingalone hearing_impairment, col chi

*Education
tab edu hearing_impairment, col chi


*BMI
tab bmi_cat hearing_impairment, col chi

*Alcohol
tab alcohol_3cat hearing_impairment, col chi

*Smoking
tab smoking hearing_impairment, col chi

*DIET
tab fruitveg hearing_impairment, col chi

*Physical activity
sum pa_met if hearing_impairment==0
sum pa_met if hearing_impairment==1

ttest pa_met, by(hearing_impairment)


*Prevalent CHD
tab chd_prevalent hearing_impairment, col chi

*Prevalent Stroke
tab stroke_prevalent hearing_impairment, col chi

*Prevalent heart failure
tab heartfailure_prevalent hearing_impairment, col chi

*Prevalent HTA
tab hta_prevalent hearing_impairment, col chi

*Prevalent DIABETES
tab diabetes_prevalent hearing_impairment, col chi

*Prevalent CANCER
tab cancer_prevalent hearing_impairment, col chi

*Prevalent CKD
tab ckd_prevalent hearing_impairment, col chi

*Prevalent COPD
tab copd_prevalent hearing_impairment, col chi

*Prevalent LIVER
tab liver_prevalent hearing_impairment, col chi

*Prevalent DEPRESSION
tab depression_prevalent hearing_impairment, col chi

*Prevalent MENTAL
tab mentaldisorders_prevalent hearing_impairment, col chi

*Prevalent LIVER
tab arthritis_prevalent hearing_impairment, col chi


*DEMENTIA
tab dementia_whiidef hearing_impairment, col chi

*FOLLOW-UP
gen time_yr=(lenfol - screening_date)/365.25

sum time_yr if hearing_impairment==0, detail
sum time_yr if hearing_impairment==1,detail

ttest time_yr, by(hearing_impairment)



*********HEARING AID

*N
tab hearing_aid

*Age
sum age if hearing_aid==0
sum age if hearing_aid==1

ttest age, by(hearing_aid)

*SEX
tab sex hearing_aid, col chi


*Ethnicity
tab ethn_ds hearing_aid, col chi

*living alone
tab livingalone hearing_aid, col chi

*Education
tab edu hearing_aid, col chi


*BMI
tab bmi_cat hearing_aid, col chi

*Alcohol
tab alcohol_3cat hearing_aid, col chi

*Smoking
tab smoking hearing_aid, col chi

*DIET
tab fruitveg hearing_aid, col chi

*Physical activity
sum pa_met if hearing_aid==0
sum pa_met if hearing_aid==1

ttest pa_met, by(hearing_aid)


*Prevalent CHD
tab chd_prevalent hearing_aid, col chi

*Prevalent Stroke
tab stroke_prevalent hearing_aid, col chi

*Prevalent heart failure
tab heartfailure_prevalent hearing_aid, col chi

*Prevalent HTA
tab hta_prevalent hearing_aid, col chi

*Prevalent DIABETES
tab diabetes_prevalent hearing_aid, col chi

*Prevalent CANCER
tab cancer_prevalent hearing_aid, col chi

*Prevalent CKD
tab ckd_prevalent hearing_aid, col chi

*Prevalent COPD
tab copd_prevalent hearing_aid, col chi

*Prevalent LIVER
tab liver_prevalent hearing_aid, col chi

*Prevalent DEPRESSION
tab depression_prevalent hearing_aid, col chi

*Prevalent MENTAL
tab mentaldisorders_prevalent hearing_aid, col chi

*Prevalent LIVER
tab arthritis_prevalent hearing_aid, col chi


*FOLLOW-UP

sum time_yr if hearing_aid==0, detail
sum time_yr if hearing_aid==1,detail

ttest time_yr, by(hearing_aid)

*DEMENTIA
tab dementia_whiidef hearing_aid, col chi




********************************************************************************
********************************************************************************
/*                               TABLE 2                                      */
********************************************************************************
********************************************************************************

**********
/*SEX ADJUSTED ANALYSES*/
*********

capture prog drop hearing_dementia
program hearing_dementia
args variable variable2

*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25

replace sex=2 if sex==0



*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



sum age_dementia, detail


*follow up
sum time_yr,detail

* N cases
tab  `variable' dementia_whiidef 

* Rate of dementia
stptime, by(`variable') per(1000) 



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat, nolog
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail


end

hearing_dementia hearing_impairment   

hearing_dementia hearing_aid





********************************************************************************
********************************************************************************
/*                               TABLE 3                                      */
********************************************************************************
********************************************************************************

capture prog drop hearing_dementia
program hearing_dementia
args variable variable2

*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25

replace sex=2 if sex==0

drop if `variable'==1
replace `variable'=1 if `variable'==2
replace `variable'=2 if `variable'==3

. label define hearing_imp_aid 1 "", modify

. label define hearing_imp_aid 1 "hearing impairment and No hearing aid", add

. label define hearing_imp_aid 2 "", modify

. label define hearing_imp_aid 2 "Hearing impairment and hearing aid", add

. label define hearing_imp_aid 3 "", modify

*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



sum age_dementia, detail


*follow up
sum time_yr,detail

* N cases
tab  `variable' dementia_whiidef 

* Rate of dementia
stptime, by(`variable') per(1000) 



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat, nolog
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail


end

hearing_dementia hearing_impairment_aid 




************************
/* 
TABLE 3B
HEARING AID USE IN THOSE WITH HEARING IMPAIRMENT

*/
************************



capture prog drop hearing_dementia
program hearing_dementia
args variable variable2


*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25

keep if `variable2'==1

replace sex=2 if sex==0



*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat, nolog
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia hearing_aid hearing_impairment 



********************************************************************************
********************************************************************************
/*                               TABLE 4
                CALCULATION OF HR FOR OBJECTIVE MEASURES ALONE                */
********************************************************************************
********************************************************************************


capture prog drop hearing_dementia
program hearing_dementia
args variable variable2

replace sex=2 if sex==0

drop if `variable'==.



*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25



*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia hearing_inspoor 



********************************************************************************
********************************************************************************
/*                         TABLE S1
              Prevalence of hearing impairment and hearing aid use           */
********************************************************************************
********************************************************************************

gen age_2cat=.
replace age_2cat=0 if age<60
replace age_2cat=1 if age>=60


codebook age_2cat

tab hearing_impairment age_2cat, col chi

tab hearing_aid age_2cat, col chi




********************************************************************************
********************************************************************************
/*                               TABLE S2         

                            STRATIFIED ANALYSES BY FOLLOW-UP LENGTH           */
********************************************************************************
********************************************************************************

*****************
*<10 years of follow-up
*****************

capture prog drop hearing_dementia_stratified_fup
program hearing_dementia_stratified_fup
args variable variable2

replace sex=2 if sex==0


*follow-up

gen screening_10=(screening_date)+(10*365.25)
format %td screening_10

count if lenfol>screening_10 & dementia_whiidef==0 & mortality==0
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==0 & mortality==0

count if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
gen change1=1 if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
replace dementia_whiidef=0 if change1==1
drop change1

count if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
gen change2=1 if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
replace mortality=0 if change2==1
drop change2




*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
gen time=lenfol- screening_date

gen time_yr=time/365.25


sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail



end

hearing_dementia_stratified_fup hearing_impairment


hearing_dementia_stratified_fup hearing_aid


*HEARING IMPAIRMENT 3 CATEGORIES
capture prog drop hearing_dementia
program hearing_dementia
args variable variable3


*follow-up
gen screening_10=(screening_date)+(10*365.25)
format %td screening_10

count if lenfol>screening_10 & dementia_whiidef==0 & mortality==0
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==0 & mortality==0

count if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
gen change1=1 if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
replace dementia_whiidef=0 if change1==1
drop change1

count if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
gen change2=1 if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
replace mortality=0 if change2==1
drop change2


drop if `variable'==1
replace `variable'=1 if `variable'==2
replace `variable'=2 if `variable'==3

. label define hearing_imp_aid 1 "", modify

. label define hearing_imp_aid 1 "hearing impairment and No hearing aid", add

. label define hearing_imp_aid 2 "", modify

. label define hearing_imp_aid 2 "Hearing impairment and hearing aid", add

. label define hearing_imp_aid 3 "", modify


*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25


replace sex=2 if sex==0



*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog

estat phtest, detail


*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia hearing_impairment_aid 


*HEARING AID IN THOSE WITH HEARING IMPAIRMENT
capture prog drop hearing_dementia
program hearing_dementia
args variable variable2 


*follow-up
gen screening_10=(screening_date)+(10*365.25)
format %td screening_10

count if lenfol>screening_10 & dementia_whiidef==0 & mortality==0
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==0 & mortality==0

count if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
gen change1=1 if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==1 & mortality==0
replace dementia_whiidef=0 if change1==1
drop change1

count if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
gen change2=1 if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
replace lenfol=screening_10 if lenfol>screening_10 & dementia_whiidef==0 & mortality==1
replace mortality=0 if change2==1
drop change2



*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25


keep if `variable2'==1

replace sex=2 if sex==0



*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog

estat phtest, detail


*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia hearing_aid hearing_impairment 




*****************
*>=10 years of follow-up
*****************

capture prog drop hearing_dementia_strat_fup
program hearing_dementia_strat_fup
args variable variable2


replace sex=2 if sex==0




*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25

keep if time_yr>=10

gen screening_10=(screening_date)+(10*365.25)




*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_10) id(n_eid) origin(date_birth)



*follow up
gen time_10=(lenfol- screening_10)/365.25

sum time_10, detail



* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)


*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia_strat_fup hearing_impairment 

hearing_dementia_strat_fup hearing_aid 


*HEARING IMPAIRMENT 3 CATEGORIES
capture prog drop hearing_dementia
program hearing_dementia
args variable variable3


*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25

keep if time_yr>=10

gen screening_10=(screening_date)+(10*365.25)

drop if `variable'==1
replace `variable'=1 if `variable'==2
replace `variable'=2 if `variable'==3

. label define hearing_imp_aid 1 "", modify

. label define hearing_imp_aid 1 "hearing impairment and No hearing aid", add

. label define hearing_imp_aid 2 "", modify

. label define hearing_imp_aid 2 "Hearing impairment and hearing aid", add

. label define hearing_imp_aid 3 "", modify


replace sex=2 if sex==0


*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_10) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)



*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia hearing_impairment_aid


*HEARING AID IN THOSE WITH HEARING IMPAIRMENT

capture prog drop hearing_dementia
program hearing_dementia
args variable variable2 variable3


*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25

keep if time_yr>=10

gen screening_10=(screening_date)+(10*365.25)


keep if `variable2'==1

replace sex=2 if sex==0




*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_10) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)


*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia hearing_aid hearing_impairment 


********************************************************************************
********************************************************************************
/*                               TABLE S3  
                          OUTCOME: DEMENTIA SUBTYPES                          */
********************************************************************************
********************************************************************************

capture prog drop hearing_dementia_subtypes_2
program hearing_dementia_subtypes_2
args variable variable2 


replace sex=2 if sex==0


*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25



*We declare time and event for Cox Regression analyses

stset lenfol, fail(`variable2') scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' `variable2'

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat, nolog
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail


end

*AD
hearing_dementia_subtypes_2 hearing_impairment dementia_AD 
gen age_AD=(lenfol- date_birth)/365.25 if dementia_AD==1
sum age_AD, detail

hearing_dementia_subtypes_2 hearing_aid dementia_AD 


*VaD
hearing_dementia_subtypes_2 hearing_impairment dementia_VD 

hearing_dementia_subtypes_2 hearing_aid dementia_VD 
gen age_VD=(lenfol- date_birth)/365.25 if dementia_VD==1
sum age_VD, detai





********************************************************************************
********************************************************************************
/*                               TABLE S4         

                              ANALYSES WITH APOE                              */
********************************************************************************
********************************************************************************


* TEST INTERACTION
capture prog drop hearing_dementia
program hearing_dementia
args variable


*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25


*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)


*CHECK INTERACTION WITH SEX AND ETHNICITY
stcox i.`variable' i.apoe3 i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
est store M1

stcox i.`variable'##i.apoe3 i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
est store M2

lrtest M1 M2



end

hearing_dementia hearing_impairment /*No interaction*/

hearing_dementia hearing_aid /*No interaction*/



/*ANALYSES STRATIFIED BY APOE STATUS - OUTCOME: ALL CAUSE DEMENTIA*/
capture prog drop hearing_dementia2_apoe
program hearing_dementia2_apoe
args variable variable2 


*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25


keep if apoe3==`variable2'

replace sex=2 if sex==0



*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)


*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 2
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat, nolog
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases , nolog
estat phtest, detail





end


*NON-APOE carriers
hearing_dementia2_apoe hearing_impairment 0  
hearing_dementia2_apoe hearing_aid 0

*APOE carriers
hearing_dementia2_apoe hearing_impairment 1   
hearing_dementia2_apoe hearing_aid 1





********************************************************************************
********************************************************************************
/*                         TABLE S5
             CONCORDANCE BETWEEN SELF-REPORTED AND OBJECTIVE MEASURES         */
********************************************************************************
********************************************************************************

*HEARING IMPAIRMENT
tab  hearing_impairment hearing_inspoor, col chi
kap hearing_impairment hearing_inspoor 

*HEARING AID
tab  hearing_aid hearing_inspoor, col chi
kap hearing_aid hearing_inspoor 




********************************************************************************
********************************************************************************
/*                         TABLE S6
                         SELF-REPORTED AND OBJECTIVE MEASURES                 */
********************************************************************************
********************************************************************************


capture prog drop hearing_dementia
program hearing_dementia
args variable variable2

replace sex=2 if sex==0

drop if `variable'==.



*follow-up
gen time=lenfol- screening_date

gen time_yr=time/365.25



*We declare time and event for Cox Regression analyses

stset lenfol, fail(dementia_whiidef) scale(365.25) entry(screening_date) id(n_eid) origin(date_birth)



*follow up
sum time_yr,detail


* N cases
tab  `variable' dementia_whiidef

* Rate of dementia
stptime, by(`variable') per(1000)



*COX model 1
stcox i.`variable' i.sex i.edu i.ethn_ds i.livingalone, nolog
estat phtest, detail

*Cox model 3
stcox i.`variable'  i.sex i.edu i.ethn_ds i.livingalone i.fruitveg i.smoking i.alcohol_3cat i.pa_q5 i.bmi_cat number_chronicdiseases, nolog
estat phtest, detail

end

hearing_dementia final_hearing_inspoor 


hearing_dementia final_aid_inspoor 






********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
****************** ANALYSES IN WHITEHALL II AND UK BIOBANK *********************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************





********************************************************************************
********************************************************************************
/*                         TABLE 4
                         META-ANALYSIS FOR PAF CALCULATION                 */
********************************************************************************
********************************************************************************


*HEARING IMPAIRMENT - MODEL 3
keep if variable=="hearing_impairment" & model=="m3"

generate double log_estimate = log(estimate)
generate double log_min95 = log(min95)
generate double log_max95 = log(max95)

meta set log_estimate log_min95 log_max95, civartolerance(1e-1) 

meta summarize, random(reml) eform(hr)

meta summarize, fixed eform(hr) 



*HEARING AID - MODEL 3
keep if variable=="hearing_aid" &  model=="m3"

generate double log_estimate = log(estimate)
generate double log_min95 = log(min95)
generate double log_max95 = log(max95)

meta set log_estimate log_min95 log_max95, civartolerance(1e-1) 

meta summarize, random(reml) eform(hr) 

meta summarize, fixed eform(hr)