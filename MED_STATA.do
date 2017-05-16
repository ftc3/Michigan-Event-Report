clear all
set more off, permanently
cd C:\users\fcousin\desktop\MED_Report1\

capture log close
log using MED_STATAlog.log, replace


*** PLEASE READ:

**   It is probably helpful to review this starting from the mid-point,
**   " * THE ANALYSIS BELOW..." Event data was analyzed before Michigan
**   school data was, although in the write-up, Michigan school data comes
**   first.




*** saving non master data for later merge************

import excel "MED2_STATA_2014.xlsx", sheet("Section3") firstrow clear
drop ratingcomments
save section3.dta, replace
import excel "MED2_STATA_2014.xlsx", sheet("Section4") firstrow clear
drop usefulcomments
save section4.dta, replace
import excel "MED2_STATA_2014.xlsx", sheet("Section1") firstrow clear

*Variable labels******************************
label variable PRE_welcoming "campus is welcoming for students pre-survey"
label variable POST_welcoming "campus is welcoming for students post-survey"

label variable PRE_believesucceed "I can succeed academically pre-suvey"
label variable POST_believesucceed "I can succeed academically post-suvey"

label variable PRE_beadmitted "I can be admitted pre-survey"
label variable POST_beadmitted "I can be admitted post-survey"

label variable PRE_fitin "I would fit in to the campus community pre-survey"
label variable POST_fitin "I would fit in to the campus community post-survey"

label variable PRE_planapply "I plan to apply to the 'university' pre-survey"
label variable POST_planapply "I plan to apply to the 'university' post-survey"

* value labels *******************************************
label define ordinal 1 "Strongly Disagree" 2 "Disagree" 3 "Uncertain" 4 "Agree" 5 "Strongly Agree"
label values PRE_welcoming POST_welcoming PRE_believesucceed POST_believesucceed PRE_beadmitted POST_beadmitted PRE_fitin POST_fitin ordinal
label drop ordinal
*I don't like looking at words in the dataset. some fractions in data.

*Plan to apply isn't meaningful as numbers
label define apply 1 "yes" 2 "no" 3 "don't know"
label values PRE_planapply POST_planapply apply
decode PRE_planapply, gen(PRE_apply)
decode POST_planapply, gen(POST_apply) 
*just don't treat original like numbers are meaningful tab works the same
tab PRE_apply
tab PRE_planapply

* Plan to apply before and after frequencies and percentages
tab PRE_planapply
tab POST_planapply


* Group analysis ****************************************
list group
****changing engineering--> "Engineering"
encode group, gen(group2)
label list group2
tab group2
*correcting value label
recode group2 (8=3) (10=5) (6=.) (7 11=6) (9=7)
label drop group2
label define group2 1 "Af-Am Studies" 2 "Business" 3 "Engineering" 4 "Natural Science" 5 "Nursing" 6 "Sociology" 7 "History" 
label list group2
*switching default group
drop group
rename group2 group
tab group


table group, c(freq mean POST_welcoming)

tab group POST_welcoming
****tab group POST_welcoming, missing <--this shows us missings from both categories
tab POST_welcoming
*not many values missing from the observations missing a group

* PRE POST comparisons *******************************

tab PRE_welcoming POST_welcoming
*this only shows us pairs

tab PRE_welcoming
tab POST_welcoming

ttest POST_welcoming==PRE_welcoming
ttest POST_welcoming==PRE_welcoming, unpaired
*should I also use the unequal option for unequal variance?
*not possible for one sample or paired tests


program analyze
    args y x
	tab `x'
	tab `y'
	tab `x' `y'
	tab `x' `y', missing
	ttest `y'==`x'
	ttest `y'==`x', unpaired unequal
end

*see what change in variance assumption does to unpaired
*see diff between paired and unpaired with = variance assumption

analyze POST_welcoming PRE_welcoming
analyze POST_believesucceed PRE_believesucceed
analyze POST_beadmitted PRE_beadmitted
analyze POST_fitin PRE_fitin

*** PROGRAMS FOR TABS FOR PAPER****************************
program paper
    args y x
	tab `x' `y', missing matcell(`y')
	putexcel set `y', replace
	putexcel C3=matrix(`y')
end
*for re-editing: program drop paper
paper POST_welcoming PRE_welcoming
paper POST_believesucceed PRE_believesucceed
paper POST_beadmitted PRE_beadmitted
paper POST_fitin PRE_fitin

program seemeans
    args y x
	ttest `y'==`x'
	
	ttest `y'==`x', unpaired unequal
end

*********************************************************** OBTAINED FROM PROF JOHN HANSON

** CREATING DIFFERENCE OF MEANS TABLE
// This do file makes a table that contains difference of means statistics for a number of variables

// Create an Excel spreadsheet.
putexcel set table_means, replace

// Label the columns of the table we want to create. This block places column labels in row 1.
putexcel A1 = ("Variable") 
putexcel B1 = ("Mean Post")  
putexcel C1 = ("Mean Pre")
putexcel D1 = ("Difference")
putexcel E1 = ("t-stat")
putexcel F1 = ("P-value")

// Perform first t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_welcoming==POST_welcoming

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 1 in row 2
putexcel A2 = ("Welcoming")
putexcel B2 = (`r(mu_2)')
putexcel C2 = (`r(mu_1)')
putexcel D2 = (diff)
putexcel E2 = (-r(t))
putexcel F2 = (`r(p)')
// Perform second t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_believesucceed==POST_believesucceed

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 2 in row 3
putexcel A3 = ("Succeed")
putexcel B3 = (`r(mu_2)')
putexcel C3 = (`r(mu_1)')
putexcel D3 = (diff)
putexcel E3 = (-r(t))
putexcel F3 = (`r(p)')

// Perform third t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_beadmitted==POST_beadmitted

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 3 in row 4
putexcel A4 = ("Be admitted") 
putexcel B4 = (`r(mu_2)') 
putexcel C4 = (`r(mu_1)')
putexcel D4 = (diff)
putexcel E4 = (-r(t))
putexcel F4 = (`r(p)')

// Perform fourth t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_fitin==POST_fitin

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 3 in row 4
putexcel A5 = ("Fit in") 
putexcel B5 = (`r(mu_2)')
putexcel C5 = (`r(mu_1)')
putexcel D5 = (diff)
putexcel E5 = (-r(t))
putexcel F5 = (`r(p)')



******** Unpaired, unequal variance ***************************

putexcel A11 = ("Variable") 
putexcel B11 = ("Mean Post")  
putexcel C11 = ("Mean Pre")
putexcel D11 = ("Difference")
putexcel E11 = ("t-stat")
putexcel F11 = ("P-value")

// Perform first t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_welcoming==POST_welcoming, unpaired unequal

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 1 in row 2
putexcel A12 = ("Welcoming")
putexcel B12 = (`r(mu_2)')
putexcel C12 = (`r(mu_1)')
putexcel D12 = (diff)
putexcel E12 = (-r(t))
putexcel F12 = (`r(p)')
// Perform second t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_believesucceed==POST_believesucceed, unpaired unequal

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 2 in row 3
putexcel A13 = ("Succeed")
putexcel B13 = (`r(mu_2)')
putexcel C13 = (`r(mu_1)')
putexcel D13 = (diff)
putexcel E13 = (-r(t))
putexcel F13 = (`r(p)')

// Perform third t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_beadmitted==POST_beadmitted, unpaired unequal

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 3 in row 4
putexcel A14 = ("Be admitted") 
putexcel B14 = (`r(mu_2)') 
putexcel C14 = (`r(mu_1)')
putexcel D14 = (diff)
putexcel E14 = (-r(t))
putexcel F14 = (`r(p)')

// Perform fourth t-test, access saved results, peform calculations for difference of means, standard errors, and the standard error of the difference.
ttest PRE_fitin==POST_fitin, unpaired unequal

scalar diff=r(mu_2)-r(mu_1)
scalar se_1=r(sd_1)/sqrt(r(N_1))
scalar se_2=r(sd_2)/sqrt(r(N_2))
scalar se_diff=sqrt(se_1^2 + se_2^2)

// This block enters data for variable 3 in row 4
putexcel A15 = ("Fit in") 
putexcel B15 = (`r(mu_2)')
putexcel C15 = (`r(mu_1)')
putexcel D15 = (diff)
putexcel E15 = (-r(t))
putexcel F15 = (`r(p)')

************************************************************
*******************Other Data*******************************


*merging other sections 3 and 4-- event rating

merge 1:1 ID using section3.dta
rename _merge merge1
merge 1:1 ID using section4.dta

tabstat eventrating campustourrating academicrating dininghallrating dormrating roundtablerating, statistics(mean n p50 p75 p25 min max) columns(statistics)

ttest roundtablerating==eventrating
*this is interesting. the roundtable had statistically significant(@95%) greater
**rating than the whole event was rated. Based on ordinal values




***** Plan apply data *****
tab PRE_planapply POST_planapply, cell missing
tab PRE_planapply
tab POST_planapply

*just looking at data
tab eventuseful
tab campustouruseful
tab academicuseful
tab dininghalluseful
tab dormuseful
tab roundtableuseful

*post responses predicting overall event rating
probit eventuseful POST_welcoming POST_believesucceed POST_beadmitted POST_fitin 

estat summarize


sort PRE_welcoming
**12 have no data for any of the post survey. but one did fill out the qualitative
**11 have no data for any of the pre survey

tabstat PRE_welcoming POST_welcoming PRE_believesucceed POST_believesucceed PRE_beadmitted POST_beadmitted PRE_fitin POST_fitin, statistics(mean n  p50 p75 p25 min max ) columns(statistics)
*** lets get a difference of post and pre means dropped into this table!!!


*************************************************************
* LOOKING AT MISSING DATA
*************************************************************
misstable summarize PRE_welcoming PRE_believesucceed PRE_beadmitted PRE_fitin PRE_planapply POST_welcoming POST_believesucceed POST_beadmitted POST_fitin POST_planapply eventrating campustourrating academicrating dininghallrating roundtablerating eventuseful campustouruseful academicuseful dininghalluseful dormuseful roundtableuseful

misstable pattern PRE_welcoming PRE_believesucceed PRE_beadmitted PRE_fitin PRE_planapply POST_welcoming POST_believesucceed POST_beadmitted POST_fitin POST_planapply eventrating campustourrating academicrating dininghallrating roundtablerating eventuseful campustouruseful academicuseful dininghalluseful dormuseful roundtableuseful




************************************************************************
************************************************************************
************************************************************************



* THE ANALYSIS BELOW WAS DONE AFTER THAT ABOVE BUT IS IN THE FIRST HALF
* OF THE WRITE UP, IN ORDER TO CONTEXTUALIZE THE SCHOOLS THAT STUDENTS
* OF THE EVENT COME FROM



************************************************************************
************************************************************************
************************************************************************




********** For showing freshman underrep trends at UM**********
import excel "UM_new_freshman_trends.xlsx", sheet("freshtrendstata") firstrow clear

drop D J N S T U

twoway (connected Underreppct Term, sort) (connected Blackpct Term) (connected Latinopct Term) (connected AmInpct Term)
*use ytitle( ) xtitle( ) for title names
** label each point on the graph with it's percent, change the y axis values to percent
gen Underrep = Underreppct * 100
gen Black = Blackpct * 100
gen Latino = Latinopct * 100
gen AmIn = AmInpct * 100
format %4.1f Underrep Black Latino AmIn

graph twoway (connected Underrep Term, sort) (connected Black Term) (connected Latino Term) (connected AmIn Term), ylabel(0(4)16) ymtick(2(2)16) ymtick(,grid) ylabel(, format(%2.0f)) ytitle(Percent of Freshman Class) xtitle(Fall Term) title(Underrepresented Freshman Enrollment)
graph save umadmissions, replace
*want to mlabel(Underrep) mlabposition(12) ONLY EVERY 5 VALUES...
*Graph edited variable names; Legend: down first, Black color: cranberry,


************************ MCAC School Data****************
*Race
import excel "MCAC_schools_race.xlsx", sheet("Fall Bldg K-12 Total Data") firstrow clear
rename Hisp Latino
gen Underrep = Black + Latino + AmIn
foreach race in Black Latino AmIn Underrep {
    drop `race'p
	gen `race'p = `race'/tot_all
}
rename BCODE StateSchoolID
rename DCODE StateDistrictID
rename BNAME SchoolName
rename DNAME District
save schoolrace.dta, replace

*looking at data
tabstat Blackp Latinop AmInp Underrepp, statistics(mean sd p50)

*Free/reduced lunch

import excel "ncesdata2014.xlsx", sheet(ncesdata_D2072BA9) clear
drop in 1/14
export excel using "nces2014.xlsx", replace

import excel "nces2014.xlsx", firstrow clear

save ncesMI.dta, replace
use ncesMI.dta, replace
destring Students Teachers StudentTeacherRatio FreeLunch ReducedLunch,replace force
drop Flunchr FRlunchr

gen FRlunch = (FreeLunch+ReducedLunch)/Students
gen Flunch = FreeLunch/Students

reg Flunch Students Teachers
* trial regression

*looking at data
tabstat Flunch FRlunch, statistics(mean sd p50 skew)


merge 1:m StateSchoolID using schoolrace.dta
* 7 percent of data unsuccessfully merged

list SchoolName District if _merge == 1 | 2


reg FRlunch Blackp Latinop AmInp
outreg2 using 567project.doc, nolabel replace alpha (.001, .01, .05, .1) symbol (***,**,*, ~)
reg FRlunch Blackp Latinop AmInp StudentTeacherRatio
outreg2 using 567project.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)
reg FRlunch Blackp Latinop AmInp StudentTeacherRatio Students
outreg2 using 567project.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)


reg Flunch Blackp Latinop AmInp
outreg2 using 567project1.doc, nolabel replace alpha (.001, .01, .05, .1) symbol (***,**,*, ~)
reg Flunch Blackp Latinop AmInp StudentTeacherRatio
outreg2 using 567project1.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)
reg Flunch Blackp Latinop AmInp StudentTeacherRatio Students
outreg2 using 567project1.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)

predict pred_Flunch

list Flunch pred_Flunch in 1/10
* schools have pretty notable differences between predicted values and actual
* for free lunch rate


rvfplot
graph save residualvsfitted, replace
rvpplot Blackp
graph save residualvspredictorBlack, replace
rvpplot AmInp
graph save residualvspredictorAmIn, replace


*, mlabel(SchoolName) msize(vtiny)

reg FRlunch Black Latino AmIn StudentTeacherRatio Students
outreg2 using 567project.doc, nolabel replace alpha (.001, .01, .05, .1) symbol (***,**,*, ~)
reg FRlunch Black Latino AmIn StudentTeacherRatio Students, robust
outreg2 using 567project.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)


reg Flunch Blackp Latinop AmInp, robust
outreg2 using 567project1.doc, nolabel replace alpha (.001, .01, .05, .1) symbol (***,**,*, ~)
reg Flunch Blackp Latinop AmInp StudentTeacherRatio, robust
outreg2 using 567project1.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)

encode Locale, generate(urbanicity)

reg Flunch Blackp Latinop AmInp StudentTeacherRatio Students i.urbanicity, cluster(ZIP)
outreg2 using 567project1.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)
reg Flunch Blackp Latinop AmInp StudentTeacherRatio Students i.urbanicity, robust
outreg2 using 567project1.doc, nolabel append alpha (.001, .01, .05, .1) symbol (***,**,*, ~)

label list urbanicity
* N is missing from the regression becasue there is no data for schools with N value

twoway (scatter Flunch Blackp) (lfit Flunch Blackp), ytitle(School Free Lunch Rate) xtitle(Proportion of Black Students) legend(off) title(OLS Fit Proportion Black)
graph save samplefit, replace

tabstat Flunch FRlunch Blackp Latinop AmInp Underrepp, statistics(mean sd p50 max min skew) col(statistics) save
estout r(StatTotal) using "statsall.doc", replace

* drop all but MCAC schools, do some comparison, be done.

gen MCACSchools = 1 if StateSchoolID== "00223" | StateSchoolID== "00286" | StateSchoolID=="01044"| StateSchoolID=="01697"| StateSchoolID=="01865"| StateSchoolID=="01870"| StateSchoolID=="02652"| StateSchoolID=="02777"| StateSchoolID=="03197"| StateSchoolID=="02756"| StateSchoolID=="03092"| StateSchoolID=="03336"| StateSchoolID=="03554"
keep if MCACSchools == 1

tabstat Flunch FRlunch Blackp Latinop AmInp Underrepp, statistics(mean sd p50 max min skew) col(statistics) save
estout r(StatTotal) using "stats.doc", replace

twoway (scatter Flunch Blackp, mlabel(SchoolName)) (lfit Flunch Blackp), ytitle(School Free Lunch Rate) xtitle(Proportion of Black Students) legend(off) title(OLS Fit MCAC Proportion Black) 
graph save mcacfit, replace

