*===============================================================================
* DATA AVAILABILITY
* -----------------------------
* This study uses the RESTRICTED-USE National Longitudinal Study of Adolescent
* to Adult Health (Add Health), Waves I, II, and IV, together with the Add
* Health family/sibling identifier files. Under the Add Health restricted-use
* data-use agreement, the raw data CANNOT be redistributed and are therefore
* NOT included in this replication package. Researchers may obtain the data
* directly from the Carolina Population Center:
*   https://addhealth.cpc.unc.edu/data/   (restricted-use contract required)
*===============================================================================

clear all
set more off
version 17

* Set the working directory that holds the restricted-use working file.
* global root "C:/path/to/your/AddHealth/workdir"
* cd "$root"

use "addhealth_raw.dta", clear     // merged restricted-use working file 

*===============================================================================
* 1. VARIABLE CONSTRUCTION  
*===============================================================================

*-------------------------------------------------------------------------------
* 1.1 Identifiers
*-------------------------------------------------------------------------------
* famid (family id) and fsample (full-sibling analytic-sample indicator) are
* merged in from the Add Health sibling-pairs identifier file.

*-------------------------------------------------------------------------------
* 1.2 Demographics: gender, age, race/ethnicity                (Wave I)
*-------------------------------------------------------------------------------
* Gender (bio_sex): male / female
rename bio_sex gender
gen male=gender
replace male=0 if gender==2
replace male=. if gender==6|gender==8   
gen female=1-male

* Age at Wave I = interview year - birth year
rename iyear interviewyear
rename h1gi1y birthyear
gen age_w1=interviewyear-birthyear
replace age_w1=. if age_w1==-1

* Race/ethnicity 
rename h1gi4 hispanic
replace hispanic=0 if hispanic==6|hispanic==8|hispanic==9
rename h1gi6a white
replace white=0 if white==6|white==8|white==9
rename h1gi6b black
replace black=0 if black==6|black==8|black==9
rename h1gi6d asian
replace asian=0 if asian==6|asian==8|asian==9
replace white=0 if black==1
replace white=0 if hispanic==1
replace hispanic=0 if black==1

* Single categorical race/ethnicity variable and its dummy set
gen racecat=.
replace racecat=0 if white==1    & racecat==.
replace racecat=1 if black==1    & racecat==.
replace racecat=2 if hispanic==1 & racecat==.
replace racecat=3 if asian==1    & racecat==.
replace racecat=4 if racecat==.
label def racecatL 0 "White" 1 "Black" 2 "Hispanic" 3 "Asian" 4 "Other"
label val racecat racecatL
gen race_white    = (racecat==0)
gen race_black    = (racecat==1)
gen race_hispanic = (racecat==2)
gen race_asian    = (racecat==3)
gen race_other    = (racecat==4)

*-------------------------------------------------------------------------------
* 1.3 Family background and birth order                        (Wave I + parent)
*-------------------------------------------------------------------------------
* Co-residence with both biological parents
rename pc2 livebiomom_w1
recode livebiomom_w1 6=. 7=1
rename pc6b livebiodad_w1
recode livebiodad_w1 6=. 7=1
gen bothpar_w1=1 if livebiomom_w1==1 & livebiodad_w1==1
replace bothpar_w1=0 if bothpar_w1~=1
replace bothpar_w1=. if livebiomom_w1==.|livebiodad_w1==.

* Number of siblings 
rename h1hr14 numsibs_w1
recode numsibs_w1 96=. 98=. 99=.
replace numsibs_w1=numsibs_w1-1

* First-born status
local z=11
foreach w in a b c d e f g h i j k l m n o p q r {
    recode h1hr7`w' 996=. 997=. 998=. 999=.
    gen ssiblingage`z'=h1hr7`w' if h1hr3`w'==5 | h1hr3`w'==8
    local z=`z'+1
}
egen oldestage=rowmax(ssiblingage*)
gen firstborn=0
replace firstborn=1 if oldestage<age_w1

* Rural status 
gen location_w1=h1ir12
replace location_w1=. if location_w1>6
gen rural=1 if location_w1==1
replace rural=0 if location_w1~=1 & location_w1~=.

* Family income, in $10,000s (rescaled by /100 as in original build)
gen faminc_w1=pa55
replace faminc_w1=. if faminc_w1>9000
replace faminc_w1=faminc_w1/100

* Maternal education in years (recoded to years of schooling)
gen momeduc_w1=h1rm1
recode momeduc_w1 1=8 2=11 3=11 4=12 5=12 6=13 7=14 8=16 9=17 ///
       10=0 11=. 12=. 17=. 97=. 98=. 99=. 96=.

* Standardized Picture Vocabulary Test score 
gen pvtscore_w1=ah_pvt
egen pvt_std=std(pvtscore_w1)

*-------------------------------------------------------------------------------
* 1.4 Independent variable: Learning disability                (Wave I, parent)
*-------------------------------------------------------------------------------
gen ld_w1=pc38
recode ld_w1 6/8=.

*-------------------------------------------------------------------------------
* 1.5 Dependent variable: Years of schooling completed         (Wave IV)
*-------------------------------------------------------------------------------
gen educyrs_w4=h4ed2
recode educyrs_w4 1=8 2=10 3=12 4=12 5=12 6=13 7=15 8=16 9=17 ///
       10=17 11=19 12=18 13=18 96=. 98=.

*-------------------------------------------------------------------------------
* 1.6 Mechanism variables                                      (Wave II)
*-------------------------------------------------------------------------------
* (a) Educational aspirations and expectations
gen collaspire_w2=h2ee1          // college aspirations (want to go)
gen collexpect_w2=h2ee2        // college expectations (likely to go)
recode collaspire_w2 collexpect_w2 (6/max=.)

* (b) Educational effort
gen skipdays_w2=h2ed2                 // days skipped school without an excuse
recode skipdays_w2 (900/max=.)
gen truancy_w2=skipdays_w2                // truancy (0=none, 1=any)
recode truancy_w2 (2/max=1)
gen diffattn_w2=h2ed12             // difficulty paying attention in school
recode diffattn_w2 (6/max=.)
gen diffhw_w2=h2ed13              // difficulty getting homework done
recode diffhw_w2 (6/max=.)

* (c) School-based relationships
gen diffteach_w2=h2ed11         // difficulty getting along with teachers
recode diffteach_w2 (6/max=.)
gen diffpeers_w2=h2ed14         // difficulty getting along with other students
recode diffpeers_w2 (6/max=.)
* School attachment: alpha scale of close/part-of/happy-at school
foreach i of numlist 15 16 18 {
    gen h2ed`i'r=h2ed`i'
    recode h2ed`i'r (6/max=.)
    replace h2ed`i'r=6-h2ed`i'r
}
gen schclose_w2=h2ed15r
gen schpart_w2=h2ed16r
gen schhappy_w2=h2ed18r
alpha schclose_w2 schpart_w2 schhappy_w2, item gen(schattach_w2)

* Full-sample indicator used in OLS column 1 of Table 2
gen all=1

*-------------------------------------------------------------------------------
* 1.7 Variable labels
*-------------------------------------------------------------------------------
label var educyrs_w4 "Years of schooling completed"
label var ld_w1         "Learning disabilities"
label var collaspire_w2 "College aspirations"
label var collexpect_w2 "College expectations"
label var truancy_w2       "Truancy"
label var diffattn_w2     "Difficulty in paying attention in school"
label var diffhw_w2      "Difficulty in getting homework done"
label var schattach_w2     "School attachment"
label var diffpeers_w2 "Difficulty getting along with other students"
label var diffteach_w2 "Difficulty getting along with teachers"
label var age_w1        "Age"
label var female       "Girl"
label var pvt_std  "Standardized PVT score"
label var firstborn   "First-born status"
label var numsibs_w1    "Number of siblings"
label var momeduc_w1 "Maternal education"
label var faminc_w1  "Family income"
label var bothpar_w1 "Co-residence with both biological parents"
label var rural        "Rural status"

*===============================================================================
* 2. ANALYTIC SAMPLE AND MULTIPLE IMPUTATION
*===============================================================================

* Analytic-sample indicator: respondents with observed (non-imputed) values on
* both the dependent variable (years of schooling, Wave IV) and the independent
* variable (learning disability, Wave I).
gen insample = (educyrs_w4<. & ld_w1<.)

* ---- Multiple imputation by chained equations (10 imputations) ---------------
mi set wide

mi register imputed                                                          ///
    age_w1 pvt_std firstborn numsibs_w1 momeduc_w1 faminc_w1         ///
    bothpar_w1 rural ld_w1 educyrs_w4                                     ///
    collaspire_w2 collexpect_w2 schattach_w2 diffpeers_w2      ///
    diffteach_w2 truancy_w2 diffattn_w2 diffhw_w2
mi register regular female race_black race_hispanic race_asian race_other

mi impute chained                                                            ///
    (pmm, knn(3)) age_w1 pvt_std numsibs_w1 momeduc_w1 faminc_w1      ///
                  educyrs_w4 collaspire_w2 collexpect_w2 schattach_w2           ///
                  diffpeers_w2 diffteach_w2 diffattn_w2 diffhw_w2                  ///
    (logit) firstborn bothpar_w1 rural ld_w1 truancy_w2     ///
    = female race_black race_hispanic race_asian race_other                          ///
    , add(10) augment force noisily

*===============================================================================
* 3. ANALYSIS
*===============================================================================

* ---- Covariate sets ----------------------------------------------------------
* Full control set (OLS): individual- + family-level covariates.
global control  age_w1 female race_black race_hispanic race_asian race_other         ///
                pvt_std firstborn numsibs_w1                              ///
                momeduc_w1 faminc_w1 bothpar_w1 rural
* Sibling fixed-effects control set: only covariates that vary within family.
global controlf age_w1 female pvt_std firstborn

* Mechanism domains (Wave II).
global eduexpect collaspire_w2 collexpect_w2
global social    schattach_w2 diffpeers_w2 diffteach_w2
global effort    truancy_w2 diffattn_w2 diffhw_w2

*-------------------------------------------------------------------------------
* TABLE 1. Descriptive statistics, by gender (no imputed values)
*-------------------------------------------------------------------------------
global descr educyrs_w4 ld_w1                                               ///
             collaspire_w2 collexpect_w2                                      ///
             truancy_w2 diffattn_w2 diffhw_w2                                          ///
             schattach_w2 diffpeers_w2 diffteach_w2                              ///
             age_w1 female race_white race_black race_hispanic race_asian race_other   ///
             pvt_std firstborn numsibs_w1                                 ///
             momeduc_w1 faminc_w1 bothpar_w1 rural

* Means / SDs for the total sibling sample and by gender.
estimates clear
estpost summarize $descr if insample==1 & fsample==1
est store T
estpost summarize $descr if insample==1 & fsample==1 & female==1
est store G
estpost summarize $descr if insample==1 & fsample==1 & male==1
est store B
esttab T G B using "Table1_descriptives.rtf", replace compress label nonumber ///
    mtitle("Total" "Girls" "Boys")                                           ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")                  ///
    addnote("Summary statistics contain no imputed values.")

* Gender differences.
estpost ttest $descr if insample==1 & fsample==1, by(female)
esttab using "Table1_ttest_p.rtf", replace wide nonumber cells(p(fmt(3)))
foreach var of varlist ld_w1 firstborn bothpar_w1 rural racecat {
    tabulate `var' female if insample==1 & fsample==1, chi2
}

*-------------------------------------------------------------------------------
* TABLE 2. LD (Wave I) and educational attainment (Wave IV)
*-------------------------------------------------------------------------------
estimates clear
eststo: mi estimate, post: reg   educyrs_w4 ld_w1 $control  if all==1     & insample==1
eststo: mi estimate, post: reg   educyrs_w4 ld_w1 $control  if fsample==1 & insample==1
eststo: mi estimate, post: xtreg educyrs_w4 ld_w1 $controlf if fsample==1 & insample==1, ///
        i(famid) fe cluster(famid)
esttab using "Table2_main.rtf", replace compress label nogaps depvars         ///
    b(3) se(3) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) nobase                   ///
    stats(N, fmt(0) labels("N (individuals)"))                                ///
    mtitle("OLS: Full" "OLS: Sibling" "Sibling FE")

*-------------------------------------------------------------------------------
* TABLE 3. LD and educational attainment, by gender (sibling FE)
*-------------------------------------------------------------------------------
estimates clear
eststo: mi estimate, post: xtreg educyrs_w4 ld_w1 $controlf if female==1 & fsample==1 & insample==1, ///
        i(famid) fe cluster(famid)
eststo: mi estimate, post: xtreg educyrs_w4 ld_w1 $controlf if male==1   & fsample==1 & insample==1, ///
        i(famid) fe cluster(famid)
eststo: mi estimate, post: xtreg educyrs_w4 i.ld_w1##i.male $controlf if fsample==1 & insample==1, ///
        i(famid) fe cluster(famid)
esttab using "Table3_by_gender.rtf", replace compress label nogaps depvars     ///
    b(3) se(3) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) nobase                    ///
    stats(N, fmt(0) labels("N (individuals)"))                                 ///
    mtitle("Girls" "Boys" "Pooled (interaction)")

*-------------------------------------------------------------------------------
* TABLE 4. LD (Wave I) and mechanism variables (Wave II), GIRLS only (sibling FE)
*-------------------------------------------------------------------------------
estimates clear
foreach out in $eduexpect $effort $social {
    eststo: mi estimate, post: xtreg `out' ld_w1 $controlf if female==1 & fsample==1 & insample==1, ///
            i(famid) fe cluster(famid)
}
esttab using "Table4_mechanisms_girls.rtf", replace compress label nogaps      ///
    keep(ld_w1) b(3) se(3) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) nobase         ///
    stats(N, fmt(0) labels("N (individuals)"))                                 ///
    mtitle("Want" "Likely" "Truancy" "Attention" "Homework"                    ///
           "Attachment" "Students" "Teachers")

*-------------------------------------------------------------------------------
* TABLE 5. Sobel mediation tests, GIRLS only
*-------------------------------------------------------------------------------

local COND female==1 & insample==1 & fsample==1

quietly mi estimate, post: xtreg educyrs_w4 ld_w1 $controlf if `COND', i(famid) fe cluster(famid)
local c = _b[ld_w1]
di _newline as txt "Total effect (c, girls) = " as res %6.3f `c'
di _newline as txt %-22s "Mechanism" %10s "indirect" %8s "SE" %8s "p" %8s "%med" %7s "ES"

* ---- 5.1 Univariate ----------------------------------------------------------
local meds collaspire_w2 collexpect_w2 truancy_w2 diffattn_w2 diffhw_w2 schattach_w2 diffpeers_w2 diffteach_w2
foreach m of local meds {
    quietly mi estimate, post: xtreg `m' ld_w1 $controlf if `COND', i(famid) fe cluster(famid)
    local a  = _b[ld_w1]
    local sa = _se[ld_w1]
    quietly mi estimate, post: xtreg educyrs_w4 ld_w1 $controlf `m' if `COND', i(famid) fe cluster(famid)
    local b  = _b[`m']
    local sb = _se[`m']
    local ind = (`a')*(`b')
    local se  = sqrt((`a')^2*(`sb')^2 + (`b')^2*(`sa')^2)
    local p   = 2*(1-normal(abs(`ind'/`se')))

    * --- kappa-squared --------------------------------------------------------
    local ksum = 0
    forvalues imp = 1/10 {
        capture drop _rx _rm _ry
        quietly areg _`imp'_ld_w1      _`imp'_age_w1 _`imp'_pvt_std _`imp'_firstborn if `COND', absorb(famid)
        quietly predict _rx if e(sample), dresiduals
        quietly areg _`imp'_`m'        _`imp'_age_w1 _`imp'_pvt_std _`imp'_firstborn if `COND', absorb(famid)
        quietly predict _rm if e(sample), dresiduals
        quietly areg _`imp'_educyrs_w4 _`imp'_age_w1 _`imp'_pvt_std _`imp'_firstborn if `COND', absorb(famid)
        quietly predict _ry if e(sample), dresiduals
        quietly corr _rx _rm _ry
        matrix C = r(C)
        local rxm = C[2,1]
        local rxy = C[3,1]
        local rmy = C[3,2]
        local ksum = `ksum' + abs((`rmy'-`rxm'*`rxy')/sqrt((1-`rxm'^2)*(1-`rxy'^2)))
    }
    capture drop _rx _rm _ry
    local kap = `ksum'/10

    di as res %-22s "`m'" %10.3f `ind' %8.3f `se' %8.3f `p' %8.1f 100*`ind'/`c' %7.2f `kap'
}

* ---- 5.2 Multivariate, by mechanism domain (joint b-paths) -------------------
foreach dom in eduexpect social effort {
    quietly mi estimate, post: xtreg educyrs_w4 ld_w1 $controlf ${`dom'} if `COND', i(famid) fe cluster(famid)
    matrix Vb = e(V)
    local k = 0
    foreach m of varlist ${`dom'} {
        local ++k
        local mn`k' "`m'"
        local b`k'  = _b[`m']
        local cb`k' = colnumb(Vb,"`m'")
    }
    local K = `k'
    forvalues i = 1/`K' {
        quietly mi estimate, post: xtreg `mn`i'' ld_w1 $controlf if `COND', i(famid) fe cluster(famid)
        local a`i'  = _b[ld_w1]
        local va`i' = (_se[ld_w1])^2
    }
    local ind = 0
    forvalues i = 1/`K' {
        local ind = `ind' + (`a`i'')*(`b`i'')
    }
    local var = 0
    forvalues i = 1/`K' {
        local var = `var' + (`b`i'')^2*(`va`i'') + (`a`i'')^2*(el(Vb,`cb`i'',`cb`i''))
        forvalues j = 1/`K' {
            if `j' > `i' local var = `var' + 2*(`a`i'')*(`a`j'')*(el(Vb,`cb`i'',`cb`j''))
        }
    }
    local se = sqrt(`var')
    local p  = 2*(1-normal(abs(`ind'/`se')))

    di as res %-22s "[domain] `dom'" %10.3f `ind' %8.3f `se' %8.3f `p' %8.1f 100*`ind'/`c'
}

*===============================================================================
* END OF FILE
*===============================================================================

