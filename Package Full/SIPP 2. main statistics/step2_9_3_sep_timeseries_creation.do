********************************************************************************
*
* Copyright 2023 Ludo Visschers and Carlos Carrillo-Tudela
*
*
* Redistribution and use in source and binary forms, with or without 
*	modification, are permitted provided that the following conditions are met:
*
* 1. Redistributions of source code must retain the above copyright notice, 
*	this list of conditions and the following disclaimer.
*
* 2. Redistributions in binary form must reproduce the above copyright notice, 
*	this list of conditions and the following disclaimer in the documentation 
*	and/or other materials provided with the distribution.
*
* 3. Neither the name of the copyright holder nor the names of its contributors 
*	may be used to endorse or promote products derived from this software 
*	without specific prior written permission.
*
* 4. If using this code or its results (whole or in part, with or without 
* 	modifications) in academic work, please cite:  
*		Carrillo-Tudela, Carlos and Ludo Visschers, "Unemployment and Endogenous 
*		Reallocation over the Business Cycle" 
*	in its published version, in the references of the publications associated 
*	with aforementioned academic work.
*
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*

********************************************************************************




//===================================================
// PREAMBLE 
//==================================================

clear all
set maxvar 20000
version 13

cd "$workingdir"
global workingdir `c(pwd)'
do "${workingdir}/global_paths.do"
cd "${tempdata}"

set more off
set varabbrev off
*set trace off 



global lstarttime=c(current_time)
global lstartdate=c(current_date)
display "started at ${lstarttime} on ${lstartdate}"
	


global sttg = "0"
global epanbwidth=3


global wavecond " & wave>4 & interview_no2>14 & sample_timetogo>=0"
global wavecond " & wave>5 & interview_no2>20 & sample_timetogo>=0"
global mwavecond " & wave>1 & interview_no2>4 & sample_timetogo>=0"
	* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
global jfwavecond "& wave>1 & interview_no2>4 & sample_timetogo>18-n_spellength "	// this 
global sttg "1"
global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "
global noagric "  locc1bfr_mmo!=45 & locc1aft_mmo!=45 "


//====================================
// PROGRAMS
//====================================
capture program drop star_exe
program star_exe 
	args xlscol xlsrow table_pos matrixname


	
	
if "`matrixname'"=="" {
	matrix rtablel=r(table)
}
if "`matrixname'"!="" {
	matrix rtablel=`matrixname'
}

*matrix list rtablel
*display "`table_pos'"
local pval=rtablel[4,`table_pos']
local coeffx=rtablel[1,`table_pos']
local stderrx=rtablel[2, `table_pos']
*display "`pval'"
if `pval'<=0.1 & `pval'>0.05 {
local star="*" 
}	
if `pval'>0.01 & `pval'<=0.05 {
local star="**" 
}	
if `pval'<=0.01 {
local star="***" 
}	
if `pval'>0.1 {
local star="" 
}	
local coeffx=string(`coeffx', "%8.4f")
local coeffstar="`coeffx'`star'"
local stderr2=string(`stderrx', "%8.4f")
*display "`stderr2'"
local stderr3="(`stderr2')"
display "`coeffstar'"
display "`stderr3'"


putexcel `xlscol'`xlsrow'=("`coeffstar'")
local xlsrowplus=`xlsrow'+1
putexcel `xlscol'`xlsrowplus'=("`stderr3'")

end


capture program drop tstar_exe
program tstar_exe   // COLUMN FIRST, THEN ROW 
	args xlscol xlsrow rtable_pos matrixname
	
if "`matrixname'"=="" {
	matrix localrtablexxxx=r(table)
}
if "`matrixname'"!="" {
	matrix localrtablexxxx=`matrixname'
}

*matrix list rtable
*display "`table_pos'"
local pval=localrtablexxxx[4,`rtable_pos']
local coeffx=localrtablexxxx[1,`rtable_pos']
local stderrx=localrtablexxxx[2, `rtable_pos']
*display "`pval'"
if `pval'<=0.1 & `pval'>0.05 {
local star="*" 
}	
if `pval'>0.01 & `pval'<=0.05 {
local star="**" 
}	
if `pval'<=0.01 {
local star="***" 
}	
if `pval'>0.1 {
local star="" 
}	
local coeffx=string(`coeffx', "%8.4f")
local coeffstar="`coeffx'`star'"
local stderr2=string(`stderrx', "%8.4f")
*display "`stderr2'"
local stderr3="(`stderr2')"
display "`coeffstar'"
display "`stderr3'"

tokenize "`c(alpha)'"

putexcel ``xlscol''`xlsrow'=("`coeffstar'")
local xlscolplus=`xlscol'+1

putexcel ``xlscolplus''`xlsrow'=("`stderr3'")

end


/*
** auxiliary programs
do "${codedir}aux_programs.do"
			/* programs:
					xsnet_calcx
					occmat_dim_exe	
			*/
** correction matrices
do "${codedir}Ginv matrices.do"
*/

 
use "${outputdata}/corewave_occlfmin_ctv.dta", clear

replace locc1bfr_rtmm=3 if locc1bfr_mmo==53
replace locc1aft_rtmm=3 if locc1aft_mmo==53

**IMPORT BLS EU, EN rates
merge m:1 quarter using "${workingdir}/Aggregate Data/jf_sepu_bls.dta"

/*
merge m:1 quarter using "C:\data\aggdata_2018.dta", gen(_maggdate)

capture drop merge_qtr
merge m:1 quarter using "${datadir1}aggdata_u_y_may2017.dta", keepusing(hp*) gen(merge_qtr)
drop if merge_qtr==2


capture drop merge_qtru
merge m:1 quarter using "C:\Users\lviss\Dropbox\CTV_Revisions\Aggregate Data Updated\aggdata_4merge.dta", keepusing(unrate) gen(merge_qtru)
drop if merge_qtru==2
drop merge_qtru
*/


capture drop mm_clsfication_dum
gen mm_clsfication_dum =0 if panel<=2001
replace mm_clsfication_dum=1 if panel>=2002

capture drop all_clsfication_dum
gen all_clsfication_dum=0 if panel<=1991
replace all_clsfication_dum=1 if panel<=2001 & panel>1991
replace all_clsfication_dum=2 if panel<=2008 & panel>2001

capture drop educ2
gen educ2=educ
replace educ2=4 if educ==5


// AGE  (with wavecondition)
capture drop age_2dum
gen age_2dum=1 if tage>=20 & tage<=30
replace age_2dum=2 if tage>=35 & tage<=55


capture drop entry_ind
			gen byte entry_ind=0
			replace entry_ind=1 if max_educ==1 & tage>18
			replace entry_ind=1 if max_educ==2 & tage>18
			replace entry_ind=1 if max_educ==3 & tage>22
			replace entry_ind=1 if max_educ==4 & tage>22
			replace entry_ind=1 if max_educ==5 & tage>24
			

capture drop entry_ind_old
ren entry_ind entry_ind_old
capture drop entry_ind
gen entry_ind=1 if educ<=2 & tage>=20
replace entry_ind=1 if educ>=3 & educ<=5 & tage>=24

*** RECESSION INDICATORS
capture drop recession_yr
gen recession_yr=0
replace recession_yr=1 if year==1990
replace recession_yr=1 if year==1991
replace recession_yr=1 if year==2001
replace recession_yr=1 if year==2008
replace recession_yr=1 if year==2009


/*
**** GOOD TIMES BAD TIMES INDICATOR
su hp_lunrate, detail
sort quarter
capture drop hp_lunrate_temp
count if hp_lunrate==. & quarter!=quarter[_n-1]
gen hp_lunrate_temp=hp_lunrate if quarter!=quarter[_n-1]
_pctile hp_lunrate_temp, p(25 75)
local low_upct=r(r1)
local high_upct=r(r2)
capture drop rec_exp_ind
gen byte rec_exp_ind=0
replace rec_exp_ind=-1 if hp_lunrate>=`high_upct'
replace rec_exp_ind=1 if hp_lunrate<=`low_upct'

sort personkey yearmonth
capture drop hp_lunrate_temp

su hp_lunrate, detail
sort quarter
capture drop hp_lunrate_temp
count if hp_lunrate==. & quarter!=quarter[_n-1]
gen hp_lunrate_temp=hp_lunrate if quarter!=quarter[_n-1]
_pctile hp_lunrate_temp, p(33 67)
local low_upct=r(r1)
local high_upct=r(r2)
capture drop rec_exp_ind3
gen byte rec_exp_ind3=1
replace rec_exp_ind3=0 if hp_lunrate>=`high_upct'
replace rec_exp_ind3=2 if hp_lunrate<=`low_upct'

sort personkey yearmonth
capture drop hp_lunrate_temp
*/

/*
capture drop agegroup
gen agegroup=1 if tage>=22 & tage<=25
replace agegroup=2 if tage>=26 & tage<=30
replace agegroup=3 if tage>=31 & tage<=35
replace agegroup=4 if tage>=36 & tage<=40
replace agegroup=5 if tage>=41 & tage<=45
replace agegroup=6 if tage>=46 & tage<=50
replace agegroup=7 if tage>=51 & tage<=55
replace agegroup=8 if tage>=55 & tage<=60
replace agegroup=9 if tage>=61 & tage<=65
*/
label define label_mm  ///
 11 "Mgt occs" ///
 13 "Bus&Fin Operations"  ///
 15 "Computer and math. occ"  ///
 17 "architect & eng. occ"  ///
 19 "Life, phys, and socsci occ"  ///
 21 "Comm & soc service occ"  ///
 23 "Legal"  ///
 25 "Educ, training, and library"  ///
 27 "Arts/Dsgn/entrtmnt/sports/media"  ///
 29 "Healthcare pract & tech occs"  ///
 31 "Healthcare support"  ///
 33 "Protective service"  ///
 35 "Food prep/serving & rel."  ///
 37 "Building/grounds clean&maint."  ///
 39 "Personal care/service occ"   ///
 41 "Sales & rel. occupations"  ///
 43 "Office/Admin Support"  ///
 45 "Farm/Fish/Forestry"  ///
 47 "Construction/Extraction" ///
 49 "Install/Maint/Repair Occ" ///
 51 "Production occupations" ///
 53 "Transportation& mat moving", replace

 lab val locc1bfr_mmo label_mm		
 lab val locc1aft_mmo label_mm			
 
capture drop ms_sum
gen byte ms_sum=1 if ms<=2
replace ms_sum=ms if ms>2
capture drop ms2
gen byte ms2=1 if ms<=2
replace ms2=ms if ms>2


capture drop agebin
gen byte agebin=1 if tage>=20 & tage<=30
replace agebin=2 if tage>=35 & tage<=55



capture drop durdistr_stability
gen byte durdistr_stability=.
replace durdistr_stability=1 if quarter>=100 & quarter<=118
replace durdistr_stability=1 if quarter>=125 & quarter<=142
replace durdistr_stability=1 if quarter>=150 & quarter<=159
replace durdistr_stability=1 if quarter>=169 & quarter<=175
replace durdistr_stability=1 if quarter>=180 & quarter<=190
replace durdistr_stability=1 if quarter>=198 & quarter<=214

// fullseam_qtr_ind: those quarters that have full rotations (and therefore balanced seams), in some panel. nofullseam_ind!=1 is indicating this within the panel
capture drop fullseam_qtr_ind
gen byte fullseam_qtr_ind=.
replace fullseam_qtr_ind=1 if quarter>=95 & quarter<=118
replace fullseam_qtr_ind=1 if quarter>=121 & quarter<=142
replace fullseam_qtr_ind=1 if quarter>=146 & quarter<=158
replace fullseam_qtr_ind=1 if quarter>=166 & quarter<=174
replace fullseam_qtr_ind=1 if quarter>=177 & quarter<=190
replace fullseam_qtr_ind=1 if quarter>=195 & quarter<=213

// using restriction on time in sample (interview_no2), create seams 12 or 14 months later
capture drop fullseam_12lag_qtr_ind
gen byte fullseam_12lag_qtr_ind=.
replace fullseam_12lag_qtr_ind=1 if quarter>=99 & quarter<=118
replace fullseam_12lag_qtr_ind=1 if quarter>=125 & quarter<=142
replace fullseam_12lag_qtr_ind=1 if quarter>=150 & quarter<=158
replace fullseam_12lag_qtr_ind=1 if quarter>=170 & quarter<=174
replace fullseam_12lag_qtr_ind=1 if quarter>=181 & quarter<=190
replace fullseam_12lag_qtr_ind=1 if quarter>=199 & quarter<=213


capture drop fullseam_14lag_qtr_ind
gen byte fullseam_14lag_qtr_ind=.
replace fullseam_14lag_qtr_ind=1 if quarter>=100 & quarter<=118
replace fullseam_14lag_qtr_ind=1 if quarter>=126 & quarter<=142
replace fullseam_14lag_qtr_ind=1 if quarter>=151 & quarter<=158
replace fullseam_14lag_qtr_ind=1 if quarter>=171 & quarter<=174
replace fullseam_14lag_qtr_ind=1 if quarter>=182 & quarter<=190
replace fullseam_14lag_qtr_ind=1 if quarter>=200 & quarter<=213

	**    capture drop firstlastwave
     capture drop firstlastwave
	 gen firstlastwave=0
     capture replace firstlastwave=1 if wave==1|wave==2
     capture replace firstlastwave=1 if wave>=8 & panel==1984             ! 8 waves
     capture replace firstlastwave=1 if wave>=7 & panel==1985             ! 7 waves
     capture replace firstlastwave=1 if wave>=7 & panel==1986             ! 7 waves
     capture replace firstlastwave=1 if wave>=7 & panel==1987             ! 7 waves
     capture replace firstlastwave=1 if wave>=6 & panel==1988             ! 6 waves
     capture replace firstlastwave=1 if wave>=8 & panel==1990             ! 8 waves
     capture replace firstlastwave=1 if wave>=8 & panel==1991             ! 8 waves
     capture replace firstlastwave=1 if wave>=9 & panel==1992             ! 9 waves
     capture replace firstlastwave=1 if wave>=9 & panel==1993             ! 9 waves
     capture replace firstlastwave=1 if wave>=12 & panel==1996            ! 12 waves
     capture replace firstlastwave=1 if wave>=9 & panel==2001             ! 9 waves
     capture replace firstlastwave=1 if wave>=12 & panel==2004            ! 12 waves
     capture replace firstlastwave=1 if wave>=16 & panel==2008             ! 16 waves
	 
	** seams at every month, given firstlastwave!=1
	sort personkey yearmonth
	capture drop seam_interior_flw_ind
	gen seam_interior_flw_ind=0
	replace seam_interior_flw_ind=1 if wave[_n-1]==wave[_n] - 1 & firstlastwave!=1


	sort personkey yearmonth
	capture drop seam_interior_ind
	gen seam_interior_ind=0
	replace seam_interior_ind=1 if wave[_n-1]==wave[_n] - 1 
	
	* BASED ON TAB, WE CAN SEE WHICH QUARTERS HAVE CONSISTENTLY ALL MONTHS A SEAM
	tab yearmonth seam_interior_ind
	
	* use this to indicate those quarters that work
	capture drop seam_interior_all_q_ind
	gen seam_interior_all_q_ind=0
	replace seam_interior_all_q_ind=1 if quarter>=tq(1983q4) & quarter<=tq(2013q1)
	replace seam_interior_all_q_ind=0 if quarter>=tq(1988q1) & quarter<=tq(1988q1)
	replace seam_interior_all_q_ind=0 if quarter>=tq(1989q4) & quarter<=tq(1990q1)
	replace seam_interior_all_q_ind=0 if quarter>=tq(1995q4) & quarter<=tq(1996q1)
	replace seam_interior_all_q_ind=0 if quarter>=tq(1999q4) & quarter<=tq(2000q1)
	replace seam_interior_all_q_ind=0 if quarter>=tq(2003q4) & quarter<=tq(2004q1)
	replace seam_interior_all_q_ind=0 if quarter>=tq(2007q4) & quarter<=tq(2004q3)
	
	tab yearmonth seam_interior_ind [aw=pweight2], row
	* impose that percentage of obs at seam should be about 0.25 in obs and in weight
	* concretely, the rules are: if two subsequent observations within the quarter are below 20, exclude
	* if one observation in a quarter is below 17, exclude!
	capture drop seam_interior_all_q_strict_ind
	gen seam_interior_all_q_strict_ind=seam_interior_all_q_ind
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1984q4) & quarter<=tq(1985q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1984q4) & quarter<=tq(1985q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1986q1) & quarter<=tq(1986q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1986q4) & quarter<=tq(1987q2)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1987q4) & quarter<=tq(1988q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1989q1) & quarter<=tq(1990q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1990q4) & quarter<=tq(1991q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1991q4) & quarter<=tq(1992q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1992q4) & quarter<=tq(1992q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1994q4) & quarter<=tq(1994q4)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1995q4) & quarter<=tq(1996q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1999q4) & quarter<=tq(2000q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(2003q4) & quarter<=tq(2004q1)
	replace seam_interior_all_q_strict_ind=0 if quarter>=tq(2007q4) & quarter<=tq(2008q3)

	
/*
capture drop merge_qtruee
merge m:1 quarter using "C:\Users\lviss\Dropbox\CTV_Revisions\Aggregate Data Updated\hp_u_all_durwvcearly_ldt_sept2018.dta", gen(merge_qtruee) 
drop if merge_qtruee==2
drop merge_qtruee


// vacancies
 capture drop hp_lvacancy 
 capture drop lvacancy 
 capture drop hp_ltightness 
 capture drop hp_lvacancy 
 capture drop tightness 
 capture drop ltightness 
 capture drop _merge 
 merge m:1 quarter using "C:\Users\lviss\Dropbox\CTV_Revisions\Aggregate Data Updated\pwd.dta", ///
			keepusing( hp_lvacancy lvacancy hp_ltightness hp_lvacancy tightness ltightness )
*/			
			
sort personkey yearmonth 


** define leocc1bfr_mmo2, based on leocc1bfr_mmo and empl_ctv
capture drop leocc1bfr2_mmo
gen leocc1bfr2_mmo=leocc1bfr_mmo
replace leocc1bfr2_mmo=leocc1bfr2_mmo[_n-1] if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl_ctv==1 & empl!=1 & empl_ctv[_n-1]==1 & empl[_n-1]==1 ///
				& leocc1bfr_mmo[_n-1]!=leocc1bfr_mmo



sort personkey yearmonth
global addcondition=""

capture program drop eu_ahead_exe
program define eu_ahead_exe
			args monthsahead namesuffix
capture drop eu`monthsahead'm_ctv`namesuffix'
capture drop eu`monthsahead'm_ctv_nun`namesuffix'

gen byte eu`monthsahead'm_ctv`namesuffix'=.
replace eu`monthsahead'm_ctv`namesuffix'=0 if empl_ctv==1 & sample_timetogo>=`monthsahead' & personkey==personkey[_n+`monthsahead'] ///
														& yearmonth==yearmonth[_n+`monthsahead']-`monthsahead' $addcondition
gen byte eu`monthsahead'm_ctv_nun`namesuffix'=.
replace eu`monthsahead'm_ctv_nun`namesuffix'=0 if empl_ctv==1 & sample_timetogo>=`monthsahead' & personkey==personkey[_n+`monthsahead'] ///
														& yearmonth==yearmonth[_n+`monthsahead']-`monthsahead' $addcondition

forvalues i=1(1)`monthsahead' {

replace eu`monthsahead'm_ctv`namesuffix'=. if eu`monthsahead'm_ctv`namesuffix'!=. & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (empl_ctv[_n+`i']!=1 & unempl_ctv[_n+`i']!=1) $addcondition
replace eu`monthsahead'm_ctv`namesuffix'=1 if eu`monthsahead'm_ctv`namesuffix'!=. & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (unempl_ctv[_n+`i']==1) $addcondition
replace eu`monthsahead'm_ctv_nun`namesuffix'=1 if eu`monthsahead'm_ctv_nun`namesuffix'!=. & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (unempl_ctv[_n+`i']==1) $addcondition
*replace eu`monthsahead'm_ctv_nun=-1 if (eu`monthsahead'm_ctv_nun!=. & eu`monthsahead'm_ctv_nun!=1) & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (empl_ctv[_n+`i']!=1 & unempl_ctv[_n+`i']!=1)
}

end 

** NUMBER IS MONTHS AHEAD 
eu_ahead_exe 1	



***********************************
** GENERATE QUARTERLY TIME SERIES
***********************************


** PROPORTIONS PER QUARTER 

sort quarter entry_ind

capture drop ts_sep_q
by quarter: egen ts_sep_q=sum(pweight2) if entry_ind==1 & leu_ctv==1 
capture drop ts_empsep_q
by quarter: egen ts_empsep_q=sum(pweight2) if entry_ind==1 & (leu_ctv==0 | leu_ctv==1) 

replace ts_sep_q=ts_sep_q/ts_empsep_q


*preserve 
*duplicates drop quarter ts_sep_q, force
*scatter ts_sep_q quarter 
*restore 

capture drop ts_sep_yng_q
by quarter: egen ts_sep_yng_q=sum(pweight2) if entry_ind==1 & leu_ctv==1 & age_2dum==1
capture drop ts_empsep_yng_q
by quarter: egen ts_empsep_yng_q=sum(pweight2) if entry_ind==1 & (leu_ctv==0 | leu_ctv==1) & age_2dum==1 

replace ts_sep_yng_q=ts_sep_yng_q/ts_empsep_yng_q


capture drop ts_sep_prm_q
by quarter: egen ts_sep_prm_q=sum(pweight2) if entry_ind==1 & leu_ctv==1 & age_2dum==2
capture drop ts_empsep_prm_q
by quarter: egen ts_empsep_prm_q=sum(pweight2) if entry_ind==1 & (leu_ctv==0 | leu_ctv==1) & age_2dum==2

replace ts_sep_prm_q=ts_sep_prm_q/ts_empsep_prm_q

** NEED TO FILL OUT 

sort quarter ts_sep_q 
replace ts_sep_q=ts_sep_q[_n-1] if ts_sep_q[_n-1]!=. & ts_sep_q ==. & quarter[_n-1]==quarter
gsort quarter -ts_sep_q 
replace ts_sep_q=ts_sep_q[_n-1] if ts_sep_q[_n-1]!=. & ts_sep_q ==. & quarter[_n-1]==quarter

sort quarter ts_sep_yng_q 
replace ts_sep_yng_q=ts_sep_yng_q[_n-1] if ts_sep_yng_q[_n-1]!=. & ts_sep_yng_q ==. & quarter[_n-1]==quarter
gsort quarter -ts_sep_yng_q 
replace ts_sep_yng_q=ts_sep_yng_q[_n-1] if ts_sep_yng_q[_n-1]!=. & ts_sep_yng_q ==. & quarter[_n-1]==quarter

sort quarter ts_sep_prm_q 
replace ts_sep_prm_q=ts_sep_prm_q[_n-1] if ts_sep_prm_q[_n-1]!=. & ts_sep_prm_q ==. & quarter[_n-1]==quarter
gsort quarter -ts_sep_prm_q 
replace ts_sep_prm_q=ts_sep_prm_q[_n-1] if ts_sep_prm_q[_n-1]!=. & ts_sep_prm_q ==. & quarter[_n-1]==quarter

count if quarter!=. & ts_sep_q==.
gen test_q=0 if quarter!=. & ts_sep_q!=.
replace test_q=1 if quarter!=. & ts_sep_q==.

tab quarter test_q

count if quarter!=. & ts_sep_yng_q==.
count if quarter!=. & ts_sep_prm_q==.


***** BY SUPEROCC

count if entry_ind==1 & (leu_ctv==0 | leu_ctv==1) 
tab occ_mmo if entry_ind==1 & (leu_ctv==0 | leu_ctv==1) , m

*assign superocc
capture drop eocc_rtmm_nomgt
gen eocc_rtmm_nomgt=1 if occ_mmo>=13 & occ_mmo<=29
replace eocc_rtmm_nomgt=2 if occ_mmo>=41 & occ_mmo<=43
replace eocc_rtmm_nomgt=3 if (occ_mmo>=31 & occ_mmo<=39) | (occ_mmo==53)
replace eocc_rtmm_nomgt=4 if occ_mmo>=47 & occ_mmo<=51
* fill up gaps between employment and ctv-type unemployment
sort personkey yearmonth
count if eocc_rtmm_nomgt[_n-1]!=. & empl_ctv[_n-1]==1 & empl_ctv==1 & eocc_rtmm_nomgt==. & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & occ_mmo!=11 & occ_mmo!=45
count if eocc_rtmm_nomgt[_n-1]!=. & empl_ctv[_n-1]==1 & empl_ctv==1 & eocc_rtmm_nomgt==. & personkey[_n+1]==personkey[_n-1] & yearmonth[_n+1]==yearmonth[_n-1]+2 & occ_mmo!=11 & occ_mmo!=45
* these are about 411 obs
count if eocc_rtmm_nomgt[_n-1]!=. & empl_ctv[_n-1]==1 & empl_ctv==1 & eocc_rtmm_nomgt==. & personkey[_n+1]==personkey[_n-1] & yearmonth[_n+1]==yearmonth[_n-1]+2 & unempl_ctv[_n+1]==1 & occ_mmo!=11 & occ_mmo!=45
count if eocc_rtmm_nomgt[_n-1]!=. & empl_ctv[_n-1]==1 & empl_ctv==1 & eocc_rtmm_nomgt==. & personkey[_n+1]==personkey[_n-1] & yearmonth[_n+1]==yearmonth[_n-1]+2 & unempl==1

** REPLACE ONLY IF GAP EXISTS BECAUSE OF UNEMPL==1 CODING BUT NOT UNEMPL_CTV CODING (I.E. EMPL_CTV==1)
replace eocc_rtmm_nomgt=eocc_rtmm_nomgt if eocc_rtmm_nomgt[_n-1]!=. & empl_ctv[_n-1]==1 & empl_ctv==1 & eocc_rtmm_nomgt==. & personkey[_n+1]==personkey[_n-1] & yearmonth[_n+1]==yearmonth[_n-1]+2 & unempl_ctv[_n+1]==1 & occ_mmo!=11 & occ_mmo!=45 & unempl==1


forvalues j=1(1)4{
sort quarter entry_ind

capture drop ts_sep_q_superocc_`j'
by quarter: egen ts_sep_q_superocc_`j'=sum(pweight2) if entry_ind==1 & leu_ctv==1 & eocc_rtmm_nomgt==`j'
capture drop ts_empsep_q_superocc_`j'
by quarter: egen ts_empsep_q_superocc_`j'=sum(pweight2) if entry_ind==1 & (leu_ctv==0 | leu_ctv==1) & eocc_rtmm_nomgt==`j' 

replace ts_sep_q_superocc_`j'=ts_sep_q_superocc_`j'/ts_empsep_q_superocc_`j'
}




sort quarter eocc_rtmm_nomgt entry_ind 
capture drop ts_sep_q_superocc
by quarter eocc_rtmm_nomgt : egen ts_sep_q_superocc=sum(pweight2) if entry_ind==1 & leu_ctv==1 & eocc_rtmm_nomgt!=.
capture drop ts_empsep_q_superocc
by quarter eocc_rtmm_nomgt : egen ts_empsep_q_superocc=sum(pweight2) if entry_ind==1 & (leu_ctv==0 | leu_ctv==1) & eocc_rtmm_nomgt!=. 

replace ts_sep_q_superocc=ts_sep_q_superocc/ts_empsep_q_superocc


tab unempl_ctv eocc_rtmm_nomgt if earlier_empl==1 & wave>4 & interview_no>14 [aw=pweight2], col nof


**************************************************************************
**** SEPARATIONS BY SUPEROCC TIME SERIES
**************************************************************************

preserve

drop if (ts_sep_q_superocc==. )
drop if quarter<tq(1980q1)

duplicates drop quarter eocc_rtmm_nomgt, force 

keep quarter eocc_rtmm_nomgt ts_sep_q_superocc* ts_empsep_q_superocc* seam_interior*

capture drop ts_lsep_q_superocc
gen ts_lsep_q_superocc=log(ts_sep_q_superocc)

capture drop ts_lsep_q_superocc_1
gen ts_lsep_q_superocc_1=log(ts_sep_q_superocc_1)

capture drop ts_lsep_q_superocc_2
gen ts_lsep_q_superocc_2=log(ts_sep_q_superocc_2)

capture drop ts_lsep_q_superocc_3
gen ts_lsep_q_superocc_3=log(ts_sep_q_superocc_3)

capture drop ts_lsep_q_superocc_4
gen ts_lsep_q_superocc_4=log(ts_sep_q_superocc_4)

export excel using "${outputdata}/ts_sep_superocc_for_tramo.xlsx", sheet("sep") sheetreplace firstrow(variables) nolabel
save "${outputdata}/ts_sep_superocc_for_tramo.dta", replace 


restore 

*************************
** OVERALL SEPARATIONS
**************************


drop if (ts_sep_prm_q==. | ts_sep_yng_q==. | ts_sep_q==.)
duplicates drop quarter, force

*** generate logged series
capture drop ts_lsep_q
gen ts_lsep_q=log(ts_sep_q)

capture drop ts_lsep_yng_q
gen ts_lsep_yng_q=log(ts_sep_yng_q)

capture drop ts_lsep_prm_q
gen ts_lsep_prm_q=log(ts_sep_prm_q)

capture drop ts_lsep_bls
gen ts_lsep_bls=log(sepu_bls)

corr ts_lsep_q ts_lsep_bls
corr ts_lsep_q ts_lsep_bls if seam_interior_all_q_ind==1
corr ts_lsep_q ts_lsep_bls if seam_interior_all_q_strict_ind==1
corr  ts_lsep_q ts_lsep_bls if seam_interior_all_q_strict_ind==1 & quarter>tq(1996q1)
scatter ts_lsep_q ts_lsep_bls quarter if seam_interior_all_q_strict_ind==1
** save data, dta and csv/xls for TRAMO


******
*** GENERATE FIVE-Q SERIES, using seam_interior_all_q_strict_ind
******
*use "C:\data\ts_sep_for_tramo.dta", clear
sort quarter 
tokenize  lsep lsep_yng lsep_prm sep sep_yng sep_prm
forvalues i=1(1)6 {
capture drop q3_``i''_q 

 tssmooth ma q3_``i''_q = ts_``i''_q if seam_interior_all_q_ind[_n-2]==1 & ///
										seam_interior_all_q_ind[_n-1]==1 & seam_interior_all_q_ind==1 & seam_interior_all_q_ind[_n+1]==1 & ///
										seam_interior_all_q_ind[_n+2]==1, window(2 1 2)
 
										
}

tssmooth ma q3_lsep_bls_q = ts_lsep_bls, window(2 1 2) 
tssmooth ma q3_sep_bls_q = sepu_bls, window(2 1 2) 


drop ts_empsep_q ts_empsep_yng_q ts_empsep_prm_q
replace ts_sep_q=. if seam_interior_all_q_strict_ind!=1
replace ts_sep_yng_q=. if seam_interior_all_q_strict_ind!=1
replace ts_sep_prm_q=. if seam_interior_all_q_strict_ind!=1
replace ts_lsep_q=. if seam_interior_all_q_strict_ind!=1
replace ts_lsep_yng_q=. if seam_interior_all_q_strict_ind!=1
replace ts_lsep_prm_q=. if seam_interior_all_q_strict_ind!=1



keep quarter hp* ts_* sepu_bls seam* q3_*

reg ts_sep_q sepu_bls
reg ts_lsep_q ts_lsep_bls


export excel using "${outputdata}/ts_sep_before_tramo.xlsx", sheet("sep") sheetreplace firstrow(variables) nolabel
save "${outputdata}/ts_sep_before_tramo.dta", replace 
*use "${outputdata}/ts_sep_before_tramo.dta", clear

sort quarter 
tokenize  lsep lsep_yng lsep_prm sep sep_yng sep_prm
forvalues i=1(1)6 {

 replace q3_``i''_q = . if seam_interior_all_q_ind[_n-2]!=1 | ///
										seam_interior_all_q_ind[_n-1]!=1 | seam_interior_all_q_ind!=1 | seam_interior_all_q_ind[_n+1]!=1 | ///
										seam_interior_all_q_ind[_n+2]!=1
 
										
}
export excel using "${outputdata}/ts_sep_for_tramo.xlsx", sheet("sep") sheetreplace firstrow(variables) nolabel
save "${outputdata}/ts_sep_for_tramo.dta", replace 

su ts_sep_yng_q 
su ts_sep_prm_q 

keep quarter ts_lsep_q ts_lsep_yng_q ts_lsep_prm_q ts_lsep_bls q3_lsep_q q3_lsep_yng_q q3_lsep_prm_q q3_lsep_bls_q
keep quarter ts_lsep_q q3_lsep_q 

export excel using "${outputdata}/ts_sep_paper_for_tramo.xlsx", sheet("sep") sheetreplace firstrow(variables) nolabel







********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"