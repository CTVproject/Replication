

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



//=========================================================
/*
	CALCULATE TIME SERIES PROPERTIES AND CROSS CORRELATIONS OF
	
		- UNEMPLOYMENT
		- OUTPUT P WORKER
		- JOB FINDING
		- SEPARATION
		- VACANCIES
		- IMPLIED TIGHTNESS
		- OCC MOBILITY SERIES 
*/
//==========================================================




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





use "${outputdata}/jf_hp_ts2020.dta", clear




** create indicator
sort quarter
capture drop jf_orig_ind
capture drop jf_orig_5q_ind
gen jf_orig_ind=0
gen jf_orig_5q_ind=0
replace jf_orig_ind=1 if ts_ljf_q!=.
replace jf_orig_5q_ind=1 if ts_ljf_q[_n-2]!=. & ts_ljf_q[_n-1]!=. & ts_ljf_q!=. & ts_ljf_q[_n+1]!=. & ts_ljf_q[_n+2]!=.


 
 


//==============================================================================
//
//		S E P A R A T I O N S
//
//==============================================================================

cap drop _merge_sep
merge 1:1 quarter using "${outputdata}/sep_hp_ts2020.dta", gen(_merge_sep)


//==============================================================================
//
//		U N E M P L O Y M E N T  / N U N E M P L O Y M E N T
//
//==============================================================================


cap drop _merge_u		
merge 1:1 quarter using "${outputdata}/u_hp_ts2020.dta", gen(_merge_u)
	
cap drop _merge_nun		
merge 1:1 quarter using "${outputdata}/nun_ts2020.dta", gen(_merge_nun)

cap drop ur_all_durwvcearly
gen ur_all_durwvcearly=exp(lur_all_durwvcearly)


//==============================================================================
//
//		O C C .    M O B I L I T Y 
//
//==============================================================================


cap drop _merge_occ		
merge 1:1 quarter using "${outputdata}/occmob_ts_after_tramo.dta", gen(_merge_occ)




//==============================================================================
//
//		O U T P U T   /  V A C A N C I E S   /   O T H E R    S E R I E S`'
//
//==============================================================================


cap drop _merge_aggdata		
merge 1:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", gen(_merge_aggdata)


drop if quarter<=tq(1983q4)
drop if quarter>=tq(2014q1)



** generate indicator
capture drop durdist_stab5q
capture drop u_ctv_orig_5q
sort quarter
gen durdist_stab5q=0
gen u_ctv_orig_5q=0
replace durdist_stab5q=1 if durdistr_stability==1 & durdistr_stability[_n-2]==1 & durdistr_stability[_n-1]==1 & durdistr_stability[_n+1]==1  & durdistr_stability[_n+2]==1 
replace u_ctv_orig_5q=1 if lur_all_durwvcearly!=. & lur_all_durwvcearly[_n-2]!=. & lur_all_durwvcearly[_n-1]!=. & lur_all_durwvcearly[_n+1]!=.  & lur_all_durwvcearly[_n+2]!=. 

capture drop durdist_stab3q
capture drop u_ctv_orig_3q
sort quarter
gen durdist_stab3q=0
gen u_ctv_orig_3q=0
replace durdist_stab3q=1 if durdistr_stability==1 & durdistr_stability[_n-1]==1 & durdistr_stability[_n+1]==1  
replace u_ctv_orig_3q=1 if lur_all_durwvcearly!=.  & lur_all_durwvcearly[_n-1]!=. & lur_all_durwvcearly[_n+1]!=.  


*** generate tightness ctv
capture drop ltight_u_ctv_ext
gen ltight_u_ctv_ext=lvacancy-lur_all_durwvcearly_ext

cap drop hp_ltight_u_ctv_ext
tsfilter hp hp_ltight_u_ctv_ext = ltight_u_ctv_ext, smooth(1600)

cap drop q3_ltight_u_ctv_ext
tssmooth ma q3_ltight_u_ctv_ext = ltight_u_ctv_ext, window(2 1 2)



** hp filter q3 tightness

cap drop hp_q3_ltight_u_ctv_ext
tsfilter hp hp_q3_ltight_u_ctv_ext = q3_ltight_u_ctv_ext, smooth(1600)

** hp filter lvacancy
cap drop hp_lvacancy_1
tsfilter hp hp_lvacancy_1 = lvacancy, smooth(1600)



** q3 u bls
cap drop q3_lunrate_bls
tssmooth ma q3_lunrate_bls = lunrate_bls, window(2 1 2)
tsfilter hp hp_q3_lunrate_bls = q3_lunrate_bls, smooth(1600)

	corr hp_lubls_sm5 hp_q3_lunrate_bls 
	reg hp_lubls_sm5 hp_q3_lunrate_bls 
	
** q3 outpw
cap drop q3_loutpw
tssmooth ma q3_loutpw = loutpw, window(2 1 2)
tsfilter hp hp_q3_loutpw = q3_loutpw, smooth(1600)

** q3 ur_durwvcearly
cap drop q3_lur_all_durwvcearly_ext
tssmooth ma q3_lur_all_durwvcearly_ext = lur_all_durwvcearly_ext, window(2 1 2)
tsfilter hp hp_q3_lur_all_durwvcearly_ext = q3_lur_all_durwvcearly_ext, smooth(1600)




//==============================================================================
//
//	******	T I M E   S E R I E S   ( C O - ) M O V E M E N T S ******
//
//==============================================================================



tsset quarter

cap drop hp_qn3_mob_12_mm_stat
gen hp_qn3_mob_12_mm_stat =hp_qn3_mob_12_mm_xlin_sm  if qn3_mob_mm!=.

************************
** 3Q - UNEMPLOYMENT 
************************



cap log close cyclresultslog1
quietly {
log using "${mainresultsdir}/table4_appxtable3_data.txt", replace text name(cyclresultslog1)

** CORRELATION MATRIX
	** (we use the bp_qn1_mob_12_mm!=. restriction because HP filtering is unreliable at both ends of the time series, we ignore quarters that BP filtering would drop)
noisily: display "-------------------------------------------------------------"
noisily: display " TABLE 4: CYCLICAL TIMESERIES (CO)MOVEMENTS (+table 3 onl.appx)"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  "   ---pairwise correlation table--- "
noisily: display  ""

	
noisily: pwcorr hp_q3_lur_all_durwvcearly_ext hp_lvacancy_sm5 hp_q3_ltight_u_ctv_ext hp_q3_lsep_q hp_q3_ljf_q hp_q3_loutpw hp_qn3_mob_12_mm_stat if lur_all_durwvcearly!=. & durdistr_stability==1 &  bp_qn1_mob_12_mm!=. 
noisily: display  ""
noisily: display  ""
noisily: display  " **NOTE**: variables are in order:"
noisily: display  " (1) HP-filt log (smoothed) unemployment rate (of those with earlier employment)"
noisily: display  " (2) HP-filt log (smoothed) vacancies"
noisily: display  " (3) HP-filt log (smoothed) implied tightness"
noisily: display  " (4) HP-filt log (smoothed) separation rate"
noisily: display  " (5) HP-filt log (smoothed) job finding rate"
noisily: display  " (6) HP-filt log (smoothed) output per worker"
noisily: display  " (7) HP-filt log (smoothed) occ mob rate p. unemployed worker"
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  "   ---standard deviations (4th column, same order as in note above)--- "

noisily: su hp_q3_lur_all_durwvcearly_ext hp_lvacancy_sm5 hp_q3_ltight_u_ctv_ext hp_q3_lsep_q hp_q3_ljf_q hp_q3_loutpw hp_qn3_mob_12_mm_stat if lur_all_durwvcearly!=. & durdistr_stability==1 

noisily: display  ""
noisily: display  ""

noisily: display  "   ---autocorrelations (same order)--- "
noisily: display  ""

** AUTOCORRELATION
tokenize hp_q3_lur_all_durwvcearly_ext hp_lvacancy_sm5 hp_q3_ltight_u_ctv_ext hp_q3_lsep_q hp_q3_ljf_q hp_q3_loutpw hp_qn3_mob_12_mm_stat 
forvalues i=1(1)7 {
	noisily: display  "autocorrelation: (`i')"
	noisily: cap n corr ``i''  l.``i'' if lur_all_durwvcearly!=. & durdistr_stability==1 & lur_all_durwvcearly[_n-1]!=. & durdistr_stability[_n-1]==1 
}
log close cyclresultslog1
}

************************
** 1Q 
************************




cap log close cyclresultslog2
quietly {
log using "${mainresultsdir}/appxtable3_unsmoothed_data.txt", replace text name(cyclresultslog2)

** CORRELATION MATRIX
	** (we use the bp_qn1_mob_12_mm!=. restriction because HP filtering is unreliable at both ends of the time series, we ignore quarters that BP filtering would drop)
noisily: display "-------------------------------------------------------------"
noisily: display " TABLE 3 ONLINE APPENDIX, PART 2 -- 1Q DATA, NOT 5Q SMOOTHED"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  "   ---pairwise correlation table--- "
noisily: display  ""

	
noisily: pwcorr hp_lur_all_durwvcearly_ext hp_lvacancy_1 hp_ltight_u_ctv_ext hp_ts_lsep_q hp_ts_ljf_q hp_loutpw if lur_all_durwvcearly!=. & durdistr_stability==1 &  bp_qn1_mob_12_mm!=. 

noisily: display  " note: variables are in order:"
noisily: display  " (1) HP-filt log (smoothed) unemployment rate (of those with earlier employment)"
noisily: display  " (2) HP-filt log (smoothed) vacancies"
noisily: display  " (3) HP-filt log (smoothed) implied tightness"
noisily: display  " (4) HP-filt log (smoothed) separation rate"
noisily: display  " (5) HP-filt log (smoothed) job finding rate"
noisily: display  " (6) HP-filt log (smoothed) output per worker"
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  "   ---standard deviations (4th column, same order as in note above)--- "
noisily: display  ""
noisily: su hp_lur_all_durwvcearly_ext  hp_lvacancy_1  hp_ltight_u_ctv_ext hp_lsep hp_ts_ljf_q if lur_all_durwvcearly!=. & durdistr_stability==1

noisily: display  "standard deviation: (6): output per worker over the entire sample"
noisily: su hp_loutpw  if quarter>=tq(1984q1) & quarter<=tq(2013q4)

noisily: display  ""
noisily: display  ""

noisily: display  "   ---autocorrelations (same order)--- "
noisily: display  ""

** AUTOCORRELATION
tokenize hp_lur_all_durwvcearly_ext hp_lvacancy_1 hp_ltight_u_ctv_ext hp_ts_lsep_q hp_ts_ljf_q 
forvalues i=1(1)5 {
	noisily: display  "autocorrelation: (`i')"
	noisily: cap n corr ``i''  l.``i'' if lur_all_durwvcearly!=. & durdistr_stability==1 & lur_all_durwvcearly[_n-1]!=. & durdistr_stability[_n-1]==1 
}
noisily: display  "autocorrelation: (6): output per worker over the entire sample"
noisily corr hp_loutpw  l.hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4)
log close cyclresultslog2
}


********************************
**  NUN 
******************************




** CORRELATION TABLE 
pwcorr hp_lnun_ext_sm5  hp_lvacancy_sm5  hp_ltight_nun_ext_sm5  hp_q3_lsep hp_q3_ljfn_cnune_instr_q hp_q3_loutpw  if nun_orig ==1  & durdistr_stability==1 & bp_qn1_mob_12_mm!=.

** ST DEV
su hp_lnun_ext_sm5  hp_lvacancy_sm5  hp_ltight_nun_ext_sm5  hp_q3_lsep hp_q3_ljfn_cnune_instr_q hp_q3_loutpw  if nun_orig ==1 & durdistr_stability==1


** AUTOCORRELATION
tokenize hp_lnun_ext_sm5 hp_lvacancy_sm5 hp_ltight_u_ctv_ext hp_ltight_nun_ext_sm5  hp_q3_lsep hp_q3_ljfn_cnune_instr_q
forvalues i=1(1)5 {
cap n corr ``i''  l.``i'' if lur_all_durwvcearly!=. & durdistr_stability==1 & lur_all_durwvcearly[_n-1]!=. & durdistr_stability[_n-1]==1 
}
corr hp_loutpw  l.hp_loutpw if quarter>=tq(1983q4) & quarter<=tq(2013q4)



cap log close cyclresultslog3
quietly {
log using "${mainresultsdir}/appxtable6_nun_data.txt", replace text name(cyclresultslog3)

** CORRELATION MATRIX
	** (we use the bp_qn1_mob_12_mm!=. restriction because HP filtering is unreliable at both ends of the time series, we ignore quarters that BP filtering would drop)
noisily: display "-------------------------------------------------------------"
noisily: display " TABLE 6 ONLINE APPENDIX, NUN DATA -- 5Q SMOOTHED"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  "   ---pairwise correlation table--- "
noisily: display  ""

	
noisily: pwcorr hp_lnun_ext_sm5  hp_lvacancy_sm5  hp_ltight_nun_ext_sm5  hp_q3_lsep hp_q3_ljfn_cnune_instr_q hp_q3_loutpw  if nun_orig ==1  & durdistr_stability==1 & bp_qn1_mob_12_mm!=.

noisily: display  " note: variables are in order:"
noisily: display  " (1) HP-filt log (smoothed) NUN rate (of those with earlier employment)"
noisily: display  " (2) HP-filt log (smoothed) vacancies"
noisily: display  " (3) HP-filt log (smoothed) implied (NUN-based) tightness "
noisily: display  " (4) HP-filt log (smoothed) separation rate"
noisily: display  " (5) HP-filt log (smoothed) job finding rate from NUN-spells"
noisily: display  " (6) HP-filt log (smoothed) output per worker"
noisily: display  ""
noisily: display  ""
noisily: display  ""
noisily: display  "   ---standard deviations (4th column, same order)--- "
noisily: display  ""
noisily: su hp_lnun_ext_sm5  hp_lvacancy_sm5  hp_ltight_nun_ext_sm5  hp_q3_lsep hp_q3_ljfn_cnune_instr_q if nun_orig ==1  & durdistr_stability==1 

noisily: display  "standard deviation: (6): output per worker over the entire sample"
noisily: su hp_loutpw  if quarter>=tq(1984q1) & quarter<=tq(2013q4)

noisily: display  ""
noisily: display  ""

noisily: display  "   ---autocorrelations (same order)--- "
noisily: display  ""

** AUTOCORRELATION
tokenize hp_lnun_ext_sm5 hp_lvacancy_sm5 hp_ltight_u_ctv_ext hp_ltight_nun_ext_sm5  hp_q3_lsep hp_q3_ljfn_cnune_instr_q
forvalues i=1(1)5 {
	noisily: display  "autocorrelation: (`i')"
	noisily: cap n corr ``i''  l.``i'' if lur_all_durwvcearly!=. & durdistr_stability==1 & lur_all_durwvcearly[_n-1]!=. & durdistr_stability[_n-1]==1 
}
noisily: display  "autocorrelation: (6): output per worker over the entire sample"
noisily corr hp_loutpw  l.hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4)
log close cyclresultslog3
}



********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"