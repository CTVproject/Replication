

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




/* 

IN THIS SECTION, we construct the two pictures that capture the duration profile shift over the cycle

1) corrected duration profile in bad time and in good times, correcting for the linear time trend
	sort by age category
								
					
2) duration shift
					
					
	INPUTS: load in corrected and uncorrected 
										
		OCCUPATIONAL MOBILITY PER DURATION  x QUARTER  (standard MOG)
		OCCUPATIONAL MOBILITY PER QUARTER (RTMM, other cuts)
								
				
*/


// RAW PICTURE OF DURATION PROFILE (corrected and uncorrected for measurement error)


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






global locfileversion=1222




do "${step2codedir}/aux_directory/aux_programs.do"


***********************************************
***  DATA READ-IN
***********************************************

** START: MM 
 import excel "${outputdata}/timeseriesdur_mm.xls", sheet("ts") firstrow clear
 *import excel "${outputdata}/timeseriesdur_mm2.xls", sheet("ts") firstrow clear
 
 cap n ren A quarter
 cap n ren B u_spellength

 
 
 
 sort quarter
destring quarter, replace
cap n destring u_spellength, replace
cap n destring n_spellength, replace


** rename depending on version of timeseriesdur_mm.xlsx, should not be necessary now
cap n ren q1d* qn1*
cap n ren q3d* qn3*



sort quarter n_spellength
format quarter %tq

 capture drop merge_qtrfill
 *merge m:1 quarter using "${datadir1}aggdata_u_y_may2017.dta", keepusing(hp*) gen(merge_qtrfill)
 merge m:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", keepusing(hp*) gen(merge_qtrfill)
 drop if merge_qtrfill==2
 drop merge_qtrfill
 
 
 cap n rename hp_lunrate hp_lunrate_fred
 set varabbrev off
 cap n drop hp_lunrate
 gen hp_lunrate=hpf_lunrate_bls_0  // use the BLS version, not FRED
  
  
 cap n gen u_spellength=n_spellength
 sort quarter u_spellength
 drop if quarter<tq(1983q1)
 set varabbrev off
 cap drop S T U V
 
 cap drop q2d*
 cap drop q4d*
 
 tab quarter
 
 save "${tempdata}/tsdur_cycle_mm.dta", replace

 	

//***********************************************************************
// CREATE CUMULATIVE MOBILITY NUMBERS FROM QUARTER x N_SPELLENGTH DATA
//**************************************************************************
					/* THIS IS SHARED WITH SECTION2_3_CYCLICAL_DETRENDED_PICTURE */
					
					
cap use  "${tempdata}/tsdur_cycle_mm.dta"


capture program drop cumulative_mob
program define cumulative_mob
		args name_infix
		
forvalues i=1(2)3 {		
sort quarter n_spellength 
 
capture drop  qn`i'_occmove_`name_infix' 
capture drop qn`i'_occstay_`name_infix'
gen qn`i'_occmove_`name_infix'=qn`i'_mobcorr_`name_infix'*qn`i'_obs_mobcorr_`name_infix'
gen qn`i'_occstay_`name_infix'=(1.0-qn`i'_mobcorr_`name_infix')*qn`i'_obs_mobcorr_`name_infix'

capture drop qn`i'_m_asc_`name_infix' 
capture drop qn`i'_s_asc_`name_infix' 
gen qn`i'_m_asc_`name_infix'=0
gen qn`i'_s_asc_`name_infix'=0
replace qn`i'_m_asc_`name_infix'=qn`i'_occmove_`name_infix' if n_spellength==1 & qn`i'_occmove_`name_infix'!=.
replace qn`i'_s_asc_`name_infix'= qn`i'_occstay_`name_infix' if n_spellength==1 & qn`i'_occstay_`name_infix'!=.
forvalues h=1(1)18 {
replace qn`i'_m_asc_`name_infix'=qn`i'_occmove_`name_infix'+qn`i'_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'!=.
replace qn`i'_s_asc_`name_infix'= qn`i'_occstay_`name_infix'+qn`i'_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'!=.
replace qn`i'_m_asc_`name_infix'=qn`i'_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'==.
replace qn`i'_s_asc_`name_infix'= qn`i'_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'==.
}
 
gsort quarter -n_spellength

capture drop qn`i'_m_desc12_`name_infix' 
capture drop qn`i'_s_desc12_`name_infix' 
gen qn`i'_m_desc12_`name_infix'=0
gen qn`i'_s_desc12_`name_infix'= 0
replace qn`i'_m_desc12_`name_infix'=qn`i'_occmove_`name_infix' if n_spellength==12 & qn`i'_occmove_`name_infix'!=.
replace qn`i'_s_desc12_`name_infix'= qn`i'_occstay_`name_infix' if n_spellength==12 & qn`i'_occstay_`name_infix'!=.
forvalues h=1(1)11 {
replace qn`i'_m_desc12_`name_infix'=qn`i'_occmove_`name_infix'+qn`i'_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'!=.
replace qn`i'_s_desc12_`name_infix'= qn`i'_occstay_`name_infix'+qn`i'_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'!=.
replace qn`i'_m_desc12_`name_infix'=qn`i'_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'==.
replace qn`i'_s_desc12_`name_infix'= qn`i'_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'==.
}

capture drop qn`i'_m_desc14_`name_infix' 
capture drop qn`i'_s_desc14_`name_infix' 

gen qn`i'_m_desc14_`name_infix'=0
gen qn`i'_s_desc14_`name_infix'= 0
replace qn`i'_m_desc14_`name_infix'=qn`i'_occmove_`name_infix' if n_spellength==14 & qn`i'_occmove_`name_infix'!=.
replace qn`i'_s_desc14_`name_infix'= qn`i'_occstay_`name_infix' if n_spellength==14 & qn`i'_occstay_`name_infix'!=.
forvalues h=1(1)13 {
replace qn`i'_m_desc14_`name_infix'=qn`i'_occmove_`name_infix'+qn`i'_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'!=.
replace qn`i'_s_desc14_`name_infix'= qn`i'_occstay_`name_infix'+qn`i'_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'!=.
replace qn`i'_m_desc14_`name_infix'=qn`i'_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'==.
replace qn`i'_s_desc14_`name_infix'= qn`i'_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'==.
 } 
 sort quarter n_spellength 
 
 
 
 ** 1) cumulative (from 1 to x)
capture drop qn`i'_cum_mob_asc_`name_infix'
gen  qn`i'_cum_mob_asc_`name_infix'=qn`i'_m_asc_`name_infix'/(qn`i'_m_asc_`name_infix'+qn`i'_s_asc_`name_infix' )
capture drop qn`i'_cum_mob_asc_`name_infix'_obs
gen qn`i'_cum_mob_asc_`name_infix'_obs=qn`i'_m_asc_`name_infix'+qn`i'_s_asc_`name_infix' 

 ** 2) cumulative (from x to 12)
capture drop qn`i'_cum_mob_desc12_`name_infix'
gen  qn`i'_cum_mob_desc12_`name_infix'=qn`i'_m_desc12_`name_infix'/(qn`i'_m_desc12_`name_infix'+qn`i'_s_desc12_`name_infix' )
 capture drop qn`i'_cum_mob_desc12_`name_infix'_obs
 gen qn`i'_cum_mob_desc12_`name_infix'_obs=(qn`i'_m_desc12_`name_infix'+qn`i'_s_desc12_`name_infix' )
 
 ** 3) cumulative (from x to 14) 
capture drop qn`i'_cum_mob_desc14_`name_infix'
gen  qn`i'_cum_mob_desc14_`name_infix'=qn`i'_m_desc14_`name_infix'/(qn`i'_m_desc14_`name_infix'+qn`i'_s_desc14_`name_infix' )
 capture drop qn`i'_cum_mob_desc14_`name_infix'_obs
 gen  qn`i'_cum_mob_desc14_`name_infix'_obs=(qn`i'_m_desc14_`name_infix'+qn`i'_s_desc14_`name_infix' )
 }
 compress
 
 end

 //RRRRRRR
 cumulative_mob mm
 //RRRRRRR
 
 save "${tempdata}/tsdur_cycle_mm.dta", replace

 
 //***** UNCORRECTED SERIES 
 capture program drop uncorr_cumulative_mob
program define uncorr_cumulative_mob
		args name_infix
		
forvalues i=1(2)3 {		
sort quarter n_spellength 
 
capture drop  qn`i'u_occmove_`name_infix' 
capture drop qn`i'u_occstay_`name_infix'
gen qn`i'u_occmove_`name_infix'=qn`i'_mob_`name_infix'*qn`i'_obs_mob_`name_infix'
gen qn`i'u_occstay_`name_infix'=(1.0-qn`i'_mob_`name_infix')*qn`i'_obs_mob_`name_infix'

capture drop qn`i'u_m_asc_`name_infix' 
capture drop qn`i'u_s_asc_`name_infix' 
gen qn`i'u_m_asc_`name_infix'=0
gen qn`i'u_s_asc_`name_infix'=0
replace qn`i'u_m_asc_`name_infix'=qn`i'u_occmove_`name_infix' if n_spellength==1 & qn`i'u_occmove_`name_infix'!=.
replace qn`i'u_s_asc_`name_infix'= qn`i'u_occstay_`name_infix' if n_spellength==1 & qn`i'u_occstay_`name_infix'!=.
forvalues h=1(1)18 {
replace qn`i'u_m_asc_`name_infix'=qn`i'u_occmove_`name_infix'+qn`i'u_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'!=.
replace qn`i'u_s_asc_`name_infix'= qn`i'u_occstay_`name_infix'+qn`i'u_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'!=.
replace qn`i'u_m_asc_`name_infix'=qn`i'u_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'==.
replace qn`i'u_s_asc_`name_infix'= qn`i'u_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'==.
}
 
gsort quarter -n_spellength

capture drop qn`i'u_m_desc12_`name_infix' 
capture drop qn`i'u_s_desc12_`name_infix' 
gen qn`i'u_m_desc12_`name_infix'=0
gen qn`i'u_s_desc12_`name_infix'= 0
replace qn`i'u_m_desc12_`name_infix'=qn`i'u_occmove_`name_infix' if n_spellength==12 & qn`i'u_occmove_`name_infix'!=.
replace qn`i'u_s_desc12_`name_infix'= qn`i'u_occstay_`name_infix' if n_spellength==12 & qn`i'u_occstay_`name_infix'!=.
forvalues h=1(1)11 {
replace qn`i'u_m_desc12_`name_infix'=qn`i'u_occmove_`name_infix'+qn`i'u_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'!=.
replace qn`i'u_s_desc12_`name_infix'= qn`i'u_occstay_`name_infix'+qn`i'u_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'!=.
replace qn`i'u_m_desc12_`name_infix'=qn`i'u_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'==.
replace qn`i'u_s_desc12_`name_infix'= qn`i'u_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'==.
}

capture drop qn`i'u_m_desc14_`name_infix' 
capture drop qn`i'u_s_desc14_`name_infix' 

gen qn`i'u_m_desc14_`name_infix'=0
gen qn`i'u_s_desc14_`name_infix'= 0
replace qn`i'u_m_desc14_`name_infix'=qn`i'u_occmove_`name_infix' if n_spellength==14 & qn`i'u_occmove_`name_infix'!=.
replace qn`i'u_s_desc14_`name_infix'= qn`i'u_occstay_`name_infix' if n_spellength==14 & qn`i'u_occstay_`name_infix'!=.
forvalues h=1(1)13 {
replace qn`i'u_m_desc14_`name_infix'=qn`i'u_occmove_`name_infix'+qn`i'u_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'!=.
replace qn`i'u_s_desc14_`name_infix'= qn`i'u_occstay_`name_infix'+qn`i'u_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'!=.
replace qn`i'u_m_desc14_`name_infix'=qn`i'u_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'==.
replace qn`i'u_s_desc14_`name_infix'= qn`i'u_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'==.
 } 
 sort quarter n_spellength 
 
 
 
 ** 1) cumulative (from 1 to x)
capture drop qn`i'u_cum_mob_asc_`name_infix'
gen  qn`i'u_cum_mob_asc_`name_infix'=qn`i'u_m_asc_`name_infix'/(qn`i'u_m_asc_`name_infix'+qn`i'u_s_asc_`name_infix' )
capture drop qn`i'u_cum_mob_asc_`name_infix'_obs
gen qn`i'u_cum_mob_asc_`name_infix'_obs=qn`i'u_m_asc_`name_infix'+qn`i'u_s_asc_`name_infix' 

 ** 2) cumulative (from x to 12)
capture drop qn`i'u_cum_mob_desc12_`name_infix'
gen  qn`i'u_cum_mob_desc12_`name_infix'=qn`i'u_m_desc12_`name_infix'/(qn`i'u_m_desc12_`name_infix'+qn`i'u_s_desc12_`name_infix' )
 capture drop qn`i'u_cum_mob_desc12_`name_infix'_obs
 gen qn`i'u_cum_mob_desc12_`name_infix'_obs=(qn`i'u_m_desc12_`name_infix'+qn`i'u_s_desc12_`name_infix' )
 
 ** 3) cumulative (from x to 14) 
capture drop qn`i'u_cum_mob_desc14_`name_infix'
gen  qn`i'u_cum_mob_desc14_`name_infix'=qn`i'u_m_desc14_`name_infix'/(qn`i'u_m_desc14_`name_infix'+qn`i'u_s_desc14_`name_infix' )
 capture drop qn`i'u_cum_mob_desc14_`name_infix'_obs
 gen  qn`i'u_cum_mob_desc14_`name_infix'_obs=(qn`i'u_m_desc14_`name_infix'+qn`i'u_s_desc14_`name_infix' )
 }
 compress
 
 end
 
 //RRRRRR
 uncorr_cumulative_mob mm
//RRRRRRR
 
 
 save "${tempdata}/tsdur_cycle_mm.dta", replace	
					
					

//==============================================================
//  DEFINE GOOD AND BAD PERIODS 
//=============================================================
					
					/*  MAKE SURE THAT THESE DEFINITIONS ARE IN AGREEMENT WITH THE DEFINITIONS USED
								
								IN SECTION2_3_CYCLICAL_NETMOBILITY_CALCULATION
					
					for this reason we consider all quarters between 1984q1 & 2013q4, except 2000q2 2000q3 2008q1
					from 2_3_cyclical_netmobility:
					
					
				for comparison: a quick run of the code in that file seems to yield
				
				. display "$p33 "
				-.042842410504818 

				. display "$p67 
				.0370313916355371

					*/
					
global ts_quartersconsidered " quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=tq(2000q2) & quarter!=tq(2000q3) & quarter!=tq(2008q1)" 					
count if ${ts_quartersconsidered}  & n_spellength==1										
cap drop good_tercile_times
cap drop bad_tercile_times
cap drop badgoodtimes_ind
cap drop badgoodepisodes_ind


cap drop alltimes_ind
gen alltimes_ind=1

sort quarter
su hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=quarter[_n-1] & ${ts_quartersconsidered}, detail

global p25=r(p25)
global p75=r(p75)
global p10=r(p10)
global p90=r(p90)

_pctile hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=quarter[_n-1] & ${ts_quartersconsidered}, nq(10)
return list
global p20=r(r2)
global p80=r(r8)

_pctile hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=quarter[_n-1] & ${ts_quartersconsidered}, nq(3)
return list
global p33=r(r1)
global p67=r(r2)
display "$p33 "
display "$p67 "


sort quarter
capture drop badgoodtimes_ind
gen badgoodtimes_ind=1 if hp_lunrate>=$p67
replace badgoodtimes_ind=2 if hp_lunrate<=$p33

tab quarter if badgoodtimes_ind==1
tab quarter if badgoodtimes_ind==2

					
//==============================================================================
//  DURATION PROFILES IN GOOD AND BAD TIMES 
//===============================================================================					
					


// 1) RAW PICTURE
capture program drop cumulative_mob_periods
program define cumulative_mob_periods
		args name_infix time_ind name_suffix

		su `time_ind'
		local time_ind_max=r(max)

				
		forvalues i=1(2)3 {		
		sort quarter n_spellength 
		 
		** weighted move and stay observations 
		capture drop  qn`i'u_occmove_`name_infix' 
		capture drop qn`i'u_occstay_`name_infix'
		gen qn`i'u_occmove_`name_infix'=qn`i'_mob_`name_infix'*qn`i'_obs_mob_`name_infix'
		gen qn`i'u_occstay_`name_infix'=(1.0-qn`i'_mob_`name_infix')*qn`i'_obs_mob_`name_infix'
		capture drop  qn`i'_occmove_`name_infix' 
		capture drop qn`i'_occstay_`name_infix'
		gen qn`i'_occmove_`name_infix'=qn`i'_mobcorr_`name_infix'*qn`i'_obs_mobcorr_`name_infix'
		gen qn`i'_occstay_`name_infix'=(1.0-qn`i'_mobcorr_`name_infix')*qn`i'_obs_mobcorr_`name_infix'

		** by spellength and period
		capture drop qn`i'u_occmove_`name_infix'_dur
		capture drop qn`i'u_occstay_`name_infix'_dur
		capture drop qn`i'_occmove_`name_infix'_dur
		capture drop qn`i'_occstay_`name_infix'_dur

		bysort `time_ind' n_spellength: egen qn`i'u_occmove_`name_infix'_dur=total(qn`i'u_occmove_`name_infix')
		bysort `time_ind' n_spellength: egen qn`i'u_occstay_`name_infix'_dur=total(qn`i'u_occstay_`name_infix')
		bysort `time_ind' n_spellength: egen qn`i'_occmove_`name_infix'_dur=total(qn`i'_occmove_`name_infix')
		bysort `time_ind' n_spellength: egen qn`i'_occstay_`name_infix'_dur=total(qn`i'_occstay_`name_infix')

		sort quarter n_spellength

		capture drop qn`i'u_m_asc_`name_infix'_dur 
		capture drop qn`i'u_s_asc_`name_infix'_dur 
		gen qn`i'u_m_asc_`name_infix'_dur=0
		gen qn`i'u_s_asc_`name_infix'_dur=0
		replace qn`i'u_m_asc_`name_infix'_dur=qn`i'u_occmove_`name_infix'_dur if n_spellength==1 & qn`i'u_occmove_`name_infix'_dur!=.
		replace qn`i'u_s_asc_`name_infix'_dur= qn`i'u_occstay_`name_infix'_dur if n_spellength==1 & qn`i'u_occstay_`name_infix'_dur!=.
		forvalues h=1(1)18 {
		replace qn`i'u_m_asc_`name_infix'_dur=qn`i'u_occmove_`name_infix'_dur+qn`i'u_m_asc_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'_dur!=.
		replace qn`i'u_s_asc_`name_infix'_dur= qn`i'u_occstay_`name_infix'_dur+qn`i'u_s_asc_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'_dur!=.
		replace qn`i'u_m_asc_`name_infix'_dur=qn`i'u_m_asc_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'_dur==.
		replace qn`i'u_s_asc_`name_infix'_dur= qn`i'u_s_asc_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'_dur==.
		}

		capture drop qn`i'_m_asc_`name_infix'_dur 
		capture drop qn`i'_s_asc_`name_infix'_dur 
		gen qn`i'_m_asc_`name_infix'_dur=0
		gen qn`i'_s_asc_`name_infix'_dur=0
		replace qn`i'_m_asc_`name_infix'_dur=qn`i'_occmove_`name_infix'_dur if n_spellength==1 & qn`i'_occmove_`name_infix'_dur!=.
		replace qn`i'_s_asc_`name_infix'_dur= qn`i'_occstay_`name_infix'_dur if n_spellength==1 & qn`i'_occstay_`name_infix'_dur!=.
		forvalues h=1(1)18 {
		replace qn`i'_m_asc_`name_infix'_dur=qn`i'_occmove_`name_infix'_dur+qn`i'_m_asc_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'_dur!=.
		replace qn`i'_s_asc_`name_infix'_dur= qn`i'_occstay_`name_infix'_dur+qn`i'_s_asc_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'_dur!=.
		replace qn`i'_m_asc_`name_infix'_dur=qn`i'_m_asc_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'_dur==.
		replace qn`i'_s_asc_`name_infix'_dur= qn`i'_s_asc_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'_dur==.
		}




		 
		gsort quarter -n_spellength

		capture drop qn`i'u_m_desc12_`name_infix'_dur 
		capture drop qn`i'u_s_desc12_`name_infix'_dur 
		gen qn`i'u_m_desc12_`name_infix'_dur=0
		gen qn`i'u_s_desc12_`name_infix'_dur= 0
		replace qn`i'u_m_desc12_`name_infix'_dur=qn`i'u_occmove_`name_infix'_dur if n_spellength==12 & qn`i'u_occmove_`name_infix'_dur!=.
		replace qn`i'u_s_desc12_`name_infix'_dur= qn`i'u_occstay_`name_infix'_dur if n_spellength==12 & qn`i'u_occstay_`name_infix'_dur!=.
		forvalues h=1(1)11 {
		replace qn`i'u_m_desc12_`name_infix'_dur=qn`i'u_occmove_`name_infix'_dur+qn`i'u_m_desc12_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'_dur!=.
		replace qn`i'u_s_desc12_`name_infix'_dur= qn`i'u_occstay_`name_infix'_dur+qn`i'u_s_desc12_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'_dur!=.
		replace qn`i'u_m_desc12_`name_infix'_dur=qn`i'u_m_desc12_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'_dur==.
		replace qn`i'u_s_desc12_`name_infix'_dur= qn`i'u_s_desc12_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'_dur==.
		}


		capture drop qn`i'_m_desc12_`name_infix'_dur 
		capture drop qn`i'_s_desc12_`name_infix'_dur 
		gen qn`i'_m_desc12_`name_infix'_dur=0
		gen qn`i'_s_desc12_`name_infix'_dur= 0
		replace qn`i'_m_desc12_`name_infix'_dur=qn`i'_occmove_`name_infix'_dur if n_spellength==12 & qn`i'_occmove_`name_infix'_dur!=.
		replace qn`i'_s_desc12_`name_infix'_dur= qn`i'_occstay_`name_infix'_dur if n_spellength==12 & qn`i'_occstay_`name_infix'_dur!=.
		forvalues h=1(1)11 {
		replace qn`i'_m_desc12_`name_infix'_dur=qn`i'_occmove_`name_infix'_dur+qn`i'_m_desc12_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'_dur!=.
		replace qn`i'_s_desc12_`name_infix'_dur= qn`i'_occstay_`name_infix'_dur+qn`i'_s_desc12_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'_dur!=.
		replace qn`i'_m_desc12_`name_infix'_dur=qn`i'_m_desc12_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'_dur==.
		replace qn`i'_s_desc12_`name_infix'_dur= qn`i'_s_desc12_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'_dur==.
		}


		capture drop qn`i'u_m_desc14_`name_infix'_dur 
		capture drop qn`i'u_s_desc14_`name_infix'_dur 

		gen qn`i'u_m_desc14_`name_infix'_dur=0
		gen qn`i'u_s_desc14_`name_infix'_dur= 0
		replace qn`i'u_m_desc14_`name_infix'_dur=qn`i'u_occmove_`name_infix'_dur if n_spellength==14 & qn`i'u_occmove_`name_infix'_dur!=.
		replace qn`i'u_s_desc14_`name_infix'_dur= qn`i'u_occstay_`name_infix'_dur if n_spellength==14 & qn`i'u_occstay_`name_infix'_dur!=.
		forvalues h=1(1)13 {
		replace qn`i'u_m_desc14_`name_infix'_dur=qn`i'u_occmove_`name_infix'_dur+qn`i'u_m_desc14_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'_dur!=.
		replace qn`i'u_s_desc14_`name_infix'_dur= qn`i'u_occstay_`name_infix'_dur+qn`i'u_s_desc14_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'_dur!=.
		replace qn`i'u_m_desc14_`name_infix'_dur=qn`i'u_m_desc14_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'_dur==.
		replace qn`i'u_s_desc14_`name_infix'_dur= qn`i'u_s_desc14_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'_dur==.
		 } 

		 capture drop qn`i'_m_desc14_`name_infix'_dur 
		capture drop qn`i'_s_desc14_`name_infix'_dur 

		gen qn`i'_m_desc14_`name_infix'_dur=0
		gen qn`i'_s_desc14_`name_infix'_dur= 0
		replace qn`i'_m_desc14_`name_infix'_dur=qn`i'_occmove_`name_infix'_dur if n_spellength==14 & qn`i'_occmove_`name_infix'_dur!=.
		replace qn`i'_s_desc14_`name_infix'_dur= qn`i'_occstay_`name_infix'_dur if n_spellength==14 & qn`i'_occstay_`name_infix'_dur!=.
		forvalues h=1(1)13 {
		replace qn`i'_m_desc14_`name_infix'_dur=qn`i'_occmove_`name_infix'_dur+qn`i'_m_desc14_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'_dur!=.
		replace qn`i'_s_desc14_`name_infix'_dur= qn`i'_occstay_`name_infix'_dur+qn`i'_s_desc14_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'_dur!=.
		replace qn`i'_m_desc14_`name_infix'_dur=qn`i'_m_desc14_`name_infix'_dur[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'_dur==.
		replace qn`i'_s_desc14_`name_infix'_dur= qn`i'_s_desc14_`name_infix'_dur[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'_dur==.
		 } 

		 
		 sort quarter n_spellength 
		 

		 
		 
		 
		 
		 *** CONSTRUCTION OF CUMULATIVE PROFILES
		 
		 ****** UNCORRECTED
		 
		 ** 1) cumulative (from 1 to x)
		capture drop qn`i'u_cmob_asc_`name_infix'_dur`name_suffix'
		gen  qn`i'u_cmob_asc_`name_infix'_dur`name_suffix'=qn`i'u_m_asc_`name_infix'_dur/(qn`i'u_m_asc_`name_infix'_dur+qn`i'u_s_asc_`name_infix'_dur )
		capture drop qn`i'u_cmob_asc_`name_infix'_dur_obs`name_suffix'
		gen qn`i'u_cmob_asc_`name_infix'_dur_obs`name_suffix'=qn`i'u_m_asc_`name_infix'_dur+qn`i'u_s_asc_`name_infix'_dur 

		 ** 2) cumulative (from x to 12)
		capture drop qn`i'u_cmob_dsc12_`name_infix'_dur`name_suffix'
		gen  qn`i'u_cmob_dsc12_`name_infix'_dur`name_suffix'=qn`i'u_m_desc12_`name_infix'_dur/(qn`i'u_m_desc12_`name_infix'_dur+qn`i'u_s_desc12_`name_infix'_dur )
		 capture drop qn`i'u_cmob_dsc12_`name_infix'_dur_obs`name_suffix'
		 gen qn`i'u_cmob_dsc12_`name_infix'_dur_obs`name_suffix'=(qn`i'u_m_desc12_`name_infix'_dur+qn`i'u_s_desc12_`name_infix'_dur )
		 
		 ** 3) cumulative (from x to 14) 
		capture drop qn`i'u_cmob_dsc14_`name_infix'_dur`name_suffix'
		gen  qn`i'u_cmob_dsc14_`name_infix'_dur`name_suffix'=qn`i'u_m_desc14_`name_infix'_dur/(qn`i'u_m_desc14_`name_infix'_dur+qn`i'u_s_desc14_`name_infix'_dur )
		 capture drop qn`i'u_cmob_dsc14_`name_infix'_dur_obs`name_suffix'
		 gen  qn`i'u_cmob_dsc14_`name_infix'_dur_obs`name_suffix'=(qn`i'u_m_desc14_`name_infix'_dur+qn`i'u_s_desc14_`name_infix'_dur )
		 
		 
		 ******  CORRECTED 
		 
		 ** 1) cumulative (from 1 to x)
		capture drop qn`i'_cmob_asc_`name_infix'_dur`name_suffix'
		gen  qn`i'_cmob_asc_`name_infix'_dur`name_suffix'=qn`i'_m_asc_`name_infix'_dur/(qn`i'_m_asc_`name_infix'_dur+qn`i'_s_asc_`name_infix'_dur )
		capture drop qn`i'_cmob_asc_`name_infix'_dur_obs`name_suffix'
		gen qn`i'_cmob_asc_`name_infix'_dur_obs`name_suffix'=qn`i'_m_asc_`name_infix'_dur+qn`i'_s_asc_`name_infix'_dur 

		 ** 2) cumulative (from x to 12)
		capture drop qn`i'_cmob_dsc12_`name_infix'_dur`name_suffix'
		gen  qn`i'_cmob_dsc12_`name_infix'_dur`name_suffix'=qn`i'_m_desc12_`name_infix'_dur/(qn`i'_m_desc12_`name_infix'_dur+qn`i'_s_desc12_`name_infix'_dur )
		 capture drop qn`i'_cmob_dsc12_`name_infix'_dur_obs`name_suffix'
		 gen qn`i'_cmob_dsc12_`name_infix'_dur_obs`name_suffix'=(qn`i'_m_desc12_`name_infix'_dur+qn`i'_s_desc12_`name_infix'_dur )
		 
		 ** 3) cumulative (from x to 14) 
		capture drop qn`i'_cmob_dsc14_`name_infix'_dur`name_suffix'
		gen  qn`i'_cmob_dsc14_`name_infix'_dur`name_suffix'=qn`i'_m_desc14_`name_infix'_dur/(qn`i'_m_desc14_`name_infix'_dur+qn`i'_s_desc14_`name_infix'_dur )
		 capture drop qn`i'_cmob_dsc14_`name_infix'_dur_obs`name_suffix'
		 gen  qn`i'_cmob_dsc14_`name_infix'_dur_obs`name_suffix'=(qn`i'_m_desc14_`name_infix'_dur+qn`i'_s_desc14_`name_infix'_dur )
		 
		******* housekeeping

		capture drop  qn`i'u_occmove_`name_infix' 
		capture drop qn`i'u_occstay_`name_infix'
		capture drop  qn`i'_occmove_`name_infix' 
		capture drop qn`i'_occstay_`name_infix'
		 
		capture drop qn`i'u_occmove_`name_infix'_dur
		capture drop qn`i'u_occstay_`name_infix'_dur
		capture drop qn`i'_occmove_`name_infix'_dur
		capture drop qn`i'_occstay_`name_infix'_dur
		 
		capture drop qn`i'u_m_asc_`name_infix'_dur 
		capture drop qn`i'u_s_asc_`name_infix'_dur 
		capture drop qn`i'_m_asc_`name_infix'_dur 
		capture drop qn`i'_s_asc_`name_infix'_dur 

		capture drop qn`i'u_m_desc12_`name_infix'_dur 
		capture drop qn`i'u_s_desc12_`name_infix'_dur 
		capture drop qn`i'_m_desc12_`name_infix'_dur 
		capture drop qn`i'_s_desc12_`name_infix'_dur 

		capture drop qn`i'u_m_desc14_`name_infix'_dur 
		capture drop qn`i'u_s_desc14_`name_infix'_dur 
		capture drop qn`i'_m_desc14_`name_infix'_dur 
		capture drop qn`i'_s_desc14_`name_infix'_dur 
		 
		 
		 }
		 compress
 
 
 end


 
 
 cumulative_mob_periods  mm badgoodtimes_ind bg

*/	 
// ***************************************************************************************
//  Cumulative profiles per high and low unemployment episodes
//*****************************************************************************************
	 
	 
capture program drop cummob_highlow_u_exe
program define cummob_highlow_u_exe
		args indic
			
	capture drop cum_mob_`indic'_high_u_wgt
	capture drop cum_mob_`indic'_high_u_mrgs
	capture drop cum_mob_`indic'_high_u_mrgs_lb
	capture drop cum_mob_`indic'_high_u_mrgs_ub

	capture drop cum_mob_`indic'_low_u_wgt
	capture drop cum_mob_`indic'_low_u_mrgs
	capture drop cum_mob_`indic'_low_u_mrgs_lb
	capture drop cum_mob_`indic'_low_u_mrgs_ub

	gen cum_mob_`indic'_high_u_wgt=.
	gen cum_mob_`indic'_high_u_mrgs=.
	gen cum_mob_`indic'_high_u_mrgs_lb=.
	gen cum_mob_`indic'_high_u_mrgs_ub=.

	gen cum_mob_`indic'_low_u_wgt=.
	gen cum_mob_`indic'_low_u_mrgs=.
	gen cum_mob_`indic'_low_u_mrgs_lb=.
	gen cum_mob_`indic'_low_u_mrgs_ub=.

	global max_uspell_mrg=16
	global max_count_length=18

	capture drop qn1_obs_mobcorr_`indic'_temp
	gen qn1_obs_mobcorr_`indic'_temp=qn1_obs_mobcorr_`indic'
	replace qn1_obs_mobcorr_`indic'_temp=0 if qn1_obs_mobcorr_`indic'<0


	**** UNDERLYING REGRESSION
	reg qn1_mobcorr_`indic' c.hp_lunrate##i.n_spellength quarter [aw=qn1_obs_mobcorr_`indic'_temp] if n_spellength<=18, vce(cluster quarter)


	**** MARGINS

	forvalues ll=1(1)$max_count_length {


	display "**********************`ll'**************************"


	*** HIGH UNEMPLOYMENT

	count if hp_lunrate>=$p67 & n_spellength>=`ll' & n_spellength<=$max_uspell_mrg & quarter>=tq(1985q1) & quarter<=tq(2013q1) & qn1_obs_mobcorr_`indic' !=.
	display "count ......." r(N)

	if r(N)>0 {
	su qn1_obs_mobcorr_`indic' if hp_lunrate>=$p67 & n_spellength>=`ll' & n_spellength<=$max_uspell_mrg & quarter>=tq(1985q1) & quarter<=tq(2013q1)
	local sumwgt_temp=r(N)*r(mean)
			*display "`sumwgt_temp'"
	replace cum_mob_`indic'_high_u_wgt=`sumwgt_temp' if n_spellength==`ll' 

	margins if hp_lunrate>=$p67 & n_spellength>=`ll' & n_spellength<=$max_uspell_mrg & quarter>=tq(1985q1) & quarter<=tq(2013q1), at((mean) quarter)
	matrix rtable=r(table)
	replace cum_mob_`indic'_high_u_mrgs=rtable[1,1] if n_spellength==`ll' 
	replace cum_mob_`indic'_high_u_mrgs_lb=rtable[5,1] if n_spellength==`ll' 
	replace cum_mob_`indic'_high_u_mrgs_ub=rtable[6,1] if n_spellength==`ll' 
	 }
	*** LOW UNEMPLOYMENT 


	count if hp_lunrate<=$p33 & n_spellength>=`ll' & n_spellength<=$max_uspell_mrg & quarter>=tq(1985q1) & quarter<=tq(2013q1) & qn1_obs_mobcorr_`indic' !=.
	display "count ......." r(N)
	if r(N)>0 {
	su qn1_obs_mobcorr_`indic' if hp_lunrate<=$p33 & n_spellength>=`ll' & n_spellength<=$max_uspell_mrg & quarter>=tq(1985q1) & quarter<=tq(2013q1)
	local sumwgt_temp=r(N)*r(mean)
			*display "`sumwgt_temp'"
	replace cum_mob_`indic'_low_u_wgt=`sumwgt_temp' if n_spellength==`ll'  

	margins if hp_lunrate<=$p33 & n_spellength>=`ll' & n_spellength<=$max_uspell_mrg & quarter>=tq(1985q1) & quarter<=tq(2013q1), at((mean) quarter)
	matrix rtable=r(table)
	replace cum_mob_`indic'_low_u_mrgs=rtable[1,1] if n_spellength==`ll' 
	replace cum_mob_`indic'_low_u_mrgs_lb=rtable[5,1] if n_spellength==`ll' 
	replace cum_mob_`indic'_low_u_mrgs_ub=rtable[6,1] if n_spellength==`ll' 
	}
	}

end


	cummob_highlow_u_exe mm
	
	
save "${tempdata}/section2_5_cyclical_survival_shift_temp.dta", replace












//=======================================
// SMOOTH PROFILES
//==========================================


global smoothing_window=18

** DROP A LOT OF VARIABLES
keep cum_mob_*  n_spellength
duplicates drop cum_mob_*  n_spellength, force



	//PPPPPPPPPPPPPPPPPPPPPPPPPP
	capture program drop lowess_grossmob_high_low_u_exe
	program define lowess_grossmob_high_low_u_exe
				args ind

		capture drop lows_pdur_`ind'_high_u
		*capture drop lows_pdur_`ind'_corr_highu
		capture drop lows_pdur_`ind'_low_u
		*capture drop lows_pdur_`ind'_corr_lowu
		
		lowess cum_mob_`ind'_low_u_mrgs  n_spellength if n_spellength<=${smoothing_window}, gen(lows_pdur_`ind'_lowu) bwidth(1) nograph
		lowess cum_mob_`ind'_high_u_mrgs  n_spellength if n_spellength<=${smoothing_window}, gen(lows_pdur_`ind'_highu) bwidth(1) nograph
		
		cap n save "${tempdata}/lows_highlow_u_durprof_temp_`ind'.dta", replace

	end 
	//PPPPPPPPPPPPPPPPPPPPPPPPPP
	
	
	lowess_grossmob_high_low_u_exe mm
	
	//PPPPPPPPPPPPPPPPPPPPPPPPPP
	capture program drop merge_smooth_highlow_u_exe 
	program define merge_smooth_highlow_u_exe 
				args ind

		capture drop lpoly_pdur_`ind'_high_u
		capture drop lpoly_pdur_`ind'_low_u
		capture drop tempweight_`ind'_low_u
		capture drop tempweight_`ind'_high_u
		gen tempweight_`ind'_low_u=round(cum_mob_`ind'_low_u_wgt)
		gen tempweight_`ind'_high_u=round(cum_mob_`ind'_high_u_wgt)

		lpoly cum_mob_`ind'_low_u_mrgs n_spellength [fw=tempweight_`ind'_low_u] if n_spellength<=18 , gen(lpoly_pdur_`ind'_low_u) degree(2) at(n_spellength) bwidth(2) nograph
		lpoly cum_mob_`ind'_high_u_mrgs n_spellength [fw=tempweight_`ind'_high_u] if n_spellength<=18 , gen(lpoly_pdur_`ind'_high_u) degree(2) at(n_spellength) bwidth(2) nograph
		
		save "${tempdata}\lpoly_highlow_u_durprof_temp_`ind'.dta", replace

		capture drop tempweight_`ind'_low_u
		capture drop tempweight_`ind'_high_u
		
	end 
	//PPPPPPPPPPPPPPPPPPPPPPPPPP
	
	merge_smooth_highlow_u_exe  mm

	
**********************	
use "${tempdata}/section2_5_cyclical_survival_shift_temp.dta", clear
	
	//PPPPPPPPPPPPPPPPPPPPPPPPPP
	capture program drop merge_smooth_highlow_u_exe
	program define merge_smooth_highlow_u_exe
			args ind

			capture drop _merge
			capture drop lows_pdur_`ind'_lowu 
			capture drop lows_pdur_`ind'_highu 
			capture drop lpoly_pdur_`ind'_high_u 
			capture drop lpoly_pdur_`ind'_low_u
			
	*merge m:1 n_spellength using  "${datadir}/lows_highlow_u_durprof_temp_`ind'.dta", keepusing(lows_pdur_`ind'_lowu lows_pdur_`ind'_highu)
	merge m:1 n_spellength using  "${tempdata}\lpoly_highlow_u_durprof_temp_`ind'.dta", keepusing(lows_pdur_`ind'_lowu lows_pdur_`ind'_highu lpoly_pdur_`ind'_high_u lpoly_pdur_`ind'_low_u) 
	  
	end
	//PPPPPPPPPPPPPPPPPPPPPPPPPP

	merge_smooth_highlow_u_exe  mm
	
	

***********************************************
**  draw picture
************************************************
cap n use "${tempdata}/section2_5_cyclical_survival_shift_temp.dta"


	
	
//********************************
//  PICTURES USING  14 months at most
//*************************************


* set key constraint
 global max_uspell_mrg=14
 



//******************************************************************************
// picture with observations dropped when standard deviations become too wide
//*****************************************************************************
		
	//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
		capture program drop durprofileshift_pic_exe3
program define durprofileshift_pic_exe3
			args indic lbmark ubmark
			
sort  n_spellength quarter


	su cum_mob_`indic'_high_u_wgt if n_spellength==1
	local pop_highu_1=r(mean)
	display "`pop_highu_1'"

	forvalues i=2(1)$max_uspell_mrg {
	su cum_mob_`indic'_high_u_wgt if n_spellength==`i'
	local pop_highu_`i'=r(mean)
	display "`pop_highu_`i''"

	*local pop_highu_`i'=10*(`pop_highu_`i'')

	local pop_highu_`i'= `pop_highu_`i'' / `pop_highu_1'
	local pop_highu_`i'=10*`pop_highu_`i''
	display "`pop_highu_`i''"

	}
	
	forvalues i=1(1)$max_uspell_mrg {
	su cum_mob_`indic'_low_u_wgt if n_spellength==`i'
	local pop_lowu_`i'=r(mean)
	display "`pop_lowu_`i''"
	
	*local pop_lowu_`i'=10*(`pop_lowu_`i'')

	local pop_lowu_`i'= `pop_lowu_`i'' / `pop_highu_1'
	local pop_lowu_`i'=10*`pop_lowu_`i''
	display "`pop_lowu_`i''"

	}
	
	local pop_highu_1=10
	
	** ADJUST RANGES OF VARIABLES ***
	capture drop cum_mob_`indic'_high_u_mrgs_lba
	capture drop cum_mob_`indic'_high_u_mrgs_uba
	capture drop cum_mob_`indic'_low_u_mrgs_lba
	capture drop cum_mob_`indic'_low_u_mrgs_uba
	capture drop lpoly_pdur_`indic'_low_ua
	capture drop lpoly_pdur_`indic'_high_ua
	capture drop cum_mob_`indic'_high_u_mrgs_a
	capture drop cum_mob_`indic'_low_u_mrgs_a
	
	** band
	global maxconfband=0.2
	
	gen cum_mob_`indic'_high_u_mrgs_lba=cum_mob_`indic'_high_u_mrgs_lb if cum_mob_`indic'_high_u_mrgs_lb>=`lbmark'  & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_high_u_mrgs_uba=cum_mob_`indic'_high_u_mrgs_ub if cum_mob_`indic'_high_u_mrgs_ub<=`ubmark' & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
 	gen cum_mob_`indic'_low_u_mrgs_lba=cum_mob_`indic'_low_u_mrgs_lb if cum_mob_`indic'_low_u_mrgs_lb>=`lbmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_low_u_mrgs_uba=cum_mob_`indic'_low_u_mrgs_ub if cum_mob_`indic'_low_u_mrgs_ub<=`ubmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	gen lpoly_pdur_`indic'_high_ua=lpoly_pdur_`indic'_high_u if lpoly_pdur_`indic'_high_u>=`lbmark' & lpoly_pdur_`indic'_high_u<=`ubmark' & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
	gen lpoly_pdur_`indic'_low_ua=lpoly_pdur_`indic'_low_u if lpoly_pdur_`indic'_low_u>=`lbmark' & lpoly_pdur_`indic'_low_u<=`ubmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_high_u_mrgs_a=cum_mob_`indic'_high_u_mrgs if cum_mob_`indic'_high_u_mrgs>=`lbmark' & cum_mob_`indic'_high_u_mrgs<=`ubmark' & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_low_u_mrgs_a=cum_mob_`indic'_low_u_mrgs if cum_mob_`indic'_low_u_mrgs>=`lbmark' & cum_mob_`indic'_low_u_mrgs<=`ubmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	
	global maxtick 12
	#delimit ;
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=1 & n_spellength<=2, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_1' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=2 & n_spellength<=3, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_2' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=3 & n_spellength<=4, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_3' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=4 & n_spellength<=5, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_4' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=5 & n_spellength<=6, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_5' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=6 & n_spellength<=7, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_6' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=7 & n_spellength<=8, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_7' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=8 & n_spellength<=9, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_8' thin thin) xlabel(1(1)${maxtick}, labsize(large)) ||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=9 & n_spellength<=10, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_9' thin thin) xlabel(1(1)${maxtick}, labsize(large)) ||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=10 & n_spellength<=11, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_10' thin thin) xlabel(1(1)${maxtick}, labsize(large)) ||
	scatter lpoly_pdur_`indic'_high_ua cum_mob_`indic'_high_u_mrgs_lba  cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a n_spellength if n_spellength>=11 & n_spellength<=12, 
		connect(l l l i) msymbol(i i i Oh) msize(medsmall ..) lpattern(solid longdash longdash) lcolor(black black black) mcolor(black black black black) lwidth(*`pop_highu_11' thin thin) xlabel(1(1)${maxtick}, labsize(large)) ||
	
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=1 & n_spellength<=2, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_1' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=2 & n_spellength<=3, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_2' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=3 & n_spellength<=4, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_3' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=4 & n_spellength<=5, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_4' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=5 & n_spellength<=6, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_5' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=6 & n_spellength<=7, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_6' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=7 & n_spellength<=8, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_7' thin thin) xlabel(1(1)${maxtick}, labsize(large))||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=8 & n_spellength<=9, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_8' thin thin) xlabel(1(1)${maxtick}, labsize(large)) ||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=9 & n_spellength<=10, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_9' thin thin) xlabel(1(1)${maxtick}, labsize(large)) ||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=10 & n_spellength<=11, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_10' thin thin) xlabel(1(1)${maxtick}, labsize(large)) ||
	scatter lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba  cum_mob_`indic'_low_u_mrgs_uba  cum_mob_`indic'_low_u_mrgs_a  n_spellength if n_spellength>=11 & n_spellength<=12, 
		connect(l l l i) msymbol(i i i Sh) msize(medsmall ..) lpattern(solid "--...." "--....") lcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) mcolor(midblue*0.8 midblue*0.8 midblue*0.8 midblue*0.8) lwidth(*`pop_lowu_11' thin thin) xlabel(1(1)${maxtick}, labsize(large)) 
	legend(order( 45 46 1 2) cols(1) label(1 "Times of High U") label(45 "Times of Low U ") label(2 "   95% CI") label(46 "   95% CI") ring(0) pos(5) symxsize(10) size(*1.2)) xtitle("Months in Unemployment", size(large)) ytitle("Proportion Moving Occupation" "Upon Exit U", size(large) height(10)) graphregion(color(white)) ylabel(0.4(0.05)0.65, labsize(large))
		;
		#delimit cr
graph export "${outputdata}ts_cycl_durprofile_`indic'_14_mcb2.pdf", as(pdf) replace
if "`indic'"=="mm" {
	graph export "${mainresultsdir}/fig3a.pdf", as(pdf) replace
}
	
	capture drop cum_mob_`indic'_high_u_mrgs_lb_adj 
	capture drop cum_mob_`indic'_high_u_mrgs_ub_adj
	capture drop cum_mob_`indic'_low_u_mrgs_lb_adj
	capture drop cum_mob_`indic'_low_u_mrgs_ub_adj
	capture drop lpoly_pdur_`indic'_low_ua
	capture drop lpoly_pdur_`indic'_high_ua
	capture drop cum_mob_`indic'_high_u_mrgs_a
	capture drop cum_mob_`indic'_low_u_mrgs_a
	
	
	
end
 //XXXXXXXXXXXXXXXXXXX
	durprofileshift_pic_exe3 mm 0.3 0.7 //XXXXXXXXX

	
	
**********************************************************************
** CYCLICAL DURATION PROFILE SHIFT, FOR CALIBRATION
*********************************************************************	
	
preserve

keep n_spellength lows_pdur_mm_lowu lows_pdur_mm_highu
duplicates drop n_spellength lows_pdur_mm_lowu lows_pdur_mm_highu, force
drop if n_spellength>12
ren n_spellength duration
ren lows_pdur_mm_lowu mobduration_exp 
ren lows_pdur_mm_highu mobduration_rec


save "${mainresultsdir}/cycldurationshift_mog.dta", replace
restore 
	
	
	
*/
 //===============================================================
 //  SHIFT OF THE MOBILITY WITH PERCENTILES OF THE DURATION DISTRIBUTION
 //===============================================================
global max_uspell_mrg=16
global max_count_length=18

*use "${tempdata}/section2_3_cyclical_survival_shift_mark2.dta", clear

	duplicates drop n_spellength, force 
		
	capture program drop ts_distr_shift_exe3
	program define ts_distr_shift_exe3
				args indic lbmark ubmark
				
	su cum_mob_`indic'_high_u_wgt if n_spellength==1
	local pop_highu_1=r(mean)
	display "`pop_highu_1'"
	su cum_mob_`indic'_low_u_wgt if n_spellength==1
	local pop_lowu_1=r(mean)
	display "`pop_lowu_1'"
	
	capture drop cum_mob_`indic'_high_u_wgt_norm
	capture drop cum_mob_`indic'_high_u_wgt_nrmc
	gen cum_mob_`indic'_high_u_wgt_nrmc=(1-(cum_mob_`indic'_high_u_wgt/`pop_highu_1'))*100
	gen cum_mob_`indic'_high_u_wgt_norm=(1-(cum_mob_`indic'_high_u_wgt/`pop_highu_1'))*100
	capture drop cum_mob_`indic'_low_u_wgt_norm
	capture drop cum_mob_`indic'_low_u_wgt_nrmc
	gen cum_mob_`indic'_low_u_wgt_nrmc=(1-(cum_mob_`indic'_low_u_wgt/`pop_lowu_1'))*100
	gen cum_mob_`indic'_low_u_wgt_norm=(1-(cum_mob_`indic'_low_u_wgt/`pop_lowu_1'))*100
	
	capture drop cum_mob_`indic'_high_u_mrgs_lba
	capture drop cum_mob_`indic'_high_u_mrgs_uba
	capture drop cum_mob_`indic'_low_u_mrgs_lba
	capture drop cum_mob_`indic'_low_u_mrgs_uba
	capture drop lpoly_pdur_`indic'_low_ua
	capture drop lpoly_pdur_`indic'_high_ua
	capture drop cum_mob_`indic'_high_u_mrgs_a
	capture drop cum_mob_`indic'_low_u_mrgs_a
	
	** band
	global maxconfband=0.2
	
	gen cum_mob_`indic'_high_u_mrgs_lba=cum_mob_`indic'_high_u_mrgs_lb if cum_mob_`indic'_high_u_mrgs_lb>=`lbmark'  & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_high_u_mrgs_uba=cum_mob_`indic'_high_u_mrgs_ub if cum_mob_`indic'_high_u_mrgs_ub<=`ubmark' & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
 	gen cum_mob_`indic'_low_u_mrgs_lba=cum_mob_`indic'_low_u_mrgs_lb if cum_mob_`indic'_low_u_mrgs_lb>=`lbmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_low_u_mrgs_uba=cum_mob_`indic'_low_u_mrgs_ub if cum_mob_`indic'_low_u_mrgs_ub<=`ubmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	gen lpoly_pdur_`indic'_high_ua=lpoly_pdur_`indic'_high_u if lpoly_pdur_`indic'_high_u>=`lbmark' & lpoly_pdur_`indic'_high_u<=`ubmark' & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
	gen lpoly_pdur_`indic'_low_ua=lpoly_pdur_`indic'_low_u if lpoly_pdur_`indic'_low_u>=`lbmark' & lpoly_pdur_`indic'_low_u<=`ubmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_high_u_mrgs_a=cum_mob_`indic'_high_u_mrgs if cum_mob_`indic'_high_u_mrgs>=`lbmark' & cum_mob_`indic'_high_u_mrgs<=`ubmark' & cum_mob_`indic'_high_u_mrgs_ub-cum_mob_`indic'_high_u_mrgs_lb<=$maxconfband
	gen cum_mob_`indic'_low_u_mrgs_a=cum_mob_`indic'_low_u_mrgs if cum_mob_`indic'_low_u_mrgs>=`lbmark' & cum_mob_`indic'_low_u_mrgs<=`ubmark' & cum_mob_`indic'_low_u_mrgs_ub-cum_mob_`indic'_low_u_mrgs_lb<=$maxconfband
	
	
	
	*lpoly_pdur_`indic'_high_u 
	#delimit ;
	scatter 	lpoly_pdur_`indic'_high_ua  cum_mob_`indic'_high_u_mrgs_lba cum_mob_`indic'_high_u_mrgs_uba  cum_mob_`indic'_high_u_mrgs_a  cum_mob_`indic'_high_u_wgt_norm if n_spellength<=12, 
	xtitle("Percentage x of unemployment spells, ranked increasingly in duration") ytitle("Average occ. mobility of (100-x)%" "longest spells") graphregion(color(white)) msymbol(i i i Dh) connect(l l l i) lpattern(solid dash dash) mlabel(n_spellength .)  
	lcolor(black black black) lwidth(thick thin thin ) mcolor(black .. ) mlabcolor(black .. ) mlabpos(6) mlabgap(2) yscale(titlegap(2))||
	scatter 	lpoly_pdur_`indic'_low_ua cum_mob_`indic'_low_u_mrgs_lba cum_mob_`indic'_low_u_mrgs_uba   cum_mob_`indic'_low_u_mrgs_a cum_mob_`indic'_low_u_wgt_norm if n_spellength<=12, 
	msymbol(i i i Th) connect(l l l i) mlabel(n_spellength .) lpattern(solid shortdash shortdash) 
	lcolor(midblue .. ) lwidth(thick ) mcolor(midblue .. ) mlabcolor(midblue .. ) mlabpos(12) mlabgap(2) legend(order(1 5 2 6) rows(2) label(1 "Bad Times") label(2 "95% CI") label(5 "Good times") label(6 "95% CI"))
	
	;	
	#delimit cr
	graph export "${outputdata}ts_cycl_durprofile_`indic'_distrshift_12m_bnds.pdf", as(pdf) replace
	if "`indic'"=="mm" {
		graph export "${mainresultsdir}/fig6_dataonly.pdf", as(pdf) replace
	}

	
	//xxxxxxxxxxxxxxxxxxxx
	
	end
		
	
	ts_distr_shift_exe3 mm 0.3 0.7
	
			
		

**********************************************************************
** SAVING THE INFO FOR FIGURE 6
*********************************************************************	

	/* COMPARISON WITH step4_s5_drawing_pictures.do
	gdcm_lb_d				- cum_mob_`indic'_low_u_mrgs_lba 
    gdcm_ub_d				- cum_mob_`indic'_low_u_mrgs_uba 
    gdcmdur_d				- cum_mob_`indic'_low_u_mrgs_a
							- smoothed: lpoly_pdur_`indic'_low_ua
	gdnspel_d_inv			- cum_mob_`indic'_low_u_wgt_norm
    
    badcm_lb_d 				- cum_mob_`indic'_high_u_mrgs_lba 
    badcm_ub_d 				- cum_mob_`indic'_high_u_mrgs_uba  
    badcmdur_d				- cum_mob_`indic'_high_u_mrgs_a
							- smoothed: lpoly_pdur_`indic'_high_ua
	badnspel_d_inv			- cum_mob_`indic'_high_u_wgt_norm
							
    badcmdur_m          MODEL EQUIVALENT
    gdcmdur_m           MODEL EQUIVALENT
	*/ 
	

preserve

keep n_spellength cum_mob_mm_low_u_mrgs_lba cum_mob_mm_low_u_mrgs_uba ///
cum_mob_mm_low_u_mrgs_a cum_mob_mm_low_u_wgt_norm lpoly_pdur_mm_low_ua ///
cum_mob_mm_high_u_mrgs_lba cum_mob_mm_high_u_mrgs_uba ///
cum_mob_mm_high_u_mrgs_a cum_mob_mm_high_u_wgt_norm lpoly_pdur_mm_high_ua

lab var cum_mob_mm_low_u_mrgs_lba "expansion occmob p. months in unemployment, ci lower bound"
lab var cum_mob_mm_low_u_mrgs_uba "expansion occmob p. months in unemployment, ci upper bound" 
lab var cum_mob_mm_low_u_mrgs_a   "expansion occmob p. months in unemployment"
lab var cum_mob_mm_low_u_wgt_norm "expansion cum prop of spells p. months in unemployment"	
lab var lpoly_pdur_mm_low_ua 	  "expansion occmob p. months in unemployment (smoothed)"
lab var cum_mob_mm_high_u_mrgs_lba "recession occmob p. months in unemployment, ci lower bound"
lab var cum_mob_mm_high_u_mrgs_uba "recession occmob p. months in unemployment, ci upper bound"
lab var cum_mob_mm_high_u_mrgs_a   "recession occmob p. months in unemployment"
lab var cum_mob_mm_high_u_wgt_norm "recession cum prop of spells p. months in unemployment"	
lab var lpoly_pdur_mm_high_ua	   "recession occmob p. months in unemployment (smoothed)"

ren cum_mob_mm_low_u_mrgs_lba gdcm_lb_d
ren cum_mob_mm_low_u_mrgs_uba gdcm_ub_d
ren cum_mob_mm_low_u_mrgs_a   gdcmdur_d	
ren cum_mob_mm_low_u_wgt_norm gdnspel_d_inv	
ren lpoly_pdur_mm_low_ua 	  gdcmdur_sm_d	
ren cum_mob_mm_high_u_mrgs_lba badcm_lb_d
ren cum_mob_mm_high_u_mrgs_uba badcm_ub_d
ren cum_mob_mm_high_u_mrgs_a   badcmdur_d		
ren cum_mob_mm_high_u_wgt_norm badnspel_d_inv
ren lpoly_pdur_mm_high_ua	   badcmdur_sm_d	

save "${mainresultsdir}/fig6data_cycldurshift_ptiles.dta", replace

restore 



********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"