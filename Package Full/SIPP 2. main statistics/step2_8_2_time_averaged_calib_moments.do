
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







********************************************************************************
***
***  1. CALCULATE SEPARATION MOMENTS FOR CALIBRATION, UNEMP. CONCENTRATION
***	 2. OCC SIZE EVOLUTION 1984-2012
***	 3. MISCODING STATS				
***
********************************************************************************


  


//===================================
** PRELIMINARY
//===================================

	clear

	cd "$workingdir"
	global workingdir `c(pwd)'
	do "${workingdir}/global_paths.do"
	
	version 13
	
	set more off
	set varabbrev off

	
	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"

	cd "${tempdata}"

	quietly do "${step2codedir}/aux_directory/aux_programs.do"
	quietly do "${step1codedir}/Ginv_matrices.do"

	
cap n use "${outputdata}/corewave_occlfmin_ctv.dta", clear
	
	
drop if quarter<=tq(1970q1)

** define leocc1bfr_mmo2, based on leocc1bfr_mmo and empl_ctv
capture drop leocc1bfr2_mmo
gen leocc1bfr2_mmo=leocc1bfr_mmo
replace leocc1bfr2_mmo=leocc1bfr2_mmo[_n-1] if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl_ctv==1 & empl!=1 & empl_ctv[_n-1]==1 & empl[_n-1]==1 ///
				& leocc1bfr_mmo[_n-1]!=leocc1bfr_mmo


//===================================================
**SEPARATION RATES: 
//====================================================
*
*- one year ahead rates: take an employed person, with at least one year left in sample, does this person have an unempl_ctv observation in that year
*- one year ahead rates: take an employed person, with at least one year left in sample, does this person have an unempl_ctv observation in that year, part of (in)complete_uspell
*- one year ahead rates: take an employed person, with at least 2.5 years left in sample, does this person have an unempl_ctv observation in that year, part of complete_uspell
*- one year ahead rates: take an employed person, with at least one year left in sample, does this person have an unempl_ctv observation in that year, part of (in)complete_nunspell
*- one year ahead rates: take an employed person, with at least 2.5 years left in sample, does this person have an unempl_ctv observation in that year, part of complete_nunspell


// PRESENCE OF U NEXT YEAR 
		* pure u or e: eu12m_ctv
		* u relative to everyone who is currently employed (even incl. those who leave the labor force for the rest of the year): eu12m_ctv_nun: DEFAULT, EVEN FOR U CALIBRATION !!!

sort personkey yearmonth
global addcondition=""

capture program drop eu_ahead_exe
program define eu_ahead_exe
			args monthsahead namesuffix
capture drop eu`monthsahead'm_ctv`namesuffix'
capture drop eu`monthsahead'm_ctv_2`namesuffix'

gen byte eu`monthsahead'm_ctv`namesuffix'=.
replace eu`monthsahead'm_ctv`namesuffix'=0 if empl_ctv==1 & sample_timetogo>=`monthsahead' & personkey==personkey[_n+`monthsahead'] ///
														& yearmonth==yearmonth[_n+`monthsahead']-`monthsahead' $addcondition
gen byte eu`monthsahead'm_ctv_2`namesuffix'=.
replace eu`monthsahead'm_ctv_2`namesuffix'=0 if empl_ctv==1 & sample_timetogo>=`monthsahead' & personkey==personkey[_n+`monthsahead'] ///
														& yearmonth==yearmonth[_n+`monthsahead']-`monthsahead' $addcondition

forvalues i=1(1)`monthsahead' {

replace eu`monthsahead'm_ctv`namesuffix'=.     if eu`monthsahead'm_ctv`namesuffix'!=.     & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (empl_ctv[_n+`i']!=1 & unempl_ctv[_n+`i']!=1) $addcondition
replace eu`monthsahead'm_ctv`namesuffix'=1     if eu`monthsahead'm_ctv`namesuffix'!=.     & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (unempl_ctv[_n+`i']==1) $addcondition
replace eu`monthsahead'm_ctv_2`namesuffix'=1 if eu`monthsahead'm_ctv_2`namesuffix'!=. & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (unempl_ctv[_n+`i']==1) $addcondition
*replace eu`monthsahead'm_ctv_2=-1 if (eu`monthsahead'm_ctv_2!=. & eu`monthsahead'm_ctv_2!=1) & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (empl_ctv[_n+`i']!=1 & unempl_ctv[_n+`i']!=1)
}

end 

** NUMBER IS MONTHS AHEAD 
global addcondition " "
eu_ahead_exe 1			
eu_ahead_exe 2
eu_ahead_exe 3
eu_ahead_exe 4
eu_ahead_exe 5
eu_ahead_exe 6
eu_ahead_exe 7
eu_ahead_exe 8
eu_ahead_exe 9
eu_ahead_exe 10
eu_ahead_exe 11

// ONE MONTH AHEAD
global addcondition " "
*eu_ahead_exe 1
su eu1m_ctv [w=pweight2]
global addcondition " & tage>=20 & tage<=30 & entry_ind==1 "
eu_ahead_exe 1 _young
su eu1m_ctv_young [w=pweight2]
local sep_yng_mean=r(mean)
global addcondition " & tage>=35 & tage<=55 & entry_ind==1 "
eu_ahead_exe 1 _prime
su eu1m_ctv_prime [w=pweight2]
local sep_prm_mean=r(mean)
local rel_sep_yng2prm=`sep_yng_mean'/`sep_prm_mean'
display "`rel_sep_yng2prm'"


			/*
			quietly {
			cap log close relsepratelog
			log using "${mainresultsdir}/table2_apptable4_5_7_rel_separation_y_p.txt", replace text name(relsepratelog)

			noisily: display  ""
			noisily: display  "--------------------------------------------------------"
			noisily: display  "RELATIVE SEPARATION RATE YOUNG TO PRIME-AGED"
			noisily: display  "--------------------------------------------------------"
			noisily: display  ""
			noisily: display  " equals `rel_sep_yng2prm'"

			log close relsepratelog
				

			}		
			*/

//nun
su eu1m_ctv_2_young [w=pweight2]
local sep_yng_mean=r(mean)
su eu1m_ctv_2_prime [w=pweight2]
local sep_prm_mean=r(mean)
local rel_sep_yng2prm=`sep_yng_mean'/`sep_prm_mean'
display "`rel_sep_yng2prm'"



// ONE YEAR AHEAD
global addcondition " "

eu_ahead_exe 12
su eu12m_ctv [w=pweight2]
su eu12m_ctv_2 [w=pweight2]
global addcondition " & tage>=20 & tage<=30 & entry_ind==1 "
eu_ahead_exe 12 _young
su eu12m_ctv_young [w=pweight2]
su eu12m_ctv_2_young [w=pweight2]
global addcondition " & tage>=35 & tage<=55 & entry_ind==1 "
eu_ahead_exe 12 _prime
su eu12m_ctv_prime [w=pweight2]
su eu12m_ctv_2_prime [w=pweight2]
// ALL OTHER DURATIONS
global addcondition " "
forvalues i=2(1)11 {
su eu`i'm_ctv [w=pweight2]
su eu`i'm_ctv_2 [w=pweight2] 
}

// 30 months ahead, at moment of hiring from unemployment
sort personkey yearmonth

global addcondition " & (empl_ctv[_n-1]!=1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & complete_nunspell[_n-1]==1 & lne_c_mmo[_n-1]==0 & wave[_n-1]<=3 & interview_no2[_n-1]<=12 & n_spellength[_n-1]>=2 & n_spellength[_n-1]<=10 )"
count if empl_ctv==1 & sample_timetogo>=30 & personkey==personkey[_n+30] & yearmonth==yearmonth[_n+30]-30 $addcondition

eu_ahead_exe 30 _sepnocc_2_5
su eu30m_ctv_sepnocc_2_5 [w=pweight2]
su eu30m_ctv_2_sepnocc_2_5 [w=pweight2]


//=================================
// POST-HIRE SEPARATION RATES 
//===================================


// AVERAGE MONTHLY SEPARATION RATE OVER THE FIRST 12 MONTHS OF EMPLOYMENT (POST HIRE FROM U), FOR OCCUPATIONAL MOVERS AND STAYERS 

		 ** employ_dur_ctv uses the empl_ctv, unempl_ctv status to calculate time in employment. 
		 ** This is different from employ_dur, which takes empl/unempl, and therefore some quick transitions through unemployment are counted 
		 **	as separate spells 
		 
capture drop employ_dur_ctv
sort personkey yearmonth
gen employ_dur_ctv=1 if empl_ctv==1 & unempl_ctv[_n-1]==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
replace employ_dur_ctv=employ_dur_ctv[_n-1]+1 if empl_ctv==1 & empl_ctv[_n-1]==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & employ_dur_ctv==. & employ_dur_ctv[_n-1]!=.

//=========
// FIRST WAY OF CALCULATING IT: using leocc1bfr_mmo, and occ_mmo

capture drop eu_avemonthly_12mposthire
capture drop eu_avemonthly_12mposthire_y
capture drop eu_avemonthly_12mposthire_p
capture drop eu_avemonthly_12mposthire_move
capture drop eu_avemonthly_12mposthire_move_y
capture drop eu_avemonthly_12mposthire_move_p
capture drop eu_avemonthly_12mposthire_stay
capture drop eu_avemonthly_12mposthire_stay_y
capture drop eu_avemonthly_12mposthire_stay_p


		** interview_no2>=28, because full unemployment spell needs to be completed, and a year needs to be in sample!
		** CAREFUL: leocc1bfr2_mmo uses unempl and empl, not unempl_ctv, empl_ctv, and so sometimes switches already forward in the month of separation (empl==0 & empl_ctv==1
global employstay_cond "employ_dur_ctv<=12 & empl_ctv==1 & empl_ctv[_n+1]==1 & personkey[_n-1]==personkey[_n+1] & yearmonth[_n-1]==yearmonth[_n+1]-2"

gen eu_avemonthly_12mposthire=0 if  ${employstay_cond} & entry_ind==1 & interview_no2>=28
gen eu_avemonthly_12mposthire_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & interview_no2>=28
gen eu_avemonthly_12mposthire_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & interview_no2>=28
gen eu_avemonthly_12mposthire_move=0 if  ${employstay_cond} & entry_ind==1 & leocc1bfr2_mmo!=occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
gen eu_avemonthly_12mposthire_move_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & leocc1bfr2_mmo!=occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
gen eu_avemonthly_12mposthire_move_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & leocc1bfr2_mmo!=occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
gen eu_avemonthly_12mposthire_stay=0  if  ${employstay_cond} & entry_ind==1 & leocc1bfr2_mmo==occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
gen eu_avemonthly_12mposthire_stay_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & leocc1bfr2_mmo==occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
gen eu_avemonthly_12mposthire_stay_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & leocc1bfr2_mmo==occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28

global employsep_cond "employ_dur_ctv<=12 & empl_ctv==1 & unempl_ctv[_n+1]==1 & personkey[_n-1]==personkey[_n+1] & yearmonth[_n-1]==yearmonth[_n+1]-2"
replace eu_avemonthly_12mposthire=1 if  ${employsep_cond} & entry_ind==1 & interview_no2>=28
replace eu_avemonthly_12mposthire_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & interview_no2>=28
replace eu_avemonthly_12mposthire_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & interview_no2>=28
replace eu_avemonthly_12mposthire_move=1 if  ${employsep_cond} & entry_ind==1 & leocc1bfr2_mmo!=occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
replace eu_avemonthly_12mposthire_move_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & leocc1bfr2_mmo!=occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
replace eu_avemonthly_12mposthire_move_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & leocc1bfr2_mmo!=occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
replace eu_avemonthly_12mposthire_stay=1  if  ${employsep_cond} & entry_ind==1 & leocc1bfr2_mmo==occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
replace eu_avemonthly_12mposthire_stay_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & leocc1bfr2_mmo==occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28
replace eu_avemonthly_12mposthire_stay_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & leocc1bfr2_mmo==occ_mmo & occ_mmo!=. &  leocc1bfr2_mmo!=. & interview_no2>=28


su eu_avemonthly_12mposthire eu_avemonthly_12mposthire_y eu_avemonthly_12mposthire_p ///
eu_avemonthly_12mposthire_move eu_avemonthly_12mposthire_move_y eu_avemonthly_12mposthire_move_p ///
 eu_avemonthly_12mposthire_stay eu_avemonthly_12mposthire_stay_y eu_avemonthly_12mposthire_stay_p

 
 *** FOOTNOTE 22
 su eu_avemonthly_12mposthire_move eu_avemonthly_12mposthire_stay
 su eu_avemonthly_12mposthire_move eu_avemonthly_12mposthire_stay [w=pweight2]
 //=======================================
 // SECOND WAY OF CALCULATING IT : using locc1bfr_mmo and locc1aft_mmo, while taking sample backwards by employ_dur_ctv
 
capture drop eu_avmth2_12mposthire
capture drop eu_avmth2_12mposthire_y
capture drop eu_avmth2_12mposthire_p
capture drop eu_avmth2_12mposthire_move
capture drop eu_avmth2_12mposthire_move_y
capture drop eu_avmth2_12mposthire_move_p
capture drop eu_avmth2_12mposthire_stay
capture drop eu_avmth2_12mposthire_stay_y
capture drop eu_avmth2_12mposthire_stay_p


		** interview_no2>=28, because full unemployment spell needs to be completed, and a year needs to be in sample!
		
global employstay_cond "employ_dur_ctv==1 & empl_ctv==1 & empl_ctv[_n+1]==1 & personkey[_n-1]==personkey[_n+1] & yearmonth[_n-1]==yearmonth[_n+1]-2 & locc1bfr_mmo[_n-1]!=. & locc1aft_mmo[_n-1]!=."
gen eu_avmth2_12mposthire=0 if  ${employstay_cond} & entry_ind==1 & interview_no2>=28
gen eu_avmth2_12mposthire_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & interview_no2>=28
gen eu_avmth2_12mposthire_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & interview_no2>=28
gen eu_avmth2_12mposthire_move=0 if  ${employstay_cond} & entry_ind==1 & locc1bfr_mmo[_n-1]!=locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
gen eu_avmth2_12mposthire_move_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-1]!=locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
gen eu_avmth2_12mposthire_move_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-1]!=locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
gen eu_avmth2_12mposthire_stay=0  if  ${employstay_cond} & entry_ind==1 & locc1bfr_mmo[_n-1]==locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
gen eu_avmth2_12mposthire_stay_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-1]==locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
gen eu_avmth2_12mposthire_stay_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-1]==locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28


global employsep_cond "employ_dur_ctv==1 & empl_ctv==1 & unempl_ctv[_n+1]==1 & personkey[_n-1]==personkey[_n+1] & yearmonth[_n-1]==yearmonth[_n+1]-2 & locc1bfr_mmo[_n-1]!=. & locc1aft_mmo[_n-1]!=."
replace eu_avmth2_12mposthire=1 if  ${employsep_cond} & entry_ind==1 & interview_no2>=28
replace eu_avmth2_12mposthire_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & interview_no2>=28
replace eu_avmth2_12mposthire_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & interview_no2>=28
replace eu_avmth2_12mposthire_move=1 if  ${employsep_cond} & entry_ind==1 & locc1bfr_mmo[_n-1]!=locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
replace eu_avmth2_12mposthire_move_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-1]!=locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
replace eu_avmth2_12mposthire_move_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-1]!=locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay=1  if  ${employsep_cond} & entry_ind==1 & locc1bfr_mmo[_n-1]==locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-1]==locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-1]==locc1aft_mmo[_n-1] & locc1aft_mmo[_n-1]!=. &  locc1bfr_mmo[_n-1]!=. & interview_no2>=28




forvalues i=2(1)12 {

global employstay_cond "employ_dur_ctv==`i' & empl_ctv==1 & empl_ctv[_n+1]==1 & personkey[_n-`i']==personkey[_n+1] & yearmonth[_n-`i']==yearmonth[_n+1]-(1+`i') & locc1bfr_mmo[_n-`i']!=. & locc1aft_mmo[_n-`i']!=."
replace eu_avmth2_12mposthire=0 if  ${employstay_cond} & entry_ind==1 & interview_no2>=28
/*
replace eu_avmth2_12mposthire_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & interview_no2>=28
replace eu_avmth2_12mposthire_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & interview_no2>=28
replace eu_avmth2_12mposthire_move=0 if  ${employstay_cond} & entry_ind==1 & locc1bfr_mmo[_n-`i']!=locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_move_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-`i']!=locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_move_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-`i']!=locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay=0  if  ${employstay_cond} & entry_ind==1 & locc1bfr_mmo[_n-`i']==locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay_y=0 if  ${employstay_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-`i']==locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay_p=0 if  ${employstay_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-`i']==locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
*/

global employsep_cond "employ_dur_ctv==`i' & empl_ctv==1 & unempl_ctv[_n+1]==1 & personkey[_n-`i']==personkey[_n+1] & yearmonth[_n-`i']==yearmonth[_n+1]-(1+`i') & locc1bfr_mmo[_n-`i']!=. & locc1aft_mmo[_n-`i']!=."
replace eu_avmth2_12mposthire=1 if  ${employsep_cond} & entry_ind==1 & interview_no2>=28
/*
replace eu_avmth2_12mposthire_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & interview_no2>=28
replace eu_avmth2_12mposthire_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & interview_no2>=28
replace eu_avmth2_12mposthire_move=1 if  ${employsep_cond} & entry_ind==1 & locc1bfr_mmo[_n-`i']!=locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_move_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-`i']!=locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_move_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-`i']!=locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay=1  if  ${employsep_cond} & entry_ind==1 & locc1bfr_mmo[_n-`i']==locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay_y=1 if  ${employsep_cond} & entry_ind==1 & tage>=20 & tage<=30 & locc1bfr_mmo[_n-`i']==locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
replace eu_avmth2_12mposthire_stay_p=1 if  ${employsep_cond} & entry_ind==1 & tage>=35 & tage<=55 & locc1bfr_mmo[_n-`i']==locc1aft_mmo[_n-`i'] & locc1aft_mmo[_n-`i']!=. &  locc1bfr_mmo[_n-`i']!=. & interview_no2>=28
*/
}


// SIMILAR OUTCOMES !!!

su eu_avmth2_12mposthire
/*
 eu_avmth2_12mposthire_y eu_avmth2_12mposthire_p ///
eu_avmth2_12mposthire_move eu_avmth2_12mposthire_move_y eu_avmth2_12mposthire_move_p ///
 eu_avmth2_12mposthire_stay eu_avmth2_12mposthire_stay_y eu_avmth2_12mposthire_stay_p,  f(%8.4f)

fsum eu_avemonthly_12mposthire eu_avemonthly_12mposthire_y eu_avemonthly_12mposthire_p ///
eu_avemonthly_12mposthire_move eu_avemonthly_12mposthire_move_y eu_avemonthly_12mposthire_move_p ///
 eu_avemonthly_12mposthire_stay eu_avemonthly_12mposthire_stay_y eu_avemonthly_12mposthire_stay_p,  f(%8.4f)
*/


//==========================================
// THIRD MEASURE: MONTH-BY-MONTH SEPARATION RATE: no reference to previous occupational mobility
 
sort personkey yearmonth

capture program drop eu_ahead_post_ue_exe
program define eu_ahead_post_ue_exe
			args monthsahead
			
sort personkey yearmonth

capture drop eu`monthsahead'm_athire_ctv
capture drop eu`monthsahead'm_athire_ctv_2

gen byte eu`monthsahead'm_athire_ctv=0 if empl_ctv==1 & unempl_ctv[_n-1]==1 & sample_timetogo>=`monthsahead' & personkey[_n-1]==personkey[_n+`monthsahead'] & yearmonth[_n-1]==yearmonth[_n+`monthsahead']-`monthsahead'-1
gen byte eu`monthsahead'm_athire_ctv_2=0 if empl_ctv==1 & unempl_ctv[_n-1]==1 & sample_timetogo>=`monthsahead' & personkey[_n-1]==personkey[_n+`monthsahead'] & yearmonth[_n-1]==yearmonth[_n+`monthsahead']-`monthsahead'-1

forvalues i=1(1)`monthsahead' {

replace eu`monthsahead'm_athire_ctv=. if eu`monthsahead'm_athire_ctv!=. & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (empl_ctv[_n+`i']!=1 & unempl_ctv[_n+`i']!=1)
replace eu`monthsahead'm_athire_ctv=1 if eu`monthsahead'm_athire_ctv!=. & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (unempl_ctv[_n+`i']==1)
replace eu`monthsahead'm_athire_ctv_2=1 if eu`monthsahead'm_athire_ctv_2!=. & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (unempl_ctv[_n+`i']==1)
*replace eu`monthsahead'm_athire_ctv_2=-1 if (eu`monthsahead'm_athire_ctv_2!=. & eu`monthsahead'm_athire_ctv_2!=1) & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & (empl_ctv[_n+`i']!=1 & unempl_ctv[_n+`i']!=1)
}

end 

** NUMBER IS MONTHS AHEAD 
eu_ahead_post_ue_exe 1
eu_ahead_post_ue_exe 2
eu_ahead_post_ue_exe 3
eu_ahead_post_ue_exe 4
eu_ahead_post_ue_exe 5
eu_ahead_post_ue_exe 6
eu_ahead_post_ue_exe 7
eu_ahead_post_ue_exe 8
eu_ahead_post_ue_exe 9
eu_ahead_post_ue_exe 10
eu_ahead_post_ue_exe 11
eu_ahead_post_ue_exe 12


forvalues i=1(1)12 {
su eu`i'm_athire_ctv [w=pweight2]
}


forvalues i=1(1)12 {
su eu`i'm_athire_ctv_2 [w=pweight2] 
}

//====================================			
**UNEMPLOYMENT CONCENTRATION: THIS IS WHERE WE CALCULATE THE MOMENT FOR THE PAPER
//====================================




**** within 3 yrs
sort personkey yearmonth 

capture drop total_utime
capture drop total_etime
gen total_utime=0 if (personkey!=personkey[_n-1] | interview_no2==1)  & unempl_ctv!=1 
replace total_utime=1 if (personkey!=personkey[_n-1] | interview_no2==1) & unempl_ctv==1 
gen total_etime=1 if (personkey!=personkey[_n-1] | interview_no2==1) & empl_ctv==1 
replace total_etime=0 if (personkey!=personkey[_n-1] | interview_no2==1) & empl_ctv!=1 

sort personkey yearmonth 
forvalues i=1(1)64 {
count if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & total_utime[_n-1]>=0 & total_utime[_n-1]<. & total_utime==.
if r(N) == 0 {
continue, break 
}
replace total_utime=total_utime[_n-1] if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & unempl_ctv!=1 & total_utime[_n-1]>=0 & total_utime[_n-1]<.
replace total_utime=total_utime[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & unempl_ctv==1 & total_utime[_n-1]>=0 & total_utime[_n-1]<.
}


forvalues i=1(1)64 {
count if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & total_etime[_n-1]>=0 & total_etime[_n-1]<. & total_etime==.
if r(N)==0 {
continue, break
}
replace total_etime=total_etime[_n-1] if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl_ctv!=1 & total_etime[_n-1]>=0 & total_etime[_n-1]<. & total_etime==.
replace total_etime=total_etime[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl_ctv==1  & total_etime[_n-1]>=0 & total_etime[_n-1]<. & total_etime==.

}




sort personkey yearmonth
capture drop total_utime_36mahead
gen total_utime_36mahead=total_utime[_n+35]-total_utime if interview_no2[_n+35]==interview_no2+35 & personkey==personkey[_n+35]
capture drop pure_u_or_etime_36mahead
gen pure_u_or_etime_36mahead=0 if interview_no2[_n+35]==interview_no2+35 & personkey==personkey[_n+35]
replace pure_u_or_etime_36mahead=1 if ((total_utime[_n+35]-total_utime)+(total_etime[_n+35]-total_etime))==35 & interview_no2[_n+35]==interview_no2+35 & personkey==personkey[_n+35]


** rolling 36 window used in calibration
cap n tab total_utime_36mahead [aw=pweight2] if interview_no2[_n+36]==interview_no2+36 & personkey==personkey[_n+36] & entry_ind==1 & tage<=65 & tage>=18 & empl_ctv==1 


//====================================			
**REPORTING THE RESULTS
//====================================


su eu1m_ctv_2_young [w=pweight2]
local sep_yng_mean=r(mean)
su eu1m_ctv_2_prime [w=pweight2]
local sep_prm_mean=r(mean)
local rel_sep_yng2prm=`sep_yng_mean'/`sep_prm_mean'
display "`rel_sep_yng2prm'"

su eu12m_ctv_2 [w=pweight2]
local sep_eu12m=r(mean)
su eu12m_athire_ctv_2 [w=pweight2] 
local sep_eu12m_posthire=r(mean)
local sep_eu12m_posthire_all=`sep_eu12m_posthire'/`sep_eu12m'
display "`sep_eu12m_posthire_all'"


tab total_utime_36mahead [aw=pweight2] if interview_no2[_n+36]==interview_no2+36 ///
						& personkey==personkey[_n+36] & entry_ind==1 & tage<=65 & tage>=18 & empl_ctv==1 

su pweight2 if total_utime_36mahead!=. & interview_no2[_n+36]==interview_no2+36 ///
						& personkey==personkey[_n+36] & entry_ind==1 & tage<=65 & tage>=18 & empl_ctv==1 
local totalpop=r(sum)

su pweight2 if total_utime_36mahead==0 & interview_no2[_n+36]==interview_no2+36 ///
						& personkey==personkey[_n+36] & entry_ind==1 & tage<=65 & tage>=18 & empl_ctv==1 
local uproplocal=1.0-(r(sum)/`totalpop')


quietly {
cap log close relsepratelog
log using "${mainresultsdir}/table2_appxtable4_separation_uconcentration.txt", replace text name(relsepratelog)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "RELATIVE SEPARATION RATE YOUNG TO PRIME-AGED"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " equals `rel_sep_yng2prm'"
noisily: display  "        -> sep yng = `sep_yng_mean' and sep prime = `sep_prm_mean'"


noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "RELATIVE SEPARATION 1 YR POST-HIRE FROM U REL. TO ALL"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " equals `sep_eu12m_posthire_all'"
noisily: display  "        -> sep 12m post-hire = `sep_eu12m_posthire' and sep 12m for emp = ``sep_eu12m'"


noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "FOOTNOTE 22 SEPARATION RISK POST-HIRE FOR OCC STAYER/MOVERS"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: su eu_avemonthly_12mposthire_move eu_avemonthly_12mposthire_stay [w=pweight2]
noisily: display  ""
noisily: display  "where eu_avemonthly_12mposthire_move is the ave monthly separation risk"
noisily: display  "in the first year post-hire of those who found a job out of unemployment"
noisily: display  "in a different occupation than previously; "
noisily: display  "eu_avemonthly_12mposthire_stay is the corresponding stat for occ stayers"
noisily: display  "above abbrev as eu_avemon~ve and eu_avemon~ay, respectively"

noisily: display  ""
noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "UNEMPLOYMENT CONCENTRATION"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: display  "`uproplocal'"
noisily: display  "the statistic here is the proportion of the currently employed"
noisily: display  "that will experience unemployment within the next 3 years"





log close relsepratelog
	

}		


********************************************************************************
**   MISCODING FLOWS
********************************************************************************



		
****** MISCODING FLOWS OF ALL EMPLOYED ******************************		

tab occ_mmo [aw=pweight2] if empl_ctv==1, matcell(mogmatrix)

matrix list mogmatrix
matrix unitmog=J(22,1,1)
matrix summogmatrix=unitmog'*mogmatrix

matrix mogmatrixdistr=mogmatrix/summogmatrix[1,1]
matrix list mogmatrixdistr			// 22 occ distribution matrix


matrix mogmiscode=mogmatrixdistr # Gmat_mm
matrix list mogmiscode

matrix tempmog=(mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr,    mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr) 

matrix list tempmog
matrix mogmiscode=hadamard(tempmog, Gmat_mm)
matrix list mogmiscode

* EXPORT 22 MOG MISCODING MATRIX

putexcel set "${outputdata}/mogmiscoding_flows_v2.xlsx", sheet("miscoding_flows_employed") replace
putexcel B2=matrix(mogmiscode)




* MISCODING AT THE SUPER OCC LEVEL 

matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

										 
										 
matrix avemisc_rtnm=rtmmconv_tpose*mogmiscode*rtmmconv_tpose'
matrix list avemisc_rtnm

matrix unitsupocc=J(4,1,1)
matrix list unitsupocc

matrix norm_misc_aux_matrix=unitsupocc'*avemisc_rtnm
matrix list norm_misc_aux_matrix
matrix miscode_rtnm=J(4,4,0)
forvalues i=1(1)4 {
forvalues j=1(1)4 {
 matrix miscode_rtnm[`i',`j']=avemisc_rtnm[`i',`j']/norm_misc_aux_matrix[1,`i']

 }
}
matrix list miscode_rtnm
version 13
putexcel set "${outputdata}/mogmiscoding_flows_v2.xlsx", sheet("superocc_misc_flows_empl",  replace) modify
sleep 5
putexcel B2=matrix(miscode_rtnm)


//========================================================
//  UNEMPLOYMENT -- USED IN THE CALIBRATION
//========================================================


tab locc1bfr_mmo [aw=pweight2] if locc1bfr_mmo!=. & unempl_ctv==1, matcell(mogmatrix)

matrix list mogmatrix
matrix unitmog=J(22,1,1)
matrix summogmatrix=unitmog'*mogmatrix

matrix mogmatrixdistr=mogmatrix/summogmatrix[1,1]
matrix list mogmatrixdistr


matrix mogmiscode=mogmatrixdistr # Gmat_mm
matrix list mogmiscode

matrix tempmog=(mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr,    mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr, mogmatrixdistr) 

matrix list tempmog
matrix mogmiscode=hadamard(tempmog, Gmat_mm)
matrix list mogmiscode

matrix nomgt_noag=I(22)
matrix noag=I(22)
matrix nomgt_noag[1,1]=0
matrix nomgt_noag[18,18]=0
matrix noag[18,18]=0

matrix mogmiscode_nomgt_noag=nomgt_noag*mogmiscode*nomgt_noag

matrix mogmiscode_noag=noag*mogmiscode*noag


xsnet_calcx mogmiscode_nomgt_noag
local miscoding22_nomgt_noag=r(tmobrate)
xsnet_calcx mogmiscode_noag
local miscoding22_noag=r(tmobrate)
xsnet_calcx mogmiscode
local miscoding22_allocc=r(tmobrate)


putexcel set "${outputdata}/mogmiscoding_flows_v2.xlsx", sheet("22occ_misc_flows_unempl" replace) modify
putexcel B2=matrix(mogmiscode)

** superocc miscoding 

matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

matrix avemisc_rtnm=rtmmconv_tpose*mogmiscode*rtmmconv_tpose'

xsnet_calcx avemisc_rtnm
local miscoding4_nomgt_noagg=r(tmobrate)

putexcel set "${outputdata}/mogmiscoding_flows_v2.xlsx", sheet("superocc_misc_flows_u" replace) modify
matrix list avemisc_rtnm
putexcel A8=("miscoding flows")
putexcel B8=matrix(avemisc_rtnm)


matrix unitsupocc=J(4,1,1)
matrix list unitsupocc

matrix norm_misc_aux_matrix=unitsupocc'*avemisc_rtnm
matrix list norm_misc_aux_matrix
matrix miscode_rtnm=J(4,4,0)
forvalues i=1(1)4 {
forvalues j=1(1)4 {
 matrix miscode_rtnm[`i',`j']=avemisc_rtnm[`i',`j']/norm_misc_aux_matrix[1,`i']

 }
}
matrix list miscode_rtnm
putexcel set "${outputdata}/mogmiscoding_flows_v2.xlsx", sheet("superocc_misc_flows_u" replace) modify
putexcel B2=matrix(miscode_rtnm)


*** invert it 
mata
a = st_matrix("miscode_rtnm")
inv_a = luinv(a)
st_matrix("invmiscode_rtnm", inv_a)
end
matrix list invmiscode_rtnm






quietly {
cap log close resultsmiscodelog
log using "${mainresultsdir}/calib_miscodingstats.txt", replace text name(resultsmiscodelog)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PROBABILITY MISCODING 22 OCC, IN UNEMPLOYMENT"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " equals `miscoding22_allocc'"


noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PROBABILITY MISCODING MOG, w/out AGRIC & MGT"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " equals `miscoding22_nomgt_noag'"


noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "PROBABILITY MISCODING ACROSS 4 SUPEROCCS, NO AGRIC & MGT"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: display  " equals `miscoding4_nomgt_noagg' "
noisily: display  ""

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "WITHIN SUPEROCC MISCODING"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
local miscoding_withinocc=0.5*(`miscoding22_allocc'+`miscoding22_nomgt_noag')-`miscoding4_nomgt_noagg'
noisily: display  " equals `miscoding_withinocc' "
noisily: display  ""
noisily: display  " where we take the average difference between the 22-occ MOG miscoding "
noisily: display  " and miscoding across the 4 super-occupations, to find the within miscoding"
noisily: display  "	2 measures of 22-occ miscoding, with mgt as in the excess mobility calibration"
noisily: display  "	and without, as in the net mobility stats, we simply take the average measure"
noisily: display  " for the calibration"


noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "MISCODING ACROSS SUPEROCCS"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: matrix list miscode_rtnm
noisily: display  "1=NRC, 2=RC, 3=NRM, 4=RM"

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "DEGARBLING MISCODING ACROSS SUPEROCCS, GAMMAINV matrix"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: matrix list invmiscode_rtnm
noisily: display  "1=NRC, 2=RC, 3=NRM, 4=RM"



log close resultsmiscodelog
	

}		



global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"





















