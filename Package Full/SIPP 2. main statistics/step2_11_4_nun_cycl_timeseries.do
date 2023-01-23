


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
This short do file takes icnun_durwvc (incomplete NUN spells in periods with 
Unemployemnt duration distribution stability, no sample-timetogo restriction, but 
wavecond applying (at least 4 waves and 14 interview months in sample, to deal with
left-censoring (spell started before).


global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1 | (outlf_ctv==1 & (incomplete_nunspell==1 | complete_nunspell==1))) & durdistr_stability==1 ${wavecond} "
global wavecond " & wave>4 & interview_no2>14 & sample_timetogo>1"

*/




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




clear
use "${tempdata}/timeseries_u.dta", clear


*preserve
*ONLY KEEP RELEVANT VARIABLES

keep ur_all_icnun_durwvnosttg urate_bls quarter
*save "C:\data\ts_urate_jf_sep\timeseries_u_april2020.dta", replace


*** bring in vacancy data
merge 1:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", ///
			keepusing( lvacancy lunrate)

drop if _merge==2
drop _merge

*save "C:\data\ts_urate_jf_sep\timeseries_u_april2020.dta", replace
			
export delimited using "${tempdata}/nun_ts2.csv", replace

* generate logged series
cap drop lurate_bls
cap n gen lurate_bls=log(urate_bls)
cap drop lur_all_icnun_durwvnosttg 
cap n gen lur_all_icnun_durwvnosttg =log(ur_all_icnun_durwvnosttg )


notes: " ur_all_icnun_durwvnosttg based on (unempl_ctv==1 | empl_ctv==1 | (outlf_ctv==1 & (incomplete_nunspell==1 | complete_nunspell==1))) & durdistr_stability==1 ${wavecond} & wave>4 & interview_no2>14 & sample_timetogo>1"


* fill in small holes in bls series (inconsequential)
replace urate_bls=0.333333*(2*urate_bls[_n-1]+urate_bls[_n+2]) if urate_bls==. & quarter[_n-1]==quarter[_n+2]-3 & urate_bls[_n-1]!=. & urate_bls[_n+1]==. & urate_bls[_n+2]!=.
replace urate_bls=0.5*(urate_bls[_n-1]+urate_bls[_n+1]) if urate_bls==. & quarter[_n-1]==quarter[_n+1]-2 & urate_bls[_n-1]!=. & urate_bls[_n+1]!=.


// PREDICT SERIES TO FILL UP GAPS 

* predict icnun series by running regression on bls urate and quartic in time
cap n drop pred_ur_all_icnun_durwvcnosttg
reg ur_all_icnun_durwvnosttg urate_bls c.quarter##c.quarter##c.quarter##c.quarter
predict pred_ur_all_icnun_durwvcnosttg

* predict log icnun series: THIS HAS A LOWER R-SQ, so we go with the level predicted series 
cap n drop lpred_lur_all_icnun_durwvcnosttg
reg lur_all_icnun_durwvnosttg lurate_bls c.quarter##c.quarter##c.quarter##c.quarter
predict lpred_lur_all_icnun_durwvcnosttg




// CREATE LOG PREDICTED SERIES, CREATE _EXT  SERIES (ONLY GAPS ARE FILLED WITH PREDICTED OBS)
//  HP FILTER THESE 

* predicted series
	* pred_lur_all_icnun_durwvcnosttg: fill in gaps and instrument original series by u_bls
capture pred_lur_all_icnun_durwvcnosttg
gen pred_lur_all_icnun_durwvcnosttg=log( pred_ur_all_icnun_durwvcnosttg)
	* only fill in gaps
gen ur_all_icnun_durwvnosttg_ext=ur_all_icnun_durwvnosttg
replace ur_all_icnun_durwvnosttg_ext= pred_ur_all_icnun_durwvcnosttg if ur_all_icnun_durwvnosttg==.

* log the _EXT series 
gen lur_all_icnun_durwvnosttg_ext=log( ur_all_icnun_durwvnosttg_ext)


notes: _ext original series plus gaps filled by predicted
notes: _pred predicted series i.e. gaps filled by predicted, orig data instrumented


* hp filter these series
tsfilter hp hp_pred_lur_all_icnun_durwvc = pred_lur_all_icnun_durwvcnosttg, smooth(1600)
tsfilter hp hp_lur_all_icnun_durwvnosttg_ext = lur_all_icnun_durwvnosttg_ext, smooth(1600)

* hp filtered also the u bls series
cap n drop hp_lubls
cap n drop lubls
gen lubls=log(urate_bls)
tsfilter hp hp_lubls = lubls, smooth(1600)

* VOLATILITY STATS ARE CALCULATED ONLY FOR QUARTERS IN WHICH WE HAD AN ORIGINAL OBSERVATION 
su hp_pred_lur_all_icnun_durwvc hp_lur_all_icnun_durwvnosttg_ext if ur_all_icnun_durwvnosttg!=.

// VACANCY DATA, TIGHTNESS_NUN 

** bring in vacancy data 
capture drop ltight_nun_ext
gen ltight_nun_ext=lvacancy-lur_all_icnun_durwvnosttg_ext 

capture drop ltight_nun_pred
gen ltight_nun_pred=lvacancy-pred_lur_all_icnun_durwvcnosttg 

cap drop hp_pred_ltight_nun 
cap drop hp_ltight_nun_ext

tsfilter hp hp_pred_ltight_nun = ltight_nun_pred, smooth(1600)
tsfilter hp hp_ltight_nun_ext = ltight_nun_ext, smooth(1600)
tsfilter hp hp_lvacancy = lvacancy, smooth(1600)



// INDICATORS OF ORIGINAL INFORMATION!!!
capture drop nun_orig
gen nun_orig=0
replace nun_orig=1 if ur_all_icnun_durwvnosttg!=.

capture drop nun_orig_5q
gen nun_orig_5q=0
replace nun_orig_5q=1 if ur_all_icnun_durwvnosttg[_n-2]!=. & ur_all_icnun_durwvnosttg[_n-1]!=. ///
				& ur_all_icnun_durwvnosttg!=. & ur_all_icnun_durwvnosttg[_n+1]!=. & ur_all_icnun_durwvnosttg[_n+2]!=.



// give a simpler name to nun 
capture drop hp_pred_lnun 
capture drop hp_lnun_ext
capture drop pred_lnun 
capture drop lnun_ext
gen hp_pred_lnun = hp_pred_lur_all_icnun_durwvc
gen pred_lnun = pred_lur_all_icnun_durwvcnosttg
gen lnun_ext = lur_all_icnun_durwvnosttg_ext
gen hp_lnun_ext = hp_lur_all_icnun_durwvnosttg_ext 
				
// HP FILTERED TIME SERIES TO SAVE + INDIC
/* 
	quarter hp_lubls hp_lvacancy hp_pred_lnun hp_lnun_ext pred_lnun lnun_ext hp_pred_ltight_nun hp_ltight_nun_ext hp_ltightness nun_orig nun_orig_5q				
*/				
				
*********************
** 5Q measures
*********************

* create moving average of the logged series
/*
lubls
lvacancy
ltightness_bls 
ltight_nun_ext
ltight_nun_pred
pred_lnun 
lnun_ext 
*/

cap drop lubls_sm5
cap drop lvacancy_sm5
cap drop ltightness_bls_sm5 
cap drop ltight_nun_ext_sm5
cap drop ltight_nun_pred_sm5
cap drop pred_lnun_sm5
cap drop lnun_ext_sm5

tssmooth ma lubls_sm5 = lubls, window(2 1 2)
tssmooth ma lvacancy_sm5 = lvacancy, window(2 1 2)
*tssmooth ma ltightness_bls_sm5 = ltightness_bls, window(2 1 2)
tssmooth ma ltight_nun_ext_sm5 = ltight_nun_ext, window(2 1 2)
tssmooth ma ltight_nun_pred_sm5 = ltight_nun_pred, window(2 1 2)
tssmooth ma pred_lnun_sm5 = pred_lnun, window(2 1 2)
tssmooth ma lnun_ext_sm5 = lnun_ext, window(2 1 2)

** HP FILTER 


cap drop hp_lubls_sm5
cap drop hp_lvacancy_sm5
cap drop hp_ltightness_bls_sm5 
cap drop hp_ltight_nun_ext_sm5
cap drop hp_ltight_nun_pred_sm5
cap drop hp_pred_lnun_sm5
cap drop hp_lnun_ext_sm5

tsfilter hp hp_lubls_sm5 = lubls_sm5, smooth(1600)
tsfilter hp hp_lvacancy_sm5 = lvacancy_sm5, smooth(1600)
*tsfilter hp hp_ltightness_bls_sm5 = ltightness_bls_sm5, smooth(1600)
tsfilter hp hp_ltight_nun_ext_sm5 = ltight_nun_ext_sm5, smooth(1600)
tsfilter hp hp_ltight_nun_pred_sm5 = ltight_nun_pred_sm5, smooth(1600)
tsfilter hp hp_pred_lnun_sm5 = pred_lnun_sm5, smooth(1600)
tsfilter hp hp_lnun_ext_sm5 = lnun_ext_sm5, smooth(1600)



su hp_lnun_ext_sm5 hp_pred_lnun_sm5 
su hp_lnun_ext_sm5 hp_pred_lnun_sm5 if nun_orig==1
su hp_lnun_ext_sm5 hp_pred_lnun_sm5 if nun_orig_5q==1

sort quarter
reg hp_lnun_ext_sm5 l.hp_lnun_ext_sm5 
reg hp_pred_lnun_sm5 l.hp_pred_lnun_sm5 







pwcorr hp_lnun_ext_sm5  hp_ltight_nun_ext_sm5  hp_lvacancy_sm5 if nun_orig ==1 




export delimited using "${outputdata}/nun_ts2020.csv", replace
save "${outputdata}/nun_ts2020.dta", replace 

keep quarter hp_lubls_sm5 hp_lvacancy_sm5 hp_ltight_nun_ext_sm5 hp_ltight_nun_pred_sm5 hp_pred_lnun_sm5 hp_lnun_ext_sm5 nun_orig* ///
hp_lubls hp_lvacancy hp_pred_lnun hp_lnun_ext pred_lnun lnun_ext hp_pred_ltight_nun hp_ltight_nun_ext 
//hp_ltightness_bls_sm5 hp_ltightness 
				
export delimited using "${outputdata}/nun_hp_ts2020.csv", replace
save "${outputdata}/nun_hp_ts2020.dta", replace 
*restore 



********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"