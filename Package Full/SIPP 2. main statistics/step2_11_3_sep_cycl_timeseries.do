

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






//============================================================================
*******************************************
** READ IN TRAMO-ED DATA, HP FILTERED AND GENERATE STATS 
*******************************************
//============================================================================




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




/* NAMES: sep.xlsx, sep_yng, sep_prm, lsep, lsep_yng, lsep_prm in
		C:\TSW+\SAVED*/

** merge with other time series
capture program drop ts_sep_aftertramo_to_dta_exe
	program define ts_sep_aftertramo_to_dta_exe
						args name
	clear

	*import excel "C:/TSW+/SAVED/`name'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow
	import excel "${outputdata}/`name'.xlsx", sheet("s0_`name'") cellrange(A2:K124) firstrow

	gen quarter=quarterly(DATE, "QY")
	format quarter %tq 
	ren Xorig  `name'_xorig
	ren Xint `name'_xint
	ren Xlin `name'_xlin
	keep quarter `name'*
	mvdecode  `name'*, 	mv(-99999)

	saveold "${outputdata}/`name'_aftertramo.dta", replace 


	end 

	ts_sep_aftertramo_to_dta_exe ts_lsep_q 
	ts_sep_aftertramo_to_dta_exe q3_lsep_q 
	
	
** save as a total timeseries dataseries incl. sep 
use "${outputdata}/ts_sep_for_tramo.dta", clear

capture program drop ts_sep_merge_exe
	program define ts_sep_merge_exe
						args name
	
	capture drop _merge_`name'
	merge 1:1 quarter using "${outputdata}/`name'_aftertramo.dta", gen(_merge_`name')


end // program ts_sep_merge_exe



	ts_sep_merge_exe ts_lsep_q 
	ts_sep_merge_exe q3_lsep_q 
	
	
local ts_sep_list "ts_lsep_q q3_lsep_q"
foreach lname of local ts_sep_list {
capture drop hp_`lname'
cap n tsfilter hp hp_`lname'=  `lname'_xint  if quarter>=tq(1984q1) & quarter<=tq(2013q4), smooth(1600)

* drop value if it was a TRAMO-ED value
replace hp_`lname'=. if `lname'==.
}



capture drop sepu_bls
capture drop lsep_bls
merge m:1 quarter using "${workingdir}/Aggregate Data/jf_sepu_bls.dta", keepusing(sepu_bls)
table quarter, c(mean sepu_bls)

capture drop lsep_bls
gen lsep_bls=log(sepu_bls)
capture drop hp_lsep_bls
capture drop hp_sep_bls
cap n tsfilter hp hp_lsep_bls=  lsep_bls  if quarter>=tq(1990q1) & quarter<=tq(2013q4), smooth(1600)
cap n tsfilter hp hp_sep_bls=  sepu_bls  if quarter>=tq(1990q1) & quarter<=tq(2013q4), smooth(1600)

set varabbrev off
cap drop hp_lsep
cap drop hp_q3_lsep
gen hp_lsep=hp_ts_lsep_q
gen hp_q3_lsep=hp_q3_lsep_q


	
capture drop sep_orig_5q
capture drop sep_orig
gen sep_orig_5q=0
gen sep_orig=0

replace sep_orig_5q=1 if ts_lsep_q!=. & ts_lsep_q[_n-2]!=. & ts_lsep_q[_n-1]!=. & ts_lsep_q[_n+1]!=.  & ts_lsep_q[_n+2]!=.
replace sep_orig=1 if ts_lsep_q[_n-1]!=. 


keep quarter hp_q3_lsep hp_ts_lsep_q sep_orig_5q hp_lsep hp_lsep_bls hp_q3_lsep_q sep_orig


export delimited using "${outputdata}/sep_hp_ts2020.csv", replace
save "${outputdata}/sep_hp_ts2020.dta", replace



/*


*/



********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"

