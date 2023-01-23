

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





*******************************************************************************
*******************************************************************************
**   ------> TRAMO TRAMO <--------                                     ********
*******************************************************************************
*******************************************************************************

/*
LIST OF TIME SERIES MADE WITH TRAMO:
C:\TSW+\SAVED\....xlsx
q3_ljf_cue_prm_q

jf_raw ljf_raw ts_jf_q ts_ljf_q q3_jf_q q3_ljf_q 
jf_yng_raw ljf_yng_raw ts_jf_yng_q ts_ljf_yng_q q3_jf_yng_q q3_ljf_yng_q 
jf_prm_raw ljf_prm_raw ts_jf_prm_q ts_ljf_prm_q q3_jf_prm_q q3_ljf_prm_q 

jf_cue_raw ljf_cue_raw q3_jf_cue_q q3_ljf_cue_q						; ts_jf_cue_q ts_ljf_cue_q <-- not included because too big gaps
jf_cue_yng_raw ljf_cue_yng_raw q3_jf_cue_yng_q q3_ljf_cue_yng_q
jf_cue_prm_raw ljf_cue_prm_raw q3_jf_cue_prm_q q3_ljf_cue_prm_q

jfn_cnune_raw ljfn_cnune_raw q3_jfn_cnune_q q3_ljfn_cnune_q 
jfn_cnune_yng_raw ljfn_cnune_yng_raw q3_jfn_cnune_yng_q q3_ljfn_cnune_yng_q 
jfn_cnune_prm_raw ljfn_cnune_prm_raw q3_jfn_cnune_prm_q q3_ljfn_cnune_prm_q 

*/
			*******************************************
			** READ IN TRAMO-ED DATA, HP FILTERED AND GENERATE STATS 
			*******************************************


/* NAMES: sep.xlsx, sep_yng, sep_prm, lsep, lsep_yng, lsep_prm in
		C:\TSW+\SAVED*/

** merge with other time series
capture program drop ts_jf_aftertramo_to_dta_exe
	program define ts_jf_aftertramo_to_dta_exe
						args name
	clear

	** uprop
	*local name "unemp_cat`indic'q"
	import excel "${outputdata}/`name'.xlsx", sheet("s0_`name'") cellrange(A2:K$rowno_exe) firstrow

	gen quarter=quarterly(DATE, "QY")
	format quarter %tq 
	ren Xorig  `name'_xorig
	ren Xint `name'_xint
	ren Xlin `name'_xlin
	keep quarter `name'*
	mvdecode  `name'*, 	mv(-99999)

	saveold "${tempdata}/`name'_aftertramo.dta", replace 


	end 
	
	display "=========================================="

	display "If program breaks with error:"
	display "      invalid row range in cellrange() option"
	display "      lower right row is after data area in worksheet"
	display "adjust global rowno_exe to fit the data area in the xlsx sheet"
	
	display "=========================================="
	
	
	
	global rowno_exe 125
	
	*ts_jf_aftertramo_to_dta_exe jf_raw 
	*ts_jf_aftertramo_to_dta_exe ljf_raw 
	*ts_jf_aftertramo_to_dta_exe ts_jf_q 
	ts_jf_aftertramo_to_dta_exe ts_ljf_q 
	*ts_jf_aftertramo_to_dta_exe q3_jf_q 
	ts_jf_aftertramo_to_dta_exe q3_ljf_q 
	
	
	/*
	global rowno_exe 124
	ts_jf_aftertramo_to_dta_exe jf_cue_raw 
	ts_jf_aftertramo_to_dta_exe ljf_cue_raw 
	ts_jf_aftertramo_to_dta_exe q3_jf_cue_q 
	ts_jf_aftertramo_to_dta_exe q3_ljf_cue_q 
	*/
	
	
	// NUN
	
	
	/*
	global rowno_exe 125
	ts_jf_aftertramo_to_dta_exe jfn_raw 
	ts_jf_aftertramo_to_dta_exe ljfn_raw 
	ts_jf_aftertramo_to_dta_exe ts_jfn_q 
	ts_jf_aftertramo_to_dta_exe ts_ljfn_q 
	ts_jf_aftertramo_to_dta_exe q3_jfn_q 
	ts_jf_aftertramo_to_dta_exe q3_ljfn_q 
	*/
	
	global rowno_exe 119
	ts_jf_aftertramo_to_dta_exe q3_ljfn_cnune_q 
	
	global rowno_exe 119
	ts_jf_aftertramo_to_dta_exe q3_ljfn_cnune_instr_q 
	
***********************************************
** DATA	MERGE ALL VARIABLES DERIVED ABOVE
***********************************************

use "${outputdata}/ts_jf_paper_before_tramo.dta", clear

//PPPPPPPPPPPPPPPP

capture program drop ts_jf_merge_exe
program define ts_jf_merge_exe
				args name

		capture drop _jfmerge_`name'1
		capture noisily merge 1:1 quarter using  "${tempdata}/`name'_aftertramo.dta", gen(_jfmerge_`name'1) 

		/*
		capture drop _jfmerge_`name'2
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_l`name'_raw_aftertramo_sept2019.dta", gen(_jfmerge_`name'2) 

		capture drop _jfmerge_`name'3
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_ts_`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'3) 

		capture drop _jfmerge_`name'4
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_ts_l`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'4) 

		capture drop _jfmerge_`name'5
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_q3_`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'5) 

		capture drop _jfmerge_`name'6
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_q3_l`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'6) 
		*/
end 

//RRRRRRRRRRRRRRRRR
//RRRRRRRRRRRRRRRRR

ts_jf_merge_exe ts_ljf_q 
ts_jf_merge_exe q3_ljf_q 
ts_jf_merge_exe q3_ljfn_cnune_q 
ts_jf_merge_exe q3_ljfn_cnune_instr_q 

drop if quarter>=tq(2014q1)
//PPPPPPPPPPPPPPPPP

/*
capture program drop ts_jf_fmerge_exe
program define ts_jf_fmerge_exe
				args name

				
		capture drop _jfmerge_`name'0
		capture noisily merge 1:1 quarter using  "C:\data\ts_`name'_before_tramo.dta", gen(_jfmerge_`name'1) 
		
		capture drop _jfmerge_`name'1
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_`name'_raw_aftertramo_sept2019.dta", gen(_jfmerge_`name'1) 

		capture drop _jfmerge_`name'2
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_l`name'_raw_aftertramo_sept2019.dta", gen(_jfmerge_`name'2) 

		capture drop _jfmerge_`name'3
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_ts_`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'3) 

		capture drop _jfmerge_`name'4
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_ts_l`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'4) 

		capture drop _jfmerge_`name'5
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_q3_`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'5) 

		capture drop _jfmerge_`name'6
		capture noisily merge 1:1 quarter using  "${datadir1}ts_series/qtr_q3_l`name'_q_aftertramo_sept2019.dta", gen(_jfmerge_`name'6) 
		
end 

		ts_jf_fmerge_exe jf_yng
		ts_jf_fmerge_exe jf_prm
		ts_jf_fmerge_exe jf_cue
		ts_jf_fmerge_exe jf_cue_yng
		ts_jf_fmerge_exe jf_cue_prm
		ts_jf_fmerge_exe jfn
		ts_jf_fmerge_exe jfn_all
		ts_jf_fmerge_exe jfn_yng
		ts_jf_fmerge_exe jfn_prm
		ts_jf_fmerge_exe jfn_cnune
		ts_jf_fmerge_exe jfn_cnune_yng
		ts_jf_fmerge_exe jfn_cnune_prm
*/
	
		

/*
		tokenize ts_jf_instr_q ts_ljf_instr_q q3_jf_instr_q q3_ljf_instr_q ts_jf_yng_instr_q ts_ljf_yng_instr_q ///
		q3_jf_yng_instr_q q3_ljf_yng_instr_q ts_jf_prm_instr_q ts_ljf_prm_instr_q q3_jf_prm_instr_q ///
		q3_ljf_prm_instr_q ts_jf_cue_instr_q ts_ljf_cue_instr_q q3_jf_cue_instr_q q3_ljf_cue_instr_q /// 
		ts_jf_cue_yng_instr_q ts_ljf_cue_yng_instr_q q3_jf_cue_yng_instr_q q3_ljf_cue_yng_instr_q ///
		ts_jf_cue_prm_instr_q ts_ljf_cue_prm_instr_q q3_jf_cue_prm_instr_q q3_ljf_cue_prm_instr_q ///
		ts_jfn_instr_q ts_ljfn_instr_q q3_jfn_instr_q q3_ljfn_instr_q ts_jfn_yng_instr_q ts_ljfn_yng_instr_q ///
		q3_jfn_yng_instr_q q3_ljfn_yng_instr_q ts_jfn_prm_instr_q ts_ljfn_prm_instr_q q3_jfn_prm_instr_q /// 
		q3_ljfn_prm_instr_q ts_jfn_cnune_instr_q ts_ljfn_cnune_instr_q q3_jfn_cnune_instr_q q3_ljfn_cnune_instr_q /// 
		ts_jfn_cnune_yng_instr_q ts_ljfn_cnune_yng_instr_q q3_jfn_cnune_yng_instr_q q3_ljfn_cnune_yng_instr_q /// 
		ts_jfn_cnune_prm_instr_q ts_ljfn_cnune_prm_instr_q q3_jfn_cnune_prm_instr_q q3_ljfn_cnune_prm_instr_q
*qtr_ts_jf_q_aftertramo_sept2019
*qtr_ts_ljf_q_aftertramo_sept2019
*qtr_ljf_raw_aftertramo_sept2019
*qtr_jf_raw_aftertramo_sept2019
*qtr_q3_ljf_q_aftertramo_sept2019
*qtr_q3_jf_q_aftertramo_sept2019
*/

	*capture n drop hp_ts_jf_instr_q
	*cap n tsfilter hp hp_ts_jf_instr_q=  ts_jf_instr_q if quarter>=tq(1984q1) & quarter<=tq(2013q4), smooth(1600)
// HP-FILTERING 	
local jf_hp_list "ts_ljf_q q3_ljf_q q3_ljfn_cnune_q q3_ljfn_cnune_instr_q"	

foreach lname of local jf_hp_list {
display "hp filtering variable: `lname'"
capture drop hp_`lname'
cap n tsfilter hp hp_`lname'=  `lname'_xint if quarter>=tq(1984q1) & quarter<=tq(2013q4), smooth(1600)

* drop value if it was a TRAMO-ED value
capture replace hp_`lname'=. if `lname'==.
}

tsset quarter


** TEST CYCLICAL ST DEVs
su hp_ts_ljf_q if ts_ljf_q!=.
su hp_q3_ljf_q if q3_ljf_q!=. & l2.q3_ljf_q!=. ///
			& l.q3_ljf_q!=. & f.q3_ljf_q!=. & f2.q3_ljf_q!=.

su hp_q3_ljfn_cnune_q if q3_ljfn_cnune_q!=. & l2.q3_ljfn_cnune_q!=. ///
			& l.q3_ljfn_cnune_q!=. & f.q3_ljfn_cnune_q!=. & f2.q3_ljfn_cnune_q!=.

su hp_q3_ljfn_cnune_instr_q if q3_ljfn_cnune_instr_q!=. & l2.q3_ljfn_cnune_instr_q!=. ///
			& l.q3_ljfn_cnune_instr_q!=. & f.q3_ljfn_cnune_instr_q!=. & f2.q3_ljfn_cnune_instr_q!=.

compress
save "${outputdata}/jf_hp_ts2020.dta", replace 



********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"