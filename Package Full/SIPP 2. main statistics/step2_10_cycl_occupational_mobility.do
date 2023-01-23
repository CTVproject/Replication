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





cd "${workingdir}"
global workingdir `c(pwd)'
do "${workingdir}/global_paths.do"
	

	version 13
	
	set more off
	set varabbrev off
	set trace off
	
	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"



	




	****************************************************************************
	**** POST TRAMO LOAD-IN
	****************************************************************************
	
	// STEP 1) CONVERT XLS FILES TO DTA SO THEY CAN BE MERGED
	// STEP 2) MERGE

	
	capture program drop ts_aftertramo_xls_to_dta_exe
	program define ts_aftertramo_xls_to_dta_exe
						args name
	clear
						
	import excel "${outputdata}/`name'.xlsx", sheet("s0_`name'") cellrange(A2:K121) firstrow

	gen quarter=quarterly(DATE, "QY")
	format quarter %tq 
	ren Xorig  `name'_xorig
	ren Xint `name'_xint
	ren Xlin `name'_xlin
	keep quarter `name'*
	mvdecode  `name'*, 	mv(-99999)

	save "${tempdata}/`name'_aftertramo.dta", replace 


	end 

	 cap n ts_aftertramo_xls_to_dta_exe  q1_mob_mm
	 cap n ts_aftertramo_xls_to_dta_exe  q1_mob_mm_nun 
	 cap n ts_aftertramo_xls_to_dta_exe  q1_mob_mmmy 
	 cap n ts_aftertramo_xls_to_dta_exe  q1_mob_mmmp 
	 cap n ts_aftertramo_xls_to_dta_exe  q3_mob_mm 
	 cap n ts_aftertramo_xls_to_dta_exe  q3_mob_mm_nun 
	 cap n ts_aftertramo_xls_to_dta_exe  q3_mob_mmmy
	 cap n ts_aftertramo_xls_to_dta_exe  q3_mob_mmmp
	 cap n ts_aftertramo_xls_to_dta_exe  qn1_mob_12_mm 
	 cap n ts_aftertramo_xls_to_dta_exe  qn1u_mob_12_mm 
	 cap n ts_aftertramo_xls_to_dta_exe  qn3_mob_12_mm
	 cap n ts_aftertramo_xls_to_dta_exe  qn3u_mob_12_mm  
	 cap n ts_aftertramo_xls_to_dta_exe  qn1_mob_14_mm
	 cap n ts_aftertramo_xls_to_dta_exe  qn1_mob_12_mm_u
	 cap n ts_aftertramo_xls_to_dta_exe  qn1u_mob_12_mm_u  
	 cap n ts_aftertramo_xls_to_dta_exe  qn3_mob_12_mm_u 
	 cap n ts_aftertramo_xls_to_dta_exe  qn3u_mob_12_mm_u 
	 cap n ts_aftertramo_xls_to_dta_exe  q1_mob_mm_nun_u
	 cap n ts_aftertramo_xls_to_dta_exe  q3_mob_mm_nun_u
	 cap n ts_aftertramo_xls_to_dta_exe  lunrate_bls
	 
	 // SEATS SERIES MERGING 
	 
	 
	capture program drop ts_aftertseats_xls_to_dta_exe
	program define ts_aftertseats_xls_to_dta_exe
						args name
	clear
						
	cap n import excel "${outputdata}/`name'.xlsx", cellrange(A2:R121) firstrow
	cap n import excel "${outputdata}/`name'.xlsx", cellrange(A2:P113) firstrow
	
	*cap n import excel "${outputdata}/`name'.xlsx", sheet("s0_`"'`name'`"'") cellrange(A2:R121) firstrow
	*cap n import excel "${outputdata}/`name'.xlsx", sheet("s0_`name'") cellrange(A2:R121) firstrow
	gen quarter=quarterly(DATE, "QY")
	format quarter %tq 
	cap n ren Series `name'_Series
	cap n ren TrendCycle `name'_tcycl
	cap n ren SAserrec  `name'_SAser
	cap n ren CalendarAdjusted `name'_caladj
	cap n ren SAserAllSeas  `name'_SAseralt
	cap n ren Seasonal `name'_Seasonal
	cap n ren Calendar  `name'_Calendar
	cap n ren Irregular  `name'_Irregular
	cap n ren Transitory `name'_transy
	cap n ren Preadjust `name'_Preadjust
	cap n ren Cycle `name'_cycl
	cap n ren LONGTERMTREND  `name'_trend
	cap n ren StochTrendCycle  `name'_stochtc
	cap n ren StochSASeries `name'_stochsa

	keep quarter `name'*
	mvdecode  `name'*, 	mv(-99999)
	tsset quarter

	 
	
	save "${tempdata}/`name'.dta", replace 


	end 
	 
	  cap n ts_aftertseats_xls_to_dta_exe  q1_mob_mm_seats
	 cap n ts_aftertseats_xls_to_dta_exe  q1_mob_mm_nun_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  q1_mob_mmmy_seats
	 cap n ts_aftertseats_xls_to_dta_exe  q1_mob_mmmp_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  q3_mob_mm_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  q3_mob_mm_nun_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  q3_mob_mmmy_seats
	 cap n ts_aftertseats_xls_to_dta_exe  q3_mob_mmmp_seats
	 cap n ts_aftertseats_xls_to_dta_exe  qn1_mob_12_mm_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  qn1u_mob_12_mm_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  qn3_mob_12_mm_seats
	 cap n ts_aftertseats_xls_to_dta_exe  qn3u_mob_12_mm_seats  
	 cap n ts_aftertseats_xls_to_dta_exe  qn1_mob_12_mm_u_seats  
	 cap n ts_aftertseats_xls_to_dta_exe  qn1u_mob_12_mm_u_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  qn3_mob_12_mm_u_seats  
	 cap n ts_aftertseats_xls_to_dta_exe  qn3u_mob_12_mm_u_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  q1_mob_mm_nun_u_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  q3_mob_mm_nun_u_seats 
	 cap n ts_aftertseats_xls_to_dta_exe  lunrate_bls_seats 
	 
	 
	 
	 
	
	 // MERGING 

	use "${outputdata}/occmob_ts_for_tramo.dta", clear
	
	
	
	capture program drop merge_aftertramoseats_exe
	program define merge_aftertramoseats_exe
				args name
	
	capture drop _merge
	cap n merge 1:1 quarter using "${tempdata}/`name'_aftertramo.dta"
	capture drop _merge
	cap n merge 1:1 quarter using "${tempdata}/`name'_seats.dta"
	
	end 
	
	 cap n merge_aftertramoseats_exe  q1_mob_mm
	 cap n merge_aftertramoseats_exe  q1_mob_mm_nun 
	 cap n merge_aftertramoseats_exe  q1_mob_mmmy 
	 cap n merge_aftertramoseats_exe  q1_mob_mmmp 
	 cap n merge_aftertramoseats_exe  q3_mob_mm 
	 cap n merge_aftertramoseats_exe  q3_mob_mm_nun 
	 cap n merge_aftertramoseats_exe  q3_mob_mmmy
	 cap n merge_aftertramoseats_exe  q3_mob_mmmp
	 cap n merge_aftertramoseats_exe  qn1_mob_12_mm 
	 cap n merge_aftertramoseats_exe  qn1u_mob_12_mm 
	 cap n merge_aftertramoseats_exe  qn3_mob_12_mm
	 cap n merge_aftertramoseats_exe  qn3u_mob_12_mm  
	 cap n merge_aftertramoseats_exe  qn1_mob_14_mm
	 cap n merge_aftertramoseats_exe  qn3_mob_14_mm 
	 cap n merge_aftertramoseats_exe  qn1_mob_12_mm_u  
	 cap n merge_aftertramoseats_exe  qn1u_mob_12_mm_u 
	 cap n merge_aftertramoseats_exe  qn3_mob_12_mm_u  
	 cap n merge_aftertramoseats_exe  qn3u_mob_12_mm_u 
	 cap n merge_aftertramoseats_exe  q1_mob_mm_nun_u
	 cap n merge_aftertramoseats_exe  q3_mob_mm_nun_u
	 cap n merge_aftertramoseats_exe  lunrate_bls 
	 
	 save "${outputdata}/occmob_ts_after_tramo.dta", replace 

	 
	 
	 
	 
	*****************************************************************
	** DETRENDING AND SMOOTHING
	**************************************************************** 
	
	global smoothfactor=1600
	
	tokenize "q1_mob_mm q1_mob_mm_nun q1_mob_mmmy q1_mob_mmmp q3_mob_mm q3_mob_mm_nun q3_mob_mmmy q3_mob_mmmp qn1_mob_12_mm qn1u_mob_12_mm qn3_mob_12_mm qn3u_mob_12_mm  qn1_mob_14_mm qn1_mob_12_mm_u  qn3_mob_12_mm_u  qn3_mob_14_mm qn3u_mob_12_mm_u qn1u_mob_12_mm_u q1_mob_mm_nun_u q3_mob_mm_nun_u"
	 
	*** using XLIN series after TRAMO/SEATS
	global laddcondition "if quarter<=tq(2013q4)"
	
	forvalues i=1(1)22 {
	display ""
	display "hp filtering ``i''_xlin"
	capture  drop l``i''_xlin
	cap gen l``i''_xlin=log(``i''_xlin)
	capture drop hp_``i''
	cap n tsfilter hp hp_``i''=l``i''_xlin ${laddcondition}, smooth($smoothfactor)
	capture drop bp_``i''
	cap n tsfilter bk bp_``i''=l``i''_xlin ${laddcondition}, min(6) max(32)
	}
	
	
	
	** using TrendCycle Component of SEATS, after TRAMO+SEATS
	
	
	tokenize "q1_mob_mm q1_mob_mm_nun q1_mob_mmmy q1_mob_mmmp qn1_mob_12_mm qn1u_mob_12_mm lunrate_bls q1_mob_mm_u q1_mob_mm_nun_u q1_mob_mmmy_u q1_mob_mmmp_u qn1_mob_12_mm_u qn1u_mob_12_mm_u"
	
	global laddcondition "if quarter<=tq(2013q4)"
	
	forvalues i=1(1)13 {
	display "hp filtering ``i''_tcycl"
	capture  drop l``i''_seats_tcycl
	cap  gen l``i''_tcycl=log(``i''_seats_tcycl)
	capture  drop hp_``i''_tcycl
	cap n tsfilter hp hp_``i''_tcycl=l``i''_tcycl  ${laddcondition}, smooth($smoothfactor)
	capture  drop l``i''_seats_SAser
	cap  gen l``i''_sa=log(``i''_seats_SAser)
	capture  drop hp_``i''_sa
	cap n tsfilter hp hp_``i''_sa=l``i''_sa  ${laddcondition}, smooth($smoothfactor)
	
	}
	
	
	
	
	
	** smoothing 
	
	global lowess_bw=0.1
	global smoothfactor=1600

	capture program drop ts_lowess_exe
	program define ts_lowess_exe
				args name

		*capture drop qn3_mob_12_`indic'_xlin_sm
		capture drop `name'_sm
		lowess `name' quarter  if quarter>tq(1985q3), bwidth(${lowess_bw}) gen(`name'_sm) nograph
		capture drop l`name'_sm
		cap n gen l`name'_sm=log(`name'_sm)
		capture drop hp_`name'_sm
		cap n tsfilter hp hp_`name'_sm=l`name'_sm, smooth($smoothfactor)
									
	end

	
	ts_lowess_exe qn3_mob_12_mm_xlin
	ts_lowess_exe qn3u_mob_12_mm_xlin
	ts_lowess_exe qn1_mob_12_mm_xlin
	ts_lowess_exe qn1u_mob_12_mm_xlin
	ts_lowess_exe qn3_mob_12_mm_u_xlin
	ts_lowess_exe qn3u_mob_12_mm_u_xlin
	ts_lowess_exe qn1_mob_12_mm_u_xlin
	ts_lowess_exe qn1u_mob_12_mm_u_xlin
	
	
	
	 save "${outputdata}/occmob_ts_after_tramo.dta", replace 

	 
	 
	 
	 
	 
	 ************************************************************
	 ***  LOAD IN AGGREGATE SERIES, SMOOTH ETC. 
	 ************************************************************ loutpw
	 
	 

	**** DURDIST STABILITY

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

	capture drop durdist_stab5q
	sort quarter
	gen durdist_stab5q=0
	replace durdist_stab5q=1 if durdistr_stability==1 & durdistr_stability[_n-2]==1 & durdistr_stability[_n-1]==1 & durdistr_stability[_n+1]==1  & durdistr_stability[_n+2]==1 

	capture drop durdist_stab3q
	sort quarter
	gen durdist_stab3q=0
	replace durdist_stab3q=1 if durdistr_stability==1 & durdistr_stability[_n-1]==1 & durdistr_stability[_n+1]==1  
	
	



	 
	capture drop _merge
	merge 1:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta"
	 
	 
	** 5Q smoothed series for output and unemployment 
	local q3smlist "lunrate lunrate_bls lunrate_bls_0 loutpw loutpw2"
	
	foreach smvar of local q3smlist {
		sort quarter
		capture drop q3_`smvar'
		gen q3_`smvar'=(`smvar'[_n-2]+`smvar'[_n-1]+`smvar'+ `smvar'[_n+1]+ `smvar'[_n+2])/5
	}	
		
	
	
	
	
	** Bandpass filter 1Q aggregate series
	
	tokenize "lunrate lunrate_bls lunrate_bls_0 loutpw"
	global laddcondition "if quarter<=tq(2013q4)"
	
	forvalues i=1(1)4 {
	display "bp filtering u and y"
	capture drop bp_``i''
	cap n tsfilter bk bp_``i''=``i'' ${laddcondition}, min(6) max(32)
	}
	
	 
	** filter q3 aggregate series (hp+bp)
	global laddcondition "if quarter<=tq(2013q4)"
	tokenize "q3_lunrate q3_lunrate_bls q3_lunrate_bls_0 q3_loutpw q3_loutpw2"
	 
	forvalues i=1(1)5 {
	display "Q3 series hp filtering u and y"
	capture drop hp_``i''
	cap n tsfilter hp hp_``i''=``i'' ${laddcondition}, smooth(${smoothfactor})
	display "Q3 series bp filtering u and y"
	capture drop bp_``i''
	cap n tsfilter bk bp_``i''=``i'' ${laddcondition}, min(6) max(32)
	}
	
	
	
	capture drop season
	gen season=quarter-floor(quarter/4)*4+1
	format quarter %tq		

	
	** local polynomial smoothing of the one-quarter  
	 
	 
	 
	 **********************************************************
	 ** RESULTS, WRITTEN TO FILE
	 **********************************************************
	
	
	
	
	
cap log close regresultslog3
quietly {
log using "${mainresultsdir}/table1_regs_i_ii.txt", replace text name(regresultslog3)

noisily: display "-------------------------------------------------------------"
noisily: display " TABLE 1, REG (i) and (ii) -- MAIN RESULTS"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""

noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (i), panel A"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg hp_qn3_mob_12_mm hp_q3_lunrate_bls i.season [w=qn1_cum_mob_desc12_mm_obs] if qn3_mob_12_mm!=., vce(robust)
n
noisily: display  ""
noisily: display  ""			
	
		

**************
** Uncorrected
**************




noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (ii), panel A"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg hp_qn3u_mob_12_mm hp_q3_lunrate_bls i.season  [w=qn1_cum_mob_desc12_mm_obs] if qn3_mob_12_mm!=., vce(robust)
log off regresultslog3
}		
		
****************
** ROBUSTNESS
****************

quietly {
log on regresultslog3
noisily: display  ""
noisily: display  ""
noisily: display "-------------------------------------------------------------"
noisily: display " ROBUSTNESS: BAND-PASS FILTERED 1Q (not separately smoothed)"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (i) Gamma Corrected, Band-Pass Filtered"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg bp_qn1_mob_12_mm  bp_lunrate_bls  [w=qn1_cum_mob_desc12_mm_obs] ///
					if qn1_mob_12_mm!=. , vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
		

noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (ii) Gamma Uncorrected, Band-Pass Filtered"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg bp_qn1u_mob_12_mm  bp_lunrate_bls [w=qn1_cum_mob_desc12_mm_obs] ///
					if qn1_mob_12_mm!=. , vce(robust)
log off regresultslog3 
}		
		

quietly {
log on regresultslog3
noisily: display  ""
noisily: display  ""
noisily: display "-------------------------------------------------------------"
noisily: display " ROBUSTNESS: TRAMO/SEATS TrendCycle (not separately smoothed)"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (i) Gamma Corrected, Tramo/Seats trendcycle component"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg hp_qn1_mob_12_mm_tcycl hp_lunrate_bls_tcycl [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=. , vce(robust)
noisily: display  ""
noisily: display  ""			
		

noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (ii) Gamma Uncorrected, Tramo/Seats trendcycle component"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg hp_qn1u_mob_12_mm_tcycl hp_lunrate_bls_tcycl [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=. , vce(robust)
					
log off regresultslog3 
}		
		
	

quietly {
log on regresultslog3
noisily: display  ""
noisily: display  ""
noisily: display "-------------------------------------------------------------"
noisily: display " ROBUSTNESS: TRAMO/SEATS lowess smoothed Q1 series"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (i) Gamma Corrected, Tramo/Seats trendcycle component"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg hp_qn1_mob_12_mm_xlin_sm hp_lunrate_bls i.season [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=. , vce(robust)
noisily: display  ""
noisily: display  ""			
		

noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg(ii) Gamma Uncorrected, lowess smoothed Q1 series"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg hp_qn1u_mob_12_mm_xlin_sm hp_lunrate_bls i.season [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=. , vce(robust)
					
log off regresultslog3 
}		
		
log close regresultslog3




noisily: reg hp_qn1_mob_12_mm hp_lunrate_bls [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=. & bp_qn1_mob_12_mm!=. , vce(robust)

noisily: reg hp_qn1_mob_12_mm hp_lunrate_bls [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=. , vce(robust)
						
noisily: reg hp_qn1_mob_12_mm hp_loutpw [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=. & bp_qn1_mob_12_mm!=. , vce(robust)

noisily: reg hp_qn1_mob_12_mm hp_loutpw [w=qn1_cum_mob_desc12_mm_obs] ///
						if qn1_mob_12_mm!=.  , vce(robust)						

/*
						
*******************************************************
			** stats for calibration, Q3 smoothed 
*******************************************************
			


cap log close regresultslog4
quietly {
log using "${mainresultsdir}/table4_occmob.txt", replace text name(regresultslog4)

noisily: display "-------------------------------------------------------------"
noisily: display " TABLE 4 - OCC MOBILITY TIME SERIES "
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""

noisily: display  ""
noisily: display  "   ---autocorrelation--- "
noisily: reg hp_qn3_mob_12_mm_xlin_sm l.hp_qn3_mob_12_mm_xlin_sm if qn3_mob_12_mm!=. & bp_qn1_mob_12_mm!=.
noisily: display  ""
	

noisily: display  ""
noisily: display  "   ---st dev--- "
noisily: su hp_qn3_mob_12_mm_xlin_sm if qn3_mob_12_mm!=. & bp_qn1_mob_12_mm!=.
noisily: display  ""

noisily: display  ""
noisily: display  "   ---corr mob, u--- "
noisily: corr hp_qn3_mob_12_mm_xlin_sm hp_q3_lunrate_bls if qn3_mob_12_mm!=.   & bp_qn1_mob_12_mm!=.
noisily: display  ""
	
noisily: display  ""
noisily: display  "   ---corr mob, y--- "
noisily: corr hp_qn3_mob_12_mm_xlin_sm hp_q3_loutpw if qn3_mob_12_mm!=.   & bp_qn1_mob_12_mm!=.
noisily: display  ""

log off regresultslog4
}		
		
*/


********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"