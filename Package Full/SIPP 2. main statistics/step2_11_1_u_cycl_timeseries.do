

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


 
 
 
 //====================================
 //  PULL AGGREGATE SERIES TOGETHER
 //====================================

 
 

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



	global use_tramoed_u_series=1 
	
// AUX PROGRAMS


cap n program drop excel_col_exe
	program define excel_col_exe, rclass
			args lnumber

	local excelcc=`lnumber'
	tokenize "`c(alpha)'"
	display "`lnumber', and resulting number `excelcc'"
	if `excelcc'<=26 {
	return local xlscol "``excelcc''"
	}
	if `excelcc'>26 {
	local firstletter=floor((`excelcc'-1)/26)
	local secondletter=`excelcc'-26*`firstletter'
	return local xlscol "``firstletter''``secondletter''"
	}

end

 
/*
 // output and unemployment
 use "C:\data\aggdata_2018.dta", clear
 cap n ren unrate unrate_bls
 // BLS JF AND SEP
 merge 1:1 quarter using "C:\Users\lviss\Dropbox\CTV_Revisions\Aggregate Data Updated\jf_sepu_bls.dta"
 
 
 
 
 // young and prime unemployment from the bls
 capture drop _merge_uy_up
 merge 1:1 quarter using "C:\Users\lviss\Dropbox\CTV_Revisions\Aggregate Data Updated\youth_and_prime_aged_u.dta", gen(_merge_uy_up)
 ren lns14000036 uy_bls
 su uy_bls
 if r(mean)>1 {
 replace uy_bls=uy_bls/100
 }
 ren lns14000060 up_bls
 su up_bls
 if r(mean)>1 {
 replace up_bls=up_bls/100
 }
 
 
 
 // vacancies
 capture drop hp_lvacancy 
 capture drop lvacancy 
 capture drop hp_ltightness 
 capture drop hp_lvacancy 
 capture drop tightness 
 capture drop ltightness 
 capture drop _merge 
 merge 1:1 quarter using "C:\Users\lviss\Dropbox\CTV_Revisions\Aggregate Data Updated\pwd.dta", ///
			keepusing( hp_lvacancy lvacancy hp_ltightness hp_lvacancy tightness ltightness unrate)
 */
 
 // unemployment series from the SIPP
 use "${tempdata}/timeseries_u.dta", clear
 	  
 
 
 
 su urate_bls
 if r(mean)>1 {
 replace urate_bls=urate_bls/100
 }

 
merge 1:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta",  
keep if  quarter>=tq(1983q1) & quarter<=tq(2013q4) 

cap n gen unrate_bls=unemp_bls
 su unrate_bls
 if r(mean)>1 {
 replace unrate_bls=unrate_bls/100
 }
cap n gen lunrate_bls=log(unrate_bls)
 
sort quarter
tsset quarter
tsfill

replace urate_bls=unrate_bls if urate_bls==.

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


 
 
 
 // CREATE HP_UNRATE with 0<u<1 (NOT IN PERCENTAGE POINTS)
 ** USE UNRATE_BLS
 capture drop hp_unrate
 tsfilter hp hp_unrate = urate_bls, smooth(1600)   

regress unrate_bls quarter if quarter>=tq(1984q1) & quarter<=tq(2013q4)
 predict unrate_ldt, res
 su unrate_ldt, detail

 regress lunrate_bls quarter if quarter>=tq(1984q1) & quarter<=tq(2013q4)
 cap n predict lunrate_ldt, res
 su lunrate_ldt, detail

 
regress loutpw quarter if quarter>=tq(1984q1) & quarter<=tq(2013q4)
 cap n predict loutpw_ldt, res
 su loutpw_ldt, detail
 
 
 
 //=============================================================================
 //  PRODUCTIVITY SERIES
 //=============================================================================

global filename "${tempdata}/ts_stats_u.xls"
local sheetname "prod_u"
cap n putexcel set "${filename}"
cap n putexcel set "${filename}", sheet("`sheetname'", replace)
cap n putexcel set "${filename}", modify sheet("`sheetname'", replace)

 putexcel A3=("autocorrelation")
 putexcel A4=("specification")
 putexcel A5=("std dev")
 putexcel A6=("corr w/u")
 putexcel A7=("emp el w/u")
 putexcel A8=("emp el w/y")
 tsset quarter
 
 
 putexcel B1=("PRODUCTIVITY")
 putexcel B2=("hp_loutpw")
 reg hp_loutpw l.hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel B3=(regtable[1,1])
 putexcel B4=(e(cmdline))
 su hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel B5=(r(sd))
 corr hp_loutpw hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel B6=(r(rho))
 reg hp_loutpw hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel B7=(regtable[1,1])
 capture drop test_ypred
 predict test_ypred
 
 putexcel C1=("UNEMPLOYMENT")
 putexcel C2=("hp_lunrate")
 reg hp_lunrate l.hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel C3=(regtable[1,1])
 putexcel C4=(e(cmdline))
 su hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel C5=(r(sd))
 corr hp_lunrate hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel C6=(r(rho))
 reg hp_lunrate hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel C8=(regtable[1,1])
 capture drop test_upred
 predict test_upred
 
 
 
 //=============================================================================
 //  PREDICT AND HP-FILTER  ---- UNEMPLOYMENT 
 //=============================================================================

 
 // UNEMPLOYMENT STATS 
 // PREDICT
 
 
  global sheetname "u_ts_with_ubls"
cap n putexcel set "${filename}", sheet("${sheetname}", replace)
cap n putexcel set "${filename}", modify sheet("${sheetname}", replace)


putexcel A1=("series_name")
putexcel A3=("--HP FILTERED--")


putexcel A4=("no_obs")
putexcel A5=("mean_level")
putexcel A6=("ac")
putexcel A7=("std dev")
putexcel A8=("el w. u")
putexcel A9=("(se)")
putexcel A10=("el w. y")
putexcel A11=("(se)")
putexcel A12=("corr w y") 
putexcel A13=("corr w u")
putexcel A14=("semi-el w. u")
putexcel A15=("(se)")
putexcel A16=("semi-el w. y")
putexcel A17=("(se)")
putexcel A18=("abs change  w. u")
putexcel A19=("(se)")

putexcel A23=("--LIN DETRENDED--")
putexcel A24=("ac")
putexcel A25=("std dev")
putexcel A26=("el w. u")
putexcel A27=("(se)")
putexcel A28=("el w. y")
putexcel A29=("(se)")
putexcel A30=("corr w y") 
putexcel A31=("corr w u")
putexcel A32=("semi-el w. u")
putexcel A33=("(se)")
putexcel A34=("semi-el w. y")
putexcel A35=("(se)")
putexcel A36=("abs change  w. u")
putexcel A37=("(se)")


 
 ** seeing which 
 capture drop lurate_bls
 *capture drop lunrate_bls
 cap n gen lurate_bls=log(urate_bls)
 *cap n gen lunrate_bls=lurate_bls
 corr log_ur_all_durwvcearly lunrate
 corr log_ur_all_durwvcearly lurate_bls
 corr log_ur_all_durwvcearly lunrate_bls
 *corr ur_all_durwvcearly unrate
 corr ur_all_durwvcearly urate_bls
 corr ur_all_durwvcearly unrate_bls
 
 
 //========================================================
 // BASIC PROGRAM WITH ONLY ONE SERIES USED TO PREDICT
 //==========================================================
 
 global colno=2
 
 //PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
 //PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
 
				 capture program drop lhp_predict_with_u_exe
				 program define lhp_predict_with_u_exe
							args dataseries_in colno refseries 
									*refseries here helps fill in the missings

				cap n putexcel set "${filename}", sheet("${sheetname}", replace)
				cap n putexcel set "${filename}", modify sheet("${sheetname}", replace)

					* correlation, and elasticity, std dev, and ac only take into account those values where the original series exists
									
				 excel_col_exe `colno'
				 local col=r(xlscol)

				 putexcel `col'1=("`dataseries_in'_ext")
				 
				 
				 reg `dataseries_in' `refseries'
				 
				 capture drop `dataseries_in'_ext
				 predict `dataseries_in'_ext
				 replace `dataseries_in'_ext=`dataseries_in' if `dataseries_in'!=.
				 
				 capture drop l`dataseries_in'_ext
				 gen l`dataseries_in'_ext=log(`dataseries_in'_ext)
				 
				 capture drop hp_l`dataseries_in'_ext
				 tsfilter hp hp_l`dataseries_in'_ext = l`dataseries_in'_ext, smooth(1600)   
				 
				 capture drop hp_`dataseries_in'_ext
				 tsfilter hp hp_`dataseries_in'_ext = `dataseries_in'_ext, smooth(1600)   
				 
				 su `dataseries_in'
				 * no obs, and mean
				 putexcel `col'4=(r(N))
				 putexcel `col'5=(r(mean))
				 
				 * stdev hpfiltered log series
				 su hp_l`dataseries_in'_ext if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.,detail
				 putexcel `col'7=(r(sd))
				 
				 * ac hpf log
				 reg hp_l`dataseries_in'_ext l.hp_l`dataseries_in'_ext if $hp_quartercond & `dataseries_in'!=. & `dataseries_in'[_n-1]!=. $add_selection & `dataseries_in'!=. & `dataseries_in'[_n-1]!=.
				 matrix regreturn=r(table)
				 putexcel `col'6=(regreturn[1,1])
				 
				 * elasticity with hpf log outputpw
				 reg hp_l`dataseries_in'_ext hp_loutpw if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'10=(regreturn[1,1])
				 putexcel `col'11=(regreturn[2,1])

				 * corr hpf with hpf outpt
				 corr hp_l`dataseries_in'_ext hp_loutpw if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 putexcel `col'12=(r(rho)) 
				 
				 * el with hpf log ubls
				 reg hp_l`dataseries_in'_ext hp_lunrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'8=(regreturn[1,1])
				 putexcel `col'9=(regreturn[2,1])
				 
				 * cor hpf log with hp log u
				 corr hp_l`dataseries_in'_ext hp_lunrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 putexcel `col'13=(r(rho))
				 
				 **** SEMI-ELASTICITIES 
				 reg hp_`dataseries_in'_ext hp_loutpw if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'16=(regreturn[1,1])
				 putexcel `col'17=(regreturn[2,1])

				 
				 reg hp_`dataseries_in'_ext hp_lunrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'14=(regreturn[1,1])
				 putexcel `col'15=(regreturn[2,1])
				 
				 
				 **** ABSOLUTE CHANGES with U_BLS 
				 reg hp_`dataseries_in'_ext hp_unrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'18=(regreturn[1,1])
				 putexcel `col'19=(regreturn[2,1])
				 
				 
				 **** LINEARLY DETRENDED 
				 
				 capture drop ld_l`dataseries_in'
				 capture drop l`dataseries_in'
				 capture drop ld_`dataseries_in'
				 gen l`dataseries_in'=log(`dataseries_in')
				 
				 reg l`dataseries_in'  quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 predict ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=., res
				 
				 reg `dataseries_in'  quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 predict ld_`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=., res
				 
				 
				 su ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.,detail
				 putexcel `col'25=(r(sd))
				 
				 reg ld_l`dataseries_in' l.ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `refseries'[_n-1]!=. & `dataseries_in'!=. & `dataseries_in'[_n-1]!=.
				 matrix regreturn=r(table)
				 putexcel `col'24=(regreturn[1,1])
				 
				 reg ld_l`dataseries_in' loutpw_ldt quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'28=(regreturn[1,1])
				 putexcel `col'29=(regreturn[2,1])

				 corr ld_l`dataseries_in' loutpw_ldt if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'30=(r(rho)) 
				 
				 reg ld_l`dataseries_in' lunrate_ldt quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'26=(regreturn[1,1])
				 putexcel `col'27=(regreturn[2,1])
				 
				 corr ld_l`dataseries_in' lunrate_ldt if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'31=(r(rho))
				 
				 	 **** SEMI-ELASTICITIES 
				 reg ld_`dataseries_in' loutpw_ldt if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'34=(regreturn[1,1])
				 putexcel `col'35=(regreturn[2,1])

				 reg ld_`dataseries_in' lunrate_ldt if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'32=(regreturn[1,1])
				 putexcel `col'33=(regreturn[2,1])
				 
				 
				 **** ABSOLUTE CHANGES with U_BLS 
				 reg ld_`dataseries_in' unrate_ldt if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'36=(regreturn[1,1])
				 putexcel `col'37=(regreturn[2,1])
				 
			
				 
				 
				 end 
				 
 
 
 //=================================
 // PROGRAM WITHOUT THE PREDICTION
 //=================================
 

 //PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
 //PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
				 
				 capture program drop lhp_ts_analysis_exe
				 program define lhp_ts_analysis_exe
							args dataseries_in colno refseries 
									*refseries here tells which quarters are taken into accoutn
							
				cap n putexcel set "${filename}", sheet("${sheetname}", replace)
				cap n putexcel set "${filename}", modify sheet("${sheetname}", replace)
				 
				 excel_col_exe `colno'
				 local col=r(xlscol)

				 putexcel `col'1=("`dataseries_in'")
				 
				 capture drop l`dataseries_in'
				 gen l`dataseries_in'=log(`dataseries_in')
				 
				 
				 **** HP FILTERED 
				 /*
				 capture drop hp_l`dataseries_in'
				 tsfilter hp hp_l`dataseries_in' = l`dataseries_in', smooth(1600)   
				 
				 su hp_l`dataseries_in' if $hp_quartercond & `refseries'!=. & `dataseries_in'!=.,detail
				 putexcel `col'5=(r(sd))
				 
				 reg hp_l`dataseries_in' l.hp_l`dataseries_in' if $hp_quartercond & `refseries'!=. & `refseries'[_n-1]!=. & `dataseries_in'!=. & `dataseries_in'[_n-1]!=.
				 matrix regreturn=r(table)
				 putexcel `col'4=(regreturn[1,1])
				 
				 reg hp_l`dataseries_in' hp_loutpw if $hp_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'8=(regreturn[1,1])
				 putexcel `col'9=(regreturn[2,1])

				 corr hp_l`dataseries_in' hp_loutpw if $hp_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'10=(r(rho)) 
				 
				 reg hp_l`dataseries_in' hp_lunrate if $hp_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'6=(regreturn[1,1])
				 putexcel `col'7=(regreturn[2,1])
				 
				 corr hp_l`dataseries_in' hp_lunrate if $hp_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'11=(r(rho))
				 
				 
				 **** LINEARLY DETRENDED 
				 
				 capture drop ld_l`dataseries_in'
				 reg l`dataseries_in'  quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 predict ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=., res
				 
				 su ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.,detail
				 putexcel `col'15=(r(sd))
				 
				 reg ld_l`dataseries_in' l.ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `refseries'[_n-1]!=. & `dataseries_in'!=. & `dataseries_in'[_n-1]!=.
				 matrix regreturn=r(table)
				 putexcel `col'14=(regreturn[1,1])
				 
				 reg ld_l`dataseries_in' loutpw_ldt quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'18=(regreturn[1,1])
				 putexcel `col'19=(regreturn[2,1])

				 corr ld_l`dataseries_in' loutpw_ldt if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'20=(r(rho)) 
				 
				 reg ld_l`dataseries_in' lunrate_ldt quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'16=(regreturn[1,1])
				 putexcel `col'17=(regreturn[2,1])
				 
				 corr ld_l`dataseries_in' lunrate_ldt if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'21=(r(rho))
				 */
				 
				 capture drop hp_l`dataseries_in'
				 tsfilter hp hp_l`dataseries_in' = l`dataseries_in', smooth(1600)   
				 
				 capture drop hp_`dataseries_in'
				 tsfilter hp hp_`dataseries_in' = `dataseries_in', smooth(1600)   
				 
				 su `dataseries_in'
				 * no obs, and mean
				 putexcel `col'4=(r(N))
				 putexcel `col'5=(r(mean))
				 
				 * stdev hpfiltered log series
				 su hp_l`dataseries_in' if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.,detail
				 putexcel `col'7=(r(sd))
				 
				 * ac hpf log
				 reg hp_l`dataseries_in' l.hp_l`dataseries_in' if $hp_quartercond & `dataseries_in'!=. & `dataseries_in'[_n-1]!=. $add_selection & `dataseries_in'!=. & `dataseries_in'[_n-1]!=.
				 matrix regreturn=r(table)
				 putexcel `col'6=(regreturn[1,1])
				 
				 * elasticity with hpf log outputpw
				 reg hp_l`dataseries_in' hp_loutpw if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'10=(regreturn[1,1])
				 putexcel `col'11=(regreturn[2,1])

				 * corr hpf with hpf outpt
				 corr hp_l`dataseries_in' hp_loutpw if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 putexcel `col'12=(r(rho)) 
				 
				 * el with hpf log ubls
				 reg hp_l`dataseries_in' hp_lunrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'8=(regreturn[1,1])
				 putexcel `col'9=(regreturn[2,1])
				 
				 * cor hpf log with hp log u
				 corr hp_l`dataseries_in' hp_lunrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 putexcel `col'13=(r(rho))
				 
				 **** SEMI-ELASTICITIES 
				 reg hp_`dataseries_in' hp_loutpw if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'16=(regreturn[1,1])
				 putexcel `col'17=(regreturn[2,1])

				 
				 reg hp_`dataseries_in' hp_lunrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'14=(regreturn[1,1])
				 putexcel `col'15=(regreturn[2,1])
				 
				 
				 **** ABSOLUTE CHANGES with U_BLS 
				 reg hp_`dataseries_in' hp_unrate if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'18=(regreturn[1,1])
				 putexcel `col'19=(regreturn[2,1])
				 
				 
				 **** LINEARLY DETRENDED 
				 
				 capture drop ld_l`dataseries_in'
				 capture drop l`dataseries_in'
				 capture drop ld_`dataseries_in'
				 gen l`dataseries_in'=log(`dataseries_in')
				 
				 reg l`dataseries_in'  quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 predict ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=., res
				 
				 reg `dataseries_in'  quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 predict ld_`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=., res
				 
				 
				 su ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.,detail
				 putexcel `col'25=(r(sd))
				 
				 reg ld_l`dataseries_in' l.ld_l`dataseries_in' if $ld_quartercond & `refseries'!=. & `refseries'[_n-1]!=. & `dataseries_in'!=. & `dataseries_in'[_n-1]!=.
				 matrix regreturn=r(table)
				 putexcel `col'24=(regreturn[1,1])
				 
				 reg ld_l`dataseries_in' loutpw_ldt quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'28=(regreturn[1,1])
				 putexcel `col'29=(regreturn[2,1])

				 corr ld_l`dataseries_in' loutpw_ldt if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'30=(r(rho)) 
				 
				 reg ld_l`dataseries_in' lunrate_ldt quarter if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'26=(regreturn[1,1])
				 putexcel `col'27=(regreturn[2,1])
				 
				 corr ld_l`dataseries_in' lunrate_ldt if $ld_quartercond & `refseries'!=. & `dataseries_in'!=.
				 putexcel `col'31=(r(rho))
				 
				 	 **** SEMI-ELASTICITIES 
				 reg ld_`dataseries_in' loutpw_ldt if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'34=(regreturn[1,1])
				 putexcel `col'35=(regreturn[2,1])

				 reg ld_`dataseries_in' lunrate_ldt if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'32=(regreturn[1,1])
				 putexcel `col'33=(regreturn[2,1])
				 
				 
				 **** ABSOLUTE CHANGES with U_BLS 
				 reg ld_`dataseries_in' unrate_ldt if $hp_quartercond & `dataseries_in'!=. $add_selection & `dataseries_in'!=.
				 matrix regreturn=r(table)
				 putexcel `col'36=(regreturn[1,1])
				 putexcel `col'37=(regreturn[2,1])
				 
				 
				 end 
				 
 //=============================================================================
 // PREDECT LDT AND HPF UNEMPLOYMENT SERIES 
 //=============================================================================
 
 // OVERALL UNEMPLOYMENT RATE 
 sort quarter
 global hp_quartercond " quarter>=tq(1983q1) & quarter<=tq(2013q4) "
 global ld_quartercond " quarter>=tq(1983q1) & quarter<=tq(2013q4) "
 
 putexcel B2=("--ALL--")
 lhp_predict_with_u_exe ur_all_durwvcearly 2 unrate_bls
 lhp_predict_with_u_exe  ur_all_norestr 3 unrate_bls
 lhp_predict_with_u_exe  ur_all_icnun_durwvnosttg  4 unrate_bls
 lhp_predict_with_u_exe  ur_all_icnun_dwvsttg24  5 unrate_bls
 lhp_predict_with_u_exe  ur_all_icnun_dwvsttg30 6 unrate_bls
 
 
 // YOUNG
 ren *young* *yng*
 
 putexcel I2=("--yng--")
 lhp_predict_with_u_exe ur_yng_durwvcearly 9 unrate_bls
 lhp_predict_with_u_exe  ur_yng_norestr 10 unrate_bls
 lhp_predict_with_u_exe  ur_yng_icnun_durwvnosttg  11 unrate_bls
 lhp_predict_with_u_exe  ur_yng_icnun_dwvsttg24  12 unrate_bls
 lhp_predict_with_u_exe  ur_yng_icnun_dwvsttg30 13 unrate_bls
 
 // PRIME 
 ren *prime* *prm*
 
 putexcel O2=("--prm--")
 lhp_predict_with_u_exe ur_prm_durwvcearly 15 unrate_bls
 lhp_predict_with_u_exe  ur_prm_norestr 16 unrate_bls
 lhp_predict_with_u_exe  ur_prm_icnun_durwvnosttg  17 unrate_bls
 lhp_predict_with_u_exe  ur_prm_icnun_dwvsttg24  18 unrate_bls
 lhp_predict_with_u_exe  ur_prm_icnun_dwvsttg30 19 unrate_bls
 
 
 
capture drop hp_lunrate_bls
 tsfilter hp hp_lunrate_bls = lunrate_bls, smooth(1600)   

 
 **** SAVE SERIES
 keep quarter durdistr_stability fullseam_qtr_ind fullseam_12lag_qtr_ind fullseam_14lag_qtr_ind ///  quarter + indicators 
 unrate_bls quarter outpw outpw2 lunrate* hp_lunrate* hp_loutpw* ///
 hp_lur_all_durwvcearly_ext hp_ur_all_durwvcearly_ext lur_all_durwvcearly lur_all_durwvcearly_ext /// all durwvc measure
 hp_lur_yng_durwvcearly_ext hp_ur_yng_durwvcearly_ext lur_yng_durwvcearly lur_yng_durwvcearly_ext ///  yng durwvc measure
 hp_lur_prm_durwvcearly_ext hp_ur_prm_durwvcearly_ext lur_prm_durwvcearly lur_prm_durwvcearly_ext // prm durwvc
 
save "${outputdata}/u_hp_ts2020.dta", replace 
 
keep quarter lur_all_durwvcearly_ext hp_lur_all_durwvcearly_ext hp_ur_all_durwvcearly_ext lur_all_durwvcearly
  
save "${outputdata}/timeseries_u_durwvcearly.dta", replace
 

 
/* 
********************************************************************************
** ALTERNATIVE, TRAMO LUR_ALL_DURWVCEARLY.
******************************************************************************** 
 
 
/* 
********************************************************************************
** 
**		TRAMO / SEATS USING TSW+  
**
********************************************************************************



In brief, we use the standard in the tsw program , with automatic procedure parameter RSA=4
then we save the TRAMO and SEATS output with the appropriate name (see below in program) 	

TRAMO: we have three series:
	-xorig series: original input, with -99999 for missing values
	-xint: interpolated series, but original values when non-interpolated
	-xlin: clear of deterministic variation. 	

														
** BY DEFAULT WE USE XLIN SERIES (that one is calendar adjusted, plus other deterministic issues)
(XINT is interpolated, but not adjusted for deterministic (seasonal) variation; however, XLIN allows
for clearing out structural breaks, which in some settings would not be desired)

														
** SEATS (on top of TRAMO output) further decomposes the time series.
 - trend-cycle series: smooth series that captures both trend and cycle, but outliers and SA corrected
	this series is relevant as the quarter-by-quarter (unsmoothed) mobility is quite noisy
 
 
 
*** HOW TO USE TSW+ *****

1) Open/Run the program
2) Click the 'Series' button, this opens the standard 'Open' dialog in Windows.
	 - Go to the outputdata directory, and select u_ts_for_tramo.xlsx
	 - A window pops up, inquiring about the periodicity, Press OK at periodity 4.
	 - In the series list window, expand the series list. 
3) In the series list, select the series to apply TRAMO/SEATS to, e.g. qn3_mob_12_mm
	 - On the right-hand side, next to the series window, verify that the following 
		options are set
			-- iter=0
			-- Seats/Tramo to Tramo/Seats
4) Press the 'Model (+)' button. Verify the automatic procedure parameter is 
	set to (RSA=)4
5) Press 'Run'
6) Click button 'Out Tables', choose option 'Out Table Tramo'
	- this shows the Xorig, Xint, Xlin timeseries
7) At the bottom, press the right-most Save Excel button, save the file as 
	[series name].xlsx, in our example lur_all_durwvcearly.xlsx.
	- It is important that the names are kept, as below will import the resulting 
		series back into STATA
8) Close the Out Table window, returning to the main program
9) Click button 'Out Tables', choose option 'Out Table Seats'
10) Click Save Excel button at the bottom, save file as 
	[series name]_seats.xlsx (in our example qn3_mob_12_mm_seats.xlsx)
	- close 'Seats' window, and move to the next series (i.e. return to step 3)
	- cycle through step 3 to step 10 for all series of interest.

end) Copy all the series produced to the output (${outputdata} directory 
		(if not already saved there, by hand), where the input series were found. 
		By default TSW will suggest the .\TSW+\SAVED\ directory for saving files. 
		At the end of saving all files in the latter, one could copy-paste 
		these files into the ${outputdata} directory.
		This is not unimportant: below STATA will look for those files there. 
		However, see below about using the reference series to keep STATA running,
		and just verifying the TRAMO/SEATS at the very end of replication. 
		

*****
Inside a subdirectory of our step 2 code directory (${step2codedir}/Tramo_Seats_series_REFERENCE), we have saved the before and 
after timeseries we are using in our paper. This means that, when replicating, STATA can run 
uninterruptedly till the very end, and the TRAMO/SEATS step can be verified ex post
		- check that the before-tramo series produced in the replication overlap
		with the reference before-tramo series. 
		- do tramo/seats (via TSW+) on the series produced in replication, and 
		compare these to the after-tramo reference series. (If copying these
		series into the ${outputdata} directory, verify the timestamp 
		to make sure that the series previously there is overwritten!)
To facilitate this comparison, this do-file, at the beginning, placed a copy of 
these reference series in the ${outputdata} directory, so all series are in one
location (though the after-tramo series were put there by hand).

*/



 
******************************************************************************** 
** READ IN TRAMO-ED SERIES 
********************************************************************************


	capture program drop tsudist_aftertramo_to_dta_exe
	program define tsudist_aftertramo_to_dta_exe
						args filename indic name
	clear

	** uprop
	*local name "unemp_cat`indic'q"
						display "`indic'"
	*import excel "C:/TSW+/SAVED/`filename'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow
	import excel "${outputdata}/`filename'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow

	gen quarter=quarterly(DATE, "QY")
	format quarter %tq 
	ren Xorig  `name'_xorig
	ren Xint `name'_xint
	ren Xlin `name'_xlin
	keep quarter `name'*
	mvdecode  `name'*, 	mv(-99999)

	*save "${tempdata}/qtr_`filename'_aftertramo_aug2019.dta", replace 
	save "${tempdata}/qtr_`filename'_aftertramo_.dta", replace 



				** luprop
				/*
				clear
				*local name "lunemp_cat`indic'q"
									display "`indic'"
				import excel "C:/TSW+/SAVED/`filename'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow

				gen quarter=quarterly(DATE, "QY")
				format quarter %tq 
				ren Xorig  `name'_xorig
				ren Xint `name'_xint
				ren Xlin `name'_xlin
				keep quarter `name'*
				mvdecode  `name'*, 	mv(-99999)
				
				saveold "${datadir1}ts_series/qtr_ludist`indic'prop_aftertramo_aug2019.dta", replace 
				*/
	end 


	
	 tsudist_aftertramo_to_dta_exe lur_all_durwvcearly 0 lur_all_durwvcearly
	 tsudist_aftertramo_to_dta_exe ur_all_durwvcearly 1 ur_all_durwvcearly
	 
	 
**
**  MERGE HP-FILTER THESE
**

clear
*use "H:\laptop beast hdd\data\udistrprops_for_tramo.dta"
*global temptempdata "H:/laptop beast hdd/data/ts_series/"
use "${tempdata}/qtr_lur_all_durwvcearly_aftertramo_.dta", clear
tokenize ur_all_durwvcearly
forvalues i=1(1)1 {
		capture drop merge_distr`i'
		*merge m:1 quarter using "${temptempdata}/qtr_unemp`i'q_aftertramo.dta", gen(merge_distr`i')
		merge 1:1 quarter using "${tempdata}/qtr_``i''_aftertramo_.dta", gen(merge_distr`i')
		drop if merge_distr`i'==2

		/*
		capture drop merge_distrl`i'
		merge m:1 quarter using "${datadir1}ts_series/qtr_lunemp`i'q_aftertramo_aug2019.dta", gen(merge_distrl`i')
		drop if merge_distrl`i'==2
		*/
}

tsset quarter
tsfill

cap n ren *ur_all_durwvcearly* *ur_all_durwvcearly_tramo*

** hp filter

	
	cap n tsfilter hp hp_lur_all_durwvcearly_tramo =  lur_all_durwvcearly_tramo_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_ur_all_durwvcearly_tramo =  ur_all_durwvcearly_tramo_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	


** HOW SENSITIVE ARE THESE ELASTICITIES TO SMOOTHING 
capture drop unrate2
cap n ren unrate unrate2

capture drop merge_qtru
merge 1:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", keepusing(lunrate*) gen(merge_qtru)
*drop merge_qtru

capture drop merge_orig
merge 1:1 quarter using "${outputdata}/timeseries_u_durwvcearly.dta", gen(merge_orig)
*drop merge_orig

drop if quarter<tq(1980q1)
cap n gen ur_all_durwvcearly=exp(lur_all_durwvcearly)

cap n tsfilter hp hp_lur_all_durwvcearly_ext_test =  lur_all_durwvcearly_ext if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)

*corr hp_lur_all_durwvcearly_tramo hp_lur_all_durwvcearly_ext_test  if lur_all_durwvcearly!=.
*scatter hp_lur_all_durwvcearly_tramo hp_lur_all_durwvcearly_ext_test  quarter if lur_all_durwvcearly!=.

capture drop lur_all_durwvcearly_inst
*reg log_ur_all_durwvcearly unrate
reg lur_all_durwvcearly lunrate_bls
predict lur_all_durwvcearly_inst

cap n tsfilter hp hp_lur_all_durwvcearly_inst =  lur_all_durwvcearly_inst if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)


su hp_lur_all_durwvcearly_tramo hp_lur_all_durwvcearly_ext_test  hp_lur_all_durwvcearly_inst if lur_all_durwvcearly!=.
corrgram hp_lur_all_durwvcearly_tramo if lur_all_durwvcearly!=. & lur_all_durwvcearly[_n-1]!=., noplot lags(1)
corrgram hp_lur_all_durwvcearly_ext_test if lur_all_durwvcearly!=. & lur_all_durwvcearly[_n-1]!=., noplot lags(1)
corrgram hp_lur_all_durwvcearly_inst if lur_all_durwvcearly!=. & lur_all_durwvcearly[_n-1]!=., noplot lags(1)


** 5Q  UNEMP RATE LOGGED 

**
**  SMOOTH THE U DISTRIBUTION PROPERTIONS
**


capture program drop ts_smooth_and_filter_exe
program define ts_smooth_and_filter_exe
			args name_in 

	capture drop sm5_`name_in'
	capture drop hp_sm5_`name_in'
	cap n tssmooth ma sm5_`name_in'= `name_in', window(2 1 2)
	cap n tsfilter hp hp_sm5_`name_in' =  sm5_`name_in'  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
				
				
end program


ts_smooth_and_filter_exe lur_all_durwvcearly_ext 
ts_smooth_and_filter_exe lur_all_durwvcearly_inst 
cap n gen lur_all_durwvcearly_tramo=lur_all_durwvcearly_tramo_xlin
ts_smooth_and_filter_exe lur_all_durwvcearly_tramo

su hp_sm5_lur_all_durwvcearly_ext hp_sm5_lur_all_durwvcearly_inst hp_sm5_lur_all_durwvcearly_tramo
corrgram hp_sm5_lur_all_durwvcearly_tramo if lur_all_durwvcearly!=. & lur_all_durwvcearly[_n-1]!=., noplot lags(1)
corrgram hp_sm5_lur_all_durwvcearly_ext if lur_all_durwvcearly!=. & lur_all_durwvcearly[_n-1]!=., noplot lags(1)
corrgram hp_sm5_lur_all_durwvcearly_inst if lur_all_durwvcearly!=. & lur_all_durwvcearly[_n-1]!=., noplot lags(1)

*/


********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"