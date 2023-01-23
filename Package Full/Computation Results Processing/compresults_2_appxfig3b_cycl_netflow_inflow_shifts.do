
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
***  - CYCLICAL INFLOW CHANGES RELATES TO NET FLOW CHANGES, APPENDIX FIG 3B
***  - TABLE 6, DECOMPOSITION OF OCC SIZE CHANGES
***
********************************************************************************



	clear
	do "${workingdir}/global_paths.do"
	
	version 13
	
	set more off
	set varabbrev off

	
	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"





** load in the model generated data 
import delimited "${fortrandir}/Gross and Net Mobility version/shifts_netflow_inflow.csv", numericcols(66) clear 
global lversion netmob
cap n ren ïocc occ
cap n ren occupation occ
cap n ren nmob_cshift_m nmob_cshft_m
cap n ren infl_cshift_m infl_cshft_m
cap n drop occ_from
gen occ_from=occ

** load in the actual SIPP data!!!
capture drop _merge
cap n merge 1:1 occ using "${mainresultsdir}/table2_panelC_netflow_inflowdistr_cycleshift.dta"
cap n merge 1:1 occ_from using "${mainresultsdir}/table2_panelC_netflow_inflowdistr_cycleshift.dta"

capture drop nmob_cshft_mpic
capture drop nmob_cshft_dpic

capture drop infl_cshft_mpic
capture drop infl_cshft_dpic

gen nmob_cshft_mpic= nmob_cshft_m
gen infl_cshft_mpic= -infl_cshft_m

gen nmob_cshft_dpic= rec_exp_netflowshift
gen infl_cshft_dpic= -inflowshift_exp_rec


capture drop pred_infl_cshft_mpic
reg infl_cshft_mpic nmob_cshft_mpic
predict pred_infl_cshft_mpic

capture drop pred_infl_cshft_dpic
reg infl_cshft_dpic nmob_cshft_dpic
predict pred_infl_cshft_dpic


******* APPENDIX FIGURE 3B

scatter infl_cshft_mpic nmob_cshft_mpic , msymbol(O) mcolor(blue) mlabel(occ_label) mlabcolor(blue) mlabsize(large) mlabpos(3) || scatter infl_cshft_dpic nmob_cshft_dpic , msymbol(Oh) mlabsize(large) msize(vlarge) yline(0, lcolor(black)) xline(0, lcolor(black)) mcolor(midblue) mlabel(occ_label) mlabcolor(midblue) mlabpos(12) graphregion(color(white)) || scatter pred_infl_cshft_mpic nmob_cshft_mpic , connect(l) msymbol(i) lpattern(dash_dot) lcolor(blue) || scatter pred_infl_cshft_dpic nmob_cshft_dpic , connect(l) msymbol(i) lpattern(solid) lcolor(midblue) legend(label(1 "Model") label(2 "Data") label(3 "Model, Fitted Values") label(4 "Data, Fitted Values") ring(0) cols(1) position(5) size(*1.4)) ytitle("Change Recession - Expansion" "Proportion Occ among Destinations", size(large) height(12)) xtitle("Change Recession - Expansion in Net Mobility", size(vlarge)) ysc(r(-0.07 0.07)) ylabel(-0.06(0.02)0.06, labsize(large)) xlabel(-0.01(0.01)0.02, labsize(large))

graph export "${mainresultsdir}/appxfig3b.pdf", as (pdf) replace


****** appendix table 2  model


quietly {
cap log close udurelaslog
log using "${mainresultsdir}/appxtable2_model.txt", replace text name(udurelaslog)

noisily: display  ""
noisily: display  "-----------------------------------------------------------------"
noisily: display  "NORMALIZED ELASTICITY U. DURATION P. SUPEROCC TO AGGREGATE U RATE"
noisily: display  "-----------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
version 13
noisily: table occlabel, c(mean udur_elas_m)

 
log close udurelaslog
	

}	
	
****** TABLE 6 PAPER
keep occ occlabel begin_distr end_distr_d end_distr_m decomp_exofl_m decomp_endofl_m decomp_endo_gd_m decomp_endo_bad_m
export excel using "${mainresultsdir}/table6.xlsx", firstrow(variables) replace

quietly {
cap log close table6log
log using "${mainresultsdir}/table6_explanation.txt", replace text name(table6log)

noisily: display  ""
noisily: display  "-----------------------------------------------------------------"
noisily: display  "DECOMPOSITION OF SUPEROCC EVOLUTION - EXPLANATION VARIABLES"
noisily: display  "-----------------------------------------------------------------"
noisily: display  ""
noisily: display  " begin_distr: the initial distribution (1984)"
noisily: display  " end_distr_d: the end distribution in the data(2012)"
noisily: display  " end_distr_m: end distribution in the model"
noisily: display  " decomp_exofl_m: the change in size through retirement/entry in diff. occ"
noisily: display  " decomp_endofl_m: the change in occsize through net mobility of the unempl."
noisily: display  " decomp_endo_gd_m: the part of endofl change occuring when urate<median"
noisily: display  " decomp_endo_bad_m: the part of endofl change occuring when urate>=median"
noisily: display  ""

log close table6log
	

}	



global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"