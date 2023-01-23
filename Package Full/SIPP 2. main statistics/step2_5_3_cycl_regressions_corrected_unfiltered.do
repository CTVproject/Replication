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




clear
import excel "${outputdata}/timeseriesdur_mm.xls", sheet("ts") firstrow


sort quarter n_spellength
destring quarter, replace
cap n destring u_spellength, replace
cap n destring n_spellength, replace

*cap n ren qn1_mobcorr_mm q1d_mobcorr_mm

format quarter %tq
drop if quarter<tq(1983q1)

capture drop season
gen season=quarter-floor(quarter/4)*4+1


*merge in aggregate data series		
merge m:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta"

quietly {

 
 
		
		

cap log close regresultslog2
log using "${mainresultsdir}/table1_regs_iv.txt", replace text name(regresultslog2)

noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (iv), panel A"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg qn1_mobcorr_mm hpf_lunrate_bls quarter i.season [aw=qn1_obs_mobcorr_mm], vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
	
		

**************
** With Spell duration
**************


** regression without controls (only linear trend)
local occmob_ind "lne_c_mmo"

noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (iv), panel B"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg qn1_mobcorr_mm hpf_lunrate_bls n_spellength quarter i.season  [aw=qn1_obs_mobcorr_mm], vce(cluster quarter)
log close regresultslog2
}		
		


**** ROBUSTNESS, DURDIST STABILITY

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


quietly {
cap log close regresultslog2_r1
log using "${outputdata}/table1_regs_iv_robust.txt", replace text name(regresultslog2_r1)

noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (iv), panel A, ROBUSTNESS DURDISTR"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg qn1_mobcorr_mm hpf_lunrate_bls quarter [aw=qn1_obs_mobcorr_mm] if durdistr_stability==1, vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
		

**************
** With Spell duration
**************

** regression without controls (only linear trend)
local occmob_ind "lne_c_mmo"
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (iv), panel B, ROBUSTNESS DURDISTR"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg qn1_mobcorr_mm hpf_lunrate_bls n_spellength quarter [aw=qn1_obs_mobcorr_mm] if durdistr_stability==1, vce(cluster quarter)
log close regresultslog2_r1
}		
		

********************************************************************************		
		
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"