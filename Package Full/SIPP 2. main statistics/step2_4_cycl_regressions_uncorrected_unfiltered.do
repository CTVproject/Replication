
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





// PRELIMINARIES

clear

***** PATHS 
cd "${workingdir}"			// !!! set in the global_paths.do, if necessary !!!
global workingdir "`c(pwd)'"			
do "${workingdir}/global_paths.do"

version 13
set more off
set varabbrev off


	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"




quietly {

use "${outputdata}/reduced_u_n_ctv.dta", clear
		

capture drop season
gen season=quarter-floor(quarter/4)*4+1



	global cyclicalvar "hpf_lunrate_bls"
	global noagric_apptemp " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
	global noagric 		   " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
	
	global sttg = "0"
	global addcondition "" // & durdistr_stability==1 
	global wavecond " & wave>3 & interview_no>14 & sample_timetogo>1"
	global nuncond "& complete_uspell==1"

local occmob_ind "lne_c_mmo"
global gl_weight "[pw=pweight2]"

cap n log close regresultslog
log using "${mainresultsdir}/table1_regs_v_vii.txt", replace text name(regresultslog)
** regression without controls (only linear trend)
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (v), panel A"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} quarter i.season ${gl_weight} ///
			if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength<=12 & n_spellength>=1 $addcondition  $noagric_apptemp $nuncond ,vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
** regression with demographic controls, and source occupation, classification dummies
local occmob_ind "lne_c_mmo"
local socc_grp "locc1bfr_mmo"
noisily: display  ""
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (vii), panel A"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg `occmob_ind' ${cyclicalvar}  ib2.educ2 i.sex i.race quarter i.`socc_grp' c.tage##c.tage##c.tage##c.tage quarter i.mm_clsfication_dum i.season ${gl_weight}  ///
		if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength <=12 & n_spellength  >=1 $addcondition $noagric_apptemp $nuncond   ,vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
	
		

**************
** With Spell duration
**************

global gl_weight "[pw=pweight2]"
** regression without controls (only linear trend)
local occmob_ind "lne_c_mmo"
log on regresultslog
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (v), panel B"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} c.n_spellength quarter i.season ${gl_weight} ///
			if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength<=12 & n_spellength>=1 $addcondition  $noagric_apptemp $nuncond ,vce(cluster quarter)
			
** regression with demographic controls, and source occupation, classification dummies
local occmob_ind "lne_c_mmo"
local socc_grp "locc1bfr_mmo"
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (vii), panel B"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} c.n_spellength  ib2.educ2 i.sex i.race quarter i.`socc_grp' c.tage##c.tage##c.tage##c.tage quarter i.mm_clsfication_dum i.season ${gl_weight}  ///
		if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength <=12 & n_spellength  >=1 $addcondition $noagric_apptemp $nuncond  ,vce(cluster quarter)

		
log close regresultslog
		
		

******************************************		
**** ROBUSTNESS, WITH DURDISTR CONDITION
******************************************



	global cyclicalvar "hpf_lunrate_bls"
	global noagric_apptemp " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
	global noagric 		   " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
	
	global sttg = "0"
	global addcondition " & durdistr_stability==1 "
	global wavecond " & wave>3 & interview_no>14 & sample_timetogo>1"
	global nuncond "& complete_uspell==1"

local occmob_ind "lne_c_mmo"
global gl_weight "[pw=pweight2]"

cap n log close regresultslog_r1
log using "${outputdata}/table1_regs_v_vii_robust.txt", replace text name(regresultslog_r1)

** regression without controls (only linear trend)
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (v), panel A, DURDISTR"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} quarter ${gl_weight} ///
			if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength<=12 & n_spellength>=1 $addcondition  $noagric_apptemp $nuncond ,vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
** regression with demographic controls, and source occupation, classification dummies
local occmob_ind "lne_c_mmo"
local socc_grp "locc1bfr_mmo"
noisily: display  ""
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (vii), panel A, DURDISTR"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg `occmob_ind' ${cyclicalvar}  ib2.educ2 i.sex i.race quarter i.`socc_grp' c.tage##c.tage##c.tage##c.tage quarter i.mm_clsfication_dum ${gl_weight}  ///
		if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength <=12 & n_spellength  >=1 $addcondition $noagric_apptemp $nuncond   ,vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
log off regresultslog_r1		
		

**************
** With Spell duration
**************


** regression without controls (only linear trend)
local occmob_ind "lne_c_mmo"
log on regresultslog_r1
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (v), panel B, DURDISTR"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} c.n_spellength quarter ${gl_weight} ///
			if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength<=12 & n_spellength>=1 $addcondition  $noagric_apptemp $nuncond & durdistr_stability==1,vce(cluster quarter)
			
** regression with demographic controls, and source occupation, classification dummies
local occmob_ind "lne_c_mmo"
local socc_grp "locc1bfr_mmo"
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (vii), panel B,DURDISTR"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} c.n_spellength  ib2.educ2 i.sex i.race quarter i.`socc_grp' c.tage##c.tage##c.tage##c.tage quarter i.mm_clsfication_dum ${gl_weight}  ///
		if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength <=12 & n_spellength  >=1 $addcondition $noagric_apptemp $nuncond  & durdistr_stability==1  ,vce(cluster quarter)
		

		
	
		
*************************************		
**** ROBUSTNESS, UNWEIGHTED, PANEL A 
*************************************

		
global gl_weight ""

** regression without controls (only linear trend)
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (v), panel A, DURDISTR, UNWGT"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} quarter ${gl_weight} ///
			if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength<=12 & n_spellength>=1 $addcondition  $noagric_apptemp $nuncond ,vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
** regression with demographic controls, and source occupation, classification dummies
local occmob_ind "lne_c_mmo"
local socc_grp "locc1bfr_mmo"
noisily: display  ""
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (vii), panel A, DURDISTR, UNWGT"
noisily: display  "-------------------------"
noisily: display  ""

noisily: reg `occmob_ind' ${cyclicalvar}  ib2.educ2 i.sex i.race quarter i.`socc_grp' c.tage##c.tage##c.tage##c.tage quarter i.mm_clsfication_dum ${gl_weight}  ///
		if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength <=12 & n_spellength  >=1 $addcondition $noagric_apptemp $nuncond   ,vce(cluster quarter)
noisily: display  ""
noisily: display  ""			
		

**************
** With Spell duration
**************

** regression without controls (only linear trend)
local occmob_ind "lne_c_mmo"
log on regresultslog_r1
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (v), panel B, DURDISTR, UNWGT"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} c.n_spellength quarter ${gl_weight} ///
			if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength<=12 & n_spellength>=1 $addcondition  $noagric_apptemp $nuncond & durdistr_stability==1,vce(cluster quarter)
			
** regression with demographic controls, and source occupation, classification dummies
local occmob_ind "lne_c_mmo"
local socc_grp "locc1bfr_mmo"
noisily: display  ""
noisily: display  "-------------------------"
noisily: display  "table 1, reg (vii), panel B,DURDISTR, UNWGT"
noisily: display  "-------------------------"
noisily: display  ""
noisily: reg `occmob_ind' ${cyclicalvar} c.n_spellength  ib2.educ2 i.sex i.race quarter i.`socc_grp' c.tage##c.tage##c.tage##c.tage quarter i.mm_clsfication_dum ${gl_weight}  ///
		if `occmob_ind'!=. $wavecond & sample_timetogo>$sttg & complete_nspell==1   & entry_ind==1 & n_spellength <=12 & n_spellength  >=1 $addcondition $noagric_apptemp $nuncond  & durdistr_stability==1  ,vce(cluster quarter)
log close regresultslog_r1
}		


********************************************************************************
	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"