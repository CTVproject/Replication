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
********************************************************************************

clear

***** PATHS 
cd "${workingdir}"			// !!! set in the global_paths.do, if necessary !!!
global workingdir "`c(pwd)'"			
do "${workingdir}/global_paths.do"

version 13
set more off
set varabbrev off


use  "${outputdata}/corewave_occlfmin_ctv.dta", clear
 






	

	//==========================
	// OTHER VARIABLES 
	//========================
	
				capture drop entry_ind
				gen byte entry_ind=0
				replace entry_ind=1 if max_educ==1 & tage>18
				replace entry_ind=1 if max_educ==2 & tage>18
				replace entry_ind=1 if max_educ==3 & tage>22
				replace entry_ind=1 if max_educ==4 & tage>22
				replace entry_ind=1 if max_educ==5 & tage>22
				


			capture drop ms_sum
			gen ms_sum=1 if ms<=2
			replace ms_sum=ms if ms>2

			
				** age_2dum
				
			capture drop age_2dum
			gen age_2dum=1 if tage>=20 & tage<=30
			replace age_2dum=2 if tage>=35 & tage<=55


//===========================================================
//  CYCLICAL U AND Y VARIABLES 
//===========================================================

// READ-IN DATA 
cap n drop outpw2 outpw 
capture drop merge_qtr_hired
merge m:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", gen(merge_qtr_hired)


 
 


capture drop durdistr_stability
gen durdistr_stability=.
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

// OVERALL WE SEE THAT DURDISTR AND SEAM EFFECTS CORRECTION APPEAR TO CLOSELY LINE UP! 


			
			
//==================================================
// TIME SERIES 
//=================================================

* seam indicator
capture drop seam_bfr_ind
capture drop seam_aft_ind

gen seam_bfr_ind=1 if personkey==personkey[_n+1] & wave==wave[_n+1]-1 & rot==rot[_n+1] & yearmonth==yearmonth[_n+1]-1
gen seam_aft_ind=1 if personkey==personkey[_n-1] & wave==wave[_n-1]+1 & rot==rot[_n-1] & yearmonth==yearmonth[_n-1]+1

table yearmonth panel if seam_aft_ind==1, c( freq)
table yearmonth panel if seam_bfr_ind==1, c( freq)

capture drop nofullseam_ind
gen nofullseam_ind=0
replace nofullseam_ind=1 if quarter==tq(1983q2) & panel==1984
replace nofullseam_ind=1 if quarter==tq(1983q3) & panel==1984
replace nofullseam_ind=1 if quarter==tq(1986q1) & panel==1984
replace nofullseam_ind=1 if quarter==tq(1986q2) & panel==1984

replace nofullseam_ind=1 if quarter==tq(1984q4) & panel==1985
replace nofullseam_ind=1 if quarter==tq(1985q1) & panel==1985
replace nofullseam_ind=1 if quarter==tq(1987q2) & panel==1985
replace nofullseam_ind=1 if quarter==tq(1987q3) & panel==1985

replace nofullseam_ind=1 if quarter==tq(1985q3) & panel==1986
replace nofullseam_ind=1 if quarter==tq(1985q4) & panel==1986
replace nofullseam_ind=1 if quarter==tq(1987q4) & panel==1986
replace nofullseam_ind=1 if quarter==tq(1988q1) & panel==1986

replace nofullseam_ind=1 if quarter==tq(1986q3) & panel==1987
replace nofullseam_ind=1 if quarter==tq(1986q4) & panel==1987
replace nofullseam_ind=1 if quarter==tq(1989q1) & panel==1987
replace nofullseam_ind=1 if quarter==tq(1989q2) & panel==1987

replace nofullseam_ind=1 if quarter==tq(1987q4) & panel==1988
replace nofullseam_ind=1 if quarter==tq(1988q1) & panel==1988
replace nofullseam_ind=1 if quarter==tq(1989q4) & panel==1988
replace nofullseam_ind=1 if quarter==tq(1990q1) & panel==1988

replace nofullseam_ind=1 if quarter==tq(1989q4) & panel==1990
replace nofullseam_ind=1 if quarter==tq(1990q1) & panel==1990
replace nofullseam_ind=1 if quarter==tq(1992q2) & panel==1990
replace nofullseam_ind=1 if quarter==tq(1992q3) & panel==1990

replace nofullseam_ind=1 if quarter==tq(1990q4) & panel==1991
replace nofullseam_ind=1 if quarter==tq(1991q1) & panel==1991
replace nofullseam_ind=1 if quarter==tq(1993q2) & panel==1991
replace nofullseam_ind=1 if quarter==tq(1993q3) & panel==1991

replace nofullseam_ind=1 if quarter==tq(1991q4) & panel==1992
replace nofullseam_ind=1 if quarter==tq(1992q1) & panel==1992
replace nofullseam_ind=1 if quarter==tq(1994q4) & panel==1992
replace nofullseam_ind=1 if quarter==tq(1995q1) & panel==1992

replace nofullseam_ind=1 if quarter==tq(1992q4) & panel==1993
replace nofullseam_ind=1 if quarter==tq(1993q1) & panel==1993
replace nofullseam_ind=1 if quarter==tq(1995q4) & panel==1993
replace nofullseam_ind=1 if quarter==tq(1996q1) & panel==1993

replace nofullseam_ind=1 if quarter==tq(1995q4) & panel==1996
replace nofullseam_ind=1 if quarter==tq(1996q1) & panel==1996
replace nofullseam_ind=1 if quarter==tq(1996q2) & panel==1996
replace nofullseam_ind=1 if quarter==tq(1999q4) & panel==1996
replace nofullseam_ind=1 if quarter==tq(2000q1) & panel==1996

replace nofullseam_ind=1 if quarter==tq(2000q4) & panel==2001
replace nofullseam_ind=1 if quarter==tq(2001q1) & panel==2001
replace nofullseam_ind=1 if quarter==tq(2001q2) & panel==2001 
replace nofullseam_ind=1 if quarter==tq(2003q4) & panel==2001
replace nofullseam_ind=1 if quarter==tq(2004q1) & panel==2001

replace nofullseam_ind=1 if quarter==tq(2003q4) & panel==2004
replace nofullseam_ind=1 if quarter==tq(2004q1) & panel==2004
replace nofullseam_ind=1 if quarter==tq(2006q2) & panel==2004
replace nofullseam_ind=1 if quarter==tq(2007q4) & panel==2004
replace nofullseam_ind=1 if quarter==tq(2008q1) & panel==2004

replace nofullseam_ind=1 if quarter==tq(2008q2) & panel==2008
replace nofullseam_ind=1 if quarter==tq(2008q3) & panel==2008
replace nofullseam_ind=1 if quarter==tq(2013q3) & panel==2008
replace nofullseam_ind=1 if quarter==tq(2013q4) & panel==2008

table yearmonth panel if nofullseam_ind!=1, c( freq)

table yearmonth panel if nofullseam_ind!=1 , c( mean en)


	 tab quarter en if nofullseam_ind!=1 & panel<=1990, row nof
	 tab quarter en if nofullseam_ind!=1 & panel<=1990 & sample_timetogo>=4, row nof
	table yearmonth panel if seam_aft_ind==1 & panel>=1990, c( freq)


capture drop mm_clsfication_dum
gen mm_clsfication_dum =0 if panel<=2001
replace mm_clsfication_dum=1 if panel>=2002

capture drop all_clsfication_dum
gen all_clsfication_dum=0 if panel<=1991
replace all_clsfication_dum=1 if panel<=2001 & panel>1991
replace all_clsfication_dum=2 if panel<=2008 & panel>2001


*** RECESSION INDICATORS
capture drop recession_yr
capture drop year
gen year=yofd(dofm(yearmonth))
gen recession_yr=0
replace recession_yr=1 if year==1990
replace recession_yr=1 if year==1991
replace recession_yr=1 if year==2001
replace recession_yr=1 if year==2008
replace recession_yr=1 if year==2009

**** GOOD TIMES BAD TIMES INDICATOR
su hp_lunrate, detail
sort quarter
capture drop hp_lunrate_temp
count if hp_lunrate==. & quarter!=quarter[_n-1]
gen hp_lunrate_temp=hp_lunrate if quarter!=quarter[_n-1]
_pctile hp_lunrate_temp, p(25 75)
local low_upct=r(r1)
local high_upct=r(r2)
capture drop rec_exp_ind
gen byte rec_exp_ind=0
replace rec_exp_ind=-1 if hp_lunrate>=`high_upct'
replace rec_exp_ind=1 if hp_lunrate<=`low_upct'

sort personkey yearmonth
capture drop hp_lunrate_temp

su hp_lunrate, detail
sort quarter
capture drop hp_lunrate_temp
count if hp_lunrate==. & quarter!=quarter[_n-1]
gen hp_lunrate_temp=hp_lunrate if quarter!=quarter[_n-1]
_pctile hp_lunrate_temp, p(33 67)
local low_upct=r(r1)
local high_upct=r(r2)
capture drop rec_exp_ind3
gen byte rec_exp_ind3=0
replace rec_exp_ind3=-1 if hp_lunrate>=`high_upct'
replace rec_exp_ind3=1 if hp_lunrate<=`low_upct'

sort personkey yearmonth
capture drop hp_lunrate_temp




*save, replace 





//====================================
// PROGRAMS
//====================================
capture program drop star_exe
program star_exe 
	args xlscol xlsrow table_pos
matrix rtable=r(table)
*matrix list rtable
*display "`table_pos'"
local pval=rtable[4,`table_pos']
local coeffx=rtable[1,`table_pos']
local stderrx=rtable[2, `table_pos']
*display "`pval'"
if `pval'<=0.1 & `pval'>0.05 {
local star="*" 
}	
if `pval'>0.01 & `pval'<=0.05 {
local star="**" 
}	
if `pval'<=0.01 {
local star="***" 
}	
if `pval'>0.1 {
local star="" 
}	
local coeffx=string(`coeffx', "%8.4f")
local coeffstar="`coeffx'`star'"
local stderr2=string(`stderrx', "%8.4f")
*display "`stderr2'"
local stderr3="(`stderr2')"
display "`coeffstar'"
display "`stderr3'"


putexcel `xlscol'`xlsrow'=("`coeffstar'")
local xlsrowplus=`xlsrow'+1
putexcel `xlscol'`xlsrowplus'=("`stderr3'")

end


												/*
											//=========================================
											// U_SPELLENGTH/N_SPELLENGTH REDEFINITION
											//=========================================


											*capture n drop n_spellength_old1
											*capture n drop u_spellength_old1

											ren n_spellength no_spellength
											ren u_spellength uo_spellength

											sort personkey yearmonth
											gen u_spellength=.
											replace u_spellength=1 if uo_spellength==1 & rwkesr1>=4 & rwkesr5[_n-1]>=4 & rwkesr4[_n-1]>=4 & rwkesr3[_n-1]>=4 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
											replace u_spellength=0 if uo_spellength==1 & ~(rwkesr1>=4 & rwkesr5[_n-1]>=4 & rwkesr4[_n-1]>=4 & rwkesr3[_n-1]>=4) & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
											replace u_spellength=0 if uo_spellength==0

											replace u_spellength=u_spellength[_n-1]+1 if uo_spellength==uo_spellength[_n-1]+1 & uo_spellength!=. & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1

											sort personkey yearmonth
											gen n_spellength=.
											replace n_spellength=1 if no_spellength==1 & rwkesr1>=4 & rwkesr5[_n-1]>=4 & rwkesr4[_n-1]>=4 & rwkesr3[_n-1]>=4 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
											replace n_spellength=0 if no_spellength==1 & ~(rwkesr1>=4 & rwkesr5[_n-1]>=4 & rwkesr4[_n-1]>=4 & rwkesr3[_n-1]>=4) & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
											replace n_spellength=0 if no_spellength==0

											replace n_spellength=n_spellength[_n-1]+1 if no_spellength==no_spellength[_n-1]+1 & no_spellength!=. & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1


											// type of spell, pure unemployment spell is 1
											capture drop ntype_spell
											gen byte ntype_spell=0 if lne==1 & complete_nspell==1 & complete_uspell!=1
											replace ntype_spell=1 if lne==1 & complete_nspell==1 & complete_uspell==1
												
											* second type: pure n vs mixed nun	
											capture drop ntype_spell2
											gen byte ntype_spell2=.
											replace ntype_spell2=0 if lne==1 & complete_nspell==1 & complete_nunspell!=1
											replace ntype_spell2=1 if lne==1 & complete_nspell==1 & complete_nunspell==1
											*/	
	
 	

//===========================================================
// WORKERS WITH PREVIOUS EMPLOYMENT
//===========================================================

capture drop earlier_empl
sort personkey yearmonth
gen earlier_empl=0
replace earlier_empl=1 if empl_ctv==1
replace earlier_empl=1 if earlier_empl[_n-1]==1 & personkey==personkey[_n-1]

global wavecond " & wave>4 & interview_no2>14 & sample_timetogo>1"
tab age_2dum earlier_empl if unempl_ctv==1 $wavecond
tab age_2dum earlier_empl if unempl_ctv==1 $wavecond & interview_no2>20, row nof
tab age_2dum earlier_empl if unempl_ctv==1 $wavecond & interview_no2>20 & continuous_spell==1, row nof	

//===================================================================
//   INCOMPLETE USPELL /NUNSPELLS
//====================================================================

sort personkey yearmonth
* RIGHT-CENSORED SPELLS
capture drop incomplete_uspell

capture drop incomplete_nspell
capture drop incomplete_nunspell
capture drop incomplete_u_in_nunspell

* LEFT-CENSORED SPELLS
capture drop incomplete_uspell_leftcens
capture drop incomplete_nspell_leftcens
capture drop incomplete_nunspell_leftcens
capture drop incomplete_u_in_nunspell_leftcens

** INCOMPLETE U SPELL
gen incomplete_uspell=1 if u_spellength==1 & complete_uspell!=1
replace incomplete_uspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_uspell!=1 & unempl_ctv==1 & incomplete_uspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
replace incomplete_uspell=0 if n_spellength==n_spellength[_n-1]+1 & complete_uspell!=1 & outlf_ctv==1 & incomplete_uspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_uspell=-1 if n_spellength==n_spellength[_n-1]+1 & complete_uspell!=1 & empl_ctv==1 & incomplete_uspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
											
** INCOMPLETE N SPELL											
gen incomplete_nspell=1 if n_spellength==1 & complete_nspell!=1
replace incomplete_nspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_nspell!=1 & (unempl_ctv==1|outlf_ctv==1) & incomplete_nspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_nspell=-1 if n_spellength==n_spellength[_n-1]+1 & complete_nspell!=1 & empl_ctv==1 & incomplete_nspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 

** INCOMPLETE NUN SPELL											
gen incomplete_nunspell=1 if u_spellength==1 & complete_nunspell!=1
replace incomplete_nunspell=0 if n_spellength==1 & complete_nunspell!=1 & outlf_ctv==1
replace incomplete_nunspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & unempl_ctv==1 & incomplete_nunspell[_n-1]>=0 & incomplete_nunspell[_n-1]<=1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
replace incomplete_nunspell=0 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell[_n-1]==0 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
replace incomplete_nunspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
											
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_nunspell=-1 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & empl_ctv==1 & incomplete_nunspell[_n-1]!=. & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
					
							
gsort personkey -yearmonth

replace incomplete_uspell=incomplete_uspell[_n-1] if incomplete_uspell==1 & incomplete_uspell[_n-1]<=0 & incomplete_uspell[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
replace incomplete_uspell=incomplete_uspell[_n-1] if incomplete_uspell==0 & incomplete_uspell[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
											
replace incomplete_nspell=incomplete_nspell[_n-1] if incomplete_nspell==1 & incomplete_nspell[_n-1]<=0 & incomplete_nspell[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
replace incomplete_nspell=incomplete_nspell[_n-1] if incomplete_nspell==0 & incomplete_nspell[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1

replace incomplete_nunspell=incomplete_nunspell[_n-1] if incomplete_nunspell>=0 & incomplete_nunspell<=1 & incomplete_nunspell[_n-1]<=1 & incomplete_nunspell[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
											
											
** NOW SET UP THE LEFT=CENSORING INCOMPLETE SPELLS (keep the inverse sorting, the reverse it)											
gen 	incomplete_uspell_leftcens=1 if lue_ctv==1 & complete_uspell!=1
replace incomplete_uspell_leftcens=1 if  complete_uspell!=1 & unempl_ctv==1 & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_uspell_leftcens=0 if  complete_uspell!=1 & outlf_ctv==1 & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_uspell_leftcens=-1 if  (complete_uspell[_n-1]!=1 & complete_uspell[_n-2]!=1) & empl==1 & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
*however, do to some cases of outlf==1 & empl_ctv==1  (those who lost a job e.g. at the end of the month, and did not search in the two weeks after), we have some completed 
* nunspells, that are not counted as completed uspells, even tho they are preceded by empl_ctv. A reason for not considering these spells is it is in fact unclear 
* when the worker starts searching, i.e. when is effective unemployment spell starts. (SEE THE ABOVE COMMENT) It is counted as completed nunspell.
replace incomplete_uspell_leftcens=0 if  complete_uspell!=1 & (empl_ctv==1& outlf==1) & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 


											
** INCOMPLETE N SPELL											
gen incomplete_nspell_leftcens=1 if lne_ctv==1 & complete_nspell!=1
replace incomplete_nspell_leftcens=1 if  complete_nspell!=1 & (unempl_ctv==1|outlf_ctv==1) & incomplete_nspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_nspell_leftcens=-1 if  complete_nspell[_n-1]!=1 & empl_ctv==1 & incomplete_nspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 

** INCOMPLETE NUN SPELL											
gsort personkey -yearmonth
capture drop incomplete_nunspell_leftcens
gen incomplete_nunspell_leftcens=1 if lne_ctv==1 & complete_nunspell!=1 & unempl_ctv==1
replace incomplete_nunspell_leftcens=0 if n_spellength==1 & complete_nunspell!=1 & outlf_ctv==1
replace incomplete_nunspell_leftcens=1 if  complete_nunspell!=1 & unempl_ctv==1 & incomplete_nunspell_leftcens[_n-1]>=0 & incomplete_nunspell_leftcens[_n-1]<=1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=0 if  complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell_leftcens[_n-1]==0 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=1 if  complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
											
* the following should not occur: -1
display "the following should not occur: -1"
gsort personkey -yearmonth
replace incomplete_nunspell_leftcens=-1 if  complete_nunspell[_n-1]!=1 & empl==1 & incomplete_nunspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=0 if  complete_nunspell!=1 & (empl_ctv==1 & outlf==1) & incomplete_nunspell_leftcens[_n-1]==0 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=-1 if  complete_nunspell!=1 & (empl_ctv==1 & outlf==1) & incomplete_nunspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
												& yearmonth==yearmonth[_n-1]-1 

											
											
sort personkey yearmonth 
replace incomplete_uspell_leftcens=incomplete_uspell_leftcens[_n-1] if incomplete_uspell_leftcens==1 & incomplete_uspell_leftcens[_n-1]<=0 & incomplete_uspell_leftcens[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1 
replace incomplete_uspell_leftcens=incomplete_uspell_leftcens[_n-1] if incomplete_uspell_leftcens==0 & incomplete_uspell_leftcens[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1 

replace incomplete_nspell_leftcens=incomplete_nspell_leftcens[_n-1] if incomplete_nspell_leftcens==1 & incomplete_nspell_leftcens[_n-1]<=0 & incomplete_nspell_leftcens[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
replace incomplete_nspell_leftcens=incomplete_nspell_leftcens[_n-1] if incomplete_nspell_leftcens==0 & incomplete_nspell_leftcens[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1 											

replace incomplete_nunspell_leftcens=incomplete_nunspell_leftcens[_n-1] if incomplete_nunspell_leftcens>=0 & incomplete_nunspell_leftcens<=1 & incomplete_nunspell_leftcens[_n-1]<=1 & incomplete_nunspell_leftcens[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
	

	
gsort personkey -yearmonth


capture drop incompl_ndur
capture drop incompl_udur
capture drop compl_udur
capture drop compl_ndur

gen incompl_ndur=n_spellength  if n_spellength>0 & n_spellength!=.
gen incompl_udur=u_spellength if u_spellength>0 & u_spellength!=.
gen compl_ndur=n_spellength  if n_spellength>0 & n_spellength!=. & complete_nspell==1
gen compl_udur=u_spellength if u_spellength>0 & u_spellength!=. & complete_uspell==1



replace incompl_ndur=incompl_ndur[_n-1] if n_spellength[_n-1]!=. & n_spellength==n_spellength[_n-1]-1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & n_spellength>=1
replace incompl_udur=incompl_udur[_n-1] if u_spellength[_n-1]!=. & u_spellength==u_spellength[_n-1]-1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & u_spellength>=1
replace compl_ndur=compl_ndur[_n-1] if n_spellength[_n-1]!=. & n_spellength==n_spellength[_n-1]-1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & n_spellength>=1
replace compl_udur=compl_udur[_n-1] if u_spellength[_n-1]!=. & u_spellength==u_spellength[_n-1]-1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & u_spellength>=1


sort personkey yearmonth
	
capture drop incomplete_pure_uspell
gen incomplete_pure_uspell=1 if incompl_ndur==incompl_udur & incompl_ndur!=. 


capture drop out_wave
capture drop out_durdistr_stability
capture drop out_interview_no2 
gen out_wave=.
gen byte out_durdistr_stability=.
gen out_interview_no2=.
forvalues i=1(1)24 {
capture drop f`i'_durdistr_stability
gen byte f`i'_durdistr_stability=.
replace out_wave=wave[_n+`i'] if n_spellength==1 & compl_ndur!=. & compl_ndur==`i' & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i'
replace out_durdistr_stability=durdistr_stability[_n+`i'] if n_spellength==1 & compl_ndur!=. & compl_ndur==`i' & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i'					
replace out_interview_no2=interview_no2[_n+`i'] if n_spellength==1 & compl_ndur!=. & compl_ndur==`i' & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i'					
replace f`i'_durdistr_stability=1 if n_spellength==1  & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & durdistr_stability[_n+`i']==1				
}

replace out_wave=out_wave[_n-1] if out_wave[_n-1]!=. $personbfr & n_spellength>1 & n_spellength<.
replace out_durdistr_stability=out_durdistr_stability[_n-1] if out_durdistr_stability[_n-1]!=. $personbfr & n_spellength>1 & n_spellength<.
replace out_interview_no2=out_interview_no2[_n-1] if out_interview_no2[_n-1]!=. $personbfr & n_spellength>1 & n_spellength<.

global out_wavecond " & out_interview_no2>14 & out_wave>4 "
global out_wavecond_lt " & out_interview_no2>20 & out_wave>5 "
global out_wavecond_lt2 " & out_interview_no2>24 & out_wave>6 "			
global out_wavecond_lt3 " & out_interview_no2>28 & out_wave>7 "			


											
*save, replace
sort personkey yearmonth
save  "${outputdata}/corewave_occlfmin_ctv.dta", replace


********************************************************************************
** TOWARDS COREWAVE_LF_WITH_U_CTV.DTA
********************************************************************************


cap n use "${outputdata}/corewave_occlfmin_ctv.dta"


//===== SAVE ONLY THOSE WITH  U/N-spells
//=== SAVE COMPLETE U/NUN SPELLS ONLY


sort personkey yearmonth
capture drop u_in_sample

bysort personkey: egen u_in_sample=max(unempl)

keep if u_in_sample==1



global personmaft " & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 "
global personmbfr " & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 "

capture drop f_empl_ctv
gen f_empl_ctv=empl_ctv[_n+1] if personkey==personkey[_n+1] ///
& yearmonth==yearmonth[_n+1]-1


sort personkey yearmonth
capture drop earlier_empl
gen earlier_empl=0
replace earlier_empl=1 if empl_ctv==1
replace earlier_empl=1 if earlier_empl[_n-1]==1 & personkey==personkey[_n-1]




** some extra variables, from jf_statistics_sipp_aug2018_part1_jf_jfdur.do

capture drop entry_ind_old
cap n ren entry_ind entry_ind_old
capture drop entry_ind
gen entry_ind=1 if educ<=2 & tage>=20
replace entry_ind=1 if educ>=3 & educ<=5 & tage>=24


capture drop u_in_n_spellength
gen u_in_n_spellength=1 if n_spellength==1 & unempl_ctv==1
replace u_in_n_spellength=0 if n_spellength==1 & unempl_ctv==0
forvalues i=1(1)24{
replace u_in_n_spellength=u_in_n_spellength[_n-1] if n_spellength>1 & unempl_ctv==0  $personmbfr & u_in_n_spellength[_n-1]!=. & n_spellength!=.
replace u_in_n_spellength=u_in_n_spellength[_n-1]+1 if n_spellength>1 & unempl_ctv==1  $personmbfr & u_in_n_spellength[_n-1]!=. & n_spellength!=.
}


** those who will be in the sample till the end
capture drop maxwave
capture drop maxwaveperson
capture drop untilendpanel_ind

sort panel personkey yearmonth
bysort panel: egen maxwave=max(wave)
bysort personkey: egen maxwaveperson=max(wave)
gen byte untilendpanel_ind=1 if maxwaveperson==maxwave
capture drop maxwave
capture drop maxwaveperson


global wavecond " & interview_no2>14 & wave>4 & sample_timetogo>=0"
global datevar "jl18"


  
  
  
//============================================================================== 
  //=======================================
  //   DURATION DISTRIbUTION, SURVIVAL 
  //=======================================
//==============================================================================  

* for the duration **distribution** we do not to keep the overall stocks of unemployed, relative the labour force, in mind
* however, we can do this analysis also with relative to the overall labour force (and we will...)






*** COMPLETE U, Ustar DURATIONS
		* Ustar = take nunspells, count only Uperiods for duration

* U_COUNTER_NUN does not take 0 spellength into account.
		
		
capture drop u_counter_nun
capture drop compl_udur
capture drop compl_nundur
capture drop compl_u_in_nundur
capture drop compl_u_in_nundur_prop
capture drop compl_ndur

sort personkey yearmonth
gen u_counter_nun=.
replace u_counter_nun=0 if outlf_ctv==1 & n_spellength==1
replace u_counter_nun=1 if unempl_ctv==1 & n_spellength==1
forvalues i=1(1)16 {
replace u_counter_nun=u_counter_nun[_n-1] if unempl_ctv!=1 & n_spellength==n_spellength[_n-1]+1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & n_spellength>1
replace u_counter_nun=u_counter_nun[_n-1]+1 if unempl_ctv==1 & n_spellength==n_spellength[_n-1]+1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & n_spellength>1
}


tab u_counter_nun if complete_nunspell==1 & complete_uspell!=1 & lue_ctv==1
tab u_counter_nun if complete_nunspell==1 & complete_uspell==1 & lue_ctv==1 
tab n_spellength if complete_nunspell==1 & complete_uspell!=1 & lue_ctv==1
tab u_spellength if complete_uspell==1 & lue_ctv==1


gen compl_udur=u_spellength if lue_ctv==1
gen compl_nundur=n_spellength if (lue_ctv==1 | lne_ctv==1) & u_counter_nun>0
gen compl_u_in_nundur=u_counter_nun if (lue_ctv==1 | lne_ctv==1) & u_counter_nun>0
gen compl_ndur=n_spellength if (lne_ctv==1 | lue_ctv==1)

forvalues i=1(1)60 {
replace compl_udur=compl_udur[_n+1] if unempl_ctv==1 & unempl_ctv[_n+1]==1 & n_spellength==n_spellength[_n+1]-1 & personkey==personkey[_n+1] ///
											& yearmonth==yearmonth[_n+1]-1 & n_spellength>=0 & compl_udur[_n+1]!=. & compl_udur==.
replace compl_nundur=compl_nundur[_n+1] if (unempl_ctv==1|outlf_ctv==1) & (unempl_ctv[_n+1]==1|outlf_ctv[_n+1]==1) & n_spellength==n_spellength[_n+1]-1 & personkey==personkey[_n+1] ///
											& yearmonth==yearmonth[_n+1]-1 & n_spellength>=0 & compl_nundur[_n+1]!=. & compl_nundur==.
replace compl_u_in_nundur=compl_u_in_nundur[_n+1] if (unempl_ctv==1|outlf_ctv==1) & (unempl_ctv[_n+1]==1|outlf_ctv[_n+1]==1) & n_spellength==n_spellength[_n+1]-1 & personkey==personkey[_n+1] ///
											& yearmonth==yearmonth[_n+1]-1 & n_spellength>=0 & compl_u_in_nundur[_n+1]!=. & compl_u_in_nundur==.
replace compl_ndur=compl_ndur[_n+1] if (unempl_ctv==1|outlf_ctv==1) & (unempl_ctv[_n+1]==1|outlf_ctv[_n+1]==1) & n_spellength==n_spellength[_n+1]-1 & personkey==personkey[_n+1] ///
											& yearmonth==yearmonth[_n+1]-1 & n_spellength>=0 & compl_ndur[_n+1]!=. & compl_ndur==.

											}
											
gen compl_u_in_nundur_prop=compl_u_in_nundur/compl_nundur
	 
tab compl_udur if u_spellength==1
fre compl_udur [aw=pweight2] if u_spellength==1 & compl_udur<=18 & complete_uspell==1 & sample_timetogo>14 & interview_no2>14




sort personkey yearmonth
* RIGHT-CENSORED SPELLS
capture drop incomplete_uspell

capture drop incomplete_nspell
capture drop incomplete_nunspell
capture drop incomplete_u_in_nunspell

* LEFT-CENSORED SPELLS
capture drop incomplete_uspell_leftcens
capture drop incomplete_nspell_leftcens
capture drop incomplete_nunspell_leftcens
capture drop incomplete_u_in_nunspell_leftcens

** INCOMPLETE U SPELL
gen incomplete_uspell=1 if u_spellength==1 & complete_uspell!=1
replace incomplete_uspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_uspell!=1 & unempl_ctv==1 & incomplete_uspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
replace incomplete_uspell=0 if n_spellength==n_spellength[_n-1]+1 & complete_uspell!=1 & outlf_ctv==1 & incomplete_uspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_uspell=-1 if n_spellength==n_spellength[_n-1]+1 & complete_uspell!=1 & empl_ctv==1 & incomplete_uspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
											
** INCOMPLETE N SPELL											
gen incomplete_nspell=1 if n_spellength==1 & complete_nspell!=1
replace incomplete_nspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_nspell!=1 & (unempl_ctv==1|outlf_ctv==1) & incomplete_nspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_nspell=-1 if n_spellength==n_spellength[_n-1]+1 & complete_nspell!=1 & empl_ctv==1 & incomplete_nspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 

** INCOMPLETE NUN SPELL											
gen incomplete_nunspell=1 if u_spellength==1 & complete_nunspell!=1
replace incomplete_nunspell=0 if n_spellength==1 & complete_nunspell!=1 & outlf_ctv==1
replace incomplete_nunspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & unempl_ctv==1 & incomplete_nunspell[_n-1]>=0 & incomplete_nunspell[_n-1]<=1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
replace incomplete_nunspell=0 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell[_n-1]==0 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
replace incomplete_nunspell=1 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
											
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_nunspell=-1 if n_spellength==n_spellength[_n-1]+1 & complete_nunspell!=1 & empl_ctv==1 & incomplete_nunspell[_n-1]!=. & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]+1 
					
							
gsort personkey -yearmonth

replace incomplete_uspell=incomplete_uspell[_n-1] if incomplete_uspell==1 & incomplete_uspell[_n-1]<=0 & incomplete_uspell[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
replace incomplete_uspell=incomplete_uspell[_n-1] if incomplete_uspell==0 & incomplete_uspell[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
											
replace incomplete_nspell=incomplete_nspell[_n-1] if incomplete_nspell==1 & incomplete_nspell[_n-1]<=0 & incomplete_nspell[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
replace incomplete_nspell=incomplete_nspell[_n-1] if incomplete_nspell==0 & incomplete_nspell[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1

replace incomplete_nunspell=incomplete_nunspell[_n-1] if incomplete_nunspell>=0 & incomplete_nunspell<=1 & incomplete_nunspell[_n-1]<=1 & incomplete_nunspell[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
											
											
** NOW SET UP THE LEFT=CENSORING INCOMPLETE SPELLS (the inverse sorting, the reverse it)											
gen 	incomplete_uspell_leftcens=1 if lue_ctv==1 & complete_uspell!=1
replace incomplete_uspell_leftcens=1 if  complete_uspell!=1 & unempl_ctv==1 & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_uspell_leftcens=0 if  complete_uspell!=1 & outlf_ctv==1 & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_uspell_leftcens=-1 if  (complete_uspell[_n-1]!=1 & complete_uspell[_n-2]!=1) & empl==1 & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
*however, do to some cases of outlf==1 & empl_ctv==1  (those who lost a job e.g. at the end of the month, and did not search in the two weeks after), we have some completed 
* nunspells, that are not counted as completed uspells, even tho they are preceded by empl_ctv. A reason for not considering these spells is it is in fact unclear 
* when the worker starts searching, i.e. when is effective unemployment spell starts. (SEE THE ABOVE COMMENT) It is counted as completed nunspell.
replace incomplete_uspell_leftcens=0 if  complete_uspell!=1 & (empl_ctv==1& outlf==1) & incomplete_uspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 


											
** INCOMPLETE N SPELL											
gen incomplete_nspell_leftcens=1 if lne_ctv==1 & complete_nspell!=1
replace incomplete_nspell_leftcens=1 if  complete_nspell!=1 & (unempl_ctv==1|outlf_ctv==1) & incomplete_nspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
* the following should not occur: -1
display "the following should not occur: -1"
replace incomplete_nspell_leftcens=-1 if  complete_nspell[_n-1]!=1 & empl_ctv==1 & incomplete_nspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 

** INCOMPLETE NUN SPELL											
gsort personkey -yearmonth
capture drop incomplete_nunspell_leftcens
gen incomplete_nunspell_leftcens=1 if lne_ctv==1 & complete_nunspell!=1 & unempl_ctv==1
replace incomplete_nunspell_leftcens=0 if n_spellength==1 & complete_nunspell!=1 & outlf_ctv==1
replace incomplete_nunspell_leftcens=1 if  complete_nunspell!=1 & unempl_ctv==1 & incomplete_nunspell_leftcens[_n-1]>=0 & incomplete_nunspell_leftcens[_n-1]<=1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=0 if  complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell_leftcens[_n-1]==0 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=1 if  complete_nunspell!=1 & outlf_ctv==1 & incomplete_nunspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
											
* the following should not occur: -1
display "the following should not occur: -1"
gsort personkey -yearmonth
replace incomplete_nunspell_leftcens=-1 if  complete_nunspell[_n-1]!=1 & empl==1 & incomplete_nunspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=0 if  complete_nunspell!=1 & (empl_ctv==1 & outlf==1) & incomplete_nunspell_leftcens[_n-1]==0 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n-1]-1 
replace incomplete_nunspell_leftcens=-1 if  complete_nunspell!=1 & (empl_ctv==1 & outlf==1) & incomplete_nunspell_leftcens[_n-1]==1 & personkey==personkey[_n-1] ///
												& yearmonth==yearmonth[_n-1]-1 

											
											
sort personkey yearmonth 

replace incomplete_uspell_leftcens=incomplete_uspell_leftcens[_n-1] if incomplete_uspell_leftcens==1 & incomplete_uspell_leftcens[_n-1]<=0 & incomplete_uspell_leftcens[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1 
replace incomplete_uspell_leftcens=incomplete_uspell_leftcens[_n-1] if incomplete_uspell_leftcens==0 & incomplete_uspell_leftcens[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1 

replace incomplete_nspell_leftcens=incomplete_nspell_leftcens[_n-1] if incomplete_nspell_leftcens==1 & incomplete_nspell_leftcens[_n-1]<=0 & incomplete_nspell_leftcens[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
replace incomplete_nspell_leftcens=incomplete_nspell_leftcens[_n-1] if incomplete_nspell_leftcens==0 & incomplete_nspell_leftcens[_n-1]==-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1 											

replace incomplete_nunspell_leftcens=incomplete_nunspell_leftcens[_n-1] if incomplete_nunspell_leftcens>=0 & incomplete_nunspell_leftcens<=1 & incomplete_nunspell_leftcens[_n-1]<=1 & incomplete_nunspell_leftcens[_n-1]>=-1 & personkey==personkey[_n-1] ///
											& yearmonth==yearmonth[_n+1]-1
	

	
gsort personkey -yearmonth


capture drop incompl_ndur
capture drop incompl_udur

gen incompl_ndur=n_spellength  if n_spellength>0 & n_spellength!=.
gen incompl_udur=u_spellength if u_spellength>0 & u_spellength!=.
replace incompl_ndur=incompl_ndur[_n-1] if n_spellength[_n-1]!=. & n_spellength==n_spellength[_n-1]-1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & n_spellength>=1
replace incompl_udur=incompl_udur[_n-1] if u_spellength[_n-1]!=. & u_spellength==u_spellength[_n-1]-1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & u_spellength>=1


sort personkey yearmonth
	
capture drop incomplete_pure_uspell
gen incomplete_pure_uspell=1 if incompl_ndur==incompl_udur & incompl_ndur!=. 
											


//==============================================================================
// INCOMPLETE DURATION DISTRIBUTION (OVER THE CYCLE POSSIBLY)
//==============================================================================


//==============================================================================
***** UNEMPLOYMENT SURVIVAL
//==============================================================================



tab n_spellength if incomplete_nunspell==1,m 


/* MAX_XLENGTH = THE LENGTH OF THE INCOMPLETE SPELL */


gsort personkey -yearmonth
capture drop max_nlength
capture drop max_ulength
capture drop max_nunlength
capture drop max_u_in_nlength

gen max_ulength=u_spellength if (complete_uspell==1 | incomplete_uspell==1) & n_spellength!=. & n_spellength>0
replace max_ulength=max_ulength[_n-1] if max_ulength[_n-1]!=. & n_spellength==n_spellength[_n-1]-1 & personkey==personkey[_n-1] & (complete_uspell==1 | incomplete_uspell==1)

gen max_nlength=n_spellength if (complete_nspell==1 | incomplete_nspell==1) & n_spellength!=. & n_spellength>0
replace max_nlength=max_nlength[_n-1] if max_nlength[_n-1]!=. & n_spellength==n_spellength[_n-1]-1 & personkey==personkey[_n-1] & (complete_nspell==1 | incomplete_nspell==1)

gen max_nunlength=n_spellength if (complete_nunspell==1 | incomplete_nunspell==1) & n_spellength!=. & n_spellength>0
replace max_nunlength=max_nunlength[_n-1] if max_nunlength[_n-1]!=. & n_spellength==n_spellength[_n-1]-1 & personkey==personkey[_n-1] & (complete_nunspell==1 | incomplete_nunspell==1)

gen max_u_in_nlength=u_counter_nun if (complete_nunspell==1 | incomplete_nunspell==1) & n_spellength!=. & n_spellength>0
replace max_u_in_nlength=max_u_in_nlength[_n-1] if max_u_in_nlength[_n-1]!=. & n_spellength==n_spellength[_n-1]-1 & personkey==personkey[_n-1] & (complete_nunspell==1 | incomplete_nunspell==1)
sort personkey yearmonth


sort personkey yearmonth


forvalues i=1(1)18 {
capture drop usurv_ind_`i'm
capture drop nsurv_ind_`i'm
capture drop u_in_nunsurv_ind_`i'm
capture drop nunsurv_ind_`i'm


gen byte usurv_ind_`i'm=1 if u_spellength==1 & max_ulength>=`i' & max_ulength!=.
gen byte nsurv_ind_`i'm=1 if n_spellength==1 & max_nlength>=`i' & max_nlength!=.
gen byte nunsurv_ind_`i'm=1 if n_spellength==1 & max_nunlength>=`i' & max_nunlength!=.
gen byte u_in_nunsurv_ind_`i'm=1 if n_spellength==1 & max_u_in_nlength>=`i' & max_u_in_nlength!=.

replace usurv_ind_`i'm=0 if u_spellength==1 & max_ulength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
replace nsurv_ind_`i'm=0 if n_spellength==1 & max_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
replace nunsurv_ind_`i'm=0 if n_spellength==1 & max_nunlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
replace u_in_nunsurv_ind_`i'm=0 if n_spellength==1 & max_u_in_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)

}




*** outflow timed restrictions

global personmaft " & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 "
global personmbfr " & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 "
sort personkey yearmonth

capture drop out_wave
capture drop out_durdistr_stability
capture drop out_interview_no2 
gen out_wave=.
gen byte out_durdistr_stability=.
gen out_interview_no2=.
forvalues i=1(1)24 {
capture drop f`i'_durdistr_stability
gen byte f`i'_durdistr_stability=.
replace out_wave=wave[_n+`i'] if n_spellength==1 & compl_ndur!=. & compl_ndur==`i' & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i'
replace out_durdistr_stability=durdistr_stability[_n+`i'] if n_spellength==1 & compl_ndur!=. & compl_ndur==`i' & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i'					
replace out_interview_no2=interview_no2[_n+`i'] if n_spellength==1 & compl_ndur!=. & compl_ndur==`i' & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i'					
replace f`i'_durdistr_stability=1 if n_spellength==1  & personkey==personkey[_n+`i'] & yearmonth==yearmonth[_n+`i']-`i' & durdistr_stability[_n+`i']==1				
}

replace out_wave=out_wave[_n-1] if out_wave[_n-1]!=. $personbfr & n_spellength>1 & n_spellength<.
replace out_durdistr_stability=out_durdistr_stability[_n-1] if out_durdistr_stability[_n-1]!=. $personbfr & n_spellength>1 & n_spellength<.
replace out_interview_no2=out_interview_no2[_n-1] if out_interview_no2[_n-1]!=. $personbfr & n_spellength>1 & n_spellength<.

global out_wavecond " & out_interview_no2>14 & out_wave>4 "
global out_wavecond_lt " & out_interview_no2>20 & out_wave>5 "
global out_wavecond_lt2 " & out_interview_no2>24 & out_wave>6 "			
global out_wavecond_lt3 " & out_interview_no2>28 & out_wave>7 "			


*** inflow timed restrictions 

sort personkey yearmonth
capture drop in_sample_timetogo
gen in_sample_timetogo=sample_timetogo if n_spellength==1
replace in_sample_timetogo=in_sample_timetogo[_n-1] if n_spellength==n_spellength[_n-1]+1 & n_spellength[_n-1]<. & n_spellength>1 $personmbfr



save "${outputdata}/corewave_lf_with_u_ctv.dta", replace 

