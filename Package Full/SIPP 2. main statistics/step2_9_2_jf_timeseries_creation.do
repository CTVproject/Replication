

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







/* **************************************************************************

	CYCLICALITY OF THE JOB FINDING RATE 
	
**************************************************************************** */



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

	
cap n use "${outputdata}/corewave_lf_with_u_ctv.dta", clear

/*

Summary of some findings of the series below:

Job finding rate out of nonemployment is much less cyclical; 
however, if we look at those in nunspells, we observe equal relative cyclicality while smaller absolute (semi-elasticity) cyclicality. 
Standard deviations of the smoothed series “cnune” measure are marginally smaller than “cue”
The jfn series (which covers everybody current in nonemployment) is much less cyclical than the job finding rate, 
and also displays a downward (demographic?) trend. 
However, looking at cnune (completed nunspells) sees many of the same patterns as completed u-spells and job finding rate from unemployment. 



*/



//====================================================
//  PRELIMINARIES
//====================================================


** those who will be in the sample till the end


capture drop maxwave
capture drop maxwaveperson
capture drop untilendpanel_ind

sort panel personkey yearmonth
bysort panel: egen maxwave=max(wave)
sort personkey yearmonth
bysort personkey: egen maxwaveperson=max(wave)
gen byte untilendpanel_ind=1 if maxwaveperson==maxwave
capture drop maxwave
capture drop maxwaveperson



***** AUXILIARY PROGRAM


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
capture program drop wsummarize
	program wsummarize, byable(recall) 
		version 8.2  
		syntax [varlist] [if] [in] [, Format(str) listwise ]
		
		if "`listwise'" != "" local listwise "novarlist" 
		marksample touse, strok `listwise'  
		qui count if `touse' 
		if r(N) == 0 error 2000 

		// variable name width  
		local w = 0 
		foreach v of local varlist { 
			cap confirm string variable `v' 
			if _rc { 
				local w = max(`w', length("`v'")) 
				local vlist `vlist' `v' 
			}	
		}	
		local ++w

		if "`format'" == "" local format %6.0f %9.3f %9.3f %9.3f %9.3f 
		else {
			if _caller() >= 9 { 
				foreach f in `format' { 
					confirm format `f' 
				}
			}	
			
			local nf : word count `format' 
			if `nf' < 5 { 
				local flast : word `nf' of `format' 
				forval j = `= `nf' + 1'/5 { 
					local format `format' `flast' 
				}
			}
		}
		
		tokenize "`format'" 
		local j = 1
		foreach f in `format' { 
			local s = 1 + real(substr("`f'", index("`f'", "%") + 1, 1))
			local s`j++' "%`s's"
		}

		di as txt _n "{space `w'} " `s1' "Obs" `s2' "Mean" ///
			`s3' "SD" `s4' "Min" `s5' "Max" 
		
		foreach v of local vlist { 
			qui su `v' if `touse' 
			di as txt "`v'{col `w'}  "    ///
			" " as res `1' r(N)           /// 
			" " as res `2' r(mean)        /// 
			" " as res `3' r(sd)          /// 
			" " as res `4' r(min)         /// 
			" " as res `5' r(max)          
		}
	end 


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



// PRELIMINARIES

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





capture drop entry_ind_old
cap n ren entry_ind entry_ind_old
capture drop entry_ind
gen entry_ind=1 if educ<=2 & tage>=20
replace entry_ind=1 if educ>=3 & educ<=5 & tage>=24

sort personkey yearmonth 
capture drop u_in_n_spellength
gen u_in_n_spellength=1 if n_spellength==1 & unempl_ctv==1
replace u_in_n_spellength=0 if n_spellength==1 & unempl_ctv==0
forvalues i=1(1)24{
replace u_in_n_spellength=u_in_n_spellength[_n-1] if n_spellength>1 & unempl_ctv==0  $personmbfr & u_in_n_spellength[_n-1]!=. & n_spellength!=.
replace u_in_n_spellength=u_in_n_spellength[_n-1]+1 if n_spellength>1 & unempl_ctv==1  $personmbfr & u_in_n_spellength[_n-1]!=. & n_spellength!=.
}



**IMPORT BLS EU, EN rates
merge m:1 quarter using "${workingdir}/Aggregate Data/jf_sepu_bls.dta"

capture drop mm_clsfication_dum
gen mm_clsfication_dum =0 if panel<=2001
replace mm_clsfication_dum=1 if panel>=2002

capture drop all_clsfication_dum
gen all_clsfication_dum=0 if panel<=1991
replace all_clsfication_dum=1 if panel<=2001 & panel>1991
replace all_clsfication_dum=2 if panel<=2008 & panel>2001

capture drop educ2
gen educ2=educ
replace educ2=4 if educ==5


// AGE  (with wavecondition)
capture drop age_2dum
gen age_2dum=1 if tage>=20 & tage<=30
replace age_2dum=2 if tage>=35 & tage<=55


capture drop entry_ind
			gen byte entry_ind=0
			replace entry_ind=1 if max_educ==1 & tage>18
			replace entry_ind=1 if max_educ==2 & tage>18
			replace entry_ind=1 if max_educ==3 & tage>22
			replace entry_ind=1 if max_educ==4 & tage>22
			replace entry_ind=1 if max_educ==5 & tage>24
			

capture drop entry_ind_old
ren entry_ind entry_ind_old
capture drop entry_ind
gen entry_ind=1 if educ<=2 & tage>=20
replace entry_ind=1 if educ>=3 & educ<=5 & tage>=24

*** RECESSION INDICATORS
capture drop recession_yr
gen recession_yr=0
replace recession_yr=1 if year==1990
replace recession_yr=1 if year==1991
replace recession_yr=1 if year==2001
replace recession_yr=1 if year==2008
replace recession_yr=1 if year==2009



label define label_mm  ///
 11 "Mgt occs" ///
 13 "Bus&Fin Operations"  ///
 15 "Computer and math. occ"  ///
 17 "architect & eng. occ"  ///
 19 "Life, phys, and socsci occ"  ///
 21 "Comm & soc service occ"  ///
 23 "Legal"  ///
 25 "Educ, training, and library"  ///
 27 "Arts/Dsgn/entrtmnt/sports/media"  ///
 29 "Healthcare pract & tech occs"  ///
 31 "Healthcare support"  ///
 33 "Protective service"  ///
 35 "Food prep/serving & rel."  ///
 37 "Building/grounds clean&maint."  ///
 39 "Personal care/service occ"   ///
 41 "Sales & rel. occupations"  ///
 43 "Office/Admin Support"  ///
 45 "Farm/Fish/Forestry"  ///
 47 "Construction/Extraction" ///
 49 "Install/Maint/Repair Occ" ///
 51 "Production occupations" ///
 53 "Transportation& mat moving", replace

 lab val locc1bfr_mmo label_mm		
 lab val locc1aft_mmo label_mm			
 
capture drop ms_sum
gen byte ms_sum=1 if ms<=2
replace ms_sum=ms if ms>2
capture drop ms2
gen byte ms2=1 if ms<=2
replace ms2=ms if ms>2


capture drop agebin
gen byte agebin=1 if tage>=20 & tage<=30
replace agebin=2 if tage>=35 & tage<=55



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

	**    capture drop firstlastwave
     capture drop firstlastwave
	 gen firstlastwave=0
     capture replace firstlastwave=1 if wave==1|wave==2
     capture replace firstlastwave=1 if wave>=8 & panel==1984             ! 8 waves
     capture replace firstlastwave=1 if wave>=7 & panel==1985             ! 7 waves
     capture replace firstlastwave=1 if wave>=7 & panel==1986             ! 7 waves
     capture replace firstlastwave=1 if wave>=7 & panel==1987             ! 7 waves
     capture replace firstlastwave=1 if wave>=6 & panel==1988             ! 6 waves
     capture replace firstlastwave=1 if wave>=8 & panel==1990             ! 8 waves
     capture replace firstlastwave=1 if wave>=8 & panel==1991             ! 8 waves
     capture replace firstlastwave=1 if wave>=9 & panel==1992             ! 9 waves
     capture replace firstlastwave=1 if wave>=9 & panel==1993             ! 9 waves
     capture replace firstlastwave=1 if wave>=12 & panel==1996            ! 12 waves
     capture replace firstlastwave=1 if wave>=9 & panel==2001             ! 9 waves
     capture replace firstlastwave=1 if wave>=12 & panel==2004            ! 12 waves
     capture replace firstlastwave=1 if wave>=16 & panel==2008             ! 16 waves
	 
	** seams at every month, given firstlastwave!=1
	sort personkey yearmonth
	capture drop seam_interior_flw_ind
	gen seam_interior_flw_ind=0
	replace seam_interior_flw_ind=1 if wave[_n-1]==wave[_n] - 1 & firstlastwave!=1


	sort personkey yearmonth
	capture drop seam_interior_ind
	gen seam_interior_ind=0
	replace seam_interior_ind=1 if wave[_n-1]==wave[_n] - 1 
	
	* BASED ON TAB, WE CAN SEE WHICH QUARTERS HAVE CONSISTENTLY ALL MONTHS A SEAM
	tab yearmonth seam_interior_ind
	
	* use this to indicate those quarters that work
	capture program drop seam_interior_exe
	program define seam_interior_exe
			
			capture drop seam_interior_all_q_ind
			gen seam_interior_all_q_ind=0
			replace seam_interior_all_q_ind=1 if quarter>=tq(1983q4) & quarter<=tq(2013q1)
			replace seam_interior_all_q_ind=0 if quarter>=tq(1988q1) & quarter<=tq(1988q1)
			replace seam_interior_all_q_ind=0 if quarter>=tq(1989q4) & quarter<=tq(1990q1)
			replace seam_interior_all_q_ind=0 if quarter>=tq(1995q4) & quarter<=tq(1996q1)
			replace seam_interior_all_q_ind=0 if quarter>=tq(1999q4) & quarter<=tq(2000q1)
			replace seam_interior_all_q_ind=0 if quarter>=tq(2003q4) & quarter<=tq(2004q1)
			replace seam_interior_all_q_ind=0 if quarter>=tq(2007q4) & quarter<=tq(2008q3)
			
			* impose that percentage of obs at seam should be about 0.25 in obs and in weight
			* concretely, the rules are: if two subsequent observations within the quarter are below 20, exclude
			* if one observation in a quarter is below 17, exclude!
			capture drop seam_interior_all_q_strict_ind
			gen seam_interior_all_q_strict_ind=seam_interior_all_q_ind
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1984q4) & quarter<=tq(1985q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1984q4) & quarter<=tq(1985q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1986q1) & quarter<=tq(1986q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1986q4) & quarter<=tq(1987q2)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1987q4) & quarter<=tq(1988q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1989q1) & quarter<=tq(1990q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1990q4) & quarter<=tq(1991q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1991q4) & quarter<=tq(1992q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1992q4) & quarter<=tq(1992q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1994q4) & quarter<=tq(1994q4)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1995q4) & quarter<=tq(1996q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(1999q4) & quarter<=tq(2000q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(2003q4) & quarter<=tq(2004q1)
			replace seam_interior_all_q_strict_ind=0 if quarter>=tq(2007q4) & quarter<=tq(2008q3)
			
			
							
				capture drop nofullseam_ind2
				gen nofullseam_ind2=0
				replace nofullseam_ind2=1 if quarter==tq(1983q2) 
				replace nofullseam_ind2=1 if quarter==tq(1983q3) 
				replace nofullseam_ind2=1 if quarter==tq(1986q1) 
				replace nofullseam_ind2=1 if quarter==tq(1986q2) 

				replace nofullseam_ind2=1 if quarter==tq(1984q4) 
				replace nofullseam_ind2=1 if quarter==tq(1985q1) 
				replace nofullseam_ind2=1 if quarter==tq(1987q2) 
				replace nofullseam_ind2=1 if quarter==tq(1987q3) 

				replace nofullseam_ind2=1 if quarter==tq(1985q3) 
				replace nofullseam_ind2=1 if quarter==tq(1985q4) 
				replace nofullseam_ind2=1 if quarter==tq(1987q4) 
				replace nofullseam_ind2=1 if quarter==tq(1988q1) 

				replace nofullseam_ind2=1 if quarter==tq(1986q3) 
				replace nofullseam_ind2=1 if quarter==tq(1986q4) 
				replace nofullseam_ind2=1 if quarter==tq(1989q1) 
				replace nofullseam_ind2=1 if quarter==tq(1989q2) 

				replace nofullseam_ind2=1 if quarter==tq(1987q4) 
				replace nofullseam_ind2=1 if quarter==tq(1988q1) 
				replace nofullseam_ind2=1 if quarter==tq(1989q4) 
				replace nofullseam_ind2=1 if quarter==tq(1990q1) 

				replace nofullseam_ind2=1 if quarter==tq(1989q4) 
				replace nofullseam_ind2=1 if quarter==tq(1990q1) 
				replace nofullseam_ind2=1 if quarter==tq(1992q2) 
				replace nofullseam_ind2=1 if quarter==tq(1992q3) 

				replace nofullseam_ind2=1 if quarter==tq(1990q4) 
				replace nofullseam_ind2=1 if quarter==tq(1991q1) 
				replace nofullseam_ind2=1 if quarter==tq(1993q2) 
				replace nofullseam_ind2=1 if quarter==tq(1993q3) 

				replace nofullseam_ind2=1 if quarter==tq(1991q4) 
				replace nofullseam_ind2=1 if quarter==tq(1992q1) 
				replace nofullseam_ind2=1 if quarter==tq(1994q4) 
				replace nofullseam_ind2=1 if quarter==tq(1995q1) 

				replace nofullseam_ind2=1 if quarter==tq(1992q4) 
				replace nofullseam_ind2=1 if quarter==tq(1993q1) 
				replace nofullseam_ind2=1 if quarter==tq(1995q4) 
				replace nofullseam_ind2=1 if quarter==tq(1996q1) 

				replace nofullseam_ind2=1 if quarter==tq(1995q4) 
				replace nofullseam_ind2=1 if quarter==tq(1996q1) 
				replace nofullseam_ind2=1 if quarter==tq(1996q2) 
				replace nofullseam_ind2=1 if quarter==tq(1999q4) 
				replace nofullseam_ind2=1 if quarter==tq(2000q1) 

				replace nofullseam_ind2=1 if quarter==tq(2000q4) 
				replace nofullseam_ind2=1 if quarter==tq(2001q1) 
				replace nofullseam_ind2=1 if quarter==tq(2001q2) 
				replace nofullseam_ind2=1 if quarter==tq(2003q4) 
				replace nofullseam_ind2=1 if quarter==tq(2004q1) 

				replace nofullseam_ind2=1 if quarter==tq(2003q4) 
				replace nofullseam_ind2=1 if quarter==tq(2004q1) 
				replace nofullseam_ind2=1 if quarter==tq(2006q2) 
				replace nofullseam_ind2=1 if quarter==tq(2007q4) 
				replace nofullseam_ind2=1 if quarter==tq(2008q1) 

				replace nofullseam_ind2=1 if quarter==tq(2008q2) 
				replace nofullseam_ind2=1 if quarter==tq(2008q3) 
				replace nofullseam_ind2=1 if quarter==tq(2013q3) 
				replace nofullseam_ind2=1 if quarter==tq(2013q4) 

	end 
	
	seam_interior_exe
			
			tab yearmonth seam_interior_ind [aw=pweight2], row

			
sort personkey yearmonth 

capture drop interview_no22
gen interview_no22=1 if personkey!=personkey[_n-1] 
replace interview_no22=1 if personkey==personkey[_n-1] & yearmonth>yearmonth[_n-1]+1
replace interview_no22=interview_no22[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1

//=======================================
//  JOB FINDING
//=======================================

/* We will have 9 measures of the job finding rate for the unemployed:
			1-3) The standard job finding rate (just looking at those who are in unemployment, and seeing if they move to e or stay unemployed (ALL/YNG/PRM)
			4-6) The ctv unemployment rate (14 months in sample)
			7-9) The ctv unemployment rate with 18 months in sample
			
			We also have 9 measures of the job finding rate for the nun-ners
			
			In this section, we calculate the relevant job finding rate, then
				
				- log it
				- consider only those quarters with more than 100 obs
				- log that series
				- calculate the "instrumented" series, using the bls jf, to fill missing observations, and get rid of noise
				- log that series too
				- calculate the 5q series for those series with gaps
				- save all these series 
							in "C:\data\ts_`jfname'_before_tramo.xlsx"
				- save only `jfname'_raw  l`jfname'_raw ts_`jfname'_q ts_l`jfname'_q q3_`jfname'_q q3_l`jfname'_q series
							in "C:\data\ts_`jfname'_for_tramo.xlsx"
			
			
			*/




//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
// PROGRAM THAT GETS THE JF SERIES READY FOR TRAMO
//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


	  capture program drop jf_constr_exe
	  program define jf_constr_exe
					args jfname
			rename table1 `jfname'_raw
			rename table2 `jfname'_wobs
			rename table3 jf_bls
			tsset quarter
			tsfill

			// minimum no. obs
			capture drop ts_`jfname'_q
			gen ts_`jfname'_q=`jfname'_raw if `jfname'_wobs>100

			// LOGGED 
			capture drop ts_l`jfname'_q
			gen ts_l`jfname'_q=log(ts_`jfname'_q)

			// JF STANDARD-ALL SMOOTHED
			seam_interior_exe
			capture drop q3_`jfname'_q
			capture drop q3_l`jfname'_q

			tssmooth ma q3_`jfname'_q = ts_`jfname'_q if seam_interior_all_q_ind[_n-2]==1 & ///
													seam_interior_all_q_ind[_n-1]==1 & seam_interior_all_q_ind==1 & seam_interior_all_q_ind[_n+1]==1 & ///
													seam_interior_all_q_ind[_n+2]==1, window(2 1 2)
			tssmooth ma q3_l`jfname'_q = ts_l`jfname'_q if seam_interior_all_q_ind[_n-2]==1 & ///
													seam_interior_all_q_ind[_n-1]==1 & seam_interior_all_q_ind==1 & seam_interior_all_q_ind[_n+1]==1 & ///
													seam_interior_all_q_ind[_n+2]==1, window(2 1 2)

	 end
	 
		 
		 capture program drop jf_constr_instr_exe
						  program define jf_constr_instr_exe
										args jfname
								rename table1 `jfname'_raw
								rename table2 `jfname'_wobs
								*rename table3 jf_bls
								tsset quarter
								tsfill
								sort quarter


								**IMPORT BLS EU, EN rates
								capture drop _mergetemp1
								merge m:1 quarter using "${workingdir}/Aggregate Data/jf_sepu_bls.dta", gen(_mergetemp1)
								
								capture drop l2jf_bls
								gen l2jf_bls=log(jf_bls)
								
								seam_interior_exe
								
								// logged
								capture drop l`jfname'_raw
								gen l`jfname'_raw=log(`jfname'_raw) 

								
								
								// minimum no. obs
								capture drop ts_`jfname'_q
								gen ts_`jfname'_q=`jfname'_raw if `jfname'_wobs>100

								// LOGGED 
								capture drop ts_l`jfname'_q
								gen ts_l`jfname'_q=log(ts_`jfname'_q)
								
								// instrumented
								capture drop ts_`jfname'_instr_q
								reg `jfname'_raw jf_bls if `jfname'_raw!=. & `jfname'_wobs>100 & nofullseam_ind2!=1 
								predict ts_`jfname'_instr_q
								
								// instrumented, LOGGED 
								capture drop ts_l`jfname'_instr_q
								reg l`jfname'_raw l2jf_bls if `jfname'_raw!=. & `jfname'_wobs>100 & nofullseam_ind2!=1 
								predict ts_l`jfname'_instr_q
								

								// JF STANDARD-ALL SMOOTHED
								capture drop q3_`jfname'_q
								capture drop q3_l`jfname'_q

								tssmooth ma q3_`jfname'_q = `jfname'_raw if seam_interior_all_q_ind[_n-2]==1 & ///
																		seam_interior_all_q_ind[_n-1]==1 & seam_interior_all_q_ind==1 & seam_interior_all_q_ind[_n+1]==1 & ///
																		seam_interior_all_q_ind[_n+2]==1 & `jfname'_raw!=. & `jfname'_raw[_n-1]!=. & `jfname'_raw[_n-2]!=. /// 
																		& `jfname'_raw[_n+1]!=. & `jfname'_raw[_n+2]!=.  , window(2 1 2)
								tssmooth ma q3_l`jfname'_q = l`jfname'_raw if seam_interior_all_q_ind[_n-2]==1 & ///
																		seam_interior_all_q_ind[_n-1]==1 & seam_interior_all_q_ind==1 & seam_interior_all_q_ind[_n+1]==1 & ///
																		seam_interior_all_q_ind[_n+2]==1 & `jfname'_raw!=. & `jfname'_raw[_n-1]!=. & `jfname'_raw[_n-2]!=. /// 
																		& `jfname'_raw[_n+1]!=. & `jfname'_raw[_n+2]!=.  , window(2 1 2)
								
							
								
								capture drop q3_`jfname'_instr_q
								capture drop q3_l`jfname'_instr_q

								tssmooth ma q3_`jfname'_instr_q = ts_`jfname'_instr_q if quarter>=tq(1990q1), window(2 1 2)
								tssmooth ma q3_l`jfname'_instr_q = ts_l`jfname'_instr_q if quarter>=tq(1990q1), window(2 1 2)

					 end
					
					capture program drop jf_keepsave_exe
					program define jf_keepsave_exe
										args jfname
								
								keep quarter `jfname'_raw  l`jfname'_raw ts_`jfname'_q ts_l`jfname'_q q3_`jfname'_q q3_l`jfname'_q
								export excel using "${tempdata}/ts_`jfname'_for_tramo.xlsx", sheet("jf") firstrow(variables) nolabel replace 
								save "${tempdata}/ts_`jfname'_for_tramo.dta",   replace

					 end
		
	// JF STANDARD-ALL
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if unempl_ctv==1 & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
		  & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
		  , c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jf 
		 
	capture drop q3_jf_bls_q
	tssmooth ma q3_jf_bls_q = jf_bls if seam_interior_all_q_ind[_n-2]==1 & ///
											seam_interior_all_q_ind[_n-1]==1 & seam_interior_all_q_ind==1 & seam_interior_all_q_ind[_n+1]==1 & ///
											seam_interior_all_q_ind[_n+2]==1, window(2 1 2)
	capture drop q3_ljf_bls_q
	tssmooth ma q3_ljf_bls_q = log(jf_bls) if seam_interior_all_q_ind[_n-2]==1 & ///
											seam_interior_all_q_ind[_n-1]==1 & seam_interior_all_q_ind==1 & seam_interior_all_q_ind[_n+1]==1 & ///
											seam_interior_all_q_ind[_n+2]==1, window(2 1 2)
	 
		 
	capture drop q3_jf_bls_full_q
	tssmooth ma q3_jf_bls_full_q = jf_bls, window(2 1 2)
	capture drop q3_ljf_bls_full_q
	tssmooth ma q3_ljf_bls_full_q = log(jf_bls), window(2 1 2)
	 

	export excel using "${tempdata}/ts_jf_all_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jf_all_before_tramo.dta", replace

		jf_keepsave_exe jf 

	restore 
		// JF STANDARD YOUNG
		preserve 
		sort personkey yearmonth
		table quarter [aw=pweight2] if age_2dum==1 & unempl_ctv==1 & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
			  & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
			  , c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

			 
			 
			 jf_constr_instr_exe jf_yng 
			 
		export excel using "${tempdata}/ts_jf_yng_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
		save "${tempdata}/ts_jf_yng_before_tramo.dta",   replace
			
			jf_keepsave_exe jf_yng
			
		restore 

		// JF STANDARD PRIME 
		preserve 
		sort personkey yearmonth
		table quarter [aw=pweight2] if age_2dum==2 & unempl_ctv==1 & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
			  & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
			  , c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

			 
			 
			 jf_constr_instr_exe jf_prm 
			 
		export excel using "${tempdata}/ts_jf_prm_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
		save "${tempdata}/ts_jf_prm_before_tramo.dta",   replace

			jf_keepsave_exe jf_prm
			
		restore 


	// now with earlier employment in sample: need two censoring dimension to be addressed.
	// JF CUE-ALL
		preserve 
		sort personkey yearmonth

		table quarter [aw=pweight2] if (unempl_ctv==1) & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_uspell==1 & interview_no22>14 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

			 
			
						 
			 
			 jf_constr_instr_exe jf_cue 
		 

	export excel using "${tempdata}/ts_jf_cue_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jf_cue_before_tramo.dta",   replace

			jf_keepsave_exe jf_cue
			

	restore 
	// JF CUE YOUNG
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==1 & (unempl_ctv==1) & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_uspell==1 & interview_no22>14 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jf_cue_yng 
		 
	export excel using "${tempdata}/ts_jf_cue_yng_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jf_cue_yng_before_tramo.dta",   replace

			jf_keepsave_exe jf_cue_yng
			

	restore 

	// JF CUE PRIME 
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==2 & (unempl_ctv==1) & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_uspell==1 & interview_no22>14 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jf_cue_prm 
		 
	export excel using "${tempdata}/ts_jf_cue_prm_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jf_cue_prm_before_tramo.dta",   replace

			jf_keepsave_exe jf_cue_prm
			

	restore 

	// now with earlier employment in sample: need two censoring dimension to be addressed.
	// JF CUE2-ALL

		preserve 
		sort personkey yearmonth

		table quarter [aw=pweight2] if (unempl_ctv==1) & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_uspell==1 & interview_no22>18 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

			 
			
						 
			 
			 jf_constr_instr_exe jf_cue2 
		 

	export excel using "${tempdata}/ts_jf_cue2_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jf_cue2_before_tramo.dta",   replace


			jf_keepsave_exe jf_cue2
			

	restore 
	// JF cue2 YOUNG
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==1 & (unempl_ctv==1) & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_uspell==1 & interview_no22>18 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jf_cue2_yng 
		 
	export excel using "${tempdata}/ts_jf_cue2_yng_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jf_cue2_yng_before_tramo.dta",   replace


			jf_keepsave_exe jf_cue2_yng
			

	restore 

	// JF cue2 PRIME 
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==2 & (unempl_ctv==1) & (unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_uspell==1 & interview_no22>18 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lue_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jf_cue2_prm 
		 
	export excel using "${tempdata}/ts_jf_cue2_prm_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jf_cue2_prm_before_tramo.dta",   replace



			jf_keepsave_exe jf_cue2_prm
			



	restore 

	********************************
	** OUTFLOW RATE FROM NUN
	********************************

	// JF STANDARD-ALL
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
		  & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1 /// 
		  , c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jfn 
		 

	export excel using "${tempdata}/ts_jfn_all_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jfn_all_before_tramo.dta",   replace


			jf_keepsave_exe jfn
			



	restore 
		// JF STANDARD YOUNG
		preserve 
		sort personkey yearmonth
		table quarter [aw=pweight2] if age_2dum==1 & (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
			  & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
			   , c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

			 
			 
			 jf_constr_instr_exe jfn_yng 
			 
		export excel using "${tempdata}/ts_jfn_yng_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
		save "${tempdata}/ts_jfn_yng_before_tramo.dta",   replace


			jf_keepsave_exe jfn_yng
			


		restore 

		// JF STANDARD PRIME 
		preserve 
		sort personkey yearmonth
		table quarter [aw=pweight2] if age_2dum==2 & (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
			  & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
			  , c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

			 
			 
			 jf_constr_instr_exe jfn_prm 
			 
		export excel using "${tempdata}/ts_jfn_prm_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
		save "${tempdata}/ts_jfn_prm_before_tramo.dta",   replace
		

			jf_keepsave_exe jfn_prm
			



		restore 


	// now with earlier employment in sample: need two censoring dimension to be addressed.
	// JF cnune-ALL
		preserve 
		sort personkey yearmonth

		table quarter [aw=pweight2] if (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_nunspell==1 & interview_no22>14 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

			 
			
						 
			 
			 jf_constr_instr_exe jfn_cnune 
		 

	export excel using "${tempdata}/ts_jfn_cnune_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jfn_cnune_before_tramo.dta",   replace



			jf_keepsave_exe jfn_cnune
			



	restore 
	// JF cnune YOUNG
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==1 & (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_nunspell==1 & interview_no22>14 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jfn_cnune_yng 
		 
	export excel using "${tempdata}/ts_jfn_cnune_yng_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jfn_cnune_yng_before_tramo.dta",   replace


			jf_keepsave_exe jfn_cnune_yng
			
	restore 

	// JF cnune PRIME 
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==2 & (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_nunspell==1 & interview_no22>14 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jfn_cnune_prm 
		 
	export excel using "${tempdata}/ts_jfn_cnune_prm_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jfn_cnune_prm_before_tramo.dta",   replace


			jf_keepsave_exe jfn_cnune_prm
			
	restore 

	// now with earlier employment in sample: need two censoring dimension to be addressed.
	// JF cnune2-ALL

		preserve 
		sort personkey yearmonth

		table quarter [aw=pweight2] if (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_nunspell==1 & interview_no22>18 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

			 
			
						 
			 
			 jf_constr_instr_exe jfn_cnune2 
		 

	export excel using "${tempdata}/ts_jfn_cnune2_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jfn_cnune2_before_tramo.dta",   replace



			jf_keepsave_exe jfn_cnune2
			
	restore 
	// JF cnune2 YOUNG
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==1 & (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_nunspell==1 & interview_no22>18 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jfn_cnune2_yng 
		 
	export excel using "${tempdata}/ts_jfn_cnune2_yng_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jfn_cnune2_yng_before_tramo.dta",   replace


			jf_keepsave_exe jfn_cnune2_yng
			
	restore 

	// JF cnune2 PRIME 
	preserve 
	sort personkey yearmonth
	table quarter [aw=pweight2] if age_2dum==2 & (unempl_ctv==1| outlf_ctv==1) & (outlf_ctv[_n+1]==1| unempl_ctv[_n+1]==1 | empl_ctv[_n+1]==1) ///
				& personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & entry_ind==1  /// 
				& entry_ind==1    & complete_nunspell==1 & interview_no22>18 & u_spellength<=18 ///
				& sample_timetogo>=12, c(mean lne_ctv rawsum pweight2 mean jf_bls) replace

		 
		 
		 jf_constr_instr_exe jfn_cnune2_prm 
		 
	export excel using "${tempdata}/ts_jfn_cnune2_prm_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/ts_jfn_cnune2_prm_before_tramo.dta",   replace


			jf_keepsave_exe jfn_cnune2_prm
			
	restore 

**** MERGE ONLY THOSE THAT ARE NEEDED FOR THE PAPER INTO ONE XLSX FILE 	
	
use "${tempdata}/ts_jf_all_before_tramo.dta", clear
capture drop _merge_jfcue
merge 1:1 quarter using "${tempdata}/ts_jf_cue_before_tramo.dta", gen(_merge_jfcue)
capture drop _merge_jfn1
merge 1:1 quarter using "${tempdata}/ts_jfn_cnune_before_tramo.dta", gen(_merge_jfn1)
keep quarter ts_ljf_q q3_ljf_q q3_ljfn_cnune_q q3_ljfn_cnune_instr_q 

export excel using "${outputdata}/ts_jf_paper_before_tramo.xlsx", sheet("jf") sheetreplace firstrow(variables) nolabel
save "${outputdata}/ts_jf_paper_before_tramo.dta", replace
	
	

********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"