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




/* THIS PROGRAM reads in the corrected occupational mobility per 
			
			-QUARTER X N_SPELLENGTH OCC MOBILITY SERIES for our baseline U series
            -QUARTERLY OCC MOBILITY series for our NUN series, MM-YNG, MM, MM-PRM
			
and uses this to 
			
			1) calculate the time series of overall occupational mobility 
					(corrected and uncorrected, for 12 and 14 month windows)
			2) deliver these to TSW64+.exe, for TRAMO SEATS
			3) after TRAMO-ing these (with standard (automated) RSA=4), the time series are read back into STATA
			
				THIS STARTS PART II OF THIS DO-FILE
					- LOAD IN XLS TRAMOED SERIES. merge these 
					- save these post_tramo_ts.dta", replace

				NOTE THAT THERE ARE TWO SERIES 
						INPUT into TRAMO e.g. qn3u_mob_12_`indic'_u: qn3u refers to uncorrected for coding error: qn3u
																							_u refers to unrestricted 
					
					
			4) there they are HP filtered, and linearly detrended. 
						DDDD-> ALSO SMOOTHING: SP polynomial. _SM_ lowess
			5) report CORRELATION tables
					a)  ts_correlations?
					b) HP filtered 
					DDDD --> c) linearly detrended 
			5) pictures are drawn
			
			6) duration shift over the cycle is calculated and plotted

			
			
			Note, this program also contains:
					i) dummy indicators to take into account the left and right censoring 
							there is a category _u variables that is _UNRESTRICTED with respect to the censoring adjustment

				DONE - fix mmnp category
				DONE -finish linear detrending bit
				DONE - pictures with only corrected mobility
				DONE - repeat exercise for uncorrected mobility: detrend, smooth etc.
				DONE - pictures with raw (uncorrected) mobility
				(- now repeat for separation timing)
				- add HP logged series of the raw data 
				- 
							
			INPUT: THE QUARTER X N_SPELLENGTH IS PRODUCED on the SERVER			
			behind this are do files: SECTION2_3_cyclical_timeseries_2009/1909/1809.do
			earlier: 
			 SECTION2_3_cyclical_gammacorrected_timeseries_sept2017.do
			
			
							** produce another quarter x n_spellengths with first lastwave restriction
							** separation timing 

							
			TO DO:
				DONE - rtmm_nun
				DONE - rtnm corr and uncorr picture
				- USE THE APPROPRIATE WEIGHTS WHEN DOING THE REGRESSION/POLYNOMIAL (SUM OF ALL DURATION WEIGHTS)
				
				- confidence intervals of correlations? pwcorr, star(0.05)  add-on program: CORRCI (ssc install corrci)
				- make sure LOUTPW measure is used, not the original LOUTPW2
 */


*************************************************
*** INITIALIZATION
*************************************************

	


	cd "${workingdir}"
	global workingdir `c(pwd)'
	do "${workingdir}/global_paths.do"
	

	version 13
	
	set more off
	set varabbrev off

	
	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"

	cd "${tempdata}"

	set more off
	set varabbrev off
	
	do "${step2codedir}/aux_directory/aux_programs.do"

	global locfileversion=1022

***********************************************	
*** COPY REFERENCE SERIES TO OUTPUT DIRECTORY
***********************************************

cd "${outputdata}"


global files : dir . files "*_reference.xls*"

	display ${files}

	foreach f of global files {
		display "DELETING `f'"
		cap n erase `f' 
		
	}



cd "${step2codedir}/Tramo_Seats_series_REFERENCE/"

global files : dir . files "*.*"

	display ${files}

	foreach f of global files {
		display "COPYING `f' from ${step2codedir}/Tramo_Seats_series_REFERENCE/ to ${outputdata}"
		cap n copy `f' "${outputdata}"
		
	}


***********************************************
***  DATA READ-IN
***********************************************

** START: MM 
 clear
 import excel "${outputdata}/timeseriesdur_mm.xls", sheet("ts") firstrow
 cap n ren A quarter
 cap n ren B u_spellength
 cap n gen u_spellength=n_spellength
 
 
 
 sort quarter
destring quarter, replace
cap n destring u_spellength, replace
cap n destring n_spellength, replace

keep qn1* qn3* u_spellength n_spellength quarter

sort quarter n_spellength
format quarter %tq

/*
*** HERE WE CHANGED THE MEASURE FOR HP_LOUTPW
 capture drop merge_qtrfill
 merge m:1 quarter using "${aggdatadir}aggdata_from_pwd_030517", keepusing(hp_lunrate hp_loutpw outpw) gen(merge_qtrfill)
 ren hp_loutpw hp_loutpw2
 
 cap n gen u_spellength=n_spellength
 sort quarter u_spellength
drop if quarter<tq(1983q1)
 
 tab quarter
*/ 
 save "${tempdata}/ts_occmob_mm.dta", replace
 
** MM timeseries aggregated across all durations 
clear
import excel "${outputdata}/timeseries_mm.xls", sheet("ts") firstrow
 
keep q1* q3* quarter
destring quarter, replace
sort quarter 


format quarter %tq
drop if quarter<tq(1983q1)
 
gen n_spellength=1 

cap ren q3_mob_* q3u_mob_*_u
cap ren q1_mob_* q1u_mob_*_u
cap ren q3_mobcorr_* q3_mob_*_u
cap ren q1_mobcorr_* q1_mob_*_u

 
capture drop merge_qtrfill_mmall
 merge 1:1 quarter n_spellength using "${tempdata}/ts_occmob_mm.dta", gen(merge_qtrfill_mmall)
save "${tempdata}/ts_occmob_mm.dta", replace


 
*****************************************************************************
*** PICTURE OF DURATION DISTRIBUTION: USED IN THE DATA APPENDIX 
*****************************************************************************

/* the following pictures are in the results collection, because they are used 
for the durdistr_stability dummy, see Supplementary Appendix,data construction*/
/*
preserve
cap n drop if merge_qtrfill==2
keep qn1_obs_mobcorr_mm u_spellength quarter
reshape wide qn1_obs_mobcorr_mm, i(quarter) j(u_spellength)

#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 qn1_obs_mobcorr_mm15 
qn1_obs_mobcorr_mm16 qn1_obs_mobcorr_mm17 qn1_obs_mobcorr_mm18, over(quarter) stack percent legend(off) ;
#delimit cr
graph export "${allresultsdir}/duration_distribution_18m.pdf", as(pdf) replace 

#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 , over(quarter,  label(nolabels)) stack percent legend(off) ;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m.pdf", as(pdf) replace

*** BY PERIOD 

* period 1
format quarter %tq

#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 if quarter>=tq(1984q4) & quarter<=tq(1989q4), stack percent  legend(off) graphregion(color(white)) 
bargap(5)
over(quarter, relabel(1 "*1984q4"
2 " 1985q1"
3 " " 
4 " "
5 " "
6 " 1986q1"
7 " " 
8 " "
9 " "
10 " 1987q1"
11 " "
12 " "
13 " "
14 " 1988q1"
15 " "
16 " "
17 " "
18 " 1989q1"
19 " "
20 " 1989q3"
21 "*1989q4"
))

;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m_period1_2.pdf", as(pdf) replace


#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 if quarter>tq(1990q4) & quarter<=tq(1995q4), stack percent  legend(off) graphregion(color(white)) 
over(quarter, relabel(1 "*1990q4" 
2 "*1991q1"
3 " " 
4 " "
5 " 1992q1"
6 " "
7 " " 
8 " "
9 " 1993q1"
10 " "
11 " "
12 " "
13 " 1994q1"
14 " "
15 " "
16 " "
17 " 1995q1"
18 " "
19 " 1995q3"
20 "*1995q4"
))
;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m_period2_2A.pdf", as(pdf) replace

 * because of wave 5 starting only in 1991q1
 
 
#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 if quarter>tq(1991q1) & quarter<=tq(1995q4), stack percent  legend(off) graphregion(color(white)) 
over(quarter, relabel(1 "*1991q1" 
2 " 1991q2"
3 " " 
4 " 1992q1"
5 " "
6 " "
7 " " 
8 " 1993q1"
9 " "
10 " "
11 " "
12 " 1994q1"
13 " "
14 " "
15 " "
16 " 1995q1"
17 " "
18 " 1995q3"
19 "*1995q4"
))
;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m_period2_2.pdf", as(pdf) replace

 
 
 
#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 if quarter>=tq(1997q2) & quarter<=tq(2000q1), stack percent  legend(off) graphregion(color(white)) 
over(quarter, relabel(1 "*1997q2" 
2 " 1997q3"
3 " " 
4 " 1998q1"
5 " "
6 " "
7 " " 
8 " 1999q1"
9 " "
10 " "
11 " 1999q4"
12 "*2000q1"
))
;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m_period3_2.pdf", as(pdf) replace


#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 if quarter>=tq(2002q1) & quarter<=tq(2003q4),  stack percent 
legend(rows(3) order(1 2 3 4 5 6 7 8 9 10 11 12 13 14) position(12)
label(1 "1 month") label(2 "2") label(3 "3") label(4 "4") label(5 "5") label(6 "6 months") label(7 "7") label(8 "8") label(9 "9") 
label(10 "10")  label(11 "11 months")  label(12 "12") label(13 "13")  label(14 "14") 
)  
graphregion(color(white)) 
over(quarter, relabel(1 "*2002q1" 
2 " 2002q2"
3 " " 
4 " "
5 " 2003q1"
6 " "
7 " " 
8 " 2003q4 "
))
;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m_period4_2.pdf", as(pdf) replace

#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 if quarter>=tq(2005q1) & quarter<=tq(2007q4), 
over(quarter, relabel(1 " 2005q1" 
2 " "
3 " " 
4 " "
5 " 2006q1"
6 " "
7 " " 
8 " "
9 " 2007q1"
10 " "
11 " 2007q3" 
12 "*2007q4"
))
 
stack percent  legend(off) graphregion(color(white)) 
bargap(5) 
;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m_period5_2.pdf", as(pdf) replace

#delimit ;
graph hbar qn1_obs_mobcorr_mm1 qn1_obs_mobcorr_mm2 qn1_obs_mobcorr_mm3 
qn1_obs_mobcorr_mm4 qn1_obs_mobcorr_mm5 qn1_obs_mobcorr_mm6 qn1_obs_mobcorr_mm7 
qn1_obs_mobcorr_mm8 qn1_obs_mobcorr_mm9 qn1_obs_mobcorr_mm10 qn1_obs_mobcorr_mm11 
qn1_obs_mobcorr_mm12 qn1_obs_mobcorr_mm13 qn1_obs_mobcorr_mm14 if quarter>=tq(2009q3) & quarter<=tq(2013q4), 
over(quarter, relabel(1 " 2009q3" 
2 " "
3 " 2010q1" 
4 " "
5 " "
6 " "
7 " 2011q1" 
8 " "
9 " "
10 " "
11 " 2012q1" 
12 " "
13 " "
14 " "
15 " 2013q1"  
16 " "
17 " 2013q3"
18 "*2013q4" ))
 stack percent  legend(off) graphregion(color(white)) 
bargap(5) 
;
#delimit cr
graph export "${allresultsdir}/duration_distribution_14m_period6_2.pdf", as(pdf) replace




restore 
*/
***************************************************************************************************8
**  PREPARATION FOR TRAMO
****************************************************************************************************



		** CREATE CUMULATIVE MOBILITY NUMBERS FROM QUARTER x N_SPELLENGTH DATA
 
 
cap n use "${tempdata}/ts_occmob_mm.dta"



//PPPPPPP---CUMULATIVE_MOB
capture program drop cumulative_mob
program define cumulative_mob
		args name_infix
		
		forvalues i=1(2)3 {		
		sort quarter n_spellength 
		 
		capture drop  qn`i'_occmove_`name_infix' 
		capture drop qn`i'_occstay_`name_infix'
		gen qn`i'_occmove_`name_infix'=qn`i'_mobcorr_`name_infix'*qn`i'_obs_mobcorr_`name_infix'
		gen qn`i'_occstay_`name_infix'=(1.0-qn`i'_mobcorr_`name_infix')*qn`i'_obs_mobcorr_`name_infix'

		capture drop qn`i'_m_asc_`name_infix' 
		capture drop qn`i'_s_asc_`name_infix' 
		gen qn`i'_m_asc_`name_infix'=0
		gen qn`i'_s_asc_`name_infix'=0
		replace qn`i'_m_asc_`name_infix'=qn`i'_occmove_`name_infix' if n_spellength==1 & qn`i'_occmove_`name_infix'!=.
		replace qn`i'_s_asc_`name_infix'= qn`i'_occstay_`name_infix' if n_spellength==1 & qn`i'_occstay_`name_infix'!=.
		forvalues h=1(1)18 {
		replace qn`i'_m_asc_`name_infix'=qn`i'_occmove_`name_infix'+qn`i'_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'!=.
		replace qn`i'_s_asc_`name_infix'= qn`i'_occstay_`name_infix'+qn`i'_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'!=.
		replace qn`i'_m_asc_`name_infix'=qn`i'_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'==.
		replace qn`i'_s_asc_`name_infix'= qn`i'_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'==.
		}
		 
		gsort quarter -n_spellength

		capture drop qn`i'_m_desc12_`name_infix' 
		capture drop qn`i'_s_desc12_`name_infix' 
		gen qn`i'_m_desc12_`name_infix'=0
		gen qn`i'_s_desc12_`name_infix'= 0
		replace qn`i'_m_desc12_`name_infix'=qn`i'_occmove_`name_infix' if n_spellength==12 & qn`i'_occmove_`name_infix'!=.
		replace qn`i'_s_desc12_`name_infix'= qn`i'_occstay_`name_infix' if n_spellength==12 & qn`i'_occstay_`name_infix'!=.
		forvalues h=1(1)11 {
		replace qn`i'_m_desc12_`name_infix'=qn`i'_occmove_`name_infix'+qn`i'_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'!=.
		replace qn`i'_s_desc12_`name_infix'= qn`i'_occstay_`name_infix'+qn`i'_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'!=.
		replace qn`i'_m_desc12_`name_infix'=qn`i'_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'==.
		replace qn`i'_s_desc12_`name_infix'= qn`i'_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'==.
		}

		capture drop qn`i'_m_desc14_`name_infix' 
		capture drop qn`i'_s_desc14_`name_infix' 

		gen qn`i'_m_desc14_`name_infix'=0
		gen qn`i'_s_desc14_`name_infix'= 0
		replace qn`i'_m_desc14_`name_infix'=qn`i'_occmove_`name_infix' if n_spellength==14 & qn`i'_occmove_`name_infix'!=.
		replace qn`i'_s_desc14_`name_infix'= qn`i'_occstay_`name_infix' if n_spellength==14 & qn`i'_occstay_`name_infix'!=.
		forvalues h=1(1)13 {
		replace qn`i'_m_desc14_`name_infix'=qn`i'_occmove_`name_infix'+qn`i'_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'!=.
		replace qn`i'_s_desc14_`name_infix'= qn`i'_occstay_`name_infix'+qn`i'_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'!=.
		replace qn`i'_m_desc14_`name_infix'=qn`i'_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occmove_`name_infix'==.
		replace qn`i'_s_desc14_`name_infix'= qn`i'_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'_occstay_`name_infix'==.
		 } 
		 sort quarter n_spellength 
		 
		 
		 
		 ** 1) cumulative (from 1 to x)
		capture drop qn`i'_cum_mob_asc_`name_infix'
		gen  qn`i'_cum_mob_asc_`name_infix'=qn`i'_m_asc_`name_infix'/(qn`i'_m_asc_`name_infix'+qn`i'_s_asc_`name_infix' )
		capture drop qn`i'_cum_mob_asc_`name_infix'_obs
		gen qn`i'_cum_mob_asc_`name_infix'_obs=qn`i'_m_asc_`name_infix'+qn`i'_s_asc_`name_infix' 

		 ** 2) cumulative (from x to 12)
		capture drop qn`i'_cum_mob_desc12_`name_infix'
		gen  qn`i'_cum_mob_desc12_`name_infix'=qn`i'_m_desc12_`name_infix'/(qn`i'_m_desc12_`name_infix'+qn`i'_s_desc12_`name_infix' )
		 capture drop qn`i'_cum_mob_desc12_`name_infix'_obs
		 gen qn`i'_cum_mob_desc12_`name_infix'_obs=(qn`i'_m_desc12_`name_infix'+qn`i'_s_desc12_`name_infix' )
		 
		 ** 3) cumulative (from x to 14) 
		capture drop qn`i'_cum_mob_desc14_`name_infix'
		gen  qn`i'_cum_mob_desc14_`name_infix'=qn`i'_m_desc14_`name_infix'/(qn`i'_m_desc14_`name_infix'+qn`i'_s_desc14_`name_infix' )
		 capture drop qn`i'_cum_mob_desc14_`name_infix'_obs
		 gen  qn`i'_cum_mob_desc14_`name_infix'_obs=(qn`i'_m_desc14_`name_infix'+qn`i'_s_desc14_`name_infix' )
		 }
		 compress
 
end 
//PPPPPPP---CUMULATIVE_MOB

	
 cumulative_mob mm
 
//***** UNCORRECTED SERIES 
 
 
//PPPPPPP---UNCORRECTED CUMULATIVE_MOB
 capture program drop uncorr_cumulative
program define uncorr_cumulative
		args name_infix
		
		forvalues i=1(2)3 {		
		sort quarter n_spellength 
		 
		capture drop  qn`i'u_occmove_`name_infix' 
		capture drop qn`i'u_occstay_`name_infix'
		gen qn`i'u_occmove_`name_infix'=qn`i'_mob_`name_infix'*qn`i'_obs_mob_`name_infix'
		gen qn`i'u_occstay_`name_infix'=(1.0-qn`i'_mob_`name_infix')*qn`i'_obs_mob_`name_infix'

		capture drop qn`i'u_m_asc_`name_infix' 
		capture drop qn`i'u_s_asc_`name_infix' 
		gen qn`i'u_m_asc_`name_infix'=0
		gen qn`i'u_s_asc_`name_infix'=0
		replace qn`i'u_m_asc_`name_infix'=qn`i'u_occmove_`name_infix' if n_spellength==1 & qn`i'u_occmove_`name_infix'!=.
		replace qn`i'u_s_asc_`name_infix'= qn`i'u_occstay_`name_infix' if n_spellength==1 & qn`i'u_occstay_`name_infix'!=.
		forvalues h=1(1)18 {
		replace qn`i'u_m_asc_`name_infix'=qn`i'u_occmove_`name_infix'+qn`i'u_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'!=.
		replace qn`i'u_s_asc_`name_infix'= qn`i'u_occstay_`name_infix'+qn`i'u_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'!=.
		replace qn`i'u_m_asc_`name_infix'=qn`i'u_m_asc_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'==.
		replace qn`i'u_s_asc_`name_infix'= qn`i'u_s_asc_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]+1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'==.
		}
		 
		gsort quarter -n_spellength

		capture drop qn`i'u_m_desc12_`name_infix' 
		capture drop qn`i'u_s_desc12_`name_infix' 
		gen qn`i'u_m_desc12_`name_infix'=0
		gen qn`i'u_s_desc12_`name_infix'= 0
		replace qn`i'u_m_desc12_`name_infix'=qn`i'u_occmove_`name_infix' if n_spellength==12 & qn`i'u_occmove_`name_infix'!=.
		replace qn`i'u_s_desc12_`name_infix'= qn`i'u_occstay_`name_infix' if n_spellength==12 & qn`i'u_occstay_`name_infix'!=.
		forvalues h=1(1)11 {
		replace qn`i'u_m_desc12_`name_infix'=qn`i'u_occmove_`name_infix'+qn`i'u_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'!=.
		replace qn`i'u_s_desc12_`name_infix'= qn`i'u_occstay_`name_infix'+qn`i'u_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'!=.
		replace qn`i'u_m_desc12_`name_infix'=qn`i'u_m_desc12_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'==.
		replace qn`i'u_s_desc12_`name_infix'= qn`i'u_s_desc12_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'==.
		}

		capture drop qn`i'u_m_desc14_`name_infix' 
		capture drop qn`i'u_s_desc14_`name_infix' 

		gen qn`i'u_m_desc14_`name_infix'=0
		gen qn`i'u_s_desc14_`name_infix'= 0
		replace qn`i'u_m_desc14_`name_infix'=qn`i'u_occmove_`name_infix' if n_spellength==14 & qn`i'u_occmove_`name_infix'!=.
		replace qn`i'u_s_desc14_`name_infix'= qn`i'u_occstay_`name_infix' if n_spellength==14 & qn`i'u_occstay_`name_infix'!=.
		forvalues h=1(1)13 {
		replace qn`i'u_m_desc14_`name_infix'=qn`i'u_occmove_`name_infix'+qn`i'u_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'!=.
		replace qn`i'u_s_desc14_`name_infix'= qn`i'u_occstay_`name_infix'+qn`i'u_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'!=.
		replace qn`i'u_m_desc14_`name_infix'=qn`i'u_m_desc14_`name_infix'[_n-1] if n_spellength== n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occmove_`name_infix'==.
		replace qn`i'u_s_desc14_`name_infix'= qn`i'u_s_desc14_`name_infix'[_n-1] if n_spellength==n_spellength[_n-1]-1 & quarter==quarter[_n-1] & qn`i'u_occstay_`name_infix'==.
		 } 
		 sort quarter n_spellength 
		 
		 
		 
		 ** 1) cumulative (from 1 to x)
		capture drop qn`i'u_cum_mob_asc_`name_infix'
		gen  qn`i'u_cum_mob_asc_`name_infix'=qn`i'u_m_asc_`name_infix'/(qn`i'u_m_asc_`name_infix'+qn`i'u_s_asc_`name_infix' )
		capture drop qn`i'u_cum_mob_asc_`name_infix'_obs
		gen qn`i'u_cum_mob_asc_`name_infix'_obs=qn`i'u_m_asc_`name_infix'+qn`i'u_s_asc_`name_infix' 

		 ** 2) cumulative (from x to 12)
		capture drop qn`i'u_cum_mob_desc12_`name_infix'
		gen  qn`i'u_cum_mob_desc12_`name_infix'=qn`i'u_m_desc12_`name_infix'/(qn`i'u_m_desc12_`name_infix'+qn`i'u_s_desc12_`name_infix' )
		 capture drop qn`i'u_cum_mob_desc12_`name_infix'_obs
		 gen qn`i'u_cum_mob_desc12_`name_infix'_obs=(qn`i'u_m_desc12_`name_infix'+qn`i'u_s_desc12_`name_infix' )
		 
		 ** 3) cumulative (from x to 14) 
		capture drop qn`i'u_cum_mob_desc14_`name_infix'
		gen  qn`i'u_cum_mob_desc14_`name_infix'=qn`i'u_m_desc14_`name_infix'/(qn`i'u_m_desc14_`name_infix'+qn`i'u_s_desc14_`name_infix' )
		 capture drop qn`i'u_cum_mob_desc14_`name_infix'_obs
		 gen  qn`i'u_cum_mob_desc14_`name_infix'_obs=(qn`i'u_m_desc14_`name_infix'+qn`i'u_s_desc14_`name_infix' )
		 }
		 compress
 
end
//PPPPPPP---UNCORRECTED CUMULATIVE_MOB
 uncorr_cumulative mm
 

 
//***********************************************************************
// CREATE CUMULATIVE MOBILITY NUMBERS FROM QUARTER x N_SPELLENGTH DATA X *****PANEL************** 
//**************************************************************************
 
 

capture drop mm_clsfication_dum
gen mm_clsfication_dum =0 if quarter<=tq(2000q1)
replace mm_clsfication_dum=1 if quarter>=tq(2000q4)
 
 
** in fullrot=1 quarter must contain three rotations with positive outflows, spread relatively equally 
capture drop fullrot
gen byte fullrot=1 
replace fullrot=0 if quarter==tq(1995q4)
replace fullrot=0 if quarter==tq(2000q1)
replace fullrot=0 if quarter==tq(2000q4)
 

 * with more relaxed sttg restriction
capture drop fullrot_df
gen byte fullrot_df=1 

** last wave cut out, because long-term guys are more likely to report at end of wave (and hence are missed)
/*
capture drop panellastquarter 	
gen panellastquarter=0
replace panellastquarter=1 if quarter==tq(1989q4) & panel==1988
replace panellastquarter=1 if quarter==tq(1995q4) & panel==1993
replace panellastquarter=1 if quarter==tq(2000q1) & panel==1996
replace panellastquarter=1 if quarter==tq(2003q4) & panel==2001
replace panellastquarter=1 if quarter==tq(2007q4) & panel==2004
replace panellastquarter=1 if quarter==tq(2013q4) & panel==2008

capture drop panelfirstquarter_14intv
gen panelfirstquarter_14intv=0
*/

	capture drop interpol_all
	gen interpol_all=0 if quarter>=tq(1982q3)
	replace interpol_all=1 if quarter>=tq(1982q3) & quarter<=tq(1983q3)
	replace interpol_all=1 if quarter>=tq(1989q4) & quarter<=tq(1990q4)
	replace interpol_all=1 if quarter>=tq(1995q4) & quarter<=tq(1997q1)
	replace interpol_all=1 if quarter>=tq(2000q1) & quarter<=tq(2001q4)
	replace interpol_all=1 if quarter>=tq(2003q4) & quarter<=tq(2004q4)
	replace interpol_all=1 if quarter>=tq(2007q4) & quarter<=tq(2009q2)
	 
	capture drop ltu
	gen ltu=0
	replace ltu=1 if u_spellength>6 & u_spellength<. 
	 
	** collect duration distribution shifts
	capture drop durdistr_stability
	gen durdistr_stability=.
	replace durdistr_stability=1 if quarter>=100 & quarter<=118
	replace durdistr_stability=1 if quarter>=125 & quarter<=142
	replace durdistr_stability=1 if quarter>=150 & quarter<=159
	replace durdistr_stability=1 if quarter>=169 & quarter<=175
	replace durdistr_stability=1 if quarter>=180 & quarter<=190
	replace durdistr_stability=1 if quarter>=169 & quarter<=175
	replace durdistr_stability=1 if quarter>=198 & quarter<=214
	 

	 capture drop fullseam_qtr_ind
	gen byte fullseam_qtr_ind=.
	replace fullseam_qtr_ind=1 if quarter>=95 & quarter<=118
	replace fullseam_qtr_ind=1 if quarter>=121 & quarter<=142
	replace fullseam_qtr_ind=1 if quarter>=146 & quarter<=158
	replace fullseam_qtr_ind=1 if quarter>=166 & quarter<=174
	replace fullseam_qtr_ind=1 if quarter>=177 & quarter<=190
	replace fullseam_qtr_ind=1 if quarter>=195 & quarter<=213
	* scatter interpol_all durdistr_stability quarter if quarter>=tq(1982q3) & (interpol_all>0| durdistr_stability>0), msymbol(Oh Dh) 
	**table quarter, c(mean interpol_all mean durdistr_stability )


	
save "${tempdata}/ts_occmob_mm.dta", replace
	 
	 
	 
**********************************************************
**  OVERALL MOBILITY (I.E. CUM. DURATION = 1) FOR TRAMO
********************************************** **********
	 
	 
sort quarter u_spellength
keep if u_spellength==1
	
tsset quarter	 
tsfill

global min_obscount=0

*** generate series

			
				**** Q3 SMOOTHED, CORRECTED FIRST, THEN UNCORRECTED*******

//PPPPPPP -- QN3_FORTRAMO_EXE
capture program drop qn3_fortramo_exe
program define qn3_fortramo_exe
				args indic
	capture drop qn3_mob_12_`indic'
	gen qn3_mob_12_`indic'=qn3_cum_mob_desc12_`indic' if qn3_cum_mob_desc12_`indic'[_n-2]!=. & qn3_cum_mob_desc12_`indic'[_n-1]!=. & qn3_cum_mob_desc12_`indic' !=. & qn3_cum_mob_desc12_`indic'[_n+1]!=. ///
	 & qn3_cum_mob_desc12_`indic'[_n+2]!=. & durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn3_mob_14_`indic'
	gen qn3_mob_14_`indic'=qn3_cum_mob_desc14_`indic' if qn3_cum_mob_desc14_`indic'[_n-2]!=. & qn3_cum_mob_desc14_`indic'[_n-1]!=. & qn3_cum_mob_desc14_`indic' !=. & qn3_cum_mob_desc14_`indic'[_n+1]!=. ///
	 & qn3_cum_mob_desc14_`indic'[_n+2]!=. & durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn3_mob_14_`indic'_u
	gen qn3_mob_14_`indic'_u=qn3_cum_mob_desc14_`indic' if qn3_cum_mob_desc14_`indic'[_n-2]!=. & qn3_cum_mob_desc14_`indic'[_n-1]!=. & qn3_cum_mob_desc14_`indic' !=. & qn3_cum_mob_desc14_`indic'[_n+1]!=. ///
	 & qn3_cum_mob_desc14_`indic'[_n+2]!=. & u_spellength==1 & quarter>tq(1984q1) ///
	 & qn1_cum_mob_desc14_`indic'_obs>=$min_obscount	
	capture drop qn3_mob_12_`indic'_u
	gen qn3_mob_12_`indic'_u=qn3_cum_mob_desc12_`indic' if qn3_cum_mob_desc12_`indic'[_n-2]!=. & qn3_cum_mob_desc12_`indic'[_n-1]!=. & qn3_cum_mob_desc12_`indic' !=. & qn3_cum_mob_desc12_`indic'[_n+1]!=. ///
	 & qn3_cum_mob_desc12_`indic'[_n+2]!=. & u_spellength==1 & quarter>tq(1984q1) ///
	 & qn1_cum_mob_desc12_`indic'_obs>=$min_obscount
 end 
//PPPPPPP -- QN3_FORTRAMO_EXE 
 
 
qn3_fortramo_exe mm


** MM-NUN, MM-YNG, MM-PRM, MM (v2)
//PPPPPPP -- Q3_FORTRAMO_EXE 
capture program drop q3_fortramo_exe
program define q3_fortramo_exe
				args indic

	set varabbrev off
	gen q3_mob_`indic'=q3_mob_`indic'_u if q3_mob_`indic'_u[_n-2]!=. & q3_mob_`indic'_u[_n-1]!=. & q3_mob_`indic'_u !=. & q3_mob_`indic'_u[_n+1]!=. ///
		 & q3_mob_`indic'_u[_n+2]!=. & durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	
end 
//PPPPPPP -- Q3_FORTRAMO_EXE 

q3_fortramo_exe mm
q3_fortramo_exe mm_nun
q3_fortramo_exe mmmy
q3_fortramo_exe mmmp


/*
preserve
	keep quarter qn3_mob_1*  
	export excel using "${tempdata}/occmob_ts_for_tramo_corr.xlsx", sheet("q3") sheetreplace firstrow(variables) nolabel
	save "${tempdata}/occmob_ts_for_tramo.dta", replace
 restore 
*/
	
	
//PPPPPPPPP - QN3U_FORTRAMO_EXE
capture program drop qn3u_fortramo_exe
program define qn3u_fortramo_exe
					args indic
	capture drop qn3u_mob_12_`indic'
	gen qn3u_mob_12_`indic'=qn3u_cum_mob_desc12_`indic' if qn3u_cum_mob_desc12_`indic'[_n-2]!=. & qn3u_cum_mob_desc12_`indic'[_n-1]!=. & qn3u_cum_mob_desc12_`indic' !=. & qn3u_cum_mob_desc12_`indic'[_n+1]!=. ///
	 & qn3u_cum_mob_desc12_`indic'[_n+2]!=. & durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn3u_mob_14_`indic'
	gen qn3u_mob_14_`indic'=qn3u_cum_mob_desc14_`indic' if qn3u_cum_mob_desc14_`indic'[_n-2]!=. & qn3u_cum_mob_desc14_`indic'[_n-1]!=. & qn3u_cum_mob_desc14_`indic' !=. & qn3u_cum_mob_desc14_`indic'[_n+1]!=. ///
	 & qn3u_cum_mob_desc14_`indic'[_n+2]!=. & durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn3u_mob_14_`indic'_u
	gen qn3u_mob_14_`indic'_u=qn3u_cum_mob_desc14_`indic' if qn3u_cum_mob_desc14_`indic'[_n-2]!=. & qn3u_cum_mob_desc14_`indic'[_n-1]!=. & qn3u_cum_mob_desc14_`indic' !=. & qn3u_cum_mob_desc14_`indic'[_n+1]!=. ///
	 & qn3u_cum_mob_desc14_`indic'[_n+2]!=. & u_spellength==1 & quarter>tq(1984q1)  
	capture drop qn3u_mob_12_`indic'_u
	gen qn3u_mob_12_`indic'_u=qn3u_cum_mob_desc12_`indic' if qn3u_cum_mob_desc12_`indic'[_n-2]!=. & qn3u_cum_mob_desc12_`indic'[_n-1]!=. & qn3u_cum_mob_desc12_`indic' !=. & qn3u_cum_mob_desc12_`indic'[_n+1]!=. ///
	 & qn3u_cum_mob_desc12_`indic'[_n+2]!=. & u_spellength==1 & quarter>tq(1984q1) 
end 
//PPPPPPPPP - QN3U_FORTRAMO_EXE


 qn3u_fortramo_exe mm

//PPPPPPPPP - Q3U_FORTRAMO_EXE
capture program drop q3u_fortramo_exe
program define q3u_fortramo_exe
				args indic

	set varabbrev off
	gen q3u_mob_`indic'=q3u_mob_`indic'_u if q3u_mob_`indic'_u[_n-2]!=. & q3u_mob_`indic'_u[_n-1]!=. & q3u_mob_`indic'_u !=. & q3u_mob_`indic'_u[_n+1]!=. ///
		 & q3u_mob_`indic'_u[_n+2]!=. & durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	
end 
//PPPPPPPPP - Q3U_FORTRAMO_EXE

q3u_fortramo_exe mm
q3u_fortramo_exe mm_nun
q3u_fortramo_exe mmmy
q3u_fortramo_exe mmmp


		**** Q1 UNSMOOTHED, CORRECTED FIRST, THEN UNCORRECTED*******
 
 
//PPPPPPP  --- qn1_fortramo_exe
capture program drop qn1_fortramo_exe
program define qn1_fortramo_exe
				args indic
	capture drop qn1_mob_12_`indic'
	gen qn1_mob_12_`indic'=qn1_cum_mob_desc12_`indic' if qn1_cum_mob_desc12_`indic'!=. & durdistr_stability==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn1_mob_14_`indic'
	gen qn1_mob_14_`indic'=qn1_cum_mob_desc14_`indic' if qn1_cum_mob_desc12_`indic'!=. & durdistr_stability==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn1_mob_14_`indic'_u
	gen qn1_mob_14_`indic'_u=qn1_cum_mob_desc14_`indic' if qn1_cum_mob_desc12_`indic'!=. & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn1_mob_12_`indic'_u
	gen qn1_mob_12_`indic'_u=qn1_cum_mob_desc12_`indic' if qn1_cum_mob_desc12_`indic'!=. & u_spellength==1 & quarter>tq(1984q1) 
 end 
//PPPPPPP  --- qn1_fortramo_exe 
 
 qn1_fortramo_exe mm

** MM-NUN, MM-YNG, MM-PRM, MM (v2)
//PPPPPPP  --- q1_fortramo_exe 
capture program drop q1_fortramo_exe
program define q1_fortramo_exe
				args indic

	set varabbrev off
	gen q1_mob_`indic'=q1_mob_`indic'_u if q1_mob_`indic'_u !=. /// 
	& durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	
end 
//PPPPPPP  --- q1_fortramo_exe 

q1_fortramo_exe mm
q1_fortramo_exe mm_nun
q1_fortramo_exe mmmy
q1_fortramo_exe mmmp
 
/*
preserve
	 keep quarter qn1_mob_1*  
	export excel using "C:\data\occmob_ts_for_tramo_corr.xlsx", sheet("q1") sheetreplace firstrow(variables) nolabel
	export excel using "C:\data\occmob_tsq1_for_tramo_corr.xlsx", sheet("q1") sheetreplace firstrow(variables) nolabel
	*save "C:\data\occmob_ts_for_tramo.dta", replace
restore 
*/

//PPPPPPP  --- qn1u_fortramo_exe 
capture program drop qn1u_fortramo_exe
program define qn1u_fortramo_exe
				args indic
	capture drop qn1u_mob_12_`indic'
	gen qn1u_mob_12_`indic'=qn1u_cum_mob_desc12_`indic' if qn1u_cum_mob_desc12_`indic'!=. & durdistr_stability==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn1u_mob_14_`indic'
	gen qn1u_mob_14_`indic'=qn1u_cum_mob_desc14_`indic' if qn1u_cum_mob_desc12_`indic'!=. & durdistr_stability==1 & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn1u_mob_14_`indic'_u
	gen qn1u_mob_14_`indic'_u=qn1u_cum_mob_desc14_`indic' if qn1u_cum_mob_desc12_`indic'!=. & u_spellength==1 & quarter>tq(1984q1) 
	capture drop qn1u_mob_12_`indic'_u
	gen qn1u_mob_12_`indic'_u=qn1u_cum_mob_desc12_`indic' if qn1u_cum_mob_desc12_`indic'!=. & u_spellength==1 & quarter>tq(1984q1) 
end 
//PPPPPPP  --- qn1u_fortramo_exe 
 
 qn1u_fortramo_exe mm
 

 ** MM-NUN, MM-YNG, MM-PRM, MM (v2)
//PPPPPPP  --- q1u_fortramo_exe 
capture program drop q1u_fortramo_exe
program define q1u_fortramo_exe
				args indic

	set varabbrev off
	gen q1u_mob_`indic'=q1u_mob_`indic'_u if q1u_mob_`indic'_u !=. ///
	& durdistr_stability[_n+2]==1 & durdistr_stability[_n-2]==1 & u_spellength==1 & quarter>tq(1984q1) 
	
end 
//PPPPPPP  --- q1u_fortramo_exe 

q1u_fortramo_exe mm
q1u_fortramo_exe mm_nun
q1u_fortramo_exe mmmy
q1u_fortramo_exe mmmp
 

 
	capture drop _merge
	merge 1:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", keepusing(lunrate lunrate_bls lunrate_bls_0 loutpw loutpw2)
	drop if _merge!=3
	 
 
 
 preserve 
	keep quarter ///
			/* ** ADDITIONAL SERIES COMMENTED OUT, COMMENT IN, IF INTERESTED ** */ ///
	/*
	q1_mob_mm_nun_u /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, NUN spells 
	q1_mob_mmmy_u /// one quarter, unsmoothed, occ mobility YOUNG workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q1_mob_mmmp_u /// one quarter, unsmoothed, occ mobility PRIME-AGED workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q3_mob_mm_u /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q3_mob_mm_nun_u /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, NUN spells 
	q3_mob_mmmy_u /// 5 quarter rolling window smoothed, occ mobility YOUNG workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q3_mob_mmmp_u /// 5 quarter rolling window smoothed, occ mobility PRIME-AGED workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	qn1_mob_12_mm_u /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
	qn1u_mob_12_mm_u /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure unemployment spells only
	qn3_mob_12_mm_u /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
	qn3u_mob_12_mm_u  /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure unemployment spells only	
	q1_mob_mm_u /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	*/ ///
			/* WITH UNIFORM DURDISTR_RESTRICTION */ ///
	q1_mob_mm /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q1_mob_mm_nun /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, NUN spells 
	/*
	q1_mob_mmmy /// one quarter, unsmoothed, occ mobility YOUNG workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q1_mob_mmmp /// one quarter, unsmoothed, occ mobility PRIME-AGED workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	*/ ///
	q3_mob_mm /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q3_mob_mm_nun /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, NUN spells 
	/*
	q3_mob_mmmy /// 5 quarter rolling window smoothed, occ mobility YOUNG workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	q3_mob_mmmp /// 5 quarter rolling window smoothed, occ mobility PRIME-AGED workers, 2000 MOG, all durations (<=18m), GAMMA CORRECTED, pure unemployment spells only
	*/ ///
	qn1_mob_12_mm /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
	qn1u_mob_12_mm /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure unemployment spells only
	qn3_mob_12_mm /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
	qn3u_mob_12_mm  /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure unemployment spells only 
	lunrate_bls /// use tramo-seats e.g. for trend-cycle analysis, in a robustness regression. 
	lunrate_bls_0 ///
	lunrate ///
	loutpw ///
	loutpw2 ///
	/*  
	qn1_mob_14_mm /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=14m, GAMMA CORRECTED, pure unemployment spells only
	qn3_mob_14_mm /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=14m, GAMMA CORRECTED, pure unemployment spells only
	*/
	
export excel using "${outputdata}/occmob_ts_for_tramo.xlsx", sheet("all") firstrow(variables) nolabel replace

keep quarter ///
	qn1_mob_12_mm /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
	qn1u_mob_12_mm /// one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure unemployment spells only
	qn3_mob_12_mm /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
	qn3u_mob_12_mm  /// 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure 
	lunrate_bls // use tramo-seats e.g. for trend-cycle analysis, in a robustness regression. 
export excel using "${outputdata}/ts_occmob_paper_for_tramo.xlsx", sheet("all") firstrow(variables) nolabel replace


restore 

save "${outputdata}/occmob_ts_for_tramo.dta", replace 
	
	
/*

********************************************************************************
** 
**		TRAMO / SEATS USING TSW+  
**
********************************************************************************


********************************************************************************
NOTE: the only series that are directly relevant for the main text are
	- qn3_mob_12_mm
	- qn3u_mob_12_mm
	- qn1_mob_12_mm
	- qn1u_mob_12_mm
THE TRAMO/SEATS series are use in step2_10 and subsequently step2_11
IN step2_10, we further also need 
	- lunrate_bls (the TRAMO/SEATS trendcycle component, for robustness) 
We further need SEPARATIONS and JOB FINDING SERIES, 
	- ts_ljf_q 
	- q3_ljf_q
	- q3_ljfn_cnune_q 
	- q3_ljfn_cnune_instr_q 
	
	
IN step2_12_....do
	- unemp_cat0q
	- unemp_cat1q
	- unemp_cat2q
	- unemp_cat3q
	- unemp_cat4q
	
	
So, if interested in reproducing the results in the paper, one can simply restrict
oneself to these series. Here we focus on the first two series.
********************************************************************************



In brief, we use the standard tramo-seats options in the tsw program, with the 
automatic procedure parameter set to RSA=4,
then we save the TRAMO and SEATS output with the appropriate name (see below) 	

TRAMO: we have three series:
	-xorig series: original input, with -99999 for missing values
	-xint: interpolated series, but original values when non-interpolated
	-xlin: clear of deterministic variation. 	

														
** BY DEFAULT WE USE XLIN SERIES (that one is calendar adjusted, plus other deterministic issues)
(XINT is interpolated, but not adjusted for deterministic (seasonal) variation)

														
** SEATS (on top of TRAMO output) further decomposes the time series.
 - trend-cycle series: smooth series that captures both trend and cycle, but outliers and SA corrected
	this series is relevant as the quarter-by-quarter (unsmoothed) mobility is quite noisy
 
 
 
*** HOW TO USE TSW+ *****

1) Open/Run the program
2) Click the 'Series' button, this opens the standard 'Open' dialog in Windows.
	 - Go to the outputdata directory, and select occmob_ts_for_tramo.xlsx
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
	[series name].xlsx, in our example qn3_mob_12_mm.xlsx.
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
In our step 3 code directory (${step3codedir}), we have saved the before and 
after timeseries we are using in our paper, in subdirectory 
.\Tramo_Seats_series_REFERENCE\. This means that, when replicating, STATA can run 
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
********************************************************************************

**  					POST 			TRAMO 

********************************************************************************
********************************************************************************


	**** DONE IN STEP2_10 and STEP2_11 ***********


global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"