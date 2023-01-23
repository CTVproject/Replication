

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
	 
		Survival rates

	**************************************************************************** */

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
	version 13

	
	
	/*
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
	*/
	
		/*
	TWO MEASURES:

	 - most natural one (perhaps): measure at inflow. Survival 4m is the proportion of inflowing spells that will last more than four months.
	 - second measure, in line with other outflow measures, and when taking the overall sample all together, close to equivalent: the completed spells at outflow

	 - we calculate job finding rates from these survival rates. Note, choices about censoring have to be made, especially when thinking about job finding rates across mover and stayer spells (because they need to be completed in the data. 
	Note, the job finding rate in the SIPP might be too high for the completed spells measure, because attrition means that the number of completed spells in the cross section of the data is lower than the number of spells that could have been there with infinite panel lenght, since censored spells are absent. 
	
	*/


	/*
	capture drop u_in_n_spellength
	gen u_in_n_spellength=1 if n_spellength==1 & unempl_ctv==1
	replace u_in_n_spellength=0 if n_spellength==1 & unempl_ctv==0
	forvalues i=1(1)24{
	replace u_in_n_spellength=u_in_n_spellength[_n-1] if n_spellength>1 & unempl_ctv==0  $personmbfr & u_in_n_spellength[_n-1]!=. & n_spellength!=.
	replace u_in_n_spellength=u_in_n_spellength[_n-1]+1 if n_spellength>1 & unempl_ctv==1  $personmbfr & u_in_n_spellength[_n-1]!=. & n_spellength!=.
	}
	*/

	//====================================================
	//  PRELIMINARIES
	//====================================================


	** those who will be in the sample till the end

	version 13
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

		
	*********** SURVIVAL INDICATOR AT ***INFLOW***


	forvalues i=1(1)25 {
	capture drop usurv_ind_`i'm
	capture drop nsurv_ind_`i'm
	capture drop u_in_nunsurv_ind_`i'm
	capture drop nunsurv_ind_`i'm


	gen byte usurv_ind_`i'm=1 if u_spellength==1 & max_ulength>=`i' & max_ulength!=.
	gen byte nsurv_ind_`i'm=1 if n_spellength==1 & max_nlength>=`i' & max_nlength!=.
	gen byte nunsurv_ind_`i'm=1 if n_spellength==1 & max_nunlength>=`i' & max_nunlength!=.
	gen byte u_in_nunsurv_ind_`i'm=1 if n_spellength==1 & max_u_in_nlength>=`i' & max_u_in_nlength!=.

	** check the way the zeros are assigned: here when the person is in sample i periods ahead, but found employment
	replace usurv_ind_`i'm=0 if u_spellength==1 & max_ulength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	replace nsurv_ind_`i'm=0 if n_spellength==1 & max_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	replace nunsurv_ind_`i'm=0 if n_spellength==1 & max_nunlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	replace u_in_nunsurv_ind_`i'm=0 if n_spellength==1 & max_u_in_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)

	}




	*** SURVIVAL INDICATOR OUTFLOW
	forvalues i=1(1)25 {
	capture drop usurvout_ind_`i'm
	capture drop nsurvout_ind_`i'm
	capture drop u_in_nunsurvout_ind_`i'm
	capture drop nunsurvout_ind_`i'm


	gen byte usurvout_ind_`i'm=1 if lue_ctv==1 & u_spellength>=`i' 
	gen byte nsurvout_ind_`i'm=1 if lne_ctv==1 & n_spellength>=`i' 
	gen byte nunsurvout_ind_`i'm=1 if lne_ctv==1 & max_nunlength>=`i' & max_nunlength!=.
	gen byte u_in_nunsurvout_ind_`i'm=1 if lne_ctv==1 & max_u_in_nlength>=`i' & max_u_in_nlength!=.

	if `i'>1{
	local l=`i'-1
	forvalues j=1(1)`l'{
	local k=`i'-`j'
	replace usurvout_ind_`i'm=0 if lue_ctv==1 & u_spellength==`j' & personkey==personkey[_n+`k'] & yearmonth==yearmonth[_n+`k']-(`k')
	replace nsurvout_ind_`i'm=0 if lne_ctv==1 & max_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	replace nunsurvout_ind_`i'm=0 if lne_ctv==1 & max_nunlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	replace u_in_nunsurvout_ind_`i'm=0 if lne_ctv==1 & max_u_in_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	}
	}
	}



	*** SURVIVAL INDICATOR OUTFLOW (conditioning on being in sample for 32 (or alternative...) months after start spell)

	forvalues i=1(1)25 {
	capture drop usurvout2_ind_`i'm
	capture drop nsurvout2_ind_`i'm
	capture drop u_in_nunsurvout2_ind_`i'm
	capture drop nunsurvout2_ind_`i'm


	gen byte usurvout2_ind_`i'm=.
	gen byte nsurvout2_ind_`i'm=.
	gen byte nunsurvout2_ind_`i'm=.
	gen byte u_in_nunsurvout2_ind_`i'm=.


	forvalues j=`i'(1)25 {
	replace usurvout2_ind_`i'm=1 if lue_ctv==1 & u_spellength==`j' 
	replace nsurvout2_ind_`i'm=1 if lne_ctv==1 & n_spellength>=`i' 
	replace nunsurvout2_ind_`i'm=1 if lne_ctv==1 & max_nunlength>=`i' & max_nunlength!=.
	replace u_in_nunsurvout2_ind_`i'm=1 if lne_ctv==1 & max_u_in_nlength>=`i' & max_u_in_nlength!=.
	}

	if `i'>1{
	local l=`i'-1
	forvalues j=1(1)`l' {
	local k=`i'-`j'
	replace usurvout2_ind_`i'm=0 if lue_ctv==1 & u_spellength==`j' & personkey==personkey[_n+32-`j'] & yearmonth==yearmonth[_n+`k']-(`k')
	replace nsurvout2_ind_`i'm=0 if lne_ctv==1 & max_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	replace nunsurvout2_ind_`i'm=0 if lne_ctv==1 & max_nunlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	replace u_in_nunsurvout2_ind_`i'm=0 if lne_ctv==1 & max_u_in_nlength<`i' & personkey==personkey[_n+`i'-1] & yearmonth==yearmonth[_n+`i'-1]-(`i'-1)
	}
	}
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


		*cap n putexcel set "C:\data\jfflows_surv_summstats2_dec19.xls", sheet("SURV2")
		*cap n putexcel set "C:\data\jfflows_surv_summstats2_dec19.xls", sheet("SURV2") modify 

		
	cap n putexcel set "${outputdata}/survivalprofile.xls", sheet("SURV")
	cap n putexcel set "${outputdata}/survivalprofile.xls", sheet("SURV") modify 

		
	putexcel A1=("name")

	forvalues i=1(1)24 {
	local letterin=`i'+3
	local letterin2=`i'+27
	excel_col_exe `letterin'
	local letterout=r(xlscol)
	excel_col_exe `letterin2'
	local letterout2=r(xlscol)

	putexcel `letterout'1=("surv_m`i'")
	putexcel `letterout2'1=("obs_surv_m`i'")

	}


	excel_col_exe 51
	local letterout=r(xlscol)
	putexcel `letterout'1=("command")

	global pos=2


	//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
	//------------------------------------------------------------------------------
	//     calculate survival rates
	//------------------------------------------------------------------------------
	//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

							capture program drop surv_sumstats_calc_exe
							program define surv_sumstats_calc_exe
													args surv_ind  filename  
									local survvarname "surv_`filename'"
									sort personkey yearmonth
									putexcel A${pos}=("`survvarname'")
									
									
									forvalues i=1(1)24 {
									local letterin=`i'+3
									local letterin2=`i'+27
									excel_col_exe `letterin'
									local letterout=r(xlscol)
									excel_col_exe `letterin2'
									local letterout2=r(xlscol)
									
									global f_ind=`i'
									display "`survvarname', duration `i'"
									su  `surv_ind'_`i'm [aw=pweight2] if `surv_ind'_`i'm!=. & `surv_ind'_1m==1 & ${sumstats_surv_addcondition} $subset_cond 
									   // from: su  nunsurv_ind_5m if nunsurv_ind_1m==1 & nunsurv_ind_5m!=. & complete_nunspell==1 & sample_timetogo>=24 & compl_nundur<=24
										// jf: su `jf_ind' [aw=pweight2] if (personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1) & ${sumstats_jf_addcondition} $subset_cond 
									local command "su  `surv_ind'_`i'm [aw=pweight2] if `surv_ind'_`i'm!=. & `surv_ind'_1m==1 & ${sumstats_surv_addcondition} $subset_cond   "
									putexcel `letterout'${pos}=(r(mean))
									putexcel `letterout2'${pos}=(r(sum_w))

									}

									excel_col_exe 51
									local letterout=r(xlscol)
									putexcel `letterout'${pos}=("`command'")


									global pos=${pos}+1
									
							end 

							
// some draft calculations

/*
//=================================
** AVE JF
//=================================
/*
FOR INCOMPLETE SPELLS, we must make sure that we have a spell distribution that includes long spells with their lower job finding rate
FOR COMPLETE SPELLS, we have censoring issues on both sides: we need to make sure that we include long spells, and we need to measure far away from the end of sample 
(because spells have to be completed before the end of sample, to be included in the measurement, which can create an UPWARDS bias)
FOR THIS REASON: JFOCC and JFNOCC statistics perhaps should be targeted as ratios!!! not as levels...

ALSO WHEN DERIVING JF RATES FROM THE SURVIVAL FUNCTIONS, MORE DURATION INFORMATION IS USED, SO THAT MIGHT BE A BETTER WAY OF CALCULATING AVERAGE JF RATES 
*/
sort personkey yearmonth
** this one yields 0.175
su lue [aw=pweight2] if (incomplete_uspell==1 | complete_uspell==1) & wave>4 & interview_no2>=16 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
su lue [aw=pweight2] if (incomplete_uspell==1 | complete_uspell==1) & wave>4 & interview_no2>=16 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
** more strict on including lt spells, but emphasizes 2008 panel
su lue [aw=pweight2] if (incomplete_uspell==1 | complete_uspell==1) & wave>6 & interview_no2>=24 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
** this one also yields 0.175, incl. lt spells, but restricts impact of 2008 panel
su lue [aw=pweight2] if (incomplete_uspell==1 | complete_uspell==1) & wave>6 & wave<=8 & interview_no2>=24 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1

** complete spells jf rates are higher, but not enough length to really rule out the upwards bias. 
su lue [aw=pweight2] if ( complete_uspell==1) & wave>6 & interview_no2>=24 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & sample_timetogo>=18
su lue [aw=pweight2] if ( complete_uspell==1) & wave>6 & wave<=8 & interview_no2>=24 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & sample_timetogo>=18
su lue [aw=pweight2] if ( complete_uspell==1) & wave>4 & interview_no2>=16 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & sample_timetogo>=18
su lue [aw=pweight2] if ( complete_uspell==1) & wave>4 & interview_no2>=16 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & sample_timetogo>=24
su lue [aw=pweight2] if ( complete_uspell==1) & wave>4 & wave<=6 & interview_no2>=16 & (empl_ctv[_n+1]==1 | unempl_ctv[_n+1]==1) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & sample_timetogo>=24
*/

** SURVIVAL PROFILE
global subset_cond "" 
// DEFAULT FOR CALIBRATION
global pos=2
// CALIBRATION*******
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=4 & entry_ind==1"
surv_sumstats_calc_exe usurv_ind au_inflsttg32_w4_test
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=24 & wave<=6 & entry_ind==1"
surv_sumstats_calc_exe usurv_ind au_inflsttg24_w6_test
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=6 & entry_ind==1"
surv_sumstats_calc_exe usurv_ind au_inflsttg32_w6_test
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=24 & wave<=4 & entry_ind==1"
surv_sumstats_calc_exe usurv_ind au_inflsttg24_w4_test

global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=24 & wave<=6 & entry_ind==1 & tage>=20 & tage<=30"
surv_sumstats_calc_exe usurv_ind yu_inflsttg24_w6_test
// CALIBRATION*******
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=4 & entry_ind==1 & tage>=20 & tage<=30"
surv_sumstats_calc_exe usurv_ind yu_inflsttg32_w4_test
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=24 & wave<=6 & entry_ind==1 & tage>=35 & tage<=55"
surv_sumstats_calc_exe usurv_ind pu_inflsttg24_w6_test
// CALIBRATION*******
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=4 & entry_ind==1 & tage>=35 & tage<=55"
surv_sumstats_calc_exe usurv_ind pu_inflsttg32_w4_test

// CALIBRATION*******
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=4 & entry_ind==1 & sex==1"
surv_sumstats_calc_exe usurv_ind amu_inflsttg32_w4_test
// CALIBRATION*******
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=4 & entry_ind==1 & tage>=20 & tage<=30 & sex==1"
surv_sumstats_calc_exe usurv_ind ymu_inflsttg32_w4_test
// CALIBRATION*******
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=4 & entry_ind==1 & tage>=35 & tage<=55 & sex==1"
surv_sumstats_calc_exe usurv_ind pmu_inflsttg32_w4_test


gsort personkey -yearmonth
capture drop umove_mmo_ind
gen umove_mmo_ind=lue_c_mmo if lue_c_mmo!=.
replace umove_mmo_ind=umove_mmo_ind[_n-1] if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & unempl_ctv==1 & umove_mmo_ind==.
sort personkey yearmonth

** COMPLETE SPELLS
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4"
surv_sumstats_calc_exe usurv_ind all_inflsttg32_w4_cu


** COMPLETE SPELLS MOVERS
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4 & umove_mmo_ind==1"
surv_sumstats_calc_exe usurv_ind all_inflsttg32_w4_cumov

** COMPLETE SPELLS STAYERS 
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4 & umove_mmo_ind==0"
surv_sumstats_calc_exe usurv_ind all_inflsttg32_w4_custay


// YOUNG 
** COMPLETE SPELLS
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4  & tage>=20 & tage<=30"
surv_sumstats_calc_exe usurv_ind yu_inflsttg32_w4_cu


** COMPLETE SPELLS MOVERS
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4 & umove_mmo_ind==1 & tage>=20 & tage<=30"
surv_sumstats_calc_exe usurv_ind yu_inflsttg32_w4_cumov

** COMPLETE SPELLS STAYERS 
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4 & umove_mmo_ind==0 & tage>=20 & tage<=30"
surv_sumstats_calc_exe usurv_ind yu_inflsttg32_w4_custay



// PRIME
** COMPLETE SPELLS
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4   & tage>=35 & tage<=55 "
surv_sumstats_calc_exe usurv_ind pu_inflsttg32_w4_cu


** COMPLETE SPELLS MOVERS
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4 & umove_mmo_ind==1 & tage>=35 & tage<=55 "
surv_sumstats_calc_exe usurv_ind pu_inflsttg32_w4_cumov

** COMPLETE SPELLS STAYERS 
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=32 & wave<=4 & umove_mmo_ind==0 & tage>=35 & tage<=55 "
surv_sumstats_calc_exe usurv_ind pu_inflsttg32_w4_custay



global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=36 & wave<=5"
surv_sumstats_calc_exe usurv_ind all_inflsttg36_w3
***===> THIS MIGHT BE A GOOD STARTING POINT FOR COMPLETE SPELLS 
global sumstats_surv_addcondition " (complete_uspell==1) & sample_timetogo>=36"
surv_sumstats_calc_exe usurv_ind all_inflsttg36_w4_cu


global sumstats_surv_addcondition " (incomplete_nunspell==1 | complete_nunspell==1) & sample_timetogo>=32 & wave<=4"
surv_sumstats_calc_exe nunsurv_ind anun_inflsttg32_w4_test
global sumstats_surv_addcondition " (incomplete_nunspell==1 | complete_nunspell==1) & sample_timetogo>=32 & wave<=4 & tage>=20 & tage<=30"
surv_sumstats_calc_exe nunsurv_ind ynun_inflsttg32_w4_test
global sumstats_surv_addcondition " (incomplete_nunspell==1 | complete_nunspell==1) & sample_timetogo>=32 & wave<=4 & tage>=35 & tage<=55"
surv_sumstats_calc_exe nunsurv_ind pnun_inflsttg32_w4_test



// NUN-CALIBRATION********
global sumstats_surv_addcondition " (incomplete_nunspell==1 | complete_nunspell==1) & sample_timetogo>=32 & wave<=4 & sex==1"
surv_sumstats_calc_exe nunsurv_ind amnun_inflsttg32_w4_test
// NUN-CALIBRATION********
global sumstats_surv_addcondition " (incomplete_nunspell==1 | complete_nunspell==1) & sample_timetogo>=32 & wave<=4 & tage>=20 & tage<=30 & sex==1"
surv_sumstats_calc_exe nunsurv_ind ymnun_inflsttg32_w4_test
// NUN-CALIBRATION********
global sumstats_surv_addcondition " (incomplete_nunspell==1 | complete_nunspell==1) & sample_timetogo>=32 & wave<=4 & tage>=35 & tage<=55 & sex==1"
surv_sumstats_calc_exe nunsurv_ind pmnun_inflsttg32_w4_test


sleep 100

**************************************
** REPORT SERIES USED IN CALIBRATION
**************************************
version 13
import excel "${outputdata}/survivalprofile.xls", sheet("SURV") firstrow clear


keep if (name == "surv_au_inflsttg32_w4_test" | name == "surv_yu_inflsttg32_w4_test" | name == "surv_pu_inflsttg32_w4_test" ///
		 | name == "surv_all_inflsttg32_w4_cu" | name == "surv_all_inflsttg32_w4_cumov" | name == "surv_all_inflsttg32_w4_custay" ///
		 | name == "surv_yu_inflsttg32_w4_cu" | name == "surv_yu_inflsttg32_w4_cumov" | name == "surv_yu_inflsttg32_w4_custay" ///
		 | name == "surv_pu_inflsttg32_w4_cu" | name == "surv_pu_inflsttg32_w4_cumov" | name == "surv_pu_inflsttg32_w4_custay" ///
		 | name == "surv_amnun_inflsttg32_w4_test" | name == "surv_ymnun_inflsttg32_w4_test" | name == "surv_pmnun_inflsttg32_w4_test")
		 
capture drop B
capture drop C
keep name surv*
reshape long surv_m, i(name) j(month)
ren surv_m s
reshape wide s, i(month) j(name) string
ren ssurv* surv*
ren *inflsttg32_w4_* *_*
ren *__test *
ren *__* *_*



** calculate implied job finding rates
capture program drop jfcalc_exe
program define jfcalc_exe
	args name
	
sort month
capture drop hazprofile_`name'
gen hazprofile_`name'=1-surv_`name'[_n+1]/surv_`name'
	
end program 

jfcalc_exe au 
jfcalc_exe pu 
jfcalc_exe yu


jfcalc_exe all_cu 
jfcalc_exe all_cumov 
jfcalc_exe all_custay 

jfcalc_exe yu_cu 
jfcalc_exe yu_cumov 
jfcalc_exe yu_custay 

jfcalc_exe pu_cu 
jfcalc_exe pu_cumov 
jfcalc_exe pu_custay 


jfcalc_exe amnun 
jfcalc_exe pmnun 
jfcalc_exe ymnun 

** construct implied incomplete hazards for movers and stayers
	/* we do this because censoring has much more of a bite for the hazard of
	movers and stayers, as we need to see their spell finish
	so we use the relative job finding rate of stayers and movers at durations
	derived above, but make sure the overall level of job finding equals the level of
	job finding of the incomplete spells   */

** ALL WORKERS
	
capture drop hazprofile_au_move_inferred
capture drop hazprofile_au_stay_inferred

gen hazprofile_au_move_inferred=(hazprofile_au/hazprofile_all_cu)*hazprofile_all_cumov
gen hazprofile_au_stay_inferred=(hazprofile_au/hazprofile_all_cu)*hazprofile_all_custay	

capture drop surv_au_move_inferred
capture drop surv_au_stay_inferred

gen surv_au_move_inferred=1 if month==1
gen surv_au_stay_inferred=1 if month==1

replace surv_au_move_inferred=surv_au_move_inferred[_n-1]*(1-hazprofile_au_move_inferred[_n-1]) if month>1
replace surv_au_stay_inferred=surv_au_stay_inferred[_n-1]*(1-hazprofile_au_stay_inferred[_n-1]) if month>1

** YOUNG
capture drop hazprofile_yu_move_inferred
capture drop hazprofile_yu_stay_inferred

gen hazprofile_yu_move_inferred=(hazprofile_yu/hazprofile_yu_cu)*hazprofile_yu_cumov
gen hazprofile_yu_stay_inferred=(hazprofile_yu/hazprofile_yu_cu)*hazprofile_yu_custay	

capture drop surv_yu_move_inferred
capture drop surv_yu_stay_inferred

gen surv_yu_move_inferred=1 if month==1
gen surv_yu_stay_inferred=1 if month==1

replace surv_yu_move_inferred=surv_yu_move_inferred[_n-1]*(1-hazprofile_yu_move_inferred[_n-1]) if month>1
replace surv_yu_stay_inferred=surv_yu_stay_inferred[_n-1]*(1-hazprofile_yu_stay_inferred[_n-1]) if month>1



** PRIME AGED 
capture drop hazprofile_pu_move_inferred
capture drop hazprofile_pu_stay_inferred

gen hazprofile_pu_move_inferred=(hazprofile_pu/hazprofile_pu_cu)*hazprofile_pu_cumov
gen hazprofile_pu_stay_inferred=(hazprofile_pu/hazprofile_pu_cu)*hazprofile_pu_custay	

capture drop surv_pu_move_inferred
capture drop surv_pu_stay_inferred

gen surv_pu_move_inferred=1 if month==1
gen surv_pu_stay_inferred=1 if month==1

replace surv_pu_move_inferred=surv_pu_move_inferred[_n-1]*(1-hazprofile_pu_move_inferred[_n-1]) if month>1
replace surv_pu_stay_inferred=surv_pu_stay_inferred[_n-1]*(1-hazprofile_pu_stay_inferred[_n-1]) if month>1


** drop completed spell information
drop *cu*
** drop months>20
drop if month>20


lab var surv_amnun "Survival Profile Males (All) in NUN spells"
lab var surv_au "Survival Profile All Workers in (pure) U spells"
lab var surv_pmnun "Survival Profile Prime-aged Males in NUN spells"
lab var surv_pu "Survival Profile Prime-Aged Workers in (pure) U spells"
lab var surv_ymnun "Survival Profile Young Males in NUN spells"
lab var surv_yu "Survival Profile Yong Workers in (pure) U spells"

lab var hazprofile_amnun "JF Hazard Profile Males in NUN spells"
lab var hazprofile_au "JF Hazard Profile (All) in (pure) U spells"
lab var hazprofile_pmnun "JF Hazard  Profile Prime-aged Males in NUN spells"
lab var hazprofile_pu "JF Hazard  Profile Prime-Aged in (pure) U spells"
lab var hazprofile_ymnun "JF Hazard  Profile Young Males in NUN spells"
lab var hazprofile_yu "JF Hazard  Profile Young in (pure) U spells"


lab var hazprofile_au_move_inferred "Inferred JF Hazard (ex post) Occ Movers (all workers)"
lab var hazprofile_au_stay_inferred "Inferred JF Hazard (ex post) Occ Stayer (all workers)"
lab var hazprofile_yu_move_inferred "Inferred JF Hazard (ex post) Occ Movers (young)"
lab var hazprofile_yu_stay_inferred "Inferred JF Hazard (ex post) Occ Stayer (young)"
lab var hazprofile_pu_move_inferred "Inferred JF Hazard (ex post) Occ Movers (prime-aged)"
lab var hazprofile_pu_stay_inferred "Inferred JF Hazard (ex post) Occ Stayer (prime-aged)"

lab var surv_au_move_inferred "Survival Profile (ex post) Occ Movers (all workers, inferred)"
lab var surv_au_stay_inferred "Survival Profile (ex post) Occ Stayers (all workers, inferred)"
lab var surv_yu_move_inferred "Survival Profile (ex post) Occ Movers (young, inferred)"
lab var surv_yu_stay_inferred "Survival Profile (ex post) Occ Stayers (young, inferred)"
lab var surv_pu_move_inferred "Survival Profile (ex post) Occ Movers (prime-aged, inferred)"
lab var surv_pu_stay_inferred "Survival Profile (ex post) Occ Stayers (prime-aged, inferred)"

save "${mainresultsdir}/survivalprofiles_and_hazards.dta", replace

export excel using "${mainresultsdir}/survivalprofiles_and_hazards.xls", firstrow(varlabels) replace





********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"


