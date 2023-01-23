
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



//======================================
//  SIPP 1984 PANEL, LOAD IN DATA, DEFINE VARIABLES FOR USE
//======================================


		********************************************************************************************************************************************************
		** PREAMBLE
		*********************************************************************************************************************************************************

		clear
		capture log close
		*set mem 3000m
		set more off
		*set maxvar 10000


		** GLOBALS
		global gnulin=0
		* control parts of the do-file
		global core_if=0	// read in all data from addition core waves
		global tm_if=1	       // read in the topical modules
		global read_in_data=1
		global weights_if=1   // loads in the longitudinal weights
		global subset_if=0   // starts reducing datasets into subsets with relevant variables for different projects
		global list=0
		global fp_ind=1
		
		
// read in different panels
		global panel=1984
		global yeardata "84"
		global locfileversion "1016"
		

set varabbrev off
		
		


		*********************************************************************************************
		** DATE AND TIME OF RUN
		***********************************************************************************************8
		display c(current_date)
		display c(current_time)
		
		
		
		**************************************************************************************************
		** AUXILIARY PROGRMA
		****************************************************************************************************

		
					capture program drop capturedrop
					program define capturedrop
					
					local i=1
					while "``i''"!="" {

					set more off
					set varabbrev off

					local dropvar  "``i''"
					display "`dropvar'"
					capture noisily drop `dropvar'
					local ++i
					}
					end program
					
								
		capture program drop idfillout
		program define idfillout
		
			capture drop prs_`1'
			by personkey: egen prs_`1'=max(`1')
		end program idfillout 
			
		capture program drop idfillreplace
		program define idfillreplace
		
			capture drop prs_`1'
			ren `1' prs_`1'
			sort personkey
			by personkey: egen `1'=max(prs_`1')
			drop prs_`1'
		end program idfillreplace 
	
		
		********
		** SELECTION OF THE VARIABLES
		********

		if $read_in_data==0 {
							cd "$outputdata"
							use ${panel}_v${fileversion}.dta, clear
							*use ${panel}_v1016.dta, clear
		}
		
		if $read_in_data==1 {

		cd "$outputdata"
		
		use ${panel}total_v${fileversion}.dta, clear



		// make consistent with 1990 panel
		ren c_strat core_gvarstrat 
		
		display "no intvw_tm2: interview status recorded for wave 2 for both tm adn core wave, prob"
		capture gen intvw_tm2=intvw
		
		
		ren eeno1 eeno1_old
		ren eeno2 eeno2_old
		
		gen eeno1=.
		gen eeno2=.
		
		capture noisily ren ws1_2003 ws12003
		capture noisily ren ws2_2103 ws22103
		capture noisily ren ws1_wks ws1wks
		capture noisily ren ws2_wks ws2wks
		capture noisily ren ws1wksx_ ws1wks
		capture noisily ren ws2wksx_ ws2wks
		capture noisily ren iretir iretird 
		*capture noisily ren iws12014 isw12024
		cap n ren pp_intvw intvw
		capture n ren ws1_* ws1*
		capture n ren ws2_* ws2*
		capture n gen ws12003=estlemp1
		capture no gen ws22103=estlemp2
		cap n gen imp_startprevjb_yr=imp_startprevjbyr_tm 
		cap n gen byte imp_startprevjb_mth=.
		cap n gen byte imp_endprevjb_yr=imp_endprevjbyr_tm 
		cap n gen byte imp_endprevjb_mth=.  
		
		
		
		
		// summarize variables
			cap n describe personkey wpfinwgt hsc core_gvarstrat  panel // PERSON IDENTIFIERS, weights, halfsample codes; left out: strat 
		   cap n describe  wave yearmonth srefmon rot pp_intv intvw intvw_tm2 pp_mis // dates, times, rotation, refmonth, interview status, etc 
		   cap n describe educ higrade sex race tage ms c_tfipsst ethnicty eafnow // person characteristics; left out: educ_tm2
		   cap n describe eeno1_old eeno1 eeno2_old eeno2 fp_eeno1 fp_eeno2 ws12014 ws22114 ws1wks ws2wks c_ejbhrs1 c_ejbhrs2 // firm identifiers & properties
												// note: there is no direct link, based on firm identifiers between the retrospective topical module and the core wave variables; left out: eeno1_startpanel eeno2_startpanel 
												// there is no change of activity or still with employer (ws12003/ws22103); there is no stopjob1/2
		   cap n describe tsjdate1 tsjdate2 sday1 smonth1 sday2 smonth2 // startdates 
		   cap n describe tsjdate_startpanel imp_tsjdate_startpanel_yr imp_tsjdate_startpanel_mth startprevjbyr_tm imp_startprevjb_yr imp_startprevjb_mth // start dates from topical module
		   cap n describe startprevjb_tm 
		   cap n describe tejdate1 tejdate2 eday1 emonth1 eday2 emonth2 // enddates of jobs 
		   cap n describe endprevjb_tm endprevjbyr_tm imp_endprevjb_yr imp_endprevjb_mth  // enddates of jobs according to topical module
		   cap n describe fp_tjbocc1 fp_tjbocc2 occprevjb imp_occ c_tjbocc1 iws1occ c_tjbocc2 iws2occ // occupations
		   cap n describe fp_ejbind1 fp_ejbind2 indprevjb imp_ind c_ejbind1 iws1ind c_ejbind2 iws2ind // industries
		   cap n describe fp_rmesr c_rmesr rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5	// employment status during panel 
		   cap n describe tm8234 tm8236  // retrospective question about time in job
		   cap n describe firstyearlf alwayswork6m years6mworked notworked6m timesnotworked6m recentbreak6mfr recentbreak6mto rsnbreak6m rsnendprevjb //
		   //, and time in labour force: left out: tm8210, tm8218 tm8220 tm8240 tm8242  tm8248 tm8250  tm8252  tm8268  tm8270   tm8280 tm8284  
		   cap n describe imp_8234 imp_8274 imp_8276 imp_8278 imp_8272 // and its imputation flags; no imp_829a: left out: su imp_8282  		   asg_8286 imp_8288 imp_8290 imp_8292 imp_8294  
		   cap n describe  iws22112 idisab ise12260  ireasab itakjob iretird  iwksptr iws22124 ise1ind ise2ind //
		   cap n describe iws1ind iws2ind iws1occ ise1occ iws2occ ise2occ iwksjob iwkswop iwkslok itakjobn iws12012 iws12024	//
		   cap n describe fp_eclwrk1 fp_eclwrk2 c_eclwrk1 c_eclwrk2 // left out: eunion1 eunion2 ecntrc1 ecntrc2	in core wave
		   /* in 1984 panel, not included so far for analysis below */
		   cap n describe tm8166 tm8168 tm8170 tm8172 tm8174 // topical module 3, properties of job (no. workers/ unionized etc.)
		   cap n describe tmpay_hr tmpay_wk tmpay_mth tmpay_yr tm_hrswk  imp_tmpay  // topical module 3, properties of job: wage, hours 
		   cap n describe tmpay_prevjb_hr tmpay_prevjb_wk tmpay_prevjb_mth tmpay_prevjb_yr imp_tmpay_prevjb  // topical module 3, properties of previous job
			cap n describe tm8160 tm8162 tm8164  // controls wrt to having job/ SE or PE
			cap n describe tmimp41 tmimp42  // original flags for imputation of curjb tenure; recoded imputation indicator variable is used instead
			cap n describe tmimp67 tmimp71 tmimp75 tmimp79 
			/* INCLUDE FROM 1984 */
			cap n describe tm2_markedworking tm2_curjbse yearlastworked rsno2wkwrk 
			cap n describe tmcurjb_tenure_yr tmcurjb_tenure_mth 
			cap n describe tmcurjb_ongoing tmcurjb_rsnend tm_ageover21 tmcurjb_10yrless tm_hadprevjb 
			cap n describe prevjbse startprevjbyr_tm endprevjbyr_tm 
			cap n describe tm_empgap_weeks tm_empgap_months tm_empgap_years tm_noempgap empgap 
			cap n describe tm_part_fulltime_overall firstbreak6mfr firstbreak6mto rsnfirstbreak6m 
			cap n describe secondbreak6mfr secondbreak6mto rsnsecondbreak6m thirdbreak6mfr thirdbreak6mto rsnthirdbreak6m fourthbreak6mfr fourthbreak6mto rsnfourthbreak6m 
			/* include imputation indicators from 1984 */
			cap n describe imp_8234 imp_8246 imp_tm_empgap imp_tmcurjb_rsnend imp_prevjbse  // imputations of occupations and imputation of rsn2wknowrk
			cap n describe imp_tm_part_fulltime_overall imp_firstbreak6mfr imp_firstbreak6mto imp_rsnfirstbreak6m imp_secondbreak6mfr imp_secondbreak6mto 
			cap n describe imp_rsnsecondbreak6m imp_thirdbreak6mfr imp_thirdbreak6mto imp_rsnthirdbreak6m imp_fourthbreak6mfr imp_fourthbreak6mto imp_rsnfourthbreak6m
	
		// keep variables
		keep personkey wpfinwgt hsc core_gvarstrat  panel /* PERSON IDENTIFIERS, weights, halfsample codes 
		   */ wave yearmonth srefmon rot pp_intv intvw intvw_tm2 pp_mis /* dates, times, rotation, refmonth, interview status, etc 
		   */ educ higrade sex race tage ms c_tfipsst ethnicty eafnow /* person characteristics 
		   */ eeno1_old eeno1 eeno2_old eeno2 fp_eeno1 fp_eeno2 ws12014 ws22114 ws1wks ws2wks c_ejbhrs1 c_ejbhrs2 /* firm identifiers & properties
		   */ tsjdate1  tsjdate2 sday1 smonth1 sday2 smonth2 /* startdates 
		   */ tsjdate_startpanel imp_tsjdate_startpanel_yr imp_tsjdate_startpanel_mth startprevjbyr_tm imp_startprevjb_yr imp_startprevjb_mth  /* start dates from topical module
		   startprevjb_tm 
		   */ tejdate1 tejdate2 eday1 emonth1 eday2 emonth2 /* enddates of jobs 
		   */ endprevjb_tm endprevjbyr_tm imp_endprevjb_yr imp_endprevjb_mth  /* enddates from topical module
		   */ fp_tjbocc1 fp_tjbocc2 occprevjb imp_occ c_tjbocc1 iws1occ c_tjbocc2 iws2occ /* occupations
		   */ occprevjb_nocurrentjb  imp_occprevjb_nocurrentjb /* // only reported if there is NO current job in the work history TM 
		   */ indprevjb_nocurrentjb  imp_indprevjb_nocurrentjb /* // only reported if there is NO current job in the work history TM 
		   */ fp_ejbind1 fp_ejbind2 indprevjb imp_ind c_ejbind1 iws1ind c_ejbind2 iws2ind /* industries
		   */ fp_rmesr c_rmesr rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5	/* employment status during panel 
		   */ tm8234 tm8236  /* retrospective question about time in job
		   */ firstyearlf alwayswork6m years6mworked notworked6m timesnotworked6m recentbreak6mfr recentbreak6mto rsnbreak6m rsnendprevjb /*
		   */ /* tm8210 tm8218 tm8220 tm8240 tm8242 tm8248 tm8250 tm8252 tm8268 tm8270  tm8280 tm8282 tm8284  */ /*, and time in labour force
		   */ imp_8234 imp_8274 imp_8276 imp_8278 imp_8272 imp_8290 imp_8292 imp_8294 /* and its imputation flags; no imp_829a imp_8282 asg_8286 imp_8288 
		   */  iws22112 idisab ise12260  ireasab itakjob iretird  iwksptr iws22124 ise1ind ise2ind /*
		   */ iws1ind iws2ind iws1occ ise1occ iws2occ ise2occ iwksjob iwkswop iwkslok itakjobn iws12012 iws12024	/*
		   */ fp_eclwrk1 fp_eclwrk2 c_eclwrk1 c_eclwrk2 /* eunion1 ecntrc1 eunion2 ecntrc2	
		   */ c_se_wks1 c_ehrsbs1 c_se_wks2 c_ehrsbs2 /* SE
			included from 1984 */ /*
			*/ tm2_markedworking tm2_curjbse yearlastworked rsno2wkwrk /*
			*/ tmcurjb_tenure_yr tmcurjb_tenure_mth /*
			*/ tmcurjb_ongoing tmcurjb_rsnend tm_ageover21 tmcurjb_10yrless tm_hadprevjb /*
			*/ prevjbse startprevjbyr_tm endprevjbyr_tm /*
			*/ tm_empgap_weeks tm_empgap_months tm_empgap_years tm_noempgap empgap /*
			*/ tm_part_fulltime_overall firstbreak6mfr firstbreak6mto rsnfirstbreak6m /*
			*/  secondbreak6mfr secondbreak6mto rsnsecondbreak6m thirdbreak6mfr thirdbreak6mto rsnthirdbreak6m fourthbreak6mfr fourthbreak6mto rsnfourthbreak6m /*
			*/ imp_startprevjbyr_tm imp_endprevjbyr_tm imp_tm_empgap imp_tmcurjb_rsnend imp_prevjbse  /* // imputations of occupations and imputation of rsn2wknowrk
			*/ imp_tm_part_fulltime_overall imp_firstbreak6mfr imp_firstbreak6mto imp_rsnfirstbreak6m imp_secondbreak6mfr imp_secondbreak6mto /*
			*/ imp_rsnsecondbreak6m imp_thirdbreak6mfr imp_thirdbreak6mto imp_rsnthirdbreak6m imp_fourthbreak6mfr imp_fourthbreak6mto imp_rsnfourthbreak6m /*
			*/ eeno2_startpanel eeno1_startpanel imp_startprevjb_yr imp_startprevjb_mth imp_endprevjb_yr imp_endprevjb_mth intvw_month tsjdate_startpanel retro_endprevjb_tm pp_wave /*
			*/ personkey yearmonth wave wavestart pp_mis pp_mis_core c_eclwrk1 fp_eclwrk1 fp_rmesr c_rmesr eeno1 eeno1_startpanel eeno2 eeno2_startpanel ///
				startprevjbyr_tm confirmed_endprevjb_ind endprevjb_tm endprevjbyr_tm tmcurjb_ongoing tmcurjb_tenure retro_endprevjb_tm ///
				empgap tsjdate1 tsjdate2 tsjdate_startpanel tejdate1 tejdate2 rot su_id pp_entry imp_tmcurjb_tenure imp_empgap occcurjb_ongoing_dum tm_minprevjb_tenure ///
				eenrlm eeveret rtakjob rnotake wantjob rsnnotlkg inotake itakjob irsnnotlkg ///
				fp_tpmsum1 fp_tpmsum2 c_tpmsum1 c_tpmsum2 fp_tpyrate1 fp_tpyrate2 c_tpyrate1 c_rpyper1 c_tpyrate2 c_rpyper2
	
			display "got rid of additional variables"
			* NJOBS is out
			capture noisily gen njobs=.
			capture noisily gen ws1chg=.
			capture noisily gen ws2chg=.
			
			gen ws1chg_4m=ws12014
			gen ws2chg_4m=ws22114

			notes ws1chg_4m: ws12014
			notes ws2chg_4m: ws22114
			
			gen byte imp_8266=imp_prevjbse
		
			notes imp_prevjbse: imp_8266
			
		save "1984_corewave_occmob.dta", replace
		
		
		**********
		** SOME HOUSEKEEPING
		**************

		
		compress

		drop if tage<18
			mvdecode firstyearlf alwayswork6m years6mworked notworked6m timesnotworked6m recentbreak6mfr recentbreak6mto rsnbreak6m ///
			imp_8274 imp_8276 imp_8278 imp_8290 imp_8292 imp_8294 eday1 eday2 sday1 sday2 emonth1 emonth2 smonth1 smonth2, mv(0)
		
		drop if tage>66
		
		** drop non-interviews in core/fp
		drop if pp_intv>2
		
		mvdecode fp_tjbocc* eeno1_old eeno2_old fp_eeno*, mv(0)
		*save ${panel}_retro.dta, replace

		* imputed set to missing if not imputed, 1=imputed
		mvdecode imp*, mv(0)
		mvdecode iws*, mv(0)
		
		mvdecode ws1chg, mv(0)
		mvdecode ws2chg, mv(0)
		
		*** employer changes?
		/*
		mvdecode ws12003 ws22103 stopjob1 stopjob2 ws12004 ws22104 , mv(0)
		format %6.0g ws12003 ws22103 stopjob1 stopjob2 ws1chg ws2chg
		format %4.0g fp_eeno1 fp_eeno2 wave tage
		format %8.0g eeno1_old eeno2_old eeno1 eeno2
		capture label define ws12003l2 1 "emp.prev wv" 2 "e.not.prev.wv"
		lab val ws12003 ws12003l2 
		lab val ws22103 ws12003l2 
		capture lab var ws12025 "hours firm 1"
		capture lab var ws22125 "hours firm 2"
		*/
		
		* shrink column width
		format %24.0g pp_intv
		format %3.0f c_tjbocc1
		format %3.0f c_tjbocc2
		format %2.0f eday1
		format %2.0f eday2
		format %2.0f sday1
		format %2.0f sday2
		format %2.0f eeno*
		format %2.0f emonth1
		format %2.0f emonth2
		format %2.0f smonth1
		format %2.0f smonth2
		format %5.0f rwkesr1
		format %5.0f rwkesr2
		format %5.0f rwkesr3
		format %5.0f rwkesr4
		format %5.0f rwkesr5
		label define rwkesrl 1 `"job"', modify
		label define rwkesrl 2 `"job-abs"', modify
		label define rwkesrl 3 `"job, laidoff/look"', modify
		label define rwkesrl 5 `"no job/not look"', modify
		label define rwkesrl 4 `"no job,look/layoff"', modify
		format %5.0f ws1wks
		format %5.0f ws2wks
		label define ws1wks 0 `"None/NiU"', modify
		label define ws2wks 0 `"None/NiU"', modify

		
		*FP/CORE Employment status, basic
		capture drop c_enu
		capture drop fp_enu
		gen c_enu=.
		gen fp_enu=.
		capture label define enu_label 1 "E" 2 "U" 3 "N"
		lab val c_enu enu_label
		label val fp_enu enu_label
		format %2.0g c_enu
		format %2.0g fp_enu
		replace c_enu=1 if c_rmesr>=1 & c_rmesr<=5
		replace c_enu=2 if c_rmesr>=6 & c_rmesr<=7
		replace c_enu=3 if c_rmesr==8 
		replace fp_enu=1 if fp_rmesr>=1 & fp_rmesr<=5
		replace fp_enu=2 if fp_rmesr>=6 & fp_rmesr<=7
		replace fp_enu=3 if fp_rmesr==8 
		
		
		* make sure weights are integer
		capture drop fwpfinwgt
		gen fwpfinwgt=floor(wpfinwgt)
		ren wpfinwgt wpfinwgt_old
		ren fwpfinwgt wpfinwgt

		/*
		cap n ren empgap emgap_old
		capture drop empgap
	    gen empgap=0 if tm_noempgap==1
		replace empgap=1 if tm_empgap_weeks>=1 & tm_empgap_weeks<=6
		replace empgap=floor((tm_empgap_weeks+1)/4) if tm_empgap_weeks>7 & tm_empgap_weeks!=. 
		replace empgap=tm_empgap_months if tm_empgap_months  >0 & tm_empgap_months!=.
		replace empgap=12*tm_empgap_years if tm_empgap_years>0 & tm_empgap_years!=.
		*/
		
		cap n ren empgap emgap_old
		capture drop empgap
	    gen empgap=0 if tm_noempgap==1
		replace empgap=0 if tm_empgap_weeks>=1 & tm_empgap_weeks<=3
		replace empgap=floor((tm_empgap_weeks+1)/4) if tm_empgap_weeks>4 & tm_empgap_weeks!=. 
		replace empgap=tm_empgap_months if tm_empgap_months  >0 & tm_empgap_months!=.
		replace empgap=12*tm_empgap_years if tm_empgap_years>0 & tm_empgap_years!=.
		
		
		* recode occupations (according to the David Dorn's mapping into 1990 SOC, then aggregated)
		
		cd "$codedir"
		do "1984_91_recode_occdd_80_to_90_step0_1_2.do"
		cd "$outputdata"
		display "running recodes"
		recode_dd80_to_90 fp_tjbocc1 
		recode_dd80_to_90 fp_tjbocc2 
		recode_dd80_to_90 occprevjb 
		recode_dd80_to_90 occprevjb 
		recode_dd80_to_90 occprevjb_nocurrentjb
		recode_dd80_to_90 c_tjbocc1 
		recode_dd80_to_90 c_tjbocc2
		
		capture drop donotconsider_dum
		gen byte donotconsider_dum=.
		
		* using the SOC1980 aggregation
		
		/*
		capture program drop aggregate_occ_8691
		program define aggregate_occ_8691


			set more off
			if "`1'"!="" {

			set more off
			set varabbrev off

			global occupation  "`1'"

			// AGGREGATION FOR 1980 SOC ----- THIS IS THE OLD STEPTWO8691.do program	

			capture gen ${occupation}_1d=.

			forvalues i=4(1)37 {
					replace ${occupation}_1d=1 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=43(1)199 {
					replace ${occupation}_1d=2 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}
					
			forvalues i=203(1)235 {
					replace ${occupation}_1d=3 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=243(1)285 {
					replace ${occupation}_1d=4 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=303(1)389 {
					replace ${occupation}_1d=5 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=403(1)407 {
					replace ${occupation}_1d=8 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=413(1)427 {
					replace ${occupation}_1d=8 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=433(1)469 {
					replace ${occupation}_1d=8 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=473(1)476 {
					replace ${occupation}_1d=9 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=477(1)499 {
					replace ${occupation}_1d=9 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=503(1)549 {
					replace ${occupation}_1d=11 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=553(1)617 {
					replace ${occupation}_1d=12 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=633(1)699 {
					replace ${occupation}_1d=13 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=703(1)799 {
					replace ${occupation}_1d=14 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=803(1)859 {
					replace ${occupation}_1d=15 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=863(1)889 {
					replace ${occupation}_1d=16 if ${occupation}==`i' &  (panel==1984 | panel==1985 | panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}
			}
		end 

		*run the program (aggregate_occ_8691) for the variables in question
		aggregate_occ_8691 fp_tjbocc1 
		aggregate_occ_8691 fp_tjbocc2 
		aggregate_occ_8691 occprevjb 
		aggregate_occ_8691 occprevjb_nocurrentjb
		aggregate_occ_8691 c_tjbocc1 
		aggregate_occ_8691 c_tjbocc2

		* hardly any difference in the two occupational codes (_dd_1d and _1d)
		count if occprevjb_1d!=.
		count if occprevjb_dd_1d!=.
		count if occprevjb_dd_1d!=occprevjb_1d
		
		count if c_tjbocc1_dd_1d!=.
		count if c_tjbocc1_1d!=.
		count if c_tjbocc1_dd_1d!=c_tjbocc1_1d
*/
		

		

		********
		** MORE AUXILIARY STUFF
		********

		

		* occupational tenure, relative to interview month

			* interview months in  wave 2: june, july, august, september
				* rotation 4: interview in august
				* rotation 3: interview in july
				* rotation 2: interview in june
				* rotation 1: interview in september
	
	/* // COVERED IN CONSTRUCTION FILE in case of 1984
	capture drop intvw_month
	gen intvw_month=ym(${panel},5) if rot==4		
	replace intvw_month=ym(${panel},8) if rot==3		
	replace intvw_month=ym(${panel},7) if rot==2
	replace intvw_month=ym(${panel},6) if rot==1
	*/
	
	order personkey yearmonth, first
	/*
		* append tm2 data also to wave 1; then tm2 information can be found alongside wave 1 and wave 2 variables
		gsort personkey -yearmonth
		local tm2list "intvw_tm2 tsjdate_startpanel imp_tsjdate_startpanel_yr imp_tsjdate_startpanel_mth startprevjb_tm startprevjbyr_tm imp_startprevjb_yr imp_startprevjb_mth endprevjb_tm endprevjbyr_tm imp_endprevjb_yr imp_endprevjb_mth occprevjb imp_occ indprevjb imp_ind tm8218 tm8220 tm8234 tm8236 "
		foreach var of local tm2list  {
			replace `var'=`var'[_n-1]  if personkey[_n-1]==personkey & wave==1 & `var'==.
		}
		* and again for the labour force variables
		local tm2list "firstyearlf alwayswork6m years6mworked notworked6m timesnotworked6m recentbreak6mfr recentbreak6mto rsnbreak6m imp_8274 imp_8276 imp_8278 imp_8290 imp_8292 imp_8294"
		foreach var of local tm2list  {
			replace `var'=`var'[_n-1]  if personkey[_n-1]==personkey & wave==1 & `var'==.
		}
		* and again for aggregated and recoded variables
		gsort personkey -yearmonth
		local tm2list "occprevjb_1d occprevjb_dd_1d occprevjb_dd rsnendprevjb"
		foreach var of local tm2list  {
			replace `var'=`var'[_n-1]  if personkey[_n-1]==personkey & wave==1 & `var'==.
		}
		
		*/
		}



		sort personkey yearmonth 

		* drop if tm2 is imputed
		*drop if (intvw_tm2>2 & intvw_tm2!=.) & wave<=2
		
		drop if pp_mis_core==2
		

		cd "$outputdata"
		save "1984_corewave_occmob.dta", replace
		*use "1984_corewave_occmob.dta", clear

		
		cap n ren iws1occ	ajbocc1
		cap n ren iws2occ	ajbocc2
		
		
							sort personkey yearmonth
						capture drop interview_no
						gen interview_no=1 if personkey!=personkey[_n-1]
						replace interview_no=interview_no[_n-1]+1 if personkey==personkey[_n-1]


***	
*Create employment, unemployment and non-particpation indicators
*Based on esr= monthly labour force indicator
gen byte mempl = 0
replace mempl = 1 if fp_rmesr==1 | fp_rmesr==2 | fp_rmesr==3 | fp_rmesr==4 | fp_rmesr==5
*1 With a job entire month, worked all weeks.
*2 With a job all month, absent from work w/out pay 1+ weeks, absence not due to layoff
*3 With job all month, absent from work w/out pay 1+ weeks, absence due to layoff
*4 With a job at least 1 but not all weeks, no time on layoff and no time looking for work
*5 With job at least 1 but not all weeks, some weeks on layoff or looking for work

gen byte munempl = 0
replace munempl =1 if fp_rmesr==6 | fp_rmesr==7
*6 No job all month, on layoff or looking for work all weeks.
*7 No job, at least one but not all weeks on layoff or looking for work

gen byte moutlf = 0
replace moutlf =1 if fp_rmesr==8
*8 No job, no time on layoff and no time looking for work.

gen byte mlaborforce =0
replace mlaborforce =1 if mempl==1 | munempl==1


*** EMPLOYMENT/UNEMPLOYMENT/NLF BASED ON WEEKLY RECODES -- second week
            **** use full panel recodes because they are consistent with starting dates 
display "------ EMPL/ UNEMPL definition ------"            
sort personkey yearmonth
gen byte empl = 0
replace empl = 1 if ((rwkesr2==1|rwkesr2==2|rwkesr2==3) | fp_rmesr==1| fp_rmesr==2| fp_rmesr==3)
*1 With a job entire month, worked all weeks.
*2 With a job all month, absent from work w/out pay 1+ weeks, absence not due to layoff
*3 With job all month, absent from work w/out pay 1+ weeks, absence due to layoff
*4 With a job at least 1 but not all weeks, no time on layoff and no time looking for work
*5 With job at least 1 but not all weeks, some weeks on layoff or looking for work

sort personkey yearmonth
gen byte unempl = 0
replace unempl =1 if rwkesr2==4  & fp_rmesr!=1 & fp_rmesr!=2 & fp_rmesr!=3
replace unempl =1 if rwkesr2==5 & rwkesr1==4  & fp_rmesr!=1 & fp_rmesr!=2 & fp_rmesr!=3
replace unempl =1 if rwkesr2==5 & yearmonth==yearmonth[_n-1]+1 & personkey==personkey[_n-1] &(rwkesr5[_n-1]==4|rwkesr4[_n-1]==4|rwkesr3[_n-1]==4|rwkesr2[_n-1]==4) & fp_rmesr!=1 & fp_rmesr!=2 & fp_rmesr!=3
replace unempl =1 if rwkesr2==5 & yearmonth!=yearmonth[_n-1]+1 & personkey!=personkey[_n-1] & yearmonth==yearmonth[_n+1]-1 & fp_rmesr!=1 & fp_rmesr!=2 & fp_rmesr!=3 ///
                                                & personkey==personkey[_n+1] & empl[_n+1]==1

												/* SPECIAL ADJUSTMENT FOR early 1985 */
replace unempl=1 if unempl!=1 & munempl==1 & pp_wave==6

												
												
												* LOOKING IN WEEK 2, not looking in week 2 but in previous month

gen byte outlf = 0
replace outlf =1 if (unempl==0 & empl==0)

gen byte laborforce =0
replace laborforce =1 if empl==1 | unempl==1


			//===================================
			// MARK ACTIVE/DOMINANT FIRMS: TIE-BREAKING 1
			//===================================
			
			*merge 1:1 personkey yearmonth using "C:\data\retrosipp\1984total_v1016.dta", keepusing(fp_tpmsum1 fp_tpmsum2 c_tpmsum1 c_tpmsum2)
			*merge 1:1 personkey yearmonth using "C:\data\retrosipp\1984total_v1016.dta", keepusing(fp_tpyrate1 fp_tpyrate2 c_tpyrate1 c_rpyper1 c_tpyrate2 c_rpyper2)
			
			
			
			replace eeno1=eeno1_old
			replace eeno2=eeno2_old
			
			mvdecode ws1wks, mv(9)
			mvdecode ws2wks, mv(9)
			
			capture drop firmno
			gen firmno=.
			replace firmno= eeno1 if   eeno1!=. &  eeno2==. & empl==1
			replace firmno= eeno2 if   eeno2!=. &  eeno1==. & empl==1 & firmno==.

			// by tej/tsj
			replace firmno= eeno1 if  firmno==. & eeno1!=. &  eeno2!=. & empl==1 & (tsjdate1!=. & tsjdate1< dofm(yearmonth)+14) & (tejdate1!=. & tejdate1>dofm(yearmonth)+6) & (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)
			replace firmno= eeno2 if  firmno==. & eeno1!=. &  eeno2!=. & empl==1 & (tsjdate2!=. & tsjdate2< dofm(yearmonth)+14) & (tejdate2!=. & tejdate2>dofm(yearmonth)+6) & (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)

			// by weeks worked 
			replace firmno= eeno1 if  firmno==. & eeno1!=. &  eeno2!=. & empl==1 & ws1wks>0 & ws1wks<9 & (ws2wks>=9 | ws2wks==0)
			replace firmno= eeno2 if  firmno==. & eeno1!=. &  eeno2!=. & empl==1 & ws2wks>0 & ws2wks<9 & (ws1wks>=9 | ws1wks==0)

			// by hours, then by income
			replace firmno= eeno1 if firmno==. & ( eeno1!=. &  eeno2!=.) & ( c_ejbhrs1>= c_ejbhrs2) &  c_ejbhrs1!=. &  c_ejbhrs2!=. & empl==1 & firmno==. & firmno==.
			replace firmno= eeno2 if firmno==. & ( eeno1!=. &  eeno2!=.) & ( c_ejbhrs2>  c_ejbhrs1) &  c_ejbhrs1!=. &  c_ejbhrs2!=. & empl==1 & firmno==. & firmno==.
			replace firmno= eeno1 if firmno==. & ( eeno1!=. &  eeno2!=.) & ( c_tpyrate1>=  c_tpyrate2) &  c_tpyrate1!=. &  c_tpyrate2!=. & empl==1 & firmno==. & firmno==.
			replace firmno= eeno2 if firmno==. & ( eeno1!=. &  eeno2!=.) & ( c_tpyrate2>  c_tpyrate1) &  c_tpyrate1!=. &  c_tpyrate2!=. & empl==1 & firmno==. & firmno==.

			// set firmno to missing, if the end date is past, or starting date not yet there


		
		
			/*
			//================================
			// MARK RECALLS
			//================================
			
			
			
				**** UNEMPLOYMENT SPELLS WITH RETURN TO THE SAME FIRM
				sort personkey yearmonth
				capture drop unempl_recall
				gen unempl_recall=unempl

                * assign current firm ids 
				capture drop previous_firmid1
				capture drop previous_firmid2
				gen previous_firmid1= eeno1  if eeno1!=. & empl==1 & panel>=1990
                gen previous_firmid2= eeno2  if eeno2!=. & empl==1 & panel>=1990
                
                * take firm ids further into future
                replace previous_firmid1=previous_firmid1[_n-1] if personkey==personkey[_n-1] & previous_firmid1==. & previous_firmid1[_n-1]!=. ///
                        & (empl==0 | (empl==1 & empl[_n-1]==1)) & panel>=1990
                replace previous_firmid2=previous_firmid2[_n-1] if personkey==personkey[_n-1] & previous_firmid1==. & previous_firmid1[_n-1]!=. ///
                        & (empl==0 | (empl==1 & empl[_n-1]==1)) & panel>=1990
                
                * mark an unemployment transition to the same firm
				capture drop samefirm
               gen samefirm=0
               replace samefirm=1 if  empl==0 & empl[_n+1]==1 & ((previous_firmid1== eeno1[_n+1] & previous_firmid1!=.) | ///
                           (previous_firmid1== eeno2[_n+1] & previous_firmid1!=.) |  ( previous_firmid2== eeno1[_n+1] & previous_firmid2!=.)  | ///
                           (previous_firmid2== eeno2[_n+1] & previous_firmid2!=.) ) & personkey==personkey[_n+1] & panel>=1990

                gsort personkey -yearmonth

                *take back the unemployment spell to 
                replace samefirm=1 if yearmonth==yearmonth[_n-1]-1 & samefirm[_n-1]==1 & empl==0  & panel>=1990
                replace unempl_recall=2 if unempl==1 & samefirm==1 & panel>=1990
		*/

	
				//=====================================
				//  HIGHLIGHT GOVERNMENT EMPLOYEES
				//======================================
				
				** drop if government employee
				capture drop gov_ind
				capture drop gov_ind1
				capture drop gov_ind2
				gen byte gov_ind1=.
				gen byte gov_ind2=.
				gen byte gov_ind=.

								
				replace gov_ind1=1 if (fp_eclwrk1==3|fp_eclwrk1==4|fp_eclwrk1==5) & empl==1
				replace gov_ind2=1 if (fp_eclwrk2==3|fp_eclwrk2==4|fp_eclwrk2==5) & empl==1
				replace gov_ind=1 if (gov_ind1==1| gov_ind2==1)
				sort personkey yearmonth


				capture drop maxgov
                by personkey: egen maxgov=max(gov_ind)
                *drop if maxgov==1
                *drop maxgov
                *drop gov_ind

		
				//==============================================================
				// OCCUPATIONAL CODING
				//==============================================================
				
				
				***********************************
				***  CARLOS' CODE // DOMINANT FIRM BASED
				***********************************
										* this code uses the occupation of the dominant firm
										* and uses recall to get rid of occupations that the worker has visited before 
										* occupation is set to missing if worker becomes nlf
										* in this version: use full panel files!!!! Since all we need from firms is occupation, not firm identity, it's okay that we do not use the revised jobids 1990-1993
										* it is important to use the full panel files here, since otherwise ejbhrs is wavely, instead of monthly, and will not contain additional information...

				capture drop occup1da
				capture drop occup1d
				gen occup1da=.

				replace occup1da= c_tjbocc1_dd_1d if firmno== eeno1 & empl==1 & ajbocc1!=1 & eeno1!=.
				replace occup1da= c_tjbocc2_dd_1d if firmno== eeno2 & empl==1 & ajbocc2!=1 & eeno2!=.

				replace occup1da=. if occup1da==0

				*********************************************************************************
				*****Imputing the occupation to the unemployed
				*********************************************************************************
				sort personkey yearmonth
				// create new variable so that you can check that the changes are correct
				gen occup1d=occup1da

				*replace missing occupation with value from prior employment spells
				bysort personkey (yearmonth): replace occup1d=occup1d[_n-1] if occup1d==. & empl==1

				* if first observations were non-employment, replace missing occupation with value from later employment spell
				*gsort personkey -yearmonth
				*bysort personkey: replace occup1d=occup1d[_n-1] if occup1d==. & empl==1 & empl[_n-1]==1 & occup1d[_n-1]!=.

				* return to original sorting
				sort personkey yearmonth
			

			
							

				*********************************************************************************
				***** keep BOTH FIRM'S INFO WHEN employed in both firms tm2
				*********************************************************************************

				capture drop occup1tm1
				capture drop occup1tm2
					
				gen occup1tm1=.
				gen occup1tm2=.

				replace occup1tm1=c_tjbocc1_dd_1d if empl==1 & c_tjbocc1_dd_1d!=. & ajbocc1!=1
				replace occup1tm2=c_tjbocc2_dd_1d if empl==1 & c_tjbocc2_dd_1d!=. & ajbocc2!=1



				** set occupation to missing, if starting date hasn't arrived, or ending date has passed
				replace occup1tm1=.  if (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup1tm2=.  if (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if employment spell continuous and firm is the same
				replace occup1tm1=occup1tm1[_n-1]  if personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup1tm1[_n-1]!=. & occup1tm1==. & eeno1!=.
				replace occup1tm2=occup1tm2[_n-1]  if personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup1tm2[_n-1]!=. & occup1tm2==. & eeno2!=.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup1tm1=occup1tm1[_n-1]  if personkey==personkey[_n-1] & empl==1 & occup1tm1[_n-1]!=. & occup1tm1==. & occup1tm2==.
				replace occup1tm2=occup1tm2[_n-1]  if personkey==personkey[_n-1] & empl==1 & occup1tm2[_n-1]!=. & occup1tm2==. & occup1tm1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup1tm1=occup1tm1[_n-1]  if personkey==personkey[_n-1] & empl==0 & occup1tm1[_n-1]!=. & occup1tm1==.
				replace occup1tm2=occup1tm2[_n-1]  if personkey==personkey[_n-1] & empl==0 & occup1tm2[_n-1]!=. & occup1tm2==.

				* with tm2 data in pre-1996 waves
				*replace occup1tm1=occprevjb[_n+4] if wave==1 & wave[_n+3]==1 & empl!=1

		//=======================================
		// ENTRY AND EXIT
		//=======================================
		
		sort personkey yearmonth
		
		// entry: from school
		capture drop max_educ
		by personkey: egen max_educ=max(educ)
		
		idfillreplace firstyearlf
		
		*lab define ersend93label 1 "laid off" 2 "retirement/old age" 8 "discharged/fired" 11 "temporary job ended" 12 "quit for another job" 15 "quit other reason" 
		*lab val ersend1 ersend93label
		*lab val ersend2 ersend93label
		
		
		capture drop nlfschool
		gen byte nlfschool=0
		cap n replace nlfschool=1 if outlf==1 & rnotake==3
		replace nlfschool=1 if outlf==1 & eenrlm==1
		*replace nlfschool=1 if outlf==1 & empl[_n-1]==1 & (ersend1==7 | ersend2==7)  & personkey==personkey[_n-1] 
		lab var nlfschool "Person is NLF: schooling"


	*those in formal education at first observation, and as determined by age:
	*1 if <=18 and never seen before and in school and at most some high school edu
	* OR <=22 and never seen before and in school and more than some high school edu
	*OR <=23 and --"-- more than college education (????)
	*OR seen before but was designated as firstschool in prev period
		capture drop firstschool
		gen byte firstschool = 0
		replace firstschool = 1 if tage <= 19 & personkey != personkey[_n-1] & nlfschool == 1 & educ<=2
		replace firstschool = 1 if tage <= 22 & personkey != personkey[_n-1] & nlfschool == 1 & educ>2
		replace firstschool = 1 if tage <= 23 & personkey != personkey[_n-1] & nlfschool == 1 & educ>4	
		replace firstschool = 1 if personkey == personkey[_n-1] & firstschool[_n-1] == 1 & nlfschool == 1
		lab var firstschool "Dummy if a person is in his first education period"

		
		// if in school before 19 (HS) or 22 (college), we do not count the previous labour market history
		gsort personkey -yearmonth
		replace nlfschool=1 if nlfschool[_n-1]==1 & max_educ>2 & tage<=22 & personkey==personkey[_n-1]
		replace nlfschool=1 if nlfschool[_n-1]==1 & max_educ>=1 & tage<=19 & personkey==personkey[_n-1]
		sort personkey yearmonth
		
		replace unempl=0 if nlfschool==1
		replace empl=0 if nlfschool==1
		replace outlf=1 if nlfschool==1
		
		
		// exit: to retirement
		
		*person declares he's not working because of retirement, or declares he quit job to retire, or has previously declared this
		capture drop retired
		gen byte retired = 0
		*replace retired = 1 if ersnowrk == 4 & outlf==1
		*replace retired = 1 if outlf==1 & empl[_n-1]==1 & (ersend1==2 | ersend2==2)
		replace retired = 1 if outlf==1 & (eeveret==1) & personkey==personkey[_n-1] 
		lab var retired "Person doesn't work this month because of retirement this month"

		// replace subsequent history by outlf
		replace retired = 1 if retired[_n-1]==1 & personkey==personkey[_n-1] & tage>=55
		replace unempl=0 if retired==1
		replace empl=0 if retired==1
		replace outlf=1 if retired==1
		
		
		tab wave
		compress
		
		
		
		
		cd "$outputdata"
		save "1984_corewave_occmob.dta", replace

		// drop as many variables as possible (topical module stuff merged in later)
		
		
		capture n drop  c_tjbocc2_1d 
		capture n drop  c_tjbocc1_1d 
		capture n drop  occprevjb_nocurrentjb_1d 
		capture n drop  occprevjb_1d 
		capture n drop  fp_tjbocc2_1d 
		capture n drop  fp_tjbocc1_1d 
		capture n drop  donotconsider_dum
		
		
		capture n drop  occprevjb_dd 
		capture n drop  occprevjb_dd_1d 
		capture n drop  occprevjb_nocurrentjb_dd 
		capture n drop  occprevjb_nocurrentjb_dd_1d
		
		capture n drop  c_ehrsbs1 
		capture n drop  ise1occ 
		capture n drop  ise1ind 
		capture n drop  ise12260 
		capture n drop  c_ehrsbs2 
		capture n drop  ise2occ 
		capture n drop  ise2ind
		
		capture n drop  smonth2 
		capture n drop  sday2 
		capture n drop  emonth2 
		capture n drop  eday2
		
		capture n drop  smonth1 
		capture n drop  sday1 
		capture n drop  emonth1 
		capture n drop  eday1
		
		
		capture n drop  firstbreak6mfr 
		capture n drop  firstbreak6mto 
		capture n drop  rsnfirstbreak6m 
		capture n drop  secondbreak6mfr 
		capture n drop  secondbreak6mto 
		capture n drop  rsnsecondbreak6m 
		capture n drop  thirdbreak6mfr 
		capture n drop  thirdbreak6mto 
		capture n drop  rsnthirdbreak6m 
		capture n drop  fourthbreak6mfr 
		capture n drop  fourthbreak6mto 
		capture n drop  rsnfourthbreak6m 
		*capture n drop  imp_8234 
		*capture n drop  imp_tmcurjb_rsnend 
		*capture n drop  imp_prevjbse 
		*capture n drop  imp_startprevjbyr_tm 
		*capture n drop  imp_endprevjbyr_tm 
		*capture n drop  imp_tm_empgap 
		*capture n drop  imp_8272 
		*capture n drop  imp_8274 
		*capture n drop  imp_8276 
		capture n drop  imp_tm_part_fulltime_overall 
		capture n drop  imp_firstbreak6mfr 
		capture n drop  imp_firstbreak6mto 
		capture n drop  imp_rsnfirstbreak6m 
		capture n drop  imp_secondbreak6mfr 
		capture n drop  imp_secondbreak6mto 
		capture n drop  imp_rsnsecondbreak6m 
		capture n drop  imp_thirdbreak6mfr 
		capture n drop  imp_thirdbreak6mto 
		capture n drop  imp_rsnthirdbreak6m 
		capture n drop  imp_fourthbreak6mfr 
		capture n drop  imp_fourthbreak6mto 
		capture n drop  imp_rsnfourthbreak6m
			capture n drop  c_tbsind1 
			capture n drop  c_tbsocc1 
			capture n drop  c_sewks1 
			capture n drop  c_tbmsum1 
			capture n drop  c_ebno1 
			capture n drop  c_ehrsbs1 
			capture n drop  c_tbsind2 
			capture n drop  c_tbsocc2 
			capture n drop  c_sewks2 
			capture n drop  c_tbmsum2 
			capture n drop  c_ebno2 
			capture n drop  c_ehrsbs2 
			capture n drop  fp_se1_wk 
			capture n drop  fp_se2_wk 
			capture n drop  fp_se1hrs 
			capture n drop  fp_se2hrs 
			capture n drop  job_or_se 
			capture n drop  ebno1 
			capture n drop  ebno2 
			capture n drop  pp_intv 
			capture n drop  intvw_tm2 
			capture n drop  intvw pp_mis 
			
			
			compress
		
		
		
		save "1984_corewave_occmob_min.dta", replace

	

