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






***********************************************
*** WHOLE SAMPLE MANIPULATIONS
***********************************************

/*

(-) define sample_timetogo, firstlastwave etc
(-) define occupations in unemployment spells
		-> note: we can define a stay when occupations are matched before and after the spell, including with 2 jobs
		-> however, when we do the gamma_inv correction, we need a single transition; hence we define a source_occ, destination_occ 
			that is only assigned during the unemployment spell
			and the transition pair is unique 
(-) merge with the relevant information from the retrospective master data. So we can study repeat mobility and possibly the effect of long duration nonemployment/unemployment on future unemployment outcomes
(-) note, intvw_month: this is the interview month of the topical module with the labour market history

*/


		********************************************************************************************************************************************************
		** PREAMBLE
		*********************************************************************************************************************************************************

		clear
		*capture log close
		*set mem 3000m
		*set more off
		*set maxvar 10000
        version 13

		** GLOBALS
		global gnulin=0
		global yeardata=16
		* control parts of the do-file
		global core_if=0	// read in all data from addition core waves
		global tm_if=1	       // read in the topical modules
		global read_in_data=1
		global weights_if=1   // loads in the longitudinal weights
		global subset_if=0   // starts reducing datasets into subsets with relevant variables for different projects
		global list=0
		global fp_ind=1
		global heading_ind=1
		
// read in different panels
		global locfileversion=1016  // OCT 2016
		

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
		** SELECTION OF DATA
		********
					/*
					cycle through the different panels to load all the data
					*/

		cd "$outputdata"		
		use "2008_corewave_occmob_min.dta", clear
		append using "2004_corewave_occmob_min.dta"
		append using "2001_corewave_occmob_min.dta"
		append using "1996_corewave_occmob_min.dta"
		append using "1993_corewave_occmob_min.dta"
		append using "1992_corewave_occmob_min.dta"
		append using "1991_corewave_occmob_min.dta"
		append using "1990_corewave_occmob_min.dta"
		append using "1988_corewave_occmob_min.dta"
		append using "1987_corewave_occmob_min.dta"
		append using "1986_corewave_occmob_min.dta"
		append using "1985_corewave_occmob_min.dta"
		append using "1984_corewave_occmob_min.dta"
		
		
		// minimize dataset for manipulations

		capture n drop lgtpn1wt 
		capture n drop lgtpn2wt 
		capture n drop lgtpn3wt 
		capture n drop lgtpn4wt 
		capture n drop lgtpn5wt 
		capture n drop lgtcy1wt 
		capture n drop lgtcy2wt 
		capture n drop lgtcy3wt 
		capture n drop lgtcy4wt 
		capture n drop lgtcy5wt
		
		capture n drop aafnow
		capture n drop renroll 
		capture n drop arenroll 
		capture n drop eenrlm 
		capture n drop eeducate 
		capture n drop epdjbthn 
		capture n drop apdjbthn
		capture n drop eeveret 
		capture n drop aeveret 
		capture n drop edisabl 
		capture n drop adisabl 
		capture n drop edisprev 
		capture n drop adisprev 
		capture n drop ersnowrk 
		capture n drop arsnowrk 
		capture n drop eabre 
		capture n drop aabre 
		capture n drop eptresn 
		capture n drop aptresn 
		capture n drop elkwrk 
		capture n drop alkwrk 
		capture n drop elayoff 
		capture n drop alayoff 
		*capture n drop rtakjob rnotake 
		
		capture n drop emoonlit 
		capture n drop amoonlit
		*apyrate1 rpyper1
		
		capture n drop lgtpnwt1 
		capture n drop lgtpnwt2 
		capture n drop lgtpnwt3 
		capture n drop elstwrkm 
		capture n drop alstwrkm 
		capture n drop eprvjbmn 
		capture n drop aprvjbmn 
		capture n drop efrmrmn 
		capture n drop afrmrmn 
		capture n drop ayrsince 
		capture n drop eyrsinc2 
		capture n drop ayrsinc2 
		capture n drop eothtime 
		capture n drop aothtime 
		capture n drop ecntothr 
		capture n drop acntothr 
		capture n drop tfstyrfr 
		capture n drop afstyrfr 
		capture n drop tfstyrto 
		capture n drop afstyrto 
		capture n drop endprevjb_tm 
		capture n drop startprevjb_tm 
		capture n drop eyrsince 
		capture n drop lgtpnlwt
		
		
		capture n drop tm8210 
		capture n drop eeno1_startpanel 
		capture n drop tm8218 
		capture n drop tm8220 
		capture n drop tm8234 
		capture n drop tm8236 
		capture n drop tm8240 
		capture n drop tm8242 
		capture n drop tm8248 
		capture n drop tm8250 
		capture n drop tm8252 
		capture n drop indprevjb 
		capture n drop occprevjb 
		capture n drop prevjbse 
		capture n drop tm8268 
		capture n drop tm8270 
		capture n drop rsnendprevjb 
		capture n drop alwayswork6m 
		capture n drop years6mworked 
		capture n drop tm8280 
		capture n drop tm8282 
		capture n drop tm8284 
		capture n drop notworked6m 
		capture n drop timesnotworked6m 
		capture n drop recentbreak6mfr 
		capture n drop recentbreak6mto 
		capture n drop rsnbreak6m 
		capture n drop imp_8234 
		capture n drop imp_8266 
		capture n drop imp_8272 
		capture n drop imp_8274 
		capture n drop imp_8276 
		
		capture n drop imp_8278 
		capture n drop imp_8282 
		capture n drop imp_8286 
		capture n drop imp_8288 
		capture n drop imp_8290 
		capture n drop imp_829a 
		capture n drop imp_8292 
		capture n drop imp_8294 
		capture n drop imp_ind 
		capture n drop imp_occ 
		capture n drop tsjdate_startpanel 
		capture n drop imp_tsjdate_startpanel_yr 
		capture n drop imp_tsjdate_startpanel_mth 
		capture n drop endprevjbyr_tm 
		capture n drop imp_endprevjb_yr 
		capture n drop imp_endprevjb_mth 
		capture n drop startprevjbyr_tm 
		capture n drop imp_startprevjb_yr 
		capture n drop imp_startprevjb_mth
		
		capture n drop occprevjb_nocurrentjb 
		capture n drop indprevjb_nocurrentjb 
		capture n drop tm2_markedworking 
		capture n drop tm2_curjbse 
		capture n drop yearlastworked 
		capture n drop rsno2wkwrk 
		capture n drop tmcurjb_tenure_yr 
		capture n drop tmcurjb_tenure_mth 
		capture n drop tmcurjb_ongoing 
		capture n drop tmcurjb_rsnend 
		capture n drop tm_ageover21 
		capture n drop tmcurjb_10yrless 
		capture n drop tm_hadprevjb 
		capture n drop tm_empgap_weeks 
		capture n drop tm_empgap_months 
		capture n drop tm_empgap_years 
		capture n drop tm_noempgap 
		capture n drop tm_part_fulltime_overall 
		capture n drop imp_tmcurjb_rsnend 
		capture n drop imp_prevjbse 
		capture n drop imp_startprevjbyr_tm 
		capture n drop imp_endprevjbyr_tm 
		capture n drop imp_tm_empgap 
		capture n drop imp_occprevjb_nocurrentjb 
		capture n drop imp_indprevjb_nocurrentjb 
		capture n drop tmcurjb_tenure 
		capture n drop imp_tmcurjb_tenure 
		capture n drop emgap_old 
		capture n drop imp_empgap 
		capture n drop pp_mis_core
		
		
		capture n drop occcurjb_ongoing_dum 
		capture n drop retro_endprevjb_tm 
		capture n drop confirmed_endprevjb_ind 
		capture n drop tm_minprevjb_tenure 
		capture n drop eeno2_startpanel 
		capture n drop empgap
		
		
		*save corewave_occmob_all_1811.dta, replace
		*use corewave_occmob_all_2810.dta, clear
		
		
		//===========================================================================
		global oc "1d"
		global oc2 "1tm"
		global digit "1d"

		global sttg "12"

		global age1 "(agegroup==0|agegroup==1|agegroup==2)"
		global age2 "(agegroup==3|agegroup==4|agegroup==5|agegroup==6)"
		global age3 "(agegroup==7|agegroup==8|agegroup==9)"

		//=======================================================================
		// (RE)DEFINE CONTINUOUS SPELL, QUARTER, E/U/O, OCCUPATIONS, OCCUPATIONS IN UNEMPLOYMENT, 
		// COMPLETE U SPELL, SAMPLE-TIME-TO-GO, GOV_IND, DOMINANT FIRM
		//=======================================================================

		
		capture drop agegroup
		gen agegroup=.
		replace agegroup = 0 if tage > 15 & tage <= 21
		replace agegroup = 1 if tage > 21 & tage <= 25
		replace agegroup = 2 if tage > 25 & tage <= 30
		replace agegroup = 3 if tage > 30 & tage <= 35
		replace agegroup = 4 if tage > 35 & tage <= 40
		replace agegroup = 5 if tage > 40 & tage <= 45
		replace agegroup = 6 if tage > 45 & tage <= 50
		replace agegroup = 7 if tage > 50 & tage <= 55
		replace agegroup = 8 if tage > 55 & tage <= 60
		replace agegroup = 9 if tage > 60 & tage <= 65

		
		sort panel personkey yearmonth
		
		capture drop interview_no
		gen int interview_no=1 if personkey!=personkey[_n-1] 
		replace interview_no=interview_no[_n-1]+1 if personkey==personkey[_n-1] & interview_no[_n-1]!=. // & panel==panel[_n-1] & yearmonth>=yearmonth[_n-1]+1
				
		** generate new continuous spell variables
		sort personkey yearmonth
		capture drop continuous_spell
		gen int continuous_spell=.
		replace continuous_spell=1 if interview_no==1
		replace continuous_spell=1 if (personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1)
		
		replace continuous_spell=0 if (personkey!=personkey[_n-1] & continuous_spell==. )
		replace continuous_spell=-1 if continuous_spell==.
		
		tab continuous_spell wave
		
		tab continuous_spell
		capture drop mincont
		by personkey: egen mincont=min(continuous_spell)
		tab mincont

			
		/* notice, there are many interrupted spells, but these line up with the longitudinal weights */
		
		**    capture drop firstlastwave
     gen byte firstlastwave=0
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
     
             //  because seam effects at the beginning and the end are important
      // 
     
     capture drop year
     gen year=yofd(dofm(yearmonth))
     
	 
		*** time series in quarters
		capture drop quarter
		gen quarter=qofd(dofm(yearmonth))
		format yearmonth %tm
		format quarter %tq
           **
           **See how long workers are still in sample
           * *                          only count workers that are long enough in sample
     
     gsort personkey -yearmonth
     capture drop sample_timetogo
     gen sample_timetogo=.
     replace sample_timetogo=1 if personkey!=personkey[_n-1]
     replace sample_timetogo=1 if personkey==personkey[_n-1] & yearmonth!=yearmonth[_n-1]-1
     replace sample_timetogo=sample_timetogo[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & sample_timetogo[_n-1]!=.
     sort personkey yearmonth 
		
		
	//=============================================
	// weights
	//============================================= 
			
			
//************************************************
//   TRANSITION INDICATORS 
//************************************************
    *** define a transition in the first period of the new state (it is a 'has occurred'-indicator)



			display "---- INDICATORS FOR TRANSITIONS -----"
			
			// standard unemployment transition
			sort personkey yearmonth
			*set varabbrev off
			capture drop ue
			capture drop eu
			*set varabbrev on

			
			gen ue=.
			gen eu=.
			replace ue=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1
			replace eu=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & unempl==1 & empl[_n-1]==1


			// ustar transition (unemployment, within a couple of months of separation)
			
			sort personkey yearmonth
			*set varabbrev off
			*set varabbrev on

			capture drop uestar
			capture drop eustar
			gen uestar=.
			gen eustar=.
			replace uestar=1 if personkey==personkey[_n-2] & yearmonth==yearmonth[_n-2]+2 & empl==1 & (unempl[_n-1]==1|outlf[_n-1]==1) & (unempl[_n-2]==1|outlf[_n-2]==1) & (unempl[_n-1]==1|unempl[_n-2]==1)
			replace uestar=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & (unempl[_n-1]==1) 
			replace eustar=1 if personkey[_n+1]==personkey[_n-1] & yearmonth[_n+1]==yearmonth[_n-1]+2 & empl[_n-1]==1 & (unempl[_n+1]==1|outlf[_n+1]==1) & (unempl==1|outlf==1) & (unempl[_n+1]==1|unempl==1)
			replace eustar=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl[_n-1]==1 & (unempl==1)

			
			
			
			// nonemployment transition // nstar transition (nonemployment, but attached)
	
	
					
			
     /*		ERSEND ONLY PRESENT SINCE 1986
           where Nstar stands for nonemployment for seemingly standard market reasons, EXCLUDING LAYOFFS
					 * one can confine our analysis to men!
                     *drop if sex==2
     
                  * and can select further, and use ersend
                 /*
                 V          1 .On Layoff
     V          2 .Retirement or old age
     V          3 .Childcare problems
     V          4 .Other family/personal obligations
     V          5 .Own illness
     V          6 .Own injury
     V          7 .School/Training
     V          8 .Discharged/fired
     V          9 .Employer bankrupt
     V         10 .Employer sold business
     V         11 .Job was temporary and ended
     V         12 .Quit to take another job
     V         13 .Slack work or business conditions
     V         14 .Unsatisfactory work arrangements
     V            .(hours, pay, etc)
     V         15 .Quit for some other reason
     V         -1 .Not in Universe
                 */
     */
     
     display "defining ERSEND condition"
     
     global ersendc " & (ersend1==8 | ersend1==9 | ersend1==10 | ersend1==11 | ersend1==13 | ersend1==14 | ersend1==15 | ersend2==8 | ersend2==9 | ersend2==10 | ersend2==11 | ersend2==13 | ersend2==14 | ersend2==15) "
     
     
     
      * define a transition in the first period of the new state (it is a 'has occurred'-indicator)
     
     sort personkey yearmonth
     
     display " ENstar flow construction"
     // EN with ersend condition: ENSTAR
     capture drop enstar
     gen enstar=.
     replace enstar=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl[_n-1]==1 & (unempl==1|(outlf==1  $ersendc))
     
     display " EN flow construction "
      // second version, without ersend condition
     *set varabbrev off
      capture drop ne
      capture drop en
     *set varabbrev on
     
     gen ne=0
     gen en=0
     
     replace ne=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & (unempl[_n-1]==1 | outlf[_n-1]==1)
     replace en=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl[_n-1]==1 & (unempl==1|(outlf==1))
     
			
			
			
			
			
			//=================================================================
			// transitions with and without occupational changes
			//=================================================================
			
			
			
			
			
			capture program drop occmob_manip
			program define occmob_manip
			
						global oc2 "`1'"
						
						sort personkey yearmonth

						
						*global oc2 "1tm"

						capture drop ue_c_$oc2
						capture drop ue_n_$oc2


						// move occupation 2 into occupation 1
						gen occup${oc2}1_temp=occup${oc2}2 if occup${oc2}1==. & occup${oc2}2!=.
						replace occup${oc2}2=. if occup${oc2}1_temp==occup${oc2}2 & occup${oc2}2!=.
						replace occup${oc2}1=occup${oc2}1_temp if occup${oc2}1_temp!=.
						capture drop occup${oc2}1_temp



						// use both occupations, without assigning a main job

						
						display "---- OCCUPATIONAL MOBILITY THROUGH UNEMPLOYMENT? -----"
						
						* outflow
						sort personkey yearmonth
						gen ue_c_${oc2}=0 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 
						gen ue_n_${oc2}=0 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1

						display "CONSTRUCTING ue_c_${oc2}"

							replace ue_c_${oc2}=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 &( /*

						 CASE 1: before and after there are two occupations
						*/((occup${oc2}1!=. &occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=.& occup${oc2}2[_n-1]!=.) ///
						  & (occup${oc2}1 != occup${oc2}2[_n-1] & occup${oc2}2 != occup${oc2}1[_n-1]  & ///
							occup${oc2}2!=occup${oc2}2[_n-1] & occup${oc2}1!=occup${oc2}1[_n-1])) /*

						CASE 2: occupation missing in one case, before the unemployment spell -- notice reverse sorting
						*/ | ((occup${oc2}1!=. & occup${oc2}2!=. & occup${oc2}1[_n-1]!=.& occup${oc2}2[_n-1]==.) & ///
						(occup${oc2}1 != occup${oc2}1[_n-1] & occup${oc2}2 != occup${oc2}1[_n-1]  )) /*

						 CASE 3: occupation missing in one case, after  the unemployment spell
						 */ | ((occup${oc2}1!=. & occup${oc2}2==. & occup${oc2}1[_n-1]!=.& occup${oc2}2[_n-1]!=.) & ///
						(occup${oc2}1 != occup${oc2}1[_n-1] & occup${oc2}1 != occup${oc2}2[_n-1] )) /*

						CASE 4: occupation 2 missing in both cases
						 */ | ((occup${oc2}1!=. & occup${oc2}2==. & occup${oc2}1[_n-1]!=.& occup${oc2}2[_n-1]==.) & ///
						(occup${oc2}1 != occup${oc2}1[_n-1] )))

					  * none of the four combinations yield an occupational match before and after ue

						display "CONSTRUCTING ue_n_${oc2}"
							replace ue_n_${oc2}=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=.| occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=.|occup${oc2}2[_n-1]!=.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0

					 
						******** checking those that are not assigned to occ moving or occ staying 
						
						
						
								* the main cause is that the worker was initially non/unemployed, so no previous occupational information
										* a secondary cause is imputation

									/*
									capture drop indicator_temp
									gen byte indicator_temp=1 if ue_n_${oc2}==0 & ue_c_${oc2}==0
									idfillreplace indicator_temp
									*/
						replace ue_n_${oc2}=. if ue_n_${oc2}==0 & ue_c_${oc2}==0
						replace ue_c_${oc2}=. if ue_n_${oc2}==. & ue_c_${oc2}==0

									
						
						****
						capture drop lue_c_${oc2}
						capture drop lue_n_${oc2}
						sort personkey yearmonth

						gen lue_c_${oc2}=ue_c_${oc2}[_n+1] if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
						gen lue_n_${oc2}=ue_n_${oc2}[_n+1] if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1

						
						// define occbefore_${oc2} and occafter_${oc2}
						
						capture drop occbefore_$oc2
						capture drop occafter_$oc2
						gen occbefore_${oc2}=.
						gen occafter_${oc2}=.
			
								
							* first thing to realize is that in some rare cases, there is only one firm, but it is in the occup${oc2}2 spot... solution: create a temporary variable
							* that pushes 
							capture drop occup${oc2}1_backup
							gen occup${oc2}1_backup=occup${oc2}1
							
							capture drop occup${oc2}2_backup
							gen occup${oc2}2_backup=occup${oc2}2
							
							replace occup${oc2}1=occup${oc2}2 if occup${oc2}2!=. & occup${oc2}1==.
							*replace occup${oc2}2=. if occup${oc2}1==occup${oc2}2 & occup${oc2}1!=.
						
						
						* overlap, one before, one after 
						replace occafter_${oc2}=occup${oc2}1  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. & occup${oc2}2==.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]==.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & occup${oc2}1==occup${oc2}1[_n-1]
						
						replace occbefore_${oc2}=occup${oc2}1[_n-1]  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. & occup${oc2}2==.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]==.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & occup${oc2}1==occup${oc2}1[_n-1]
								 
						* overlap, one before, two after
						replace occafter_${oc2}=occup${oc2}1  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. &  occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]==.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & occup${oc2}1==occup${oc2}1[_n-1]
						replace occbefore_${oc2}=occafter_${oc2} if occbefore_${oc2}==.
								 
						replace occafter_${oc2}=occup${oc2}2  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. &  occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]==.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & occup${oc2}2==occup${oc2}1[_n-1]
						replace occbefore_${oc2}=occafter_${oc2} if occbefore_${oc2}==.
						
						
						
						* overlap, one after, two before
						
						replace occbefore_${oc2}=occup${oc2}1[_n-1]  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. & occup${oc2}2==.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]!=.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & (occup${oc2}1==occup${oc2}1[_n-1] )
						replace occbefore_${oc2}=occup${oc2}2[_n-1]  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. & occup${oc2}2==.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]!=.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & (occup${oc2}1==occup${oc2}2[_n-1] )
						replace occafter_${oc2}=occbefore_${oc2} if occafter_${oc2}==.
						
						
						* overlap, two before, two after
						replace occafter_${oc2}=occup${oc2}1  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. &  occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]!=.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & (occup${oc2}1==occup${oc2}1[_n-1] | occup${oc2}1==occup${oc2}2[_n-1])
						replace occafter_${oc2}=occup${oc2}2  if ue_n_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & (occup${oc2}1 !=. & occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]!=.) /// there is information to assign occupational changes
								 & ue_c_${oc2}==0 & (occup${oc2}2==occup${oc2}1[_n-1] | occup${oc2}2==occup${oc2}2[_n-1])
						replace occbefore_${oc2}=occafter_${oc2} if occbefore_${oc2}==.
						
						
						capture drop occup${oc2}1
						capture drop occup${oc2}2
						
						cap n ren occup${oc2}1_backup occup${oc2}1
						cap n ren occup${oc2}2_backup occup${oc2}2
						
						
						** no overlap **
								/* choose by dominant firm */
						sort panel personkey yearmonth
								
								* occupation after	
						replace occafter_${oc2}=occup${oc2}1  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & occup${oc2}1!=. & occup${oc2}2==.
						replace occafter_${oc2}=occup${oc2}2  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & occup${oc2}1==. & occup${oc2}2!=.
								
						replace occafter_${oc2}=occup${oc2}1  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & firmno==eeno1 & occup${oc2}1!=. & occup${oc2}2!=.
						replace occafter_${oc2}=occup${oc2}2  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & firmno==eeno2 & occup${oc2}1!=. & occup${oc2}2!=.
								 
								 
									* occupation before
									
						replace occbefore_${oc2}=occup${oc2}1[_n-1]  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & occup${oc2}1[_n-1]!=. & occup${oc2}2[_n-1]==.
						replace occbefore_${oc2}=occup${oc2}2[_n-1]  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & occup${oc2}2[_n-1]!=. & occup${oc2}1[_n-1]==.
							
							
										* carrying over the dominant firm no 
										* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
						*               at most only until the next employment spell, not further)
						capture drop firm_ind
						gen int firm_ind=1 if firmno==eeno1 & eeno1!=.
						replace firm_ind=2 if firmno==eeno2 & eeno2!=.
						replace firm_ind=firm_ind[_n-1]  if personkey==personkey[_n-1] & empl==0 & occup${oc2}1!=. & occup${oc2}2!=. & firm_ind==.
						
							
						replace occbefore_${oc2}=occup${oc2}1[_n-1]  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & firmno[_n-1]==1 & occup${oc2}1[_n-1]!=.
						replace occbefore_${oc2}=occup${oc2}2[_n-1]  if ue_c_${oc2}==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & unempl[_n-1]==1 ///
								 & firmno[_n-1]==2 & occup${oc2}2[_n-1]!=.
						
						
						tab occbefore_${oc2} ue_c_${oc2}, m
						tab occafter_${oc2} ue_c_${oc2}, m
						
						tab occbefore_${oc2} ue_n_${oc2}, m
						tab occafter_${oc2} ue_n_${oc2}, m
			
			
			end
			
			//----> RUN PROGRAM
			
			occmob_manip 1tm
			
			
			**********************************************
			** ALTERNATIVE DEFINITIONS OF OCCUPATIONS
			**********************************************
			
			** three digit, standard
			
			
			
			** define different measures of occupations
			
			
			* 1) 13 mog occupations (with priv hh and protective services + (mech/rep/constr/extrac/prec prod))
			
			* 2a) HOMOGENIZED DD 1d (make sure that the 90-selfmap and 80to90dd are consistent with 1980 soc, i.e. get rid of selfmap remapping where not in agreement with 80t090dd
			
			* 2b) HOMOGENIZED DD 1d (make sure that the 90-selfmap and 80to90dd are consistent with 1990 soc, i.e. apply selfmap remapping also in 80t090dd, where needed for consistency
			
			* 3) trying to get rid of helpers in occupation 16
			
			* 4) own measure
			
			
			
// alternative way of bunching

/*

management

management support

prof speciality 1: engineers, scientists and teachers, social scientists + tech support for science +engineers + computer + n.e.c.
prof speciality 2: health diagnosing and therapists, health technician + tech support health
prof speciality 3: social work and law (social work and law is a candidate to merge with prof speciality 1) 
prof speciality 4: artistic
			


sales, fin + commodities + wholesale
			(financial services, insurance, real estate to professional services?)
sales, non fire, non retail ?(merge with retail?)			
sales, retail

admin support: keep

service: into five categories (incl. protective and private hh services)

// protective services 7
// food prep		81
// health care services, not prof spec or tech support 82 (can move to health care?)
// housekeeping, cleaning, and janitorial jobs 83 (except cooks)
// PERSONAL CARE and hospitality 84 (changed from 85)
	/* child care workers */
	/* misc. personal care and service occs */
	/* personal appearance occs */
	/* recreation and hospitality occs */

farm/fish/forest

prec prod
construction
extractive
mech repairers

mach operators

transport

handlers and laborers
	assign helpers to the relevant categories

21 groups

	put captains of fishing vessels in  transport? (very few)
	where to put vehicle and equipment cleaners? in cleaning services?
	

*/
			
			
		* 5) pure 2-digit david dorn (with 3-digit low-skilled services) (out of curiosity)
		
					 
			
			
			
			
			
			
			
			
			
			
			
			//********************************************************************
			// SELECTING THE TYPE OF SPELLS
			//***********************************************************************
			
			
			
			
			
			**** select those spells than are EUE (instead of involving NLF)

			sort personkey yearmonth
			capture drop u_spellength
			gen u_spellength=.
			replace u_spellength=1 if eu==1
			replace u_spellength=1+u_spellength[_n-1] if unempl==1 & u_spellength[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1

			**** select those spells that are EU..UE
			
			sort personkey yearmonth
			capture drop un_spellength
			gen un_spellength=.
			replace un_spellength=1 if eustar==1
			replace un_spellength=1+un_spellength[_n-1] if (unempl==1|outlf==1) & un_spellength[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1

			
			
			gsort personkey -yearmonth
			capture drop complete_uspell
			gen complete_uspell=.
			replace complete_uspell=1 if ue[_n-1]==1 & u_spellength!=.
			replace complete_uspell=1 if unempl==1 & complete_uspell[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1

			gsort personkey -yearmonth
			capture drop complete_unuspell
			gen complete_unuspell=.
			replace complete_unuspell=1 if uestar[_n-1]==1 & un_spellength!=.
			replace complete_unuspell=1 if (unempl==1|outlf==1) & complete_unuspell[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1


			gsort personkey -yearmonth
			capture drop complete_unspell
			gen complete_unspell=.
			replace complete_unspell=1 if (ue[_n-1]==1|ne[_n-1]==1) & un_spellength!=.
			replace complete_unspell=1 if (unempl==1|outlf==1) & complete_unspell[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1

			
			
			// occupational mobility in complete spells 
			
			gsort personkey -yearmonth

			// stock
			capture drop co_${digit}
			gen co_${digit}=.
			replace co_${digit}=1 if ue_c_${oc2}[_n-1]==1 & complete_uspell==1
			replace co_${digit}=1 if unempl==1 & co_${digit}[_n-1]==1 & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1

			capture drop no_${digit}
			gen no_${digit}=.
			replace no_${digit}=1 if ue_n_${oc2}[_n-1]==1 & complete_uspell==1
			replace no_${digit}=1 if unempl==1 & no_${digit}[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1

			replace co_${digit}=0 if no_${digit}==1
			replace no_${digit}=0 if co_${digit}==1


			// inflow
			capture drop co_inflow_${digit}
			gen co_inflow_${digit}=1 if co_${digit}==1 & eu==1
			replace co_inflow_${digit}=0 if no_${digit}==1 & eu==1

			capture drop no_inflow_${digit}
			gen no_inflow_${digit}=1 if no_${digit}==1 & eu==1
			replace no_inflow_${digit}=0 if co_${digit}==1 & eu==1

			compress			
			
			
			
*********************************************
** ROBUSTNESS WITH RESPECT TO NOT IN THE LABOUR FORCE
*********************************************
			
       
      sort personkey yearmonth
      
      set varabbrev off
      capture drop ne_c_${oc2}
      capture drop ne_n_${oc2}
      set varabbrev on
      
      gen ne_c_${oc2}=0
      gen ne_n_${oc2}=0
      
      display "construct occupational changes nonemployment to employment flows"
          replace ne_c_${oc2}=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & (unempl[_n-1]==1|outlf[_n-1]==1) & ( /*
      
                  CASE 1: before and after there are two occupations
                  */ ((occup${oc2}1!=. &occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=.& occup${oc2}2[_n-1]!=.) ///
                & (occup${oc2}1 != occup${oc2}2[_n-1] & occup${oc2}2 != occup${oc2}1[_n-1] ///
                & occup${oc2}2!=occup${oc2}2[_n-1] & occup${oc2}1!=occup${oc2}1[_n-1])) /*
      
                  CASE 2: occupation missing in one case, before the unemployment spell -- notice reverse sorting
                  */ | ((occup${oc2}1!=. & occup${oc2}2!=. & occup${oc2}1[_n-1]!=.&  occup${oc2}2[_n-1]==.) & ///
                  (occup${oc2}1 != occup${oc2}1[_n-1] & occup${oc2}2 != occup${oc2}1[_n-1] )) /*
      
                   CASE 3: occupation missing in one case, after  the unemployment spell
                   */ | ((occup${oc2}1!=. & occup${oc2}2==. & occup${oc2}1[_n-1]!=.&  occup${oc2}2[_n-1]!=.) & ///
                  (occup${oc2}1 != occup${oc2}1[_n-1] & occup${oc2}1 != occup${oc2}2[_n-1] )) /*
      
                  CASE 4: occupation 2 missing in both cases
                   */ | ((occup${oc2}1!=. & occup${oc2}2==. & occup${oc2}1[_n-1]!=.& occup${oc2}2[_n-1]==.) & ///
                  (occup${oc2}1 != occup${oc2}1[_n-1] )))
      
                * none of the four combinations yield an occupational match before and after ne
      
      display "construct nonoccupational changes nonemployment to employment flows"
          replace ne_n_${oc2}=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & (unempl[_n-1]==1|outlf[_n-1]==1) ///
               & (occup${oc2}1 !=.| occup${oc2}2!=.) & (occup${oc2}1[_n-1]!=.|occup${oc2}2[_n-1]!=.) ///  there is information to assign occupational changes
               & ne_c_${oc2}==0
      
           *select those spells that are E(N/U)E
      
      
	  
	  display "N SPELL LENGTH"
      sort personkey yearmonth
      capture drop n_spellength
	  
	  
      gen n_spellength=.
      replace n_spellength=1 if en==1
      replace n_spellength=1+n_spellength[_n-1] if (unempl==1|outlf==1) & n_spellength[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      gsort personkey -yearmonth
      capture drop complete_nspell
      gen complete_nspell=.
      replace complete_nspell=1 if ne[_n-1]==1 & n_spellength!=.
      replace complete_nspell=1 if (unempl==1|outlf==1) & complete_nspell[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1
      
	  gsort personkey -yearmonth
      capture drop complete_nuspell
	  gen complete_nuspell=.
	  replace complete_nuspell=1 if uestar[_n-1]==1 & n_spellength!=.
      replace complete_nuspell=1 if (unempl==1|outlf==1) & complete_nuspell[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1
      
	  
	  display "NUN SPELL ADJUSTMENT"
      gsort personkey -yearmonth
	  capture drop complete_nunspell
	  gen complete_nunspell=complete_nspell
	  
	  replace complete_nunspell=2 if complete_nspell==1 & unempl==1
	  replace complete_nunspell=2 if complete_nspell==1 & (unempl==1 | outlf==1 ) & complete_nunspell[_n-1]==2 & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1
     
	 
	  sort personkey yearmonth
	  replace complete_nunspell=2 if complete_nspell==1 & outlf==1 & complete_nunspell[_n-1]==2 & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
	  replace complete_nunspell=. if complete_nunspell==1
	  replace complete_nunspell=1 if complete_nunspell==2
	
	  sort personkey yearmonth
      display "NSTAR SPELL"
	  capture drop complete_nstarspell
      gen complete_nstarspell=.
      replace complete_nstarspell=1 if complete_nspell==1 & enstar==1
      replace complete_nstarspell=1 if complete_nspell==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & complete_nstarspell[_n-1]==1
      
      
      
      
      
      
      
      
                                                                                                            
                                                                                                            *
      
      
      * ENstarE spells that lead to occ changes, 2-occupation measure
      gsort personkey -yearmonth
      capture drop co_nestar_${digit}
      gen co_nestar_${digit}=.
      replace co_nestar_${digit}=1 if ne_c_${oc2}[_n-1]==1 & complete_nstarspell==1
      replace co_nestar_${digit}=1 if complete_nstarspell==1 & co_nestar_${digit}[_n-1]==1 & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1
      
      capture drop no_nestar_${digit}
      gen no_nestar_${digit}=.
      replace no_nestar_${digit}=1 if ne_n_${oc2}[_n-1]==1 & complete_nstarspell==1
      replace no_nestar_${digit}=1 if complete_nstarspell==1 & no_nestar_${digit}[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1
      
      replace co_nestar_${digit}=0 if no_nestar_${digit}==1
      replace no_nestar_${digit}=0 if co_nestar_${digit}==1
      
      * ENE spells that lead to occ changes, 2-occupation measure
      
      capture drop co_ne_${digit}
      gen co_ne_${digit}=.
      replace co_ne_${digit}=1 if ne_c_${oc2}[_n-1]==1 & complete_nspell==1
      replace co_ne_${digit}=1 if complete_nspell==1 & co_ne_${digit}[_n-1]==1 & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1
      
      capture drop no_ne_${digit}
      gen no_ne_${digit}=.
      replace no_ne_${digit}=1 if ne_n_${oc2}[_n-1]==1 & complete_nspell==1
      replace no_ne_${digit}=1 if complete_nspell==1 & no_ne_${digit}[_n-1]!=. & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]-1
      
      replace co_ne_${digit}=0 if no_ne_${digit}==1
      replace no_ne_${digit}=0 if co_ne_${digit}==1
      
      
      
      
      *     inflow variables ENstarE
      capture drop co_nestar_inflow_${digit}
      gen co_nestar_inflow_${digit}=1 if co_nestar_${digit}==1 & enstar==1
      replace co_nestar_inflow_${digit}=0 if no_nestar_${digit}==1 & enstar==1
      
      capture drop no_nestar_inflow_${digit}
      gen no_nestar_inflow_${digit}=1 if no_nestar_${digit}==1 & enstar==1
      replace no_nestar_inflow_${digit}=0 if co_nestar_${digit}==1 & enstar==1
      
      capture drop co_ne_inflow_${digit}
      gen co_ne_inflow_${digit}=1 if co_ne_${digit}==1 & en==1
      replace co_ne_inflow_${digit}=0 if no_ne_${digit}==1 & en==1
      
      capture drop no_ne_inflow_${digit}
      gen no_ne_inflow_${digit}=1 if no_ne_${digit}==1 & en==1
      replace no_ne_inflow_${digit}=0 if co_ne_${digit}==1 & en==1
      
      

			
			
			
			
			
			
			
			
			
			
			
			
			//=============================================================================
			//***************************************************************************
			//  REPEAT MOBILITY
			///**************************************************************************
			//==============================================================================
			
			
			
**************************************
*** generalized indicators, to keep track of repeat mobility
**************************************

display "GENERALIZED INDICATORS"
sort personkey yearmonth

capture drop gen_u_spell_${digit}
gen byte gen_u_spell_${digit}=.
replace gen_u_spell_${digit}=0 if eu[_n+1]==1 & (sample_timetogo > $sttg+1) & gen_u_spell_${digit}==.
replace gen_u_spell_${digit}=1 if eu==1 & sample_timetogo > $sttg & gen_u_spell_${digit}==.
replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
*replace gen_u_spell_${digit}=2 if (gen_u_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
*replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (empl==1 & gen_u_spell_${digit}[_n-1]==2) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
*replace gen_u_spell_${digit}=3 if (gen_u_spell_${digit}[_n-1]==2 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
*replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==3) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
*replace gen_u_spell_${digit}=4 if (gen_u_spell_${digit}[_n-1]==3 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1

** NOW FILL IN WHETHER there was an occupation change in the new job
replace gen_u_spell_${digit}=21 if ((gen_u_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_c_${oc2}==1)
replace gen_u_spell_${digit}=21 if (empl==1 & gen_u_spell_${digit}[_n-1]==21) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=22 if ((gen_u_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_n_${oc2}==1)
replace gen_u_spell_${digit}=22 if (empl==1 & gen_u_spell_${digit}[_n-1]==22) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1

** carried over to the next unemployment spell
replace gen_u_spell_${digit}=31 if (gen_u_spell_${digit}[_n-1]==21 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==31) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=32 if (gen_u_spell_${digit}[_n-1]==22 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==32) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1

*** then look if the next employment spell is with or without occupational change
replace gen_u_spell_${digit}=411 if (gen_u_spell_${digit}[_n-1]==31 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_c_${oc2}==1
replace gen_u_spell_${digit}=412 if (gen_u_spell_${digit}[_n-1]==31 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_n_${oc2}==1
replace gen_u_spell_${digit}=421 if (gen_u_spell_${digit}[_n-1]==32 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_c_${oc2}==1
replace gen_u_spell_${digit}=422 if (gen_u_spell_${digit}[_n-1]==32 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_n_${oc2}==1

**** ADD GO ANOTHER ROUND
*replace gen_u_spell_${digit}=21 if (empl==1 & (gen_u_spell_${digit}[_n-1]==411|gen_u_spell_${digit}[_n-1]==421)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=411 if (empl==1 & gen_u_spell_${digit}[_n-1]==411) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=412 if (empl==1 & gen_u_spell_${digit}[_n-1]==412) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
*replace gen_u_spell_${digit}=22 if (empl==1 & (gen_u_spell_${digit}[_n-1]==412|gen_u_spell_${digit}[_n-1]==422)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=421 if (empl==1 & gen_u_spell_${digit}[_n-1]==421) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=422 if (empl==1 & gen_u_spell_${digit}[_n-1]==422) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1

** carried over to the next unemployment spell
replace gen_u_spell_${digit}=511 if (gen_u_spell_${digit}[_n-1]==411 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==511) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=512 if (gen_u_spell_${digit}[_n-1]==412 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==512) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=521 if (gen_u_spell_${digit}[_n-1]==421 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==521) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=522 if (gen_u_spell_${digit}[_n-1]==422 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (unempl==1 & gen_u_spell_${digit}[_n-1]==522) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1



*** then look if the next employment spell is with or without occupational change
replace gen_u_spell_${digit}=6111 if (gen_u_spell_${digit}[_n-1]==511 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_c_${oc2}==1
replace gen_u_spell_${digit}=6112 if (gen_u_spell_${digit}[_n-1]==511 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_n_${oc2}==1
replace gen_u_spell_${digit}=6121 if (gen_u_spell_${digit}[_n-1]==512 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_c_${oc2}==1
replace gen_u_spell_${digit}=6122 if (gen_u_spell_${digit}[_n-1]==512 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_n_${oc2}==1
replace gen_u_spell_${digit}=6211 if (gen_u_spell_${digit}[_n-1]==521 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_c_${oc2}==1
replace gen_u_spell_${digit}=6212 if (gen_u_spell_${digit}[_n-1]==521 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_n_${oc2}==1
replace gen_u_spell_${digit}=6221 if (gen_u_spell_${digit}[_n-1]==522 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_c_${oc2}==1
replace gen_u_spell_${digit}=6222 if (gen_u_spell_${digit}[_n-1]==522 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ue_n_${oc2}==1


replace gen_u_spell_${digit}=gen_u_spell_${digit}[_n-1] if (gen_u_spell_${digit}[_n-1]>6000 & gen_u_spell_${digit}[_n-1]<7000 & empl==1) ///
                                & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1

*** then look if the next employment spell is with or without occupational change
replace gen_u_spell_${digit}=7111 if (gen_u_spell_${digit}[_n-1]==6111 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=7112 if (gen_u_spell_${digit}[_n-1]==6112 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=7121 if (gen_u_spell_${digit}[_n-1]==6121 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=7122 if (gen_u_spell_${digit}[_n-1]==6122 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=7211 if (gen_u_spell_${digit}[_n-1]==6211 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=7212 if (gen_u_spell_${digit}[_n-1]==6212 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=7221 if (gen_u_spell_${digit}[_n-1]==6221 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
replace gen_u_spell_${digit}=7222 if (gen_u_spell_${digit}[_n-1]==6222 & unempl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1


display "GENERALIZED INDICATORS: FOR N/Nstar transitions"
      sort personkey yearmonth
      
      capture drop gen_n_spell_${digit}
      gen byte gen_n_spell_${digit}=.
      replace gen_n_spell_${digit}=0 if en[_n+1]==1 & sample_timetogo > $sttg+1 & gen_n_spell_${digit}==.
      replace gen_n_spell_${digit}=1 if en==1 & sample_timetogo > $sttg & gen_n_spell_${digit}==.
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      *replace gen_n_spell_${digit}=2 if (gen_n_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      *replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if (empl==1 & gen_n_spell_${digit}[_n-1]==2) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      *replace gen_n_spell_${digit}=3 if (gen_n_spell_${digit}[_n-1]==2 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      *replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==3) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      *replace gen_n_spell_${digit}=4 if (gen_n_spell_${digit}[_n-1]==3 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
         ***NOW FILL IN WHETHER there was an occupation change in the new job
         ***        the spells that remain 2,3,4, we likely cannot assign an occupation, because the worker was working, but not in the job for which the
         ***        occupation is stored, according to the starting date.
      
      replace gen_n_spell_${digit}=21 if ((gen_n_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1)
      replace gen_n_spell_${digit}=21 if (empl==1 & gen_n_spell_${digit}[_n-1]==21) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=22 if ((gen_n_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1)
      replace gen_n_spell_${digit}=22 if (empl==1 & gen_n_spell_${digit}[_n-1]==22) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
         ** carried over to the next nonemployment spell
      replace gen_n_spell_${digit}=31 if (gen_n_spell_${digit}[_n-1]==21 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==31) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=32 if (gen_n_spell_${digit}[_n-1]==22 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==32) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
        * then look if the next employment spell is with or without occupational change
      replace gen_n_spell_${digit}=411 if (gen_n_spell_${digit}[_n-1]==31 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_n_spell_${digit}=412 if (gen_n_spell_${digit}[_n-1]==31 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_n_spell_${digit}=421 if (gen_n_spell_${digit}[_n-1]==32 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_n_spell_${digit}=422 if (gen_n_spell_${digit}[_n-1]==32 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      
           **ADD GO ANOTHER ROUND
      
      replace gen_n_spell_${digit}=411 if (empl==1 & gen_n_spell_${digit}[_n-1]==411) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=412 if (empl==1 & gen_n_spell_${digit}[_n-1]==412) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      replace gen_n_spell_${digit}=421 if (empl==1 & gen_n_spell_${digit}[_n-1]==421) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=422 if (empl==1 & gen_n_spell_${digit}[_n-1]==422) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
         ** carried over to the next nonemployment spell
      replace gen_n_spell_${digit}=511 if (gen_n_spell_${digit}[_n-1]==411 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==511) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=512 if (gen_n_spell_${digit}[_n-1]==412 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==512) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=521 if (gen_n_spell_${digit}[_n-1]==421 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==521) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=522 if (gen_n_spell_${digit}[_n-1]==422 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_n_spell_${digit}[_n-1]==522) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      
        * then look if the next employment spell is with or without occupational change
      replace gen_n_spell_${digit}=6111 if (gen_n_spell_${digit}[_n-1]==511 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_n_spell_${digit}=6112 if (gen_n_spell_${digit}[_n-1]==511 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_n_spell_${digit}=6121 if (gen_n_spell_${digit}[_n-1]==512 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_n_spell_${digit}=6122 if (gen_n_spell_${digit}[_n-1]==512 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_n_spell_${digit}=6211 if (gen_n_spell_${digit}[_n-1]==521 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_n_spell_${digit}=6212 if (gen_n_spell_${digit}[_n-1]==521 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_n_spell_${digit}=6221 if (gen_n_spell_${digit}[_n-1]==522 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_n_spell_${digit}=6222 if (gen_n_spell_${digit}[_n-1]==522 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      
      
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if (gen_n_spell_${digit}[_n-1]>6000 & gen_n_spell_${digit}[_n-1]<7000 & empl==1) ///
                                      & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
        * then look if the next employment spell is with or without occupational change
      replace gen_n_spell_${digit}=7111 if (gen_n_spell_${digit}[_n-1]==6111 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=7112 if (gen_n_spell_${digit}[_n-1]==6112 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=7121 if (gen_n_spell_${digit}[_n-1]==6121 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=7122 if (gen_n_spell_${digit}[_n-1]==6122 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=7211 if (gen_n_spell_${digit}[_n-1]==6211 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=7212 if (gen_n_spell_${digit}[_n-1]==6212 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=7221 if (gen_n_spell_${digit}[_n-1]==6221 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_n_spell_${digit}=7222 if (gen_n_spell_${digit}[_n-1]==6222 & (unempl==1|outlf==1)) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      replace gen_n_spell_${digit}=gen_n_spell_${digit}[_n-1] if (gen_n_spell_${digit}[_n-1]>7000 & gen_n_spell_${digit}[_n-1]<8000 & (unempl==1|outlf==1)) ///
                                      & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      
          * NEstar
      
      
      capture drop gen_nstar_spell_${digit}
      gen byte gen_nstar_spell_${digit}=.
      replace gen_nstar_spell_${digit}=1 if enstar==1 & sample_timetogo > $sttg & gen_nstar_spell_${digit}==.
      replace gen_nstar_spell_${digit}=0 if gen_nstar_spell_${digit}[_n+1]==1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|outlf==1) & gen_nstar_spell_${digit}[_n-1]==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=2 if (gen_nstar_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if (empl==1 & gen_nstar_spell_${digit}[_n-1]==2) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=3 if (gen_nstar_spell_${digit}[_n-1]==2 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|(outlf==1)) & gen_nstar_spell_${digit}[_n-1]==3) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=4 if (gen_nstar_spell_${digit}[_n-1]==3 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
         ** NOW FILL IN WHETHER there was an occupation change in the new job
      replace gen_nstar_spell_${digit}=21 if  ((gen_nstar_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1)
      replace gen_nstar_spell_${digit}=21 if (empl==1 & gen_nstar_spell_${digit}[_n-1]==21) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=22 if ((gen_nstar_spell_${digit}[_n-1]==1 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1)
      replace gen_nstar_spell_${digit}=22 if (empl==1 & gen_nstar_spell_${digit}[_n-1]==22) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
         ** carried over to the next unemployment spell
      replace gen_nstar_spell_${digit}=31 if (gen_nstar_spell_${digit}[_n-1]==21 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|(outlf==1)) & gen_nstar_spell_${digit}[_n-1]==31) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=32 if (gen_nstar_spell_${digit}[_n-1]==22 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|(outlf==1)) & gen_nstar_spell_${digit}[_n-1]==32) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
        * then look if the next employment spell is with or without occupational change
      replace gen_nstar_spell_${digit}=411 if (gen_nstar_spell_${digit}[_n-1]==31 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_nstar_spell_${digit}=412 if (gen_nstar_spell_${digit}[_n-1]==31 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_nstar_spell_${digit}=421 if (gen_nstar_spell_${digit}[_n-1]==32 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_nstar_spell_${digit}=422 if (gen_nstar_spell_${digit}[_n-1]==32 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      
           * ADD GO ANOTHER ROUND
      
      replace gen_nstar_spell_${digit}=411 if (empl==1 & gen_nstar_spell_${digit}[_n-1]==411) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=412 if (empl==1 & gen_nstar_spell_${digit}[_n-1]==412) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      replace gen_nstar_spell_${digit}=421 if (empl==1 & gen_nstar_spell_${digit}[_n-1]==421) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=422 if (empl==1 & gen_nstar_spell_${digit}[_n-1]==422) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
         * carried over to the next unemployment spell
      replace gen_nstar_spell_${digit}=511 if (gen_nstar_spell_${digit}[_n-1]==411 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|(outlf==1)) & gen_nstar_spell_${digit}[_n-1]==511) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=512 if (gen_nstar_spell_${digit}[_n-1]==412 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|(outlf==1)) & gen_nstar_spell_${digit}[_n-1]==512) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=521 if (gen_nstar_spell_${digit}[_n-1]==421 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|(outlf==1)) & gen_nstar_spell_${digit}[_n-1]==521) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=522 if (gen_nstar_spell_${digit}[_n-1]==422 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if ((unempl==1|(outlf==1)) & gen_nstar_spell_${digit}[_n-1]==522) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      
        * then look if the next employment spell is with or without occupational change
      replace gen_nstar_spell_${digit}=6111 if (gen_nstar_spell_${digit}[_n-1]==511 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_nstar_spell_${digit}=6112 if (gen_nstar_spell_${digit}[_n-1]==511 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_nstar_spell_${digit}=6121 if (gen_nstar_spell_${digit}[_n-1]==512 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_nstar_spell_${digit}=6122 if (gen_nstar_spell_${digit}[_n-1]==512 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_nstar_spell_${digit}=6211 if (gen_nstar_spell_${digit}[_n-1]==521 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_nstar_spell_${digit}=6212 if (gen_nstar_spell_${digit}[_n-1]==521 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      replace gen_nstar_spell_${digit}=6221 if (gen_nstar_spell_${digit}[_n-1]==522 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_c_${oc2}==1
      replace gen_nstar_spell_${digit}=6222 if (gen_nstar_spell_${digit}[_n-1]==522 & empl==1) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1 & ne_n_${oc2}==1
      
      
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if (gen_nstar_spell_${digit}[_n-1]>6000 & gen_nstar_spell_${digit}[_n-1]<7000 & empl==1) ///
                                      & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
        * then look if the next employment spell is with or without occupational change
      replace gen_nstar_spell_${digit}=7111 if (gen_nstar_spell_${digit}[_n-1]==6111 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=7112 if (gen_nstar_spell_${digit}[_n-1]==6112 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=7121 if (gen_nstar_spell_${digit}[_n-1]==6121 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=7122 if (gen_nstar_spell_${digit}[_n-1]==6122 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=7211 if (gen_nstar_spell_${digit}[_n-1]==6211 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=7212 if (gen_nstar_spell_${digit}[_n-1]==6212 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=7221 if (gen_nstar_spell_${digit}[_n-1]==6221 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      replace gen_nstar_spell_${digit}=7222 if (gen_nstar_spell_${digit}[_n-1]==6222 & (unempl==1|(outlf==1 $ersendc))) & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
      replace gen_nstar_spell_${digit}=gen_nstar_spell_${digit}[_n-1] if (gen_nstar_spell_${digit}[_n-1]>7000 & gen_nstar_spell_${digit}[_n-1]<8000 & (unempl==1|outlf==1)) ///
                                      & personkey[_n-1]==personkey & yearmonth==yearmonth[_n-1]+1
      
          * add the employment durations after n-spells
      capture noisily drop employ_dur
	  gen employ_dur=.
	  
      replace employ_dur=0 if (gen_n_spell_${digit}==21|gen_n_spell_${digit}==22) & gen_n_spell_${digit}[_n-1]==1
      replace employ_dur=employ_dur[_n-1]+1 if (gen_n_spell_${digit}==21|gen_n_spell_${digit}==22) & (gen_n_spell_${digit}[_n-1]==21|gen_n_spell_${digit}[_n-1]==22)
      
      replace employ_dur=0 if (gen_n_spell_${digit}==411|gen_n_spell_${digit}==412|gen_n_spell_${digit}==421|gen_n_spell_${digit}==422) & (gen_n_spell_${digit}[_n-1]==31|gen_n_spell_${digit}[_n-1]==32)
      replace employ_dur=employ_dur[_n-1]+1 if (gen_n_spell_${digit}==411|gen_n_spell_${digit}==412|gen_n_spell_${digit}==421|gen_n_spell_${digit}==422) & employ_dur[_n-1]>=0
      
      
      replace employ_dur=0 if (gen_n_spell_${digit}==6111|gen_n_spell_${digit}==6112|gen_n_spell_${digit}==6121|gen_n_spell_${digit}==6122 ///
                              | gen_n_spell_${digit}==6211|gen_n_spell_${digit}==6212|gen_n_spell_${digit}==6221|gen_n_spell_${digit}==6222) ///
                              & (gen_n_spell_${digit}[_n-1]==511|gen_n_spell_${digit}[_n-1]==512|gen_n_spell_${digit}[_n-1]==521|gen_n_spell_${digit}[_n-1]==522)
      replace employ_dur=employ_dur[_n-1]+1 if (gen_n_spell_${digit}==6111|gen_n_spell_${digit}==6112|gen_n_spell_${digit}==6121|gen_n_spell_${digit}==6122 ///
                              | gen_n_spell_${digit}==6211|gen_n_spell_${digit}==6212|gen_n_spell_${digit}==6221|gen_n_spell_${digit}==6222) ///
                               & employ_dur[_n-1]>=0
      
      
			
			//===================================
			// DIFFERENT KINDS OF NONEMPLOYMENT
			//====================================
			

			/*
			- U			pure unemployment spells
			- UNU		initially searching, finally searching
			- UN		initially searching, 
			- NU		finally searching
			- NUN		any search behavior
			- N			completed nonemployment spells (U or N)
			- NSTAR		some attachmetn in reason for job dismissal	
			
			
			
			
			*/
			
			
			
			capture drop nspell_ind
			gen nspell_ind=complete_nspell
			idfillreplace nspell_ind
			
			su wpfinwgt if panel==1984
			if r(mean)>=1000000 {
			replace wpfinwgt=wpfinwgt/10000 if panel==1984
			}

			save "${outputdata}\corewave_all_ctv.dta", replace
