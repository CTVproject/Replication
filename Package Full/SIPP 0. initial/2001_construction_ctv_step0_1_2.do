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
//  SIPP 2001 PANEL, LOAD IN DATA, DEFINE VARIABLES FOR USE
//======================================


/*


1) load in data for each panel. For each panel do the following:

a) drop noninterviews
b) drop SE
c) define E/U/O
d) define active/dominant firms for each 
e) highlight government employees

2) define occupation variables used for calculating occupational transitions. NO IMPUTATIONS

3) first iteration: sort out entry in the labour force and retirement



*/


		********************************************************************************************************************************************************
		** PREAMBLE
		*********************************************************************************************************************************************************

		clear
        clear matrix
        clear mata
        
        version 13
		*capture log close
		*set mem 3000m
		set more off
		set maxvar 10000


		** GLOBALS
		global gnulin=0
		global yeardata=04
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
			use 2001total_v${fileversion}.dta, clear
			cd "$outputdata"
			cap n gen ewk1bfor=nojobbeforew1
			capture gen ehowmany=yearsnotworked6m
			cap gen eyrsince=alwayswork6m
			
		keep personkey wpfinwgt hsc strat panel lgtkey lgtpnwt1 lgtpnwt2 lgtpnwt3 lgtcy1wt lgtcy2wt lgtcy3wt  /* PERSON IDENTIFIERS
		*/ wave yearmonth rhcalmn rhcalyr  yearmonth rot /*
		*/ eppintvw eoutcome eoutcome_tm1 eppintvw_tm1 eppmis4_tm1 /*
		*/ sex asex race arace tage aage ems ams tfipsst eorigin aorigin eafnow aafnow/*
		*/ educ eeducate aeducate renroll arenroll eenrlm aenrlm /*
		*/ eeveret aeveret ersnowrk arsnowrk /*
		*/ elkwrk rtakjob rnotake eabre ebuscntr emoonlit edisabl edisprev eptresn alkwrk aabre amoonlit adisabl adisprev aptresn /*
		*/ epdjbthn apdjbthn elayoff alayoff ejbhrs1 ejbhrs2 ajbhrs* eclwrk1 eclwrk2 aclwrk* /*
		*/ eeno1 eeno2 estlemp1 astlemp1 estlemp2 astlemp2 tejdate* tsjdate* aejdate* asjdate* /*
    	*/ tjbocc1 tjbocc2 ajbocc1 ajbocc2 ejbind1 ejbind2 ajbind1 ajbind2/*
		*/ rwkesr* rmesr rwksperm /*
    	*/ eeveret aeveret ersnowrk arsnowrk /*
		*/ eocctim1 aocctim1 eocctim2 aocctim2/* no longer information whether tm1 previous job was self-employment or paid employment
    	*/ tpmsum1 tpmsum2 apmsum1 apmsum2 /*
        */ ersend1 ersend2 epayhr1 epayhr2  arsend1 arsend2 apayhr1 apayhr2 rpyper1 rpyper2 tpyrate1 tpyrate2 apyrate1 apyrate2/*
    	*/ ehrsbs1 ahrsbs1 ehrsbs2 ahrsbs2 /*
    	*/ tlstwrky elstwrkm tprvjbyr eprvjbmn tfrmryr efrmrmn firstyearlf emnreson alwayswork6m eyrsince eyrsinc2 lf_ft /* LABHIST TM1
		*/ caregive_nlf recentbreak_care6mfr recentbreak_care6mto eothtime ecntothr tfstyrfr tfstyrto enwresn alstwrky /* LABHIST TM1
    	*/ alstwrkm aprvjbyr aprvjbmn afrmryr afrmrmn amakmnyr amnreson ayrsince ayrsince ayrsinc2 awrk35hr aoff6mtn anowrkfr anowrkto aothtime /* LABHIST TM1
    	*/ acntothr afstyrfr afstyrto anwresn endprevjb_tm endprevjbyr_tm startprevjb_tm startprevjbyr_tm/* LABHIST TM1
    	*/ yearslf continuous_spell/*
    	*/ ebno1 ebno2 tsbdate1 tsbdate2 tebdate* aebdate* asbdate* ebuscntr ejobcntr
		
		
		
		
		
		cap n ren ems ms
		tab wave

		

		save "2001_corewave_occmob.dta", replace

		// drop those with ambiguous interview status
		tab eppintvw eoutcome, m
		tab eppintvw eoutcome, m nol
		drop if eppintvw>=3
		
		// drop: those with army, self-employment, before 18, after 66, those who have not yet worked 6months
		
		
** drop if selfemployed
        /*(two things can be done here: drop self-employment spells,
        and those people who are self-employed at some time; we opt for the
        latter */
		tab ebuscntr ejobcntr,m 
		
		display "------ DROPPING SELFEMPLOYED -----"        
		capture drop se_ind
		gen se_ind=.
		capture replace se_ind=1 if (ebuscntr>=1&ebuscntr!=.)
		capture replace se_ind=1 if (ebno1!=.|ebno2!=.)
		

		sort personkey yearmonth
		by personkey: egen maxse=max(se_ind)

    quietly {
			cap log close se_count2001_log
			log using "${outputdata}/se_drop_counter2001.txt", replace text name(se_count2001_log)

			noisily: display  ""
			noisily: display  "--------------------------------------------------------"
			noisily: display  "   SE drops among total population, 2001 panel "
			noisily: display  "--------------------------------------------------------"
			noisily: display  ""
			noisily: display  "count of all observations:"
            noisily: count
			noisily: display  "count of all SE observations to be dropped next"
            noisily: count if maxse==1
			

			log close se_count2001_log
				

			}


		drop if maxse==1
		drop maxse
		drop se_ind


        	
		** drop if too young or too old
		drop if tage<18 | tage>66

		** drop if in armed forces
		tab eafnow
		drop if eafnow==1
		drop eafnow

		
	
		drop ebno1 ebno2 tsbdate1 tsbdate2 tebdate* aebdate* asbdate* ebuscntr ehrsbs1 ahrsbs1 ehrsbs2 ahrsbs2 
		
		//===============================
		// MONTHLY EMPLOYMENT STATUS
		//================================
				// partially shared with retrospective_occmob_2004
				
				
		*FP/CORE Employment status, basic
		capture drop fp_enu
		gen fp_enu=.
		capture label define enu_label 1 "E" 2 "U" 3 "N"
		label val fp_enu enu_label
		format %2.0g fp_enu
		replace fp_enu=1 if rmesr>=1 & rmesr<=5
		replace fp_enu=2 if rmesr>=6 & rmesr<=7
		replace fp_enu=3 if rmesr==8 
		
		
		/*
		* make sure weights are integer
		capture drop fwpfinwgt
		gen fwpfinwgt=floor(wpfinwgt)
		ren wpfinwgt wpfinwgt_old
		ren fwpfinwgt wpfinwgt
		*/

	if $heading_ind==1 {
		display as error "======================================="
		display as error " --- OCCUPATIONAL RECODING ---- 			  "
		display as error "======================================="
		}
														
														
				* recode occupations (according to the David Dorn's mapping into 1990 SOC, then aggregated)
			 
			do "$codedir\1992_2001_recode_occdd_90_to_90_step0_1_2.do"
			cd "$outputdata"
	
		recode_dd90_to_90 tjbocc1 
		recode_dd90_to_90 tjbocc2 
		*recode_dd90_to_90 occprevjb 
		
		/*
		capture drop donotconsider_dum
		gen byte donotconsider_dum=.
		
		* using the SOC1990 aggregation
		
		capture program drop aggregate_occ_9201
		program define aggregate_occ_9201


			set more off
			if "`1'"!="" {

			set more off
			set varabbrev off

			global occupation  "`1'"

			// AGGREGATION FOR 1990 SOC ----- THIS IS THE OLD STEPTWO9201.do program	
			/* note that since the occupations are homogenized to SOC1990 beforehand, this step is essentially the same
			for all panels */

			capture gen ${occupation}_1d=.

			forvalues i=4(1)37 {
					replace ${occupation}_1d=1 if ${occupation}==`i'  
					}

			forvalues i=43(1)199 {
					replace ${occupation}_1d=2 if ${occupation}==`i'  
					}
					
			forvalues i=203(1)235 {
					replace ${occupation}_1d=3 if ${occupation}==`i'  
					}

			forvalues i=243(1)285 {
					replace ${occupation}_1d=4 if ${occupation}==`i'  
					}

			forvalues i=303(1)389 {
					replace ${occupation}_1d=5 if ${occupation}==`i'  
					}

			forvalues i=403(1)407 {
					replace ${occupation}_1d=8 if ${occupation}==`i'  
					}

			forvalues i=413(1)427 {
					replace ${occupation}_1d=8 if ${occupation}==`i'  
					}

			forvalues i=433(1)469 {
					replace ${occupation}_1d=8 if ${occupation}==`i'  
					}

			forvalues i=473(1)476 {
					replace ${occupation}_1d=9 if ${occupation}==`i'  
					}

			forvalues i=477(1)499 {
					replace ${occupation}_1d=9 if ${occupation}==`i'  
					}

			forvalues i=503(1)549 {
					replace ${occupation}_1d=11 if ${occupation}==`i'  
					}

			forvalues i=553(1)617 {
					replace ${occupation}_1d=12 if ${occupation}==`i'  
					}

			forvalues i=633(1)699 {
					replace ${occupation}_1d=13 if ${occupation}==`i'  
					}

			forvalues i=703(1)799 {
					replace ${occupation}_1d=14 if ${occupation}==`i'  
					}

			forvalues i=803(1)859 {
					replace ${occupation}_1d=15 if ${occupation}==`i'  
					}

			forvalues i=863(1)889 {
					replace ${occupation}_1d=16 if ${occupation}==`i'  
					}
			}
		end 

		*run the program (aggregate_occ_8691) for the variables in question
		aggregate_occ_9201 tjbocc1 
		aggregate_occ_9201 tjbocc2 
		*aggregate_occ_9201 occprevjb 
		*/
	
		
	






//-------------------------------------------
// telling stata about the SURVEY DESIGN
//---------------------------------------------------

**svyset ghlfsam [pweight=wpfnwgt] , strata(gvarstr) fay(0.5)|| lgtkey                       // if we want to use the data as repeated cross-sections
**svyset ghlfsam [pweight=LGTPNLWT] , strata(gvarstr) fay(0.5)|| lgtkey               // if we want to use the data as panel, or if we want only those people with all 12 interviews
*svyset ghlfsam [pweight=wpfnwgt] , strata(gvarstr) fay(0.5)|| lgtkey,  singleunit(centered)




					// drop empty observations
					drop if personkey==""



					describe educ 
					tab educ
					sort panel personkey yearmonth
					



					//==================================
					// DATES (start date / end date) 
					//==================================




					*** time series in quarters
					capture drop quarter
					gen quarter=qofd(dofm(yearmonth))
					format yearmonth %tm
					format quarter %tq

					capture drop secondweek_date
					gen secondweek_date=dofm(yearmonth)+14

					// backup eeno 
					gen byte backup_eeno1=eeno1
					gen byte backup_eeno2=eeno2

					// backup start/enddates
					gen tsjdate1_backup=tsjdate1
					gen tsjdate2_backup=tsjdate2
					gen tejdate1_backup=tejdate1
					gen tejdate2_backup=tejdate2


					// EENO
					**** CONVERT DATES INTO STATA FORMAT



					 // THERE IS SOME **STUPIDITY** IN THE DATA: dates are reported like 31 feb 1998, and STATA skips these. We have to deal with this.
					 // GENERATING END DATES


					// generate string endtimes temporarily
					capture drop tenddate1string 
					capture drop tenddate2string 

					gen tenddate1string=string(tejdate1, "%15.0g")
					gen tenddate2string=string(tejdate2, "%15.0g")



					capture drop enddate1
					capture drop enddate2
					gen enddate1=date(tenddate1string, "YMD")
					gen enddate2=date(tenddate2string, "YMD")


					format enddate1 %td
					format enddate2 %td

					// CORRECT FOR UTTER STUPIDITY: yes, february 31st exists! 

					// ***** check: subtract up to a week from the mistaken data
					count if tejdate1!=. & tejdate1!=-1 & enddate1==.
					count if tejdate2!=. & tejdate2!=-1 & enddate2==.


					forvalues ii=1(1)2	{
					forvalues h=1(1)7		{
										count if tejdate`ii'!=. & tejdate`ii'!=-1 & enddate`ii'==.
										if r(N)>0 {
										capture drop temp_ind
										gen byte temp_ind=0
										replace temp_ind=1 if tejdate`ii'!=. & tejdate`ii'!=-1 & enddate`ii'==.	
										replace tejdate`ii'=tejdate`ii'-1 if temp_ind==1
										replace tenddate`ii'string=string(tejdate`ii', "%15.0g") if temp_ind==1
										replace enddate`ii'=date(tenddate`ii'string, "YMD") if temp_ind==1
												}
									}
									}


					drop tenddate1string tenddate2string

					*capture ren tejdate1 tejdate_old
					*capture ren enddate1 tejdate1

					*capture ren tejdate2 tejdate_old
					*capture ren enddate2 tejdate2

					// GENERATE STARTING DATES

					capture drop tstartdate1string
					capture drop tstartdate2string 


					gen tstartdate1string=string(tsjdate1, "%15.0g")
					gen tstartdate2string=string(tsjdate2, "%15.0g")

					capture drop startdate1
					capture drop startdate2

					gen startdate1=date(tstartdate1string, "YMD")
					gen startdate2=date(tstartdate2string, "YMD")

					format startdate1 %td
					format startdate2 %td

					// CORRECT FOR STUPIDITY
					count if tsjdate1!=. & tsjdate1!=-1 & startdate1==.
					count if tsjdate2!=. & tsjdate2!=-1 & startdate2==.


					forvalues ii=1(1)2	{
					forvalues h=1(1)7		{
										count if tsjdate`ii'!=. & tsjdate`ii'!=-1 & startdate`ii'==.
										if r(N)>0 {
										capture drop temp_ind
										gen byte temp_ind=0
										replace temp_ind=1 if tsjdate`ii'!=. & tsjdate`ii'!=-1 & startdate`ii'==.	
										replace tsjdate`ii'=tsjdate`ii'-1 if temp_ind==1
										replace tstartdate`ii'string=string(tsjdate`ii', "%15.0g") if temp_ind==1
										replace startdate`ii'=date(tstartdate`ii'string, "YMD") if temp_ind==1
												}
									}
									}



					drop tstartdate1string tstartdate2string
					capture drop tejdate1 tejdate2
					capture drop tsjdate1 tsjdate2

					capture ren startdate1 tsjdate1
					capture ren startdate2 tsjdate2
					capture ren enddate1 tejdate1
					capture ren enddate2 tejdate2

						
						  * in particular, not worrying about ambig_master, just whether occupational tenure, tsjdate_startpanel or endprevjb_tm (or at least endprevjbyr_tm) is imputed 
						
					capture drop intvw_month
					gen intvw_month=ym(2001, 2) if rot==1 & wave==1
					replace intvw_month=ym(2001, 3) if rot==2 & wave==1
					replace intvw_month=ym(2001, 4) if rot==3 & wave==1
					replace intvw_month=ym(2001, 5) if rot==4 & wave==1
	

						// interview no: 
						sort personkey yearmonth
						capture drop interview_no
						gen interview_no=1 if personkey!=personkey[_n-1]
						replace interview_no=interview_no[_n-1]+1 if personkey==personkey[_n-1]

			//================================
			//  LABOR FORCE STATUS 
			//================================
				

					*Create employment, unemployment and non-particpation indicators
					*Based on esr= monthly labour force indicator
			gen byte mempl = 0
			replace mempl = 1 if rmesr==1 | rmesr==2 | rmesr==3 | rmesr==4 | rmesr==5
			*1 With a job entire month, worked all weeks.
			*2 With a job all month, absent from work w/out pay 1+ weeks, absence not due to layoff
			*3 With job all month, absent from work w/out pay 1+ weeks, absence due to layoff
			*4 With a job at least 1 but not all weeks, no time on layoff and no time looking for work
			*5 With job at least 1 but not all weeks, some weeks on layoff or looking for work

			gen byte munempl = 0
			replace munempl =1 if rmesr==6 | rmesr==7
			*6 No job all month, on layoff or looking for work all weeks.
			*7 No job, at least one but not all weeks on layoff or looking for work

			gen byte moutlf = 0
			replace moutlf =1 if rmesr==8
			*8 No job, no time on layoff and no time looking for work.

			gen byte mlaborforce =0
			replace mlaborforce =1 if mempl==1 | munempl==1


			*** EMPLOYMENT/UNEMPLOYMENT/NLF BASED ON WEEKLY RECODES -- second week
						**** use full panel recodes because they are consistent with starting dates 
			sort personkey yearmonth
			gen byte empl = 0
			replace empl = 1 if ((rwkesr2==1|rwkesr2==2|rwkesr2==3) | rmesr==1| rmesr==2| rmesr==3)
			*1 With a job entire month, worked all weeks.
			*2 With a job all month, absent from work w/out pay 1+ weeks, absence not due to layoff
			*3 With job all month, absent from work w/out pay 1+ weeks, absence due to layoff
			*4 With a job at least 1 but not all weeks, no time on layoff and no time looking for work
			*5 With job at least 1 but not all weeks, some weeks on layoff or looking for work

			sort personkey yearmonth
			gen byte unempl = 0
			replace unempl =1 if rwkesr2==4  &  rmesr!=1 &  rmesr!=2 &  rmesr!=3
			replace unempl =1 if rwkesr2==5 & rwkesr1==4  &  rmesr!=1 &  rmesr!=2 &  rmesr!=3
			replace unempl =1 if rwkesr2==5 & yearmonth==yearmonth[_n-1]+1 & personkey==personkey[_n-1] &(rwkesr5[_n-1]==4|rwkesr4[_n-1]==4|rwkesr3[_n-1]==4|rwkesr2[_n-1]==4) &  rmesr!=1 &  rmesr!=2 &  rmesr!=3
			replace unempl =1 if rwkesr2==5 & yearmonth!=yearmonth[_n-1]+1 & personkey!=personkey[_n-1] & yearmonth==yearmonth[_n+1]-1 &  rmesr!=1 &  rmesr!=2 &  rmesr!=3 ///
															& personkey==personkey[_n+1] & empl[_n+1]==1
			* LOOKING IN WEEK 2, not looking in week 2 but in previous month

			gen byte outlf = 0
			replace outlf =1 if (unempl==0 & empl==0)

			gen byte laborforce =0
			replace laborforce =1 if empl==1 | unempl==1


			*** CARRY OVER STARTDATES
			sort personkey yearmonth

			
			
			
			
			//===================================
			// MARK ACTIVE/DOMINANT FIRMS: TIE-BREAKING 1
			//===================================
						
			capture drop firmno
			gen firmno=.
			replace firmno= eeno1 if   eeno1!=. &  eeno2==. & empl==1
			replace firmno= eeno2 if   eeno2!=. &  eeno1==. & empl==1 & firmno==.

			// by tej/tsj
			replace firmno= eeno1 if  eeno1!=. &  eeno2!=. & empl==1 & (tsjdate1!=. & tsjdate1< dofm(yearmonth)+14) & (tejdate1!=. & tejdate1>dofm(yearmonth)+6) & (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)
			replace firmno= eeno2 if  eeno1!=. &  eeno2!=. & empl==1 & (tsjdate2!=. & tsjdate2< dofm(yearmonth)+14) & (tejdate2!=. & tejdate2>dofm(yearmonth)+6) & (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)

			// by hours, then by income
			replace firmno= eeno1 if ( eeno1!=. &  eeno2!=.) & ( ejbhrs1>= ejbhrs2) &  ejbhrs1!=. &  ejbhrs2!=. & empl==1 & firmno==. & firmno==.
			replace firmno= eeno2 if ( eeno1!=. &  eeno2!=.) & ( ejbhrs2>  ejbhrs1) &  ejbhrs1!=. &  ejbhrs2!=. & empl==1 & firmno==. & firmno==.
			replace firmno= eeno1 if ( eeno1!=. &  eeno2!=.) & ( tpmsum1>=  tpmsum2) &  tpmsum1!=. &  tpmsum2!=. & empl==1 & firmno==. & firmno==.
			replace firmno= eeno2 if ( eeno1!=. &  eeno2!=.) & ( tpmsum2>  tpmsum1) &  tpmsum1!=. &  tpmsum2!=. & empl==1 & firmno==. & firmno==.

			// set firmno to missing, if the end date is past, or starting date not yet there

            


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

				replace gov_ind1=1 if (eclwrk1==3|eclwrk1==4|eclwrk1==5) & empl==1
				replace gov_ind2=1 if (eclwrk2==3|eclwrk2==4|eclwrk2==5) & empl==1
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
				***  TAKING INTO ACCOUNT ALL VISIBLE OCCUPATIONAL HISTORY
				***********************************
							    		* this code uses the occupation of the dominant firm
										* and uses recall to get rid of occupations that the worker has visited before 
										* occupation is set to missing if worker becomes nlf
										* in this version: use full panel files!!!! Since all we need from firms is occupation, not firm identity, it's okay that we do not use the revised jobids 1990-1993
										* it is important to use the full panel files here, since otherwise ejbhrs is wavely, instead of monthly, and will not contain additional information...

				capture drop occup1da
				capture drop occup1s
				gen occup1da=.

				replace occup1da= tjbocc1_dd_1d if firmno== eeno1 & empl==1 & ajbocc1==0
				replace occup1da= tjbocc2_dd_1d if firmno== eeno2 & empl==1 & ajbocc2==0

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

				replace occup1tm1=tjbocc1_dd_1d if empl==1 & tjbocc1_dd_1d!=. & ajbocc1==0
				replace occup1tm2=tjbocc2_dd_1d if empl==1 & tjbocc2_dd_1d!=. & ajbocc2==0



				** set occupation to missing, if starting date hasn't arrived, or ending date has passed
				replace occup1tm1=.  if (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup1tm2=.  if (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if employment spell continuous and firm is the same
				replace occup1tm1=occup1tm1[_n-1]  if personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & occup1tm1[_n-1]!=. & occup1tm1==.
				replace occup1tm2=occup1tm2[_n-1]  if personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & occup1tm2[_n-1]!=. & occup1tm2==.


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
		
		capture drop nlfschool
		gen byte nlfschool=0
		cap n replace nlfschool=1 if outlf==1 & ersnowrk==7
		replace nlfschool=1 if outlf==1 & eenrlm==1
		replace nlfschool=1 if outlf==1 & empl[_n-1]==1 & (ersend1==7 | ersend2==7)  & personkey==personkey[_n-1] 
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
		replace retired = 1 if ersnowrk == 4 & outlf==1
		replace retired = 1 if outlf==1 & empl[_n-1]==1 & (ersend1==2 | ersend2==2)
		replace retired = 1 if outlf==1 & retired[_n-1]==1 & personkey==personkey[_n-1] 
		lab var retired "Person doesn't work this month because of retirement this month"

		// replace subsequent history by outlf
		replace retired = 1 if retired[_n-1]==1 & personkey==personkey[_n-1] & tage>=55
		replace unempl=0 if retired==1
		replace empl=0 if retired==1
		replace outlf=1 if retired==1
		
		
		tab wave
		compress
		
		
		
		
		
		drop backup_eeno1 backup_eeno2 tsjdate1_backup tsjdate2_backup tejdate1_backup tejdate2_backup temp_ind previous_firmid2 previous_firmid1 previous_firmid1 samefirm
		
		cd "$outputdata"
		save "2001_corewave_occmob.dta", replace
		*use "2001_corewave_occmob.dta", clear
		

		// drop as many variables as possible (topical module stuff merged in later)
		
			capture n drop eppintvw eoutcome eoutcome_tm1 eppintvw_tm1 eppmis4_tm1 
			capture n drop  tlstwrky tprvjbyr tfrmryr emnreson alwayswork6m lf_ft  // LABHIST TM1 ewk1bfor awk1bfor  twk1lsjb awk1lsjb  yearsnotworked6m  ahowmany
			capture n drop  caregive_nlf recentbreak_care6mfr recentbreak_care6mto enwresn alstwrky // LABHIST TM1  twk1lsjb awk1lsjb tlstwrky alstwrky 
			capture n drop aprvjbyr afrmryr amakmnyr amnreson awrk35hr aoff6mtn anowrkfr anowrkto // LABHIST TM1  //
			capture n drop anwresn endprevjbyr_tm startprevjbyr_tm // LABHIST TM1 // monthly start and end dates previous jobs not possible: endprevjb_tm startprevjb_tm 
			*capture n drop  ebno1 ebno2 tsbdate1 tsbdate2 tebdate* aebdate* asbdate* ebuscntr ejobcntr /*
			*/ 
			
			compress
		
		
		
		save "2001_corewave_occmob_min.dta", replace
	
