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



//==========================================
// PRELIMINARIES
//===========================================


*do "${workingdir}/global_paths.do"


* initialization stuff
set more off
set varabbrev off
version 13

** Log it 
local logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture n log close step2_8_log
capture noisily log using "${tempdata}/step2_8log_`logdate'.txt", text name(step2_8_log)
capture noisily log using "${tempdata}/step2_8log_`logdate'.txt", append text name(step2_8_log)

		
		
		
***********************************************
*** SOME PROGRAMS
***********************************************

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
					






*************************
** LOAD DATA AND SET WEIGHTS
************************


****** USING THE FULL DATA SET 
use "${outputdata}/corewave_all_ctv.dta", clear


//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// CHECK WEIGHT ON wpfinwgt
version 13
table panel, c(mean wpfinwgt freq)
su wpfinwgt if panel==1984
 if r(mean)>=1000000 {
replace wpfinwgt=wpfinwgt/10000 if panel==1984
}


		* pweight

			sort panel personkey yearmonth
			
			capture drop sumweight
			by panel: egen sumweight=sum(wpfinwgt) if tage>=20 
			by panel: su sumweight
			
			
			// EACH PANEL CONTRIBUTES THE SAME
			capture drop pweight
			gen pweight=wpfinwgt/sumweight
			
			bysort panel: su pweight


		* pweight2
			// EACH PERSON CONTRIBUTES THE SAME ON AVERAGE, BUT RESPECT RELATIVE WEIGHTING 
			capture drop pweight2
			gen pweight2=.
			
			count if tage>=20 & panel==1984
			replace pweight2=pweight*r(N) if panel==1984
			count if tage>=20 & panel==1985
			replace pweight2=pweight*r(N) if panel==1985
			count if tage>=20 & panel==1986
			replace pweight2=pweight*r(N) if panel==1986

			count if tage>=20 & panel==1987
			replace pweight2=pweight*r(N) if panel==1987
			count if tage>=20 & panel==1988
			replace pweight2=pweight*r(N) if panel==1988
			count if tage>=20 & panel==1990
			replace pweight2=pweight*r(N) if panel==1990
			count if tage>=20 & panel==1991
			replace pweight2=pweight*r(N) if panel==1991
			count if tage>=20 & panel==1992
			replace pweight2=pweight*r(N) if panel==1992
			count if tage>=20 & panel==1993
			replace pweight2=pweight*r(N) if panel==1993
			count if tage>=20 & panel==1996
			replace pweight2=pweight*r(N) if panel==1996
			count if tage>=20 & panel==2001
			replace pweight2=pweight*r(N) if panel==2001
			count if tage>=20 & panel==2004
			replace pweight2=pweight*r(N) if panel==2004
			count if tage>=20 & panel==2008
			replace pweight2=pweight*r(N) if panel==2008
			
			
			count if personkey!="" & panel==1984
			count if personkey!="" & panel==1985
			count if personkey!="" & panel==1986
			count if personkey!="" & panel==1987
			
			count if panel==1984
			count if strat==. & panel==1984
			
			replace strat=core_gvarstrat if panel==1984 & strat==.

** IDENTIFIERS

* make hh identifier
capture drop hh_idx
destring personkey, gen(hh_idx)
replace hh_idx=hh_idx/1000 if panel<1996
replace hh_idx=hh_idx/10000 if panel>=1996
replace hh_idx=floor(hh_idx) if panel<1996
replace hh_idx=floor(hh_idx) if panel>=1996
cap n drop hh_ids93
gen hh_ids93=string(hh_idx,"%20.0g")

capture drop hh_id_2
destring personkey, gen(hh_id_2)
replace hh_id_2=hh_id_2/100000 if panel<1996
replace hh_id_2=hh_id_2/10000000 if panel>=1996
replace hh_id_2=floor(hh_id_2) if panel<1996
replace hh_id_2=floor(hh_id_2) if panel>=1996
cap n drop hh_ids93_2
gen hh_ids93_2=string(hh_id_2,"%20.0g")


capture drop newstrat
gen newstrat=panel*1000+strat
svyset hsc [pw=pweight2],  strata(newstrat) singleunit(scaled)


	
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// CHECK WEIGHT ON wpfinwgt
table panel, c(mean wpfinwgt freq)
table panel, c(mean pweight2 freq)


// delete observations of those who are exclusively out the labor force
capture drop full_n_indic
capture drop full_n_indic_t

gen full_n_indic_t=-1 if empl==1 | unempl==1
bysort personkey: egen full_n_indic=max(full_n_indic_t)

drop if full_n_indic==.
drop full_n_indic
drop full_n_indic_t





		//===================================
		// GLOBALS FOR ANALYSIS
		//================================

		global wavecond= " & wave > 4 "
		global sttg = "3"
		global epanbwidth=3

		global wavecond " & wave>4 & interview_no>14 & sample_timetogo>3"
		global mwavecond " & wave>1 & interview_no>4 & sample_timetogo>3"
			* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
		global jfwavecond "& wave>1 & interview_no>4 & sample_timetogo>18-n_spellength "	// this 
		global jfwavecond "& wave>1 & interview_no>4 & (sample_timetogo>20-n_spellength ) & entry_ind==1"  // combined with durations below 14!!! OTHERWISE 
		global sttg "1"
		global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
		global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "


		//==================================
		// minimizing data set
		//======================================

		compress


		capture program drop drop_indiv_exe
		program define drop_indiv_exe
							args varlistx


		forvalues 	i=1(1)100 {
		if "``i''"!=""{
		capture n drop ``i''				
		if _rc==111 {
		display "error variable ``i''"
		}
		}
		}
		end 

		drop_indiv_exe  estlemp1 astlemp1 asjdate1 aejdate1  arsend1 ajbhrs1 eocctim1 aocctim1 eclwrk1 aclwrk1 tpmsum1 apmsum1 epayhr1 apayhr1 tpyrate1 apyrate1 rpyper1 estlemp2 astlemp2 asjdate2 aejdate2  arsend2 ajbhrs2 eocctim2 aocctim2 eclwrk2 aclwrk2 tpmsum2 apmsum2 epayhr2 apayhr2 tpyrate2 apyrate2 rpyper2  aenrlm aeducate co_nestar_1d no_nestar_1d co_ne_1d no_ne_1d co_nestar_inflow_1d no_nestar_inflow_1d co_ne_inflow_1d no_ne_inflow_1d gen_u_spell_1d gen_n_spell_1d gen_nstar_spell_1d
		drop_indiv_exe ws1chg_4m ws2chg_4m pp_wave inotake irsnnotlkg c_rpyper1 c_tpyrate2 c_rpyper2 rsnnotlkg wantjob c_se_wks1 c_se_wks2 fp_tjbocc1_dd fp_tjbocc1_dd_1d fp_tjbocc2_dd fp_tjbocc2_dd_1d c_tjbocc1_dd c_tjbocc1_dd_1d c_tjbocc2_dd c_tjbocc2_dd_1d su_id pp_entry ienrold fp_eenrlm renrold ise1occ ise1ind ise12260 ise2occ ise2ind
		drop_indiv_exe  ws2wks c_tpmsum2 c_eclwrk2 ws2chg sday2 smonth2 eday2 emonth2 stopjob2 eunion2 ecntrc2
		drop_indiv_exe  ws1wks c_tpmsum1 c_eclwrk1 ws1chg sday1 smonth1 eday1 emonth1 stopjob1  c_tpyrate1 eunion1 ecntrc1

		drop if eafnow==1
		drop if tage<18
		drop if tage>66

		capture drop lf_indic
		gen lf_indic=1 if unempl==1 | empl==1

		capture drop lf_person_ind
		bysort personkey: egen lf_person_ind=max(lf_indic)

drop if lf_person_ind==.
drop lf_indic
drop lf_person_ind

tab quarter

//******************************************
// AUXILIARY VARIABLES
//******************************************

***  interview_no

sort personkey yearmonth
capture drop interview_no
gen int interview_no=1 if personkey!=personkey[_n-1] 
replace interview_no=interview_no[_n-1]+1 if personkey==personkey[_n-1] & interview_no[_n-1]!=. // & panel==panel[_n-1] & yearmonth>=yearmonth[_n-1]+1


sort panel personkey yearmonth
capture drop interview_no2		
gen int interview_no2=1 if personkey!=personkey[_n-1]
replace interview_no2=interview_no2[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
replace interview_no2=1 if personkey==personkey[_n-1] & yearmonth!=yearmonth[_n-1]+1
replace interview_no2=interview_no2[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
replace interview_no2=1 if personkey==personkey[_n-1] & yearmonth!=yearmonth[_n-1]+1

				
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
		bysort personkey: egen mincont=min(continuous_spell)
		sort personkey yearmonth
		
		tab quarter mincont, row nof
		tab quarter interview_no


		     
     gsort personkey -yearmonth
     capture drop sample_timetogo
     gen sample_timetogo=.
     replace sample_timetogo=1 if personkey!=personkey[_n-1]
     replace sample_timetogo=1 if personkey==personkey[_n-1] & yearmonth!=yearmonth[_n-1]-1
     replace sample_timetogo=sample_timetogo[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & sample_timetogo[_n-1]!=.
     sort personkey yearmonth 
		
		

		
		
		
		compress

//==============================================================
// Employment/unemployment transition indicators // COMPLETE SPELL INDICATORS
//================================================================

***  USE THE NEW DEFINITION 
***  USE THE SELECTION OF QUARTERS WHERE WE CAN CALCULATE OCCUPATIONAL MOBILITY OVER (TAKE INTO ACCOUNT CENSORING )
***  THEN USE THE SAME WAY OF CALCULATING IN BOTH DATA AND ESTIMATION


//===========================
//  STANDARD DEFINITION 
//=============================


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

			capture drop uestar
			capture drop eustar
			
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
     gen enstar=0 if empl==1
     replace enstar=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl[_n-1]==1 & (unempl==1|(outlf==1  $ersendc))
     
     display " EN flow construction "
      // second version, without ersend condition
     *set varabbrev off
      capture drop ne
      capture drop en
     *set varabbrev on
     
     gen ne=0 if outlf==1 | unempl==1
     gen en=0 if empl==1
     
     replace ne=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl==1 & (unempl[_n-1]==1 | outlf[_n-1]==1)
     replace en=1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl[_n-1]==1 & (unempl==1|(outlf==1))
     
	 tab quarter en if firstlastwave!=1, row nof
			
			
			
			



** define lagged switch indicators
sort personkey yearmonth
capture drop lue
gen byte lue=0 if unempl==1 
replace lue=1 if ue[_n+1]==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1


sort personkey yearmonth
capture drop lne
gen byte lne=0 if (unempl==1 | outlf==1)
replace lne=1 if ne[_n+1]==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1

sort personkey yearmonth
capture drop leu
gen byte leu=0 if empl==1 
replace leu=1 if eu[_n+1]==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1


sort personkey yearmonth
capture drop len
gen byte len=0 if empl==1
replace len=1 if en[_n+1]==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1





****** COMPLETE U/N SPELLENGTH


			
			
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
      
      
      
      
      






*** ADJUST SPELLENGTHS: do not count those less than one month as unemployed 

capture drop no_spellength
capture drop uo_spellength
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



//============================================================
// UNEMPL / SEPARATIONS INTO NEW U_SPELL/N_SPELL DEFINITION 
//==============================================================

tab u_spellength if lue==1
tab u_spellength uo_spellength if lue==1 & u_spellength<=6

// len_ctv, leu_ctv#
sort personkey yearmonth
capture drop len_ctv
capture drop leu_ctv
capture drop lne_ctv
capture drop lue_ctv

capture drop empl_ctv
capture drop unempl_ctv
capture drop outlf_ctv

gen empl_ctv=empl
gen unempl_ctv=unempl
gen outlf_ctv=outlf

replace empl_ctv=1 if u_spellength==0 & uo_spellength==1  & empl_ctv!=1
replace empl_ctv=1 if n_spellength==0 & no_spellength==1  & empl_ctv!=1

replace unempl_ctv=0 if u_spellength==0 & uo_spellength==1 & unempl_ctv==1
replace outlf_ctv=0 if n_spellength==0 & no_spellength==1 & outlf_ctv==1

gen leu_ctv=0 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl_ctv[_n+1]==1 & empl_ctv==1
gen len_ctv=0 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl_ctv[_n+1]==1 & empl_ctv==1

replace leu_ctv=1 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & unempl_ctv[_n+1]==1 & empl_ctv==1
replace len_ctv=1 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & (unempl_ctv[_n+1]==1 | outlf_ctv[_n+1]==1) & empl_ctv==1

gen lue_ctv=0 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & unempl_ctv[_n+1]==1 & unempl_ctv==1
gen lne_ctv=0 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & (unempl_ctv[_n+1]==1 | outlf_ctv[_n+1]==1) & ( unempl_ctv==1 | outlf_ctv==1) 

replace lue_ctv=1 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & unempl_ctv==1 & empl_ctv[_n+1]==1
replace lne_ctv=1 if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & (unempl_ctv==1 | outlf_ctv==1) & empl_ctv[_n+1]==1


cap drop lue_c_1tm 
cap drop lue_n_1tm

compress
*save, replace



//=====================================================================================================================================
//
// 							 OCCUPATIONAL CODES 
//
//======================================================================================================================================



global fullrecode_ind=0



			/* below we have the choice to go through the full recoding process, or just derive the minimum set of recoded occupations/industries. The latter (fullrecode_ind==0)
				additionally deletes all other occupational variables, to save space */


if $fullrecode_ind==1 {
do "${step0codedir}/occ_ind_recodes/occupational_recodes_v2.do" 
}

if $fullrecode_ind!=1 {
				sort personkey yearmonth 
				
				capture drop occup3basic1
				capture drop occup3basic2
					
				gen occup3basic1=.
				gen occup3basic2=.


		
// 1984 - 1985
sort personkey yearmonth
global panelcondition " (panel>=1984 & panel<=1985) "	
	
				replace occup3basic1=. if ${panelcondition} 
				replace occup3basic2=. if ${panelcondition} 
				
	
				replace occup3basic1=c_tjbocc1  if ${panelcondition} &  empl==1 & c_tjbocc1 !=. & ajbocc1!=1
				replace occup3basic2=c_tjbocc2  if ${panelcondition} &  empl==1 & c_tjbocc2 !=. & ajbocc2!=1
				replace occup3basic1=. if occup3basic1==0
				replace occup3basic2=. if occup3basic2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==. & eeno1!=.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==. & eeno2!=.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.


// 1986 -1993
sort personkey yearmonth
global panelcondition " (panel>=1986 & panel<=1993) "	


				replace occup3basic1=tjbocc1  if   (panel>=1986 & panel<=1993)  &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if   (panel>=1986 & panel<=1993)  &  empl==1 & tjbocc2 !=. & ajbocc2==0
				replace occup3basic1=. if occup3basic1==0 &   (panel>=1986 & panel<=1993)  
				replace occup3basic2=. if occup3basic2==0 &   (panel>=1986 & panel<=1993)  



				** set occupation to missing, if   (panel>=1986 & panel<=1993)  &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if   (panel>=1986 & panel<=1993)  &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace occup3basic2=.  if   (panel>=1986 & panel<=1993)  &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if   (panel>=1986 & panel<=1993)  &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

		
// 1996 -2001
sort personkey yearmonth
global panelcondition " (panel>=1996 & panel<=2001) "	

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0 
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace occup3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

				
				
// 2004 -2008
sort personkey yearmonth
global panelcondition " (panel>=2004 & panel<=2008) "	

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0 & tjbocc1!=984
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0 & tjbocc1!=984



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace occup3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

				
			
				replace occup3basic1=. if occup3basic1==0
				replace occup3basic2=. if occup3basic2==0

//*****************************8
//  INDUSTRIES
//******************************


/*
A quick note:

 before 1996: use core wave industries (because of job-id recoding 1990-1993, and because it has an imputation flag attached (as with occupations)
 relative to occupations, note that c_tjboccx recoded into tjboccx for 1986-1993, and iwsxocc into ajboccx. no such recoding has taken place for industries, os we use 
 the original variable names.

*/


		
				*********************************************************************************
				***** keep BOTH FIRM'S INFO WHEN employed in both firms tm2
				*********************************************************************************

				capture drop ind3basic1
				capture drop ind3basic2
					
				gen ind3basic1=.
				gen ind3basic2=.
			
				/*
				imputation: ajbind1/2 for panel>=1996
				imputation: iws1/2ind for panel<=1993
				
				which industry variable:
				c_ejbind1/2 pre-1996: USE 1984, 1985
				fp_ejbind1/2 pre-1996: 
				ejbind1/2 1996 and later
				
				*/

				* replace "if ${panelcondition} & " by "if ${panelcondition} &  $panelcondition &", where global panelcondition=PANEL in question 
		
// 1984 & 1985?
sort personkey yearmonth
global panelcondition " (panel>=1984 & panel<=1985)  "	
	
	
				replace ind3basic1=c_ejbind1  if ${panelcondition} &  empl==1 & c_ejbind1!=. & c_ejbind1>0  & iws1ind!=1
				replace ind3basic2=c_ejbind2  if ${panelcondition} &  empl==1 & c_ejbind2!=. & c_ejbind2>0  & iws2ind!=1



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace ind3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace ind3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & ind3basic1[_n-1]!=. & ind3basic1==. & eeno1!=.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & ind3basic2[_n-1]!=. & ind3basic2==. & eeno2!=.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic1[_n-1]!=. & ind3basic1==. & ind3basic2==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic2[_n-1]!=. & ind3basic2==. & ind3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic2[_n-1]!=. & ind3basic2==.

// 1986 (-1993?)
sort personkey yearmonth
global panelcondition " (panel>=1986 & panel<=1993) "	


				replace ind3basic1=c_ejbind1  if ${panelcondition} &  empl==1 & c_ejbind1!=. & c_ejbind1>0  & iws1ind==0
				replace ind3basic2=c_ejbind2  if ${panelcondition} &  empl==1 & c_ejbind2!=. & c_ejbind2>0  & iws2ind==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace ind3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace ind3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & ind3basic2[_n-1]!=. & ind3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic1[_n-1]!=. & ind3basic1==. & ind3basic2==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic2[_n-1]!=. & ind3basic2==. & ind3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic2[_n-1]!=. & ind3basic2==.


// 1996 (-2008?)
sort personkey yearmonth
global panelcondition " (panel>=1996 & panel<=2008) "	

				replace ind3basic1=ejbind1  if ${panelcondition} &  empl==1 & ejbind1!=. & ejbind1>0  & ajbind1==0
				replace ind3basic2=ejbind2  if ${panelcondition} &  empl==1 & ejbind2!=. & ejbind2>0  & ajbind2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace ind3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace ind3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & ind3basic2[_n-1]!=. & ind3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic1[_n-1]!=. & ind3basic1==. & ind3basic2==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic2[_n-1]!=. & ind3basic2==. & ind3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic2[_n-1]!=. & ind3basic2==.

				
				replace ind3basic1=. if ind3basic1==0
				replace ind3basic2=. if ind3basic2==0

				
				
				
				
				

//===================
// LOCC BFR/AFT LIND BFR/AFT
//=====================


	/* At the moment before returning to unemployment, we note occupation before becoming unemployed, and the occupation after becoming unemployed */

capture program drop indocc_beforeafter_exe
program define indocc_beforeafter_exe
	
	sort personkey yearmonth
	
	local var_in="`1'"
	local var_outbefore="`2'"
	local var_outafter="`3'"
	capture drop l`var_outbefore'1
	capture drop l`var_outbefore'2
	capture drop l`var_outafter'1
	capture drop l`var_outafter'2
	
	gen l`var_outbefore'1=`var_in'1 if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	gen l`var_outbefore'2=`var_in'2 if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	gen l`var_outafter'1=`var_in'1[_n+1] if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	gen l`var_outafter'2=`var_in'2[_n+1] if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1

	
	end 

	* DEFINE LOCC LIND
	indocc_beforeafter_exe ind3basic ind3bfr_b ind3aft_b
	indocc_beforeafter_exe occup3basic occ3bfr_b occ3aft_b
 	
// MARK DOMINANT FIRM, JUST IN CASE: BE CAREFUL!!!!
	capture drop firm_ind
	gen int firm_ind=1 if firmno==eeno1 & eeno1!=. & empl==1
	replace firm_ind=2 if firmno==eeno2 & eeno2!=. & empl==1
	replace firm_ind=firm_ind[_n-1]  if personkey==personkey[_n-1] & empl==0 & firm_ind==.
						
	capture drop lfirm_ind
	gen int lfirm_ind=1 if firmno[_n+1]==eeno1[_n+1] & eeno1[_n+1]!=.  & lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	replace lfirm_ind=2 if firmno[_n+1]==eeno2[_n+1] & eeno2[_n+1]!=.  & lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
						

*******  during nonemployment spell; compare occ current employment to occupation in the next employment, after a (n)u(n) spell. 
*******  Later LEOCC/LEIND CAN ALSO BE USED FOR COMPARISONS DURING EMPL

	capture program drop indocc_espell_exe
	program define indocc_espell_exe
		
		sort personkey yearmonth
		
		local var_in="`1'"
		local var_outbefore="`2'"
		local var_outafter="`3'"
		capture drop le`var_outbefore'1
		capture drop le`var_outbefore'2
		
		** OCCUPATION/INDUSTRY WHILE BEING EMPLOYED;
		gen le`var_outbefore'1=`var_in'1[_n-1] if (len[_n-1]==1) & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl[_n-1]==1 
		gen le`var_outbefore'2=`var_in'2[_n-1] if (len[_n-1]==1) & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & empl[_n-1]==1 
		
		** OCCUPATION/INDUSTRY AFTER FINDING A JOB
		replace le`var_outbefore'1=le`var_outbefore'1[_n-1] if empl==0 & empl[_n-1]==0 ///
												& personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & le`var_outbefore'1[_n-1]!=. & le`var_outbefore'1==.
		replace le`var_outbefore'2=le`var_outbefore'2[_n-1] if empl==0 & empl[_n-1]==0 ///
												& personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1 & le`var_outbefore'2[_n-1]!=. & le`var_outbefore'2==.
		
		
		capture drop le`var_outafter'1
		capture drop le`var_outafter'2
		
		gen le`var_outafter'1=`var_in'1[_n+1] if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
		gen le`var_outafter'2=`var_in'2[_n+1] if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
		
		forvalues i=1(1)48{
		replace le`var_outafter'1=le`var_outafter'1[_n+1] if (lne==0 ) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==0 &  le`var_outafter'1==.
		replace le`var_outafter'2=le`var_outafter'2[_n+1] if (lne==0 ) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==0 &  le`var_outafter'2==.
		replace lfirm_ind=lfirm_ind[_n+1] if  (lne==0 ) & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==0 &  lfirm_ind==.
		}
		
		end 

		indocc_espell_exe ind3basic ind3bfr_b ind3aft_b
		indocc_espell_exe occup3basic occ3bfr_b occ3aft_b

** DROP SOME VARIABLES

*cap n drop rmesr rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rwksperm 
cap n drop eeno1 ejbind1 ajbind1 tjbocc1 ajbocc1 eeno2 ejbind2 ajbind2 tjbocc2 ajbocc2 
cap n drop tjbocc1_dd tjbocc1_dd_1d tjbocc2_dd tjbocc2_dd_1d 
cap n drop tjbocc1_1d tjbocc2_1d 
cap n drop fp_eeno1 fp_eeno2 fp_eclwrk1 fp_eclwrk2 fp_tjbocc1 fp_tjbocc2 fp_ejbind1 fp_ejbind2 fp_ws1wk fp_ws2wk fp_tpyrate1 fp_tpyrate2 
cap n drop eeno1_old eeno2_old 
cap n drop iwksjob iwkswop iwkslok ireasab itakjob itakjobn iwksptr idisab iretird njobs ws12003 ws12004 iws1ind iws12012 iws12024 ws22103 ws22104 iws2ind iws22112 iws22124
cap n drop sumweight pweight


}
compress 




//============================================================================================
// INDUSTRY AGGREGATION  & NEW OCCUPATIONAL DEFINITIONS
//=========================+===================================================================






// 1980 Census Industrial Classification (until 91 panels)
// 1990 Census Industrial Classification (1992-2001 panels)
// 2000 Census Industrial Classification (2004-2008 panels)

capture program drop indagg_exe
program define indagg_exe

local var_out="`2'"
local var_in="`1'"

*local var_in="ind3basic1"

capture drop tempagg   // 13 
capture drop tempaggm 	// merged manufacturing, ...

gen int tempagg=.
gen int tempaggm=.

// 1980 Census Industrial Classification, aggregated -- 1990 Census Industries, aggregated

* 1= AGRICULTURE
replace tempagg=1 if `var_in'>=1 & `var_in'<=32 & panel<=2001 
replace tempaggm=1 if `var_in'>=1 & `var_in'<=32 & panel<=2001 


* 2= MINING
replace tempagg=2 if `var_in'>=40 & `var_in'<=50 & panel<=2001 
replace tempaggm=2 if `var_in'>=40 & `var_in'<=50 & panel<=2001 


* 3= CONSTRUCTION
replace tempagg=3 if `var_in'>=60 & `var_in'<=60 & panel<=2001 
replace tempaggm=3 if `var_in'>=60 & `var_in'<=60 & panel<=2001 

* 4= MANUFACTURING, AGGREGATE: MEASURE 2
replace tempaggm=4 if `var_in'>=100 & `var_in'<=392 & panel<=2001 


	* 4= MANUFACTURING OF NON-DURABLES, MEASURE 1
	replace tempagg=4 if `var_in'>=100 & `var_in'<=222 & panel<=2001 


	* 5= MANUFACTURING OF DURABLES, MEASURE 1
	replace tempagg=5 if `var_in'>=230 & `var_in'<=392 & panel<=2001 


* 6= TRANSPORTATION/COMMUNICATIONS/UTILITY
replace tempagg=6 if `var_in'>=400 & `var_in'<=472 & panel<=2001 
replace tempaggm=6 if `var_in'>=400 & `var_in'<=472 & panel<=2001 

* 7= WHOLESALE TRADE, AGGREGATED: MEASURE 2
replace tempaggm=7 if `var_in'>=500 & `var_in'<=571 & panel<=2001 

	* 7= WHOLESALE TRADE, DURABLE
	replace tempagg=7 if `var_in'>=500 & `var_in'<=532 & panel<=2001 

	* 8= WHOLESALE TRADE, NON-DURABLE
	replace tempagg=8 if `var_in'>=540 & `var_in'<=571 & panel<=2001 

* 9= RETAIL TRADE
replace tempagg=9 if `var_in'>=580 & `var_in'<=691 & panel<=2001 
replace tempaggm=9 if `var_in'>=580 & `var_in'<=691 & panel<=2001 

* 10= FIRE
replace tempagg=10 if `var_in'>=700 & `var_in'<=712 & panel<=2001 
replace tempaggm=10 if `var_in'>=700 & `var_in'<=712 & panel<=2001 

* 11 BUSINESS & REPAIR 
replace tempagg=11 if `var_in'>=721 & `var_in'<=760 & panel<=2001 
replace tempaggm=11 if `var_in'>=721 & `var_in'<=760 & panel<=2001 

	* special case: R&D, split out into commercial R&D, and scientific etc. R&D in 1980 classificaiton, becomes R&D in 1990 classificaiton
	replace tempagg=14 if `var_in'==730 & panel<=1991
	replace tempaggm=14 if `var_in'==730 & panel<=1991 

* 12 PERSONAL SERVICES 
replace tempagg=12 if `var_in'>=761 & `var_in'<=791 & panel<=2001 
replace tempaggm=12 if `var_in'>=761 & `var_in'<=791 & panel<=2001 


* 13 ENTERTAINMENT AND RECREATION
replace tempagg=13 if `var_in'>=800 & `var_in'<=810 & panel<=2001 
replace tempaggm=13 if `var_in'>=800 & `var_in'<=810 & panel<=2001 
	

* 14 PROFESSIONAL AND RELATED SERVICES
replace tempagg=14 if `var_in'>=812 & `var_in'<=893 & panel<=2001 
replace tempaggm=14 if `var_in'>=812 & `var_in'<=893 & panel<=2001 
	

* 15 PUBLIC ADMINISTRATION
replace tempagg=15 if `var_in'>=900 & `var_in'<=932 & panel<=2001 
replace tempaggm=15 if `var_in'>=900 & `var_in'<=932 & panel<=2001 
		
	
	
	
// 2000 Census Industrial Classification



* 1= AGRICULTURE
replace tempagg=1 if `var_in'>=170 & `var_in'<=290 & panel>=2004 & panel<=2008  
replace tempaggm=1 if `var_in'>=170 & `var_in'<=290 & panel>=2004 & panel<=2008  


* 2= MINING
replace tempagg=2 if `var_in'>=370 & `var_in'<=490 & panel>=2004 & panel<=2008  
replace tempaggm=2 if `var_in'>=370 & `var_in'<=490 & panel>=2004 & panel<=2008  


* 3= CONSTRUCTION
replace tempagg=3 if `var_in'>=770 & `var_in'<=770 & panel>=2004 & panel<=2008  
replace tempaggm=3 if `var_in'>=770 & `var_in'<=770 & panel>=2004 & panel<=2008  

* 4= MANUFACTURING, AGGREGATE: MEASURE 2
replace tempaggm=4 if `var_in'>=1070 & `var_in'<=3990 & panel>=2004 & panel<=2008  


	* 4= MANUFACTURING OF NON-DURABLES, MEASURE 1
	replace tempagg=4 if `var_in'>=1070 & `var_in'<=2390 & panel>=2004 & panel<=2008  


	* 5= MANUFACTURING OF DURABLES, MEASURE 1
	replace tempagg=5 if `var_in'>=2470 & `var_in'<=3990 & panel>=2004 & panel<=2008  

	// EXCEPTIONS:
	* retail bakeries
	replace tempagg=9 if `var_in'>=1190 & `var_in'<=1190 & panel>=2004 & panel<=2008  
	replace tempaggm=9 if `var_in'>=1190 & `var_in'<=1190 & panel>=2004 & panel<=2008  


* 6= TRANSPORTATION/COMMUNICATIONS/UTILITY

/* transportation: 1990: 400-432; 2000: 6070-6390 */
replace tempagg=6 if `var_in'>=6070 & `var_in'<=6390 & panel>=2004 & panel<=2008  
replace tempaggm=6 if `var_in'>=6070 & `var_in'<=6390 & panel>=2004 & panel<=2008  

/* communication ==> information: tricky category! */
replace tempagg=6 if `var_in'>=6670 & `var_in'<=6695 & panel>=2004 & panel<=2008  
replace tempaggm=6 if `var_in'>=6670 & `var_in'<=6695 & panel>=2004 & panel<=2008  
 
 * others in the information category
 	* newspaper and other publishers: 1990 manufacturing, nondur
	replace tempagg=4 if `var_in'>=6470 & `var_in'<=6480 & panel>=2004 & panel<=2008  
	replace tempaggm=4 if `var_in'>=6470 & `var_in'<=6480 & panel>=2004 & panel<=2008  
	* software publishing and sound recording and other information/data processing services to business services
	replace tempagg=11 if (`var_in'==6490 | `var_in'==6590 | `var_in'==6780 | `var_in'==6790) & panel>=2004 & panel<=2008  
	replace tempaggm=11 if (`var_in'==6490 | `var_in'==6590 | `var_in'==6780 | `var_in'==6790) & panel>=2004 & panel<=2008  
	* motion pictures and video to entertainment
	replace tempagg=13 if (`var_in'==6570 ) & panel>=2004 & panel<=2008  
	replace tempaggm=13 if (`var_in'==6570 ) & panel>=2004 & panel<=2008  
		*libraries to prof and rel services 
	replace tempagg=14 if (`var_in'==6770 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'==6770 ) & panel>=2004 & panel<=2008  


/* utility */
replace tempagg=6 if `var_in'>=0570 & `var_in'<=0690 & panel>=2004 & panel<=2008  
replace tempaggm=6 if `var_in'>=0570 & `var_in'<=0690 & panel>=2004 & panel<=2008  
 
	
* 7= WHOLESALE TRADE, AGGREGATED: MEASURE 2
replace tempaggm=7 if `var_in'>=4070 & `var_in'<=4590 & panel>=2004 & panel<=2008  

	* 7= WHOLESALE TRADE, DURABLE
	replace tempagg=7 if `var_in'>=4070 & `var_in'<=4290 & panel>=2004 & panel<=2008  

	* 8= WHOLESALE TRADE, NON-DURABLE
	replace tempagg=8 if `var_in'>=4370 & `var_in'<=4590 & panel>=2004 & panel<=2008  

* 9= RETAIL TRADE
replace tempagg=9 if `var_in'>=4670 & `var_in'<=5790 & panel>=2004 & panel<=2008  
replace tempaggm=9 if `var_in'>=4670 & `var_in'<=5790 & panel>=2004 & panel<=2008  

* 10= FIRE
replace tempagg=10 if `var_in'>=6870 & `var_in'<=7070 & panel>=2004 & panel<=2008  
replace tempaggm=10 if `var_in'>=6870 & `var_in'<=7070 & panel>=2004 & panel<=2008  

* 11 BUSINESS & REPAIR 
/* messy concordance, lots of back and forth with prof and related services */
replace tempagg=11 if `var_in'>=7080 & `var_in'<=7780 & panel>=2004 & panel<=2008  
replace tempaggm=11 if `var_in'>=7080 & `var_in'<=7780 & panel>=2004 & panel<=2008  

	* special case: video rental
	replace tempagg=13 if `var_in'==7170 & panel>=2004 & panel<=2008  
	replace tempaggm=13 if `var_in'==7170 & panel>=2004 & panel<=2008  

	* special case: veterinary and landscape to agriculture
	replace tempagg=1 if (`var_in'==7480 | `var_in'==7770 ) & panel>=2004 & panel<=2008  
	replace tempaggm=1 if (`var_in'==7480 | `var_in'==7770 ) & panel>=2004 & panel<=2008  

	* special case: travel arrangement to TRANSPORT
	replace tempagg=6 if `var_in'==7670 & panel>=2004 & panel<=2008  
	replace tempaggm=6 if `var_in'==7670 & panel>=2004 & panel<=2008  

	
	* special case: waste management to UTILITIES
	replace tempagg=6 if `var_in'==7790 & panel>=2004 & panel<=2008  
	replace tempaggm=6 if `var_in'==7790 & panel>=2004 & panel<=2008  

	* to prof services
	/* 727 728 729 739 746 749 */
	replace tempagg=14 if ((`var_in'>=7270 & `var_in'<=7290 ) | `var_in'==7390 | `var_in'== 7460 | `var_in'==7490 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if ((`var_in'>=7270 & `var_in'<=7290 ) | `var_in'==7390 | `var_in'== 7460 | `var_in'==7490 )& panel>=2004 & panel<=2008  

* 12 PERSONAL SERVICES 
replace tempagg=12 if `var_in'>=8660 & `var_in'<=9090 & panel>=2004 & panel<=2008  
replace tempaggm=12 if `var_in'>=8660 & `var_in'<=9090 & panel>=2004 & panel<=2008  

	* to retail
	replace tempagg=14 if (`var_in'== 8680 | `var_in'==8690 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'>=8680 & `var_in'<=8690 ) & panel>=2004 & panel<=2008  
	
	* to business services and repair
	replace tempagg=14 if (`var_in'>= 8770 & `var_in'<=8880 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'>=8770 & `var_in'<=8880 )  & panel>=2004 & panel<=2008  

* 13 ENTERTAINMENT AND RECREATION
replace tempagg=13 if (`var_in'>=8560 & `var_in'<=8590 ) & panel>=2004 & panel<=2008  
replace tempaggm=13 if (`var_in'>=8560 & `var_in'<=8590 ) & panel>=2004 & panel<=2008  
		
	*museums to prof services
	replace tempagg=14 if (`var_in'>=8570 & `var_in'<=8570 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'>=8570 & `var_in'<=8570 ) & panel>=2004 & panel<=2008  


* 14 PROFESSIONAL AND RELATED SERVICES
replace tempagg=14 if `var_in'>=7860 & `var_in'<=8470 & panel>=2004 & panel<=2008  
replace tempaggm=14 if `var_in'>=7680 & `var_in'<=8470 & panel>=2004 & panel<=2008  
replace tempagg=14 if `var_in'>=9160 & `var_in'<=9190 & panel>=2004 & panel<=2008  
replace tempaggm=14 if `var_in'>=9160 & `var_in'<=9190 & panel>=2004 & panel<=2008  

replace tempagg=12 if `var_in'>=9290 & `var_in'<=9290 & panel>=2004 & panel<=2008  
replace tempaggm=12 if `var_in'>=9290 & `var_in'<=9290 & panel>=2004 & panel<=2008  
	

* 15 PUBLIC ADMINISTRATION
replace tempagg=15 if `var_in'>=9370 & `var_in'<=9590 & panel>=2004 & panel<=2008  
replace tempaggm=15 if `var_in'>=9370 & `var_in'<=9590 & panel>=2004 & panel<=2008  
		


// LABELING THE VARIABLES

capture drop `var_out'
capture drop `var_out'm
ren tempagg `var_out'
ren tempaggm `var_out'm

end 

indagg_exe ind3basic1 ind1basic1
indagg_exe ind3basic2 ind1basic2

// 13-16 major industry groups
indagg_exe lind3bfr_b1 lind1bfr_b1
indagg_exe lind3bfr_b2 lind1bfr_b2
indagg_exe lind3aft_b1 lind1aft_b1
indagg_exe lind3aft_b2 lind1aft_b2

* renaming the aggregated variables so they end in 1 or 2, referring to eeno1, eeno2

capture drop lind1aft_bm1
capture drop lind1aft_bm2
capture drop lind1bfr_bm1
capture drop lind1bfr_bm2


ren lind1aft_b1m lind1aft_bm1
ren lind1aft_b2m lind1aft_bm2
ren lind1bfr_b1m lind1bfr_bm1
ren lind1bfr_b2m lind1bfr_bm2


// AGGREGATE LEIND MEASURES AS WELL 

capture drop leind1aft_bm1
capture drop leind1aft_bm2
capture drop leind1bfr_bm1
capture drop leind1bfr_bm2


indagg_exe leind3bfr_b1 leind1bfr_b1
indagg_exe leind3bfr_b2 leind1bfr_b2
indagg_exe leind3aft_b1 leind1aft_b1
indagg_exe leind3aft_b2 leind1aft_b2

* renaming the aggregated variables so they end in 1 or 2, referring to eeno1, eeno2



ren leind1aft_b1m leind1aft_bm1
ren leind1aft_b2m leind1aft_bm2
ren leind1bfr_b1m leind1bfr_bm1
ren leind1bfr_b2m leind1bfr_bm2


compress
*save, replace 	

//=================================================================
// isolate only the relevant variables
//===================================================================
keep personkey yearmonth wave panel unemp* emp* outlf* lue* lne* len* leu* locc3bfr_b* leocc3bfr_b* locc3aft_b* leocc3aft_b*  occup3basic* ///
ind3basic* lind3bfr_b* leind3bfr_b* lind3aft_b* leind3aft_b* firm_ind* lfirm_ind* lind* leind* locc* leocc* tage educ* ///
pweight2 hh_ids93 hh_ids93_2 newstrat interview_no interview_no2 continuous_spell mincont sample_timetogo ///
ue eu uestar eustar enstar ne en lue leu lne uo_spellength un_spellength complete_uspell complete_unuspell complete_unspell ///
no_spellength complete_nspell complete_nuspell complete_nunspell complete_nstarspell u_spellength n_spellength ntype_spell ///
ntype_spell2 empl_ctv unempl_ctv outlf_ctv leu_ctv len_ctv lue_ctv lne_ctv wave rot strat hsc sex asex race arace eorigin aorigin /// 
wpfinwgt tage aage ms ams ejobcntr rtakjob rnotake tfipsst c_tfipsst educ max_educ firstschool educ_tm2 yearslf ///
empl unempl outlf laborforce unempl_recall gov_ind1 gov_ind2 gov_ind maxgov max_educ firstyearlf nlfschool firstschool retired donotconsider_dum core_gvarstrat c_tfipsst c_rmesr 



//==================================================================================
// DIFFERENT AGGREGATIONS FOR OCCUPATIONS
//==================================================================================


/*
some changes: 
- groundskeepers to services (not agriculture)
- inspectors (SOC2000 874) covers prec prod inspectros and machine operators inspectors
- electr(on)ic equipment assembler (soc80 683; soc90 683; soc00: 772), mapped into 785 (soc90dd) assemblers; 
- soc00 770 First-line supervisors/managers of production and operating workers (51-1011) recoded to operators/assemblers: 
	
- other questionmarks: merging laundry operators with private household laundry (soc90:403), even though 432 is housekeeping cleaners is a separate category in soc 2000
- 613 supervisors mining, do not put as 628 supervisors precision production (soc00: 620). Keep it as 613
- soc800 673 (patternmakers); soc90: misc apparel; soc00 844 (not in dd concordance?)
- soc00 846: textile workers, all others? to 13 or 14?  (similar problem as 770, it spans 13 and 14)

*/


******** BEFORE OCC
*** 1980 SOC
capture drop occ
capture n drop occ1990dd
capture drop locc3bfr_dd1
capture drop locc3bfr_dd2

gen occ=locc3bfr_b1 if panel<=1991 	
capture drop _merge
*merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90adj.dta"
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd locc3bfr_dd1
capture drop _merge
capture n drop occ
gen occ=locc3bfr_b2 if panel<=1991 	
*merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90adj.dta"
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd locc3bfr_dd2
*** 1990 SOC
capture drop occ
gen occ=locc3bfr_b1 if panel>1991 & panel<=2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3bfr_dd1=occ1990dd if panel>1991 & panel<=2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3bfr_b2 if panel>1991 & panel<=2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3bfr_dd2=occ1990dd  if panel>1991 & panel<=2001 	
capture n drop occ
cap n drop occ1990dd
**** 2000 SOC 
gen occ=locc3bfr_b1 if panel>2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3bfr_dd1=occ1990dd if panel>2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3bfr_b2 if panel>2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3bfr_dd2=occ1990dd  if panel>2001 	
cap n drop occ
cap n drop occ1990dd

replace locc3bfr_dd1=702 if locc3bfr_b1==770 & panel>=2004 & panel<=2008 
replace locc3bfr_dd2=702 if locc3bfr_b2==770 & panel>=2004 & panel<=2008 
replace locc3bfr_dd1=487 if locc3bfr_b1==602 & panel>=2004 & panel<=2008 
replace locc3bfr_dd2=487 if locc3bfr_b2==602 & panel>=2004 & panel<=2008 

******** AFTEROCC
*** 1980 SOC
capture drop occ
capture drop locc3aft_dd1
capture drop locc3aft_dd2

gen occ=locc3aft_b1 if panel<=1991 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd locc3aft_dd1
capture drop _merge
capture n drop occ
gen occ=locc3aft_b2 if panel<=1991 	
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd locc3aft_dd2
*** 1990 SOC
capture drop occ
gen occ=locc3aft_b1 if panel>1991 & panel<=2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3aft_dd1=occ1990dd if panel>1991 & panel<=2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3aft_b2 if panel>1991 & panel<=2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3aft_dd2=occ1990dd  if panel>1991 & panel<=2001 	
capture n drop occ
cap n drop occ1990dd
**** 2000 SOC 
gen occ=locc3aft_b1 if panel>2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3aft_dd1=occ1990dd if panel>2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3aft_b2 if panel>2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3aft_dd2=occ1990dd  if panel>2001 	
cap n drop occ
cap n drop occ1990dd


replace locc3aft_dd1=702 if locc3aft_b1==770 & panel>=2004 & panel<=2008 
replace locc3aft_dd2=702 if locc3aft_b2==770 & panel>=2004 & panel<=2008 
replace locc3aft_dd1=487 if locc3aft_b1==602 & panel>=2004 & panel<=2008 
replace locc3aft_dd2=487 if locc3aft_b2==602 & panel>=2004 & panel<=2008 

sort personkey yearmonth

// AGGREGATION AND RECODING FOR  LEOCC _DD

******** BEFORE OCC
*** 1980 SOC
capture drop occ
capture n drop occ1990dd
capture drop leocc3bfr_dd1
capture drop leocc3bfr_dd2

gen occ=leocc3bfr_b1 if panel<=1991 	
capture drop _merge
*merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90adj.dta"
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd leocc3bfr_dd1
capture drop _merge
capture n drop occ
gen occ=leocc3bfr_b2 if panel<=1991 	
*merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90adj.dta"
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd leocc3bfr_dd2
*** 1990 SOC
capture drop occ
gen occ=leocc3bfr_b1 if panel>1991 & panel<=2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace leocc3bfr_dd1=occ1990dd if panel>1991 & panel<=2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=leocc3bfr_b2 if panel>1991 & panel<=2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace leocc3bfr_dd2=occ1990dd  if panel>1991 & panel<=2001 	
capture n drop occ
cap n drop occ1990dd
**** 2000 SOC 
gen occ=leocc3bfr_b1 if panel>2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace leocc3bfr_dd1=occ1990dd if panel>2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=leocc3bfr_b2 if panel>2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace leocc3bfr_dd2=occ1990dd  if panel>2001 	
cap n drop occ
cap n drop occ1990dd

replace leocc3bfr_dd1=702 if leocc3bfr_b1==770 & panel>=2004 & panel<=2008 
replace leocc3bfr_dd2=702 if leocc3bfr_b2==770 & panel>=2004 & panel<=2008 
replace leocc3bfr_dd1=487 if leocc3bfr_b1==602 & panel>=2004 & panel<=2008 
replace leocc3bfr_dd2=487 if leocc3bfr_b2==602 & panel>=2004 & panel<=2008 



capture drop occ
capture drop leocc3aft_dd1
capture drop leocc3aft_dd2

gen occ=leocc3aft_b1 if panel<=1991 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd leocc3aft_dd1
capture drop _merge
capture n drop occ
gen occ=leocc3aft_b2 if panel<=1991 	
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd leocc3aft_dd2
*** 1990 SOC
capture drop occ
gen occ=leocc3aft_b1 if panel>1991 & panel<=2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace leocc3aft_dd1=occ1990dd if panel>1991 & panel<=2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=leocc3aft_b2 if panel>1991 & panel<=2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace leocc3aft_dd2=occ1990dd  if panel>1991 & panel<=2001 	
capture n drop occ
cap n drop occ1990dd
**** 2000 SOC 
gen occ=leocc3aft_b1 if panel>2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace leocc3aft_dd1=occ1990dd if panel>2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=leocc3aft_b2 if panel>2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace leocc3aft_dd2=occ1990dd  if panel>2001 	
cap n drop occ
cap n drop occ1990dd


replace leocc3aft_dd1=702 if leocc3aft_b1==770 & panel>=2004 & panel<=2008 
replace leocc3aft_dd2=702 if leocc3aft_b2==770 & panel>=2004 & panel<=2008 
replace leocc3aft_dd1=487 if leocc3aft_b1==602 & panel>=2004 & panel<=2008 
replace leocc3aft_dd2=487 if leocc3aft_b2==602 & panel>=2004 & panel<=2008 

sort personkey yearmonth



//==================================================
// PROGRAM TO SELECT BEFORE AND AFTER OCCUPATION 
//=====================================================

sort personkey yearmonth
capture n program drop mobselection1_exe
program define mobselection1_exe   // [before variables1/2] [after variables1/2]


						local var_bfr= "`1'"
						local var_aft ="`2'"
	set varabbrev off
	capture drop `var_bfr'
	capture drop `var_aft'
	
	gen `var_bfr'=.
	gen `var_aft'=.

	replace `var_bfr'=`var_bfr'1 if  `var_bfr'2==. & `var_bfr'1!=.
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1==. & `var_bfr'2!=.
	** 2 firms before 
	** one overlap, take the overlapping occupation
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & (`var_bfr'2!=`var_aft'1 & `var_bfr'2!=`var_aft'2)
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & (`var_bfr'1!=`var_aft'1 & `var_bfr'1!=`var_aft'2)
	** double overlap, which one counts? GO WITH THE OVERLAP with the dominant occupation before the unemployment spell
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & firm_ind==1
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & firm_ind==2
	** 2 firms before, but no overlap
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)  & firm_ind==1
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)  & firm_ind==2
	
	replace `var_aft'=`var_aft'1 if  `var_aft'2==. & `var_aft'1!=.
	replace `var_aft'=`var_aft'2 if  `var_aft'1==. & `var_aft'2!=.
	** 2 firms after
	** one overlap, take the overlapping occupation
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & (`var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2)
	** double overlap, which one counts? GO WITH THE OVERLAP with the dominant occupation before the unemployment spell
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & firm_ind==1
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & firm_ind==2
	** 2 firms after, but no overlap with previous firms
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2) & lfirm_ind==1
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2) & lfirm_ind==2

	
	
end 


capture n program drop le_mobselection_exe
program define le_mobselection_exe   // [before variables1/2] [after variables1/2]

sort personkey yearmonth 

						local var_bfr= "`1'"
						local var_aft ="`2'"
	set varabbrev off
	capture drop `var_bfr'
	capture drop `var_aft'
	
	gen `var_bfr'=.
	gen `var_aft'=.

	
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'2==. & `var_bfr'1!=.
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1==. & `var_bfr'2!=.
	** 2 firms before 
	** double overlap, which one counts? NOW GO WITH THE OVERLAP with the dominant occupation AFTER the unemployment spell
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & firm_ind==1
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & firm_ind==2
	
	
	display "AFTERWARDS"
	replace `var_aft'=`var_aft'1 if  `var_aft'2==. & `var_aft'1!=.
	replace `var_aft'=`var_aft'2 if  `var_aft'1==. & `var_aft'2!=.
	** 2 firms after, but no overlap with previous firms
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & lfirm_ind==1
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & lfirm_ind==2
	
	
end 


//======================================================
** AGGREGATION -- STANDARD 
//======================================================

capture program drop aggregate1_exe
program define aggregate1_exe

display " ============RECODING AT A MAJOR OCCUPATIONAL GROUP LEVEL=========================="

local var_in="`1'"
local aggvar_out="`2'"

if "`3'"==""{
local added_condition=" panel>=1984 "
}
if "`3'"!=""{
local added_condition="`3'"
}
capture drop `aggvar_out'
gen `aggvar_out'=.

		replace `aggvar_out'=1 if `var_in'>=4 & `var_in'<=37   & `added_condition'
		replace `aggvar_out'=2 if `var_in'>=43 & `var_in'<=199   & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=203 & `var_in'<=235     & `added_condition'
		replace `aggvar_out'=4 if `var_in'>=243 & `var_in'<=285   & `added_condition'
		replace `aggvar_out'=5 if `var_in'>=303 & `var_in'<=389  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=403 & `var_in'<=407  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=413 & `var_in'<=427  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=433 & `var_in'<=471  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=408 & `var_in'<=408  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		
		replace `aggvar_out'=9 if `var_in'>=472 & `var_in'<=476  & `added_condition'
		replace `aggvar_out'=9 if `var_in'>=477 & `var_in'<=499  & `added_condition'
		replace `aggvar_out'=11 if `var_in'>=503 & `var_in'<=549  & `added_condition'
		replace `aggvar_out'=12 if `var_in'>=553 & `var_in'<=617 & `added_condition'
		replace `aggvar_out'=13 if `var_in'>=628 & `var_in'<=699  & `added_condition'
		replace `aggvar_out'=14 if `var_in'>=702 & `var_in'<=702  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=14 if `var_in'>=703 & `var_in'<=799  & `added_condition'
		replace `aggvar_out'=15 if `var_in'>=803 & `var_in'<=859  & `added_condition'
		replace `aggvar_out'=16 if `var_in'>=863 & `var_in'<=889  & `added_condition'



label define label_1dd 1 "managing occupations" 2 "professional speciality" 3 "technicians and rel support" ///
		4 "sales occ." 5 "admin support" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val `aggvar_out' label_1dd		

end
sort personkey yearmonth
aggregate1_exe locc3aft_dd1 locc1aft_dd1
aggregate1_exe locc3aft_dd2 locc1aft_dd2
aggregate1_exe locc3bfr_dd1 locc1bfr_dd1
aggregate1_exe locc3bfr_dd2 locc1bfr_dd2


aggregate1_exe leocc3aft_dd1 leocc1aft_dd1
aggregate1_exe leocc3aft_dd2 leocc1aft_dd2
aggregate1_exe leocc3bfr_dd1 leocc1bfr_dd1
aggregate1_exe leocc3bfr_dd2 leocc1bfr_dd2

//================================
* 4 groups from cortes et al (hs)
//================================
cap n drop locc1bfr_hs1
cap n drop locc1bfr_hs2
cap n drop locc1aft_hs1
cap n drop locc1aft_hs2

gen locc1bfr_hs1=.
gen locc1bfr_hs2=.
gen locc1aft_hs1=.
gen locc1aft_hs2=.

cap n drop leocc1bfr_hs1
cap n drop leocc1bfr_hs2
cap n drop leocc1aft_hs1
cap n drop leocc1aft_hs2

gen leocc1bfr_hs1=.
gen leocc1bfr_hs2=.
gen leocc1aft_hs1=.
gen leocc1aft_hs2=.


cap n program drop dd2hs_exe
program define dd2hs_exe

	
	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 | `ddvar'==3
	replace `var_out'=2 if `ddvar'==4 | `ddvar'==5 
	replace `var_out'=3 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8
	replace `var_out'=4 if `ddvar'==11 | `ddvar'==12 | `ddvar'==13 | `ddvar'==14 | `ddvar'==15 | `ddvar'==16
	* agriculture to missing
	
end 

dd2hs_exe locc1bfr_dd1 locc1bfr_hs1 
dd2hs_exe locc1bfr_dd2 locc1bfr_hs2
dd2hs_exe locc1aft_dd1 locc1aft_hs1 
dd2hs_exe locc1aft_dd2 locc1aft_hs2 

dd2hs_exe leocc1bfr_dd1 leocc1bfr_hs1 
dd2hs_exe leocc1bfr_dd2 leocc1bfr_hs2
dd2hs_exe leocc1aft_dd1 leocc1aft_hs1 
dd2hs_exe leocc1aft_dd2 leocc1aft_hs2 


** with transportation reassigned

cap n drop locc1bfr_ths1
cap n drop locc1bfr_ths2
cap n drop locc1aft_ths1
cap n drop locc1aft_ths2

gen locc1bfr_ths1=.
gen locc1bfr_ths2=.
gen locc1aft_ths1=.
gen locc1aft_ths2=.

cap n program drop dd2ths_exe
program define dd2ths_exe

	
	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 | `ddvar'==3
	replace `var_out'=2 if `ddvar'==4 | `ddvar'==5 
	replace `var_out'=3 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8 | `ddvar'==15 
	replace `var_out'=4 if `ddvar'==11 | `ddvar'==12 | `ddvar'==13 | `ddvar'==14 | `ddvar'==16
	* agriculture to missing
	
end 

dd2ths_exe locc1bfr_dd1 locc1bfr_ths1 
dd2ths_exe locc1bfr_dd2 locc1bfr_ths2
dd2ths_exe locc1aft_dd1 locc1aft_ths1 
dd2ths_exe locc1aft_dd2 locc1aft_ths2 


cap n drop leocc1bfr_ths1
cap n drop leocc1bfr_ths2
cap n drop leocc1aft_ths1
cap n drop leocc1aft_ths2

gen leocc1bfr_ths1=.
gen leocc1bfr_ths2=.
gen leocc1aft_ths1=.
gen leocc1aft_ths2=.


dd2ths_exe leocc1bfr_dd1 leocc1bfr_ths1 
dd2ths_exe leocc1bfr_dd2 leocc1bfr_ths2
dd2ths_exe leocc1aft_dd1 leocc1aft_ths1 
dd2ths_exe leocc1aft_dd2 leocc1aft_ths2 


* 22 own definitions, based on the Census 2000 (MM in latin) categorization

cap n drop locc1bfr_mm1
cap n drop locc1bfr_mm2
cap n drop locc1aft_mm1
cap n drop locc1aft_mm2

gen locc1bfr_mm1=.
gen locc1bfr_mm2=.
gen locc1aft_mm1=.
gen locc1aft_mm2=.

cap n program drop merge2000soc_exe
program define merge2000soc_exe

local ddvar_in="`1'"
local aggvar_out="`2'"

capture drop occ1990dd
gen occ1990dd=`ddvar_in'

capture drop _merge
merge m:1 occ1990dd using "${step0codedir}/occ_ind_recodes/occ1990dd2000soc.dta"

replace `aggvar_out'=occ2000rec
drop occ1990dd
drop occ2000rec
cap n drop _merge2000socrec
ren _merge _merge2000socrec

/*
using IPUMS recoding, not all occupations are recoded (some occupations are absent in IPUMS?
177		120		3.57	3.57
	235		139	4.14	7.71
	387		759	22.59	30.30
	408		189	5.63	35.92
	433		358	10.65	46.58
	450		78	2.32	48.90
	451		926	27.56	76.46
	466		147	4.38	80.83
	467		8	0.24	81.07
	470		36	1.07	82.14
	471		39	1.16	83.30
	472		106	3.15	86.46
	653		140	4.17	90.63
	684		84	2.50	93.13
	702		95	2.83	95.95
	789		58	1.73	97.68
	834		4	0.12	97.80
	873		74	2.20	100.00



*/
replace `aggvar_out'=21 if `ddvar_in'==177
replace `aggvar_out'=19 if `ddvar_in'==235
replace `aggvar_out'=25 if `ddvar_in'==387
replace `aggvar_out'=51 if `ddvar_in'==408 // laundry, but it is mostly laundry workers in cat 14 (748)

replace `aggvar_out'=35 if `ddvar_in'==433 // food prep (433)
replace `aggvar_out'=37 if `ddvar_in'==450  
replace `aggvar_out'=37 if `ddvar_in'==451
replace `aggvar_out'=39 if `ddvar_in'==466
replace `aggvar_out'=39 if `ddvar_in'==467
replace `aggvar_out'=39 if `ddvar_in'==470
replace `aggvar_out'=39 if `ddvar_in'==471 // transportation attendant (but also inspector in soc2000?)
replace `aggvar_out'=39 if `ddvar_in'==472

replace `aggvar_out'=47 if `ddvar_in'==653
replace `aggvar_out'=51 if `ddvar_in'==684
replace `aggvar_out'=51 if `ddvar_in'==702
replace `aggvar_out'=51 if `ddvar_in'==789
replace `aggvar_out'=53 if `ddvar_in'==834
replace `aggvar_out'=51 if `ddvar_in'==873

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

 lab val `aggvar_out' label_mm		

 
end 

merge2000soc_exe locc3bfr_dd1 locc1bfr_mm1
tab locc3bfr_dd1 if locc3bfr_dd1!=. & locc1bfr_mm1==.
merge2000soc_exe locc3bfr_dd2 locc1bfr_mm2
tab locc3bfr_dd2 if locc3bfr_dd2!=. & locc1bfr_mm2==.
merge2000soc_exe locc3aft_dd1 locc1aft_mm1
tab locc3aft_dd1 if locc3aft_dd1!=. & locc1aft_mm1==.
merge2000soc_exe locc3aft_dd2 locc1aft_mm2
tab locc3aft_dd2 if locc3aft_dd2!=. & locc1aft_mm2==.



mobselection1_exe locc1bfr_mm locc1aft_mm

// LEOCC MEASURE
cap n drop leocc1bfr_mm1
cap n drop leocc1bfr_mm2
cap n drop leocc1aft_mm1
cap n drop leocc1aft_mm2

gen leocc1bfr_mm1=.
gen leocc1bfr_mm2=.
gen leocc1aft_mm1=.
gen leocc1aft_mm2=.

merge2000soc_exe leocc3bfr_dd1 leocc1bfr_mm1
tab leocc3bfr_dd1 if leocc3bfr_dd1!=. & leocc1bfr_mm1==.
merge2000soc_exe leocc3bfr_dd2 leocc1bfr_mm2
tab leocc3bfr_dd2 if leocc3bfr_dd2!=. & leocc1bfr_mm2==.
merge2000soc_exe leocc3aft_dd1 leocc1aft_mm1
tab leocc3aft_dd1 if leocc3aft_dd1!=. & leocc1aft_mm1==.
merge2000soc_exe leocc3aft_dd2 leocc1aft_mm2
tab leocc3aft_dd2 if leocc3aft_dd2!=. & leocc1aft_mm2==.



le_mobselection_exe leocc1bfr_mm leocc1aft_mm




*matrixgen_exe locc1bfr_mm locc1aft_mm mmmat
*xsnet_calcx mmmat


*****************************************
** using the original SOC 2000 codes, in combination with 1990 dd codes for panels<=2001


cap n drop locc1bfr_mmo1
cap n drop locc1bfr_mmo2
cap n drop locc1aft_mmo1
cap n drop locc1aft_mmo2

gen locc1bfr_mmo1=locc1bfr_mm1
gen locc1bfr_mmo2=locc1bfr_mm2
gen locc1aft_mmo1=locc1aft_mm1
gen locc1aft_mmo2=locc1aft_mm2

cap n drop leocc1bfr_mmo1
cap n drop leocc1bfr_mmo2
cap n drop leocc1aft_mmo1
cap n drop leocc1aft_mmo2

gen leocc1bfr_mmo1=leocc1bfr_mm1
gen leocc1bfr_mmo2=leocc1bfr_mm2
gen leocc1aft_mmo1=leocc1aft_mm1
gen leocc1aft_mmo2=leocc1aft_mm2



cap n program drop mm_mmo_adj_exe
program define mm_mmo_adj_exe

local var_in="`1'"			// original 2000 soc coded variable (3digit)
local aggvar_out="`2'"

local added_condition=" panel>=2004 & panel<=2008 " 
		replace `aggvar_out'=11 if `var_in'>=1 & `var_in'<=43   & `added_condition'
		replace `aggvar_out'=13 if `var_in'>=50 & `var_in'<=95   & `added_condition'
		replace `aggvar_out'=15 if `var_in'>=100 & `var_in'<=124     & `added_condition'
		replace `aggvar_out'=17 if `var_in'>=130 & `var_in'<=156   & `added_condition'
		replace `aggvar_out'=19 if `var_in'>=160 & `var_in'<=196  & `added_condition'
		replace `aggvar_out'=21 if `var_in'>=200 & `var_in'<=206  & `added_condition'
		replace `aggvar_out'=23 if `var_in'>=210 & `var_in'<=215  & `added_condition'
		replace `aggvar_out'=25 if `var_in'>=220 & `var_in'<=255  & `added_condition'
		replace `aggvar_out'=27 if `var_in'>=260 & `var_in'<=296  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		replace `aggvar_out'=29 if `var_in'>=300 & `var_in'<=354  & `added_condition'
		replace `aggvar_out'=31 if `var_in'>=360 & `var_in'<=365  & `added_condition'
		replace `aggvar_out'=33 if `var_in'>=370 & `var_in'<=395  & `added_condition'
		replace `aggvar_out'=35 if `var_in'>=400 & `var_in'<=416 & `added_condition'
		replace `aggvar_out'=37 if `var_in'>=420 & `var_in'<=425  & `added_condition'
		replace `aggvar_out'=39 if `var_in'>=430 & `var_in'<=465  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=41 if `var_in'>=470 & `var_in'<=496  & `added_condition'
		replace `aggvar_out'=43 if `var_in'>=500 & `var_in'<=593  & `added_condition'
		replace `aggvar_out'=45 if `var_in'>=600 & `var_in'<=613  & `added_condition'
		replace `aggvar_out'=47 if `var_in'>=620 & `var_in'<=694  & `added_condition'
		replace `aggvar_out'=49 if `var_in'>=700 & `var_in'<=762  & `added_condition'
		replace `aggvar_out'=51 if `var_in'>=770 & `var_in'<=896 & `added_condition'
		replace `aggvar_out'=53 if `var_in'>=900 & `var_in'<=975  & `added_condition'

		
label define label_mmo  ///
 11 "Mgt occs" ///
 13 "Bus&Fin Operations"  ///
 15 "Computer and math. occ"  ///
 17 "architect & eng. occ"  ///
 19 "Life, phys, and socsci occ"  ///
 21 "Comm & soc service occ"  ///
 23 "Legal"  ///
 25 "Educ, training, and library"  ///
 27 "Arts/Dsgn/Ent/sports/media"  ///
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

 lab val `aggvar_out' label_mmo
		
end 


mm_mmo_adj_exe locc3bfr_b1 locc1bfr_mmo1
mm_mmo_adj_exe locc3bfr_b2 locc1bfr_mmo2
mm_mmo_adj_exe locc3aft_b1 locc1aft_mmo1
mm_mmo_adj_exe locc3aft_b2 locc1aft_mmo2

// LEOCC MEASURE 

mm_mmo_adj_exe leocc3bfr_b1 leocc1bfr_mmo1
mm_mmo_adj_exe leocc3bfr_b2 leocc1bfr_mmo2
mm_mmo_adj_exe leocc3aft_b1 leocc1aft_mmo1
mm_mmo_adj_exe leocc3aft_b2 leocc1aft_mmo2

compress
*save, replace

//====================================================
// FURTHER MOBSELECTION: CREATE LOCCBEFORE LOCCAFTER LINDBEFORE LINDAFTER
//=====================================================



// assign
mobselection1_exe lind1bfr_b lind1aft_b
mobselection1_exe locc1bfr_dd locc1aft_dd

*mobselection1_exe locc1bfr_ddmog locc1aft_ddmog
*mobselection1_exe locc1bfr_xi locc1aft_xi
*mobselection1_exe locc1bfr_aj locc1aft_aj

*mobselection1_exe locc1bfr_ad locc1aft_ad
*mobselection1_exe locc1bfr_sog locc1aft_sog
mobselection1_exe locc1bfr_hs locc1aft_hs
mobselection1_exe locc1bfr_ths locc1aft_ths
*mobselection1_exe locc1bfr_3cat locc1aft_3cat


*mobselection1_exe locc1bfr_nohlp locc1aft_nohlp
mobselection1_exe locc1bfr_mmo locc1aft_mmo

// LEOCC / LEIND MEASURE
le_mobselection_exe leocc1bfr_mmo leocc1aft_mmo
le_mobselection_exe leocc1bfr_hs leocc1aft_hs
le_mobselection_exe leocc1bfr_ths leocc1aft_ths
le_mobselection_exe leocc1bfr_dd leocc1aft_dd
le_mobselection_exe leind1bfr_b leind1aft_b



//=======================================
// AGGREGATE FURTHER THE PRE-SELECTED OCCUPATIONAL CODES
//========================================

*****************88
** 3 categories
*********************8

capture drop locc1bfr_3cat
capture drop locc1aft_3cat

gen locc1bfr_3cat=1 if locc1bfr_dd>=1 & locc1bfr_dd<=5
gen locc1aft_3cat=1 if locc1aft_dd>=1 & locc1aft_dd<=5
replace locc1bfr_3cat=2 if ((locc1bfr_dd>=6 & locc1bfr_dd<=8) | locc1bfr_dd==15)
replace locc1aft_3cat=2 if ((locc1aft_dd>=6 & locc1aft_dd<=8) | locc1aft_dd==15)
replace locc1bfr_3cat=3 if ((locc1bfr_dd>=9 & locc1bfr_dd<=16) & locc1bfr_dd!=15)
replace locc1aft_3cat=3 if ((locc1aft_dd>=9 & locc1aft_dd<=16) & locc1aft_dd!=15)

capture drop leocc1bfr_3cat
capture drop leocc1aft_3cat

gen leocc1bfr_3cat=1 if leocc1bfr_dd>=1 & leocc1bfr_dd<=5
gen leocc1aft_3cat=1 if leocc1aft_dd>=1 & leocc1aft_dd<=5
replace leocc1bfr_3cat=2 if ((leocc1bfr_dd>=6 & leocc1bfr_dd<=8) | leocc1bfr_dd==15)
replace leocc1aft_3cat=2 if ((leocc1aft_dd>=6 & leocc1aft_dd<=8) | leocc1aft_dd==15)
replace leocc1bfr_3cat=3 if ((leocc1bfr_dd>=9 & leocc1bfr_dd<=16) & leocc1bfr_dd!=15)
replace leocc1aft_3cat=3 if ((leocc1aft_dd>=9 & leocc1aft_dd<=16) & leocc1aft_dd!=15)

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


** label dd
label define label_1dd 1 "managing occupations" 2 "professional speciality" 3 "technicians and rel support" ///
		4 "sales occ." 5 "admin support" 6 "priv hh serv" 7 "protective serv" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

** label 4 super-categories
label  define label_4sc 1 "NRC" 2 "RC" 3 "NRM" 4 "RM", replace
label define label_3cat 1 "Cognitive" 2 "NR Manual" 3 "R Manual", replace
label define label_ind15 1 "Agriculture" 2 "Mining" 3 "Construction" 4 "Nondur Manfct" 5 "Dur Manfct" 6 "TCU"  /// transport, comm, utility
								 7 "whsale dur." 8 "whsale nondur." 9 "retail" 10 "FIRE" 11 "Bus&Rep Serv" 12 "Pers. Serv." 13 "E'tmnt&Rec" 14 "Prof Serv" 15 "public admin", replace
								 
lab val locc1bfr_dd label_1dd	
lab val locc1bfr_ths label_4sc
lab val locc1bfr_hs label_4sc
lab val locc1bfr_3cat label_3cat
lab val lind1bfr_b label_ind15	
lab val locc1aft_dd label_1dd	
lab val locc1aft_ths label_4sc
lab val locc1aft_hs label_4sc
lab val locc1aft_3cat label_3cat
lab val lind1aft_b label_ind15	


lab val leocc1bfr_dd label_1dd	
lab val leocc1bfr_ths label_4sc
lab val leocc1bfr_hs label_4sc
lab val leocc1bfr_3cat label_3cat
lab val lind1bfr_b label_ind15	
lab val leocc1aft_dd label_1dd	
lab val leocc1aft_ths label_4sc
lab val leocc1aft_hs label_4sc
lab val leocc1aft_3cat label_3cat
lab val leind1aft_b label_ind15	

*capture drop lue_c_rtmm
*	capture drop lne_c_rtmm
	capture drop locc1bfr_rtmm
	capture drop locc1aft_rtmm
	gen locc1bfr_rtmm=.
	replace locc1bfr_rtmm=1 if locc1bfr_mmo>=11 & locc1bfr_mmo<=29
	replace locc1bfr_rtmm=2 if (locc1bfr_mmo==41 | locc1bfr_mmo==43 )
	replace locc1bfr_rtmm=3 if ((locc1bfr_mmo>=31 & locc1bfr_mmo<=39 ) | locc1bfr_mmo==53)
	replace locc1bfr_rtmm=4 if locc1bfr_mmo>=47 & locc1bfr_mmo<=51
	gen locc1aft_rtmm=.
	replace locc1aft_rtmm=1 if locc1aft_mmo>=11 & locc1aft_mmo<=29
	replace locc1aft_rtmm=2 if (locc1aft_mmo==41 | locc1aft_mmo==43 )
	replace locc1aft_rtmm=3 if ((locc1aft_mmo>=31 & locc1aft_mmo<=39) | locc1aft_mmo==53)
	replace locc1aft_rtmm=4 if locc1aft_mmo>=47 & locc1aft_mmo<=51
	
								 
lab val locc1bfr_rtmm label_4sc	
lab val locc1aft_rtmm label_4sc


	capture drop leocc1bfr_rtmm
	capture drop leocc1aft_rtmm
	gen leocc1bfr_rtmm=.
	replace leocc1bfr_rtmm=1 if leocc1bfr_mmo>=11 & leocc1bfr_mmo<=29
	replace leocc1bfr_rtmm=2 if leocc1bfr_mmo==41 | leocc1bfr_mmo==43 
	replace leocc1bfr_rtmm=3 if ((leocc1bfr_mmo>=31 & leocc1bfr_mmo<=39) | leocc1bfr_mmo==53)
	replace leocc1bfr_rtmm=4 if leocc1bfr_mmo>=47 & leocc1bfr_mmo<=51
	gen leocc1aft_rtmm=.
	replace leocc1aft_rtmm=1 if leocc1aft_mmo>=11 & leocc1aft_mmo<=29
	replace leocc1aft_rtmm=2 if leocc1aft_mmo==41 | leocc1aft_mmo==43 
	replace leocc1aft_rtmm=3 if ((leocc1aft_mmo>=31 & leocc1aft_mmo<=39 ) | leocc1aft_mmo==53)
	replace leocc1aft_rtmm=4 if leocc1aft_mmo>=47 & leocc1aft_mmo<=51

				 
lab val leocc1bfr_rtmm label_4sc	
lab val leocc1aft_rtmm label_4sc


compress
*save, replace

/*
// check whether the before - after tie-breaking difference in mobselection and le_mobselection creates much difference

tab locc1bfr_mmo leocc1bfr_mmo if locc1bfr_mmo!=. , m
tab locc1bfr_mmo leocc1bfr_mmo if locc1bfr_mmo!=. , m col nof
tab locc1bfr_mmo leocc1bfr_mmo if locc1bfr_mmo!=. & leocc1bfr_mmo!=., row nof

tab locc1aft_mmo leocc1aft_mmo if locc1aft_mmo!=. , m
tab locc1aft_mmo leocc1aft_mmo if locc1aft_mmo!=. , m col nof
tab locc1aft_mmo leocc1aft_mmo if locc1aft_mmo!=. & leocc1aft_mmo!=., row nof

tab locc1aft_mmo leocc1aft_mmo if locc1aft_mmo!=. & n_spellength!=., m 

count if leocc1bfr_mmo==locc1bfr_mmo & locc1bfr_mmo!=.
count if leocc1bfr_mmo!=locc1bfr_mmo & locc1bfr_mmo!=. & leocc1bfr_mmo!=.


count if leocc1aft_mmo==locc1aft_mmo & locc1aft_mmo!=.
count if leocc1aft_mmo!=locc1aft_mmo & locc1aft_mmo!=. & leocc1aft_mmo!=.

// TIE-BREAKING creates differences for about 0.5% of cases BFR, and 0.1% AFT
*/

//=================================================
// OCC, OCCBEFORE AND OCCAFTER, IND, INDBEFORE AND INDAFTER  
//==========================================================


capture program drop leocc_fillout_exe
program define leocc_fillout_exe
			syntax namelist

tokenize `namelist' 

local indocc "`1'"

sort personkey yearmonth
forvalues i=2(1)10 {
if "``i''"!="" {

replace le`indocc'1bfr_``i''=le`indocc'1bfr_``i''[_n-1] if le`indocc'1bfr_``i''==. & le`indocc'1bfr_``i''[_n-1]!=. & empl==1 & lne[_n-1]==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
replace le`indocc'1bfr_``i''=le`indocc'1bfr_``i''[_n-1] if le`indocc'1bfr_``i''==. & le`indocc'1bfr_``i''[_n-1]!=. & empl==1 & empl[_n-1]==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1


}
}

gsort personkey -yearmonth


forvalues i=2(1)10 {
if "``i''"!=""{

replace le`indocc'1aft_``i''=le`indocc'1aft_``i''[_n-1] if le`indocc'1aft_``i''==. & le`indocc'1aft_``i''[_n-1]!=. & empl==1 & len==1 & empl[_n-1]!=. & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1
replace le`indocc'1aft_``i''=le`indocc'1aft_``i''[_n-1] if le`indocc'1aft_``i''==. & le`indocc'1aft_``i''[_n-1]!=. & empl==1 & empl[_n-1]==1 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1


}
}
sort personkey yearmonth 
end 

leocc_fillout_exe occ mmo dd rtmm
leocc_fillout_exe ind b


** DEFINE MAIN OCC DURING EMPLOYMENT 


** rtmm, ind, dd, mmo only? 


** INDUSTRY
capture drop ind3_b
capture drop ind_b
gen ind3_b=ind3basic1 if firm_ind==1
replace ind3_b=ind3basic2 if firm_ind==2
indagg_exe ind3_b ind_b

capture drop ind3_b

** OCCUPATION


capture drop occ
capture drop occ3

gen occ3=occup3basic1 if firm_ind==1
replace occ3=occup3basic2 if firm_ind==2


******** BEFORE OCC
*** 1980 SOC

gen occ=occ3 if panel<=1991 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90adj.dta"
drop if _merge==2
ren occ1990dd occ3_dd
capture drop _merge
*** 1990 SOC
capture drop occ
cap n drop occ1990dd
gen occ=occ3 if panel>1991 & panel<=2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace occ3_dd=occ1990dd if panel>1991 & panel<=2001 	
capture n drop occ
cap n drop occ1990dd
**** 2000 SOC 
gen occ=occ3 if panel>2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace occ3_dd=occ1990dd if panel>2001 	

replace occ3_dd=702 if occ3_dd==770 & panel>=2004 & panel<=2008 
replace occ3_dd=487 if occ3_dd==602 & panel>=2004 & panel<=2008 

capture drop occ
capture drop occ1990dd

** AGGREGATION DD MM MMO RTMM 
aggregate1_exe occ3_dd occ_dd

capture drop occ_mm
gen occ_mm=.
merge2000soc_exe occ3_dd occ_mm

capture drop occ_mmo
gen occ_mmo=occ_mm
mm_mmo_adj_exe occ3 occ_mmo

replace occ_mmo=occ_mm if occ_mmo==. & panel<2004

	capture drop occ_rtmm
	gen occ_rtmm=.
	replace occ_rtmm=1 if occ_mmo>=11 & occ_mmo<=29
	replace occ_rtmm=2 if (occ_mmo==41 | occ_mmo==43 )
	replace occ_rtmm=3 if ((occ_mmo>=31 & occ_mmo<=39) | occ_mmo==53)
	replace occ_rtmm=4 if occ_mmo>=47 & occ_mmo<=51
	
*save, replace 	

//==========================================================
// CLEANUP
//==========================================================

drop_indiv_exe 	 tjbocc1_1d tjbocc2_1d fp_rmesr fp_eeno1 fp_eeno2 fp_eclwrk1 fp_eclwrk2 fp_tjbocc1 fp_tjbocc2 fp_ejbind1 fp_ejbind2 fp_ws1wk fp_ws2wk fp_tpmsum1 fp_tpmsum2 fp_ejbhrs1 fp_ejbhrs2 fp_tpyrate1 ///
					fp_tpyrate2 eeno1_old eeno2_old co_1d no_1d co_inflow_1d no_inflow_1d ne_c_1tm ne_n_1tm occbefore_1tm occafter_1tm occup1tm1 occup1tm2
drop_indiv_exe 	 c_ejbind2 c_tpmsum2 c_eclwrk2 c_ejbhrs2 educ_tm2 c_strat c_tjbocc1 c_tjbocc2 c_enu
drop_indiv_exe   locc1bfr_mmo1 locc1bfr_mmo2 locc1aft_mmo1 locc1aft_mmo2 leocc1bfr_mmo1 leocc1bfr_mmo2 leocc1aft_mmo1 leocc1aft_mmo2
drop_indiv_exe   leocc1bfr_mm1 leocc1bfr_mm2 leocc1aft_mm1 leocc1aft_mm2 _merge2000socrec
drop_indiv_exe   leind3bfr_b1 leind3bfr_b2 leind3aft_b1 leind3aft_b2 leocc3bfr_b1 leocc3bfr_b2 leocc3aft_b1 leocc3aft_b2 lind1bfr_b1 lind1bfr_bm1 ///
					lind1bfr_b2 lind1bfr_bm2 lind1aft_b1 lind1aft_bm1 lind1aft_b2 lind1aft_bm2 leind1bfr_b1 leind1bfr_bm1 leind1bfr_b2  ///
					leind1bfr_bm2 leind1aft_b1 leind1aft_bm1 leind1aft_b2 leind1aft_bm2 locc3bfr_dd1 locc3bfr_dd2 locc3aft_dd1 locc3aft_dd2 /// 
					leocc3bfr_dd1 leocc3bfr_dd2 leocc3aft_dd1 leocc3aft_dd2 locc1aft_dd1 locc1aft_dd2 locc1bfr_dd1 locc1bfr_dd2  ///
					leocc1aft_dd1 leocc1aft_dd2 leocc1bfr_dd1 leocc1bfr_dd2 locc1bfr_hs1 locc1bfr_hs2 locc1aft_hs1 locc1aft_hs2   ///
					leocc1bfr_hs1 leocc1bfr_hs2 leocc1aft_hs1 leocc1aft_hs2 locc1bfr_ths1 locc1bfr_ths2 locc1aft_ths1  ///
					locc1aft_ths2 leocc1bfr_ths1 leocc1bfr_ths2 leocc1aft_ths1 leocc1aft_ths2 locc1bfr_mm1 locc1bfr_mm2 locc1aft_mm1 locc1aft_mm2 tag
drop_indiv_exe   ind3basic1 ind3basic2 lind3bfr_b1 lind3bfr_b2 lind3aft_b1 lind3aft_b2 locc3bfr_b1 locc3bfr_b2 locc3aft_b1 locc3aft_b2
drop_indiv_exe 	 leind1aft_b1 leind1aft_b2 leind1aft_bm1 leind1aft_bm2 leind1bfr_b1 leind1bfr_b2 leind1bfr_bm1 leind1bfr_bm2 leind3aft_b1 leind3aft_b2 leind3bfr_b1 leind3bfr_b2
drop_indiv_exe   leocc1aft_dd1 leocc1aft_dd2 leocc1aft_hs1 leocc1aft_hs2 leocc1aft_mm1 leocc1aft_mm2 leocc1aft_mmo1 leocc1aft_mmo2 leocc1aft_ths1 leocc1aft_ths2 
drop_indiv_exe 	 leocc1bfr_dd1 leocc1bfr_dd2 leocc1bfr_hs1 leocc1bfr_hs2 leocc1bfr_mm1 leocc1bfr_mm2 leocc1bfr_mmo1 leocc1bfr_mmo2 leocc1bfr_ths1 leocc1bfr_ths2 
drop_indiv_exe 	 leocc3aft_b1 leocc3aft_b2 leocc3aft_dd1 leocc3aft_dd2 leocc3bfr_b1 leocc3bfr_b2 leocc3bfr_dd1 leocc3bfr_dd2
drop_indiv_exe 	 occ3 occ3_dd
compress



//============================================================
// OCCMOB INDICATORS UPON JOB FINDING: LUE_C... 
//============================================================



			
// FIVE MEASURES
	* 13 occupations
	* 22 occupations
	* 4 occupations hs and ths
		* 3 occupations
	* occ & ind together
	
	* STANDARD
	capture drop lue_c_dd
	gen byte lue_c_dd=0 if lue==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd==locc1aft_dd			
	replace lue_c_dd=1 if lue==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd

	
	capture drop lne_c_dd
	gen byte lne_c_dd=0 if lne==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd==locc1aft_dd			
	replace lne_c_dd=1 if lne==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd
	
	* 22 occuaptions
	capture drop lue_c_mmo
	gen byte lue_c_mmo=0 if lue==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo==locc1aft_mmo			
	replace lue_c_mmo=1 if lue==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo!=locc1aft_mmo

	capture drop lne_c_mmo
	gen byte lne_c_mmo=0 if lne==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo==locc1aft_mmo			
	replace lne_c_mmo=1 if lne==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo!=locc1aft_mmo

	
		* robustness
			capture drop lue_ce_mmo
			gen byte lue_ce_mmo=0 if lue==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & leocc1bfr_mmo!=. & leocc1aft_mmo!=.  & leocc1bfr_mmo==leocc1aft_mmo			
			replace lue_ce_mmo=1 if lue==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & leocc1bfr_mmo!=. & leocc1aft_mmo!=.  & leocc1bfr_mmo!=leocc1aft_mmo			

			capture drop lne_ce_mmo
			gen byte lne_ce_mmo=0 if lne==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & leocc1bfr_mmo!=. & leocc1aft_mmo!=.  & leocc1bfr_mmo==leocc1aft_mmo				
			replace lne_ce_mmo=1 if lne==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & leocc1bfr_mmo!=. & leocc1aft_mmo!=.  & leocc1bfr_mmo!=leocc1aft_mmo			

	tab lue_c_mmo lue_ce_mmo, m
	tab lue_c_mmo lue_ce_mmo if lue_c_mmo!=. , m nof cell
	
	* 4 category (transportation to routine manual); excludes agriculture
	capture drop lue_c_hs
	gen byte lue_c_hs=0 if lue==1 & locc1bfr_hs!=. & locc1aft_hs!=. & locc1bfr_hs==locc1aft_hs			
	replace lue_c_hs=1 if lue==1 & locc1bfr_hs!=. & locc1aft_hs!=. & locc1bfr_hs!=locc1aft_hs

	capture drop lne_c_hs
	gen byte lne_c_hs=0 if lne==1 & locc1bfr_hs!=. & locc1aft_hs!=. & locc1bfr_hs==locc1aft_hs			
	replace lne_c_hs=1 if lne==1 & locc1bfr_hs!=. & locc1aft_hs!=. & locc1bfr_hs!=locc1aft_hs

	* 4 category (transportation to nr manual); excludes agriculture
	capture drop lue_c_ths
	gen byte lue_c_ths=0 if lue==1 & locc1bfr_ths!=. & locc1aft_ths!=. & locc1bfr_ths==locc1aft_ths			
	replace lue_c_ths=1 if lue==1 & locc1bfr_ths!=. & locc1aft_ths!=. & locc1bfr_ths!=locc1aft_ths

	capture drop lne_c_ths
	gen byte lne_c_ths=0 if lne==1 & locc1bfr_ths!=. & locc1aft_ths!=. & locc1bfr_ths==locc1aft_ths			
	replace lne_c_ths=1 if lne==1 & locc1bfr_ths!=. & locc1aft_ths!=. & locc1bfr_ths!=locc1aft_ths

	* 3 category (transportation to nrm); excludes agriculture
	capture drop lue_c_3cat
	gen byte lue_c_3cat=0 if lue==1 & locc1bfr_3cat!=. & locc1aft_3cat!=. & locc1bfr_3cat==locc1aft_3cat			
	replace lue_c_3cat=1 if lue==1 & locc1bfr_3cat!=. & locc1aft_3cat!=. & locc1bfr_3cat!=locc1aft_3cat

	capture drop lne_c_3cat
	gen byte lne_c_3cat=0 if lne==1 & locc1bfr_3cat!=. & locc1aft_3cat!=. & locc1bfr_3cat==locc1aft_3cat			
	replace lne_c_3cat=1 if lne==1 & locc1bfr_3cat!=. & locc1aft_3cat!=. & locc1bfr_3cat!=locc1aft_3cat

	
	* INDUSTRY
	capture drop lue_i_b
	gen byte lue_i_b=0 if lue==1 & lind1bfr_b!=. & lind1aft_b!=. & lind1bfr_b==lind1aft_b			
	replace lue_i_b=1 if lue==1 & lind1bfr_b!=. & lind1aft_b!=. & lind1bfr_b!=lind1aft_b

	capture drop lne_i_b
	gen byte lne_i_b=0 if lne==1 & lind1bfr_b!=. & lind1aft_b!=. & lind1bfr_b==lind1aft_b			
	replace lne_i_b=1 if lne==1 & lind1bfr_b!=. & lind1aft_b!=. & lind1bfr_b!=lind1aft_b

	
	* INDUSTRY/OCCUPATION CROSSPRODUCT
	capture drop lue_ic
	gen byte lue_ic=0 if lue==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_mmo!=. & locc1aft_mmo!=. & ~(locc1bfr_mmo!=locc1aft_mmo & lind1bfr_b!=lind1aft_b) 						
	replace lue_ic=1 if lue==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_mmo!=. & locc1aft_mmo!=. & (locc1bfr_mmo!=locc1aft_mmo & lind1bfr_b!=lind1aft_b) 

	capture drop lne_ic
	gen byte lne_ic=0 if lne==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_mmo!=. & locc1aft_mmo!=. & ~(locc1bfr_mmo!=locc1aft_mmo & lind1bfr_b!=lind1aft_b) 						
	replace lne_ic=1 if lne==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_mmo!=. & locc1aft_mmo!=. & (locc1bfr_mmo!=locc1aft_mmo & lind1bfr_b!=lind1aft_b) 

	* INDUSTRY/OCCUPATION CROSSPRODUCT DD
	capture drop lue_iodd
	gen byte lue_iodd=0 if lue==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_dd!=. & locc1aft_dd!=. & ~(locc1bfr_dd!=locc1aft_dd & lind1bfr_b!=lind1aft_b) 						
	replace lue_iodd=1 if lue==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_dd!=. & locc1aft_dd!=. & (locc1bfr_dd!=locc1aft_dd & lind1bfr_b!=lind1aft_b) 

	capture drop lne_iodd
	gen byte lne_iodd=0 if lne==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_dd!=. & locc1aft_dd!=. & ~(locc1bfr_dd!=locc1aft_dd & lind1bfr_b!=lind1aft_b) 						
	replace lne_iodd=1 if lne==1 & lind1bfr_b!=. & lind1aft_b!=. & locc1bfr_dd!=. & locc1aft_dd!=. & (locc1bfr_dd!=locc1aft_dd & lind1bfr_b!=lind1aft_b) 

	** RTMM 
	capture drop lue_c_rtmm
	gen byte lue_c_rtmm=0 if lue_c_mmo==0
	replace lue_c_rtmm=0 if locc1bfr_rtmm==locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lue_c_mmo!=.
	replace lue_c_rtmm=1 if locc1bfr_rtmm!=locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lue_c_mmo!=.
	capture drop lne_c_rtmm
	gen byte lne_c_rtmm=0 if lne_c_mmo==0
	replace lne_c_rtmm=0 if locc1bfr_rtmm==locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lne_c_mmo!=.
	replace lne_c_rtmm=1 if locc1bfr_rtmm!=locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lne_c_mmo!=.
	
	tab lue_c_rtmm lue_c_mmo, m

	
	
capture drop earlier_empl
sort personkey yearmonth
gen earlier_empl=0
replace earlier_empl=1 if empl_ctv==1
replace earlier_empl=1 if earlier_empl[_n-1]==1 & personkey==personkey[_n-1]


	***************
	**  MORE CONSERVATIVE MEASURE
	***************

	/*
	* STANDARD
	capture drop lue_c2_dd
	gen byte lue_c2_dd=0 if lue==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd==locc1aft_dd			
	replace lue_c2_dd=1 if lue==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd
	** reset if one of the before/after occupations overlaps with the occupation on the other side
	replace lue_c2_dd=0 if lue==1 & locc1bfr_dd1!=. & locc1bfr_dd2!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1bfr_dd1==locc1aft_dd & lue_c2_dd!=0 
	replace lue_c2_dd=0 if lue==1 & locc1bfr_dd1!=. & locc1bfr_dd2!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1bfr_dd2==locc1aft_dd & lue_c2_dd!=0 
	replace lue_c2_dd=0 if lue==1 & locc1aft_dd1!=. & locc1aft_dd2!=. & locc1bfr_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1aft_dd1==locc1bfr_dd & lue_c2_dd!=0 
	replace lue_c2_dd=0 if lue==1 & locc1aft_dd1!=. & locc1aft_dd2!=. & locc1bfr_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1aft_dd2==locc1bfr_dd & lue_c2_dd!=0 
	** reset if a pair can be formed even though the main occupations do not overlap
	replace lue_c2_dd=0 if lue==1 & locc1aft_dd1!=. & locc1aft_dd2!=. & locc1bfr_dd1!=. & locc1bfr_dd2!=. & locc1bfr_dd!=locc1aft_dd & ((locc1aft_dd2==locc1bfr_dd2) ///
	| (locc1aft_dd2==locc1bfr_dd1) | (locc1aft_dd1==locc1bfr_dd2) | (locc1aft_dd1==locc1bfr_dd1)) & lue_c2_dd!=0 
	
	capture drop lne_c2_dd
	gen byte lne_c2_dd=0 if lne==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd==locc1aft_dd			
	replace lne_c2_dd=1 if lne==1 & locc1bfr_dd!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd
	** reset if one of the before/after occupations overlaps with the occupation on the other side
	replace lne_c2_dd=0 if lne==1 & locc1bfr_dd1!=. & locc1bfr_dd2!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1bfr_dd1==locc1aft_dd & lne_c2_dd!=0 
	replace lne_c2_dd=0 if lne==1 & locc1bfr_dd1!=. & locc1bfr_dd2!=. & locc1aft_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1bfr_dd2==locc1aft_dd & lne_c2_dd!=0 
	replace lne_c2_dd=0 if lne==1 & locc1aft_dd1!=. & locc1aft_dd2!=. & locc1bfr_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1aft_dd1==locc1bfr_dd & lne_c2_dd!=0 
	replace lne_c2_dd=0 if lne==1 & locc1aft_dd1!=. & locc1aft_dd2!=. & locc1bfr_dd!=. & locc1bfr_dd!=locc1aft_dd & locc1aft_dd2==locc1bfr_dd & lne_c2_dd!=0 
	** reset if a pair can be formed even though the main occupations do not overlap
	replace lne_c2_dd=0 if lne==1 & locc1aft_dd1!=. & locc1aft_dd2!=. & locc1bfr_dd1!=. & locc1bfr_dd2!=. & locc1bfr_dd!=locc1aft_dd & ((locc1aft_dd2==locc1bfr_dd2) ///
	| (locc1aft_dd2==locc1bfr_dd1) | (locc1aft_dd1==locc1bfr_dd2) | (locc1aft_dd1==locc1bfr_dd1)) & lne_c2_dd!=0 
	*/
	
	* 22 occuaptions
	capture drop lue_c_mmo
	gen byte lue_c_mmo=0 if lue==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo==locc1aft_mmo			
	replace lue_c_mmo=1 if lue==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo!=locc1aft_mmo

	capture drop lne_c_mmo
	gen byte lne_c_mmo=0 if lne==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo==locc1aft_mmo			
	replace lne_c_mmo=1 if lne==1 & locc1bfr_mmo!=. & locc1aft_mmo!=. & locc1bfr_mmo!=locc1aft_mmo

	
//============================================================
// OCCMOB INDICATORS UPON JOB SEPARATION: LEU_C... LEN_C ... LEU_CTV ... LEN_CTV
//============================================================

	
			
// FIVE MEASURES
	* 13 occupations
	* 22 occupations
	* 4 occupations hs and ths
	* 3 occupations
	* occ & ind together
	
		* STANDARD
		sort personkey yearmonth 
		
		global len_addcond ""
	
		capture program drop len_gen_exe
		program define len_gen_exe
				args name sep_ind occindbfr occindaft
		
		capture drop `name'
		gen byte `name'=.
		replace `name'=0 if `sep_ind'==1 & `occindbfr'[_n+1]!=. & `occindaft'[_n+1]!=. & `occindbfr'[_n+1]==`occindaft'[_n+1] & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 $len_addcond		
		replace `name'=1 if `sep_ind'==1 & `occindbfr'[_n+1]!=. & `occindaft'[_n+1]!=. & `occindbfr'[_n+1]!=`occindaft'[_n+1] & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 	 $len_addcond		

		end 
		
		len_gen_exe leu_c_dd leu leocc1bfr_dd leocc1aft_dd
		len_gen_exe leu_c_mmo leu leocc1bfr_mmo leocc1aft_mmo
		len_gen_exe leu_c_hs leu leocc1bfr_hs leocc1aft_hs
		len_gen_exe leu_c_3cat leu leocc1bfr_3cat leocc1aft_3cat
		len_gen_exe leu_c_ind leu lind1bfr_b lind1aft_b
		len_gen_exe leu_c_rtmm leu leocc1bfr_rtmm leocc1aft_rtmm
		
		capture drop leu_c_ic
		gen leu_c_ic=0 if (leu_c_mmo==0 | leu_c_ind==0) & leu_c_mmo!=. & leu_c_ind!=.
		replace leu_c_ic=1 if (leu_c_mmo==1 & leu_c_ind==1) & leu_c_mmo!=. & leu_c_ind!=.
		
		capture drop leu_c_iodd
		gen leu_c_iodd=0 if (leu_c_dd==0 | leu_c_ind==0) & leu_c_dd!=. & leu_c_ind!=.
		replace leu_c_iodd=1 if (leu_c_dd==1 & leu_c_ind==1) & leu_c_dd!=. & leu_c_ind!=.
		
		len_gen_exe len_c_dd len locc1bfr_dd locc1aft_dd
		len_gen_exe len_c_mmo len locc1bfr_mmo locc1aft_mmo
		len_gen_exe len_c_hs len locc1bfr_hs locc1aft_hs
		len_gen_exe len_c_3cat len locc1bfr_3cat locc1aft_3cat
		len_gen_exe len_c_ind len lind1bfr_b lind1aft_b
		len_gen_exe len_c_rtmm len locc1bfr_rtmm locc1aft_rtmm
		
		capture drop len_c_ic
		gen len_c_ic=0 if (len_c_mmo==0 | len_c_ind==0) & len_c_mmo!=. & len_c_ind!=.
		replace len_c_ic=1 if (len_c_mmo==1 & len_c_ind==1) & len_c_mmo!=. & len_c_ind!=.
		
		capture drop len_c_iodd
		gen len_c_iodd=0 if (len_c_dd==0 | len_c_ind==0) & len_c_dd!=. & len_c_ind!=.
		replace len_c_iodd=1 if (len_c_dd==1 & len_c_ind==1) & len_c_dd!=. & len_c_ind!=.

	
	
	    capture program drop len_ctv_gen_exe
		program define len_ctv_gen_exe
				args name sep_ind occindbfr occindaft
		
		capture drop `name'
		gen byte `name'=.
		replace `name'=0 if `sep_ind'==1 & `occindbfr'[_n+1]!=. & `occindaft'[_n+1]!=. & `occindbfr'[_n+1]==`occindaft'[_n+1] & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 $len_addcond		
		replace `name'=1 if `sep_ind'==1 & `occindbfr'[_n+1]!=. & `occindaft'[_n+1]!=. & `occindbfr'[_n+1]!=`occindaft'[_n+1] & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 	 $len_addcond		

		end 
		
		len_gen_exe leu_ctv_dd leu_ctv locc1bfr_dd locc1aft_dd
		len_gen_exe leu_ctv_mmo leu_ctv locc1bfr_mmo locc1aft_mmo
		len_gen_exe leu_ctv_hs leu_ctv locc1bfr_hs locc1aft_hs
		len_gen_exe leu_ctv_3cat leu_ctv locc1bfr_3cat locc1aft_3cat
		len_gen_exe leu_ctv_ind leu_ctv lind1bfr_b lind1aft_b
		len_gen_exe leu_ctv_rtmm leu_ctv locc1bfr_rtmm locc1aft_rtmm
		
		capture drop leu_ctv_ic
		gen leu_ctv_ic=0 if (leu_ctv_mmo==0 | leu_ctv_ind==0) & leu_ctv_mmo!=. & leu_ctv_ind!=.
		replace leu_ctv_ic=1 if (leu_ctv_mmo==1 & leu_ctv_ind==1) & leu_ctv_mmo!=. & leu_ctv_ind!=.
		
		capture drop leu_ctv_iodd
		gen leu_ctv_iodd=0 if (leu_ctv_dd==0 | leu_ctv_ind==0) & leu_ctv_dd!=. & leu_ctv_ind!=.
		replace leu_ctv_iodd=1 if (leu_ctv_dd==1 & leu_ctv_ind==1) & leu_ctv_dd!=. & leu_ctv_ind!=.
		
		len_gen_exe len_ctv_dd len_ctv locc1bfr_dd locc1aft_dd
		len_gen_exe len_ctv_mmo len_ctv locc1bfr_mmo locc1aft_mmo
		len_gen_exe len_ctv_hs len_ctv locc1bfr_hs locc1aft_hs
		len_gen_exe len_ctv_3cat len_ctv locc1bfr_3cat locc1aft_3cat
		len_gen_exe len_ctv_ind len_ctv lind1bfr_b lind1aft_b
		len_gen_exe len_ctv_rtmm len_ctv locc1bfr_rtmm locc1aft_rtmm
		
		capture drop len_ctv_ic
		gen len_ctv_ic=0 if (len_ctv_mmo==0 | len_ctv_ind==0) & len_ctv_mmo!=. & len_ctv_ind!=.
		replace len_ctv_ic=1 if (len_ctv_mmo==1 & len_ctv_ind==1) & len_ctv_mmo!=. & len_ctv_ind!=.
		
		capture drop len_ctv_iodd
		gen len_ctv_iodd=0 if (len_ctv_dd==0 | len_ctv_ind==0) & len_ctv_dd!=. & len_ctv_ind!=.
		replace len_ctv_iodd=1 if (len_ctv_dd==1 & len_ctv_ind==1) & len_ctv_dd!=. & len_ctv_ind!=.

	
	
	
	
	
	*** delete occupation and industry variables not directly used
capture drop quarter
capture drop quarter_sep

gen quarter=qofd(dofm(yearmonth))
format quarter %tq
gen quarter_sep=qofd(dofm(yearmonth-n_spellength))
format quarter_sep %tq 

compress 
drop if personkey=="" 

save "${outputdata}/corewave_occlfmin_ctv.dta", replace 

	