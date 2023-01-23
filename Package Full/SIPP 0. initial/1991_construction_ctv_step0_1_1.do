
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
** STEP 0.1.1 CTV DATA CONSTRUCTION, 1991 PANEL  
********************************************************************************

* helpful input from Carl Singleton gratefully acknowledged  


  ************** NOTES
  /*
  - full panel industry and occupation are edited and some entries are IMPUTED, without imputation flags! use corewave
  - full panel firm numbers are prefixed by fp_ (fp_eeno1, fp_eeno2)
  - care has to be taken that the full panel information associated with fp eeno label X does not get mixed with a different core-wave eeno with the same X
	- therefore all full panel variables have their prefix fp_ restored. Some core-wave variables still keep their prefix c_ for extra emphasis, for now.
  - it can be that this is much less an issue for self-employment, but it is still good to compare fp SE vars with corresponding core SE var
  - imputation flags for occ/ind but not all imputation flags are loaded into this data. Good to keep an eye on it.	
  - make sure tm2 also is appended to wave 1 data
  - (deactivate the ws12003 to estlemp conversion, just copy the variable, and the recoding / deactivated the njobs conversion, instead just create jobcntr)
  - also load change-of-activity variable WS12004, WS22104
  - WS12003/ESTLEMP1 in the extraction do-file, gets the wrong variable label it seems, so no recode of answers is necessary
  */


clear 
*capture log close 
*set mem 3000m 
set more off 
set maxvar 10000


** GLOBALS
global yeardata "91"
global gnulin=0
* control parts of the do-file
global core_if=1	// read inal data from additioncore waves
global tm_if=1	       // read in the topical modules
global weights_if=1   // loads in the longitudinal weights
global subset_if=0   // starts reducing datasets into subsets with relevant variables for different projects
global locfileversion=1016  // JULY 2016
global list=0
global fp_ind=1		// full panel data extracted




** RUN MAIN CODE


if $fp_ind==1{
************************ FULL PANEL

		* Longitudinal data: create three files 1) by id; 2) by id wave; 3) by id wave month
		
        ****
        * id
        ****
	
		cd "$rawdata"
		use rot pp_id pp_entry pp_pnum pnlwgt fnlwgt91 fnlwgt92 /*
		*/ varstrat halfsamp sc1360 disab sex race ethnicty using sip${yeardata}fp, clear
		cd "$tempdata"
        capture drop id
		ren halfsamp halfsam

        *generate id
        egen id=concat(pp_id pp_entry pp_pnum)

        *generate across-panel id
        destring pp_pnum, generate(pp_pnum_n)
        destring pp_entry, generate(pp_entry_n)
        destring pp_id, generate(pp_id_n)
        capture drop personkey*
        gen double personkeytemp=.
        format personkeytemp %22.0f
        replace personkeytemp=pp_entry_n+100*pp_id_n+100000000000*${yeardata}
        gen personkeytemp2=string(personkeytemp, "%20.0g")
        egen personkey=concat(personkeytemp2 pp_pnum)
        capture drop pp_pnum_n pp_entry_n pp_id_n
        capture drop personkeytemp personkeytemp2
        gen panel=19${yeardata}
	
	*rename
        capture ren disab disab_panel
        ren varstrat strat
        ren halfsam hsc
        
        
		sort id
		save testi, replace
		
       ****
		* id wave
        ***
        
           
		cd "$rawdata"
		use pp_id pp_entry pp_pnum pp_intv* higrade* grd_cmp* in_af_* att_sch* usualhr* geo_ste* /*
		*** Additional variables
		*/ ed_fina* sc1672* sc1674* sc1676* sc1678* sc1680* sc1682* sc1684* sc1686* sc1688* sc1690* sc1692* /*
		*/ using sip${yeardata}fp, clear
		cd "$tempdata"
		renpfix pp_intv pp_int
		renpfix pp_int pp_intv

        *generate id
        egen id=concat(pp_id pp_entry pp_pnum)
		
        sort id
	reshape long pp_intv in_af_ grd_cmp higrade usualhr ed_fina sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 att_sch geo_ste, i(id) j(wave)
        renpfix in_af_ eafnow
        rename geo_ste fp_tfipsst
	
	ren usualhr ehrsall
		ren ed_fina eedfund
		** Read across not great - do not match well nor mutually exclusively across years apart from those below
		*ren sc1672 easst
		*ren sc1674 easst
		*ren sc1676 easst
		ren sc1678 easst01
		*ren sc1680 easst
		*ren sc1682 easst
		*ren sc1684 easst
		*ren sc1686 easst
		ren sc1688 easst10
		*ren sc1690 easst
		ren sc1692 easst11
	
        sort id wave

        drop if wave>8


		save testw, replace	
		
       ****
		* id month
        ****
        
		cd "$rawdata"
		use rot pp_id pp_entry pp_pnum ms* pp_mis* age* hh_add* ms_* clswk1* clswk2* jobid1* jobid2* /*
        */ wshrs1* wshrs2* ind1_*   ind2_*  esr_* wksper* occ1_* occ2_* ernam1* ernam2* hrrat1* hrrat2* /*
        */ se_wb1* se_wb2* se_hr1* se_hr2* wksem1* wksem2* mthjbw* mthwop* mthwks* enrl_m* /*
        */  busid1* busid2* se_am1* se_am2* se_oc1* se_oc2* seind1* seind2* se_hr1* se_hr2* /*
        */ typbs1* typbs2* /*
		*** Additional variables in main panel
		*/ pp_ear* pp_inc* using sip${yeardata}fp, clear
		cd "$tempdata"

        *generate id
        egen id=concat(pp_id pp_entry pp_pnum)
        
        *the getting-rid-of-zeros renaming
		capture renpfix age_ tage
		*capture renpfix age0 tage
		*capture renpfix hh_add0 hh_add
        *capture renpfix pp_mis0 pp_mis
        *capture renpfix ws1_cl0 ws1_cl
        *capture renpfix ws2_cl0 ws2_cl

		sort id
		reshape long hh_add ms_ pp_mis tage clswk1  clswk2  jobid1  jobid2  /* 
         */ wshrs1  wshrs2  ind1_    ind2_   esr_  wksper  occ1_  occ2_  ernam1  ernam2  hrrat1  hrrat2 /*
         */ se_wb1  se_wb2  se_hr1  se_hr2  wksem1  wksem2  mthjbw  mthwop  mthwks  enrl_m  /*
         */  busid1  busid2  se_am1  se_am2 seind1 seind2 typbs1 typbs2 se_oc1 se_oc2 pp_ear pp_inc, i(id) j(mnth) string
	
        *the great renaming
        rename clswk1 fp_eclwrk1
        rename clswk2 fp_eclwrk2
        rename jobid1 fp_eeno1
        rename jobid2 fp_eeno2
        rename wshrs1 fp_ejbhrs1
        rename wshrs2 fp_ejbhrs2
        rename ind1_ fp_ejbind1
        rename ind2_ fp_ejbind2
        rename esr_ fp_rmesr
        rename wksper rwksperm
        rename occ1_ fp_tjbocc1
        rename occ2_ fp_tjbocc2
        rename ernam1 fp_tpmsum1
        rename ernam2 fp_tpmsum2
        rename hrrat1 fp_tpyrate1
        rename hrrat2 fp_tpyrate2
        rename se_hr1 fp_se1hrs
        rename se_hr2 fp_se2hrs
        rename se_wb1 fp_se1_wk
        rename se_wb2 fp_se2_wk
        rename enrl_m fp_eenrlm
        rename wksem1 fp_ws1wk
        rename wksem2 fp_ws2wk
        rename mthjbw fp_wksjb_
        rename mthwks fp_weeksl
*         rename clswk1 eclwrk1
*        rename clswk2 eclwrk2
*        rename jobid1 fp_eeno1
*        rename jobid2 fp_eeno2
*        rename wshrs1 ejbhrs1
*        rename wshrs2 ejbhrs2
*        rename ind1_ ejbind1
*        rename ind2_ ejbind2
*        rename esr_  rmesr
*        rename wksper rwksperm
*        rename occ1_ tjbocc1
*        rename occ2_ tjbocc2
*        rename ernam1 tpmsum1
*        rename ernam2 tpmsum2
*        rename hrrat1 tpyrate1
*        rename hrrat2 tpyrate2
*        rename enrl_m eenrlm
*        rename wksem1 ws1wk
*        rename wksem2 ws2wk
*        rename mthjbw wksjb_
*        rename mthwks weeksl
*
*        rename se_hr1 ehrsbs1
*        rename se_hr2 ehrsbs2
*        rename se_wb1 se_wks1
*        rename se_wb2 se_wks2

        rename busid1 ebno1
        rename busid2 ebno2
        rename se_am1 tbmsum1
        rename se_am2 tbmsum2
        rename se_oc1 tbsocc1
        rename se_oc2 tbsocc2
        rename seind1 tbsind1
        rename seind2 tbsind2

        gen eincpb1=.
        gen eincpb2=.
        gen epropb1=.
        gen epropb2=.

        replace eincpb1=1 if typbs1==3
        replace eincpb1=2 if typbs1>0 & typbs1<3
        replace eincpb1=-1 if typbs1==0

        replace eincpb2=1 if typbs2==3
        replace eincpb2=2 if typbs2>0 & typbs2<3
        replace eincpb2=-1 if typbs2==0

        replace epropb1=1 if typbs1==1
        replace epropb1=2 if typbs1==2
        replace epropb1=-1 if typbs1<1|typbs1>2

        replace epropb2=1 if typbs2==1
        replace epropb2=2 if typbs2==2
        replace epropb2=-1 if typbs2<1|typbs2>2

        *drop typbs1 typbs2
		
		*** Additional variables
				
		ren pp_ear tpearn
		ren pp_inc tptotinc


        * CREATE TIME SERIES VARIABLE
        destring mnth, gen(interview)
        sort id interview
        capture drop yearmonth
        capture gen yearmonth=.
        replace yearmonth=tm(1990m12)+interview if rot==1
        replace yearmonth=tm(1990m9)+interview if rot==2
        replace yearmonth=tm(1990m10)+interview if rot==3
        replace yearmonth=tm(1990m11)+interview if rot==4

        gen year=year(dofm(yearmonth))
		gen month=month(dofm(yearmonth))

        gen wave=ceil(interview/4)
        drop if wave>8

		sort id yearmonth
		save testm, replace

}


***************** CORE	

if $core_if==1 {

       local i=1
        display "CORE WAVE `i'"
        cd "$rawdata"
	
	use suid rot entry pnum wave fnlwgt hstrat hhsc hwgt intvw  /*
            */ month year refmth hmsa hstate hmetro  /*
            */ takjob takjobn reasab empled wksptr disab njobs /*
            */ wesr1 wesr2 wesr3 wesr4 wesr5 weeks esr /*
            */ wksjob wkswop wkslok enrold ienrold /*
            */ ws1chg ws2chg ws1occ ws2occ ws1ind ws2ind ws1wks ws2wks /*
            */ ws12002 ws22102 ws12003 ws22103 ws12004 ws22104 ws12012 ws22112 /*    2012 /2112 = class of worker
            */ ws12016 ws12018 ws12020 ws12022 ws12023 ws12024 ws12025 ws12026 ws12028 ws12029 /*
            */ ws22116 ws22118 ws22120 ws22122 ws22123 ws22124 ws22125 ws22126 ws22128 ws22129 /*
            */ ws1amt ws2amt /*
           */ se12214 se22314 se12260 se22360 se1amt se2amt se12220 se22320 se1ind se2ind se12218 se22318 /*
            */ se12256 se22356 se1occ se2occ se12224 se22324 se12222 se22322 se12226 se12228 se12230 se22326 se22328 se22330 /*
            */ se12234 se22334 se12232 se22332 se12222 se22322 se12212 se22312 uhours se12201 se22301 se12202 se22302 se1wks se2wks se12203 se22303 se12252 se22352 /*
	    	    		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ hitm36b ws12046 ws22146 ws12044 ws22144 earn totinc /*
		*** Training
		*/ edasst gibill othvet wkstdy pell supped ndsl stloan jtpa emplyr fsship othaid /*
        *** IMPUTATIONS FLAGS *****
		*/ iws22112 idisab ise12260  ireasab itakjob iretird iwksptr iws22124 ise1ind ise2ind /*
		*/ iws1ind iws2ind iws1occ ise1occ iws2occ ise2occ iwksjob iwkswop iwkslok itakjobn iws12012 iws12024	/*
            */ using sip${yeardata}w1, clear
		cd "$tempdata" 
            *generate id
            egen id=concat(suid entry pnum)
            
                    
            gen yearmonth=ym(year, month)
	    
	                
            ***** SAVE
         *across panel data
        gen panel=19${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring suid entry pnum)
                    
            ***** SAVE
      
			
        sort id yearmonth
		compress
		save temp19${yeardata}, replace 
		
local i=`i'+1

		while `i'>=2 & `i'<9 {
                    display "CORE WAVE `i'"
            cd "$rawdata" 
	use suid rot entry pnum wave  fnlwgt hstrat hhsc hwgt intvw  /*
            */ month year refmth hmsa hstate hmetro  /*
            */ takjob takjobn reasab empled wksptr disab njobs /*
            */ wesr1 wesr2 wesr3 wesr4 wesr5 weeks esr /*
            */ retird wksjob wkswop wkslok enrold ienrold /*
            */ ws1chg ws2chg ws1occ ws2occ ws1ind ws2ind ws1wks ws2wks /*
            */ ws12002 ws22102 ws12003 ws22103 ws12012 ws22112 ws12004 ws22104 /*    2012 /2112 = class of worker
            */ ws12016 ws12018 ws12020 ws12022 ws12023 ws12024 ws12025 ws12026 ws12028 ws12029 /*
            */ ws22116 ws22118 ws22120 ws22122 ws22123 ws22124 ws22125 ws22126 ws22128 ws22129 /*
            */ ws1amt ws2amt /*
            */ se12214 se22314 se12260 se22360 se1amt se2amt se12220 se22320 se1ind se2ind se12218 se22318 /*
            */ se12256 se22356 se1occ se2occ se12224 se22324 se12222 se22322 se12226 se12228 se12230 se22326 se22328 se22330 /*
            */ se12234 se22334 se12232 se22332 se12222 se22322 se12212 se22312 uhours se12201 se22301 se12202 se22302 se1wks se2wks se12203 se22303 se12252 se22352 /*
	    	    		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ hitm36b ws12046 ws22146 ws12044 ws22144 earn totinc /*
		*** Training
		*/ edasst gibill othvet wkstdy pell supped ndsl stloan jtpa emplyr fsship othaid /*
        *** IMPUTATIONS FLAGS *****
		*/ iws22112 idisab ise12260  ireasab itakjob iretird iwksptr iws22124 ise1ind ise2ind /*
		*/ iws1ind iws2ind iws1occ ise1occ iws2occ ise2occ iwksjob iwkswop iwkslok itakjobn iws12012 iws12024	/*
            */ using sip${yeardata}w`i', clear
			cd "$tempdata" 

            *generate id
            egen id=concat(suid entry pnum)
            
                    
            gen yearmonth=ym(year, month)
	    
	                
            ***** SAVE
         *across panel data
        gen panel=19${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring suid entry pnum)
                    
	            ***** APPEND TO DATA SETS BEFORE AND SAVE
            append using temp19${yeardata}
			
            sort id yearmonth
			compress
			save temp19${yeardata}, replace
			local i=`i'+1
		}
		
	           *use temp19${yeardata}, clear
            sort id yearmonth
	    compress
            save temp19${yeardata}, replace
	    save temp19${yeardata}core, replace


           ****** REDEFINE VARIABLES
            ren suid su_id
            ren entry pp_entry
            ren pnum pp_pnum
            ren fnlwgt wpfinwgt
            ren hstrat core_gvarstrat
            ren hhsc core_ghlfsam
            ren hwgt whfnwgt
            ren month rhcalmn
            replace year=year+1900
            ren year rhcalyr
            ren refmth srefmon
            ren hmetro tmetro
            ren hmsa tmsa
            ren hstate tfipsst
	   
	    * Cats do not match to 2008
	    ren ws1occ c_tjbocc1
            ren ws2occ c_tjbocc2
	    * Cats do not match to 2008
	    ren ws1ind c_ejbind1
            ren ws2ind c_ejbind2
	    
	    ren takjob rtakjob
            ren takjobn rnotake
            ren reasab eabre 
            ren wksptr eptresn
            ren disab edisab
	    
	        *ren njobs ejobcntr
            gen ejobcntr=njobs
            *replace ejobcntr=. if ejobcntr==0

            
            ren wesr1 rwkesr1
            ren wesr2 rwkesr2
            ren wesr3 rwkesr3
            ren wesr4 rwkesr4
            ren wesr5 rwkesr5
            ren weeks c_rwksperm
            ren esr c_rmesr
            ren wksjob c_rmwkwjb

           ren ws12002 eeno1
            ren ws22102 eeno2

             /* estlemp
            ren  ws12003 estlemp1
            ren ws22103 estlemp2
            capture lab var estlemp1 "Still working for employer1 (at time of interview)"
            recode estlemp1 (1=2) (2=1)
            capture lab var estlemp2 "Still working for employer2 (at time of interview)"
            recode estlemp2 (1=2) (2=1)*/

             *ESTLEMP / WS12003 - WS22103
		* CAREFUL HERE: ESTLEMP refers to 'still being employed by employer 1, by the interview date
		* WS12003/WS22103 refers to the current wave's employer 1 being the same as one in the previous wave
            // CAREFUL: the var labeling in that comes with the .do/dictionary file is probably incorrect, so no need to recode!
	    *capture lab var estlemp1 "Still working for employer1 (at time of interview)"
        *capture lab var estlemp2 "Still working for employer2 (at time of interview)"

            
            *eclwrk
            ren ws12012 c_eclwrk1
            ren ws22112 c_eclwrk2

            capture label define c_eclwrkl 1 "private" 2 "private (merge with 1) not for profit" ///
            3 "local gov" 4 "state gov" 5 "fed gov" 6 "unpaid"
            recode c_eclwrk* (6=.) (5=3) (3=5) (7=6)
            label value c_eclwrk* c_eclwrkl
            
            *date of job ending and job beginning
            

        ** STOPPED WORKING on this job
        ren ws12023 stopjob1
        ren ws22123 stopjob2
        capture lab var stopjob1 "Stopped working for employer 1 (by the time of interview), only asked to those who do not have the full four-months at this employer!"
	    capture lab var stopjob2 "Stopped working for employer 2 (by the time of interview), only asked to those who do not have the full four-months at this employer!"


        ** REASON STOPPED WORKING
        rename     ws12024   ersend1   //1f "What is the main reason ... stopped"
        rename     ws22124   ersend2

 // NOTE IT IS VERY TRICKY WITH THIS QUESTION TO COMPARE
 // ANSWERS
/* ws_2024 1986 1987 1988 1990
V          0 .Not in universe
V          1 .Laid off
V          2 .Retired
V          3 .Discharged
V          4 .Job was temporary and ended
V          5 .Quit to take another job
V          6 .Quit for some other reason

ersend1
V         -1 .Not in universe
V          1 .On layoff
V          2 .Retirement or old age
V          3 .Childcare problems
V          4 .Other family/personal
V            .obligations
V          5 .Own illness
V          6 .Own injury
V          7 .School/training
V          8 .Discharged/fired
V          9 .Employer bankrupt
V         10 .Employer sold business
V         11 .Job was temporary and ended
V         12 .Quit to take another job
V          13.Slack work or business
V            .conditions
V         14 .Unsatisfactory work arrangements
V            .(hours, pay, etc)
V         15 .Quit for some other reason
*/
recode ersend1 (6=15) (5=12) (4=11) (3=8)
recode ersend2 (6=15) (5=12) (4=11) (3=8)

* PAYMENTS / INCOME
ren ws12025 c_ejbhrs1
ren ws22125 c_ejbhrs2

ren ws12026 c_epayhr1
 ren ws22126 c_epayhr2
 
ren ws12028 c_tpyrate1
ren ws22128 c_tpyrate2

ren ws12029 c_rpyper1
ren ws22129 c_rpyper2

ren ws1amt c_tpmsum1
ren ws2amt c_tpmsum2

replace yearmonth=ym(rhcalyr, rhcalmn)

*** selfemployed renaming and missing value decoding


mvdecode se*, mv(0)

ren se12214 egrosb1 
ren se22314 egrosb2
ren se12202 se_bus_sameaslastwave_1
ren se22302 se_bus_sameaslastwave_2
ren se12260 se_earnings_lastyear_ind1
ren se22360 se_earnings_lastyear_ind2
ren se1amt  c_tbmsum1
ren se2amt  c_tbmsum2
ren se1ind  c_tbsind1
ren se2ind  c_tbsind2
ren se12218 tempb1
ren se22318 tempb2
ren se12256 tprftb1
ren se22356 tprftb2
ren se1occ  c_tbsocc1
ren se2occ  c_tbsocc2
ren se12224 ehprtb1
ren se22324 ehprtb2
ren se12226 epartb11
ren se12228 epartb12
ren se12230 epartb21
ren se22326 epartb22
ren se22328 epartb31
ren se22330 epartb32
ren se12234 eoincb1
ren se22334 eoincb2
ren se12232 eslryb1
ren se22332 eslryb2
ren se12212 c_ehrsbs1
ren se22312 c_ehrsbs2
ren se12201 c_ebno1
ren se22301 c_ebno2
ren se1wks  c_sewks1
ren se2wks  c_sewks2
*ren se12202 c_se_previouswave_ind_bs1
*ren se22302 c_se_previouswave_ind_bs2
ren se12203 c_se_activitychange_ind_bs1
ren se22303 c_se_activitychange_ind_bs2
ren se12252 c_se_otherhh_gaveinfo_ind_b1
ren se22352 c_se_otherhh_gaveinfo_ind_b2

ren se12220 c_eincpb1
ren se22320 c_eincpb2
ren se12222 c_epropb1
ren se22322 c_epropb2

       
		ren earn c_tpearn
		ren totinc c_tptotinc
		ren uhours c_ehrsall
		ren edasst c_eedfund
		** Read across not great - do not match well nor mutually exclusively across years apart from those below - more work needed perhaps to match
		*ren gibill easst
		*ren othvet easst
		*ren wkstdy easst
		ren pell c_easst01
		*ren supped easst
		*ren ndsl easst
		*ren stloan easst
		*ren jtpa easst
		ren emplyr easst10
		*ren fsship easst
		ren othaid c_easst11
		
		* Cats do not match to 2008
		ren hitm36b eoutcome
		
		ren ws12044 eunion1
		ren ws22144 eunion2
		ren ws12046 ecntrc1
		ren ws22146 ecntrc2
		
*** save it
        sort id yearmonth
	capture egen idwave=concat(id wave)
        save temp19${yeardata}, replace
	
	*** JOB ID REDEFINITION (works on firm numbers in the core waves)

        
        sort idwave
        keep idwave eeno1 eeno2
        
        mvdecode _all, mv(0)
        duplicates drop 
        reshape long eeno, i(idwave) j(jobrecordno) 
        
        drop if eeno==.        
        save testjit, replace

        cd "$rawdata" 
        use suid entry pnum jobid* wave using sip${yeardata}jid, clear
        cd "$tempdata"
        
        *generate id
        egen id=concat(suid entry pnum)
        egen idwave=concat(id wave)
        
        ren jobid eeno
        sort idwave
        
        merge idwave eeno using  testjit, sort
        
        drop _merge
        reshape wide jobid* eeno , i(idwave) j(jobrecordno)
        
        merge idwave using temp19${yeardata}, sort uniqmaster
        
        drop suid entry pnum idwave
        ren eeno1 eeno1_old 
        ren eeno2 eeno2_old
        ren jobid_revised1 eeno1
        ren jobid_revised2 eeno2
        
        
*** save it
            sort id wave yearmonth
			save temp19${yeardata}, replace
			
	    save temp19${yeardata}core, replace
	    
}

	    
if $tm_if==1{

 ******** Note, all TM variables need checking relative to 2008

 ******** TOPICAL MODULE 2 - Work history, education, training

 local i=2       // contains TM1 with tenure data!
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata"
		use id rotation pnum entry item36b intvw tm8400 tm8408 tm8416 /*
            */ tm8422 tm8430 tm8206 tm8208 tm8214 tm8216 tm8218 tm8220 tm8222  tm8240 tm8242 tm8248 /*
            */ tm8234 tm8236 /*
            */ tm8250 tm8246 tm8266 tm8268 tm8270 tm8252 tmind3 tmind4 tm8272 /*
            */ tm8274 tm8276 tm8278 tm8280 tm8282 tm8284 tm8286 tm8288 tm8290 tm8292 tm8294 /*
	     ** Additional variables - assume only those below need checking for consistency
	     */ tm8224 tm8226 tm8228 /*
	     ** All will require renaming etc
	     ** Education
	     */ tm8402 tm8404 tm8406 tm8410 tm8412 tm8414 tm8418 tm8420 tm8424 tm8426 tm8428 /*
	     */ tm8432 tm8434 tm8436 tm8438 tm8440 tm8442 /*
	     ** Training
	     */ tm8446 tm8448 tm8450 tm8452 tm8454 tm8456 tm8458 tm8460 tm8462 tm8464 tm8466 tm8468 /*
	     */ tm8470 tm8472 tm8474 tm8476 tm8478 tm8480 tm8482 tm8484 tm8486 tm8488 tm8490 tm8492 /*
	     */ tm8494 tm8496 tm8498 tm8500 tm8502 tm8504 tm8506 tm8508 tm8510 tm8512  /*
        IMPUTATIONS FLAGS
	     */ imp_8218 imp_8220 imp_8226 imp_8224 imp_8228 imp_8230 imp_8232 imp_8234 imp_8240 imp_8242 /*
	     */ imp_8244 imp_8246 imp_8248 imp_8250 imp_8252 imp_8266 imp_8268 imp_8270 /*
	     */ imp_8272 imp_8274 imp_8276 imp_8278 imp_8282 imp_8286 imp_8288 imp_8290 imp_829a imp_8292 imp_8294 imp_ind imp_occ /*

            */ using sip${yeardata}t`i', clear
         cd "$tempdata"
	

            *generate id
            ren id suid
            egen id=concat(suid entry pnum)
            gen wave=`i'

            sort id
	                ***** REDEFINE SOME VARIABLES
			
	     ren intvw intvw_tm2
	    lab var intvw_tm2 "interview status in topical module 2"
	    
	    ren item36b item36b_tm2
	    lab var item36b_tm2 "reason noninterview - household interview code, tm2 (control item 36b)"
           

	    mvdecode tm820*, mv(0)
            mvdecode tm821*, mv(0)

            ren tm8206 eeno1_startpanel
            ren tm8208 ebno1_startpanel
            ren tm8214 eeno2_startpanel
            ren tm8216 ebno2_startpanel

            gen tsjdate_startpanel=ym(tm8220,tm8218) if eeno1_startpanel!=.|eeno2_startpanel!=.
            gen tsbdate_startpanel=ym(tm8220,tm8218) if ebno1_startpanel!=.|ebno2_startpanel!=.
            lab var tsjdate_startpanel "start data main job in tm2"
            lab var tsbdate_startpanel "start data main job in tm2"


            format tsjdate_startpanel %tm
	
	       gen byte imp_tsjdate_startpanel_yr=1 if imp_8220==1
	       gen byte imp_tsjdate_startpanel_mth=1 if imp_8218==1
	

            * end previous job
            gen endprevjb_tm=.
            replace endprevjb_tm=ym(tm8242, tm8240) if (tm8242!=0 & tm8240!=0)
            replace endprevjb_tm=ym(tm8250, tm8248) if (tm8250!=0  & tm8248!=0)
            lab var endprevjb_tm "end date job previous to panel"

            gen endprevjbyr_tm=.
            replace endprevjbyr_tm=tm8242  if tm8242!=0
            replace endprevjbyr_tm=tm8250 if tm8250!=0
            lab var endprevjbyr_tm "end year job previous to panel"

           *imputation
		  gen imp_endprevjb_yr=1 if imp_8242==1 & (tm8242!=0)
		  replace imp_endprevjb_yr=1 if imp_8250==1 & (tm8250!=0)
	
        gen imp_endprevjb_mth=1 if imp_8240==1 & (tm8242!=0)
		replace imp_endprevjb_mth=1 if imp_8248==1 & (tm8248!=0)
	
** FOR ALL JOBS THAT ENDED *****AFTER 1979???****, they ask more questions, including starting date.
		

            * start previous job
            gen startprevjb_tm=.
            replace startprevjb_tm=ym(tm8270, tm8268) if (tm8270!=0 & tm8268!=0)
            lab var startprevjb_tm "start date job previous to panel"

            gen startprevjbyr_tm=.
            replace startprevjbyr_tm=tm8270  if tm8270!=0
            lab var startprevjbyr_tm "start year job previous to panel"


        *imputation		
		gen imp_startprevjb_yr=1 if imp_8270==1 & (tm8270!=0)
		gen imp_startprevjb_mth=1 if imp_8268==1 & (tm8268!=0)

*CAREFUL HERE: the employment during w2, w1 is the most recent of wave 1 (or the most recent of w2, if no employment in wave 1. There might be jobs
	       * in between the most recent job in wave 1, and the job most recently ended before wave 1.
            capture drop empgap
	    gen empgap=tsjdate_startpanel-endprevjb_tm
	    capture drop empgap_noimp
	    gen empgap_noimp=empgap
	    replace empgap_noimp=. if imp_tsjdate_startpanel_yr==1 | imp_endprevjb_yr==1 | imp_tsjdate_startpanel_mth==1 | imp_endprevjb_mth==1
		

            * reason no work that lasted more than 2 wks
            ren tm8246 rsno2wkwrk
            lab var rsno2wkwrk "Reason never worked more than 2 weeks straight"

            * employed or self-employed in previous job
            ren tm8266 prevjbse
            lab var prevjbse "Previous job employed(1) or self-employed(2)"
            replace prevjbse=. if prevjbse==0
            
            *industry of previous job
            ren tmind3 indprevjb
            lab var indprevjb "industry of job previous to panel"
            replace indprevjb=. if indprevjb==0
            
            *occupation of previous job
            ren tmind4 occprevjb
            lab var occprevjb "occupation of job previous to panel"
            replace occprevjb=. if occprevjb==0
                        
            *reason previous job ended
            ren tm8272 rsnendprevjb
            lab var rsnendprevjb "reason previous job ended"
            
            *year first worked 6months
            ren tm8274 firstyearlf 
            lab var firstyearlf "year first worked at least 6 months"
            
            *always worked at least 6months for every year since
            ren tm8276 alwayswork6m
            lab var alwayswork6m "Always worked at least six months in every year"
            
            *how many years worked at least 6 months
            ren tm8278 years6mworked
            lab var years6mworked "Number of years worked at least six months"
            replace years6mworked=. if years6mworked==0
            
            
            *any years since entering not worked >6mtns Y/N
            ren tm8286 notworked6m
            
            *how many times since entering not worked 6mths straight
            ren tm8288 timesnotworked6m
            
            *most recent time not worked 6 months from and to
            ren tm8290 recentbreak6mfr
            ren tm8292 recentbreak6mto
            
            *reason of this recent break
            ren tm8294 rsnbreak6m
	    
	    *** Additional Variables
	    
	    *** Note, these variables below relate to employer size, and are equivalent to variables found in core waves for 2004-2008
	    ** May need further recoding for consistency
	    
	    ren tm8224 tempsiz1
	    ren tm8226 eemploc1
	    ren tm8228 tempall1
	    
	    gen eadvncfd = tm8428 if tm8422 > 0 | tm8422 < 4
	    gen ebachfld = tm8428 if tm8422 ==4
	    gen eassocfd = tm8428 if tm8422 ==5
	    gen evocfld = tm8428 if tm8422 ==6
	    
	    * note, engineering splits in 2008
	    * Economics in 88 recoded to social sciences etc...
	    * recodes not straightforward - perhaps decide late rif necessary
	    * recode eadvncfd (2=14) (4=18) (5=6) (6=7) (7=8) (9=10)
	    
	    ren tm8414 epubhs
	    recode epubhs (3=2) (4=3)
	    
	    ren tm8404 tlstschl
	    replace tlstschl = 1 if tm8406==1
	    replace tlstschl = 9999 if tm8406==2
	    drop tm8406 tm8402
	    
	    ren tm8412 thsyr
	    drop tm8410
	    
	    ren tm8420 tcollstr
	    drop tm8418
	    
	    ren tm8440 tlastcol
	    replace tlastcol = 1 if tm8442==1
	    drop tm8438 tm8442

	    gen tadvncyr = tm8426 if tm8422 > 0 | tm8422 < 4
	    gen tbachfyr = tm8426 if tm8422 ==4
	    gen tassocyr = tm8426 if tm8422 ==5
	    gen tvocyr = tm8426 if tm8422 ==6

	    *****************************************************
	    * Note recoding/renaming training variables relative to 2008 is challenging.
	    * In 2008, separate questions for training related to "job skills" and "job search & trainig  for new job"
	    * In 1988, questions all realte to both these types of training
	    * Renaming below assumes al in 88 fall in first category. However, this is clearly unsatisfactory.
	    * Perhaps need to combine types in 2008 and later panels
	    * Or could potentially distinguish types in 88 using variables retained on training programme sponsorship
	    
	    
	    ren tm8446 ercvtrn1
	    ren tm8502 eweekt1
	    ren tm8504 etrn1tim
	    * Note, for recode, no equivalent to 1, i.e. less that 1 full day in 88
	    recode etrn1tim (-4=2) (-3=4)
	    replace etrn1tim =3 if eweekt1>0
	    
	    gen ewhotrn1 = tm8510
	    replace ewhotrn1 = 2 if tm8506 ==1
	    replace ewhotrn1 = 3 if tm8508 ==1
	    replace ewhotrn1 = 4 if tm8512 ==1
	    drop tm8506 tm8508 tm8510 tm8512
	   
	    * Note: recode apprenticeship program to 2008 value 5 "At current or previous employer's place of work"
	    gen elctntr1 = tm8474
	    replace elctntr1 = 2 if tm8480 == 1
	    replace elctntr1 = 3 if tm8476 == 1
	    replace elctntr1 = 4 if tm8478 == 1
	    replace elctntr1 = 5 if tm8482 == 1
	    replace elctntr1 = 5 if tm8488 == 1
	    replace elctntr1 = 5 if tm8472 == 1
	    replace elctntr1 = 6 if tm8486 == 1
	    replace elctntr1 = 7 if tm8490 == 1
	    replace elctntr1 = 8 if tm8492 == 1
	    replace elctntr1 = 9 if tm8494 == 1
	    replace elctntr1 = 9 if tm8484 == 1
	    drop tm8472 tm8474 tm8476 tm8478 tm8480 tm8482 tm8484 tm8486 tm8488 tm8490 tm8492 tm8494
	    
	    ren tm8496 ejbbtrn1

		*Note Recoding type of training not straightforward - which way?
	    *gen etyp1tr = tm8466
	    *replace etyp1tr = 2 if tm8460 == 1
	    *replace etyp1tr = 3 if tm8462 == 1
	    *replace etyp1tr = 4 if tm8464 == 1
	    *replace etyp1tr = 5 if tm8468 == 1
	    *replace etyp1tr = 6 if tm8470 == 1
	    *drop tm8460 tm8462 tm8464 tm8466 tm8468 tm8470
	
     

	    
	    ***** APPEND TO DATA SETS BEFORE AND SAVE
	save temp19${yeardata}tm, replace
	compress
	
   ******** TOPICAL MODULE 7  BUSINESS VALUE
      
        local i=7       // contains TM1 with tenure data!
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use id rotation pnum entry tm8000 tm8002 tm8004 tm8006 tm8008 tm8012 /*
            */ tm8016 tm8018 tm8020 tm8022 tm8024 tm8028 /*
            */ using sip${yeardata}t`i', clear
         cd "$tempdata" 
            	

            *generate id wave
            ren id suid
            egen id=concat(suid entry pnum)
            
            gen wave=`i'
            sort id wave
            
            
            ***** REDEFINE SOME VARIABLES
            mvdecode tm80*, mv(0)
            replace tm8008=0 if tm8008==-3
            replace tm8012=0 if tm8012==-3
            
            ren tm8000 se_indicator_1
            ren tm8002 se_proprietor_ind_1
            ren tm8016 se_indicator_2
            ren tm8018 se_proprietor_ind_2
            ren tm8006 se_otherhh_gaveinfo_1
            ren tm8022 se_otherhh_gaveinfo_2
            
            ren tm8004 evbow1
            ren tm8008 tvbva1
            ren tm8012 tvbde1
            ren tm8020 evbow2
            ren tm8024 tvbva2_ind
            ren tm8028 tvbde2_ind
            
            
                
		***** APPEND TO DATA SETS BEFORE AND SAVE
			append using temp19${yeardata}tm
			sort id wave
			save temp19${yeardata}tm, replace
			compress
			
   ******** RESEARCH TOPICAL MODULE 5 & 8  BUSINESS VALUE
   
   ** Note, business value data is available for 8, but appears incomplete 
      
 forvalues i=8(3)8{       // contains TM1 with tenure data!
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use id rotation pnum entry  /*
            */ tm8006 tmind1 tmind2 tm8012 tm8062 tm8014 tm8064 /*
            */ tm8016 tm8066 tm8010 tm8060 tm8112 tm8162 tm8108 /*
            */ tm8110 tm8114 tm8116 tm8164 tm8166 tm8018 tm8068 /*
            */ tm8118 tm8168 tm8120 tm8170 tm8158 tm8160 tm8202 /*
            */ tm8252 tm8210 tm8260 tm8212 tm8218 tm8262 tm8268 /*
            */ tm8214 tm8264 tm8220 tm8270 tm8020 tm8070 /*
            */ using sip${yeardata}r`i', clear
         cd "$tempdata" 
            	

            *generate id wave
            ren id suid
            egen id=concat(suid entry pnum)
            
            gen wave=`i'
            sort id wave
            
            
            ***** REDEFINE SOME VARIABLES
            mvdecode tm80*, mv(0)
            mvdecode tm81*, mv(0)
            mvdecode tm82*, mv(0)
            
            
            ren tm8006 se_aira_numberbusinesses
            ren tmind1 se_aira_ind1
            ren tmind2 se_aira_ind2
            ren tm8012 se_aira_otherhh_gaveinfo1
            ren tm8062 se_aira_otherhh_gaveinfo2
            ren tm8014 se_aira_otherhh_pnum1
            ren tm8064 se_aira_otherhh_pnum2
            ren tm8016 se_aira_otherhh_ebno1
            ren tm8066 se_aira_otherhh_ebno2
            ren tm8010 se_aira_ebno1
            ren tm8060 se_aira_ebno2
            
            ren tm8112 ihhown1
            ren tm8162 ihhown2
            ren tm8108 iownrs11
            ren tm8110 iownrs12
            ren tm8114 rpcnthh1
            ren tm8116 rpctown1
            ren tm8164 rpcnthh2
            ren tm8166 rpctown2
            ren tm8018 ibsform1
            ren tm8068 ibsform2
            ren tm8118 tgrsrcp1
            ren tm8168 tgrsrcp2
            ren tm8120 ttotexp1
            ren tm8170 ttotexp2
            ren tm8158 iownrs21
            ren tm8160 iownrs22
            
            ren tm8202 tnetinc1
            *ren tm8204 tnetinc2
            ren tm8252 tnetinc3
            *ren tm8254 tnetinc4
            ren tm8260 iothinc2
            ren tm8212 inetin11
            ren tm8218 inetin21
            ren tm8262 inetin31
            ren tm8268 inetin41
            ren tm8214 tnetin12
            *ren tm8216 tnetin13
            ren tm8264 tnetin22
            *ren tm8266 tnetin23
            ren tm8220 tnetin32
            *ren tm8222 tnetin33
            ren tm8270 tnetin42
            *ren tm8272 tnetin43
            
            ren tm8020 ibsloct1
            ren tm8070 ibsloct2
  
		***** APPEND TO DATA SETS BEFORE AND SAVE
			append using temp19${yeardata}tm
			sort id wave
			save temp19${yeardata}tm, replace
			compress


			}
***** Note, error observed in 1990 t5 regarding pnum  is not an issue in 91

local i=5
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use id rotation pnum entry  /*
	    ***** School Enrollment and Financing
	    */ tm9610 tm9612 tm9616 tm9618/*
            */ tm9620/*
	    */ tm9628 tm9632 tm9636 tm9640 tm9644 tm9648 tm9652 tm9656 tm9660 tm9664 tm9668 tm9672/*
	    */ tm9630 tm9634 tm9638 tm9642 tm9646 tm9650 tm9654 tm9658 tm9662 tm9666 tm9670 tm9674 tmtedfin/*
            */ using sip${yeardata}t`i', clear
         cd "$tempdata" 
            	

            *generate id wave
            ren id suid
            egen id=concat(suid entry pnum)
            
            gen wave=`i'
            sort id wave
            
	    ren tm9610 renrold
	    ren tm9612 egrlevel
	    recode egrlevel (10=9) (11=9) (12=10)
	    gen epubpriv = tm9616 if egrlevel==2
	    drop tm9616
	    * Note, wider value fields in 88, therefore need to recode in later panels
	    ren tm9618 ttuition
	    ren tm9620 tothcost
	    
	
		* note, similarly with easst above, more work perhaps needed on recoding values.
	    ren tm9628 redaid2
	    replace redaid2=1  if tm9632==1 
	    ren tm9636 redaid3
	    ren tm9640 redaid1
	    ren tm9644 redaid4
	    ren tm9648 redaid5
	    replace redaid5=1  if tm9652==1
	    replace redaid4=1  if tm9656==1
	    ren tm9660 redaid10
	    ren tm9664 redaid7
	    ren tm9668 redaid6
	    ren tm9672 redaid11
	    drop tm9632 tm9652 tm9656
	    
	    ren tm9630 tedamt2
	    replace tedamt2 = tm9634 if tedamt2<=0
	    replace tedamt2 = tedamt2 + tm9634 if tedamt2>0
	    ren tm9638 tedamt3
	    ren tm9642 tedamt1
	    ren tm9646 tedamt4
	    ren tm9650 tedamt5
	    replace tedamt5 = tm9654 if tedamt5<=0
	    replace tedamt5 = tedamt2 + tm9654 if tedamt5>0
	    replace tedamt4 = tm9658 if tedamt4<=0
	    replace tedamt4 = tedamt2 + tm9658 if tedamt4>0
	    ren tm9662 tedamt10
	    ren tm9666 tedamt7
	    ren tm9670 tedamt6
	    ren tm9674 tedamt11
	    drop tm9634 tm9654 tm9658
	    *note rounding of amounts, may differ from later panels
	    ren tmtedfin ttotamt
	    
                
		***** APPEND TO DATA SETS BEFORE AND SAVE
			*merge id wave using temp19${yeardata}tm , _merge(mrg5) sort uniqmaster
			append using temp19${yeardata}tm
			sort id wave
			save temp19${yeardata}tm, replace
			compress	
			
		

local i=8     
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use id rotation pnum entry  /*
	    ***** School Enrollment and Financing
	    */ tm9610 tm9612 tm9616 tm9618/*
            */ tm9620/*
	    */ tm9628 tm9632 tm9636 tm9640 tm9644 tm9648 tm9652 tm9656 tm9660 tm9664 tm9668 tm9672/*
	    */ tm9630 tm9634 tm9638 tm9642 tm9646 tm9650 tm9654 tm9658 tm9662 tm9666 tm9670 tm9674 tmtedfin/*
	
            */ using sip${yeardata}t`i', clear
         cd "$tempdata" 
            	

            *generate id wave
            ren id suid
            egen id=concat(suid entry pnum)
            
            gen wave=`i'
            sort id wave
            
	    ren tm9610 renrold
	    ren tm9612 egrlevel
	    recode egrlevel (10=9) (11=9) (12=10)
	    gen epubpriv = tm9616 if egrlevel==2
	    drop tm9616
	    * Note, wider value fields in 88, therefore need to recode in later panels
	    ren tm9618 ttuition
	    ren tm9620 tothcost
	    
	
		* note, similarly with easst above, more work perhaps needed on recoding values.
	    ren tm9628 redaid2
	    replace redaid2=1  if tm9632==1 
	    ren tm9636 redaid3
	    ren tm9640 redaid1
	    ren tm9644 redaid4
	    ren tm9648 redaid5
	    replace redaid5=1  if tm9652==1
	    replace redaid4=1  if tm9656==1
	    ren tm9660 redaid10
	    ren tm9664 redaid7
	    ren tm9668 redaid6
	    ren tm9672 redaid11
	    drop tm9632 tm9652 tm9656
	    
	    ren tm9630 tedamt2
	    replace tedamt2 = tm9634 if tedamt2<=0
	    replace tedamt2 = tedamt2 + tm9634 if tedamt2>0
	    ren tm9638 tedamt3
	    ren tm9642 tedamt1
	    ren tm9646 tedamt4
	    ren tm9650 tedamt5
	    replace tedamt5 = tm9654 if tedamt5<=0
	    replace tedamt5 = tedamt2 + tm9654 if tedamt5>0
	    replace tedamt4 = tm9658 if tedamt4<=0
	    replace tedamt4 = tedamt2 + tm9658 if tedamt4>0
	    ren tm9662 tedamt10
	    ren tm9666 tedamt7
	    ren tm9670 tedamt6
	    ren tm9674 tedamt11
	    drop tm9634 tm9654 tm9658
	    *note rounding of amounts, may differ from later panels
	    ren tmtedfin ttotamt
	    
	
			             
		***** APPEND TO DATA SETS BEFORE AND SAVE
			merge id wave using temp19${yeardata}tm , _merge(mrg8) sort uniqmaster
			tab mrg8
			*append using temp19${yeardata}tm
			sort id wave
			save temp19${yeardata}tm, replace
			compress
			
***** APPEND TM DATASET TO CORE DATA SET
	cd "$tempdata"
        capture use temp19${yeardata}tm
	sort id wave
	capture drop _mrgtot
        merge id wave using temp19${yeardata}core, _merge(_mrgtot) sort uniqmaster
        sort id wave yearmonth
	tab _mrgtot           
	save temp19${yeardata}, replace
	display "_mrgtot==1 means that the individual appears in the TM somewhere, but not in core waves; STRANGE!"
	display "_mrgtot==2 means that the individual appears in the core wave dataset somewhere, but did not partake in the TM"
	display "_mrmtot==3 individual in the core wave data set and did partake in some TM"
	compress
				
	}

   *** THE BIG MERGE 
    
		use testi, clear
		drop if pp_id==""
		capture drop _mrgtot1 _mrgtot2 
		merge id using testw, sort _merge(_mrgtot1) uniqmaster
		tab _mrgtot1
		rename _mrgtot1 mtst1
		sort id wave 
		merge id wave using testm, _merge(_mrgtot2) sort uniqmaster
		tab _mrgtot2
		rename _mrgtot2 mtst2
		sort id yearmonth

		capture drop _mrgtot3
		merge id yearmonth using temp19${yeardata}, sort _merge(_mrgtot3) uniqmaster
		tab _mrgtot3
		display "_mrgtot==1 means that the individual appears in the FP somewhere, but not in core/TM waves"
		display "_mrgtot==2 means that the individual appears in the core wave dataset somewhere, but did not in the FP"
		display "_mrmtot==3 individual in the core wave data set and FP"
		rename _mrgtot3 mtst3
		drop if mtst3~=3
		drop mtst*

        
cd "$outputdata"

save 19${yeardata}total_raw, replace
*saveold 19${yeardata}total_raw2013, replace

*************************************************
             // START AND END DATE DATE
*************************************************

capture gen tsjdate1=.
    // if rhcalmn+4-srefmon>12 then the highest possible start date that is in this wave is in the next year

        // if the starting rhcalmn is above the highest possible start date, it must have occured this year
                replace tsjdate1=mdy(ws12016, ws12018, rhcalyr) if ws12016>rhcalmn-srefmon-8 & rhcalmn-srefmon>8
        // however, if the rhcalmn is lower than the highest possible rhcalmn in the next year, it will refer to a rhcalmn next year
                replace tsjdate1=mdy(ws12016, ws12018, rhcalyr+1) if ws12016<=rhcalmn-srefmon-8 & ws12016<=rhcalmn-srefmon-8

    // else the highest possible start date in this wave is in this year, if rhcalmn-srefmon<=8

        // and so any date before this date, in this year is assigned to this years
                replace tsjdate1=mdy(ws12016, ws12018, rhcalyr) if rhcalmn-srefmon<=8 & ws12016<=rhcalmn+4-srefmon
        // higher rhcalmns have to have occurred in the past year
                replace tsjdate1=mdy(ws12016, ws12018, rhcalyr-1) if rhcalmn-srefmon<=8 & ws12016>rhcalmn+4-srefmon


// end dates cannot occur before the beginning of this wave
capture gen tejdate1=.

        // the beginning of this wave can occur in the last year  if rhcalmn-srefmon<0

                // then if if ws12020>rhcalmn-srefmon+12
                          replace tejdate1=mdy(ws12020, ws12022, rhcalyr-1) if rhcalmn-srefmon<0 & ws12020>rhcalmn-srefmon+12
                //if ws12020<=rhcalmn-srefmon+12 {
                            replace tejdate1=mdy(ws12020, ws12022, rhcalyr) if rhcalmn-srefmon<0 & ws12020<=rhcalmn-srefmon+12

        //  the beginning of this wave is this year if rhcalmn-srefmon>=0
        //  the starting rhcalmn is higher than the beginning of the wave

            //if ws12020>rhcalmn-srefmon {
                replace tejdate1=mdy(ws12020, ws12022, rhcalyr) if rhcalmn-srefmon>=0 & ws12020>rhcalmn-srefmon
           // the starting rhcalmn is lower than the beginning rhcalmn of the wave, so it must be next year, if ws12020<=rhcalmn-srefmon
                replace tejdate1=mdy(ws12020, ws12022, rhcalyr+1) if rhcalmn-srefmon>=0 & ws12020<=rhcalmn-srefmon


**************** SECOND FIRM


capture gen tsjdate2=.
    // if rhcalmn+4-srefmon>12 then the highest possible start date that is in this wave is in the next year, if rhcalmn-srefmon>8 {

        // if the starting rhcalmn is above the highest possible start date, it must have occured this year, if ws22116>rhcalmn-srefmon-8{
                replace tsjdate2=mdy(ws22116, ws22118, rhcalyr) if rhcalmn-srefmon>8 & ws22116>rhcalmn-srefmon-8
        // however, if the rhcalmn is lower than the highest possible rhcalmn in the next year, it will refer to a rhcalmn next year,  if ws22116<=rhcalmn-srefmon-8 {
                replace tsjdate2=mdy(ws22116, ws22118, rhcalyr+1) if rhcalmn-srefmon>8 & ws22116<=rhcalmn-srefmon-8

        // else the highest possible start date in this wave is in this year, if rhcalmn-srefmon<=8 {
        // and so any date before this date, in this year is assigned to this years, if ws22116<=rhcalmn+4-srefmon {
            replace tsjdate2=mdy(ws22116, ws22118, rhcalyr) if rhcalmn-srefmon<=8 & ws22116<=rhcalmn+4-srefmon

        // higher rhcalmns have to have occurred in the past year, if ws22116>rhcalmn+4-srefmon {
            replace tsjdate2=mdy(ws22116, ws22118, rhcalyr-1) if rhcalmn-srefmon<=8 & ws22116>rhcalmn+4-srefmon


        // end dates cannot occur before the beginning of this wave
capture gen tejdate2=.

        // the beginning of this wave can occur in the last year,if rhcalmn-srefmon<0 {

        // then if ws22120>rhcalmn-srefmon+12 {
                replace tejdate2=mdy(ws22120, ws22122, rhcalyr-1) if rhcalmn-srefmon<0 & ws22120>rhcalmn-srefmon+12

        //if ws22120<=rhcalmn-srefmon+12 {
                replace tejdate2=mdy(ws22120, ws22122, rhcalyr) if rhcalmn-srefmon<0 & ws22120<=rhcalmn-srefmon+12

        //  the beginning of this wave is this year, if rhcalmn-srefmon>=0 {
        //  the starting rhcalmn is higher than the beginning of the wave, if ws22120>rhcalmn-srefmon {
                replace tejdate2=mdy(ws22120, ws22122, rhcalyr) if rhcalmn-srefmon>=0 & ws22120>rhcalmn-srefmon

        // the starting rhcalmn is lower than the beginning rhcalmn of the wave, so it must be next year, if ws22120<=rhcalmn-srefmon {
                replace tejdate2=mdy(ws22120, ws22122, rhcalyr+1) if rhcalmn-srefmon>=0  & ws22120<=rhcalmn-srefmon




format tejdate1 %td
format tsjdate1 %td
format tsjdate2 %td
format tejdate2 %td

 rename     ws12016     smonth1 //2f "Month in which this person"
 rename     ws12018     sday1 //2f "Day of month shown in WS1-2016"
 rename     ws12020     emonth1 //2f "Month in which this person left"
 rename     ws12022     eday1 //2f "Day of month shown in WS1-2020"

 rename     ws22116     smonth2 //2f "Month in which this person"
 rename     ws22118     sday2 //2f "Day of month shown in WS1-2016"
 rename     ws22120     emonth2 //2f "Month in which this person left"
 rename     ws22122     eday2 //2f "Day of month shown in WS1-2020"


        
        // dropping and further renaming
        capture ren att_sch eenrlm
        capture drop personkeytemp
        capture ren sc1360 eeveret
        format yearmonth %tm
        
         replace pnlwgt=pnlwgt*10000
         replace fnlwgt91=fnlwgt91*10000
         replace fnlwgt92=fnlwgt92*10000
         replace whfnwgt=whfnwgt*10000
         replace wpfinwgt=wpfinwgt*10000
        
        
        // missing variable redefine
        mvdecode _all, mv(-1)
        mvdecode _all, mv(-9)
        mvdecode _all, mv(-3)

        capture noisily mvdecode eeveret tmetro tmsa rwksperm rtakjob rnotake edisab , mv(0)
        capture noisily mvdecode c_epayhr* c_tpyrate* c_rpyper* ersend* grd_cmp , mv(0)
        capture noisily mvdecode eafnow tfipsst eenrlm ehrsall c_ehrsall pp_mis ms_ c_rmesr* c_eclwrk* c_tjbocc* , mv(0)
        capture noisily mvdecode c_ejbind* c_ejbhrs* c_tpyrate* higrade, mv(0)
        
        *recode/relabel variables in full panel
        capture label drop eclwrkl
        label define eclwrkl 1 "private" 2 "private (merge with 1) not for profit" 3 "local gov" 4 "state gov" 5 "fed gov" 6 "unpaid"
         capture noisily label value eclwrk* eclwrkl
	    capture noisily label value fp_eclwrk* eclwrkl
	    capture noisily label value c_eclwrk* eclwrkl
        capture noisily recode eclwrk* (6=.) (5=3) (3=5) (7=6)
	   capture noisily recode fp_eclwrk* (6=.) (5=3) (3=5) (7=6)
	   capture noisily recode c_eclwrk* (6=.) (5=3) (3=5) (7=6)



/*   1988       
        V          0 .Not in universe, not in sample,                                   
V            .nonmatch                                                          
V          1 .A private for-profit company or                                   
V            .individual ?                                                      
V          2 .A private not-for-profit, tax                                     
V            .exempt, or charitable                                             
V            .organization                                                      
V          3 .Federal government                                                
V            . (exclude Armed Forces)                                           
V          4 .State government                                                  
V          5 .Local government                                                  
V          6 .Armed Forces                                                      
V          7 .Unpaid in family business or                                      
V            .farm
 */                           
        
        *schooling data
    gen educ = 1 if higrade<=12 & higrade~=.
	replace educ = 2 if higrade==12 & grd_cmp==1
	replace educ = 3 if higrade>=21 & higrade<=24
	replace educ = 4 if higrade==24 & grd_cmp==1
	replace educ = 4 if higrade>24 & higrade~=.
	*drop higrad grd_cmp
	lab var educ "Educational Attainment"
    notes educ: SIPP: higrade & grd_cmp (92,93)

	* highest grade 12 or less and no hs diploma or ged
	gen educ_tm2 = 1 if tm8400==1 & tm8408==2
	* highest grade 12 or less and has hs diploma or ged
	replace educ_tm2 = 2 if tm8400==1 & tm8408==1
	replace educ_tm2 = 2 if tm8400==2 
	* highest grade 1 year of college and  ass., voc, or no degree
	replace educ_tm2 = 3 if tm8416==1 & tm8422>=5 & tm8422<=7
	* highest grade 1 year of college and ba degree
	replace educ_tm2 = 4 if tm8416==1 & tm8422==4
	* highest grade 1 year of college and adv. degree
	replace educ_tm2 = 5 if tm8416==1 & tm8422>=1 & tm8422<=3 & tm8430==1
	
    label var educ_tm2 "TM2 Educational Attainment"
    
    capture drop tm8400 tm8408 tm8416 tm8422 tm8430
    
    label define educl 1 "less than high school" 2 "high school grad" /*
*/ 3 "some college" 4 "college degree" 5 "post-college"
        
     lab val educ educl
     lab val educ_tm2 educl
     
     
     *** DROPPING NONINTERVIEWED OBSERVATIONS 
     capture drop if pp_mis!=1
     sort id yearmonth 
     
     gen continuous_spell=.
     replace continuous_spell=1 if personkey[_n-1]!=personkey
     replace continuous_spell=1 if personkey[_n-1]==personkey & yearmonth[_n-1]==yearmonth-1 & continuous_spell[_n-1]==1
     replace continuous_spell=0 if personkey[_n-1]==personkey & yearmonth[_n-1]!=yearmonth-1 
     replace continuous_spell=0 if personkey[_n-1]==personkey & continuous_spell[_n-1]!=1 
     
     
     lab var ehrsall "How many hours usually worked per week, in the weeks worked during the 4-month period"
     *ren uhours c_ehrsall
     lab var c_ehrsall "core: how many hours usually worked per week, in the weeks worked during the 4-month period"

     lab var eeveret  "fp: ever retired from a job or business"
     lab var pp_intv   "fp: Person's interview status for the relevant interview"
     ren ms_ ms     
     lab var pp_mis     "fp: MONTHLY INTERVIEW STATUS"
     lab var tage "Age"
     
     ren tfipsst c_tfipsst
     lab var fp_tfipsst "fp: FIPS state code"
     lab var eafnow "fp: Currently in Armed Forces Indicator (Y/N)"
     lab var higrade "fp: What is the highest grade or year of regular school this person attended ?"
     lab var grd_cmp "fp: Completed the highest grade attended? (Y/N)"

     mvdecode rwkesr*, mv(0)
     label define rwkesrl 1 "w/ job", modify     
     label define rwkesrl 2 "job, absent", modify
     label define rwkesrl 3 "job, layoff/looking", modify
     label define rwkesrl 4 "no job, look/layoff", modify
     label define rwkesrl 5 "no job, not looking", modify          
   
     lab val rwkesr* rwkesrl

     capture ren ebuscntr job_or_se
     capture ren empled job_or_se
     
cd "$outputdata"

save 19${yeardata}total_v${fileversion}, replace
*saveold 19${yeardata}total_v${fileversion}_2013, replace

        capture log close

