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
** STEP 0.1.1 CTV DATA CONSTRUCTION, 2004 PANEL  
********************************************************************************

* helpful input from Carl Singleton gratefully acknowledged  

version 13
clear all
*capture log close
*set mem 3000m
set more off
*set maxvar 10000


** GLOBALS
global yeardata "04"
global gnulin=0
* control parts of the do-file
global core_if=1	// read in the core waves
global tm_if=1	       // read in the topical modules
global weights_if=1   // loads in the longitudinal weights
global subset_if=1   // starts reducing datasets into subsets with relevant variables for different projects
global panel=2004
global locfileversion=0816

/* Subsets:
	1: Self-employment-minimal
	2: ...
	3: ...
	4: etc */


global set_imputation_missing=0  // sets imputed observations to missing in subset datasets


** RUN MAIN CODE


if $core_if==1 {
 
        local i=1
        display "CORE WAVE `i'"
        cd "$rawdata" 
		use lgtkey ssuid eentaid epppnum eppintvw esex shhadid erace tage eorigin ems renroll /*   
        */ eenrlm eeducate spanel swave wpfinwgt whfnwgt srotaton srefmon rhcalmn rhcalyr gvarstr ghlfsam /*
        */ tfipsst tmetro elkwrk rtakjob rnotake eabre ebuscntr edisabl edisprev eptresn ejobcntr /*
        */ rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rmesr rmwkwjb rmwksab rmwklkg /*
        */ epdjbthn eafnow emoonlit amoonlit tmlmsum amlmsum elayoff ejbhrs1 ejbhrs2 eclwrk1 eclwrk2 eeno1 eeno2 ejbind1 tjbocc1 ejbind2 tjbocc2 /*
        */ eocctim1 eocctim2 rwksperm tpmsum1 tpmsum2 estlemp1 estlemp2 tsjdate1 tsjdate2 tejdate1 tejdate2 /*
        */ ersend1 ersend2 epayhr1 epayhr2 rpyper1 rpyper2 tpyrate1 tpyrate2 /* 
        */ egrosb1 egrosb2 tsbdate1 tsbdate2 tebdate1 tebdate2 egrssb1 egrssb2 tbmsum1 tbmsum2 eincpb1 eincpb2 tbsind1 tbsind2 /*
        */ tempb1 tempb2 tprftb1 tprftb2 tbsocc1 tbsocc2 ehprtb1 ehprtb2 ebiznow1 ebiznow2 epartb11 epartb12 epartb21 epartb22 epartb31 /*
        */ epartb32 erendb1 erendb2 eslryb1 eslryb2 epropb1 epropb2 ehrsbs1 ehrsbs2 ebno1 ebno2 /*
		*/ eeveret aeveret ersnowrk arsnowrk /*
								ADDITIONAL VARIABLES
		*/  ecflag aeducate eppflag ebflag eoutcome ehrsall tpearn tptotinc ecntrc1 ecntrc2 tempsiz1 tempsiz2 eemploc1 eemploc2  /*
		*/  eunion1 eunion2 tempall1 tempall2 /*
        						TRAINING incl. imputation + SEVERANCE PAY
		*/  eedfund aedfund easst* aedasst ewelact1 ewelac21 ewelac22 ewelac23 ewelact3 ewelact4 awelact1 awelac21 awelac22 awelac23 awelact3 awelact4 /*
		*/  ewrkexp1 ewrkexp2 ewrkexp3 awrkexp1 awrkexp2 awrkexp3 ewhiexp1 ewhiexp2 awhiexp1 awhiexp2 t15amt a15amt ar15 er15 /*
                                IMPUTATION FLAGS
        */ aeducate aenrlm asex arace aage aorigin ams arenroll aabre adisabl adisprev aptresn alkwrk awksab awklkg  /*
        */ apdjbthn aafnow alayoff ajbhrs1 ajbhrs2 aclwrk1 aclwrk2 ajbind1 ajbocc1 ajbind2 ajbocc2 /*
        */ aocctim1 aocctim2 apmsum1 apmsum2 astlemp1 astlemp2 asjdate1 asjdate2 aejdate1 aejdate2 /*
        */ arsend1 arsend2 apayhr1 apayhr2 apyrate1 apyrate2 /* 
        */ agrosb1 agrosb2 asbdate1 asbdate2 aebdate1 aebdate2 agrssb1 agrssb2 abmsum1 abmsum2 aincpb1 aincpb2 absind1 absind2 /*
        */ aempb1 aempb2 aprftb1 aprftb2 absocc1 absocc2 ahprtb1 ahprtb2 abiznow1 abiznow2 /*
        */ arendb1 arendb2 aslryb1 aslryb2 apropb1 apropb2 ahrsbs1 ahrsbs2 ahrsall acntrc1 acntrc2 aempsiz1 aempsiz2 aemploc1 aemploc2 aunion1 aunion2 aempall1 aempall2 /*
								DATA SOURCE:
        */ using sipp04w`i', clear
		cd "$tempdata" 

		compress
        *generate id
            
            egen id=concat(ssuid eentaid epppnum)
            gen yearmonth=ym(rhcalyr, rhcalmn)
        
        *across panel data
        gen panel=20${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring ssuid eentaid epppnum)
        
        
                    
            ***** SAVE
      
			
            sort id yearmonth
		    compress
        	save temp20${yeardata}, replace 		
			
            ***** NEXT CORE WAVES 
local i=`i'+1

		while `i'>=2 & `i'<13 {
                    display "CORE WAVE `i'"
            cd "$rawdata" 
        cd "$rawdata" 
		use lgtkey ssuid eentaid epppnum eppintvw esex shhadid erace tage eorigin ems renroll /*   
        */eenrlm eeducate spanel swave wpfinwgt whfnwgt srotaton srefmon rhcalmn rhcalyr gvarstr ghlfsam /*
        */ tfipsst tmetro elkwrk rtakjob rnotake eabre ebuscntr edisabl edisprev eptresn ejobcntr ecflag /*
        */ rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rmesr rmwkwjb rmwksab rmwklkg /*
        */ epdjbthn eafnow emoonlit amoonlit tmlmsum amlmsum elayoff ejbhrs1 ejbhrs2 eclwrk1 eclwrk2 eeno1 eeno2 ejbind1 tjbocc1 ejbind2 tjbocc2 /*
        */ eocctim1  eocctim2 rwksperm tpmsum1 tpmsum2 estlemp1 estlemp2 tsjdate1 tsjdate2 tejdate1 tejdate2 /*
        */ ersend1 ersend2 epayhr1 epayhr2 rpyper1 rpyper2 tpyrate1 tpyrate2 /*
        */ egrosb1 egrosb2 tsbdate1 tsbdate2 tebdate1 tebdate2 egrssb1 egrssb2 tbmsum1 tbmsum2 eincpb1 eincpb2 tbsind1 tbsind2 /*
        */ tempb1 tempb2 tprftb1 tprftb2 tbsocc1 tbsocc2 ehprtb1 ehprtb2 ebiznow1 ebiznow2 epartb11 epartb12 epartb21 epartb22 epartb31 /*
        */ epartb32 erendb1 erendb2 eslryb1 eslryb2 epropb1 epropb2 ehrsbs1 ehrsbs2 ebno1 ebno2 /*
        */ eeveret aeveret ersnowrk arsnowrk /*
			ADDITIONAL VARIABLES, except training variables
		*/  ecflag eppflag ebflag eoutcome ehrsall tpearn tptotinc ecntrc1 ecntrc2 tempsiz1 tempsiz2 eemploc1 eemploc2  /*
		*/  eunion1 eunion2 tempall1 tempall2 /*
        						TRAINING incl. imputation + SEVERANCE PAY
		*/  eedfund aeducate aedfund easst* aedasst ewelact1 ewelac21 ewelac22 ewelac23 ewelact4 awelact1 awelac21 awelac22 awelac23 awelact4 /*
		*/  ewrkexp1 ewrkexp2 ewrkexp3 awrkexp1 awrkexp2 awrkexp3 ewhiexp1 ewhiexp2 awhiexp1 awhiexp2 t15amt a15amt ar15 er15 /*
                                IMPUTATION FLAGS
        */ aeducate aenrlm asex arace aage aorigin ams arenroll aabre adisabl adisprev aptresn alkwrk awksab awklkg  /*
        */ apdjbthn aafnow alayoff ajbhrs1 ajbhrs2 aclwrk1 aclwrk2 ajbind1 ajbocc1 ajbind2 ajbocc2 /*
        */ aocctim1 aocctim2 apmsum1 apmsum2 astlemp1 astlemp2 asjdate1 asjdate2 aejdate1 aejdate2 /*
        */ arsend1 arsend2 apayhr1 apayhr2 apyrate1 apyrate2 /* 
        */ agrosb1 agrosb2 asbdate1 asbdate2 aebdate1 aebdate2 agrssb1 agrssb2 abmsum1 abmsum2 aincpb1 aincpb2 absind1 absind2 /*
        */ aempb1 aempb2 aprftb1 aprftb2 absocc1 absocc2 ahprtb1 ahprtb2 abiznow1 abiznow2 /*
        */ arendb1 arendb2 aslryb1 aslryb2 apropb1 apropb2 ahrsbs1 ahrsbs2 ahrsall acntrc1 acntrc2 aempsiz1 aempsiz2 aemploc1 aemploc2 aunion1 aunion2 aempall1 aempall2 /*
								DATA SOURCE:
		*/ using sipp${yeardata}w`i', clear
		
		compress 
		
		
		cd "$tempdata" 
            *generate id
            
            egen id=concat(ssuid eentaid epppnum)
            gen yearmonth=ym(rhcalyr, rhcalmn)

        *across panel data
        gen panel=20${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring ssuid eentaid epppnum)

        
            ***** APPEND TO DATA SETS BEFORE AND SAVE
            append using temp20${yeardata}
			
            sort id yearmonth
			save temp20${yeardata}, replace
			local i=`i'+1
		} 
		
            sort id yearmonth
            save temp20${yeardata}, replace
	    save temp20${yeardata}core, replace
		
}

********************************************************************************
******** TOPICAL MODULES 1
********************************************************************************


if $tm_if==1 {


***********************************
** TM1: LABOUR MARKET HISTORIES
***********************************

     local i=1       // contains TM1 with tenure data!
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use lgtkey ssuid epppnum eentaid /*
			*/ eoutcome eppintvw eppmis4   /* INTERVIEW INDICATORS 
            */ ewk1bfor awk1bfor twk1lsjb awk1lsjb  tprvjbyr aprvjbyr tlstwrky alstwrky tfrmryr afrmryr  tmakmnyr amakmnyr /*
            */ eno6all1 eno6all2 eno6all3 eno6all4 eno6all5 eno6all6 eno6all7 eno6all8 eno6all9 ano6all eothtime aothtime ecntothr acntothr /*
			*/ emnreson amnreson eanyoff aanyoff ehowmany ahowmany ewrk35hr awrk35hr etimeoff atimeoff eoff6mtn aoff6mtn tfstyrfr afstyrfr tfstyrto afstyrto /*
			CAREGIVING
			*/ tnowrkfr anowrkfr tnowrkto anowrkto enwresn anwresn /*
            */ using sipp04tm`i', clear
         cd "$tempdata" 
            	
            
			 	ren eoutcome eoutcome_tm1 
				ren eppintvw eppintvw_tm1
				ren eppmis4 eppmis4_tm1
            


			
			
            *generate id
            egen id=concat(ssuid eentaid epppnum)
            sort id
            
			gen swave=`i'
			
			*generate Topical Module index
			
			gen byte topmodule=`i'
            
            ***** REDEFINE SOME VARIABLES
            
            
            * end previous job
            
            gen endprevjbyr_tm=.
            replace endprevjbyr_tm=tprvjbyr  if tprvjbyr!=-1
            replace endprevjbyr_tm=tlstwrky if tlstwrky!=0
            lab var endprevjbyr_tm "end year job previous to panel"
            
            * start previous job
            
            gen startprevjbyr_tm=.
            replace startprevjbyr_tm=tfrmryr if tfrmryr!=-1 
            lab var startprevjbyr_tm "start year job previous to panel"
            
            
            *year first worked 6months
            ren tmakmnyr firstyearlf 
            lab var firstyearlf "year first worked at least 6 months"
            
            *always worked at least 6months for every year since
            ren eanyoff alwayswork6m

            recode alwayswork6m (1=2) (2=1)
            lab var alwayswork6m "Always worked at least six months in every year"
            
            
            *generally worked full-time
            ren ewrk35hr lf_ft
            
            *most recent time not worked 6 months from and to
            ren tnowrkfr recentbreak_care6mfr
            ren tnowrkto recentbreak_care6mto
            
            *reason is caregiving
            ren eoff6mtn caregive_nlf
        
            *total amount of time out of work 6 months or so
            ren etimeoff time_nowrk6m        
            
            *no job ever before wave 1 job
            ren ewk1bfor nojobbeforew1
            
                
            ***** APPEND TO DATA SETS BEFORE AND SAVE
   
		*sort id 
		*merge id using temp20${yeardata}, _merge(_mrgtm1) sort uniqmaster
		*sort id yearmonth
		*sort lgtkey
	    save temp20${yeardata}tm, replace

                
********************************************************************************
***** THE BIG MERGE
********************************************************************************


***** APPEND TM DATASET TO CORE DATA SET
	cd "$tempdata"
        capture use temp20${yeardata}tm
	sort id swave
	capture drop _mrgtot
        merge id swave using temp20${yeardata}core, _merge(_mrgtot) sort uniqmaster
        sort id swave yearmonth
	tab _mrgtot            
	save temp20${yeardata}, replace
	display "_mrgtot==1 means that the individual appears in the TM somewhere, but not in core waves; STRANGE!"
	display "_mrgtot==2 means that the individual appears in the core wave dataset somewhere, but did not partake in the TM"
	display "_mrmtot==3 individual in the core wave data set and did partake in some TM"		

            
}        

*******************************************************************************
****** LONGITUDINAL WEIGHTS
********************************************************************************

if $weights_if==1 {

display "----------------weights---------------------------------"

	cd "$rawdata"
use sipp04lgtwgt.dta, clear
    cd "$tempdata"


    ***** APPEND TO DATA SETS BEFORE AND SAVE
        sort lgtkey ssuid epppnum
		capture drop _mrgwgt
        merge lgtkey ssuid epppnum using temp20${yeardata}, _merge(_mrgwgt) sort uniqmaster
        sort id yearmonth
			tab _mrgwgt            
	display " MASTER data set is the weights data set, _mrgwgt==3 is both in core/tm data set and weights; ==2 only in the core/tm data set; ==1 only in the lgtwgt"
	drop if _mrgwgt==1   // drop those with information only in the lgtwgt file (weights are zero for these guys)

}

**************************************************
************** Manipulations *********************

          
        ****** REDEFINE VARIABLES, to be uniform across panels
		**** note that ALLOCATION FLAGS SHOULD ALSO BE RENAMED
        ren ssuid su_id
        ren eentaid pp_entry
        ren epppnum pp_pnum
        *ren fnlwgt wpfinwgt
        format yearmonth %tm
	capture n rename srot rot
	capture n rename srotaton rot
	
	rename swave wave
	ren gvarstr strat
	ren ghlfsam hsc
	ren esex sex
	ren erace race
        
        
	// missing variable redefine
        mvdecode _all, mv(-1)
		mvdecode _all, mv(-2)
        mvdecode _all, mv(-9)
        mvdecode _all, mv(-3)
        
        *schooling data
	gen educ = 1 if eeducate<=38
	replace educ = 2 if eeducate==39
	replace educ = 3 if eeducate>=40 & eeducate<=43
	replace educ = 4 if eeducate==44 
	replace educ = 5 if eeducate>44 & eeducate<=48
	*drop higrad grd_cmp
	lab var educ "Educational Attainment"
	notes educ: SIPP: higrade & grd_cmp (92,93)

            *years in "labor force"
            gen yearslf=rhcalyr-firstyearlf if firstyearlf!=-1
                       
            
	    capture gen yearsnotworked6m=ehowmany 
            capture lab var yearsnotworked6m "Number of years NOT worked at least six months"
            capture replace yearsnotworked6m=. if yearsnotworked6m==-1
            
            capture gen year6mworked=yearslf-yearsnotworked6m

     *** DROPPING THOSE UNDER 15 yrs
	// CAREFUL: DOUBLE CHECK THAT AGES ARE READ IN CORRECTLY; currently some weird age-jumping within the same id
     drop if tage<15


     *** DROPPING NON(PROXY)INTERVIEWED OBSERVATIONS 
     
*     capture drop if pp_mis!=1
     sort id yearmonth 
     
     gen continuous_spell=.
     replace continuous_spell=1 if personkey[_n-1]!=personkey
     replace continuous_spell=1 if personkey[_n-1]==personkey & yearmonth[_n-1]==yearmonth-1 & continuous_spell[_n-1]==1
     replace continuous_spell=0 if personkey[_n-1]==personkey & yearmonth[_n-1]!=yearmonth-1 
     replace continuous_spell=0 if personkey[_n-1]==personkey & continuous_spell[_n-1]!=1 
     
     
cd "$outputdata"

*save 2004total_raw, replace
save 2004total_v${fileversion}, replace

