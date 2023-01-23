
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
** STEP 0.1.1 CTV DATA CONSTRUCTION, 2001 PANEL  
********************************************************************************

* helpful input from Carl Singleton gratefully acknowledged  

version 13
clear
*capture log close
*set mem 3000m
set more off
set maxvar 10000


** GLOBALS
global yeardata "01"
global panel "2001"
global gnulin=0
* control parts of the do-file
global core_if=1	// read in the core waves
global tm_if=1	       // read in the topical modules
global weights_if=1   // loads in the longitudinal weights
global subset_if=1   // starts reducing datasets into subsets with relevant variables for different projects
global locfileversion=0816



global set_imputation_missing=0  // sets imputed observations to missing in subset datasets

** RUN MAIN CODE

		


**** LONGITUDINALLY EDITED WAVES 2001 in wave format 

/* Relative to 2008 - variables with name changes or not present (incl. checking for equivalents):	
ehrsall = .
Labour Market history:
e6* = .
Training variables:
ewelac* = .
ewrkexp* = .
ewhiexp* = .*/

*Note - check any categorical variables - across years relative to 2008



if $core_if==1 {
 
        local i=1
        display "CORE WAVE `i'"
        cd "$rawdata" 
				use lgtkey ssuid eentaid epppnum eppintvw esex shhadid erace tage eorigin ems renroll /*   
        */eenrlm eeducate spanel swave wpfinwgt whfnwgt srotaton srefmon rhcalmn rhcalyr gvarstr ghlfsam /*
		*/ tmetro tmsa tfipsst elkwrk rtakjob rnotake eabre ebuscntr emoonlit edisabl edisprev eptresn ejobcntr /*
        */ rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rmesr rmwkwjb rmwksab rmwklkg /*
        */ epdjbthn eafnow elayoff ejbhrs1 ejbhrs2 eclwrk1 eclwrk2 eeno1 eeno2 ejbind1 tjbocc1 ejbind2 tjbocc2 /*
        */ eocctim1 eocctim2 rwksperm tpmsum1 tpmsum2 estlemp1 estlemp2 tsjdate1 tsjdate2 tejdate1 tejdate2 /*
        */ ersend1 ersend2 epayhr1 epayhr2 rpyper1 rpyper2 tpyrate1 tpyrate2 /* 
		*/ eeveret aeveret ersnowrk arsnowrk /*
        */ egrosb1 egrosb2 tsbdate1 tsbdate2 tebdate1 tebdate2 egrssb1 egrssb2 tbmsum1 tbmsum2 eincpb1 eincpb2 tbsind1 tbsind2 /*
        */ tempb1 tempb2 tprftb1 tprftb2 tbsocc1 tbsocc2 ehprtb1 ehprtb2 ebiznow1 ebiznow2 epartb11 epartb12 epartb21 epartb22 epartb31 /*
        */ epartb32 erendb1 erendb2 eslryb1 eslryb2 epropb1 epropb2 ehrsbs1 ehrsbs2 ebno1 ebno2 /*
						Additional Variables
	*/ ecflag eppflag ebflag eoutcome tpearn tptotinc ecntrc1 ecntrc2 tempsiz1 tempsiz2 tempall1 tempall2 eemploc1 eemploc2/*
	*/ eunion1 eunion2 /*
						TRAINING incl. imputation + SEVERANCE PAY
	*/ eedfund easst* t15amt er15 /*					
						IMPUTATION FLAGS (just load all in...)
	*/ arace asex aage aorigin ams arenroll /*
	*/ aenrlm aeducate /*
	*/ alkwrk aabre amoonlit adisabl adisprev aptresn /*
	*/ awklkg awksab /*
	*/ apdjbthn aafnow alayoff ajbhrs1 ajbhrs2 aclwrk1 aclwrk2 ajbind1 ajbocc1 ajbind2 ajbocc2 /*
	*/ aocctim1 aocctim2 apmsum1 apmsum2 astlemp1 astlemp2 asjdate1 asjdate2 aejdate1 aejdate2 /*
	*/ arsend1 arsend2 apayhr1 apayhr2 apyrate1 apyrate2 /*
	*/ agrosb1 agrosb2 asbdate1 asbdate2 tebdate1 aebdate1 aebdate2 agrssb1 agrssb2 abmsum1 abmsum2 aincpb1 aincpb2 absind1 absind2 /*
	*/ aempb1 aempb2 aprftb1 aprftb2 absocc1 absocc2 ahprtb1 ahprtb2 abiznow1 abiznow2 /* 
	*/ arendb1 arendb2 aslryb1 aslryb2 apropb1 apropb2 ahrsbs1 ahrsbs2 acntrc1 acntrc2 aempsiz1 aempsiz2 aempall1 aempall2 aemploc1 aemploc2/*
	*/ aunion1 aunion2 /*
	*/ using sip${yeardata}w`i', clear
		cd "$tempdata" 
		
		/*
		use lgtkey ssuid eentaid epppnum eppintvw esex shhadid erace tage eorigin ems renroll /*   
        */eenrlm eeducate spanel wave wpfinwgt whfnwgt rot srefmon rhcalmn rhcalyr gvarstr ghlfsam /*
        */ tmetro tmsa tfipsst elkwrk rtak rnotake eabre ebuscntr emoonlit edisabl edisprev eptresn ejobcntr /*
        */ rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rmesr rmwkwjb rmwksab rmwklkg /*
        */ epdjbthn eafnow elayoff ejbhrs1 ejbhrs2 eclwrk1 eclwrk2 eeno1 eeno2 ejbind1 tjbocc1 ejbind2 tjbocc2 /*
        */ eocctim1 eocctim2 rwksperm tpmsum1 tpmsum2 estlemp1 estlemp2 tsjdate1 tsjdate2 tejdate1 tejdate2 /*
        */ ersend1 ersend2 epayhr1 epayhr2 rpyper1 rpyper2 tpyrate1 tpyrate2 ebno1 /* 
        */ egrosb1 egrosb2 tsbdate1 tsbdate2 tebdate1 tebdate2 egrssb1 egrssb2 tbmsum1 tbmsum2 eincpb1 eincpb2 tbsind1 tbsind2 /*
        */ tempb1 tempb2 tprftb1 tprftb2 tbsocc1 tbsocc2 ehprtb1 ehprtb2 ebiznow1 ebiznow2 epartb11 epartb12 epartb21 epartb22 epartb31 /*
        */ epartb32 erendb1 erendb2 eslryb1 eslryb2 epropb1 epropb2 ehrsbs1 ehrsbs2 ebno1 ebno2 /*
						Additional Variables
	*/ ecflag eppflag ebflag eoutcome tpearn tptotinc ecntrc1 ecntrc2 tempsiz1 tempsiz2 tempall1 tempall2 eemploc1 eemploc2/*
	*/ eunion1 eunion2 /*
						TRAINING incl. imputation + SEVERANCE PAY
	*/ eedfund easst* t15amt er15 /*					
						IMPUTATION FLAGS (just load all in...)
	*/ a* /*
	*/
	*/ 
		compress
            *generate id
            
            egen id=concat(ssuid eentaid epppnum)
            gen yearmonth=ym(rhcalyr, rhcalmn)
        
        *across panel data
        gen panel=20${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring ssuid eentaid epppnum)

        gen wave=1   
        ren srotaton rot       
            ***** SAVE
      
			
        sort id yearmonth
		compress
		save temp20${yeardata}, replace 		

            ***** NEXT CORE WAVES 
local i=`i'+1

		while `i'>=2 & `i'<10 {
                    display "CORE WAVE `i'"
            cd "$rawdata" 
        cd "$rawdata" 
		use lgtkey ssuid eentaid epppnum eppintvw esex shhadid erace tage eorigin ems renroll /*   
        */ eenrlm eeducate spanel swave wpfinwgt whfnwgt srotaton srefmon rhcalmn rhcalyr gvarstr ghlfsam /*
		*/ tmetro tmsa tfipsst elkwrk rtakjob rnotake eabre ebuscntr emoonlit edisabl edisprev eptresn ejobcntr /*
        */ rwkesr1 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rmesr rmwkwjb rmwksab rmwklkg /*
		*/ eeveret aeveret ersnowrk arsnowrk /*
        */ epdjbthn eafnow elayoff ejbhrs1 ejbhrs2 eclwrk1 eclwrk2 eeno1 eeno2 ejbind1 tjbocc1 ejbind2 tjbocc2 /*
        */ eocctim1 eocctim2 rwksperm tpmsum1 tpmsum2 estlemp1 estlemp2 tsjdate1 tsjdate2 tejdate1 tejdate2 /*
        */ ersend1 ersend2 epayhr1 epayhr2 rpyper1 rpyper2 tpyrate1 tpyrate2 ebno1 /* 
        */ egrosb1 egrosb2 tsbdate1 tsbdate2 tebdate1 tebdate2 egrssb1 egrssb2 tbmsum1 tbmsum2 eincpb1 eincpb2 tbsind1 tbsind2 /*
        */ tempb1 tempb2 tprftb1 tprftb2 tbsocc1 tbsocc2 ehprtb1 ehprtb2 ebiznow1 ebiznow2 epartb11 epartb12 epartb21 epartb22 epartb31 /*
        */ epartb32 erendb1 erendb2 eslryb1 eslryb2 epropb1 epropb2 ehrsbs1 ehrsbs2 ebno1 ebno2 /*
						Additional Variables
	*/ ecflag eppflag ebflag eoutcome tpearn tptotinc ecntrc1 ecntrc2 tempsiz1 tempsiz2 tempall1 tempall2 eemploc1 eemploc2/*
	*/ eunion1 eunion2 /*
						TRAINING incl. imputation + SEVERANCE PAY
	*/ eedfund easst* t15amt er15 /*					
						IMPUTATION FLAGS (just load all in...)
		*/ arace asex aage aorigin ams arenroll /*
	*/ aenrlm aeducate /*
	*/ alkwrk aabre amoonlit adisabl adisprev aptresn /*
	*/ awklkg awksab /*
	*/ apdjbthn aafnow alayoff ajbhrs1 ajbhrs2 aclwrk1 aclwrk2 ajbind1 ajbocc1 ajbind2 ajbocc2 /*
	*/ aocctim1 aocctim2 apmsum1 apmsum2 astlemp1 astlemp2 asjdate1 asjdate2 aejdate1 aejdate2 /*
	*/ arsend1 arsend2 apayhr1 apayhr2 apyrate1 apyrate2 /*
	*/ agrosb1 agrosb2 asbdate1 asbdate2 tebdate1 aebdate1 aebdate2 agrssb1 agrssb2 abmsum1 abmsum2 aincpb1 aincpb2 absind1 absind2 /*
	*/ aempb1 aempb2 aprftb1 aprftb2 absocc1 absocc2 ahprtb1 ahprtb2 abiznow1 abiznow2 /* 
	*/ arendb1 arendb2 aslryb1 aslryb2 apropb1 apropb2 ahrsbs1 ahrsbs2 acntrc1 acntrc2 aempsiz1 aempsiz2 aempall1 aempall2 aemploc1 aemploc2/*
	*/ aunion1 aunion2 /*
        */ using sip${yeardata}w`i', clear
		cd "$tempdata" 
		
		compress
            *generate id
            
            egen id=concat(ssuid eentaid epppnum)
            gen yearmonth=ym(rhcalyr, rhcalmn)

        *across panel data
        gen panel=20${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring ssuid eentaid epppnum)

        ren srotaton rot 
        gen wave=`i'
            
            ***** APPEND TO DATA SETS BEFORE AND SAVE
            append using temp20${yeardata}
			
            sort id yearmonth
			save temp20${yeardata}, replace
			local i=`i'+1
		} 
	
            
            ****** REDEFINE VARIABLES - done later
            *ren ssuid su_id
            *ren eentaid pp_entry
            *ren epppnum pp_pnum
            *ren fnlwgt wpfinwgt
            
            
*** save it 
            *use temp20${yeardata}, clear 
            sort id yearmonth
            save temp20${yeardata}, replace
	    save temp20${yeardata}core, replace

}


if $tm_if==1 {


***********************************
** TM1: LABOUR MARKET HISTORIES
***********************************

*** Note: Check categories relative to 2008 - see 2008_construction.do

local i=1       // contains TM1 with tenure data!
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use lgtkey ssuid epppnum eentaid /*
			*/  eoutcome eppintvw eppmis4 /*
			*/ eprvjbmn aprvjbmn tprvjbyr aprvjbyr elstwrkm alstwrkm tlstwrky alstwrky efrmrmn afrmrmn tfrmryr afrmryr tmakmnyr amakmnyr/*
			*/ emnreson amnreson eyrsince ayrsince eyrsinc2 ayrsinc2 ewrk35hr awrk35hr /*
			*/ eoff6mtn aoff6mtn tnowrkfr anowrkfr tnowrkto anowrkto eoff6mtn aoff6mtn ewrk35hr awrk35hr /*
			*/ eothtime aothtime ecntothr acntothr tnowrkfr anowrkfr tnowrkto anowrkto tfstyrfr afstyrfr tfstyrto afstyrto enwresn anwresn /*
            */ using sip${yeardata}t`i', clear
         cd "$tempdata" 

            	
            	ren eoutcome eoutcome_tm1 
				ren eppintvw eppintvw_tm1
				ren eppmis4 eppmis4_tm1
            

            
            *generate id
            egen id=concat(ssuid eentaid epppnum)
            gen wave=1
            sort id wave
            
            
            ***** REDEFINE SOME VARIABLES
            
            
            * end previous job
            gen endprevjb_tm=.
			count if tprvjbyr!=-1 & tlstwrky!=-1
            replace endprevjb_tm=ym(tprvjbyr,eprvjbmn) if (tprvjbyr!=-1 & eprvjbmn!=-1)
            replace endprevjb_tm=ym(tlstwrky,elstwrkm) if (tlstwrky!=-1  & elstwrkm!=-1)
            lab var endprevjb_tm "end date job previous to panel"
            
            gen endprevjbyr_tm=.
            replace endprevjbyr_tm=tprvjbyr  if tprvjbyr!=-1
            replace endprevjbyr_tm=tlstwrky if tlstwrky!=0
            lab var endprevjbyr_tm "end year job previous to panel"
            
            * start previous job
            capture gen startprevjb_tm=.
            replace startprevjb_tm=ym(tfrmryr, efrmrmn) if (tfrmryr!=-1 & efrmrmn!=-1)

            gen startprevjbyr_tm=.
            replace startprevjbyr_tm=tfrmryr if tfrmryr!=-1
            
            lab var startprevjb_tm "start date job previous to panel"
            
            
            
            *year first worked 6months
            ren tmakmnyr firstyearlf 
            lab var firstyearlf "year first worked at least 6 months"
            
            *always worked at least 6months for every year since
            ren eyrsince alwayswork6m
            lab var alwayswork6m "Always worked at least six months in every year"
            
            
            *generally worked full-time
            ren ewrk35hr lf_ft
            
            *most recent time not worked 6 months from and to
            ren tnowrkfr recentbreak_care6mfr
            ren tnowrkto recentbreak_care6mto
            
            *reason is caregiving
            ren eoff6mtn caregive_nlf
        
        
            
                
            ***** APPEND TO DATA SETS BEFORE AND SAVE
        *sort id wave
        *merge id wave using temp20${yeardata}, _merge(_mrgtm1) sort uniqmaster
            
		*sort id yearmonth
            *sort lgtkey
		*tab _mrgtm1
		save temp20${yeardata}tm, replace
		compress
		
		
			
********************************************************************************
***** THE BIG MERGE
********************************************************************************


***** APPEND TM DATASET TO CORE DATA SET
	cd "$tempdata"
        capture use temp20${yeardata}tm
	sort id wave
	capture drop _mrgtot
        merge id wave using temp20${yeardata}core, _merge(_mrgtot) sort uniqmaster
        sort id wave yearmonth
	tab _mrgtot            
	save temp20${yeardata}, replace
	display "_mrgtot==1 means that the individual appears in the TM somewhere, but not in core waves; STRANGE!"
	display "_mrgtot==2 means that the individual appears in the core wave dataset somewhere, but did not partake in the TM"
	display "_mrmtot==3 individual in the core wave data set and did partake in some TM"
	compress
    
} 

********************************************************************************
****** LONGITUDINAL WEIGHTS
********************************************************************************

if $weights_if==1 {

display "----------------weights---------------------------------"

    cd "$rawdata"
use sip01lw9.dta, clear
    cd "$tempdata"


    ***** APPEND TO DATA SETS BEFORE AND SAVE
        sort lgtkey ssuid epppnum
		capture drop _mrgwgt
        merge lgtkey ssuid epppnum using temp20${yeardata}, _merge(_mrgwgt) sort uniqmaster
        sort id yearmonth
			tab _mrgwgt            
	display " MASTER data set is the weights data set, _mrgwgt==3 is both in core/tm data set and weights; ==2 only in the core/tm data set; ==1 only in the lgtwgt"
	drop if _mrgwgt==1

}

cd "$outputdata"

save 20${yeardata}total_raw, replace	     

********************************************************************************
****** MANIPULATIONS and SUBSET SELECTION
********************************************************************************


**************************************************
************** Manipulations *********************
   
            ****** REDEFINE VARIABLES, to be uniform across panels
		**** note that ALLOCATION FLAGS SHOULD ALSO BE RENAMED
        ren ssuid su_id
        ren eentaid pp_entry
        ren epppnum pp_pnum
        *ren fnlwgt wpfinwgt
        format yearmonth %tm
	ren gvarstr strat
	ren ghlfsam hsc
	ren esex sex
	ren erace race
	
	// missing variable redefine
        mvdecode _all, mv(-1)
		mvdecode _all, mv(-2)
        mvdecode _all, mv(-9)
        mvdecode _all, mv(-3)
		
	**** Additional variables redefined to be consistent with 2008
	
	*recode eprogram (2=4) (3=2) (4=3)
	*recode rpcnthh2 (2=1)

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
                       
            
	    capture ren ehowmany yearsnotworked6m
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

tab swave wave, m
drop swave          

*save 20${yeardata}total_raw, replace	   
save 20${yeardata}total_v${fileversion}.dta, replace 
*saveold 20${yeardata}total_v${fileversion}_2013.dta, replace 

