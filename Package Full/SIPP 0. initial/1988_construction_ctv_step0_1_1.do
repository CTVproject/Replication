
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
** STEP 0.1.1 CTV DATA CONSTRUCTION, 1988 PANEL  
********************************************************************************

* helpful input from Carl Singleton gratefully acknowledged  


   ************** NOTES 
  /*
  - full panel industry and occupation are edited and some entries are IMPUTED, without imputation flags! use corewave variables
  - full panel firm numbers are prefixed by fp_ (fp_eeno1, fp_eeno2)
  - care has to be taken that the full panel information associated with fp eeno label X does not get mixed with a different core-wave eeno with the same X 
	- therefore all full panel variables have their prefix fp_ restored. Some core-wave variables still keep their prefix c_ for extra emphasis, for now.
  - it can be that this is much less an issue for self-employment, but it is still good to compare fp SE vars with corresponding core SE var
  - imputation flags are there for occ/ind, but not all imputation flags are loaded into this data. Good to keep an eye on it.	
  - make sure tm2 also is appended to wave 1 data
  - (deactivate the ws12003 to estlemp conversion, just copy the variable, and the recoding / deactivated the njobs conversion, instead just create jobcntr)
  - also load change-of-activity variable WS12004, WS22104
  - WS12003/ESTLEMP1 in the extraction do-file, gets the wrong variable label it seems, so no recode of answers is necessary
  - refmth/srefmon fixed I think
  */


clear
*capture log close
*set mem 3000m
set more off, permanently
clear all
*cccc
set maxvar 20000


** GLOBALS
global yeardata "88"
* control parts of the do-file
global fp_if=1 // read full panel info
global core_if=1	// read inal data from additioncore waves
global tm_if=1	       // read in the topical modules
global weights_if=1   // loads in the longitudinal weights
global subset_if=0   // starts reducing datasets into subsets with relevant variables for different projects
global list=0
global locfileversion=1022  // AUG 2016
global list=0
global fp_ind=1		// full panel data extracted








*cccc
cd "${codedir}"
do "${codedir}/1984_88_corewave_reshape_ctv_step0_1_1.do"
cd "$rawdata"



** RUN MAIN CODE

if $fp_if==1 {
		* Longitudinal data: create three files 1) by id; 2) by id wave; 3) by id wave month
		
        ****
        * id
        ****
        
	
	
		cd "$rawdata"
		use rot pp_id pp_entry pp_pnum pnlwgt fnlwgt88 fnlwgt89 /*
		*/ varstrat halfsamp sc1360 disab sex race ethnicty using sip${yeardata}fp, clear
		cd "$tempdata"
        capture drop id
        
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
        ren halfsamp hsc
        
        
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
        
        drop if wave>6
        

		save testw, replace	
		
		
        ****
		* id month
        ****
        
		cd "$rawdata"
		use rot pp_id pp_entry pp_pnum ms* pp_mis* age* hh_add* ms_* clswk1* clswk2* jobid1* jobid2* /*
        */ wshrs1* wshrs2* ind1_*   ind2_*  esr_* wksper* occ1_* occ2_* ernam1* ernam2* hrrat1* hrrat2* /*
        */ se_wb1* se_wb2* se_hr1* se_hr2* wksem1* wksem2* mthjbw* mthwop* mthwks* enrl_m* /*
        */  busid1* busid2* se_am1* se_am2* se_oc1* se_oc2* seind1* seind2* /*
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
        rename se_hr1 fp_ehrsbs1
        rename se_hr2 fp_ehrsbs2
        rename se_wb1 fp_se1_wk
        rename se_wb2 fp_se2_wk
        rename enrl_m fp_eenrlm
        rename wksem1 fp_ws1wk
        rename wksem2 fp_ws2wk
        rename mthjbw fp_wksjb_
        rename mthwks fp_weeksl
*        rename clswk1 eclwrk1
*        rename clswk2 eclwrk2
*        rename jobid1 fp_eeno1
*        rename jobid2 fp_eeno2
*        rename wshrs1 ejbhrs1
*        rename wshrs2 ejbhrs2
	
*        rename ind1_ ejbind1i
*        rename ind2_ ejbind2i
*	lab var ejbind1i "Edited and Imputed Industry of employer 1"
*        lab var ejbind2i "Edited and Imputed Industry of employer 22"
        
*	rename esr_  rmesr
*        rename wksper rwksperm
	
*        rename occ1_ tjbocc1i
*        rename occ2_ tjbocc2i
*        lab var tjbocc1i "Edited and Imputed Occupation at employer 1"
*	lab var tjbocc2i "Edited and Imputed Occupation at employer 2"
	
*	rename ernam1 tpmsum1
*        rename ernam2 tpmsum2
*        rename hrrat1 tpyrate1
*        rename hrrat2 tpyrate2
*        rename enrl_m eenrlm
*        rename wksem1 ws1wk
*        rename wksem2 ws2wk
*        rename mthjbw wksjb_
*        rename mthwks weeksl
        
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
        replace yearmonth=tm(1987m12)+interview if rot==1
        replace yearmonth=tm(1987m9)+interview if rot==2
        replace yearmonth=tm(1987m10)+interview if rot==3
        replace yearmonth=tm(1987m11)+interview if rot==4
        
        gen year=year(dofm(yearmonth))
		gen month=month(dofm(yearmonth))

        gen wave=ceil(interview/4)
        drop if wave>6
        
		sort id yearmonth
		save testm, replace 
	
}



	
**variables couldn't find in 88 but present in 90
** njobs 


if $core_if==1 {

       local i=1
        display "CORE WAVE `i'"
        cd "$rawdata" 
		use su_id su_rot pp_pnum pp_intvw pp_entry pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year /*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1042 sc1044 sc1216 sc1218 sc1360 sc1174 sc1714 sc1238 sc1460 sc1098 sc1656 sc1658 sc1660 sc1662 sc1664 sc1666 sc1218 /*
		*/ wksjb* wkwjob* esr_* wiswop* wislok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws2_2102 ws1_2003 ws1_2004 ws2_2103 ws1_2014 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2023 ws1_2024 ws1_2025 ws1_2026 ws1_2028 ws1_2029 /*
		*/ ws2_2122 ws2_2123 ws2_2124 ws2_2104 ws2_2125 ws2_2114 ws2_2126 ws2_2128 ws1_2012 ws2_2112 ws2_2116 ws2_2118 ws2_2120 ws2_2129/*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se1_2214 se2_2314  se1_2260 se2_2360 se1_amt* se2_amt* se1_2220 se2_2320 se1_indr se2_indr se1_2218 se2_2318/*
		*/ se1_2256 se2_2356 se1_occ se2_occ se1_2224 se2_2324 se1_2222 se2_2322 se1_2226 se1_2228 se1_2230 se2_2326 se2_2328 se2_2330 /*
		*/ se1_2234 se2_2334 se1_2232 se2_2332 se1_2212 se2_2312 se1_2201 se2_2301 se1_2202 se2_2302 se1_wks* se2_wks* se1_2203 se2_2303 se1_2252 se2_2352 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ h1_int1 h2_int1 h3_int1 h4_int1 sc1230 ws1_2046 ws2_2146 ws1_2044 ws2_2144 pp_earn* pptotin* /*
		*** Training
		*/ sc1670 sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 /*
		*/ sc1716 sc1002 /*
		*** IMPUTATIONS FLAGS *****
		*/ pi39  pi16 pi03 pi35 pi26 /* idisab ireasab itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi02 pi20 pi14 pi76 pi19 pi04 pi20 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		*/ using sip${yeardata}w`i', clear
		
		cd "$tempdata" 
            *generate id
            egen id=concat(su_id pp_entry pp_pnum)
	    
	    ren ws1imp01 iws1occ
	    ren ws1imp02 iws1ind
	    ren ws1imp03 iws12012
	    ren ws1imp04 iws12014
	    ren se1imp01 ise1occ
	    ren se1imp02 ise1ind 
	    ren se1imp11 ise12260 
		ren pi03 itakjob 	// pi3: imputation sc1042
	    replace itakjob=pi19 if itakjob==0 | itakjob==. // sc1216
		ren pi04 itakjobn	//sc1044
		replace itakjob=pi20 if itakjobn==0 | itakjobn==. // sc1218
		
	    ren pi02 iwkslok 	// pi2: imputation sc1004-sc1040 
	    *ren pi03 itakjob 	// pi3: imputation sc1042
	    ren pi39 idisab   	// pi39
	    ren pi13 iwksjob  	// pi13: imputation sc1100-1134 weeks with job
		ren pi14 iwkswop 	// sc1136
	    ren pi16 ireasab 	// sc1172
	    ren pi26 iwksptr 	// sc1232
	    ren pi35 iretir 		// sc1360 (ever retired?)
	    *ren pi20 itakjobn  	// sc1218
	    ren ws2imp01 iws2occ
	    ren ws2imp02 iws2ind
	    ren ws2imp03 iws22112
	    ren ws2imp04 iws22124
	    ren se2imp01 ise2occ  
	    ren se2imp02 ise2ind
	    ren se2imp11 ise22360 
		
		ren pp_intvw intvw 
		*ren sc1218 rnotake
	   
	   // NOTE NO SALARY IMPUTATION INDICATORS YET
	    
	*Splitting 18 weeks of data over 4 months
	*creates rwkesr
	*does not do any other renaming or reshaping
	coremanip 
	
	reshape long  h@_strat h@_hsc /*
		*/ fnlwgt_ h@_wgt /*
		*/ h@_month h@_year /*
		*/ h@_metro h@_msa h@_state/*
		*/ wksjb esr_ wiswop ws1wksx_ ws2wksx_ /*
		*/ rwkspermx_ rwkesr1x_ rwkesr2x_ rwkesr3x_ rwkesr4x_ rwkesr5x_ ws1_amt ws2_amt/*
		*/ se1_amt se2_amt/*
		*/ se1_wks se2_wks /*	
		*/ h@_int1 pp_earn pptotin/*
		*/ ,i(id) j(refmth) string
	rename h_month month
	gen float year=h_year+1900
	
	gen yearmonth=ym(year, month)
            
            ***** SAVE
      
        *across panel data
        gen panel=19${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring su_id pp_entry pp_pnum)
                    
            ***** SAVE
      
			
        sort id yearmonth
		compress
		save temp19${yeardata}, replace 		
 	
	
	local i=`i'+1

	while `i'>=2 & `i'<7 {
           display "CORE WAVE `i'"
        cd "$rawdata" 	
		use su_id su_rot pp_pnum pp_entry pp_intvw pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year/*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1042 sc1044 sc1216 sc1218 sc1360 sc1044 sc1042 sc1174 sc1714 sc1238 sc1460 sc1098 sc1660 sc1662 sc1664 sc1666 sc1218 /*
		*/ sc1042 sc1044 sc1216 sc1218 sc1360 sc1174 sc1714 sc1238 sc1460 sc1098 sc1656 sc1658 sc1660 sc1662 sc1664 sc1666 sc1218 /*
		*/ wksjb* wkwjob* esr_* wiswop* wislok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws2_2102 ws1_2003 ws1_2004 ws2_2103 ws1_2014 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2023 ws1_2024 ws1_2025 ws1_2026 ws1_2028 ws1_2029/*
		*/ ws2_2122 ws2_2123 ws2_2124 ws2_2104 ws2_2125 ws2_2114 ws2_2126 ws2_2128 ws1_2012 ws2_2112 ws2_2116 ws2_2118 ws2_2120 ws2_2129/*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
		*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se1_2214 se2_2314  se1_2260 se2_2360 se1_amt* se2_amt* se1_2220 se2_2320 se1_indr se2_indr se1_2218 se2_2318/*
		*/ se1_2256 se2_2356 se1_occ se2_occ se1_2224 se2_2324 se1_2222 se2_2322 se1_2226 se1_2228 se1_2230 se2_2326 se2_2328 se2_2330 /*
		*/ se1_2234 se2_2334 se1_2232 se2_2332 se1_2212 se2_2312 se1_2201 se2_2301 se1_2202 se2_2302 se1_wks* se2_wks* se1_2203 se2_2303 se1_2252 se2_2352 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ h1_int1 h2_int1 h3_int1 h4_int1 sc1230 ws1_2046 ws2_2146 ws1_2044 ws2_2144 pp_earn* pptotin*/*
		*** Training
		*/ sc1670 sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 /*
		*** IMPUTATIONS FLAGS *****
		*/ pi39  pi16 pi03 pi35 pi26 /* idisab ireasab itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi02 pi20 pi14 pi76 pi19 pi04 pi20 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		*** using file ***********
		*/ using sip${yeardata}rt`i', clear
		
		cd "$tempdata" 
            *generate id
            egen id=concat(su_id pp_entry pp_pnum)
			
			
			
	    ren ws1imp01 iws1occ
	    ren ws1imp02 iws1ind
	    ren ws1imp03 iws12012
	    ren ws1imp04 iws12024
	    ren se1imp01 ise1occ
	    ren se1imp02 ise1ind 
	    ren se1imp11 ise12260 
	    ren pi02 iwkslok 	// pi2: imputation sc1004-sc1040 
	    ren pi03 itakjob 	// pi3: imputation sc1042
	    replace itakjob=pi19 if itakjob==0 | itakjob==. // sc1216
		ren pi04 itakjobn	//sc1044
		replace itakjob=pi20 if itakjobn==0 | itakjobn==. // sc1218
		ren pi39 idisab   	// pi39
	    ren pi13 iwksjob  	// pi13: imputation sc1100-1134 weeks with job
		ren pi14 iwkswop 	// sc1136
	    ren pi16 ireasab 	// sc1172
	    ren pi26 iwksptr 	// sc1232
	    ren pi35 iretir 		// sc1360 (ever retired?)
	    gen byte ienrolm=pi76 
		
	    ren ws2imp01 iws2occ
	    ren ws2imp02 iws2ind
	    ren ws2imp03 iws22112
	    ren ws2imp04 iws22124
	    ren se2imp01 ise2occ  
	    ren se2imp02 ise2ind
	    ren se2imp11 ise22360 
	   
	   ren pp_intvw intvw
	   *ren sc1218 rnotake
	    
	*Splitting 18 weeks of data over 4 months
	*creates rwkesr
	*does not do any other renaming or reshaping
	coremanip 
	
	reshape long  h@_strat h@_hsc /*
		*/ fnlwgt_ h@_wgt /*
		*/ h@_month h@_year /*
		*/ h@_metro h@_msa h@_state/*
		*/ wksjb esr_ wiswop ws1wksx_ ws2wksx_ /*
		*/ rwkspermx_ rwkesr1x_ rwkesr2x_ rwkesr3x_ rwkesr4x_ rwkesr5x_ ws1_amt ws2_amt/*
		*/ se1_amt se2_amt/*
		*/ se1_wks se2_wks /*	
		*/ h@_int1 pp_earn pptotin/*
		*/ ,i(id) j(refmth) string
	rename h_month month
	gen float year=h_year+1900
	
	gen yearmonth=ym(year, month)
            
            ***** SAVE
      
        *across panel data
        gen panel=19${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring su_id pp_entry pp_pnum)
	
	            ***** APPEND TO DATA SETS BEFORE AND SAVE
            append using temp19${yeardata}
			
            sort id yearmonth
			compress
			save temp19${yeardata}, replace
			local i=`i'+1
		} 
	
            
            ****** REDEFINE VARIABLES - done later
            *ren ssuid su_id
            *ren eentaid pp_entry
            *ren epppnum pp_pnum
            *ren fnlwgt wpfinwgt
            
            
*** save it 
            *use temp19${yeardata}, clear 
            sort id yearmonth
	    compress
            save temp19${yeardata}, replace
	    save temp19${yeardata}core, replace



*** Some renaming, consistent with 2008+
	    ren pp_wave wave
            ren fnlwgt_ wpfinwgt
            ren h_strat c_strat
            ren h_hsc c_hsc
            ren h_wgt whfnwgt
            ren month rhcalmn
            ren year rhcalyr
	    drop h_year
            destring refmth, gen(srefmon)
            ren h_metro tmetro
            ren h_msa tmsa
            ren h_state tfipsst
	    * Cats do not match to 2008
	    ren ws1_occ c_tjbocc1
            ren ws2_occ c_tjbocc2
	    * Cats do not match to 2008
	    ren ws1_ind c_ejbind1
            ren ws2_ind c_ejbind2

            ren sc1042 rtakjob
			replace rtakjob=sc1216 if rtakjob==0 | rtakjob==.
            ren sc1044 rnotake
			replace rnotake=sc1218 if rnotake==0 | rnotake==.
			
			gen coreenrolm=sc1656
			gen corenrolmth=1 if sc1658==1 | sc1660==1 | sc1662==1 | sc1664==1 | sc1666==1
			ren pi76 icorenrolm
			ren sc1098 eabre 
            ren sc1238 eptresn
            ren sc1460 edisab
            
            
            ren rwkesr1x_ rwkesr1
            ren rwkesr2x_ rwkesr2
            ren rwkesr3x_ rwkesr3
            ren rwkesr4x_ rwkesr4
            ren rwkesr5x_ rwkesr5
            ren rwkspermx_ c_rwksperm
            ren esr_ c_rmesr
            ren wksjb c_rmwkwjb
	    
	    drop job_* abs_* look_*
            
                   
            ren ws1_2002 eeno1
            ren ws2_2102 eeno2

            *estlemp            
            gen estlemp1=ws1_2003 
            gen estlemp2=ws2_2103 
            capture lab var estlemp1 "Still working for employer1 (at time of interview)"
            *recode estlemp1 (1=2) (2=1)
            capture lab var estlemp2 "Still working for employer2 (at time of interview)"
            *recode estlemp2 (1=2) (2=1)
            
            *eclwrk
            ren ws1_2012 c_eclwrk1
            ren ws2_2112 c_eclwrk2

            capture label define c_eclwrkl 1 "private" 2 "private (merge with 1) not for profit" ///
            3 "local gov" 4 "state gov" 5 "fed gov" 6 "unpaid"
            recode c_eclwrk* (6=.) (5=3) (3=5) (7=6)
            label value c_eclwrk* c_eclwrkl
            
            *date of job ending and job beginning
            

        ** STOPPED WORKING on this job
        ren ws1_2023 stopjob1
        ren ws2_2123 stopjob2
        
        ** REASON STOPPED WORKING
        rename     ws1_2024   ersend1   //1f "What is the main reason ... stopped"
        rename     ws2_2124   ersend2

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
ren ws1_2025 c_ejbhrs1
ren ws2_2125 c_ejbhrs2

ren ws1_2026 c_epayhr1
 ren ws2_2126 c_epayhr2
 
ren ws1_2028 c_tpyrate1
ren ws2_2128 c_tpyrate2

ren ws1_2029 c_rpyper1
ren ws2_2129 c_rpyper2

ren ws1_amt c_tpmsum1
ren ws2_amt c_tpmsum2

replace yearmonth=ym(rhcalyr, rhcalmn)

*** selfemployed renaming and missing value decoding


mvdecode se*, mv(0)

ren se1_2214 egrosb1 
ren se2_2314 egrosb2
ren se1_2202 se_bus_sameaslastwave_1
ren se2_2302 se_bus_sameaslastwave_2
ren se1_2260 se_earnings_lastyear_ind1
ren se2_2360 se_earnings_lastyear_ind2
ren se1_amt  c_tbmsum1
ren se2_amt  c_tbmsum2

* Cats do not match to 2008
ren se1_indr  c_tbsind1
ren se2_indr c_tbsind2

* Cats do not match to 2008
ren se1_2218 tempb1
ren se2_2318 tempb2

ren se1_2256 tprftb1
ren se2_2356 tprftb2

* Cats do not match to 2008
ren se1_occ  c_tbsocc1
ren se2_occ  c_tbsocc2

ren se1_2224 ehprtb1
ren se2_2324 ehprtb2

ren se1_2226 epartb11
ren se1_2228 epartb12
ren se1_2230 epartb21
ren se2_2326 epartb22
ren se2_2328 epartb31
ren se2_2330 epartb32

ren se1_2234 eoincb1
ren se2_2334 eoincb2
ren se1_2232 eslryb1
ren se2_2332 eslryb2
ren se1_2212 c_ehrsbs1
ren se2_2312 c_ehrsbs2
ren se1_2201 c_ebno1
ren se2_2301 c_ebno2
ren se1_wks  c_se_wks1
ren se2_wks  c_se_wks2

ren se1_2203 se_activitychange_ind_bs1
ren se2_2303 se_activitychange_ind_bs2
ren se1_2252 se_otherhh_gaveinfo_ind_b1
ren se2_2352 se_otherhh_gaveinfo_ind_b2

ren se1_2220 c_eincpb1
ren se2_2320 c_eincpb2
ren se1_2222 c_epropb1
ren se2_2322 c_epropb2


* Variables with no direct equivalent in 2008, which are useful, or could be replicated from information in later panels:
** these are not renamed

*sc1174 - main reason absent without pay - unclear why this differs from sc1098
*sc1714 self-employed or not during period
*wiswop - weeks without pay in month
* ws1_wks ws2_wks [gives actual number of weeks employed in jobs]
	
*** Additional variables (consistent with 2015 versions of datasets - assume need checking):


        ren pp_earn c_tpearn
		ren pptotin c_tptotinc
		ren sc1230 c_ehrsall
		ren sc1670 c_eedfund
		** Read across not great - do not match well nor mutually exclusively across years apart from those below - more work needed perhaps to match
		*ren sc1672 easst
		*ren sc1674 easst
		*ren sc1676 easst
		ren sc1678 c_easst01
		*ren sc1680 easst
		*ren sc1682 easst
		*ren sc1684 easst
		*ren sc1686 easst
		ren sc1688 c_easst10
		*ren sc1690 easst
		ren sc1692 c_easst11
		
		* Cats do not match to 2008
		ren h_int1 eoutcome
		
		ren ws1_2044 eunion1
		ren ws2_2144 eunion2
		ren ws1_2046 ecntrc1
		ren ws2_2146 ecntrc2
		
		ren ws1_2004 ws12004
		ren ws2_2104 ws22104
		
		ren ws1_2014 ws12014
		ren ws2_2114 ws22114

*** save it
        sort id yearmonth
        save temp19${yeardata}, replace
	    save temp19${yeardata}core, replace
	

}


if $tm_if==1 {

 ******** Note, all TM variables need checking relative to 2008
 
 ******** TOPICAL MODULE 2 - Work history, education, training
 
 local i=2       // contains TM1 with tenure data!
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use su_id su_rot pp_pnum pp_entry pp_wave tm8400 tm8408 tm8416 /*
            */ tm8422 tm8430 /*
			WORKING HISTORY
			*/ tm8206 tm8208 tm8210 tm8214 tm8216 tm8218 tm8220 tm8222/*
			*/ tm8224 tm8226 tm8228 tm8230 tm8232 tm8234 tm8236 tm8240 tm8242 tm8248 /*
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
          ** Imputation **
		 */ imp_8218 imp_8220 imp_8226 imp_8224 imp_8228 imp_8230 imp_8232 imp_8234 imp_8240 imp_8242 /*
	     */ imp_8244 imp_8246 imp_8248 imp_8250 imp_8252 imp_8266 imp_8268 imp_8270 /*
	     */ imp_8272 imp_8274 imp_8276 imp_8278 imp_8282 imp_8286 imp_8288 imp_8290 imp_829a imp_8292 imp_8294 imp_ind imp_occ /*
         */ using sip${yeardata}rt`i', clear
         cd "$tempdata" 
            	

            *generate id
            egen id=concat(su_id pp_entry pp_pnum)
            
            sort id
	    
	                ***** REDEFINE SOME VARIABLES
			
	    ren pp_wave wave
	    
            mvdecode tm820*, mv(0)
            mvdecode tm821*, mv(0)
            
            ren tm8206 eeno1_startpanel
            ren tm8208 ebno1_startpanel
            ren tm8214 eeno2_startpanel
            ren tm8216 ebno2_startpanel
            gen tsjdata_startpanel=ym(tm8220,tm8218) if eeno1_startpanel!=.|eeno2_startpanel!=.
            gen tsbdata_startpanel=ym(tm8220,tm8218) if ebno1_startpanel!=.|ebno2_startpanel!=.
            lab var tsjdata_startpanel "start data main job in tm2"
            lab var tsbdata_startpanel "start data main job in tm2"
        
		
		
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
	
local i=5 
                display "TOPICAL MODULE  `i'   School Enrollment & Financing"
                cd "$rawdata" 
			    use su_id su_rot pp_pnum pp_entry pp_wave /*
            */ tm9610  tm9612 tm9616 tm9618/*
            */ tm9620/*
	    */ tm9628 tm9632 tm9636 tm9640 tm9644 tm9648 tm9652 tm9656 tm9660 tm9664 tm9668 tm9672/*
	    */ tm9630 tm9634 tm9638 tm9642 tm9646 tm9650 tm9654 tm9658 tm9662 tm9666 tm9670 tm9674 tmtedfin/*
            */ using sip${yeardata}rt`i', clear
         cd "$tempdata" 
	 
	             *generate id
            egen id=concat(su_id pp_entry pp_pnum)
            
            sort id
	    
	    ren pp_wave wave
            
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
	    * note rounding of amounts, may differ from later panels
	    ren tmtedfin ttotamt
	    

		***** APPEND TO DATA SETS BEFORE AND SAVE
			append using temp19${yeardata}tm
			sort id wave
			save temp19${yeardata}tm, replace
			compress
			
********* NB Annual Income and Retiremtn Accounts missing for 1988 ???

********************************************************************************
***** MERGE
********************************************************************************


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
			

 
if $list==1{ 

}



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

*** Create reference month variable

/*
capture drop srefmon
gen srefmon=.
forvalues i=1(3)4{
replace srefmon = rhcalmn if rot==1 & wave==`i'
replace srefmon = rhcalmn-9 if rot==2 & wave==`i' & rhcalmn>=10
replace srefmon = rhcalmn+3 if rot==2 & wave==`i' & rhcalmn<10
replace srefmon = rhcalmn-10 if rot==2 & wave==`i' & rhcalmn>=11
replace srefmon = rhcalmn+2 if rot==2 & wave==`i' & rhcalmn<11
replace srefmon = rhcalmn-11 if rot==2 & wave==`i' & rhcalmn>=12
replace srefmon = rhcalmn+1 if rot==2 & wave==`i' & rhcalmn<12
}

forvalues i=2(3)5{
replace srefmon = rhcalmn-4 if rot==1 & wave==`i'
replace srefmon = rhcalmn-1 if rot==2 & wave==`i'
replace srefmon = rhcalmn-2 if rot==3 & wave==`i'
replace srefmon = rhcalmn-3 if rot==4 & wave==`i'
}

forvalues i=3(3)6{
replace srefmon = rhcalmn-8 if rot==1 & wave==`i'
replace srefmon = rhcalmn-5 if rot==2 & wave==`i'
replace srefmon = rhcalmn-6 if rot==3 & wave==`i'
replace srefmon = rhcalmn-7 if rot==4 & wave==`i'
}	 
*/
*************************************************
             // START AND END DATE DATE
*************************************************  
 

capture gen tsjdate1=.
    // if rhcalmn+4-srefmon>12 then the highest possible start date that is in this wave is in the next year

        // if the starting rhcalmn is above the highest possible start date, it must have occured this year
                replace tsjdate1=mdy(ws1_2016, ws1_2018, rhcalyr) if ws1_2016>rhcalmn-srefmon-8 & rhcalmn-srefmon>8
        // however, if the rhcalmn is lower than the highest possible rhcalmn in the next year, it will refer to a rhcalmn next year
                replace tsjdate1=mdy(ws1_2016, ws1_2018, rhcalyr+1) if ws1_2016<=rhcalmn-srefmon-8 & ws1_2016<=rhcalmn-srefmon-8

    // else the highest possible start date in this wave is in this year, if rhcalmn-srefmon<=8 
    
        // and so any date before this date, in this year is assigned to this years
                replace tsjdate1=mdy(ws1_2016, ws1_2018, rhcalyr) if rhcalmn-srefmon<=8 & ws1_2016<=rhcalmn+4-srefmon
        // higher rhcalmns have to have occurred in the past year
                replace tsjdate1=mdy(ws1_2016, ws1_2018, rhcalyr-1) if rhcalmn-srefmon<=8 & ws1_2016>rhcalmn+4-srefmon


// end dates cannot occur before the beginning of this wave 
capture gen tejdate1=.

        // the beginning of this wave can occur in the last year  if rhcalmn-srefmon<0 
        
                // then if if ws12020>rhcalmn-srefmon+12 
                          replace tejdate1=mdy(ws1_2020, ws1_2022, rhcalyr-1) if rhcalmn-srefmon<0 & ws1_2020>rhcalmn-srefmon+12
                //if ws12020<=rhcalmn-srefmon+12 {
                            replace tejdate1=mdy(ws1_2020, ws1_2022, rhcalyr) if rhcalmn-srefmon<0 & ws1_2020<=rhcalmn-srefmon+12

        //  the beginning of this wave is this year if rhcalmn-srefmon>=0 
        //  the starting rhcalmn is higher than the beginning of the wave
            
            //if ws12020>rhcalmn-srefmon {
                replace tejdate1=mdy(ws1_2020, ws1_2022, rhcalyr) if rhcalmn-srefmon>=0 & ws1_2020>rhcalmn-srefmon
           // the starting rhcalmn is lower than the beginning rhcalmn of the wave, so it must be next year, if ws12020<=rhcalmn-srefmon 
                replace tejdate1=mdy(ws1_2020, ws1_2022, rhcalyr+1) if rhcalmn-srefmon>=0 & ws1_2020<=rhcalmn-srefmon


**************** SECOND FIRM


capture gen tsjdate2=.
    // if rhcalmn+4-srefmon>12 then the highest possible start date that is in this wave is in the next year, if rhcalmn-srefmon>8 {
    
        // if the starting rhcalmn is above the highest possible start date, it must have occured this year, if ws22116>rhcalmn-srefmon-8{
                replace tsjdate2=mdy(ws2_2116, ws2_2118, rhcalyr) if rhcalmn-srefmon>8 & ws2_2116>rhcalmn-srefmon-8
        // however, if the rhcalmn is lower than the highest possible rhcalmn in the next year, it will refer to a rhcalmn next year,  if ws22116<=rhcalmn-srefmon-8 {
                replace tsjdate2=mdy(ws2_2116, ws2_2118, rhcalyr+1) if rhcalmn-srefmon>8 & ws2_2116<=rhcalmn-srefmon-8

        // else the highest possible start date in this wave is in this year, if rhcalmn-srefmon<=8 {
        // and so any date before this date, in this year is assigned to this years, if ws22116<=rhcalmn+4-srefmon {
            replace tsjdate2=mdy(ws2_2116, ws2_2118, rhcalyr) if rhcalmn-srefmon<=8 & ws2_2116<=rhcalmn+4-srefmon

        // higher rhcalmns have to have occurred in the past year, if ws22116>rhcalmn+4-srefmon {
            replace tsjdate2=mdy(ws2_2116, ws2_2118, rhcalyr-1) if rhcalmn-srefmon<=8 & ws2_2116>rhcalmn+4-srefmon


        // end dates cannot occur before the beginning of this wave 
capture gen tejdate2=.

        // the beginning of this wave can occur in the last year,if rhcalmn-srefmon<0 {

        // then if ws22120>rhcalmn-srefmon+12 {
                replace tejdate2=mdy(ws2_2120, ws2_2122, rhcalyr-1) if rhcalmn-srefmon<0 & ws2_2120>rhcalmn-srefmon+12

        //if ws22120<=rhcalmn-srefmon+12 {
                replace tejdate2=mdy(ws2_2120, ws2_2122, rhcalyr) if rhcalmn-srefmon<0 & ws2_2120<=rhcalmn-srefmon+12

        //  the beginning of this wave is this year, if rhcalmn-srefmon>=0 {
        //  the starting rhcalmn is higher than the beginning of the wave, if ws22120>rhcalmn-srefmon {
                replace tejdate2=mdy(ws2_2120, ws2_2122, rhcalyr) if rhcalmn-srefmon>=0 & ws2_2120>rhcalmn-srefmon

        // the starting rhcalmn is lower than the beginning rhcalmn of the wave, so it must be next year, if ws22120<=rhcalmn-srefmon {
                replace tejdate2=mdy(ws2_2120, ws2_2122, rhcalyr+1) if rhcalmn-srefmon>=0  & ws2_2120<=rhcalmn-srefmon




format tejdate1 %td
format tsjdate1 %td
format tsjdate2 %td
format tejdate2 %td

 rename     ws1_2016     smonth1 //2f "Month in which this person"
 rename     ws1_2018     sday1 //2f "Day of month shown in WS1-2016"
 rename     ws1_2020     emonth1 //2f "Month in which this person left"
 rename     ws1_2022     eday1 //2f "Day of month shown in WS1-2020"

 rename     ws2_2116     smonth2 //2f "Month in which this person"
 rename     ws2_2118     sday2 //2f "Day of month shown in WS1-2016"
 rename     ws2_2120     emonth2 //2f "Month in which this person left"
 rename     ws2_2122     eday2 //2f "Day of month shown in WS1-2020"


        
        // dropping and further renaming
        capture ren att_sch eenrlm
        capture drop personkeytemp
        capture ren sc1360 eeveret
        format yearmonth %tm
        
         replace pnlwgt=pnlwgt*10000
         replace fnlwgt88=fnlwgt88*10000
         replace fnlwgt89=fnlwgt89*10000
         replace whfnwgt=whfnwgt*10000
         replace wpfinwgt=wpfinwgt*10000
        
        
        // missing variable redefine
        mvdecode _all, mv(-1)
        mvdecode _all, mv(-9)
        mvdecode _all, mv(-3)

        mvdecode eeveret tmetro tmsa rwksperm rtakjob rnotake edisab /*
        */ c_epayhr* c_tpyrate* c_rpyper* estlemp* ersend* grd_cmp /*
        */ eafnow tfipsst eenrlm ehrsall c_ehrsall pp_mis ms_ fp_rmesr* c_rmesr* c_eclwrk* c_tjbocc* /*
        */ c_ejbind* c_ejbhrs* c_tpyrate* higrade, mv(0)
        
        *recode/relabel variables in full panel 
        capture label drop eclwrkl
        label define eclwrkl 1 "private" 2 "private (merge with 1) not for profit" 3 "local gov" 4 "state gov" 5 "fed gov" 6 "unpaid"
        label value fp_eclwrk* eclwrkl
	    label value c_eclwrk* eclwrkl
        recode fp_eclwrk* (6=.) (5=3) (3=5) (7=6)
		recode c_eclwrk* (6=.) (5=3) (3=5) (7=6)

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

compress
save 19${yeardata}total_v${fileversion}, replace

        capture log close

