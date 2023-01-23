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
** STEP 0.1.1 CTV DATA CONSTRUCTION, 1985 PANEL  
********************************************************************************



* helpful input from Carl Singleton gratefully acknowledged  
    ************** NOTES 
  /*
  - Again structure is that wave 1: rot 2, rot 3, rot 4, rot 1; wave 2; rot 2, rot 3, rot 4; wave 3: rot 1, rot 2, rot 3, rot 4
  - full panel industry and occupation are edited and some entries are IMPUTED, without imputation flags! Using core wave info if avoiding this
  - full panel firm numbers are prefixed by fp_ (fp_eeno1, fp_eeno2)
  - imputation flags for occ/ind but not all imputation flags for other variables are loaded into this data. Good to keep an eye on it.	
  - No employment history module.
  - No such variable as ws12003, WS12004 etc, and self-employed equiv: c
				change in kind of work (dependent question) is absent, because occupations are recoded  every wave!
 - These 'missings' below might just be a reflection of the weird core panel structure, giving trouble when matching it with the full panels
			- ws12002, firm number, missing wave 2. Present subsequent waves???
			- Bizarrely, se1occ missing in wave 2 only. se2occ present???
  - se12202 etc firm number, equivalent to 2201 in subsequent panels
  IMPUTATION FIXES THAT NEED TO BE MADE IN SURROUNDING PANELS TOO!!!
  - imputation of reason for part-time wksptr (sc1238) is here pi28, not pi26
		ren pi26 iwkspt	// sc1232 (weeks worked part-time)
		ren pi28  iwksptr 	// sc1238 (reason part-time)
  - imputation ws1imp04 is paid by the hour
  
  - 
  */

clear
*capture log close
*set mem 3000m
set more off
set maxvar 10000


** GLOBALS
global yeardata "85"
global gnulin=0
* control parts of the do-file
global core_if=1	// read inal data from additioncore waves
global tm_if=1	       // read in the topical modules
global weights_if=1   // loads in the longitudinal weights
global subset_if=0   // starts reducing datasets into subsets with relevant variables for different projects
global locfileversion=1016  // SEPT 2016
global list=0
global fp_ind=1		// full panel data extracted


do "${codedir}/1984_88_corewave_reshape_ctv_step0_1_1.do"
cd "$rawdata"


	
*This version of program only reshapes 18 weeks of labour market data to monthly

	capture program drop idfillreplace
		program define idfillreplace
		
			capture drop prs_`1'
			ren `1' prs_`1'
			by personkey: egen `1'=max(prs_`1')
			drop prs_`1'
		end program idfillreplace 
	

** RUN MAIN CODE

if $fp_ind==1{
************************ FULL PANEL 

		* Longitudinal data: create three files 1) by id; 2) by id wave; 3) by id wave month
		
        ****
        * id
        ****
        
		cd "$rawdata"
	use rot su_id pp_entry pp_pnum pnlwgt fnlwgt85 fnlwgt86 /*
	*/ varstrat halfsamp sc1360 disab sex race ethnicty using sip${yeardata}fp, clear
		cd "$tempdata"
        capture drop id
        
        *generate id
        egen id=concat(su_id pp_entry pp_pnum)
        
        *generate across-panel id
        destring pp_pnum, generate(pp_pnum_n)
        destring pp_entry, generate(pp_entry_n)
        destring su_id, generate(pp_id_n)
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
		use su_id pp_entry pp_pnum pp_intv* higrade* grd_cmp* in_af_* att_sch* usualhr* state* /*
		*** Additional variables
		  * ed_fina* sc1672* sc1674* sc1676* sc1678* sc1680* sc1682* sc1684* sc1686* sc1688* sc1690* sc1692* 
		*/ using sip${yeardata}fp, clear
		cd "$tempdata"
		renpfix pp_intv pp_int
		renpfix pp_int pp_intv

        *generate id
        egen id=concat(su_id pp_entry pp_pnum)
		
        sort id
		reshape long pp_intv in_af_ grd_cmp higrade usualhr ed_fina sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 att_sch state_, i(id) j(wave)
        renpfix in_af_ eafnow
        rename state_ fp_tfipsst
	
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
		use rot su_id pp_entry pp_pnum ms* pp_mis* age* ms_* ws1_cl* ws2_cl* ws1_ei* ws2_ei* /*
        */ ws1_hr* ws2_hr* ws1_in* ws2_in* esr_* wksper* ws1_oc* ws2_oc* ws1_am* ws2_am* ws1_ra* ws2_ra* /*
        */ se1_wk* se2_wk* se1_hr* se2_hr* ws1_wk* ws2_wk* mthjbw* mthwop* mthwks* enrl_m* /*
        */  se1_bi* se2_bi* se1_am* se2_am* se1_oc* se2_oc* se1_in* se2_in* /*
         */se1_ty* se2_ty* /*
		*** Additional variables in main panel
		*/ pp_ear* pp_inc* using sip${yeardata}fp, clear
		cd "$tempdata"

        *generate id
        egen id=concat(su_id pp_entry pp_pnum)
        
        *the getting-rid-of-zeros renaming
		capture renpfix age_ tage
		*capture renpfix age0 tage
		*capture renpfix hh_add0 hh_add
        *capture renpfix pp_mis0 pp_mis
        *capture renpfix ws1_cl0 ws1_cl
        *capture renpfix ws2_cl0 ws2_cl
        
		sort id
		reshape long ms_ pp_mis tage ws1_cl ws2_cl ws1_ei ws2_ei  /* 
         */ ws1_hr ws2_hr ws1_in ws2_in esr_ wksper ws1_oc ws2_oc ws1_am ws2_am ws1_ra ws2_ra /* 
         */ se1_wk  se2_wk  se1_hr  se2_hr  ws1_wk ws2_wk mthjbw  mthwop  mthwks  enrl_m  /* 
         */ se1_bi se2_bi se1_am se2_am se1_oc se2_oc se1_in se2_in se1_ty se2_ty pp_ear pp_inc, i(id) j(mnth) string 
	 
        *the great renaming
        rename ws1_cl fp_eclwrk1
       rename ws2_cl fp_eclwrk2
       rename ws1_ei fp_eeno1
        rename ws2_ei fp_eeno2
        rename ws1_hr fp_ejbhrs1
        rename ws2_hr fp_ejbhrs2
        rename ws1_in fp_ejbind1
        rename ws2_in fp_ejbind2
        rename esr_ fp_rmesr
        rename wksper rwksperm
        rename ws1_oc fp_tjbocc1
        rename ws2_oc fp_tjbocc2
        rename ws1_am fp_tpmsum1
        rename ws2_am fp_tpmsum2
        rename ws1_ra fp_tpyrate1
        rename ws2_ra fp_tpyrate2
        rename enrl_m fp_eenrlm
        rename ws1_wk fp_ws1wk
        rename ws2_wk fp_ws2wk
        rename mthjbw fp_wksjb_
        rename mthwks fp_weeksl
		
			

		// believe this are left as fp_* in 1987 panel
		rename se1_hr fp_ehrsbs1
        rename se2_hr fp_ehrsbs2
        rename se1_wk fp_se1_wk
        rename se2_wk fp_se2_wk
        
		*rename se1_hr ehrsbs1
        *rename se2_hr ehrsbs2
        *rename se1_wk se1_wk
        *rename se2_wk se2_wk
        
		
*	rename ws1_cl eclwrk1
*        rename ws2_cl eclwrk2
*        rename ws1_ei fp_eeno1
*        rename ws2_ei fp_eeno2
*	rename ws1_hr ejbhrs1
*        rename ws2_hr ejbhrs2
*        rename ws1_in ejbind1
*        rename ws2_in ejbind2
*	lab var ejbind1i "Edited and Imputed Industry of employer 1"
*        lab var ejbind2i "Edited and Imputed Industry of employer 2"
*        rename esr_ rmesr
*        rename wksper rwksperm
*        rename ws1_oc tjbocc1
*        rename ws2_oc tjbocc2
*        lab var tjbocc1i "Edited and Imputed Occupation at employer 1"
*	lab var tjbocc2i "Edited and Imputed Occupation at employer 2"
*        rename ws1_am tpmsum1
*        rename ws2_am tpmsum2
*        rename ws1_ra tpyrate1
*       rename ws2_ra tpyrate2
*        rename enrl_m eenrlm
*        rename ws1_wk ws1wk
*        rename ws2_wk ws2wk
*        rename mthjbw wksjb_
*        rename mthwks weeksl
        
*        rename se1_hr ehrsbs1
*        rename se2_hr ehrsbs2
*        rename se1_wk se_wks1
*        rename se2_wk se_wks2
        
        rename se1_bi ebno1 
        rename se2_bi ebno2 
	rename se1_am tbmsum1 
	rename se2_am tbmsum2 
	
	rename se1_oc tbsocc1
	rename se2_oc tbsocc2 
	rename se1_in tbsind1 
	rename  se2_in tbsind2 

       gen eincpb1=.
        gen eincpb2=.
        gen epropb1=.
        gen epropb2=.
        
        replace eincpb1=1 if se1_ty==3
        replace eincpb1=2 if se1_ty>0 & se1_ty<3
        replace eincpb1=-1 if se1_ty==0
        
        replace eincpb2=1 if se2_ty==3
        replace eincpb2=2 if se2_ty>0 & se2_ty<3
        replace eincpb2=-1 if se2_ty==0
        
        replace epropb1=1 if se1_ty==1
        replace epropb1=2 if se1_ty==2
        replace epropb1=-1 if se1_ty<1|se1_ty>2
        
        replace epropb2=1 if se2_ty==1
        replace epropb2=2 if se2_ty==2
        replace epropb2=-1 if se2_ty<1|se2_ty>2
        
        *drop se1_ty se2_ty
	
	rename se1_ty typbs1
	rename se2_ty typbs2
	
	
			*** Additional variables
				
		ren pp_ear tpearn
		ren pp_inc tptotinc

        
        * CREATE TIME SERIES VARIABLE
        destring mnth, gen(interview)
        sort id interview 
        capture drop yearmonth
        capture gen yearmonth=.
        replace yearmonth=tm(1984m12)+interview if rot==1
        replace yearmonth=tm(1984m9)+interview if rot==2
        replace yearmonth=tm(1984m10)+interview if rot==3
        replace yearmonth=tm(1984m11)+interview if rot==4
        
        gen year=year(dofm(yearmonth))
		gen month=month(dofm(yearmonth))

        gen wave=ceil(interview/4)
        drop if wave>8
        
		sort id yearmonth
		save testm, replace
}	
if $core_if==1 {

	local i=1
        display "CORE WAVE `i'"
        cd "$rawdata" 
		use su_id su_rot pp_intvw pp_pnum pp_entry pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year/*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1044 sc1042 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ sc1044 sc1042 sc1044 sc1048 sc1052 sc1050 sc1054 sc1216 sc1218 sc1222 sc1224 sc1226 sc1228 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ wksjb* wkwjob* esr_* wkswop* wkslok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws2_2002 ws1_2014 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2024 ws1_2026 ws1_2028 ws1_2030 /*
		*/ ws2_2022 ws2_2024 ws1_2014 ws2_2026 ws2_2028 ws1_2012 ws2_2012 ws2_2016 ws2_2018 ws2_2020 ws2_2030/*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se12214 se22214  se12260 se22260 se1amt* se2amt* se12220 se22220 se1_indr se2_indr se12218 se22218/*
		*/ se12256 se22256 se1occ se2occ se12224 se22224 se12222 se22222 se12226 se12228 se12230 se22226 se22228 se22230 /*
		*/ se12234 se22234 se12232 se22232 se12212 se22212 se1wks* se2wks* se12252 se22252 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ h1_int1 h2_int1 h3_int1 h4_int1 sc1230 ws1_2046 ws2_2046 ws1_2044 ws2_2044 pp_earn* pptotin* /*
		*** Training
		*/ sc1656 sc1658 sc1660 sc1662 sc1664 sc1666 sc1668 sc1670 sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 /*
								*** IMPUTATIONS FLAGS *****
		*/ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
			*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/*
		*/ pi39  pi16 pi03 pi35 pi28 pi26 /* idisab ireasab (reason absent without pay) itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi14 pi02 pi04 pi20 pi19 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		*/ using sip${yeardata}w`i', clear
		
		cd "$tempdata" 
            *generate id
            egen id=concat(su_id pp_entry pp_pnum)

* Note ws variables different numbers compared with 1988
	    
*1988 = 1986
*ws2023 = ?
*ws2024 = ?
*ws2025 = ws2024
*ws2029 = ws2030
		
		// rename imputation variables
		/*
		ren ws1imp01 iws1occ
	    ren ws1imp02 iws1ind
	    ren ws1imp03 iws12012
	    ren ws1imp04 iws12026
	    ren se1imp01 ise1occ
	    ren se1imp02 ise1ind 
	    ren se1imp11 ise12260 
	    ren pi02 iwkslok 	// pi2: imputation sc1004-sc1040 
	    ren pi03 itakjob 	// pi3: imputation sc1042
	    ren pi39 idisab   	// pi39
	    ren pi13 iwksjob  	// pi13: imputation sc1100-1134 weeks with job
		ren pi14 iwkswop 	// sc1136
	    ren pi16 ireasab 	// sc1172
	    ren pi26 iwksptr 	// sc1232
	    ren pi35 iretir 		// sc1360 (ever retired?)
	    ren pi20 itakjobn  	// sc1218
	    ren ws2imp01 iws2occ
	    ren ws2imp02 iws2ind
	    ren ws2imp03 iws22112
	    ren ws2imp04 iws22124
	    ren se2imp01 ise2occ  
	    ren se2imp02 ise2ind
	    ren se2imp11 ise22360 */
		
		ren ws1imp01 iws1occ
	    ren ws1imp02 iws1ind
	    ren ws1imp03 iws12012
	    ren ws1imp04 iws12024
	    ren se1imp01 ise1occ
	    ren se1imp02 ise1ind 
	    ren se1imp11 ise12260 
		ren pi03 itakjob 	// pi3: imputation sc1042
	    replace itakjob=pi19 if itakjob==0 | itakjob==. // sc1216
		ren pi04 itakjobn	//sc1044
		replace itakjobn=pi20 if itakjobn==0 | itakjobn==. // sc1218
		
	    
	    ren pi02 iwkslok 	// pi2: imputation sc1004-sc1040 
	    *ren pi03 itakjob 	// pi3: imputation sc1042
	    ren pi39 idisab   	// pi39
	    ren pi13 iwksjob  	// pi13: imputation sc1100-1134 weeks with job
		ren pi14 iwkswop 	// sc1136
	    ren pi16 ireasab 	// sc1172
	    ren pi26 iwkspt	// sc1232 (weeks worked part-time)
		ren pi28  iwksptr 	// sc1238 (reason part-time)
	    ren pi35 iretir 		// sc1360 (ever retired?)
	    *ren pi20 itakjobn  	// sc1218
	    ren ws2imp01 iws2occ
	    ren ws2imp02 iws2ind
	    ren ws2imp03 iws22112
	    ren ws2imp04 iws22124
	    ren se2imp01 ise2occ  
	    ren se2imp02 ise2ind
	    ren se2imp11 ise22360 
		
		
	rename ws2_2002 ws2_2102    
	rename ws2_2022 ws2_2122
	rename ws2_2024 ws2_2124
	rename ws2_2028 ws2_2128
	rename ws2_2026 ws2_2126
	rename ws2_2012 ws2_2112
	rename ws2_2016 ws2_2116
	rename ws2_2018 ws2_2118
	rename ws2_2020 ws2_2120
	rename ws2_2030 ws2_2130
	rename ws2_2044 ws2_2144
	rename ws2_2046 ws2_2146
	
	rename ws1_2024 ws1_2025
	rename ws2_2124 ws2_2125
	
	rename wkswop* wiswop*
	rename wkslok* wislok*
	
	rename se22214 se22314
	
	rename se22260 se22360  
	rename se22220 se22320 
	rename se22218 se22318
	rename se22256 se22356 
	*rename se2occ se_2occ 
	rename se22224 se22324 
	rename se22222 se22322 
	rename se22226 se22326 
	rename se22228 se22328 
	rename se22230 se22330 
	rename se22232 se22332 
	rename se22234 se22334
	rename se22212 se22312
	
	rename se22252 se22352

	    
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
		*/ se1amt se2amt/*
		*/ se1wks se2wks /*	
		*/ h@_int1 pp_earn pptotin/*
		*/ ,i(id) j(refmth) string
	rename h_month month
	gen float year=h_year+1900
	
	gen yearmonth=ym(year, month)
      
        *across panel data
        gen panel=19${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring su_id pp_entry pp_pnum)
                    
            ***** SAVE
	    
	            sort id yearmonth
		compress
		save temp19${yeardata}, replace 	
	
	local i=`i'+1
	
	
	
	local i=2
	while `i'>=2 & `i'<9 {
           display "CORE WAVE `i'"
        cd "$rawdata"

if `i' == 2 { 
		use su_id su_rot pp_intvw pp_pnum pp_entry pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year/*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1044 sc1042 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ sc1044 sc1042 sc1044 sc1048 sc1052 sc1050 sc1054 sc1216 sc1218 sc1222 sc1224 sc1226 sc1228 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ wksjb* wkwjob* esr_* wiswop* wislok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2024 ws1_2026 ws1_2028 ws1_2030 ws1_2014 ws2_2114 /*
		*/ ws2_2122 ws2_2124  ws2_2126 ws2_2128 ws1_2012 ws2_2112 ws2_2116 ws2_2118 ws2_2120 ws2_2130/*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se12214 se22314  se12260 se22360 se1amt* se2amt* se12220 se22320 se1_indr se2_indr se12218 se22318/*
		*/ se12256 se22356 se2occ se12224 se22324 se12222 se22322 se12226 se12228 se12230 se22326 se22328 se22330 /*
		*/ se12234 se22334 se12232 se22332 se12212 se22312 se1wks* se2wks* se12252 se22352 se1_2202 se2_2202 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ h1_int1 h2_int1 h3_int1 h4_int1 sc1230 pp_earn* pptotin* /*
		*** Training
		*/ sc1656 sc1658 sc1660 sc1662 sc1664 sc1666 sc1668 sc1670 sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 /*
										*** IMPUTATIONS FLAGS *****
		*/ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
		*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/**/ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
			*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/*
		*/ pi39  pi16 pi03 pi35 pi26 pi28 /* idisab ireasab (reason absent without pay) itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi14 pi02 pi04  pi20 pi19 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		*/ using sip${yeardata}w`i', clear
	}	
	
	
	if `i'==5 | `i'==6 | `i'==8{ 
		use su_id su_rot pp_intvw pp_pnum pp_entry pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year/*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1044 sc1042 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ sc1044 sc1042 sc1044 sc1048 sc1052 sc1050 sc1054 sc1216 sc1218 sc1222 sc1224 sc1226 sc1228 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ wksjb* wkwjob* esr_* wiswop* wislok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws2_2102 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2024 ws1_2026 ws1_2028 ws1_2030 ws1_2014 /*
		*/ ws2_2122 ws2_2124  ws2_2126 ws2_2128 ws1_2012 ws2_2112 ws2_2116 ws2_2118 ws2_2120 ws2_2130 ws2_2114 /*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se12214 se22314  se12260 se22360 se1amt* se2amt* se12220 se22320 se1_indr se2_indr se12218 se22318/*
		*/ se12256 se22356 se1_occ se2occ se12224 se22324 se12222 se22322 se12226 se12228 se12230 se22326 se22328 se22330 /*
		*/ se12234 se22334 se12232 se22332 se12212 se22312 se1wks* se2wks* se12252 se22352 se1_2202 se2_2202 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ h1_int1 h2_int1 h3_int1 h4_int1 sc1230 pp_earn* pptotin* /*
		*** Training
		*/ sc1656 sc1658 sc1660 sc1662 sc1664 sc1666 sc1668 sc1670 sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 /*
										*** IMPUTATIONS FLAGS *****
		*/ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
		*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/*
		*/ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
			*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/*
		*/ pi39  pi16 pi03 pi35 pi26 pi28/* idisab ireasab (reason absent without pay) itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi14 pi02 pi04 pi20 pi19 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		*/ using sip${yeardata}w`i', clear
	}	
	
		if `i' == 3 | `i'==4 | `i'==7 { 
		use su_id su_rot pp_intvw pp_pnum pp_entry pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year/*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1044 sc1042 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ sc1044 sc1042 sc1044 sc1048 sc1052 sc1050 sc1054 sc1216 sc1218 sc1222 sc1224 sc1226 sc1228 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ wksjb* wkwjob* esr_* wiswop* wislok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws2_2102 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2024 ws1_2026 ws1_2028 ws1_2030 ws1_2014 /*
		*/ ws2_2122 ws2_2124  ws2_2126 ws2_2128 ws1_2012 ws2_2112 ws2_2116 ws2_2118 ws2_2120 ws2_2130 ws2_2114 /*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se12214 se22314  se12260 se22360 se1amt* se2amt* se12220 se22320 se1_indr se2_indr se12218 se22318/*
		*/ se12256 se22356 se1_occ se2occ se12224 se22324 se12222 se22322 se12226 se12228 se12230 se22326 se22328 se22330 /*
		*/ se12234 se22334 se12232 se22332 se12212 se22312 se1wks* se2wks* se12252 se22352 se1_2202 se2_2202 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/ h1_int1 h2_int1 h3_int1 h4_int1 sc1230 pp_earn* pptotin* /*
		*** Training
		*/ sc1656 sc1658 sc1660 sc1662 sc1664 sc1666 sc1668 sc1670 sc1672 sc1674 sc1676 sc1678 sc1680 sc1682 sc1684 sc1686 sc1688 sc1690 sc1692 /*
										*** IMPUTATIONS FLAGS *****
		*/ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
		*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/*
		*/ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
			*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/*
		*/ pi39  pi16 pi03 pi35 pi26 pi28 /* idisab ireasab (reason absent without pay) itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi14 pi02 pi04 pi20 pi19 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		*/ using sip${yeardata}rt`i', clear
	}	
		cd "$tempdata" 
            *generate id
            egen id=concat(su_id pp_entry pp_pnum)

* Note ws variables different numbers compared with 1988
	    
*1988 = 1986
*ws2023 = ?
*ws2024 = ?
*ws2025 = ws2024
*ws2029 = ws2030

*se1occ missing completely? But not for second job... wtf

		/*
		ren ws1imp01 iws1occ
	    ren ws1imp02 iws1ind
	    ren ws1imp03 iws12012
	    ren ws1imp04 iws12024
	    ren se1imp01 ise1occ
	    ren se1imp02 ise1ind 
	    ren se1imp11 ise12260 
	    ren pi02 iwkslok 	// pi2: imputation sc1004-sc1040 
	    ren pi03 itakjob 	// pi3: imputation sc1042
	    ren pi39 idisab   	// pi39
	    ren pi13 iwksjob  	// pi13: imputation sc1100-1134 weeks with job
		ren pi14 iwkswop 	// sc1136
	    ren pi16 ireasab 	// sc1172
	    ren pi26 iwkspt 	// sc1232
	    ren pi28 iwksptr 	// sc1238
	    
		ren pi35 iretir 		// sc1360 (ever retired?)
	    ren pi20 itakjobn  	// sc1218
	    ren ws2imp01 iws2occ
	    ren ws2imp02 iws2ind
	    ren ws2imp03 iws22112
	    ren ws2imp04 iws22124
	    ren se2imp01 ise2occ  
	    ren se2imp02 ise2ind
	    ren se2imp11 ise22360 
		*/
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
		replace itakjobn=pi20 if itakjobn==0 | itakjobn==. // sc1218
		
	    
	    ren pi39 idisab   	// pi39
	    ren pi13 iwksjob  	// pi13: imputation sc1100-1134 weeks with job
		ren pi14 iwkswop 	// sc1136
	    ren pi16 ireasab 	// sc1172
	    ren pi26 iwkspt	// sc1232 (weeks worked part-time)
		ren pi28  iwksptr 	// sc1238 (reason part-time)
	    ren pi35 iretir 		// sc1360 (ever retired?)
	    *ren pi20 itakjobn  	// sc1218




* union variables missing



	capture rename se1_occ se1occ
	rename ws1_2024 ws1_2025
	rename ws2_2124 ws2_2125
	
	rename se1_2202 se1_2201 
	rename se2_2202 se2_2301
	    
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
		*/ se1amt se2amt/*
		*/ se1wks se2wks /*	
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
	    *ren pp_wave wave
            ren fnlwgt_ wpfinwgt
            ren h_strat c_strat
            ren h_hsc c_hsc
            ren h_wgt whfnwgt
            ren month rhcalmn
            ren year rhcalyr
	    drop h_year
            *ren refmth srefmon
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
            *ren  ws1_2003 estlemp1
            *ren ws2_2103 estlemp2
            *capture lab var estlemp1 "Still working for employer1 (at time of interview)"
            *recode estlemp1 (1=2) (2=1)
            *capture lab var estlemp2 "Still working for employer2 (at time of interview)"
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
        *ren ws1_2023 stopjob1
        *ren ws2_2123 stopjob2
        
        ** REASON STOPPED WORKING
        *rename     ws1_2024   ersend1   //1f "What is the main reason ... stopped"
        *rename     ws2_2124   ersend2

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
*recode ersend1 (6=15) (5=12) (4=11) (3=8)
*recode ersend2 (6=15) (5=12) (4=11) (3=8)

* PAYMENTS / INCOME
ren ws1_2025 c_ejbhrs1
ren ws2_2125 c_ejbhrs2

ren ws1_2026 c_epayhr1
ren ws2_2126 c_epayhr2
 
ren ws1_2028 c_tpyrate1
ren ws2_2128 c_tpyrate2

ren ws1_2030 c_rpyper1
ren ws2_2130 c_rpyper2

ren ws1_amt c_tpmsum1
ren ws2_amt c_tpmsum2

replace yearmonth=ym(rhcalyr, rhcalmn)

*** selfemployed renaming and missing value decoding


mvdecode se*, mv(0)

ren se12214 egrosb1 
ren se22314 egrosb2
*ren se12202 se_bus_sameaslastwave_1
*ren se22302 se_bus_sameaslastwave_2
ren se12260 se_earnings_lastyear_ind1
ren se22360 se_earnings_lastyear_ind2
ren se1amt  c_tbmsum1
ren se2amt  c_tbmsum2

* Cats do not match to 2008
ren se1_indr  c_tbsind1
ren se2_indr c_tbsind2

* Cats do not match to 2008
ren se12218 tempb1
ren se22318 tempb2

ren se12256 tprftb1
ren se22356 tprftb2

* Cats do not match to 2008
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
ren se1_2201 c_ebno1
ren se2_2301 c_ebno2
ren se1wks  c_se_wks1
ren se2wks  c_se_wks2

*ren se12203 se_activitychange_ind_bs1
*ren se22303 se_activitychange_ind_bs2
ren se12252 se_otherhh_gaveinfo_ind_b1
ren se22352 se_otherhh_gaveinfo_ind_b2

ren se12220 c_eincpb1
ren se22320 c_eincpb2
ren se12222 c_epropb1
ren se22322 c_epropb2


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
		
		

*** save it
        sort id yearmonth
        save temp19${yeardata}, replace
	    save temp19${yeardata}core, replace	

}

if $tm_if==1{

forvalues i=3(4)7{      //
                display "ONLY TOPICAL MODULE  `i'"
        cd "$rawdata" 
			use su_id su_rot pp_pnum pp_entry pp_wave tm8000 tm8002 tm8004 tm8006 tm8008 tm8012 /*
            */ tm8016 tm8018 tm8020 tm8022 tm8024 tm8028 /*
            */ using sip${yeardata}rt`i', clear
         cd "$tempdata" 
            	

            *generate id wave
            egen id=concat(su_id pp_entry pp_pnum)
            
            *ren pp_wave wave
            sort id pp_wave
         
		 
		 
            
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
            
	    
	 if `i'==3{   
	    	    ***** APPEND TO DATA SETS BEFORE AND SAVE
	save temp19${yeardata}tm, replace
	compress
	}
            
        if `i'==7{        
		***** APPEND TO DATA SETS BEFORE AND SAVE
			append using temp19${yeardata}tm
			sort id pp_wave
			save temp19${yeardata}tm, replace
			compress
	}
	}			

********************************************************************************
***** MERGE
********************************************************************************


***** APPEND TM DATASET TO CORE DATA SET
	cd "$tempdata"
        capture use temp19${yeardata}tm
	sort id pp_wave
	capture drop _mrgtot
        merge id pp_wave using temp19${yeardata}core, _merge(_mrgtot) sort uniqmaster
        sort id pp_wave yearmonth
	tab _mrgtot            
	save temp19${yeardata}, replace
	display "_mrgtot==1 means that the individual appears in the TM somewhere, but not in core waves; STRANGE!"
	display "_mrgtot==2 means that the individual appears in the core wave dataset somewhere, but did not partake in the TM"
	display "_mrmtot==3 individual in the core wave data set and did partake in some TM"
	compress
**** Note - TMs 5 & 8 missing completely so no "School Enrollment and Financing"

**** Employment and training histroy also missing

}

    *** THE BIG MERGE 

		use testi, clear
		drop if su_id==""
		capture drop _mrgtot1 _mrgtot2 
		merge id using testw, sort _merge(_mrgtot1) uniqmaster
		tab _mrgtot1
		rename _mrgtot1 mtst1
		sort id wave 
		merge id wave using testm, _merge(_mrgtot2) sort uniqmaster
		tab _mrgtot2
		rename _mrgtot2 mtst2
		sort id yearmonth

		// MERGE FP WITH CORE+TM FILE
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
forvalues i=1(3)7{
replace srefmon = rhcalmn if rot==1 & wave==`i'
replace srefmon = rhcalmn-9 if rot==2 & wave==`i' & rhcalmn>=10
replace srefmon = rhcalmn+3 if rot==2 & wave==`i' & rhcalmn<10
replace srefmon = rhcalmn-10 if rot==2 & wave==`i' & rhcalmn>=11
replace srefmon = rhcalmn+2 if rot==2 & wave==`i' & rhcalmn<11
replace srefmon = rhcalmn-11 if rot==2 & wave==`i' & rhcalmn>=12
replace srefmon = rhcalmn+1 if rot==2 & wave==`i' & rhcalmn<12
}

forvalues i=2(3)8{
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
******************	*******************************  

capture drop srefmon     
destring refmth, gen(srefmon)

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
         replace fnlwgt85=fnlwgt85*10000
         replace fnlwgt86=fnlwgt86*10000
         replace whfnwgt=whfnwgt*10000
         replace wpfinwgt=wpfinwgt*10000
        
        
        // missing variable redefine
        mvdecode _all, mv(-1)
        mvdecode _all, mv(-9)
        mvdecode _all, mv(-3)

        capture mvdecode eeveret tmetro tmsa rwksperm rtakjob rnotake edisab, mv(0)
        capture mvdecode c_epayhr* c_tpyrate* c_rpyper* estlemp* ersend* grd_cmp, mv(0)
        capture mvdecode eafnow tfipsst eenrlm ehrsall c_ehrsall pp_mis ms_ fp_rmesr* c_rmesr* c_eclwrk* c_tjbocc*, mv(0)
        capture mvdecode c_ejbind* c_ejbhrs* c_tpyrate* higrade, mv(0)
        
        *recode/relabel variables in full panel 
        capture label drop eclwrkl
        label define eclwrkl 1 "private" 2 "private (merge with 1) not for profit" 3 "local gov" 4 "state gov" 5 "fed gov" 6 "unpaid"
        capture label value eclwrk* eclwrkl
	capture label value fp_eclwrk* eclwrkl
	capture label value c_eclwrk* eclwrkl
        capture recode eclwrk* (6=.) (5=3) (3=5) (7=6)
	capture recode fp_eclwrk* (6=.) (5=3) (3=5) (7=6)
	capture recode c_eclwrk* (6=.) (5=3) (3=5) (7=6)

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
	*gen educ_tm2 = 1 if tm8400==1 & tm8408==2
	* highest grade 12 or less and has hs diploma or ged
	*replace educ_tm2 = 2 if tm8400==1 & tm8408==1
	*replace educ_tm2 = 2 if tm8400==2 
	* highest grade 1 year of college and  ass., voc, or no degree
	*replace educ_tm2 = 3 if tm8416==1 & tm8422>=5 & tm8422<=7
	* highest grade 1 year of college and ba degree
	*replace educ_tm2 = 4 if tm8416==1 & tm8422==4
	* highest grade 1 year of college and adv. degree
	*replace educ_tm2 = 5 if tm8416==1 & tm8422>=1 & tm8422<=3 & tm8430==1
	
    *label var educ_tm2 "TM2 Educational Attainment"
    
    *capture drop tm8400 tm8408 tm8416 tm8422 tm8430
    
		label define educl 1 "less than high school" 2 "high school grad" /*
		*/ 3 "some college" 4 "college degree" 5 "post-college"
        
     lab val educ educl
     *lab val educ_tm2 educl
     
     
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
     
cd "$outputdata"

save 19${yeardata}total_v${fileversion}, replace

        capture log close
