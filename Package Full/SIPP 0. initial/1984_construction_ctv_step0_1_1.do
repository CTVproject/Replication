
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
** STEP 0.1.1 CTV DATA CONSTRUCTION, 1984 PANEL  
********************************************************************************



* helpful input from Carl Singleton gratefully acknowledged  


 
	
  
clear
*capture log close
*set mem 3000m
set more off
set maxvar 10000


** GLOBALS
global yeardata "84"
global panel=1984
global gnulin=0
* control parts of the do-file
global core_if=1	// read inal data from additioncore waves
global tm_if=1	       // read in the topical modules
global weights_if=1   // loads in the longitudinal weights
global subset_if=0   // starts reducing datasets into subsets with relevant variables for different projects
*global fileversion=1022  // AUG 2016 ---> set in the main file
global list=0
global fp_ind=1		// full panel data extracted







*cccc
cd "${codedir}"
cd "$rawdata"

	
		




*This version of program only reshapes 18 weeks of labour market data to monthly
do "${codedir}/1984_88_corewave_reshape_ctv_step0_1_1.do"

			
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
        * No varstrat
		cd "$rawdata"
	use rot su_id pp_entry pp_pnum pnlwgt fnlwgt84 fnlwgt85 /*
	*/ hs_hsc sc1360 disab sex race ethnicty using sip${yeardata}fp, clear
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
        *ren varstrat strat
        ren hs_hsc hsc
	
	       
		sort id
		save testi, replace
 
        ****
		* id wave
        ***
        
	* No usual hours of work variable
           
		cd "$rawdata"
		use su_id pp_entry pp_pnum pp_intv* higrade* grd_cmp* in_af_* att_sch* state* /*
		*** Additional variables - none here
		* ed_fina* sc1672* sc1674* sc1676* sc1678* sc1680* sc1682* sc1684* sc1686* sc1688* sc1690* sc1692*
		*/ using sip${yeardata}fp, clear
		cd "$tempdata"
		renpfix pp_intv pp_int
		renpfix pp_int pp_intv

        *generate id
        egen id=concat(su_id pp_entry pp_pnum)
		
        sort id
		reshape long pp_intv in_af_ grd_cmp higrade att_sch state_, i(id) j(wave)
        renpfix in_af_ eafnow
        rename state_ fp_tfipsst
	
	*ren usualhr ehrsall
		*ren ed_fina eedfund
		** Read across not great - do not match well nor mutually exclusively across years apart from those below
		*ren sc1672 easst
		*ren sc1674 easst
		*ren sc1676 easst
		*ren sc1678 easst01
		*ren sc1680 easst
		*ren sc1682 easst
		*ren sc1684 easst
		*ren sc1686 easst
		*ren sc1688 easst10
		*ren sc1690 easst
		*ren sc1692 easst11
	
        sort id wave
        
        drop if wave>9
        

		save testw, replace
		
        ****
		* id month
        ****
        * No enrl_m* - school enrollment variable
	* No se1_bi* - buiness id
	* No se1_oc* se2_oc* se1_in* se2_in* se1_ty* se2_ty* - (I think)
	
		cd "$rawdata"
		use rot su_id pp_entry pp_pnum ms* pp_mis* age* hh_add* ms_* clswk1* clswk2* jobid1* jobid2* /*
        */ w2024_* w2124_* ws1_in* ws2_in* esr_* wksper* ws1_oc* ws2_oc* ws1_am* ws2_am* w2028* w2128* /*
        */ se1_wk* se2_wk* s2212* s2312* ws1_wk* ws2_wk* wksjb_* mthwop* weeksl*/*
        */  se1_am* se2_am* /*
  
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
		reshape long ms_ pp_mis tage hh_add clswk1 clswk2 jobid1 jobid2  /* 
         */ w2024_ w2124_ ws1_in ws2_in esr_ wksper ws1_oc ws2_oc ws1_am ws2_am w2028_ w2128_ /* 
         */ se1_wk se2_wk s2212_ s2312_ ws1_wk ws2_wk wksjb_ mthwop weeksl /* 
         */ se1_am se2_am pp_ear pp_inc, i(id) j(mnth) string 
	 
        *the great renaming
	rename clswk1 fp_eclwrk1
        rename clswk2 fp_eclwrk2
        rename jobid1 fp_eeno1
        rename jobid2 fp_eeno2
	rename w2024_ fp_ejbhrs1
        rename w2124_ fp_ejbhrs2
        rename ws1_in fp_ejbind1
        rename ws2_in fp_ejbind2
        rename esr_ fp_rmesr
        rename wksper rwksperm
        rename ws1_oc fp_tjbocc1
        rename ws2_oc fp_tjbocc2
        rename ws1_am fp_tpmsum1
        rename ws2_am fp_tpmsum2
        rename w2028_ fp_tpyrate1
        rename w2128_ fp_tpyrate2
        *rename enrl_m fp_eenrlm
        rename ws1_wk fp_ws1wk
        rename ws2_wk fp_ws2wk
        rename wksjb_ fp_wksjb_
        rename weeksl fp_weeksl
	
        rename s2212_ ehrsbs1
        rename s2312_ ehrsbs2
        rename se1_wk se_wks1
        rename se2_wk se_wks2

*	rename clswk1 eclwrk1
*        rename clswk2 eclwrk2
*        rename jobid1 fp_eeno1
*        rename jobid2 fp_eeno2
*	rename w2024_ ejbhrs1
*        rename w2124_ ejbhrs2
*        rename ws1_in ejbind1
*        rename ws2_in ejbind2
*        rename esr_ rmesr
*        rename wksper rwksperm
*        rename ws1_oc tjbocc1
*        rename ws2_oc tjbocc2
*        rename ws1_am tpmsum1
*        rename ws2_am tpmsum2
*        rename w2028_ tpyrate1
*        rename w2128_ tpyrate2
        *rename enrl_m eenrlm
*        rename ws1_wk ws1wk
*        rename ws2_wk ws2wk
        *rename mthjbw wksjb_
        *rename mthwks weeksl
        
*        rename s2212_ ehrsbs1
*        rename s2312_ ehrsbs2
*        rename se1_wk se_wks1
*        rename se2_wk se_wks2
        
        *rename se1_bi ebno1 
        *rename se2_bi ebno2 
	rename se1_am tbmsum1 
	rename se2_am tbmsum2 
	
	*rename se1_oc tbsocc1
	*rename se2_oc tbsocc2 
	*rename se1_in tbsind1 
	*rename  se2_in tbsind2 

       *gen eincpb1=.
        *gen eincpb2=.
       * gen epropb1=.
        *gen epropb2=.
        
        *replace eincpb1=1 if se1_ty==3
        *replace eincpb1=2 if se1_ty>0 & se1_ty<3
        *replace eincpb1=-1 if se1_ty==0
        
        *replace eincpb2=1 if se2_ty==3
        *replace eincpb2=2 if se2_ty>0 & se2_ty<3
        *replace eincpb2=-1 if se2_ty==0
        
        *replace epropb1=1 if se1_ty==1
        *replace epropb1=2 if se1_ty==2
        *replace epropb1=-1 if se1_ty<1|se1_ty>2
        
        *replace epropb2=1 if se2_ty==1
        *replace epropb2=2 if se2_ty==2
        *replace epropb2=-1 if se2_ty<1|se2_ty>2
        
        *drop se1_ty se2_ty
	
	
			*** Additional variables
				
		ren pp_ear tpearn
		ren pp_inc tptotinc

        
        * CREATE TIME SERIES VARIABLE
	* NOTE: Differnt order of rotations to other panels
	
        destring mnth, gen(interview)
        sort id interview 
        capture drop yearmonth
        capture gen yearmonth=.
        replace yearmonth=tm(1983m5)+interview if rot==1
        replace yearmonth=tm(1983m6)+interview if rot==2
        replace yearmonth=tm(1983m7)+interview if rot==3
        replace yearmonth=tm(1983m8)+interview if rot==4
		format yearmonth %tm 
		
        gen year=year(dofm(yearmonth))
		gen month=month(dofm(yearmonth))

        gen wave=ceil(interview/4)

        tab rot wave
        
		drop if wave>9
		
		
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
		*/ sc1044 sc1042 sc1044 sc1048 sc1052 sc1050 sc1054 sc1216 sc1218 sc1222 sc1224 sc1226 sc1228 sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ wksjb* wkwjob* esr_* wkswop* wkslok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws1_2012 ws1_2014 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2024 ws1_2026 ws1_2028 ws1_2030 /*
		*/ ws2_2002 ws2_2012 ws2_2014 ws2_2016 ws2_2018 ws2_2020 ws2_2022 ws2_2024  ws2_2026 ws2_2028 ws2_2030/*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se12214 se22214  se12260 se22260 se1amt* se2amt* se12220 se22220 se1ind se2ind se12218 se22218/*
		*/ se12256 se22256 se1occ se2occ se12224 se22224 se12222 se22222 se12226 se12228 se12230 se22226 se22228 se22230 /*
		*/ se12234 se22234 se12232 se22232 se12212 se22212 se1wks* se2wks* se12252 se22252 se12202 se22202 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/  sc1230  pp_earn* pptotin* /*
		*** Training
		*/ sc1658 sc1660 sc1662 sc1664 sc1666 sc1668 sc1670 sc1672 sc1674 sc1676 sc1678 /*
										*** IMPUTATIONS FLAGS *****
		  /* */ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
		 */ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/* */
		*/ pi39  pi16 pi03 pi35 pi26 /* idisab ireasab (reason absent without pay) itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi14 pi02 pi20 pi04 pi20 pi07 pi08 pi19 pi23 pi24 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		
		*/ using sip${yeardata}w`i', clear
		
		cd "$tempdata" 
            *generate id
            egen id=concat(su_id pp_entry pp_pnum)
			
				tab su_rot 
				
		gen rtakjob=sc1042
		replace rtakjob=sc1216 if sc1216!=0 & sc1042==0 
		
		gen rnotake=sc1044
		replace rnotake=sc1218 if sc1218!=0 & sc1044==0
		
		gen rsnnotlkg=sc1054
		replace rsnnotlkg=sc1228 if sc1228!=0 & sc1054==0
		
		gen wantjob=sc1048
		replace wantjob=sc1222 if sc1222!=0 & sc1048==0
		
		replace rtakjob=sc1052 if rtakjob==0 | rtakjob==.
		replace rtakjob=sc1226 if rtakjob==0 | rtakjob==.
		
		ren pi04 inotake
		replace inotake=pi20 if inotake==0 | inotake==.
		
		ren pi08 irsnnotlkg
		replace irsnnotlkg=pi24 if irsnnotlkg==0 | irsnnotlkg==.
		
		ren pi03 itakjob 	// pi3: imputation sc1042
		replace itakjob=pi07 if itakjob==0 | itakjob==.
		replace itakjob=pi19 if itakjob==0 | itakjob==.
		replace itakjob=pi23 if itakjob==0 | itakjob==.
		
		ren ws1imp01 iws1occ
	    ren ws1imp02 iws1ind
	    ren ws1imp03 iws12012
	    ren ws1imp04 iws12024
	    ren se1imp01 ise1occ
	    ren se1imp02 ise1ind 
	    ren se1imp11 ise12260 
	    ren pi02 iwkslok 	// pi2: imputation sc1004-sc1040 
	    
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
	    ren se2imp11 ise22360 
			
* Note ws variables different numbers compared with 1988
	    
*1988 = 1986
*ws2023 = ?
*ws2024 = ?
*ws2025 = ws2024
*ws2029 = ws2030

*Note variables on training differ in number from 88
*1988 = 1986
*sc1670 = sc1660
*sc1672 = sc1662
*sc1674 = sc1668
*sc1676 = ?
*sc1678 = sc1664
*sc1680 = sc1666
*sc1682 = sc1678
*sc1684 = sc1676
*sc1686 = sc1674
*sc1688 = sc1672
*sc1690 = sc1670
*sc1692 = ?

*ws2025 = ws2024
*ws2029 = ws2030


*h1_int1 h2_int1 h3_int1 h4_int1 mising
 * ws1_2046 ws2_2046 ws1_2044 ws2_2044 missing
 
 	rename ws2_2014 ws2_2114
	
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
	*rename ws2_2044 ws2_2144
	*rename ws2_2046 ws2_2146
	
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
	
	rename se12202 se1_2201 
	rename se22202 se2_2301 
	
	rename se22252 se22352

rename sc1678 sc1682
rename sc1676 sc1684
rename sc1674 sc1686
rename sc1672 sc1688
rename sc1670 sc1690	
rename sc1660 sc1670
rename sc1662 sc1672
rename sc1668 sc1674
rename sc1664 sc1678
rename sc1666 sc1680



	    
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
		*/ pp_earn pptotin /*
		*/ ,i(id) j(refmth) string
	rename h_month month
	gen float year=h_year+1900
	
	
	gen yearmonth=ym(year, month)
    
	gen pp_mis_core=.
	
	
            ***** SAVE
      
        *across panel data
        gen panel=19${yeardata}
        gen str2 yearstring="${yeardata}"
        egen personkey=concat(yearstring su_id pp_entry pp_pnum)
                    
	        sort id yearmonth
		compress
		save temp19${yeardata}, replace 	
	
	local i=`i'+1
	
	
	
	local i=2
	while `i'>=2 & `i'<10 {
           display "CORE WAVE `i'"
        cd "$rawdata"

	if `i' == 2 | `i'==6 | `i'==9 { 
		use su_id su_rot pp_intvw pp_pnum pp_entry pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ pp_mis_1 pp_mis_2 pp_mis_3 pp_mis_4 pp_mis_5 /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year/*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1044 sc1042 sc1044 sc1048 sc1052 sc1050 sc1054 sc1216 sc1218 sc1222 sc1224 sc1226 sc1228  sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ wksjb* wkwjob* esr_* wkswop* wkslok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws1_2012 ws1_2014 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2024 ws1_2026 ws1_2028 ws1_2030 /*
		*/ ws2_2002 ws2_2012 ws2_2014 ws2_2016 ws2_2018 ws2_2020 ws2_2030 ws2_2022 ws2_2024  ws2_2026 ws2_2028 /*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se12214 se22214  se12260 se22260 se1amt* se2amt* se12220 se22220 se1ind se2ind se12218 se22218/*
		*/ se12256 se22256 se1occ se2occ se12224 se22224 se12222 se22222 se12226 se12228 se12230 se22226 se22228 se22230 /*
		*/ se12234 se22234 se12232 se22232 se12212 se22212 se1wks* se2wks* se12252 se22252 se12202 se22202 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/  sc1230  pp_earn* pptotin* /*
		*** Training
		*/ sc1658 sc1660 sc1662 sc1664 sc1666 sc1668 sc1670 sc1672 sc1674 sc1676 sc1678 /*
										*** IMPUTATIONS FLAGS *****
		/* */ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
		*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/* */
		*/ pi39  pi16 pi03 pi35 pi26 /* idisab ireasab (reason absent without pay) itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi14 pi02 pi20 pi04 pi20 pi08 pi24 pi07 pi19 pi23 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		
		*/ using sip${yeardata}w`i', clear
	}	
	
		if `i' == 3 | `i'==4 | `i'==5 | `i'==7 | `i'==8 { 
		use su_id su_rot pp_intvw pp_pnum pp_entry pp_wave h1_strat  h2_strat h3_strat h4_strat h1_hsc h2_hsc h3_hsc h4_hsc /*
		*/ fnlwgt_1 fnlwgt_2 fnlwgt_3 fnlwgt_4 h1_wgt h2_wgt h3_wgt h4_wgt /*
		*/ pp_mis_1 pp_mis_2 pp_mis_3 pp_mis_4 pp_mis_5 /*
		*/ h1_month h2_month h3_month h4_month h1_year h2_year h3_year h4_year/*
		*/ h1_metro h2_metro h3_metro h4_metro h1_msa h2_msa h3_msa h4_msa h1_state h2_state h3_state h4_state/*
		*/ sc1044 sc1042 sc1044 sc1048 sc1052 sc1050 sc1054 sc1216 sc1218 sc1222 sc1224 sc1226 sc1228  sc1174 sc1714 sc1238 sc1460 sc1098/*
		*/ wksjb* wkwjob* esr_* wkswop* wkslok* ws1_occ ws2_occ ws1_ind ws2_ind ws1_wks* ws2_wks* /*
		*/ ws1_2002 ws1_2012 ws1_2014 ws1_2016 ws1_2018 ws1_2020 ws1_2022 ws1_2024 ws1_2026 ws1_2028 ws1_2030 /*
		*/ ws2_2002 ws2_2012 ws2_2014 ws2_2016 ws2_2018 ws2_2020 ws2_2022 ws2_2024  ws2_2026 ws2_2028 ws2_2030/*
		  *** Pay Frequency variables not available in 90s
		* ws1_2030 ws1_2032 ws1_2034 ws1_2036 ws1_2038 ws2_2130 ws2_2132 ws2_2134 ws2_2136 ws2_2138
		*/ wksper* ws1_amt* ws2_amt*/*
		*** Following variables vary significantly relative to 1990+. wesr* is equivalent.
		*/ weeksa* weeksl* /*
			*** se variables - relate to self_employment - note need all need checking relative to 2008 wording etc.
		*/ se12214 se22214  se12260 se22260 se1amt* se2amt* se12220 se22220 se1ind se2ind se12218 se22218/*
		*/ se12256 se22256 se1occ se2occ se12224 se22224 se12222 se22222 se12226 se12228 se12230 se22226 se22228 se22230 /*
		*/ se12234 se22234 se12232 se22232 se12212 se22212 se1wks* se2wks* se12252 se22252 se12202 se22202 /*	
		*** Additional variables (consistent with 2015 versions of datasets - assume need checking):
		*/  sc1230  pp_earn* pptotin* /*
		*** Training
		*/ sc1658 sc1660 sc1662 sc1664 sc1666 sc1668 sc1670 sc1672 sc1674 sc1676 sc1678 /*
										*** IMPUTATIONS FLAGS *****
		/* */ ws2imp03 se1imp11 pi12 pi03 pi35 pi28 ws2imp04 se1imp02 se2imp02 /*
		*/ ws1imp02 ws2imp02 ws1imp01 se1imp01 ws2imp01 se2imp01	/* */
		*/ pi39  pi16 pi03 pi35 pi26 /* idisab ireasab (reason absent without pay) itakjob iretird iwksptr FOR MORE EXTENSIVE ANALYSIS, NEED TO INCLUDE IMPUTATION HERE TOO
		*/ pi13 pi14 pi02 pi20 pi04 pi20 pi08 pi24 pi07 pi19 pi23 /* iwksjob iwkswop iwkslok itakjobn 
		*/ ws1imp01 ws1imp02 ws1imp03 ws1imp04 se1imp01 se2imp01  se1imp02 se2imp02 /*  NEED ISE22360 or the related imputation, in the later panels!!!
		*/ ws2imp01 ws2imp02 ws2imp03 ws2imp04 se1imp11 se2imp11 /*
		
		*/ using sip${yeardata}rt`i', clear
		
		
		
	}	
		cd "$tempdata" 
            *generate id
            egen id=concat(su_id pp_entry pp_pnum)

			tab su_rot 
				
				
				
		gen rtakjob=sc1042
		replace rtakjob=sc1216 if sc1216!=0 & sc1042==0 
		
		gen rnotake=sc1044
		replace rnotake=sc1218 if sc1218!=0 & sc1044==0
		
		gen rsnnotlkg=sc1054
		replace rsnnotlkg=sc1228 if sc1228!=0 & sc1054==0
		
		gen wantjob=sc1048
		replace wantjob=sc1222 if sc1222!=0 & sc1048==0
		
		replace rtakjob=sc1052 if rtakjob==0 | rtakjob==.
		replace rtakjob=sc1226 if rtakjob==0 | rtakjob==.
		
		ren pi04 inotake
		replace inotake=pi20 if inotake==0 | inotake==.
		
		ren pi08 irsnnotlkg
		replace irsnnotlkg=pi24 if irsnnotlkg==0 | irsnnotlkg==.
		
		ren pi03 itakjob 	// pi3: imputation sc1042
		replace itakjob=pi07 if itakjob==0 | itakjob==.
		replace itakjob=pi19 if itakjob==0 | itakjob==.
		replace itakjob=pi23 if itakjob==0 | itakjob==.
		
* imputations
			
		ren ws1imp01 iws1occ
	    ren ws1imp02 iws1ind
	    ren ws1imp03 iws12012
	    ren ws1imp04 iws12024
	    ren se1imp01 ise1occ
	    ren se1imp02 ise1ind 
	    ren se1imp11 ise12260 
	    ren pi02 iwkslok 	// pi2: imputation sc1004-sc1040 
	    *ren pi03 itakjob 	// pi3: imputation sc1042
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
	    ren se2imp11 ise22360 
	
* Note ws variables different numbers compared with 1988
	rename ws2_2014 ws2_2114
	
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
	*rename ws2_2044 ws2_2144
	*rename ws2_2046 ws2_2146
	
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
	
	rename se12202 se1_2201 
	rename se22202 se2_2301 
	
	rename se22252 se22352

rename sc1678 sc1682
rename sc1676 sc1684
rename sc1674 sc1686
rename sc1672 sc1688
rename sc1670 sc1690	
rename sc1660 sc1670
rename sc1662 sc1672
rename sc1668 sc1674
rename sc1664 sc1678
rename sc1666 sc1680
	    
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
		*/ pp_earn pptotin pp_mis_/*
		*/ ,i(id) j(refmth) string
	rename h_month month
	gen float year=h_year+1900
	
	ren pp_mis_ pp_mis_core
	
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
			tab pp_wave
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

            *ren sc1042 rtakjob
            *ren sc1044 rnotake
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
ren se1ind  c_tbsind1
ren se2ind c_tbsind2

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
*ren se1_2201 c_ebno1
*ren se2_2301 c_ebno2
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
		*ren sc1692 c_easst11
		
		* Cats do not match to 2008
		
		*ren h_int1 eoutcome
		
		*ren ws1_2044 eunion1
		*ren ws2_2144 eunion2
		*ren ws1_2046 ecntrc1
		*ren ws2_2146 ecntrc2
		
		

*** save it
        sort id yearmonth
        save temp19${yeardata}, replace
		replace yearmonth=yearmonth-4 if su_rot==3 & pp_wave==9
		save temp19${yeardata}core, replace	

}


if $tm_if==1 {

 ******** Note, all TM variables need checking relative to 2008
 
 ******** TOPICAL MODULE 3 - Work history, education, training
 
 ******** Work history 


 local i=3       // contains TM1 with tenure data!
                display _col(5) "ONLY TOPICAL MODULE  `i' ; ONLY WORKHISTORY "
        cd "$rawdata" 
				use su_id su_rot pp_pnum pp_entry pp_wave  pp_intvw /*
				*/ tm8136  tm8138 /* tm8136: worked marked on ISS; tm8138 (employed or self-employed)
				*/ tm8140 tm8142 /* year last worked (if not working now); main reason never worked
				*/ tm8160 /* PE or SE in last work; only if not employed right now?
				*/ tm8162  /* last job in 1983 or 1984?  (USEFUL?)
				*/ tm8164  /*  PE or SE in current job
				*/ tm8166 tm8168 tm8170 tm8172 tm8174 /* properties of current job (subsequently asked in the core wave)
				*/ tm8176 tm8178 /* CURRENT JOB TENURE IN YEARS (8176) and MONTHS (8178)
				*/ tm8182 tm8184 tm8186 tm8188 /* CURRENT JOB: Pay before deductions: p. hours/week/month/year
				*/ tm8200 tm8202 /* CODE DIFFERENT WITH SUBSEQUENT PANELS. 'CURRENT JOB' ongoing? (8200: no=2=not ongoing; 8202:reason) 
				*/ tm8204 tm8206 tm8208 /* checks: over 21; curjb_tenure<=10yrs resp.;  previous job held? (yes/no)
				*/ tm8260 tm8262 tm8264 /* resp: prev jb SE or PE; year started prevjb; year ended prevjb
				*/ t3aocc t3aind t3aocc2 t3aind2 /* topical module occupations and industries: but which ones??!!!
				*/  tm8266  /* PREV JB: hours
				*/ tm8268 tm8270 tm8272 tm8274 /* PREVIOUS JOB: Pay before deductions: p. hours/week/month/year
				*/ tm8278 tm8280 tm8282 tm8284 /* EMPGAP measures
				*/ tm8286 tm8288 tm829* tm83*/* 
				further occupational variables 
				*/ tm8194 tm8196 /* tm8234 tm8236 occupational tenure 
	     	     IMPUTATION FLAGS
				*/ tmimp27 tmimp47 tmimp41 tmimp42 tmimp43 tmimp49/*
				*/ tmimp55 tmimp56 tmimp57 tmimp59 /*
				*/ tmimp6* tmimp7* tmimp80 /*
				*/ tmim153 /* occupation and industry imputation
	     
            */ using sip${yeardata}rt`i', clear
         cd "$tempdata" 
            	
		***tm8228 category 5 (500 to 999) and 6 (1000 or more) missing
            *generate id
            egen id=concat(su_id pp_entry pp_pnum)
			
			sort id

	                ***** REDEFINE SOME VARIABLES
			
	    *ren pp_wave wave
	    
		ren tm8136 tm2_markedworking
		ren tm8138 tm2_curjbse
		
		ren tm8140 yearlastworked
		ren tm8142 rsno2wkwrk
		ren tmimp27 imp_8246 
		
		ren t3aocc2 occprevjb  // only reported if there is a previous job in the work history TM 
		ren t3aind2 indprevjb  // only reported if there is a previous job in the work history TM 
		ren t3aocc occprevjb_nocurrentjb // only reported if there is NO current job in the work history TM 
		ren t3aind indprevjb_nocurrentjb  // only reported if there is NO current job in the work history TM 
		
		gen byte imp_occ=tmim153
		gen byte imp_ind=tmim153
		gen byte imp_occprevjb_nocurrentjb =tmim153
		gen byte imp_indprevjb_nocurrentjb =tmim153
		
		gen tmcurjb_tenure=12*tm8176+tm8178 if ~(tm8176==0 & tm8178==0)
		replace tmcurjb_tenure=. if tmcurjb_tenure==0
		gen imp_tmcurjb_tenure=0 if tmcurjb_tenure!=.
		replace imp_tmcurjb_tenure=1 if (tmimp41==1 | tmimp42==1)
		ren tm8176 tmcurjb_tenure_yr
		ren tm8178 tmcurjb_tenure_mth
		
		
		// occupational tenure
		ren tm8194 tm8236
		ren tm8196 tm8234
		// and imputation
		ren tmimp47 imp_8234
		
		// PAY in jobs
		ren tm8182 tmpay_hr
		ren tm8184 tmpay_wk
		ren tm8186 tmpay_mth
		ren tm8188 tmpay_yr
		
		ren tmimp43 imp_tmpay
		
		
		ren tm8200 tmcurjb_ongoing
		ren tm8202 tmcurjb_rsnend
		ren tmimp49 imp_tmcurjb_rsnend
		ren tm8204 tm_ageover21
		ren tm8206 tmcurjb_10yrless
		ren tm8208 tm_hadprevjb
		
	        
	** FOR ALL CASES THAT CURRENT JOB STARTED *****AFTER 1974****, they ask more questions, including starting date. 
		
		// PREVIOUS JOB
		ren tm8260 prevjbse
		ren tmimp55 imp_prevjbse
		gen startprevjbyr_tm=tm8262 if tm8262<tm8264 & tm8264!=. & tm8262!=0
		replace startprevjbyr_tm=tm8264 if tm8262>tm8264 & tm8264!=0 & tm8262!=.
		gen endprevjbyr_tm=tm8264 if tm8262<tm8264 & tm8264!=. & tm8262!=0
		replace endprevjbyr_tm=tm8262 if tm8262>tm8264 & tm8264!=0 & tm8262!=.
		
		capture drop tm8262
		capture drop tm8264
		
		lab var startprevjbyr_tm "start year job previous to tm3 job"
		lab var endprevjbyr_tm "end year job previous to tm3 job"
	  
		ren tmimp56 imp_startprevjbyr_tm
		ren tmimp57 imp_endprevjbyr_tm
		*cap n format endprevjb_tm %tm
		
		* if start year is strictly after end year
		
		ren tm8266 tm_hrswk
	  
		ren tm8278 tm_empgap_weeks
		ren tm8280 tm_empgap_months
		ren tm8282 tm_empgap_years
		ren tm8284 tm_noempgap
		
		ren tmimp60 imp_tm_empgap
		
		ren tm8286 rsnendprevjb
		ren tmimp61 imp_8272 // tm8272 is rsnendprevjb
		
		// pay previous job
		ren tm8268 tmpay_prevjb_hr
		ren tm8270 tmpay_prevjb_wk
		ren tm8272 tmpay_prevjb_mth
		ren tm8274 tmpay_prevjb_yr 
		ren tmimp59 imp_tmpay_prevjb 
		
		ren tm8288 firstyearlf
		ren tmimp62 imp_8274
		
		
		
		gen alwayswork6m=1 if tm8290==-5
		gen years6mworked=tm8290 if tm8290>0
		ren tmimp63 imp_8276
		gen imp_8278=imp_8276
		
            lab var firstyearlf "year first worked at least 6 months"
            lab var alwayswork6m "Always worked at least six months in every year"
            lab var years6mworked "Number of years worked at least six months"
            
		ren tm8292 tm_part_fulltime_overall
		ren tmimp64 imp_tm_part_fulltime_overall
		
		ren tm8296 notworked6m // since 21years old; asked to all <=65yo, dummy 1=yes, 2=no
		
		ren tm8298 firstbreak6mfr
		ren tm8300 firstbreak6mto
		ren tm8304 rsnfirstbreak6m
		
		ren tmimp65 imp_firstbreak6mfr
		ren tmimp66 imp_firstbreak6mto
		replace imp_firstbreak6mfr=1 if tmimp67==1
		replace imp_firstbreak6mto=1 if tmimp67==1
		ren tmimp68 imp_rsnfirstbreak6m
		
		gen timesnotworked6m=1 if notworked6m==1
		replace timesnotworked6m=1+tm8308 if tm8308>0
		
		ren tm8310 secondbreak6mfr
		ren tm8312 secondbreak6mto
		ren tm8316 rsnsecondbreak6m
		
		
		ren tmimp69 imp_secondbreak6mfr
		ren tmimp70 imp_secondbreak6mto
		replace imp_secondbreak6mfr=1 if tmimp71==1
		replace imp_secondbreak6mto=1 if tmimp71==1
		ren tmimp72 imp_rsnsecondbreak6m
		
		ren tm8318 thirdbreak6mfr
		ren tm8320 thirdbreak6mto
		ren tm8324 rsnthirdbreak6m
		
		
		ren tmimp73 imp_thirdbreak6mfr
		ren tmimp74 imp_thirdbreak6mto
		replace imp_thirdbreak6mfr=1 if tmimp75==1
		replace imp_thirdbreak6mto=1 if tmimp75==1
		ren tmimp76 imp_rsnthirdbreak6m
		
		ren tm8326 fourthbreak6mfr
		ren tm8328 fourthbreak6mto
		ren tm8332 rsnfourthbreak6m
		
		
		ren tmimp77 imp_fourthbreak6mfr
		ren tmimp78 imp_fourthbreak6mto
		replace imp_fourthbreak6mfr=1 if tmimp79==1
		replace imp_fourthbreak6mto=1 if tmimp79==1
		ren tmimp80 imp_rsnfourthbreak6m
		
		
		
		// RECODE RECENT BREAK
							* note that if there is a fifth break this is missed, but we still incorporate the fourth break as if it were the most recent one
							
            *most recent time not worked 6 months from and to
        gen recentbreak6mfr=firstbreak6mfr if timesnotworked6m==1
        gen recentbreak6mto=firstbreak6mto if timesnotworked6m==1
        gen rsnbreak6m=rsnfirstbreak6m if timesnotworked6m==1
		gen imp_recentbreak6mfr=imp_firstbreak6mfr if timesnotworked6m==1
        gen imp_recentbreak6mto=imp_firstbreak6mto if timesnotworked6m==1
        gen imp_rsnbreak6m=imp_rsnfirstbreak6m if timesnotworked6m==1
		
		replace recentbreak6mfr=secondbreak6mfr if timesnotworked6m==2
        replace recentbreak6mto=secondbreak6mto if timesnotworked6m==2
        replace rsnbreak6m=rsnsecondbreak6m if timesnotworked6m==2
		replace imp_recentbreak6mfr=imp_secondbreak6mfr if timesnotworked6m==2
        replace imp_recentbreak6mto=imp_secondbreak6mto if timesnotworked6m==2
        replace imp_rsnbreak6m=imp_rsnsecondbreak6m if timesnotworked6m==2
		
		    
		replace recentbreak6mfr=thirdbreak6mfr if timesnotworked6m==3
        replace recentbreak6mto=thirdbreak6mto if timesnotworked6m==3
        replace rsnbreak6m=rsnthirdbreak6m if timesnotworked6m==3
		replace imp_recentbreak6mfr=imp_thirdbreak6mfr if timesnotworked6m==3
        replace imp_recentbreak6mto=imp_thirdbreak6mto if timesnotworked6m==3
        replace imp_rsnbreak6m=imp_rsnthirdbreak6m if timesnotworked6m==3
		
		replace recentbreak6mfr=fourthbreak6mfr if timesnotworked6m==4
        replace recentbreak6mto=fourthbreak6mto if timesnotworked6m==4
        replace rsnbreak6m=rsnfourthbreak6m if timesnotworked6m==4
	    replace imp_recentbreak6mfr=imp_fourthbreak6mfr if timesnotworked6m==4
        replace imp_recentbreak6mto=imp_fourthbreak6mto if timesnotworked6m==4
        replace imp_rsnbreak6m=imp_rsnfourthbreak6m if timesnotworked6m==4
	    
		gen imp_8290=imp_recentbreak6mfr
		gen imp_8292=imp_recentbreak6mto
		gen imp_8294=imp_rsnbreak6m
		
		// set missing to missing
            mvdecode tm81*, mv(0)
            mvdecode tm82*, mv(0)
	    // set don't knows to missing
			mvdecode tm81*, mv(-1)
            mvdecode tm82*, mv(-1)
	    
		// keep appropriate zeroes
		
		replace tm_noempgap=. if tm_noempgap==0
		replace tm_noempgap=1 if tm_noempgap==-3
		
	    ren pp_intvw intvw_tm2
	    lab var intvw_tm2 "interview status in topical module 2"
	    

		* CAN GENERATE EMPGAP DIRECTLY IN THE 1984 PANEL 
		capture drop empgap
	    gen empgap=0 if tm_noempgap==1
		replace empgap=1 if tm_empgap_weeks>=1 & tm_empgap_weeks<=6
		replace empgap=floor((tm_empgap_weeks+1)/4) if tm_empgap_weeks>7 & tm_empgap_weeks!=. 
		replace empgap=tm_empgap_months if tm_empgap_months  >0 & tm_empgap_months!=.
		replace empgap=12*tm_empgap_years if tm_empgap_years>0 & tm_empgap_years!=.
		
		tab empgap
		
	    capture drop empgap_noimp
	    gen empgap_noimp=empgap
	    replace empgap_noimp=. if imp_tm_empgap==1
		
		gen byte imp_empgap=imp_tm_empgap
		
       // START/END DATES FROM TOPICAL MODULE, EMPGAP
       // we can infer these only partially (in 1984, things work differently from the subsequent panels)
	   
	   // WE DO THIS IN *****retrospective_occmob_1984.do*******, because we want to use the information in the 
	   // entire panel 
	   
									   
									   
									  /*
									   gen tm3_intvw_month=
									   
											gen tsjdate_startpanel=ym(tm8220,tm8218) if eeno1_startpanel!=.|eeno2_startpanel!=.
											lab var tsjdate_startpanel "start data main job in tm2"
											format tsjdate_startpanel %tm
										
										gen byte imp_tsjdate_startpanel_yr=1 if imp_8220==1 
										gen byte imp_tsjdate_startpanel_mth=1 if imp_8218==1 
											
									   * end previous job: we have the end year, but not the end month
											gen endprevjb_tm=.
										* if did not have a job in wave 2 (or wave 1)
										replace endprevjb_tm=ym(tm8242, tm8240) if (tm8242!=0 & tm8240!=0)
										* if had a job in wave 2 (or wave 1) 
											replace endprevjb_tm=ym(tm8250, tm8248) if (tm8250!=0  & tm8248!=0)
											lab var endprevjb_tm "end date job previous to panel"
										
												
											lab var endprevjbyr_tm "end year job previous to panel"
										
										
										gen imp_endprevjb_mth=1 if imp_8240==1 & (tm8242!=0)
										replace imp_endprevjb_mth=1 if imp_8248==1 & (tm8248!=0)	 
										
											* start previous job
											gen startprevjb_tm=.
											replace startprevjb_tm=ym(tm8270, tm8268) if (tm8270!=0 & tm8268!=0)
											lab var startprevjb_tm "start date job previous to panel"
											format startprevjb_tm %tm
										
											
										*imputation		
										gen imp_startprevjb_mth=1 if imp_8268==1 & (tm8268!=0)
										
										*/
		
	expand 2, gen(expandvar)
	replace pp_wave=2 if expandvar==1
	expand 2 if expandvar==1, gen(expandvarsq)
	replace pp_wave=1 if expandvarsq==1
	drop expandvar*
	
	tab pp_wave 
	*/
	    ***** APPEND TO DATA SETS BEFORE AND SAVE
	save temp19${yeardata}tm, replace
	compress
			 

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
	display "_mrgtot==3 individual in the core wave data set and did partake in some TM"
	compress
			
}     

    *** THE BIG MERGE 
cd "$tempdata"
		use testi, clear
		drop if su_id==""
		expand 9, gen(expandvar)
		replace expandvar=0 if expandvar!=1
		sort id expandvar
		gen wave=1 if expandvar==0
		replace wave=wave[_n-1]+1 if expandvar==1
		tab wave 
		drop expandvar
		
		
		
		capture drop _mrgtot1 _mrgtot2 
		merge id wave using testw, sort _merge(_mrgtot1) uniqmaster
		tab _mrgtot1
		rename _mrgtot1 mtst1
		sort id wave 
		merge id wave using testm, _merge(_mrgtot2) sort uniqmaster
		tab _mrgtot2
		rename _mrgtot2 mtst2
		sort id yearmonth

		expand 4 if wave==9, gen(expandvar)
		sort id wave yearmonth
		replace yearmonth=yearmonth[_n-1]+1 if wave==9
		
		
		// ADD ALL THE CORE WAVES TO IT
		capture drop _mrgtot3
		merge id yearmonth using temp19${yeardata}, sort _merge(_mrgtot3) uniqmaster
		tab _mrgtot3
		display "_mrgtot==1 means that the individual appears in the FP somewhere, but not in core/TM waves"
		display "_mrgtot==2 means that the individual appears in the core wave dataset somewhere, but did not in the FP"
		display as error "NOTE: wave 9 is only in the core: if needed, use expand to create an empty wave in the fp dataset "
		display "_mrmtot==3 individual in the core wave data set and FP"
		rename _mrgtot3 mtst3
		*drop if mtst3~=3
		*drop mtst*

        
cd "$outputdata"

replace pp_mis_core=pp_mis if wave==1
*replace rot=su_rot if wave==9
compress
save 19${yeardata}total_raw, replace

capture use 19${yeardata}total_raw
drop if pp_mis==0
count if wpfinwgt==. & wave==1 & pp_mis==1
drop if su_rot==. & wave==1

drop if pp_mis!=1 & wave<9
drop if srefmon==5
drop if wave==9 & mtst3==1



sort personkey yearmonth

capture program drop wave9fillout
program define wave9fillout
		
			replace `1'=`1'[_n-1] if `1'[_n-1]!=. & `1'==. & pp_wave==9 & personkey==personkey[_n-1] 
			
end program wave9fillout
			
// FP variables fillout to wave 9 
wave9fillout fnlwgt84 
wave9fillout fnlwgt85 
wave9fillout hsc 
wave9fillout sc1360 
wave9fillout disab_panel  
wave9fillout sex 
wave9fillout race 
wave9fillout ethnicty 
wave9fillout higrade 
wave9fillout grd_cmp 
wave9fillout eafnow
wave9fillout att_sch
wave9fillout tage

replace tage=tage+1 if tage==tage[_n-13] & pp_wave==9 & personkey==personkey[_n-13]


/*
*** Create reference month variable - Note, 1984 has different ordering of rotations to other panels

capture drop srefmon
gen srefmon=.
forvalues i=1(3)7{
replace srefmon = rhcalmn-5 if rot==1 & wave==`i'
replace srefmon = rhcalmn-6 if rot==2 & wave==`i' 
replace srefmon = rhcalmn-7 if rot==3 & wave==`i'
replace srefmon = rhcalmn-8 if rot==4 & wave==`i'
}

forvalues i=2(3)8{
replace srefmon = rhcalmn+3 if rot==1 & wave==`i'
replace srefmon = rhcalmn-9 if rot==1 & wave==`i' & rhcalmn>=10
replace srefmon = rhcalmn+2 if rot==2 & wave==`i'
replace srefmon = rhcalmn-10 if rot==2 & wave==`i' & rhcalmn>=11
replace srefmon = rhcalmn+1 if rot==3 & wave==`i'
replace srefmon = rhcalmn-11 if rot==3 & wave==`i' & rhcalmn>=12
replace srefmon = rhcalmn if rot==4 & wave==`i'
}

forvalues i=3(3)6{
replace srefmon = rhcalmn-1 if rot==1 & wave==`i'
replace srefmon = rhcalmn-2 if rot==2 & wave==`i' 
replace srefmon = rhcalmn-3 if rot==3 & wave==`i'
replace srefmon = rhcalmn-4 if rot==4 & wave==`i'
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


 
 *********************************************************************8
 ** Topical Module Start Dates/end dates etc.
 *************************************************************************
 
 /* This is more involved in the 1984 panel, however the structure comes back e.g. in the 2008 panel, where last job start and end is just given in years*/	 
 
 
				mvdecode eeno*, mv(0)
				mvdecode eeno*, mv(9)
				mvdecode endprevjbyr*, mv(0)
				mvdecode startprevjbyr*, mv(0)
 // firm identity?
			/* the firm identifiers seem to stick to firm identity "1" too much; for this reason it seems better not to focus too much on them;
			we still want to look at the twice coded occupations; however, to be reasonably sure that prevjb and curjb are indeed picked right, we will only consider  prevjb jobs that end in 1984, and curjb jobs that start in 1984, with retrospective info 
			that is consistent
			*/
 
 // tsjdate_startpanel
 
	capture drop intvw_month
	gen intvw_month=.
	display "${panel}"
	replace intvw_month=ym(${panel},5) if rot==4		
	replace intvw_month=ym(${panel},8) if rot==3		
	replace intvw_month=ym(${panel},7) if rot==2
	replace intvw_month=ym(${panel},6) if rot==1

	/* when runnign from a file that did not adjust empgap yet
			capture drop empgap
	    gen empgap=0 if tm_noempgap==1
		replace empgap=1 if tm_empgap_weeks>=1 & tm_empgap_weeks<=6
		replace empgap=floor((tm_empgap_weeks+1)/4) if tm_empgap_weeks>7 & tm_empgap_weeks!=. 
		replace empgap=tm_empgap_months if tm_empgap_months  >0 & tm_empgap_months!=.
		replace empgap=12*tm_empgap_years if tm_empgap_years>0 & tm_empgap_years!=.
	
	*/
		// set tsjdate_startpanel according to tmcurjb_tenure (however, imprecision, since in cases, tenure is reported in years, meaning that there really is a -6m, +6m interval around this tsjdate_startpanel month)
		mvdecode tmcurjb_tenure_mth tmcurjb_tenure_yr tmcurjb_ongoing, mv(0)
		capture drop tsjdate_startpanel
		capture drop occcurjb_ongoing_dum
		gen tsjdate_startpanel=.
		gen occcurjb_ongoing_dum=.
		replace tsjdate_startpanel=.
		replace occcurjb_ongoing_dum=.
							// need to make sure that job is indeed continuing at the end of wave 3, which means that we will need to also consider this when defining eeno1/2_startpanel
							// also if there issues with interrupted employment spells during tenure, these tsjdate_startpanels are not filled in
		*global ongoingcondition "rot<4 & ((fp_rmesr<6 & pp_wave==3 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==4 & srefmon[_n+1]==1 & personkey==personkey[_n+1] & tmcurjb_ongoing==1 ) | (fp_rmesr<6 & pp_wave==3 & srefmon==4 & personkey!=personkey[_n+1] & tmcurjb_ongoing==1))"
		// without the rot<4 restriction (why did we have it before?)
		global ongoingcondition "((fp_rmesr<6 & pp_wave==3 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==4 & srefmon[_n+1]==1 & personkey==personkey[_n+1] & tmcurjb_ongoing==1 ) | (fp_rmesr<6 & pp_wave==3 & srefmon==4 & personkey!=personkey[_n+1] & tmcurjb_ongoing==1))"
		sort personkey yearmonth
		
		replace occcurjb_ongoing_dum=1 if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & $ongoingcondition
		replace tsjdate_startpanel=intvw_month-tmcurjb_tenure_mth if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & $ongoingcondition
		replace tsjdate_startpanel=intvw_month-12*tmcurjb_tenure_yr if tmcurjb_tenure_yr!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & $ongoingcondition 
		format tsjdate_startpanel %tm
		gsort personkey -yearmonth
		replace tsjdate_startpanel=tsjdate_startpanel[_n-1] if personkey==personkey[_n-1] &  tsjdate_startpanel[_n-1]!=.
		sort personkey -yearmonth
		
		
		
	    
		// set imputation marker to 0 if tmcurjb_tenure can be calculated as above
		capture drop imp_tsjdate_startpanel_yr
		capture drop imp_tsjdate_startpanel_mth
		gen imp_tsjdate_startpanel_yr=0 if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & tsjdate_startpanel!=.
		gen imp_tsjdate_startpanel_mth=0 if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & tsjdate_startpanel!=. 
		
		// adjust tsjdate_startpanel with fp_rmesr unemployment/nlf info: only one month, and only for those with tenure<=12 months
		sort personkey yearmonth
		capture drop tsj_rmesr_adjustment
			* if tsjdate_startdata falls in a nonemployment month, and there is employment in the subsequent month, bump tsjdate_startdate up by a month 
		gen tsj_rmesr_adjustment=1  if tsjdate_startpanel==yearmonth & fp_rmesr>5 & fp_rmesr[_n+1]<=5 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
		tab tsj_rmesr_adjustment
		idfillreplace tsj_rmesr_adjustment
		replace tsjdate_startpanel=tsjdate_startpanel+tsj_rmesr_adjustment if tsjdate_startpanel!=. & tsj_rmesr_adjustment !=. // implied end job date might be one month later (inherent imprecision), trying to offset this by looking at the rmesr series
		
		// incorporate empgap (take into account that empgap+ curjb_tenure might not necessarily give endprevjbyr_tm; mistakes are made!) 
		// note also that, when putting this next to the rmesr measures there might be a(n additional) one-month discrepancy, as the maximum precision is only at the month level
		
		capture drop retro_endprevjb_tm
		capture gen retro_endprevjb_tm=.
		replace retro_endprevjb_tm=tsjdate_startpanel-empgap if empgap!=. & imp_empgap!=1 & tsjdate_startpanel!=. // implied end job date might be one month later (inherent imprecision), trying to offset this by looking at the rmesr series
		format retro_endprevjb_tm %tm
	
		// rmesr adjustment of endprevjb/tsjdate_startpanel, in case there is an empgap: only for those with tenure<=12 months
		sort personkey yearmonth
		capture drop retro_endprevjb_adjustment
		gen retro_endprevjb_adjustment=-1  if retro_endprevjb_tm==yearmonth & fp_rmesr>5 & fp_rmesr[_n-1]<=5 & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
		replace retro_endprevjb_adjustment=1  if retro_endprevjb_tm==yearmonth & empgap>=0 & empgap!=. & fp_rmesr<=5 & fp_rmesr[_n+1]<=5 & fp_rmesr[_n+2]>5 & personkey==personkey[_n+2] & yearmonth==yearmonth[_n+2]-2
		tab retro_endprevjb_adjustment
		idfillreplace retro_endprevjb_adjustment 
		replace retro_endprevjb_tm=retro_endprevjb_tm+retro_endprevjb_adjustment if empgap!=. & imp_empgap!=1 & tsjdate_startpanel!=. & retro_endprevjb_adjustment!=. & retro_endprevjb_tm!=. // implied end job date might be one month later (inherent imprecision), trying to offset this by looking at the rmesr series
		
		 // ------> CHECK RETROSPECTIVE END OF PREVIOUS JOB with REPORTED END YEAR OF PREVIOUS JOB
		 
		// mark when retro_endprevjb and endprevjbyr are consistent; adjust when granularity means that answers are consistent when taking into account imprecision
		capture drop confirmed_endprevjb_ind
		gen confirmed_endprevjb_ind=1 if yofd(dofm(retro_endprevjb_tm))==endprevjbyr_tm & endprevjbyr_tm!=. 
		replace confirmed_endprevjb_ind=0 if yofd(dofm(retro_endprevjb_tm))!=endprevjbyr_tm & endprevjbyr_tm!=. &  retro_endprevjb_tm!=.
		replace confirmed_endprevjb_ind=2 if yofd(dofm(retro_endprevjb_tm+12))==endprevjbyr_tm & endprevjbyr_tm!=. &  retro_endprevjb_tm!=. & confirmed_endprevjb_ind!=1
		replace confirmed_endprevjb_ind=3 if yofd(dofm(retro_endprevjb_tm-12))==endprevjbyr_tm & endprevjbyr_tm!=. &  retro_endprevjb_tm!=. & confirmed_endprevjb_ind!=1
		tab confirmed_endprevjb_ind
		tab endprevjbyr_tm confirmed_endprevjb_ind
		
		// define endprevjb_tm, using retro_endprevjb and endprevjbyr_tm 
		capture drop endprevjb_tm
		capture gen endprevjb_tm=.
		capture replace endprevjb_tm=.
		replace endprevjb_tm=retro_endprevjb_tm if confirmed_endprevjb_ind==1
		// bump up/down endprevjb_tm if within 6 months, but only if curjb_tenure is in years, and there is no logical inconsistency to do so. 
		replace endprevjb_tm=mofd(dofy(endprevjbyr_tm+1))-1 if confirmed_endprevjb_ind==3 & yofd(dofm(retro_endprevjb_tm-7))==endprevjbyr_tm & tmcurjb_tenure>=12
		replace endprevjb_tm=mofd(dofy(endprevjbyr_tm)) if confirmed_endprevjb_ind==2 & yofd(dofm(retro_endprevjb_tm+7))==endprevjbyr_tm & tmcurjb_tenure>=24
		format endprevjb_tm %tm
		// adjust tsjdate_startpanel to keep things in line, but only if empgap is precise, and tmcurjb_tenure is not
		replace tsjdate_startpanel=endprevjb_tm+empgap if confirmed_endprevjb_ind>=2 & confirmed_endprevjb_ind<=3 & endprevjb_tm!=. & empgap<12
		
		tab endprevjbyr_tm retro_endprevjb_tm if yofd(dofm(retro_endprevjb_tm))==1984
		tab endprevjbyr_tm retro_endprevjb_tm if yofd(dofm(retro_endprevjb_tm))==1983
		tab endprevjbyr_tm retro_endprevjb_tm if yofd(dofm(retro_endprevjb_tm))==1982
		
		// come up with a startdate for those previous jobs? 
		 * conservative take: take the smallest possible tenure in the previous job??? CAN RENAME IT STARTPREVJB_TM, but here MAXSTARTPREVJB to signify this conservativeness
		capture drop maxstartprevjb_tm 
		gen maxstartprevjb_tm=mofd(dofy(startprevjbyr_tm+1)-15) if endprevjb_tm!=. 
		replace maxstartprevjb_tm=endprevjb_tm-1 if endprevjb_tm<=maxstartprevjb_tm & maxstartprevjb_tm!=.
		format maxstartprevjb_tm %tm
		
		// minimum prev job tenure
		capture drop tm_minprevjb_tenure
		gen tm_minprevjb_tenure=endprevjb_tm-maxstartprevjb_tm if maxstartprevjb_tm!=. & endprevjb_tm!=. 
		tab tm_minprevjb_tenure
		
		/*
		// TO GET AN IDEA IF WE CAN DO RETROSPECTIVE ANALYSIS
		
		// with empgap<12
		// occ moving unclear
		count if startprevjbyr_tm==endprevjbyr_tm & ((tmcurjb_tenure_yr==tm8236 & tm8236!=. & tm8236!=0)|(tmcurjb_tenure_mth==tm8234 & tm8234!=.& tm8234!=0)) & empgap>0 & empgap<12
		// occ moving in empgap
		count if startprevjbyr_tm!=endprevjbyr_tm & ((tmcurjb_tenure_yr==tm8236 & tm8236!=. & tm8236!=0)|(tmcurjb_tenure_mth==tm8234 & tm8234!=.& tm8234!=0)) & empgap>0 & empgap<12
		// occ staying in empgap
		count if startprevjbyr_tm==endprevjbyr_tm & (tmcurjb_tenure<12 & tm8236!=. & tm8236>=1 & tm8234==.)  & empgap>0 & empgap<12
		count if startprevjbyr_tm!=endprevjbyr_tm & (tmcurjb_tenure<12 & tm8236!=. & tm8236>=1 & tm8234==.) & empgap>0 & empgap<12
		count if startprevjbyr_tm==endprevjbyr_tm & ((tmcurjb_tenure_yr<tm8236 & tm8236!=. & tm8236!=0 & tm8234==.)|(tmcurjb_tenure_mth<tm8234 & tm8234!=. & tm8234!=0 & tm8236==.)) & empgap>0 & empgap<12
		count if startprevjbyr_tm!=endprevjbyr_tm & ((tmcurjb_tenure_yr<tm8236 & tm8236!=. & tm8236!=0 & tm8234==.)|(tmcurjb_tenure_mth<tm8234 & tm8234!=. & tm8234!=0 & tm8236==.)) & empgap>0 & empgap<12
		
		// with empgap<12 & prevjbtenure> x months
		// occ moving in empgap
		count if startprevjbyr_tm!=endprevjbyr_tm & ((tmcurjb_tenure_yr==tm8236 & tm8236!=. & tm8236!=0)|(tmcurjb_tenure_mth==tm8234 & tm8234!=.& tm8234!=0)) & empgap>0 & empgap<12 & startprevjbyr_tm<endprevjbyr_tm
		// occ staying in empgap
		count if startprevjbyr_tm!=endprevjbyr_tm & (tmcurjb_tenure<12 & tm8236!=. & tm8236>=1 & tm8234==.) & empgap>0 & empgap<12 & startprevjbyr_tm<endprevjbyr_tm
		count if startprevjbyr_tm!=endprevjbyr_tm & ((tmcurjb_tenure_yr<tm8236 & tm8236!=. & tm8236!=0 & tm8234==.)|(tmcurjb_tenure_mth<tm8234 & tm8234!=. & tm8234!=0 & tm8236==.)) & empgap>0 & empgap<12 & startprevjbyr_tm<endprevjbyr_tm
		*/
		
		// mark tsjdate_startpanel firm
		
				* need wave start 
				sort personkey yearmonth
				capture drop wavestart
				gen wavestart=yearmonth if pp_wave!=pp_wave[_n-1]
				replace wavestart=wavestart[_n-1] if pp_wave==pp_wave[_n-1] & personkey==personkey[_n-1]
				format wavestart %tm
				
		
				// START DURING TM WAVE 
				capture drop eeno2_startpanel
				
				global ongoingcondition "( fp_rmesr<6 & pp_wave==3 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==4 & srefmon[_n+1]==1 & personkey==personkey[_n+1] ) "
				replace occcurjb_ongoing_dum=1 if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & $ongoingcondition
				// only 1 firm
				gen eeno2_startpanel=eeno1 if tmcurjb_tenure<=4 & pp_wave==3 & eeno1!=. & eeno2==. & ((tsjdate_startpanel<=mofd(tsjdate1+21) & tsjdate_startpanel>=mofd(tsjdate1-21))| tsjdate_startpanel==wavestart ) 
				// two firms
					* it's eeno1
					replace eeno2_startpanel=eeno1 if tmcurjb_tenure<=4 & pp_wave==3 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2 & tejdate2 <tsjdate1 & $ongoingcondition /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate1+21) & tsjdate_startpanel>=mofd(tsjdate1-21))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate2+21) )| tsjdate2==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					& imp_tmcurjb_tenure!=1
						// fill out over the wave 
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==3 & personkey==personkey[_n+1] & pp_wave[_n+1]==3 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==3 & personkey==personkey[_n+1] & pp_wave[_n+1]==3 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==3 & personkey==personkey[_n+1] & pp_wave[_n+1]==3 & eeno2_startpanel[_n+1]!=.
				
					* it's eeno2
					replace eeno2_startpanel=eeno2 if tmcurjb_tenure<=4 & pp_wave==3 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2 & tejdate1 <tsjdate2 & $ongoingcondition /// firm 1 ends before start firm 2, and 
					& ((tsjdate_startpanel<=mofd(tsjdate2+21) & tsjdate_startpanel>=mofd(tsjdate2-21))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate1+21) )| tsjdate1==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					& imp_tmcurjb_tenure!=1
					// fill out over the wave 
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==3 & personkey==personkey[_n+1] & pp_wave[_n+1]==3 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==3 & personkey==personkey[_n+1] & pp_wave[_n+1]==3 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==3 & personkey==personkey[_n+1] & pp_wave[_n+1]==3 & eeno2_startpanel[_n+1]!=.
				
				// PREVIOUS FIRM IS FIRM ENDING DURING WAVE 
						/* this is slightly trickier, because we only know prev firm end year, and it might thus be that there is another firm that also ends in 1984. In particular, we do not know if the other firm in the wave (if any) 
								is indeed the firm that is referred to as previous firm. What can we do about that?
										- consider the case of a seeming firm that does not change occupations during 1984. (problem: this might select occupations which can be clearly coded) 
										- focus on previous firms that end in wave 2 (due to empgap) 
										- below: focus current firms which start in wave 2 (and therefore have limited scope for multiple firms) 
						*/
				// previous jb in previous wave, is covered below. 
				
				//------------------------
				// PP WAVE 2			
				//-----------------------
				
				
				// START IN WAVE BEFORE TM WAVE; since the first month is (for any rotation) in 1983, we only have to consider starting dates inside the panel for eeno2_startpanel analys, though for eeno1_startpanel analysis 1983 tjsdate_st... also useful 
				global ongoingcondition "fp_rmesr[_n+4]<6 & pp_wave[_n+4]==3 & srefmon[_n+4]==4 & fp_rmesr[_n+5]<6 & pp_wave[_n+5]==4 & srefmon[_n+5]==1 & personkey==personkey[_n+5] & fp_rmesr<6 & pp_wave==2 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==3 & srefmon[_n+1]==1 & personkey==personkey[_n+1]"
				global onlyonefirmcondition "( personkey==personkey[_n+4] & eeno2[_n+4]==. )"
				replace occcurjb_ongoing_dum=1 if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & $ongoingcondition
				
							capture drop wave2ongoing
							gen byte wave2ongoing=1 if $ongoingcondition & $onlyonefirmcondition 
							replace wave2ongoing=1 if wave2ongoing[_n+1]==1 & personkey==personkey[_n+1] 
							replace wave2ongoing=1 if wave2ongoing[_n+1]==1 & personkey==personkey[_n+1] 
							replace wave2ongoing=1 if wave2ongoing[_n+1]==1 & personkey==personkey[_n+1] 
				
				// only 1 firm
				replace eeno2_startpanel=. if eeno2_startpanel!=. & tmcurjb_tenure>4  & tmcurjb_tenure<=8  
				replace eeno2_startpanel=eeno1 if tmcurjb_tenure>4  & tmcurjb_tenure<=8  & pp_wave==2 & eeno1!=. & eeno2==.  & tmcurjb_ongoing==1 & $ongoingcondition & $onlyonefirmcondition ///
																							& ((tsjdate_startpanel<=mofd(tsjdate1+21) & tsjdate_startpanel>=mofd(tsjdate1-21))| tsjdate_startpanel==wavestart ) & (tejdate1==.|tejdate1>=dofm(wavestart+4)-7)
				// two firms (back to back)
					* it's eeno1
					replace eeno2_startpanel=eeno1 if tmcurjb_tenure>4 & tmcurjb_tenure<=8 & pp_wave==2 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2  & $onlyonefirmcondition ///
					& (tejdate2 <=tsjdate1+7 | (tejdate2 <=tsjdate1+21 & tsjdate2<=tsjdate1-28 & tsjdate_startpanel>mofd(tsjdate2)))  & $ongoingcondition /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate1+21) & tsjdate_startpanel>=mofd(tsjdate1-21))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate2+21) )| tsjdate2==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					&  (tejdate1==.|tejdate1>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
						// fill out over the wave 
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==2 & personkey==personkey[_n+1] & pp_wave[_n+1]==2 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==2 & personkey==personkey[_n+1] & pp_wave[_n+1]==2 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==2 & personkey==personkey[_n+1] & pp_wave[_n+1]==2 & eeno2_startpanel[_n+1]!=.
				
					* it's eeno2
					replace eeno2_startpanel=eeno2 if tmcurjb_tenure>4 & tmcurjb_tenure<=8 & pp_wave==2 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2  & $onlyonefirmcondition ///
					& (tejdate1 <=tsjdate2+7 | (tejdate1 <=tsjdate2+21 & tsjdate1<=tsjdate2-28 & tsjdate_startpanel>mofd(tsjdate1)))   & $ongoingcondition /// firm 1 ends before start firm 2, and 
					& ((tsjdate_startpanel<=mofd(tsjdate2+21) & tsjdate_startpanel>=mofd(tsjdate2-21))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate1+21) )| tsjdate1==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					& (tejdate2==.|tejdate2>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					// fill out over the wave 
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==2 & personkey==personkey[_n+1] & pp_wave[_n+1]==2 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==2 & personkey==personkey[_n+1] & pp_wave[_n+1]==2 & eeno2_startpanel[_n+1]!=.
						replace eeno2_startpanel=eeno2_startpanel[_n+1] if eeno2_startpanel==. & pp_wave==2 & personkey==personkey[_n+1] & pp_wave[_n+1]==2 & eeno2_startpanel[_n+1]!=.
				
				
				
				
				//--------------------
				// 		WAVE 1
				//-----------------------
				
				
				// --- ROT 1-3 ----
					* rotation folded into ongoingcondition
				sort personkey yearmonth	
				global ongoingcondition " ( rot<4 & fp_rmesr[_n+4]<6 & pp_wave[_n+4]==2 & srefmon[_n+4]==4 & fp_rmesr[_n+5]<6 & pp_wave[_n+5]==3 & srefmon[_n+5]==1 & personkey==personkey[_n+5] & fp_rmesr<6 & pp_wave==1 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==2 & srefmon[_n+1]==1 & personkey==personkey[_n+1]  & fp_rmesr[_n+8]<6 & pp_wave[_n+8]==3 & srefmon[_n+8]==4 & fp_rmesr[_n+9]<6 & pp_wave[_n+9]==4 & srefmon[_n+9]==1 & personkey==personkey[_n+9] )"
				global onlyonefirmcondition "( personkey==personkey[_n+8] & eeno2[_n+4]==. & eeno2[_n+8]==. )"
				replace occcurjb_ongoing_dum=1 if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & $ongoingcondition
				
							capture drop wave1ongoing
							gen byte wave1ongoing=1 if $ongoingcondition & $onlyonefirmcondition 
							replace wave1ongoing=1 if wave1ongoing[_n+1]==1 & personkey==personkey[_n+1] 
							replace wave1ongoing=1 if wave1ongoing[_n+1]==1 & personkey==personkey[_n+1] 
							replace wave1ongoing=1 if wave1ongoing[_n+1]==1 & personkey==personkey[_n+1] 
				
				capture drop eeno1_startpanel
				gen eeno1_startpanel=.
				replace eeno1_startpanel=.
				// only 1 firm 
				replace eeno1_startpanel=eeno1 if tmcurjb_tenure>8  & tmcurjb_tenure<=11  & pp_wave==1 & eeno1!=. & eeno2==.  & tmcurjb_ongoing==1 & $ongoingcondition & $onlyonefirmcondition ///
																							& ((tsjdate_startpanel<=mofd(tsjdate1+31) & tsjdate_startpanel>=mofd(tsjdate1-31)) | tsjdate_startpanel==wavestart ) & (tejdate1==.|tejdate1>=dofm(wavestart+4)-7)
				// two firms (back to back)
						// going back 11 months at most
					* it's eeno1
					replace eeno1_startpanel=eeno1 if tmcurjb_tenure>8 & tmcurjb_tenure<=11 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2  & $ongoingcondition & $onlyonefirmcondition ///
					& (tejdate2 <=tsjdate1+7 | (tejdate2 <=tsjdate1+21 & tsjdate2<=tsjdate1-28 & tsjdate_startpanel>mofd(tsjdate2)))  /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate1+31) & tsjdate_startpanel>=mofd(tsjdate1-31))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate2+21) )| tsjdate2==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					&  (tejdate1==.|tejdate1>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					* it's eeno1, and jobs are clearly sequential 
					replace eeno1_startpanel=eeno1 if eeno1_startpanel==. & tmcurjb_tenure>8 & tmcurjb_tenure<=11 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2  & $ongoingcondition & $onlyonefirmcondition ///
					& (tejdate2 <=tsjdate1 )  /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate1+31) & tsjdate_startpanel>=mofd(tsjdate1-31)) &  tsjdate_startpanel!=wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					&  (tsjdate2==. | tsjdate2<=dofm(wavestart)+3) ///
					&  (tejdate1==.|tejdate1>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					
						// fill out over the wave 
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
				
					* it's eeno2
					replace eeno1_startpanel=eeno2 if tmcurjb_tenure>8 & tmcurjb_tenure<=11 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2 & $ongoingcondition & $onlyonefirmcondition  /// 
					& (tejdate1 <=tsjdate2+7 | (tejdate1 <=tsjdate2+21 & tsjdate1<=tsjdate2-28 & tsjdate_startpanel>mofd(tsjdate1)))   /// firm 1 ends before start firm 2, and 
					& ((tsjdate_startpanel<=mofd(tsjdate2+31) & tsjdate_startpanel>=mofd(tsjdate2-31))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate1+21) )| tsjdate1==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					& (tejdate2==.|tejdate2>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					* it's eeno2, and jobs are clearly sequential 
					replace eeno1_startpanel=eeno2 if eeno1_startpanel==. & tmcurjb_tenure>8 & tmcurjb_tenure<=11 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2  & $ongoingcondition & $onlyonefirmcondition ///
					& (tejdate1 <=tsjdate2 )  /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate2+31) & tsjdate_startpanel>=mofd(tsjdate2-31)) &  tsjdate_startpanel!=wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					&  (tsjdate1==. | tsjdate1<=dofm(wavestart)+3) ///
					&  (tejdate2==.|tejdate2>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					
					
					// fill out over the wave 
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
				
				// --- ROT 4 ----
				sort personkey yearmonth 
				global ongoingcondition " ( rot==4 & fp_rmesr[_n+4]<6 & pp_wave[_n+4]==3 & srefmon[_n+4]==4 & fp_rmesr[_n+5]<6 & pp_wave[_n+5]==4 & srefmon[_n+5]==1 & personkey==personkey[_n+5] & fp_rmesr<6 & pp_wave==1 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==3 & srefmon[_n+1]==1 & personkey==personkey[_n+1] )"
				global onlyonefirmcondition "( personkey==personkey[_n+4] & eeno2[_n+4]==. )"
				replace occcurjb_ongoing_dum=1 if tmcurjb_tenure_mth!=. & tmcurjb_ongoing==1 & imp_tmcurjb_tenure!=1 & $ongoingcondition
				// only 1 firm
				replace eeno1_startpanel=. if eeno1_startpanel!=. & tmcurjb_tenure>4  & tmcurjb_tenure<=8 & $ongoingcondition & $onlyonefirmcondition
				replace eeno1_startpanel=eeno1 if tmcurjb_tenure>8  & tmcurjb_tenure<=11  & pp_wave==1 & eeno1!=. & eeno2==.  & tmcurjb_ongoing==1 & $ongoingcondition & $onlyonefirmcondition ///
																							& ((tsjdate_startpanel<=mofd(tsjdate1+31) & tsjdate_startpanel>=mofd(tsjdate1-31)) | tsjdate_startpanel==wavestart ) & (tejdate1==.|tejdate1>=dofm(wavestart+4)-7)
				// two firms (back to back)
						// going back 11 months at most
					* it's eeno1
					replace eeno1_startpanel=eeno1 if tmcurjb_tenure>4 & tmcurjb_tenure<=8 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2 & $ongoingcondition & $onlyonefirmcondition ///
					& (tejdate2 <=tsjdate1+7 | (tejdate2 <=tsjdate1+21 & tsjdate2<=tsjdate1-28 & tsjdate_startpanel>mofd(tsjdate2)))  & $ongoingcondition /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate1+31) & tsjdate_startpanel>=mofd(tsjdate1-31))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate2+21) )| tsjdate2==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					&  (tejdate1==.|tejdate1>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					* clearly sequential 
					replace eeno1_startpanel=eeno1 if eeno1_startpanel==. & tmcurjb_tenure>8 & tmcurjb_tenure<=11 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2  & $ongoingcondition & $onlyonefirmcondition ///
					& (tejdate2 <=tsjdate1 )  /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate1+31) & tsjdate_startpanel>=mofd(tsjdate1-31)) &  tsjdate_startpanel!=wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					&  (tsjdate2==. | tsjdate2<=dofm(wavestart)+3) ///
					&  (tejdate1==.|tejdate1>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					
					// fill out over the wave 
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
				
					* it's eeno2
					replace eeno1_startpanel=eeno2 if tmcurjb_tenure>4 & tmcurjb_tenure<=8 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2 & $ongoingcondition & $onlyonefirmcondition /// 
					& (tejdate1 <=tsjdate2+7 | (tejdate1 <=tsjdate2+21 & tsjdate1<=tsjdate2-28 & tsjdate_startpanel>mofd(tsjdate1)))   & $ongoingcondition /// firm 1 ends before start firm 2, and 
					& ((tsjdate_startpanel<=mofd(tsjdate2+31) & tsjdate_startpanel>=mofd(tsjdate2-31))| tsjdate_startpanel==wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					& ((tsjdate_startpanel>mofd(tsjdate1+21) )| tsjdate1==dofm(wavestart) ) /// other firm's tsjdate is either out of range of tsjdate_startpanel, or at begin wave
					& (tejdate2==.|tejdate2>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
										* it's eeno2, and jobs are clearly sequential 
					replace eeno1_startpanel=eeno2 if eeno1_startpanel==. & tmcurjb_tenure>8 & tmcurjb_tenure<=11 & pp_wave==1 & eeno1!=. & eeno2!=. & tmcurjb_ongoing==1 & ws1_2014==2 & ws2_2114==2  & $ongoingcondition & $onlyonefirmcondition ///
					& (tejdate1 <=tsjdate2 )  /// firm 2 ends before start firm 1, and 
					& ((tsjdate_startpanel<=mofd(tsjdate2+31) & tsjdate_startpanel>=mofd(tsjdate2-31)) &  tsjdate_startpanel!=wavestart )  /// tsjdate_startpanel coincides with tsjdate1
					&  (tsjdate1==. | tsjdate1<=dofm(wavestart)+3) ///
					&  (tejdate2==.|tejdate2>=dofm(wavestart+4)-7) ///
					& imp_tmcurjb_tenure!=1
					// fill out over the wave 
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
						replace eeno1_startpanel=eeno1_startpanel[_n+1] if eeno1_startpanel==. & pp_wave==1 & personkey==personkey[_n+1] & pp_wave[_n+1]==1 & eeno1_startpanel[_n+1]!=.
				
				/*
				browse personkey yearmonth wave wavestart pp_mis pp_mis_core c_eclwrk1 fp_eclwrk1 fp_rmesr c_rmesr eeno1 eeno1_startpanel eeno2 eeno2_startpanel ///
							startprevjbyr_tm confirmed_endprevjb_ind endprevjb_tm endprevjbyr_tm tmcurjb_ongoing tmcurjb_tenure retro_endprevjb_tm ///
							empgap tsjdate1 tsjdate2 tsjdate_startpanel tejdate1 tejdate2 rot su_id pp_entry * ///
							if tsjdate_startpanel!=. & tmcurjb_tenure<12 & tmcurjb_tenure>8 & imp_tmcurjb_tenure!=1 & tmcurjb_ongoing==1 // prevjb_startpanel  startprevjb_tm tsjdate_startpanel_precise endprevjb_tm_p retro_endprevjb_insample 
				*/
				
				//--------------------
				// 		START BEFORE PANEL 
				//-----------------------
				
				// ROT 1-3 
				global past_ongoingcondition " ( rot<4 & fp_rmesr[_n-4]<6 & pp_wave[_n-4]==2 & srefmon[_n-4]==4 & fp_rmesr[_n-3]<6 & pp_wave[_n-3]==3 & srefmon[_n-3]==1 & personkey==personkey[_n-4] & fp_rmesr[_n-8]<6 & pp_wave[_n-8]==1 & srefmon[_n-8]==4 & fp_rmesr[_n-7]<6 & pp_wave[_n-7]==2 & srefmon[_n-7]==1 & personkey==personkey[_n-8]  & fp_rmesr[_n-11]<6 & pp_wave[_n-11]==1 & srefmon[_n-11]==1 & personkey==personkey[_n-11] )"
				global cur_ongoingcondition 	"(( fp_rmesr<6 & pp_wave==3 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==4 & srefmon[_n+1]==1 & personkey==personkey[_n+1] ) "
				global cur_ongoingcondition 	"(( fp_rmesr<6 & pp_wave==3 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==4 & srefmon[_n+1]==1 & personkey==personkey[_n+1] ) | ( fp_rmesr<6 & pp_wave==3 & srefmon==4 & personkey!=personkey[_n+1] )) "
				global onlyonefirmcondition "( personkey==personkey[_n-8] & eeno2[_n-8]==. & eeno2[_n-4]==. )"
				
				sort personkey yearmonth 
				// only firm in wave 3
				replace eeno1_startpanel=eeno1 if tmcurjb_tenure>=12 & pp_wave==3 & eeno1!=. & eeno2==. & tmcurjb_tenure!=.  & $cur_ongoingcondition & $onlyonefirmcondition // & $past_ongoingcondition 
				// two firms in wave 3, eeno1 firm stops, eeno2 firm starts 
				replace eeno1_startpanel=eeno1 if tmcurjb_tenure>=12 & pp_wave==3 & eeno1!=. & eeno2!=. & ws1_2014==2  & tmcurjb_tenure!=. & $past_ongoingcondition & $onlyonefirmcondition ///
																		& tsjdate1==dofm(wavestart) & tejdate1<dofm(wavestart+4)-1 & ws2_2114==2 & tejdate2>tejdate1 & tsjdate2>tejdate1-14
				replace eeno1_startpanel=eeno2 if tmcurjb_tenure>=12 & pp_wave==3 & eeno1!=. & eeno2!=. & ws1_2014==2  & tmcurjb_tenure!=. & $past_ongoingcondition & $onlyonefirmcondition ///
																		& tsjdate2==dofm(wavestart) & tejdate2<dofm(wavestart+4)-1 & ws2_2114==2 & tejdate1>tejdate2 & tsjdate1>tejdate2-14
				
				
				// ROT 4 
				global past_ongoingcondition " ( rot==4 & fp_rmesr[_n-4]<6 & pp_wave[_n-4]==1 & srefmon[_n-4]==4 & fp_rmesr[_n-3]<6 & pp_wave[_n-3]==3 & srefmon[_n-3]==1 & personkey==personkey[_n-4] & fp_rmesr[_n-7]<6 & pp_wave[_n-7]==1 & srefmon[_n-7]==1 & personkey==personkey[_n-7] )"
				global cur_ongoingcondition 	"( fp_rmesr<6 & pp_wave==3 & srefmon==4 & fp_rmesr[_n+1]<6 & pp_wave[_n+1]==4 & srefmon[_n+1]==1 & personkey==personkey[_n+1] ) "
				global onlyonefirmcondition "( personkey==personkey[_n-4] & eeno2[_n-4]==. )"
				
				sort personkey yearmonth 
				// only firm in wave 3
				replace eeno1_startpanel=eeno1 if tmcurjb_tenure>8 & pp_wave==3 & eeno1!=. & eeno2==. & tmcurjb_tenure!=. & $past_ongoingcondition & $cur_ongoingcondition & $onlyonefirmcondition
				// two firms in wave 3, eeno1 firm stops, eeno2 firm starts 
				replace eeno1_startpanel=eeno1 if tmcurjb_tenure>8 & pp_wave==3 & eeno1!=. & eeno2!=. & ws1_2014==2  & tmcurjb_tenure!=. & $past_ongoingcondition & $onlyonefirmcondition ///
																		& tsjdate1==dofm(wavestart) & tejdate1<dofm(wavestart+4)-1 & ws2_2114==2 & tejdate2>tejdate1 & tsjdate2>tejdate1-14
				replace eeno1_startpanel=eeno2 if tmcurjb_tenure>8 & pp_wave==3 & eeno1!=. & eeno2!=. & ws1_2014==2  & tmcurjb_tenure!=. & $past_ongoingcondition & $onlyonefirmcondition ///
																		& tsjdate2==dofm(wavestart) & tejdate2<dofm(wavestart+4)-1 & ws2_2114==2 & tejdate1>tejdate2 & tsjdate1>tejdate2-14
				
				// filling out
				replace eeno1_startpanel=eeno1_startpanel[_n+1] if pp_wave==pp_wave[_n+1] & personkey==personkey[_n+1] & eeno1_startpanel==. & eeno1_startpanel[_n+1]!=.
				replace eeno1_startpanel=eeno1_startpanel[_n+1] if pp_wave==pp_wave[_n+1] & personkey==personkey[_n+1] & eeno1_startpanel==. & eeno1_startpanel[_n+1]!=.
				replace eeno1_startpanel=eeno1_startpanel[_n+1] if pp_wave==pp_wave[_n+1] & personkey==personkey[_n+1] & eeno1_startpanel==. & eeno1_startpanel[_n+1]!=.
 
 
        
        // dropping and further renaming
        capture ren att_sch eenrlm
        capture drop personkeytemp
        capture ren sc1360 eeveret
        format yearmonth %tm
        
         replace pnlwgt=pnlwgt*10000
         replace fnlwgt84=fnlwgt84*10000
         replace fnlwgt85=fnlwgt85*10000
         replace whfnwgt=whfnwgt*10000
         replace wpfinwgt=wpfinwgt*10000
        
        
        // missing variable redefine
        mvdecode _all, mv(-1)
        mvdecode _all, mv(-9)
        mvdecode _all, mv(-3)

        capture mvdecode eeveret tmetro tmsa rwksperm rtakjob rnotake edisab, mv(0)
        capture mvdecode c_epayhr* c_tpyrate* c_rpyper* ersend* grd_cmp, mv(0) // estlemp* 
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

     mvdecode eeno*, mv(0)
	 mvdecode eeno*, mv(9)
	 mvdecode intvw_tm2 occprevjb_nocurrentjb indprevjb_nocurrentjb occprevjb indprevjb tm2_markedworking tm2_curjbse yearlastworked rsno2wkwrk tm8160 tm8162 tm8164 tm8166 tm8168 tm8170 ///
					tm8172 tm8174 tmcurjb_tenure_yr tmcurjb_tenure_mth tmpay_hr tmpay_wk tmpay_mth tmpay_yr tm8236 tm8234 tmcurjb_ongoing tmcurjb_rsnend tm_ageover21 tmcurjb_10yrless ///
					tm_hadprevjb prevjbse startprevjbyr_tm tm_hrswk tmpay_prevjb_hr tmpay_prevjb_wk tmpay_prevjb_mth tmpay_prevjb_yr tm_empgap_weeks tm_empgap_months tm_empgap_years ///
					tm_noempgap rsnendprevjb firstyearlf tm8290 tm_part_fulltime_overall tm8294 notworked6m firstbreak6mfr firstbreak6mto rsnfirstbreak6m tm8306 tm8308 secondbreak6mfr ///
					secondbreak6mto rsnsecondbreak6m thirdbreak6mfr thirdbreak6mto rsnthirdbreak6m fourthbreak6mfr fourthbreak6mto rsnfourthbreak6m, mv(0)
	  mvdecode endprev*, mv(0)
	 
	 
	 
	 cd "$outputdata"


save 19${yeardata}total_v${fileversion}, replace

capture log close
