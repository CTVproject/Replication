
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
***
*** OCCUPATIONAL MOBILITY - UNEMPLOYMENT DURATION PROFILES in MODEL vs DATA
***	SURVIVAL AND HAZARD PROFILES in MODEL vs DATA
***
********************************************************************************


/* FILE CONTAINS THE CODE TO DRAW THE DURATION PROFILES FOR MOBILITY AND SURVIVAL,
	THE DURATION SHIFT PICTURE (FIG 6, MAIN TEXT) AND THE EXCESS vs NET MOBILITY 
	PROFILE (FIG 3A, ONLINE APPENDIX)
*/


	clear

	cd "$workingdir"
	global workingdir `c(pwd)'

	do "${workingdir}/global_paths.do"
	
	version 13
	
	set more off
	set varabbrev off

	
	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"
/*	

********************************************************************************
**
** LOAD IN THE EMPIRICAL DURATION AND SURVIVAL PROFILES, PREPARE FOR PICS*******
**
********************************************************************************


** prepare occ.mob/u-duration profile (all, young, prime)	 
use "${outputdata}/durationprofiles_mog.dta", clear	


ren occmob_mog_corr cmdur_d 
ren occmob_mog_corr_yng cmdury_d 
ren occmob_mog_corr_prm cmdurp_d 
ren occmob_mog_raw mdur_d 
ren occmob_mog_raw_yng  mdury_d 
ren occmob_mog_raw_prm  mdurp_d 

keep u_duration cmdur_d cmdury_d cmdurp_d mdur_d mdury_d mdurp_d
save "${tempdata}/profiles1.dta", replace


** prepare cyclical occ.mob/u-duration profile (high u, low u times)
use "${outputdata}/cycldurationshift_mog.dta", clear	

ren duration u_duration 
ren mobduration_rec badmdur_d 
ren mobduration_exp gdmdur_d 

save "${tempdata}/profiles2.dta", replace


** prepare survival profiles
use "${mainresultsdir}/survivalprofiles_and_hazards.dta", clear	


ren surv_au surv_d 
ren surv_yu survy_d 
ren surv_pu survp_d 
ren surv_au_stay_inferred stsurv_d 
ren surv_yu_stay_inferred stsurvy_d 
ren surv_pu_stay_inferred stsurvp_d 
ren surv_au_move_inferred mvsurv_d 
ren surv_yu_move_inferred mvsurvy_d 
ren surv_pu_move_inferred mvsurvp_d 

ren month u_duration

save "${tempdata}/profiles3.dta", replace


*ren badnspel_d 
*ren gdnspel_d 
*/
********************************************************************************
**
** DURATION PROFILES OF THE FULL (NET AND GROSS MOBILITY) CALIBRATION **********
**
********************************************************************************
	 
	 ** load in the model generated data 
	 import delimited "${fortrandir}/Gross and Net Mobility version/profiles10_000.csv", numericcols(66) clear 
	 global lversion netmob
	 

	 
	 
	 
	 /*
	 cap drop cmdur_d 
	 cap drop cmdury_d 
	 cap drop cmdurp_d 
	 cap drop mdur_d 
	 cap drop mdury_d 
	 cap drop mdurp_d 
	 cap drop badmdur_d 
	 cap drop gdmdur_d 
	 cap drop badnspel_d 
	 cap drop gdnspel_d 
	 cap drop surv_d 
	 cap drop survy_d 
	 cap drop survp_d 
	 cap drop stsurv_d 
	 cap drop stsurvy_d 
	 cap drop stsurvp_d 
	 cap drop mvsurv_d 
	 cap drop mvsurvy_d 
	 cap drop mvsurvp_d
	 
	 capture drop _merge
	 merge 1:1 u_duration using "${tempdata}/profiles1.dta"
	 capture drop _merge
	 merge 1:1 u_duration using "${tempdata}/profiles2.dta"
	 capture drop _merge
	 merge 1:1 u_duration using "${tempdata}/profiles3.dta"
	 
	 
	 
	 */
	 cap n destring _all, replace 
	 ren month u_duration
	 capture drop _merge
	 merge 1:1 u_duration using "${outputdata}/durationprofiles_mog.dta"
	 cap gen duration=u_duration
	 capture drop _merge
	 merge 1:1 duration using "${mainresultsdir}/cycldurationshift_mog.dta"
	 cap drop duration
	 
	 
	 ** set info on which durations are targeted in the calibration
	capture drop target_all
	gen byte target_all=0
	replace target_all=1 if u_duration<=12 & (u_duration==1 |u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	
	capture drop target_yngprm
	gen byte target_yngprm=0
	replace target_yngprm=1 if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	
	capture drop target_badtimes
	gen byte target_badtimes=0
	replace target_badtimes=1 if u_duration<=12 & u_duration>=1
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	replace target_goodtimes=1 if u_duration<=8 & u_duration>=1
	
	 
	 
	** some smoothing to deal with noise 
	 capture drop lows_cmdur*
	 capture drop lows_mdur*
	 
		capture program drop lowess_grossmob_exe
		program define lowess_grossmob_exe
					args name dur

		capture drop lows_`name'
		lowess `name' u_duration if u_duration<=`dur', gen(lows_`name') bwidth(0.5) nograph
	
		
		end 

		lowess_grossmob_exe cmdur_m 16
		lowess_grossmob_exe cmdur_d 16
		lowess_grossmob_exe mdur_m 16
		lowess_grossmob_exe mdur_d 16
		lowess_grossmob_exe cmdury_m 16
		lowess_grossmob_exe cmdury_d 16
		lowess_grossmob_exe cmdurp_m 16
		lowess_grossmob_exe cmdurp_d 16
		lowess_grossmob_exe mdury_m 16
		lowess_grossmob_exe mdury_d 16
		lowess_grossmob_exe mdurp_m 16
		lowess_grossmob_exe mdurp_d 16
		
		lowess_grossmob_exe occmob_mog_raw 16
		lowess_grossmob_exe occmob_mog_corr 16
		lowess_grossmob_exe occmob_mog_corr_yng 16
		lowess_grossmob_exe occmob_mog_corr_prm 16
		lowess_grossmob_exe occmob_mog_raw_yng 16
		lowess_grossmob_exe occmob_mog_raw_prm 16

		
		
		
		capture program drop lpoly_grossmob_exe
		program define lpoly_grossmob_exe
					args name dur //ind

		capture drop lpoly_`name'
		lpoly `name' u_duration if u_duration<=`dur' , gen(lpoly_`name') degree(2) at(u_duration) bwidth(2) nograph
	
		
		end 

		lpoly_grossmob_exe cmdur_m 16
		lpoly_grossmob_exe cmdur_d 16
		lpoly_grossmob_exe mdur_m 16
		lpoly_grossmob_exe mdur_d 16 
		lpoly_grossmob_exe cmdury_m 16
		lpoly_grossmob_exe cmdury_d 16
		lpoly_grossmob_exe cmdurp_m 16
		lpoly_grossmob_exe cmdurp_d 16
		lpoly_grossmob_exe mdury_m 16
		lpoly_grossmob_exe mdury_d 16
		lpoly_grossmob_exe mdurp_m 16
		lpoly_grossmob_exe mdurp_d 16
		
	 
		
		** save the targets
		preserve
		keep u_duration lows_occmob* target* mobduration*
		save "${mainresultsdir}/xtra_mobdurprofile_targets.dta", replace
		restore
		
		
	/*
	PICTURE OF THE OVERALL OCC MOBILITY  DURATION PROFILE 
	*/

	#delimit ;				
			scatter occmob_mog_raw mdur_m  mdur_m u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank dash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gs6 gs6) mlcolor(gs6 gs6 gs6) lcolor(gs6 gs6 gs6)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70)) graphregion(color(white)) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.4)
			/* TARGETS */
			|| scatter lows_occmob_mog_raw mdur_m  u_duration if u_duration<=12 & (u_duration==1 |u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12),
			connect(i i) lwidth(none ) lpattern(black) msymbol(Oh O) msize(vlarge medlarge) mlwidth(thick) mfcolor(gs6 black) mlcolor(black black)   
			||
			
			scatter occmob_mog_corr cmdur_m  cmdur_m u_duration if u_duration<=12,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gold gold) mlcolor(gold gold gold) lcolor(gold gold gold)  
			/* CORRECTED PROFILE */
			legend(order(4 5 1 3  6 8) label(4 "Target, Data") label(5 "Model") label(1 "Uncorrected Mobility, Data") label(3 "Model")
			label(6 "Mobility, Data") label(8 "Model") ring(0) size(*1.3) symxsize(*0.4) colgap(2))  xtitle("Months in Unemployment", size(vlarge)) ytitle("Proportion Moving Occupation" "(MOG) Upon Exit U", size(vlarge) height(12))
			;
	#delimit cr	
	graph export "${mainresultsdir}/fig5a.pdf", as(pdf) replace
	
	
	
	
	
	/*
	PICTURE OF THE AGE-SPECIFIC OCC MOBILITY DURATION  PROFILE 
	*/

	
	
	#delimit ;		
	/* --- YOUNG ---- */	
			scatter occmob_mog_corr_yng cmdury_m cmdury_m u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank longdash) msymbol(dh i i ) msize(medium medium) mlwidth(medthick) mfcolor(midgreen midgreen) mlcolor(midgreen midgreen midgreen) lcolor(midgreen midgreen midgreen)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70)) graphregion(color(white))  graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.4)
			
			
			/* TARGETS */
			|| scatter lows_occmob_mog_corr_yng cmdury_m u_duration if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12),
			connect(i i) lwidth(none ) lpattern(black) msymbol(Dh D) msize(vlarge large) mlwidth(thick) mfcolor(midgreen dkgreen) mlcolor(dkgreen dkgreen)   
			
	/* --- PRIME ---- */
			||
			scatter occmob_mog_corr_prm cmdurp_m  lpoly_cmdurp_m u_duration if u_duration<=12,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(navy navy) mlcolor(navy navy navy) lcolor(navy navy navy)  
			/* TARGETS */
			|| scatter lows_occmob_mog_corr_prm cmdurp_m  u_duration if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12),
			connect(i i) lwidth(none ) lpattern(black) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(navy dknavy) mlcolor(dknavy dknavy)   
			legend(order(- "Young Unemployed" - " Prime-Aged " 4  9  5 10  3  8) label(4 "Target - Data") label(5 "Target - Model") label(1 "Mobility - Data") label(3 "Mobility - Model")
			label(6 "Mobility - Data") label(8 "Mobility - Model") label(9 "Target - Data ") label(10 "Target - Model ") ring(0) pos(5) size(*1.3) symxsize(*0.5)) xtitle("Months in Unemployment", size(vlarge))
			;
			
	#delimit cr
	graph export "${mainresultsdir}/fig5b.pdf", as(pdf) replace
	

	
	
	
	
	/*
	PICTURE OF THE CYCLICAL SHIFT OF THE OCC MOB DURATION PROFILE 
	*/


		lpoly_grossmob_exe badcmdur_m	12   // consider 12 months for high U times
		lpoly_grossmob_exe badcmdur_d   12
		lpoly_grossmob_exe gdcmdur_m	9	 // consider 9 months for low U times
		lpoly_grossmob_exe gdcmdur_d    9
		lowess_grossmob_exe badcmdur_m	 12
		lowess_grossmob_exe badcmdur_d   12
		lowess_grossmob_exe gdcmdur_m	 9
		lowess_grossmob_exe gdcmdur_d    9
		
		
		 

	#delimit ;				
			scatter mobduration_rec badcmdur_m  lows_badcmdur_m  u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank shortdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gs4 gs4) mlcolor(gs4 gs4 gs4) lcolor(gs4 gs4 gs4)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70))  graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.4)
			/* TARGETS */
			|| scatter mobduration_rec badcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=12),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs4 black) mlcolor(black black)   
			||
			scatter mobduration_exp gdcmdur_m  lows_gdcmdur_m  u_duration if u_duration<=9,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(sh i i ) msize(medium medium) mlwidth(medthick) mfcolor(midblue midblue) mlcolor(midblue midblue midblue) lcolor(midblue midblue midblue)  
			/* TARGETS */
			/* TARGETS */
			|| scatter mobduration_exp gdcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=8),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Sh S) msize(vlarge large) mlwidth(thick) mfcolor(midblue edkblue) mlcolor(edkblue edkblue)   
			legend(order(- "Times of High U" - " Times of Low U " 4    9  5 10 ) label(4 "Target - Data") label(5 "Target - Model ") label(1 "Corr. Mob. Data") label(3 "Corr. Mob. Model")
			label(6 "Corr. Mob. Data") label(8 "Corr. Mob. Model") label(9 "Target - Data") label(10 "Target - Model") ring(0) size(*1.3) symxsize(*0.5)) xtitle("Months in Unemployment", size(vlarge))
			;
	#delimit cr		
	graph export "${mainresultsdir}/fig5c.pdf", as(pdf) replace
		
		
	

quietly {
cap log close mobdurprofilelog1
log using "${mainresultsdir}/xtra_mobdurprofile_grossnet_calibration.txt", replace text name(mobdurprofilelog1)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "OCC MOBILITY - UNEMPL DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_raw mean mdur_m mean target_all)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "YOUNG: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_yng mean cmdury_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PRIME: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_prm mean cmdurp_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "HIGH U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean mobduration_rec  mean badcmdur_m mean target_badtimes)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "LOW U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=8, c(mean mobduration_exp  mean gdcmdur_m mean target_goodtimes)
noisily: display ""
  
 
log close mobdurprofilelog1
	

}		
		
		


quietly {
cap log close mobdurprofilelogyp1
log using "${mainresultsdir}/table2_occmob_rel_y_p_moment_model_data", replace text name(mobdurprofilelogyp1)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "OCC MOBILITY - *RELATIVE PROFILES* YOUNG/PRIME-AGED (MODEL & DATA TARGET )"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " we use the average value (averaged over the first 12 months of duration"
noisily: display  " of the young vs the prime-aged UNCORRECTED profile as an additional "
noisily: display  " calibration target (mainly for emphasis)"

version 13
 
noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE YOUNG (DATA)"
noisily: display  ""
noisily: su occmob_mog_raw_yng if u_duration<=12
local ave_mprofile_young_data= r(mean)
noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE PRIME (DATA)"
noisily: display  ""

noisily: su occmob_mog_raw_prm if u_duration<=12
local ave_mprofile_prime_data= r(mean)

noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE YOUNG (MODEL)"
noisily: display  ""
noisily: su mdury_m if u_duration<=12
local ave_mprofile_young_model= r(mean)
noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE PRIME (DATA)"
noisily: display  ""

noisily: su mdurp_m if u_duration<=12
local ave_mprofile_prime_model= r(mean)


noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "RELATIVE PROFILES Y vs P (MODEL -- DATA)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
local rel_y_p_mprofile_model=`ave_mprofile_young_model'/`ave_mprofile_prime_model'
local rel_y_p_mprofile_data=`ave_mprofile_young_data'/`ave_mprofile_prime_data'
noisily: display  " model:  `rel_y_p_mprofile_model'; vs data: `rel_y_p_mprofile_data' "
 
log close mobdurprofilelogyp1
	

}		
		
			
	
//==================================
//  DURATION DISTRIBUTION SHIFT BY PTILES
//===================================	
		
		
		
		
		sort u_duration
		capture drop n_spellength
		gen n_spellength=u_duration
		
		
		
		
		capture drop badnspel_d_inv 
		capture drop gdnspel_d_inv 
		capture drop badcm_lb_d 
		capture drop badcm_ub_d 
		capture drop gdcm_lb_d 
		capture drop gdcm_ub_d 
		capture drop badcmdur_sm_d 
		capture drop gdcmdur_sm_d 
		capture drop badcmdur_d 
		capture drop gdcmdur_d
		
		capture drop _merge
		merge 1:1 n_spellength using "${mainresultsdir}/fig6data_cycldurshift_ptiles.dta"
		
	
		*capture drop gdnspel_d_inv
		*capture drop badnspel_d_inv
		*gen gdnspel_d_inv=(1.0-gdnspel_d)*100
		*gen badnspel_d_inv=(1.0-badnspel_d)*100
		
		capture drop gdnspel_m_inv
		capture drop badnspel_m_inv
		gen gdnspel_m_inv=(1.0-gdnspel_m)*100
		gen badnspel_m_inv=(1.0-badnspel_m)*100
	
	
		*su gdnspel*
		
		
		sort gdnspel_d_inv
		#delimit ;				
			twoway rarea gdcm_lb_d gdcm_ub_d gdnspel_d_inv if u_duration<=9, color(midblue%10) 
			||
			rarea badcm_lb_d badcm_ub_d badnspel_d_inv if u_duration<=12, color(gs8%10) 
			||
			scatter badcmdur_m  badnspel_m_inv if u_duration<=12,
			connect(l) lwidth(medthick) lpattern(solid) msymbol(o i i ) msize(medium medium) mlwidth(medthick) /*xsc(reverse)*/ lcolor(black) mcolor(black)
			||
			scatter badcmdur_d badnspel_d_inv if u_duration<=12, 
			connect(i l l) lwidth(none medthin medthin) lpattern(blank longdash longdash) msymbol(oh i i) msize(medlarge medium) mlwidth(medthick) mfcolor(black black) mlcolor(black black black) lcolor(black black black)  ylab(0.1(0.1)0.7, labsize(medlarge)) ysc(r(0.2 0.7)) graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(4) ysize(4) /*  xsc(reverse) */
			legend(ring(0) position(5))
			
			/* TARGETS */
			||
			scatter gdcmdur_m  gdnspel_m_inv if u_duration<=9,
			connect(l l) lwidth(medthick) lpattern(longdash) msymbol(s i i ) msize(medium medium) mlwidth(medthick) lcolor(midblue) mcolor(midblue) /*xsc(reverse) */
			||
			scatter gdcmdur_d gdnspel_m_inv if u_duration<=9, 
			connect(i ) lwidth(none ) lpattern(blank ) msymbol(sh i i) msize(medlarge medlarge) mlwidth(medthick) mfcolor(midblue) mlcolor(midblue) lcolor(midblue) mcolor(midblue) 
			legend(order(- "{bf:Times of Low U}" - "{bf:Times of High U}"  6 4     5 3 1 2      ) label(4 "Occ Mobility, Data ") label(6 "Occ Mobility, Data ") label(2 "   95% CI") label(1 "    95% CI")
			label(3 "Mobility, Model") label(5 "Mobility, Model") label(9 "Target - Data") label(10 "Target - Model") size(*1.1) keygap(0) symxsize(*0.35)) xtitle("Percentile of U. Duration Distribution", size(medlarge)) ytitle("Prop. Moving Occupation (MOG) Upon Exit U", size(medlarge))
			;
			graph export "${mainresultsdir}/fig6.pdf", as(pdf) replace;
			#delimit cr		

			

//==========================
//  SURVIVAL PROFILES
//==========================	


capture drop month
gen month=u_duration
capture drop _merge
merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
drop month



sort u_duration
#delimit ;				
		scatter surv_m surv_au u_duration if u_duration<=20,
		connect(l) lwidth(thick) lpattern(longdash) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(gs6 gs6) mlcolor(gs6 gs6 gs6) lcolor(gs6 gs6 gs6)  xlab(2(4)20, labsize(large))   ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1)) graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.35) ysize(1.2)
		/* TARGETS */
		|| scatter surv_au surv_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs6 gs4) mlcolor(black black)
		/*legend(order(- "RHS axis - Linear" - " LHS axis - Log" 3    7  4 8 ) label(3 "Target - Data") label(4 "Target - Model ") label(7 "Target - Data") label(8 "Target - Model ") position(1) size(*1) symxsize(*0.5)) */
		legend(order(3 4) label(3 "Target, Data") label(4 "Target, Model ") label(7 "Target - Data") label(8 "Target - Model ") position(2) size(*1.4) symxsize(*0.5) ring(0) cols(1) ) 
		
		|| 
		scatter surv_m surv_au u_duration if u_duration<=20,
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.015 1.0) axis(2))  connect(l) lwidth(thick) lpattern(longdash) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(gs6%30 gs6%30) mlcolor(gs6%30 gs6%30 gs6%30) lcolor(gs6%30 gs6%30 gs6%30)  xlab(2(4)20, labsize(large))  
		/* TARGETS */
		|| scatter surv_au surv_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		yaxis(2) connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs6%30 gs4%30) mlcolor(black%30 black%30)
		//||
		//scatter surv_m u_duration if u_duration<=20 & (u_duration==1 | u_duration==20), connect(l) lwidth(medthick) lpattern(dot) lcolor(gs6) msymbol(i) yaxis(2)
		ytitle("Survival in Unemployment", axis(1) size(vlarge)) ytitle("", axis(2)) /* ytitle("Survival, log scale (lighter colors)", axis(2) size(vlarge))*/ xtitle("Months in Unemployment", size(vlarge));
		
#delimit cr		
graph export "${mainresultsdir}/fig5d.pdf", as(pdf) replace




#delimit ;				
		scatter survy_m surv_yu u_duration if u_duration<=20,
		connect(l) lwidth(thick) lpattern(longdash) msymbol(i dh i ) msize(medium medium) mlwidth(medthick) mfcolor(midgreen midgreen) mlcolor(midgreen midgreen midgreen) lcolor(midgreen midgreen midgreen)  xlab(2(4)20, labsize(large))   ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1)) graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.3) ysize(1.2)
		/* TARGETS */
		|| scatter surv_yu survy_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		connect(i i) lwidth(none ) lpattern(blank) msymbol(Dh D) msize(vlarge large) mlwidth(thick) mfcolor(midgreen midgreen) mlcolor(dkgreen dkgreen)
		legend(order(3   4  7  8 ) label(3 "Young: Target, Data") label(4 "Model") label(7 "Prime: Target, Data") label(8 "Model") rows(2) position(1) size(*1.3) symxsize(*0.2) ring(0) colgap(2.5))   // ring(0) 
		|| 
		scatter survy_m surv_yu u_duration if u_duration<=20,
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.02 1.0) axis(2))  connect(l) lwidth(thick) lpattern(longdash) msymbol(i dh i ) msize(medium medium) mlwidth(medthick) mfcolor(midgreen%25 midgreen%25) mlcolor(midgreen%25 midgreen%25 midgreen%25) lcolor(midgreen%25 midgreen%25 midgreen%25)  xlab(2(4)20)  
		/* TARGETS */
		|| scatter surv_yu survy_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		yaxis(2) connect(i i) lwidth(none ) lpattern(blank) msymbol(Dh D) msize(vlarge large) mlwidth(thick) mfcolor(midgreen%25 midgreen%25) mlcolor(dkgreen%25 dkgreen%25)
		//||
		//scatter survy_m u_duration if u_duration<=20 & (u_duration==1 | u_duration==20), connect(l) lwidth(medthick) lpattern(dot) lcolor(midgreen) msymbol(i) yaxis(2)
		/*ytitle("Survival in Unemployment", axis(1) size(large)) ytitle("Survival, log scale (lighter colors)", axis(2) size(large))*/ xtitle("Months in Unemployment", size(vlarge))
		||
		// PRIME AGE
				scatter survp_m surv_pu u_duration if u_duration<=20,
		connect(l) lwidth(thick) lpattern(solid) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(navy navy) mlcolor(navy navy navy) lcolor(navy navy navy)  xlab(2(4)20, labsize(large))   ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1))  
		/* TARGETS */
		|| scatter surv_pu survp_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(navy navy) mlcolor(dknavy dknavy)
		// ring(0) 
		|| 
		scatter survp_m surv_pu u_duration if u_duration<=20,
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.015 1.0) axis(2))  connect(l) lwidth(thick) lpattern(solid) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(navy%25 navy%25) mlcolor(navy%25 navy%25 navy%25) lcolor(navy%25 navy%25 navy%25)  xlab(2(4)20)  
		/* TARGETS */
		|| scatter surv_pu survp_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		yaxis(2) connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(navy%50 navy%50) mlcolor(dknavy%50 dknavy%50)
		//||
		//scatter survp_m u_duration if u_duration<=20 & (u_duration==1 | u_duration==20), connect(l) lwidth(medthick) lpattern(dot) lcolor(navy) msymbol(i) yaxis(2)
		;

		
#delimit cr		
graph export "${mainresultsdir}/fig5e.pdf", as(pdf) replace


//**** STAYER MOVER
replace mvsurv_d=surv_au_move_inferred 
replace stsurv_d=surv_au_stay_inferred


* max duration 
global maxdurpic_stmv=12

#delimit ;
		// MOVERS
		scatter mvsurv_m mvsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		connect(l) lwidth(thick) lpattern("._") msymbol(i + i ) msize(medium large) mlwidth(thick) mfcolor(sienna sienna) mlcolor(sienna sienna sienna) lcolor(sienna sienna sienna)  xlab(1(2)${maxdurpic_stmv}, labsize(large))   ylab(0(0.2)1.0) ysc(r(0.0 1.0) axis(1)) graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.35) ysize(1.2)
		legend(order(1  2 3 4 ) label(1 "Movers: Model") label(2 "Data") label(4 "Data") label(3 "Stayers: Model") rows(2) position(1) size(*1.3) symxsize(*0.55) ring(0) colgap(2)) // ring(0) 
		|| 
		scatter mvsurv_m mvsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.3 0.4 0.5 0.7 1.0, axis(2)) ysc(r(0.02 1.0) axis(2))  connect(l) lwidth(thick) lpattern("._") msymbol(i + i ) msize(medium large) mlwidth(thick) mfcolor(sienna%25 sienna%25) mlcolor(sienna%25 sienna%25 sienna%25) lcolor(sienna%25 sienna%25 sienna%25)  
		/*ytitle("Survival in Unemployment", axis(1) size(large))*/ ytitle("Survival, log scale (lighter colors)", axis(2) size(vlarge)) xtitle("Months in Unemployment", size(vlarge))
		||
		// STAYERS
				scatter stsurv_m stsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		connect(l) lwidth(thick) lpattern(solid) msymbol(i x i ) msize(medium large) mlwidth(thick) mfcolor(purple purple) mlcolor(purple purple purple) lcolor(purple purple purple)  ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1)) 
		|| 
		scatter stsurv_m stsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.015 1.0) axis(2))  connect(l) lwidth(thick) lpattern(solid) msymbol(i x i ) msize(medium large) mlwidth(thick) mfcolor(purple%25 purple%25) mlcolor(purple%25 purple%25 purple%25) lcolor(purple%25 purple%25 purple%25)  
		;

		
#delimit cr		
graph export "${mainresultsdir}/fig5f.pdf", as(pdf) replace


** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_au
replace survy_d =surv_yu
replace survp_d = surv_pu
replace stsurv_d = surv_au_stay_inferred 
replace mvsurv_d = surv_au_move_inferred 
replace stsurvy_d =surv_yu_stay_inferred 
replace mvsurvy_d =surv_yu_move_inferred 
replace stsurvp_d =surv_pu_stay_inferred
replace mvsurvp_d =surv_pu_move_inferred 
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog1
log using "${mainresultsdir}/xtra_survprofile_grossnet_calibration.txt", replace text name(survprofilelog1)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog1
	

}		
		
		
		
*************************************************
** JOB FINDING HAZARD PROFILES FOR SUPP APPENDIX
*************************************************




capture program drop jf_haz_calc_nos_exe
program define jf_haz_calc_nos_exe
			syntax varlist 
			
	sort u_duration
	
	local lowbwidth=0.4
	local lpolybwidth=0.5
	local maxudur=16
			
	foreach v of local varlist {

		capture drop hz_`v'
		gen hz_`v'=.
		replace hz_`v'=1.0-(`v'[_n+1]/`v') if u_duration[_n+1]==u_duration+1 & `v'[_n+1]!=. & `v'!=.
		
	
	}
end 


jf_haz_calc_nos_exe surv_d survy_d survp_d stsurv_d mvsurv_d stsurvp_d mvsurvy_d stsurvy_d mvsurvp_d
jf_haz_calc_nos_exe surv_m survy_m survp_m stsurv_m mvsurv_m stsurvp_m mvsurvy_m stsurvy_m mvsurvp_m 



capture drop u_durationflow
gen u_durationflow=u_duration+0.5



**** BY MOVER/STAYER
#delimit ;
scatter hz_mvsurvp_m hz_stsurvp_m u_durationflow if u_durationflow<=12, 
connect(l l l l l l l l) msymbol( i i i i i i i i) ysc(r(0.0 0.3)) lwidth(vthick vthick vthick vthick thin thin thin thin) /// lwidth(thick thin thick thin thin thick thin thick) ytitle("Prob. UE") 
xlabel(1(2)11, labsize(large)) xtitle("Months in Unemployment", size(vlarge))  ylabel(0.1(0.1)0.30, labsize(large)) graphregion(color(white)) lcolor(olive_teal dkgreen lavender dknavy) lpattern(solid longdash dash dash_dot)
||
scatter hz_mvsurvp_d hz_stsurvp_d u_durationflow if u_durationflow<=12, 
msymbol(Dh Oh)  ysc(r(0.0 0.3)) mcolor(olive_teal dkgreen lavender dknavy) mlwidth(thick thick thick thick) msize(medlarge medlarge medsmall medsmall)
legend(order (- "Occ. Movers " - "Occ. Stayers" 1 "Model " 2 "Model "  3 "Data" 4 "Data " ))
legend(ring(0) size(*1.1)) xsize(1.4) ysize(1.2)
;
#delimit cr
graph export "${mainresultsdir}/appxfig2c_prime.pdf", as(pdf) replace


#delimit ;
scatter hz_mvsurvy_m hz_stsurvy_m u_durationflow if u_durationflow<=12, 
connect(l l l l l l l l) msymbol( i i i i i i i i) ysc(r(0.0 0.3)) lwidth(vthick vthick vthick vthick thin thin thin thin) /// lwidth(thick thin thick thin thin thick thin thick) ytitle("Prob. UE") 
xlabel(1(2)11, labsize(large)) xtitle("Months in Unemployment", size(vlarge))  ylabel(0.1(0.1)0.30, labsize(large))  graphregion(color(white)) lcolor(lavender dknavy) lpattern("_.." dash)
||
scatter hz_mvsurvy_d hz_stsurvy_d u_durationflow if u_durationflow<=12, 
msymbol(Dh Oh)  ysc(r(0.0 0.3)) mcolor(lavender dknavy) mlwidth(thick thick thick thick) msize(medlarge medlarge medsmall medsmall)
/*legend(order (- "Occ. Movers "  1 "Model " 3 "Data"- "Occ. Stayers" 2 "Model "   4 "Data " ))*/
legend(order (1 "Occ Movers Model" 3 "Occ Movers Data"  2 "Occ Stayers Model"   4 "Occ Stayers Data" ))
legend(ring(0) size(*1.1) cols(1) pos(7) symxsize(*0.7)) xsize(1.4) ysize(1.2)
;
#delimit cr
graph export "${mainresultsdir}/appxfig2b_yng.pdf", as(pdf) replace


#delimit ;
scatter hz_mvsurv_m hz_stsurv_m u_durationflow if u_durationflow<=12, 
connect(l l l l l l l l) msymbol( i i i i i i i i) ysc(r(0.0 0.3)) lwidth(vthick vthick vthick vthick thin thin thin thin) /// lwidth(thick thin thick thin thin thick thin thick) 
xlabel(1(2)11, labsize(large)) xtitle("Months in Unemployment", size(vlarge))  ylabel(0.1(0.1)0.30, labsize(large)) graphregion(color(white)) lcolor(sienna purple) lpattern(solid longdash)
||
scatter hz_mvsurv_d hz_stsurv_d u_durationflow if u_durationflow<=12, 
msymbol(Dh Oh)  ysc(r(0.0 0.3)) mcolor(sienna purple) mlwidth(thick thick thick thick) msize(medlarge medlarge medsmall medsmall)
/*legend(order (- "Occ. Movers " - "Occ. Stayers" 1 "Model " 2 "Model "  3 "Data" 4 "Data " ))*/
legend(order (1 "Occ Movers Model" 3 "Occ Movers Data"  2 "Occ Stayers Model"   4 "Occ Stayers Data" ) cols(1) pos(7))
legend(ring(0) size(*1.2) symxsize(*0.7)) xsize(1.4) ysize(1.2)
;
#delimit cr
graph export "${mainresultsdir}/appxfig2a_all.pdf", as(pdf) replace



**** ALL/YOUNG/PRIME

#delimit ;
scatter hz_survp_m u_durationflow if u_durationflow<=12, 
connect(l l l l l l l l) msymbol( i i i i i i i i) ysc(r(0.10 0.31)) lwidth(vthick vthick vthick vthick thin thin thin thin) /// lwidth(thick thin thick thin thin thick thin thick) ytitle("Prob. UE")
xlabel(1(2)11, labsize(large)) xtitle("Months in Unemployment", size(vlarge))  ylabel(0.1(0.05)0.30, labsize(large))  graphregion(color(white)) lcolor(olive_teal dkgreen lavender dknavy) lpattern(solid __..)
||
scatter hz_survp_d u_durationflow if u_durationflow<=12, 
msymbol(Sh Oh)  ysc(r(0.1 0.31)) mcolor(olive_teal dkgreen lavender dknavy) mlwidth(thick thick thick thick) msize(medlarge medlarge medsmall medsmall) 
legend(order (  1 "Model " 2 "Data" ) pos(1))
legend(ring(0) size(*1.4)) xsize(1.4) ysize(1.2)
;
#delimit cr
graph export "${mainresultsdir}/appxfig1c_prime.pdf", as(pdf) replace


#delimit ;
scatter hz_survy_m u_durationflow if u_durationflow<=12, 
connect(l l l l l l l l) msymbol( i i i i i i i i) ysc(r(0.10 0.31)) lwidth(vthick vthick vthick vthick thin thin thin thin) /// lwidth(thick thin thick thin thin thick thin thick) 
xlabel(1(2)11, labsize(large)) xtitle("Months in Unemployment", size(vlarge))  ylabel(0.1(0.05)0.30, labsize(large))  graphregion(color(white)) lcolor(lavender dknavy) lpattern(solid __..)
||
scatter hz_survy_d u_durationflow if u_durationflow<=12, 
msymbol(Sh Oh)  ysc(r(0.1 0.31)) mcolor(lavender dknavy) mlwidth(thick thick thick thick) msize(medlarge medlarge medsmall medsmall)
legend(order (1 "Model " 2 "Data" ) pos(7) cols(1))
legend(ring(0) size(*1.4)) xsize(1.4) ysize(1.2)
;
#delimit cr
graph export "${mainresultsdir}/appxfig1b_young.pdf", as(pdf) replace


#delimit ;
scatter hz_surv_m u_durationflow if u_durationflow<=12, 
connect(l l l l l l l l) msymbol( i i i i i i i i) ysc(r(0.10 0.31)) lwidth(vthick vthick vthick vthick thin thin thin thin) /// lwidth(thick thin thick thin thin thick thin thick) 
xlabel(1(2)11, labsize(large)) xtitle("Months in Unemployment", size(vlarge))  ylabel(0.1(0.05)0.30, labsize(large))    graphregion(color(white)) lcolor(sienna purple) lpattern(solid __..)
||
scatter hz_surv_d u_durationflow if u_durationflow<=12, 
msymbol(Sh Oh)  ysc(r(0.1 0.31)) mcolor(sienna purple) mlwidth(thick thick thick thick) msize(medlarge medlarge medsmall medsmall)
legend(order (1 "Model " 2 "Data" ) pos(1) cols(1))
legend(ring(0) size(*1.4)) xsize(1.4) ysize(1.2)
;
#delimit cr
graph export "${mainresultsdir}/appxfig1a_all.pdf", as(pdf) replace






********************************************************************************
** EXCESS/NET MOBILITY DURATION PROFILE IN THE MODEL
********************************************************************************




clear 
import delimited "${fortrandir}/Gross and Net Mobility version/xsnetprofile.csv", numericcols(66) clear 
	
		
sort dur
	
	capture drop u_duration
	gen u_duration=dur
	capture drop zeroline
	gen zeroline=0
	
	
	
	capture drop lcgrsmobdr
	capture drop lcxsmobdur 
	lowess cgrsmobdr u_duration, gen(lcgrsmobdr) 
	lowess cxsmobdur  u_duration, gen(lcxsmobdur) 
	
	capture drop avemobline
	gen avemobline=lcxsmobdur if dur==1
	replace avemobline=avemobline[_n-1] if dur>1
		
		#delimit ;	
		twoway rarea lcgrsmobdr lcxsmobdur u_duration if u_duration<=12, fcolor(sienna) fintensity(inten30) lcolor(sienna) lwidth(medium) lpattern(longdash) xlab(1(1)12)  /*||
		rarea  mgtnet_excessmob_rtmm_mw excessmob_rtmm_mw u_duration if u_duration<=12, color(sandb) xlab(1(1)12) fintensity(inten30)*/ ylab(0(0.1)0.5, labsize(large)) ysc(r(0 0.5)) || 
		rarea  lcxsmobdur zeroline u_duration if u_duration<=12, color(gold) lcolor(dkorange) lwidth(thin) xlab(1(1)12, labsize(large)) graphregion(color(white))
		xtitle("Months (m) in Unemployment", size(large))  ytitle("Occupation Mobility (Model)" , height(9) size(large))
		 legend(on width(150) size(*1.2) order(1 2 - "   of spells {&ge} {it:m} months " - "   across spells {&ge} {it:m}" 3 - "   months") cols(2) pos(12) ring(0) symxsize(*0.5)   label(1 "Net Mobility across set") 
		label(2 "Excess Realloc.")  label(3 "Excess Reall. all u. spells") )  
		|| scatter avemobline u_duration if u_duration<=12, connect(l) msymbol(none) lcolor(dkorange) lpattern(-..);
		#delimit cr
		graph export "${mainresultsdir}/appxfig3a.pdf", as(pdf) replace


		
		
		
		

********************************************************************************
**
********    DURATION PROFILES OF THE EXCESS MOBILITY  CALIBRATION 	  **********
**
********************************************************************************		




	 import delimited "${fortrandir}/Excess Mobility version/profiles7_000.csv", numericcols(66) clear 
	 global lversion xsmob
	 
	 
	 cap n destring _all, replace 
	 ren month u_duration
	 capture drop _merge
	 merge 1:1 u_duration using "${outputdata}/durationprofiles_mog.dta"
	 cap gen duration=u_duration
	 capture drop _merge
	 merge 1:1 duration using "${mainresultsdir}/cycldurationshift_mog.dta"
	 cap drop duration
	 
	 
	 capture drop lows_cmdur*
	 capture drop lows_mdur*
	 
		capture program drop lowess_grossmob_exe
		program define lowess_grossmob_exe
					args name dur

		capture drop lows_`name'
		lowess `name' u_duration if u_duration<=`dur', gen(lows_`name') bwidth(0.5) nograph
	
		
		end 

		lowess_grossmob_exe cmdur_m 16
		lowess_grossmob_exe cmdur_d 16
		lowess_grossmob_exe mdur_m 16
		lowess_grossmob_exe mdur_d 16
		lowess_grossmob_exe cmdury_m 16
		lowess_grossmob_exe cmdury_d 16
		lowess_grossmob_exe cmdurp_m 16
		lowess_grossmob_exe cmdurp_d 16
		lowess_grossmob_exe mdury_m 16
		lowess_grossmob_exe mdury_d 16
		lowess_grossmob_exe mdurp_m 16
		lowess_grossmob_exe mdurp_d 16
		
		lowess_grossmob_exe occmob_mog_raw 16
		lowess_grossmob_exe occmob_mog_corr 16
		lowess_grossmob_exe occmob_mog_corr_yng 16
		lowess_grossmob_exe occmob_mog_corr_prm 16
		lowess_grossmob_exe occmob_mog_raw_yng 16
		lowess_grossmob_exe occmob_mog_raw_prm 16
		
		capture program drop lpoly_grossmob_exe
		program define lpoly_grossmob_exe
					args name dur //ind

		capture drop lpoly_`name'
		lpoly `name' u_duration if u_duration<=`dur' , gen(lpoly_`name') degree(2) at(u_duration) bwidth(2) nograph
	
		
		end 

		lpoly_grossmob_exe cmdur_m 16
		lpoly_grossmob_exe cmdur_d 16
		lpoly_grossmob_exe mdur_m 16
		lpoly_grossmob_exe mdur_d 16 
		lpoly_grossmob_exe cmdury_m 16
		lpoly_grossmob_exe cmdury_d 16
		lpoly_grossmob_exe cmdurp_m 16
		lpoly_grossmob_exe cmdurp_d 16
		lpoly_grossmob_exe mdury_m 16
		lpoly_grossmob_exe mdury_d 16
		lpoly_grossmob_exe mdurp_m 16
		lpoly_grossmob_exe mdurp_d 16
		
	 
	#delimit ;				
			scatter occmob_mog_raw mdur_m  mdur_m u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank dash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gs6 gs6) mlcolor(gs6 gs6 gs6) lcolor(gs6 gs6 gs6)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70)) graphregion(color(white)) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.4)
			/* TARGETS */
			|| scatter lows_occmob_mog_raw mdur_m  u_duration if u_duration<=12 & (u_duration==1 |u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12),
			connect(i i) lwidth(none ) lpattern(black) msymbol(Oh O) msize(vlarge medlarge) mlwidth(thick) mfcolor(gs6 black) mlcolor(black black)   
			||			
			scatter occmob_mog_corr cmdur_m  cmdur_m u_duration if u_duration<=12,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gold gold) mlcolor(gold gold gold) lcolor(gold gold gold)  
			/* CORRECTED PROFILE */
			legend(order(4 5 1 3  6 8) label(4 "Target, Data") label(5 "Model") label(1 "Uncorrected Mobility, Data") label(3 "Model")
			label(6 "Mobility, Data") label(8 "Model") ring(0) size(*1.3) symxsize(*0.4) colgap(2))  xtitle("Months in Unemployment", size(vlarge)) ytitle("Proportion Moving Occupation" "(MOG) Upon Exit U", size(vlarge) height(12) )
			;
	#delimit cr	
	graph export "${mainresultsdir}/appxfig5a.pdf", as(pdf) replace
	
	
	capture drop target_all
	gen byte target_all=0
	replace target_all=1 if u_duration<=12 & (u_duration==1 |u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	

	
	
	/*
	PICTURE OF THE AGE-SPECIFIC OCC MOBILITY DURATION  PROFILE 
	*/

	
	
	#delimit ;		
	/* --- YOUNG ---- */	
			scatter occmob_mog_corr_yng cmdury_m cmdury_m u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank longdash) msymbol(dh i i ) msize(medium medium) mlwidth(medthick) mfcolor(midgreen midgreen) mlcolor(midgreen midgreen midgreen) lcolor(midgreen midgreen midgreen)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70)) graphregion(color(white))  graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.4)
			
			
			/* TARGETS */
			|| scatter lows_occmob_mog_corr_yng cmdury_m u_duration if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12),
			connect(i i) lwidth(none ) lpattern(black) msymbol(Dh D) msize(vlarge large) mlwidth(thick) mfcolor(midgreen dkgreen) mlcolor(dkgreen dkgreen)   
			
	/* --- PRIME ---- */
			||
			scatter occmob_mog_corr_prm cmdurp_m  lpoly_cmdurp_m u_duration if u_duration<=12,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(navy navy) mlcolor(navy navy navy) lcolor(navy navy navy)  
			/* TARGETS */
			|| scatter lows_occmob_mog_corr_prm cmdurp_m  u_duration if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12),
			connect(i i) lwidth(none ) lpattern(black) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(navy dknavy) mlcolor(dknavy dknavy)   
			legend(order(- "Young Unemployed" - " Prime-Aged " 4  9  5 10  3  8) label(4 "Target - Data") label(5 "Target - Model") label(1 "Mobility - Data") label(3 "Mobility - Model")
			label(6 "Mobility - Data") label(8 "Mobility - Model") label(9 "Target - Data ") label(10 "Target - Model ") ring(0) pos(5) size(*1.3) symxsize(*0.5)) xtitle("Months in Unemployment", size(vlarge))
			;
			
	#delimit cr
	graph export "${mainresultsdir}/appxfig5b.pdf", as(pdf) replace
	

	capture drop target_yngprm
	gen byte target_yngprm=0
	replace target_yngprm=1 if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	

	
	
	
	/*
	PICTURE OF THE CYCLICAL SHIFT OF THE OCC MOB DURATION PROFILE 
	*/


		lpoly_grossmob_exe badcmdur_m	12   // consider 12 months for high U times
		lpoly_grossmob_exe badcmdur_d   12
		lpoly_grossmob_exe gdcmdur_m	9	 // consider 9 months for low U times
		lpoly_grossmob_exe gdcmdur_d    9
		lowess_grossmob_exe badcmdur_m	 12
		lowess_grossmob_exe badcmdur_d   12
		lowess_grossmob_exe gdcmdur_m	 9
		lowess_grossmob_exe gdcmdur_d    9
		
		
		 

	#delimit ;				
			scatter mobduration_rec badcmdur_m  lows_badcmdur_m  u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank shortdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gs4 gs4) mlcolor(gs4 gs4 gs4) lcolor(gs4 gs4 gs4)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70))  graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.4)
			/* TARGETS */
			|| scatter mobduration_rec badcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=12),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs4 black) mlcolor(black black)   
			||
			scatter mobduration_exp gdcmdur_m  lows_gdcmdur_m  u_duration if u_duration<=9,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(sh i i ) msize(medium medium) mlwidth(medthick) mfcolor(midblue midblue) mlcolor(midblue midblue midblue) lcolor(midblue midblue midblue)  
			/* TARGETS */
			/* TARGETS */
			|| scatter mobduration_exp gdcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=8),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Sh S) msize(vlarge large) mlwidth(thick) mfcolor(midblue edkblue) mlcolor(edkblue edkblue)   
			legend(order(- "Times of High U" - " Times of Low U " 4    9  5 10 ) label(4 "Target - Data") label(5 "Target - Model ") label(1 "Corr. Mob. Data") label(3 "Corr. Mob. Model")
			label(6 "Corr. Mob. Data") label(8 "Corr. Mob. Model") label(9 "Target - Data") label(10 "Target - Model") ring(0) size(*1.3) symxsize(*0.5)) xtitle("Months in Unemployment", size(vlarge))
			;
	#delimit cr		
	graph export "${mainresultsdir}/appxfig5c.pdf", as(pdf) replace
		
		
		
		
	
	#delimit ;				
			scatter mobduration_rec badcmdur_m  lows_badcmdur_m  u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank shortdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gs4 gs4) mlcolor(gs4 gs4 gs4) lcolor(gs4 gs4 gs4)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70))  graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.2)
			/* TARGETS */
			|| scatter mobduration_rec badcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=12),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs4 black) mlcolor(black black)   
			||
			scatter mobduration_exp gdcmdur_m  lows_gdcmdur_m  u_duration if u_duration<=9,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(sh i i ) msize(medium medium) mlwidth(medthick) mfcolor(midblue midblue) mlcolor(midblue midblue midblue) lcolor(midblue midblue midblue)  
			/* TARGETS */
			/* TARGETS */
			|| scatter mobduration_exp gdcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=8),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Sh S) msize(vlarge large) mlwidth(thick) mfcolor(midblue edkblue) mlcolor(edkblue edkblue)   
			legend(order(- "Times of High U" - " Times of Low U " 4    9  5 10 ) label(4 "Target - Data") label(5 "Target - Model ") label(1 "Corr. Mob. Data") label(3 "Corr. Mob. Model")
			label(6 "Corr. Mob. Data") label(8 "Corr. Mob. Model") label(9 "Target - Data") label(10 "Target - Model") ring(0) size(*1.3) symxsize(*0.5)) xtitle("Months in Unemployment", size(vlarge)) ytitle("Proportion Moving Occupation" "(MOG) Upon Exit U", size(vlarge) height(12))
			;
	#delimit cr		
	graph export "${mainresultsdir}/appxfig8a.pdf", as(pdf) replace
		 	
	capture drop target_badtimes
	gen byte target_badtimes=0
	replace target_badtimes=1 if u_duration<=12 & u_duration>=1
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	replace target_goodtimes=1 if u_duration<=8 & u_duration>=1
	

quietly {
cap log close mobdurprofilelog2
log using "${mainresultsdir}/appxtable4_mobdurprofiles_xs_calibration.txt", replace text name(mobdurprofilelog2)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "OCC MOBILITY - UNEMPL DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_raw mean mdur_m mean target_all)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "YOUNG: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_yng mean cmdury_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PRIME: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_prm mean cmdurp_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "HIGH U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean mobduration_rec  mean badcmdur_m mean target_badtimes)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "LOW U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=8, c(mean mobduration_exp  mean gdcmdur_m mean target_goodtimes)
noisily: display ""
  
 
log close mobdurprofilelog2
	

}	
	 
	 
	 

quietly {
cap log close mobdurprofilelogyp2
log using "${mainresultsdir}/appxtable4_occmob_rel_y_p_moment_model_data", replace text name(mobdurprofilelogyp2)
noisily: display  ""
noisily: display  " we use the average value (averaged over the first 12 months of duration"
noisily: display  " of the young vs the prime-aged UNCORRECTED profile as an additional "
noisily: display  " calibration target (mainly for emphasis)"

version 13
 
noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE YOUNG (DATA)"
noisily: display  ""
noisily: su occmob_mog_raw_yng if u_duration<=12
local ave_mprofile_young_data= r(mean)
noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE PRIME (DATA)"
noisily: display  ""

noisily: su occmob_mog_raw_prm if u_duration<=12
local ave_mprofile_prime_data= r(mean)

noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE YOUNG (MODEL)"
noisily: display  ""
noisily: su mdury_m if u_duration<=12
local ave_mprofile_young_model= r(mean)
noisily: display  ""
noisily: display  " ---- AVERAGE VALUE PROFILE PRIME (DATA)"
noisily: display  ""

noisily: su mdurp_m if u_duration<=12
local ave_mprofile_prime_model= r(mean)


noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "RELATIVE PROFILES Y vs P (MODEL -- DATA)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
local rel_y_p_mprofile_model=`ave_mprofile_young_model'/`ave_mprofile_prime_model'
local rel_y_p_mprofile_data=`ave_mprofile_young_data'/`ave_mprofile_prime_data'
noisily: display  " model:  `rel_y_p_mprofile_model'; vs data: `rel_y_p_mprofile_data' "
 
log close mobdurprofilelogyp2
	

}		
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 	
	
	
	
	
	

//==========================
//  SURVIVAL PROFILES
//==========================	


capture drop month
gen month=u_duration
capture drop _merge
merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
drop month



sort u_duration
#delimit ;				
		scatter surv_m surv_au u_duration if u_duration<=20,
		connect(l) lwidth(thick) lpattern(longdash) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(gs6 gs6) mlcolor(gs6 gs6 gs6) lcolor(gs6 gs6 gs6)  xlab(2(4)20, labsize(large))   ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1)) graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.35) ysize(1.2)
		/* TARGETS */
		|| scatter surv_au surv_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs6 gs4) mlcolor(black black)
		/*legend(order(- "RHS axis - Linear" - " LHS axis - Log" 3    7  4 8 ) label(3 "Target - Data") label(4 "Target - Model ") label(7 "Target - Data") label(8 "Target - Model ") position(1) size(*1) symxsize(*0.5)) */
		legend(order(3 4) label(3 "Target, Data") label(4 "Target, Model ") label(7 "Target - Data") label(8 "Target - Model ") position(2) size(*1.4) symxsize(*0.5) ring(0) cols(1) ) 
		
		|| 
		scatter surv_m surv_au u_duration if u_duration<=20,
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.015 1.0) axis(2))  connect(l) lwidth(thick) lpattern(longdash) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(gs6%30 gs6%30) mlcolor(gs6%30 gs6%30 gs6%30) lcolor(gs6%30 gs6%30 gs6%30)  xlab(2(4)20, labsize(large))  
		/* TARGETS */
		|| scatter surv_au surv_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		yaxis(2) connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs6%30 gs4%30) mlcolor(black%30 black%30)
		//||
		//scatter surv_m u_duration if u_duration<=20 & (u_duration==1 | u_duration==20), connect(l) lwidth(medthick) lpattern(dot) lcolor(gs6) msymbol(i) yaxis(2)
		ytitle("Survival in Unemployment", axis(1) size(vlarge)) ytitle("", axis(2)) /* ytitle("Survival, log scale (lighter colors)", axis(2) size(vlarge))*/ xtitle("Months in Unemployment", size(vlarge));
		
#delimit cr		
graph export "${mainresultsdir}/appxfig5d.pdf", as(pdf) replace




#delimit ;				
		scatter survy_m surv_yu u_duration if u_duration<=20,
		connect(l) lwidth(thick) lpattern(longdash) msymbol(i dh i ) msize(medium medium) mlwidth(medthick) mfcolor(midgreen midgreen) mlcolor(midgreen midgreen midgreen) lcolor(midgreen midgreen midgreen)  xlab(2(4)20, labsize(large))   ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1)) graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.3) ysize(1.2)
		/* TARGETS */
		|| scatter surv_yu survy_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		connect(i i) lwidth(none ) lpattern(blank) msymbol(Dh D) msize(vlarge large) mlwidth(thick) mfcolor(midgreen midgreen) mlcolor(dkgreen dkgreen)
		legend(order(3   4  7  8 ) label(3 "Young: Target, Data") label(4 "Model") label(7 "Prime: Target, Data") label(8 "Model") rows(2) position(1) size(*1.3) symxsize(*0.2) ring(0) colgap(2.5))   // ring(0) 
		|| 
		scatter survy_m surv_yu u_duration if u_duration<=20,
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.02 1.0) axis(2))  connect(l) lwidth(thick) lpattern(longdash) msymbol(i dh i ) msize(medium medium) mlwidth(medthick) mfcolor(midgreen%25 midgreen%25) mlcolor(midgreen%25 midgreen%25 midgreen%25) lcolor(midgreen%25 midgreen%25 midgreen%25)  xlab(2(4)20)  
		/* TARGETS */
		|| scatter surv_yu survy_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		yaxis(2) connect(i i) lwidth(none ) lpattern(blank) msymbol(Dh D) msize(vlarge large) mlwidth(thick) mfcolor(midgreen%25 midgreen%25) mlcolor(dkgreen%25 dkgreen%25)
		//||
		//scatter survy_m u_duration if u_duration<=20 & (u_duration==1 | u_duration==20), connect(l) lwidth(medthick) lpattern(dot) lcolor(midgreen) msymbol(i) yaxis(2)
		/*ytitle("Survival in Unemployment", axis(1) size(large)) ytitle("Survival, log scale (lighter colors)", axis(2) size(large))*/ xtitle("Months in Unemployment", size(vlarge))
		||
		// PRIME AGE
				scatter survp_m surv_pu u_duration if u_duration<=20,
		connect(l) lwidth(thick) lpattern(solid) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(navy navy) mlcolor(navy navy navy) lcolor(navy navy navy)  xlab(2(4)20, labsize(large))   ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1))  
		/* TARGETS */
		|| scatter surv_pu survp_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(navy navy) mlcolor(dknavy dknavy)
		// ring(0) 
		|| 
		scatter survp_m surv_pu u_duration if u_duration<=20,
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.015 1.0) axis(2))  connect(l) lwidth(thick) lpattern(solid) msymbol(i oh i ) msize(medium medium) mlwidth(medthick) mfcolor(navy%25 navy%25) mlcolor(navy%25 navy%25 navy%25) lcolor(navy%25 navy%25 navy%25)  xlab(2(4)20)  
		/* TARGETS */
		|| scatter surv_pu survp_m u_duration if u_duration<=20 & (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20),
		yaxis(2) connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(navy%50 navy%50) mlcolor(dknavy%50 dknavy%50)
		//||
		//scatter survp_m u_duration if u_duration<=20 & (u_duration==1 | u_duration==20), connect(l) lwidth(medthick) lpattern(dot) lcolor(navy) msymbol(i) yaxis(2)
		;

		
#delimit cr		
graph export "${mainresultsdir}/appxfig5e.pdf", as(pdf) replace


//**** STAYER MOVER
replace mvsurv_d=surv_au_move_inferred 
replace stsurv_d=surv_au_stay_inferred


* max duration 
global maxdurpic_stmv=12

#delimit ;
		// MOVERS
		scatter mvsurv_m mvsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		connect(l) lwidth(thick) lpattern("._") msymbol(i + i ) msize(medium large) mlwidth(thick) mfcolor(sienna sienna) mlcolor(sienna sienna sienna) lcolor(sienna sienna sienna)  xlab(1(2)${maxdurpic_stmv}, labsize(large))   ylab(0(0.2)1.0) ysc(r(0.0 1.0) axis(1)) graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.35) ysize(1.2)
		legend(order(1  2 3 4 ) label(1 "Movers: Model") label(2 "Data") label(4 "Data") label(3 "Stayers: Model") rows(2) position(1) size(*1.3) symxsize(*0.55) ring(0) colgap(2)) // ring(0) 
		|| 
		scatter mvsurv_m mvsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.3 0.4 0.5 0.7 1.0, axis(2)) ysc(r(0.02 1.0) axis(2))  connect(l) lwidth(thick) lpattern("._") msymbol(i + i ) msize(medium large) mlwidth(thick) mfcolor(sienna%25 sienna%25) mlcolor(sienna%25 sienna%25 sienna%25) lcolor(sienna%25 sienna%25 sienna%25)  
		/*ytitle("Survival in Unemployment", axis(1) size(large))*/ ytitle("Survival, log scale (lighter colors)", axis(2) size(vlarge)) xtitle("Months in Unemployment", size(vlarge))
		||
		// STAYERS
				scatter stsurv_m stsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		connect(l) lwidth(thick) lpattern(solid) msymbol(i x i ) msize(medium large) mlwidth(thick) mfcolor(purple purple) mlcolor(purple purple purple) lcolor(purple purple purple)  ylab(0(0.2)1.0, labsize(large)) ysc(r(0.0 1.0) axis(1)) 
		|| 
		scatter stsurv_m stsurv_d u_duration if u_duration<=${maxdurpic_stmv},
		yaxis(2) yscale(log axis(2)) ylabel(0.02 0.05 0.1 0.2 0.4 0.7 1.0, axis(2) labsize(large)) ysc(r(0.015 1.0) axis(2))  connect(l) lwidth(thick) lpattern(solid) msymbol(i x i ) msize(medium large) mlwidth(thick) mfcolor(purple%25 purple%25) mlcolor(purple%25 purple%25 purple%25) lcolor(purple%25 purple%25 purple%25)  
		;
		
#delimit cr		
graph export "${mainresultsdir}/appxfig5f.pdf", as(pdf) replace


** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_au
replace survy_d =surv_yu
replace survp_d = surv_pu
replace stsurv_d = surv_au_stay_inferred 
replace mvsurv_d = surv_au_move_inferred 
replace stsurvp_d =surv_yu_stay_inferred 
replace mvsurvy_d =surv_yu_move_inferred 
replace stsurvy_d =surv_pu_stay_inferred
replace mvsurvp_d =surv_pu_move_inferred 
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog2
log using "${mainresultsdir}/appxtable4_survprofile_xs_calibration.txt", replace text name(survprofilelog2)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog2
	

}		
		
		

********************************************************************************
**
****PROFILES OF THE EXCESS MOBILITY CALIBRATION WITHOUT SKILL DEP *****
**
********************************************************************************		
		
		

	 import delimited "${fortrandir}/Robustness No Skill Depreciation/profilenoskdep_000.csv", numericcols(66) clear 
	 global lversion xsmob_noskilldep
	 
	 cap n destring _all, replace 
	 capture drop _merge
	 merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
	 ren month u_duration		
		
	 capture drop _merge 
	 merge 1:1 u_duration using "${mainresultsdir}/xtra_mobdurprofile_targets.dta"
	 
	 
	
	//===================
	// BAD TIMES ADN GOOD TIMES 
	//===================		
			
	*badcmdur_d		;
		lpoly_grossmob_exe badcmdur_m	12
		lpoly_grossmob_exe badcmdur_d 12
		lpoly_grossmob_exe gdcmdur_m	9
		lpoly_grossmob_exe gdcmdur_d 9
		lowess_grossmob_exe badcmdur_m	 12
		lowess_grossmob_exe badcmdur_d 12
		lowess_grossmob_exe gdcmdur_m	 9
		lowess_grossmob_exe gdcmdur_d 9
		
		
		
#delimit ;				
			scatter mobduration_rec badcmdur_m  lows_badcmdur_m  u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank shortdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gs4 gs4) mlcolor(gs4 gs4 gs4) lcolor(gs4 gs4 gs4)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70))  graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.2)
			/* TARGETS */
			|| scatter mobduration_rec badcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=12),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs4 black) mlcolor(black black)   
			||
			scatter mobduration_exp gdcmdur_m  lows_gdcmdur_m  u_duration if u_duration<=9,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(sh i i ) msize(medium medium) mlwidth(medthick) mfcolor(midblue midblue) mlcolor(midblue midblue midblue) lcolor(midblue midblue midblue)  
			/* TARGETS */
			/* TARGETS */
			|| scatter mobduration_exp gdcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=8),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Sh S) msize(vlarge large) mlwidth(thick) mfcolor(midblue edkblue) mlcolor(edkblue edkblue)   
			legend(order(- "Times of High U" - " Times of Low U " 4    9  5 10 ) label(4 "Target - Data") label(5 "Target - Model ") label(1 "Corr. Mob. Data") label(3 "Corr. Mob. Model")
			label(6 "Corr. Mob. Data") label(8 "Corr. Mob. Model") label(9 "Target - Data") label(10 "Target - Model") ring(0) size(*1.3) symxsize(*0.5)) xtitle("Months in Unemployment", size(vlarge))
			;
	#delimit cr		
	graph export "${mainresultsdir}/appxfig8b.pdf", as(pdf) replace

	
/*
	capture drop target_badtimes
	gen byte target_badtimes=0
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	

quietly {
cap log close mobdurprofilelog2
log using "${mainresultsdir}/xtra_mobdurprofiles_xsnoskilldep_calibration.txt", replace text name(mobdurprofilelog2)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "OCC MOBILITY - UNEMPL DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_raw mean mdur_m mean target_all)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "YOUNG: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_yng mean cmdury_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PRIME: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_prm mean cmdurp_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "HIGH U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean mobduration_rec  mean badcmdur_m mean target_badtimes)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "LOW U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=8, c(mean mobduration_exp  mean gdcmdur_m mean target_goodtimes)
noisily: display ""
  
 
log close mobdurprofilelog2
	

}	
	
*/




** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_au
replace survy_d =surv_yu
replace survp_d = surv_pu
replace stsurv_d = surv_au_stay_inferred 
replace mvsurv_d = surv_au_move_inferred 
replace stsurvp_d =surv_yu_stay_inferred 
replace mvsurvy_d =surv_yu_move_inferred 
replace stsurvy_d =surv_pu_stay_inferred
replace mvsurvp_d =surv_pu_move_inferred 
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog3
log using "${mainresultsdir}/xtra_survprofile_xsnoskilldep_calibration.txt", replace text name(survprofilelog3)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog3
	

}		
		

********************************************************************************
**
****PROFILES OF THE EXCESS MOBILITY CALIBRATION -- NUN *****
**
********************************************************************************		
		
		

	 import delimited "${fortrandir}/Robustness NUN/profile_nun_000.csv", numericcols(66) clear 
	 global lversion xsmob_nun
	 
	 cap n destring _all, replace 
	 capture drop _merge
	 merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
	 ren month u_duration		
		
	 cap gen n_duration=u_duration	
	 capture drop _merge 
	 merge 1:1 n_duration using "${outputdata}/cycldurationshift_mog_nun_males.dta"
	 
	 capture drop _merge 
	 merge 1:1 n_duration using "${outputdata}/durationprofiles_mog_nun_males.dta"
	 
	 
	 ** set info on which durations are targeted in the calibration
	capture drop target_all
	gen byte target_all=0
	replace target_all=1 if u_duration<=12 & (u_duration==1 |u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	
	capture drop target_yngprm
	gen byte target_yngprm=0
	replace target_yngprm=1 if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	
	capture drop target_badtimes
	gen byte target_badtimes=0
	replace target_badtimes=1 if u_duration<=12 & u_duration>=1
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	replace target_goodtimes=1 if u_duration<=8 & u_duration>=1
	
	

	capture drop target_badtimes
	gen byte target_badtimes=0
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	

quietly {
cap log close mobdurprofilelog4
log using "${mainresultsdir}/xtra_mobdurprofiles_xsnun_calibration.txt", replace text name(mobdurprofilelog4)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "OCC MOBILITY - UNEMPL DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_raw mean mdur_m mean target_all)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "YOUNG: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_corr_yng mean cmdury_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PRIME: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_corr_prm mean cmdurp_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "HIGH U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_corr_bad  mean badcmdur_m mean target_badtimes)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "LOW U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=8, c(mean occmob_mog_corr_good   mean gdcmdur_m mean target_goodtimes)
noisily: display ""
  
 
log close mobdurprofilelog4
	

}	
	





** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_amnun
replace survy_d =surv_ymnun
replace survp_d = surv_pmnun
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog4
log using "${mainresultsdir}/xtra_survprofile_xsnun_calibration.txt", replace text name(survprofilelog4)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog4
	

}		
		

********************************************************************************
**
****PROFILES OF THE EXCESS MOBILITY CALIBRATION WITHOUT SKILL DEP *****
**
********************************************************************************		
		
		

	 import delimited "${fortrandir}/Robustness No Skill Depreciation/profilenoskdep_000.csv", numericcols(66) clear 
	 global lversion xsmob_noskilldep
	 
	 cap n destring _all, replace 
	 capture drop _merge
	 merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
	 ren month u_duration		
		
	 capture drop _merge 
	 merge 1:1 u_duration using "${mainresultsdir}/xtra_mobdurprofile_targets.dta"
	 
	 
	
	//===================
	// BAD TIMES ADN GOOD TIMES 
	//===================		
			
	*badcmdur_d		;
		lpoly_grossmob_exe badcmdur_m	12
		lpoly_grossmob_exe badcmdur_d 12
		lpoly_grossmob_exe gdcmdur_m	9
		lpoly_grossmob_exe gdcmdur_d 9
		lowess_grossmob_exe badcmdur_m	 12
		lowess_grossmob_exe badcmdur_d 12
		lowess_grossmob_exe gdcmdur_m	 9
		lowess_grossmob_exe gdcmdur_d 9
		
		
		
#delimit ;				
			scatter mobduration_rec badcmdur_m  lows_badcmdur_m  u_duration if u_duration<=12,
			connect(i i l) lwidth(none none thick) lpattern(blank blank shortdash) msymbol(oh i i ) msize(medium medium) mlwidth(medthick) mfcolor(gs4 gs4) mlcolor(gs4 gs4 gs4) lcolor(gs4 gs4 gs4)  xlab(1(2)12, labsize(large))   ylab(0.3(0.1)0.7, labsize(large)) ysc(r(0.25 0.70))  graphregion(color(white)) graphregion(margin(zero )) graphregion(margin(2 2 2 2)) plotregion(margin(0 0 0 0)) xsize(1.5) ysize(1.2)
			/* TARGETS */
			|| scatter mobduration_rec badcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=12),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Oh O) msize(vlarge large) mlwidth(thick) mfcolor(gs4 black) mlcolor(black black)   
			||
			scatter mobduration_exp gdcmdur_m  lows_gdcmdur_m  u_duration if u_duration<=9,
			connect(i i l) lwidth(blank blank thick) lpattern(longdash) msymbol(sh i i ) msize(medium medium) mlwidth(medthick) mfcolor(midblue midblue) mlcolor(midblue midblue midblue) lcolor(midblue midblue midblue)  
			/* TARGETS */
			/* TARGETS */
			|| scatter mobduration_exp gdcmdur_m  u_duration if u_duration<=12 & (u_duration>=1 & u_duration<=8),
			connect(i i) lwidth(none ) lpattern(blank) msymbol(Sh S) msize(vlarge large) mlwidth(thick) mfcolor(midblue edkblue) mlcolor(edkblue edkblue)   
			legend(order(- "Times of High U" - " Times of Low U " 4    9  5 10 ) label(4 "Target - Data") label(5 "Target - Model ") label(1 "Corr. Mob. Data") label(3 "Corr. Mob. Model")
			label(6 "Corr. Mob. Data") label(8 "Corr. Mob. Model") label(9 "Target - Data") label(10 "Target - Model") ring(0) size(*1.3) symxsize(*0.5)) xtitle("Months in Unemployment", size(vlarge))
			;
	#delimit cr		
	graph export "${mainresultsdir}/appxfig8b.pdf", as(pdf) replace

	
/*
	capture drop target_badtimes
	gen byte target_badtimes=0
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	

quietly {
cap log close mobdurprofilelog2
log using "${mainresultsdir}/xtra_mobdurprofiles_xsnoskilldep_calibration.txt", replace text name(mobdurprofilelog2)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "OCC MOBILITY - UNEMPL DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_raw mean mdur_m mean target_all)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "YOUNG: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_yng mean cmdury_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PRIME: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean lows_occmob_mog_corr_prm mean cmdurp_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "HIGH U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean mobduration_rec  mean badcmdur_m mean target_badtimes)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "LOW U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=8, c(mean mobduration_exp  mean gdcmdur_m mean target_goodtimes)
noisily: display ""
  
 
log close mobdurprofilelog2
	

}	
	
*/




** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_au
replace survy_d =surv_yu
replace survp_d = surv_pu
replace stsurv_d = surv_au_stay_inferred 
replace mvsurv_d = surv_au_move_inferred 
replace stsurvp_d =surv_yu_stay_inferred 
replace mvsurvy_d =surv_yu_move_inferred 
replace stsurvy_d =surv_pu_stay_inferred
replace mvsurvp_d =surv_pu_move_inferred 
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog3
log using "${mainresultsdir}/xtra_survprofile_xsnoskilldep_calibration.txt", replace text name(survprofilelog3)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog3
	

}		
		

********************************************************************************
**
****PROFILES OF THE EXCESS MOBILITY CALIBRATION -- NUN *****
**
********************************************************************************		
		
		

	 import delimited "${fortrandir}/Robustness NUN/profile_nun_000.csv", numericcols(66) clear 
	 global lversion xsmob_nun
	 
	 cap n destring _all, replace 
	 capture drop _merge
	 merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
	 ren month u_duration		
		
	 cap gen n_duration=u_duration	
	 capture drop _merge 
	 merge 1:1 n_duration using "${outputdata}/cycldurationshift_mog_nun_males.dta"
	 
	 capture drop _merge 
	 merge 1:1 n_duration using "${outputdata}/durationprofiles_mog_nun_males.dta"
	 
	 
	 ** set info on which durations are targeted in the calibration
	capture drop target_all
	gen byte target_all=0
	replace target_all=1 if u_duration<=12 & (u_duration==1 |u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	
	capture drop target_yngprm
	gen byte target_yngprm=0
	replace target_yngprm=1 if u_duration<=12 & (u_duration==2 |u_duration==4 |u_duration==8 |u_duration==10 |u_duration==12)
	
	capture drop target_badtimes
	gen byte target_badtimes=0
	replace target_badtimes=1 if u_duration<=12 & u_duration>=1
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	replace target_goodtimes=1 if u_duration<=8 & u_duration>=1
	
	

	capture drop target_badtimes
	gen byte target_badtimes=0
	
	capture drop target_goodtimes
	gen byte target_goodtimes=0 if u_duration<=8
	

quietly {
cap log close mobdurprofilelog4
log using "${mainresultsdir}/xtra_mobdurprofiles_xsnun_calibration.txt", replace text name(mobdurprofilelog4)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "OCC MOBILITY - UNEMPL DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_raw mean mdur_m mean target_all)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "YOUNG: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_corr_yng mean cmdury_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "PRIME: OCC MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_corr_prm mean cmdurp_m mean target_yngprm)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "HIGH U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=12, c(mean occmob_mog_corr_bad  mean badcmdur_m mean target_badtimes)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "LOW U: MOBILITY - DURATION PROFILE (MODEL/DATA/TARGET)"
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | occmob data | occmob model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=8, c(mean occmob_mog_corr_good   mean gdcmdur_m mean target_goodtimes)
noisily: display ""
  
 
log close mobdurprofilelog4
	

}	
	





** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_amnun
replace survy_d =surv_ymnun
replace survp_d = surv_pmnun
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog4
log using "${mainresultsdir}/xtra_survprofile_xsnun_calibration.txt", replace text name(survprofilelog4)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog4
	

}		
		

********************************************************************************
**
****SURVIVAL PROFILES OF THE NO REALLOCATION CALIBRATIONS *****
**
********************************************************************************		
		
	//===================
	// CALIB 1 
	//===================		
	
	 import delimited "${fortrandir}/Robustness No Reallocation/No Reallocation Model I/profilev16r1_34000.csv", numericcols(66) clear 
	 global lversion noreall
	 
	 cap n destring _all, replace 
	 capture drop _merge
	 merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
	 ren month u_duration		
		



** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_au
replace survy_d =surv_yu
replace survp_d = surv_pu
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog5
log using "${mainresultsdir}/xtra_survprofile_noreall1_calibration.txt", replace text name(survprofilelog5)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog5
	

}		
		
	//===================
	// CALIB 2 
	//===================		
	
	 import delimited "${fortrandir}/Robustness No Reallocation/No Reallocation Model II/profilev16r1_34000.csv", numericcols(66) clear 
	 global lversion noreall2
	 
	 cap n destring _all, replace 
	 capture drop _merge
	 merge 1:1 month using "${mainresultsdir}/survivalprofiles_and_hazards.dta"
	 ren month u_duration		
		



** replace all of the empirical moments from the calibration by the newly calculated ones 
replace surv_d =surv_au
replace survy_d =surv_yu
replace survp_d = surv_pu
		
		
capture drop target_surv
gen target_surv=0
replace target_surv=1 if (u_duration==2 | u_duration==4 | u_duration==8 | u_duration==12 | u_duration==16 | u_duration==20)
		

quietly {
cap log close survprofilelog6
log using "${mainresultsdir}/xtra_survprofile_noreall2_calibration.txt", replace text name(survprofilelog6)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "  SURVIVAL PROFILE  --  ALL "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean surv_d mean surv_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  YOUNG "
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survy_d mean survy_m mean target_surv)
noisily: display ""
noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  " SURVIVAL PROFILE  --  PRIME ""
noisily: display  "--------------------------------------------------------"
noisily: display  ""
noisily: display  " COLS: duration | surv data | surv model | calib target dummy"
version 13
noisily: table u_duration if u_duration<=20, c(mean survp_d mean survp_m mean target_surv)
noisily: display ""
  
 
log close survprofilelog6
	

}		
		
	
	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"