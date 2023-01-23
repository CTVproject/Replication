
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


//=======================================================
// OCCUPATIONAL MEASUREMENT ERROR
//=======================================================


/* This file contains the calculations for the construction of 
the 'garbling' measurement error matrices



comments:
- inverting the matrix is done in a Mathematica notebook, and (quicker) inside
	Matlab (code is only a few lines)
- 



*/

//===================================================
// PRELIMINARIES
//==================================================
	
	
	cd "$workingdir"
	global workingdir `c(pwd)'
	do "${workingdir}/global_paths.do"
	
	version 13
	
	set more off
	set varabbrev off

	
	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"
	
	
cap n ssc install latab, replace
cap n ssc install outtable, replace
			
		capture program drop idfillreplace
		program define idfillreplace
		
			capture drop prs_`1'
			ren `1' prs_`1'
			sort personkey
			by personkey: egen `1'=max(prs_`1')
			drop prs_`1'
		end program idfillreplace 


** GLOBALS
global gnulin=0
* control parts of the do-file
global fileversion=1022     // version of the {panelyear}total_v$fileversion.dta
global locfileversion=1016  // local file version
global distributiontest=0	// testing the 1985 vs 1986 distributions
global original_occdd=1 	// USING ORIGINAL RECODING -- TAKES TIME
global fulloutput=0 		// =1 then outputs different versions of the transition 
							//	matrix and miscode flow matrices


**********
** LOG
***********
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}/ctv_step1log_${logdate}.txt", text
capture noisily log using "${tempdata}/ctv_step1log_${logdate}.txt", append text
	

	
*********
*** MAIN 
**********
	
	
cd "${outputdata}"
use 1985total_v${fileversion}.dta, clear
append using 1986total_v${fileversion}.dta
append using 1987total_v${fileversion}.dta
append using 1988total_v${fileversion}.dta
append using 1984total_v${fileversion}.dta


/*
use 1984total_v1016.dta, clear
append using 1985total_v1016.dta
append using 1986total_v1016.dta
append using 1987total_v816.dta
append using 1988total_raw.dta
*/


// drop noninterviews/children
replace pp_intvw=pp_intv if panel>=1988
drop if pp_intvw>=3 
drop if pp_intvw<1 

replace eeno1=. if eeno1==0 & panel==1988
replace eeno2=. if eeno2==0 & panel==1988

	// ages between 18 and 66
drop if tage<18
drop if tage>66
*drop if pp_intvw==0

// generate employment indicator

		*FP/CORE Employment status, basic
		capture drop c_enu
		capture drop fp_enu
		gen c_enu=.
		gen fp_enu=.
		capture label define enu_label 1 "E" 2 "U" 3 "N"
		lab val c_enu enu_label
		label val fp_enu enu_label
		format %2.0g c_enu
		format %2.0g fp_enu
		replace c_enu=1 if c_rmesr>=1 & c_rmesr<=5
		replace c_enu=2 if c_rmesr>=6 & c_rmesr<=7
		replace c_enu=3 if c_rmesr==8 
		replace fp_enu=1 if fp_rmesr>=1 & fp_rmesr<=5
		replace fp_enu=2 if fp_rmesr>=6 & fp_rmesr<=7
		replace fp_enu=3 if fp_rmesr==8 
		
capture program drop wsummarize
program wsummarize, byable(recall) 
	version 8.2  
	syntax [varlist] [if] [in] [, Format(str) listwise ]
	
	if "`listwise'" != "" local listwise "novarlist" 
	marksample touse, strok `listwise'  
	qui count if `touse' 
	if r(N) == 0 error 2000 

	// variable name width  
	local w = 0 
	foreach v of local varlist { 
		cap confirm string variable `v' 
		if _rc { 
			local w = max(`w', length("`v'")) 
			local vlist `vlist' `v' 
		}	
	}	
	local ++w

	if "`format'" == "" local format %6.0f %9.2f %9.2f %9.2f %9.2f 
	else {
		if _caller() >= 9 { 
			foreach f in `format' { 
				confirm format `f' 
			}
		}	
		
		local nf : word count `format' 
		if `nf' < 5 { 
			local flast : word `nf' of `format' 
			forval j = `= `nf' + 1'/5 { 
				local format `format' `flast' 
			}
		}
	}
	
	tokenize "`format'" 
	local j = 1
	foreach f in `format' { 
		local s = 1 + real(substr("`f'", index("`f'", "%") + 1, 1))
		local s`j++' "%`s's"
	}

	di as txt _n "{space `w'} " `s1' "Obs" `s2' "Mean" ///
		`s3' "SD" `s4' "Min" `s5' "Max" 
	
	foreach v of local vlist { 
		qui su `v' if `touse' 
		di as txt "`v'{col `w'}  "    ///
		" " as res `1' r(N)           /// 
		" " as res `2' r(mean)        /// 
		" " as res `3' r(sd)          /// 
		" " as res `4' r(min)         /// 
		" " as res `5' r(max)          
	}
end 		
		
//================================================		
// industrial and occupational coding and aggregation
//==================================================

if $original_occdd==1 {


// ORIGINAL DD RECODE
* recode occupations (according to the David Dorn's mapping into 1990 SOC, then aggregated)
		cd "$workingdir"
		display "do $workingdir/SIPP 0. initial/occ_recode_1980_to_1990_daviddorn_jan2014.do"
		do "$workingdir/SIPP 0. initial/1984_91_recode_occdd_80_to_90_step0_1_2.do"
		
		cd "${outputdata}"
		
		
		display "running recodes"
		recode_dd80_to_90 fp_tjbocc1 
		recode_dd80_to_90 fp_tjbocc2 
		recode_dd80_to_90 occprevjb 
		recode_dd80_to_90 c_tjbocc1 
		recode_dd80_to_90 c_tjbocc2
		
		capture drop donotconsider_dum
		gen byte donotconsider_dum=.
		
		* using the SOC1980 aggregation
		
		capture program drop aggregate_occ_8691
		program define aggregate_occ_8691


			set more off
			if "`1'"!="" {

			set more off
			set varabbrev off

			global occupation  "`1'"

			// AGGREGATION FOR 1980 SOC ----- THIS IS THE OLD STEPTWO8691.do program	

			capture gen ${occupation}_1d=.

			forvalues i=4(1)37 {
					replace ${occupation}_1d=1 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=43(1)199 {
					replace ${occupation}_1d=2 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}
					
			forvalues i=203(1)235 {
					replace ${occupation}_1d=3 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=243(1)285 {
					replace ${occupation}_1d=4 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=303(1)389 {
					replace ${occupation}_1d=5 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=403(1)407 {
					replace ${occupation}_1d=8 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=413(1)427 {
					replace ${occupation}_1d=8 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=433(1)469 {
					replace ${occupation}_1d=8 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=473(1)476 {
					replace ${occupation}_1d=9 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=477(1)499 {
					replace ${occupation}_1d=9 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=503(1)549 {
					replace ${occupation}_1d=11 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=553(1)617 {
					replace ${occupation}_1d=12 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=633(1)699 {
					replace ${occupation}_1d=13 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=703(1)799 {
					replace ${occupation}_1d=14 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=803(1)859 {
					replace ${occupation}_1d=15 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}

			forvalues i=863(1)889 {
					replace ${occupation}_1d=16 if ${occupation}==`i' &  (panel==1986 | panel==1987 | panel==1988 | panel==1989 | panel==1990 | panel==1991  )
					}
			}
		end 

		*run the program (aggregate_occ_8691) for the variables in question
		aggregate_occ_8691 fp_tjbocc1 
		aggregate_occ_8691 fp_tjbocc2 
		aggregate_occ_8691 occprevjb 
		aggregate_occ_8691 c_tjbocc1 
		aggregate_occ_8691 c_tjbocc2

		* hardly any difference in the two occupational codes (_dd_1d and _1d)
		count if occprevjb_1d!=.
		count if occprevjb_dd_1d!=.
		count if occprevjb_dd_1d!=occprevjb_1d
		
		count if c_tjbocc1_dd_1d!=.
		count if c_tjbocc1_1d!=.
		count if c_tjbocc1_dd_1d!=c_tjbocc1_1d


cd "${outputdata}"
*compress
*save 8488measurement.dta, replace 
*use 8586measurement.dta, clear
}

// BASIC Aggregated

** AGGREGATION -- STANDARD 
capture program drop basicagg_exe
program define basicagg_exe

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
		replace `aggvar_out'=6 if `var_in'>=403 & `var_in'<=407  & `added_condition'
		replace `aggvar_out'=7 if `var_in'>=413 & `var_in'<=427  & `added_condition'
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
		4 "sales occ." 5 "admin support" 6 "priv hh serv" 7 "protective serv" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val `aggvar_out' label_1dd		

end // basicagg_exe

capture program drop basicagg13_exe
program define basicagg13_exe

display " ============RECODING AT A MAJOR OCCUPATIONAL GROUP LEVEL (13 OCCUPATIONS) =========================="

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
		4 "sales occ." 5 "admin support" 6 "priv hh serv" 7 "protective serv" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val `aggvar_out' label_1dd		

end // basicagg13_exe





// DD aggregated from dta
capture program drop recode80dd90merge_orig
program define recode80dd90merge_orig

local var_in="`1'"
local var_out="`2'"
cap n drop occ
gen occ=`var_in' if panel<=1991 	
capture drop _merge80ms
merge m:1 occ using "${workingdir}/SIPP 0. initial/occ_ind_recodes/80dd90.dta", gen(_merge80ms)
drop if _merge80ms==2
capture drop `var_out'
ren occ1990dd `var_out'

end // recode80dd90merge_orig


capture program drop recode80dd90merge_adj
program define recode80dd90merge_adj

local var_in="`1'"
local var_out="`2'"
cap n drop occ
gen occ=`var_in' if panel<=1991 	
capture drop _merge80ms
merge m:1 occ using "${workingdir}/SIPP 0. initial/occ_ind_recodes/80dd90adj.dta", gen(_merge80ms)
drop if _merge80ms==2
capture drop `var_out'
ren occ1990dd `var_out'

end // recode80dd90merge_adj


// HS


// AD

cap n program drop dd3_2_ad_exe
program define dd3_2_ad_exe

	local var_in="`1'"
	local aggvar_out="`2'"

	if "`3'"==""{
local added_condition=" panel>=1984 "
}
if "`3'"!=""{
local added_condition="`3'"
}

	
		replace `aggvar_out'=1 if `var_in'>=4 & `var_in'<=37   & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=43 & `var_in'<=199   & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=203 & `var_in'<=235     & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=243 & `var_in'<=258   & `added_condition'
		replace `aggvar_out'=2 if `var_in'>=274 & `var_in'<=285   & `added_condition' // only retail sales, no clue what happens to wholesale sellers, 259-273?
		replace `aggvar_out'=2 if `var_in'>=303 & `var_in'<=389  & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=403 & `var_in'<=408  & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=413 & `var_in'<=427  & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=433 & `var_in'<=471  & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=408 & `var_in'<=408  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		replace `aggvar_out'=6 if `var_in'>=472 & `var_in'<=476  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=477 & `var_in'<=499  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=503 & `var_in'<=549  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=553 & `var_in'<=617 & `added_condition'
		replace `aggvar_out'=4 if `var_in'>=628 & `var_in'<=699  & `added_condition'
		replace `aggvar_out'=5 if `var_in'>=702 & `var_in'<=702  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=5 if `var_in'>=703 & `var_in'<=799  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=803 & `var_in'<=859  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=863 & `var_in'<=889  & `added_condition'

end // dd3_2_ad_exe


// adjusted: which management/laborer occ should go where 


// SOC2000



// INDUSTRIES

	* aggregate industry 
	
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

		end // indagg_exe


mvdecode eeno1 eeno2, mv(9)



//======================================================
// SELECT THE SPELLS NEEDED FOR COMPARISON EXERCISE
//=======================================================



* select a spell: if single firm at each month before and after seam, full employment during the four months before and after seam
* not imputed occupation

* mark indicator in the first month after the transition
mvdecode eeno1, mv(0)
mvdecode eeno2, mv(0)
sort personkey yearmonth
capture drop select_ind*
capture drop selectmonth
gen byte select_ind=.
gen byte select_ind1=.
gen byte select_ind2=.
gen byte select_ind3=.
gen byte select_ind4=.
gen byte select_ind5=.
gen byte select_ind6=.
gen byte select_ind7=.
gen byte select_ind8=.
gen byte select_indall=.


replace select_ind1=1 if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey[_n+3]==personkey[_n-4] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1 & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									   /// & c_enu[_n-1]==1 & c_enu==1 & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 //  & eenrlm>1 & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 

replace select_ind2=1 if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 

replace select_ind3=1 if wave>=2 & wave[_n-4]==wave-1 & wave[_n+3]==wave & srefmon==1 & personkey==personkey[_n-4] & personkey==personkey[_n+3] ///
									& fp_enu[_n-4]==1 & fp_enu[_n-4]==1 & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n-1]==1 & fp_enu==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1& fp_enu[_n+3 ]==1 ///
                                    & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4)

replace select_ind4=1 if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & pp_intvw==1 //   & srefmon[_n-4]==1 & srefmon[_n+3]==4 

replace select_ind5=1 if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1   & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
									
replace select_ind6=1 if tage>19 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_ind6=1 if select_ind6==1 & pp_intvw!=0
replace select_ind6=0 if select_ind6==1 & pp_intvw==0

replace select_ind7=1 if tage>19 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_ind7=1 if select_ind7==1 & pp_intvw!=0
replace select_ind7=0 if select_ind7==1 & pp_intvw==0

replace select_ind8=1 if tage>17 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_ind8=1 if select_ind8==1 & pp_intvw!=0
replace select_ind8=0 if select_ind8==1 & pp_intvw==0

capture gen select_ind9=.
replace select_ind9=.
replace select_ind9=1 if tage>19 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_ind9=1 if select_ind9==1 & pp_intvw!=0
replace select_ind9=0 if select_ind9==1 & pp_intvw==0


capture gen select_ind10=.
replace select_ind10=.
replace select_ind10=1 if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & fp_eeno1!=. & fp_eeno2==. & fp_eeno1[_n-1]!=. & fp_eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_ind10=1 if select_ind9==1 & pp_intvw!=0
replace select_ind10=0 if select_ind9==1 & pp_intvw==0
tab select_ind10 panel



									
// main selection set
replace select_ind=1 if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_ind=1 if select_ind==1 & pp_intvw!=0
replace select_ind=0 if select_ind==1 & pp_intvw==0
tab select_ind
tab select_ind panel
tab select_ind10 panel

count if select_ind==1									
count if select_ind==1 & panel==1985									
count if select_ind==1 & panel>=1986									

count if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & yearmonth>=tm(1986m2) & yearmonth<=tm(1987m4) ///
									 & eenrlm==1 & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 


// all panels: 1984 - 1988 panels
sort personkey yearmonth
replace select_indall=1 if tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=.  ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_indall=1 if select_ind==1 & pp_intvw!=0
replace select_indall=0 if select_ind==1 & pp_intvw==0
tab select_indall
tab select_indall panel
count if select_indall==1									
count if select_indall==1 & panel==1985									
count if select_indall==1 & panel>=1986									
// allow for fp_eeno1 in wave2 of 1985
mvdecode fp_eeno1, mv(0)
mvdecode fp_eeno2, mv(0)
replace select_indall=1 if wave<=4 & panel==1985 & tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & fp_eeno1!=. & eeno1==. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=.  ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_indall=1 if wave<=4 & panel==1985 & tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & fp_eeno1!=. & eeno1==. & eeno2==. & fp_eeno1[_n-1]!=. & eeno1[_n-1]==. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=.  ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
replace select_indall=1 if wave<=4 & panel==1985 & tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & fp_eeno1[_n-1]!=. & eeno1[_n-1]==. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=.  ///
									 & (eenrlm!=1 & eenrlm!=2)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 

// what's the deal with 1988: eeno1 eeno2 have zeros when missing --> fixed

// what's the deal with 1984: eenrlm has a different meaning 1=yes, enrolled, 2=not enrolled, it seems
replace select_indall=1 if panel==1984 & tage>18 & tage<66 & wave>=2 & wave[_n-1]==wave-1 &  srefmon==1 & personkey==personkey[_n-1] /// & personkey==personkey[_n+3] 
									 & fp_enu[_n-1]==1 & fp_enu==1  /// & fp_enu[_n-4]==1 & fp_enu[_n-3]==1 & fp_enu[_n-2]==1 & fp_enu[_n+1]==1 & fp_enu[_n+2]==1 & fp_enu[_n+3]==1  ///
									 & c_enu[_n-1]==1 & c_enu==1   /// & c_enu[_n-4]==1 & c_enu[_n-3]==1 & c_enu[_n-2]==1 & c_enu[_n+1]==1 & c_enu[_n+2]==1 & c_enu[_n+3]==1  ///
									 & eeno1!=. & eeno2==. & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=.  ///
									 & (eenrlm!=1)  & (ebno1==.|ebno1==0) & (ebno2==.|ebno2==0) & (ebno1[_n-1]==.|ebno1[_n-1]==0) & (ebno2[_n-1]==.|ebno2[_n-1]==0) & c_ejbhrs1>=20 & c_ejbhrs1[_n-1]>=20 //  & pp_intvw!=0 & srefmon[_n-4]==1 & srefmon[_n+3]==4 
									 
									 

tab wave rot if panel==1985 & eenrlm!=1 & eenrlm!=2 & iws1occ==0 
tab wave rot if panel==1985 & eenrlm!=1 & eenrlm!=2 & iws1occ==0 ///
& eeno1!=. & eeno2==.  & eeno1[_n-1]!=. & eeno2[_n-1]==. & iws1occ==0 & iws1occ[_n-1]==0 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=.

tab fp_eeno1 rot if panel==1985 & eenrlm!=1 & eenrlm!=2 & iws1occ==0  & wave==2, m
									
capture drop selectmonth
gen selectmonth=yearmonth if select_ind==1
replace selectmonth=0 if select_ind!=1 & select_indall==1

tab pp_intvw panel
tab pp_intv panel
tab pp_mis panel

tab select_ind panel

tab sex panel if select_ind==1, col nof
ci sex if select_ind==1 & panel==1985
ci sex if select_ind==1 & panel==1986


//==========================================================================
//  FURTHER RECODING
//==========================================================================

* occupations

capture drop occafter
capture drop occbefore
capture drop occ3bfr_b
capture drop occ3aft_b
capture drop occ3bfr_ddo
capture drop occ3aft_ddo


gen occafter=.
gen occbefore=.
gen occ3bfr_b=.
gen occ3aft_b=.
gen occ3bfr_ddo=.
gen occ3aft_ddo=.


if $original_occdd==1 {
* original dd measure
sort personkey yearmonth
capture drop occafter
capture drop occbefore
gen occafter=c_tjbocc1_dd_1d if selectmonth!=. & c_tjbocc1_dd_1d!=.
gen occbefore=c_tjbocc1_dd_1d[_n-1] if selectmonth!=. & personkey==personkey[_n-1] & wave==wave[_n-1]+1
}

* original 3digit measure
capture drop occ3bfr_b
capture drop occ3aft_b
gen occ3aft_b=c_tjbocc1 if selectmonth!=. & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. & iws1occ!=1
gen occ3bfr_b=c_tjbocc1[_n-1] if selectmonth!=. & personkey==personkey[_n-1] & wave==wave[_n-1]+1 & iws1occ[_n-1]!=1 & c_tjbocc1!=. & c_tjbocc1[_n-1]!=. 


* newly merged dd measure (DDN)
capture drop occ3bfr_ddo
capture drop occ3aft_ddo
recode80dd90merge_orig occ3bfr_b occ3bfr_ddo
recode80dd90merge_orig occ3aft_b occ3aft_ddo






recode80dd90merge_adj occ3bfr_b occ3bfr_ddn
recode80dd90merge_adj occ3aft_b occ3aft_ddn
basicagg_exe occ3bfr_ddn occ1bfr_ddn
basicagg_exe occ3aft_ddn occ1aft_ddn
basicagg_exe occ3bfr_ddo occ1bfr_ddo
basicagg_exe occ3aft_ddo occ1aft_ddo

* 13 occupations, as used in the paper
basicagg13_exe occ3bfr_ddn occ1bfr_13dd
basicagg13_exe occ3aft_ddn occ1aft_13dd



* AD


cap n drop occ1bfr_ad
cap n drop occ1aft_ad
gen occ1bfr_ad=.
gen occ1aft_ad=.
 
dd3_2_ad_exe occ3bfr_ddn occ1bfr_ad
dd3_2_ad_exe occ3aft_ddn occ1aft_ad

* HS
cap n drop occ1bfr_hs
cap n drop occ1aft_hs

gen occ1bfr_hs=.
gen occ1aft_hs=.

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

dd2hs_exe occ1bfr_ddn occ1bfr_hs 
dd2hs_exe occ1aft_ddn occ1aft_hs 

* HS2 -- transportation into NRM


* HS
cap n drop occ1bfr_hs2
cap n drop occ1aft_hs2

gen occ1bfr_hs2=.
gen occ1aft_hs2=.

cap n program drop dd2hs2_exe
program define dd2hs2_exe

	
	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 | `ddvar'==3
	replace `var_out'=2 if `ddvar'==4 | `ddvar'==5 
	replace `var_out'=3 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8 | `ddvar'==15 
	replace `var_out'=4 if `ddvar'==11 | `ddvar'==12 | `ddvar'==13 | `ddvar'==14 | `ddvar'==16
	* agriculture to missing
	
end 

dd2hs2_exe occ1bfr_ddn occ1bfr_hs2
dd2hs2_exe occ1aft_ddn occ1aft_hs2


* 3 CAT
cap n drop occ1bfr_3cat
cap n drop occ1aft_3cat

gen occ1bfr_3cat=.
gen occ1aft_3cat=.

cap n program drop dd23cat_exe
program define dd23cat_exe

	
	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 | `ddvar'==3 |  `ddvar'==4 | `ddvar'==5 
	replace `var_out'=2 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8 | `ddvar'==15 
	replace `var_out'=3 if `ddvar'==11 | `ddvar'==12 | `ddvar'==13 | `ddvar'==14 | `ddvar'==16
	* agriculture to missing
	
end 

dd23cat_exe occ1bfr_ddn occ1bfr_3cat
dd23cat_exe occ1aft_ddn occ1aft_3cat


* SOG

cap n drop occ1bfr_sog
cap n drop occ1aft_sog

gen occ1bfr_sog=.
gen occ1aft_sog=.

cap n program drop dd2sog_exe
program define dd2sog_exe

	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 
	replace `var_out'=2 if `ddvar'==3| `ddvar'==4 | `ddvar'==5 
	replace `var_out'=3 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8
	replace `var_out'=4 if `ddvar'==9 | `ddvar'==10
	replace `var_out'=5 if  `ddvar'==11 | `ddvar'==12 | `ddvar'==13 
	replace `var_out'=6 if  `ddvar'==14 | `ddvar'==15 | `ddvar'==16
	* agriculture to missing
end 

dd2sog_exe occ1bfr_ddn occ1bfr_sog 
dd2sog_exe occ1aft_ddn occ1aft_sog 

* 22 (2000 SOC)

cap n drop occ1bfr_mm
cap n drop occ1aft_mm

gen occ1bfr_mm=.
gen occ1aft_mm=.

cap n program drop merge2000soc_exe
program define merge2000soc_exe

local ddvar_in="`1'"
local aggvar_out="`2'"

capture drop occ1990dd
gen occ1990dd=`ddvar_in'

capture drop _merge
merge m:1 occ1990dd using "${workingdir}/SIPP 0. initial/occ_ind_recodes/occ1990dd2000soc.dta"

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

merge2000soc_exe occ3bfr_ddn occ1bfr_mm
merge2000soc_exe occ3aft_ddn occ1aft_mm



* industries
sort personkey yearmonth
capture drop ind3bfr_b
capture drop ind3aft_b
gen ind3aft_b=c_ejbind1 if selectmonth!=. & c_ejbind1!=. & c_ejbind1[_n-1]!=. & personkey==personkey[_n-1] & wave==wave[_n-1]+1 & iws1ind!=1 & iws1ind[_n-1]!=1
gen ind3bfr_b=c_ejbind1[_n-1] if selectmonth!=. & personkey==personkey[_n-1] & wave==wave[_n-1]+1 & c_ejbind1!=. & c_ejbind1[_n-1]!=. & iws1ind!=1 & iws1ind[_n-1]!=1

indagg_exe	ind3bfr_b ind1bfr_b	
indagg_exe	ind3aft_b ind1aft_b


// 20 groups, with management & related moved to sales/prof specialty/office-admin suport, and helpers out of transport/mat moving 




//=================================
//  WEIGHTS
//===================================

// normalize weights across 1985 vs (1986+1987)
capture drop pweight85
capture drop pweight85sum
capture drop pweight86
capture drop pweight86sum
capture drop pweight8687
capture drop pweight8687sum


capture drop pweight85_2
capture drop pweight85sum_2
capture drop pweight86_2
capture drop pweight86sum_2
capture drop pweight8687_2
capture drop pweight8687sum_2

capture drop fnlwgt
gen fnlwgt=fnlwgt86 if year==1986
replace fnlwgt=fnlwgt87 if year==1987 & panel>=1986
replace fnlwgt=wpfinwgt if year==1987 & panel==1985

* RESET SELECTMONTH FOR WEIGHTING
replace selectmonth=. if selectmonth==0

sort panel selectmonth
*bysort selectmonth: egen pweight85sum=sum(fnlwgt) if panel==1985 & select_ind==1
*bysort selectmonth: egen pweight86sum=sum(fnlwgt) if panel==1986 & select_ind==1
*bysort selectmonth: egen pweight8687sum=sum(fnlwgt) if (panel==1986 | panel==1987) & select_ind==1


bysort selectmonth: egen pweight85sum=sum(wpfinwgt) if panel==1985 & select_ind==1
bysort selectmonth: egen pweight86sum=sum(wpfinwgt) if panel==1986 & select_ind==1
bysort selectmonth: egen pweight8687sum=sum(wpfinwgt) if (panel==1986 | panel==1987) & select_ind==1



egen pweight85sum_2=sum(wpfinwgt) if panel==1985 & select_ind==1
egen pweight86sum_2=sum(wpfinwgt) if panel==1986 & select_ind==1
egen pweight8687sum_2=sum(wpfinwgt) if (panel==1986 | panel==1987) & select_ind==1


sort personkey yearmonth

*gen pweight85=fnlwgt/pweight85sum if panel==1985 & selectmonth!=.
*gen pweight86=fnlwgt/pweight86sum if panel==1986 & selectmonth!=.
*gen pweight8687=fnlwgt/pweight8687sum if (panel==1986 | panel==1987) & selectmonth!=.

gen pweight85=wpfinwgt/pweight85sum if panel==1985 & selectmonth!=.
gen pweight86=wpfinwgt/pweight86sum if panel==1986 & selectmonth!=.
gen pweight8687=wpfinwgt/pweight8687sum if (panel==1986 | panel==1987) & selectmonth!=.

gen pweight85_2=wpfinwgt/pweight85sum_2 if panel==1985 & selectmonth!=.
gen pweight86_2=wpfinwgt/pweight86sum_2 if panel==1986 & selectmonth!=.
gen pweight8687_2=wpfinwgt/pweight8687sum_2 if (panel==1986 | panel==1987) & selectmonth!=.


/*
sort panel selectmonth
bysort selectmonth: egen pweight85sum=sum(fnlwgt) if panel==1985 & select_ind==1
bysort selectmonth: egen pweight86sum=sum(fnlwgt) if panel==1986 & select_ind==1
bysort selectmonth: egen pweight8687sum=sum(fnlwgt) if panel==1986 | panel==1987 & select_ind==1

sort personkey yearmonth
gen pweight85=fnlwgt/pweight85sum if panel==1985 & selectmonth!=.
gen pweight86=fnlwgt/pweight86sum if panel==1986 & selectmonth!=.
gen pweight8687=fnlwgt/pweight8687sum if (panel==1986 | panel==1987) & selectmonth!=.
*/

capture drop pweight_all
gen pweight_all=pweight85 if panel==1985
replace pweight_all=pweight8687 if panel==1986 | panel==1987


capture drop pweight_all_2
gen pweight_all_2=pweight85_2 if panel==1985
replace pweight_all_2=pweight8687_2 if panel==1986 | panel==1987


capture drop panel2
gen panel2=1985 if panel==1985
replace panel2=1986 if panel==1986 | panel==1987
svyset su_id [pw=pweight_all],  strata(strat)

		//===========
		// check select_ind==1

		// Overall coding agreement in the 1985 panel sample
		tab occ1bfr_13dd occ1aft_13dd if tage>=20 & tage<=66 & pp_intvw!=0 & select_ind==1 & panel==1985
		display r(N)  // XXXXX 1985 panel count
		tab occ1bfr_13dd occ1aft_13dd if tage>=20 & tage<=66 & pp_intvw!=0 & select_ind==1 & panel==1986
		display r(N)	// XXXXX 1986 panel count
		tab occ1bfr_13dd occ1aft_13dd if tage>=20 & tage<=66 & pp_intvw!=0 & select_ind==1 & panel==1987
		display r(N)	// XXXXXX 1987 panel count
		tab panel if tage>=20 & tage<=66 & pp_intvw!=0 & select_indall==1,m  

		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
// =================================================================================
//		
//    PART ONE: WORK ON THE 1985-1986: in the 1985-86 measurement_error file 
//
// =================================================================================
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
//===============================================
// TEST THE DISTRIBUTION
//===============================================


// LOOSE TESTING OF GENDER

				local vvar sex
				local lselect_ind="select_ind10"
				svy: mean `vvar' if `lselect_ind'==1 & panel<=1987, over(panel2)
				lincom [`vvar']1985-[`vvar']1986
				* to report p-value
				te [`vvar']1985-[`vvar']1986=0
				


// LOOSE TESTING OF OCCUPATIONS
				capture drop dum_toccbefore*
				tab occ1bfr_13dd, gen(dum_toccbefore)
				local vvar dum_toccbefore1
				svy: mean `vvar' if `lselect_ind'==1 & panel<=1987, over(panel2)
				lincom [`vvar']1985-[`vvar']1986
				* to report p-value
				te [`vvar']1985-[`vvar']1986=0
				capture drop dum_toccbefore*





				// occupation after
				capture drop dum_toccafter*
				tab occ1aft_13dd, gen(dum_toccafter)
				local vvar dum_toccafter1
				svy: mean `vvar' if `lselect_ind'==1 & panel<=1987, over(panel2)
				lincom [`vvar']1985-[`vvar']1986
				* to report p-value
				te [`vvar']1985-[`vvar']1986=0
				capture drop dum_toccafter*





				capture program drop tdist_stats
				program define tdist_stats
					args vvar lselect_ind lcc
				
				svy: mean `vvar' if `lselect_ind'==1 & panel<=1987, over(panel)
				matrix aa=r(table)
				matrix aa=aa[1,1..3]
				putexcel A`lcc'=("`vvar'")
				putexcel B`lcc'=matrix(aa)
				
				svy: mean `vvar' if `lselect_ind'==1 & panel<=1987, over(panel2)
				lincom [`vvar']1985-[`vvar']1986
				* to report p-value
				te [`vvar']1985-[`vvar']1986=0
				putexcel E`lcc'=(r(p))
				end 
				

				cap n program drop distributiontest_exe
				program define distributiontest_exe
					args select_ind xlsa
				
				*local select_ind="select_ind"
				
				*display "`1'"
				*local select_ind="`1'"
				if "`xlsa'"!="" {
				local xls="`xlsa'"
				}
				if "`xlsa'"=="" {
				local xls="distributionoutput2"
				
				}
				local cc=1
				putexcel set "${step1resultsdir}/`xls'.xls", sheet("distribution", replace) replace
				putexcel B`cc'=("1985") C1=("1986") D1=("1987") E1=("p-value")
				local cc=`cc'+1
				
				
				
				// gender
				local cc=2
				display "`select_ind'"
				tdist_stats sex `select_ind' `cc'
				*tdist_stats sex select_ind 2
				local cc=`cc'+1
				
				* effect of sample design is meaningful
				*mean sex [pw=pweight_all] if `select_ind'==1 & panel<=1987, over(panel2)
				*lincom [sex]1985-[sex]1986

				// marital status
				capture drop married
				capture drop nevermarried
				gen married=0 if `select_ind'==1
				replace married=1 if married==0 & ms<=2
				gen nevermarried=0 if `select_ind'==1
				replace nevermarried=1 if nevermarried==0 & ms==6

				
				tdist_stats married `select_ind' `cc'
				local cc=`cc'+1
				
				
				
				cap n lab define married_label 1 "married" 0 "not married"
				lab val married married_label 
				latab married panel [aw=wpfinwgt], col dec(2)

				svy: mean nevermarried if `select_ind'==1 & panel<=1987, over(panel2)
				lincom [nevermarried]1985-[nevermarried]1986

				cap n lab define nevermarried_label 1 "never married" 0 "married before"
				lab val nevermarried nevermarried_label 
				latab nevermarried panel [aw=wpfinwgt], col dec(2)
				// education
				capture drop dum_educ*
				tab educ if `select_ind'==1, gen (dum_educ)

				forvalues i=1(1)4 {
				*svy: mean dum_educ`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_educ`i']1985-[dum_educ`i']1986
				tdist_stats dum_educ`i' `select_ind' `cc'
				local cc=`cc'+1
				}

				// race 
				capture drop dum_race*
				tab race if `select_ind'==1, gen (dum_race)

				forvalues i=1(1)4 {
				*svy: mean dum_race`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_race`i']1985-[dum_race`i']1986
				tdist_stats dum_race`i' `select_ind' `cc'
				local cc=`cc'+1
				}

				// occupations
				capture drop dum_occbefore*
				tab occ1bfr_13dd, gen(dum_occbefore)
				su occb*

				forvalues i=1(1)13 {
				*svy: mean dum_occbefore`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_occbefore`i']1985-[dum_occbefore`i']1986
				tdist_stats dum_occbefore`i' `select_ind' `cc'
				local cc=`cc'+1
				}




				// occupation after
				capture drop dum_occafter*
				tab occ1aft_13dd, gen(dum_occafter)
				forvalues i=1(1)13 {
				*svy: mean dum_occafter`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_occafter`i']1985-[dum_occafter`i']1986
				tdist_stats dum_occafter`i' `select_ind' `cc'
				local cc=`cc'+1
				}

				// industry before
				capture drop dum_ind1bfr*
				tab ind1bfr_b, gen(dum_ind1bfr)
				forvalues i=1(1)15 {
				*svy: mean dum_ind1bfr`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_ind1bfr`i']1985-[dum_ind1bfr`i']1986
				tdist_stats dum_ind1bfr`i' `select_ind' `cc'
				local cc=`cc'+1
				
				}


				// industry after
				capture drop dum_ind1aft*
				tab ind1aft_b, gen(dum_ind1aft)
				forvalues i=1(1)15 {
				*svy: mean dum_ind1aft`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_ind1aft`i']1985-[dum_ind1aft`i']1986
				tdist_stats dum_ind1aft`i' `select_ind' `cc'
				local cc=`cc'+1
				
				}

				// agegroups
				local select_ind="select_ind"
				capture drop agegroup*
				gen byte agegroup1=0
				gen byte agegroup2=0
				gen byte agegroup3=0
				gen byte agegroup4=0
				gen byte agegroup5=0
				gen byte agegroup6=0
				gen byte agegroup7=0
				gen byte agegroup8=0
				gen byte agegroup9=0
				replace   agegroup1=1 if tage>=19 & tage<25
				replace   agegroup2=1 if tage>=25 & tage<30
				replace   agegroup3=1 if tage>=30 & tage<35
				replace   agegroup4=1 if tage>=35 & tage<40
				replace   agegroup5=1 if tage>=40 & tage<45
				replace   agegroup6=1 if tage>=45 & tage<50
				replace   agegroup7=1 if tage>=50 & tage<55
				replace   agegroup8=1 if tage>=55 & tage<60
				replace   agegroup9=1 if tage>=60 & tage<65
				forvalues i=1(1)9 {
				svy: mean agegroup`i' if `select_ind'==1 & panel<1987, over(panel2)
				lincom [agegroup`i']1985-[agegroup`i']1986
				tdist_stats agegroup`i' `select_ind' `cc'
				local cc=`cc'+1
				
				}

				
				tab sex panel [iw=pweight_all] if `select_ind'==1
				tab sex panel [iw=pweight_all] if `select_ind'==1, col nof
				tab sex panel [aw=wpfinwgt] if `select_ind'==1, col nof

				*gen panel2=1985 if panel==1985
				*replace panel2=1986 if panel>=1986 & panel<=1987

				// metropolitan 
				*svy: mean tmetro if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [tmetro]1985-[tmetro]1986
				tdist_stats tmetro `select_ind' `cc'
				local cc=`cc'+1
				
					// some summary statistics

					tab fp_tfipsst panel [iw=pweight_all] if `select_ind'==1
					tab tmetro panel [iw=pweight_all] if `select_ind'==1
					tab tmetro panel [iw=pweight_all] if `select_ind'==1, col nof
					tab pp_intvw panel [iw=pweight_all] if `select_ind'==1, col nof
					tab pp_mis panel [iw=pweight_all] if `select_ind'==1, col nof
					*tab pp_mis_core panel [iw=pweight_all] if `select_ind'==1, col nof
					tab educ panel [iw=pweight_all] if `select_ind'==1, col nof
					tab educ panel if `select_ind'==1, col nof
					tab educ sex if `select_ind'==1 & panel==1985, col nof
					tab educ sex if `select_ind'==1 & panel==1986, col nof

				cap n lab define panellabel 1985 "1985" 1986 "1986" 1987 "1987"
				cap n lab val panel panellabel
				cap n lab define metrolabel 1 "metro area" 2 "non-metro area"
				cap n lab val tmetro metrolab
				cap n lab val occbefore label_1dd
				cap n lab val occafter label_1dd

				latab educ panel [aw=pweight_all] if `select_ind'==1 , col dec(2)
				latab sex panel [aw=pweight_all] if `select_ind'==1 , col dec(2)
				latab tmetro panel [aw=pweight_all] if `select_ind'==1 , col dec(2)
				latab race panel [aw=pweight_all] if `select_ind'==1 , col dec(2)
				latab occbefore panel [aw=pweight_all] if `select_ind'==1 , col dec(2) 
				latab occafter panel [aw=pweight_all] if `select_ind'==1 , col dec(2) 

				cap n label define select_label 1 "select"
				cap n lab val `select_ind' select_label
				latab `select_ind' panel 

				// count estlemp1 and ws1_2004
				tab estlemp1 panel if `select_ind'==1 
				tab ws1_2004 panel if `select_ind'==1

				end // distributiontest_exe

				
				
				
				
				cap n program drop distributiontest_shortexe
				program define distributiontest_shortexe
					args select_ind xlsa
				
				*local select_ind="select_ind"
				
				*display "`1'"
				*local select_ind="`1'"
				if "`xlsa'"!="" {
				local xls="`xlsa'"
				}
				if "`xlsa'"=="" {
				local xls="distributionoutput2"
				
				}
				local cc=1
				putexcel set "`xls'.xls", sheet("distribution", replace) replace
				putexcel B`cc'=("1985") C1=("1986") D1=("1987") E1=("p-value")
				local cc=`cc'+1
				
				
				
				// gender
				local cc=2
				display "`select_ind'"
				tdist_stats sex `select_ind' `cc'
				*tdist_stats sex select_ind 2
				local cc=`cc'+1
				
				* effect of sample design is meaningful
				*mean sex [pw=pweight_all] if `select_ind'==1 & panel<=1987, over(panel2)
				*lincom [sex]1985-[sex]1986

				// marital status
				capture drop married
				capture drop nevermarried
				gen married=0 if `select_ind'==1
				replace married=1 if married==0 & ms<=2
				gen nevermarried=0 if `select_ind'==1
				replace nevermarried=1 if nevermarried==0 & ms==6

				
				tdist_stats married `select_ind' `cc'
				local cc=`cc'+1
				
				
				/*
				cap n lab define married_label 1 "married" 0 "not married"
				lab val married married_label 
				latab married panel [aw=wpfinwgt], col dec(2)

				svy: mean nevermarried if `select_ind'==1 & panel<=1987, over(panel2)
				lincom [nevermarried]1985-[nevermarried]1986

				cap n lab define nevermarried_label 1 "never married" 0 "married before"
				lab val nevermarried nevermarried_label 
				latab nevermarried panel [aw=wpfinwgt], col dec(2)
				// education
				capture drop dum_educ*
				tab educ if `select_ind'==1, gen (dum_educ)

				forvalues i=1(1)4 {
				*svy: mean dum_educ`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_educ`i']1985-[dum_educ`i']1986
				tdist_stats dum_educ`i' `select_ind' `cc'
				local cc=`cc'+1
				}

				// race 
				capture drop dum_race*
				tab race if `select_ind'==1, gen (dum_race)

				forvalues i=1(1)4 {
				*svy: mean dum_race`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_race`i']1985-[dum_race`i']1986
				tdist_stats dum_race`i' `select_ind' `cc'
				local cc=`cc'+1
				}
				*/
				// occupations
				capture drop dum_occbefore*
				tab occ1bfr_13dd, gen(dum_occbefore)
				su occb*

				forvalues i=1(1)13 {
				*svy: mean dum_occbefore`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_occbefore`i']1985-[dum_occbefore`i']1986
				tdist_stats dum_occbefore`i' `select_ind' `cc'
				local cc=`cc'+1
				}




				// occupation after
				capture drop dum_occafter*
				tab occ1aft_13dd, gen(dum_occafter)
				forvalues i=1(1)13 {
				*svy: mean dum_occafter`i' if `select_ind'==1 & panel<1987, over(panel2)
				*lincom [dum_occafter`i']1985-[dum_occafter`i']1986
				tdist_stats dum_occafter`i' `select_ind' `cc'
				local cc=`cc'+1
				}

			
				end // distributiontest_shortexe

				/*
				if "`2'"!=""{
					* write to table 
					* outtable using table1, mat(v)
				}
				*/
				
distributiontest_shortexe select_ind shdist_standard

if ${distributiontest}==1 {
	distributiontest_shortexe select_ind6 shdist_version6
	distributiontest_shortexe select_ind7 shdist_version7
	distributiontest_shortexe select_ind8 shdist_version8				
	distributiontest_shortexe select_ind9 shdist_version9				
					
	distributiontest_exe select_ind dist_standard
	distributiontest_exe select_ind6 dist_version6
	distributiontest_exe select_ind7 dist_version7
	distributiontest_exe select_ind8 dist_version8				
	distributiontest_exe select_ind9 dist_version9				
	distributiontest_exe select_ind9 dist_version10				
}				

tab select_ind panel
			/* testing the composition */
			tab c_tjbocc1_dd_1d [aw=pweight86] if selectmonth==tm(1986m2) & panel==1986
			tab c_tjbocc1_dd_1d [aw=pweight85] if selectmonth==tm(1986m2) & panel==1985
			tab c_tjbocc1_dd_1d [aw=pweight85] if selectmonth==tm(1986m3) & panel==1985
			tab c_tjbocc1_dd_1d [aw=pweight86] if selectmonth==tm(1986m3) & panel==1986
			tab c_tjbocc1_dd_1d [aw=pweight86] if selectmonth==tm(1986m4) & panel==1986
			tab c_tjbocc1_dd_1d [aw=pweight85] if selectmonth==tm(1986m4) & panel==1985
			tab c_tjbocc1_dd_1d [aw=pweight85] if selectmonth==tm(1987m1) & panel==1985
			tab c_tjbocc1_dd_1d [aw=pweight8687] if selectmonth==tm(1987m1) & (panel==1986 | panel==1987)

			
// recoded in 1986 and 1987: estlemp1==2 and ws1_2004==1
							tab c_tjbocc1_dd_1d [aw=pweight86] if selectmonth==tm(1986m2) & panel==1986 & (estlemp1==2 | ws1_2004==1)
	
	
/*
// generate occupations of focus, incl. lagged occupation

						tab occbefore occafter [aw=pweight86] if selectmonth==tm(1986m2) & panel==1986 & (estlemp1==2 | ws1_2004==1), row nof
						tab occbefore occafter [aw=pweight86] if selectmonth==tm(1986m2) & panel==1986 & ~(estlemp1==2 | ws1_2004==1), row nof	
						tab occbefore occafter [aw=pweight86] if selectmonth==tm(1986m2) & panel==1986 & ~(estlemp1==2 | ws1_2004==1) & estlemp1!=0
						tab occbefore occafter [aw=pweight86] if selectmonth==tm(1986m2) & panel==1986 & ~(estlemp1==2 | ws1_2004==1) & estlemp1!=0, cell nof
									* essentially no deviations changes for those that do not change employer and activity. 
									** STILL WANT TO DO DISTRIBUTIONAL ANALYSIS
						tab occbefore occafter [aw=pweight8687] if selectmonth==tm(1986m2) & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1),  matcell(temp_b313)
									
						tab occbefore occafter [aw=pweight85] if selectmonth==tm(1986m2) & panel==1985
						
						
						
						// testing ground: all transition in 1985
						tab occbefore occafter [aw=pweight85] if selectmonth==tm(1986m2) & panel==1985, matcell(temp_a313)
						tab occbefore occafter [aw=pweight85] if selectmonth==tm(1986m2) & panel==1985, m
						
						// testing ground: transitions but only for those who get recoded
						tab occbefore occafter [aw=pweight8687] if selectmonth==tm(1986m2) & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1),  matcell(temp_b313)
						matrix list temp_a313
						matrix list temp_b313
						
						matrix temp_c313=temp_a313-temp_b313
						matrix list temp_c313
						
						matrix unit13 = ( 1 , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
						matrix list unit13
						
						matrix occmin=temp_c313*unit13'
						matrix list occmin
// for each selectmonth, calculate the difference between the occ85 and occ86+ transition matrix.
global ibegin=tm(1986m2)
display "$ibegin"
global iend=tm(1987m4)
display "$iend"

matrix sumoccmatrix=J(13, 13, 0)
matrix sumoccvector=J(13,1,0)
matrix list sumoccvector

// CAREFUL WITH THE MONTH BY MONTH ANALYSIS: sometimes an occupation is missing, and this gives trouble
/*
forvalues i=$ibegin(1)$iend {
							display "----------------------------- `i' -------------------------------------"
							tab occbefore occafter [aw=pweight85] if selectmonth==`i' & panel==1985, matcell(temp_a`i')
							tab occbefore occafter [aw=pweight8687] if selectmonth==`i' & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1),  matcell(temp_b`i')
							
							matrix temp_c`i'=temp_a`i'-temp_b`i'
							matrix occmin_`i'=temp_c`i'*unit13'
							matrix sumoccmatrix=sumoccmatrix+temp_c`i'
							matrix sumoccvector=sumoccvector+ temp_c`i'*unit13'
						
}							// temp_d: distribution of sure stayers, to be compared to temp_c
*/				
/*			
tab occbefore occafter [aw=pweight8687] if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 & ws1_2004!=1),  matcell(temp_d)
tab occ1bfr_ddn occ1aft_ddn [aw=pweight8687] if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0,  matcell(temp_d)
gen markoccdisagree=.
replace markoccdisagree=1 if occ1bfr_ddn!=occ1aft_ddn & occ1bfr_ddn!=. & occ1aft_ddn!=.  & selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1
idfillreplace markoccdisagree
gen markoccdisagree2=1 if occ1bfr_ddn!=occ1aft_ddn & occ1bfr_ddn!=. & occ1aft_ddn!=.  & selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1

sort personkey yearmonth 
browse personkey wave yearmonth markoccdisagree2 c_tjbocc1 c_tjbocc2 eeno1 eeno2 selectmonth select_ind panel estlemp1 estlemp2 ws1_2004 ws2_2104 if markoccdisagree==1
	* seems some issue when the wave is not completely in sample? This effect seems to go away when selecting pp_intvw!=0
*/
tab occbefore occafter [aw=pweight8687] if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 & ws1_2004!=1) & pp_intvw!=0,  matcell(temp_d)
tab occ1bfr_ddn occ1aft_ddn [aw=pweight8687] if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0,  matcell(temp_d)
tab occ1aft_ddn [aw=pweight8687] if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0
* unweighted
tab occ1aft_ddn if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0, matcell(temp_du)

	/*
	INTERESTINGLY ENOUGH, there is some occupational mobility here, about 0.4% of the cases. 
	mark these observations.
	
	*/
	
	
	
							// temp_a independently coded; temp_b: those at risk for moving
							tab occbefore occafter [aw=pweight85] if selectmonth>=$ibegin & selectmonth<=$iend & panel==1985, matcell(temp_a)
							tab occbefore occafter [aw=pweight8687] if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1),  matcell(temp_b)
							tab occ1bfr_ddn occ1aft_ddn [aw=pweight85] if selectmonth>=$ibegin & selectmonth<=$iend & panel==1985, matcell(temp_a)
							tab occ1bfr_ddn occ1aft_ddn [aw=pweight8687] if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1),  matcell(temp_b)
							
							* unweighted
							tab occ1bfr_ddn occ1aft_ddn if selectmonth>=$ibegin & selectmonth<=$iend & panel==1985 & pp_intvw!=0, matcell(temp_au)
							tab occ1bfr_ddn occ1aft_ddn if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1) & pp_intvw!=0,  matcell(temp_bu)
							
							matrix temp_c=temp_a-temp_b
							matrix temp_cu=temp_au-temp_bu
							matrix list temp_c
							matrix list temp_cu // unweighted
							
							* CORRECTION MATRIX MAKES REWEIGHTS BEFORE
							
								// ---> mathematics of it: sample management a bit more in 1986? then need to 
							
							// THREE CORRECTIONS: (1) weighted, unadjusted; (2) unweighted, unadjusted; (3) weighted, adjusted
							
							* step 1 towards adjustment: check if the ratio of occ changes is in proportion...
							
							
							
							matrix occmin=temp_c*unit13'
							
							matrix occmindiag=diag(occmin)
							matrix trans_c=invsym(occmindiag)*temp_c
							matrix list trans_c
							
							
							
							
							// transition matrix a
							matrix occmin_a=temp_a*unit13'
							matrix occmindiag_a=diag(occmin_a)
							matrix trans_a=invsym(occmindiag_a)*temp_a
							matrix list trans_a
							
							
							// transition matrix b
							matrix occmin_b=temp_b*unit13'
							matrix occmindiag_b=diag(occmin_b)
							matrix trans_b=invsym(occmindiag_b)*temp_b
							matrix list trans_b
							matrix list trans_c
							
							
							
							// implied staying rate:
							global trace_c=trace(temp_c)
							display $trace_c
							matrix sum_c=unit13*occmin
							matrix list sum_c
							
									
							// symmetric matrix off-diagonal
							matrix def temp_csym=J(13,13,0)
							forvalues i = 1(1)13 {
							forvalues j = 1(1)13 {
										if `i'==`j' {
										matrix temp_csym[`i',`j']= temp_c[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_csym[`i',`j']= (temp_c[`i',`j']+temp_c[`j',`i'])/2
										}
							}
							}
							// SAVE TRANSITION FLOW MATRIX: to excell
							matrix list temp_csym
							putexcel A1=matrix(temp_csym) using results, replace
							
							matrix list temp_csym
							putexcel A1=matrix(temp_csym) using results1910, replace
							
							
							// transition matrix for latex
							matrix occmin_csym=temp_csym*unit13'
							matrix occmindiag_csym=diag(occmin_csym)
							matrix trans_csym=invsym(occmindiag_csym)*temp_csym
							matrix list trans_csym
							matrix list trans_c
							
							// ---> here is the outtable
							outtable using trans_csym, mat(trans_csym) replace center f(%5.3f)
							
							mat list trans_c
							
							
							// implied staying rate
							global trace_csym=trace(temp_csym)
							display $trace_c
							matrix sum_c=unit13*occmin
							matrix list sum_c
							
*/


///==================================================================================
//PROGRAMS
// =====================================================================================


 
capture program drop xsnet_calcx
program define xsnet_calcx, rclass

*matrix matrik=durmat2
						*display "entering program"
						
						if "`2'"!="" {
						matrix prepost=`2'
						matrix matrikspp=`1'
						matrix matriks=prepost'*matrikspp*prepost
						}
						if "`2'"=="" {
						matrix matriks=`1'
						}
						local matdim=colsof(matriks)
						*display "matdim: ", `matdim'
						
						*display "continuing program"
						
						matrix unitx= J(1,`matdim', 1)
						*matrix list unitx
						
						
						
						matrix temp_outflowvector=matriks*unitx'  // warning outflow includes self-flows
						matrix temp_inflowvector=unitx*matriks    // warning inflow includes self-flows 
						*matrix list temp_outflowvector
						*matrix list temp_inflowvector
						
						matrix temp_netflowvector=temp_inflowvector'-temp_outflowvector
						matrix temp_netflowvector_abs=temp_netflowvector
						forvalues i=1(1)`matdim' {
									if temp_netflowvector_abs[`i',1]<0{
									matrix temp_netflowvector_abs[`i',1]=-temp_netflowvector_abs[`i',1]
									}
						}
						*matrix list temp_netflowvector
						*matrix list temp_netflowvector_abs
						
						
						matrix sum_all=unitx*temp_outflowvector
						*matrix list sum_all
						
						
						
						
						global sum_all=sum_all[1,1]
						*display $sum_all
						
						global trace_temp=trace(matriks)
						*display $trace_temp
						
						global staying_temp=$trace_temp / $sum_all
						display "STAYING PROPORTION", $staying_temp
						
						
						
						
						
						matrix netflow_temp=unitx*temp_netflowvector_abs
						*matrix list netflow_temp
						
						
						global netflow_temp=0.5*netflow_temp[1,1]
						global excessflow_temp=${sum_all} - ${trace_temp} - (0.5*netflow_temp[1,1])
						global excessflowprop_temp=${excessflow_temp}/(${sum_all}-${trace_temp})
						global netflowprop_temp=(0.5*${netflow_temp})/(${sum_all}-${trace_temp})
						global netflow_all_u_temp=(0.5*${netflow_temp})/(${sum_all})
					
						
						/*
						display "TOTAL MOVERS  " $excessflow_temp
						display "STAYER  " $trace_temp
						display "NET FLOW  " $netflow_temp
						display "ALL INDIV  " $sum_all
						display "PROP OF U NEEDED  " $netflow_all_u_temp
						display "PROP NET REALL  ", $excessflowprop_temp
						
						*/
						
						
						// calculate transition matrix
						matrix transmat_temp=J(`matdim', `matdim',0)
						forvalues i=1(1)`matdim' {
						forvalues j=1(1)`matdim' {
										matrix transmat_temp[`i',`j']=matriks[`i', `j']/temp_outflowvector[`i',1]
						}
						}
						
						*matrix temp_inflowvector=I(13)*temp_inflowvector'
						matrix temp_inflowvector=temp_inflowvector'
						
						
						// adjust gross inflow and outflow vectors
						forvalues i=1(1)`matdim' {
									matrix temp_outflowvector[`i', 1]=temp_outflowvector[`i', 1]-matriks[`i', `i']
									matrix temp_inflowvector[`i', 1]=temp_inflowvector[`i', 1]-matriks[`i', `i']
									}
									
						// calculate net reallocation rate
						matrix netreallocc_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netreallocc_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/(temp_outflowvector[`i',1]+temp_inflowvector[`i',1])
						}
						// calculate flow normalized by spells
						matrix netflownorm_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netflownorm_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/($sum_all)
						}
								// calculate flow normalized by spells
						matrix netflowoflow_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netflowoflow_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/($sum_all-$trace_temp)
						}
				
						
				
					
					
						matrix pairwise_netflow=matriks
						
						matrix pairwise_netreallocation=matriks
						
						
						
						forvalues i=1(1)`matdim' {
						forvalues j=`i'(1)`matdim' {
						matrix pairwise_netflow[`i', `j']=(pairwise_netflow[`i', `j']-pairwise_netflow[`j', `i'])*100/${sum_all}
						matrix pairwise_netflow[`j', `i']=0
						if (matriks[`i', `j']+ matriks[`j', `i'])>=0.002* ${sum_all} {
						matrix pairwise_netreallocation[`i', `j']=(matriks[`i', `j']-matriks[`j', `i'])/(matriks[`i', `j']+ matriks[`j', `i'])
						matrix pairwise_netreallocation[`j', `i']=0
						}
						if (matriks[`i', `j']+ matriks[`j', `i'])<0.002* ${sum_all} {
						matrix pairwise_netreallocation[`i', `j']=0
						matrix pairwise_netreallocation[`j', `i']=0
						}
						}
						}
						
						* fill out the net flow matrix below the diagonal
						forvalues i=2(1)`matdim' {
						local m=`i'-1
						forvalues j=1(1)`m' {
						
						if pairwise_netflow[`j', `i']!=0 {
								matrix pairwise_netflow[`i', `j']=-pairwise_netflow[`j', `i']
						}
						}
						}
						
						* fill out the net reallocation matrix below the diagonal
						forvalues i=2(1)`matdim' {
						local m=`i'-1
						forvalues j=1(1)`m' {
						
						if pairwise_netreallocation[`j', `i']!=0 {
								matrix pairwise_netreallocation[`i', `j']=-pairwise_netreallocation[`j', `i']
						}
						}
						}
						
						
													
							// relative flow heterogeneity matrix
							/*
							captures proportion of outflows from occ i that go to occupation j, relative to the proportion of all occ's that go to occupation j (excluding self-flows, i.e. excluding all flows from k to k, and all flows from j)
							*/
							
							matrix flowdev_rel=matriks
							matrix sumoutflows_excl_j=J(1,1,0)
							matrix flowdev_abs=J(`matdim',`matdim',0)
							
							
							forvalues i=1(1)`matdim' {
							forvalues j=1(1)`matdim' {
													matrix flowdev_rel[`i',`j']=0
													
													if `j'!=`i' {
													local rel_ijflow=(matriks[`i', `j']) /(temp_outflowvector[`i',1]) // prop of outflows from i that go to j (excl. self-flows)
													matrix sumoutflows_excl_j[1,1]=unitx*temp_outflowvector-temp_outflowvector[`j',1] // all outflows that are not self-flows, excl outflows from j
													*display "test"
													*matrix list sumoutflows_excl_j
													local rel_j_inflow=temp_inflowvector[`j',1] / (sumoutflows_excl_j[1,1]) // prop of inflows into j (excl self-flows) of overall non-self-flows
													matrix flowdev_rel[`i',`j']=(`rel_ijflow'-`rel_j_inflow')/`rel_j_inflow'
													matrix flowdev_abs[`i',`j']=(`rel_ijflow'-`rel_j_inflow')
													
													}
							}
							}

							
							

						
						
						
					
					
					
						
						
						
					
						********RETURNED VARS AND MATRICES
						return matrix toutflowvector=temp_outflowvector  // outflows destionation per source occupation
						return matrix tinflowvector=temp_inflowvector    // for each destination occupation, vector of source occupations
						return matrix tnetflowvector=temp_netflowvector  // net flows per occupation
						return matrix tnetflowvector_abs=temp_netflowvector_abs // absolute value of net flows per occupation
						return matrix tnetflownorm=netflownorm_temp // net flows relative to unemployment spells
						return matrix tnetreallocc=netreallocc_temp // net reallocation rate per occupation (outflows-inflow)/(outflows+inflows)
						return matrix tnetflowoflow=netflowoflow_temp // net flows relative to total unemplyoed workers changing occupations
						return matrix ttransmat=transmat_temp      // transition matrix
						return matrix tpairwise_netflow=pairwise_netflow // net flow across i,j occupation pairs
						return matrix tpairwise_netreallrate=pairwise_netreallocation // net reallocation rate between i, j occupation pairs
						return matrix tflowdev_rel=flowdev_rel
						return matrix tflowdev_abs=flowdev_abs
												
						return scalar tsum_all=$sum_all
						return scalar tnetflow=$netflow_temp
						return scalar texcessflow=$excessflow_temp
						return scalar texcessflowprop=$excessflowprop_temp
						return scalar tnetflowprop=$netflowprop_temp
						return scalar tnetflow_all_u=$netflow_all_u_temp
						return scalar tmobrate=1.0-$staying_temp 
						return scalar tstaying=$staying_temp 
						
			
						
						
end
							
//=====================================							
// Gamma: matrix of true occupational stayers
//=====================================


		/*
		standard weights
		
		*/

		
		
capture program drop gamma_exe
program define gamma_exe, rclass							
						
sort personkey yearmonth 


global ibegin=tm(1986m2)
display "$ibegin"
global iend=tm(1987m4)
display "$iend"


local var_from="`1'"
local var_to="`2'"
local gamma_out="`3'"
local awweight="`4'"

if "`5'"!="" {
local file_out="`5'"
}
if "`5'"=="" {
local file_out="gamma_out"
}

/*
display "variable: from: `1'"
display "variable: to: `2'"
display "results out: `3'"
display "weighting `4'"


local var_from="occ1bfr_ddn"
local var_to="occ1aft_ddn"
local gamma_out="occgama"
local awweight="[aw=pweight_all]"
*/


*/

// OCCUPATIONAL STAYERS IN 1986/1987
		display "tab `var_from' `var_to' `awweight' "
		tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0,  matcell(temp_d)
		tab `var_from' `var_to' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0, matcell(temp_du)
// ALL WORKERS IN 1986/1987
		tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & ((estlemp1==1 & ws1_2004==2)| (estlemp1==2 | ws1_2004==1)) & select_ind==1 & pp_intvw!=0,  matcell(temp_all)
		tab `var_from' `var_to' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & ((estlemp1==1 & ws1_2004==2)| (estlemp1==2 | ws1_2004==1)) & select_ind==1 & pp_intvw!=0, matcell(temp_allu)
// ALL INDEPENDENTLY CODED WORKERS IN 1985
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend & panel==1985 & pp_intvw!=0, matcell(temp_a)
							tab `var_from' `var_to' if selectmonth>=$ibegin & selectmonth<=$iend & panel==1985 & pp_intvw!=0, matcell(temp_au)
// THOSE AT RISK OF OCCUPATIONAL MOVING IN 1986/1987	
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==2 | ws1_2004==1) & pp_intvw!=0,  matcell(temp_b)
							tab `var_from' `var_to' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1) & pp_intvw!=0,  matcell(temp_bu)
// JOB MOVERS IN 1986/87 panels
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==2 ) & pp_intvw!=0,  matcell(temp_bjm)
// ACTIVITY MOVERS in 1986/87 panels
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 & ws1_2004==1) & pp_intvw!=0,  matcell(temp_bam)
// JOB STAYERS in 1986/87 panel				
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 ) & pp_intvw!=0,  matcell(temp_bjs)
						
								xsnet_calcx temp_bjm
								local mobrate_jobchg86=r(tmobrate)
							* implied (uncorrected) mobility rate in 1986/87, for job stayers (incl. activity changers)
								xsnet_calcx temp_bjs
								local mobrate_jobstay86=r(tmobrate)
							* implied (uncorrected)  mobility rate in 1986/87, for activity stayers
								xsnet_calcx temp_bam
								local mobrate_actchg86=r(tmobrate)

// MANIPULATIONS
* dimensions and unit vector
local matdim=colsof(temp_all)
matrix unitvec=J(1,`matdim', 1)

	
matrix tempsum=unitvec*temp_all*unitvec'
local sum_all=tempsum[1,1]
*display `sum_all'
matrix tempsum=unitvec*temp_allu*unitvec'
local sum_allu=tempsum[1,1]

matrix tempsum=unitvec*temp_d*unitvec'
local sum_d=tempsum[1,1]
matrix tempsum=unitvec*temp_du*unitvec'
local sum_du=tempsum[1,1]

matrix tempsum=unitvec*temp_a*unitvec'
local sum_a=tempsum[1,1]
local sum_au=tempsum[1,1]

matrix tempsum=unitvec*temp_b*unitvec'
local sum_b=tempsum[1,1]
local sum_bu=tempsum[1,1]

// calculate relative distributions

* source_distr86
matrix sdistr86_all=(unitvec*temp_all')'/`sum_all'
matrix sdistr86_all=temp_all*unitvec'
matrix sdistr86_all=sdistr86_all/`sum_all'
matrix sdistr86_allu=(unitvec*temp_allu')'/`sum_allu'
matrix sdistr86_b=(unitvec*temp_b')'/`sum_b'
matrix sdistr86_bu=(unitvec*temp_bu')'/`sum_bu'
matrix sdistr86_d=(unitvec*temp_d')'/`sum_d'
matrix sdistr86_du=(unitvec*temp_du')'/`sum_du'


* source_distr85
matrix sdistr85_a=(unitvec*temp_a')'/`sum_a'
matrix sdistr85_au=(unitvec*temp_au')'/`sum_au'

* dest_distr86
matrix destdistr86_all=(unitvec*temp_all)'/`sum_all'
matrix destdistr86_allu=(unitvec*temp_allu)'/`sum_allu'
matrix destdistr86_b=(unitvec*temp_b)'/`sum_b'
matrix destdistr86_bu=(unitvec*temp_bu)'/`sum_bu'
matrix destdistr86_d=(unitvec*temp_d)'/`sum_d'
matrix destdistr86_du=(unitvec*temp_du)'/`sum_du'

* dest_distr85
matrix destdistr85_a=(unitvec*temp_a)'/`sum_a'
matrix destdistr85_au=(unitvec*temp_au)'/`sum_au'


// normalize matrices so that all workers in a panel add up to 1 mass 
matrix temp_all=temp_all/`sum_all'
matrix temp_allu=temp_allu/`sum_allu'
matrix temp_a=temp_a/`sum_a'
matrix temp_b=temp_b/`sum_all'
matrix temp_d=temp_d/`sum_all'
matrix temp_au=temp_au/`sum_au'
matrix temp_bu=temp_bu/`sum_allu'
matrix temp_du=temp_du/`sum_allu'

// normalize the distribution over source occupations, to match the 85 distribution (use the weighted distributions)
* correction diagonal matrix
matrix list sdistr86_all
matrix temp_bnorms=diag(sdistr86_all)
matrix list temp_bnorms
matrix temp_bnorms=inv(temp_bnorms)*diag(sdistr85_a)
matrix list temp_bnorms

*matrix list temp_bnormd
*matrix list temp_bnorms
/* HERE IS CHECK WETHER NUMBERS ADD UP 
matrix temp_bnorms=temp_bnorms*temp_all
matrix list temp_bnorms
matrix sumtest0=temp_bnorms*unitvec'
matrix list sumtest0  // correct matrix distribution over source occupations
matrix sumtest=unitvec*temp_bnorms*unitvec'
matrix list sumtest   // add up all mass to 1

matrix testbnorms=unitvec*temp_bnorms
matrix sumtest2=testbnorms*unitvec'
matrix sumtest3=(unitvec*temp_all)/`sum_all'

	matrix list testbnorms
	matrix list sumtest2
	matrix list sumtest3
	matrix list sdistr85_a
	matrix list sdistr86_all
*/
matrix temp_bnorms=temp_bnorms*temp_b
matrix list temp_bnorms
matrix temp_cnorms=temp_a-temp_bnorms
matrix list temp_cnorms


matrix temp_bnormd=diag(destdistr86_all)
matrix temp_bnormd=inv(temp_bnormd)*diag(destdistr85_a)

matrix temp_bnormd=temp_b*temp_bnormd
matrix list temp_bnormd
	
// TEMP_NORM takes into account both marginals, and reweights both source and destination 
		// occupations such that both marginals overlap
* write an optimization routine in MATA? 
		
	
	
// CREATE MATRICES OF 1985 NOT AT RISKS: temp_c 							
							
						
								matrix temp_c=temp_a-temp_b
								matrix temp_cu=temp_au-temp_bu
								matrix temp_cnorms=temp_a-temp_bnorms
								matrix temp_cnormd=temp_a-temp_bnormd
								
								matrix list temp_c
								matrix list temp_cu // unweighted
								matrix list temp_cnorms
								matrix list temp_cnormd
				

										capture n {
										xsnet_calcx temp_cnormd
										matrix list r(ttransmat)
										xsnet_calcx temp_cnorms
										matrix list r(ttransmat)
										xsnet_calcx temp_cu
										matrix list r(ttransmat)
										xsnet_calcx temp_c
										matrix list r(ttransmat)
										}
										
										
										* resulting matrices have to be reweighted to add up to 1.
										matrix sum_c=unitvec*temp_c*unitvec'
										local sum_c=sum_c[1,1]
										
										matrix temp_c=temp_c/`sum_c'
										matrix list temp_c
										
										
										matrix sum_cu=unitvec*temp_cu*unitvec'
										local sum_cu=sum_cu[1,1]
										
										matrix temp_cu=temp_cu/`sum_cu'
										matrix list temp_cu
										
										
										matrix sum_cnormd=unitvec*temp_cnormd*unitvec'
										local sum_cnormd=sum_cnormd[1,1]
										
										matrix temp_cnormd=temp_cnormd/`sum_cnormd'
										matrix list temp_cnormd
										
										
										matrix sum_cnorms=unitvec*temp_cnorms*unitvec'
										local sum_cnorms=sum_cnorms[1,1]
										
										matrix temp_cnorms=temp_cnorms/`sum_cnorms'
										matrix list temp_cnorms
										
										matrix test_cnorm=temp_c'-diag(temp_c*(inv(diag(sdistr85_a))*destdistr85_a))
										display det(test_cnorm) // essentially zero
										
										* CORRECTION MATRIX MAKES REWEIGHTS BEFORE
											
												// ---> mathematics of it: sample management a bit more in 1986? then need to 
											
											// THREE CORRECTIONS: (1) weighted, unadjusted; (2) unweighted, unadjusted; (3) weighted, adjusted
										
											* step 1 towards adjustment: check if the ratio of occ changes is in proportion...
											
							
// SYMMETRIC C-MATRIX									
							// symmetric matrix off-diagonal
							local matdim=colsof(temp_c)
							
							matrix def temp_csym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_csym[`i',`j']= temp_c[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_csym[`i',`j']= (temp_c[`i',`j']+temp_c[`j',`i'])/2
										}
							}
							}
							
							
							// symmetric matrix off-diagonal
							matrix def temp_cusym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_cusym[`i',`j']= temp_cu[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_cusym[`i',`j']= (temp_cu[`i',`j']+temp_cu[`j',`i'])/2
										}
							}
							}
							
					
							
							
							matrix def temp_cnormdsym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_cnormdsym[`i',`j']= temp_cnormd[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_cnormdsym[`i',`j']= (temp_cnormd[`i',`j']+temp_cnormd[`j',`i'])/2
										}
							}
							}
							
							matrix def temp_cnormssym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_cnormssym[`i',`j']= temp_cnorms[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_cnormssym[`i',`j']= (temp_cnorms[`i',`j']+temp_cnorms[`j',`i'])/2
										}
							}
							}
							xsnet_calcx temp_c
							local netmobrate_err=r(tnetflow_all_u) 
							matrix trans_csym=r(ttransmat)
							local rmobrate_err=r(tmobrate) 
							display "code error mobility", r(tmobrate)
							
							xsnet_calcx temp_cusym
							matrix trans_cusym=r(ttransmat)
							local rmobrate_erru=r(tmobrate)
							display "unweighted: code error mobility", r(tmobrate)
							
							xsnet_calcx temp_cnormdsym
							matrix trans_cnormdsym=r(ttransmat)
							local rmobrate_errnormd=r(tmobrate)
							display "normalized destination occ: code error mobility", r(tmobrate)
							
							xsnet_calcx temp_cnormssym
							matrix trans_cnormssym=r(ttransmat)
							local rmobrate_errnorms=r(tmobrate)
							display "normalized source occ: code error mobility", r(tmobrate)
							matrix sdistr85_csym=temp_csym*unitvec'
							matrix sum_csym=unitvec*temp_csym*unitvec'
							local sum_csym=sum_csym[1,1]
							matrix sdistr85_c=sdistr85_csym/`sum_csym'
							
// SYMMETRIC C-MATRIX, WITHOUT NONZERO CELLS; rermalize afterwards!

		/*
		nonzero elements realize when there more 'true mobility observations' than spurious+true mobility observations
		*/

							// SAVE TRANSITION FLOW MATRIX: to excell
							matrix list temp_csym
							putexcel A1=matrix(temp_csym) using "${step1resultsdir}/`file_out'_c", replace
							
							if ${fulloutput}==1 {
							matrix list temp_cusym
							putexcel A1=matrix(temp_cusym) using "${step1resultsdir}/`file_out'_cu", replace
							matrix list temp_cnormdsym
							putexcel c2=matrix(temp_cnormdsym) using "${step1resultsdir}/`file_out'_cnormdsym", replace
							matrix list temp_cnormssym
							putexcel c2=matrix(temp_cnormssym) using "${step1resultsdir}/`file_out'_cnormssym", replace
							}
							
							
							*matrix list temp_csym
							*putexcel A1=matrix(temp_csym) using results1910, replace
							
							
							
							// ---> here is the outtable
							if ${fulloutput}==1 {
							outtable using "${step1resultsdir}/trans_csym", mat(trans_csym) replace center f(%5.3f)
							outtable using "${step1resultsdir}/trans_cusym", mat(trans_cusym) replace center f(%5.3f)
							outtable using "${step1resultsdir}/trans_cnormdsym", mat(trans_cnormdsym) replace center f(%5.3f)
							outtable using "${step1resultsdir}/trans_cnormssym", mat(trans_cnormssym) replace center f(%5.3f)
							}
							
							
							
//in: occupation/industry before, occupation/industry out, weighting
							
// RETURNS
* gamma matrix
return matrix rgamma=temp_csym
* gamma matrix unweighted
return matrix rgamma_u=temp_cusym
* gamma matrix normalized on source occupations
return matrix rgamma_ns=temp_cnormssym
* gamma matrix normalized on destination occupations
return matrix rgamma_nd=temp_cnormdsym
* distribution of not-at-risk in 1986/1987
return matrix rdistr_nar86=sdistr86_d
* implied distribution of not-at-risk in 1985 
return matrix rdistr_nar85=sdistr85_csym
* source occupation distribution in 1986/1987
return matrix rdistr_source86=sdistr86_all
* source occupation distribution in 1985
return matrix rdistr_source85=sdistr85_a
* destination occupation distribution in 1986/1987
return matrix rdistr_distr86=destdistr86_all 

* destination occupation distribution in 1985 
return matrix rdistr_distr85=destdistr85_a

* transition matrices
return matrix rtrans_gamma=trans_csym
return matrix rtrans_gamma_unw=trans_cusym
return matrix rtrans_gamma_ns=trans_cnormssym
return matrix rtrans_gamma_nd=trans_cnormdsym


* implied mobility rate in 1986/87, for job changers
return scalar rmobrate_jj86=`mobrate_jobchg86'

* implied (uncorrected) mobility rate in 1986/87, for job stayers (incl. activity changers)
return scalar rmobrate_jobstay86=`mobrate_jobstay86'
* implied (uncorrected)  mobility rate in 1986/87, for activity stayers
return scalar rmobrate_actchg86=`mobrate_actchg86'

* implied 'fake' mobility rate in 1985 panel, for implied job stayers
return scalar rmobrate_err=`rmobrate_err'
* other spurious mobility (different measures)
return scalar rmobrate_erru=`rmobrate_erru'
return scalar rmobrate_errnorms=`rmobrate_errnorms'
return scalar rmobrate_errnormd=`rmobrate_errnormd'
* prop of net flows in the initial (potentially asymmetric c-matrix)
return scalar netmobrate_err=`netmobrate_err'
* amount of negative flows in net matrix 
*return matrix rneg

end



/*

/* GAMMA_EXE INPUTS
1 local var_from="occ1bfr_ddn"
2 local var_to="occ1aft_ddn"
3 local gamma_out="occgama"
4 local awweight="[aw=pweight_all]"
*/
gamma_exe occ1bfr_ddn occ1aft_ddn occgamma_ddn2 [aw=pweight_all] gamma_ddn2
gamma_exe occ1bfr_nohma occ1aft_nohma occgamma_nohma [aw=pweight_all] gamma_nohma
*/

									//***************************
									// 22 groups following 2000SOC


									cap n drop occ1bfr_mm
									cap n drop occ1aft_mm

									gen occ1bfr_mm=.
									gen occ1aft_mm=.

									cap n program drop merge2000soc_exe
									program define merge2000soc_exe

									local ddvar_in="`1'"
									local aggvar_out="`2'"

									capture drop occ1990dd
									gen occ1990dd=`ddvar_in'

									capture drop _merge
									merge m:1 occ1990dd using "${workingdir}/SIPP 0. initial/occ_ind_recodes/occ1990dd2000soc.dta"

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

									merge2000soc_exe occ3bfr_ddn occ1bfr_mm
									merge2000soc_exe occ3aft_ddn occ1aft_mm

gamma_exe occ1bfr_mm occ1aft_mm occgamma_mm [aw=pweight_all]



*************
** GAMMA v2 - saves transition matrices
*************

// PROGRAM TO FILL UP MATRICES 
capture program drop matrix_fillup_exe
program define matrix_fillup_exe
	args lvar_from lvar_to lawweight lmatrix_name
		
		tab `lvar_from' `lvar_to' `lawweight' if ${local_ifcond},  matcell(`lmatrix_name')
		
		
		
		** test dimensions
												local colno=colsof(`lmatrix_name')
												local rowno=rowsof(`lmatrix_name')
												
												display "`colno'"
												display "`rowno'"
												
												* if dimensions do not work out
												
								if (`colno'!=${occdim} | `rowno' != ${occdim}) {
												
												matrix define mattemp=J(${occdim}, ${occdim},0)
												
													
													*** expand matrix 
													
													local ii=1
													forvalues i=1(1)${occdim} {
														
														* test if occupation is present for column
														count if `lvar_to'!=. & ${local_ifcond} & `lvar_to'== occno_matrix[`i',1]
														if r(N)>0 {
															forvalues j=1(1)`rowno' {
															matrix mattemp[`j',`i']=`lmatrix_name'[`j',`ii']
																					}
															local ii=`ii'+1
																  }
													}
													
													matrix `lmatrix_name'=J(${occdim}, ${occdim},0)
													local jj=1
													forvalues j=1(1)${occdim} {
														count if `lvar_from'!=. & ${local_ifcond} & `lvar_from'== occno_matrix[`j',1]
														if r(N)>0 {
															forvalues i=1(1)${occdim} {
															matrix `lmatrix_name'[`j',`i']=mattemp[`jj',`i']
																						}
															local jj=`jj'+1
																  }
													
													}
								}						
													
		
		
		
		
		
end 
		
capture program drop matrix_fillup_unw_exe
program define matrix_fillup_unw_exe
	args lvar_from lvar_to  lmatrix_name
		
		tab `lvar_from' `lvar_to'  if ${local_ifcond},  matcell(`lmatrix_name')
		
		
		
		** test dimensions
												local colno=colsof(`lmatrix_name')
												local rowno=rowsof(`lmatrix_name')
												
												display "`colno'"
												display "`rowno'"
												
												* if dimensions do not work out
												
								if (`colno'!=${occdim} | `rowno' != ${occdim}) {
												
												matrix define mattemp=J(${occdim}, ${occdim},0)
												
													
													*** expand matrix 
													local ii=1
													forvalues i=1(1)${occdim} {
														
														* test if occupation is present for column
														count if `lvar_to'!=. & ${local_ifcond} & `lvar_to'== occno_matrix[`i',1]
														if r(N)>0 {
															forvalues j=1(1)`rowno' {
															matrix mattemp[`j',`i']=`lmatrix_name'[`j',`ii']
																					}
															local ii=`ii'+1
																  }
													}
													
													matrix `lmatrix_name'=J(${occdim}, ${occdim},0)
													local jj=1
													forvalues j=1(1)${occdim} {
														count if `lvar_from'!=. & ${local_ifcond} & `lvar_from'== occno_matrix[`j',1]
														if r(N)>0 {
															forvalues i=1(1)${occdim} {
															matrix `lmatrix_name'[`j',`i']=mattemp[`jj',`i']
																					}
															local jj=`jj'+1
																  }
													
													}
								
								display "matrix with empty rows/columns"
								matrix list `lmatrix_name'
								}						
													
		
		
		
		
		
end 
		


cap n program drop transmat_exe
program define transmat_exe, rclass

local mat_in="`1'"
local rowdim=rowsof(`mat_in')
local coldim=colsof(`mat_in')

matrix unitcoldim=J(`coldim',1,1)
matrix unitrowdim=J(1,`rowdim',1)

matrix source_distr=`mat_in'*unitcoldim
matrix sum_all=unitrowdim*source_distr
local sum_all=sum_all[1,1]
matrix transmat=J(`rowdim', `coldim',0)
forvalues i=1(1)`rowdim' {
forvalues j=1(1)`coldim' {
			matrix transmat[`i',`j']=`mat_in'[`i',`j']/source_distr[`i',1]
		}
		}
matrix source_distr=source_distr/`sum_all'

return matrix rsource_distr=source_distr		
return matrix rtransmat=transmat

end


	
//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
capture program drop gamma_exe_v2
program define gamma_exe_v2, rclass							
						
sort personkey yearmonth 


global ibegin=tm(1986m2)
display "$ibegin"
global iend=tm(1987m4)
display "$iend"


local var_from="`1'"
local var_to="`2'"
local gamma_out="`3'"
local awweight="`4'"

if "`5'"!="" {
local file_out="`5'"
}
if "`5'"=="" {
local file_out="`gamma_out'"
}

/*
display "variable: from: `1'"
display "variable: to: `2'"
display "results out: `3'"
display "weighting `4'"


local var_from="occ1bfr_ddn"
local var_to="occ1aft_ddn"
local gamma_out="occgama"
local awweight="[aw=pweight_all]"
*/

// OCCUPATION IDS


local maxoccno=1
su `var_from'	
if r(max)>`maxoccno' {			
			local maxoccno=r(max)
			}
	
capture tab `var_to'
local no_occs1=r(r)

su `var_to'
if r(max)>`maxoccno' {			
			local maxoccno=r(max)
			}
capture tab `var_to'			
local no_occs2=r(r)

local rcount=`no_occs2' + `no_occs1'

matrix define tempoccmat=J(`rcount', 1, 0)

local occdim=0
forvalues i=1(1)`maxoccno' {
					cap n count if (`var_from'==`i' |`var_to'==`i')
					if r(N)>0 {
					local occdim = `occdim' +1 
					matrix tempoccmat[`occdim',1]=`i'
					}
					}
matrix define occno_matrix=J(`occdim',1,0)
forvalues j=1(1)`occdim' {
					matrix occno_matrix[`j',1]=tempoccmat[`j',1]
}
global occdim=`occdim'

matrix list occno_matrix






// OCCUPATIONAL STAYERS IN 1986/1987
		display "tab `var_from' `var_to' `awweight' "
		
		global local_ifcond "selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0"
		*global testline "`var_from' `var_to' `awweight' if ${local_ifcond}"
		*tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0,  matcell(temp_d)
		matrix_fillup_exe `var_from' `var_to' `awweight' temp_d
		
	
		
		*tab `var_from' `var_to' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==1 & ws1_2004==2) & select_ind==1 & pp_intvw!=0, matcell(temp_du)
		matrix_fillup_unw_exe `var_from' `var_to' temp_du

// ALL WORKERS IN 1986/1987
	 global local_ifcond "selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & ((estlemp1==1 & ws1_2004==2)| (estlemp1==2 | ws1_2004==1)) & select_ind==1 & pp_intvw!=0"
	 matrix_fillup_exe `var_from' `var_to' `awweight' temp_all
	 putexcel set "${outputdir}/`file_out'_c_${vv}", replace
	 *putexcel set "${outputdir}/`file_out'_85allworker_${vv}", replace
	 putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("absflowmat_85all", replace) modify
	 putexcel A1=matrix(temp_all)
							
	 matrix_fillup_unw_exe `var_from' `var_to' temp_allu
	 
	 /*
		tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & ((estlemp1==1 & ws1_2004==2)| (estlemp1==2 | ws1_2004==1)) & select_ind==1 & pp_intvw!=0,  matcell(temp_all)
		tab `var_from' `var_to' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & ((estlemp1==1 & ws1_2004==2)| (estlemp1==2 | ws1_2004==1)) & select_ind==1 & pp_intvw!=0, matcell(temp_allu)
	*/
		
// ALL INDEPENDENTLY CODED WORKERS IN 1985
	/*
	
	tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend & panel==1985 & pp_intvw!=0, matcell(temp_a)
							tab `var_from' `var_to' if selectmonth>=$ibegin & selectmonth<=$iend & panel==1985 & pp_intvw!=0, matcell(temp_au)
	*/
	
	global local_ifcond "selectmonth>=$ibegin & selectmonth<=$iend & panel==1985 & pp_intvw!=0"
	matrix_fillup_exe `var_from' `var_to' `awweight' temp_a
	matrix_fillup_unw_exe `var_from' `var_to' temp_au
	
	
// THOSE AT RISK OF OCCUPATIONAL MOVING IN 1986/1987	

	global local_ifcond "selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==2 | ws1_2004==1) & pp_intvw!=0"
	matrix_fillup_exe `var_from' `var_to' `awweight' temp_b
	matrix_fillup_unw_exe `var_from' `var_to' temp_bu
	
	/*
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==2 | ws1_2004==1) & pp_intvw!=0,  matcell(temp_b)
							tab `var_from' `var_to' 			if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1986) & (estlemp1==2 | ws1_2004==1) & pp_intvw!=0,  matcell(temp_bu)
	*/
	
// JOB MOVERS IN 1986/87 panels

	global local_ifcond "selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==2 ) & pp_intvw!=0"
	matrix_fillup_exe `var_from' `var_to' `awweight' temp_bjm
	
	/*
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1==2 ) & pp_intvw!=0,  matcell(temp_bjm)
	*/
	
	
// ACTIVITY MOVERS in 1986/87 panels
	global local_ifcond "selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 & ws1_2004==1) & pp_intvw!=0"
	matrix_fillup_exe `var_from' `var_to' `awweight' temp_bam
	
	/*
							tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 & ws1_2004==1) & pp_intvw!=0,  matcell(temp_bam)
	*/
	
	
// JOB STAYERS in 1986/87 panel				
	global local_ifcond "selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 ) & pp_intvw!=0"
	matrix_fillup_exe `var_from' `var_to' `awweight' temp_bjs
						
						/*
						tab `var_from' `var_to' `awweight' if selectmonth>=$ibegin & selectmonth<=$iend  & (panel==1986 | panel==1987) & (estlemp1!=2 ) & pp_intvw!=0,  matcell(temp_bjs)
						*/
						
						
								capture noisily xsnet_calcx temp_bjm
								local mobrate_jobchg86=r(tmobrate)
							* implied (uncorrected) mobility rate in 1986/87, for job stayers (incl. activity changers)
								capture noisily xsnet_calcx temp_bjs
								local mobrate_jobstay86=r(tmobrate)
							* implied (uncorrected)  mobility rate in 1986/87, for activity stayers
								capture noisily xsnet_calcx temp_bam
								local mobrate_actchg86=r(tmobrate)

// MANIPULATIONS
* dimensions and unit vector
local matdim=colsof(temp_all)
matrix unitvec=J(1,`matdim', 1)

	
matrix tempsum=unitvec*temp_all*unitvec'
local sum_all=tempsum[1,1]
*display `sum_all'
matrix tempsum=unitvec*temp_allu*unitvec'
local sum_allu=tempsum[1,1]

matrix tempsum=unitvec*temp_d*unitvec'
local sum_d=tempsum[1,1]
matrix tempsum=unitvec*temp_du*unitvec'
local sum_du=tempsum[1,1]

matrix tempsum=unitvec*temp_a*unitvec'
local sum_a=tempsum[1,1]
local sum_au=tempsum[1,1]

matrix tempsum=unitvec*temp_b*unitvec'
local sum_b=tempsum[1,1]
local sum_bu=tempsum[1,1]

// calculate relative distributions

* source_distr86
matrix sdistr86_all=(unitvec*temp_all')'/`sum_all'
matrix sdistr86_all=temp_all*unitvec'
matrix sdistr86_all=sdistr86_all/`sum_all'
matrix sdistr86_allu=(unitvec*temp_allu')'/`sum_allu'
matrix sdistr86_b=(unitvec*temp_b')'/`sum_b'
matrix sdistr86_bu=(unitvec*temp_bu')'/`sum_bu'
matrix sdistr86_d=(unitvec*temp_d')'/`sum_d'
matrix sdistr86_du=(unitvec*temp_du')'/`sum_du'


* source_distr85
matrix sdistr85_a=(unitvec*temp_a')'/`sum_a'
matrix sdistr85_au=(unitvec*temp_au')'/`sum_au'

* dest_distr86
matrix destdistr86_all=(unitvec*temp_all)'/`sum_all'
matrix destdistr86_allu=(unitvec*temp_allu)'/`sum_allu'
matrix destdistr86_b=(unitvec*temp_b)'/`sum_b'
matrix destdistr86_bu=(unitvec*temp_bu)'/`sum_bu'
matrix destdistr86_d=(unitvec*temp_d)'/`sum_d'
matrix destdistr86_du=(unitvec*temp_du)'/`sum_du'

* dest_distr85
matrix destdistr85_a=(unitvec*temp_a)'/`sum_a'
matrix destdistr85_au=(unitvec*temp_au)'/`sum_au'


// normalize matrices so that all workers in a panel add up to 1 mass 
matrix temp_all=temp_all/`sum_all'
matrix temp_allu=temp_allu/`sum_allu'
matrix temp_a=temp_a/`sum_a'
matrix temp_b=temp_b/`sum_all'
matrix temp_d=temp_d/`sum_all'
matrix temp_au=temp_au/`sum_au'
matrix temp_bu=temp_bu/`sum_allu'
matrix temp_du=temp_du/`sum_allu'

// normalize the distribution over source occupations, to match the 85 distribution (use the weighted distributions)
* correction diagonal matrix
matrix list sdistr86_all
matrix temp_bnorms=diag(sdistr86_all)
matrix list temp_bnorms
matrix temp_bnorms=inv(temp_bnorms)*diag(sdistr85_a)
matrix list temp_bnorms

*matrix list temp_bnormd
*matrix list temp_bnorms
/* HERE IS CHECK WETHER NUMBERS ADD UP 
matrix temp_bnorms=temp_bnorms*temp_all
matrix list temp_bnorms
matrix sumtest0=temp_bnorms*unitvec'
matrix list sumtest0  // correct matrix distribution over source occupations
matrix sumtest=unitvec*temp_bnorms*unitvec'
matrix list sumtest   // add up all mass to 1

matrix testbnorms=unitvec*temp_bnorms
matrix sumtest2=testbnorms*unitvec'
matrix sumtest3=(unitvec*temp_all)/`sum_all'

	matrix list testbnorms
	matrix list sumtest2
	matrix list sumtest3
	matrix list sdistr85_a
	matrix list sdistr86_all
*/
matrix temp_bnorms=temp_bnorms*temp_b
matrix list temp_bnorms
matrix temp_cnorms=temp_a-temp_bnorms
matrix list temp_cnorms


matrix temp_bnormd=diag(destdistr86_all)
matrix temp_bnormd=inv(temp_bnormd)*diag(destdistr85_a)

matrix temp_bnormd=temp_b*temp_bnormd
matrix list temp_bnormd
	
// TEMP_NORM takes into account both marginals, and reweights both source and destination 
		// occupations such that both marginals overlap
* write an optimization routine in MATA? 
		
	
	
// CREATE MATRICES OF 1985 NOT AT RISKS: temp_c 							
							
						
								matrix temp_c=temp_a-temp_b
								matrix temp_cu=temp_au-temp_bu
								matrix temp_cnorms=temp_a-temp_bnorms
								matrix temp_cnormd=temp_a-temp_bnormd
								
								matrix list temp_c
								matrix list temp_cu // unweighted
								matrix list temp_cnorms
								matrix list temp_cnormd
				

										capture n {
										xsnet_calcx temp_cnormd
										matrix list r(ttransmat)
										xsnet_calcx temp_cnorms
										matrix list r(ttransmat)
										xsnet_calcx temp_cu
										matrix list r(ttransmat)
										xsnet_calcx temp_c
										matrix list r(ttransmat)
										}
										
										
										* resulting matrices have to be reweighted to add up to 1.
										matrix sum_c=unitvec*temp_c*unitvec'
										local sum_c=sum_c[1,1]
										
										matrix temp_c=temp_c/`sum_c'
										matrix list temp_c
										
										
										matrix sum_cu=unitvec*temp_cu*unitvec'
										local sum_cu=sum_cu[1,1]
										
										matrix temp_cu=temp_cu/`sum_cu'
										matrix list temp_cu
										
										
										matrix sum_cnormd=unitvec*temp_cnormd*unitvec'
										local sum_cnormd=sum_cnormd[1,1]
										
										matrix temp_cnormd=temp_cnormd/`sum_cnormd'
										matrix list temp_cnormd
										
										
										matrix sum_cnorms=unitvec*temp_cnorms*unitvec'
										local sum_cnorms=sum_cnorms[1,1]
										
										matrix temp_cnorms=temp_cnorms/`sum_cnorms'
										matrix list temp_cnorms
										
										matrix test_cnorm=temp_c'-diag(temp_c*(inv(diag(sdistr85_a))*destdistr85_a))
										display det(test_cnorm) // essentially zero
										
										* CORRECTION MATRIX MAKES REWEIGHTS BEFORE
											
												// ---> mathematics of it: sample management a bit more in 1986? then need to 
											
											// THREE CORRECTIONS: (1) weighted, unadjusted; (2) unweighted, unadjusted; (3) weighted, adjusted
										
											* step 1 towards adjustment: check if the ratio of occ changes is in proportion...
											
							
// SYMMETRIC C-MATRIX									
							// symmetric matrix off-diagonal
							local matdim=colsof(temp_c)
							
							matrix def temp_csym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_csym[`i',`j']= temp_c[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_csym[`i',`j']= (temp_c[`i',`j']+temp_c[`j',`i'])/2
										}
							}
							}
							
							
							// symmetric matrix off-diagonal
							matrix def temp_cusym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_cusym[`i',`j']= temp_cu[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_cusym[`i',`j']= (temp_cu[`i',`j']+temp_cu[`j',`i'])/2
										}
							}
							}
							
					
							
							
							matrix def temp_cnormdsym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_cnormdsym[`i',`j']= temp_cnormd[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_cnormdsym[`i',`j']= (temp_cnormd[`i',`j']+temp_cnormd[`j',`i'])/2
										}
							}
							}
							
							matrix def temp_cnormssym=J(`matdim',`matdim',0)
							forvalues i = 1(1)`matdim' {
							forvalues j = 1(1)`matdim' {
										if `i'==`j' {
										matrix temp_cnormssym[`i',`j']= temp_cnorms[`i',`j']
										}
										if `i'!=`j' {
										matrix temp_cnormssym[`i',`j']= (temp_cnorms[`i',`j']+temp_cnorms[`j',`i'])/2
										}
							}
							}
							xsnet_calcx temp_c
							local netmobrate_err=r(tnetflow_all_u) 
							matrix trans_csym=r(ttransmat)
							local rmobrate_err=r(tmobrate) 
							display "code error mobility", r(tmobrate)
							
							xsnet_calcx temp_cusym
							matrix trans_cusym=r(ttransmat)
							local rmobrate_erru=r(tmobrate)
							display "unweighted: code error mobility", r(tmobrate)
							
							xsnet_calcx temp_cnormdsym
							matrix trans_cnormdsym=r(ttransmat)
							local rmobrate_errnormd=r(tmobrate)
							display "normalized destination occ: code error mobility", r(tmobrate)
							
							xsnet_calcx temp_cnormssym
							matrix trans_cnormssym=r(ttransmat)
							local rmobrate_errnorms=r(tmobrate)
							display "normalized source occ: code error mobility", r(tmobrate)
							matrix sdistr85_csym=temp_csym*unitvec'
							matrix sum_csym=unitvec*temp_csym*unitvec'
							local sum_csym=sum_csym[1,1]
							matrix sdistr85_c=sdistr85_csym/`sum_csym'
							
// SYMMETRIC C-MATRIX, WITHOUT NONZERO CELLS; rermalize afterwards!

		/*
		nonzero elements realize when there more 'true mobility observations' than spurious+true mobility observations
		*/

							// SAVE TRANSITION FLOW MATRIX: to excell
**# Bookmark #1
							
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("flowmat_c", replace) modify
							matrix list temp_csym
							matrix list temp_csym
							putexcel A1=matrix(temp_csym)
							transmat_exe temp_csym
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("transmat_c", replace) modify
							matrix transmattemp=r(rtransmat)
							putexcel A1=matrix(transmattemp)	
							
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("flowmat_cu", replace) modify
							matrix list temp_cusym
							putexcel A1=matrix(temp_cusym)
							transmat_exe temp_cusym
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("transmat_cu", replace) modify
							matrix transmattemp=r(rtransmat)
							putexcel A1=matrix(transmattemp)
							
							
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("flowmat_cnormdsym", replace) modify
							matrix list temp_cnormdsym
							putexcel A1=matrix(temp_cnormdsym) 
							transmat_exe temp_cnormdsym
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("transmat_cnormdsym", replace) modify
							matrix transmattemp=r(rtransmat)
							putexcel A1=matrix(transmattemp)
							
							
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("flowmat_cnormdsym", replace) modify
							matrix list temp_cnormssym
							putexcel A1=matrix(temp_cnormssym) 
							transmat_exe temp_cnormssym
							putexcel set "${outputdir}/`file_out'_c_${vv}", sheet("transmat_cnormssym", replace) modify
							matrix transmattemp=r(rtransmat)
							putexcel A1=matrix(transmattemp)
							
							*matrix list temp_csym
							*putexcel A1=matrix(temp_csym) using results1910, replace
							
							
							/*
							// ---> here is the outtable
							outtable using "C:/Users/lviss/Dropbox/CTV_Revisions/data 2016/trans_csym", mat(trans_csym) replace center f(%5.3f)
							outtable using "C:/Users/lviss/Dropbox/CTV_Revisions/data 2016/trans_cusym", mat(trans_cusym) replace center f(%5.3f)
							outtable using "C:/Users/lviss/Dropbox/CTV_Revisions/data 2016/trans_cnormdsym", mat(trans_cnormdsym) replace center f(%5.3f)
							outtable using "C:/Users/lviss/Dropbox/CTV_Revisions/data 2016/trans_cnormssym", mat(trans_cnormssym) replace center f(%5.3f)
							*/
							
							
							
//in: occupation/industry before, occupation/industry out, weighting
							
// RETURNS
* gamma matrix
return matrix rgamma=temp_csym
* gamma matrix unweighted
return matrix rgamma_u=temp_cusym
* gamma matrix normalized on source occupations
return matrix rgamma_ns=temp_cnormssym
* gamma matrix normalized on destination occupations
return matrix rgamma_nd=temp_cnormdsym
* distribution of not-at-risk in 1986/1987
return matrix rdistr_nar86=sdistr86_d
* implied distribution of not-at-risk in 1985 
return matrix rdistr_nar85=sdistr85_csym
* source occupation distribution in 1986/1987
return matrix rdistr_source86=sdistr86_all
* source occupation distribution in 1985
return matrix rdistr_source85=sdistr85_a
* destination occupation distribution in 1986/1987
return matrix rdistr_distr86=destdistr86_all 

* destination occupation distribution in 1985 
return matrix rdistr_distr85=destdistr85_a

* transition matrices
return matrix rtrans_gamma=trans_csym
return matrix rtrans_gamma_unw=trans_cusym
return matrix rtrans_gamma_ns=trans_cnormssym
return matrix rtrans_gamma_nd=trans_cnormdsym


* implied mobility rate in 1986/87, for job changers
return scalar rmobrate_jj86=`mobrate_jobchg86'

* implied (uncorrected) mobility rate in 1986/87, for job stayers (incl. activity changers)
return scalar rmobrate_jobstay86=`mobrate_jobstay86'
* implied (uncorrected)  mobility rate in 1986/87, for activity stayers
return scalar rmobrate_actchg86=`mobrate_actchg86'

* implied 'fake' mobility rate in 1985 panel, for implied job stayers
return scalar rmobrate_err=`rmobrate_err'
* other spurious mobility (different measures)
return scalar rmobrate_erru=`rmobrate_erru'
return scalar rmobrate_errnorms=`rmobrate_errnorms'
return scalar rmobrate_errnormd=`rmobrate_errnormd'
* prop of net flows in the initial (potentially asymmetric c-matrix)
return scalar netmobrate_err=`netmobrate_err'
* amount of negative flows in net matrix 
*return matrix rneg

end

**`'local globals': where to put the output and how to call it
global outputdir "${step1resultsdir}"
global vv "v22"


//RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
gamma_exe_v2 occ1bfr_mm occ1aft_mm occgamma_mm [aw=pweight_all]
//RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
							
							
//=====================================================
//   OCCUPATIONAL MANIPULATIONS
//=====================================================


	/* NOT NECESSARY: MATHEMATICA DOES THE TRICK */
	
	// load back in the parameters 
		//pause

		// calculate /Gamma^{-1}, save it as a table and plot it.

		// -----> FOR THE MAIN DATA MANIPULATION FILE
		// matrix manipulations: find corrected matrix (as (\Gamma^{-1})^T T_obs \Gamma^{-1} for each quarter)

		// assign 

/*		
		
//=============================================
//  GETTING THE GAMMAs FOR MANY DIFFERENT CLASSIFICATIONS
//===============================================		

// standard dd - orginal
tab occ1bfr_ddo
gamma_exe occ1bfr_ddo occ1aft_ddo occgamma_ddo [aw=pweight_all] gamma_ddo

// standard dd - adj

gamma_exe occ1bfr_ddn occ1aft_ddn occgamma_ddn [aw=pweight_all] gamma_ddn
return list
tab occ1bfr_ddn

// standard for paper, 13 occ, based on david dorn ddn
tab occ1bfr_13dd
tab occ1bfr_13dd if tage>=20 & tage<=66 & fp_enu==1
gamma_exe occ1bfr_13dd occ1aft_13dd occgamma_13dd [aw=pweight_all] gamma_13dd
return list

// HS
gamma_exe occ1bfr_hs occ1aft_hs occgamma_hs [aw=pweight_all] gamma_hs
return list
tab occ1bfr_hs

// HS2 
gamma_exe occ1bfr_hs2 occ1aft_hs2 occgamma_hs2 [aw=pweight_all] gamma_hs2
return list
tab occ1bfr_hs2


// sog 
gamma_exe occ1bfr_sog occ1aft_sog occgamma_sog [aw=pweight_all] gamma_sog
return list
tab occ1bfr_sog


// ad 
gamma_exe occ1bfr_ad occ1aft_ad occgamma_ad [aw=pweight_all] gamma_ad
return list
tab occ1bfr_ad

// mm 
gamma_exe occ1bfr_mm occ1aft_mm occgamma_mm [aw=pweight_all] gamma_mm
return list
tab occ1bfr_mm



// no helpers
gamma_exe occ1bfr_nohlp occ1aft_nohlp occgamma_nohlp [aw=pweight_all] gamma_nohlp
return list
tab occ1bfr_nohlp


// cognitive/routine-manual/non-routine-manual
gamma_exe occ1bfr_3cat occ1aft_3cat occgamma_3cat [aw=pweight_all] gamma_3cat
return list
tab occ1bfr_3cat


// no laborers only
gamma_exe occ1bfr_nolab occ1aft_nolab occgamma_nolab [aw=pweight_all] gamma_nolab
return list
tab occ1bfr_nolab


// no laborers and managers
gamma_exe occ1bfr_nohma occ1aft_nohma occgamma_nohma [aw=pweight_all] gamma_nohma
return list
tab occ1bfr_nohma

// 11 occ
gamma_exe occ1bfr_11occa occ1aft_11occa occgamma_11occa [aw=pweight_all] gamma_11occa
return list
tab occ1bfr_11occa

// no laborers and managers  -- version b
gamma_exe occ1bfr_nohmb occ1aft_nohmb occgamma_nohmb [aw=pweight_all] gamma_nohmb
return list
tab occ1bfr_nohmb

// 11 occ --version
gamma_exe occ1bfr_11occb occ1aft_11occb occgamma_11occb [aw=pweight_all] gamma_11occb
return list
tab occ1bfr_11occb

// industry
gamma_exe ind1bfr_b ind1aft_b indgamma_b [aw=pweight_all] gamma_indb
return list
tab ind1bfr_b

gamma_exe ind1bfr_bm ind1aft_bm indgamma_bm [aw=pweight_all] gamma_indbm
return list
tab ind1bfr_bm

*/


	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"