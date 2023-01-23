


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


	
	
//==============================================================
//  UNEMPLOYMENT TIME SERIES STATS 
//==============================================================




//===================================================
// PREAMBLE 
//==================================================

clear all
set maxvar 20000
version 13

cd "$workingdir"
global workingdir `c(pwd)'
do "${workingdir}/global_paths.do"
cd "${tempdata}"

set more off
set varabbrev off
*set trace off 



global lstarttime=c(current_time)
global lstartdate=c(current_date)
display "started at ${lstarttime} on ${lstartdate}"


version 13

use personkey yearmonth quarter wave interview_no2 sample_timetogo unrate* unempl_ctv outlf_ctv empl_ctv ///
				nofullseam_ind earlier_empl durdistr_stability complete* age_2dum locc1bfr* locc1aft* lind* pweight2 continuous_spell leocc1* n_spellength u_spellength incompl* compl* sex ///
				occ* using "${outputdata}/corewave_occlfmin_ctv.dta", clear

cap drop unrate_oldocclfmin
cap n gen unrate_oldocclfmin=unrate
cap n gen unrate=.
cap n replace unrate=unrate_bls_0 if unrate_bls_0!=. 


global wavecond " & wave>4 & interview_no2>14 & sample_timetogo>=0"
global nunwavecond " & wave>5 & interview_no2>20 "
global nunwavecond2 " & wave>4 & interview_no2>24 "


version 13 

// PROGRAM TO GENERATE TIME SERIES OF UNEMPLOYMENT
				*	1st argument: name of variable produced
				*   2nd argument: infix into filename produced 

				
capture program drop unemp_ts_calc_exe
program define unemp_ts_calc_exe
						args u_ind  filename
			local uvarname "ur_`filename'"
			
			version 13
			cap n restore, not
			preserve
			table quarter [aw=pweight2] if  ${ts_u_addcondition} $subset_cond , c(mean `u_ind' mean unrate) replace
			capture drop `uvarname'
			capture drop urate_bls
			ren table1 `uvarname'
			ren table2 urate_bls
			save "${tempdata}/U_tscomp_`filename'.dta", replace
			drop urate_bls 
			save "${tempdata}/U_ts_`filename'.dta", replace
			su `uvarname'
			restore 
end 


capture program drop subset_unemp_ts_calc_exe
program define subset_unemp_ts_calc_exe
									syntax namelist(max=1 id="filename prefix") [if/]
			display "`namelist'"

			global subset_cond ""
			if "`if'"!="" {
			global subset_cond " & `if'"
			display "$subset_cond"
			}
									
			*** ALL, NO RESTRICTION
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1) "
			unemp_ts_calc_exe unempl_ctv `namelist'_norestr

			** ALL, SEAMS OK
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1) & nofullseam_ind!=1"
			unemp_ts_calc_exe unempl_ctv `namelist'_fullseam

			** ALL , DURDISTR STABILITY
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1) & durdistr_stability==1"
			unemp_ts_calc_exe unempl_ctv `namelist'_durstab

			** ALL , DURDISTR STABILITY &  WAVECOND
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1) & durdistr_stability==1 ${wavecond}"
			unemp_ts_calc_exe unempl_ctv `namelist'_durwvcond


			** ALL , DURDISTR STABILITY &  WAVECOND & EARLIER EMPLOYMETN BEFORE BECOMING NONEMPLOYED
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1) & durdistr_stability==1 ${wavecond} & earlier_empl==1"
			unemp_ts_calc_exe unempl_ctv `namelist'_durwvearl

			** ALL , DURDISTR STABILITY &  WAVECOND & EARLIER EMPLOYMETN BEFORE BECOMING NONEMPLOYED
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1) & durdistr_stability==1 ${wavecond} & earlier_empl==1"
			unemp_ts_calc_exe unempl_ctv `namelist'_durwvcearly

			
			
			//=========================================
			//  LOOK AT NUNSPELLS, WITh NUN as employment status  (i.e. nunstock), so NUNrate= NUN/(NUN+EMPL)
			//==========================================

*/
			capture drop icompl_nunempl_ctv
			gen icompl_nunempl_ctv=0
			replace icompl_nunempl_ctv=1 if (unempl_ctv==1|outlf_ctv==1) & (incomplete_nunspell==1 | complete_nunspell==1) 


			** ALL , DURDISTR STABILITY &  WAVECOND, icomplETE_NUN_SPELL 
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1 | (outlf_ctv==1 & (incomplete_nunspell==1 | complete_nunspell==1))) & durdistr_stability==1 ${wavecond} "
			unemp_ts_calc_exe icompl_nunempl_ctv `namelist'_icnun_durwvnosttg

/*
			** ALL , DURDISTR STABILITY &  WAVECOND, icomplETE_NUN_SPELL & RIGHT-CENSORING ADDRESSED
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1 | (outlf_ctv==1 & (incomplete_nunspell==1 | complete_nunspell==1))) & durdistr_stability==1 ${wavecond}  & sample_timetogo>14"
			
			unemp_ts_calc_exe icompl_nunempl_ctv `namelist'_icnun_dwvsttg
*/

			** ALL , DURDISTR STABILITY &  WAVECOND, icomplETE_NUN_SPELL & RIGHT-CENSORING ADDRESSED
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1 | (outlf_ctv==1 & (incomplete_nunspell==1 | complete_nunspell==1))) & durdistr_stability==1 ${wavecond}  & sample_timetogo>24"
			unemp_ts_calc_exe icompl_nunempl_ctv `namelist'_icnun_dwvsttg24

			** ALL , DURDISTR STABILITY &  WAVECOND, icomplETE_NUN_SPELL & RIGHT-CENSORING ADDRESSED
			global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1 | (outlf_ctv==1 & (incomplete_nunspell==1 | complete_nunspell==1))) & durdistr_stability==1 ${wavecond}  & sample_timetogo>30"
			unemp_ts_calc_exe icompl_nunempl_ctv `namelist'_icnun_dwvsttg30


			
			
end 

subset_unemp_ts_calc_exe all
subset_unemp_ts_calc_exe young if age_2dum==1
subset_unemp_ts_calc_exe prime if age_2dum==2





//==================================================================
//  ANALYZE  UNEMPLOYMENT RATES PER QUARTER
//==================================================================


use "${tempdata}/U_tscomp_all_norestr.dta", clear
capture drop log_ur_all_norestr
capture drop log_urate_bls
gen log_ur_all_norestr=log(ur_all_norestr)
gen log_urate_bls=log(urate_bls)
tsset quarter
tsfill

cap n putexcel set "${outputdata}/ts_stocks_results.xls", sheet("Ustocks")
cap n putexcel set "${outputdata}/ts_stocks_results.xls", sheet("Ustocks") modify 
putexcel A1=("series")
putexcel B1=("mean")
putexcel C1=("no. obs")
putexcel D1=("dln u/dln ubls")
putexcel E1=("se")
putexcel F1=("dln u/dln ubls -tt")
putexcel G1=("se")
putexcel H1=("corr ln u - ln ubls")
putexcel I1=("d u/d ubls")
putexcel J1=("se")
putexcel K1=("d u/d ubls -tt")
putexcel L1=("se")

// BASELINE BLS U


putexcel A2=("u_bls")
su urate_bls
putexcel B2=(r(mean))
putexcel C2=(r(N))
	



global pos=3


	*tokenize "all_norestr all_fullseam all_durstab all_durwvcond all_durwvcearly all_compl_1 all_compl_2 all_complnun_1 all_complnun_2 all_complustar_1 all_complustar_2"

	capture program drop read_in_exe
	program define read_in_exe
				syntax namelist
	
				tokenize "`namelist'" 
				forvalues i=1(1)50 {
				if "``i''"!="" {
		
				di "************************"
				di "*     ``i''          *"
				di "************************"
				putexcel A$pos=("``i''")
				
				capture drop _mx_``i''
				capture drop ur_``i''
				*cap n merge 1:1 quarter using "C:\data\ts_urate_jf_sep\indiv_series\U_ts_``i''_${datevar}.dta", gen(_mx_``i'')
				cap n merge 1:1 quarter using "${tempdata}/U_ts_``i''.dta", gen(_mx_``i'')
				lab var ur_``i'' "``i''"
				su ur_``i''
				putexcel B$pos=(r(mean))
				putexcel C$pos=(r(N))
				
				** LOGGED VARIABLES 
				capture drop log_ur_``i''
				gen log_ur_``i''=log(ur_``i'')
				
				reg log_ur_``i'' log_urate_bls
				matrix rtable=r(table)
				putexcel D$pos=(rtable[1,1])
				putexcel E$pos=(rtable[2,1])
				
				reg log_ur_``i'' log_urate_bls quarter
				matrix rtable=r(table)
				putexcel F$pos=(rtable[1,1])
				putexcel G$pos=(rtable[2,1])
				
				corr log_ur_``i'' log_urate_bls
				putexcel H$pos=(r(rho))
				
				reg ur_``i''  urate_bls
				matrix rtable=r(table)
				putexcel I$pos=(rtable[1,1])
				putexcel J$pos=(rtable[2,1])
				reg ur_``i''  urate_bls quarter
				matrix rtable=r(table)
				putexcel K$pos=(rtable[1,1])
				putexcel L$pos=(rtable[2,1])
				global pos=${pos}+1
				}
				}
	end
		** all
		
	read_in_exe  all_norestr all_fullseam all_durstab all_durwvcond all_durwvcearly ///	
			all_icnun_durwvnosttg all_icnun_dwvsttg24 all_icnun_dwvsttg30 ///
			young_norestr young_fullseam young_durstab young_durwvcond young_durwvcearly ///	
			young_icnun_durwvnosttg young_icnun_dwvsttg24 young_icnun_dwvsttg30 ///
			prime_norestr prime_fullseam prime_durstab prime_durwvcond prime_durwvcearly ///	
			prime_icnun_durwvnosttg prime_icnun_dwvsttg24 prime_icnun_dwvsttg30 
			
			


capture drop durdistr_stability
gen byte durdistr_stability=.
replace durdistr_stability=1 if quarter>=100 & quarter<=118
replace durdistr_stability=1 if quarter>=125 & quarter<=142
replace durdistr_stability=1 if quarter>=150 & quarter<=159
replace durdistr_stability=1 if quarter>=169 & quarter<=175
replace durdistr_stability=1 if quarter>=180 & quarter<=190
replace durdistr_stability=1 if quarter>=198 & quarter<=214



cap log close mean_unemplog
quietly {
log using "${mainresultsdir}/table2_mean_unemp.txt", replace text name(mean_unemplog)

noisily: display "-------------------------------------------------------------"
noisily: display " TABLE 2 CALIBRATION TARGET: MEAN U URATE (with earlier empl)"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""

noisily: su ur_all_durwvcearly
noisily: display  ""

log close mean_unemplog
}


cap log close mean_unemplog
quietly {
log using "${mainresultsdir}/calib_mean_nunemp_xsmob_nun.txt", replace text name(mean_nunemplog)

noisily: display "-------------------------------------------------------------"
noisily: display " NUN CALIBRATION TARGET: MEAN NUN URATE (with earlier empl)"
noisily: display "-------------------------------------------------------------"
noisily: display  ""
noisily: display  ""

noisily: su ur_all_icnun_dwvsttg24
noisily: display  ""

log close mean_nunemplog
}

		
save "${tempdata}/timeseries_u.dta", replace
*use "${tempdata}/timeseries_u.dta", clear

* SELECT ONLY SERIES FOR TRAMO (can be extended, of course)
cap n gen lur_all_durwvcearly=log_ur_all_durwvcearly
cap n gen ur_all_durwvcearly=exp(log_ur_all_durwvcearly)
keep quarter ur_all_durwvcearly lur_all_durwvcearly

export excel using "${outputdata}/u_ts_paper_for_tramo.xlsx", sheet("all") firstrow(variables) nolabel replace
			

********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"			
			
			
			