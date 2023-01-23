
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





/*
- extend: two samples: wavecond, mwavecond
	wavecond: harsher on censoring, less observations. 
	(for net mobility more observations are useful)

1 occid_class    *
2 occlabel_class *
3 occmob_class *
4 occdistr_class *
5 netflow_class
6 ave_netflow_class
7 netreall_class *
8 netflow/u *
9 ave_grossflow * = occmob_class!!!
10 scaled_netflow *

CLASS = 
x wavecond, mwavecond
(class: mm, mm without mgt, dd, dd without mgt, hs ths, 3cat, ind)

*/

// PRELIMINARIES

clear

***** PATHS 
cd "${workingdir}"			// !!! set in the global_paths.do, if necessary !!!
global workingdir "`c(pwd)'"			
do "${workingdir}/global_paths.do"

version 13
set more off
set varabbrev off


	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"

use "${outputdata}/reduced_u_n_ctv.dta", clear
 
 
* auxiliary programs: in particular matrices 
do "${step2codedir}/aux_directory/aux_programs.do"
do "${step1codedir}/Ginv_matrices.do"



//=======================
// GLOBALS
//======================


global wavecond= " & wave > 4 "
global sttg = "3"
global epanbwidth=3

global wavecond " & wave>4 & interview_no>14 & sample_timetogo>3"
global mwavecond " & wave>1 & interview_no>4 & sample_timetogo>3"
	* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
global jfwavecond "& wave>1 & interview_no>4 & sample_timetogo>18-n_spellength "	// this 
global sttg "1"
global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "


//============================
//  GOOD VS BAD TIMES 
//===========================

		global ts_quartersconsidered " quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=tq(2000q2) & quarter!=tq(2000q3) & quarter!=tq(2008q1)" 					
count if ${ts_quartersconsidered}  & n_spellength==1										
cap drop good_tercile_times
cap drop bad_tercile_times
cap drop badgoodtimes_ind
cap drop badgoodepisodes_ind


global hpu_series "hpf_lunrate_bls_0"

cap drop alltimes_ind
gen alltimes_ind=1

sort quarter
su ${hpu_series} if quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=quarter[_n-1] & ${ts_quartersconsidered}, detail

global p50=r(p50)
global p25=r(p25)
global p75=r(p75)
global p10=r(p10)
global p90=r(p90)

_pctile ${hpu_series} if quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=quarter[_n-1] & ${ts_quartersconsidered}, nq(10)
return list
global p20=r(r2)
global p80=r(r8)

_pctile ${hpu_series} if quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=quarter[_n-1] & ${ts_quartersconsidered}, nq(3)

return list
global p33=r(r1)
global p67=r(r2)
display "$p33 "
display "$p67 "



sort quarter
capture drop badgoodtimes_ind
gen badgoodtimes_ind=1 if ${hpu_series}>=$p67
replace badgoodtimes_ind=2 if ${hpu_series}<=$p33

sort quarter
capture drop medianbadgoodtimes_ind
gen medianbadgoodtimes_ind=1 if ${hpu_series}>$p50
replace medianbadgoodtimes_ind=2 if ${hpu_series}<=$p50


//------------------------------
//  LABELLING
//------------------------------

/* MAKE SURE LABELLING IS OK */

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

/*								 
cap n lab val locc1bfr_dd label_1dd	
cap n lab val locc1bfr_ths label_4sc
cap n lab val locc1bfr_hs label_4sc
cap n lab val locc1bfr_3cat label_3cat
cap n lab val lind1bfr_b label_ind15	
*/


// *** NEW RT CATEGORIZATION

	
	capture drop lue_c_rtmm
	capture drop lne_c_rtmm
	capture drop locc1bfr_rtmm
	capture drop locc1aft_rtmm
	gen locc1bfr_rtmm=.
	replace locc1bfr_rtmm=1 if locc1bfr_mmo>=11 & locc1bfr_mmo<=29
	replace locc1bfr_rtmm=2 if (locc1bfr_mmo==41 | locc1bfr_mmo==43 )
	replace locc1bfr_rtmm=3 if ((locc1bfr_mmo>=31 & locc1bfr_mmo<=39) | locc1bfr_mmo==53)
	replace locc1bfr_rtmm=4 if locc1bfr_mmo>=47 & locc1bfr_mmo<=51
	
	gen locc1aft_rtmm=.
	replace locc1aft_rtmm=1 if locc1aft_mmo>=11 & locc1aft_mmo<=29
	replace locc1aft_rtmm=2 if (locc1aft_mmo==41 | locc1aft_mmo==43)
	replace locc1aft_rtmm=3 if ((locc1aft_mmo>=31 & locc1aft_mmo<=39)  | locc1aft_mmo==53)
	replace locc1aft_rtmm=4 if locc1aft_mmo>=47 & locc1aft_mmo<=51
	gen lue_c_rtmm=0 if lue_c_mmo==0
	replace lue_c_rtmm=0 if locc1bfr_rtmm==locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lue_c_mmo!=.
	replace lue_c_rtmm=1 if locc1bfr_rtmm!=locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lue_c_mmo!=.
	gen lne_c_rtmm=0 if lne_c_mmo==0
	replace lne_c_rtmm=0 if locc1bfr_rtmm==locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lne_c_mmo!=.
	replace lne_c_rtmm=1 if locc1bfr_rtmm!=locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lne_c_mmo!=.
	
	tab lue_c_rtmm lue_c_mmo, m

								 
lab val locc1bfr_rtmm label_4sc	
lab val locc1aft_rtmm label_4sc


//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPp

cap n program drop excel_col_exe
	program define excel_col_exe, rclass
			args lnumber

	local excelcc=`lnumber'
	tokenize "`c(alpha)'"
	display "`lnumber', and resulting number `excelcc'"
	if `excelcc'<=26 {
	return local xlscol "``excelcc''"
	}
	if `excelcc'>26 {
	local firstletter=floor((`excelcc'-1)/26)
	local secondletter=`excelcc'-26*`firstletter'
	return local xlscol "``firstletter''``secondletter''"
	}

	end 





//==================================================================
//   test corrected mobility per occupation
//==================================================================


	global nuncond  " & complete_uspell==1 "
	global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
	
	matrix mat_noagric22=I(22)
	matrix mat_noagric22[18,18]=0
	
	
	/*
	findit matselrc
	matrix A = matuniform(10,10)
	matselrc A B , r(1/3) c(1/3, 5/7)
	matrix list B
	*/
	
	** mat_noagric21 and Ginv_mm_noagric
	
	matrix mat_noagric21=J(22,21,0)
	forvalues i=1(1)17 {
		matrix mat_noagric21[`i', `i']=1
	}
	forvalues i=19(1)22 {
		matrix mat_noagric21[`i', `i'-1]=1
	}
	matrix list mat_noagric21
	
	
	matrix Ginv_mm_noagric=Ginv_mm*mat_noagric21
	
	** mat_noagric_nomgt20 and Ginv_mm_noagric_nomgt
	
			*matselrc Ginv_mm_noagric Ginv_mm_noagric_nomgt2, c(2/21)
			*matrix list Ginv_mm_noagric_nomgt
	
	matrix mat_noagric_nomgt20=J(21,20,0)
	forvalues i=2(1)21 {
		matrix mat_noagric_nomgt20[`i', `i'-1]=1
	}
	matrix list mat_noagric_nomgt20
	
	
	matrix Ginv_mm_noagric_nomgt =Ginv_mm*mat_noagric21*mat_noagric_nomgt20
	matrix list Ginv_mm_noagric_nomgt
	
	/* structure: in diagonal blocks? try in parallel first*/
	
	*global rowcounter=1
	global columncc=1
					
					display "$nuncond"
					display "$wavecond"
					display "$noagric22"
	
	
	
	************ PART I :  OCCUPATIONAL IDENTITY, LABELLING AND DISTRIBUTION
	
	capture program drop occ_distr_exe
	program define occ_distr_exe
			args occmob_ind source_occ indic
					forvalues i=1(1)3 {
					local ccc=$columncc-1
					local ccc=`ccc'+`i'
					quietly excel_col_exe `ccc'
					local lett`i'=r(xlscol)
					display "`lett`i'' "
					}
				   
				   ** save distribution
				   sort `source_occ'
					display "tabulating"
				   tabulate `source_occ' [aw=pweight2] if `occmob_ind' !=. $localwavecond & sample_timetogo>$sttg $nuncond  $local_excl_occ & entry_ind==1 & n_spellength<=14 & n_spellength>=1, matcell(freq) matrow(names)
				   
				    local rows = rowsof(names)
					local row = 2
					putexcel `lett1'1=("occlabel_`indic'")
					putexcel `lett2'1=("occid_`indic'")
					*putexcel `lett3'1=("occdistr_`indic'")
					
					
					
forvalues i = 1/`rows' {
 
					local val = names[`i',1]
					local val_lab : label (`source_occ') `val'
			 
					local freq_val = freq[`i',1]
			 
					*local percent_val = `freq_val'/`r(N)'*100
					*local percent_val : display %9.2f `percent_val'
			 
					
					putexcel `lett1'`row'=("`val_lab'")   `lett2'`row'=("`val'") /*`lett3'`row'=(`percent_val') */
					local row = `row' + 1
					}
					putexcel `lett1'`row'=("closing_occ") `lett3'`row'=(0) `lett2'`row'=("0")
end 					
					
	//************************ PART 2: TRANSITION MOMENTS				
					
					
capture program drop occtrans_extract_exe
program define occtrans_extract_exe
						args occmob_ind loc_occ_before loc_occ_after Ginv_in indic
					
					forvalues i=2(1)10 {
					local ccc=$columncc-1
					local ccc=`ccc'+`i'
					quietly excel_col_exe `ccc'
					local lett`i'=r(xlscol)
					display "`i'" "---" "`lett`i'' "
					}
	
					tab `loc_occ_before' `loc_occ_after'  [aw=pweight2] if `occmob_ind' !=. $localwavecond & sample_timetogo>$sttg $nuncond   & entry_ind==1 & n_spellength<=14 & n_spellength>=1, matcell(test)
					xsnet_calcx test `Ginv_in'

					
					matrix netflowprop=r(tnetflowprop)
					
					matrix netflownorm=r(tnetflownorm)
					
					local netflow_all_u=r(tnetflow_all_u)
					
					
					
					
			*** calculating mobility rates per occupation
					matrix transmat=r(ttransmat)
					display "transition matrix--------------"
					matrix list transmat
					local rws=rowsof(transmat)
					
					** average occupation mobility
					putexcel `lett4'1=("ave_grossmob_`indic'")
					putexcel `lett5'1=("occmob_`indic'")
					local rwsplus=`rws'+1
					forvalues i=1(1)`rwsplus' {
					local rowpos=`i'+1
					putexcel `lett4'`rowpos'=(r(tmobrate))
					}
					
			** occ mobility per occupations
					matrix occmob_export=J(`rws',1,0)
					forvalues i=1(1)`rws' {
					matrix occmob_export[`i',1]=1.0-transmat[`i',`i']
					}
					matrix list occmob_export
					putexcel `lett5'2=matrix(occmob_export)
					local rwsend=`rws'+2
					putexcel `lett5'`rwsend'=(0)
					
			** net reallocation rate
					putexcel `lett6'1=("netreallrate_`indic'")
					matrix netreallrate=r(tnetreallocc)
					putexcel `lett6'2=matrix(netreallrate)
					putexcel `lett6'`rwsend'=(0)
			** 
					putexcel `lett7'1=("netflow_all_u_`indic'")
					local rwsplus=`rws'+1
					forvalues i=1(1)`rwsplus' {
					local rowpos=`i'+1
					putexcel `lett7'`rowpos'=(`netflow_all_u')
					}
			
			
			*** calculating source occ distribution, for computation later
					matrix corr_mat=`Ginv_in''*test*`Ginv_in'
					
					display "******************flow matrix *****************************"
					matrix list corr_mat
					matrix define socc_distr_acc=J(`rws',1,1)
					matrix define socc_distr=corr_mat*socc_distr_acc 
					matrix list socc_distr

					display "******************source occ distribution*****************************"
					matrix list socc_distr
					
					display "** total flows ** "
					matrix totalflows=socc_distr'*socc_distr_acc
					matrix list totalflows
					local totfl=totalflows[1,1]
					display "******************source occ distribution*****************************"
					matrix freq=socc_distr
					matrix freq=freq/`totfl'
					
		*** calculating net outflow, scaled by socc distribution			
					matrix netflowvector=r(tnetflowvector)
					matrix transmat=r(ttransmat)
					local rws=rowsof(transmat)
					matrix scaled_netflow=J(`rws',1,0)
					
					forvalues i=1(1)`rws' {
							matrix scaled_netflow[`i',1]=netflowvector[`i',1]/socc_distr[`i',1]
					}
					putexcel `lett9'1=("scaled_netflow_`indic'")
					putexcel `lett9'2=matrix(scaled_netflow)
					putexcel `lett9'`rwsend'=(0)
					display "scaled netflow matrix"
					matrix list scaled_netflow
		
		** freq/distr
		putexcel `lett3'1=("occdistr_`indic'")
		putexcel `lett3'2=matrix(freq)
		
		** netflow (normalized)
					putexcel `lett8'1=("netflows_`indic'")
					putexcel `lett8'2=matrix(netflownorm)
					putexcel `lett8'`rwsend'=(0)
		*** netflowprop of occupational mobility
					putexcel `lett10'1=("ave_netflowprop_`indic'")
					forvalues i=1(1)`rwsplus' {
					local rowpos=`i'+1
					putexcel `lett10'`rowpos'=(r(tnetflowprop))
					}
end
 	
	
// PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP	
capture program drop occtransmat_exe
program define occtransmat_exe, rclass
						args occmob_ind loc_occ_before_a loc_occ_before loc_occ_after Ginv_in excl_matrix indic
					
					
					tab `loc_occ_before' `loc_occ_after'  [aw=pweight2] if `occmob_ind' !=. $localwavecond & sample_timetogo>$sttg $nuncond   & entry_ind==1 & n_spellength<=14 & n_spellength>=1, matcell(test)
					matrix Gamma_adj=`Ginv_in'*`excl_matrix'
					xsnet_calcx test Gamma_adj
					
					local tfl=r(tsum_all)
					
					
					
					
** saving all matrices 
					matrix transmat=r(ttransmat)
					matrix flows=Gamma_adj' * test * Gamma_adj
					local rws=rowsof(transmat)
					
					display "******************flow matrix *****************************"
					matrix define socc_distr_acc=J(`rws',1,1)
					matrix define socc_distr=flows*socc_distr_acc 
					matrix list socc_distr
					matrix freq=socc_distr

					
					display "** total flows ** "
					matrix totalflows=socc_distr'*socc_distr_acc
					matrix list totalflows
					local totfl=totalflows[1,1]
					display "total flows numbers should equal "
					display "`totfl'"
					display "`tfl'"
					
					
					
					local ccc=$columncc
					quietly excel_col_exe `ccc'
					local lett1=r(xlscol)
					
					local ccc=$columncc+1
					quietly excel_col_exe `ccc'
					local lett2=r(xlscol)
					
					local ccc=$columncc+2
					quietly excel_col_exe `ccc'
					local lett3=r(xlscol)
					
					*** NOW FOR NAMES THE COLUMNS
					tabulate `loc_occ_before_a' [aw=pweight2] if `occmob_ind' !=. $localwavecond & sample_timetogo>$sttg $nuncond  $local_excl_occ & entry_ind==1 & n_spellength<=14 & n_spellength>=1, matcell(freq2) matrow(names)
			
					*display "matrix list names"
					*matrix list names 
					*display "matrix list freq"
					*matrix list freq
				    local rows = rowsof(names)
					local row = 2
					
					** define lett1, lett2
					
					display "OCC AND OCC DISTR"
					
					** put in the columns with info
					putexcel `lett1'1=("occid_`indic'")
					*putexcel `lett2'1=("occdistr_`indic'")
					*putexcel `lett3'1=("occfreq_`indic'")
					
					forvalues i = 1/`rows' {
 
					local val = names[`i',1]
					*local val_lab : label (`source_occ') `val'
			 
					local freq_val = freq[`i',1]
					
					local percent_val = `freq_val'/`totfl'*100
					local percent_val : display %9.2f `percent_val'
			 
					
					putexcel `lett1'`row'=(" `val' ")  `lett2'`row'=(`percent_val')    `lett3'`row'=(`freq_val')  /*`lett2'`row'=("`val_lab'") */ 
					local row = `row' + 1
					}
					
					
					*** setting up the column with counter, column with occ id
					
					
					
					
					** setting up the names along the first row
					forvalues i=1(1)`rws' {
					local ccc=$columncc+2
					local ccc=`ccc'+`i'
					quietly excel_col_exe `ccc'
					local lett=r(xlscol)
					putexcel `lett'1=("occflowto_`indic'_`i'")
						if `i'==1 {
						putexcel `lett'2=matrix(flows)
						}
					}
					
					forvalues i=1(1)`rws' {
					local ccc=$columncc+2+`rws'
					local ccc=`ccc'+`i'
					quietly excel_col_exe `ccc'
					local lett=r(xlscol)
					putexcel `lett'1=("transmatto_`indic'_`i'")
					if `i'==1 {
						putexcel `lett'2=matrix(transmat)
						}
					
					}
					local move_along=$columncc+3+2*`rws'
					return scalar move_along=`move_along'
					
					end
					
	
	//=====================================================
	// RUN PROGRAMS
	//==================================================== 
	
	
*** MMO 
	global filename "${mainresultsdir}/net_gross_mobility_agg_and_occspec"
	
	*global filename "${tempdata}/net_gross_transmat_oct2019"
	global sheetname1 "mmo"
	
	global sheet1counter=1
	global sheet2counter=1
	global sheet3counter=1
	
	cap n putexcel set "${filename}.xls", replace 
	cap n putexcel set "${filename}.xls", sheet("${sheetname1}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname1}")
	
	** MMO - WAVECOND - INCL MANAGEMENT
					display "$nuncond"
					display "$noagric22"
					display "$wavecond"
					global local_excl_occ "$noagric22"
					global localwavecond "$wavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	global columncc=1
	
	occ_distr_exe lue_c_mmo locc1bfr_mmo mmo 
	occtrans_extract_exe lue_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric mmo
	
	
	global columncc=11
	** MMO - MWAVECOND - INCL MANAGEMENT
	 				display "$nuncond"
					display "$noagric22"
					display "$mwavecond"
					global local_excl_occ "$noagric22"
					global localwavecond "$mwavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	
	
	occ_distr_exe lue_c_mmo locc1bfr_mmo mmo_mw
	occtrans_extract_exe lue_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric mmo_mw
	global columncc=21
	** MM - WAVECOND - EXCL MANAGEMENT
	display "$nuncond"
					display "$noagric22"
					display "$wavecond"
					global local_excl_occ "$noagric22 & locc1bfr_mmo!=11 & locc1aft_mmo!=11 "
					global localwavecond "$wavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	
	
	occ_distr_exe lue_c_mmo locc1bfr_mmo mmo_nomgt 
	*matselrc Ginv_mm_noagric Ginv_mm_noagric_nomgt, c(2/21)
	occtrans_extract_exe lue_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric_nomgt mmo_nomgt
	global columncc=31
	** MM - MWAVECOND - EXCL MANAGEMENT
	display "$nuncond"
					display "$noagric22"
					display "$mwavecond"
					global local_excl_occ "$noagric22 & locc1bfr_mmo!=11 & locc1aft_mmo!=11 "
					global localwavecond "$mwavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	
	
	occ_distr_exe lue_c_mmo locc1bfr_mmo mmo_nomgt_mw 
	*matselrc Ginv_mm_noagric Ginv_mm_noagric_nomgt, c(2/21)
	occtrans_extract_exe lue_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric_nomgt mmo_nomgt_mw
	global columncc=41
	
	/*
	
	***** ROBUSTNESS NUN 
	
	** MMO - WAVECOND - INCL MANAGEMENT NUN
					display "$nuncond"
					global nuncond " & complete_nunspell==1 "
					display "$noagric22"
					display "$wavecond"
					global local_excl_occ "$noagric22"
					global localwavecond "$wavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	global columncc=41
	
	occ_distr_exe lne_c_mmo locc1bfr_mmo mmo_nun 
	occtrans_extract_exe lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric mmo_nun
	
	** MMO - MWAVECOND - INCL MANAGEMENT NUN
	 				global nuncond " & complete_nunspell==1 "
					display "$nuncond"
					display "$noagric22"
					display "$mwavecond"
					global local_excl_occ "$noagric22"
					global localwavecond "$mwavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	global columncc=51
	
	occ_distr_exe lne_c_mmo locc1bfr_mmo mmo_nun_mw
	occtrans_extract_exe lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric mmo_nun_mw
	
	** MM - WAVECOND - EXCL MANAGEMENT  NUN
	display "$nuncond"
					global nuncond " & complete_nunspell==1 "
					display "$noagric22"
					display "$wavecond"
					global local_excl_occ "$noagric22 & locc1bfr_mmo!=11 & locc1aft_mmo!=11 "
					global localwavecond "$wavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	global columncc=61
	
	occ_distr_exe lne_c_mmo locc1bfr_mmo mmo_nomgt_nun 
	*matselrc Ginv_mm_noagric Ginv_mm_noagric_nomgt, c(2/21)
	occtrans_extract_exe lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric_nomgt mmo_nomgt_nun
	
	** MM - MWAVECOND - EXCL MANAGEMENT  NUN
	display "$nuncond"
					global nuncond " & complete_nunspell==1 "
					display "$noagric22"
					display "$mwavecond"
					global local_excl_occ "$noagric22 & locc1bfr_mmo!=11 & locc1aft_mmo!=11 "
					global localwavecond "$mwavecond"
					display "$local_excl_occ"
					display "$localwavecond"
	global columncc=71
	
	occ_distr_exe lne_c_mmo locc1bfr_mmo mmo_nomgt_nun_mw 
	*matselrc Ginv_mm_noagric Ginv_mm_noagric_nomgt, c(2/21)
	occtrans_extract_exe lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm_noagric_nomgt mmo_nomgt_nun_mw
	
	global nuncond " & complete_uspell==1 "
	*/
	
	
	
	
	
	
	/*
	
	
//=========================================
// ADDITIONAL CALCULATIONS FOR PAPER 
//=========================================


global nuncond " & complete_uspell==1 "
global local_excl_occmmo " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
tab locc1bfr_mmo [aw=pweight2] if lue_c_mmo !=. $mwavecond & sample_timetogo>$sttg $nuncond  $local_excl_occmmo & entry_ind==1 & n_spellength<=14 & n_spellength>=1

global local_excl_occmmo " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 & locc1bfr_mmo!=11 & locc1aft_mmo!=11"
tab locc1bfr_mmo [aw=pweight2] if lue_c_mmo !=. $mwavecond & sample_timetogo>$sttg $nuncond  $local_excl_occmmo & entry_ind==1 & n_spellength<=14 & n_spellength>=1

//
*/

clear

/* input

occ: occupation labels 11-51
occmob: occupational mobility associated with these occupations
distr:  distr

*/

clear
set more off

** MMO
clear
import excel "${mainresultsdir}/net_gross_mobility_agg_and_occspec.xls", sheet("mmo") firstrow
gen occ_counter=_n

** merge with file containing labels
merge 1:1 occ_counter using  "${step2codedir}/aux_directory/occspec_fig2_aux.dta", gen(_merge_rgnal)
capture ren abbrev abbrev_mmo
replace abbrev_mmo = "" in 22			

destring(occid_*), replace			

//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		* program to prepare for pictures
capture program drop dataprep_ng_exe
program define dataprep_ng_exe	
				args indic

sort occid_`indic'
capture n replace occid_`indic'=0 if occid_`indic'==. & occlabel_`indic'=="closing_occ"

forvalues i=1(1)55 {
capture drop occid_`indic'`i'
}
separate occmob_`indic', by(occid_`indic') veryshortlabel

** ALTERNATIVE MEAN GROSS MOBILITY MEASURE: HOWEVER: 
mean occmob_`indic' [aw=occdistr_`indic']
matrix rtable=r(table)
capture drop meanmob_`indic'
gen meanmob_`indic'=rtable[1,1]

sort occid_`indic'
capture drop cumdistr_`indic'
gen cumdistr_`indic'=0
replace cumdistr_`indic'=cumdistr_`indic'[_n-1]+occdistr_`indic'[_n-1] if cumdistr_`indic'[_n-1]!=.

*replace cumdistr = 100 in 22


// sort by occmob
gsort -occmob_`indic'
capture drop cumdistrmob_`indic'
gen cumdistrmob_`indic'=0
replace cumdistrmob_`indic'=cumdistrmob_`indic'[_n-1]+occdistr_`indic'[_n-1] if cumdistrmob_`indic'[_n-1]!=.
replace cumdistrmob_`indic'=cumdistrmob_`indic'*100 
replace cumdistrmob_`indic'=100 if cumdistrmob_`indic'>100 & cumdistrmob_`indic'<.
// label positions
capture drop cumdistrmob_lpos_`indic'
gen cumdistrmob_lpos_`indic'=0.5*(cumdistrmob_`indic'+cumdistrmob_`indic'[_n+1])

sort occid_`indic'

*capture drop netflow*
forvalues i=1/55{
capture drop scaled_netflow_`indic'`i'
}
separate scaled_netflow_`indic', by(occid_`indic') veryshortlabel

/*
capture drop neg_ave_reall
gen neg_ave_reall=-ave_reall
capture drop neg_ave_reall_nomgt
gen neg_ave_reall_nomgt=-ave_reall_nomgt
*/

** make sure that netflow_all_u_`indic' is completely filled out 
replace netflow_all_u_`indic'=netflow_all_u_`indic'[_n-1] if netflow_all_u_`indic'==. & netflow_all_u_`indic'[_n-1]!=.


** shift the labels around a bit
capture drop occmob_`indic'_adj 
gen occmob_`indic'_adj=occmob_`indic'
*replace occmob_`indic'_adj=occmob_`indic'+0.03 if occid_`indic'==19
*replace occmob_`indic'_adj=occmob_`indic'+0.05 if occid_`indic'==15



end


#delimit ;
tokenize "mmo mmo_mw"
;

#delimit cr
display "`1'"
forvalues i=1/38 {
cap n dataprep_ng_exe	"``i''"
}

save "${tempdata}/net_gross_all.dta", replace

//========================================
// PLOT 
//========================================


* adjust abbreviations

replace abbrev_mmo="Personal Care" if occ==39


replace abbrev_mmo="Biz&Finance Ops." if occ==13
replace abbrev_mmo="Computer&Math Occs" if occ==15
replace abbrev_mmo="Science Occs" if occ==19
replace abbrev_mmo="Comm/SocService" if occ==21
replace abbrev_mmo="Legal" if occ==23
replace abbrev_mmo="Health Practioners" if occ==29
replace abbrev_mmo="Protective Services" if occ==33
replace abbrev_mmo="Food Prep/Services" if occ==35
replace abbrev_mmo="Building Services" if occ==37
replace abbrev_mmo="Personal Care" if occ==39
replace abbrev_mmo="Production Occs" if occ==51
replace abbrev_mmo="Transport Occ" if occ==53




*********************************************************************************
//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
// MMO - MW
capture drop abbrev_mmo_mw
gen abbrev_mmo_mw=abbrev_mmo

gsort -occmob_mmo_mw
#delimit ;
twoway bar occmob_mmo_mw11 scaled_netflow_mmo_mw11 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green) ||  
bar occmob_mmo_mw13 scaled_netflow_mmo_mw13 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)|| 
bar occmob_mmo_mw15 scaled_netflow_mmo_mw15 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)|| 
bar occmob_mmo_mw17 scaled_netflow_mmo_mw17 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo_mw19 scaled_netflow_mmo_mw19 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo_mw21 scaled_netflow_mmo_mw21 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo_mw23 scaled_netflow_mmo_mw23 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo_mw25 scaled_netflow_mmo_mw25 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo_mw27 scaled_netflow_mmo_mw27 cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo_mw29 scaled_netflow_mmo_mw29  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo_mw31 scaled_netflow_mmo_mw31  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo_mw33 scaled_netflow_mmo_mw33  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo_mw35 scaled_netflow_mmo_mw35  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo_mw37 scaled_netflow_mmo_mw37  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo_mw39 scaled_netflow_mmo_mw39  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo_mw41 scaled_netflow_mmo_mw41  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(yellow yellow) lwidth(vvvthin) fcolor(yellow*0.2 yellow)||  
bar occmob_mmo_mw43 scaled_netflow_mmo_mw43  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(yellow yellow) lwidth(vvvthin) fcolor(yellow*0.2 yellow)||  
bar occmob_mmo_mw47 scaled_netflow_mmo_mw47  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(blue blue) lwidth(vvvthin) fcolor(blue*0.2 blue)||  
bar occmob_mmo_mw49 scaled_netflow_mmo_mw49  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(blue blue) lwidth(vvvthin) fcolor(blue*0.2 blue)||  
bar occmob_mmo_mw51 scaled_netflow_mmo_mw51  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(blue blue) lwidth(vvvthin) fcolor(blue*0.2 blue)||  
bar occmob_mmo_mw53 scaled_netflow_mmo_mw53  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)|| 
bar occmob_mmo_mw0 scaled_netflow_mmo_mw0  cumdistrmob_mmo_mw, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)|| 
scatter ave_grossmob_mmo_mw cumdistrmob_mmo_mw, connect(l l) lcolor(maroon*0.3) lwidth(thick) msymbol(i i) yline(0, lcolor(black)) ysc(r(-0.5 1.05)) ylabel(-0.4(0.2)1.0, labsize(large)) ||
scatter netflow_all_u_mmo_nomgt_mw  cumdistrmob_mmo_mw, connect(l l) lcolor(maroon maroon) lpattern(longdash longdash) lwidth(medthick thick) msymbol(i i) ||
scatter netflow_all_u_mmo_mw cumdistrmob_mmo_mw, connect(l l) lcolor(maroon) lpattern(shortdash shortdash) lwidth(medthin medthin) msymbol(i i) ||
/* scatter  occmob_mmo_mw cumdistrmob_lpos_mmo_mw  if (occ!=55 & occ!=31 & occ!=49 & (occ<15 | occ>23)), msymbol(i) mlab(occlabel_mmo_mw) mlabpos(8) mlabcolor(black) graphregion(color(white))
* xsize(8) mlabangle(90) || */
function y=0, range(cumdistrmob_mmo_mw) clcolor(fg) ||
scatter  occmob_mmo_mw_adj cumdistrmob_lpos_mmo_mw, msymbol(i) mlab(abbrev_mmo_mw) mlabpos(1) mlabsize(medsmall) xsize(8) 
mlabcolor(black) graphregion(color(white)) mlabangle(45) mlabgap(0) 
xtitle("Occupational composition of unemployment spells (in pp)", size(medlarge)) 
ytitle("Occupational Mobility Rate",size(medlarge))
legend(cols(6) size(*1.05) symxsize(7) keygap(0) pos(5) ring(0) 
order(45 - "Gross Mobility" 1 21 31 35 47 - "Net Mobility " 2 22 32 36 46) /*  */
label(45 "Ave Gross Mobility")
label(1 " NRC") 
label(21 "NRM") 
label(31 "RC") 
label(35 "RM") 
label(41 "RM/NRM")
label(47 "Ave Net Mobility")
label(2 " NRC") 
label(22 "NRM") 
label(32 "RC") 
label(36 "RM") 
label(42 "RM/NRM")
label(46 "Ave Net Mobility, excl. Mgt") )
xlabel(0(20)100, labsize(large))
;
#delimit cr
graph export "${mainresultsdir}/fig2.pdf", as(pdf) replace

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
********************************************************************************



********************************************************************************
** OTHER PLOTS
********************************************************************************

// MMO -wAVECOND
gsort -occmob_mmo
#delimit ;
twoway bar occmob_mmo11 scaled_netflow_mmo11 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green) ||  
bar occmob_mmo13 scaled_netflow_mmo13 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)|| 
bar occmob_mmo15 scaled_netflow_mmo15 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)|| 
bar occmob_mmo17 scaled_netflow_mmo17 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo19 scaled_netflow_mmo19 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo21 scaled_netflow_mmo21 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo23 scaled_netflow_mmo23 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo25 scaled_netflow_mmo25 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo27 scaled_netflow_mmo27 cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo29 scaled_netflow_mmo29  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(green green) lwidth(vvvthin) fcolor(green*0.2 green)||  
bar occmob_mmo31 scaled_netflow_mmo31  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo33 scaled_netflow_mmo33  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo35 scaled_netflow_mmo35  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo37 scaled_netflow_mmo37  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo39 scaled_netflow_mmo39  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)||  
bar occmob_mmo41 scaled_netflow_mmo41  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(yellow yellow) lwidth(vvvthin) fcolor(yellow*0.2 yellow)||  
bar occmob_mmo43 scaled_netflow_mmo43  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(yellow yellow) lwidth(vvvthin) fcolor(yellow*0.2 yellow)||  
bar occmob_mmo47 scaled_netflow_mmo47  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(blue blue) lwidth(vvvthin) fcolor(blue*0.2 blue)||  
bar occmob_mmo49 scaled_netflow_mmo49  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(blue blue) lwidth(vvvthin) fcolor(blue*0.2 blue)||  
bar occmob_mmo51 scaled_netflow_mmo51  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(blue blue) lwidth(vvvthin) fcolor(blue*0.2 blue)||  
bar occmob_mmo53 scaled_netflow_mmo53  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)|| 
bar occmob_mmo0 scaled_netflow_mmo0  cumdistrmob_mmo, bartype(spanning spanning) base(0) lcolor(red red) lwidth(vvvthin) fcolor(red*0.2 red)|| 
scatter ave_grossmob_mmo cumdistrmob_mmo, connect(l ) lcolor(maroon*0.3) lwidth(thick) msymbol(i i) yline(0, lcolor(black)) ysc(r(-0.4 1)) ylabel(-0.6(0.2)1.1) ||
scatter netflow_all_u_mmo_nomgt  cumdistrmob_mmo, connect(l ) lcolor(maroon) lpattern(longdash ) lwidth(medthick ) msymbol(i ) xsc(r(0 100)) ||
scatter netflow_all_u_mmo cumdistrmob_mmo, connect(l ) lcolor(maroon) lpattern(shortdash shortdash) lwidth(medthin medthin) msymbol(i i) ||
/* scatter  occmob_mmo cumdistrmob_lpos_mmo  if (occ!=55 & occ!=31 & occ!=49 & (occ<15 | occ>23)), msymbol(i) mlab(occlabel_mmo) mlabpos(8) mlabcolor(black) graphregion(color(white))
* xsize(8) mlabangle(90) || */
function y=0, range(cumdistrmob_mmo) clcolor(fg) ||
scatter  occmob_mmo_adj cumdistrmob_lpos_mmo, msymbol(i) mlab(abbrev_mmo) mlabpos(1) mlabsize(medium) xsize(8) 
mlabcolor(black) graphregion(color(white)) mlabangle(60) mlabgap(0) 
xtitle("Occupational composition of unemployment spells (in pp)") 
ytitle("Occupational Mobility Rate")
legend(cols(6) symxsize(8) keygap(1) size(small) pos(6) 
order(45 - "Gross Mob. p.Occ:" 1 21 31 35 47 - "Net Mob. p.Occ: " 2 22 32 36 46) /*  */
label(45 "Mean Gross Occ. Mobility")
label(1 " NRC") 
label(21 "NRM") 
label(31 "RC") 
label(35 "RM") 
label(47 "Mean Net Occ. Mobility")
label(2 " NRC") 
label(22 "NRM") 
label(32 "RC") 
label(36 "RM") 
label(46 "Mean Net Occ Mob, excl. Mgt") ) ||
scatter netflow_all_u_mmo_nomgt  cumdistrmob_mmo, connect(l ) lcolor(maroon) lpattern(longdash ) lwidth(medthick ) msymbol(i ) xsc(r(0 100)) ||
;
#delimit cr
graph export "${allresultsdir}/fig2_robustness_wavecond.pdf", as(pdf) replace


********************************************************************************
	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"