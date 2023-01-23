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
***  							NET MOBILITY
***	
***
********************************************************************************


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

cd ${tempdata}

*use "H:\laptop beast hdd\data\u_n_temp_aug2017.dta", clear
use "${outputdata}/reduced_u_n_ctv.dta", clear


quietly do "${step2codedir}/aux_directory/aux_programs.do"
quietly do "${step1codedir}/Ginv_matrices.do"






//=======================
// DATA GLOBALS 
//======================


global hpu_series "hpf_lunrate_bls_0"

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


//======================
// ADDITIONAL DEFINITIONS / VARIABLES
//--------------------- 

capture drop lue_c_rtmm
	capture drop lne_c_rtmm
	capture drop locc1bfr_rtmm
	capture drop locc1aft_rtmm
	gen locc1bfr_rtmm=.
	replace locc1bfr_rtmm=1 if locc1bfr_mmo>=11 & locc1bfr_mmo<=29
	replace locc1bfr_rtmm=2 if (locc1bfr_mmo==41 | locc1bfr_mmo==43 )
	replace locc1bfr_rtmm=3 if (locc1bfr_mmo>=31 & locc1bfr_mmo<=39) | locc1bfr_mmo==53
	replace locc1bfr_rtmm=4 if locc1bfr_mmo>=47 & locc1bfr_mmo<=51
	gen locc1aft_rtmm=.
	replace locc1aft_rtmm=1 if locc1aft_mmo>=11 & locc1aft_mmo<=29
	replace locc1aft_rtmm=2 if (locc1aft_mmo==41 | locc1aft_mmo==43 )
	replace locc1aft_rtmm=3 if (locc1aft_mmo>=31 & locc1aft_mmo<=39 ) | locc1aft_mmo==53
	replace locc1aft_rtmm=4 if locc1aft_mmo>=47 & locc1aft_mmo<=51
	gen lue_c_rtmm=0 if lue_c_mmo==0
	replace lue_c_rtmm=0 if locc1bfr_rtmm==locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lue_c_mmo!=.
	replace lue_c_rtmm=1 if locc1bfr_rtmm!=locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lue_c_mmo!=.
	gen lne_c_rtmm=0 if lne_c_mmo==0
	replace lne_c_rtmm=0 if locc1bfr_rtmm==locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lne_c_mmo!=.
	replace lne_c_rtmm=1 if locc1bfr_rtmm!=locc1aft_rtmm & locc1bfr_rtmm<=4 & locc1bfr_rtmm>=1 & lne_c_mmo!=.
	

	global nuncond  " & complete_uspell==1 "
	global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
	global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "
	global noagricind "& lind1bfr_b!=1 & lind1aft_b!=1 "
	
	matrix mat_noagric22=I(22)
	matrix mat_noagric22[18,18]=0
	
	matrix mat_noagric13=I(13)
	matrix mat_noagric13[7,7]=0
	
	matrix mat_noagricind=I(15)
	matrix mat_noagricind[1,1]=0
	
	/*
	findit matselrc
	matrix A = matuniform(10,10)
	matselrc A B , r(1/3) c(1/3, 5/7)
	matrix list B
	*/
	
	
	matrix mat_noagric21=J(22,21,0)
	forvalues i=1(1)17 {
		matrix mat_noagric21[`i', `i']=1
	}
	forvalues i=19(1)22 {
		matrix mat_noagric21[`i', `i'-1]=1
	}
	matrix list mat_noagric21
	

	matrix Ginv_mm_noagric=Ginv_mm*mat_noagric21
		/* structure: in diagonal blocks? try in parallel first*/
	
	*global rowcounter=1
	global columncc=1
					
					display "$nuncond"
					display "$wavecond"
					display "$noagric22"
	
	


//============================
//  GOOD VS BAD TIMES 
//===========================



global ts_quartersconsidered " quarter>=tq(1984q1) & quarter<=tq(2013q4) & quarter!=tq(2000q2) & quarter!=tq(2000q3) & quarter!=tq(2008q1)" 					
count if ${ts_quartersconsidered}  & n_spellength==1										
cap drop good_tercile_times
cap drop bad_tercile_times
cap drop badgoodtimes_ind
cap drop badgoodepisodes_ind


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



		// PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP	
		//  PROGRAM
		// PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


capture n program drop occtransmat_exe
program define occtransmat_exe, rclass
						args occmob_ind loc_occ_before_a loc_occ_before loc_occ_after Ginv_in excl_matrix indic
					
					
					tab `loc_occ_before' `loc_occ_after'  [aw=pweight2] if `occmob_ind' !=. $localwavecond & sample_timetogo>$sttg $nuncond   & entry_ind==1 & n_spellength<=14 & n_spellength>=1, matcell(test)
					matrix Gamma_adj=`Ginv_in'*`excl_matrix'
					xsnet_calcx test Gamma_adj
					
					
					** also save inflow distribution, and net mobility distribution
					matrix inflowvec=r(tinflowvector)
					matrix netreallvec=r(tnetflownorm)
					local lmobrate=r(tmobrate)
					
					
					local tfl=r(tsum_all)
					
					*pause
					
					
** saving all matrices 
					matrix transmat=r(ttransmat)
					matrix flows=Gamma_adj' * test * Gamma_adj
					
					display "******************flow matrix *****************************"
					matrix list flows
					local rws=rowsof(transmat)
					
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
					putexcel `lett2'1=("occdistr_`indic'")
					putexcel `lett3'1=("occfreq_`indic'")
					
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
					
					local ccc=$columncc+3+2*`rws'
					quietly excel_col_exe `ccc'
					local lett=r(xlscol)
					putexcel `lett'1=("inflowvec_`indic'")
					putexcel `lett'2=matrix(inflowvec)
					
					local ccc=`ccc'+1
					quietly excel_col_exe `ccc'
					local lett=r(xlscol)
					putexcel `lett'1=("netreallvec_`indic'")
					putexcel `lett'2=matrix(netreallvec)
					
					local ccc=`ccc'+1
					quietly excel_col_exe `ccc'
					local lett=r(xlscol)
					putexcel `lett'1=("grossmob_`indic'")
					forvalues i=1(1)`rws' {
						local j=`i'+1
						putexcel `lett'`j'=("`lmobrate'")
					}
					
					local move_along=$columncc+6+2*`rws'
					return scalar move_along=`move_along'
					
					end
					


//=================
//  TRANSITION MATRICES ETC
//=================	


			global filename "${outputdata}/net_gross_mob_transmat"
		global sheetname1 "transmat_rtmm"
		
		global sheet1counter=1
		global sheet2counter=1
		global sheet3counter=1
		
		cap n putexcel set "${filename}.xls", replace 
		cap n putexcel set "${filename}.xls", sheet("${sheetname1}")
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname1}")
		
		
	matrix excl_matrix=I(4)

		
		global columncc=1
		// ROUTMM NO MGT
		display "$noagric13"
						display "$wavecond"
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$wavecond"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_all
		global columncc=r(move_along)	
		
		// ROUTMM MWAVE NO MGT
		display "$noagric13"
						display "$mwavecond"
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$mwavecond"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_mw_all
		global columncc=r(move_along)	
		
	 *& badgoodtimes_ind==1	
		// ROUTMM NO MGT
		display "$noagric13"
						display "$wavecond"
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$wavecond & badgoodtimes_ind==1"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_bad
		global columncc=r(move_along)	
		
		// ROUTMM MWAVE NO MGT
		display "$noagric13"
						display "$mwavecond"
				
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$mwavecond & badgoodtimes_ind==1"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_mw_bad
		global columncc=r(move_along)	
		
		
		// ROUTMM NO MGT
		display "$noagric13"
						display "$wavecond"
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$wavecond & badgoodtimes_ind==2"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_good
		global columncc=r(move_along)	
		
		// ROUTMM MWAVE NO MGT
		display "$noagric13"
						display "$mwavecond"
						
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$mwavecond & badgoodtimes_ind==2"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_mw_good
		global columncc=r(move_along)	
		
		
		// RELATIVE TO THE MEDIAN 
		
		// ROUTMM NO MGT
		display "$noagric13"
						display "$wavecond"
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$wavecond & medianbadgoodtimes_ind==1"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_medbad
		global columncc=r(move_along)	
		
		// ROUTMM MWAVE NO MGT
		display "$noagric13"
						display "$mwavecond"
						
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$mwavecond & medianbadgoodtimes_ind==1"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_mw_medbad
		global columncc=r(move_along)	
		
		
		// ROUTMM NO MGT
		display "$noagric13"
						display "$wavecond"
						global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$wavecond & medianbadgoodtimes_ind==2"
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_medgood
		global columncc=r(move_along)	
		
		// ROUTMM MWAVE NO MGT
		display "$noagric13"
						display "$mwavecond"
						
					    global local_excl_occ " & locc1bfr_mmo!=11 & locc1aft_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45"
						global localwavecond "$mwavecond & medianbadgoodtimes_ind==2 "
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 

		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		occtransmat_exe  lue_c_mmo locc1bfr_rtmm locc1bfr_mmo locc1aft_mmo Ginv_rtmm excl_matrix rtmm_nomgt_mw_medgood
		global columncc=r(move_along)	

		
//========================
//  NET MOBILITY CALIBRATION TARGETS
//========================
		
import excel "${outputdata}/net_gross_mob_transmat.xls", sheet("transmat_rtmm") firstrow clear
		

** PANEL 2 - PANEL B TRANSMAT & NET MOBILITY

keep occid_rtmm_nomgt_mw_all transmatto_rtmm_nomgt_mw_all_1 transmatto_rtmm_nomgt_mw_all_2 transmatto_rtmm_nomgt_mw_all_3 transmatto_rtmm_nomgt_mw_all_4 netreallvec_rtmm_nomgt_mw_all grossmob_rtmm_nomgt_mw_all

ren occid_rtmm_nomgt_mw_all occ_from

cap n destring occ_from, replace

capture drop occ_label
gen occ_label="NRC" if occ_from==1
replace occ_label="RC" if occ_from==2
replace occ_label="NRM" if occ_from==3
replace occ_label="RM" if occ_from==4
order occ_label, after(occ_from)
ren transmatto_rtmm_nomgt_mw_all* occ_to_transmat*

gen netreallrate=-netreallvec_rtmm_nomgt_mw_all
drop netreallvec_rtmm_nomgt_mw_all

ren grossmob_rtmm_nomgt_mw_all grossmob_overall
cap n destring grossmob_overall, replace
export excel using "${mainresultsdir}/table2_panelB_transmat_netmob", firstrow(variables) replace
save "${mainresultsdir}/table2_panelB_transmat_netmob.dta", replace


***** in mod_auxdata we can directly input the transition matrices, so here we s
**				to save them in the mainresultsdir as well






** TABLE 2 - PANEL C CYCLICAL SHIFT NET MOBILITY AND INFLOW DISTRIBUTION 

import excel "${outputdata}/net_gross_mob_transmat.xls", sheet("transmat_rtmm") firstrow clear
		
keep 	occid_rtmm_nomgt_mw_bad inflowvec_rtmm_nomgt_mw_bad netreallvec_rtmm_nomgt_mw_bad inflowvec_rtmm_nomgt_mw_medgood netreallvec_rtmm_nomgt_mw_medgoo	

ren occid_rtmm_nomgt_mw_bad occ_from 
cap n destring occ_from, replace
gen occ_label="NRC" if occ_from==1
replace occ_label="RC" if occ_from==2
replace occ_label="NRM" if occ_from==3
replace occ_label="RM" if occ_from==4
order occ_label, after(occ_from)

gen netmob_bad=- netreallvec_rtmm_nomgt_mw_bad 
gen netmob_good= -netreallvec_rtmm_nomgt_mw_medgoo	
gen inflow_bad=inflowvec_rtmm_nomgt_mw_bad
gen inflow_good=inflowvec_rtmm_nomgt_mw_medgood

drop inflowvec_rtmm_nomgt_mw_bad netreallvec_rtmm_nomgt_mw_bad inflowvec_rtmm_nomgt_mw_medgood netreallvec_rtmm_nomgt_mw_medgoo	

egen sum_inflows_good=sum(inflow_good)
egen sum_inflows_bad=sum(inflow_bad)

gen inflow_distr_good=inflow_good/sum_inflows_good
gen inflow_distr_bad=inflow_bad/sum_inflows_bad

drop inflow_good inflow_bad sum*

gen rec_exp_netflowshift=netmob_bad-netmob_good
gen inflowshift_exp_rec=inflow_distr_good-inflow_distr_bad

export excel using "${mainresultsdir}/table2_panelC_netflow_inflowdistr_cycleshift.xls", firstrow(variables) replace
save "${mainresultsdir}/table2_panelC_netflow_inflowdistr_cycleshift", replace


//=======================
// TURN IT INTO A PICTURE
//=======================

use "${mainresultsdir}/table2_panelC_netflow_inflowdistr_cycleshift.dta", clear

cap n label drop rtmmlabel
lab def rtmmlabel 1 "NRC"
lab def rtmmlabel 2 "RC", add
lab def rtmmlabel 3 "NRM", add
lab def rtmmlabel 4 "RM", add
cap n gen occ=occ_from

lab val occ rtmmlabel
lab var occ "Occupation"


#delimit ;
graph bar (mean) netmob_bad netmob_good, over(occ)
graphregion(color(white)) bargap(-30) asyvars bar(1, color(black)) bar(2, color(midblue%50))
legend(order(2 1) label(1 "Times of High U") label(2 "Times of Low U") ring(0) pos(11) cols(1) size(*1.0) symxsize(*0.5)) yline(0, lcolor(black)) scale(*1.5)
;
#delimit cr
graph export "${mainresultsdir}/fig3b.pdf", as(pdf) replace


********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"