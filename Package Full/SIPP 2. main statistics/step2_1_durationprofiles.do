
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
*** OCCUPATIONAL MOBILITY- UNEMPLOYMENT DURATION PROFILES 
***				IN THE POOLED SIPP
***
********************************************************************************


* FIRST PART: CALCULATES THESE PROFILES FROM THE DATA, saves these as xlsx files
* SECOND PART: DRAW PICTURES (FIG 1A, FIG 1B), saves calibration targets




		*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		*!!!!!  MAKE SURE THE WORKINGDIR GLOBAL (LN 80) IS SET TO !!!!!!!!
		*!!!!!  THE ROOT DIRECTORY OF THE REPLICATION PACKAGE!!!!!!!!!!!!!
		*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		
			


//==========================================
// PRELIMINARIES
//===========================================



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


*** auxiliary programs 
do "${step2codedir}/aux_directory/aux_programs.do"
do "${step1codedir}/Ginv_matrices.do"

//====================================
// FIRST PART -- DURATION PROFILES: (CONDITIONAL) AVERAGES/PLOTS
//==========================================


use "${outputdata}/reduced_u_n_ctv.dta", clear


//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
** aux progrma for column coordinate in excel
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


//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

/* THIS PROGRAM CALCULATES duration profiles (adjusted, unadjusted) for the main text, and for the appendix 
	NOTE: when occupations are
 to be excluded from the calculation, we have to feed that into this program using the adjusted identity matrix, with the 
	diagonal elements corresponding to the 
*/
//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


		cap program drop propocc_exe
		program define propocc_exe
				syntax anything(name=inputlist) if/
				*syntax varlist(min=3 max=3) occmov_ind source_occ dest_occ [if/] , name_infix() corr_matrix rowpos colpos

				
				/* FIRST ELEMENT: name for variables
					2nd: occ move indicator variable
					3rd: occ before
					4th: occ after
					5: correction matrix
					6: row position of summary variables
					7: column position of duration profile
					(notice column position of occ-spec duration profile (separate sheet) is passed as a global
					8: excl matrix to select a subset of occupations
					*/
				
				/* rowpos= row in the summary spreadsheet
					colpos= colulmn in the profile spreadsheet 
					excl_matrix*/
				
				tokenize `inputlist'
				display "`inputlist'"
				
				local name_infix="`1'"
				local occmov_ind="`2'"
				local source_occ="`3'"
				local dest_occ="`4'"
				local corr_matrix="`5'"
				local rowpos="`6'"
				local colpos="`7'"
				local excl_matrix="`8'"
				
				*display "`name_infix'"
				*display "`colpos'"
				display "`excl_matrix'"
		
			** make sure globals are already defined, otherwise define them
			
			if "$filename"==""{
									global filename "tempname"
									display " NEEDED TO DEFINE FILENAME GLOBAL "
									
									}
		
			if "$profilesheetname"==""{
									global profilesheetname "profile"
									display " NEEDED TO DEFINE FILENAME GLOBAL "
									
									}
		
			
			if "$colposcounter"==""{
									global colposcounter=2
									display " NEEDED TO DEFINE COLPOSCOUNTER GLOBAL "
									
									}
		
			if "$colposcounter_occspec"==""{
									global colposcounter_occspec=3
									display " NEEDED TO DEFINE COLPOSCOUNTER_OCCSPEC GLOBAL "
									
									}
		
		
		
		set varabbrev off
		set more off
				
		sort personkey yearmonth



		capture drop propocc_`name_infix'
		capture drop propocc_`name_infix'_lb
		capture drop propocc_`name_infix'_ub
		gen propocc_`name_infix'=.
		gen propocc_`name_infix'_lb=.
		gen propocc_`name_infix'_ub=.


		capture drop mpropocc_`name_infix'
		capture drop mpropocc_`name_infix'_lb
		capture drop mpropocc_`name_infix'_ub
		gen mpropocc_`name_infix'=.
		gen mpropocc_`name_infix'_lb=.
		gen mpropocc_`name_infix'_ub=.

		capture drop jfind_dur_`name_infix'
		capture drop jfind_dur_`name_infix'_lb
		capture drop jfind_dur_`name_infix'_ub
		gen jfind_dur_`name_infix'=.
		gen jfind_dur_`name_infix'_lb=.
		gen jfind_dur_`name_infix'_ub=.


		capture drop mpropocc_corr_`name_infix'
		gen mpropocc_corr_`name_infix'=.


		capture drop propocc_corr_`name_infix'
		gen propocc_corr_`name_infix'=.


			***** MATRIX SETUP: define sizes and mapping rows to identities, and the occupational exclusion matrix
		
		
		** TEST OVERALL SIZE OF THE MATRIX 
		tab `source_occ' `dest_occ' [aw=pweight2] if  `if' & `source_occ'!=. & `dest_occ'!=., matcell(tempmatrixx)

		local rowno_overall=rowsof(tempmatrixx)
		local colno_overall=colsof(tempmatrixx)
		if `colno_overall'!=`rowno_overall' {
		exit 1
		}
		display "row no", `rowno_overall'
		display "col no", `colno_overall'


		**** occ mat dimensions
		occmat_dim_exe `source_occ'
		matrix occmap=r(occno_matrix)
		local local_occno=r(occno)
		display "occ no" r(occno)

			if "`excl_matrix'"==""{
									matrix excl_matrix_loc=I(`rowno_overall')
									display " NO OCCUPATION SELECTION"
									}
		
			if "`excl_matrix'"!="" {
									matrix excl_matrix_loc=`excl_matrix'
									}
		
		
		**** restrictions lifted from the excl_matrix
		
		local occ_excluded=""
										forvalues occt=1(1)`local_occno' {
														if excl_matrix_loc[`occt',`occt']==0 {
														local occt_id=occmap[`occt',1] 
														local occ_excluded= "`occ_excluded' & `source_occ'!=`occt_id' & `dest_occ'!=`occt_id' "
														display "`occ_excluded'"
										}
										}
		
		
		
		
		
		
		***** REPORT BASIC STATS
		
		
		global sheetname "basic_stats"

		cap n putexcel set "${outputdata}/${filename}.xls", sheet("${sheetname}")
		cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", replace)

		
		
		

		ci `occmov_ind' [w=pweight2] if (`occmov_ind'!=.) & `if' $wavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'

		
		putexcel B1=("observations")
		putexcel C1=("occ. mobility")
		putexcel D1=("job finding rate")
		putexcel E1=("obs. jf rate")
		
		
		putexcel A`rowpos'=(" `name_infix' ")
		putexcel B`rowpos'=(r(N))
		putexcel C`rowpos'=(r(mean))

		ci lne [w=pweight2] if `if' $jfwavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'
		putexcel D`rowpos'=(r(mean))
		putexcel E`rowpos'=(r(N))


		global sheetname "$profilesheetname"

		cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

		

			excel_col_exe  ${colposcounter}
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'")

			local tempnumx=${colposcounter}+1
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'_lb")

			
			local tempnumx=${colposcounter}+2
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'_ub")

			local tempnumx=${colposcounter}+3
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'")

			
			local tempnumx=${colposcounter}+4
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'_lb")

			local tempnumx=${colposcounter}+5
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'_ub")

			local tempnumx=${colposcounter}+6
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'")

			local tempnumx=${colposcounter}+7
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'_lb")

			local tempnumx=${colposcounter}+8
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'_ub")

			local tempnumx=${colposcounter}+9
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfnumbers_`name_infix'")

			local tempnumx=${colposcounter}+10
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mobcorr_`name_infix'")
			
			local tempnumx=${colposcounter}+11
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmobcorr_`name_infix'")
			
			
			** count in propocc
			
			local tempnumx=${colposcounter}+12
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("pocount_`name_infix'")
		
			** count in mpropocc
			
			local tempnumx=${colposcounter}+13
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mpocount_`name_infix'")
		
			display "columns initiated"
			
			
		global sheetname "occ_spec"
		cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

		excel_col_exe  ${colposcounter_occspec}
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'")
		
		excel_col_exe  ${colposcounter_occspec}+1
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'_corr")
		
			
		forvalues dur=1(1)18{

		
				display "========================================================================="
				display "========================================================================="
				display ""
				display " duration `dur'"
				display ""
				display "========================================================================="
				display "========================================================================="
				
			local rowpos2=`dur'+1
									
									* cumulative profile 
									display "PROP OCC/NOC STOCK with DURATION=`dur'"
												count if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  $wavecond & n_spellength>=`dur'  & n_spellength<=18 `occ_excluded'
												if r(N)>0 {	
												
												** -- RAW DATA
										
												svy: mean `occmov_ind' if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 `occ_excluded'
												matrix mean1=r(table)
												replace propocc_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace propocc_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace propocc_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'
														
												
													global sheetname "$profilesheetname"

													cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+1
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+2
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

												
												
												** -- CORRECTED DATA
												
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  ///
																							$wavecond & n_spellength>=`dur'  & n_spellength<=18 , matcell(durmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix durmat=J(`rowno_overall', `colno_overall',0)
													
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix durmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $wavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING TRANSITION MATRIX "
												matrix list durmat
												
												**** uncorrected matrix, for occupation-spec mobility rates
												
												xsnet_calcx durmat excl_matrix_loc
												matrix ttransmat=r(ttransmat)
												
													forvalues occc=1(1)`local_occno' {
													local local_staying_coeff`occc'=ttransmat[`occc', `occc']
													}
												
													*fill in the per occupation mobility rates 
													global sheetname "occ_spec"
													cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")


													forvalues oo=1(1)`local_occno' {
														local temp_pos=((`dur'-1)*`local_occno')+1+`oo'
													
														excel_col_exe  ${colposcounter_occspec}
														local coltemp=r(xlscol)
														local local2_temp "local_staying_coeff`oo'"
														putexcel `coltemp'`temp_pos'=(1.0-``local2_temp'')
													
													}
													
													
													
												*** corrected matrix
												
												matrix corr_excl_matrix=`corr_matrix'*excl_matrix_loc
												xsnet_calcx durmat corr_excl_matrix
								
												replace propocc_corr_`name_infix'=r(tmobrate) if n_spellength>=`dur' & n_spellength<=`dur' 
												local popcount=r(tsum_all)
												local xlsoutput=r(tmobrate)
												matrix ttransmat=r(ttransmat)
												
													forvalues occc=1(1)`local_occno' {
													local local_staying_coeff`occc'=ttransmat[`occc', `occc']
													}
													
													*fill in the per occupation mobility rates 
													global sheetname "occ_spec"
													cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

														forvalues oo=1(1)`local_occno' {
														local temp_pos=((`dur'-1)*`local_occno')+1+`oo'
													
														excel_col_exe  ${colposcounter_occspec}+1
														local coltemp=r(xlscol)
														local local2_temp "local_staying_coeff`oo'"
														putexcel `coltemp'`temp_pos'=(1.0-``local2_temp'')
													
														}
												
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}+10
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
													
													
																	excel_col_exe  ${colposcounter}+12
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`popcount')
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

												
												}	

									* marginal profile 				

									
												count if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  $mwavecond & n_spellength==`dur'  
										   
												if r(N)>0 {
												
												** -- RAW DATA
										
												svy: mean `occmov_ind' if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' `occ_excluded'
												matrix mean1=r(table)
												replace mpropocc_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace mpropocc_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace mpropocc_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'


													global sheetname "$profilesheetname"

													cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")
												

																	excel_col_exe  ${colposcounter}+3
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+4
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+5
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

												
												** -- CORRECTED DATA
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  ///
																							$mwavecond & n_spellength==`dur'  , matcell(mdurmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix mdurmat=J(`rowno_overall', `colno_overall',0)
														
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix mdurmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $wavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING MARGINAL TRANSITION MATRIX "
												matrix list mdurmat
												
												*** corrected matrix 
												
												
												matrix corr_excl_matrix=`corr_matrix'*excl_matrix_loc
												xsnet_calcx mdurmat corr_excl_matrix
												local popcount=r(tsum_all)

												display "================================= MARGINAL COUNT <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
												display r(tsum_all)
												
												replace mpropocc_corr_`name_infix'=r(tmobrate) if n_spellength==`dur' 
													local xlsoutput=r(tmobrate)
													
												global sheetname "$profilesheetname"
												cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

													excel_col_exe  ${colposcounter}+13
													local coltemp=r(xlscol)
													putexcel `coltemp'`rowpos2'=(`popcount')

																													

																	excel_col_exe  ${colposcounter}+11
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
									* job finding rate (NOTE EXCLUSION OF OCCUPATIONS ACCORDING TO EXCL MATRIX)	
									
										*matrix occmap=r(occno_matrix)
										*local local_occno=r(occno)
		
										
												svy: mean lne if (lne!=.) & `if' $jfwavecond  & n_spellength==`dur' `occ_excluded'
												matrix mean1=r(table)
												replace jfind_dur_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace jfind_dur_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace jfind_dur_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'	
													
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")
														

																	excel_col_exe  ${colposcounter}+6
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+7
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+8
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+9
																	local coltemp=r(xlscol)
																	local xlsoutput=e(N)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

													
													
													
													
													}
									
												
									
													}
													
		global colposcounter=${colposcounter}+14
		global colposcounter_occspec=${colposcounter_occspec}+2
		
		end // program to calculate the nonemployment statistics (or basically any statistics!!! gender, source occupation, by reason of separation etc. )


		
	//==========================================================================
	// BASIC PROPORTION - U SPELL -- 22 category
	//==========================================================================
	
	
	*cap n 

		global colposcounter=2
		global colposcounter_occspec=3
		global filename "section2_durationprofile_mm"
	global sheetname "basic_stats"
	*cap n putexcel set "${filename}.xls", replace sheet("${sheetname}", replace)

	cap n putexcel set "${outputdata}/${filename}.xls", replace 
	cap n putexcel set "${outputdata}/${filename}.xls", sheet("${sheetname}")
	cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")
	*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

	putexcel B1=("observations")
	putexcel C1=("occ. mobility")
	putexcel D1=("job finding rate")

	global profilesheetname "profile"
	cap n putexcel set "${outputdata}/${filename}.xls", sheet("${profilesheetname}")
	cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${profilesheetname}")

	forvalues cc=1(1)18 {
	local rowcc=1+`cc'
	putexcel A1=("u_duration")
	putexcel A`rowcc'=("`cc'")
	}

		global sheetname "occ_spec"
		cap n putexcel set "${outputdata}/${filename}.xls", sheet("${sheetname}")
		cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

		local rowcc=1
		putexcel A1=("u_duration")
		putexcel B1=("s_occupation")
		
		occmat_dim_exe locc1bfr_mmo
		matrix occmap=r(occno_matrix)
		local local_occno=r(occno)
		display "occ no" r(occno)

		
		forvalues cc=1(1)18 {
		forvalues oo=1(1)`local_occno' {
			local rowcc=`rowcc'+1
			putexcel A`rowcc'=("`cc'")
			local occindtemp=occmap[`oo',1]
			putexcel B`rowcc'=("`occindtemp'")
			}
			}
		
		
		
		* basic 

		
		** occupation selection matrix
		matrix mm_noagric_matrix=I(22)
		matrix mm_noagric_matrix[18,18]=0  // no agriculture
		
		
		*cap putexcel set "durationprofiles_${fileversion}.xls", sheet("main", replace)
		*cap n putexcel set "durationprofiles_${fileversion}.xls", modify sheet("main", replace)
		
		
		
version 13	
global wavecond= " & wave > 3 "
global sttg = "0"
global epanbwidth=3

global wavecond " & wave>4 & interview_no>14 "						//!!!!!!!!!!!!!!!!
global mwavecond " & wave>1 & interview_no>4 & sample_timetogo>3"
	* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
global jfwavecond "& wave>1 & interview_no>4 & sample_timetogo>18-n_spellength "	// this 
global sttg "1"
global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "


		
		
	******* MAIN 	
		propocc_exe mm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 2 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 // & locc1bfr_mmo!=45 & locc1aft_mmo!=45
		*propocc_exe mm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 2 $colposcounter if complete_uspell==1 & entry_ind==1 // & locc1bfr_mmo!=45 & locc1aft_mmo!=45
	
	
	
	******* BY AGE
	* young
	display "$colposcounter"  // 100
	display "$colposcounter_occspec" //17
	propocc_exe mm_yng lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 9 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & tage>=20 & tage<=30
	* prime
	display "$colposcounter"  //114
	display "$colposcounter_occspec" // 19
	propocc_exe mm_prm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 10 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & tage>=35 & tage<=55
	
	
	
	
	************* BY GENDER
	
	* female
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
	propocc_exe mm_fem lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 3 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & sex==2 
	
	* male
	display "$colposcounter"    // 30
	display "$colposcounter_occspec" // 7
	
	propocc_exe mm_male lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 4 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & sex==1 

	
	********** BY EDUC
	
	* highschool dropouts 
	display "$colposcounter" // 44
	display "$colposcounter_occspec"  // 9
	propocc_exe mm_hsd lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 5 $colposcounter mm_noagric_matrix  if complete_uspell==1 & entry_ind==1 & educ==1 
	* highschool grads
	display "$colposcounter"  // 58
	display "$colposcounter_occspec" // 11
	propocc_exe mm_hsg lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 6 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & educ==2 
	
	* some college
	display "$colposcounter"  // 72
	display "$colposcounter_occspec" //13
 	propocc_exe mm_scol lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 7 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & educ==3 
	
	* college grads+ 
	display "$colposcounter"   //86
	display "$colposcounter_occspec" // 15
	propocc_exe mm_col lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 8 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & educ>=4 
	

	
	
//======================================================================================
//  PROPOCC EXE FOR OCCUPATIONS CLASSIFICATIONS THAT ARE  AGGREGATED FROM SCRATCH
//======================================================================================	


//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
/* THIS PROGRAM CALCULATES duration profiles (adjusted, unadjusted) for the main text, and for the appendix 
	NOTE: when occupations are
 to be excluded from the calculation, we have to feed that into this program using the adjusted identity matrix, with the 
	diagonal elements corresponding to the */
//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


		cap program drop propocc_agg_exe
		program define propocc_agg_exe
				syntax anything(name=inputlist) if/
				*syntax varlist(min=3 max=3) occmov_ind source_occ dest_occ [if/] , name_infix() corr_matrix rowpos colpos

				
				/* FIRST ELEMENT: name for variables
					2nd: occ move indicator variable
					3rd: occ before
					4th: occ after
					5: correction matrix
					6: row position of summary variables
					7: column position of duration profile
					(notice column position of occ-spec duration profile (separate sheet) is passed as a global
					8: excl matrix to select a subset of occupations
					*/
				
				/* rowpos= row in the summary spreadsheet
					colpos= colulmn in the profile spreadsheet 
					excl_matrix*/
				
				tokenize `inputlist'
				display "`inputlist'"
				
				local name_infix="`1'"
				local occmov_ind="`2'"
				local source_occ="`3'"
				local dest_occ="`4'"
				local corr_matrix="`5'"
				local rowpos="`6'"
				local colpos="`7'"
				local excl_matrix="`8'"
				local aggregator_matrix="`9'"
				
				*display "`name_infix'"
				*display "`colpos'"
				display "`excl_matrix'"
		
			** make sure globals are already defined, otherwise define them
			
			if "$filename"==""{
									global filename "tempname"
									display " NEEDED TO DEFINE FILENAME GLOBAL "
									
									}
		
			if "$profilesheetname"==""{
									global profilesheetname "profile"
									display " NEEDED TO DEFINE FILENAME GLOBAL "
									
									}
		
			
			if "$colposcounter"==""{
									global colposcounter=2
									display " NEEDED TO DEFINE COLPOSCOUNTER GLOBAL "
									
									}
		
			if "$colposcounter_occspec"==""{
									global colposcounter_occspec=3
									display " NEEDED TO DEFINE COLPOSCOUNTER_OCCSPEC GLOBAL "
									
									}
		
		
		
		set varabbrev off
		set more off
				
		sort personkey yearmonth



		capture drop propocc_`name_infix'
		capture drop propocc_`name_infix'_lb
		capture drop propocc_`name_infix'_ub
		gen propocc_`name_infix'=.
		gen propocc_`name_infix'_lb=.
		gen propocc_`name_infix'_ub=.


		capture drop mpropocc_`name_infix'
		capture drop mpropocc_`name_infix'_lb
		capture drop mpropocc_`name_infix'_ub
		gen mpropocc_`name_infix'=.
		gen mpropocc_`name_infix'_lb=.
		gen mpropocc_`name_infix'_ub=.

		capture drop jfind_dur_`name_infix'
		capture drop jfind_dur_`name_infix'_lb
		capture drop jfind_dur_`name_infix'_ub
		gen jfind_dur_`name_infix'=.
		gen jfind_dur_`name_infix'_lb=.
		gen jfind_dur_`name_infix'_ub=.


		capture drop mpropocc_corr_`name_infix'
		gen mpropocc_corr_`name_infix'=.


		capture drop propocc_corr_`name_infix'
		gen propocc_corr_`name_infix'=.


			***** MATRIX SETUP: define sizes and mapping rows to identities, and the occupational exclusion matrix
		
		
		** TEST OVERALL SIZE OF THE MATRIX 
		tab `source_occ' `dest_occ' [aw=pweight2] if  `if' & `source_occ'!=. & `dest_occ'!=., matcell(tempmatrixx)

		local rowno_overall=rowsof(tempmatrixx)
		local colno_overall=colsof(tempmatrixx)
		if `colno_overall'!=`rowno_overall' {
		exit 1
		}
		display "row no", `rowno_overall'
		display "col no", `colno_overall'


		**** occ mat dimensions
		occmat_dim_exe `source_occ'
		matrix occmap=r(occno_matrix)
		local local_occno=r(occno)
		display "occ no" r(occno)

			if "`excl_matrix'"==""{
									matrix excl_matrix_loc=I(`rowno_overall')
									display " NO OCCUPATION SELECTION"
									}
		
			if "`excl_matrix'"!="" {
									matrix excl_matrix_loc=`excl_matrix'
									}
		
		
			if "`aggregator_matrix'"==""{
									matrix agg_matrix_loc=I(`rowno_overall')
									display " NO OCCUPATION SELECTION"
									}
		
			if "`aggregator_matrix'"!="" {
									matrix agg_matrix_loc=`aggregator_matrix'
									}
		
		**** restrictions lifted from the excl_matrix
		
		local occ_excluded=""
										forvalues occt=1(1)`local_occno' {
														if excl_matrix_loc[`occt',`occt']==0 {
														local occt_id=occmap[`occt',1] 
														local occ_excluded= "`occ_excluded' & `source_occ'!=`occt_id' & `dest_occ'!=`occt_id' "
														display "`occ_excluded'"
										}
										}
		
		*** NUMBER OF AGGREGATED OCCUPATIONS
		local local_occno_agg=rowsof(`aggregator_matrix')
		
		
		
		
		***** REPORT BASIC STATS
		
		
		global sheetname "basic_stats"

		cap n putexcel set "${filename}.xls", sheet("${sheetname}")
		
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", replace)

		
		
		

		ci `occmov_ind' [w=pweight2] if (`occmov_ind'!=.) & `if' $wavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'

		
		putexcel B1=("observations")
		putexcel C1=("occ. mobility")
		putexcel D1=("job finding rate")
		putexcel E1=("obs. jf rate")
		
			display "filling in basic stats"
		putexcel A`rowpos'=(" `name_infix' ")
		putexcel B`rowpos'=(r(N))
		putexcel C`rowpos'=(r(mean))

		ci lne [w=pweight2] if `if' $jfwavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'
		putexcel D`rowpos'=(r(mean))
		putexcel E`rowpos'=(r(N))


		global sheetname "$profilesheetname"

		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

		

			excel_col_exe  ${colposcounter}
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'")

			local tempnumx=${colposcounter}+1
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'_lb")

			
			local tempnumx=${colposcounter}+2
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'_ub")

			local tempnumx=${colposcounter}+3
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'")

			
			local tempnumx=${colposcounter}+4
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'_lb")

			local tempnumx=${colposcounter}+5
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'_ub")

			local tempnumx=${colposcounter}+6
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'")

			local tempnumx=${colposcounter}+7
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'_lb")

			local tempnumx=${colposcounter}+8
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'_ub")

			local tempnumx=${colposcounter}+9
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfnumbers_`name_infix'")

			local tempnumx=${colposcounter}+10
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mobcorr_`name_infix'")
			
			local tempnumx=${colposcounter}+11
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmobcorr_`name_infix'")
			
			
			** count in propocc
			
			local tempnumx=${colposcounter}+12
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("pocount_`name_infix'")
		
			** count in mpropocc
			
			local tempnumx=${colposcounter}+13
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mpocount_`name_infix'")
		
			display "columns initiated"
			
			
		global sheetname "occ_spec"
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

		excel_col_exe  ${colposcounter_occspec}
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'")
		
		excel_col_exe  ${colposcounter_occspec}+1
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'_corr")
		
			
		forvalues dur=1(1)18{

		
				display "========================================================================="
				display "========================================================================="
				display ""
				display " duration `dur'"
				display ""
				display "========================================================================="
				display "========================================================================="
				
			local rowpos2=`dur'+1
									
									* cumulative profile 
									display "PROP OCC/NOC STOCK with DURATION=`dur'"
												count if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  $wavecond & n_spellength>=`dur'  & n_spellength<=18 `occ_excluded'
												if r(N)>0 {	
												
												** -- RAW DATA
										
												svy: mean `occmov_ind' if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 `occ_excluded'
												matrix mean1=r(table)
												replace propocc_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace propocc_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace propocc_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'
														
												
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+1
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+2
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

												
												
												** -- CORRECTED DATA
												
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  ///
																							$wavecond & n_spellength>=`dur'  & n_spellength<=18 , matcell(durmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix durmat=J(`rowno_overall', `colno_overall',0)
													
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix durmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $wavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING TRANSITION MATRIX "
												matrix list durmat
												
												**** uncorrected matrix, for occupation-spec mobility rates
												
												xsnet_calcx durmat agg_matrix_loc'
												matrix ttransmat=r(ttransmat)
												
													forvalues occc=1(1)`local_occno_agg' {
													local local_staying_coeff`occc'=ttransmat[`occc', `occc']
													}
												
													*fill in the per occupation mobility rates 
													global sheetname "occ_spec"
													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")


													forvalues oo=1(1)`local_occno_agg' {
														local temp_pos=((`dur'-1)*`local_occno_agg')+1+`oo'
													
														excel_col_exe  ${colposcounter_occspec}
														local coltemp=r(xlscol)
														local local2_temp "local_staying_coeff`oo'"
														putexcel `coltemp'`temp_pos'=(1.0-``local2_temp'')
													
													}
													
													
													
												*** corrected matrix
												
												matrix corr_agg_matrix=`corr_matrix'*agg_matrix_loc'
												xsnet_calcx durmat corr_agg_matrix
								
												replace propocc_corr_`name_infix'=r(tmobrate) if n_spellength>=`dur' & n_spellength<=`dur' 
												local popcount=r(tsum_all)
												local xlsoutput=r(tmobrate)
												matrix ttransmat=r(ttransmat)
												
													forvalues occc=1(1)`local_occno_agg' {
													local local_staying_coeff`occc'=ttransmat[`occc', `occc']
													}
													
													*fill in the per occupation mobility rates 
													global sheetname "occ_spec"
													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

														forvalues oo=1(1)`local_occno_agg' {
														local temp_pos=((`dur'-1)*`local_occno_agg')+1+`oo'
													
														excel_col_exe  ${colposcounter_occspec}+1
														local coltemp=r(xlscol)
														local local2_temp "local_staying_coeff`oo'"
														putexcel `coltemp'`temp_pos'=(1.0-``local2_temp'')
													
														}
												
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}+10
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
													
													
																	excel_col_exe  ${colposcounter}+12
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`popcount')
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

												
												}	

									* marginal profile 				

									
												count if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  $mwavecond & n_spellength==`dur'  
										   
												if r(N)>0 {
												
												** -- RAW DATA
										
												svy: mean `occmov_ind' if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' `occ_excluded'
												matrix mean1=r(table)
												replace mpropocc_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace mpropocc_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace mpropocc_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'


													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
												

																	excel_col_exe  ${colposcounter}+3
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+4
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+5
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

												
												** -- CORRECTED DATA
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  ///
																							$mwavecond & n_spellength==`dur'  , matcell(mdurmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix mdurmat=J(`rowno_overall', `colno_overall',0)
														
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix mdurmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $wavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING MARGINAL TRANSITION MATRIX "
												matrix list mdurmat
												
												*** corrected matrix 
												
												
												matrix corr_excl_matrix=`corr_matrix'*agg_matrix_loc'
												xsnet_calcx mdurmat corr_excl_matrix
												local popcount=r(tsum_all)

												display "================================= MARGINAL COUNT <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
												display r(tsum_all)
												
												replace mpropocc_corr_`name_infix'=r(tmobrate) if n_spellength==`dur' 
													local xlsoutput=r(tmobrate)
													
												global sheetname "$profilesheetname"
												cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

													excel_col_exe  ${colposcounter}+13
													local coltemp=r(xlscol)
													putexcel `coltemp'`rowpos2'=(`popcount')

																													

																	excel_col_exe  ${colposcounter}+11
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
									* job finding rate (NOTE EXCLUSION OF OCCUPATIONS ACCORDING TO EXCL MATRIX)	
									
										*matrix occmap=r(occno_matrix)
										*local local_occno=r(occno)
		
										
												svy: mean lne if (lne!=.) & `if' $jfwavecond  & n_spellength==`dur' `occ_excluded'
												matrix mean1=r(table)
												replace jfind_dur_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace jfind_dur_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace jfind_dur_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'	
													
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
														

																	excel_col_exe  ${colposcounter}+6
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+7
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+8
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+9
																	local coltemp=r(xlscol)
																	local xlsoutput=e(N)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

													
													
													
													
													}
									
												
									
													}
													
		global colposcounter=${colposcounter}+14
		global colposcounter_occspec=${colposcounter_occspec}+2
		
		end // program to calculate the nonemployment statistics (or basically any statistics!!! gender, source occupation, by reason of separation etc. )

	
	
******** 2ND VERSION

	cap program drop propocc_agg_exe
		program define propocc_agg_exe
				syntax anything(name=inputlist) if/
				*syntax varlist(min=3 max=3) occmov_ind source_occ dest_occ [if/] , name_infix() corr_matrix rowpos colpos

				
				/* FIRST ELEMENT: name for variables
					2nd: occ move indicator variable
					3rd: occ before
					4th: occ after
					5: correction matrix
					6: row position of summary variables
					7: column position of duration profile
					(notice column position of occ-spec duration profile (separate sheet) is passed as a global
					8: excl matrix to select a subset of occupations
					*/
				
				/* rowpos= row in the summary spreadsheet
					colpos= colulmn in the profile spreadsheet 
					excl_matrix*/
				
				tokenize `inputlist'
				display "`inputlist'"
				
				local name_infix="`1'"
				local occmov_ind="`2'"
				local source_occ="`3'"
				local dest_occ="`4'"
				local corr_matrix="`5'"
				local rowpos="`6'"
				local colpos="`7'"
				local excl_matrix="`8'"
				local aggregator_matrix="`9'"
				
				*display "`name_infix'"
				*display "`colpos'"
				display "`excl_matrix'"
		
			** make sure globals are already defined, otherwise define them
			
			if "$filename"==""{
									global filename "tempname"
									display " NEEDED TO DEFINE FILENAME GLOBAL "
									
									}
		
			if "$profilesheetname"==""{
									global profilesheetname "profile"
									display " NEEDED TO DEFINE FILENAME GLOBAL "
									
									}
		
			
			if "$colposcounter"==""{
									global colposcounter=2
									display " NEEDED TO DEFINE COLPOSCOUNTER GLOBAL "
									
									}
		
			if "$colposcounter_occspec"==""{
									global colposcounter_occspec=3
									display " NEEDED TO DEFINE COLPOSCOUNTER_OCCSPEC GLOBAL "
									
									}
		
		
		
		set varabbrev off
		set more off
				
		sort personkey yearmonth



		capture drop propocc_`name_infix'
		capture drop propocc_`name_infix'_lb
		capture drop propocc_`name_infix'_ub
		gen propocc_`name_infix'=.
		gen propocc_`name_infix'_lb=.
		gen propocc_`name_infix'_ub=.


		capture drop mpropocc_`name_infix'
		capture drop mpropocc_`name_infix'_lb
		capture drop mpropocc_`name_infix'_ub
		gen mpropocc_`name_infix'=.
		gen mpropocc_`name_infix'_lb=.
		gen mpropocc_`name_infix'_ub=.

		capture drop jfind_dur_`name_infix'
		capture drop jfind_dur_`name_infix'_lb
		capture drop jfind_dur_`name_infix'_ub
		gen jfind_dur_`name_infix'=.
		gen jfind_dur_`name_infix'_lb=.
		gen jfind_dur_`name_infix'_ub=.


		capture drop mpropocc_corr_`name_infix'
		gen mpropocc_corr_`name_infix'=.


		capture drop propocc_corr_`name_infix'
		gen propocc_corr_`name_infix'=.


			***** MATRIX SETUP: define sizes and mapping rows to identities, and the occupational exclusion matrix
		
		
		** TEST OVERALL SIZE OF THE MATRIX 
		tab `source_occ' `dest_occ' [aw=pweight2] if  `if' & `source_occ'!=. & `dest_occ'!=., matcell(tempmatrixx)

		local rowno_overall=rowsof(tempmatrixx)
		local colno_overall=colsof(tempmatrixx)
		if `colno_overall'!=`rowno_overall' {
		exit 1
		}
		display "row no", `rowno_overall'
		display "col no", `colno_overall'


		**** occ mat dimensions
		occmat_dim_exe `source_occ'
		matrix occmap=r(occno_matrix)
		local local_occno=r(occno)
		display "occ no" r(occno)

			if "`excl_matrix'"==""{
									matrix excl_matrix_loc=I(`rowno_overall')
									display " NO OCCUPATION SELECTION"
									}
		
			if "`excl_matrix'"!="" {
									matrix excl_matrix_loc=`excl_matrix'
									}
		
		
			if "`aggregator_matrix'"==""{
									matrix agg_matrix_loc=I(`rowno_overall')
									display " NO OCCUPATION SELECTION"
									}
		
			if "`aggregator_matrix'"!="" {
									matrix agg_matrix_loc=`aggregator_matrix'
									}
		
		**** restrictions lifted from the excl_matrix
		
		local occ_excluded=""
										forvalues occt=1(1)`local_occno' {
														if excl_matrix_loc[`occt',`occt']==0 {
														local occt_id=occmap[`occt',1] 
														local occ_excluded= "`occ_excluded' & `source_occ'!=`occt_id' & `dest_occ'!=`occt_id' "
														display "`occ_excluded'"
										}
										}
		
		*** NUMBER OF AGGREGATED OCCUPATIONS
		local local_occno_agg=rowsof(`aggregator_matrix')
		
		
		
		
		***** REPORT BASIC STATS
		
		
		global sheetname "basic_stats"

		cap n putexcel set "${filename}.xls", sheet("${sheetname}")
		
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", replace)

		
		
		

		ci `occmov_ind' [w=pweight2] if (`occmov_ind'!=.) & `if' $wavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'

		
		putexcel B1=("observations")
		putexcel C1=("occ. mobility")
		putexcel D1=("job finding rate")
		putexcel E1=("obs. jf rate")
		
			display "filling in basic stats"
		putexcel A`rowpos'=(" `name_infix' ")
		putexcel B`rowpos'=(r(N))
		putexcel C`rowpos'=(r(mean))

		ci lne [w=pweight2] if `if' $jfwavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'
		putexcel D`rowpos'=(r(mean))
		putexcel E`rowpos'=(r(N))


		global sheetname "$profilesheetname"

		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

		

			excel_col_exe  ${colposcounter}
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'")

			local tempnumx=${colposcounter}+1
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'_lb")

			
			local tempnumx=${colposcounter}+2
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mob_`name_infix'_ub")

			local tempnumx=${colposcounter}+3
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'")

			
			local tempnumx=${colposcounter}+4
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'_lb")

			local tempnumx=${colposcounter}+5
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmob_`name_infix'_ub")

			local tempnumx=${colposcounter}+6
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'")

			local tempnumx=${colposcounter}+7
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'_lb")

			local tempnumx=${colposcounter}+8
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfdur_`name_infix'_ub")

			local tempnumx=${colposcounter}+9
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("jfnumbers_`name_infix'")

			local tempnumx=${colposcounter}+10
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mobcorr_`name_infix'")
			
			local tempnumx=${colposcounter}+11
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mrgmobcorr_`name_infix'")
			
			
			** count in propocc
			
			local tempnumx=${colposcounter}+12
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("pocount_`name_infix'")
		
			** count in mpropocc
			
			local tempnumx=${colposcounter}+13
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("mpocount_`name_infix'")
		
			display "columns initiated"
			
			
		global sheetname "occ_spec"
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

		excel_col_exe  ${colposcounter_occspec}
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'")
		
		excel_col_exe  ${colposcounter_occspec}+1
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'_corr")
		
			
		forvalues dur=1(1)18{

		
				display "========================================================================="
				display "========================================================================="
				display ""
				display " duration `dur'"
				display ""
				display "========================================================================="
				display "========================================================================="
				
			local rowpos2=`dur'+1
									
									* cumulative profile 
									display "PROP OCC/NOC STOCK with DURATION=`dur'"
												count if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  $wavecond & n_spellength>=`dur'  & n_spellength<=18 `occ_excluded'
												if r(N)>0 {	
												
												** -- RAW DATA
										
												svy: mean `occmov_ind' if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 `occ_excluded'
												matrix mean1=r(table)
												replace propocc_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace propocc_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace propocc_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'
														
												
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+1
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+2
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

												
												
												** -- CORRECTED DATA
												
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  ///
																							$wavecond & n_spellength>=`dur'  & n_spellength<=18 , matcell(durmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix durmat=J(`rowno_overall', `colno_overall',0)
													
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $wavecond  & n_spellength>=`dur' & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix durmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $wavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING TRANSITION MATRIX "
												matrix list durmat
												
												**** uncorrected matrix, for occupation-spec mobility rates
												
												xsnet_calcx durmat agg_matrix_loc'
												matrix ttransmat=r(ttransmat)
												
													forvalues occc=1(1)`local_occno_agg' {
													local local_staying_coeff`occc'=ttransmat[`occc', `occc']
													}
												
													*fill in the per occupation mobility rates 
													global sheetname "occ_spec"
													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")


													forvalues oo=1(1)`local_occno_agg' {
														local temp_pos=((`dur'-1)*`local_occno_agg')+1+`oo'
													
														excel_col_exe  ${colposcounter_occspec}
														local coltemp=r(xlscol)
														local local2_temp "local_staying_coeff`oo'"
														putexcel `coltemp'`temp_pos'=(1.0-``local2_temp'')
													
													}
													
													
													
												*** corrected matrix
												
												matrix corr_agg_matrix=`corr_matrix'*agg_matrix_loc'
												xsnet_calcx durmat corr_agg_matrix
								
												replace propocc_corr_`name_infix'=r(tmobrate) if n_spellength>=`dur' & n_spellength<=`dur' 
												local popcount=r(tsum_all)
												local xlsoutput=r(tmobrate)
												matrix ttransmat=r(ttransmat)
												
													forvalues occc=1(1)`local_occno_agg' {
													local local_staying_coeff`occc'=ttransmat[`occc', `occc']
													}
													
													*fill in the per occupation mobility rates 
													global sheetname "occ_spec"
													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

														forvalues oo=1(1)`local_occno_agg' {
														local temp_pos=((`dur'-1)*`local_occno_agg')+1+`oo'
													
														excel_col_exe  ${colposcounter_occspec}+1
														local coltemp=r(xlscol)
														local local2_temp "local_staying_coeff`oo'"
														putexcel `coltemp'`temp_pos'=(1.0-``local2_temp'')
													
														}
												
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}+10
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
													
													
																	excel_col_exe  ${colposcounter}+12
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`popcount')
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

												
												}	

									* marginal profile 				

									
												count if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  $mwavecond & n_spellength==`dur'  
										   
												if r(N)>0 {
												
												** -- RAW DATA
										
												svy: mean `occmov_ind' if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' `occ_excluded'
												matrix mean1=r(table)
												replace mpropocc_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace mpropocc_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace mpropocc_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'


													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
												

																	excel_col_exe  ${colposcounter}+3
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+4
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+5
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

												
												** -- CORRECTED DATA
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. & sample_timetogo>$sttg & `if'  ///
																							$mwavecond & n_spellength==`dur'  , matcell(mdurmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix mdurmat=J(`rowno_overall', `colno_overall',0)
														
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $mwavecond  & n_spellength==`dur' /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix mdurmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $wavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING MARGINAL TRANSITION MATRIX "
												matrix list mdurmat
												
												*** corrected matrix 
												
												
												matrix corr_excl_matrix=`corr_matrix'*agg_matrix_loc'
												xsnet_calcx mdurmat corr_excl_matrix
												local popcount=r(tsum_all)

												display "================================= MARGINAL COUNT <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
												display r(tsum_all)
												
												replace mpropocc_corr_`name_infix'=r(tmobrate) if n_spellength==`dur' 
													local xlsoutput=r(tmobrate)
													
												global sheetname "$profilesheetname"
												cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

													excel_col_exe  ${colposcounter}+13
													local coltemp=r(xlscol)
													putexcel `coltemp'`rowpos2'=(`popcount')

																													

																	excel_col_exe  ${colposcounter}+11
																	local coltemp=r(xlscol)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
									* job finding rate (NOTE EXCLUSION OF OCCUPATIONS ACCORDING TO EXCL MATRIX)	
									
										*matrix occmap=r(occno_matrix)
										*local local_occno=r(occno)
		
										
												svy: mean lne if (lne!=.) & `if' $jfwavecond  & n_spellength==`dur' `occ_excluded'
												matrix mean1=r(table)
												replace jfind_dur_`name_infix'=mean1[1,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace jfind_dur_`name_infix'_lb=mean1[5,1] if n_spellength>=`dur' & n_spellength<=`dur' 
												replace jfind_dur_`name_infix'_ub=mean1[6,1] if n_spellength>=`dur' & n_spellength<=`dur'	
													
													
													global sheetname "$profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
														

																	excel_col_exe  ${colposcounter}+6
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+7
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[5,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	
																	excel_col_exe  ${colposcounter}+8
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[6,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+9
																	local coltemp=r(xlscol)
																	local xlsoutput=e(N)
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

													
													
													
													
													}
									
												
									
													}
													
		global colposcounter=${colposcounter}+14
		global colposcounter_occspec=${colposcounter_occspec}+2
		
		end // program to calculate the nonemployment statistics (or basically any statistics!!! gender, source occupation, by reason of separation etc. )

		
	
	
//======================================================================	
// RTMM
//=======================================================================


	cd "${outputdata}"
	global colposcounter=2
	global colposcounter_occspec=3
	global filename "section2_durationprofile_rtmm"
	global sheetname "basic_stats"
	*cap n putexcel set "${filename}.xls", replace sheet("${sheetname}", replace)

	cap n putexcel set "${filename}.xls", replace 
	cap n putexcel set "${filename}.xls", sheet("${sheetname}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
	*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

	putexcel B1=("observations")
	putexcel C1=("occ. mobility")
	putexcel D1=("job finding rate")

	global profilesheetname "profile"
	cap n putexcel set "${filename}.xls", sheet("${profilesheetname}")
	cap n putexcel set "${filename}.xls", modify sheet("${profilesheetname}")

	forvalues cc=1(1)18 {
	local rowcc=1+`cc'
	putexcel A1=("u_duration")
	putexcel A`rowcc'=("`cc'")
	}

		global sheetname "occ_spec"
		cap n putexcel set "${filename}.xls", sheet("${sheetname}")
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

		local rowcc=1
		putexcel A1=("u_duration")
		putexcel B1=("s_occupation")
		
		occmat_dim_exe locc1bfr_rtmm
		matrix occmap=r(occno_matrix)
		local local_occno=r(occno)
		display "occ no" r(occno)

		
		forvalues cc=1(1)18 {
		forvalues oo=1(1)`local_occno' {
			local rowcc=`rowcc'+1
			putexcel A`rowcc'=("`cc'")
			local occindtemp=occmap[`oo',1]
			putexcel B`rowcc'=("`occindtemp'")
			}
			}
			
		
	* occupation selection matrix: based on input of MMO occupations
		matrix rtmm_noagric_matrix=I(22)
		matrix rtmm_noagric_matrix[18,18]=0  
		
	** GINV_RTMM constructed from GINV_MM and aggregator matrix 	
	matrix rtmmconv_tpose= ( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 
	matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		
	** command
	propocc_agg_exe rtmm lne_c_rtmm locc1bfr_mmo locc1aft_mmo Ginv_mm 2 $colposcounter rtmm_noagric_matrix rtmmconv_tpose if complete_uspell==1 & entry_ind==1 
	
	** no management
	* occupation selection matrix: based on input of MMO occupations
	matrix rtnm_noagric_matrix=I(22)
	matrix rtnm_noagric_matrix[18,18]=0  
	matrix rtnm_noagric_matrix[1,1]=0  
		
	** GINV_RTMM constructed from GINV_MM and aggregator matrix 	
	matrix rtnmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
									     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 
	
	matrix Ginv_rtnm=Ginv_mm*rtnmconv_tpose'
		
	** RT No Management MM
	propocc_agg_exe rtnm lne_c_rtmm locc1bfr_mmo locc1aft_mmo Ginv_mm 3 $colposcounter rtnm_noagric_matrix rtnmconv_tpose if complete_uspell==1 & entry_ind==1 
	
	** RTMM NUN MALES
	propocc_agg_exe rtmm_nunm lne_c_rtmm locc1bfr_mmo locc1aft_mmo Ginv_mm 4 $colposcounter rtmm_noagric_matrix rtmmconv_tpose if complete_nunspell==1 & entry_ind==1 & sex==1
	
	** RT No Management MM NUN MALES
	propocc_agg_exe rtnm_nunm lne_c_rtmm locc1bfr_mmo locc1aft_mmo Ginv_mm 5 $colposcounter rtnm_noagric_matrix rtnmconv_tpose if complete_nunspell==1 & entry_ind==1 & sex==1

	
	
//==============================================================================
// NUN MALES (ONLINE APPENDIX)
//==============================================================================	
	
	
	*cap n 

		global colposcounter=2
		global colposcounter_occspec=3
		global filename "section2_durationprofile_nun_male"
	global sheetname "basic_stats"
	*cap n putexcel set "${filename}.xls", replace sheet("${sheetname}", replace)

	cap n putexcel set "${filename}.xls", replace 
	cap n putexcel set "${outputdata}/${filename}.xls", sheet("${sheetname}")
	cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")
	*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

	putexcel B1=("observations")
	putexcel C1=("occ. mobility")
	putexcel D1=("job finding rate")

	global profilesheetname "profile"
	cap n putexcel set "${outputdata}/${filename}.xls", sheet("${profilesheetname}")
	cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${profilesheetname}")

	forvalues cc=1(1)18 {
	local rowcc=1+`cc'
	putexcel A1=("u_duration")
	putexcel A`rowcc'=("`cc'")
	}

		global sheetname "occ_spec"
		cap n putexcel set "${outputdata}/${filename}.xls", sheet("${sheetname}")
		cap n putexcel set "${outputdata}/${filename}.xls", modify sheet("${sheetname}")

		local rowcc=1
		putexcel A1=("u_duration")
		putexcel B1=("s_occupation")
		
		occmat_dim_exe locc1bfr_mmo
		matrix occmap=r(occno_matrix)
		local local_occno=r(occno)
		display "occ no" r(occno)

		
		forvalues cc=1(1)18 {
		forvalues oo=1(1)`local_occno' {
			local rowcc=`rowcc'+1
			putexcel A`rowcc'=("`cc'")
			local occindtemp=occmap[`oo',1]
			putexcel B`rowcc'=("`occindtemp'")
			}
			}
		
		
		
		* basic 

		
		** occupation selection matrix
		matrix mm_noagric_matrix=I(22)
		matrix mm_noagric_matrix[18,18]=0  // no agriculture
		
		
		*cap putexcel set "durationprofiles_${fileversion}.xls", sheet("main", replace)
		*cap n putexcel set "durationprofiles_${fileversion}.xls", modify sheet("main", replace)
		
		
		
		
global sttg = "0"
global epanbwidth=3
global wavecond " & wave>4 & interview_no2>14 & sample_timetogo>=0 & durdistr_stability==1"
		
		
		
		

	
	
	
	********** NUN PROFILES MALES
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
		propocc_exe mm_nunmale lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 3 $colposcounter mm_noagric_matrix if complete_nunspell==1 & entry_ind==1 & sex==1
		
	********** NUN PROFILES MALES YOUNG
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
		propocc_exe mm_nunmyng lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 4 $colposcounter mm_noagric_matrix if complete_nunspell==1 & entry_ind==1 & sex==1 & tage>=20 & tage<=30
		
	********** NUN PROFILES MALES PRIME
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
		propocc_exe mm_nunmprm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 5 $colposcounter mm_noagric_matrix if complete_nunspell==1 & entry_ind==1 & sex==1 & tage>=35 & tage<=55
	
	********** NUN PROFILES MALES GOOD TIMES
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
		propocc_exe mm_nunmgood lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 6 $colposcounter mm_noagric_matrix if complete_nunspell==1 & entry_ind==1 & sex==1 & rec_exp_ind3==1
	
	
	********** NUN PROFILES MALES BAD TIMES
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
		propocc_exe mm_nunmbad lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 7 $colposcounter mm_noagric_matrix if complete_nunspell==1 & entry_ind==1 & sex==1 & rec_exp_ind3==-1
	
	
	********** REGULAR U PURE PROFILES GOOD TIMES
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
		propocc_exe mm_ugood lue_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 8 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & rec_exp_ind3==1
	
	********** REGULAR U PURE PROFILES BAD TIMES
	display "$colposcounter"    // 16
	display "$colposcounter_occspec" // 5
	
		propocc_exe mm_ubad lue_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 9 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & rec_exp_ind3==-1
	

	******* MAIN 	
		propocc_exe mm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 10 $colposcounter mm_noagric_matrix if complete_uspell==1 & entry_ind==1 // & locc1bfr_mmo!=45 & locc1aft_mmo!=45
		*propocc_exe mm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 2 $colposcounter if complete_uspell==1 & entry_ind==1 // & locc1bfr_mmo!=45 & locc1aft_mmo!=45
	
	
	
	

	
	
	
	
	
*************************************************************************************************************************************************************************************	
*************************************************************************************************************************************************************************************	
	
	
	
	
	
	

	
********************************************************************************
**
			** PART 2. DRAWING PICTURES AND SAVING TARGETS **
***
********************************************************************************	
	
	
	
/* THIS DO FILE DRAWS FIG 1A and FIG 1B in SECTION 2, SAVES THE OCCUPATIONAL 
(adapted from section2_durationprofile_appendix_pictures_jan2018.do)
*/

set varabbrev off

*************************************
** LOADING DATA & MERGING 
*************************************

* mmo
clear
import excel "${outputdata}/section2_durationprofile_mm.xls", sheet("profile") firstrow
cap  n destring u_duration, replace
save "${tempdata}/durationprofiletemp_mm.dta", replace
* mmo - routine
clear
import excel "${outputdata}/section2_durationprofile_rtmm.xls", sheet("profile") firstrow
cap  n destring u_duration, replace
save "${tempdata}/durationprofiletemp_rtmm.dta", replace

** mm_nun_male
clear
import excel "${outputdata}/section2_durationprofile_nun_male.xls", sheet("profile") firstrow
cap  n destring u_duration, replace
save "${tempdata}/durationprofiletemp_mm_nun_male.dta", replace


*** MERGING


* mmo _rtmm
capture drop _merge_rtmm
merge 1:1 u_duration using "${tempdata}\durationprofiletemp_rtmm.dta", gen(_merge_rtmm) 


drop mrg* 
drop jf*
drop mob_mm mob_mm_lb mob_mm_ub mobcorr_mm pocount_mm mpocount_mm

* mm
capture drop _merge_mm
merge 1:1 u_duration using "${tempdata}/durationprofiletemp_mm.dta", gen(_merge_mm)

drop mrg* 
drop jf*




************************************
*** for use as frequency weight
*************************************



** ALL
	capture drop fweightc*
	
	* mm
	gen fweightc_mm=round(mpocount_mm)
	list fweightc_mm mpocount_mm

	* mm-yng
	gen fweightc_mm_yng=round(mpocount_mm_yng)
	list fweightc_mm_yng mpocount_mm_yng

	* mm-prm
	gen fweightc_mm_prm=round(mpocount_mm_prm)
	list fweightc_mm_prm mpocount_mm_prm
	
	* mm-nun-male
	gen fweightc_mm_nunmale=round(mobcorr_mm_nunmale)
	list fweightc_mm_nunmale mpocount_mm_nunmale

	* mm-nun-male-yng
	gen fweightc_mm_nunmyng=round(mpocount_mm_nunmyng)
	list fweightc_mm_nunmyng mpocount_mm_nunmyng

	* mm-nun-male-prm
	gen fweightc_mm_nunmprm=round(mpocount_mm_nunmprm)
	list fweightc_mm_nunmprm mpocount_mm_nunmprm
	
	* rtmm
	gen fweightc_rtmm=round(mpocount_rtmm)
	list fweightc_rtmm mpocount_rtmm

	* rtnm
	gen fweightc_rtnm=round(mpocount_rtnm)
	list fweightc_rtnm mpocount_rtnm
	
	
	* mm-fem
	gen fweightc_mm_fem=round(mpocount_mm_fem)
	list fweightc_mm_fem mpocount_mm_fem

	* mm-male
	gen fweightc_mm_male=round(mpocount_mm_male)
	list fweightc_mm_male mpocount_mm_male
	

	* mm-hsg
	gen fweightc_mm_hsg=round(mpocount_mm_hsg)
	list fweightc_mm_hsg mpocount_mm_hsg

	* mm-col
	gen fweightc_mm_col=round(mpocount_mm_col)
	list fweightc_mm_col mpocount_mm_col
	
	

	
	
//=====================================================
//  MOBILITY DURATION PROFILE PICTURES
//========================================================


** auxiliary programs
do "${step2codedir}/aux_directory/aux_programs.do"
			/* programs:
					xsnet_calcx
					occmat_dim_exe	
			*/
** correction matrices
do "${step1codedir}/Ginv_matrices.do"



//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
	capture program drop lowess_grossmob_exe
	program define lowess_grossmob_exe
				args suffix_in ind

	capture drop lows_pdur_`ind'
	capture drop lows_pdur_`ind'_corr

	lowess mob_`suffix_in' u_duration if u_duration<=18, gen(lows_pdur_`ind') bwidth(1) nograph
	lowess mobcorr_`suffix_in' u_duration if u_duration<=18, gen(lows_pdur_`ind'_corr) bwidth(1) nograph


	end 

	
	

	capture program drop lpoly_grossmob_exe
	program define lpoly_grossmob_exe
				args suffix_in ind

	capture drop lpoly_pdur_`ind'
	capture drop lpoly_pdur_`ind'_corr
	capture drop fweightc_`suffix_in'
	gen fweightc_`suffix_in'=round(mpocount_`suffix_in')

	lpoly mob_`suffix_in' u_duration [fw=fweightc_`suffix_in'] if u_duration<=18 , gen(lpoly_pdur_`ind') degree(2) at(u_duration) bwidth(2) nograph
	lpoly mobcorr_`suffix_in' u_duration [fw=fweightc_`suffix_in'] if u_duration<=18 , gen(lpoly_pdur_`ind'_corr) degree(2) at(u_duration) bwidth(2) nograph


	end 

	save "${tempdata}/durationprofiletemp2.dta", replace
	
	
	
	
/*    STYLE OVERALL PICTURE 
		#delimit ;	
		twoway rarea  mgtnet_excessmob_mmo excessmob_mmo u_spellength if u_spellength<=12, color(sandb) xlab(1(1)12) fintensity(inten30) ylab(0(0.1)0.7) ysc(r(0 0.7))|| 
		rarea lowess_pdur_mmo_adj mgtnet_excessmob_mmo u_spellength if u_spellength<=12, color(sienna) xlab(1(1)12)  ||
		rarea  excessmob_mmo zeroline u_spellength if u_spellength<=12, color(gold) lcolor(dkorange) lwidth(vvthin) xlab(1(1)12) || 
		scatter lowess_pdur_mmo pdur_mmo_ub pdur_mmo_lb u_spellength if u_spellength<=12, 
		connect( l l l) lwidth(medthin medium medium) lpattern(solid shortdash shortdash) msymbol( i i i) lcolor(erose*0.7 erose erose ) 
		|| scatter pdur_mmo  pdur_mmo_gamma u_spellength if u_spellength<=12, 
		msymbol(circle x) mcolor(erose black) msize(small medium) mlwidth(medium medthick) graphregion(color(white))
		xtitle("Length of Incomplete Unemployment Spell ") title("Occupational Mobility of the Unemployed", size(medlarge) ) ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		subtitle("and (incomplete) unemployment duration (22 MOG)") legend(on width(150) size(small) order( 2 9 1  4 3 10 ) rows(3) label(1 "Net Outflow from Management") label(2 "Net mobility (excl. mgt, smoothed)") 
		label(8 "Corrected Gross Mobility ") label(5 "95% CI Uncorrected Gross Mob.") label(9 "{&Gamma}-corrected Gross Mobility ")  label(4 "Uncorrected Gross Mobility ") label(3 "Excess Reallocation (smoothed)") 
		label(10 "Excess Reallocation (smoothed)") note("95% CI for uncorrected mobility in light-grey/brown dashed line; dots/crosses: unsmoothed observations", size(vsmall) span))  
		|| scatter lowess_pdur_mmo_adj excessmob_mmo u_spellength if u_spellength<=12, color(black dkorange) lpattern(longdash "-..-..") connect(l l) msymbol(i i) lwidth(thick thick);	
		#delimit cr
		graph export "${tempdata}/data\main_duration2_mmo_v5.pdf", as(pdf) replace
			*/
		

	// LOWESS ALL 
	

	
	cap n lowess_grossmob_exe mm mm
	cap n lowess_grossmob_exe mm_yng mm_yng
	cap n lowess_grossmob_exe mm_prm mm_prm
	
	cap n lowess_grossmob_exe mm_male mm_male
	cap n lowess_grossmob_exe mm_fem mm_fem
	cap n lowess_grossmob_exe mm_hsg mm_hsg 
	cap n lowess_grossmob_exe mm_col mm_col 
	
	cap n lowess_grossmob_exe mm_nunmale mm_nunmale
	cap n lowess_grossmob_exe mm_nunmyng mm_nunmyng
	cap n lowess_grossmob_exe mm_nunmprm mm_nunmprm
	
	cap n lowess_grossmob_exe rtmm rtmm
	cap n lowess_grossmob_exe rtnm rtnm
	
	cap n lpoly_grossmob_exe mm mm
	cap n lpoly_grossmob_exe mm_yng mm_yng
	cap n lpoly_grossmob_exe mm_prm mm_prm
	
	cap n lpoly_grossmob_exe mm_male mm_male
	cap n lpoly_grossmob_exe mm_fem mm_fem
	cap n lpoly_grossmob_exe mm_hsg mm_hsg 
	cap n lpoly_grossmob_exe mm_col mm_col 
	
	cap n lpoly_grossmob_exe mm_nunmale mm_nunmale
	cap n lpoly_grossmob_exe mm_nunmyng mm_nunmyng
	cap n lpoly_grossmob_exe mm_nunmprm mm_nunmprm
	
	cap n lpoly_grossmob_exe rtmm rtmm
	cap n lpoly_grossmob_exe rtnm rtnm
	
	
	
********************************************************************	
*** BASIC PICTURE: overall, corrected + uncorrected ( +se bands)
*********************************************************************




	/*
#delimit ;				
		scatter mob_mm mob_mm_ub mob_mm_lb lpoly_pdur_mm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Th i i i) mlwidth(medthick) mlcolor(purple*0.4) lcolor(purple*0.3 purple*0.3 purple*0.3 purple*0.3)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter lpoly_pdur_mm_corr u_duration if u_duration<=12, connect(l) lwidth(thick)
		 msymbol(i ) mlwidth(medthick ) lcolor("80 0 80") mcolor("80 0 80")  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white))
		xtitle("Time in Unemployment (months) ") ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		/* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("All workers") */
		legend(on width(150) size(small) order( 5 2 1  4) rows(2) label(5 "All ({&Gamma}-corrected, smoothed)") label(1 "All (uncorrected, unsmoothed)") 
		label(2 "All (uncorrected) 95% CI") label(4 "All (uncorrected, smoothed)") )  
		;
#delimit cr

		graph export "${tempdata}/main_duration2_all_mm_v10.pdf", as(pdf) replace

		
		
#delimit ;				
		scatter mob_rtmm mob_rtmm_ub mob_rtmm_lb lpoly_pdur_rtmm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Th i i i) mlwidth(medthick) mlcolor(purple*0.4) lcolor(purple*0.3 purple*0.3 purple*0.3 purple*0.3)  xlab(1(1)12)   ylab(0.1(0.1)0.5) ysc(r(0.1 0.5)) 
		|| scatter lpoly_pdur_rtmm_corr u_duration if u_duration<=12, connect(l) lwidth(thick)
		 msymbol(i ) mlwidth(medthick ) lcolor(black) mcolor(black)  xlab(1(1)12)   ylab(0.1(0.1)0.5) ysc(r(0.1 0.5)) graphregion(color(white))
		xtitle("Time in Unemployment (months) ") ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		/* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("All workers") */
		legend(on width(150) size(small) order( 5 2 1  4) rows(2) label(5 "All ({&Gamma}-corrected, smoothed)") label(1 "All (uncorrected, unsmoothed)") 
		label(2 "All (uncorrected) 95% CI") label(4 "All (uncorrected, smoothed)") )  
		;
#delimit cr

		graph export "${tempdata}/main_duration2_all_rtmm_v10.pdf", as(pdf) replace
*/

		
******************* COMBINING MM AND RTMM 


#delimit ;				
		scatter mob_mm mob_mm_ub mob_mm_lb lpoly_pdur_mm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Th i i i ) msize(medsmall) mlwidth(medthick) mfcolor(gs4*0.2) mlcolor(gs4*0.35) lcolor(gs4*0.2 gs4*0.4 gs4*0.4 gs4*0.2)  xlab(1(1)12)   ylab(0.1(0.1)0.7) ysc(r(0.1 0.7)) 
		|| scatter mob_mm mob_mm_ub mob_mm_lb lpoly_pdur_mm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Th i i i ) msize(medsmall) mlwidth(medthick) mfcolor(purple*0.2) mlcolor(purple*0.35) lcolor(purple*0.15 purple*0.25 purple*0.25 purple*0.15)  xlab(1(1)12)   ylab(0.1(0.1)0.7) ysc(r(0.1 0.7)) 
		|| scatter lpoly_pdur_mm_corr u_duration if u_duration<=12, connect(l) lwidth(thick) msymbol(Th) msize(medlarge) mlwidth(thick)
		 mlwidth(medthick ) lcolor(purple) mcolor(purple)  xlab(1(1)12)   ylab(0.1(0.1)0.7) ysc(r(0.1 0.7)) graphregion(color(white))  /* lcolor("80 0 80") mcolor("80 0 80")  */
		||scatter mob_rtmm mob_rtmm_ub mob_rtmm_lb lpoly_pdur_rtmm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Sh i i i) msize(medium) mlwidth(medthick) mfcolor(gold*0.3) mlcolor(gold*0.4) lcolor(gold*0.25 gold*0.5 gold*0.5 gold*0.25)  xlab(1(1)12)   ylab(0.1(0.1)0.5) ysc(r(0.1 0.5)) 
		|| scatter lpoly_pdur_rtmm_corr u_duration if u_duration<=12, connect(l) lwidth(vthick) mlwidth(thick)
		 msymbol(Sh ) msize(medlarge)  lcolor(gold) mcolor(gold)  xlab(1(1)12, labsize(large))   ylab(0.1(0.1)0.7, labsize(large)) ysc(r(0.1 0.7)) graphregion(color(white))   /* lcolor(black) mcolor(black)  */

		xtitle("Time in Unemployment (months)", size(large)) ytitle("Proportion Moving Occupation" "Upon Exit U" , height(15) size(large))
		/* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("All workers") */
		legend(on width(150)  order(- "{bf:{&Gamma}}-{bf:corrected}" - "{bf:Uncorrected}" 9 2 14  4) rows(3) label(9 "Major Occupation Groups") label(14 "4-cat. NRMC (NR/Rout x Man/Cogn) ") 
		label(2 " 95% CI") label(4 "Smoothed ") symxsize(*0.5) ring(0) position(6) size(*1.2))  
		;
#delimit cr

		graph export "${mainresultsdir}/fig1a.pdf", as(pdf) replace

		
	





/*
#delimit ;				
		scatter lpoly_pdur_mm_yng_corr lpoly_pdur_mm_prm_corr u_duration if u_duration<=12,
		connect( l l ) lwidth(thick thick ) lpattern(solid dash ) msymbol(i i ) lcolor(navy cranberry)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter mob_mm_yng mob_mm_prm u_duration if u_duration<=12,
		 msymbol(Dh Oh ) mlwidth(medthick medthick) mcolor(navy*0.3 cranberry*0.3 )  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white))
		xtitle("Time in Unemployment (months) ") /* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) */
		ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		subtitle("Young and Prime-Aged") legend(on width(150) size(small) order( 1 2 3 4) rows(2) label(1 "Young ({&Gamma}-corrected, smoothed)") label(2 "Prime-aged ({&Gamma}-corrected, sm)") 
		label(3 "Young (uncorrected, unsmoothed)") label(4 "Prime-aged (uncorrected, unsm.)") )  
		;
#delimit cr
		graph export "${tempdata}/main_duration2_age_mm_v10.pdf", as(pdf) replace
*/

	
		capture drop mob_mm_yng_ub_adj
		gen mob_mm_yng_ub_adj = mob_mm_yng_ub  if mob_mm_yng_ub <0.72

		
		#delimit ;				
		scatter mob_mm_yng  mob_mm_yng_ub_adj mob_mm_yng_lb lpoly_pdur_mm_yng  u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Dh i i i ) msize(medsmall) mlwidth(thick) mfcolor(midgreen*0.3) mlcolor(midgreen*0.3) lcolor(midgreen*0.1 midgreen*0.35 midgreen*0.35 midgreen*0.1)  xlab(1(1)12)   ylab(0.3(0.1)0.7) ysc(r(0.3 0.7)) 
		|| scatter mob_mm_prm  mob_mm_prm_ub mob_mm_prm_lb lpoly_pdur_mm_prm  u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Oh i i i ) msize(medium) mlwidth(dthick) mfcolor(dknavy*0.3) mlcolor(dknavy*0.3) lcolor(dknavy*0.1 dknavy*0.3 dknavy*0.3 dknavy*0.1)  xlab(1(1)12)   ylab(0.3(0.1)0.7) ysc(r(0.3 0.7)) 
		|| scatter mob_mm_yng mob_mm_prm u_duration if u_duration<=12, msize(medsmall medsmall)
		 msymbol(Dh Oh ) mlwidth(thick thick) mcolor(midgreen*0.3 dknavy*0.3 )  mfcolor(midgreen*0.3 dknavy*0.3 )   xlab(1(1)12, labsize(large))   ylab(0.1(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white))
		
		|| scatter lpoly_pdur_mm_yng_corr lpoly_pdur_mm_prm_corr u_duration if u_duration<=12,
		connect( l l ) lwidth(thick vthick ) lpattern(solid solid) msymbol(Dh Oh ) mlwidth(thick thick) lcolor(midgreen dknavy )  msize(medlarge large) mcolor(midgreen dknavy )  xlab(1(1)12)   ylab(0.1(0.1)0.7, labsize(large)) ysc(r(0.1 0.7)) 
		
		xtitle("Time in Unemployment (months)", size(large) ) /* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) */
		/* ytitle("Prop. Moving Occ. upon Exit U", height(9) size(large))*/
		legend(on width(150) order(- "{bf:{&Gamma}-corrected}" - "{bf:Uncorrected}" 11 9 12 10) rows(3) label(11 "Young (20-30yo)") label(12 "Prime-aged (35-55yo)") 
		label(9 "Young (raw data)") label(10 "Prime-aged (raw data)") ring(0) position(6) symxsize(*0.5) size(*1.2))  
		/*legend(on width(150) size(small) order(- "--{&Gamma}-corrected--" - "--Uncorrected--" 9 2 14  4) rows(3) label(9 "Major Occupation Groups (2000 Clsf)") label(14 "4-cat. NR/Rout Cogn/Man ") 
		label(2 " 95% CI") label(4 "Smoothed ") )  */
		;
#delimit cr
		graph export "${mainresultsdir}/fig1b.pdf", as(pdf) replace
		
/*
#delimit ;				
		scatter mob_mm mob_mm_ub mob_mm_lb lpoly_pdur_mm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(T i i i ) msize(medsmall) mlwidth(medthick) mfcolor(gs4*0.2) mlcolor(gs4*0.35) lcolor(gs4*0.2 gs4*0.4 gs4*0.4 gs4*0.2)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter mob_mm mob_mm_ub mob_mm_lb lpoly_pdur_mm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(T i i i ) msize(medsmall) mlwidth(medthick) mfcolor(purple*0.2) mlcolor(purple*0.35) lcolor(purple*0.15 purple*0.25 purple*0.25 purple*0.15)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter lpoly_pdur_mm_corr u_duration if u_duration<=12, connect(l) lwidth(thick) msymbol(T) msize(medsmall) 
		 mlwidth(medthick ) lcolor(purple) mcolor(purple)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white))  /* lcolor("80 0 80") mcolor("80 0 80")  */
		||scatter mob_rtmm mob_rtmm_ub mob_rtmm_lb lpoly_pdur_rtmm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(S i i i) msize(medsmall) mlwidth(medthick) mfcolor(gold*0.3) mlcolor(gold*0.4) lcolor(gold*0.25 gold*0.5 gold*0.5 gold*0.25)  xlab(1(1)12)   ylab(0.1(0.1)0.5) ysc(r(0.1 0.5)) 
		|| scatter lpoly_pdur_rtmm_corr u_duration if u_duration<=12, connect(l) lwidth(thick)
		 msymbol(S ) msize(medsmall) mlwidth(medthick ) lcolor(gold) mcolor(gold)  xlab(1(1)12)   ylab(0.1(0.1)0.7) ysc(r(0.1 0.7)) graphregion(color(white))   /* lcolor(black) mcolor(black)  */

		xtitle("Time in Unemployment (months) (months)") ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		/* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("All workers") */
		legend(on width(150) size(small) order(- "--{&Gamma}-corrected--" - "--Uncorrected--" 9 2 14  4) rows(3) label(9 "Major Occupation Groups (2000 Clsf)") label(14 "4-cat. NR/Rout Cogn/Man ") 
		label(2 " 95% CI") label(4 "Smoothed ") )  
		;
#delimit cr

		graph export "${tempdata}/main_duration2_rtmm_mm_v10_2.pdf", as(pdf) replace		
		
		
		
		
  ** FOR APPENDIX 

capture drop mob_mm_yng_ub_adj
gen mob_mm_yng_ub_adj=mob_mm_yng_ub if mob_mm_yng_ub<=0.8		
		#delimit ;	
		scatter mob_mm_yng mob_mm_yng_ub_adj mob_mm_yng_lb lpoly_pdur_mm_yng u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Th i i i) mlwidth(medthick) mlcolor(navy*0.4) lcolor(navy*0.3 navy*0.3 navy*0.3 navy*0.3)  xlab(1(1)12)   ylab(0.2(0.1)0.8) ysc(r(0.2 0.8)) ||
		scatter lpoly_pdur_mm_yng_corr u_duration if u_duration<=12,
		connect( l l ) lwidth(thick thick ) lpattern(solid dash ) msymbol(i i ) lcolor(navy cranberry)  xlab(1(1)12)   ylab(0.2(0.1)0.8) ysc(r(0.2 0.8)) graphregion(color(white)) 
		xtitle("Time in Unemployment (months) ") /* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("Young and Prime-Aged") */
		ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		 legend(on width(150) size(small) order( 1 5 2  4) rows(2) label(5 "Young ({&Gamma}-corrected, smoothed)") label(2 "95% CI (raw data)") 
		label(1 "Young (raw data)") label(4 "Young (uncorrected, smoothed)") )  
		;
#delimit cr
		graph export "${tempdata}/main_duration2_age_yng_v10.pdf", as(pdf) replace
		
		
capture drop mob_mm_prm_ub_adj
gen mob_mm_prm_ub_adj=mob_mm_prm_ub if mob_mm_prm_ub<=0.8		
		#delimit ;	
		scatter mob_mm_prm mob_mm_prm_ub_adj mob_mm_prm_lb lpoly_pdur_mm_prm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Th i i i) mlwidth(medthick) mlcolor(cranberry*0.4) lcolor(cranberry*0.3 cranberry*0.3 cranberry*0.3 cranberry*0.3)  xlab(1(1)12)   ylab(0.2(0.1)0.8) ysc(r(0.2 0.8)) ||
		scatter lpoly_pdur_mm_prm_corr u_duration if u_duration<=12,
		connect( l l ) lwidth(thick thick ) lpattern(solid dash ) msymbol(i i ) lcolor(cranberry cranberry)  xlab(1(1)12)   ylab(0.2(0.1)0.8) ysc(r(0.2 0.8)) graphregion(color(white)) 
		xtitle("Time in Unemployment (months) ") /* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("Prime-aged and Prime-Aged") */
		ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		 legend(on width(150) size(small) order( 1 5 2  4) rows(2) label(5 "Prime-aged ({&Gamma}-corrected, smoothed)") label(2 "95% CI (raw data)") 
		label(1 "Prime-aged (raw data)") label(4 "Prime-aged (uncorrected, smoothed)") )  
		;
#delimit cr
		graph export "${tempdata}/main_duration2_age_prm_v10.pdf", as(pdf) replace
		

		
		
*******************************		
*** MOBILITY DURATION BY GENDER
*******************************




		#delimit ;				
		scatter mob_mm_fem mob_mm_fem_ub mob_mm_fem_lb lpoly_pdur_mm_fem u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(Th i i i) mlwidth(medthick) mlcolor(purple*0.4) lcolor(purple*0.3 purple*0.3 purple*0.3 purple*0.3)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter lpoly_pdur_mm_fem_corr u_duration if u_duration<=12, connect(l) lwidth(thick)
		 msymbol(i ) mlwidth(medthick ) lcolor("80 0 80") mcolor("80 0 80")  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white))
		xtitle("Time in Unemployment (months) ") ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		/* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("All workers") */
		legend(on width(150) size(small) order( 5 2 1  4) rows(2) label(5 "All ({&Gamma}-corrected, smoothed)") label(1 "All (uncorrected, unsmoothed)") 
		label(2 "All (uncorrected) 95% CI") label(4 "All (uncorrected, smoothed)") )  
		;
#delimit cr

*/
		
#delimit ;				
		scatter mob_mm_fem mob_mm_fem_ub mob_mm_fem_lb lpoly_pdur_mm_fem u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid solid solid solid) msymbol(Dh i i i) mlwidth(medthick) mlcolor(orange*0.6) lcolor(orange*0.2 orange*0.2 orange*0.2 orange*0.2)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) ||
		scatter mob_mm_male mob_mm_male_ub mob_mm_male_lb lpoly_pdur_mm_male u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash dash) msymbol(Th i i i) mlwidth(medthick) mlcolor(red*0.6) lcolor(red*0.2 red*0.2 red*0.2 red*0.2)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) ||
		scatter lpoly_pdur_mm_fem_corr lpoly_pdur_mm_male_corr u_duration if u_duration<=12,
		connect( l l ) lwidth(vthick vthick ) lpattern(solid dash ) msymbol(i i ) lcolor(orange red)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white)) ||
		scatter mob_mm_fem mob_mm_male u_duration if u_duration<=12,
		connect(i i) lwidth(none none) msymbol(Dh Th) mlwidth(medthick medthick) mlcolor(orange*0.6 red*0.6) xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7))
		xtitle("Time in Unemployment (months) ") /* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) 
		subtitle("Female and Male") */
		ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12)) legend(on width(150) size(small) order( 1 5 9 10) rows(2) label(1 "Female (raw data)") label(5 "Male (raw data)") 
		label(9 "Female ({&Gamma}-corrected, smoothed)") label(10 "Male ({&Gamma}-corrected, sm.)") )  
		;
#delimit cr
		graph export "${outputdata}/fig1_gender.pdf", as(pdf) replace		
		
	
/*
*/		
  

		
#delimit ;				
		scatter mob_mm_hsg mob_mm_hsg_ub mob_mm_hsg_lb lpoly_pdur_mm_hsg u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid solid solid solid) msymbol(Dh i i i) mlwidth(medthick) mlcolor(orange*0.4) lcolor(orange*0.2 orange*0.2 orange*0.2 orange*0.2)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) ||
		scatter mob_mm_col mob_mm_col_ub mob_mm_col_lb lpoly_pdur_mm_col u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash dash) msymbol(Th i i i) mlwidth(medthick) mlcolor(red*0.4) lcolor(red*0.2 red*0.2 red*0.2 red*0.2)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) ||
		scatter lpoly_pdur_mm_hsg_corr lpoly_pdur_mm_col_corr u_duration if u_duration<=12,
		connect( l l ) lwidth(vthick vthick ) lpattern(solid dash ) msymbol(i i ) lcolor(orange red)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white)) ||
		scatter mob_mm_hsg mob_mm_col u_duration if u_duration<=12,
		connect(i i) lwidth(none none) msymbol(Dh Th) mlwidth(medthick medthick) mlcolor(orange*0.6 red*0.6) xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7))
		xtitle("Time in Unemployment (months) ") /* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) 
		subtitle("Female and Male") */
		ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12)) legend(on width(150) size(small) order( 1 5 9 10) rows(2) label(1 "HS grad (raw data)") label(5 "Col grad (raw data)") 
		label(9 "HS grad ({&Gamma}-corrected, smoothed)") label(10 "Col Grad ({&Gamma}-corrected, sm.)") )  
		;
#delimit cr
		graph export "${outputdata}/fig1_educ.pdf", as(pdf) replace		
		  
  
********************************		
*** NUN etc NONEMPLOYMENT, MALES 
********************************



#delimit ;				
		scatter mob_mm_nunmale mob_mm_nunmale_ub mob_mm_nunmale_lb lpoly_pdur_mm_nunmale u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(T i i i ) msize(medsmall) mlwidth(medthick) mfcolor(gs4*0.2) mlcolor(gs4*0.35) lcolor(gs4*0.2 gs4*0.4 gs4*0.4 gs4*0.2)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter mob_mm_nunmale mob_mm_nunmale_ub mob_mm_nunmale_lb lpoly_pdur_mm_nunmale u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(T i i i ) msize(medsmall) mlwidth(medthick) mfcolor(purple*0.2) mlcolor(purple*0.35) lcolor(purple*0.15 purple*0.25 purple*0.25 purple*0.15)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter lpoly_pdur_mm_nunmale_corr u_duration if u_duration<=12, connect(l) lwidth(thick) msymbol(T) msize(medlarge) 
		 mlwidth(medthick ) lcolor(purple) mcolor(purple)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white))  /* lcolor("80 0 80") mcolor("80 0 80")  */
		||scatter mob_rtmm mob_rtmm_ub mob_rtmm_lb lpoly_pdur_rtmm u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(S i i i) msize(medium) mlwidth(medthick) mfcolor(gold*0.3) mlcolor(gold*0.4) lcolor(gold*0.25 gold*0.5 gold*0.5 gold*0.25)  xlab(1(1)12)   ylab(0.1(0.1)0.5) ysc(r(0.1 0.5)) 
		|| scatter lpoly_pdur_rtmm_corr u_duration if u_duration<=12, connect(l) lwidth(vthick)
		 msymbol(S ) msize(medlarge) mlwidth(medthick ) lcolor(gold) mcolor(gold)  xlab(1(1)12)   ylab(0.1(0.1)0.7) ysc(r(0.1 0.7)) graphregion(color(white))   /* lcolor(black) mcolor(black)  */

		xtitle("Time in Unemployment (months) (months)") ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		/* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) subtitle("All workers") */
		legend(on width(150)  order(- "{&Gamma}-corrected" - "Uncorrected" 9 2 14  4) rows(3) label(9 "Major Occupation Groups (2000 Clsf)") label(14 "4-cat. NRMC (NR/Rout Man/Cogn) ") 
		label(2 " 95% CI") label(4 "Smoothed ") )  
		;
#delimit cr

		graph export "${outputdata}/fig1a_nun_male.pdf", as(pdf) replace

	

		capture drop mob_mm_nunmyng_ub_adj
		gen mob_mm_nunmyng_ub_adj = mob_mm_nunmyng_ub  if mob_mm_nunmyng_ub <0.72
		
		
		
#delimit ;				
		scatter mob_mm_nunmyng  mob_mm_nunmyng_ub_adj mob_mm_nunmyng_lb lpoly_pdur_mm_nunmyng  u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(D i i i ) msize(medsmall) mlwidth(medthick) mfcolor(midgreen*0.1) mlcolor(midgreen*0.1) lcolor(midgreen*0.1 midgreen*0.25 midgreen*0.25 midgreen*0.1)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter mob_mm_nunmprm  mob_mm_nunmprm_ub mob_mm_nunmprm_lb lpoly_pdur_mm_nunmprm  u_duration if u_duration<=12,
		connect(i l l l) lwidth(none thin thin thick) lpattern(solid dash dash longdash) msymbol(O i i i ) msize(medsmall) mlwidth(medthick) mfcolor(dknavy*0.1) mlcolor(dknavy*0.1) lcolor(dknavy*0.1 dknavy*0.2 dknavy*0.2 dknavy*0.1)  xlab(1(1)12)   ylab(0.2(0.1)0.7) ysc(r(0.2 0.7)) 
		|| scatter mob_mm_nunmyng mob_mm_nunmprm u_duration if u_duration<=12, msize(medsmall medsmall)
		 msymbol(D O ) mlwidth(medthick medthick) mcolor(midgreen*0.3 dknavy*0.3 )  mfcolor(midgreen*0.3 dknavy*0.3 )   xlab(1(1)12)   ylab(0.1(0.1)0.7) ysc(r(0.2 0.7)) graphregion(color(white))
		
		|| scatter lpoly_pdur_mm_nunmyng_corr lpoly_pdur_mm_nunmprm_corr u_duration if u_duration<=12,
		connect( l l ) lwidth(thick vthick ) lpattern(solid solid) msymbol(D O ) lcolor(midgreen dknavy )  msize(medlarge large) mcolor(midgreen dknavy )  xlab(1(1)12)   ylab(0.1(0.1)0.7) ysc(r(0.1 0.7)) 
		
		xtitle("Time in Unemployment (months) ") /* title("Occupational Mobility - Unemployment Duration Profile ", size(medlarge) ) */
		ytitle("Proportion Moving Occupation" "Upon Exit U" , height(12))
		legend(on width(150) order(- "{&Gamma}-corrected" - "Code-error Uncorrected" 11 9 12 10) rows(3) label(11 "Young (20-30yo)") label(12 "Prime-aged (35-55yo)") 
		label(9 "Young (raw data)") label(10 "Prime-aged (raw data)") )  
		/*legend(on width(150) size(small) order(- "--{&Gamma}-corrected--" - "--Uncorrected--" 9 2 14  4) rows(3) label(9 "Major Occupation Groups (2000 Clsf)") label(14 "4-cat. NR/Rout Cogn/Man ") 
		label(2 " 95% CI") label(4 "Smoothed ") )  */
		;
#delimit cr
		graph export "${outputdata}/fig1b_nun_male.pdf", as(pdf) replace
	
	
	
	
	
	
	
	
******************************************
*** SAVING THE DATA SETS (AND REPORT FOR CALIBRATION)
******************************************

save "${tempdata}/durationprofiletemp2.dta", replace





** U SPELLS
cap use "${tempdata}/durationprofiletemp2.dta"

ren mob_mm* occmob_mog_raw*
ren mobcorr_mm* occmob_mog_corr*

keep u_duration occmob_mog_raw occmob_mog_raw_lb occmob_mog_raw_ub occmob_mog_corr ///
 occmob_mog_raw_yng occmob_mog_raw_yng_lb occmob_mog_raw_yng_ub occmob_mog_corr_yng ///
 occmob_mog_raw_prm occmob_mog_raw_prm_lb occmob_mog_raw_prm_ub occmob_mog_corr_prm ///
 lows_pdur_mm lows_pdur_mm_corr lows_pdur_mm_yng lows_pdur_mm_yng_corr lows_pdur_mm_prm lows_pdur_mm_prm_corr ///
 lpoly_pdur_mm lpoly_pdur_mm_corr lpoly_pdur_mm_yng lpoly_pdur_mm_yng_corr lpoly_pdur_mm_prm lpoly_pdur_mm_prm_corr

save "${outputdata}/durationprofiles_mog.dta", replace
*export excel using "${mainresultsdir}/durationprofiles_mog.xls", firstrow(variables) replace
export delimited using "${mainresultsdir}/durationprofiles_mog", replace 

** NUN SPELLS
use "${tempdata}/durationprofiletemp2.dta", clear



ren u_duration n_duration
ren mob_mm_nunmale* occmob_mog_raw*
ren mobcorr_mm_nunmale* occmob_mog_corr*
ren mob_mm_nunmyng* occmob_mog_raw_yng*
ren mobcorr_mm_nunmyng* occmob_mog_corr_yng*
ren mob_mm_nunmprm* occmob_mog_raw_prm*
ren mobcorr_mm_nunmprm* occmob_mog_corr_prm*


keep n_duration occmob_mog_raw occmob_mog_raw_lb occmob_mog_raw_ub occmob_mog_corr ///
occmob_mog_raw_yng occmob_mog_raw_yng_lb occmob_mog_raw_yng_ub occmob_mog_corr_yng ///
occmob_mog_raw_prm occmob_mog_raw_prm_lb occmob_mog_raw_prm_ub occmob_mog_corr_prm ///
lows_pdur_mm_nunmale lows_pdur_mm_nunmale_corr lows_pdur_mm_nunmyng lows_pdur_mm_nunmyng_corr lows_pdur_mm_nunmprm lows_pdur_mm_nunmprm_corr ///
lpoly_pdur_mm_nunmale lpoly_pdur_mm_nunmale_corr lpoly_pdur_mm_nunmyng lpoly_pdur_mm_nunmyng_corr lpoly_pdur_mm_nunmprm lpoly_pdur_mm_nunmprm_corr


save "${outputdata}/durationprofiles_mog_nun_males.dta", replace
export delimited using "${mainresultsdir}/durationprofiles_mog_nun_males.csv", replace 


** NUN SPELLS, GOOD AND BAD TIMES


use "${tempdata}/durationprofiletemp2.dta", clear


ren u_duration n_duration
ren mob_mm_nunmale* occmob_mog_raw*
ren mobcorr_mm_nunmale* occmob_mog_corr*
ren mob_mm_nunmgood* occmob_mog_raw_good*
ren mobcorr_mm_nunmgood* occmob_mog_corr_good*
ren mob_mm_nunmbad* occmob_mog_raw_bad*
ren mobcorr_mm_nunmbad* occmob_mog_corr_bad*


keep n_duration occmob_mog_raw occmob_mog_raw_lb occmob_mog_raw_ub occmob_mog_corr ///
occmob_mog_raw_good occmob_mog_raw_good_lb occmob_mog_raw_good_ub occmob_mog_corr_good ///
occmob_mog_raw_bad occmob_mog_raw_bad_lb occmob_mog_raw_bad_ub occmob_mog_corr_bad


save "${outputdata}/cycldurationshift_mog_nun_males.dta", replace
export delimited using "${mainresultsdir}/cycldurationprofiles_mog_nun_males.csv", replace 

********************************************************************************
	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"