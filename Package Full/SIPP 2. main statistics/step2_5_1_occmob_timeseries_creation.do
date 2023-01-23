	
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

THIS CREATES THE TIMESERIES OF THE OCCUPATIONAL MOBILITY
	both Gamma-corrected, as well as uncorrected


PART 1: Quarterly Timeseries, in timeseries_mm.xls
 
 -	 MM 		: 2000 SOC Occupational Mobility of the Unemployed p. Quarter
 -	 MM-NUM		: 2000 SOC Occ. Mobility across NUN-spells p. Quarter
 -   MM_YNG, MM_PRM: 2000 SOC Occ. Mob. Young Unemployed (20-30yo), Prime-Aged
						(35-55 yo)

PArt 2: Occupational mobility by Quarter x Duration cell, in timeseriesdur_mm.xls
	(This is a very timeconsuming calculation)
 
 - 	MM			: 2000 SOC Occ. Mob of the Unemployed p. Quarter x U. Duration
 
*/

********************************************
** PRELIMINARIES
********************************************

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



use "${outputdata}/reduced_u_n_ctv.dta"
cd "${tempdata}"

global locfileversion=1222





** auxiliary programs
do "${step2codedir}/aux_directory/aux_programs.do"
do "${step1codedir}/Ginv_matrices.do"


** define age dummies
capture drop age_2dum
gen byte age_2dum=1 if tage>=20 & tage<=30
replace age_2dum=2 if tage>=35 & tage<=55



***********************************************
** CYCLICAL TIME SERIES PROGRAM
***********************************************

** reports time series: flow proportion, corrected flow proportion
**	 with observation numbers (for both cases)
	

 
 

	
	
		cap program drop ts_propocc_agg_exe
		program define ts_propocc_agg_exe
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
		
			if "$ts_profilesheetname"==""{
									global profilesheetname "ts_profile"
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

		
		
		
		version 13
		ci `occmov_ind' [w=pweight2] if (`occmov_ind'!=.) & `if' $wavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'

		
		putexcel B1=("observations")
		putexcel C1=("occ. mobility")
		putexcel D1=("job finding rate")
		putexcel E1=("obs. jf rate")
		
			display "filling in basic stats"
		putexcel A`rowpos'=(" `name_infix' ")
		putexcel B`rowpos'=(r(N))
		putexcel C`rowpos'=(r(mean))

		*ci lne [w=pweight2] if `if' $jfwavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'
		*putexcel D`rowpos'=(r(mean))
		*putexcel E`rowpos'=(r(N))


		****** SET UP THE TIME SERIES 
		
		global sheetname "$ts_profilesheetname"

		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

		
		forvalues jj=1(1)4{
		
			local addno=(`jj'-1)*4
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("q`jj'_mob_`name_infix'")

			local addno=((`jj'-1)*4)+1
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("q`jj'_mobcorr_`name_infix'")

			local addno=((`jj'-1)*4)+2
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("q`jj'_obs_mob_`name_infix'")

			local addno=((`jj'-1)*4)+3
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("q`jj'_obs_mobcorr_`name_infix'")
		}
			
			
		
			
		/*	
		global sheetname "occ_spec"
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

		excel_col_exe  ${colposcounter_occspec}
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'")
		
		excel_col_exe  ${colposcounter_occspec}+1
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'_corr")
		*/

		
		forvalues h=93(1)226{

		
				display "========================================================================="
				display "========================================================================="
				display ""
				display " QUARTER `h'"
				display ""
				display "========================================================================="
				display "========================================================================="
				
			local rowpos2=`h'-91   //`dur'+1
			
			forvalues jj=1(2)3 {
			global ts_cond "& quarter>=`h'-(`jj'-1) & quarter<=`h'+(`jj'-1)"
			
			
									* cumulative profile 
									display "PROP OCC/NOC STOCK with DURATION=`dur'"
												count if `occmov_ind'!=. $ts_cond & sample_timetogo>$sttg & `if'  $locwavecond & n_spellength>=1  & n_spellength<=18 `occ_excluded'
												if r(N)>0 {	
												
												** -- RAW DATA
										
												/*
												svy: mean `occmov_ind' if (`occmov_ind'!=.) $ts_cond & sample_timetogo>$sttg & `if' $locwavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'
												matrix mean1=r(table)
												local l_n_pop=e(N_pop)
														
													global sheetname "$ts_profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}+(`j'-1)*4
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+(`j'-1)*4 +2
																	local coltemp=r(xlscol)
																	local xlsoutput=`l_n_pop'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
												*/
																	
												
												** -- CORRECTED DATA
												
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. $ts_cond & sample_timetogo>$sttg & `if'  ///
																							$locwavecond & n_spellength>=1  & n_spellength<=18 , matcell(durmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix durmat=J(`rowno_overall', `colno_overall',0)
													
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $ts_cond $locwavecond  & n_spellength>=1 & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) $ts_cond & sample_timetogo>$sttg & `if' $locwavecond  & n_spellength>=1 & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix durmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $locwavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING TRANSITION MATRIX "
												matrix list durmat
												
												**** uncorrected matrix, for occupation-spec mobility rates
												
												xsnet_calcx durmat agg_matrix_loc'
												local popcount=r(tsum_all)
												local mobrate=r(tmobrate)
													global sheetname "$ts_profilesheetname"
													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	
																	local addno=((`jj'-1)*4)
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`mobrate'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	local addno=((`jj'-1)*4)+2
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`popcount'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
													
													
												*** corrected matrix
												
												matrix corr_agg_matrix=`corr_matrix'*agg_matrix_loc'
												xsnet_calcx durmat corr_agg_matrix
								
												local popcount=r(tsum_all)
												local mobrate=r(tmobrate)
												
												
																	local addno=((`jj'-1)*4)+1
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`mobrate'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	local addno=((`jj'-1)*4)+3
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`popcount'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
													
												
												}
												}

													
		
		}
		
		global colposcounter=${colposcounter}+20
		
		end // program to calculate the nonemployment statistics (or basically any statistics!!! gender, source occupation, by reason of separation etc. )



//==========================================================
//  CALCULATE THE QUARTERLY MOBILITY RATES 
//
		

	capture n preserve

	** keep only what is necessary
	keep locc* lind* lne* lue* sample_timetogo entry_ind wave rot interview_no personkey yearmonth quarter u_spellength n_spellength complete* sex ms educ2 race tage age* pweight2 strat hh_ids93 hsc age_2dum		
			

global sttg = "3"
global epanbwidth=3

*global wavecond " & wave>4 & interview_no>14 & sample_timetogo>=1 " // original
*global mwavecond " & wave>1 & interview_no>4 & sample_timetogo>=1 " // original
global wavecond " & wave>4 & interview_no>14 & sample_timetogo>=1 & n_spellength<=14 " // adapted, 2022
global mwavecond " & wave>1 & interview_no>4 & sample_timetogo>=1 & n_spellength<=14 " // adapted, 2022

	* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
global jfwavecond "& wave>1 & interview_no>4 & sample_timetogo>18-n_spellength "	// this 
global sttg "1"
global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "


			
			
			global colposcounter=2
			global colposcounter_occspec=3
			global filename "${outputdata}/timeseries_mm"
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

		global ts_profilesheetname "ts"
		cap n putexcel set "${filename}.xls", sheet("${ts_profilesheetname}")
		cap n putexcel set "${filename}.xls", modify sheet("${ts_profilesheetname}")

		

		putexcel A1=("quarter")
		forvalues cc=93(1)226 {
		local rowcc=`cc'-91
		putexcel A`rowcc'=("`cc'")
		}

			
			
			* basic 

			
			** occupation selection matrix
			matrix mm_noagric_matrix=I(22)
			matrix mm_noagric_matrix[18,18]=0  // no agriculture
			
			matrix mm_noagmgt_matrix=I(22)
			matrix mm_noagmgt_matrix[18,18]=0  // no agriculture
			matrix mm_noagmgt_matrix[1,1]=0  // no agriculture
			
			*cap putexcel set "durationprofiles_${fileversion}.xls", sheet("main", replace)
			*cap n putexcel set "durationprofiles_${fileversion}.xls", modify sheet("main", replace)
			
			
		******* MAIN 	
			global locwavecond "${wavecond}"
			ts_propocc_agg_exe mm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 2 $colposcounter mm_noagric_matrix mm_noagric_matrix if complete_uspell==1 & entry_ind==1 
			
		******* NUN
			global locwavecond "${wavecond}"
			ts_propocc_agg_exe mm_nun lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 3 $colposcounter mm_noagric_matrix mm_noagric_matrix if complete_nunspell==1 & entry_ind==1 
		
		****** MM-YNG
			global locwavecond "${wavecond}"
			ts_propocc_agg_exe mmmy lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 4 $colposcounter mm_noagric_matrix mm_noagric_matrix  if complete_uspell==1 & entry_ind==1 & age_2dum==1
	
		****** MM-PRM
			global locwavecond "${wavecond}"
			ts_propocc_agg_exe mmmp lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 5 $colposcounter mm_noagric_matrix mm_noagric_matrix if complete_uspell==1 & entry_ind==1 & age_2dum==2
			
		
		
			
			
			
		
*************************************************
** CYCLICAL TIME SERIES with duration
*************************************************



	
		cap program drop tsdur_propocc_agg_exe
		program define tsdur_propocc_agg_exe
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
		
			if "$ts_profilesheetname"==""{
									global profilesheetname "ts_profile"
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


		****** SET UP THE TIME SERIES 
		
		global sheetname "$ts_profilesheetname"

		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")
		*cap n putexcel set "${filename}.xls", modify sheet("${sheetname}", modify)

		
		forvalues jj=1(1)4{
		
			local addno=(`jj'-1)*4
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("qn`jj'_mob_`name_infix'")

			local addno=((`jj'-1)*4)+1
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("qn`jj'_mobcorr_`name_infix'")

			local addno=((`jj'-1)*4)+2
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("qn`jj'_obs_mob_`name_infix'")

			local addno=((`jj'-1)*4)+3
			local tempnumx=${colposcounter} + `addno'
			excel_col_exe  `tempnumx'
			local coltemp=r(xlscol)
			putexcel `coltemp'1=("qn`jj'_obs_mobcorr_`name_infix'")
		}
			
			
		
			
		/*	
		global sheetname "occ_spec"
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

		excel_col_exe  ${colposcounter_occspec}
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'")
		
		excel_col_exe  ${colposcounter_occspec}+1
		local coltemp=r(xlscol)
		putexcel `coltemp'1=("occspec_dur_`name_infix'_corr")
		*/

		
		forvalues h=93(1)226{
		forvalues dur=1(1)18{

		
				display "========================================================================="
				display "========================================================================="
				display ""
				display " QUARTER `h'"
				display ""
				display "========================================================================="
				display "========================================================================="
				
			local rowpos2=(`h'-93)*18+1+`dur'   
			
			forvalues jj=1(2)3 {
			global ts_cond "& quarter>=`h'-(`jj'-1) & quarter<=`h'+(`jj'-1) & n_spellength==`dur'"
			
			
									* cumulative profile 
									display "PROP OCC/NOC STOCK with DURATION=`dur'"
												count if `occmov_ind'!=. $ts_cond & sample_timetogo>$sttg & `if'  $locwavecond & n_spellength>=1  & n_spellength<=18 `occ_excluded'
												if r(N)>0 {	
												
												** -- RAW DATA
										
												/*
												svy: mean `occmov_ind' if (`occmov_ind'!=.) $ts_cond & sample_timetogo>$sttg & `if' $locwavecond  & n_spellength>=1 & n_spellength<=18 `occ_excluded'
												matrix mean1=r(table)
												local l_n_pop=e(N_pop)
														
													global sheetname "$ts_profilesheetname"

													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	excel_col_exe  ${colposcounter}+(`j'-1)*4
																	local coltemp=r(xlscol)
																	local xlsoutput=mean1[1,1]
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	excel_col_exe  ${colposcounter}+(`j'-1)*4 +2
																	local coltemp=r(xlscol)
																	local xlsoutput=`l_n_pop'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
												*/
																	
												
												** -- CORRECTED DATA
												
												
												tab `source_occ' `dest_occ' [aw=pweight2] if `occmov_ind'!=. $ts_cond & sample_timetogo>$sttg & `if'  ///
																							$locwavecond & n_spellength>=1  & n_spellength<=18 , matcell(durmat)
												
												* test for confirmity of matrix 
												if ~(r(r)==`rowno_overall' & r(c)==`colno_overall' ) {
													
													matrix durmat=J(`rowno_overall', `colno_overall',0)
													
													forvalues i=1(1)`rowno_overall' {
													forvalues j=1(1)`colno_overall' {
													
													count if (`occmov_ind'!=.) & sample_timetogo>$sttg & `if' $ts_cond $locwavecond  & n_spellength>=1 & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1]
													
													if r(N)>0 {
													
													tab `source_occ' `dest_occ' [iw=pweight2] if (`occmov_ind'!=.) $ts_cond & sample_timetogo>$sttg & `if' $locwavecond  & n_spellength>=1 & n_spellength<=18 /// 
																	& `source_occ'==occmap[`i',1] & `dest_occ'==occmap[`j',1], matcell(tempmatpart)
													
													
													matrix durmat[`i',`j']=tempmatpart[1,1]
													}
													}
													}
													}
													
													
												*tab loccbefore loccafter [aw=pweight2] if loccbefore!=. & loccafter!=. & (lue_c_1tm!=.) & sample_timetogo>$sttg & complete_uspell==1 $locwavecond  & u_spellength>=`dur' & u_spellength<=16, matcell(durmat)
												display " RESULTING TRANSITION MATRIX "
												matrix list durmat
												
												**** uncorrected matrix, for occupation-spec mobility rates
												
												xsnet_calcx durmat agg_matrix_loc'
												local popcount=r(tsum_all)
												local mobrate=r(tmobrate)
													global sheetname "$ts_profilesheetname"
													cap n putexcel set "${filename}.xls", modify sheet("${sheetname}")

																	
																	local addno=((`jj'-1)*4)
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`mobrate'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	local addno=((`jj'-1)*4)+2
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`popcount'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
													
													
												*** corrected matrix
												
												matrix corr_agg_matrix=`corr_matrix'*agg_matrix_loc'
												xsnet_calcx durmat corr_agg_matrix
								
												local popcount=r(tsum_all)
												local mobrate=r(tmobrate)
												
												
																	local addno=((`jj'-1)*4)+1
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`mobrate'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')

																	local addno=((`jj'-1)*4)+3
																	local tempnumx=${colposcounter} + `addno'
																	excel_col_exe  `tempnumx'
																	local coltemp=r(xlscol)
																	local xlsoutput=`popcount'
																	putexcel `coltemp'`rowpos2'=(`xlsoutput')
													
												
												}
												}

													
		
		}
		}
		
		global colposcounter=${colposcounter}+20
		
		end // program to calculate the nonemployment statistics (or basically any statistics!!! gender, source occupation, by reason of separation etc. )
		
**********************************************************
****  CALCULATE  MOBILITY BY QUARTER X DURATION
**********************************************************		
*/
/*
capture n preserve

	** keep only what is necessary
	keep locc* lind* lne* lue* sample_timetogo entry_ind wave rot interview_no personkey yearmonth quarter u_spellength n_spellength complete* sex ms educ2 race tage age* pweight2 strat hh_ids93 hsc 		
*/			



** GLOBALS USED 
global sttg = "3"
global epanbwidth=3

global wavecond " & wave>4 & interview_no>14 & sample_timetogo>=1 " // original
global mwavecond " & wave>1 & interview_no>4 & sample_timetogo>=1 " // original

	* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
global jfwavecond "& wave>1 & interview_no>4 & sample_timetogo>18-n_spellength "	// this 
global sttg "1"
global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "




		global colposcounter=3
		global colposcounter_occspec=3
		global filename "${outputdata}/timeseriesdur_mm"
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

		global ts_profilesheetname "ts"
		cap n putexcel set "${filename}.xls", sheet("${ts_profilesheetname}")
		cap n putexcel set "${filename}.xls", modify sheet("${ts_profilesheetname}")

		putexcel A1=("quarter")
		putexcel B1=("n_spellength")
		local rowcc=2
		forvalues cc=93(1)226 {
		display "quarter `h'"
		forvalues dur=1(1)18 {
		putexcel A`rowcc'=("`cc'")
		putexcel B`rowcc'=("`dur'")
		local rowcc=`rowcc'+1
		}
		}

			
			
			* basic 

			
			** occupation selection matrix
			matrix mm_noagric_matrix=I(22)
			matrix mm_noagric_matrix[18,18]=0  // no agriculture
			
			matrix mm_noagmgt_matrix=I(22)
			matrix mm_noagmgt_matrix[18,18]=0  // no agriculture
			matrix mm_noagmgt_matrix[1,1]=0  // no agriculture
			
			*cap putexcel set "durationprofiles_${fileversion}.xls", sheet("main", replace)
			*cap n putexcel set "durationprofiles_${fileversion}.xls", modify sheet("main", replace)
			
			
		******* RUN PROGRAM	
			global locwavecond "${wavecond}"
			tsdur_propocc_agg_exe mm lne_c_mmo locc1bfr_mmo locc1aft_mmo Ginv_mm 2 $colposcounter mm_noagric_matrix mm_noagric_matrix if complete_uspell==1 & entry_ind==1 
			
			

********************************************************************************
	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"