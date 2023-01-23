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
version 13
clear 
clear matrix


quietly {

noisily: display "---------------running CTV data replication-----------"
noisily: display ""
noisily: display "set the *workingdir* global in global_paths.do if the master_ctv.do is not ran"
noisily: display "directly from the root directory of the replication package, and run global_paths.do"
noisily: display "before running master_ctv.do. If running from the root directory, can run"
noisily: display "master_ctv.do directly"
noisily: display ""
noisily: display " this incorporates computational results, for overall replication"
noisily: display " of both data and model results (combned), run Fortran programs first"
}


sleep 2000


cd "${workingdir}"
global workingdir `c(pwd)'



do "${workingdir}/global_paths.do"
	
	version 13
	
	set more off
	set varabbrev off

	
	global gstarttime=c(current_time)
	global gstartdate=c(current_date)
	display "started at ${gstarttime} on ${gstartdate}"

	
	


********************************************************************************
**
**						 0. Aggregate data Prep
**
********************************************************************************



** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)


	do "${workingdir}/Aggregate Data/aggdata_prep.do"




********************************************************************************
**
**						 I. CPS replication
**
********************************************************************************

	/* two things are done with CPS data: 
				1) two regressions that show occ reallocation is procyclical in the CPS as well
				2) occupational recoding based on the IPUMS cross-walk, this is used in the SIPP 
					below  */

					
** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)

do "${cpsdir}/cps_regressions_maintext.do"					
					

********************************************************************************
**
**							 II. SIPP 
**
********************************************************************************
	
	
	
		
	****************************************************************************
	*** STEP 0: download the raw SIPP data from the NBER, convert it in the ****
	*** 		files we use												****
	****************************************************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	
		do "${step0codedir}/step0_SIPP_NBER_read_in.do"
		
		
		
		
	****************************************************************************
	*** STEP 1: derive the miscoding matrix in the SIPP						****
	****************************************************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	
		do "${step1codedir}/step1_measurement_error.do"
		
		
		
*/		
		
	****************************************************************************
	** STEP 2. DERIVATION OF SIPP DATA STATISTICS USED IN CTV				****
	****************************************************************************

	/* This do-file runs the do-files associated with the pictures, tables and numbers in
	section 2, and produces the empirical data behind the pictures and tables in 
	sections 4 and 5. Some of these data are combined with model data, in section
	III. below. For a detailed overview of which do-file invoked here produces which 
	output, see the README in the ./Replication/SIPP 2. Main Statistics/ directory
	*/


	****************************************
	** 1. DURATION PROFILES
	****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	

	do "${step2codedir}/step2_1_durationprofiles.do"


			* this also produces the nun spells of online appendix,
			*   including the cyclical duration shift 
			* however, cyclical duration shift for the main calibration
			*   produced under 6. 



	***************************************
	** 2. NET, EXCESS & OCCUPATIONAL MOBILITY PER OCC 
	***************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	

	do "${step2codedir}/step2_2_netgrossmobility_occ_overall.do"



	*****************************************
	** 3. REPEAT MOBILITY
	*****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	
	do "${step2codedir}/step2_3_repeatmobility.do"



	***************************************
	** 4. SIPP REGRESSION TABLE (CYCLE), PART 1
	**************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	
	** columns (v) and (vii) of TABLE 1
	do "${step2codedir}/step2_4_cycl_regressions_uncorrected_unfiltered.do"


	***************************************
	** 5. CORRECTING THE MOBILITY TIME SERIES
	**        & SIPP REGRESSION TABLE 1, PART 2
	****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)

	
	*   5.1. QUARTERLY MOBILITY TIME SERIES
	*           & MOBILITY p. QUARTER x DURATION

	do "${step2codedir}/step2_5_1_occmob_timeseries_creation.do" // CHECK THE V3

		/* PRODUCES
			-timeseries_mm.xls   in outputdir
			-timeseriesdur_mm.xls   in outputdir */


	**** 5.2  OCCUPATIONAL MOBILITY TIME SERIES BEFORE TRAMO/FILTERING
	do "${step2codedir}/step2_5_2_occmob_timeseries_for_detrending.do"		

		/* PRODUCES
			-occmob_ts_for_tramo.xlsx 	in outputdir
			-occmob_ts_for_tramo.dta 	in outputdir 
			see after step2_9 for a description of the TRAMO procedure */

	**** 5.3  REGRESSION, column (iv) TABLE 1
	do "${step2codedir}/step2_5_3_cycl_regressions_corrected_unfiltered.do"






	***************************************
	** 6. CYCLICAL DURATION PROFILE SHIFT
	***************************************

	
** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)


	do "${step2codedir}/step2_6_cyclical_durationprofile_shift.do"

		/* PRODUCES
			-fig 3a
			-mobility duration profile good/bad times for calibration
			-data needed for data-part of fig 6    
			-NOTE: nun mobility duration profile good/bad times already
					calculated under 1. */




	****************************************
	** 7. NET MOBILITY OVER THE CYCLE AND IN THE CALIBRATION, OCCUPATIONAL EVOLUTION
	****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	
	do "${step2codedir}/step2_7_1_netmobility.do"


	do "${step2codedir}/step2_7_2_occsize_change.do"


	****************************************
	** 8. SURVIVAL PROFILES (+ HAZARDS) AND REMAINING TIME-AVERAGED CALIBRATION MOMENTS
	****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	

	do "${step2codedir}/step2_8_1_survivalprofiles.do"


	do "${step2codedir}/step2_8_2_time_averaged_calib_moments.do"

	***************************************
	*** 9. CREATION OF TIME SERIES U, JF, SEP
	***************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	

	****** 9.1 UNEMPLOYMENT / NUN 


	do "${step2codedir}/step2_9_1_unemp_timeseries_creation.do"  
				/* PRODUCES
				- FILE table2_mean_unemp.txt: mean unemp level calibration 
				- FILE table7_online_appx_mean_nunemp.txt: mean NUN/(NUN+E) rate, NUN-calibration
				- FILE u_ts_for_tramo.xlsx, optional, not part of main run
				- FILE ${outputdata}/u_hp_ts2020.dta
				- FILE ${outputdata}/timeseries_u_durwvcearly.dta 
																		*/
		

	****** 9.2 JOB FINDING RATES
	do "${step2codedir}/step2_9_2_jf_timeseries_creation.do"  
				

	****** 9.3 SEPARATION RATES
	do "${step2codedir}/step2_9_3_sep_timeseries_creation.do"  
				/* PRODUCES
				- FILE "${outputdata}/ts_sep_paper_for_tramo.xlsx
					containing the series ts_lsep_q and q3_lsep_q that, 
					after tramo, are used in step 11
				- FILES ${outputdata}/ts_sep_for_tramo.xlsx and ts_sep_for_tramo.dta
					contain a larger set of separation time series, including 
					across ages, that can also be Tramo-ed, if interested
																		*/

																		
																		
																		
																		
	********************************************************************************
	*** INTERMEZZO: TRAMO TO DEAL WITH MISSING OBSERVATIONS PRE-FILTERING
	***				TO GO FROM STEP 5+9, TO STEP 10+11, respectively
	***					(ALSO USED FOR THE FINAL CALCULATION IN STEP 12)
	***				WE USE TRAMO/SEATS
	********************************************************************************




	/* We have used the program TSW+ by Gianluca Caporello and Agustin Maravall 
	with programming help of D. Pérez Cañete and R. López Pavón, revision 941, 
	build 2015/10/14 12:07:23. Available as Freeware (for conditions etc. see
	program when installing),  included in this replication package, in the

	we use the standard seats/tramo in the tsw program , with automatic procedure parameter RSA=4
	then we save the TRAMO output with the appropriate name (see below in program) 	

	********************************************************************************
	** BY DEFAULT WE USE XLIN SERIES (that one is calendar adjusted, 
	plus other deterministic issues)
	** XINT is interpolated, but not adjusted for deterministic (seasonal) variation
	** (if XLIN compensates for a structural break, we prefer to stick to XINT)
	********************************************************************************

	Applying TRAMO/SEATS is done 'by hand' (i.e. there is no script that can be invoked by stata). 
	To not break the run of the stata program, we have added two kinds of reference time series, 
	one before tramo, one after tramo, which contain the time series as used in the paper, 
	(the after series, after we used TSW+ ourselves).
	After running the entire stata replication package, one can check the correctness 
	of the TRAMO procedure by 
		a. checking that the before-TRAMO reference series coincides with the series 
			produced by the replication package run
		b. use TSW+ to apply TRAMO/SEATS to the series produced by the STATA run, 
			and see that the series after TRAMO/SEATS coincides with the after series. 


	********************************************************************************
	** 
	**		HOW TO: TRAMO / SEATS USING TSW+  
	**
	********************************************************************************



	In brief, we use the standard in the tsw program , with automatic procedure parameter RSA=4
	then we save the TRAMO and SEATS output with the appropriate name (see below in program) 	

	TRAMO: we have three series:
		-xorig series: original input, with -99999 for missing values
		-xint: interpolated series, but original values when non-interpolated
		-xlin: clear of deterministic variation. 	

															
	** BY DEFAULT WE USE XLIN SERIES (that one is calendar adjusted, plus other deterministic issues)
	(XINT is interpolated, but not adjusted for deterministic (seasonal) variation; however, XLIN allows
	for clearing out structural breaks, which in some settings would not be desired)

															
	** SEATS (on top of TRAMO output) further decomposes the time series.
	 - trend-cycle series: smooth series that captures both trend and cycle, but outliers and SA corrected
		this series is relevant as the quarter-by-quarter (unsmoothed) mobility is quite noisy
	 
	 
	 
	*** HOW TO USE TSW+ *****

	1) Open/Run the program
	2) Click the 'Series' button, this opens the standard 'Open' dialog in Windows.
		 - Go to the outputdata directory, and select occmob_ts_for_tramo.xlsx, 
			jf_ts_for_tramo.xlsx, sep_ts_for_tramo.xlsx OR udistrprops_for_tramo.xlsx
		 - A window pops up, inquiring about the periodicity, Press OK at periodity 4.
		 - In the series list window, expand the series list. 
	3) In the series list, select the series to apply TRAMO/SEATS to, e.g. qn3_mob_12_mm
		 - On the right-hand side, next to the series window, verify that the following 
			options are set
				-- iter=0
				-- Seats/Tramo to Tramo/Seats
	4) Press the 'Model (+)' button. Verify the automatic procedure parameter is 
		set to (RSA=)4
	5) Press 'Run'
	6) Click button 'Out Tables', choose option 'Out Table Tramo'
		- this shows the Xorig, Xint, Xlin timeseries
	7) At the bottom, press the right-most Save Excel button, save the file as 
		[series name].xlsx, in our example qn3_mob_12_mm.xlsx.
		- It is important that the names are kept, as the first thing that STATA 
			will do 'post TRAMO' (not literally post, see comment below) is to 
			import the resulting series back into STATA
	8) Close the Out Table window, returning to the main program
	9) Click button 'Out Tables', choose option 'Out Table Seats'
	10) Click Save Excel button at the bottom, save file as 
		[series name]_seats.xlsx (in our example qn3_mob_12_mm_seats.xlsx)
		- close 'Seats' window, and move to the next series (i.e. return to step 3)
		- cycle through step 3 to step 10 for all series of interest.

	end) Copy all the series produced to the output (${outputdata} directory 
			(if not already saved there, by hand), where the input series were found. 
			By default TSW will suggest the .\TSW+\SAVED\ directory for saving files. 
			At the end of saving all files in the latter, one could copy-paste 
			these files into the ${outputdata} directory.
			This is not unimportant: below STATA will look for those files there. 
			However, see below about using the reference series to keep STATA running,
			and just verifying the TRAMO/SEATS at the very end of replication. 
			

	*****
	Inside a subdirectory of our step 2 code directory (${step2codedir}/Tramo_Seats_series_REFERENCE), we have saved the before and 
	after timeseries we are using in our paper. This means that, when replicating, STATA can run 
	uninterruptedly till the very end, and the TRAMO/SEATS step can be verified ex post
			- check that the before-tramo series produced in the replication overlap
			with the reference before-tramo series. 
			- do tramo/seats (via TSW+) on the series produced in replication, and 
			compare these to the after-tramo reference series. (If copying these
			series into the ${outputdata} directory, verify the timestamp 
			to make sure that the series previously there is overwritten!)
	To facilitate this comparison, this do-file, at the beginning, placed a copy of 
	these reference series in the ${outputdata} directory, so all series are in one
	location (though the after-tramo series were put there by hand).

			
			
	*/


	****************************************
	** 10. CYCLICAL OCCUPATIONAL MOBILITY
	****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	
		** post TRAMO data read in, see intermezzo/note between step 9 and 10

	do "${step2codedir}/step2_10_cycl_occupational_mobility.do"  // step3_1 renames

		/* produces 
				TABLE 1 columns (i)-(ii) 
				FILE "${outputdata}/occmob_ts_after_tramo.dta" used in step2_11_5	
																				*/
			


	****************************************
	** 11. CYCLICAL U (NUN), JF, SEP AND CYCLICAL CORRELATIONS, ETC. 
	****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	

		** post TRAMO data read in, see intermezzo/note between step 9 and 10,
		**		then filter to isolate cyclical component of time series
		**		save these

	do "${step2codedir}/step2_11_1_u_cycl_timeseries.do" 


		/* PRODUCES 
				u_hp_ts2020.dta 				in outputdir, used in step2_11_5
				timeseries_u_durwvcearly.dta" 	in outputdir */
	 
	 
	do "${step2codedir}/step2_11_2_jf_cycl_timeseries.do" 

		/* PRODUCES 
				jf_hp_ts2020.dta 				in outputdir, used in step2_11_5
																*/
	 

	do "${step2codedir}/step2_11_3_sep_cycl_timeseries.do" 

		/* PRODUCES 
				sep_hp_ts2020.dta 				in outputdir, used in step2_11_5
																*/
	 

	do "${step2codedir}/step2_11_4_nun_cycl_timeseries.do" 

		/* PRODUCES 
				nun_ts2020.dta 				in outputdir, used in step2_11_5
																			*/
	 
		****************************************************************************
		** calculate the cyclical statistics (st dev, cross correlations, autocorr)
		**		for the paper
		****************************************************************************
		
		
	do "${step2codedir}/step2_11_5_cyclical_statistics_for_paper.do" 


	****************************************
	** 12. U DURATION DISTRIBUTIONS AND CYCLICAL UNEMPLOYMENT DURATION RESPONSE
	****************************************


** LOG
capture log close _all
global logdate = string( d(`c(current_date)'), "%dCY-N-D" )
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", text name(globallog)
capture noisily log using "${tempdata}\ctv_masterlog_${logdate}.txt", append text name(globallog)
	
	
	do "${step2codedir}/step2_12_unemployment_durations.do" 


					

********************************************************************************
**
**				III. COMPUTATIONAL RESULTS PROCESSING
**
********************************************************************************
	
	

	****************************************
	** 1. DURATION PROFILES MODELS vs DATA
	****************************************

do "${sec45codedir}/compresults_1_durationprofiles.do"

	/*  OUTPUTS:
			FIGS 5a-f
			FIGURE 6
			APPENDIX FIGS 1a-c, 2a-c, 3a, 5a-f, 8a-b

			appxtable4_mobdurprofiles_xs_calibration.txt
			appxtable4_survprofile_xs_calibration.txt
			
			xtra_mobdurprofile_grossnet_calibration.txt
			xtra_survprofile_grossnet_calibration.txt
			xtra_survprofile_xsnoskilldep_calibration.txt
			xtra_mobdurprofiles_xsnoskilldep_calibration.txt 
			xtra_mobdurprofiles_xsnun_calibration.txt
			xtra_survprofile_xsnun_calibration.txt
			xtra_survprofile_noreall1_calibration.txt
			xtra_survprofile_noreall2_calibration.txt
	*/
	
	****************************************
	** 2. SEARCH ACROSS SUPEROCC BEHAVIOR, NET MOBILITY
	****************************************

do "${sec45codedir}/compresults_2_appxfig3b_cycl_netflow_inflow_shifts.do"

	/* OUTPUTS:
			appxtable2_model.txt
			APPENDIX FIGURE 3B
			TABLE6.XLSX
			table6_explanation.txt
	*/



	****************************************
	** 3. EXCESS MOB EMPL/UNEMPL DISTRIBUTION PICS; DECOMPOSITION UNEMPL, ACROSS MODELS
	****************************************

do "${sec45codedir}/compresults_3_distribution_figures_decomp.do"

	/* OUTPUTS:
			FIGURE 7A				// u distr heatmap
			APPENDIX FIGURE 7A		// u young distr heatmap
			APPENDIX FIGURE 7C		// u prime distr heatmap
			
			FIGURE 7B				// e distr heatmap
			APPENDIX FIGURE 7B		// e young distr heatmap
			APPENDIX FIGURE 7D		// e prime distr heatmap
			____________________________________________
			
			FIGURE 7A
			APPENDIX FIGURES  6A, 6B, 6C // type unempl decomposition, EXCESS MOB
			APPENDIX FIGURES  4A, 4B, 4C // decomposition GROSS/NET MOB MODEL 
			APPENDIX FIGURES  9A, 9B // decomposition NO REALLOCATION MODEL
	*/

	****************************************
	** 4. EMPL/UNEMPL DISTRIBUTION PICS, DECOMPOSITION, PART 1 (GROSS/NET MOB)
	****************************************

do "${sec45codedir}/compresults_4_distributionfigs_netmob.do"

	/* OUTPUTS:
			FIGURES 8A-8F  // u heatmaps p superocc, 
							// productivity and decomposition p superocc
	*/
	
	****************************************
	** 5. OTHER MODEL RESULTS COPIED TO RESULTS DIRECTORY
	****************************************


do "${sec45codedir}/compresults_5_furthertables.do"

	/* 'OUTPUTS'
		*** GROSS/NET MOBILITY VERSION
			TABLE 2 (MODEL PART), table2_model.txt     							// calibration moments
			TABLE 3, table3.txt				 									// estimated parameters
			TABLE 4 (GROSS/NET MODEL PART), table4_grossnetmob_model.txt		// model time series properties
			TABLE 5 (GROSS/NET MODEL PART),table5_grossnetmob_model.txt  		// incomplete duration distribution, cyclical behavior
			APPENDIX TABLE 1 (GROSS/NET MODEL PART),  appxtable1_grossnetmob_model.txt // incomplete duration distribution
			APPENDIX TABLE 3 (GROSS/NET MODEL PART) - SMOOTHED, , appxtable3_grossnetmob_model.txt    // model time series properties
			APPENDIX TABLE 3 (GROSS/NET MODEL PART) - UNSMOOTHED, appxtable3_grossnetmob_model_unsmooth.txt // model time series properties
			APPENDIX TABLE 8 (GROSS/NET MODEL PART), appxtable8_grossnetmob_model.txt // incomplete duration distribution, cyclical behavior
		
			text_grossnetmob_calibmodelstatements.txt // calculation some statements made in text, for gross/net model
		
		*** EXCESS MOBILITY VERSION
	
			TABLE 4 (EXCESS MOB MODEL PART),table4_excessmob_model.txt			// model time series properties
			TABLE 5 (EXCESS MOB MODEL PART).table5_excessmob_model.txt  		// incomplete duration distribution, cyclical behavior
			APPENDIX TABLE 1 (EXCESS MOB MODEL PART), appxtable1_excessmob_model.txt  // incomplete duration distribution
			APPENDIX TABLE 4 (EXCESS MOB MODEL PART), appxtable4_model.txt		// calibration moments
			APPENDIX TABLE 5 (EXCESS MOB MODEL PART) - SMOOTHED, appxtable5_excessmob_model.txt // model time series properties
			APPENDIX TABLE 8 (EXCESS MOB MODEL PART), appxtable8_excessmob_model.txt  // incomplete duration distribution, cyclical behavior
		
			appxtext_parameters_excessmob_model.txt	  //  parameter values for this version are mentioned in the appendix text
			text_excessmob_calibmodelstatements.txt	  // calculation some statements made in text, for excess mob model
		
		*** EXCESS MOBILITY VERSION without skill depreciation
	
			APPENDIX TABLE 5 (NO SKILL DEP MODEL PART), appxtable5_xsmobnoskilldep_model // ts cross correlation table
			xtra_xsmobnoskilldep_model_parameters.txt
		
	*** EXCESS MOBILITY VERSION NUN
	
			APPENDIX TABLE 5 (NUN MODEL PART), appxtable6_excessmob_NUN_model.txt // ts cross correlation table
			xtra_excessmob_NUN_model_parameters.txt
	
	*** NO REALLOCATION MODELS I AND II
	
			APPENDIX TABLE 7 (NO REALL MODEL I & II PART)  // appxtable7_noreall1_model.txt, appxtable7_noreall2_model.txt
			APPENDIX TABLE 8 (NO REALL MODEL I & II PART)  // appxtable8_noreall1_model.txt, appxtable8_noreall2_model.txt
		
			appxtext_noreall1_parameter_model.txt
			appxtext_noreall2_parameter_model.txt
		
	*/
	
	
	
********************************************************************************
global gendtime=c(current_time)
	global genddate=c(current_date)
	display "MASTER_CTV.do ended at ${gendtime} on ${genddate}"
	global grunningtime=((clock("${gendtime}", "hms")-clock("${gstarttime}", "hms"))/1000)+ (date("${genddate}","DMY")-date("${gstartdate}","DMY"))*86400
	display "running time ${grunningtime} seconds"
	display "start time: ${gstarttime}"
	display "end time: ${gendtime}"
	