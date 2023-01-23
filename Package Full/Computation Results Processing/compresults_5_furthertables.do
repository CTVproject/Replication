
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
*** MOVE SELECTED TABLES FROM COMPUTATION DIRECTORY TO RESULTS DIRECTORY 
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
	
	
	
	*** GROSS/NET MOBILITY VERSION
	
		/*
		NET MOB SPECIFIC
			-shifts_netflow_inflow.csv (needs to be produced as in input into compresults_2), 
				but also include durelasticity, ===> appxtable2.csv
			-xsnetprofile.csv (needs to be produced, so it can be input in compresults_1... )
			-table6.txt to table6.txt (LAST ONE!)
			
		ALL THIS IS DONE IN COMPRESULTS_2_appxfig3b_cycl_netflow_inflow_shift.doc.
		
		*/
		cap n copy "${fortrandir}/Gross and Net Mobility version/shifts_netflow_inflow.csv"  "${mainresultsdir}/appxtable2_grossnetflow_stats.csv" , replace
		*LOCATION "${fortrandir}/Gross and Net Mobility version/"
		
		* GROSS MOB AND GENERAL RESULTS
		cap n copy "${fortrandir}/Gross and Net Mobility version/parameters.txt"  "${mainresultsdir}/table3.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/moments.txt"  "${mainresultsdir}/table2_model.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/ts_xcorrelations.txt"  "${mainresultsdir}/table4_grossnetmob_model.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/ts_xcorrelations.txt"  "${mainresultsdir}/appxtable3_grossnetmob_model.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/ts_xcorrelations_unsmoothed.txt"  "${mainresultsdir}/appxtable3_grossnetmob_model_unsmooth.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/incompl_durdistr_stats.txt"  "${mainresultsdir}/appxtable1_grossnetmob_model.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/incompl_durdistr_stats.txt"  "${mainresultsdir}/appxtable8_grossnetmob_model.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/incompl_durdistr_stats.txt"  "${mainresultsdir}/table5_grossnetmob_model.txt" , replace
		cap n copy "${fortrandir}/Gross and Net Mobility version/further_stats.txt"  "${mainresultsdir}/text_grossnetmob_calibmodelstatements.txt" , replace
		* NET MOB RESULTS 
		
		
		
	*** EXCESS MOBILITY VERSION
	
		/*
		1) parameters.txt to appxtext_parameters_excessmob_model.txt
		2) moments_table.txt to appxtable4_model.txt
		3) sm_corrtable.txt to table4_excessmob_model.txt
		4) sm_corrtable.txt to appxtable5_excessmob.txt
		5) incomplete_durdistr_stats.csv to appxtable1_excessmob_model.csv
		6) incomplete_durdistr_stats.csv to appxtable11_excessmob_model.csv
		7) incomplete_durdistr_stats.csv to table5panelA_excessmob_model.csv
		8) further moments
		*/
	
		* LOCATION ${fortrandir}/Excess Mobility version/
		
		cap n copy "${fortrandir}/Excess Mobility version/parameters.txt"  "${mainresultsdir}/appxtext_parameters_excessmob_model.txt" , replace
		cap n copy "${fortrandir}/Excess Mobility version/moments.txt"  "${mainresultsdir}/appxtable4_model.txt" , replace
		cap n copy "${fortrandir}/Excess Mobility version/ts_xcorrelations.txt"  "${mainresultsdir}/table4_excessmob_model.txt" , replace
		cap n copy "${fortrandir}/Excess Mobility version/ts_xcorrelations.txt"  "${mainresultsdir}/appxtable5_excessmob_model.txt" , replace
		cap n copy "${fortrandir}/Excess Mobility version/incompl_durdistr_stats.txt"  "${mainresultsdir}/appxtable1_excessmob_model.txt" , replace
		cap n copy "${fortrandir}/Excess Mobility version/incompl_durdistr_stats.txt"  "${mainresultsdir}/appxtable8_excessmob_model.txt" , replace
		cap n copy "${fortrandir}/Excess Mobility version/incompl_durdistr_stats.txt"  "${mainresultsdir}/table5_excessmob_model.txt" , replace
		cap n copy "${fortrandir}/Excess Mobility version/further_stats.txt"  "${mainresultsdir}/text_excessmob_calibmodelstatements.txt" , replace
	
	*** EXCESS MOBILITY VERSION without skill depreciation
	
		* TS CROSS CORRELATION TABLE
		cap n copy "${fortrandir}/Robustness No Skill Depreciation/ts_xcorrelations.txt"  "${mainresultsdir}/appxtable5_xsmobnoskilldep_model.txt" , replace
		cap n copy "${fortrandir}/Robustness No Skill Depreciation/parameters.txt"  "${mainresultsdir}/xtra_xsmobnoskilldep_model_parameters.txt" , replace
		
		
	*** EXCESS MOBILITY VERSION NUN
	
		* TS CROSS CORRELATION TABLE
		cap n copy "${fortrandir}/Robustness NUN/ts_xcorrelations.txt"  "${mainresultsdir}/appxtable6_excessmob_NUN_model.txt" , replace
		cap n copy "${fortrandir}/Robustness NUN/parameters.txt"  "${mainresultsdir}/xtra_excessmob_NUN_model_parameters.txt" , replace
	
	
	*** NO REALLOCATION MODELS I AND II
	
		* INCOMPLETE DURATION DISTRIBUTION BEHAVIOR OVER THE CYCLE 
		cap n copy "${fortrandir}/Robustness No Reallocation/No Reallocation Model I/incompl_durdistr_stats.txt" "${mainresultsdir}/appxtable8_noreall1_model.txt", replace
		cap n copy "${fortrandir}/Robustness No Reallocation/No Reallocation Model II/incompl_durdistr_stats.txt" "${mainresultsdir}/appxtable8_noreall2_model.txt", replace
		* TS CROSS CORRELATION TABLE
		cap n copy "${fortrandir}/Robustness No Reallocation/No Reallocation Model I/ts_xcorrelations.txt" "${mainresultsdir}/appxtable7_noreall1_model.txt", replace
		cap n copy "${fortrandir}/Robustness No Reallocation/No Reallocation Model II/ts_xcorrelations.txt" "${mainresultsdir}/appxtable7_noreall2_model.txt", replace
		* PARAMETERS
		cap n copy "${fortrandir}/Robustness No Reallocation/No Reallocation Model I/parameters.txt" "${mainresultsdir}/appxtext_noreall1_parameter_model.txt", replace
		cap n copy "${fortrandir}/Robustness No Reallocation/No Reallocation Model II/parameters.txt" "${mainresultsdir}/appxtext_noreall2_parameter_model.txt", replace
		
		
	*******************************************************
	
	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"