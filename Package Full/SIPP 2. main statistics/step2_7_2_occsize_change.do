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
**   OCC SIZE --- EVOLUTION 1983-2013
********************************************************************************



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




	
cap n use "${outputdata}/corewave_occlfmin_ctv.dta", clear
version 13


	


do "${step2codedir}/aux_directory/aux_programs.do"
do "${step1codedir}/Ginv_matrices.do"




display "READING IN MISCODING MATRIX GAMMA_MM"
mata
b=xl() // excel file loaded tool
b.load_book("${step1codedir}/Gammamat_mm_c_v2.xlsx")
b.set_sheet("Sheet1") // identify the spreadsheet
rows=(1,22)
cols=(1,22)
Z=b.get_number((1,22),(1,22)) //import the matrix
st_matrix("Gmat_mm",Z)
end
matrix list Gmat_mm




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



// PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP	
capture program drop occtransmat_emp_exe
program define occtransmat_emp_exe, rclass
						args occ_ind occ_label  Ginv_in excl_matrix indic
						* occ_mmo locc1bfr_rtmm Ginv_rtmm excl_matrix occ_`i'_rtnm_corr
					
					tab `occ_ind ' [aw=pweight2] if `occ_ind'  !=. $localwavecond & entry_ind==1, matcell(test)
					matrix Gamma_adj=`Ginv_in'*`excl_matrix'
					
					
					
					
** saving all matrices 
					matrix socc_distr=Gamma_adj' * test 
					matrix freq=socc_distr
					local rws=rowsof(freq)
					
					display "** total flows ** "
					
					matrix define socc_distr_acc=J(`rws',1,1)
					matrix totalflows=socc_distr'*socc_distr_acc
					matrix list totalflows
					local totfl=totalflows[1,1]
					
					local ccc=$columncc
					quietly excel_col_exe `ccc'
					local lett1=r(xlscol)
					
					local ccc=$columncc+1
					quietly excel_col_exe `ccc'
					local lett2=r(xlscol)
					
							/*
							local ccc=$columncc+2
							quietly excel_col_exe `ccc'
							local lett3=r(xlscol)
							*/
							
							
					*** NOW FOR NAMES THE COLUMNS
					quietly tabulate `occ_label' [aw=pweight2] if `occ_ind' !=. $localwavecond $local_excl_occ & entry_ind==1, matcell(freq2) matrow(names)
			
					*display "matrix list names"
					*matrix list names 
					*display "matrix list freq"
					*matrix list freq
				    local rows = rowsof(names)
					local row = 2
					local row2= `row'+1+`rows'
					
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
					local percent_val : display %9.4f `percent_val'
			 
					
					putexcel `lett1'`row'=(" `val' ") `lett1'`row2'=(" `val' ")  `lett2'`row'=(`freq_val')   `lett2'`row2'=(`percent_val') /*`lett3'`row'=(`freq_val') */ /*`lett2'`row'=("`val_lab'") */ 
					local row = `row' + 1
					local row2= `row'+1+`rows'
					}
					
					matrix soccdistr=freq/`totfl'
			
					end
//END PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP					


						
		
		matrix rtmmconv_tpose= ( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 \ ///
											 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 ) 
		matrix excl_matrix=I(4)
											 
		matrix Ginv_rtmm=Ginv_mm*rtmmconv_tpose'
		
		
		//SET UP THE SPREADSHEET
		global columncc=1
		global filename "${outputdata}/occdistr_1984_2013_corr"
		global sheetname1 "rtnm corr"
	
		global sheet1counter=1
		global sheet2counter=1
		global sheet3counter=1
	
		cap n putexcel set "${filename}.xls", replace 
		cap n putexcel set "${filename}.xls", sheet("${sheetname1}") modify
		cap n putexcel set "${filename}.xls", modify sheet("${sheetname1}")
	
		global sttg=0
		global nuncond ""
						
		forvalues i=1983(1)2013 {
						display "$noagric13"
						display "$wavecond"
						global local_excl_occ " & occ_mmo!=11 & occ_mmo!=45 "
						global localwavecond " & empl_ctv==1 & (year==`i') & occ_mmo!=."
						display "$local_excl_occ"
						display "$localwavecond"
		
		
		occtransmat_emp_exe  occ_mmo locc1bfr_rtmm Ginv_rtmm excl_matrix occ_`i'_rtnm_corr
		global columncc=$columncc+4	
		
		if "`i'"=="1984" {
			matrix soccdistr_start=soccdistr
		}
		if "`i'"=="2012" {
			matrix soccdistr_end=soccdistr
		}
		
		
		}

matrix list soccdistr_start
matrix list soccdistr_end

quietly {
cap log close resultsoccsizechangelog
log using "${mainresultsdir}/table2panelB_occ_change_data.txt", replace text name(resultsoccsizechangelog)


noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  " OCC DISTRIBUTION START (1984)"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: matrix list soccdistr_start
noisily: display  "1=NRC, 2=RC, 3=NRM, 4=RM"

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  " OCC DISTRIBUTION END (2012)"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: matrix list soccdistr_end
noisily: display  "1=NRC, 2=RC, 3=NRM, 4=RM"



log close resultsoccsizechangelog
	

}		

quietly {
cap log close resultsoccsizechangelog
log using "${mainresultsdir}/table6_occ_change_data.txt", replace text name(resultsoccsizechangelog)


noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  " OCC DISTRIBUTION START (1984)"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: matrix list soccdistr_start
noisily: display  "1=NRC, 2=RC, 3=NRM, 4=RM"

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  " OCC DISTRIBUTION END (2012)"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: matrix list soccdistr_end
noisily: display  "1=NRC, 2=RC, 3=NRM, 4=RM"



log close resultsoccsizechangelog
	

}		










********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"

