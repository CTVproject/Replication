	
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


/*
THIS FILE PRODUCES THE REPEAT MOBILITY STATISTICS MENTIONED IN THE PAPER,
AND (THE ONE THAT IS) USED IN THE CALIBRATION


NOTE:
	- we are looking at those who have a pure u-spell in the first spell
	- but in the second spell we allow mixed N/U (but need to have at least one 
		month of unemployment)
	- in our preferred measure, we exclude agriculture/fishermen and construction
	- to calculate the proportion of workers changing to a third occupation, 
		after moving initially in the first spell, we drop those who are moving
		back to the first occupation. 
		
OUTPUT:


repeatmobility.xls
repeatmobility.dta


USED IN THE CALIBRATION

*/


use "${outputdata}/reduced_u_n_ctv.dta"
keep if complete_uspell==1 | complete_nunspell==1  
drop if lne_c_mmo==.
 // some small difference 11 obs additional, and some 70ish occupations coded differently in 1984 panel
capture drop age_2dum
gen byte age_2dum=1 if tage>=20 & tage<=30
replace age_2dum=2 if tage>=35 & tage<=55
 
drop if panel==1984 
 
sort personkey yearmonth

sort personkey yearmonth

** auxiliary programs
do "${step2codedir}/aux_directory/aux_programs.do"
			/* programs:
					xsnet_calcx
					occmat_dim_exe	
			*/
** correction matrices
do "${step1codedir}/Ginv_matrices.do"



//=========================================================================================
//
//  GAMMA CORRECTED REPEAT MOBILITY
//
//============================================================================================

version 13
do "${step1codedir}/Ginv_matrices.do"
do "${step2codedir}/aux_directory/aux_programs.do"

tab locc1bfr_mmo, matcell(occtemp)
occmat_dim_exe locc1bfr_mmo
matrix occmat=r(occno_matrix)


capture drop llocc1bfr_mmo
capture drop llocc1aft_mmo
sort personkey yearmonth
gen llocc1bfr_mmo=locc1bfr_mmo[_n-1] if personkey==personkey[_n-1] 
gen llocc1aft_mmo=locc1aft_mmo[_n-1] if personkey==personkey[_n-1] 


global repeat_ifcond=""




//pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
//========================================================================
// program calculating stay-AFTER-STAY MOVE-AFTER-MOVE
//=============================================================================



capture program drop repeat_stats_calc_exe
program define repeat_stats_calc_exe, rclass
			args occbfr occmdl occaft agg_indic Ginvmat name_suffix agric_no
			
if "`agric_no'"=="" {
   display "PLEASE ENTER AGRICULTURAL OCCUPATION NUMBER, or if none: 0"
   exit
}

*matrix Ginv_mm_real=Ginv_mm
*matrix Ginv_mm=Ginv_mm_real

*matrix Ginv_mm=I(22)
matrix GGinvm=`Ginvmat'#`Ginvmat'
matrix GGGinvm=GGinvm#`Ginvmat'


* matrix with observed doube-occupational 
global no_occs=rowsof(`Ginvmat')
global no_kron_occs=$no_occs * $no_occs * $no_occs
matrix mr_obs=J(1,$no_kron_occs, 0)


** capture 

preserve

keep if `occbfr'!=. & `occmdl'!=. & `occaft'!=. $repeat_ifcond
capture drop repeatflow_wgt_`agg_indic'

bysort `occbfr' `occmdl' `occaft': egen repeatflow_wgt_`agg_indic'=sum(pweight2)
*bysort locc1bfr_mmo locc1bfr_mmo locc1aft_mmo: egen repeatflow_wgt_loccbfr=sum(pweight2)
duplicates report `occbfr' `occmdl' `occaft'

*sort duplicate_flow llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo
*browse duplicate_flow llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo
duplicates drop `occbfr' `occmdl' `occaft', force
display "number of different repeat transitions"
count  // about 29% of cells are indeed nonzero

su repeatflow_wgt_`agg_indic'
display "sum all weights" r(sum)


** occupations ijk
matrix mr_obs=J(1,$no_kron_occs, 0)


local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {
	
	if (`elem'==1000 | `elem'==5000 | `elem'==10000)  {
	display "counter is `elem'"	
	}
	
	matrix mr_obs[1,`elem']=0
	quietly  su repeatflow_wgt_`agg_indic' if `occbfr'==occmat[`i',1] & `occmdl' ==occmat[`j',1] & `occaft'==occmat[`k',1] 

	if r(mean)>0 & r(mean)!=. {
	matrix mr_obs[1,`elem']=r(mean)
	}
	
	local elem=`elem'+1
}
}
}

matrix mr_true=J(1, $no_kron_occs, 0)
matrix mr_col=J($no_kron_occs, 1, 0)
matrix mr_elem=J(1, 1, 0)

display "matrix list mr_obs"
matrix list mr_obs



/*
mata 
mr_obsmata=st_matrix("mr_obs")
GGGinvmata=st_matrix("GGGinvm")
mr_truemata=mr_obsmata*GGGinvmata
st_matrix("mr_true2", mr_truemata)	
end
*/
*mata:mymataprog(arg1,arg2,etc.)
display "rowsof(GGGinvm) and rowsof(mr_obs)"
display rowsof(GGGinvm)
display rowsof(mr_obs)
*mata: matamult_exe(mr_obs,GGGinvm)
matrix mr_true2=mr_obs*GGGinvm

*** CREATE MULTIPLICATION MATRIX THAT COLLECTS ALL Stay-Stay, Stay-Move, Move-Stay, Move-Move
matrix categorize_matrix=J(${no_kron_occs},5,0)


local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {

 if `i'==`j' & `j'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',1]=1
 }
 if `i'==`j' & `j'!=`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',2]=1
 }
 if `i'!=`j' & `j'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',3]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',4]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',5]=1
 }

 *display "counter is `elem'"
 *display categorize_matrix[`elem',3]
 *display categorize_matrix[`elem',4]
 *display "counter=`elem'"
 if (`elem'==1000 | `elem'==5000 | `elem'==10000)  {
	display "counter is `elem'"	
	}
	
 local elem=1+`elem'
 
}
}
}

display "matrix list mr_obs"
matrix list mr_obs



*** PER OCCUPATION***

forvalues i=1(1)$no_occs{
matrix categorize_matrix`i'=J(${no_kron_occs},5,0)
}

local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {

 if `i'==`j' & `j'==`k'  {
 matrix categorize_matrix`i'[`elem',1]=1
 }
 if `i'==`j' & `j'!=`k'   {
 matrix categorize_matrix`i'[`elem',2]=1
 }
 if `i'!=`j' & `j'==`k'  {
 matrix categorize_matrix`i'[`elem',3]=1
 }
 if `i'!=`j' & `j'!=`k' {
 matrix categorize_matrix`i'[`elem',4]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'==`k' {
 matrix categorize_matrix`i'[`elem',5]=1
 }

 *display "counter=`elem'"
 local elem=1+`elem'
 
}
}
}





*** CREATE MULTIPLICATION MATRIX THAT COLLECTS Stay-Stay, Stay-Move, Move-Stay, Move-Move PER INITIAL OCCUPATION

matrix repeatstaymove=J(1,5,0)
matrix repeatstaymove=mr_true2*categorize_matrix

global stay_stay= repeatstaymove[1,1]/(repeatstaymove[1,1]+repeatstaymove[1,2])
display "stay after stay, corrected: ", $stay_stay


global move_move=repeatstaymove[1,4]/(repeatstaymove[1,3]+repeatstaymove[1,4])
display "move after move, corrected: ", $move_move

global move_move_back=repeatstaymove[1,5]/(repeatstaymove[1,3]+repeatstaymove[1,4])
display "move back after move, corrected: ", $move_move_back


** raw
matrix repeatstaymoveraw=J(1,5,0)
matrix repeatstaymoveraw=mr_obs*categorize_matrix

display "matrix list repeatstaymoveraw"
matrix list repeatstaymoveraw

global stay_stay_raw=repeatstaymoveraw[1,1]/(repeatstaymoveraw[1,1]+repeatstaymoveraw[1,2])
display "stay after stay raw:", $stay_stay_raw

global move_move_raw=repeatstaymoveraw[1,4]/(repeatstaymoveraw[1,3]+repeatstaymoveraw[1,4])
display "move after move, raw:" $move_move_raw

global move_move_back_raw=repeatstaymoveraw[1,5]/(repeatstaymoveraw[1,3]+repeatstaymoveraw[1,4])
display "move after move, raw:" $move_move_back_raw

***  PER OCCUPATION REPEAT STAYING 
matrix repeatstaymove_temp=J(1,5,0)
matrix repeatstaymoveraw_temp=J(1,5,0)

matrix repeatstaymove_occ=J(${no_occs},5,0)
matrix repeatstaymoveraw_occ=J(${no_occs},5,0)

forvalues i=1(1)$no_occs {
matrix repeatstaymove_temp=mr_true2*categorize_matrix`i'
forvalues j=1(1)5{
matrix repeatstaymove_occ[`i',`j']=repeatstaymove_temp[1,`j']
}
}

forvalues i=1(1)$no_occs {
matrix repeatstaymoveraw_temp=mr_obs*categorize_matrix`i'
forvalues j=1(1)5{
matrix repeatstaymoveraw_occ[`i',`j']=repeatstaymoveraw_temp[1,`j']
}
}

matrix stay_stay_occ=J(${no_occs},1,0)
matrix move_move_occ=J(${no_occs},1,0)
matrix stay_stay_raw_occ=J(${no_occs},1,0)
matrix move_move_raw_occ=J(${no_occs},1,0)
matrix move_move_back_occ=J(${no_occs},1,0)
matrix move_move_back_raw_occ=J(${no_occs},1,0)

forvalues i=1(1)$no_occs{
matrix stay_stay_occ[`i',1]=repeatstaymove_occ[`i',1]/(repeatstaymove_occ[`i',1]+repeatstaymove_occ[`i',2])
matrix stay_stay_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',1]/(repeatstaymoveraw_occ[`i',1]+repeatstaymoveraw_occ[`i',2])
matrix move_move_occ[`i',1]=repeatstaymove_occ[`i',4]/(repeatstaymove_occ[`i',3]+repeatstaymove_occ[`i',4])
matrix move_move_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',4]/(repeatstaymoveraw_occ[`i',3]+repeatstaymoveraw_occ[`i',4])
matrix move_move_back_occ[`i',1]=repeatstaymove_occ[`i',5]/(repeatstaymove_occ[`i',3]+repeatstaymove_occ[`i',4])
matrix move_move_back_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',5]/(repeatstaymoveraw_occ[`i',3]+repeatstaymoveraw_occ[`i',4])

}

matrix concat= stay_stay_occ , stay_stay_raw_occ , move_move_occ , move_move_raw_occ, move_move_back_occ, move_move_back_raw_occ
display "stay after stay (corr, raw) and move after move (corr, raw) percentages per occupation, and move back after move (corr, raw)"
matrix list concat


display "stay after stay, corrected: ", $stay_stay
display "move after move, corrected: ", $move_move
display "stay after stay raw:", $stay_stay_raw
display "move after move, raw:" $move_move_raw
display "move back after move, corrected: ", $move_move_back
display "move back after move, raw: ", $move_move_back_raw



*
return scalar stay_stay=$stay_stay
return scalar move_move=$move_move
return scalar stay_stay_raw=$stay_stay_raw
return scalar move_move_raw=$move_move_raw
return scalar move_move_back=$move_move_back
return scalar move_move_back_raw=$move_move_back_raw

return matrix repeatstaymove_occ=repeatstaymove_occ
return matrix repeatstaymoveraw_occ=repeatstaymoveraw_occ
return matrix stay_stay_occ=stay_stay_occ
return matrix stay_stay_raw_occ=stay_stay_raw_occ
return matrix move_move_occ=move_move_occ
return matrix move_move_raw_occ=move_move_raw_occ
return matrix move_move_back_occ=move_move_back_occ
return matrix move_move_back_raw_occ=move_move_back_raw_occ

restore 


end 




capture program drop repeat_stats_calc_exe_2excl
program define repeat_stats_calc_exe_2excl, rclass
			args occbfr occmdl occaft agg_indic Ginvmat name_suffix agric_no constr_no
			
if "`agric_no'"=="" {
   display "PLEASE ENTER AGRICULTURAL OCCUPATION (OR 1ST/out of 2 EXCL OCC) NUMBER, or if none: 0"
   exit
}

if "`constr_no'"=="" {
   display "PLEASE ENTER CONSTRUCTION OCCUPATION (OR 2ND EXCL OCC) NUMBER, or if none: 0"
   exit
}


*matrix Ginv_mm_real=Ginv_mm
*matrix Ginv_mm=Ginv_mm_real

*matrix Ginv_mm=I(22)
matrix GGinvm=`Ginvmat'#`Ginvmat'
matrix GGGinvm=GGinvm#`Ginvmat'


* matrix with observed doube-occupational 
global no_occs=rowsof(`Ginvmat')
global no_kron_occs=$no_occs * $no_occs * $no_occs
matrix mr_obs=J(1,$no_kron_occs, 0)


** capture 

preserve

keep if `occbfr'!=. & `occmdl'!=. & `occaft'!=. $repeat_ifcond
capture drop repeatflow_wgt_`agg_indic'

bysort `occbfr' `occmdl' `occaft': egen repeatflow_wgt_`agg_indic'=sum(pweight2)
*bysort locc1bfr_mmo locc1bfr_mmo locc1aft_mmo: egen repeatflow_wgt_loccbfr=sum(pweight2)
duplicates report `occbfr' `occmdl' `occaft'

*sort duplicate_flow llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo
*browse duplicate_flow llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo
duplicates drop `occbfr' `occmdl' `occaft', force
count  // about 29% of cells are indeed nonzero



** occupations ijk
matrix mr_obs=J(1,$no_kron_occs, 0)

local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {
	*display "counter is `elem'"	
	matrix mr_obs[1,`elem']=0
	quietly  su repeatflow_wgt_`agg_indic' if `occbfr'==occmat[`i',1] & `occmdl' ==occmat[`j',1] & `occaft'==occmat[`k',1] 

	if r(mean)>0 & r(mean)!=. {
	matrix mr_obs[1,`elem']=r(mean)
	}
	
	local elem=`elem'+1
}
}
}

matrix mr_true=J(1, $no_kron_occs, 0)
matrix mr_col=J($no_kron_occs, 1, 0)
matrix mr_elem=J(1, 1, 0)

*mata:mymataprog(arg1,arg2,etc.)
display rowsof(GGGinvm)
display rowsof(mr_obs)
*mata: matamult_exe(mr_obs,GGGinvm)
matrix mr_true2=mr_obs*GGGinvm

*** CREATE MULTIPLICATION MATRIX THAT COLLECTS ALL Stay-Stay, Stay-Move, Move-Stay, Move-Move
matrix categorize_matrix=J(${no_kron_occs},5,0)


local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {

 if `i'==`j' & `j'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' & `i'!=`constr_no' & `j'!=`constr_no' & `k'!=`constr_no' {
 matrix categorize_matrix[`elem',1]=1
 }
 if `i'==`j' & `j'!=`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no'  & `i'!=`constr_no' & `j'!=`constr_no' & `k'!=`constr_no' {
 matrix categorize_matrix[`elem',2]=1
 }
 if `i'!=`j' & `j'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no'  & `i'!=`constr_no' & `j'!=`constr_no' & `k'!=`constr_no' {
 matrix categorize_matrix[`elem',3]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' & `i'!=`constr_no' & `j'!=`constr_no' & `k'!=`constr_no' {
 matrix categorize_matrix[`elem',4]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' & `i'!=`constr_no' & `j'!=`constr_no' & `k'!=`constr_no' {
 matrix categorize_matrix[`elem',5]=1
 }

 
 *display "counter=`elem'"
 local elem=1+`elem'
 
}
}
}




*** PER OCCUPATION***

forvalues i=1(1)$no_occs{
matrix categorize_matrix`i'=J(${no_kron_occs},5,0)
}

local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {

 if `i'==`j' & `j'==`k'  {
 matrix categorize_matrix`i'[`elem',1]=1
 }
 if `i'==`j' & `j'!=`k'   {
 matrix categorize_matrix`i'[`elem',2]=1
 }
 if `i'!=`j' & `j'==`k'  {
 matrix categorize_matrix`i'[`elem',3]=1
 }
 if `i'!=`j' & `j'!=`k' {
 matrix categorize_matrix`i'[`elem',4]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'==`k' {
 matrix categorize_matrix`i'[`elem',5]=1
 }

 *display "counter=`elem'"
 local elem=1+`elem'
 
}
}
}





*** CREATE MULTIPLICATION MATRIX THAT COLLECTS Stay-Stay, Stay-Move, Move-Stay, Move-Move PER INITIAL OCCUPATION

matrix repeatstaymove=J(1,5,0)
matrix repeatstaymove=mr_true2*categorize_matrix

global stay_stay= repeatstaymove[1,1]/(repeatstaymove[1,1]+repeatstaymove[1,2])
display "stay after stay, corrected: ", $stay_stay


global move_move=repeatstaymove[1,4]/(repeatstaymove[1,3]+repeatstaymove[1,4])
display "move after move, corrected: ", $move_move

global move_move_back=repeatstaymove[1,5]/(repeatstaymove[1,3]+repeatstaymove[1,4])
display "move back after move, corrected: ", $move_move_back


** raw
matrix repeatstaymoveraw=J(1,5,0)
matrix repeatstaymoveraw=mr_obs*categorize_matrix

global stay_stay_raw=repeatstaymoveraw[1,1]/(repeatstaymoveraw[1,1]+repeatstaymoveraw[1,2])
display "stay after stay raw:", $stay_stay_raw

global move_move_raw=repeatstaymoveraw[1,4]/(repeatstaymoveraw[1,3]+repeatstaymoveraw[1,4])
display "move after move, raw:" $move_move_raw

global move_move_back_raw=repeatstaymoveraw[1,5]/(repeatstaymoveraw[1,3]+repeatstaymoveraw[1,4])
display "move after move, raw:" $move_move_back_raw

***  PER OCCUPATION REPEAT STAYING 
matrix repeatstaymove_temp=J(1,5,0)
matrix repeatstaymoveraw_temp=J(1,5,0)

matrix repeatstaymove_occ=J(${no_occs},5,0)
matrix repeatstaymoveraw_occ=J(${no_occs},5,0)

forvalues i=1(1)$no_occs {
matrix repeatstaymove_temp=mr_true2*categorize_matrix`i'
forvalues j=1(1)5{
matrix repeatstaymove_occ[`i',`j']=repeatstaymove_temp[1,`j']
}
}

forvalues i=1(1)$no_occs {
matrix repeatstaymoveraw_temp=mr_obs*categorize_matrix`i'
forvalues j=1(1)5{
matrix repeatstaymoveraw_occ[`i',`j']=repeatstaymoveraw_temp[1,`j']
}
}

matrix stay_stay_occ=J(${no_occs},1,0)
matrix move_move_occ=J(${no_occs},1,0)
matrix stay_stay_raw_occ=J(${no_occs},1,0)
matrix move_move_raw_occ=J(${no_occs},1,0)
matrix move_move_back_occ=J(${no_occs},1,0)
matrix move_move_back_raw_occ=J(${no_occs},1,0)

forvalues i=1(1)$no_occs{
matrix stay_stay_occ[`i',1]=repeatstaymove_occ[`i',1]/(repeatstaymove_occ[`i',1]+repeatstaymove_occ[`i',2])
matrix stay_stay_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',1]/(repeatstaymoveraw_occ[`i',1]+repeatstaymoveraw_occ[`i',2])
matrix move_move_occ[`i',1]=repeatstaymove_occ[`i',4]/(repeatstaymove_occ[`i',3]+repeatstaymove_occ[`i',4])
matrix move_move_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',4]/(repeatstaymoveraw_occ[`i',3]+repeatstaymoveraw_occ[`i',4])
matrix move_move_back_occ[`i',1]=repeatstaymove_occ[`i',5]/(repeatstaymove_occ[`i',3]+repeatstaymove_occ[`i',4])
matrix move_move_back_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',5]/(repeatstaymoveraw_occ[`i',3]+repeatstaymoveraw_occ[`i',4])

}

matrix concat= stay_stay_occ , stay_stay_raw_occ , move_move_occ , move_move_raw_occ, move_move_back_occ, move_move_back_raw_occ
display "stay after stay (corr, raw) and move after move (corr, raw) percentages per occupation, and move back after move (corr, raw)"
matrix list concat


display "stay after stay, corrected: ", $stay_stay
display "move after move, corrected: ", $move_move
display "stay after stay raw:", $stay_stay_raw
display "move after move, raw:" $move_move_raw
display "move back after move, corrected: ", $move_move_back
display "move back after move, raw: ", $move_move_back_raw



*
return scalar stay_stay=$stay_stay
return scalar move_move=$move_move
return scalar stay_stay_raw=$stay_stay_raw
return scalar move_move_raw=$move_move_raw
return scalar move_move_back=$move_move_back
return scalar move_move_back_raw=$move_move_back_raw

return matrix repeatstaymove_occ=repeatstaymove_occ
return matrix repeatstaymoveraw_occ=repeatstaymoveraw_occ
return matrix stay_stay_occ=stay_stay_occ
return matrix stay_stay_raw_occ=stay_stay_raw_occ
return matrix move_move_occ=move_move_occ
return matrix move_move_raw_occ=move_move_raw_occ
return matrix move_move_back_occ=move_move_back_occ
return matrix move_move_back_raw_occ=move_move_back_raw_occ

restore 


end 




//==================================
//



capture program drop repeat_stats_calc_exe_nointchg
program define repeat_stats_calc_exe_nointchg, rclass
			args occbfr occmdl1 occmdl2 occaft agg_indic Ginvmat name_suffix agric_no
			
if "`agric_no'"=="" {
   display "PLEASE ENTER AGRICULTURAL OCCUPATION NUMBER, or if none: 0"
   exit
}

local occmdl "`occmdl1'"
*matrix Ginv_mm_real=Ginv_mm
*matrix Ginv_mm=Ginv_mm_real

*matrix Ginv_mm=I(22)
matrix GGinvm=`Ginvmat'#`Ginvmat'
matrix GGGinvm=GGinvm#`Ginvmat'


* matrix with observed doube-occupational 
global no_occs=rowsof(`Ginvmat')
global no_kron_occs=$no_occs * $no_occs * $no_occs
matrix mr_obs=J(1,$no_kron_occs, 0)


** capture 

preserve

keep if `occbfr'!=. & `occmdl'!=. & `occaft'!=. $repeat_ifcond
capture drop repeatflow_wgt_`agg_indic'

bysort `occbfr' `occmdl' `occaft': egen repeatflow_wgt_`agg_indic'=sum(pweight2)
*bysort locc1bfr_mmo locc1bfr_mmo locc1aft_mmo: egen repeatflow_wgt_loccbfr=sum(pweight2)
duplicates report `occbfr' `occmdl' `occaft'

*sort duplicate_flow llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo
*browse duplicate_flow llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo
duplicates drop `occbfr' `occmdl' `occaft', force
count  // about 29% of cells are indeed nonzero



** occupations ijk
matrix mr_obs=J(1,$no_kron_occs, 0)

local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {
	*display "counter is `elem'"	
	matrix mr_obs[1,`elem']=0
	quietly  su repeatflow_wgt_`agg_indic' if `occbfr'==occmat[`i',1] & `occmdl'==occmat[`j',1] & `occaft'==occmat[`k',1] & `occmdl1'==`occmdl2' 

	if r(mean)>0 & r(mean)!=. {
	matrix mr_obs[1,`elem']=r(mean)
	}
	
	local elem=`elem'+1
}
}
}

matrix mr_true=J(1, $no_kron_occs, 0)
matrix mr_col=J($no_kron_occs, 1, 0)
matrix mr_elem=J(1, 1, 0)

/*
mata 
mr_obsmata=st_matrix("mr_obs")
GGGinvmata=st_matrix("GGGinvm")
mr_truemata=mr_obsmata*GGGinvmata
st_matrix("mr_true2", mr_truemata)	
end
*/
*mata:mymataprog(arg1,arg2,etc.)
display rowsof(GGGinvm)
display rowsof(mr_obs)
*mata: matamult_exe(mr_obs,GGGinvm)
matrix mr_true2=mr_obs*GGGinvm

*** CREATE MULTIPLICATION MATRIX THAT COLLECTS ALL Stay-Stay, Stay-Move, Move-Stay, Move-Move
matrix categorize_matrix=J(${no_kron_occs},5,0)


local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {

 if `i'==`j' & `j'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',1]=1
 }
 if `i'==`j' & `j'!=`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no'  {
 matrix categorize_matrix[`elem',2]=1
 }
 if `i'!=`j' & `j'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',3]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',4]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'==`k' & `i'!=`agric_no' & `j'!=`agric_no' & `k'!=`agric_no' {
 matrix categorize_matrix[`elem',5]=1
 }

 
 display "counter=`elem'"
 local elem=1+`elem'
 
}
}
}



*** PER OCCUPATION***

forvalues i=1(1)$no_occs{
matrix categorize_matrix`i'=J(${no_kron_occs},5,0)
}

local elem=1
forvalues i=1(1)$no_occs {
forvalues j=1(1)$no_occs {   
forvalues k=1(1)$no_occs {

 if `i'==`j' & `j'==`k'  {
 matrix categorize_matrix`i'[`elem',1]=1
 }
 if `i'==`j' & `j'!=`k'   {
 matrix categorize_matrix`i'[`elem',2]=1
 }
 if `i'!=`j' & `j'==`k'  {
 matrix categorize_matrix`i'[`elem',3]=1
 }
 if `i'!=`j' & `j'!=`k' {
 matrix categorize_matrix`i'[`elem',4]=1
 }
 if `i'!=`j' & `j'!=`k' & `i'==`k' {
 matrix categorize_matrix`i'[`elem',5]=1
 }

 display "counter=`elem'"
 local elem=1+`elem'
 
}
}
}





*** CREATE MULTIPLICATION MATRIX THAT COLLECTS Stay-Stay, Stay-Move, Move-Stay, Move-Move PER INITIAL OCCUPATION

matrix repeatstaymove=J(1,5,0)
matrix repeatstaymove=mr_true2*categorize_matrix

global stay_stay= repeatstaymove[1,1]/(repeatstaymove[1,1]+repeatstaymove[1,2])
display "stay after stay, corrected: ", $stay_stay


global move_move=repeatstaymove[1,4]/(repeatstaymove[1,3]+repeatstaymove[1,4])
display "move after move, corrected: ", $move_move

global move_move_back=repeatstaymove[1,5]/(repeatstaymove[1,3]+repeatstaymove[1,4])
display "move back after move, corrected: ", $move_move_back


** raw
matrix repeatstaymoveraw=J(1,5,0)
matrix repeatstaymoveraw=mr_obs*categorize_matrix

global stay_stay_raw=repeatstaymoveraw[1,1]/(repeatstaymoveraw[1,1]+repeatstaymoveraw[1,2])
display "stay after stay raw:", $stay_stay_raw

global move_move_raw=repeatstaymoveraw[1,4]/(repeatstaymoveraw[1,3]+repeatstaymoveraw[1,4])
display "move after move, raw:" $move_move_raw

global move_move_back_raw=repeatstaymoveraw[1,5]/(repeatstaymoveraw[1,3]+repeatstaymoveraw[1,4])
display "move after move, raw:" $move_move_back_raw

***  PER OCCUPATION REPEAT STAYING 
matrix repeatstaymove_temp=J(1,5,0)
matrix repeatstaymoveraw_temp=J(1,5,0)

matrix repeatstaymove_occ=J(${no_occs},5,0)
matrix repeatstaymoveraw_occ=J(${no_occs},5,0)

forvalues i=1(1)$no_occs {
matrix repeatstaymove_temp=mr_true2*categorize_matrix`i'
forvalues j=1(1)5{
matrix repeatstaymove_occ[`i',`j']=repeatstaymove_temp[1,`j']
}
}

forvalues i=1(1)$no_occs {
matrix repeatstaymoveraw_temp=mr_obs*categorize_matrix`i'
forvalues j=1(1)5{
matrix repeatstaymoveraw_occ[`i',`j']=repeatstaymoveraw_temp[1,`j']
}
}

matrix stay_stay_occ=J(${no_occs},1,0)
matrix move_move_occ=J(${no_occs},1,0)
matrix stay_stay_raw_occ=J(${no_occs},1,0)
matrix move_move_raw_occ=J(${no_occs},1,0)
matrix move_move_back_occ=J(${no_occs},1,0)
matrix move_move_back_raw_occ=J(${no_occs},1,0)

forvalues i=1(1)$no_occs{
matrix stay_stay_occ[`i',1]=repeatstaymove_occ[`i',1]/(repeatstaymove_occ[`i',1]+repeatstaymove_occ[`i',2])
matrix stay_stay_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',1]/(repeatstaymoveraw_occ[`i',1]+repeatstaymoveraw_occ[`i',2])
matrix move_move_occ[`i',1]=repeatstaymove_occ[`i',4]/(repeatstaymove_occ[`i',3]+repeatstaymove_occ[`i',4])
matrix move_move_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',4]/(repeatstaymoveraw_occ[`i',3]+repeatstaymoveraw_occ[`i',4])
matrix move_move_back_occ[`i',1]=repeatstaymove_occ[`i',5]/(repeatstaymove_occ[`i',3]+repeatstaymove_occ[`i',4])
matrix move_move_back_raw_occ[`i',1]=repeatstaymoveraw_occ[`i',5]/(repeatstaymoveraw_occ[`i',3]+repeatstaymoveraw_occ[`i',4])

}

matrix concat= stay_stay_occ , stay_stay_raw_occ , move_move_occ , move_move_raw_occ, move_move_back_occ, move_move_back_raw_occ
display "stay after stay (corr, raw) and move after move (corr, raw) percentages per occupation, and move back after move (corr, raw)"
matrix list concat


display "stay after stay, corrected: ", $stay_stay
display "move after move, corrected: ", $move_move
display "stay after stay raw:", $stay_stay_raw
display "move after move, raw:" $move_move_raw
display "move back after move, corrected: ", $move_move_back
display "move back after move, raw: ", $move_move_back_raw




return scalar stay_stay=$stay_stay
return scalar move_move=$move_move
return scalar stay_stay_raw=$stay_stay_raw
return scalar move_move_raw=$move_move_raw
return scalar move_move_back=$move_move_back
return scalar move_move_back_raw=$move_move_back_raw

return matrix repeatstaymove_occ=repeatstaymove_occ
return matrix repeatstaymoveraw_occ=repeatstaymoveraw_occ
return matrix stay_stay_occ=stay_stay_occ
return matrix stay_stay_raw_occ=stay_stay_raw_occ
return matrix move_move_occ=move_move_occ
return matrix move_move_raw_occ=move_move_raw_occ
return matrix move_move_back_occ=move_move_back_occ
return matrix move_move_back_raw_occ=move_move_back_raw_occ

restore 

end 




//====================================================================
// RUN REPEAT MOBILITY GAMMA-CORRECTED  // REPORT
//=====================================================================

version 13


//============================================
//   REPEAT MOBILITY STATS TAKING THE FULL PANEL (No censoring restrictions), but also looking at minimum spell length restrictions
//============================================




capture program drop report_repeatstat_exe
program define report_repeatstat_exe
			args letter
	
	
	cap n putexcel set "${filename}.xls", sheet("${sheetname1}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname1}")

	
	
	*putexcel C1=("llbfr_llaft_aft")
	/*
	putexcel C2=
	putexcel C3=
	putexcel C4=
	putexcel C5=
	*/
	putexcel `letter'2=(r(stay_stay))
	putexcel `letter'3=(r(move_move))
	putexcel `letter'4=(r(stay_stay_raw))
	putexcel `letter'5=(r(move_move_raw))
	putexcel `letter'6=(r(move_move_back))
	putexcel `letter'7=(r(move_move_back_raw))
	

	cap n putexcel set "${filename}.xls", sheet("${sheetname2}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname2}")

	
	
*putexcel `letter'1=("occ")
local j=2
matrix tempmat=r(stay_stay_occ)
putexcel `letter'`j'=matrix(tempmat)

local j=1+`j'+$no_occs
matrix tempmat=r(move_move_occ)
putexcel `letter'`j'=matrix(tempmat)

local j=1+`j'+$no_occs
matrix tempmat=r(stay_stay_raw_occ)
putexcel `letter'`j'=matrix(tempmat)

local j=1+`j'+$no_occs
matrix tempmat=r(move_move_raw_occ)
putexcel `letter'`j'=matrix(tempmat)

local j=1+`j'+$no_occs
matrix tempmat=r(move_move_back_occ)
putexcel `letter'`j'=matrix(tempmat)

local j=1+`j'+$no_occs
matrix tempmat=r(move_move_back_raw_occ)
putexcel `letter'`j'=matrix(tempmat)

end 



do "${step1codedir}/Ginv_matrices.do"
do "${step2codedir}/aux_directory/aux_programs.do"


tab locc1bfr_mmo, matcell(occtemp)
occmat_dim_exe locc1bfr_mmo
matrix occmat=r(occno_matrix)

capture drop llocc1bfr_mmo
capture drop llocc1aft_mmo
sort personkey yearmonth
gen llocc1bfr_mmo=locc1bfr_mmo[_n-1] if personkey==personkey[_n-1] 
gen llocc1aft_mmo=locc1aft_mmo[_n-1] if personkey==personkey[_n-1] 




matrix GGinvm=Ginv_mm#Ginv_mm
matrix GGGinvm=GGinvm#Ginv_mm'


* matrix with observed doube-occupational 
global no_occs=rowsof(Ginv_mm)
global no_kron_occs=$no_occs * $no_occs * $no_occs



global filename "${tempdata}/repeat_mobility_sipp_panel"
	*global filename_maintable "C:\data\section_2_2_regressions_mm"
	global sheetname1 "summ_basic_mmo"
	global sheetname2 "occspec_basic_mmo"
	
	*global sheetname2 "`scjf'_`name'_agg"
	*global sheetname3 "`scjf'_`name'_occspec"
	*global sheetname4 "`scjf'_`name'_tramo"
	
	global sheet1counter=1
	global sheet2counter=1
	global sheet3counter=1
	global sheet4counter=1
	
	cap n putexcel set "${filename}.xls", replace 
	cap n putexcel set "${filename}.xls", sheet("${sheetname1}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname1}")

putexcel A1=("measure")
putexcel B1=("description")
putexcel C1=("llbfr_llaft_aft_basic_cnun_after_cu")
putexcel D1=("llbfr_llaft_aft_cnun_after_cnun")
putexcel E1=("llbfr_llaft_aft_cu_after_cu")
putexcel F1=("llbfr_llaft_aft_basic_yng")
putexcel G1=("llbfr_llaft_aft_basic_prm")
putexcel H1=("llbfr_lbfr_aft_basic")
putexcel I1=("llbfr_lbfr_aft_basic_yng")
putexcel J1=("llbfr_lbfr_aft_basic_prm")
putexcel K1=("llbfr_llaft_aft_basic_noconstr")
putexcel L1=("llbfr_llaft_aft_basic_yng_noconstr")
putexcel M1=("llbfr_llaft_aft_basic_prm_noconstr")
putexcel N1=("llbfr_lbfr_aft_basic_noconstr")
putexcel O1=("llbfr_lbfr_aft_basic_yng_noconstr")
putexcel P1=("llbfr_lbfr_aft_basic_prm_noconstr")




	putexcel B2=("cnun after nun, 48 months window, wave<=12")
	
	putexcel A2=("sas CORR")
	putexcel A3=("mam CORR")
	putexcel A4=("sas raw")
	putexcel A5=("mam raw")
	putexcel A6=("mamoveback CORR")
	putexcel A7=("mamoveback raw")
	
	cap n putexcel set "${filename}.xls", sheet("${sheetname2}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname2}")

putexcel A1=("occ")
local j=2
forvalues i=1(1)$no_occs {
local occnum=occmat[`i',1]
putexcel A`j'=("sas_`occnum'")
local j=1+`j'
}

local j=1+`j'

forvalues i=1(1)$no_occs {
local occnum=occmat[`i',1]
putexcel A`j'=("mam_`occnum'")
local j=1+`j'
}

local j=1+`j'

forvalues i=1(1)$no_occs {
local occnum=occmat[`i',1]
putexcel A`j'=("sasr_`occnum'")
local j=1+`j'

}

local j=1+`j'

forvalues i=1(1)$no_occs {
local occnum=occmat[`i',1]
putexcel A`j'=("mamr_`occnum'")
local j=1+`j'
}

local j=4*(${no_occs}+1)+1
local j=1+`j'

forvalues i=1(1)$no_occs {
local occnum=occmat[`i',1]
putexcel A`j'=("mamback_`occnum'")
local j=1+`j'
}

local j=1+`j'

forvalues i=1(1)$no_occs {
local occnum=occmat[`i',1]
putexcel A`j'=("mambackr_`occnum'")
local j=1+`j'
}

putexcel C1=("llbfr_llaft_aft_basic_cnun_after_cu")
putexcel D1=("llbfr_llaft_aft_cnun_after_cnun")
putexcel E1=("llbfr_llaft_aft_cu_after_cu")
putexcel F1=("llbfr_llaft_aft_basic_yng")
putexcel G1=("llbfr_llaft_aft_basic_prm")
putexcel H1=("llbfr_lbfr_aft_basic")
putexcel I1=("llbfr_lbfr_aft_basic_yng")
putexcel J1=("llbfr_lbfr_aft_basic_prm")
putexcel K1=("llbfr_llaft_aft_basic_noconstr")
putexcel L1=("llbfr_llaft_aft_basic_yng_noconstr")
putexcel M1=("llbfr_llaft_aft_basic_prm_noconstr")
putexcel N1=("llbfr_lbfr_aft_basic_noconstr")
putexcel O1=("llbfr_lbfr_aft_basic_yng_noconstr")
putexcel P1=("llbfr_lbfr_aft_basic_prm_noconstr")


// BASIC, WHICH MEANS HERE CNUN FOLLOWING CU
global noagricconstr12w "& wave<=12"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic 18

report_repeatstat_exe C

** counting
display "${repeat_ifcond}"
ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w}
ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} ///
				& locc1bfr_mmo!=45 & locc1bfr_mmo[_n-1]!=45
ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} ///
				& locc1bfr_mmo!=45 & locc1bfr_mmo[_n-1]!=45 & locc1bfr_mmo!=47 & locc1bfr_mmo[_n-1]!=47
ci lne_c_mmo if personkey==personkey[_n-1] & lne_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_nunspell[_n-1]==1 ${noagricconstr12w} ///
				& locc1bfr_mmo!=45 & locc1bfr_mmo[_n-1]!=45 & locc1bfr_mmo!=47 & locc1bfr_mmo[_n-1]!=47

				
// BASIC BUT CNUN FOLLOWING CNUN
global noagricconstr12w "& wave<=12"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_nunspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic_nun 18

report_repeatstat_exe D



// BASIC BUT CU FOLLOWING CU
global noagricconstr12w "& wave<=12"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_uspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic_cu 18

report_repeatstat_exe E


** counting 
display "${repeat_ifcond}"
ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_uspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w}
ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_uspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} ///
				& locc1bfr_mmo!=45 & locc1bfr_mmo[_n-1]!=45
ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_uspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} ///
				& locc1bfr_mmo!=45 & locc1bfr_mmo[_n-1]!=45 & locc1bfr_mmo!=47 & locc1bfr_mmo[_n-1]!=47

ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]==1 & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_uspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} ///
				& locc1bfr_mmo!=45 & locc1bfr_mmo[_n-1]!=45 & locc1bfr_mmo!=47 & locc1bfr_mmo[_n-1]!=47
ci lue_c_mmo if personkey==personkey[_n-1] & lue_c_mmo[_n-1]==0 & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_uspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} ///
				& locc1bfr_mmo!=45 & locc1bfr_mmo[_n-1]!=45 & locc1bfr_mmo!=47 & locc1bfr_mmo[_n-1]!=47
				
				
// BASIC YOUNG
global noagricconstr12w "& wave<=12 & age_2dum==1"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic_yng 18

report_repeatstat_exe F

// BASIC PRM
global noagricconstr12w "& wave<=12 & age_2dum==2"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic_prm 18

report_repeatstat_exe G


// BASIC LOCC1BFR_MMO IN THE MIDDLE
global noagricconstr12w "& wave<=12"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo locc1bfr_mmo locc1aft_mmo mmo Ginv_mm basic_lbfr 18

report_repeatstat_exe H

// BASIC LOCC1BFR_MMO IN THE MIDDLE - YOUNG
global noagricconstr12w "& wave<=12 & age_2dum==1"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo locc1bfr_mmo locc1aft_mmo mmo Ginv_mm basic_lbfr_yng 18

report_repeatstat_exe I

// BASIC LOCC1BFR_MMO IN THE MIDDLE - PRIME
global noagricconstr12w "& wave<=12 & age_2dum==2"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe llocc1bfr_mmo locc1bfr_mmo locc1aft_mmo mmo Ginv_mm basic_lbfr_prm 18

report_repeatstat_exe J



// BASIC, WHICH MEANS HERE CNUN FOLLOWING CU, NO CONSTRUCTION
global noagricconstr12w "& wave<=12"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe_2excl llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic_noconstr 18 19

report_repeatstat_exe K


// BASIC, WHICH MEANS HERE CNUN FOLLOWING CU, NO CONSTRUCTION, YOUNG
global noagricconstr12w "& wave<=12 & age_2dum==1"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe_2excl llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic_noconstr_yng 18 19

report_repeatstat_exe L

// BASIC, WHICH MEANS HERE CNUN FOLLOWING CU, NO CONSTRUCTION, PRIME
global noagricconstr12w "& wave<=12 & age_2dum==2"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe_2excl llocc1bfr_mmo llocc1aft_mmo locc1aft_mmo mmo Ginv_mm basic_noconstr_prm 18 19

report_repeatstat_exe M




// BASIC LOCC1BFR_MMO IN THE MIDDLE
global noagricconstr12w "& wave<=12"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe_2excl llocc1bfr_mmo locc1bfr_mmo locc1aft_mmo mmo Ginv_mm basic_lbfr 18 19

report_repeatstat_exe N

// BASIC LOCC1BFR_MMO IN THE MIDDLE - YOUNG
global noagricconstr12w "& wave<=12 & age_2dum==1"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe_2excl llocc1bfr_mmo locc1bfr_mmo locc1aft_mmo mmo Ginv_mm basic_lbfr_yng 18 19

report_repeatstat_exe O

// BASIC LOCC1BFR_MMO IN THE MIDDLE - PRIME
global noagricconstr12w "& wave<=12 & age_2dum==2"                     
sort personkey yearmonth
global repeat_ifcond=  " & personkey==personkey[_n-1] & lue_c_mmo[_n-1]!=. & sample_timetogo+interview_no2>=48 & entry_ind==1 & complete_nunspell==1 & complete_uspell[_n-1]==1 ${noagricconstr12w} "

repeat_stats_calc_exe_2excl llocc1bfr_mmo locc1bfr_mmo locc1aft_mmo mmo Ginv_mm basic_lbfr_prm 18 19

report_repeatstat_exe P


*****************************************
** CALCULATION OF MOBILITY
******************************************
import excel "${tempdata}/repeat_mobility_sipp_panel.xls", sheet("summ_basic_mmo") firstrow clear
 xpose, clear varname format(%6.2f)
format v* %6.3f
 ren v1 sas_corr
ren v2 mam_corr
ren v3 sas_raw
ren v4 mam_raw
ren v5 movebackam_corr
ren v6 movebackam_raw
ren _varname measure
drop if _n==1
drop if _n==2
gen move3rdoccam_corr=mam_corr-movebackam_corr
gen move3rdoccam_raw=mam_corr-movebackam_raw

** OUR MOVE AFTER MOVE MEASURE CONSIDERS ALL WORKERS WHO MOVED IN THE FIRST SPELL
	** AND DID NOT MOVE BACK IN THE DENOMINATOR. 
gen mam_prefmeasure_corr=move3rdoccam_corr/(1-movebackam_corr)
gen mam_prefmeasure_raw=move3rdoccam_raw/(1-movebackam_raw)

format mo* %6.3f
format ma* %6.3f

replace measure="repmob_all" if measure=="llbfr_llaft_aft_basic_noconstr"
replace measure="repmob_yng" if measure=="llbfr_llaft_aft_basic_yng_nocons"
replace measure="repmob_prm" if measure=="llbfr_llaft_aft_basic_prm_nocons"

drop if (measure!="repmob_all" & measure!="repmob_yng" & measure!="repmob_prm" )

order measure, first

gen sas_prefmeasure_corr=sas_corr
format sas_prefmeasure_corr %6.3f
order mam_prefmeasure_corr, after(measure)
order sas_prefmeasure_corr, after(measure)

export delimited using "${mainresultsdir}/repeatmobility.csv", replace
save "${mainresultsdir}/repeatmobility.dta", replace



********************************************************************************
	global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"