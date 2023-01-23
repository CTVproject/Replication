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
***  		STATISTICS OF THE INCOMPLETE UNEMPLOYMENT DURATION DISTRIBUTION
***							AND ITS CYCLICAL BEHAVIOR 
***
********************************************************************************

/* do-file contains a bunch of xls spreadsheets produced to investigate robustness
these are commented out in the present run */

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
	


use "${outputdata}/reduced_u_n_ctv.dta"
cd "${tempdata}"

global locfileversion=1222

****************************************
** DEFAULT SELECTION CRITERIA (GLOBALS)
****************************************


global sttg = "3"
global epanbwidth=3


global wavecond " & wave>4 & interview_no>14 & sample_timetogo>3"
global mwavecond " & wave>1 & interview_no>4 & sample_timetogo>3"
	* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
global jfwavecond "& wave>1 & interview_no>4 & sample_timetogo>18-n_spellength "	// this 
global sttg "1"
global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "



**********************
** PROGRAMS AND SOME MANIP
********************** 


** auxiliary programs
do "${step2codedir}/aux_directory/aux_programs.do"
			/* programs:
					xsnet_calcx
					occmat_dim_exe	
			*/



capture drop ms2
gen ms2=ms
replace ms2=1 if ms==2

capture drop agebin
gen agebin=1 if tage>=20 & tage<=30
replace agebin=2 if tage>=35 & tage<=55
			
			
			
********************************************************************************
** 2.1 RELATIVE DURATIONS IN THE MAIN TEXT, NO CUTOFF FOR DURATION
********************************************************************************

			
			
//XXXXX
global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<."
global wavecond " & wave>5 & interview_no2>20 & sample_timetogo>=0 & durdistr_stability==1"
global hp_lunrate_var "hp_lunrate_bls" 		// unemployment variable
global hp_lunrate_tt " quarter"    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


cap lab define stmv_label 0 "Stayers" 1 "Movers"
cap lab val lne_c_mmo lue_c_mmo stmv_label  
cap lab define badgoodlabel -1 "Times of High U" 0 "Neither" 1 "Times of Low U"
cap lab val rec_exp_ind3 badgoodlabel 

version 17
cap n set trace off

quietly {
cap log close regresultslog5
log using "${mainresultsdir}/text_u_durations_comparison.txt", replace text name(regresultslog5)

noisily: display  ""
noisily: display  "--------------------------------------------------------"
noisily: display  "Unemployment Durations of Movers vs Stayers across Cycle"
noisily: display  "--------------------------------------------------------"
noisily: display  ""

//xxxxx
noisily: table rec_exp_ind3 lne_c_mmo [aw=wpfinwgt] if  complete_uspell==1  $addcondition_cycle  ${wavecond} ${ndur_bound}, stat(mean n_spellength) nformat(%5.3f)
//xxxxx

noisily: display  ""
noisily: display  "note: here we use unrestricted durations, but require that"
noisily: display  "workers has more than 20 months in sample at moment of "
noisily: display  "finding a job. In calibration model *and* data we focus on"
noisily: display  "spells between 1 and 18 months"

log close regresultslog5
	

}		

 
 
			 // ROBUSTNESS
			global wavecond " & wave>5 & interview_no2>20 & sample_timetogo>=0 & durdistr_stability==1"
			global ndur_bound " & n_spellength>=1 & n_spellength<."	
			global add_condition1 " $wavecond "	
			 
			 
			table rec_exp_ind3 lue_c_mmo [aw=wpfinwgt] if  lue_c_mmo!=. ${wavecond} ${ndur_bound}, stat(mean u_spellength) nformat(%5.3f)
			
			
			
			
********************************************************************************
** 2.2 RELATIVE DURATIONS (CROSS SECTION) IN CALIBRATION, CUTOFF <=18 MTHS DUR
********************************************************************************


/*
// UNEMPLOYMENT DURATION TABLE  :::: XSECTION
version 13
global filename_xsection "${allresultsdir}/udur_moverstayer_xsection_robustness.xls"


			capture program drop robusttable_udur_xsection_exe
			program define robusttable_udur_xsection_exe
						args sheetname

			cap putexcel set "${filename_xsection}", sheet("`sheetname'", replace)
			cap n putexcel set "${filename_xsection}", modify sheet("`sheetname'", replace)




			tokenize B C D E F G H I J K L M N
			forvalues i=1(1)9 {

			cap n putexcel ``i''1=("(`i')")
			}
			
			display "write A column"
			
			cap n putexcel A2=("Average Predicted U. Duration")
			cap n putexcel A4=("Average U. Duration Occ. Stayers")
			cap n putexcel A6=("Average U. Duration Occ. Movers")
			cap n putexcel A8=(" Coefficient Occ. Move Dummy")
			cap n putexcel A10=("Occ.Move x Prime-Age Dum.")

			cap n putexcel A12=("Worker's Characteristics")
			cap n putexcel A13=("Age ")
			cap n putexcel A14=("Source Occupation Dummies")
			cap n putexcel A15=("Destination Occupation Dummies")
			cap n putexcel A16=("Interactions with Reallocation")
			cap n putexcel A17=("F-test Interactions (p-value) ")
			cap n putexcel A18=("F-test Age x Reallocation ")
			cap n putexcel A19=("F-test Worker Char x Reall.")
			cap n putexcel A20=("F-test Occupation x Reall. ")
			*putexcel A21=("F-test Destination Occ x Reall.")
			cap n putexcel A21=("Number of Observations")

			// average -- column B 
			su u_spellength [w=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			putexcel B21=(r(N))
			putexcel B2=(round(r(mean),.01))

			reg u_spellength  [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			matrix regreturn=r(table)
			local temp_se=round(regreturn[2,1], .001)
			putexcel B3=("(`temp_se')")

			// by occupational stayers -- Column C
			reg u_spellength  lue_c_mmo [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			putexcel C21=(e(N)) 
			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,1], .001)
			local temp_se=round(regreturn[2,1], .001)
			putexcel C8=("`temp_b'")
			putexcel C9=("(`temp_se')")


			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel C4=("`temp_b'")
			putexcel C5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)

			putexcel C6=("`temp_b'")
			putexcel c7=("(`temp_se')")

			// occ mov dummy and worker's characteristics - Column D
			reg u_spellength  lue_c_mmo i.sex i.race i.ms2 i.educ2  [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			putexcel D21=(e(N))
			putexcel D12=("X")
			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,1], .001)
			local temp_se=round(regreturn[2,1], .001)
			putexcel D8=("`temp_b'")
			putexcel D9=("(`temp_se')")




			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel D4=("`temp_b'")
			putexcel D5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel D6=("`temp_b'")
			putexcel D7=("(`temp_se')")


			reg u_spellength  lue_c_mmo i.sex i.race i.ms2 i.educ2  lue_c_mmo#i.sex lue_c_mmo#i.race ///
			lue_c_mmo#i.ms2 lue_c_mmo#i.educ2 [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1

			test 2.sex#1.lue_c_mmo=0

			test 2.race#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.race#1.lue_c_mmo=0, accumulate
			}

			test 3.ms2#1.lue_c_mmo=0, accumulate
			forvalues i=4(1)5 {
			test `i'.ms2#1.lue_c_mmo=0, accumulate
			}

			test 2.educ2#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.educ2#1.lue_c_mmo=0, accumulate
			}

			putexcel D19=(r(p))



			// occ mov dummy and worker's characteristics & age - Column E
			reg u_spellength  lue_c_mmo i.sex i.race i.ms2 i.educ2  i.agebin [pw=pweight2] ///
			 if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			*reg u_spellength  lue_c_mmo i.sex i.race i.ms2 i.educ2  c.tage##c.tage##c.tage##c.tage [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			putexcel e21=(e(N))
			putexcel E12=("X")
			putexcel E13=("X")

			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,1], .001)
			local temp_se=round(regreturn[2,1], .001)
			putexcel E8=("`temp_b'")
			putexcel E9=("(`temp_se')")

			 

			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel E4=("`temp_b'")
			putexcel E5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel e6=("`temp_b'")
			putexcel e7=("(`temp_se')")

			reg u_spellength  lue_c_mmo i.sex i.race i.ms2 i.educ2  i.agebin lue_c_mmo#ib2.agebin  ///
			lue_c_mmo#i.sex lue_c_mmo#i.race lue_c_mmo#i.ms2 lue_c_mmo#i.educ2 [pw=pweight2] ///
			if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1

			test 2.sex#1.lue_c_mmo=0

			test 2.race#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.race#1.lue_c_mmo=0, accumulate
			}

			test 3.ms2#1.lue_c_mmo=0, accumulate
			forvalues i=4(1)5 {
			test `i'.ms2#1.lue_c_mmo=0, accumulate
			}

			test 2.educ2#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.educ2#1.lue_c_mmo=0, accumulate
			}

			putexcel E19=(r(p))

			
			// age bins (captured noisily because we could restrict age groups) 


			su agebin if agebin!=. $add_condition1
			if r(min)<r(max) {
			cap n test 1.agebin#1.lue_c_mmo=0
			cap n putexcel E18=(r(p))
			}

			
			// occ mov, work char + age + source occ
			reg u_spellength  lue_c_mmo i.locc1bfr_mmo i.sex i.race i.ms_sum i.educ2  [pw=pweight2] ///
			if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 & sample_timetogo>$sttg $ndur_bound $add_condition1

			*reg u_spellength  lue_c_mmo i.locc1bfr_mmo i.sex i.race i.ms2 i.educ2  i.agebin [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $wavecond
			*reg u_spellength  lue_c_mmo i.locc1bfr_mmo i.sex i.race i.ms2 i.educ2  c.tage##c.tage##c.tage##c.tage [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			putexcel F21=(e(N))
			putexcel F12=("X")
			putexcel F13=("X")
			putexcel F14=("X")

			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,1], .001)
			local temp_se=round(regreturn[2,1], .001)
			putexcel F8=("`temp_b'")
			putexcel F9=("(`temp_se')")



			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel F4=("`temp_b'")
			putexcel F5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel F6=("`temp_b'")
			putexcel F7=("(`temp_se')")



			occmap_exe locc1bfr_mmo if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			matrix occmap_mmo=r(mappingmatrix)
			global occdimtemp=r(dim)
			matrix list occmap_mmo


			reg u_spellength  lue_c_mmo i.sex i.race i.ms2 i.educ2  i.locc1bfr_mmo i.locc1bfr_mmo#i.lue_c_mmo i.agebin lue_c_mmo#ib2.agebin  lue_c_mmo#i.sex lue_c_mmo#i.race lue_c_mmo#i.ms2 lue_c_mmo#i.educ2 [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			*reg u_spellength  lue_c_mmo i.sex i.race i.ms2 i.educ2  i.locc1bfr_mmo i.locc1bfr_mmo#i.lue_c_mmo i.agebin lue_c_mmo#ib2.agebin  lue_c_mmo#i.sex lue_c_mmo#i.race lue_c_mmo#i.ms2 lue_c_mmo#i.educ2 [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45

			test 2.sex#1.lue_c_mmo=0

			test 2.race#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.race#1.lue_c_mmo=0, accumulate
			}

			test 3.ms2#1.lue_c_mmo=0, accumulate
			forvalues i=4(1)5 {
			test `i'.ms2#1.lue_c_mmo=0, accumulate
			}

			test 2.educ2#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.educ2#1.lue_c_mmo=0, accumulate
			}

			putexcel F19=(r(p))

			su agebin if agebin!=. $add_condition1
			if r(min)<r(max) {
			cap n test 1.agebin#1.lue_c_mmo=0
			cap n putexcel F18=(r(p))
			}


			test 1.lue_c_mmo#11b.locc1bfr_mmo=1.lue_c_mmo#13.locc1bfr_mmo
			forvalues i=3(1)$occdimtemp {
			local tempnum=occmap_mmo[`i',1]
			local varintact="1.lue_c_mmo#`tempnum'.locc1bfr_mmo"
			test 1.lue_c_mmo#11b.locc1bfr_mmo=`varintact', accumulate
			}

			putexcel F20=(r(p))



			// occ mov, work char + age + dest occ
			reg u_spellength  lue_c_mmo i.locc1aft_mmo i.sex i.race i.ms2 i.educ2  i.agebin [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			*reg u_spellength  lue_c_mmo i.locc1aft_mmo i.sex i.race i.ms2 i.educ2  c.tage##c.tage##c.tage##c.tage [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			putexcel G21=(e(N))
			putexcel G12=("X")
			putexcel G13=("X")
			putexcel G15=("X")

			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,1], .001)
			local temp_se=round(regreturn[2,1], .001)
			putexcel G8=("`temp_b'")
			putexcel G9=("(`temp_se')")



			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel G4=("`temp_b'")
			putexcel G5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel G6=("`temp_b'")
			putexcel G7=("(`temp_se')")


			reg u_spellength  i.agebin lue_c_mmo i.locc1aft_mmo i.locc1aft_mmo#lue_c_mmo i.sex i.race i.ms2 i.educ2 lue_c_mmo#i.sex lue_c_mmo#i.race lue_c_mmo#i.ms2 lue_c_mmo#i.educ2  [pw=pweight2] if lue_c_mmo!=. & locc1aft_mmo!=45 & locc1aft_mmo!=45
				

			occmap_exe locc1aft_mmo if lue_c_mmo!=. & locc1aft_mmo!=45 & locc1aft_mmo!=45
			matrix occmap_mmo=r(mappingmatrix)
			global occdimtemp=r(dim)
			matrix list occmap_mmo

			test 1.lue_c_mmo#11b.locc1aft_mmo=1.lue_c_mmo#13.locc1aft_mmo
			forvalues i=3(1)$occdimtemp {
			local tempnum=occmap_mmo[`i',1]
			local varintact="1.lue_c_mmo#`tempnum'.locc1aft_mmo"
			test 1.lue_c_mmo#11b.locc1aft_mmo=`varintact', accumulate
			}

			putexcel G20=(r(p))



			*test 2.agebin#1.lue_c_mmo=0
			*putexcel G18=(r(p))


				test 2.sex#1.lue_c_mmo=0

				test 1.race#1.lue_c_mmo=0, accumulate
				forvalues i=2(1)4 {
				test `i'.race#1.lue_c_mmo=0, accumulate
				}

				test 1.ms2#1.lue_c_mmo=0, accumulate
				forvalues i=2(1)5 {
				cap n test `i'.ms2#1.lue_c_mmo=0, accumulate
				}

				test 1.educ2#1.lue_c_mmo=0, accumulate
				forvalues i=2(1)4 {
				cap n test `i'.educ2#1.lue_c_mmo=0, accumulate
				}
				return list	

				display r(p)
				putexcel G19=(r(p))




			// occ mov, work char + age + dest occ
			reg u_spellength  lue_c_mmo i.locc1bfr_mmo  i.locc1aft_mmo i.sex i.race i.ms2 i.educ2  i.agebin ///
			 [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			*reg u_spellength  lue_c_mmo i.locc1bfr_mmo  i.locc1aft_mmo i.sex i.race i.ms2 i.educ2  c.tage##c.tage##c.tage##c.tage [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			putexcel H21=(e(N))
			putexcel H12=("X")
			putexcel H13=("X")
			putexcel H14=("X")
			putexcel H15=("X")

			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,1], .001)
			local temp_se=round(regreturn[2,1], .001)
			putexcel H8=("`temp_b'")
			putexcel H9=("(`temp_se')")



			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel H4=("`temp_b'")
			putexcel H5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel H6=("`temp_b'")
			putexcel H7=("(`temp_se')")

			reg u_spellength  i.agebin lue_c_mmo i.locc1bfr_mmo  i.locc1aft_mmo i.locc1aft_mmo#lue_c_mmo ///
			 i.sex i.race i.ms2 i.educ2 lue_c_mmo#i.sex lue_c_mmo#i.race lue_c_mmo#i.ms2 lue_c_mmo#i.educ2  ///
			 [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1

			occmap_exe locc1bfr_mmo if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			matrix occmap_mmo=r(mappingmatrix)
			global occdimtemp=r(dim)
			matrix list occmap_mmo

			test 1.lue_c_mmo#11b.locc1aft_mmo=1.lue_c_mmo#13.locc1aft_mmo
			forvalues i=3(1)$occdimtemp {
			local tempnum=occmap_mmo[`i',1]
			local varintact="1.lue_c_mmo#`tempnum'.locc1aft_mmo"
			test 1.lue_c_mmo#11b.locc1aft_mmo=`varintact', accumulate
			}

			putexcel H20=(r(p))



			*test 2.agebin#1.lue_c_mmo=0
			*putexcel H18=(r(p))

			cap n test 1.sex#1.lue_c_mmo=0
			cap n test 2.sex#1.lue_c_mmo=0

			cap n test 1.race#1.lue_c_mmo=0, accumulate
			forvalues i=2(1)4 {
			cap n test `i'.race#1.lue_c_mmo=0, accumulate
			}

			cap n test 1.ms2#1.lue_c_mmo=0, accumulate
			forvalues i=2(1)5 {
			cap n test `i'.ms2#1.lue_c_mmo=0, accumulate
			}

			cap n test 1.educ2#1.lue_c_mmo=0, accumulate
			forvalues i=2(1)4 {
			cap n test `i'.educ2#1.lue_c_mmo=0, accumulate
			}
				
			putexcel H19=(r(p))



			// INCLUDE PRIME-AGE DUMMY 
			reg u_spellength  lue_c_mmo##i.agebin i.locc1bfr_mmo  i.locc1aft_mmo i.sex i.race i.ms2 ///
			 i.educ2  [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			putexcel I21=(e(N))
			putexcel I12=("X")
			putexcel I13=("X")
			putexcel I14=("X")
			putexcel I15=("X")

			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,2], .001)
			local temp_se=round(regreturn[2,2], .001)
			putexcel I8=("`temp_b'")
			putexcel I9=("(`temp_se')")
			local temp_b=round(regreturn[1,8], .001)
			local temp_se=round(regreturn[2,8], .001)
			putexcel I10=("`temp_b'")
			putexcel I11=("(`temp_se')")



			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1],.001)
			putexcel I4=("`temp_b'")
			putexcel I5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel I6=("`temp_b'")
			putexcel I7=("(`temp_se')")

			// F-tests 
			reg u_spellength  i.agebin##lue_c_mmo i.locc1bfr_mmo  i.locc1aft_mmo i.locc1aft_mmo#lue_c_mmo ///
			 i.sex i.race i.ms2 i.educ2 lue_c_mmo#i.sex lue_c_mmo#i.race lue_c_mmo#i.ms2 lue_c_mmo#i.educ2  [pw=pweight2] ///
			 if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1

			occmap_exe locc1bfr_mmo if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			matrix occmap_mmo=r(mappingmatrix)
			global occdimtemp=r(dim)
			matrix list occmap_mmo

			test 1.lue_c_mmo#11b.locc1aft_mmo=1.lue_c_mmo#13.locc1aft_mmo
			forvalues i=3(1)$occdimtemp {
			local tempnum=occmap_mmo[`i',1]
			local varintact="1.lue_c_mmo#`tempnum'.locc1aft_mmo"
			test 1.lue_c_mmo#11b.locc1aft_mmo=`varintact', accumulate
			}

			putexcel I20=(r(p))


			su agebin if agebin!=. $add_condition1
			if r(min)<r(max) {
			cap n test 2.agebin#1.lue_c_mmo=0
			cap n putexcel I18=(r(p))
			}
			
			


			test 2.sex#1.lue_c_mmo=0

			test 2.race#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.race#1.lue_c_mmo=0, accumulate
			}

			test 3.ms2#1.lue_c_mmo=0, accumulate
			forvalues i=4(1)5 {
			test `i'.ms2#1.lue_c_mmo=0, accumulate
			}

			test 2.educ2#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.educ2#1.lue_c_mmo=0, accumulate
			}

			putexcel I19=(r(p))


			/*
			reg u_spellength  lue_c_mmo##i.agebin i.locc1bfr_mmo  i.locc1aft_mmo i.locc1bfr_mmo#lue_c_mmo i.sex i.race i.ms2 i.educ2  [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45, vce(robust)


			test 1.lue_c_mmo#11b.locc1bfr_mmo=1.lue_c_mmo#13.locc1bfr_mmo
			forvalues i=3(1)$occdimtemp {
			local tempnum=occmap_mmo[`i',1]
			local varintact="1.lue_c_mmo#`tempnum'.locc1bfr_mmo"
			test 1.lue_c_mmo#11b.locc1bfr_mmo=`varintact', accumulate
			}
			*/


			/* note that using interaction with source occupations seems to give the same results as with destination occupations ???
			*/


			reg u_spellength  lue_c_mmo##i.agebin i.locc1bfr_mmo  i.locc1aft_mmo i.locc1bfr_mmo ///
			i.sex##lue_c_mmo i.race##lue_c_mmo i.ms2##lue_c_mmo i.educ2##lue_c_mmo  [pw=pweight2] ///
			if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1, vce(robust)


			test 2.sex#1.lue_c_mmo=0

			test 2.race#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.race#1.lue_c_mmo=0, accumulate
			}

			test 3.ms2#1.lue_c_mmo=0, accumulate
			forvalues i=4(1)5 {
			test `i'.ms2#1.lue_c_mmo=0, accumulate
			}

			test 2.educ2#1.lue_c_mmo=0, accumulate
			forvalues i=3(1)4 {
			test `i'.educ2#1.lue_c_mmo=0, accumulate
			}

			putexcel I19=(r(p))

			**end 

			// REGRESSIONS FOR robustness 
			su u_spellength [w=pweight2] if lue_c_mmo!=.

			reg u_spellength lue_c_mmo [pw=pweight2], vce(robust) 
			margins
			reg u_spellength lue_c_mmo i.sex i.race i.educ2 i.ms c.tage##c.tage##c.tage##c.tage [pw=pweight2], vce(robust)
			reg u_spellength i.lue_c_mmo##i.sex i.lue_c_mmo##i.race i.lue_c_mmo##i.educ2 i.lue_c_mmo##i.ms c.tage##c.tage##c.tage##c.tage [pw=pweight2],vce(robust)

			reg u_spellength i.lue_c_mmo##i.sex i.lue_c_mmo##i.race i.lue_c_mmo##i.educ2 i.lue_c_mmo##i.ms2 i.lue_c_mmo##c.tage##c.tage##c.tage##c.tage [pw=pweight2], vce(robust)
			test 1.lue_c_mmo#2.sex=0
			forvalues i=2(1)4{
			test 1.lue_c_mmo#`i'.race=0, accumulate
			}
			forvalues i=2(1)4{
			test 1.lue_c_mmo#`i'.educ2=0, accumulate
			}
			forvalues i=3(1)5{
			test 1.lue_c_mmo#`i'.ms2=0, accumulate
			}

			end 
			//PPPPPPPPPPPPPPPPPPPPPPPPP
			
global wavecond " & wave>5 & interview_no2>20 & sample_timetogo>=0 & durdistr_stability==1"
global ndur_bound " & n_spellength>=1 & n_spellength<."
global add_condition1 " $wavecond "

//XXXXXXXXXX
robusttable_udur_xsection_exe mmo_w5c_dstab
//XXXXXXXXXX
*/





********* FILE FOR MAIN RESULTS ****


global wavecond " & wave>5 & interview_no2>20 & sample_timetogo>=0 & durdistr_stability==1"
global ndur_bound " & n_spellength>=1 & n_spellength<."
global add_condition1 " $wavecond "


global filename_xsection "${mainresultsdir}/table2_panelA_rel_udur_moverstayer.xlsx"
global sheetname "calib"
			cap n putexcel set "${filename_xsection}"
			cap n putexcel set "${filename_xsection}", sheet("${sheetname}", replace)
			cap n putexcel set "${filename_xsection}", modify sheet("${sheetname}", replace)



			display "write A column"
			
			
			cap n putexcel A4=("Average U. Duration Occ. Stayers")
			cap n putexcel A6=("Average U. Duration Occ. Movers")
			cap n putexcel A8=(" Coefficient Occ. Move Dummy")
			
			cap n putexcel A12=("Worker's Characteristics")
			cap n putexcel A13=("Age ")
			cap n putexcel A14=("Source Occupation Dummies")
			cap n putexcel A15=("Destination Occupation Dummies")
			*putexcel A21=("F-test Destination Occ x Reall.")
			cap n putexcel A16=("Number of Observations")


			putexcel D1=("RELATIVE U. DURATIONS")
			putexcel D2=("(controlling for demog, age, occs)")
// occ mov, work char + age + dest occ
			reg u_spellength  lue_c_mmo i.locc1bfr_mmo  i.locc1aft_mmo i.sex i.race i.ms2 i.educ2  i.agebin ///
			 [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45 $ndur_bound $add_condition1
			*reg u_spellength  lue_c_mmo i.locc1bfr_mmo  i.locc1aft_mmo i.sex i.race i.ms2 i.educ2  c.tage##c.tage##c.tage##c.tage [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=45 & locc1aft_mmo!=45
			putexcel D16=(e(N))
			putexcel D12=("X")
			putexcel D13=("X")
			putexcel D14=("X")
			putexcel D15=("X")

			matrix regreturn=r(table)
			local temp_b=round(regreturn[1,1], .001)
			local temp_se=round(regreturn[2,1], .001)
			putexcel D8=("`temp_b'")
			putexcel D9=("(`temp_se')")



			margins if lue_c_mmo==0
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel D4=("`temp_b'")
			putexcel D5=("(`temp_se')")
			margins if lue_c_mmo==1
			matrix margreturn=r(table)
			local temp_b=round(margreturn[1,1], .001)
			local temp_se=round(margreturn[2,1], .001)
			putexcel D6=("`temp_b'")
			putexcel D7=("(`temp_se')")






						
						
*************************************************************
** 2.3 CYCLICAL PATTERN UNEMPLOYMENT DURATIONS (TABLE 5, PANEL B)
*************************************************************						
	
	
capture drop merge_qtruee
*merge m:1 quarter using "${workingdir}/Aggregate Data/hp_u_all_durwvcearly_ldt_sept2018.dta", gen(merge_qtruee) 
merge m:1 quarter using "${outputdata}/timeseries_u_durwvcearly.dta", gen(merge_qtruee) 
*merge m:1 quarter using "c:\data\ctv\output/timeseries_u_durwvcearly.dta", gen(merge_qtruee)
drop if merge_qtruee==2
drop merge_qtruee


		**************************************
		** PROGRAM 
		**************************************
	/*					
	//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
	capture program drop robusttable_udur_cycle_exe
	program define robusttable_udur_cycle_exe
					args sheetname
	*local name "test"

	sleep 1
	cap n putexcel set "${filename}", sheet("`sheetname'", replace)
	cap n putexcel set "${filename}", modify sheet("`sheetname'", replace)



	// BACK TO SIMPLEST REGRESSIONS, AND SET UP


	tokenize B C D E F G H I J k l m n o p Q R S T
	forvalues i=1(1)6 {
	sleep 1
	putexcel ``i''1=("(`i')")
	}

	forvalues i=8(1)13 {
	sleep 1
	local j=`i'-1
	putexcel ``i''1=("(`j')")
	}

	putexcel A2=("coeff on unemployment occ stayers")
	putexcel A4=("coeff on unemployment occ movers")
	putexcel A6=("difference responsiveness movers/stayers")
	putexcel A8=("p-value diff ")

	putexcel A17=("Worker's Characteristics")
	putexcel A18=("Age ")
	putexcel A19=("Source Occupation Dummies")
	putexcel A20=("Destination Occupation Dummies")
	*putexcel A21=("Interactions with Reallocation")
	*putexcel A17=("F-test Interactions (p-value) ")
	*putexcel A18=("F-test Age x Reallocation ")
	*putexcel A19=("F-test Worker Char x Reall.")
	*putexcel A20=("F-test Occupation x Reall. ")
	*putexcel A21=("F-test Destination Occ x Reall.")
	putexcel A21=("Number of Observations")

	// BASIC responsiveness movers,stayers and differences in responsiveness: BASICS

	reg n_spellength  i.lne_c_mmo i.lne_c_mmo#c.$unrate_var $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	putexcel B21=(e(N))
	putexcel B22=("Basic")
	putexcel B23=(e(cmdline))
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel B2=(round(regreturn[1,3],.001))
	local temp_se=round(regreturn[2,3], .001)
	display "`temp_se'"
	putexcel B3=("(`temp_se')")
	putexcel B4=(round(regreturn[1,4],.001))
	local temp_se=round(regreturn[2,4], .001)
	display "`temp_se'"
	putexcel B5=("(`temp_se')")

	margins lne_c_mmo, dydx($unrate_var) pwcompare post
	matrix margreturn=r(table_vs)
	matrix list margreturn
	putexcel B6=(round(margreturn[1,1],.001))
	local temp_se=round(margreturn[2,1], .001)
	local temp_t=round(margreturn[4,1], .001)
	display "`temp_se'"
	putexcel b7=("(`temp_se')")
	putexcel b8=("`temp_t'")


	// WORKER CHARACTERISTICS responsiveness movers,stayers and differences in responsiveness: W. CHARACTERISTICS 
	reg n_spellength  i.lne_c_mmo i.lne_c_mmo#c.$unrate_var i.sex i.race i.ms2 i.educ2  $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	putexcel C21=(e(N))
	putexcel C24=(e(cmdline))
	putexcel C22=("Work Char")
	putexcel C17=("X")
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel C2=(round(regreturn[1,3],.001))
	local temp_se=round(regreturn[2,3], .001)
	display "`temp_se'"
	putexcel C3=("(`temp_se')")
	putexcel C4=(round(regreturn[1,4],.001))
	local temp_se=round(regreturn[2,4], .001)
	display "`temp_se'"
	putexcel C5=("(`temp_se')")

	margins lne_c_mmo, dydx($unrate_var) pwcompare post
	matrix margreturn=r(table_vs)
	matrix list margreturn
	putexcel C6=(round(margreturn[1,1],.001))
	local temp_se=round(margreturn[2,1], .001)
	local temp_t=round(margreturn[4,1], .001)
	display "`temp_se'"
	putexcel C7=("(`temp_se')")
	putexcel C8=("`temp_t'")

	// WORKER CHAR + AGE 
	reg n_spellength i.lne_c_mmo i.lne_c_mmo#c.$unrate_var i.sex i.race i.ms2 i.educ2 c.tage##c.tage##c.tage##c.tage $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	putexcel d21=(e(N))
	putexcel D25=(e(cmdline))
	putexcel d22=("Work Char +Age")

	putexcel d17=("X")
	putexcel d18=("X")
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel d2=(round(regreturn[1,3],.001))
	local temp_se=round(regreturn[2,3], .001)
	display "`temp_se'"
	putexcel d3=("(`temp_se')")
	putexcel d4=(round(regreturn[1,4],.001))
	local temp_se=round(regreturn[2,4], .001)
	display "`temp_se'"
	putexcel d5=("(`temp_se')")

	margins lne_c_mmo, dydx($unrate_var) pwcompare post
	matrix margreturn=r(table_vs)
	matrix list margreturn
	putexcel D6=(round(margreturn[1,1],.001))
	local temp_se=round(margreturn[2,1], .001)
	local temp_t=round(margreturn[4,1], .001)
	display "`temp_se'"
	putexcel D7=("(`temp_se')")
	putexcel d8=("`temp_t'")

	/*
		reg n_spellength i.lne_c_mmo i.lne_c_mmo#c.$unrate_var lne_c_mmo#i.sex lne_c_mmo#i.race lne_c_mmo#i.ms2 lne_c_mmo#i.educ2 c.tage##c.tage##c.tage##c.tage $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
		& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)

		test 2.sex#1.lne_c_mmo=0

		test 2.race#1.lne_c_mmo=0, accumulate
		forvalues i=3(1)4 {
		test `i'.race#1.lne_c_mmo=0, accumulate
		}

		test 3.ms2#1.lne_c_mmo=0, accumulate
		forvalues i=4(1)5 {
		test `i'.ms2#1.lne_c_mmo=0, accumulate
		}

		test 2.educ2#1.lne_c_mmo=0, accumulate
		forvalues i=3(1)4 {
		test `i'.educ2#1.lne_c_mmo=0, accumulate
		}

		putexcel D19=(r(p))
	*/
	// WORKER CHAR + AGE + SOURCE OCCUPATIONS
	reg n_spellength i.lne_c_mmo i.lne_c_mmo#c.$unrate_var i.sex i.race i.ms2 i.educ2 c.tage##c.tage##c.tage##c.tage i.locc1bfr_mmo $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	putexcel e21=(e(N))
	putexcel e26=(e(cmdline))
	putexcel e22=("w.char+age+s.occ")
	putexcel e17=("X")
	putexcel e18=("X")
	putexcel e19=("X")
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel e2=(round(regreturn[1,3],.001))
	local temp_se=round(regreturn[2,3], .001)
	display "`temp_se'"
	putexcel e3=("(`temp_se')")
	putexcel e4=(round(regreturn[1,4],.001))
	local temp_se=round(regreturn[2,4], .001)
	display "`temp_se'"
	putexcel e5=("(`temp_se')")

	margins lne_c_mmo, dydx($unrate_var) pwcompare post
	matrix margreturn=r(table_vs)
	matrix list margreturn
	putexcel e6=(round(margreturn[1,1],.001))
	local temp_se=round(margreturn[2,1], .001)
	local temp_t=round(margreturn[4,1], .001)
	display "`temp_se'"
	putexcel e7=("(`temp_se')")
	putexcel e8=("`temp_t'")

	// WORKER CHAR + AGE + DEST OCCUPATIONS
	reg n_spellength i.lne_c_mmo i.lne_c_mmo#c.$unrate_var i.sex i.race i.ms2 i.educ2 c.tage##c.tage##c.tage##c.tage i.locc1aft_mmo $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	putexcel f21=(e(N))
	putexcel f27=(e(cmdline))
	putexcel f22=("w.char+age+d.occ")
	putexcel f17=("X")
	putexcel f18=("X")
	putexcel f20=("X")
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel f2=(round(regreturn[1,3],.001))
	local temp_se=round(regreturn[2,3], .001)
	display "`temp_se'"
	putexcel f3=("(`temp_se')")
	putexcel f4=(round(regreturn[1,4],.001))
	local temp_se=round(regreturn[2,4], .001)
	display "`temp_se'"
	putexcel f5=("(`temp_se')")

	margins lne_c_mmo, dydx($unrate_var) pwcompare post
	matrix margreturn=r(table_vs)
	matrix list margreturn
	putexcel f6=(round(margreturn[1,1],.001))
	local temp_se=round(margreturn[2,1], .001)
	local temp_t=round(margreturn[4,1], .001)
	display "`temp_se'"
	putexcel f7=("(`temp_se')")
	putexcel f8=("`temp_t'")


	// WORKER CHAR + AGE + DEST OCCUPATIONS
	reg n_spellength i.lne_c_mmo i.lne_c_mmo#c.$unrate_var i.sex i.race i.ms2 i.educ2 c.tage##c.tage##c.tage##c.tage i.locc1bfr_mmo i.locc1aft_mmo $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	putexcel g21=(e(N))
	putexcel g28=(e(cmdline))
	putexcel g22=("w.char+age+(s+d).occ")
	putexcel g17=("X")
	putexcel g18=("X")
	putexcel g19=("X")
	putexcel g20=("X")
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel g2=(round(regreturn[1,3],.001))
	local temp_se=round(regreturn[2,3], .001)
	display "`temp_se'"
	putexcel g3=("(`temp_se')")
	putexcel g4=(round(regreturn[1,4],.001))
	local temp_se=round(regreturn[2,4], .001)
	display "`temp_se'"
	putexcel g5=("(`temp_se')")

	margins lne_c_mmo, dydx($unrate_var) pwcompare post
	matrix margreturn=r(table_vs)
	matrix list margreturn
	putexcel g6=(round(margreturn[1,1],.001))
	local temp_se=round(margreturn[2,1], .001)
	local temp_t=round(margreturn[4,1], .001)
	display "`temp_se'"
	putexcel g7=("(`temp_se')")
	putexcel g8=("`temp_t'")


	end
	//PPPPPPPPPPPPPPPPPPPPPPPP
	
	
	
global filename "${allresultsdir}/table5_panelB_udur_cycle_robust.xls"	
	
	
//========
//  DURWVCEARLY
//============

capture drop ur_all_durwvcearly_ext2
gen ur_all_durwvcearly_ext2=exp(lur_all_durwvcearly_ext)

su ur_all_durwvcearly_ext2
if r(mean)>1 {
*capture drop unrate2
replace ur_all_durwvcearly_ext2=ur_all_durwvcearly_ext2/100
}
su ur_all_durwvcearly_ext2



global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "ur_all_durwvcearly_ext2" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_urdwvcearl_dlt18 


global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "lur_all_durwvcearly_ext" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_lurdwvcearl_dlt18 



global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "hp_ur_all_durwvcearly_ext" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_hpurdwvcearl_dlt18 


global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "hp_lur_all_durwvcearly_ext" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_hplurdwvcearl_dlt18 
*/

//========
//  DURWVCEARLY - INSTRUMENTED BY LUNRATE OR UNRATE 
//============


*** CREATING THE INSTRUMENTED TIME SERIES 

		** setting up time series analysis
		preserve
		keep quarter lur_all_durwvcearly_ext durdistr_stability
			duplicates drop quarter lur_all_durwvcearly_ext , force
			tsset quarter 
			tsfill
			capture drop merge_qtru
			merge m:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", keepusing(lunrate lunrate_bls) gen(merge_qtru)
			drop if merge_qtru==2
			drop merge_qtru

		cap n gen unrate=exp(lunrate)
		cap n gen unrate_bls= exp(lunrate_bls)
		** instrumenting 
		cap n gen ur_all_durwvcearly_ext = exp(lur_all_durwvcearly_ext) 

		reg ur_all_durwvcearly_ext unrate_bls if durdistr_stability==1
		predict ur_all_durwvcearly_inst

		capture drop lur_all_durwvcearly_inst
		gen lur_all_durwvcearly_inst=log(ur_all_durwvcearly_inst)


		tsfilter hp hp_ur_all_durwvcearly_inst = ur_all_durwvcearly_inst , smooth(1600)
		tsfilter hp hp_lur_all_durwvcearly_inst = lur_all_durwvcearly_inst , smooth(1600)



		save "${outputdata}/hp_ur_all_durwvcearly_inst.dta", replace 
		restore

capture drop merge_qtrud
merge m:1 quarter using "${outputdata}/hp_ur_all_durwvcearly_inst.dta", keepusing(hp_ur_all_durwvcearly_inst hp_lur_all_durwvcearly_inst lur_all_durwvcearly_inst ur_all_durwvcearly_inst) gen(merge_qtrud)
drop if merge_qtrud==2
drop merge_qtrud




/*
** ANALYSIS 



global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "ur_all_durwvcearly_inst" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_urdwvcrlinst_dlt18 


global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "lur_all_durwvcearly_inst" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_lurdwvcrlinst_dlt18 



global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "hp_ur_all_durwvcearly_inst" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_hpurdwvcrlinst_dlt18 


global weight "wpfinwgt"
global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
global unrate_var "hp_lur_all_durwvcearly_inst" 		// unemployment variable
global unrate_tt " quarter "    // time trend?
global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "


robusttable_udur_cycle_exe wp_u_hplurdwvcrlinst_dlt18 
*/

***** save this 


	cap n putexcel set "${mainresultsdir}/table5_panelB_udur_cycle_data.xls", sheet("Table 5 Panel B", replace)
	cap n putexcel set "${mainresultsdir}/table5_panelB_udur_cycle_data.xls", modify sheet("Table 5 Panel B", replace)

	
	putexcel B1=("TABLE 5")
	putexcel B2=("Panel B")
	putexcel B3=("hp-filtered")
	putexcel A4=("movers")
	putexcel A5=("stayers")
	putexcel E3=("log u linearly detrended")
	

	global weight "wpfinwgt"
	global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
	global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
	global unrate_var "hp_lur_all_durwvcearly_inst" 		// unemployment variable
	global unrate_tt " quarter "    // time trend?
	global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "

	
	// WORKER CHAR + AGE + DEST OCCUPATIONS
	reg n_spellength i.lne_c_mmo i.lne_c_mmo#c.$unrate_var i.sex i.race i.ms2 i.educ2 c.tage##c.tage##c.tage##c.tage i.locc1bfr_mmo i.locc1aft_mmo $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel b5=(round(regreturn[1,3],.001))
	putexcel b4=(round(regreturn[1,4],.001))

	
	global weight "wpfinwgt"
	global ndur_bound " & n_spellength>=1 & n_spellength<=18 "
	global wavecond " & wave>3 & interview_no2>18 & sample_timetogo>=0 & durdistr_stability==1 "
	global unrate_var "lur_all_durwvcearly_inst" 		// unemployment variable
	global unrate_tt " quarter "    // time trend?
	global addcondition_cycle " & entry_ind==1 & n_spellength>=1 & complete_uspell==1 "

	reg n_spellength i.lne_c_mmo i.lne_c_mmo#c.$unrate_var i.sex i.race i.ms2 i.educ2 c.tage##c.tage##c.tage##c.tage i.locc1bfr_mmo i.locc1aft_mmo $unrate_tt [pw=$weight] if lne_c_mmo!=. & locc1bfr_mmo!=45 ///
	& locc1aft_mmo!=45 & entry_ind==1 & n_spellength>=1 $addcondition_cycle $wavecond  $ndur_bound , vce(cluster quarter)
	
	matrix regreturn=r(table)
	matrix list regreturn
	putexcel e5=(round(regreturn[1,3],.001))
	putexcel e4=(round(regreturn[1,4],.001))

	
	
	
	
************************************************
** 2.4 NET MOBILITY DURATION REGRESSIONS 
************************************************
						

capture drop merge_qtruee
merge m:1 quarter using "${outputdata}/timeseries_u_durwvcearly.dta", gen(merge_qtruee)

drop merge_qtruee

						
			
global mwavecond " & wave>1 & interview_no2>4 & sample_timetogo>=0"
global wavecond4 " & wave>4 & interview_no2>16 & sample_timetogo>=0 & durdistr_stability==1"
global wavecond5 " & wave>5 & interview_no2>20 & sample_timetogo>=0 & durdistr_stability==1"
global wavecond " & wave>4 & interview_no2>16 & sample_timetogo>=0 & durdistr_stability==1"
global wavecond " & wave>5 & interview_no2>20 & sample_timetogo>=0 & durdistr_stability==1"
global ndur_bound " & n_spellength>=1 & n_spellength<=18"
global add_condition1 " $wavecond4 "



// log duration, hp filtered
capture drop ludur
gen ludur=log(u_spellength)

// XXXXXXXXXXXXX                      LOG UDUR
//CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
global add_condition1 " $wavecond4 "
display "$ndur_bound"
display "$add_condition1"



quietly {
cap log close regresultslog6
log using "${mainresultsdir}/table2_appxtable2_superocc_udur_elasticity_data.txt", replace text name(regresultslog6)

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "Elasticity Unemployment Duration p. Superocc to Agg U Rate"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""



noisily: reg ludur ib4.locc1bfr_rtmm ib4.locc1bfr_rtmm#c.lur_all_durwvcearly_ext quarter [pw=pweight2] if lue_c_mmo!=. & locc1bfr_mmo!=11 & locc1bfr_mmo!=45 & locc1aft_mmo!=45 ///
$ndur_bound $add_condition1 , vce(cluster quarter)


noisily: display  ""
noisily: display  " to get to the normalized elasticities of the paper, divide "
noisily: display  " these elasticities by the *population-weighted* average elasticity"  
noisily: display  " pop.-weight is calculated by averaging the 1984 and 2012 share of occupation"
noisily: display  " (shares as calculated and used in the estimation)"


log close regresultslog6
	

}		


						
						
						
						
************************************************************
*	*************************************************
* 2.5-2.6 INCOMPLETE DURATION DISTRIBUTION 
*	**************************************************
*************************************************************


** need the entire u_n spells (because of incomplete spells)


use "${outputdata}/corewave_lf_with_u_ctv.dta", clear
version 13

*********************************************
** 2.5 INCOMPLETE DURATION DISTRIBUTION, CROSS SECTION, TABLE 1 ONLINE APPENDIX
*********************************************



global sttg = "0"
global epanbwidth=3


global wavecond " & wave>4 & interview_no2>14 & sample_timetogo>=0"
global wavecond " & wave>5 & interview_no2>20 & sample_timetogo>=0"
global mwavecond " & wave>1 & interview_no2>4 & sample_timetogo>=0"
	* for job finding: take all nonemployment spells that start after wave 1, but also start at least 16 months before the working exits the sample
global jfwavecond "& wave>1 & interview_no2>4 & sample_timetogo>18-n_spellength "	// this 
global sttg "1"
global noagric22 " & locc1bfr_mmo!=45 & locc1aft_mmo!=45 "
global noagric13 " & locc1bfr_dd!=9 & locc1aft_dd!=9 "
global noagric "  locc1bfr_mmo!=45 & locc1aft_mmo!=45 "


//====================================
// PROGRAMS
//====================================
capture program drop star_exe
program star_exe 
	args xlscol xlsrow table_pos matrixname


	
	
if "`matrixname'"=="" {
	matrix rtablel=r(table)
}
if "`matrixname'"!="" {
	matrix rtablel=`matrixname'
}

*matrix list rtablel
*display "`table_pos'"
local pval=rtablel[4,`table_pos']
local coeffx=rtablel[1,`table_pos']
local stderrx=rtablel[2, `table_pos']
*display "`pval'"
if `pval'<=0.1 & `pval'>0.05 {
local star="*" 
}	
if `pval'>0.01 & `pval'<=0.05 {
local star="**" 
}	
if `pval'<=0.01 {
local star="***" 
}	
if `pval'>0.1 {
local star="" 
}	
local coeffx=string(`coeffx', "%8.4f")
local coeffstar="`coeffx'`star'"
local stderr2=string(`stderrx', "%8.4f")
*display "`stderr2'"
local stderr3="(`stderr2')"
display "`coeffstar'"
display "`stderr3'"


putexcel `xlscol'`xlsrow'=("`coeffstar'")
local xlsrowplus=`xlsrow'+1
putexcel `xlscol'`xlsrowplus'=("`stderr3'")

end


capture program drop tstar_exe
program tstar_exe   // COLUMN FIRST, THEN ROW 
	args xlscol xlsrow rtable_pos matrixname
	
if "`matrixname'"=="" {
	matrix localrtablexxxx=r(table)
}
if "`matrixname'"!="" {
	matrix localrtablexxxx=`matrixname'
}

*matrix list rtable
*display "`table_pos'"
local pval=localrtablexxxx[4,`rtable_pos']
local coeffx=localrtablexxxx[1,`rtable_pos']
local stderrx=localrtablexxxx[2, `rtable_pos']
*display "`pval'"
if `pval'<=0.1 & `pval'>0.05 {
local star="*" 
}	
if `pval'>0.01 & `pval'<=0.05 {
local star="**" 
}	
if `pval'<=0.01 {
local star="***" 
}	
if `pval'>0.1 {
local star="" 
}	
local coeffx=string(`coeffx', "%8.4f")
local coeffstar="`coeffx'`star'"
local stderr2=string(`stderrx', "%8.4f")
*display "`stderr2'"
local stderr3="(`stderr2')"
display "`coeffstar'"
display "`stderr3'"

tokenize "`c(alpha)'"

putexcel ``xlscol''`xlsrow'=("`coeffstar'")
local xlscolplus=`xlscol'+1

putexcel ``xlscolplus''`xlsrow'=("`stderr3'")

end


/*
** auxiliary programs
do "${codedir}aux_programs.do"
			/* programs:
					xsnet_calcx
					occmat_dim_exe	
			*/
** correction matrices
do "${codedir}Ginv matrices.do"
*/



//=================================
// ADDITIONAL VARIABLES (MERGED IN AND/OR DEFINED)
//=================================


capture drop merge_qtru
merge m:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", keepusing(lunrate*) gen(merge_qtru)
drop merge_qtru

capture drop merge_qtruee
merge m:1 quarter using "${outputdata}/timeseries_u_durwvcearly.dta", gen(merge_qtruee)
*merge m:1 quarter using "c:\data\ctv\output/timeseries_u_durwvcearly.dta", gen(merge_qtruee)
drop merge_qtruee


capture drop mm_clsfication_dum
gen mm_clsfication_dum =0 if panel<=2001
replace mm_clsfication_dum=1 if panel>=2002

capture drop all_clsfication_dum
gen all_clsfication_dum=0 if panel<=1991
replace all_clsfication_dum=1 if panel<=2001 & panel>1991
replace all_clsfication_dum=2 if panel<=2008 & panel>2001

capture drop educ2
gen educ2=educ
replace educ2=4 if educ==5


// AGE  (with wavecondition)
capture drop age_2dum
gen age_2dum=1 if tage>=20 & tage<=30
replace age_2dum=2 if tage>=35 & tage<=55


capture drop entry_ind
			gen byte entry_ind=0
			replace entry_ind=1 if max_educ==1 & tage>18
			replace entry_ind=1 if max_educ==2 & tage>18
			replace entry_ind=1 if max_educ==3 & tage>22
			replace entry_ind=1 if max_educ==4 & tage>22
			replace entry_ind=1 if max_educ==5 & tage>24
			

capture drop entry_ind_old
ren entry_ind entry_ind_old
capture drop entry_ind
gen entry_ind=1 if educ<=2 & tage>=20
replace entry_ind=1 if educ>=3 & educ<=5 & tage>=24

*** RECESSION INDICATORS
capture drop recession_yr
gen recession_yr=0
replace recession_yr=1 if year==1990
replace recession_yr=1 if year==1991
replace recession_yr=1 if year==2001
replace recession_yr=1 if year==2008
replace recession_yr=1 if year==2009

**** GOOD TIMES BAD TIMES INDICATOR
su hp_lunrate, detail
sort quarter
capture drop hp_lunrate_temp
count if hp_lunrate==. & quarter!=quarter[_n-1]
gen hp_lunrate_temp=hp_lunrate if quarter!=quarter[_n-1]
_pctile hp_lunrate_temp, p(25 75)
local low_upct=r(r1)
local high_upct=r(r2)
capture drop rec_exp_ind
gen byte rec_exp_ind=0
replace rec_exp_ind=-1 if hp_lunrate>=`high_upct'
replace rec_exp_ind=1 if hp_lunrate<=`low_upct'

sort personkey yearmonth
capture drop hp_lunrate_temp

su hp_lunrate, detail
sort quarter
capture drop hp_lunrate_temp
count if hp_lunrate==. & quarter!=quarter[_n-1]
gen hp_lunrate_temp=hp_lunrate if quarter!=quarter[_n-1]
_pctile hp_lunrate_temp, p(33 67)
local low_upct=r(r1)
local high_upct=r(r2)
capture drop rec_exp_ind3
gen byte rec_exp_ind3=1
replace rec_exp_ind3=0 if hp_lunrate>=`high_upct'
replace rec_exp_ind3=2 if hp_lunrate<=`low_upct'

sort personkey yearmonth
capture drop hp_lunrate_temp

/*
capture drop agegroup
gen agegroup=1 if tage>=22 & tage<=25
replace agegroup=2 if tage>=26 & tage<=30
replace agegroup=3 if tage>=31 & tage<=35
replace agegroup=4 if tage>=36 & tage<=40
replace agegroup=5 if tage>=41 & tage<=45
replace agegroup=6 if tage>=46 & tage<=50
replace agegroup=7 if tage>=51 & tage<=55
replace agegroup=8 if tage>=55 & tage<=60
replace agegroup=9 if tage>=61 & tage<=65
*/
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

 lab val locc1bfr_mmo label_mm		
 lab val locc1aft_mmo label_mm			
 
capture drop ms_sum
gen byte ms_sum=1 if ms<=2
replace ms_sum=ms if ms>2
capture drop ms2
gen byte ms2=1 if ms<=2
replace ms2=ms if ms>2


capture drop agebin
gen byte agebin=1 if tage>=20 & tage<=30
replace agebin=2 if tage>=35 & tage<=55



capture drop durdistr_stability
gen byte durdistr_stability=.
replace durdistr_stability=1 if quarter>=100 & quarter<=118
replace durdistr_stability=1 if quarter>=125 & quarter<=142
replace durdistr_stability=1 if quarter>=150 & quarter<=159
replace durdistr_stability=1 if quarter>=169 & quarter<=175
replace durdistr_stability=1 if quarter>=180 & quarter<=190
replace durdistr_stability=1 if quarter>=198 & quarter<=214

// fullseam_qtr_ind: those quarters that have full rotations (and therefore balanced seams), in some panel. nofullseam_ind!=1 is indicating this within the panel
capture drop fullseam_qtr_ind
gen byte fullseam_qtr_ind=.
replace fullseam_qtr_ind=1 if quarter>=95 & quarter<=118
replace fullseam_qtr_ind=1 if quarter>=121 & quarter<=142
replace fullseam_qtr_ind=1 if quarter>=146 & quarter<=158
replace fullseam_qtr_ind=1 if quarter>=166 & quarter<=174
replace fullseam_qtr_ind=1 if quarter>=177 & quarter<=190
replace fullseam_qtr_ind=1 if quarter>=195 & quarter<=213

// using restriction on time in sample (interview_no2), create seams 12 or 14 months later
capture drop fullseam_12lag_qtr_ind
gen byte fullseam_12lag_qtr_ind=.
replace fullseam_12lag_qtr_ind=1 if quarter>=99 & quarter<=118
replace fullseam_12lag_qtr_ind=1 if quarter>=125 & quarter<=142
replace fullseam_12lag_qtr_ind=1 if quarter>=150 & quarter<=158
replace fullseam_12lag_qtr_ind=1 if quarter>=170 & quarter<=174
replace fullseam_12lag_qtr_ind=1 if quarter>=181 & quarter<=190
replace fullseam_12lag_qtr_ind=1 if quarter>=199 & quarter<=213


capture drop fullseam_14lag_qtr_ind
gen byte fullseam_14lag_qtr_ind=.
replace fullseam_14lag_qtr_ind=1 if quarter>=100 & quarter<=118
replace fullseam_14lag_qtr_ind=1 if quarter>=126 & quarter<=142
replace fullseam_14lag_qtr_ind=1 if quarter>=151 & quarter<=158
replace fullseam_14lag_qtr_ind=1 if quarter>=171 & quarter<=174
replace fullseam_14lag_qtr_ind=1 if quarter>=182 & quarter<=190
replace fullseam_14lag_qtr_ind=1 if quarter>=200 & quarter<=213



***************************
** INCOMPLETE DURATION DISTRIBUTION
***************************



** THERE ARE DIFFERENT MEASURES OF DISTRIBUTION OF INCOMPLETE DURATIONS, WITH SOME SHIFTS IN SHORTER DURATIONS

sort personkey yearmonth
capture drop unemp_cat
gen unemp_cat=.
replace unemp_cat=0 if u_spellength<3 & u_spellength>0
replace unemp_cat=1 if u_spellength<5 & u_spellength>=3
replace unemp_cat=2 if u_spellength<9 & u_spellength>=5
replace unemp_cat=3 if u_spellength<13 & u_spellength>=9
replace unemp_cat=4 if u_spellength<=18 & u_spellength>=13

global durwvcearlycond " & wave>4 & interview_no2>14 & sample_timetogo>1"


// INVESTIGATE QUARTERS WITH COMPLETE DISTRIBUTIONS
tab quarter unemp_cat if durdistr_stability==1 $durwvcearlycond , row nof
tab quarter unemp_cat if durdistr_stability==1 $durwvcearlycond & quarter<tq(1990q1), row nof
tab quarter unemp_cat if durdistr_stability==1 & quarter<tq(1990q1), row nof
tab quarter unemp_cat if durdistr_stability==1 $durwvcearlycond & quarter>tq(2010q1), row nof
tab quarter unemp_cat if durdistr_stability==1 & quarter>tq(2010q1), row nof
tab quarter unemp_cat if durdistr_stability!=. $durwvcearlycond & quarter>tq(2010q1), row nof
tab quarter unemp_cat if durdistr_stability==1 & quarter>tq(2010q1), row nof
tab quarter wave if quarter!=. $durwvcearlycond & durdistr_stability==1 , row nof

tab unemp_cat [aw=pweight2] if durdistr_stability==1 $durwvcearlycond 
tab unemp_cat [aw=pweight2] if durdistr_stability==1 

tab unemp_cat [aw=pweight2] if durdistr_stability==1 $durwvcearlycond  &  quarter>tq(1998q1) &  quarter<tq(2007q1) 
tab unemp_cat [aw=pweight2] if durdistr_stability==1 &  quarter>tq(1998q1) &  quarter<tq(2007q1) 

tab unemp_cat [aw=pweight2] if durdistr_stability==1 $durwvcearlycond  &  quarter<tq(1990q1) 
tab unemp_cat [aw=pweight2] if durdistr_stability==1 &  quarter<tq(1990q1)


// OBSERVATIONS 
version 13
capture drop obs_u_or_e_ctv_qtr
egen obs_u_or_e_ctv_qtr=count(personkey != "") if durdistr_stability==1 & wave>4 & interview_no>14 & entry_ind==1 & (unempl_ctv==1 | empl_ctv==1), by(quarter)

*table quarter, c(mean obs_u_or_e_ctv_qtr)
capture drop obs_u_ctv_qtr
egen obs_u_ctv_qtr=count(personkey != "") if durdistr_stability==1 & wave>4 & interview_no>14 & entry_ind==1 & (unempl_ctv==1), by(quarter)
*table quarter, c(mean obs_u_ctv_qtr)
capture drop obs_u_ctv_uspellength_qtr
egen obs_u_ctv_uspellength_qtr=count(personkey != "") if durdistr_stability==1 & wave>4 & interview_no>14 & entry_ind==1 & (unempl_ctv==1) & u_spellength>0 & u_spellength<., by(quarter)
*table quarter, c(mean obs_u_ctv_uspellength_qtr)
** CAN DO THIS SIMPLER
*table quarter if durdistr_stability==1 & wave>4 & interview_no>14 & entry_ind==1 & (unempl_ctv==1), c(count unempl_ctv rawsum pweight2)


// THESE ARE SAmPLE AVERAGES --  BUT WE NEED TIME SERIES AVERAGES FOR THE CALIBRATION!!!
//       so first we calculate the time series then take averages, for the proper measures
tab unemp_cat [aw=wpfinwgt] if durdistr_stability==1
tab unemp_cat [aw=wpfinwgt] if durdistr_stability==1 $durwvcearlycond 
tab unemp_cat [aw=wpfinwgt] if unemp_cat!=. $durwvcearlycond  

tab unemp_cat [aw=wpfinwgt] if durdistr_stability==1 $durwvcearlycond & fullseam_qtr_ind==1 & entry_ind==1
tab unemp_cat [aw=wpfinwgt] if durdistr_stability==1 & entry_ind==1

tab unemp_cat [aw=pweight2] if durdistr_stability==1
tab unemp_cat [aw=pweight2] if durdistr_stability==1 $durwvcearlycond 
tab unemp_cat [aw=pweight2] if durdistr_stability==1 $durwvcearlycond & fullseam_qtr_ind==1 & entry_ind==1
tab unemp_cat [aw=pweight2] if durdistr_stability==1 & entry_ind==1

** The distribution comes close with the first and the last set. Note that the survival profile is calculated without reference to durwvcearly, in particular it is based on spells starting in the 
** first four waves, whiel durwbcearlycond rules out measurement in those waves
/*
global pos=2
global sumstats_surv_addcondition " (incomplete_uspell==1 | complete_uspell==1) & sample_timetogo>=32 & wave<=4 & entry_ind==1"
surv_sumstats_calc_exe usurv_ind au_inflsttg32_w4_test
*/

** PROPORTIONS PER QUARTER 

sort quarter unemp_cat entry_ind

capture drop unemp_cat0q
by quarter: egen unemp_cat0q=sum(pweight2) if entry_ind==1 & unemp_cat==0 
capture drop unemp_cat1q
by quarter: egen unemp_cat1q=sum(pweight2) if entry_ind==1 & unemp_cat==1 
capture drop unemp_cat2q
by quarter: egen unemp_cat2q=sum(pweight2) if entry_ind==1 & unemp_cat==2 
capture drop unemp_cat3q
by quarter: egen unemp_cat3q=sum(pweight2) if entry_ind==1 & unemp_cat==3 
capture drop unemp_cat4q
by quarter: egen unemp_cat4q=sum(pweight2) if entry_ind==1 & unemp_cat==4 


sort quarter 

forvalues i=0(1)4 {

replace unemp_cat`i'q=unemp_cat`i'q[_n-1] if unemp_cat`i'q[_n-1]!=. & unemp_cat`i'q==. & quarter[_n-1]==quarter

}

gsort -quarter 
forvalues i=0(1)4 {

replace unemp_cat`i'q=unemp_cat`i'q[_n-1] if unemp_cat`i'q[_n-1]!=. & unemp_cat`i'q==. & quarter[_n-1]==quarter

}


sort quarter unemp_cat entry_ind

capture drop unemp_catqsum
gen unemp_catqsum=(unemp_cat0q+unemp_cat1q+unemp_cat2q+unemp_cat3q+unemp_cat4q) if unemp_cat0q!=. & unemp_cat1q!=. & unemp_cat2q!=. & unemp_cat3q!=. & unemp_cat4q!=.
replace unemp_catqsum=(unemp_cat0q+unemp_cat1q+unemp_cat2q+unemp_cat3q) if unemp_cat0q!=. & unemp_cat1q!=. & unemp_cat2q!=. & unemp_cat3q!=. & unemp_cat4q==.
replace unemp_catqsum=(unemp_cat0q+unemp_cat1q+unemp_cat2q) if unemp_cat0q!=. & unemp_cat1q!=. & unemp_cat2q!=. & unemp_cat3q==. & unemp_cat4q==.
replace unemp_catqsum=(unemp_cat0q+unemp_cat1q+unemp_cat2q+unemp_cat4q) if unemp_cat0q!=. & unemp_cat1q!=. & unemp_cat2q!=. & unemp_cat3q==. & unemp_cat4q!=.
replace unemp_cat3q=0 if unemp_cat3q==. & unemp_catqsum>0
replace unemp_cat4q=0 if unemp_cat4q==. & unemp_catqsum>0

replace unemp_cat0q=unemp_cat0q/unemp_catqsum
replace unemp_cat1q=unemp_cat1q/unemp_catqsum + unemp_cat0q
replace unemp_cat2q=unemp_cat2q/unemp_catqsum 	
replace unemp_cat3q=unemp_cat3q/unemp_catqsum 
replace unemp_cat4q=unemp_cat4q/unemp_catqsum 

capture drop unemp_catqsum

sort quarter
su unemp_cat* if durdistr_stability==1 & quarter==quarter[_n-1]+1

lab var unemp_cat0q "duration 1-2 months"
lab var unemp_cat1q "duration 1-4 months"
lab var unemp_cat2q "duration 5-8 months"
lab var unemp_cat3q "duration 9-12 months"
lab var unemp_cat4q "duration 13-18 months"

quietly {
cap log close appendixtable1log
log using "${mainresultsdir}/appxtable1_appxtable8_incompl_durdistr_data.txt", replace text name(appendixtable1log)


noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "TABLE 1/11 ONLINE APPENDIX, INCOMPL. DURATION DISTR -- ALL "
noisily: display  "-----------------------------------------------------------"
noisily: display  ""




noisily: mean unemp_cat*q if durdistr_stability==1 & quarter==quarter[_n-1]+1

noisily: display  ""
noisily: display  "unemp_cat0q=unemployment duration 1-2 months"
noisily: display  "unemp_cat1q= ... 1-4 months, unemp_cat2q= ... 5-8 months"
noisily: display  "unemp_cat3q= ... 9-12 months, unemp_cat4q= ... 13-18 months"
noisily: display  ""



log off appendixtable1log
	

}		



******
**  SAME FOR THE YOUNG
******


sort quarter unemp_cat entry_ind

capture drop unemp_yng_cat0q
by quarter: egen unemp_yng_cat0q=sum(pweight2) if entry_ind==1 & unemp_cat==0  & age_2dum==1
capture drop unemp_yng_cat1q
by quarter: egen unemp_yng_cat1q=sum(pweight2) if entry_ind==1 & unemp_cat==1  & age_2dum==1
capture drop unemp_yng_cat2q
by quarter: egen unemp_yng_cat2q=sum(pweight2) if entry_ind==1 & unemp_cat==2  & age_2dum==1 
capture drop unemp_yng_cat3q
by quarter: egen unemp_yng_cat3q=sum(pweight2) if entry_ind==1 & unemp_cat==3  & age_2dum==1
capture drop unemp_yng_cat4q
by quarter: egen unemp_yng_cat4q=sum(pweight2) if entry_ind==1 & unemp_cat==4  & age_2dum==1


sort quarter 

forvalues i=0(1)4 {

replace unemp_yng_cat`i'q=unemp_yng_cat`i'q[_n-1] if unemp_yng_cat`i'q[_n-1]!=. & unemp_yng_cat`i'q==. & quarter[_n-1]==quarter

}

gsort -quarter 
forvalues i=0(1)4 {

replace unemp_yng_cat`i'q=unemp_yng_cat`i'q[_n-1] if unemp_yng_cat`i'q[_n-1]!=. & unemp_yng_cat`i'q==. & quarter[_n-1]==quarter

}


sort quarter unemp_cat entry_ind

capture drop unemp_yng_catqsum
gen unemp_yng_catqsum=(unemp_yng_cat0q+unemp_yng_cat1q+unemp_yng_cat2q+unemp_yng_cat3q+unemp_yng_cat4q) if unemp_yng_cat0q!=. & unemp_yng_cat1q!=. & unemp_yng_cat2q!=. & unemp_yng_cat3q!=. & unemp_yng_cat4q!=.
replace unemp_yng_catqsum=(unemp_yng_cat0q+unemp_yng_cat1q+unemp_yng_cat2q+unemp_yng_cat3q) if unemp_yng_cat0q!=. & unemp_yng_cat1q!=. & unemp_yng_cat2q!=. & unemp_yng_cat3q!=. & unemp_yng_cat4q==.
replace unemp_yng_catqsum=(unemp_yng_cat0q+unemp_yng_cat1q+unemp_yng_cat2q) if unemp_yng_cat0q!=. & unemp_yng_cat1q!=. & unemp_yng_cat2q!=. & unemp_yng_cat3q==. & unemp_yng_cat4q==.
replace unemp_yng_catqsum=(unemp_yng_cat0q+unemp_yng_cat1q+unemp_yng_cat2q+unemp_yng_cat4q) if unemp_yng_cat0q!=. & unemp_yng_cat1q!=. & unemp_yng_cat2q!=. & unemp_yng_cat3q==. & unemp_yng_cat4q!=.
replace unemp_yng_cat3q=0 if unemp_yng_cat3q==. & unemp_yng_catqsum>0
replace unemp_yng_cat4q=0 if unemp_yng_cat4q==. & unemp_yng_catqsum>0

replace unemp_yng_cat0q=unemp_yng_cat0q/unemp_yng_catqsum
replace unemp_yng_cat1q=unemp_yng_cat1q/unemp_yng_catqsum + unemp_yng_cat0q
replace unemp_yng_cat2q=unemp_yng_cat2q/unemp_yng_catqsum 	
replace unemp_yng_cat3q=unemp_yng_cat3q/unemp_yng_catqsum 
replace unemp_yng_cat4q=unemp_yng_cat4q/unemp_yng_catqsum 

capture drop unemp_yng_catqsum

su unemp_yng_cat* if durdistr_stability==1

su unemp_yng_cat* if durdistr_stability==1 & quarter==quarter[_n-1]+1




quietly {
log on appendixtable1log 

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  "TABLE 1/11 ONLINE APPENDIX, INCOMPL. DURATION DISTR -- YOUNG "
noisily: display  "-----------------------------------------------------------"
noisily: display  ""



noisily: mean unemp_yng_cat* if durdistr_stability==1 & quarter==quarter[_n-1]+1


log off appendixtable1log
	

}		



******
** SAME FOR THE PRIME AGED
******


sort quarter unemp_cat entry_ind

capture drop unemp_prm_cat0q
by quarter: egen unemp_prm_cat0q=sum(pweight2) if entry_ind==1 & unemp_cat==0  & age_2dum==2
capture drop unemp_prm_cat1q
by quarter: egen unemp_prm_cat1q=sum(pweight2) if entry_ind==1 & unemp_cat==1  & age_2dum==2
capture drop unemp_prm_cat2q
by quarter: egen unemp_prm_cat2q=sum(pweight2) if entry_ind==1 & unemp_cat==2  & age_2dum==2 
capture drop unemp_prm_cat3q
by quarter: egen unemp_prm_cat3q=sum(pweight2) if entry_ind==1 & unemp_cat==3  & age_2dum==2
capture drop unemp_prm_cat4q
by quarter: egen unemp_prm_cat4q=sum(pweight2) if entry_ind==1 & unemp_cat==4  & age_2dum==2


sort quarter 

forvalues i=0(1)4 {

replace unemp_prm_cat`i'q=unemp_prm_cat`i'q[_n-1] if unemp_prm_cat`i'q[_n-1]!=. & unemp_prm_cat`i'q==. & quarter[_n-1]==quarter

}

gsort -quarter 
forvalues i=0(1)4 {

replace unemp_prm_cat`i'q=unemp_prm_cat`i'q[_n-1] if unemp_prm_cat`i'q[_n-1]!=. & unemp_prm_cat`i'q==. & quarter[_n-1]==quarter

}


sort quarter unemp_cat entry_ind

capture drop unemp_prm_catqsum
gen unemp_prm_catqsum=(unemp_prm_cat0q+unemp_prm_cat1q+unemp_prm_cat2q+unemp_prm_cat3q+unemp_prm_cat4q) if unemp_prm_cat0q!=. & unemp_prm_cat1q!=. & unemp_prm_cat2q!=. & unemp_prm_cat3q!=. & unemp_prm_cat4q!=.
replace unemp_prm_catqsum=(unemp_prm_cat0q+unemp_prm_cat1q+unemp_prm_cat2q+unemp_prm_cat3q) if unemp_prm_cat0q!=. & unemp_prm_cat1q!=. & unemp_prm_cat2q!=. & unemp_prm_cat3q!=. & unemp_prm_cat4q==.
replace unemp_prm_catqsum=(unemp_prm_cat0q+unemp_prm_cat1q+unemp_prm_cat2q) if unemp_prm_cat0q!=. & unemp_prm_cat1q!=. & unemp_prm_cat2q!=. & unemp_prm_cat3q==. & unemp_prm_cat4q==.
replace unemp_prm_catqsum=(unemp_prm_cat0q+unemp_prm_cat1q+unemp_prm_cat2q+unemp_prm_cat4q) if unemp_prm_cat0q!=. & unemp_prm_cat1q!=. & unemp_prm_cat2q!=. & unemp_prm_cat3q==. & unemp_prm_cat4q!=.
replace unemp_prm_cat3q=0 if unemp_prm_cat3q==. & unemp_prm_catqsum>0
replace unemp_prm_cat4q=0 if unemp_prm_cat4q==. & unemp_prm_catqsum>0


replace unemp_prm_cat0q=unemp_prm_cat0q/unemp_prm_catqsum
replace unemp_prm_cat1q=unemp_prm_cat1q/unemp_prm_catqsum + unemp_prm_cat0q
replace unemp_prm_cat2q=unemp_prm_cat2q/unemp_prm_catqsum 	
replace unemp_prm_cat3q=unemp_prm_cat3q/unemp_prm_catqsum 
replace unemp_prm_cat4q=unemp_prm_cat4q/unemp_prm_catqsum 

capture drop unemp_prm_catqsum


quietly {
log on appendixtable1log

noisily: display  ""
noisily: display  "------------------------------------------------------------------"
noisily: display  "TABLE 1/11 ONLINE APPENDIX, INCOMPL. DURATION DISTR -- PRIME-AGED "
noisily: display  "------------------------------------------------------------------"
noisily: display  ""



noisily: mean unemp_prm_cat* if durdistr_stability==1 & quarter==quarter[_n-1]+1


log close appendixtable1log
	

}		





******************************************
*****  cyclical pattern
******************************************




capture drop lunemp_cat0q
capture drop lunemp_cat1q
capture drop lunemp_cat2q
capture drop lunemp_cat3q
capture drop lunemp_cat4q

gen lunemp_cat0q=log(unemp_cat0q)
gen lunemp_cat1q=log(unemp_cat1q)
gen lunemp_cat2q=log(unemp_cat2q)
gen lunemp_cat3q=log(unemp_cat3q)
gen lunemp_cat4q=log(unemp_cat4q)


capture drop lunemp_yng_cat0q
capture drop lunemp_yng_cat1q
capture drop lunemp_yng_cat2q
capture drop lunemp_yng_cat3q
capture drop lunemp_yng_cat4q

gen lunemp_yng_cat0q=log(unemp_yng_cat0q)
gen lunemp_yng_cat1q=log(unemp_yng_cat1q)
gen lunemp_yng_cat2q=log(unemp_yng_cat2q)
gen lunemp_yng_cat3q=log(unemp_yng_cat3q)
gen lunemp_yng_cat4q=log(unemp_yng_cat4q)


capture drop lunemp_prm_cat0q
capture drop lunemp_prm_cat1q
capture drop lunemp_prm_cat2q
capture drop lunemp_prm_cat3q
capture drop lunemp_prm_cat4q

gen lunemp_prm_cat0q=log(unemp_prm_cat0q)
gen lunemp_prm_cat1q=log(unemp_prm_cat1q)
gen lunemp_prm_cat2q=log(unemp_prm_cat2q)
gen lunemp_prm_cat3q=log(unemp_prm_cat3q)
gen lunemp_prm_cat4q=log(unemp_prm_cat4q)







capture drop log_ur_all_durwvcearly
cap n gen log_ur_all_durwvcearly=log( ur_all_durwvcearly)
cap n gen log_ur_all_durwvcearly=lur_all_durwvcearly_ext


*********************************************************************
** TIME SERIES (SEMI-) ELASTICITIES
**********************************************************************


** 	PREPARATION OF FILTERED STATISTICS -- HAVE TO DELIVER THESE TO TRAMO 

/*save tempfile_xxxxx.dta, replace
 keep quarter qn3u_mob_1*
export excel using "C:\data\occmob_ts_for_tramo_uncorr.xlsx", sheet("q3") sheetreplace firstrow(variables) nolabel
save  "C:\data\occmob_ts_for_tramo_uncorr.dta", replace
 use tempfile_xxxxx.dta, clear*/


duplicates drop lunemp_cat0q lunemp_cat1q lunemp_cat2q lunemp_cat3q lunemp_cat4q quarter, force
*keep quarter unemp_yng* unemp_prm* lunemp_cat* unemp_cat* durdistr_stability lur_all_durwvcearly_ext hp* outpw* loutpw* unrate* lunrate*
keep quarter unemp_yng* unemp_prm* lunemp_cat* unemp_cat* durdistr_stability lur_all_durwvcearly_ext hp* outpw* loutpw* unrate* lunrate*
sort quarter
drop if durdistr_stability!=1
drop unemp_cat
drop durdistr_stability
duplicates report quarter
drop if unemp_cat0q==.
tsset quarter
tsfill

su unemp_cat*
su unemp_yng_cat* 
su unemp_prm_cat*
su lunemp_cat*


capture drop log_ur_all_durwvcearly
cap n gen log_ur_all_durwvcearly=log( ur_all_durwvcearly)
cap n gen log_ur_all_durwvcearly=lur_all_durwvcearly_ext

************************************************
** ELASTICITIES, UNFILTERED OR LINEARLY DETRENDED
**************************************************


** ELASTICITY wITH LOG DUR WVCOND EE UNEMPLOYMENT RATE

quietly {
cap log close table5panelBpart1log
log using "${mainresultsdir}/table5A_appxtable8_durdistr_elas_data.txt", replace text name(table5panelBpart1log)

noisily: display  ""
noisily: display  "------------------------------------------------------------------"
noisily: display  "TABLE 5, PANEL A, ELASTICITY (+ APPENDIX TABLE 8, PANEL B)    "
noisily: display  "------------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 1-2 MONTHS *****  "
noisily: display  ""
noisily: reg lunemp_cat0q log_ur_all_durwvcearly  
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 1-4 MONTHS *****  "
noisily: display  ""
noisily: reg lunemp_cat1q log_ur_all_durwvcearly  
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 5-8 MONTHS *****  "
noisily: display  ""
noisily: reg lunemp_cat2q log_ur_all_durwvcearly  
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 9-12 MONTHS *****  "
noisily: display  ""
noisily: reg lunemp_cat3q log_ur_all_durwvcearly  
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 13-18 MONTHS *****  "
noisily: display  ""
noisily: reg lunemp_cat4q log_ur_all_durwvcearly  

cap log close table5panelBpart1log
}




export excel using "${outputdata}/udistrprops_for_tramo.xlsx", sheet("durwvcee") sheetreplace firstrow(variables) nolabel
save "${outputdata}/udistrprops_for_tramo.dta", replace 
*use "${outputdata}/udistrprops_for_tramo.dta", clear

keep quarter unemp_cat0q unemp_cat1q unemp_cat2q unemp_cat3q unemp_cat4q
export excel using "${outputdata}/udistrprops_paper_for_tramo.xlsx", sheet("durwvcee") sheetreplace firstrow(variables) nolabel

clear





/*

********************************************************************************
** 
**		TRAMO / SEATS USING TSW+  
**
********************************************************************************


********************************************************************************
NOTE: the only series that are directly relevant for the main text are 
IN step2_5_2....do
	- qn3_mob_12_mm
	- qn3u_mob_12_mm
AND HERE IN step2_12_....do
	- unemp_cat0q
	- unemp_cat1q
	- unemp_cat2q
	- unemp_cat3q
	- unemp_cat4q
So, if interested in reproducing the results in the paper, one can simply restrict
oneself to those seven series
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
	 - Go to the outputdata directory, and select occmob_ts_for_tramo.xlsx, or here
		udistrprops_for_tramo.xlsx
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
	- It is important that the names are kept, as below will import the resulting 
		series back into STATA
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
In our step 3 code directory (${step3codedir}), we have saved the before and 
after timeseries we are using in our paper, in subdirectory 
.\Tramo_Seats_series_REFERENCE\. This means that, when replicating, STATA can run 
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



 
******************************************************************************** 
** READ IN TRAMO-ED SERIES 
********************************************************************************


	capture program drop tsudist_aftertramo_to_dta_exe
	program define tsudist_aftertramo_to_dta_exe
						args filename indic name
	clear

	** uprop
	*local name "unemp_cat`indic'q"
						display "`indic'"
	*import excel "C:/TSW+/SAVED/`filename'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow
	cap n import excel "${outputdata}/`filename'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow
	cap n import excel "${outputdata}/`filename'.xls", sheet("s0_`name'") cellrange(A2:K125) firstrow
	cap n import excel "${outputdata}/`name'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow
	
	gen quarter=quarterly(DATE, "QY")
	format quarter %tq 
	ren Xorig  `name'_xorig
	ren Xint `name'_xint
	ren Xlin `name'_xlin
	keep quarter `name'*
	mvdecode  `name'*, 	mv(-99999)

	*save "${tempdata}/qtr_`filename'_aftertramo_aug2019.dta", replace 
	save "${tempdata}/qtr_`filename'_aftertramo_.dta", replace 



				** luprop
				/*
				clear
				*local name "lunemp_cat`indic'q"
									display "`indic'"
				import excel "C:/TSW+/SAVED/`filename'.xlsx", sheet("s0_`name'") cellrange(A2:K125) firstrow

				gen quarter=quarterly(DATE, "QY")
				format quarter %tq 
				ren Xorig  `name'_xorig
				ren Xint `name'_xint
				ren Xlin `name'_xlin
				keep quarter `name'*
				mvdecode  `name'*, 	mv(-99999)
				
				saveold "${datadir1}ts_series/qtr_ludist`indic'prop_aftertramo_aug2019.dta", replace 
				*/
	end 


	
	 tsudist_aftertramo_to_dta_exe unemp0q 0 unemp_cat0q
	 tsudist_aftertramo_to_dta_exe unemp1q 1 unemp_cat1q
	 tsudist_aftertramo_to_dta_exe unemp2q 2 unemp_cat2q
	 tsudist_aftertramo_to_dta_exe unemp3q 3 unemp_cat3q
	 tsudist_aftertramo_to_dta_exe unemp4q 4 unemp_cat4q
	 
	
	 
**
**  MERGE HP-FILTER THESE
**

clear
*use "H:\laptop beast hdd\data\udistrprops_for_tramo.dta"
*global temptempdata "H:/laptop beast hdd/data/ts_series/"
use "${tempdata}/qtr_unemp0q_aftertramo_.dta", clear
forvalues i=1(1)4 {
		capture drop merge_distr`i'
		*merge m:1 quarter using "${temptempdata}/qtr_unemp`i'q_aftertramo.dta", gen(merge_distr`i')
		merge 1:1 quarter using "${tempdata}/qtr_unemp`i'q_aftertramo_.dta", gen(merge_distr`i')
		drop if merge_distr`i'==2

		/*
		capture drop merge_distrl`i'
		merge m:1 quarter using "${datadir1}ts_series/qtr_lunemp`i'q_aftertramo_aug2019.dta", gen(merge_distrl`i')
		drop if merge_distrl`i'==2
		*/
}

tsset quarter
tsfill

** hp filter

forvalues i=0(1)4 {
display "tsfilter `i'"
	
	cap n tsfilter hp hp_prop`i'udistr_xint =  unemp_cat`i'q_xint  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'udistr_xlin =  unemp_cat`i'q_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	
	
	/*
	cap n tsfilter hp hp_prop`i'udistr_yng_xint =  unemp_yng_cat`i'q_xint  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'udistr_yng_xlin =  unemp_yng_cat`i'q_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'udistr_prm_xint =  unemp_prm_cat`i'q_xint  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'udistr_prm_xlin =  unemp_prm_cat`i'q_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	
	
	cap n tsfilter hp hp_prop`i'ludistr_xlin =  lunemp_cat`i'q_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'ludistr_xint =  lunemp_cat`i'q_xint  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'ludistr_yng_xint =  lunemp_yng_cat`i'q_xint  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'ludistr_yng_xlin =  lunemp_yng_cat`i'q_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'ludistr_prm_xint =  lunemp_prm_cat`i'q_xint  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	cap n tsfilter hp hp_prop`i'ludistr_prm_xlin =  lunemp_prm_cat`i'q_xlin  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
	*/
	
}




**********************************************************
**  HOW TO DEAL WITH MEASUREMENT ERROR IN THE INDEPENDENT VARIABLES 
**********************************************************

** HOW SENSITIVE ARE THESE ELASTICITIES TO SMOOTHING 
capture drop unrate2
cap n ren unrate unrate2

capture drop merge_qtru
merge 1:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", keepusing(lunrate*) gen(merge_qtru)
drop merge_qtru

capture drop merge_qtruee
merge 1:1 quarter using "${outputdata}/timeseries_u_durwvcearly.dta", gen(merge_qtruee)
*merge m:1 quarter using "c:\data\ctv\output/timeseries_u_durwvcearly.dta", gen(merge_qtruee)
drop merge_qtruee

capture drop merge_qtr_bfr_tramo
merge 1:1 quarter using "${outputdata}/udistrprops_for_tramo.dta", gen(merge_qtr_bfr_tramo)
drop merge_qtr_bfr_tramo


drop if quarter<tq(1980q1)

** 5Q  UNEMP RATE LOGGED 

capture drop lur_all_durwvcearly_temp
*reg log_ur_all_durwvcearly unrate
reg lur_all_durwvcearly lunrate_bls
predict lur_all_durwvcearly_temp

capture drop lur_all_durwvcearly_p 
gen lur_all_durwvcearly_p=lur_all_durwvcearly 
replace lur_all_durwvcearly_p=lur_all_durwvcearly_temp if lur_all_durwvcearly==.
capture drop lur_all_durwvcearly_temp

 capture drop lur_all_durwvcearly_p5 
 tssmooth ma lur_all_durwvcearly_p5 = lur_all_durwvcearly_p, window(2 1 2)
 capture drop hp_lur_all_durwvcearly_p5 
 cap n tsfilter hp hp_lur_all_durwvcearly_p5 =  lur_all_durwvcearly_p5  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
 
**
**  SMOOTH THE U DISTRIBUTION PROPERTIONS
**


capture program drop ts_smooth_and_filter_exe
program define ts_smooth_and_filter_exe
			args name_in 

	capture drop sm5_`name_in'
	capture drop hp_sm5_`name_in'
	cap n tssmooth ma sm5_`name_in'= `name_in', window(2 1 2)
	cap n tsfilter hp hp_sm5_`name_in' =  sm5_`name_in'  if quarter>=tq(1985q1) & quarter<=tq(2013q1), smooth(1600)
				
				
end program


ts_smooth_and_filter_exe unemp_cat0q_xlin 
ts_smooth_and_filter_exe unemp_cat1q_xlin 
ts_smooth_and_filter_exe unemp_cat2q_xlin 
ts_smooth_and_filter_exe unemp_cat3q_xlin 
ts_smooth_and_filter_exe unemp_cat4q_xlin 
*ts_smooth_and_filter_exe unemp_cat5q_xlin 


**************************************
** STATS 
**************************************







quietly {
cap log close table5panelBpart2log
log using "${mainresultsdir}/table5A_appxtable8_hpdurdistr_semielas_data.txt", replace text name(table5panelBpart2log)

noisily: display  ""
noisily: display  "------------------------------------------------------------------"
noisily: display  "TABLE 5, PANEL A, ELASTICITY (+ ONLINE APP, TABLE 11, PANEL B)    "
noisily: display  " 		HP FILTERED DUR DISTRIBUTION SEMI-ELASTICITY    		 "
noisily: display  "------------------------------------------------------------------"
noisily: display  ""
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 1-2 MONTHS *****  "
noisily: display  ""
noisily: reg hp_sm5_unemp_cat0q_xlin hp_lur_all_durwvcearly_p5 if unemp_cat0q_xorig!=. & unemp_cat0q!=.
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 1-4 MONTHS *****  "
noisily: display  ""
noisily: reg hp_sm5_unemp_cat1q_xlin hp_lur_all_durwvcearly_p5 if unemp_cat1q_xorig!=. & unemp_cat1q!=.
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 5-8 MONTHS *****  "
noisily: display  ""
noisily: reg hp_sm5_unemp_cat2q_xlin hp_lur_all_durwvcearly_p5 if unemp_cat2q_xorig!=. & unemp_cat2q!=.
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 9-12 MONTHS *****  "
noisily: display  ""
noisily: reg hp_sm5_unemp_cat3q_xlin hp_lur_all_durwvcearly_p5 if unemp_cat3q_xorig!=. & unemp_cat3q!=.
noisily: display  ""
noisily: display  " ***** UNEMPLOYMENT DURATION 13-18 MONTHS *****  "
noisily: display  ""
noisily: reg hp_sm5_unemp_cat4q_xlin hp_lur_all_durwvcearly_p5 if unemp_cat4q_xorig!=. & unemp_cat4q!=.
noisily: display  ""
noisily: display  "Note: we calculated the 5Q smoothed relation both in data and"
noisily: display  " in the model."



cap log close table5panelBpart2log
}


********************************************************************************
** CALCULATING THE NORMALIZATION OF THE DATA SUPER-OCC UDUR DISTR ELASTICITIES
********************************************************************************

*divide the estimated elasticities by the population-weighted (0.5* 1984 distribution + 0.5 2012 distribution) 
* average elasticity




********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"