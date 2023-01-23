

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








******************************************
** CPS DATA REPLICATION OF CTV
******************************************

	/* The author (Ludo Visschers) has applied a CC BY-NC-SA license to this file, 	created 2019-2022 */
	

	* Table 1, column (iii) regresses the HP filtered quarterly time series of mobility 
	* of the unemployed from the CPS on the HP filtered unemployment unemployment rate
	
	* Table 1, column (vi) regresses the individual mobility outcomes 
	* (whether changing occupation or not at the end of an unemployment spell) 
	* on the HP filtered unemployment rate in that quarter
	
	
	/*
	THIS USES CPS - IPUMS DATA
	
	Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren and Michael Westberry. Integrated Public Use Microdata Series, Current Population Survey: Version 10.0 [dataset]. Minneapolis, MN: IPUMS, 2022. https://doi.org/10.18128/D030.V10.0
	*/

	
*******************************
** 0. Initialization
*******************************


	*--- PATHS and other globals
	
global cdate=c(current_date)
global cdate= subinstr("$cdate", " ", "_", .)
display "today is $cdate"



clear all
set maxvar 20000
version 13

cd "$workingdir"
global workingdir `c(pwd)'
do "${workingdir}/global_paths.do"
cd "${tempdata}"

global fileloc "${aggdatadir}"
display "${fileloc}"

set more off
set varabbrev off
*set trace off 



global lstarttime=c(current_time)
global lstartdate=c(current_date)
display "started at ${lstarttime} on ${lstartdate}"

	
	
	
	*--- DATA: from CPS-IPUMS, 
	
	   /*
	   using variables from the basic monthly CPS : 
				cpsidp year month empstat occ2010 qocc lnkfw1mwt age durunemp 
	   over period 1976-2020 
	   
	   these can be directly, and easily, downloaded from https://cps.ipums.org/cps/
	   (for which we want to express our deep gratitude)
	   
	   we further use unemployment data from FRED that can be merged into this file.
	   
	   The first of the code reproduces how we have gone from the overall sample, to 
	   the sample only including unemployment spells (which is the smaller dataset 
	   we supply with the replication package)
	   */

/*  
******** REDUCE DATA SET TO EXITERS FROM UNEMPLOYMENT, WITHOUT IMPUTED OCCS **** 	   
display "CPS directory is ${cpsdir}"
cd "${cpsdir}"
cap n unzipfile cps_ctv.zip
use cps_ctv.dta // file containing cpsidp year month empstat occ2010 qocc lnkfw1mwt age durunemp 
				// from CPS-IPUMS 
	   
		
	*--- generate yearmonth (ym), quarter
capture drop ym
gen ym=ym(year,month)
format ym %tm
		
	*-- only consider those who have unemployment OR NONEMPLOYMENT during time in sample

sort cpsidp ym
capture drop unempsample_temp
gen byte unempsample_temp=0
cap n replace unempsample_temp=1 if empstat>=20 & empstat<=36  	

capture drop unempsample_ind
sort cpsidp ym
by cpsidp: egen unempsample_ind=max(unempsample_temp)

tab unempsample_ind
capture drop unempsample_temp


drop if unempsample_ind!=1
capture drop unempsample_ind


	*--- drop if age<18 | age>66
	
drop if age<18 | age>66

	
	
	sort cpsidp ym
	capture drop keep_ind
	* keep those observations where a worker gets a job out of unemployment, has an occupation before and after that is not imputed
	gen byte keep_ind=1 if cpsidp==cpsidp[_n+1] & ym==ym[_n+1]-1 & empstat>=20 & empstat<=22 & empstat[_n+1]>=10 & empstat[_n+1]<=12 & occ2010!=. & occ2010[_n+1]!=. & (qocc<=0 | qocc>=8) & (qocc[_n+1]<=0 | qocc[_n+1]>=8)

	* before dropping, we have to make sure that we record the subsequent occupation
	capture drop next_occ2010
	gen next_occ2010=occ2010[_n+1] if cpsidp==cpsidp[_n+1] & ym==ym[_n+1]-1 & empstat>=20 & empstat<=22 & empstat[_n+1]>=10 & empstat[_n+1]<=12 & occ2010!=. & occ2010[_n+1]!=. & (qocc<=0 | qocc>=8) & (qocc[_n+1]<=0 | qocc[_n+1]>=8)
	count if next_occ2010==. & keep_ind==1
	*tab next_occ2010 if keep_ind==1, m	
	
	drop if keep_ind!=1
	drop keep_ind
	compress
	
	save "${cpsdir}/cps_excerpt_ctv.dta", replace
*/	
********************************************************************************
use "${cpsdir}/cps_excerpt_ctv.dta", clear	
* generate quarter, season	
capture drop quarter
gen quarter=qofd(dofm(ym))
format quarter %tq
sort cpsidp ym
capture drop season
gen season=quarter-floor(quarter/4)*4+1
format quarter %tq		

	
	*--- unemployment duration
	
capture drop durmonth
gen durmonth=1 if durunemp>=0 & durunemp<=4
replace durmonth=2 if durunemp>=5 & durunemp<=8
replace durmonth=3 if durunemp>=9 & durunemp<=13
replace durmonth=4 if durunemp>=14 & durunemp<=18
replace durmonth=5 if durunemp>=19 & durunemp<=22
replace durmonth=6 if durunemp>=23 & durunemp<=26
replace durmonth=7 if durunemp>=27 & durunemp<=31
replace durmonth=8 if durunemp>=32 & durunemp<=35
replace durmonth=9 if durunemp>=36 & durunemp<=39
replace durmonth=10 if durunemp>=40 & durunemp<=44
replace durmonth=11 if durunemp>=45 & durunemp<=48
replace durmonth=12 if durunemp>=49 & durunemp<=52

replace durmonth=13 if durunemp>=53 & durunemp<=57
replace durmonth=14 if durunemp>=58 & durunemp<=61
replace durmonth=15 if durunemp>=62 & durunemp<=65
replace durmonth=16 if durunemp>=66 & durunemp<=70
replace durmonth=17 if durunemp>=71 & durunemp<=74
replace durmonth=18 if durunemp>=75 & durunemp<=78
	
	
	*--- use major occupational groups (aggregate occ codes)
	
		//PPPPPPPPPPPPPPPPPPPPPPPPPP
		cap n program drop mmocc_exe
		program define mmocc_exe
				*args var_in aggvar_out

		local var_in="`1'"			// original 2000 soc coded variable (3digit)
		local aggvar_out="`2'"

		capture n drop `aggvar_out'
		gen `aggvar_out'=.

		local added_condition=" `var_in'!=. " 
				replace `aggvar_out'=11 if `var_in'>=10 & `var_in'<=430  & `added_condition'
				replace `aggvar_out'=13 if `var_in'>=500 & `var_in'<=950  & `added_condition'
				replace `aggvar_out'=15 if `var_in'>=1000 & `var_in'<=1240  & `added_condition'
				replace `aggvar_out'=17 if `var_in'>=1300 & `var_in'<=1560  & `added_condition'
				replace `aggvar_out'=19 if `var_in'>=1600 & `var_in'<=1960  & `added_condition'
				replace `aggvar_out'=21 if `var_in'>=2000 & `var_in'<=2060  & `added_condition'
				replace `aggvar_out'=23 if `var_in'>=2100 & `var_in'<=2150  & `added_condition'
				replace `aggvar_out'=25 if `var_in'>=2200 & `var_in'<=2550  & `added_condition'
				replace `aggvar_out'=27 if `var_in'>=2600 & `var_in'<=2960  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
				replace `aggvar_out'=29 if `var_in'>=3000 & `var_in'<=3540  & `added_condition'
				replace `aggvar_out'=31 if `var_in'>=3600 & `var_in'<=3650  & `added_condition'
				replace `aggvar_out'=33 if `var_in'>=3700 & `var_in'<=3950  & `added_condition'
				replace `aggvar_out'=35 if `var_in'>=4000 & `var_in'<=4160  & `added_condition'
				replace `aggvar_out'=37 if `var_in'>=4200 & `var_in'<=4250  & `added_condition'
				replace `aggvar_out'=39 if `var_in'>=4300 & `var_in'<=4650  & `added_condition' // added category to capture supervisors of production workers 
				replace `aggvar_out'=41 if `var_in'>=4700 & `var_in'<=4960  & `added_condition'
				replace `aggvar_out'=43 if `var_in'>=5000 & `var_in'<=5930  & `added_condition'
				replace `aggvar_out'=45 if `var_in'>=6000 & `var_in'<=6130  & `added_condition'
				replace `aggvar_out'=47 if `var_in'>=6200 & `var_in'<=6940  & `added_condition'
				replace `aggvar_out'=49 if `var_in'>=7000 & `var_in'<=7620  & `added_condition'
				replace `aggvar_out'=51 if `var_in'>=7700 & `var_in'<=8960  & `added_condition'
				replace `aggvar_out'=53 if `var_in'>=9000 & `var_in'<=9750  & `added_condition'

				
		label define label_mmo  ///
		 11 "Mgt occs" ///
		 13 "Bus&Fin Operations"  ///
		 15 "Computer and math. occ"  ///
		 17 "architect & eng. occ"  ///
		 19 "Life, phys, and socsci occ"  ///
		 21 "Comm & soc service occ"  ///
		 23 "Legal"  ///
		 25 "Educ, training, and library"  ///
		 27 "Arts/Dsgn/Ent/sports/media"  ///
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

		 lab val `aggvar_out' label_mmo
				
		end 
		//PPPPPPPPPPPPPPPPPP

mmocc_exe occ2010 occ00
mmocc_exe next_occ2010 next_occ00
	
*******************************
** 1. Create Mobility indicators and Mobility Quarterly Timeseries
*******************************
	
	
	

		//PPPPPPPPPPPPPPPPPPP
		capture program drop mob_calc_exe
		program define mob_calc_exe
				args occind suffix 
			
			set varabbrev off 
			sort cpsidp ym
			capture drop uocsw`suffix'
			gen uocsw`suffix'=0 if `occind' == next_`occind' & `occind'!=. & next_`occind'!=. $addcondition $maintained_cond
			replace uocsw`suffix'=1 if `occind' != next_`occind' & `occind'!=. & next_`occind'!=. $addcondition $maintained_cond
			tab uocsw`suffix'

			capture drop num`suffix'
			capture drop den`suffix'
			capture drop qocmob`suffix'
			egen num`suffix' = total(uocsw`suffix' * lnkfw1mwt * !missing(uocsw`suffix', lnkfw1mwt)), by(quarter) 
			egen den`suffix' = total(lnkfw1mwt * !missing(uocsw`suffix', lnkfw1mwt)), by(quarter) 
			gen qocmob`suffix' = num`suffix'/den`suffix'
		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPP
 
 

global maintained_cond ""
global addcondition ""


* BASELINE, NO IMPUTED, NO FARMERS ETC
*global maintained_cond "& (qocc<=0 | qocc>=8) & (qocc[_n+1]<=0 | qocc[_n+1]>=8) & occ00!=45 & occ00[_n+1]!=45 "
global maintained_cond " & occ00!=45 & next_occ00!=45 "
mob_calc_exe occ00  _mm	

 /*
*******************************
** 2. Other/Further Quarterly time series
*******************************

	frame create fredu
	frame change fredu

	
/* 
	** one needs to have set a fredkey to be able to download series from FRED https://fred.stlouisfed.org/

	
	import fred UNRATE, daterange(1975-01-01 .) aggregate(quarterly) clear
	gen quarter=qofd(daten)
	format quarter %tq
	gen lunrate=log(UNRATE)
	keep quarter UNRATE lunrate
	save "${fileloc}fred_unemp.dta", replace
	
	
*/
display "if fred_unemp file is not present, uncomment the previous code immediately above"
use "${fileloc}fred_unemp.dta", clear
replace lunrate=log(UNRATE) if lunrate==.

keep quarter lunrate season
tsset quarter

** deseasonalize
capture drop ls_unrate
reg lunrate i.season if quarter<tq(2020q2)
predict ls_unrate, res
	*add constant back
local ave_seas=e(b)[1,1]+e(b)[1,2]+e(b)[1,3]+e(b)[1,4]
replace ls_unrate=ls_unrate+e(b)[1,5]+0.25*`ave_seas' 
lab var ls_unrate "Log Unemployment Rate (deseasonalized)"


** HP filtered unemployment

capture drop hp_lunrate19
tsfilter hp hp_lunrate19 = lunrate if quarter<=tq(2019q4) & quarter>=tq(1979q1), smooth(1600)

capture drop hp_lsunrate19
tsfilter hp hp_lsunrate19 = ls_unrate if quarter<=tq(2019q4) & quarter>=tq(1979q1), smooth(1600)

global outlier_cond "& quarter!=tq(1995q2) & quarter!=tq(1995q3)"
global ts_cond_if ""

save "${fileloc}fred_unemp_hp.dta", replace	

frame change default
frame drop fredu
*/



	
	
** HP filtered mobility

*capture drop season
*gen season=quarter-floor(quarter/4)*4+1

*frame copy default qframe
*frame change qframe
cap n restore, not
preserve 

keep quarter qocmob_mm season
duplicates drop quarter qocmob_mm season, force	
tsset quarter	
	//PPPPPPPPPPPPPPPPPPPPP
		capture program drop ts_constr_exe
		program define ts_constr_exe 
					args occm_varname
		** 1. deal with outliers: 1995q2, 1995q3
				/*
				1995q2 and 1995q3 are outliers, not only in terms of occupational mobility, also in the weight/no. of unemployed, in the distribution of unemployed across reasons for unemployment etc. We therefore ignore these two observations.
				However, for HP filtering we need a series without gaps, so we take the average between observations one year before and one year after, but excl. any interpolated result for these two quarters from our subsequent analysis (also after HP filtering)
				*/
				
		capture drop `occm_varname'_orig
		gen `occm_varname'_orig=`occm_varname'
		replace `occm_varname'=0.5*(`occm_varname'[_n-4]+ `occm_varname'[_n+4]) if quarter>=tq(1995q2) & quarter<=tq(1995q3)
		replace `occm_varname'=0.5*(`occm_varname'[_n-4]+ `occm_varname'[_n+4]) if quarter>=tq(1977q3) & quarter<=tq(1977q3) & `occm_varname'==.



		** 2. log series 
		capture drop l`occm_varname'
		gen l`occm_varname'=log(`occm_varname')

		** 3. deseasonalize	
		reg l`occm_varname' i.season
		capture drop ls`occm_varname'
		predict ls`occm_varname', res
			*add constant back
		replace ls`occm_varname'=ls`occm_varname'+e(b)[1,5] // can 

		** HPFILTER


			* main series
					// note: there is a gap at 1977q3, filled up in l`occm_varname', but not in other series
		capture drop hp_ls`occm_varname'19
		tsfilter hp hp_ls`occm_varname'19 = l`occm_varname' if quarter>=tq(1979q1) & quarter<=tq(2019q4), smooth(1600)


		end 
	//PPPPPPPPPPPPPPPPPPPPPPPPPP
	
	ts_constr_exe qocmob_mm
	
*******************************
** 3a. Quarter on quarter regression
*******************************
	
**>>>>>> Table 1, column (iii)	

*cap n merge 1:1 quarter using "${fileloc}fred_unemp_hp.dta", gen(_merge_quu)
merge m:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta"


tsfilter hp hp_lunrate_bls_full=lunrate_bls, smooth(1600)


*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX	
quietly {
cap log close cpsreglog3
log using "${mainresultsdir}/table1_regs_iii_vi.txt", replace text name(cpsreglog3)

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  " ******      TABLE 1, reg (iii), panel A           ********"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: reg hp_lsqocmob_mm19 hp_lunrate_bls_full i.season if quarter<tq(2020q1)
noisily: display  ""


noisily: display  ""
noisily: display  "------------------------------------------------------------------------"
noisily: display  " ROBUSTNESS: TABLE 1, reg (iii), panel A, restricted to 1984-2013 window"
noisily: display  "------------------------------------------------------------------------"
noisily: display  ""
noisily: reg hp_lsqocmob_mm19 hp_lunrate_bls i.season if quarter>=tq(1984q1) & quarter<=tq(2013q4)
noisily: display  ""

/*
noisily: display  ""
noisily: display  "------------------------------------------------------------------------"
noisily: display  " ROBUSTNESS: TABLE 1, reg (iii), panel A, 1984-2013, seasonality"
noisily: display  "------------------------------------------------------------------------"
noisily: display  ""
noisily: reg hp_lsqocmob_mm19 hp_lunrate_bls i.season  if quarter>=tq(1984q1) & quarter<=tq(2013q4)
noisily: display  ""
*/


*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

keep quarter hp_lunrate_bls_full 
save "${tempdata}/full_hp_lunrate_bls.dta", replace
*frame change default
*frame drop qframe
restore 

*******************************
** 3b. Individual mobility on hp unemployment rate regression
*************************************


cap drop _merge_quu
cap drop _merge_quu2
*cap n merge m:1 quarter using "${fileloc}fred_unemp_hp.dta", gen(_merge_quu)
merge m:1 quarter using "${workingdir}/Aggregate Data/aggdata_ts.dta", gen(_merge_quu)
merge m:1 quarter using "${tempdata}/full_hp_lunrate_bls.dta", gen(_merge_quu2)






**>>>>> Table 1, column (vi)


*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
sort cpsidp ym
*global maintained_cond  "& (qocc<=0 | qocc>=8) & (qocc[_n+1]<=0 | qocc[_n+1]>=8) & occ00!=45 & occ00[_n+1]!=45 "
global maintained_cond  "& occ00!=45 & next_occ00!=45 "
*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  " *****       TABLE 1, reg (vi), panel A           *********"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: reg uocsw_mm hp_lunrate_bls_full quarter i.season [w=lnkfw1mwt] if quarter<=tq(2019q4) & durunemp>=5 & durunemp<=72 & age>=18 & age<=65 $maintained_cond, vce(cluster quarter)
noisily: display  ""

noisily: display  ""
noisily: display  "-----------------------------------------------------------"
noisily: display  " ******      TABLE 1, reg (vi), panel B           *********"
noisily: display  "-----------------------------------------------------------"
noisily: display  ""
noisily: reg uocsw_mm hp_lunrate_bls_full durmonth quarter i.season [w=lnkfw1mwt] if quarter<=tq(2019q4) & durunemp>=5 & durunemp<=72 & age>=18 & age<=65 $maintained_cond, vce(cluster quarter)
noisily: display  ""


noisily: display  ""
noisily: display  "----------------------------------------------------------------"
noisily: display  " ROBUSTNESS: TABLE 1, reg (vi), panel A, restricted to 1984-2013"
noisily: display  "----------------------------------------------------------------"
noisily: display  ""
noisily: reg uocsw_mm hp_lunrate_bls quarter i.season [w=lnkfw1mwt] if quarter>=tq(1984q1) & quarter<=tq(2014q4) & durunemp>=5 & durunemp<=72 & age>=18 & age<=65 $maintained_cond, vce(cluster quarter)
noisily: display  ""

noisily: display  ""
noisily: display  "----------------------------------------------------------------"
noisily: display  " ROBUSTNESS: TABLE 1, reg (vi), panel B, restricted to 1984-2013"
noisily: display  "----------------------------------------------------------------"
noisily: display  ""
noisily: reg uocsw_mm hp_lunrate_bls durmonth quarter i.season [w=lnkfw1mwt] if quarter>=tq(1984q1) & quarter<=tq(2014q4) & durunemp>=5 & durunemp<=72 & age>=18 & age<=65 $maintained_cond, vce(cluster quarter)
noisily: display  ""

/*
noisily: display  ""
noisily: display  "----------------------------------------------------------------"
noisily: display  " ROBUSTNESS: TABLE 1, reg (vi), panel A, 1984-2013, seasonality"
noisily: display  "----------------------------------------------------------------"
noisily: display  ""
noisily: reg uocsw_mm hp_lunrate_bls i.season quarter [w=lnkfw1mwt] if quarter>=tq(1984q1) & quarter<=tq(2014q4) & durunemp>=5 & durunemp<=72 & age>=18 & age<=65 $maintained_cond, vce(cluster quarter)
noisily: display  ""

noisily: display  ""
noisily: display  "----------------------------------------------------------------"
noisily: display  " ROBUSTNESS: TABLE 1, reg (vi), panel B, 1984-2013, seasonality"
noisily: display  "----------------------------------------------------------------"
noisily: display  ""
noisily: reg uocsw_mm hp_lunrate_bls durmonth quarter  i.season  [w=lnkfw1mwt] if quarter>=tq(1984q1) & quarter<=tq(2014q4) & durunemp>=5 & durunemp<=72 & age>=18 & age<=65 $maintained_cond, vce(cluster quarter)
noisily: display  ""
*/


log close cpsreglog3
	

}		

