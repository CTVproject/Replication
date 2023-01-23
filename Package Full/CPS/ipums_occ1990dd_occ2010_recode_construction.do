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

HOW TO CONSTRUCT DATA FROM CPS-IPUMS 

This do file helps construct the occ1990 to occ2010 homogenization used by IPUMS 
for the CPS. To distill it, uses CPS - IPUMS variables of the basic CPS between 
1994 and 2002, which can be downloaded from https://cps.ipums.org/

year            int     %8.0g                 survey year
serial          long    %12.0g                household serial number
month           byte    %8.0g      MONTH      month
hwtfinl         double  %12.0g                household weight, basic monthly
cpsid           double  %12.0g                cpsid, household record
asecflag        byte    %8.0g      ASECFLAG   flag for asec
inttype         byte    %8.0g      INTTYPE    interview type
pernum          byte    %8.0g                 person number in sample unit
wtfinl          double  %12.0g                final basic weight
cpsidp          double  %12.0g                cpsid, person record
empstat         byte    %8.0g      EMPSTAT    employment status
labforce        byte    %8.0g      LABFORCE   labor force status
occ             int     %8.0g                 occupation
occ2010         int     %8.0g      OCC2010    occupation, 2010 basis
occ1990         int     %8.0g      OCC1990    occupation, 1990 basis
ind1990         int     %8.0g      IND1990    industry, 1990 basis
occ1950         int     %8.0g      OCC1950    occupation, 1950 basis
ind             int     %8.0g                 industry
ind1950         int     %8.0g      IND1950    industry, 1950 basis
qempstat        byte    %8.0g      QEMPSTAT   data quality flag for empstat
qind            byte    %8.0g      QIND       data quality flag for ind
qlabforc        byte    %8.0g      QLABFORC   data quality flag for labforce
qocc            byte    %8.0g      QOCC       data quality flag for occ
nolinkflag      byte    %8.0g      NOLINKFLAG
                             flag for records that cannot be linked using cpsidp
*/
	
	
	
	
********************************************************************************			  
** IPUMS downloaded data, saved only the necessary few variables, but without manipulations
********************************************************************************


use "FILL IN IPUMS FILE HERE.dta", clear  
	// this is the CPS-IPUMS extract containing above variables 1976-2022

keep if year>1994 & year<2002
keep occ occ2010 qocc cpsidp year
save "cps_occ_code.dta", replace


/*
--------------------------------------------------------------------------------

FILE USED IN THE PAPER CONTAINS SOME MINOR DIFFERENCES WITH MOST BASIC CONSTRUCTION
HERE, ROOTED IN THE ASSOCIATION OF MULTIPLE 2010 OCCUPATIONS WITH AN OCC1990 OCC. 
WE EXPLICITLY EVEN THESE DIFFERENCES OUT HERE; this concerns less than 1% of pop


 - Operations and systems researchers and analysts (65) here with Bus&Fin Operations, 
	not Computer & Math
 - Transportation ticket and reservation agents (318) with Sales, not Admin support
 - Administrative support occupations, nec (389) with Admin support
 - Public transportation attendants 
 - Personal Service occupations, nec with Health support services, not personal care.
 
 
  - there is no 349 in the (david dorn homogenized) SIPP, but there is in IPUMS 
	in the older version, unambiguously assigned to admin support based on unique 
	link in IPUMS, we add it here for completeness 

--------------------------------------------------------------------------------
*/




*********************************
** !!!! SET DIRECTORIES !!!!!
*********************************

** in this short program everything plays out in the working directory
	* make sure both the do file and the zipped data file are there
	* as well as the comparison files
			*cps_occ_code.zip
			*occ1990dd2000soc.dta  //comparison file
			*occ1990dd_list.dta   //list of all occ1990dd occupations
	

global workingdir `c(pwd)'


*********************
** OTHER GLOBALS 
********************* 

global checks_on=1				// checks, also to make explicit relation earlier file
global first_version=1			// reconstruct version used in CTV, needs occ1990dd list


cap n unzipfile cps_occ_code.zip, replace
use "cps_occ_code.dta", clear

drop if qocc!=0
keep occ occ2010


duplicates drop occ occ2010, force

** CHECKS 
if ${checks_on}==1 {
	display "check 1: next duplicates report should only have 1 copy each"
	duplicates report occ    // should be only 1 copy each 
	display "check 2: next duplicates report can have multiple occ1990s for 1 occ2010"
	duplicates report occ2010 // can be multiple copies: same occ2010 for more occ1990s
}


sort occ
drop if occ==0

ren occ2010 occ2010new


cap n program drop mm_mmo_adj_exe
program define mm_mmo_adj_exe

local var_in="`1'"			// original 2000 soc coded variable (3digit)
local aggvar_out="`2'"

local added_condition=" `var_in'!=." 
		replace `aggvar_out'=11 if `var_in'>=1 & `var_in'<=43   & `added_condition'
		replace `aggvar_out'=13 if `var_in'>=50 & `var_in'<=95   & `added_condition'
		replace `aggvar_out'=15 if `var_in'>=100 & `var_in'<=124     & `added_condition'
		replace `aggvar_out'=17 if `var_in'>=130 & `var_in'<=156   & `added_condition'
		replace `aggvar_out'=19 if `var_in'>=160 & `var_in'<=196  & `added_condition'
		replace `aggvar_out'=21 if `var_in'>=200 & `var_in'<=206  & `added_condition'
		replace `aggvar_out'=23 if `var_in'>=210 & `var_in'<=215  & `added_condition'
		replace `aggvar_out'=25 if `var_in'>=220 & `var_in'<=255  & `added_condition'
		replace `aggvar_out'=27 if `var_in'>=260 & `var_in'<=296  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		replace `aggvar_out'=29 if `var_in'>=300 & `var_in'<=354  & `added_condition'
		replace `aggvar_out'=31 if `var_in'>=360 & `var_in'<=365  & `added_condition'
		replace `aggvar_out'=33 if `var_in'>=370 & `var_in'<=395  & `added_condition'
		replace `aggvar_out'=35 if `var_in'>=400 & `var_in'<=416 & `added_condition'
		replace `aggvar_out'=37 if `var_in'>=420 & `var_in'<=425  & `added_condition'
		replace `aggvar_out'=39 if `var_in'>=430 & `var_in'<=465  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=41 if `var_in'>=470 & `var_in'<=496  & `added_condition'
		replace `aggvar_out'=43 if `var_in'>=500 & `var_in'<=593  & `added_condition'
		replace `aggvar_out'=45 if `var_in'>=600 & `var_in'<=613  & `added_condition'
		replace `aggvar_out'=47 if `var_in'>=620 & `var_in'<=694  & `added_condition'
		replace `aggvar_out'=49 if `var_in'>=700 & `var_in'<=762  & `added_condition'
		replace `aggvar_out'=51 if `var_in'>=770 & `var_in'<=896 & `added_condition'
		replace `aggvar_out'=53 if `var_in'>=900 & `var_in'<=975  & `added_condition'

		
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



replace occ2010new=floor(occ2010new/10)
lab val occ2010new .
capture drop occ2000mog

** create major occupational groups (soc2010) linked to 3-digit 1990s soc
gen occ2000mog=.
mm_mmo_adj_exe occ2010new occ2000mog


ren occ occ1990dd

** compare with the recode used in the paper 

if ${checks_on}==1 {
merge m:1 occ1990dd using "occ1990dd2000soc.dta"

tab occ1990dd occ2000rec if occ2000mog!=occ2000rec & _merge==3
}

/*
	 
Note that we see 5 out of 300+ 3-digit occupations where the version used for the paper yields a different MOG. The reason is that in this version, we restrict the years to have a unique mapping between occ1990 and occ2010; in an earlier version we did not. This meant that the 'original' classification was soc2000 or soc2010, which was then converted into occ1990 (instead of the other way around). As a result, we had some instances of multiple occ2010 associated with an occ1990 (tag value >=1 in occ1990dd2000soc.dta) and when dropping duplicates in occ1990, stata chose the one to keep without clear criterion. We consider the error to be small, in spirit (because these cases concerns occupations are closely linked in ipums, even if the tie-breaking is forced one way) and in practice, because these occupations only cover a small proportion of the population (<1%).



 tab occ1990dd occ2000rec if occ2000mog!=occ2000rec & _merge==3

           |                       occ2000rec
occupation |        13         31         39         41         43 |     Total
-----------+-------------------------------------------------------+----------
        65 |         1          0          0          0          0 |         1 
       318 |         0          0          0          1          0 |         1 
       389 |         0          0          0          0          1 |         1 
       463 |         0          0          1          0          0 |         1 
       469 |         0          1          0          0          0 |         1 
-----------+-------------------------------------------------------+----------
     Total |         1          1          1          1          1 |         5 


 
*/
** recode the occupations to the old version
if ${first_version}==1 {
	replace occ2000mog=13 if occ1990dd==65
	replace occ2000mog=41 if occ1990dd==318
	replace occ2000mog=43 if occ1990dd==389
	replace occ2000mog=39 if occ1990dd==463
	replace occ2000mog=31 if occ1990dd==469
}


** this is a check that where the file used in the paper does not contain the occ
	* it's because they are merged away in david dorn's 1990 recode: indeed!
if (${checks_on}==1 | ${first_version}==1) {
	
	merge 1:1 occ1990dd using "occ1990dd_list.dta", gen(mergedd)

	* turn this off if we want to keep the occupations that are not part of the 
		* occ1990dd subset
	if ${first_version}==1 {
		drop if mergedd==1
	}

	tab occ1990dd if _merge==1

** finally some occupations are not present in IPUMS (first or this version) but are still present in david
	* dorn's recode. With one exception (which is not present in the new version, yes in the old version), we set these by hand inside the program merge2000soc_exe, not here)

	if ${first_version}==1 {
		drop if mergedd==2
		
	}
	

	
}

	if ${first_version}==1 {
		cap n drop if _merge==1
		cap n drop if _merge==2
		local new = _N + 1
        set obs `new'
		replace occ1990dd=349 if _n==`new'
		replace occ2000mog=43 if _n==`new'
		
	}


/*
THESE ARE THE RECODES USED IN MERGE2000SOC_EXE

			replace `aggvar_out'=21 if `ddvar_in'==177
			replace `aggvar_out'=19 if `ddvar_in'==235
			replace `aggvar_out'=25 if `ddvar_in'==387
			replace `aggvar_out'=51 if `ddvar_in'==408 // laundry, but it is mostly laundry workers in cat 14 (748)

			replace `aggvar_out'=35 if `ddvar_in'==433 // food prep (433)
			replace `aggvar_out'=37 if `ddvar_in'==450  
			replace `aggvar_out'=37 if `ddvar_in'==451
			replace `aggvar_out'=39 if `ddvar_in'==466
			replace `aggvar_out'=39 if `ddvar_in'==467
			replace `aggvar_out'=39 if `ddvar_in'==470
			replace `aggvar_out'=39 if `ddvar_in'==471 // transportation attendant (but also inspector in soc2000?)
			replace `aggvar_out'=39 if `ddvar_in'==472

			replace `aggvar_out'=47 if `ddvar_in'==653
			replace `aggvar_out'=51 if `ddvar_in'==684
			replace `aggvar_out'=51 if `ddvar_in'==702
			replace `aggvar_out'=51 if `ddvar_in'==789
			replace `aggvar_out'=53 if `ddvar_in'==834
			replace `aggvar_out'=51 if `ddvar_in'==873

the following occupations are present in the new version, and the recode matches
the new version, with the exception of 467, which goes into occ2000mog=39 in the original,
but occ2000mog=25 in the new version. (This concerns Early childhood teacher's 
assistants, in original to personal services, in new version to educators)
177, 235, 387, 433, 466, 467, 653, 684, 789, 834
			--> to keep with the original data construction, these occupations
				are dropped here, but then later covered in merge2000soc_exe


the following occupations are neither present in the old or new version of the 
IPUMS 'cross walk'
408, 450, 451, 470, 471, 472, 873
			
			
*/
if ${first_version}==1 {
		drop if occ1990dd==177
		drop if occ1990dd==235
		drop if occ1990dd==387
		drop if occ1990dd==433
		drop if occ1990dd==466
		drop if occ1990dd==467
		drop if occ1990dd==653
		drop if occ1990dd==684
		drop if occ1990dd==789
		drop if occ1990dd==834	
}

cap drop _merge
cap drop mergedd
keep occ1990dd occ2000mog

if ${checks_on}==1 {
	merge 1:1 occ1990dd using "C:\Users\lvissche\Dropbox\CTV_Revisions\Replication\SIPP 0. initial\occ_ind_recodes\occ1990dd2000soc.dta", gen(mergefinal)
	count if occ2000mog!=occ2000rec & occ2000mog!=.
	drop if mergefinal==2
	drop tag mergefinal occ2000rec
}
ren occ2000mog occ2000rec


save occ1990dd2000soc_2022.dta, replace 