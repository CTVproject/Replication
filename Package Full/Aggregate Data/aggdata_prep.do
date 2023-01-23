


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


****************************
** LOAD IN (.DTA-ify) AGGREGATE DATA
****************************





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




*** unemployment rates
global maindir "$workingdir"

* from FRED: SERIES UNRATE, https://fred.stlouisfed.org/series/UNRATE
import excel "${maindir}/Aggregate Data/UNRATE.xls", sheet("FRED Graph") cellrange(A11:B310) firstrow clear
gen quarter=qofd(observation_date)
format quarter %tq
drop observation_date
save "${maindir}/Aggregate Data/unrate_fred.dta", replace

* from BLS, SERIES LNS14000000Q: https://data.bls.gov/cgi-bin/srgate (accessed Nov 2022)
import excel "${maindir}/Aggregate Data/LNS14000000Q.xlsx", sheet("LNS14000000Q") cellrange(A18:D317) firstrow clear
destring Year, replace
gen quarter=qofd(dofy(Year))
format quarter %tq
replace quarter=quarter+1 if Period=="Q02"
replace quarter=quarter+2 if Period=="Q03"
replace quarter=quarter+3 if Period=="Q04"
ren ObservationValue unemp_bls
keep quarter unemp_bls
save "${maindir}/Aggregate Data/unrate_bls.dta", replace

* from BLS: SERIES LNS14000000Q: (accessed 2017), 0.1pp difference in 2015q1 and 2016q1, revised subsequently)
import excel "${maindir}/Aggregate Data/LNS14000000Q_2016.xlsx", sheet("LNS14000000Q") cellrange(A1:B278) firstrow clear
gen quarter=qofd(date)
format quarter %tq
drop date
save "${maindir}/Aggregate Data/unrate_bls_0.dta", replace

	
	
	
*** vacancies (HWI series, kindly provided by Regis Barnichon)
import delimited "${maindir}/Aggregate Data/HWIseries.csv", clear	
gen mth=monthly(month, "YM")
gen quarter=qofd(dofm(mth))
format quarter %tq
bysort quarter: egen hwiq=sum(hwi)
replace hwiq=hwiq/3
keep quarter hwiq
duplicates drop
duplicates report quarter
ren hwiq hwi
save "${maindir}/Aggregate Data/hwi.dta", replace

*** output per worker input series, from https://data.bls.gov/cgi-bin/srgate
/* 	prs85006043
	lns12032189q
	lns12032192q
	PRS84006043
	LNS12034560Q
	LNS12035019Q
*/	
**** putting all together, starting from bls output/worker dataset
use "${maindir}/Aggregate Data/aggdata_outpw_bls.dta", clear
drop if quarter==.
merge 1:1 quarter using "${maindir}/Aggregate Data/hwi.dta"
drop _merge
merge 1:1 quarter using "${maindir}/Aggregate Data/unrate_bls.dta"
drop _merge 
merge 1:1 quarter using "${maindir}/Aggregate Data/unrate_fred.dta"
drop _merge 

/**** THE FOLLOWING IS THE BLS UNEMP SERIES AS ACCESSED IN 2017 ****/
/*			with some slight differences with the current BLS unemp rates */
merge 1:1 quarter using "${maindir}/Aggregate Data/unrate_bls_0.dta"
drop _merge

*****************************
** CONSTRUCT OUTPUT PER WORKER, TIGHTNESS
*****************************


* data from BLS, https://data.bls.gov/cgi-bin/srgate (accessed Nov 2022)

	*** Using Nonfarm Business sector. Main measure
	/*
	PRS85006043	Major Sector Productivity and Costs	 Sector:	Nonfarm Business	
					Measure:	Output	 Duration:	Index, base year = 100
	LNS12032189Q   Q	(Seas) Employment Level 
					- Nonagriculture, Private Industries Wage and Salary Workers	
	LNS12032192Q   Q	(Seas) Employment Level 
						 - Nonagriculture, Self-employed Workers, Unincorporated
	*/
	capture drop outpw
	gen outpw= (prs85006043/(lns12032189q+lns12032192q))

	*** Using Business Sector
	capture drop outpw2
	gen outpw2=(PRS84006043/(LNS12034560Q+LNS12035019Q))*100



	capture drop tightness
	gen tightness=hwi/UNRATE


	tsset quarter








****************************
** LOG AND DETREND
****************************


// DETRENDING LOGS 
capture drop loutpw
capture drop loutpw2
capture drop lunrate
capture drop lunrate_bls
capture drop lunrate_bls_0
capture drop ltightness
capture drop lvacancy

gen loutpw2=log(outpw2)
gen loutpw=log(outpw)
gen lunrate=log(UNRATE)
gen lunrate_bls=log(unemp_bls)
gen lunrate_bls_0=log(unrate_bls_0)
gen ltightness=log(tightness)
gen lvacancy=log(hwi)

tsfilter hp hpn_lunrate hpn_lunrate_bls hpn_loutpw hpn_loutpw2 = lunrate lunrate_bls loutpw loutpw2 if quarter>=tq(1983q2) & quarter<=tq(2014q4), smooth(1600)
tsfilter hp hp_lunrate hp_lunrate_bls hp_loutpw hp_loutpw2 = lunrate lunrate_bls loutpw loutpw2 if quarter<=tq(2016q2), smooth(1600)
 
capture drop hpf_lunrate_bls
tsfilter hp hpf_lunrate_bls= lunrate_bls if quarter<=tq(2016q2)
capture drop hpf_lunrate_bls_0
tsfilter hp hpf_lunrate_bls_0= lunrate_bls_0 if quarter<=tq(2016q2)



save "${maindir}/Aggregate Data/aggdata_ts.dta", replace

***************************
** REPORT FOR CALIBRATION
***************************

**use  "C:/Users/lvissche/Dropbox/CTV_Revisions/Aggregate Data Updated/aggdata_u_y_may2017.dta", clear
*use "C:/data/aggdata_2018.dta", clear
*use "C:/Users/lvissche/Dropbox/data in cloud/data/aggdata_2018.dta", clear

global filename "${mainresultsdir}/table2_appxtable4_ ts_outpw.xls"
local sheetname "ts_stats"
cap n putexcel set "${filename}"
cap n putexcel set "${filename}", sheet("`sheetname'", replace)
cap n putexcel set "${filename}", modify sheet("`sheetname'", replace)

 putexcel A3=("autocorrelation")
 putexcel A4=("specification")
 putexcel A5=("std dev")
 putexcel A6=("corr w/u")
 putexcel A7=("emp el w/u")
 putexcel A8=("emp el w/y")
 tsset quarter
 
 
 putexcel B1=("PRODUCTIVITY")
 putexcel B2=("hp_loutpw")
 reg hp_loutpw l.hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel B3=(regtable[1,1])
 putexcel B4=(e(cmdline))
 su hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel B5=(r(sd))
 corr hp_loutpw hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel B6=(r(rho))
 reg hp_loutpw hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel B7=(regtable[1,1])
 capture drop test_ypred
 predict test_ypred
 
 /*
 putexcel C1=("UNEMPLOYMENT")
 putexcel C2=("hp_lunrate")
 reg hp_lunrate l.hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel C3=(regtable[1,1])
 putexcel C4=(e(cmdline))
 su hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel C5=(r(sd))
 corr hp_lunrate hp_lunrate if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 putexcel C6=(r(rho))
 reg hp_lunrate hp_loutpw if quarter>=tq(1984q1) & quarter<=tq(2013q4) 
 matrix regtable=r(table)
 putexcel C8=(regtable[1,1])
 capture drop test_upred
 predict test_upred
*/



