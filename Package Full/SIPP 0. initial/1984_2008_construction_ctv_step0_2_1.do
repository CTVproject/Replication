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









//====================================================================
// CORE WAVE ANALYSIS FOR PAPER 
//======================================================================

version 13


global gnulin=0
* control parts of the do-file
global locfileversion=0317  // MARCH 2017



					capture program drop capturedrop
					program define capturedrop
					
					local i=1
					while "``i''"!="" {

					set more off
					set varabbrev off

					local dropvar  "``i''"
					display "`dropvar'"
					capture noisily drop `dropvar'
					local ++i
					}
					end program
					
								
		capture program drop idfillout
		program define idfillout
		
			capture drop prs_`1'
			by personkey: egen prs_`1'=max(`1')
		end program idfillout 
			
		capture program drop idfillreplace
		program define idfillreplace
		
			capture drop prs_`1'
			ren `1' prs_`1'
			sort personkey
			by personkey: egen `1'=max(prs_`1')
			drop prs_`1'
		end program idfillreplace 
	
		



/*  LOAD DATA                                           */

use "${outputdata}/corewave_all_ctv.dta", clear

** define lagged switch indicators
sort personkey yearmonth
capture drop lue
gen byte lue=0 if unempl==1 
replace lue=1 if ue[_n+1]==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1


sort personkey yearmonth
capture drop lne
gen byte lne=0 if (unempl==1 | outlf==1)
replace lne=1 if ne[_n+1]==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1


* reduce, if necessary
capture  drop interest_ind
*gen byte interest_ind=1 if lne_c_1tm!=. | lue_c_1tm!=.
gen byte interest_ind=1 if lne!=. | lue!=.
idfillreplace interest_ind
drop if interest_ind!=1
drop interest_ind

save "${outputdata}/corewave_u_n_ctv_v1.dta", replace











/* STATE CODE */

capture drop fpstate
gen int fpstate=tfipsst
replace fpstate=c_tfipsst if panel<1996
replace fpstate=61 if fpstate==23 | fpstate==50
replace fpstate=62 if fpstate==19 | fpstate==37 | fpstate==46 
replace fpstate=63 if fpstate==2 | fpstate==30 | fpstate==16 | fpstate==56 

/* BEA regions 

New England: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island and Vermont
Mideast: Delaware, District of Columbia, Maryland, New Jersey, New York, and Pennsylvania
Great Lakes: Illinois, Indiana, Michigan, Ohio, and Wisconsin
Plains: Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota, and South Dakota
Southeast: Alabama, Arkansas, Florida, Georgia, Kentucky, Louisiana, Mississippi, North Carolina, South Carolina, Tennessee, Virginia, and West Virginia
Southwest: Arizona, New Mexico, Oklahoma, and Texas
Rocky Mountain: Colorado, Idaho, Montana, Utah, and Wyoming
Far West: Alaska, California, Hawaii, Nevada, Oregon, and Washington
*/

capture drop regions
gen regions=1  if  fpstate==9 | fpstate==25 | fpstate==61 | fpstate==33 | fpstate==44 
		// New England: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island and Vermont
replace regions=2  if  fpstate==10 | fpstate==11 | fpstate==24 | fpstate==34 | fpstate==36 | fpstate==42 
		// Mideast: Delaware, District of Columbia, Maryland, New Jersey, New York, and Pennsylvania
replace regions=3  if  fpstate==17 | fpstate==18 | fpstate==26 | fpstate==39 | fpstate==55
		// Great Lakes: Illinois, Indiana, Michigan, Ohio, and Wisconsin
replace regions=4  if  fpstate==62 | fpstate==20 | fpstate==27 | fpstate==29 | fpstate==38 | fpstate==31 | fpstate==62
		// Plains: Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota, and South Dakota
replace regions=5  if  fpstate==1 | fpstate==5 | fpstate==12 | fpstate==13 | fpstate==21 | fpstate==22 | fpstate==28 | fpstate==37 | fpstate==45 | fpstate==47 | fpstate==51 | fpstate==54 | fpstate==91
		// Southeast: Alabama, Arkansas, Florida, Georgia, Kentucky, Louisiana, Mississippi, North Carolina, South Carolina, Tennessee, Virginia, and West Virginia
replace regions=6  if  fpstate==4 | fpstate==35 | fpstate==40 | fpstate==48
		// Southwest: Arizona, New Mexico, Oklahoma, and Texas
replace regions=7  if  fpstate==8 | fpstate==49 | fpstate==63 
		// Rocky Mountain: Colorado, Idaho, Montana, Utah, and Wyoming, Alaska
replace regions=8  if  fpstate==53 | fpstate==32 | fpstate==41 | fpstate==6 | fpstate==15 
		// Far West: California, Hawaii, Nevada, Oregon, and Washington
		


capture drop firstlastwave
gen byte firstlastwave=0
capture replace firstlastwave=1 if wave==1|wave==2
capture replace firstlastwave=1 if wave>=7 & panel==1986             ! 7 waves
capture replace firstlastwave=1 if wave>=7 & panel==1987             ! 7 waves
capture replace firstlastwave=1 if wave>=6 & panel==1988             ! 6 waves
capture replace firstlastwave=1 if wave>=8 & panel==1990             ! 8 waves
capture replace firstlastwave=1 if wave>=8 & panel==1991             ! 8 waves
capture replace firstlastwave=1 if wave>=9 & panel==1992             ! 9 waves
capture replace firstlastwave=1 if wave>=9 & panel==1993             ! 9 waves
capture replace firstlastwave=1 if wave>=12 & panel==1996            ! 12 waves
capture replace firstlastwave=1 if wave>=9 & panel==2001             ! 9 waves
capture replace firstlastwave=1 if wave>=12 & panel==2004            ! 12 waves
capture replace firstlastwave=1 if wave>=16 & panel==2008             ! 16 waves


sort panel personkey yearmonth
capture drop interview_no2		
gen interview_no2=1 if personkey!=personkey[_n-1]
replace interview_no2=interview_no2[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
replace interview_no2=1 if personkey==personkey[_n-1] & yearmonth!=yearmonth[_n-1]+1
replace interview_no2=interview_no2[_n-1]+1 if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1
replace interview_no2=1 if personkey==personkey[_n-1] & yearmonth!=yearmonth[_n-1]+1


//======== PRELIMINARIES

* make hh identifier
capture drop hh_idx
destring personkey, gen(hh_idx)
replace hh_idx=hh_idx/1000 if panel<1996
replace hh_idx=hh_idx/10000 if panel>=1996
replace hh_idx=floor(hh_idx) if panel<1996
replace hh_idx=floor(hh_idx) if panel>=1996
cap n drop hh_ids93
gen hh_ids93=string(hh_idx,"%20.0g")

capture drop hh_id_2
destring personkey, gen(hh_id_2)
replace hh_id_2=hh_id_2/100000 if panel<1996
replace hh_id_2=hh_id_2/10000000 if panel>=1996
replace hh_id_2=floor(hh_id_2) if panel<1996
replace hh_id_2=floor(hh_id_2) if panel>=1996
cap n drop hh_ids93_2
gen hh_ids93_2=string(hh_id_2,"%20.0g")

svyset hh_ids93 [pw=wpfinwgt],  strata(strat) singleunit(scaled)

global wavecond " & wave>4"
global sttg "1"


sort panel personkey yearmonth
			
			capture drop sumweight
			by panel: egen sumweight=sum(wpfinwgt) if tage>=20 
			by panel: su sumweight
			
			
			// EACH PANEL CONTRIBUTES THE SAME
			capture drop pweight
			gen pweight=wpfinwgt/sumweight
			
			bysort panel: su pweight
			
			// EACH PERSON CONTRIBUTES THE SAME ON AVERAGE, BUT RESPECT RELATIVE WEIGHTING 
			capture drop pweight2
			gen pweight2=.
			
			count if tage>=20 & panel==1984
			replace pweight2=pweight*r(N) if panel==1984
			count if tage>=20 & panel==1985
			replace pweight2=pweight*r(N) if panel==1985
			count if tage>=20 & panel==1986
			replace pweight2=pweight*r(N) if panel==1986

			count if tage>=20 & panel==1987
			replace pweight2=pweight*r(N) if panel==1987
			count if tage>=20 & panel==1988
			replace pweight2=pweight*r(N) if panel==1988
			count if tage>=20 & panel==1990
			replace pweight2=pweight*r(N) if panel==1990
			count if tage>=20 & panel==1991
			replace pweight2=pweight*r(N) if panel==1991
			count if tage>=20 & panel==1992
			replace pweight2=pweight*r(N) if panel==1992
			count if tage>=20 & panel==1993
			replace pweight2=pweight*r(N) if panel==1993
			count if tage>=20 & panel==1996
			replace pweight2=pweight*r(N) if panel==1996
			count if tage>=20 & panel==2001
			replace pweight2=pweight*r(N) if panel==2001
			count if tage>=20 & panel==2004
			replace pweight2=pweight*r(N) if panel==2004
			count if tage>=20 & panel==2008
			replace pweight2=pweight*r(N) if panel==2008
			
			
			count if hh_ids93!="" & panel==1984
			count if hh_ids93!="" & panel==1985
			count if hh_ids93!="" & panel==1986
			count if hh_ids93!="" & panel==1987
			
			count if panel==1984
			count if strat==. & panel==1984
			
			replace strat=core_gvarstrat if panel==1984 & strat==.
			
svyset hh_ids93 [pw=pweight2],  strata(strat) singleunit(scaled)



** CHECK 1984 weights
su wpfinwgt if panel==1984
if r(mean)>10000000 {
	replace wpfinwgt=wpfinwgt/10000 if panel==1984	
}



// =========== BASIC STATS


*** counting spells
count if complete_uspell==1 & lne==1  $wavecond & sample_timetogo>$sttg
count if complete_unuspell==1 & lne==1 $wavecond & sample_timetogo>$sttg
count if complete_unspell==1 & lne==1 $wavecond & sample_timetogo>$sttg
count if complete_nuspell==1 & lne==1 $wavecond & sample_timetogo>$sttg
count if complete_nunspell==1 & lne==1 $wavecond & sample_timetogo>$sttg
count if complete_nstarspell==1 & lne==1 $wavecond & sample_timetogo>$sttg
count if complete_nspell==1 & lne==1 $wavecond & sample_timetogo>$sttg & n_spellength<=18


*** general
sort personkey yearmonth

/* VARIATION:
		if occupation is already mentioned
*/
capture drop occtest
capture drop ue_c_1rm // return mobility
gen ue_c_1rm=.
// check if occ1 at firm+occ change is mentioned earlier in occup1tm1
gen occtest=occup1tm1 if ue_c_1tm==1
replace occtest=occup1tm1 if occtest==.

sort personkey occtest yearmonth 
replace ue_c_1rm=0 if ue_c_1tm==1 & personkey==personkey[_n-1] & occtest==occtest[_n-1] & occtest!=.

// check if it is mentioned in occcup1tm1 when changed to occup2
replace occtest=occup1tm2 if ue_c_1tm==1
sort personkey occtest yearmonth 
replace ue_c_1rm=0 if ue_c_1tm==1 & personkey==personkey[_n-1] & occtest==occtest[_n-1] & occtest!=. & ue_c_1rm==.


// compare with occup1tm2
replace occtest=.
replace occtest=occup1tm2 

sort personkey occtest yearmonth 
replace ue_c_1rm=0 if ue_c_1tm==1 & personkey==personkey[_n-1] & occtest==occtest[_n-1] & occtest!=. & ue_c_1rm==.


replace occtest=occup1tm1 if ue_c_1tm==1
sort personkey occtest yearmonth 
replace ue_c_1rm=0 if ue_c_1tm==1 & personkey==personkey[_n-1] & occtest==occtest[_n-1] & occtest!=. & ue_c_1rm==.

replace ue_c_1rm=1 if ue_c_1tm==1 & ue_c_1rm==.
replace ue_c_1rm=0 if ue_c_1tm==0 & ue_c_1rm==.



sort personkey yearmonth

//----------- generate lagged ue_c_1tm variables
sort personkey yearmonth
capture drop lne_c_1tm
capture drop lne_n_1tm
capture drop lue_c_1rm
gen lne_c_1tm=ne_c_1tm[_n+1] if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
gen lne_n_1tm=ne_n_1tm[_n+1] if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
gen lue_c_1rm=ue_c_1rm[_n+1] if personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1
replace lne_c_1tm=. if lne_c_1tm==0 & lne_n_1tm==0
replace lne_n_1tm=. if lne_c_1tm==.


// BASIC PROPORTION - U SPELL // ---> DELETED CHUNK PROPOCC_TM PROPOCC_NUN ETC


gsort personkey -yearmonth
capture drop compl_n_spellength
gen compl_n_spellength=n_spellength if lne==1 & complete_nspell==1
replace compl_n_spellength=compl_n_spellength[_n-1] if personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]-1 & complete_nspell==1 & n_spellength!=. &  compl_n_spellength[_n-1]!=.
sort personkey yearmonth


**************************************************************

// timeseries indicators
capture drop timeseries_excl
gen byte timeseries_excl=0
replace timeseries_excl=1 if panel==1984 & quarter==tq(1986q1)

replace timeseries_excl=1 if panel==1985 & quarter==tq(1985q4)
replace timeseries_excl=1 if panel==1985 & quarter==tq(1987q2)

replace timeseries_excl=1 if panel==1986 & quarter==tq(1986q4)
replace timeseries_excl=1 if panel==1986 & quarter==tq(1987q4)

replace timeseries_excl=1 if panel==1987 & quarter==tq(1987q4)
replace timeseries_excl=1 if panel==1987 & quarter==tq(1989q1)

replace timeseries_excl=1 if panel==1988 & quarter==tq(1988q4)
replace timeseries_excl=1 if panel==1988 & quarter==tq(1989q3)

replace timeseries_excl=1 if panel==1990 & quarter==tq(1990q4)
replace timeseries_excl=1 if panel==1990 & quarter==tq(1992q2)

replace timeseries_excl=1 if panel==1991 & quarter==tq(1991q4)
replace timeseries_excl=1 if panel==1991 & quarter==tq(1993q2)

replace timeseries_excl=1 if panel==1992 & quarter==tq(1992q4)
replace timeseries_excl=1 if panel==1992 & quarter==tq(1994q3)

replace timeseries_excl=1 if panel==1993 & quarter==tq(1993q4)
replace timeseries_excl=1 if panel==1993 & quarter==tq(1995q3)
replace timeseries_excl=1 if panel==1993 & quarter==tq(1995q4)

replace timeseries_excl=1 if panel==1996 & quarter==tq(1995q4)
replace timeseries_excl=1 if panel==1996 & quarter==tq(1996q1)
replace timeseries_excl=1 if panel==1996 & quarter==tq(1996q2)
replace timeseries_excl=1 if panel==1996 & quarter==tq(1996q3)
replace timeseries_excl=1 if panel==1996 & quarter==tq(1996q4)
replace timeseries_excl=1 if panel==1996 & quarter==tq(1997q1)
replace timeseries_excl=1 if panel==1996 & quarter==tq(1999q4)
replace timeseries_excl=1 if panel==1996 & quarter==tq(2000q1)

replace timeseries_excl=1 if quarter==tq(2000q2)
replace timeseries_excl=1 if quarter==tq(2000q3)
replace timeseries_excl=1 if quarter==tq(2000q3)
replace timeseries_excl=1 if quarter==tq(2000q4)

replace timeseries_excl=1 if panel==2001 & quarter==tq(2001q1)
replace timeseries_excl=1 if panel==2001 & quarter==tq(2001q2)
replace timeseries_excl=1 if panel==2001 & quarter==tq(2001q3)
replace timeseries_excl=1 if panel==2001 & quarter==tq(2001q4)
replace timeseries_excl=1 if panel==2001 & quarter==tq(2003q3)
replace timeseries_excl=1 if panel==2001 & quarter==tq(2003q4)

replace timeseries_excl=1 if panel==2004 & quarter==tq(2004q1)
replace timeseries_excl=1 if panel==2004 & quarter==tq(2004q2)
replace timeseries_excl=1 if panel==2004 & quarter==tq(2004q3)
replace timeseries_excl=1 if panel==2004 & quarter==tq(2004q4)
replace timeseries_excl=1 if panel==2004 & quarter==tq(2007q3)
replace timeseries_excl=1 if panel==2004 & quarter==tq(2007q3)
replace timeseries_excl=1 if panel==2004 & quarter==tq(2007q4)


replace timeseries_excl=1 if panel==2008 & quarter==tq(2008q1)
replace timeseries_excl=1 if panel==2008 & quarter==tq(2008q2)
replace timeseries_excl=1 if panel==2008 & quarter==tq(2008q3)
replace timeseries_excl=1 if panel==2008 & quarter==tq(2008q4)
replace timeseries_excl=1 if panel==2008 & quarter==tq(2009q1)
replace timeseries_excl=1 if panel==2008 & quarter==tq(2009q2)
replace timeseries_excl=1 if panel==2008 & quarter==tq(2013q3)
replace timeseries_excl=1 if panel==2008 & quarter==tq(2013q4)

		
		
		
capture drop timeseries_excl2
gen byte timeseries_excl2=0
replace timeseries_excl2=1 if panel==1984 & quarter==tq(1983q4)
replace timeseries_excl2=1 if panel==1984 & quarter==tq(1984q1)
replace timeseries_excl2=1 if panel==1984 & quarter==tq(1984q2)
*replace timeseries_excl2=1 if panel==1984 & quarter==tq(1986q1)
replace timeseries_excl2=1 if panel==1984 & quarter==tq(1986q2)


replace timeseries_excl2=1 if panel==1985 & quarter==tq(1985q4)
*replace timeseries_excl2=1 if panel==1985 & quarter==tq(1987q2)
replace timeseries_excl2=1 if panel==1985 & quarter==tq(1987q3)

replace timeseries_excl2=1 if panel==1986 & quarter==tq(1986q3)
replace timeseries_excl2=1 if panel==1986 & quarter==tq(1986q4)
replace timeseries_excl2=1 if panel==1986 & quarter==tq(1988q1)
replace timeseries_excl2=1 if panel==1986 & quarter==tq(1988q2)

replace timeseries_excl2=1 if panel==1987 & quarter==tq(1987q4)
*replace timeseries_excl2=1 if panel==1987 & quarter==tq(1989q1)

replace timeseries_excl2=1 if panel==1988 & quarter==tq(1988q4)
replace timeseries_excl2=1 if panel==1988 & quarter==tq(1989q4)

replace timeseries_excl2=1 if panel==1990 & quarter==tq(1990q4)
*replace timeseries_excl2=1 if panel==1990 & quarter==tq(1992q2)
replace timeseries_excl2=1 if panel==1990 & quarter==tq(1992q3)

replace timeseries_excl2=1 if panel==1991 & quarter==tq(1991q4)
*replace timeseries_excl2=1 if panel==1991 & quarter==tq(1993q2)
replace timeseries_excl2=1 if panel==1991 & quarter==tq(1993q3)

replace timeseries_excl2=1 if panel==1992 & quarter==tq(1992q4)
replace timeseries_excl2=1 if panel==1992 & quarter==tq(1994q4)

replace timeseries_excl2=1 if panel==1993 & quarter==tq(1993q4)
replace timeseries_excl2=1 if panel==1993 & quarter==tq(1995q4)

replace timeseries_excl2=1 if panel==1996 & quarter==tq(1995q4)
replace timeseries_excl2=1 if panel==1996 & quarter==tq(1996q1)
replace timeseries_excl2=1 if panel==1996 & quarter==tq(1996q2)
replace timeseries_excl2=1 if panel==1996 & quarter==tq(1996q3)
replace timeseries_excl2=1 if panel==1996 & quarter==tq(1996q4)
replace timeseries_excl2=1 if panel==1996 & quarter==tq(1997q1)
*replace timeseries_excl2=1 if panel==1996 & quarter==tq(1999q4)
replace timeseries_excl2=1 if panel==1996 & quarter==tq(2000q1)

replace timeseries_excl2=1 if quarter==tq(2000q2)
replace timeseries_excl2=1 if quarter==tq(2000q3)
replace timeseries_excl2=1 if quarter==tq(2000q3)
replace timeseries_excl2=1 if quarter==tq(2000q4)

replace timeseries_excl2=1 if panel==2001 & quarter==tq(2001q1)
replace timeseries_excl2=1 if panel==2001 & quarter==tq(2001q2)
replace timeseries_excl2=1 if panel==2001 & quarter==tq(2001q3)
replace timeseries_excl2=1 if panel==2001 & quarter==tq(2001q4)
replace timeseries_excl2=1 if panel==2001 & quarter==tq(2003q4)

replace timeseries_excl2=1 if panel==2004 & quarter==tq(2004q1)
replace timeseries_excl2=1 if panel==2004 & quarter==tq(2004q2)
replace timeseries_excl2=1 if panel==2004 & quarter==tq(2004q3)
replace timeseries_excl2=1 if panel==2004 & quarter==tq(2004q4)
replace timeseries_excl2=1 if panel==2004 & quarter==tq(2007q4)


replace timeseries_excl2=1 if panel==2008 & quarter==tq(2008q1)
replace timeseries_excl2=1 if panel==2008 & quarter==tq(2008q2)
replace timeseries_excl2=1 if panel==2008 & quarter==tq(2008q3)
replace timeseries_excl2=1 if panel==2008 & quarter==tq(2008q4)
replace timeseries_excl2=1 if panel==2008 & quarter==tq(2009q1)
replace timeseries_excl2=1 if panel==2008 & quarter==tq(2009q2)
replace timeseries_excl2=1 if panel==2008 & quarter==tq(2013q4)


// STRINGENT MEASURE
capture drop timeseries_excls
gen byte timeseries_excls=timeseries_excl
replace timeseries_excls=1 if panel==1984 & quarter==tq(1986q1)
replace timeseries_excls=1 if panel==1985 & quarter==tq(1987q2)
replace timeseries_excls=1 if panel==1987 & quarter==tq(1989q1)
replace timeseries_excls=1 if panel==1990 & quarter==tq(1992q2)
replace timeseries_excls=1 if panel==1991 & quarter==tq(1993q2)
replace timeseries_excls=1 if panel==1996 & quarter==tq(1999q4)


**** 1-8 months unemployment duration

capture drop timeseries_excl18
gen byte timeseries_excl18=0
replace timeseries_excl18=1 if panel==1984 & quarter==tq(1983q4)
replace timeseries_excl18=1 if panel==1984 & quarter==tq(1984q1)
replace timeseries_excl18=1 if panel==1984 & quarter==tq(1984q2)
*replace timeseries_excl18=1 if panel==1984 & quarter==tq(1986q1)
replace timeseries_excl18=1 if panel==1984 & quarter==tq(1986q2)


replace timeseries_excl18=1 if panel==1985 & quarter==tq(1985q3)
*replace timeseries_excl18=1 if panel==1985 & quarter==tq(1987q2)
replace timeseries_excl18=1 if panel==1985 & quarter==tq(1987q3)

replace timeseries_excl18=1 if panel==1986 & quarter==tq(1986q3)
replace timeseries_excl18=1 if panel==1986 & quarter==tq(1988q1)
replace timeseries_excl18=1 if panel==1986 & quarter==tq(1988q2)

replace timeseries_excl18=1 if panel==1987 & quarter==tq(1987q3)
replace timeseries_excl18=1 if panel==1987 & quarter==tq(1989q2)

replace timeseries_excl18=1 if panel==1988 & quarter==tq(1988q3)
replace timeseries_excl18=1 if panel==1988 & quarter==tq(1989q4)

replace timeseries_excl18=1 if panel==1990 & quarter==tq(1990q3)
*replace timeseries_excl18=1 if panel==1990 & quarter==tq(1992q2)
replace timeseries_excl18=1 if panel==1990 & quarter==tq(1992q3)

replace timeseries_excl18=1 if panel==1991 & quarter==tq(1991q3)
*replace timeseries_excl18=1 if panel==1991 & quarter==tq(1993q2)
replace timeseries_excl18=1 if panel==1991 & quarter==tq(1993q3)

replace timeseries_excl18=1 if panel==1992 & quarter==tq(1992q3)
replace timeseries_excl18=1 if panel==1992 & quarter==tq(1994q4)

replace timeseries_excl18=1 if panel==1993 & quarter==tq(1993q3)
replace timeseries_excl18=1 if panel==1993 & quarter==tq(1995q4)

replace timeseries_excl18=1 if panel==1996 & quarter==tq(1995q4)
replace timeseries_excl18=1 if panel==1996 & quarter==tq(1996q1)
replace timeseries_excl18=1 if panel==1996 & quarter==tq(1996q2)
replace timeseries_excl18=1 if panel==1996 & quarter==tq(1996q3)
replace timeseries_excl18=1 if panel==1996 & quarter==tq(1996q4)
*replace timeseries_excl18=1 if panel==1996 & quarter==tq(1999q4)
replace timeseries_excl18=1 if panel==1996 & quarter==tq(2000q1)

replace timeseries_excl18=1 if quarter==tq(2000q2)
replace timeseries_excl18=1 if quarter==tq(2000q3)
replace timeseries_excl18=1 if quarter==tq(2000q3)
replace timeseries_excl18=1 if quarter==tq(2000q4)

replace timeseries_excl18=1 if panel==2001 & quarter==tq(2001q1)
replace timeseries_excl18=1 if panel==2001 & quarter==tq(2001q2)
replace timeseries_excl18=1 if panel==2001 & quarter==tq(2001q3)
replace timeseries_excl18=1 if panel==2001 & quarter==tq(2003q4)

replace timeseries_excl18=1 if panel==2004 & quarter==tq(2004q1)
replace timeseries_excl18=1 if panel==2004 & quarter==tq(2004q2)
replace timeseries_excl18=1 if panel==2004 & quarter==tq(2004q3)
replace timeseries_excl18=1 if panel==2004 & quarter==tq(2007q4)


replace timeseries_excl18=1 if panel==2008 & quarter==tq(2008q1)
replace timeseries_excl18=1 if panel==2008 & quarter==tq(2008q2)
replace timeseries_excl18=1 if panel==2008 & quarter==tq(2008q3)
replace timeseries_excl18=1 if panel==2008 & quarter==tq(2008q4)
replace timeseries_excl18=1 if panel==2008 & quarter==tq(2009q1)
replace timeseries_excl18=1 if panel==2008 & quarter==tq(2013q4)




**** 1-4 months unemployment duration

capture drop timeseries_excl14
gen byte timeseries_excl14=0
replace timeseries_excl14=1 if panel==1984 & quarter==tq(1983q4)
*****? replace timeseries_excl14=1 if panel==1984 & quarter==tq(1984q1)
*replace timeseries_excl14=1 if panel==1984 & quarter==tq(1986q1)
replace timeseries_excl14=1 if panel==1984 & quarter==tq(1986q2)


replace timeseries_excl14=1 if panel==1985 & quarter==tq(1985q2)
*replace timeseries_excl14=1 if panel==1985 & quarter==tq(1987q2)
replace timeseries_excl14=1 if panel==1985 & quarter==tq(1987q3)

replace timeseries_excl14=1 if panel==1986 & quarter==tq(1986q2)
replace timeseries_excl14=1 if panel==1986 & quarter==tq(1988q1)
replace timeseries_excl14=1 if panel==1986 & quarter==tq(1988q2)

replace timeseries_excl14=1 if panel==1987 & quarter==tq(1987q2)
replace timeseries_excl14=1 if panel==1987 & quarter==tq(1989q2)

replace timeseries_excl14=1 if panel==1988 & quarter==tq(1988q2)
replace timeseries_excl14=1 if panel==1988 & quarter==tq(1989q4)

replace timeseries_excl14=1 if panel==1990 & quarter==tq(1990q2)
*replace timeseries_excl14=1 if panel==1990 & quarter==tq(1992q2)
replace timeseries_excl14=1 if panel==1990 & quarter==tq(1992q3)

replace timeseries_excl14=1 if panel==1991 & quarter==tq(1991q2)
*replace timeseries_excl14=1 if panel==1991 & quarter==tq(1993q2)
replace timeseries_excl14=1 if panel==1991 & quarter==tq(1993q3)

replace timeseries_excl14=1 if panel==1992 & quarter==tq(1992q2)
replace timeseries_excl14=1 if panel==1992 & quarter==tq(1994q4)

replace timeseries_excl14=1 if panel==1993 & quarter==tq(1993q2)
replace timeseries_excl14=1 if panel==1993 & quarter==tq(1995q4)

replace timeseries_excl14=1 if panel==1996 & quarter==tq(1995q4)
replace timeseries_excl14=1 if panel==1996 & quarter==tq(1996q1)
replace timeseries_excl14=1 if panel==1996 & quarter==tq(1996q2)
*replace timeseries_excl14=1 if panel==1996 & quarter==tq(1999q4)
replace timeseries_excl14=1 if panel==1996 & quarter==tq(2000q1)

replace timeseries_excl14=1 if quarter==tq(2000q2)
replace timeseries_excl14=1 if quarter==tq(2000q3)
replace timeseries_excl14=1 if quarter==tq(2000q3)
replace timeseries_excl14=1 if quarter==tq(2000q4)

replace timeseries_excl14=1 if panel==2001 & quarter==tq(2001q1)
replace timeseries_excl14=1 if panel==2001 & quarter==tq(2001q2)
replace timeseries_excl14=1 if panel==2001 & quarter==tq(2003q4)

replace timeseries_excl14=1 if panel==2004 & quarter==tq(2004q1)
replace timeseries_excl14=1 if panel==2004 & quarter==tq(2004q2)
replace timeseries_excl14=1 if panel==2004 & quarter==tq(2007q4)


replace timeseries_excl14=1 if panel==2008 & quarter==tq(2008q1)
replace timeseries_excl14=1 if panel==2008 & quarter==tq(2008q2)
replace timeseries_excl14=1 if panel==2008 & quarter==tq(2008q3)
replace timeseries_excl14=1 if panel==2008 & quarter==tq(2008q4)
replace timeseries_excl14=1 if panel==2008 & quarter==tq(2013q4)


*****************************************