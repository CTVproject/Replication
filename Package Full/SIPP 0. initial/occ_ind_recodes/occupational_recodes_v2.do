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

This file takes the raw tjboccs and ejbinds, and maps them into uniform classifications

* SETUP *

1) define occup3basic1/2 and ind3basic1/2 to capture industry and occupations of ongoing jobs
2) recode locc1bfr/aft-1/2 lind1bfr/aft
 
 
** VARIABLES THAT NEED TO BE THERE in order to do this definition
yearmonth, personkey, panel
tjbocc1/2 ajbocc1/2 ejbind1/2 ajbind1/2 and the related variables in different panels
tsjdate1/2 tejdate1/2
empl, unempl, outlf
eeno1/2
firmno

*/



//********************************************************************************8
// CREATING THE ADDITIONAL MEASURES
//*********************************************************************************

// original 3digit indicators
		* note we have use the different codes, for each panel (unless overlap
		* rename 

		
				*********************************************************************************
				***** keep BOTH FIRM'S INFO WHEN employed in both firms tm2
				*********************************************************************************

				sort personkey yearmonth 
				
				capture drop occup3basic1
				capture drop occup3basic2
					
				gen occup3basic1=.
				gen occup3basic2=.


				* replace "if ${panelcondition} & " by "if ${panelcondition} &  $panelcondition &", where global panelcondition=PANEL in question 
		
// 1984 & 1985?
sort personkey yearmonth
global panelcondition " (panel>=1984 & panel<=1985) "	
	
				replace occup3basic1=. if ${panelcondition} 
				replace occup3basic2=. if ${panelcondition} 
				
	
				replace occup3basic1=c_tjbocc1  if ${panelcondition} &  empl==1 & c_tjbocc1 !=. & ajbocc1!=1
				replace occup3basic2=c_tjbocc2  if ${panelcondition} &  empl==1 & c_tjbocc2 !=. & ajbocc2!=1
				replace occup3basic1=. if occup3basic1==0
				replace occup3basic2=. if occup3basic2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==. & eeno1!=.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==. & eeno2!=.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

/*
// 1985 
				replace occup3basic1=c_tjbocc1  if ${panelcondition} &  empl==1 & c_tjbocc1 !=. & ajbocc1!=1
				replace occup3basic2=c_tjbocc2  if ${panelcondition} &  empl==1 & c_tjbocc2 !=. & ajbocc2!=1



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==. & eeno1!=.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==. & eeno2!=.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.
*/

// 1986 (-1993?)
sort personkey yearmonth
global panelcondition " (panel>=1986 & panel<=1993) "	


				replace occup3basic1=tjbocc1  if   (panel>=1986 & panel<=1993)  &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if   (panel>=1986 & panel<=1993)  &  empl==1 & tjbocc2 !=. & ajbocc2==0
				replace occup3basic1=. if occup3basic1==0 &   (panel>=1986 & panel<=1993)  
				replace occup3basic2=. if occup3basic2==0 &   (panel>=1986 & panel<=1993)  



				** set occupation to missing, if   (panel>=1986 & panel<=1993)  &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if   (panel>=1986 & panel<=1993)  &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace occup3basic2=.  if   (panel>=1986 & panel<=1993)  &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if   (panel>=1986 & panel<=1993)  &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if   (panel>=1986 & panel<=1993)  &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

/*
/// 1987 & 1988?

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

		



// 1990

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.




// 1991
				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

// 1992

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.



// 1993

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & (ajbocc1==0 | ajbocc1==.)
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & (ajbocc2==0 | ajbocc2==.)



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

*/	
		
// 1996 (-2001?)
sort personkey yearmonth
global panelcondition " (panel>=1996 & panel<=2001) "	

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0 
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace occup3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

/*
// 2001

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

		
*/
				
				
// 2004 (-2008)
sort personkey yearmonth
global panelcondition " (panel>=2004 & panel<=2008) "	

				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0 & tjbocc1!=984
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0 & tjbocc1!=984



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace occup3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

				
				
/*
// 2008


				replace occup3basic1=tjbocc1  if ${panelcondition} &  empl==1 & tjbocc1 !=. & ajbocc1==0
				replace occup3basic2=tjbocc2  if ${panelcondition} &  empl==1 & tjbocc2 !=. & ajbocc2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace occup3basic1=.  if ${panelcondition} &  (tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6)
				replace occup3basic2=.  if ${panelcondition} &  (tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6)

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & occup3basic2[_n-1]!=. & occup3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic1[_n-1]!=. & occup3basic1==. & occup3basic2==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & occup3basic2[_n-1]!=. & occup3basic2==. & occup3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace occup3basic1=occup3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic1[_n-1]!=. & occup3basic1==.
				replace occup3basic2=occup3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & occup3basic2[_n-1]!=. & occup3basic2==.

				* with tm2 data in pre-1996 waves
				*replace occup3basic1=occprevjb[_n+4] if ${panelcondition} &  wave==1 & wave[_n+3]==1 & empl!=1

*/
				replace occup3basic1=. if occup3basic1==0
				replace occup3basic2=. if occup3basic2==0

//*****************************8
//  INDUSTRIES
//******************************


/*
A quick note:

 before 1996: use core wave industries (because of job-id recoding 1990-1993, and because it has an imputation flag attached (as with occupations)
 relative to occupations, note that c_tjboccx recoded into tjboccx for 1986-1993, and iwsxocc into ajboccx. no such recoding has taken place for industries, os we use 
 the original variable names.

*/


		
				*********************************************************************************
				***** keep BOTH FIRM'S INFO WHEN employed in both firms tm2
				*********************************************************************************

				capture drop ind3basic1
				capture drop ind3basic2
					
				gen ind3basic1=.
				gen ind3basic2=.
			
				/*
				imputation: ajbind1/2 for panel>=1996
				imputation: iws1/2ind for panel<=1993
				
				which industry variable:
				c_ejbind1/2 pre-1996: USE 1984, 1985
				fp_ejbind1/2 pre-1996: 
				ejbind1/2 1996 and later
				
				*/

				* replace "if ${panelcondition} & " by "if ${panelcondition} &  $panelcondition &", where global panelcondition=PANEL in question 
		
// 1984 & 1985?
sort personkey yearmonth
global panelcondition " (panel>=1984 & panel<=1985)  "	
	
	
				replace ind3basic1=c_ejbind1  if ${panelcondition} &  empl==1 & c_ejbind1!=. & c_ejbind1>0  & iws1ind!=1
				replace ind3basic2=c_ejbind2  if ${panelcondition} &  empl==1 & c_ejbind2!=. & c_ejbind2>0  & iws2ind!=1



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace ind3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace ind3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & ind3basic1[_n-1]!=. & ind3basic1==. & eeno1!=.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & ind3basic2[_n-1]!=. & ind3basic2==. & eeno2!=.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic1[_n-1]!=. & ind3basic1==. & ind3basic2==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic2[_n-1]!=. & ind3basic2==. & ind3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic2[_n-1]!=. & ind3basic2==.

// 1986 (-1993?)
sort personkey yearmonth
global panelcondition " (panel>=1986 & panel<=1993) "	


				replace ind3basic1=c_ejbind1  if ${panelcondition} &  empl==1 & c_ejbind1!=. & c_ejbind1>0  & iws1ind==0
				replace ind3basic2=c_ejbind2  if ${panelcondition} &  empl==1 & c_ejbind2!=. & c_ejbind2>0  & iws2ind==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace ind3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace ind3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & c_ejbhrs1!=. & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & c_ejbhrs2!=. & ind3basic2[_n-1]!=. & ind3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic1[_n-1]!=. & ind3basic1==. & ind3basic2==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic2[_n-1]!=. & ind3basic2==. & ind3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic2[_n-1]!=. & ind3basic2==.


// 1996 (-2008?)
sort personkey yearmonth
global panelcondition " (panel>=1996 & panel<=2008) "	

				replace ind3basic1=ejbind1  if ${panelcondition} &  empl==1 & ejbind1!=. & ejbind1>0  & ajbind1==0
				replace ind3basic2=ejbind2  if ${panelcondition} &  empl==1 & ejbind2!=. & ejbind2>0  & ajbind2==0



				** set occupation to missing, if ${panelcondition} &  starting date hasn't arrived, or ending date has passed
				replace ind3basic1=.  if ${panelcondition} &  ((tsjdate1!=. & tsjdate1> dofm(yearmonth)+15) | (tejdate1!=. & tejdate1<dofm(yearmonth)+6))
				replace ind3basic2=.  if ${panelcondition} &  ((tsjdate2!=. & tsjdate2> dofm(yearmonth)+15) | (tejdate2!=. & tejdate2<dofm(yearmonth)+6))

							
				* missing occupations if ${panelcondition} &  employment spell continuous and firm is the same
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno1==eeno1[_n-1] & ejbhrs1!=. & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & eeno2==eeno2[_n-1] & ejbhrs2!=. & ind3basic2[_n-1]!=. & ind3basic2==.


				* only fill in occupation when employment continues but it is unclear where... use occupation(s) previous period
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic1[_n-1]!=. & ind3basic1==. & ind3basic2==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==1 & ind3basic2[_n-1]!=. & ind3basic2==. & ind3basic1==.


				* missing occupations during unemployment spell  (since this comes after filling up the employment spells, an occupation is taken 
				*               at most only until the next employment spell, not further)
				replace ind3basic1=ind3basic1[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic1[_n-1]!=. & ind3basic1==.
				replace ind3basic2=ind3basic2[_n-1]  if ${panelcondition} &  personkey==personkey[_n-1] & empl==0 & ind3basic2[_n-1]!=. & ind3basic2==.

				
				replace ind3basic1=. if ind3basic1==0
				replace ind3basic2=. if ind3basic2==0
				
				
//======================================================
//  DEFINING (L)OCCBEFORE AND (L)OCCAFTER FOR THE BASIC, AND THE DAVID DORNED MEASURE 
//======================================================

			
/* slightly different procedure from before
			STEP 1: define loccbefore_3basic1/2, loccafter_3basic1/2, mark the active occupation in case one of the two has to be chosen
			STEP 2: recode these in whatever preferred measure, dd
			STEP 3: chose the active loccbefore, loccafter occupation pair
*/

// LOCCBFR_3basic1/2, LOCCAFT_3basic1/2, LINDBFR_3basic1/2, LINDAFTER_3basic1/2, for all lne==1, for which it is possible
capture program drop indocc_beforeafter_exe
program define indocc_beforeafter_exe
	
	sort personkey yearmonth
	
	local var_in="`1'"
	local var_outbefore="`2'"
	local var_outafter="`3'"
	capture drop l`var_outbefore'1
	capture drop l`var_outbefore'2
	capture drop l`var_outafter'1
	capture drop l`var_outafter'2
	
	gen l`var_outbefore'1=`var_in'1 if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	gen l`var_outbefore'2=`var_in'2 if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	gen l`var_outafter'1=`var_in'1[_n+1] if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	gen l`var_outafter'2=`var_in'2[_n+1] if lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1

	
	end 

	* DEFINE LOCC LIND
	indocc_beforeafter_exe ind3basic ind3bfr_b ind3aft_b
	indocc_beforeafter_exe occup3basic occ3bfr_b occ3aft_b
 	
// MARK DOMINANT FIRM, JUST IN CASE: BE CAREFUL!!!!
	capture drop firm_ind
	gen int firm_ind=1 if firmno==eeno1 & eeno1!=. & empl==1
	replace firm_ind=2 if firmno==eeno2 & eeno2!=. & empl==1
	replace firm_ind=firm_ind[_n-1]  if personkey==personkey[_n-1] & empl==0 & firm_ind==.
						
	capture drop lfirm_ind
	gen int lfirm_ind=1 if firmno[_n+1]==eeno1[_n+1] & eeno1[_n+1]!=.  & lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
	replace lfirm_ind=2 if firmno[_n+1]==eeno2[_n+1] & eeno2[_n+1]!=.  & lne==1 & personkey==personkey[_n+1] & yearmonth==yearmonth[_n+1]-1 & empl==0 & empl[_n+1]==1
						








//============================================================================================
// INDUSTRY AGGREGATION  & NEW OCCUPATIONAL DEFINITIONS
//=========================+===================================================================


// 1980 Census Industrial Classification (until 91 panels)
// 1990 Census Industrial Classification (1992-2001 panels)
// 2000 Census Industrial Classification (2004-2008 panels)

capture program drop indagg_exe
program define indagg_exe

local var_out="`2'"
local var_in="`1'"

*local var_in="ind3basic1"

capture drop tempagg   // 13 
capture drop tempaggm 	// merged manufacturing, ...

gen int tempagg=.
gen int tempaggm=.

// 1980 Census Industrial Classification, aggregated -- 1990 Census Industries, aggregated

* 1= AGRICULTURE
replace tempagg=1 if `var_in'>=1 & `var_in'<=32 & panel<=2001 
replace tempaggm=1 if `var_in'>=1 & `var_in'<=32 & panel<=2001 


* 2= MINING
replace tempagg=2 if `var_in'>=40 & `var_in'<=50 & panel<=2001 
replace tempaggm=2 if `var_in'>=40 & `var_in'<=50 & panel<=2001 


* 3= CONSTRUCTION
replace tempagg=3 if `var_in'>=60 & `var_in'<=60 & panel<=2001 
replace tempaggm=3 if `var_in'>=60 & `var_in'<=60 & panel<=2001 

* 4= MANUFACTURING, AGGREGATE: MEASURE 2
replace tempaggm=4 if `var_in'>=100 & `var_in'<=392 & panel<=2001 


	* 4= MANUFACTURING OF NON-DURABLES, MEASURE 1
	replace tempagg=4 if `var_in'>=100 & `var_in'<=222 & panel<=2001 


	* 5= MANUFACTURING OF DURABLES, MEASURE 1
	replace tempagg=5 if `var_in'>=230 & `var_in'<=392 & panel<=2001 


* 6= TRANSPORTATION/COMMUNICATIONS/UTILITY
replace tempagg=6 if `var_in'>=400 & `var_in'<=472 & panel<=2001 
replace tempaggm=6 if `var_in'>=400 & `var_in'<=472 & panel<=2001 

* 7= WHOLESALE TRADE, AGGREGATED: MEASURE 2
replace tempaggm=7 if `var_in'>=500 & `var_in'<=571 & panel<=2001 

	* 7= WHOLESALE TRADE, DURABLE
	replace tempagg=7 if `var_in'>=500 & `var_in'<=532 & panel<=2001 

	* 8= WHOLESALE TRADE, NON-DURABLE
	replace tempagg=8 if `var_in'>=540 & `var_in'<=571 & panel<=2001 

* 9= RETAIL TRADE
replace tempagg=9 if `var_in'>=580 & `var_in'<=691 & panel<=2001 
replace tempaggm=9 if `var_in'>=580 & `var_in'<=691 & panel<=2001 

* 10= FIRE
replace tempagg=10 if `var_in'>=700 & `var_in'<=712 & panel<=2001 
replace tempaggm=10 if `var_in'>=700 & `var_in'<=712 & panel<=2001 

* 11 BUSINESS & REPAIR 
replace tempagg=11 if `var_in'>=721 & `var_in'<=760 & panel<=2001 
replace tempaggm=11 if `var_in'>=721 & `var_in'<=760 & panel<=2001 

	* special case: R&D, split out into commercial R&D, and scientific etc. R&D in 1980 classificaiton, becomes R&D in 1990 classificaiton
	replace tempagg=14 if `var_in'==730 & panel<=1991
	replace tempaggm=14 if `var_in'==730 & panel<=1991 

* 12 PERSONAL SERVICES 
replace tempagg=12 if `var_in'>=761 & `var_in'<=791 & panel<=2001 
replace tempaggm=12 if `var_in'>=761 & `var_in'<=791 & panel<=2001 


* 13 ENTERTAINMENT AND RECREATION
replace tempagg=13 if `var_in'>=800 & `var_in'<=810 & panel<=2001 
replace tempaggm=13 if `var_in'>=800 & `var_in'<=810 & panel<=2001 
	

* 14 PROFESSIONAL AND RELATED SERVICES
replace tempagg=14 if `var_in'>=812 & `var_in'<=893 & panel<=2001 
replace tempaggm=14 if `var_in'>=812 & `var_in'<=893 & panel<=2001 
	

* 15 PUBLIC ADMINISTRATION
replace tempagg=15 if `var_in'>=900 & `var_in'<=932 & panel<=2001 
replace tempaggm=15 if `var_in'>=900 & `var_in'<=932 & panel<=2001 
		
	
	
	
// 2000 Census Industrial Classification



* 1= AGRICULTURE
replace tempagg=1 if `var_in'>=170 & `var_in'<=290 & panel>=2004 & panel<=2008  
replace tempaggm=1 if `var_in'>=170 & `var_in'<=290 & panel>=2004 & panel<=2008  


* 2= MINING
replace tempagg=2 if `var_in'>=370 & `var_in'<=490 & panel>=2004 & panel<=2008  
replace tempaggm=2 if `var_in'>=370 & `var_in'<=490 & panel>=2004 & panel<=2008  


* 3= CONSTRUCTION
replace tempagg=3 if `var_in'>=770 & `var_in'<=770 & panel>=2004 & panel<=2008  
replace tempaggm=3 if `var_in'>=770 & `var_in'<=770 & panel>=2004 & panel<=2008  

* 4= MANUFACTURING, AGGREGATE: MEASURE 2
replace tempaggm=4 if `var_in'>=1070 & `var_in'<=3990 & panel>=2004 & panel<=2008  


	* 4= MANUFACTURING OF NON-DURABLES, MEASURE 1
	replace tempagg=4 if `var_in'>=1070 & `var_in'<=2390 & panel>=2004 & panel<=2008  


	* 5= MANUFACTURING OF DURABLES, MEASURE 1
	replace tempagg=5 if `var_in'>=2470 & `var_in'<=3990 & panel>=2004 & panel<=2008  

	// EXCEPTIONS:
	* retail bakeries
	replace tempagg=9 if `var_in'>=1190 & `var_in'<=1190 & panel>=2004 & panel<=2008  
	replace tempaggm=9 if `var_in'>=1190 & `var_in'<=1190 & panel>=2004 & panel<=2008  


* 6= TRANSPORTATION/COMMUNICATIONS/UTILITY

/* transportation: 1990: 400-432; 2000: 6070-6390 */
replace tempagg=6 if `var_in'>=6070 & `var_in'<=6390 & panel>=2004 & panel<=2008  
replace tempaggm=6 if `var_in'>=6070 & `var_in'<=6390 & panel>=2004 & panel<=2008  

/* communication ==> information: tricky category! */
replace tempagg=6 if `var_in'>=6670 & `var_in'<=6695 & panel>=2004 & panel<=2008  
replace tempaggm=6 if `var_in'>=6670 & `var_in'<=6695 & panel>=2004 & panel<=2008  
 
 * others in the information category
 	* newspaper and other publishers: 1990 manufacturing, nondur
	replace tempagg=4 if `var_in'>=6470 & `var_in'<=6480 & panel>=2004 & panel<=2008  
	replace tempaggm=4 if `var_in'>=6470 & `var_in'<=6480 & panel>=2004 & panel<=2008  
	* software publishing and sound recording and other information/data processing services to business services
	replace tempagg=11 if (`var_in'==6490 | `var_in'==6590 | `var_in'==6780 | `var_in'==6790) & panel>=2004 & panel<=2008  
	replace tempaggm=11 if (`var_in'==6490 | `var_in'==6590 | `var_in'==6780 | `var_in'==6790) & panel>=2004 & panel<=2008  
	* motion pictures and video to entertainment
	replace tempagg=13 if (`var_in'==6570 ) & panel>=2004 & panel<=2008  
	replace tempaggm=13 if (`var_in'==6570 ) & panel>=2004 & panel<=2008  
		*libraries to prof and rel services 
	replace tempagg=14 if (`var_in'==6770 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'==6770 ) & panel>=2004 & panel<=2008  


/* utility */
replace tempagg=6 if `var_in'>=0570 & `var_in'<=0690 & panel>=2004 & panel<=2008  
replace tempaggm=6 if `var_in'>=0570 & `var_in'<=0690 & panel>=2004 & panel<=2008  
 
	
* 7= WHOLESALE TRADE, AGGREGATED: MEASURE 2
replace tempaggm=7 if `var_in'>=4070 & `var_in'<=4590 & panel>=2004 & panel<=2008  

	* 7= WHOLESALE TRADE, DURABLE
	replace tempagg=7 if `var_in'>=4070 & `var_in'<=4290 & panel>=2004 & panel<=2008  

	* 8= WHOLESALE TRADE, NON-DURABLE
	replace tempagg=8 if `var_in'>=4370 & `var_in'<=4590 & panel>=2004 & panel<=2008  

* 9= RETAIL TRADE
replace tempagg=9 if `var_in'>=4670 & `var_in'<=5790 & panel>=2004 & panel<=2008  
replace tempaggm=9 if `var_in'>=4670 & `var_in'<=5790 & panel>=2004 & panel<=2008  

* 10= FIRE
replace tempagg=10 if `var_in'>=6870 & `var_in'<=7070 & panel>=2004 & panel<=2008  
replace tempaggm=10 if `var_in'>=6870 & `var_in'<=7070 & panel>=2004 & panel<=2008  

* 11 BUSINESS & REPAIR 
/* messy concordance, lots of back and forth with prof and related services */
replace tempagg=11 if `var_in'>=7080 & `var_in'<=7780 & panel>=2004 & panel<=2008  
replace tempaggm=11 if `var_in'>=7080 & `var_in'<=7780 & panel>=2004 & panel<=2008  

	* special case: video rental
	replace tempagg=13 if `var_in'==7170 & panel>=2004 & panel<=2008  
	replace tempaggm=13 if `var_in'==7170 & panel>=2004 & panel<=2008  

	* special case: veterinary and landscape to agriculture
	replace tempagg=1 if (`var_in'==7480 | `var_in'==7770 ) & panel>=2004 & panel<=2008  
	replace tempaggm=1 if (`var_in'==7480 | `var_in'==7770 ) & panel>=2004 & panel<=2008  

	* special case: travel arrangement to TRANSPORT
	replace tempagg=6 if `var_in'==7670 & panel>=2004 & panel<=2008  
	replace tempaggm=6 if `var_in'==7670 & panel>=2004 & panel<=2008  

	
	* special case: waste management to UTILITIES
	replace tempagg=6 if `var_in'==7790 & panel>=2004 & panel<=2008  
	replace tempaggm=6 if `var_in'==7790 & panel>=2004 & panel<=2008  

	* to prof services
	/* 727 728 729 739 746 749 */
	replace tempagg=14 if ((`var_in'>=7270 & `var_in'<=7290 ) | `var_in'==7390 | `var_in'== 7460 | `var_in'==7490 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if ((`var_in'>=7270 & `var_in'<=7290 ) | `var_in'==7390 | `var_in'== 7460 | `var_in'==7490 )& panel>=2004 & panel<=2008  

* 12 PERSONAL SERVICES 
replace tempagg=12 if `var_in'>=8660 & `var_in'<=9090 & panel>=2004 & panel<=2008  
replace tempaggm=12 if `var_in'>=8660 & `var_in'<=9090 & panel>=2004 & panel<=2008  

	* to retail
	replace tempagg=14 if (`var_in'== 8680 | `var_in'==8690 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'>=8680 & `var_in'<=8690 ) & panel>=2004 & panel<=2008  
	
	* to business services and repair
	replace tempagg=14 if (`var_in'>= 8770 & `var_in'<=8880 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'>=8770 & `var_in'<=8880 )  & panel>=2004 & panel<=2008  

* 13 ENTERTAINMENT AND RECREATION
replace tempagg=13 if (`var_in'>=8560 & `var_in'<=8590 ) & panel>=2004 & panel<=2008  
replace tempaggm=13 if (`var_in'>=8560 & `var_in'<=8590 ) & panel>=2004 & panel<=2008  
		
	*museums to prof services
	replace tempagg=14 if (`var_in'>=8570 & `var_in'<=8570 ) & panel>=2004 & panel<=2008  
	replace tempaggm=14 if (`var_in'>=8570 & `var_in'<=8570 ) & panel>=2004 & panel<=2008  


* 14 PROFESSIONAL AND RELATED SERVICES
replace tempagg=14 if `var_in'>=7860 & `var_in'<=8470 & panel>=2004 & panel<=2008  
replace tempaggm=14 if `var_in'>=7680 & `var_in'<=8470 & panel>=2004 & panel<=2008  
replace tempagg=14 if `var_in'>=9160 & `var_in'<=9190 & panel>=2004 & panel<=2008  
replace tempaggm=14 if `var_in'>=9160 & `var_in'<=9190 & panel>=2004 & panel<=2008  

replace tempagg=12 if `var_in'>=9290 & `var_in'<=9290 & panel>=2004 & panel<=2008  
replace tempaggm=12 if `var_in'>=9290 & `var_in'<=9290 & panel>=2004 & panel<=2008  
	

* 15 PUBLIC ADMINISTRATION
replace tempagg=15 if `var_in'>=9370 & `var_in'<=9590 & panel>=2004 & panel<=2008  
replace tempaggm=15 if `var_in'>=9370 & `var_in'<=9590 & panel>=2004 & panel<=2008  
		


// LABELING THE VARIABLES

capture drop `var_out'
capture drop `var_out'm
ren tempagg `var_out'
ren tempaggm `var_out'm

end 

indagg_exe ind3basic1 ind1basic1
indagg_exe ind3basic2 ind1basic2

// 13-16 major industry groups
indagg_exe lind3bfr_b1 lind1bfr_b1
indagg_exe lind3bfr_b2 lind1bfr_b2
indagg_exe lind3aft_b1 lind1aft_b1
indagg_exe lind3aft_b2 lind1aft_b2

* renaming the aggregated variables so they end in 1 or 2, referring to eeno1, eeno2

capture drop lind1aft_bm1
capture drop lind1aft_bm2
capture drop lind1bfr_bm1
capture drop lind1bfr_bm2


ren lind1aft_b1m lind1aft_bm1
ren lind1aft_b2m lind1aft_bm2
ren lind1bfr_b1m lind1bfr_bm1
ren lind1bfr_b2m lind1bfr_bm2






//==================================================================================
// DIFFERENT AGGREGATIONS FOR OCCUPATIONS
//==================================================================================

* DD
*indocc_beforeafter_exe occup3basic occ3bfr_b occ3aft_b

/*
some changes: 
- groundskeepers to services (not agriculture)
- inspectors (SOC2000 874) covers prec prod inspectros and machine operators inspectors
- electr(on)ic equipment assembler (soc80 683; soc90 683; soc00: 772), mapped into 785 (soc90dd) assemblers; 
- soc00 770 First-line supervisors/managers of production and operating workers (51-1011) recoded to operators/assemblers: 14. NOTE THAT THIS IS A TRICKY THING THAT
	NEEDS TO BE KEPT AN EYE ON: ROBuSTNESS EXERCISE
	
- other questionmarks: merging laundry operators with private household laundry (soc90:403), even though 432 is housekeeping cleaners is a separate category in soc 2000
- 613 supervisors mining, do not put as 628 supervisors precision production (soc00: 620). Keep it as 613
- soc800 673 (patternmakers); soc90: misc apparel; soc00 844 (not in dd concordance?)
- soc00 846: textile workers, all others? to 13 or 14?  (similar problem as 770, it spans 13 and 14)

*/


******** BEFORE OCC
*** 1980 SOC
capture drop occ
capture n drop occ1990dd
capture drop locc3bfr_dd1
capture drop locc3bfr_dd2

gen occ=locc3bfr_b1 if panel<=1991 	
capture drop _merge
*merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90adj.dta"
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd locc3bfr_dd1
capture drop _merge
capture n drop occ
gen occ=locc3bfr_b2 if panel<=1991 	
*merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90adj.dta"
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90a.dta"
drop if _merge==2
ren occ1990dd locc3bfr_dd2
*** 1990 SOC
capture drop occ
gen occ=locc3bfr_b1 if panel>1991 & panel<=2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3bfr_dd1=occ1990dd if panel>1991 & panel<=2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3bfr_b2 if panel>1991 & panel<=2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3bfr_dd2=occ1990dd  if panel>1991 & panel<=2001 	
capture n drop occ
cap n drop occ1990dd
**** 2000 SOC 
gen occ=locc3bfr_b1 if panel>2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3bfr_dd1=occ1990dd if panel>2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3bfr_b2 if panel>2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3bfr_dd2=occ1990dd  if panel>2001 	
cap n drop occ
cap n drop occ1990dd

replace locc3bfr_dd1=702 if locc3bfr_b1==770 & panel>=2004 & panel<=2008 
replace locc3bfr_dd2=702 if locc3bfr_b2==770 & panel>=2004 & panel<=2008 
replace locc3bfr_dd1=487 if locc3bfr_b1==602 & panel>=2004 & panel<=2008 
replace locc3bfr_dd2=487 if locc3bfr_b2==602 & panel>=2004 & panel<=2008 

******** AFTEROCC
*** 1980 SOC
capture drop occ
capture drop locc3aft_dd1
capture drop locc3aft_dd2

gen occ=locc3aft_b1 if panel<=1991 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd locc3aft_dd1
capture drop _merge
capture n drop occ
gen occ=locc3aft_b2 if panel<=1991 	
merge m:1 occ using "${step0codedir}/occ_ind_recodes/80dd90.dta"
drop if _merge==2
ren occ1990dd locc3aft_dd2
*** 1990 SOC
capture drop occ
gen occ=locc3aft_b1 if panel>1991 & panel<=2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3aft_dd1=occ1990dd if panel>1991 & panel<=2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3aft_b2 if panel>1991 & panel<=2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/90dd90.dta"
drop if _merge==2
replace locc3aft_dd2=occ1990dd  if panel>1991 & panel<=2001 	
capture n drop occ
cap n drop occ1990dd
**** 2000 SOC 
gen occ=locc3aft_b1 if panel>2001 	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3aft_dd1=occ1990dd if panel>2001 	
cap n drop occ
cap n drop occ1990dd
gen occ=locc3aft_b2 if panel>2001	
capture drop _merge
merge m:1 occ using "${step0codedir}/occ_ind_recodes/00dd90.dta"
drop if _merge==2
replace locc3aft_dd2=occ1990dd  if panel>2001 	
cap n drop occ
cap n drop occ1990dd


replace locc3aft_dd1=702 if locc3aft_b1==770 & panel>=2004 & panel<=2008 
replace locc3aft_dd2=702 if locc3aft_b2==770 & panel>=2004 & panel<=2008 
replace locc3aft_dd1=487 if locc3aft_b1==602 & panel>=2004 & panel<=2008 
replace locc3aft_dd2=487 if locc3aft_b2==602 & panel>=2004 & panel<=2008 

sort personkey yearmonth


//==================================================
// PROGRAM TO SELECT BEFORE AND AFTER OCCUPATION 
//=====================================================
sort personkey yearmonth
capture n program drop mobselection1_exe
program define mobselection1_exe   // [before variables1/2] [after variables1/2]


						local var_bfr= "`1'"
						local var_aft ="`2'"
	set varabbrev off
	capture drop `var_bfr'
	capture drop `var_aft'
	
	gen `var_bfr'=.
	gen `var_aft'=.

	replace `var_bfr'=`var_bfr'1 if  `var_bfr'2==. & `var_bfr'1!=.
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1==. & `var_bfr'2!=.
	** 2 firms before 
	** one overlap, take the overlapping occupation
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & (`var_bfr'2!=`var_aft'1 & `var_bfr'2!=`var_aft'2)
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & (`var_bfr'1!=`var_aft'1 & `var_bfr'1!=`var_aft'2)
	** double overlap, which one counts? GO WITH THE OVERLAP with the dominant occupation before the unemployment spell
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & firm_ind==1
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & firm_ind==2
	** 2 firms before, but no overlap
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)  & firm_ind==1
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)  & firm_ind==2
	
	replace `var_aft'=`var_aft'1 if  `var_aft'2==. & `var_aft'1!=.
	replace `var_aft'=`var_aft'2 if  `var_aft'1==. & `var_aft'2!=.
	** 2 firms after
	** one overlap, take the overlapping occupation
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & (`var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2)
	** double overlap, which one counts? GO WITH THE OVERLAP with the dominant occupation before the unemployment spell
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & firm_ind==1
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & firm_ind==2
	** 2 firms after, but no overlap with previous firms
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2) & lfirm_ind==1
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2) & lfirm_ind==2

	
	
end 

//======================================================
** AGGREGATION -- STANDARD 
//======================================================

capture program drop aggregate1_exe
program define aggregate1_exe

display " ============RECODING AT A MAJOR OCCUPATIONAL GROUP LEVEL=========================="

local var_in="`1'"
local aggvar_out="`2'"

if "`3'"==""{
local added_condition=" panel>=1984 "
}
if "`3'"!=""{
local added_condition="`3'"
}
capture drop `aggvar_out'
gen `aggvar_out'=.

		replace `aggvar_out'=1 if `var_in'>=4 & `var_in'<=37   & `added_condition'
		replace `aggvar_out'=2 if `var_in'>=43 & `var_in'<=199   & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=203 & `var_in'<=235     & `added_condition'
		replace `aggvar_out'=4 if `var_in'>=243 & `var_in'<=285   & `added_condition'
		replace `aggvar_out'=5 if `var_in'>=303 & `var_in'<=389  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=403 & `var_in'<=407  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=413 & `var_in'<=427  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=433 & `var_in'<=471  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=408 & `var_in'<=408  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		
		replace `aggvar_out'=9 if `var_in'>=472 & `var_in'<=476  & `added_condition'
		replace `aggvar_out'=9 if `var_in'>=477 & `var_in'<=499  & `added_condition'
		replace `aggvar_out'=11 if `var_in'>=503 & `var_in'<=549  & `added_condition'
		replace `aggvar_out'=12 if `var_in'>=553 & `var_in'<=617 & `added_condition'
		replace `aggvar_out'=13 if `var_in'>=628 & `var_in'<=699  & `added_condition'
		replace `aggvar_out'=14 if `var_in'>=702 & `var_in'<=702  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=14 if `var_in'>=703 & `var_in'<=799  & `added_condition'
		replace `aggvar_out'=15 if `var_in'>=803 & `var_in'<=859  & `added_condition'
		replace `aggvar_out'=16 if `var_in'>=863 & `var_in'<=889  & `added_condition'



label define label_1dd 1 "managing occupations" 2 "professional speciality" 3 "technicians and rel support" ///
		4 "sales occ." 5 "admin support" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val `aggvar_out' label_1dd		

end
sort personkey yearmonth
aggregate1_exe locc3aft_dd1 locc1aft_dd1
aggregate1_exe locc3aft_dd2 locc1aft_dd2
aggregate1_exe locc3bfr_dd1 locc1bfr_dd1
aggregate1_exe locc3bfr_dd2 locc1bfr_dd2


** 13 MOG, with separate protective services, hh private services, and one group incl. (merged) construction/extractive/prec prod/mechanics and repairers. 
 

capture program drop aggregate_mog_exe
program define aggregate_mog_exe

display " ============RECODING AT A MAJOR OCCUPATIONAL GROUP LEVEL=========================="

local var_in="`1'"
local aggvar_out="`2'mog"

if "`3'"==""{
local added_condition=" panel>=1984 "
}
if "`3'"!=""{
local added_condition="`3'"
}

/*
check out the following: 450 451 for a coding of agric into services, and 785 799 for coding of prec prod into machine operators

*/


capture drop `aggvar_out'
gen `aggvar_out'=.

		replace `aggvar_out'=1 if `var_in'>=4 & `var_in'<=37   & `added_condition'
		replace `aggvar_out'=2 if `var_in'>=43 & `var_in'<=199   & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=203 & `var_in'<=235     & `added_condition'
		replace `aggvar_out'=4 if `var_in'>=243 & `var_in'<=285   & `added_condition'
		replace `aggvar_out'=5 if `var_in'>=303 & `var_in'<=389  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=403 & `var_in'<=408  & `added_condition'
		replace `aggvar_out'=7 if `var_in'>=413 & `var_in'<=427  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=433 & `var_in'<=471  & `added_condition'
		replace `aggvar_out'=8 if `var_in'>=408 & `var_in'<=408  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		replace `aggvar_out'=9 if `var_in'>=472 & `var_in'<=476  & `added_condition'
		replace `aggvar_out'=10 if `var_in'>=477 & `var_in'<=499  & `added_condition'
		replace `aggvar_out'=11 if `var_in'>=503 & `var_in'<=549  & `added_condition'
		replace `aggvar_out'=12 if `var_in'>=553 & `var_in'<=617 & `added_condition'
		replace `aggvar_out'=13 if `var_in'>=628 & `var_in'<=699  & `added_condition'
		replace `aggvar_out'=14 if `var_in'>=702 & `var_in'<=702  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=14 if `var_in'>=703 & `var_in'<=799  & `added_condition'
		replace `aggvar_out'=15 if `var_in'>=803 & `var_in'<=859  & `added_condition'
		replace `aggvar_out'=16 if `var_in'>=863 & `var_in'<=889  & `added_condition'


label define label_1mog 1 "managing occupations" 2 "professional speciality" 3 "technicians and rel support" ///
		4 "sales occ." 5 "admin support" 6 "priv hh services" 7 "protective services" 8 "services" 9 "farming/fish/logging" 11 "mech/precprod/constr" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val `aggvar_out' label_1mog		

end

/*
aggregate_mog_exe locc3aft_dd1 locc1aft_dd1
aggregate_mog_exe locc3aft_dd2 locc1aft_dd2
aggregate_mog_exe locc3bfr_dd1 locc1bfr_dd1
aggregate_mog_exe locc3bfr_dd2 locc1bfr_dd2


capture n drop locc1bfr_ddmog1
capture n drop locc1bfr_ddmog2
capture n drop  locc1aft_ddmog1
capture n drop locc1aft_ddmog2
ren locc1bfr_dd1mog locc1bfr_ddmog1
ren locc1bfr_dd2mog locc1bfr_ddmog2
ren locc1aft_dd1mog locc1aft_ddmog1
ren locc1aft_dd2mog locc1aft_ddmog2
*/

* 6 summary occupational groups (sog)

/*
CAN USE STANDARD DD TO AGGREGATE
1 - managers & professional speciality
2 - tech support & admin support & sales
3 - services
4 - farm/forst/fisheries
5 - precision production and craft and repair
6 - operators, fabricators and laborers

*/

/*
cap n drop locc1bfr_sog1
cap n drop locc1bfr_sog2
cap n drop locc1aft_sog1
cap n drop locc1aft_sog2

gen locc1bfr_sog1=.
gen locc1bfr_sog2=.
gen locc1aft_sog1=.
gen locc1aft_sog2=.

cap n program drop dd2sog_exe
program define dd2sog_exe

	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 
	replace `var_out'=2 if `ddvar'==3| `ddvar'==4 | `ddvar'==5 
	replace `var_out'=3 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8
	replace `var_out'=4 if `ddvar'==9 | `ddvar'==10
	replace `var_out'=5 if  `ddvar'==11 | `ddvar'==12 | `ddvar'==13 
	replace `var_out'=6 if  `ddvar'==14 | `ddvar'==15 | `ddvar'==16
	* agriculture to missing
end 

dd2sog_exe locc1bfr_ddmog1 locc1bfr_sog1 
dd2sog_exe locc1bfr_ddmog2 locc1bfr_sog2
dd2sog_exe locc1aft_ddmog1 locc1aft_sog1 
dd2sog_exe locc1aft_ddmog2 locc1aft_sog2 






* 6 groups from Autor and dorn  (ad)

/*
1 - managers & professional speciality & tech support & protective services (& finance)
2 - precision production and craft and repair
3 - transport, construction, mechanics, mining and farm
4 - machine operators, assemblers
5 - clerical retail
6 - service occupations
*/

cap n drop locc1bfr_ad1
cap n drop locc1bfr_ad2
cap n drop locc1aft_ad1
cap n drop locc1aft_ad2

gen locc1bfr_ad1=.
gen locc1bfr_ad2=.
gen locc1aft_ad1=.
gen locc1aft_ad2=.

cap n program drop dd3_2_ad_exe
program define dd3_2_ad_exe

	local var_in="`1'"
	local aggvar_out="`2'"

	if "`3'"==""{
local added_condition=" panel>=1984 "
}
if "`3'"!=""{
local added_condition="`3'"
}

	
		replace `aggvar_out'=1 if `var_in'>=4 & `var_in'<=37   & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=43 & `var_in'<=199   & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=203 & `var_in'<=235     & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=243 & `var_in'<=258   & `added_condition'
		replace `aggvar_out'=2 if `var_in'>=274 & `var_in'<=285   & `added_condition' // only retail sales, no clue what happens to wholesale sellers, 259-273?
		replace `aggvar_out'=2 if `var_in'>=303 & `var_in'<=389  & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=403 & `var_in'<=408  & `added_condition'
		replace `aggvar_out'=1 if `var_in'>=413 & `var_in'<=427  & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=433 & `var_in'<=471  & `added_condition'
		replace `aggvar_out'=3 if `var_in'>=408 & `var_in'<=408  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		replace `aggvar_out'=6 if `var_in'>=472 & `var_in'<=476  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=477 & `var_in'<=499  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=503 & `var_in'<=549  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=553 & `var_in'<=617 & `added_condition'
		replace `aggvar_out'=4 if `var_in'>=628 & `var_in'<=699  & `added_condition'
		replace `aggvar_out'=5 if `var_in'>=702 & `var_in'<=702  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=5 if `var_in'>=703 & `var_in'<=799  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=803 & `var_in'<=859  & `added_condition'
		replace `aggvar_out'=6 if `var_in'>=863 & `var_in'<=889  & `added_condition'

end 

dd3_2_ad_exe locc3bfr_dd1 locc1bfr_ad1 
dd3_2_ad_exe locc3bfr_dd2 locc1bfr_ad2
dd3_2_ad_exe locc3aft_dd1 locc1aft_ad1 
dd3_2_ad_exe locc3aft_dd2 locc1aft_ad2 
*/


//================================
* 4 groups from cortes et al (hs)
//================================
cap n drop locc1bfr_hs1
cap n drop locc1bfr_hs2
cap n drop locc1aft_hs1
cap n drop locc1aft_hs2

gen locc1bfr_hs1=.
gen locc1bfr_hs2=.
gen locc1aft_hs1=.
gen locc1aft_hs2=.

cap n program drop dd2hs_exe
program define dd2hs_exe

	
	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 | `ddvar'==3
	replace `var_out'=2 if `ddvar'==4 | `ddvar'==5 
	replace `var_out'=3 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8
	replace `var_out'=4 if `ddvar'==11 | `ddvar'==12 | `ddvar'==13 | `ddvar'==14 | `ddvar'==15 | `ddvar'==16
	* agriculture to missing
	
end 

dd2hs_exe locc1bfr_dd1 locc1bfr_hs1 
dd2hs_exe locc1bfr_dd2 locc1bfr_hs2
dd2hs_exe locc1aft_dd1 locc1aft_hs1 
dd2hs_exe locc1aft_dd2 locc1aft_hs2 

** with transportation reassigned

cap n drop locc1bfr_ths1
cap n drop locc1bfr_ths2
cap n drop locc1aft_ths1
cap n drop locc1aft_ths2

gen locc1bfr_ths1=.
gen locc1bfr_ths2=.
gen locc1aft_ths1=.
gen locc1aft_ths2=.

cap n program drop dd2ths_exe
program define dd2ths_exe

	
	local ddvar="`1'"
	local var_out="`2'"
	
	replace `var_out'=1 if `ddvar'==1 | `ddvar'==2 | `ddvar'==3
	replace `var_out'=2 if `ddvar'==4 | `ddvar'==5 
	replace `var_out'=3 if `ddvar'==6 | `ddvar'==7 | `ddvar'==8 | `ddvar'==15 
	replace `var_out'=4 if `ddvar'==11 | `ddvar'==12 | `ddvar'==13 | `ddvar'==14 | `ddvar'==16
	* agriculture to missing
	
end 

dd2ths_exe locc1bfr_dd1 locc1bfr_ths1 
dd2ths_exe locc1bfr_dd2 locc1bfr_ths2
dd2ths_exe locc1aft_dd1 locc1aft_ths1 
dd2ths_exe locc1aft_dd2 locc1aft_ths2 







/*
//============================
// helpers reassigned, but nothing else
//=============================

cap n drop locc1bfr_nohlp1
cap n drop locc1bfr_nohlp2
cap n drop locc1aft_nohlp1
cap n drop locc1aft_nohlp2

gen locc1bfr_nohlp1=.
gen locc1bfr_nohlp2=.
gen locc1aft_nohlp1=.
gen locc1aft_nohlp2=.


/* THIS PROGRAM HAS THREE ARGUMENTS, because we need both dd variable and original variable */
cap n program drop dd2nohlp_exe
program define dd2nohlp_exe

	
	local ddvar_in="`1'"
	local bvar_in="`2'"
	
	local aggvar_out="`3'"

	if "`4'"==""{
	local added_condition=" panel>=1984 "
	}
	if "`4'"!=""{
	local added_condition="`3'"
	}

	
		replace `aggvar_out'=1 if `ddvar_in'>=4 & `ddvar_in'<=37   & `added_condition'
		replace `aggvar_out'=2 if `ddvar_in'>=43 & `ddvar_in'<=199   & `added_condition'
		replace `aggvar_out'=3 if `ddvar_in'>=203 & `ddvar_in'<=235     & `added_condition'
		replace `aggvar_out'=4 if `ddvar_in'>=243 & `ddvar_in'<=285   & `added_condition'
		replace `aggvar_out'=5 if `ddvar_in'>=303 & `ddvar_in'<=389  & `added_condition'
		replace `aggvar_out'=6 if `ddvar_in'>=403 & `ddvar_in'<=408  & `added_condition'
		replace `aggvar_out'=7 if `ddvar_in'>=413 & `ddvar_in'<=427  & `added_condition'
		replace `aggvar_out'=8 if `ddvar_in'>=433 & `ddvar_in'<=471  & `added_condition'
		replace `aggvar_out'=8 if `ddvar_in'>=408 & `ddvar_in'<=408  & `added_condition' // laudry, coming from industrial laundry and personal service laundry (soc90 748, for ind laundry)
		replace `aggvar_out'=9 if `ddvar_in'>=472 & `ddvar_in'<=476  & `added_condition'
		replace `aggvar_out'=9 if `ddvar_in'>=477 & `ddvar_in'<=499  & `added_condition'
		replace `aggvar_out'=11 if `ddvar_in'>=503 & `ddvar_in'<=549  & `added_condition'
		replace `aggvar_out'=12 if `ddvar_in'>=553 & `ddvar_in'<=617 & `added_condition'
		replace `aggvar_out'=13 if `ddvar_in'>=628 & `ddvar_in'<=699  & `added_condition'
		replace `aggvar_out'=14 if `ddvar_in'>=702 & `ddvar_in'<=702  & `added_condition' // added category to capture supervisors of production workers 
		replace `aggvar_out'=14 if `ddvar_in'>=703 & `ddvar_in'<=799  & `added_condition'
		replace `aggvar_out'=15 if `ddvar_in'>=803 & `ddvar_in'<=859  & `added_condition'
		replace `aggvar_out'=16 if `ddvar_in'>=863 & `ddvar_in'<=889  & `added_condition'
		replace `aggvar_out'=16 if `ddvar_in'>=863 & `ddvar_in'<=889  & `added_condition'
		replace `aggvar_out'=16 if `ddvar_in'>=863 & `ddvar_in'<=889  & `added_condition'
		
		
		
		/*
		// IF USING THE ORIGINAL 1980 DD recode (not the adjusted)
		* construction
		replace `aggvar_out'=12 if ((panel>=1984 & panel<=1991 & (`bvar_in'==865 | `bvar_in'==869)) | (panel>=1992 & panel<=2001 & (`bvar_in'==866 | `bvar_in'==869)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==626 | `bvar_in'==660 | `bvar_in'==673 | `bvar_in'==674))) 
		* extractive 
		replace `aggvar_out'=12 if ((panel>=1984 & panel<=1991 & (`bvar_in'==867 | `bvar_in'==867)) | (panel>=1992 & panel<=2001 & (`bvar_in'==868 | `bvar_in'==868)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==693))) 
	
		// repair
		replace `aggvar_out'=11 if ((panel>=1984 & panel<=1991 & (`bvar_in'==864 | `bvar_in'==864)) | (panel>=1992 & panel<=2001 & (`bvar_in'==865 | `bvar_in'==865)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==761))) 
		// machine feeders/off-bearers to production
		replace `aggvar_out'=14 if ((panel>=1984 & panel<=1991 & (`bvar_in'==878 | `bvar_in'==878)) | (panel>=1992 & panel<=2001 & (`bvar_in'==878 | `bvar_in'==878)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==963))) 
		
							
		// surveyor to tech support (soc80, soc90)
		replace `aggvar_out'=3 if ((panel>=1984 & panel<=1991 & (`bvar_in'==866)) | (panel>=1992 & panel<=2001 & (`bvar_in'==867)))
		*/					

		// USING THE ADJUSTED 1980 DD recode (not the adjusted)
		* construction
		replace `aggvar_out'=12 if ((panel>=1984 & panel<=1991 & (`bvar_in'==865 | `bvar_in'==869)) | (panel>=1992 & panel<=2001 & (`bvar_in'==866 | `bvar_in'==869)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==626 | `bvar_in'==660 | `bvar_in'==673 | `bvar_in'==674))) 
		* extractive 
		replace `aggvar_out'=12 if ((panel>=1984 & panel<=1991 & (`bvar_in'==867 | `bvar_in'==867)) | (panel>=1992 & panel<=2001 & (`bvar_in'==868 | `bvar_in'==868)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==693))) 
	
		// repair
		replace `aggvar_out'=11 if ((panel>=1984 & panel<=1991 & (`bvar_in'==864 | `bvar_in'==864)) | (panel>=1992 & panel<=2001 & (`bvar_in'==865 | `bvar_in'==865)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==761))) 
		// machine feeders/off-bearers to production
		replace `aggvar_out'=14 if ((panel>=1984 & panel<=1991 & (`bvar_in'==878 | `bvar_in'==878)) | (panel>=1992 & panel<=2001 & (`bvar_in'==878 | `bvar_in'==878)) | ///
							(panel>=2001 & panel<=2008 & (`bvar_in'==963))) 
		
							
		// surveyor to tech support (soc80, soc90)
		replace `aggvar_out'=3 if ((panel>=1984 & panel<=1991 & (`bvar_in'==866)) | (panel>=1992 & panel<=2001 & (`bvar_in'==867)))

		
end 

dd2nohlp_exe locc3bfr_dd1 locc3bfr_b1 locc1bfr_nohlp1 
dd2nohlp_exe locc3bfr_dd2 locc3bfr_b2 locc1bfr_nohlp2
dd2nohlp_exe locc3aft_dd1 locc3aft_b1 locc1aft_nohlp1 
dd2nohlp_exe locc3aft_dd2 locc3aft_b2 locc1aft_nohlp2 



//=========================================================================================
**** reassigning management and laborers/helpers/handlers COMPLETELY
//=========================================================================================


** GETTING RID OF LABORERS COMPLETELY


capture program drop nolab_exe
program define nolab_exe // [1 basic 3d dd variable in] [2 basic variable that gives all other occupations that are not changed in this program]
						// [3 output]
local var_in="`1'"
local var_in1d="`2'"
local var_out="`3'"

capture drop `var_out'
gen `var_out'=`var_in1d'					
						
// helpers to their respective occupation
replace `var_out'=11 if `var_in'==865

replace `var_out'=12 if `var_in'==866

replace `var_out'=12 if `var_in'==869

replace `var_out'=13 if `var_in'==873

*garbage collectors to transport
replace `var_out'=14 if `var_in'==875

*machine feeders to production
replace `var_out'=13 if `var_in'==878

*service station workers to repairers/mechanics
replace `var_out'=11 if `var_in'==885

*veh workers/machine cleaners to production
replace `var_out'=14 if `var_in'==887

*packagers to production
replace `var_out'=14 if `var_in'==888

*all others to production (or transport?)
replace `var_out'=14 if `var_in'==889

end

nolab_exe locc3bfr_dd1 locc1bfr_nohlp1 locc1bfr_nolab1
nolab_exe locc3aft_dd1 locc1aft_nohlp1 locc1aft_nolab1
nolab_exe locc3bfr_dd2 locc1bfr_nohlp2 locc1bfr_nolab2
nolab_exe locc3aft_dd2 locc1aft_nohlp2 locc1aft_nolab2

mobselection1_exe locc1bfr_nolab locc1aft_nolab


// ---- SEE MEASUREMENT ERROR CODE

/*


// managers
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc3bfr_dd1<38 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, row nof

tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc3bfr_dd1<38 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 

// 
cap n drop locc1bfr_xi1
cap n drop locc1bfr_xi2
cap n drop locc1aft_xi1
cap n drop locc1aft_xi2


gen locc1bfr_xi1=locc1bfr_dd1
gen locc1bfr_xi2=locc1bfr_dd2
gen locc1aft_xi1=locc1aft_dd1
gen locc1aft_xi2=locc1aft_dd2

// to sales	
replace locc1bfr_xi1=4 if locc3bfr_dd1==4 | locc3bfr_dd1==13 | locc3bfr_dd1==13 | locc3bfr_dd1==18 | locc3bfr_dd1==34  | locc3bfr_dd1==35  
replace locc1bfr_xi2=4 if locc3bfr_dd2==4 | locc3bfr_dd2==13 | locc3bfr_dd2==13 | locc3bfr_dd2==18 | locc3bfr_dd2==34  | locc3bfr_dd2==35
replace locc1aft_xi1=4 if locc3aft_dd1==4 | locc3aft_dd1==13 | locc3aft_dd1==13 | locc3aft_dd1==18 | locc3aft_dd1==34  | locc3aft_dd1==35
replace locc1aft_xi2=4 if locc3aft_dd2==4 | locc3aft_dd2==13 | locc3aft_dd2==13 | locc3aft_dd2==18 | locc3aft_dd2==34  | locc3aft_dd2==35

// to professional specialty
replace locc1bfr_xi1=2 if locc3bfr_dd1==15 | locc3bfr_dd1==36
replace locc1bfr_xi2=2 if locc3bfr_dd2==15 | locc3bfr_dd2==36
replace locc1aft_xi1=2 if locc3aft_dd1==15 | locc3aft_dd1==36
replace locc1aft_xi2=2 if locc3aft_dd2==15 | locc3aft_dd2==36


// to production
replace locc1bfr_xi1=14 if locc3bfr_dd1==19
replace locc1bfr_xi2=14 if locc3bfr_dd2==19
replace locc1aft_xi1=14 if locc3aft_dd1==19
replace locc1aft_xi2=14 if locc3aft_dd2==19

// to clerical
replace locc1bfr_xi1=5 if locc1bfr_xi1==1
replace locc1bfr_xi2=5 if locc1bfr_xi2==1
replace locc1aft_xi1=5 if locc1aft_xi1==1
replace locc1aft_xi2=5 if locc1aft_xi2==1

/*	
// prof speciality
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==2 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, row nof

tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==2 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 
	

// tech support
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==3 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, row nof

tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==3 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 


// fire sales
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==4 & locc3bfr_dd1<=255 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, row nof

// clerical
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==5 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, row nof 
// to which managemetn do clerical workers go?
tab locc3bfr_dd1 locc3aft_dd1 [iw=pweight2] if locc1bfr_dd==5 & locc3bfr_dd2==. & locc1aft_dd==1 & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, row nof 
	

// laborers original coding, pre-2004
tab locc3bfr_b1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==16 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 & panel<2004, row nof
tab locc3bfr_b1 locc3aft_dd1 [iw=pweight2] if locc3bfr_dd1==875 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 & panel<2004

	
	* dd coding, pre2004, not much difference
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==16 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 & panel<2004, row nof
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==16 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 & panel<2004

	* dd coding, post2004
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==16 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 & panel>=2004, row nof
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==16 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 & panel>=2004
tab locc3bfr_b1 locc1aft_dd [iw=pweight2] if locc3bfr_dd1==875 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 & panel>=2004	
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc1bfr_dd==16 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 

/* 
ALGORITHM FOR REPLACING
 - for helper: replace by occupation that is mentioned
 - highway maintenance, and rail laying in 2000 SOC: construction
 - helper surveyor: to tech support
 - service station attendant: to sales (lack of flows to prod)
 - for all others: operator (883 nec movers/handlers could also be assigned to transportation?)
*/
*/

// HELPERS/LABORERS	
// construction 
replace locc1bfr_xi1=12 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==865 | locc3bfr_b1==869)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==866 | locc3bfr_b1==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b1==626 | locc3bfr_b1==660 | locc3bfr_b1==673 | locc3bfr_b1==674))) 
replace locc1bfr_xi2=12 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==865 | locc3bfr_b2==869)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==866 | locc3bfr_b2==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b2==626 | locc3bfr_b2==660 | locc3bfr_b2==673 | locc3bfr_b2==674))) 
replace locc1aft_xi1=12 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==865 | locc3aft_b1==869)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==866 | locc3aft_b1==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b1==626 | locc3aft_b1==660 | locc3aft_b1==673 | locc3aft_b1==674))) 
replace locc1aft_xi2=12 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==865 | locc3aft_b2==869)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==866 | locc3aft_b2==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b2==626 | locc3aft_b2==660 | locc3aft_b2==673 | locc3aft_b2==674))) 
	* extractive 
replace locc1bfr_xi1=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==867 | locc3bfr_b1==867)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==868 | locc3bfr_b1==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b1==693))) 
replace locc1bfr_xi2=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==867 | locc3bfr_b2==867)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==868 | locc3bfr_b2==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b2==693))) 
replace locc1aft_xi1=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==867 | locc3aft_b1==867)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==868 | locc3aft_b1==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b1==693))) 
replace locc1aft_xi2=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==867 | locc3aft_b2==867)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==868 | locc3aft_b2==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b2==693))) 
	
// repair
replace locc1bfr_xi1=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==864 | locc3bfr_b1==864)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==865 | locc3bfr_b1==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b1==761))) 
replace locc1bfr_xi2=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==864 | locc3bfr_b2==864)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==865 | locc3bfr_b2==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b2==761))) 
replace locc1aft_xi1=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==864 | locc3aft_b1==864)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==865 | locc3aft_b1==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b1==761))) 
replace locc1aft_xi2=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==864 | locc3aft_b2==864)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==865 | locc3aft_b2==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b2==761))) 

// service station (Attendant) to sales
replace locc1bfr_xi1=4 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==885 | locc3bfr_b1==885)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==885 | locc3bfr_b1==885)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b1==936))) 
replace locc1bfr_xi2=4 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==885 | locc3bfr_b2==885)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==885 | locc3bfr_b2==885)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b2==936))) 
replace locc1aft_xi1=4 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==885 | locc3aft_b1==885)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==885 | locc3aft_b1==885)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b1==936))) 
replace locc1aft_xi2=4 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==885 | locc3aft_b2==885)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==885 | locc3aft_b2==885)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b2==936))) 
							
// surveyor to tech support (soc80, soc90)
replace locc1bfr_xi1=3 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==866)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==867)))
replace locc1bfr_xi2=3 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==866)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==867)))
replace locc1aft_xi1=3 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==866)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==867)))
replace locc1aft_xi2=3 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==866)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==867)))
							
tab locc3bfr_b1 panel if locc1bfr_xi1==16							
// all others to operators
replace locc1bfr_xi1=14 if locc1bfr_xi1==16 
replace locc1bfr_xi2=14 if locc1bfr_xi2==16
replace locc1aft_xi1=14 if locc1aft_xi1==16
replace locc1aft_xi2=14 if locc1aft_xi2==16


// ASSIGNING ONLY THOSE WHO ARE DIRECTLY LINKED TO ANOTHER OCCUPATION

// managers
tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc3bfr_dd1<38 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, row nof

tab locc3bfr_dd1 locc1aft_dd [iw=pweight2] if locc3bfr_dd1<38 & locc3bfr_dd2==. & locc1aft_dd!=. & panel>=1984 & panel<=2008 ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 & entry_ind==1 & ///
	tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1 

// 
cap n drop locc1bfr_aj1
cap n drop locc1bfr_aj2
cap n drop locc1aft_aj1
cap n drop locc1aft_aj2


gen locc1bfr_aj1=locc1bfr_dd1
gen locc1bfr_aj2=locc1bfr_dd2
gen locc1aft_aj1=locc1aft_dd1
gen locc1aft_aj2=locc1aft_dd2

// to sales	
replace locc1bfr_aj1=4 if locc3bfr_dd1==34  | locc3bfr_dd1==26  
replace locc1bfr_aj2=4 if locc3bfr_dd2==34  | locc3bfr_dd2==26
replace locc1aft_aj1=4 if locc3aft_dd1==34  | locc3aft_dd1==26
replace locc1aft_aj2=4 if locc3aft_dd2==34  | locc3aft_dd2==26

// to professional specialty
replace locc1bfr_aj1=2 if locc3bfr_dd1==15 // | locc3bfr_dd1==36
replace locc1bfr_aj2=2 if locc3bfr_dd2==15 // | locc3bfr_dd2==36
replace locc1aft_aj1=2 if locc3aft_dd1==15 // | locc3aft_dd1==36
replace locc1aft_aj2=2 if locc3aft_dd2==15 // | locc3aft_dd2==36


// to clerical
replace locc1bfr_aj1=5 if locc3bfr_dd1==23 | locc3bfr_dd1==24 | locc3bfr_dd1==25
replace locc1bfr_aj2=5 if locc3bfr_dd2==23 | locc3bfr_dd2==24 | locc3bfr_dd2==25
replace locc1aft_aj1=5 if locc3aft_dd1==23 | locc3aft_dd1==24 | locc3aft_dd1==25	
replace locc1aft_aj2=5 if locc3aft_dd2==23 | locc3aft_dd2==24 | locc3aft_dd2==25	



// HELPERS/LABORERS	
// construction 
replace locc1bfr_aj1=12 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==865 | locc3bfr_b1==869)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==866 | locc3bfr_b1==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b1==626 | locc3bfr_b1==660 | locc3bfr_b1==673 | locc3bfr_b1==674))) 
replace locc1bfr_aj2=12 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==865 | locc3bfr_b2==869)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==866 | locc3bfr_b2==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b2==626 | locc3bfr_b2==660 | locc3bfr_b2==673 | locc3bfr_b2==674))) 
replace locc1aft_aj1=12 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==865 | locc3aft_b1==869)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==866 | locc3aft_b1==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b1==626 | locc3aft_b1==660 | locc3aft_b1==673 | locc3aft_b1==674))) 
replace locc1aft_aj2=12 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==865 | locc3aft_b2==869)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==866 | locc3aft_b2==869)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b2==626 | locc3aft_b2==660 | locc3aft_b2==673 | locc3aft_b2==674))) 
	* extractive 
replace locc1bfr_aj1=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==867 | locc3bfr_b1==867)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==868 | locc3bfr_b1==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b1==693))) 
replace locc1bfr_aj2=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==867 | locc3bfr_b2==867)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==868 | locc3bfr_b2==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b2==693))) 
replace locc1aft_aj1=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==867 | locc3aft_b1==867)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==868 | locc3aft_b1==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b1==693))) 
replace locc1aft_aj2=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==867 | locc3aft_b2==867)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==868 | locc3aft_b2==868)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b2==693))) 
	
// repair
replace locc1bfr_aj1=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==864 | locc3bfr_b1==864)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==865 | locc3bfr_b1==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b1==761))) 
replace locc1bfr_aj2=11 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==864 | locc3bfr_b2==864)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==865 | locc3bfr_b2==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3bfr_b2==761))) 
replace locc1aft_aj1=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==864 | locc3aft_b1==864)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==865 | locc3aft_b1==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b1==761))) 
replace locc1aft_aj2=11 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==864 | locc3aft_b2==864)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==865 | locc3aft_b2==865)) | ///
							(panel>=2001 & panel<=2008 & (locc3aft_b2==761))) 
			
// surveyor to tech support (soc80, soc90)
replace locc1bfr_aj1=3 if ((panel>=1984 & panel<=1991 & (locc3bfr_b1==866)) | (panel>=1992 & panel<=2001 & (locc3bfr_b1==867)))
replace locc1bfr_aj2=3 if ((panel>=1984 & panel<=1991 & (locc3bfr_b2==866)) | (panel>=1992 & panel<=2001 & (locc3bfr_b2==867)))
replace locc1aft_aj1=3 if ((panel>=1984 & panel<=1991 & (locc3aft_b1==866)) | (panel>=1992 & panel<=2001 & (locc3aft_b1==867)))
replace locc1aft_aj2=3 if ((panel>=1984 & panel<=1991 & (locc3aft_b2==866)) | (panel>=1992 & panel<=2001 & (locc3aft_b2==867)))
							
*/					

/*
capture drop locc1bfr_nomgta1
capture drop locc1aft_nomgta1
capture drop locc1bfr_nohma1
capture drop locc1aft_nohma1
capture drop locc1bfr_nomgtb1
capture drop locc1aft_nomgtb1
capture drop locc1bfr_nohmb1
capture drop locc1aft_nohmb1

capture drop locc1bfr_nomgta2
capture drop locc1aft_nomgta2
capture drop locc1bfr_nohma2
capture drop locc1aft_nohma2
capture drop locc1bfr_nomgtb2
capture drop locc1aft_nomgtb2
capture drop locc1bfr_nohmb2
capture drop locc1aft_nohmb2


gen occ1bfr_nomgta=occ1bfr_ddn
gen occ1aft_nomgta=occ1aft_ddn
gen occ1bfr_nohma=occ1bfr_nohlp
gen occ1aft_nohma=occ1aft_nohlp
gen occ1bfr_nomgtb=occ1bfr_ddn
gen occ1aft_nomgtb=occ1aft_ddn
gen occ1bfr_nohmb=occ1bfr_nohlp
gen occ1aft_nohmb=occ1aft_nohlp
*/

capture program drop nohm_exe
program define nohm_exe  // [1 in: david dorned 3 digit occupation] [2 in: baseline 1digit occupation] [3 out: nohma/b name of variable out]

local var_in="`1'"
local var_in1d="`2'"
local nohmavar_out="`3'a"
local nohmbvar_out="`3'b"

capture drop `nohmavar_out'
capture drop `nohmbvar_out'

gen `nohmavar_out'=`var_in1d'
gen `nohmbvar_out'=`var_in1d'


// ceos, legislators, to administration (Version a)
replace `nohmavar_out'=5 if `var_in'==4
// ceos, legislators, to prof specialty (Version b)
replace `nohmbvar_out'=2 if `var_in'==4

// fin managers (Version a)
replace `nohmavar_out'=5 if `var_in'==7
// fin managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==7

// personnel managers (Version a)
replace `nohmavar_out'=5 if `var_in'==8
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==8


// 013 Managers, marketing, advertising, and public relations (125)
replace `nohmavar_out'=4 if `var_in'==13
// personnel managers (Version b)
replace `nohmbvar_out'=4 if `var_in'==13


// 014 Administrators, education and related fields (128)
replace `nohmavar_out'=2 if `var_in'==14
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==14


// 015 Managers, medicine and health (131)
replace `nohmavar_out'=2 if `var_in'==15
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==15


// 018 Managers, properties and real estate (1353)
replace `nohmavar_out'=5 if `var_in'==18
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==18


// Funeral directors: tech support by miscoding
replace `nohmavar_out'=3 if `var_in'==19
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==19


//022 Managers and administrators, n.e.c. (121, 126, 132-1343, 136-139)
replace `nohmavar_out'=5 if `var_in'==22
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==22

//023 Accountants and auditors (1412)
replace `nohmavar_out'=5 if `var_in'==23
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==23

// 024 Underwriters (1414)
replace `nohmavar_out'=5 if `var_in'==24
// personnel managers (Version b)
replace `nohmbvar_out'=5	 if `var_in'==24

//025 Other financial officers (1415, 1419)
replace `nohmavar_out'=5 if `var_in'==25
// 
replace `nohmbvar_out'=5 if `var_in'==25

//026 Management analysts (142)
replace `nohmavar_out'=2 if `var_in'==26
// 
replace `nohmbvar_out'=2 if `var_in'==26


//027 Personnel, training, and labor relations specialists (143)
replace `nohmavar_out'=5 if `var_in'==27
// 
replace `nohmbvar_out'=2 if `var_in'==27

//028 Purchasing agents and buyers, farm products (1443)
//029 Buyers, wholesale and retail trade except farm products (1442)
// 033 Purchasing agents and buyers, n.e.c. (1449)
			* difficult one, somewhere between sales and admin support
			
replace `nohmavar_out'=5 if `var_in'>=28 & `var_in'<=33 
//
replace `nohmbvar_out'=5 if `var_in'>=28 & `var_in'<=33


// 034 Business and promotion agents (145)
replace `nohmavar_out'=4 if `var_in'==34
// personnel managers (Version b)
replace `nohmbvar_out'=4 if `var_in'==34


// 035 Construction inspectors (1472)
//036 Inspectors and compliance officers, except construction (1473)

replace `nohmavar_out'=2 if `var_in'>=35 & `var_in'<=36 
//
replace `nohmbvar_out'=2 if `var_in'>=35 & `var_in'<=36

//037 Management related occupations, n.e.c. (149)
//022 Managers and administrators, n.e.c. (121, 126, 132-1343, 136-139)
replace `nohmavar_out'=5 if `var_in'==37
// personnel managers (Version b)
replace `nohmbvar_out'=2 if `var_in'==37

end

nohm_exe locc3bfr_dd1 locc1bfr_nolab1 locc1bfr_nohm1
nohm_exe locc3aft_dd1 locc1aft_nolab1 locc1aft_nohm1
nohm_exe locc3aft_dd2 locc1aft_nolab2 locc1aft_nohm2
nohm_exe locc3bfr_dd2 locc1bfr_nolab2 locc1bfr_nohm2




capture drop locc1bfr_nohma1
capture drop locc1bfr_nohma2
capture drop locc1bfr_nohmb1
capture drop locc1bfr_nohmb2

capture drop locc1aft_nohma1
capture drop locc1aft_nohma2
capture drop locc1aft_nohmb1
capture drop locc1aft_nohmb2


ren locc1bfr_nohm1a locc1bfr_nohma1
ren locc1bfr_nohm2a locc1bfr_nohma2
ren locc1bfr_nohm1b locc1bfr_nohmb1
ren locc1bfr_nohm2b locc1bfr_nohmb2

ren locc1aft_nohm1a locc1aft_nohma1
ren locc1aft_nohm2a locc1aft_nohma2
ren locc1aft_nohm1b locc1aft_nohmb1
ren locc1aft_nohm2b locc1aft_nohmb2


mobselection1_exe locc1bfr_nohma locc1aft_nohma
mobselection1_exe locc1bfr_nohmb locc1aft_nohmb

cap program drop matrixgen_exe
program define matrixgen_exe

local var_bfr="`1'"
local var_aft="`2'"
local mat_out="`3'"
											
tab `var_bfr' `var_aft' [iw=pweight2] if `var_bfr'!=. & `var_aft'!=.    ///
  & (lue_c_1tm==1 | lue_c_1tm==0) &(lue_c_1tm!=.) & wave>3 & complete_uspell==1 & sample_timetogo>3 ///
  & entry_ind==1 & tage>=20 & interview_no2>=14 & compl_n_spellength<=12 & timeseries_excl!=1, matcell(`mat_out')

end 

matrixgen_exe locc1bfr_nohma locc1aft_nohma mat_nohma
xsnet_calcx mat_nohma

matrixgen_exe locc1bfr_dd locc1aft_dd mat_dd
xsnet_calcx mat_dd

matrixgen_exe locc1bfr_nohlp locc1aft_nohlp mat_nohlp
xsnet_calcx mat_nohlp


matrixgen_exe locc1bfr_nolab locc1aft_nolab mat_nolab
xsnet_calcx mat_nolab

tab locc1bfr_nohma

capture drop occ1bfr_11occa
capture drop occ1aft_11occa
capture drop occ1bfr_11occb
capture drop occ1aft_11occb

gen occ1bfr_11occa=occ1bfr_nohma
gen occ1aft_11occa=occ1aft_nohma
gen occ1bfr_11occb=occ1bfr_nohmb
gen occ1aft_11occb=occ1aft_nohmb

replace occ1bfr_11occa=8 if occ1bfr_11occa==6 | occ1bfr_11occa==7
replace occ1aft_11occa=8 if occ1aft_11occa==6 | occ1aft_11occa==7
replace occ1bfr_11occb=8 if occ1bfr_11occb==6 | occ1bfr_11occb==7
replace occ1aft_11occb=8 if occ1aft_11occb==6 | occ1aft_11occb==7























		
							
* 22 own definition, based on the dd categories (has become 21, given the merger of constr/extr)


cap n drop locc1bfr_ttdd1
cap n drop locc1bfr_ttdd2
cap n drop locc1aft_ttdd1
cap n drop locc1aft_ttdd2

gen locc1bfr_ttdd1=.
gen locc1bfr_ttdd2=.
gen locc1aft_ttdd1=.
gen locc1aft_ttdd2=.


***********************

cap program drop dd2ttdd_exe
program define dd2ttdd_exe

global ddvar="`1'"
global namevar="`2'"

// protective services 


replace ${namevar}=84 if (${ddvar}>=413 & ${ddvar}<=427)
* changed from original version >=415

		/*
		gen occ3_guard=0          /* supervisors of guards; guards */
		replace occ3_guard=1 if (${ddvar}==415 | (${ddvar}>=425 & ${ddvar}<=427))
		*/
		
// food prep		
		
replace ${namevar}=82 if (${ddvar}>=433 & ${ddvar}<=444) 

// health care services, not prof spec or tech support

	/* health service occs (dental ass., health/nursing aides) */
replace ${namevar}=81 if (${ddvar}>=445 & ${ddvar}<=447)

// housekeeping, cleaning, and janitorial jobs

	/* building and grounds cleaning and maintenance occs */
replace ${namevar}=83 if (${ddvar}>=448 & ${ddvar}<=455)

	/* housekeeping, cleaning, laundry */
replace ${namevar}=83 if     (${ddvar}>=405 & ${ddvar}<=408)


// PERSONAL CARE and hospitality, incl priv household (except cooks)

	/* child care workers */
replace ${namevar}=80  if (${ddvar}==468)

	/* misc. personal care and service occs */
replace ${namevar}=80 if (${ddvar}>=469 & ${ddvar}<=472)

	/* personal appearance occs */
replace ${namevar}=80 if (${ddvar}>=457 & ${ddvar}<=458)


	/* recreation and hospitality occs */
replace ${namevar}=84 if (${ddvar}>=459 & ${ddvar}<=467)



********************************************************************
* Occupation Dummies Level 2: 16 Non-Service Occupation Groups
********************************************************************

        /* executive, administrative and managerial occs */
replace ${namevar}=11 if (${ddvar}>=3 & ${ddvar}<=22)
		/* management related occs */
replace ${namevar}=12 if (${ddvar}>=23 & ${ddvar}<=37)



    /* professional specialty occs */
replace ${namevar}=20 if ( ${ddvar}>=43 & ${ddvar}<=200  )


/* technicians and related support occs */
replace ${namevar}=30  if (${ddvar}>=203 & ${ddvar}<=235)


    /* financial sales and related occs */
replace ${namevar}=41  if (${ddvar}>=243 & ${ddvar}<=258)

    /* retail sales occs ???? CHECK*/
replace ${namevar}=42  if (${ddvar}>=259 & ${ddvar}<=283)


    /* administrative support occs */
replace ${namevar}=50  if (${ddvar}>=303 & ${ddvar}<=389)






    /* farm operators and managers */
replace ${namevar}=90  if (${ddvar}>=473 & ${ddvar}<=475)

    /* other agricultural and related occs */
replace ${namevar}=100  if (${ddvar}>=479 & ${ddvar}<=498)



    /* mechanics and repairers */
replace ${namevar}=110  if (${ddvar}>=503 & ${ddvar}<=549)



   /* construction trades */
replace ${namevar}=120  if (${ddvar}>=558 & ${ddvar}<=599)

   /* extractive occs */
replace ${namevar}=120  if (${ddvar}>=614 & ${ddvar}<=617)



   /* precision production occs */
replace ${namevar}=130  if (${ddvar}>=628 & ${ddvar}<=699)


    /* machine operators, assemblers, and inspectors */
replace ${namevar}=140  if (${ddvar}>=703 & ${ddvar}<=799)


   /* transportation and material moving occs */
replace ${namevar}=150  if (${ddvar}>=803 & ${ddvar}<=861)

/* handlers and laborers*/
replace ${namevar}=160  if (${ddvar}>=862 & ${ddvar}<=889)

label define label_ttdd 11 "executive, administrative and managerial occs " 12 "mgt rel occs" 20 "professional speciality" 30 "technicians and rel support" ///
		41 "fin sales" 42 "retail sales occ." 50 "admin support" 84 "protective services" 82 "food prep" 81 " health service rel" 83 "house/groundkeeping" 80 "personal care and hospitality" 90 "agric management" 100 "other agric & rel occ"  110 " mechanics and repair" ///
		120 "constr/extr" 121 "construction" 122 "extractive" 130 "precision production" 140 "machine operators/assemblers" ///
		150 "transportation and materials moving" 160 "laborers", replace

		/*
		1 11 "executive, administrative and managerial occs " 
		2 12 "mgt rel occs" 
		3 20 "professional speciality" 
		4 30 "technicians and rel support" ///
		5 41 "fin sales" 
		6 42 "retail sales occ." 
		7 50 "admin support" 
		8 70 "protective services" 
		9 81 "food prep" 
		10 82 " health service rel" 
		11 83 "house/groundkeeping" 
		12 84 "personal care and hospitality" 
		13 90 "agric management" 
		14 100 "other agric & rel occ"  
		15 110 " mechanics and repair" ///
		16 120 "construction/extr" 122 "extractive" 
		17 130 "precision production" 
		18 140 "machine operators/assemblers" ///
		19 150 "transportation and materials moving" 
		20 160 "laborers", replace
		*/
		
lab val ${namevar} label_ttdd		

end

dd2ttdd_exe locc3bfr_dd1 locc1bfr_ttdd1
dd2ttdd_exe locc3bfr_dd2 locc1bfr_ttdd2
dd2ttdd_exe locc3aft_dd1 locc1aft_ttdd1
dd2ttdd_exe locc3aft_dd2 locc1aft_ttdd2

mobselection1_exe locc1bfr_ttdd locc1aft_ttdd
*/

* 22 own definitions, based on the Census 2000 (MM in latin) categorization

cap n drop locc1bfr_mm1
cap n drop locc1bfr_mm2
cap n drop locc1aft_mm1
cap n drop locc1aft_mm2

gen locc1bfr_mm1=.
gen locc1bfr_mm2=.
gen locc1aft_mm1=.
gen locc1aft_mm2=.

cap n program drop merge2000soc_exe
program define merge2000soc_exe

local ddvar_in="`1'"
local aggvar_out="`2'"

capture drop occ1990dd
gen occ1990dd=`ddvar_in'

capture drop _merge
merge m:1 occ1990dd using "${step0codedir}/occ_ind_recodes/occ1990dd2000soc.dta"

replace `aggvar_out'=occ2000rec
drop occ1990dd
drop occ2000rec
cap n drop _merge2000socrec
ren _merge _merge2000socrec

/*
using IPUMS recoding, not all occupations are recoded (some occupations are absent in IPUMS?
177		120		3.57	3.57
	235		139	4.14	7.71
	387		759	22.59	30.30
	408		189	5.63	35.92
	433		358	10.65	46.58
	450		78	2.32	48.90
	451		926	27.56	76.46
	466		147	4.38	80.83
	467		8	0.24	81.07
	470		36	1.07	82.14
	471		39	1.16	83.30
	472		106	3.15	86.46
	653		140	4.17	90.63
	684		84	2.50	93.13
	702		95	2.83	95.95
	789		58	1.73	97.68
	834		4	0.12	97.80
	873		74	2.20	100.00



*/
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

 lab val `aggvar_out' label_mm		

 
end 

merge2000soc_exe locc3bfr_dd1 locc1bfr_mm1
tab locc3bfr_dd1 if locc3bfr_dd1!=. & locc1bfr_mm1==.
merge2000soc_exe locc3bfr_dd2 locc1bfr_mm2
tab locc3bfr_dd2 if locc3bfr_dd2!=. & locc1bfr_mm2==.
merge2000soc_exe locc3aft_dd1 locc1aft_mm1
tab locc3aft_dd1 if locc3aft_dd1!=. & locc1aft_mm1==.
merge2000soc_exe locc3aft_dd2 locc1aft_mm2
tab locc3aft_dd2 if locc3aft_dd2!=. & locc1aft_mm2==.

mobselection1_exe locc1bfr_mm locc1aft_mm

matrixgen_exe locc1bfr_mm locc1aft_mm mmmat
*xsnet_calcx mmmat


*****************************************
** using the original SOC 2000 codes, in combination with 1990 dd codes for panels<=2001

cap n drop locc1bfr_mmo1
cap n drop locc1bfr_mmo2
cap n drop locc1aft_mmo1
cap n drop locc1aft_mmo2

gen locc1bfr_mmo1=locc1bfr_mm1
gen locc1bfr_mmo2=locc1bfr_mm2
gen locc1aft_mmo1=locc1aft_mm1
gen locc1aft_mmo2=locc1aft_mm2


cap n program drop mm_mmo_adj_exe
program define mm_mmo_adj_exe

local var_in="`1'"			// original 2000 soc coded variable (3digit)
local aggvar_out="`2'"

local added_condition=" panel>=2004 & panel<=2008 " 
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


mm_mmo_adj_exe locc3bfr_b1 locc1bfr_mmo1
mm_mmo_adj_exe locc3bfr_b2 locc1bfr_mmo2
mm_mmo_adj_exe locc3aft_b1 locc1aft_mmo1
mm_mmo_adj_exe locc3aft_b2 locc1aft_mmo2




**********************
** 3 categories
**********************

capture drop locc1bfr_3cat
capture drop locc1aft_3cat

gen locc1bfr_3cat=1 if locc1bfr_dd>=1 & locc1bfr_dd<=5
gen locc1aft_3cat=1 if locc1aft_dd>=1 & locc1aft_dd<=5
replace locc1bfr_3cat=2 if ((locc1bfr_dd>=6 & locc1bfr_dd<=8) | locc1bfr_dd==15)
replace locc1aft_3cat=2 if ((locc1aft_dd>=6 & locc1aft_dd<=8) | locc1aft_dd==15)
replace locc1bfr_3cat=3 if ((locc1bfr_dd>=9 & locc1bfr_dd<=16) & locc1bfr_dd!=15)
replace locc1aft_3cat=3 if ((locc1aft_dd>=9 & locc1aft_dd<=16) & locc1aft_dd!=15)

//====================================================
// CREATE LOCCBEFORE LOCCAFTER LINDBEFORE LINDAFTER
//=====================================================

	/* THIS PROGRAM SELECTS AMONG THE UP TO TWO IND/OCCs before, AND THE UP TO TWO OCCS/INDS AFTER, 
	which one was before, and which one after
	it takes in the before variables (which should end in 1,2 to correspond to eeno1 and eeno2, but the 1/2 should not be given as an argument)
	and the after variables
	and it returns the unique locc/lind variables
	*/

sort personkey yearmonth
capture n program drop mobselection1_exe
program define mobselection1_exe   // [before variables1/2] [after variables1/2]

/*
						local var_bfr= "`1'"
						local var_aft ="`2'"
	set varabbrev off
	capture drop `var_bfr'
	capture drop `var_aft'
	
	gen `var_bfr'=.
	gen `var_aft'=.
c
*/

						local var_bfr= "`1'"
						local var_aft ="`2'"
	set varabbrev off
	capture drop `var_bfr'
	capture drop `var_aft'
	
	gen `var_bfr'=.
	gen `var_aft'=.

	replace `var_bfr'=`var_bfr'1 if  `var_bfr'2==. & `var_bfr'1!=.
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1==. & `var_bfr'2!=.
	** 2 firms before 
	** one overlap, take the overlapping occupation
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & (`var_bfr'2!=`var_aft'1 & `var_bfr'2!=`var_aft'2)
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & (`var_bfr'1!=`var_aft'1 & `var_bfr'1!=`var_aft'2)
	** double overlap, which one counts? GO WITH THE OVERLAP with the dominant occupation before the unemployment spell
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & firm_ind==1
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_bfr'2==`var_aft'1 | `var_bfr'2==`var_aft'2) & (`var_bfr'1==`var_aft'1 | `var_bfr'1==`var_aft'2) & firm_ind==2
	** 2 firms before, but no overlap
	replace `var_bfr'=`var_bfr'1 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)  & firm_ind==1
	replace `var_bfr'=`var_bfr'2 if  `var_bfr'1!=. & `var_bfr'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)  & firm_ind==2
	
	replace `var_aft'=`var_aft'1 if  `var_aft'2==. & `var_aft'1!=.
	replace `var_aft'=`var_aft'2 if  `var_aft'1==. & `var_aft'2!=.
	** 2 firms after
	** one overlap, take the overlapping occupation
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & (`var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2)
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2)
	** double overlap, which one counts? GO WITH THE OVERLAP with the dominant occupation before the unemployment spell
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & firm_ind==1
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'2==`var_bfr'1 | `var_aft'2==`var_bfr'2) & (`var_aft'1==`var_bfr'1 | `var_aft'1==`var_bfr'2) & firm_ind==2
	** 2 firms after, but no overlap with previous firms
	replace `var_aft'=`var_aft'1 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2) & lfirm_ind==1
	replace `var_aft'=`var_aft'2 if  `var_aft'1!=. & `var_aft'2!=. & (`var_aft'1!=`var_bfr'1 & `var_aft'1!=`var_bfr'2 & `var_aft'2!=`var_bfr'1 & `var_aft'2!=`var_bfr'2) & lfirm_ind==2

	

// assign
mobselection1_exe lind1bfr_b lind1aft_b
mobselection1_exe locc1bfr_dd locc1aft_dd

*mobselection1_exe locc1bfr_ddmog locc1aft_ddmog
*mobselection1_exe locc1bfr_xi locc1aft_xi
*mobselection1_exe locc1bfr_aj locc1aft_aj

*mobselection1_exe locc1bfr_ad locc1aft_ad
*mobselection1_exe locc1bfr_sog locc1aft_sog
mobselection1_exe locc1bfr_hs locc1aft_hs
mobselection1_exe locc1bfr_ths locc1aft_ths

*mobselection1_exe locc1bfr_nohlp locc1aft_nohlp
mobselection1_exe locc1bfr_mmo locc1aft_mmo



