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





//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// STEP 0.1.1.AUX TURNS CORE WAVES 1984-88 SIPP INTO FORMAT CONGRUENT WITH 1996
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




* helpful input from Carl Singleton gratefully acknowledged  

capture program drop coremanip
program define coremanip



 //-------------------------------------------------------
 //  PERSON IDENTIFIERS
 //------------------------------------------------------

/*
first 2 figures: starting year of panel
then 9 figure ssuid
then 2 entry
then 3 person number
*/

 //--------------------------------------------------------
 //   CHANGE DATA SET FROM WIDE IN MONTHS TO LONG
 //-------------------------------------------------------


/*
complication here is that we have 18 weeks worth of data that have to split
over months according to wksper*

For sure there is one month with 5 weeks, but there might be two months with
5 weeks. Moreover, we cannot have two adjacent months with 5 weeks (for any
normal definition), so the following options exist
5444, 4544, 4454, 4445, 5454, 5445, 4545

*/

 // generate the weekly per month variables



local temp look_ abs_ job_



foreach v of local temp              {

capture drop `v'wkonex_1=.
capture drop `v'wkonex_2=.
capture drop `v'wkonex_3=.
capture drop `v'wkonex_4=.

capture drop `v'wktwox_1=.
capture drop `v'wktwox_2=.
capture drop `v'wktwox_3=.
capture drop `v'wktwox_4=.

capture drop `v'wkthreex_1=.
capture drop `v'wkthreex_2=.
capture drop `v'wkthreex_3=.
capture drop `v'wkthreex_4=.

capture drop `v'wkfourx_1=.
capture drop `v'wkfourx_2=.
capture drop `v'wkfourx_3=.
capture drop `v'wkfourx_4=.

capture drop `v'wkfivex_1=.
capture drop `v'wkfivex_2=.
capture drop `v'wkfivex_3=.
capture drop `v'wkfivex_4=.




capture gen `v'wkonex_1=.
capture gen `v'wkonex_2=.
capture gen `v'wkonex_3=.
capture gen `v'wkonex_4=.

capture gen `v'wktwox_1=.
capture gen `v'wktwox_2=.
capture gen `v'wktwox_3=.
capture gen `v'wktwox_4=.

capture gen `v'wkthreex_1=.
capture gen `v'wkthreex_2=.
capture gen `v'wkthreex_3=.
capture gen `v'wkthreex_4=.

capture gen `v'wkfourx_1=.
capture gen `v'wkfourx_2=.
capture gen `v'wkfourx_3=.
capture gen `v'wkfourx_4=.

capture gen `v'wkfivex_1=.
capture gen `v'wkfivex_2=.
capture gen `v'wkfivex_3=.
capture gen `v'wkfivex_4=.

                                        }

tokenize weeksa weeksl wkwjob abs_ look_ job_

 // -----------------------------------------------5444-------------------------------------------

if (wksper1==5 & wksper2==4 & wksper3==4 & wksper4==4) {

forvalues j=1(1)3 {

local jj=`j'+3

replace ``jj''wkonex_1=  ``j''01
replace ``jj''wktwox_1=  ``j''02
replace ``jj''wkthreex_1=``j''03
replace ``jj''wkfourx_1=``j''04
replace ``jj''wkfivex_1=``j''05

replace ``jj''wkonex_2=``j''06
replace ``jj''wktwox_2=``j''07
replace ``jj''wkthreex_2=``j''08
replace ``jj''wkfourx_2=``j''09

replace ``jj''wkonex_3=``j''10
replace ``jj''wktwox_3=``j''11
replace ``jj''wkthreex_3=``j''12
replace ``jj''wkfourx_3=``j''13

replace ``jj''wkonex_4=``j''14
replace ``jj''wktwox_4=``j''15
replace ``jj''wkthreex_4=``j''16
replace ``jj''wkfourx_4=``j''17


}
}


 // -----------------------------------------------4544-------------------------------------------


if (wksper1==4 & wksper2==5 & wksper3==4 & wksper4==4) {

forvalues j=1(1)3 {

local jj=`j'+3

replace ``jj''wkonex_1=  ``j''01
replace ``jj''wktwox_1=  ``j''02
replace ``jj''wkthreex_1=``j''03
replace ``jj''wkfourx_1=``j''04

replace ``jj''wkonex_2=``j''05
replace ``jj''wktwox_2=``j''06
replace ``jj''wkthreex_2=``j''07
replace ``jj''wkfourx_2=``j''08
replace ``jj''wkfivex_2=``j''09

replace ``jj''wkonex_3=``j''10
replace ``jj''wktwox_3=``j''11
replace ``jj''wkthreex_3=``j''12
replace ``jj''wkfourx_3=``j''13

replace ``jj''wkonex_4=``j''14
replace ``jj''wktwox_4=``j''15
replace ``jj''wkthreex_4=``j''16
replace ``jj''wkfourx_4=``j''17


}
}


 // -----------------------------------------------4454-------------------------------------------


if (wksper1==4 & wksper2==4 & wksper3==5 & wksper4==4) {

forvalues j=1(1)3 {

local jj=`j'+3

replace ``jj''wkonex_1=  ``j''01
replace ``jj''wktwox_1=  ``j''02
replace ``jj''wkthreex_1=``j''03
replace ``jj''wkfourx_1=``j''04

replace ``jj''wkonex_2=``j''05
replace ``jj''wktwox_2=``j''06
replace ``jj''wkthreex_2=``j''07
replace ``jj''wkfourx_2=``j''08


replace ``jj''wkonex_3=``j''09
replace ``jj''wktwox_3=``j''10
replace ``jj''wkthreex_3=``j''11
replace ``jj''wkfourx_3=``j''12
replace ``jj''wkfivex_3=``j''13

replace ``jj''wkonex_4=``j''14
replace ``jj''wktwox_4=``j''15
replace ``jj''wkthreex_4=``j''16
replace ``jj''wkfourx_4=``j''17

}
}


 // -----------------------------------------------4445-------------------------------------------


if (wksper1==4 & wksper2==4 & wksper3==4 & wksper4==5) {

forvalues j=1(1)3 {

local jj=`j'+3
replace ``jj''wkonex_1=  ``j''01
replace ``jj''wktwox_1=  ``j''02
replace ``jj''wkthreex_1=``j''03
replace ``jj''wkfourx_1=``j''04

replace ``jj''wkonex_2=``j''05
replace ``jj''wktwox_2=``j''06
replace ``jj''wkthreex_2=``j''07
replace ``jj''wkfourx_2=``j''08


replace ``jj''wkonex_3=``j''09
replace ``jj''wktwox_3=``j''10
replace ``jj''wkthreex_3=``j''11
replace ``jj''wkfourx_3=``j''12


replace ``jj''wkonex_4=``j''13
replace ``jj''wktwox_4=``j''14
replace ``jj''wkthreex_4=``j''15
replace ``jj''wkfourx_4=``j''16
replace ``jj''wkfivex_4=``j''17
}
}


 // -----------------------------------------------5454-------------------------------------------

if (wksper1==5 & wksper2==4 & wksper3==5 & wksper4==4) {

forvalues j=1(1)3 {

local jj=`j'+3
replace ``jj''wkonex_1=  ``j''01
replace ``jj''wktwox_1=  ``j''02
replace ``jj''wkthreex_1=``j''03
replace ``jj''wkfourx_1=``j''04
replace ``jj''wkfivex_1=``j''05

replace ``jj''wkonex_2=``j''06
replace ``jj''wktwox_2=``j''07
replace ``jj''wkthreex_2=``j''08
replace ``jj''wkfourx_2=``j''09

replace ``jj''wkonex_3=``j''10
replace ``jj''wktwox_3=``j''11
replace ``jj''wkthreex_3=``j''12
replace ``jj''wkfourx_3=``j''13
replace ``jj''wkfivex_3=``j''14

replace ``jj''wkonex_4=``j''15
replace ``jj''wktwox_4=``j''16
replace ``jj''wkthreex_4=``j''17
replace ``jj''wkfourx_4=``j''18

}
}


 // -----------------------------------------------5454-------------------------------------------

if (wksper1==5 & wksper2==4 & wksper3==4 & wksper4==5) {

forvalues j=1(1)3 {

local jj=`j'+3
replace ``jj''wkonex_1=  ``j''01
replace ``jj''wktwox_1=  ``j''02
replace ``jj''wkthreex_1=``j''03
replace ``jj''wkfourx_1=``j''04
replace ``jj''wkfivex_1=``j''05

replace ``jj''wkonex_2=``j''06
replace ``jj''wktwox_2=``j''07
replace ``jj''wkthreex_2=``j''08
replace ``jj''wkfourx_2=``j''09

replace ``jj''wkonex_3=``j''10
replace ``jj''wktwox_3=``j''11
replace ``jj''wkthreex_3=``j''12
replace ``jj''wkfourx_3=``j''13


replace ``jj''wkonex_4=``j''14
replace ``jj''wktwox_4=``j''15
replace ``jj''wkthreex_4=``j''16
replace ``jj''wkfourx_4=``j''17
replace ``jj''wkfivex_4=``j''18
}
}


 // -----------------------------------------------4545-------------------------------------------

if (wksper1==4 & wksper2==5 & wksper3==4 & wksper4==5) {

forvalues j=1(1)3 {

local jj=`j'+3
replace ``jj''wkonex_1=  ``j''01
replace ``jj''wktwox_1=  ``j''02
replace ``jj''wkthreex_1=``j''03
replace ``jj''wkfourx_1=``j''04

replace ``jj''wkonex_2=``j''05
replace ``jj''wktwox_2=``j''06
replace ``jj''wkthreex_2=``j''07
replace ``jj''wkfourx_2=``j''08
replace ``jj''wkfivex_2=``j''09

replace ``jj''wkonex_3=``j''10
replace ``jj''wktwox_3=``j''11
replace ``jj''wkthreex_3=``j''12
replace ``jj''wkfourx_3=``j''13


replace ``jj''wkonex_4=``j''14
replace ``jj''wktwox_4=``j''15
replace ``jj''wkthreex_4=``j''16
replace ``jj''wkfourx_4=``j''17
replace ``jj''wkfivex_4=``j''18
}

}

 // RENAME VARIABLES, INCLUDING THE 4MONTH VARIABLES


/*

rename     su_rot  rot       //1f
 //rename     state tfipsst         //2f "FIPS State code for the sample unit"

rename     h1_month    rhcalmnx_1   //2f "Month for which this household is"
rename     h1_year     rhcalyrx_1   //2f "Year for which this household is"
rename     h1_wgt   whfnwgtx_1      //12.4f "Household weight--month 1."
rename     h1_hsc     core_ghlfsamx_1   //1f "Half sample code for variance estimati"
rename     h1_strat   core_gvarstrx_1   //2f "Stratum code for variance estimation"
rename     h1_metro   tmetrox_1          // living metropolitan area? y/n
rename     h1_msa     tmsax_1            // which msa

rename     h2_month  rhcalmnx_2    //2f "Month for which this household is"
rename     h2_year     rhcalyrx_2      //2f "Year for which this household is"
rename     h2_wgt   whfnwgtx_2         //12.4f "Household weight--month 2."
rename     h2_hsc     core_ghlfsamx_2       //1f "Half sample code for variance estimati"
rename     h2_strat   core_gvarstrx_2           //2f "Stratum code for variance estimation"
rename     h2_metro   tmetrox_2          // living metropolitan area? y/n
rename     h2_msa     tmsax_2            // which msa


rename     h3_month   rhcalmnx_3  //2f "Month for which this household is"
rename     h3_year     rhcalyrx_3  //2f "Year for which this household is"
rename   h3_wgt   whfnwgtx_3   //12.4f "Household weight--month 3."
rename     h3_hsc     core_ghlfsamx_3     //1f "Half sample code for variance estimati"
rename     h3_strat   core_gvarstrx_3   //2f "Stratum code for variance estimation"
rename     h3_metro   tmetrox_3          // living metropolitan area? y/n
rename     h3_msa     tmsax_3            // which msa


rename     h4_month   rhcalmnx_4    //2f "Month for which this household is"
rename     h4_year     rhcalyrx_4   //2f "Year for which this household is"
rename   h4_wgt   whfnwgtx_4       //12.4f "Household weight--month 4."
rename     h4_hsc    core_ghlfsamx_4   //1f "Half sample code for variance estimati"
rename     h4_strat  core_gvarstrx_4   //2f "Stratum code for variance estimation"
rename     h4_metro   tmetrox_4          // living metropolitan area? y/n
rename     h4_msa     tmsax_4            // which msa


replace rhcalyrx_1=rhcalyrx_1+1900
replace rhcalyrx_2=rhcalyrx_2+1900
replace rhcalyrx_3=rhcalyrx_3+1900
replace rhcalyrx_4=rhcalyrx_4+1900

*/

rename ws1_wks1 ws1wksx_1
rename ws1_wks2 ws1wksx_2
rename ws1_wks3 ws1wksx_3
rename ws1_wks4 ws1wksx_4

rename ws2_wks1 ws2wksx_1
rename ws2_wks2 ws2wksx_2
rename ws2_wks3 ws2wksx_3
rename ws2_wks4 ws2wksx_4


   * // number of weeks in the month

rename     wksper1   rwkspermx_1   //1f "Number of weeks in month 1 of the"
rename     wksper2   rwkspermx_2   //1f "Number of weeks in month 2 of the"
rename     wksper3   rwkspermx_3   //1f "Number of weeks in month 3 of the"
rename     wksper4   rwkspermx_4   //1f "Number of weeks in month 4 of the"

/*

rename pp_wave wave

rename fnlwgt_1 wpfinwgtx_1
rename fnlwgt_2 wpfinwgtx_2
rename fnlwgt_3 wpfinwgtx_3
rename fnlwgt_4 wpfinwgtx_4

rename     sc1002   elkwrk    // 1f "Did ... spend any time looking for"
replace elkwrk=-1 if elkwrk==0

rename     sc1042    rtakjob    // 1f "Could ... have taken a job during"
rename     sc1044    rnotake    // 1f "What was the main reason ... could"

rename     sc1098    eabre   // 1f "main reason absent without pay"

/* 

TROUBLE: there are more categories in 1996

 eabre:
V         -1 .Not in universe
V          1 .On layoff (temporary or
V            .indefinite)
V          2 .Slack work or business
V            .conditions
V          3 .Own injury
V          4 .Own illness/injury/medical
V            .problems
V          5 .Pregnancy/childbirth
V          6 .Taking care of children
V          7 .On vacation/personal days
V          8 .Bad weather
V          9 .Labor dispute
V         10 .New job to begin within 30 days
V         11 .Participated in a job-sharing
V            .arrangement
V         12 .Other

sc1098 1986, 1987, 1988
V          0 .Not in universe
V          1 .On layoff - skip to SC1230
V          2 .Own illness - skip to SC1230
V          3 .On vacation - skip to SC1230
V          4 .Bad weather - skip to SC1230
V          5 .Labor dispute - skip to SC1230
V          6 .New job to begin within 30
             .days - skip to SC1230
V          7 .Other - skip to SC1230
*/

replace eabre=12 if eabre==7
replace eabre=. if eabre==7

replace eabre=9 if eabre==5
replace eabre=. if eabre==5

replace eabre=8 if eabre==4
replace eabre=. if eabre==4

replace eabre=7 if eabre==3
replace eabre=. if eabre==3

replace eabre=4 if eabre==2
replace eabre=. if eabre==2

replace eabre=-1 if eabre==0
replace eabre=. if eabre==0




 // rename     sc1176       //1f " looking for
 //     work or on layoff, compare to sc1002"


 //rename     sc1230 rmhrswk       //2f "In the weeks that ... worked during"
 //rename     sc1360 eeveret       //1f "Did retire from a job ever"
rename     sc1386 edisabl_limit      //1f "Does ... have a physical, mental, or"
rename     sc1460 disabled_wave       //1f "Is disabled" ->  completely preventing work in 1996

/* self-employment and business ownership. Again, the concepts do not completely overlap
but for our purposes, to rule out those who have income from their own business
we treat them the same
*/

rename     sc1714  ebuscntr     //1f "During the 4-month period was ..."
replace ebuscntr=-1 if ebuscntr==0
replace ebuscntr=-1 if ebuscntr==1
        // NOTE: that answers 2, and 3 refer to whether the worker was self-employed (totally or in part) instead of the number of se businesses
        // however, it works to distinguish...

 // HOW MANY EMPLOYERS
rename     sc1716 ejobcntr       //1f "How many different employers did ..."
     // need to recode na/niu
replace ejobcntr=-1 if ejobcntr==0     // no contingent work indicator in sipp 1986







rename     ws1_2026  epyhrs1    //1f "Was ... paid by the hour on this"
rename    ws1_2028   tpyrate1  //4.2f "What was ...'s regular hourly pay rate"
rename     ws1_2030  rpyper1   //1f "During the 4-month period how often"
rename     ws1_2032 tpmsum1x_1     //5f "What was the total amount of pay"
rename     ws1_2034 tpmsum1x_2    //5f "What was the total amount of pay"
rename     ws1_2036 tpmsum1x_3    //5f "What was the total amount of pay"
rename     ws1_2038 tpmsum1x_4    //5f "What was the total amount of pay"

 // START AND END DATE DATE
 //rename     ws1_2016     //2f "Month in which this person"
 //rename     ws1_2018     //2f "Day of month shown in WS1-2016"
 //rename     ws1_2020     //2f "Month in which this person left"
 //rename     ws1_2022     //2f "Day of month shown in WS1-2020"





 //rename     ws2_2116     //2f "Month in which this person"
 //rename     ws2_2118     //2f "Day of month shown in WS2-2016"
 //rename     ws2_2120     //2f "Month in which this person left"
 //rename     ws2_2122     //2f "Day of month shown in WS2-2020"


rename     ws2_2126    epyhrs2  //1f "Was ... paid by the hour on this"
rename   ws2_2128  tpyrate2   //4.2f "What was ...'s regular hourly pay rate"
rename     ws2_2130   rpyper2  //1f "During the 4-month period how often"
rename     ws2_2132   tpmsum2x_1   //5f "What was the total amount of pay"
rename     ws2_2134   tpmsum2x_2  //5f "What was the total amount of pay"
rename     ws2_2136   tpmsum2x_3  //5f "What was the total amount of pay"
rename     ws2_2138   tpmsum2x_4  //5f "What was the total amount of pay"

rename     ws1_2002    eeno1  //2f "Check item E3 - employer I.D. number"
rename     ws2_2102    eeno2  //2f "Check item E3 - employer I.D. number"


 // stop to work or continue to work -- have to switch the answers, because question
 // changes from did stop, to are you still working?
capture rename ws1_2003   estlemp1  //1f "Did ... stop working for the employer"
capture replace    estlemp1=3 if estlemp1==1
capture replace    estlemp1=1 if estlemp1==2
capture replace    estlemp1=2 if estlemp1==3
capture lab var estlemp1 "Still working for employer1 (at time of interview)"


capture rename     ws2_2103 estlemp2     //1f "Did ... stop working for the employer"
capture replace    estlemp2=3 if estlemp2==1
capture replace    estlemp2=1 if estlemp2==2
capture replace    estlemp2=2 if estlemp2==3
capture lab var estlemp2 "Still working for employer2 (at time of interview)"


rename ws1_2023 stopjob1
rename ws2_2123 stopjob2
capture lab var stopjob1 "Stopped working for emp 1 during wave"
capture lab var stopjob2  "Stopped working for emp 2 during wave"

rename     ws1_2024   ersend1   //1f "What is the main reason ... stopped"


 // NOTE IT IS VERY TRICKY WITH THIS QUESTION TO COMPARE
 // ANSWERS

/* ws_2024 1986 1987 1988
V          0 .Not in universe
V          1 .Laid off
V          2 .Retired
V          3 .Discharged
V          4 .Job was temporary and ended
V          5 .Quit to take another job
V          6 .Quit for some other reason

ersend1
V         -1 .Not in universe
V          1 .On layoff
V          2 .Retirement or old age
V          3 .Childcare problems
V          4 .Other family/personal
V            .obligations
V          5 .Own illness
V          6 .Own injury
V          7 .School/training
V          8 .Discharged/fired
V          9 .Employer bankrupt
V         10 .Employer sold business
V         11 .Job was temporary and ended
V         12 .Quit to take another job
V          13.Slack work or business
V            .conditions
V         14 .Unsatisfactory work arrangements
V            .(hours, pay, etc)
V         15 .Quit for some other reason
*/
replace ersend1=15 if ersend1==6
replace ersend1=12 if ersend1==5
replace ersend1=11 if ersend1==4
replace ersend1=8 if ersend1==3
replace ersend1=-1 if ersend1==0


rename     ws1_2025  ejbhrs1   //2f "How many hours per week did ... usuall"


rename     ws2_2124    ersend2  //1f "What is the main reason ... stopped"
replace ersend2=15 if ersend2==6
replace ersend2=12 if ersend2==5
replace ersend2=11 if ersend2==4
replace ersend2=8 if ersend2==3
replace ersend2=-1 if ersend2==0


rename     ws2_2125    ejbhrs2   //2f "How many hours per week did ... usuall"

rename     sc1238      eptresn   //1f "main reason <35hrs"

/*
eptresn
V         -1 .Not in universe
V          1 .Could not find full-time job
V          2 .Wanted to work part time
V          3 .Temporarily unable to work
V            .full-time because of injury
V          4 .Temporarily not able to work
V            .full-time because of illness
V          5 .Unable to work full-time because
V            .of chronic health condition/
V            .disability
V          6 .Taking care of children/other
V            .persons
V          7 .Full-time workweek less than 35
V            .hours
V          8 .Slack work or material shortage
V          9 .Participated in a job sharing
V            .arrangement
V         10 .On vacation
V         11 .In school
V         12 .Other


1986, 1987,1988:
V          0 .Not in universe
V          1 .Could not find a full-time
             .job
V          2 .Wanted to work part-time
V          3 .Health condition or disability
V          4 .Normal working hours are less
             .than 35 hours
V          5 .Slack work or material shortage
V          6 Other

*/
replace eptresn=8 if eptresn==5
replace eptresn=7 if eptresn==4
replace eptresn=5 if eptresn==3
replace eptresn=-1 if eptresn==0

/*
rename     sc1656    renroll   //1f "Was ... enrolled in school either"
rename     sc1658   renrlma    //1f "All months"
rename     sc1660   eenrlmx_4    //1f "Last month"
rename     sc1662   eenrlmx_3    //1f "2 months ago"
rename     sc1664   eenrlmx_2   //y1f "3 months ago"
rename     sc1666   eenrlmx_1       //1f "4 months ago"
*/


/*
//y GENERATE EDUCATION VARIABLES


 //higrade     //y2f "What is the highest grade or year of"
//y grd_cmpl     //1f "Did he/she complete that grade"

capture gen hicomplgrade=.
replace hicomplgrade=higrade if grd_cmpl==1
replace hicomplgrade=higrade-1 if grd_cmpl==0 & higrade!=21
replace hicomplgrade=12 if grd_cmpl==0 & higrade==21

***** VERSION HAS TO CONFORM AS MUCH AS POSSIBLE WITH 1996 redefinition
//y note that we cannot distinguish among BA, and BA+ (MA, MBA, PhD, MD)
capture gen educ = 0
replace educ = 1 if hicomplgrade >= 9 & hicomplgrade <12     //   Less than high school diploma
replace educ = 2 if hicomplgrade ==12 & higrade==12                                      //y   Highschool diploma
replace educ = 3 if hicomplgrade >= 12 & higrade > 12 & hicomplgrade<24       //   Some College
replace educ = 4 if hicomplgrade >=24                                    //y   bachelor degree, plus
lab var educ "education"
lab value educ educlab
label define educlab   1 "Less than HS" 2 "HS diploma" 3 "Some College" 4 "College Grad" 5 "Prof/Doct degree"
*/

*/
 // GENERATE RWKESR1-5,

//y note that in the 1986 SIPP, we only have information on whether the worker is looking OR on layoff

/*D RWKESR1     2    857
*V         -1 .Not in universe
*V          1 .With job/bus - working
*V          2 .With job/bus - not on layoff,
*V            .absent w/out pay
*V          3 .With job/bus - on layoff, absent
*V            .w/out pay
*V          4 .No job/bus - looking for work or
*V            .on layoff
*V          5 .No job/bus - not looking and not
*V            .on layoff
*/

forvalues v=1(1)4     {


 //===== first week

capture gen rwkesr1x_`v'=.

 // niu/na
replace rwkesr1x_`v'=.  if job_wkonex_`v'==0 & look_wkonex_`v'==0 & abs_wkonex_`v'==0

 // with job working
replace rwkesr1x_`v'=1 if (job_wkonex_`v'==1 & abs_wkonex_`v'==2 & look_wkonex_`v'==2) | (job_wkonex_`v'==1 & (abs_wkonex_`v'==0 | look_wkonex_`v'==0))

 // with job, absent without pay
replace rwkesr1x_`v'=2 if job_wkonex_`v'==1 & abs_wkonex_`v'==1

 // with job, look/layoff
replace rwkesr1x_`v'=3 if job_wkonex_`v'==1 & look_wkonex_`v'==1

 // without job, look/layoff
replace rwkesr1x_`v'=4 if job_wkonex_`v'==2 & look_wkonex_`v'==1
replace rwkesr1x_`v'=4 if job_wkonex_`v'==0 & look_wkonex_`v'==1

 // without job, not looking
replace rwkesr1x_`v'=5 if job_wkonex_`v'==2 & look_wkonex_`v'==2
replace rwkesr1x_`v'=5 if job_wkonex_`v'==0 & look_wkonex_`v'==2

 //==== second week

capture gen rwkesr2x_`v'=.

 // niu/na
replace rwkesr2x_`v'=-1 if job_wktwox_`v'==0 & look_wktwox_`v'==0 & abs_wktwox_`v'==0

 // with job working
replace rwkesr2x_`v'=1 if (job_wktwox_`v'==1 & abs_wktwox_`v'==2 & look_wktwox_`v'==2) | (job_wktwox_`v'==1 & (abs_wktwox_`v'==0 | look_wktwox_`v'==0))

 // with job, absent without pay
replace rwkesr2x_`v'=2 if job_wktwox_`v'==1 & abs_wktwox_`v'==1

 // with job, look/layoff
replace rwkesr2x_`v'=3 if job_wktwox_`v'==1 & look_wktwox_`v'==1

 // without job, look/layoff
replace rwkesr2x_`v'=4 if job_wktwox_`v'==2 & look_wktwox_`v'==1
replace rwkesr2x_`v'=4 if job_wktwox_`v'==0 & look_wktwox_`v'==1

 // without job, not looking
replace rwkesr2x_`v'=5 if job_wktwox_`v'==2 & look_wktwox_`v'==2
replace rwkesr2x_`v'=5 if job_wktwox_`v'==0 & look_wktwox_`v'==2

 //===== third week

capture gen rwkesr3x_`v'=.

 // niu/na
replace rwkesr3x_`v'=-1 if job_wkthreex_`v'==0 & look_wkthreex_`v'==0 & abs_wkthreex_`v'==0

 // with job working
replace rwkesr3x_`v'=1 if (job_wkthreex_`v'==1 & abs_wkthreex_`v'==2 & look_wkthreex_`v'==2) | (job_wkthreex_`v'==1 & (abs_wkthreex_`v'==0 | look_wkthreex_`v'==0))

 // with job, absent without pay
replace rwkesr3x_`v'=2 if job_wkthreex_`v'==1 & abs_wkthreex_`v'==1

 // with job, look/layoff
replace rwkesr3x_`v'=3 if job_wkthreex_`v'==1 & look_wkthreex_`v'==1

 // without job, look/layoff
replace rwkesr3x_`v'=4 if job_wkthreex_`v'==2 & look_wkthreex_`v'==1
replace rwkesr3x_`v'=4 if job_wkthreex_`v'==0 & look_wkthreex_`v'==1

 // without job, not looking
replace rwkesr3x_`v'=5 if job_wkthreex_`v'==2 & look_wkthreex_`v'==2
replace rwkesr3x_`v'=5 if job_wkthreex_`v'==0 & look_wkthreex_`v'==2

 //===== fourth week

capture gen rwkesr4x_`v'=.

 // niu/na
replace rwkesr4x_`v'=-1 if job_wkfourx_`v'==0 & look_wkfourx_`v'==0 & abs_wkfourx_`v'==0

 // with job working
replace rwkesr4x_`v'=1 if (job_wkfourx_`v'==1 & abs_wkfourx_`v'==2 & look_wkfourx_`v'==2) | (job_wkfourx_`v'==1 & (abs_wkfourx_`v'==0 | look_wkfourx_`v'==0))

 // with job, absent without pay
replace rwkesr4x_`v'=2 if job_wkfourx_`v'==1 & abs_wkfourx_`v'==1

 // with job, look/layoff
replace rwkesr4x_`v'=3 if job_wkfourx_`v'==1 & look_wkfourx_`v'==1

 // without job, look/layoff
replace rwkesr4x_`v'=4 if job_wkfourx_`v'==2 & look_wkfourx_`v'==1
replace rwkesr4x_`v'=4 if job_wkfourx_`v'==0 & look_wkfourx_`v'==1

 // without job, not looking
replace rwkesr4x_`v'=5 if job_wkfourx_`v'==2 & look_wkfourx_`v'==2
replace rwkesr4x_`v'=5 if job_wkfourx_`v'==0 & look_wkfourx_`v'==2

 //==== five week

capture gen rwkesr5x_`v'=.

 // niu/na
replace rwkesr5x_`v'=-1 if job_wkfivex_`v'==0 & look_wkfivex_`v'==0 & abs_wkfivex_`v'==0

 // with job working
replace rwkesr5x_`v'=1 if (job_wkfivex_`v'==1 & abs_wkfivex_`v'==2 & look_wkfivex_`v'==2) | (job_wkfivex_`v'==1 & (abs_wkfivex_`v'==0 | look_wkfivex_`v'==0))

 // with job, absent without pay
replace rwkesr5x_`v'=2 if job_wkfivex_`v'==1 & abs_wkfivex_`v'==1

 // with job, look/layoff
replace rwkesr5x_`v'=3 if job_wkfivex_`v'==1 & look_wkfivex_`v'==1

 // without job, look/layoff
replace rwkesr5x_`v'=4 if job_wkfivex_`v'==2 & look_wkfivex_`v'==1
replace rwkesr5x_`v'=4 if job_wkfivex_`v'==0 & look_wkfivex_`v'==1

 // without job, not looking
replace rwkesr5x_`v'=5 if job_wkfivex_`v'==2 & look_wkfivex_`v'==2
replace rwkesr5x_`v'=5 if job_wkfivex_`v'==0 & look_wkfivex_`v'==2
}

capture drop     wkwjob01    //y1f "Did this person have a job or business"
capture drop     wkwjob02    //y1f "Did this person have a job or business"
capture drop     wkwjob03    //y1f "Did this person have a job or business"
capture drop     wkwjob04     //1f "Did this person have a job or business"
capture drop     wkwjob05     //1f "Did this person have a job or business"
capture drop     wkwjob06     //1f "Did this person have a job or business"
capture drop     wkwjob07     //1f "Did this person have a job or business"
capture drop     wkwjob08    //y1f "Did this person have a job or business"
capture drop     wkwjob09     //1f "Did this person have a job or business"
capture drop     wkwjob10     //1f "Did this person have a job or business"
capture drop     wkwjob11     //1f "Did this person have a job or business"
capture drop     wkwjob12    //y1f "Did this person have a job or business"
capture drop     wkwjob13    //y1f "Did this person have a job or business"
capture drop     wkwjob14     //1f "Did this person have a job or business"
capture drop     wkwjob15     //1f "Did this person have a job or business"
capture drop     wkwjob16    //y1f "Did this person have a job or business"
capture drop     wkwjob17    //y1f "Did this person have a job or business"
capture drop     wkwjob18    //y1f "Did this person have a job or business"

capture drop     weeksa01    //y1f "Was this person with a job or business"
capture drop     weeksa02    //y1f "Was this person with a job or business"
capture drop     weeksa03       //1f "Was this person with a job or business"
capture drop     weeksa04    //y1f "Was this person with a job or business"
capture drop     weeksa05     //1f "Was this person with a job or business"
capture drop     weeksa06    //y1f "Was this person with a job or business"
capture drop     weeksa07     //1f "Was this person with a job or business"
capture drop     weeksa08    //y1f "Was this person with a job or business"
capture drop     weeksa09    //y1f "Was this person with a job or business"
capture drop     weeksa10    //y1f "Was this person with a job or business"
capture drop     weeksa11     //1f "Was this person with a job or business"
capture drop     weeksa12    //y1f "Was this person with a job or business"
capture drop     weeksa13    //y1f "Was this person with a job or business"
capture drop     weeksa14     //1f "Was this person with a job or business"
capture drop     weeksa15    //y1f "Was this person with a job or business"
capture drop     weeksa16     //1f "Was this person with a job or business"
capture drop     weeksa17    //y1f "Was this person with a job or business"
capture drop     weeksa18     //1f "Was this person with a job or business"
capture drop     wkslok1     //y1f "Number of weeks looking for work or"
capture drop     wkslok2      //1f "Number of weeks looking for work or on"
capture drop     wkslok3     //y1f "Number of weeks looking for work or on"
capture drop     wkslok4      //1f "Number of weeks looking for work or on"
capture drop     weeksl01    //y1f "Was this person looking for work or on"
capture drop     weeksl02     //1f "Was this person looking for work or on"
capture drop     weeksl03    //y1f "Was this person looking for work or on"
capture drop     weeksl04     //1f "Was this person looking for work or on"
capture drop     weeksl05    //y1f "Was this person looking for work or on"
capture drop     weeksl06    //y1f "Was this person looking for work or on"
capture drop     weeksl07     //1f "Was this person looking for work or on"
capture drop     weeksl08    //y1f "Was this person looking for work or on"
capture drop     weeksl09     //1f "Was this person looking for work or on"
capture drop     weeksl10    //y1f "Was this person looking for work or on"
capture drop     weeksl11    //y1f "Was this person looking for work or on"
capture drop     weeksl12     //1f "Was this person looking for work or on"
capture drop     weeksl13     //1f "Was this person looking for work or on"
capture drop     weeksl14     //1f "Was this person looking for work or on"
capture drop     weeksl15     //1f "Was this person looking for work or on"
capture drop     weeksl16     //1f "Was this person looking for work or on"
capture drop     weeksl17     //1f "Was this person looking for work or on"
capture drop     weeksl18    //y1f "Was this person looking for work or on"


// FINALLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//y  reshaping the data set
/*

reshape long rwkesr5 rwkesr4 rwkesr3 rwkesr2 rwkesr1 tpmsum1 tpmsum2 rwksperm rhcalmn  rhcalyr /*
*/ ws1wks ws2wks whfnwgt core_ghlfsam core_gvarstr look_wkone abs_wkone job_wkone look_wktwo abs_wktwo job_wktwo look_wkthree /*
*/ abs_wkthree job_wkthree look_wkfour abs_wkfour job_wkfour look_wkfive abs_wkfive job_wkfive /*
*/ tmsa tmetro wpfinwgt, i(personkey) j(xxx) string

capture drop yearmonth
gen yearmonth=ym(rhcalyr,rhcalmn)


// dropping more variables

 capture drop abs_wk* job_wk* look_wk*
gen srefmon=.
capture replace srefmon=1 if xxx=="x_1"
capture replace srefmon=2 if xxx=="x_2"
capture replace srefmon=3 if xxx=="x_3"
capture replace srefmon=4 if xxx=="x_4"

*/



end 
