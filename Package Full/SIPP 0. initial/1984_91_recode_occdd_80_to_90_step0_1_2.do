//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// STEP 0.1.2.AUX RECODES SIPP OCC CODE 1984-91 PANEL 
//    INTO 1990 HOMOGENIZED CODE (using David Dorn's recode)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*
* this, with only small differences, implements the recoding according to David Dorn's occ1980_occ1990dd.dta (www.ddorn.net) from
* David Autor and David Dorn. "The Growth of Low Skill Service Jobs 
*    and the Polarization of the U.S. Labor Market." American Economic Review, 103(5), 1553-1597, 2013.
*
* (c) only the small incremental contribution: 2009-2022 Ludo Visschers CC BY-NC-SA (Create Commons license)
* (if you want to deviate, e.g., from the sharealike restriction, please contact me)
*
* - for the 1980s panels do the 1980->1990 recode, followed by the selfmap 90 to 90 recode
* - instead, in later do-files, we also use the IPUMS crosswalk for the homogenization into the 2000 census occ classification 
* 
*/



// these programs should be ran before variables are dropped, in particular we need the 3-digit (broad occupations) as an input, the name should be given with to fill in for $occupation

capture program drop recode_dd80_to_90
program define recode_dd80_to_90

set more off 
if "`1'"!="" {

global occupation  "`1'"
display "`1'"
global added_condition "(panel>=1984 & panel<=1991)"

capture drop ${occupation}_dd
capture gen ${occupation}_dd=${occupation}


* the following three are merged following dd (but left out of the 2016 version)
 replace ${occupation}_dd= 505 if $occupation== 506 & $added_condition
 replace ${occupation}_dd= 176 if $occupation== 177 & $added_condition
 replace ${occupation}_dd= 874 if $occupation== 873  & $added_condition
 replace ${occupation}_dd= 875 if $occupation== 874 & $added_condition 

 

replace ${occupation}_dd= 4 if $occupation== 3 & $added_condition // *
replace ${occupation}_dd= 22 if $occupation== 5 & $added_condition  // *
replace ${occupation}_dd= 33 if $occupation== 9 & $added_condition  // *
replace ${occupation}_dd= 18 if $occupation== 16 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 17 & $added_condition
replace ${occupation}_dd= 19 if $occupation== 18 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 19 & $added_condition
//.
replace ${occupation}_dd= 59 if $occupation== 49 & $added_condition
//
replace ${occupation}_dd= 68 if $occupation== 67 & $added_condition
replace ${occupation}_dd= 68 if $occupation== 68 & $added_condition
replace ${occupation}_dd= 69 if $occupation== 69 & $added_condition
replace ${occupation}_dd= 134 if $occupation== 135 & $added_condition
replace ${occupation}_dd= 165 if $occupation== 165 & $added_condition
replace ${occupation}_dd= 167 if $occupation== 167 & $added_condition
replace ${occupation}_dd= 169 if $occupation== 168 & $added_condition

replace ${occupation}_dd= 348 if $occupation== 348 & $added_condition
replace ${occupation}_dd= 348 if $occupation== 349 & $added_condition
replace ${occupation}_dd= 436 if $occupation== 437 & $added_condition

replace ${occupation}_dd= 461 if $occupation== 463 & $added_condition
replace ${occupation}_dd= 462 if $occupation== 464 & $added_condition
replace ${occupation}_dd= 463 if $occupation== 465 & $added_condition
replace ${occupation}_dd= 464 if $occupation== 466 & $added_condition
replace ${occupation}_dd= 465 if $occupation== 467 & $added_condition
//
//
//
//
replace ${occupation}_dd= 508 if $occupation== 515  & $added_condition
//
//
replace ${occupation}_dd= 634 if $occupation== 635 & $added_condition
replace ${occupation}_dd= 674 if $occupation== 673 & $added_condition
//
replace ${occupation}_dd= 729 if $occupation== 728 & $added_condition
replace ${occupation}_dd= 755 if $occupation== 758 & $added_condition

replace ${occupation}_dd= 675 if $occupation== 793 & $added_condition //**
replace ${occupation}_dd= 675 if $occupation== 794 & $added_condition //**
replace ${occupation}_dd= 675 if $occupation== 795 & $added_condition //**
replace ${occupation}_dd= 799 if $occupation== 796 & $added_condition      // inconseq
replace ${occupation}_dd= 799 if $occupation== 797 & $added_condition      // inconseq
replace ${occupation}_dd= 779 if $occupation== 798 & $added_condition      // inconseq
replace ${occupation}_dd= 799 if $occupation== 799 & $added_condition      // inconseq

//803-859 transportation
replace ${occupation}_dd= 804 if $occupation== 805 & $added_condition      // inconseq
replace ${occupation}_dd= 804 if $occupation== 806 & $added_condition      // inconseq
replace ${occupation}_dd= 809 if $occupation== 814 & $added_condition      // inconseq
replace ${occupation}_dd= 824 if $occupation== 826 & $added_condition      // inconseq
replace ${occupation}_dd= 829 if $occupation== 828 & $added_condition      // inconseq
replace ${occupation}_dd= 829 if $occupation== 829 & $added_condition     // inconseq
replace ${occupation}_dd= 829 if $occupation== 833 & $added_condition      // inconseq
replace ${occupation}_dd= 859 if $occupation== 843 & $added_condition     // inconseq
replace ${occupation}_dd= 848 if $occupation== 845 & $added_condition     // inconseq
replace ${occupation}_dd= 848 if $occupation== 848 & $added_condition
replace ${occupation}_dd= 848 if $occupation== 849 & $added_condition
replace ${occupation}_dd= 594 if $occupation== 855 & $added_condition //** grazer,dozer, scraper (mat moving) to  Paving, surfacing, and tamping equipment operators (6466) in construction
replace ${occupation}_dd= 804 if $occupation== 856 & $added_condition



//860-889 helpers
replace ${occupation}_dd= 864 if $occupation== 863  & $added_condition
replace ${occupation}_dd= 865 if $occupation== 864  & $added_condition
replace ${occupation}_dd= 866 if $occupation== 865  & $added_condition

replace ${occupation}_dd= 859 if $occupation== 876 & $added_condition // ** stevedores to material movers
replace ${occupation}_dd= 889 if $occupation== 877 & $added_condition
replace ${occupation}_dd= 889 if $occupation== 883 & $added_condition

/* note that the 1980 census construct does not assign certain helpers to occupations in which they are helping, but the self-map 90 to 90 does
we correct this by taking out these 
*/

// AGGREGATE INTO MAJOR OCCUPATIONS

*****************************************
**Recoding Occupation at a one digit level, using the 1996 recodes 
******************************************


display " ============RECODING AT A MAJOR OCCUPATIONAL GROUP LEVEL=========================="
global occidentifier "_1d"

capture drop ${occupation}_dd${occidentifier}
gen ${occupation}_dd${occidentifier}=.

display "1=management and management related occupations"
forvalues i=4(1)37 {
		replace ${occupation}_dd${occidentifier}=1 if ${occupation}_dd==`i'  & $added_condition
		}

display "2=professional speciality"
forvalues i=43(1)199 {
		replace ${occupation}_dd${occidentifier}=2 if ${occupation}_dd==`i'  & $added_condition
		}

display "3=technicians and related support organizations"		
forvalues i=203(1)235 {
		replace ${occupation}_dd${occidentifier}=3 if ${occupation}_dd==`i'  & $added_condition
		}

display "4=sales occupations"
forvalues i=243(1)285 {
		replace ${occupation}_dd${occidentifier}=4 if ${occupation}_dd==`i'  & $added_condition
		}

display "------------------300"

display "5=administrative support occupations"
forvalues i=303(1)389 {
		replace ${occupation}_dd${occidentifier}=5 if ${occupation}_dd==`i'  & $added_condition
		}


display "6=services: housekeeping and cleaning, 6, recoded 8"		
forvalues i=403(1)408 {
		replace ${occupation}_dd${occidentifier}=8 if ${occupation}_dd==`i'  & $added_condition
		}


display "7=protective services, 7, now recoded 8"		
forvalues i=413(1)427 {
		replace ${occupation}_dd${occidentifier}=8 if ${occupation}_dd==`i'  & $added_condition
		}


display "8=other services, recoded 8"		
forvalues i=433(1)472 {
		replace ${occupation}_dd${occidentifier}=8 if ${occupation}_dd==`i'  & $added_condition
		}


display "9=farming"		
forvalues i=473(1)476 {
		replace ${occupation}_dd${occidentifier}=9 if ${occupation}_dd==`i'  & $added_condition
		}

		
display "10=other aggricultural/fishing/logging, 10 now recoded 9"
forvalues i=477(1)499 {
		replace ${occupation}_dd${occidentifier}=9 if ${occupation}_dd==`i'  & $added_condition
		}
		
display "----------------500"

display "11 mechanics and repairers"
forvalues i=503(1)549 {
		replace ${occupation}_dd${occidentifier}=11 if ${occupation}_dd==`i'  & $added_condition
		}

		
display "12 construction and extractive occupations"
forvalues i=553(1)617 {
		replace ${occupation}_dd${occidentifier}=12 if ${occupation}_dd==`i'  & $added_condition
		}

display "13 precision production"
forvalues i=628(1)699 {
		replace ${occupation}_dd${occidentifier}=13 if ${occupation}_dd==`i'  & $added_condition
		}

display "14 machine operators, assemblers, inspectors"
forvalues i=703(1)799 {
		replace ${occupation}_dd${occidentifier}=14 if ${occupation}_dd==`i'  & $added_condition
		}

display "---------------800"

display "15 transportation/materials moving"
forvalues i=803(1)859 {
		replace ${occupation}_dd${occidentifier}=15 if ${occupation}_dd==`i'  & $added_condition
		}

display "16 laborers"
forvalues i=863(1)889 {
		replace ${occupation}_dd${occidentifier}=16 if ${occupation}_dd==`i'  & $added_condition
		}

capture label drop label_1dd		
label define label_1dd 1 "managing occupations" 2 "professional speciality" 3 "technicians and rel support" ///
		4 "sales occ." 5 "admin support" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val ${occupation}_dd${occidentifier} label_1dd		
		
display " ===============finished recoding============================"
}
end


