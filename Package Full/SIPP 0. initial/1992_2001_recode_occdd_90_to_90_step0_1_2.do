//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// STEP 0.1.2.AUX RECODES SIPP OCC CODE 1992-2001 PANEL 
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

// BASED ON DAVID DORN'S CODE 
// Census 1990 mapping into itself


capture program drop recode_dd90_to_90
program define recode_dd90_to_90

set more off 
if "`1'"!="" {

global occupation  "`1'"
display "`1'"
*global added_condition "(panel>=1992 & panel<=2001)"
global added_condition " panel!=."

gen ${occupation}_dd=$occupation

//managers and executives, administrators
replace ${occupation}_dd= 13 if $occupation== 197 & $added_condition

replace ${occupation}_dd= 22 if $occupation== 5 & $added_condition
replace ${occupation}_dd= 22 if $occupation==16 & $added_condition
replace ${occupation}_dd= 22 if $occupation==17 & $added_condition
replace ${occupation}_dd= 22 if $occupation==21 & $added_condition


replace ${occupation}_dd= 33 if $occupation==9 & $added_condition

replace ${occupation}_dd= 47 if $occupation== 46 & $added_condition
replace ${occupation}_dd= 47 if $occupation== 47 & $added_condition

replace ${occupation}_dd= 59 if $occupation== 49 & $added_condition
replace ${occupation}_dd= 59 if $occupation== 54 & $added_condition
replace ${occupation}_dd= 59 if $occupation== 58 & $added_condition
replace ${occupation}_dd= 59 if $occupation== 59 & $added_condition

replace ${occupation}_dd= 68 if $occupation== 67 & $added_condition
replace ${occupation}_dd= 68 if $occupation== 68 & $added_condition


replace ${occupation}_dd= 154 if $occupation== 113 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 114 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 115 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 116 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 117 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 118 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 119 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 123 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 124 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 125 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 126 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 127 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 128 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 129 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 133& $added_condition
replace ${occupation}_dd= 154 if $occupation== 134 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 135& $added_condition
replace ${occupation}_dd= 154 if $occupation== 136 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 137 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 138 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 139 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 143 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 144 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 145 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 146 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 147 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 148 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 149 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 153 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 154 & $added_condition

replace ${occupation}_dd= 466 if $occupation== 175 & $added_condition
 
replace ${occupation}_dd= 176 if $occupation== 176 & $added_condition
replace ${occupation}_dd= 176 if $occupation== 177 & $added_condition

replace ${occupation}_dd= 177 if $occupation== 465 & $added_condition

replace ${occupation}_dd= 178 if $occupation== 178 & $added_condition
replace ${occupation}_dd= 178 if $occupation== 179 & $added_condition

replace ${occupation}_dd= 214 if $occupation== 213 & $added_condition
replace ${occupation}_dd= 214 if $occupation== 214 & $added_condition
replace ${occupation}_dd= 214 if $occupation== 215 & $added_condition
replace ${occupation}_dd= 214 if $occupation== 216 & $added_condition

replace ${occupation}_dd= 218 if $occupation== 63 & $added_condition

replace ${occupation}_dd= 274 if $occupation== 257 & $added_condition
replace ${occupation}_dd= 274 if $occupation== 259 & $added_condition
replace ${occupation}_dd= 274 if $occupation== 284 & $added_condition
replace ${occupation}_dd= 274 if $occupation== 285 & $added_condition

replace ${occupation}_dd= 275 if $occupation== 263 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 264 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 265 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 266 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 267 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 268 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 269 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 274 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 275 & $added_condition

replace ${occupation}_dd= 277 if $occupation== 277 & $added_condition
replace ${occupation}_dd= 277 if $occupation== 278 & $added_condition

replace ${occupation}_dd= 303 if $occupation== 305 & $added_condition
replace ${occupation}_dd= 308 if $occupation== 304 & $added_condition
replace ${occupation}_dd= 308 if $occupation== 309 & $added_condition

replace ${occupation}_dd= 313 if $occupation== 313 & $added_condition
replace ${occupation}_dd= 313 if $occupation== 314 & $added_condition

replace ${occupation}_dd= 319 if $occupation== 319 & $added_condition
replace ${occupation}_dd= 319 if $occupation== 323 & $added_condition

replace ${occupation}_dd= 326 if $occupation== 326 & $added_condition
replace ${occupation}_dd= 326 if $occupation== 327 & $added_condition


replace ${occupation}_dd= 336 if $occupation== 325 & $added_condition

replace ${occupation}_dd= 344 if $occupation== 339 & $added_condition
replace ${occupation}_dd= 344 if $occupation== 343 & $added_condition
replace ${occupation}_dd= 344 if $occupation== 344 & $added_condition

replace ${occupation}_dd= 347 if $occupation== 345 & $added_condition

replace ${occupation}_dd= 348 if $occupation== 306 & $added_condition

replace ${occupation}_dd= 364 if $occupation== 307 & $added_condition

replace ${occupation}_dd= 389 if $occupation== 369 & $added_condition
replace ${occupation}_dd= 389 if $occupation== 374 & $added_condition

replace ${occupation}_dd= 405 if $occupation== 405 & $added_condition
replace ${occupation}_dd= 405 if $occupation== 407 & $added_condition
replace ${occupation}_dd= 405 if $occupation== 449 & $added_condition

replace ${occupation}_dd= 408 if $occupation== 403 & $added_condition
replace ${occupation}_dd= 408 if $occupation== 748 & $added_condition


replace ${occupation}_dd= 417 if $occupation== 413 & $added_condition
replace ${occupation}_dd= 417 if $occupation== 416 & $added_condition
replace ${occupation}_dd= 417 if $occupation== 417 & $added_condition

replace ${occupation}_dd= 418 if $occupation== 6 & $added_condition
replace ${occupation}_dd= 418 if $occupation== 418 & $added_condition
replace ${occupation}_dd= 418 if $occupation== 414 & $added_condition

replace ${occupation}_dd= 423 if $occupation== 423 & $added_condition
replace ${occupation}_dd= 423 if $occupation== 424 & $added_condition


replace ${occupation}_dd= 436 if $occupation== 436 & $added_condition
replace ${occupation}_dd= 436 if $occupation== 437 & $added_condition
replace ${occupation}_dd= 436 if $occupation== 404 & $added_condition

replace ${occupation}_dd= 444 if $occupation== 438 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 443 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 444 & $added_condition

replace ${occupation}_dd= 447 if $occupation== 446 & $added_condition
replace ${occupation}_dd= 447 if $occupation== 447  & $added_condition



replace ${occupation}_dd= 468 if $occupation==406 & $added_condition
replace ${occupation}_dd= 468 if $occupation==466 & $added_condition
replace ${occupation}_dd= 468 if $occupation==467 & $added_condition

replace ${occupation}_dd= 469 if $occupation== 454 & $added_condition
replace ${occupation}_dd= 469 if $occupation== 773 & $added_condition

replace ${occupation}_dd= 470 if $occupation== 456 & $added_condition
replace ${occupation}_dd= 471 if $occupation== 463 & $added_condition

replace ${occupation}_dd= 472 if $occupation== 487 & $added_condition


replace ${occupation}_dd= 473 if $occupation== 473 & $added_condition
replace ${occupation}_dd= 473 if $occupation== 474 & $added_condition

replace ${occupation}_dd= 475 if $occupation== 475 & $added_condition
replace ${occupation}_dd= 475 if $occupation== 476 & $added_condition

replace ${occupation}_dd= 479 if $occupation== 477 & $added_condition
replace ${occupation}_dd= 479 if $occupation== 479 & $added_condition
replace ${occupation}_dd= 479 if $occupation== 484 & $added_condition

replace ${occupation}_dd= 496 if $occupation== 494 & $added_condition
replace ${occupation}_dd= 496 if $occupation== 495 & $added_condition
replace ${occupation}_dd= 496 if $occupation== 496 & $added_condition


replace ${occupation}_dd= 498 if $occupation== 498 & $added_condition
replace ${occupation}_dd= 498 if $occupation== 499 & $added_condition
replace ${occupation}_dd= 498 if $occupation== 483 & $added_condition

replace ${occupation}_dd= 503 if $occupation== 503 & $added_condition
replace ${occupation}_dd= 505 if $occupation== 505 & $added_condition
replace ${occupation}_dd= 505 if $occupation== 506 & $added_condition

replace ${occupation}_dd= 508 if $occupation== 508 & $added_condition
replace ${occupation}_dd= 508 if $occupation== 515 & $added_condition

replace ${occupation}_dd= 516 if $occupation== 516 & $added_condition
replace ${occupation}_dd= 516 if $occupation== 517 & $added_condition
replace ${occupation}_dd= 518 if $occupation== 518 & $added_condition

replace ${occupation}_dd= 527 if $occupation== 527 & $added_condition
replace ${occupation}_dd= 527 if $occupation== 529 & $added_condition
replace ${occupation}_dd= 533 if $occupation== 538 & $added_condition
replace ${occupation}_dd= 535 if $occupation== 647 & $added_condition

replace ${occupation}_dd= 549 if $occupation== 547 & $added_condition
replace ${occupation}_dd= 549 if $occupation== 549 & $added_condition
replace ${occupation}_dd=  549 if $occupation==  865 & $added_condition

replace ${occupation}_dd= 558 if $occupation== 553 & $added_condition
replace ${occupation}_dd= 558 if $occupation== 554 & $added_condition
replace ${occupation}_dd= 558 if $occupation== 555 & $added_condition
replace ${occupation}_dd= 558 if $occupation== 556 & $added_condition
replace ${occupation}_dd= 558 if $occupation== 557 & $added_condition
replace ${occupation}_dd= 558 if $occupation== 558 & $added_condition

replace ${occupation}_dd= 563 if $occupation== 563 & $added_condition
replace ${occupation}_dd= 563 if $occupation== 564 & $added_condition
replace ${occupation}_dd= 563 if $occupation== 565 & $added_condition
replace ${occupation}_dd= 563 if $occupation== 566 & $added_condition

replace ${occupation}_dd= 567 if $occupation== 567 & $added_condition
replace ${occupation}_dd= 567 if $occupation== 569 & $added_condition

replace ${occupation}_dd= 575 if $occupation== 575 & $added_condition
replace ${occupation}_dd= 575 if $occupation== 576 & $added_condition

replace ${occupation}_dd= 585 if $occupation== 585 & $added_condition
replace ${occupation}_dd= 585 if $occupation== 587 & $added_condition

replace ${occupation}_dd= 628 if $occupation== 633 & $added_condition
replace ${occupation}_dd= 628 if $occupation== 864 & $added_condition

replace ${occupation}_dd= 634 if $occupation== 634 & $added_condition
replace ${occupation}_dd= 634 if $occupation== 635 & $added_condition
replace ${occupation}_dd= 634 if $occupation== 655 & $added_condition


replace ${occupation}_dd= 637 if $occupation== 637 & $added_condition
replace ${occupation}_dd= 637 if $occupation== 639 & $added_condition

replace ${occupation}_dd= 645 if $occupation== 656 & $added_condition
replace ${occupation}_dd= 645 if $occupation== 676 & $added_condition

replace ${occupation}_dd= 653 if $occupation== 646 & $added_condition


replace ${occupation}_dd= 653 if $occupation== 653 & $added_condition
replace ${occupation}_dd= 653 if $occupation== 654 & $added_condition

replace ${occupation}_dd= 658 if $occupation== 658 & $added_condition
replace ${occupation}_dd= 658 if $occupation== 659 & $added_condition

replace ${occupation}_dd= 666 if $occupation== 666 & $added_condition
replace ${occupation}_dd= 666 if $occupation== 667 & $added_condition

replace ${occupation}_dd= 669 if $occupation== 674 & $added_condition

replace ${occupation}_dd= 675 if $occupation== 786 & $added_condition
replace ${occupation}_dd= 675 if $occupation== 787 & $added_condition
replace ${occupation}_dd= 675 if $occupation== 793 & $added_condition
replace ${occupation}_dd= 675 if $occupation== 794 & $added_condition
replace ${occupation}_dd= 675 if $occupation== 795 & $added_condition


replace ${occupation}_dd= 678 if $occupation== 678 & $added_condition
replace ${occupation}_dd= 679 if $occupation== 679 & $added_condition

replace ${occupation}_dd= 689 if $occupation== 689 & $added_condition
replace ${occupation}_dd= 689 if $occupation== 693 & $added_condition

replace ${occupation}_dd= 703 if $occupation== 703 & $added_condition
replace ${occupation}_dd= 703 if $occupation== 704 & $added_condition
replace ${occupation}_dd= 703 if $occupation== 705 & $added_condition
replace ${occupation}_dd= 729 if $occupation== 728 & $added_condition
replace ${occupation}_dd= 729 if $occupation== 729 & $added_condition
replace ${occupation}_dd= 733 if $occupation== 733 & $added_condition
replace ${occupation}_dd= 733 if $occupation== 726 & $added_condition

replace ${occupation}_dd= 734 if $occupation== 734 & $added_condition
replace ${occupation}_dd= 734 if $occupation== 735 & $added_condition
replace ${occupation}_dd= 736 if $occupation== 736 & $added_condition
replace ${occupation}_dd= 734 if $occupation== 737 & $added_condition

replace ${occupation}_dd= 755 if $occupation== 755 & $added_condition
replace ${occupation}_dd= 755 if $occupation== 758 & $added_condition

replace ${occupation}_dd= 769 if $occupation== 768 & $added_condition
replace ${occupation}_dd= 769 if $occupation== 769 & $added_condition




replace ${occupation}_dd= 779 if $occupation== 777 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 779 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 725 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 714 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 715 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 717 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 798 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 759 & $added_condition

replace ${occupation}_dd= 783 if $occupation== 783 & $added_condition
replace ${occupation}_dd= 783 if $occupation== 784 & $added_condition


replace ${occupation}_dd= 785 if $occupation== 636 & $added_condition
replace ${occupation}_dd= 785 if $occupation== 683 & $added_condition

replace ${occupation}_dd= 799 if $occupation== 796 & $added_condition
replace ${occupation}_dd= 799 if $occupation== 797 & $added_condition
replace ${occupation}_dd= 799 if $occupation== 799 & $added_condition




replace ${occupation}_dd= 803 if $occupation== 803 & $added_condition
replace ${occupation}_dd= 804 if $occupation== 804 & $added_condition
replace ${occupation}_dd= 804 if $occupation== 805 & $added_condition
replace ${occupation}_dd= 804 if $occupation== 806 & $added_condition
replace ${occupation}_dd=  804 if $occupation==  856 & $added_condition

replace ${occupation}_dd= 809 if $occupation== 814 & $added_condition

replace ${occupation}_dd=  824 if $occupation==  824 & $added_condition
replace ${occupation}_dd=  824 if $occupation==  826 & $added_condition

replace ${occupation}_dd= 829 if $occupation== 497 & $added_condition
replace ${occupation}_dd=  829 if $occupation==  828 & $added_condition
replace ${occupation}_dd=  829 if $occupation==  829 & $added_condition
replace ${occupation}_dd=  829 if $occupation==  833 & $added_condition
 
replace ${occupation}_dd=  848 if $occupation==  845 & $added_condition
replace ${occupation}_dd=  848 if $occupation==  848 & $added_condition
replace ${occupation}_dd=  848 if $occupation==  849 & $added_condition



replace ${occupation}_dd= 859 if $occupation==  843 & $added_condition
replace ${occupation}_dd=  859 if $occupation==  876 & $added_condition
replace ${occupation}_dd=  859 if $occupation==  859 & $added_condition


replace ${occupation}_dd=  594 if $occupation==  855 & $added_condition
replace ${occupation}_dd=  549 if $occupation==  865 & $added_condition

replace ${occupation}_dd=  866 if $occupation==  866 & $added_condition
replace ${occupation}_dd=  218 if $occupation==  867 & $added_condition


replace ${occupation}_dd=  873 if $occupation==  873 & $added_condition
replace ${occupation}_dd=  873 if $occupation==  874 & $added_condition


replace ${occupation}_dd=  889 if $occupation==  889 & $added_condition
replace ${occupation}_dd=  889 if $occupation==  877 & $added_condition
replace ${occupation}_dd=  889 if $occupation==  883 & $added_condition

// AGGREGATE INTO MAJOR OCCUPATIONS

*****************************************
**Recoding Occupation at a one digit level, using the 1996 recodes (i.e 1990 SOC)
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

label define label_1dd 1 "managing occupations" 2 "professional speciality" 3 "technicians and rel support" ///
		4 "sales occ." 5 "admin support" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val ${occupation}_dd${occidentifier} label_1dd		
		

display " ===============finished recoding============================"
}
end

