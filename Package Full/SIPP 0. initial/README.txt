***********************************************
** 0. SIPP CONSTRUCTION OF MAIN INTERMEDIATE .DTAs
***********************************************

(c) 2011-2022 Ludo Visschers CC BY-NC-SA (Create Commons license)
(if you want to deviate, e.g., from the sharealike restriction, please contact me)


This directory contains the .do files that transform the SIPP data 
in the intermediate files that are used in the derivations of 
SIPP 1. main statistics
SIPP 2. net mobility
SIPP 3. cyclical
Miscoding


Note: one statistic is calculated at this stage: the share of sample dropped due to self-employment (CHECK!!!)
________
STEP 0.0
^^^^^^^^

SIPP NBER results in the following files:

The SIPP raw data, dct-files and do-files (that label variables, etc.) can be downloaded from 
	https://www.nber.org/research/data/survey-income-and-program-participation-sipp
We express our gratitude to Jean Roth's work on the dct and do files, which are made available 
under GNU GPL license.



The resulting dta files should be:

1984:
etc.

	=====> Files produced: <======
	YYYYtotal_raw.dta
	(where YYYY is the panel: 1984, 1985, 1986, 1987, 1988, 1990, 1991, 1992, 1993, 1996, 2001, 2004, 2008)


________
STEP 0.1
^^^^^^^^

Run core_yyyy_step1 for every panel

	=====> Files produced: <======
	YYYY_corewave_occmob_min.dta
	(where YYYY is the panel: 1984, 1985, 1986, 1987, 1988, 1990, 1991, 1992, 1993, 1996, 2001, 2004, 2008)


________
STEP 0.2
^^^^^^^^

Run corewave_step2.do     (Make sure to get the most up to date one!!!!)


Note: instead of going through step 1 and 2, a more direct combined step (that drops more variables immediately) is possible. 


	=====> Files produced: <======
	see below




______________________________
OUTPUT OF STEP 0: INTERMEDIATE DTA FILES
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- a file containing all U and N spells in the Sipp data: 

	====> ctv_u_n_spells.dta <===

- a file that contains all of the labour force (including employment spells)

	====> ctv_lf.dta  <===

  ???--> wages are in??? 

- a file that is used for the estimation of the miscoding

	====> ctv_miscoding.dta <===