# Occupational Recoding for the SIPP in CTV 

(CTV January 2023)

The do files contain a plethora of different occupational codes, recodes, and aggregations,
used for a long list of robustness exercises (most of which did not end up in the paper or the online appendix, but are, in part, in the Supplementary Appendices). 
Here we repeat the three main ways in which we have homogenized the occupational codes over the period 1984-2013. Our standard method is method 3). 

## 1) using the do files

based on david dorn's occ1990_occ1990dd.dta, occ1980_occ1990dd.dta, occ2000_occ1990dd.dta, from https://www.ddorn.net/data.htm
(source: David Autor and David Dorn. "The Growth of Low Skill Service Jobs and the Polarization of the U.S. Labor Market." American Economic Review, 103(5), 1553-1597, 2013.)

1984_91_recode_occdd_80_to_90_step0_1_2.do
1992_2001_recode_occdd_90_to_90_step0_1_2.do
2004_08_recode_occdd_00_to_90_step0_1_2.do

used in YYYY_construction_ctv_step0_1_2.do, in step 0 

These files are all found in the directory .\Replication\SIPP 0. initial\

again, based on David Dorn's files, with minor changes, occ1990_occ1990dd.dta, occ1980_occ1990dd.dta, occ2000_occ1990dd.dta, from https://www.ddorn.net/data.htm
(source: David Autor and David Dorn. "The Growth of Low Skill Service Jobs and the Polarization of the U.S. Labor Market." American Economic Review, 103(5), 1553-1597, 2013.)


## 2) using the dta files 80dd90adj.dta , 80dd90.dta, 90dd90.dta, 00dd90.dta

these dta files are again based on David Dorn's files, with minor changes, occ1990_occ1990dd.dta, occ1980_occ1990dd.dta, occ2000_occ1990dd.dta, from https://www.ddorn.net/data.htm
(source: David Autor and David Dorn. "The Growth of Low Skill Service Jobs and the Polarization of the U.S. Labor Market." American Economic Review, 103(5), 1553-1597, 2013.)
now we directly merge in the occupation codes, instead of a do file that goes through a lengthy replace... if .... list. 

	**deviation from david dorn's files:**
		occ 864 (1980: helper mechanics and repairers ) would become occ 549 in dd original (1990: Not specified mechanics and repairers), occ 865 in version used (1990: Helpers, mechanics, and repairers)
		occ 865 (1980: helper construction trade) 			865	(1990: Helpers, mechanics, and repairers)			866 in version used (1990: Helpers, construction trades)
		occ 866 (1980: helper survetyors)				866	(1990: Helpers, construction trades)				218 in version used (1990: Surveying and mapping technicians)
		occ 867 (helpers, extractive occs) would become             occ 218 in dd original (Surveying and mapping technicians), 	   occ 889 in version used (laborers except construction)
	        occ 874 						    occ 873								   occ 874   (but 1980, should not be an 874; 1990: should not be an 873)
		(1980s 873 production helpers; 1990s: 874 production helpers; dd recode keeps 873 also in 1990, but then in step 3), gets coded into 51 "production")

These files are all found in the directory .\Replication\SIPP 0. initial\occ_ind_recodes\ 

## 3) using CPS-IPUMS to instead map the occ1990dd variable into soc2010. 


This is a two-step procedure: first we translate soc1980 and soc1990 in the sipp to occ1990dd, following 2)
Then we translate occ1990dd into occ2000rec, using the cross-walk used (as inferred) from CPS IPUMS 
(source: Steven Ruggles, Sarah Flood, Ronald Goeken, Megan Schouweiler and Matthew Sobek. IPUMS USA: Version 12.0 [dataset]. Minneapolis, MN: IPUMS, 2022. https://doi.org/10.18128/D010.V12.0)

These files are all found in the directory `.\Replication\SIPP 0. initial\occ_ind_recodes\`
(the do-file that takes care of the construction of the occ1990dd2000soc.dta crosswalk is at .\Replication\CPS\ipums_occ1990dd_occ2010_recode_construction.do). This do file contains additional comments about its construction (and the different versions of the data we produced with it)

(Interestingly, there is less miscoding associated with the 1980s occupations (coded originally in the 1980 SOC) after they are mapped into the 2000 SOC -- of course, these two classifications, at the major occuaption group level are rather different -- but it is interesting that the 2000 SOC classification reduces miscoding according to MOG even in the 1980s. See the paper, and appendices for more info)

A similar exercise can be done be done in the IPUMS USA.
(IPUMS USA: Steven Ruggles, Sarah Flood, Ronald Goeken, Megan Schouweiler and Matthew Sobek. IPUMS USA: Version 12.0 [dataset]. Minneapolis, MN: IPUMS, 2022. https://doi.org/10.18128/D010.V12.0)
