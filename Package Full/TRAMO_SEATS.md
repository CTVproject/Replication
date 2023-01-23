# TRAMO/SEATS (to address gaps in timeseries)

(by CTV, January 2023)

To deal with gaps in our time series, we use TRAMO ("Time Series Regression with ARIMA Noise, Missing Observations and Outliers"), at times in conjunction with SEATS ("Seasonal Extraction in ARIMA Time Series”).

TRAMO/SEATS has been developed by Victor Gómez and Agustín Maravall from the Bank of Spain: 
    - Gómez, Victor, and Agustín Maravall Herrero. Programs TRAMO and SEATS: instructions for the user (beta version: September 1996). Banco de España. Servicio de Estudios, 1996.
    - Maravall A, Gómez V, Caporello G. Statistical and econometrics software: Tramo and seats. Statistical and Econometrics Software. 2015.

We have used the program TSW+ by Gianluca Caporello and Agustin Maravall 
with programming help of D. Pérez Cañete and R. López Pavón, revision 941, 
build 2015/10/14 12:07:23, to apply TRAMO-SEATS to the time series. This program is available as Freeware (for conditions etc. see program when installing). It should be downloadable from the Bank of Spain 
website hosting it, but in our last attempt, their link seemed to have 
expired, we are including a link to it on [our github page](https://github.com/CTVproject/Replication/blob/main/TRAMO-SEATS/TSW%2B%20x64%20.msi)
Fully spelled out, at https://github.com/CTVproject/Replication/blob/main/TRAMO-SEATS/TSW%2B%20x64%20.msi .

For more info on TSW+, see  *Caporello, Gianluca, and Agustín Maravall. "Program Tsw." Revised manual. Version May (2004)*


To give brief highlights, we use the standard seats/tramo in the TSW program, with automatic procedure 
parameter RSA=4 then we save the TRAMO output with the appropriate name (see 
below in program) 	


* [series name]_XLIN SERIES. *used by default* (this one is calendar adjusted, 
plus corrects for other deterministic issues)
* [series name]_XINT is interpolated, but not adjusted for deterministic (seasonal) variation
* (if [series name]_XLIN compensates for a structural break, we prefer to stick to [series name]_XINT)

Applying TRAMO/SEATS is done 'by hand' inside program TSW+ (i.e. there is no script that we know of that can be invoked by stata).  

To not break the (elaborate) run of the stata program, we have added two kinds of reference time series, 
one before tramo, one after tramo, which contain the time series as used in the paper, 
(the after series, after we used TSW+ ourselves).
After running the entire stata replication package, one can check the correctness 
of the TRAMO procedure by 

    a. checking that the before-TRAMO reference series coincides with the series produced by the replication package run
    b. use TSW+ to apply TRAMO/SEATS to the series produced by the STATA run, and see that the series after TRAMO/SEATS coincides with the after series that was already read into STATA before. 

## 1. Which series are TRAMO-ed, and where are they produced (and saved!)?

Although many series can be TRAMO-ed (and in our study with did -- e.g. with respect 
to many age patterns), the results in the paper and replication package require the following 16 series:
* In step2_5_2, we produce `${outputdata}/ts_occmob_paper_for_tramo.xlsx`, which contains the following 5 series
	- qn3_mob_12_mm
	- qn3u_mob_12_mm
	- qn1_mob_12_mm
	- qn1u_mob_12_mm
    - lunrate_bls (needed in step2_10 for the TRAMO/SEATS trendcycle component, for robustness) 
(The post-TRAMO/SEATS series are use in step2_10 and subsequently step2_11)

*  In step2_9_2 and step2_9_3, we produce need SEPARATIONS and JOB FINDING SERIES, these are in `${outputdata}/ts_jf_paper_before_tramo.xlsx` and `${outputdata}/ts_sep_paper_for_tramo.xlsx`, and (after applying TRAMO/SEATS) used in step2_11_2 and step2_11_3, towards step2_11_5 (which establishes the properties of the standard-type HP-filtered labour market time series)

	- ts_ljf_q 
	- q3_ljf_q
	- q3_ljfn_cnune_q 
	- q3_ljfn_cnune_instr_q 
	- ts_lsep_q 
	- q3_lsep_q 

*  FINALLY, IN step2_12_....do, we produce (and subsequently use, after TRAMO/SEATS) `${outputdata}/udistrprops_paper_for_tramo.xlsx`

	- unemp_cat0q
	- unemp_cat1q
	- unemp_cat2q
	- unemp_cat3q
	- unemp_cat4q

For a total of 16 series. 

After apply TRAMO/SEATS inside TSW, each series (and its post-TRAMO/SEATS descendents) is saved in an xlsx file. These files should bear the name of the corresponding series (e.g. `qn1_mob_12_mm.xlsx`) if one wants to use the STATA do-files of the replication package to read these files back into STATA (without writing the code for this yourself, of course, that's also an option). 


## 2. Using TSW + TRAMO/SEATS (in brief)


In brief, we use the standard in the TSW program , with automatic procedure parameter RSA=4
then we save the TRAMO and SEATS output with the appropriate name (see below in program) 	

* If TRAMO Out Tables: this allows us to save the resulting series in xls (to import into STATA) 
and yields three series of interest:
-xorig series: original input, with -99999 for missing values
-xint: interpolated series, but original values when non-interpolated
-xlin: clear of deterministic variation. 	

BY DEFAULT WE USE XLIN SERIES (that one is calendar adjusted, plus other deterministic issues)
(XINT is interpolated, but not adjusted for deterministic (seasonal) variation; however, XLIN allows
for clearing out structural breaks, which in some settings would not be desired)

                                                
* SEATS (on top of TRAMO output) further decomposes the time series.
- trend-cycle series: smooth series that captures both trend and cycle, but outliers and SA corrected
this series is relevant as the quarter-by-quarter (unsmoothed) mobility is quite noisy

## 3. HOW TO USE TSW+, in more detail 

1) Open/Run the program

2) Click the 'Series' button, this opens the standard 'Open' dialog in Windows.
    - Go to the outputdata directory, and select the file of interest
    - A window pops up, inquiring about the periodicity, Press OK at periodity 4.
    - In the series list window, expand the series list. 

3) In the series list, select the series to apply TRAMO/SEATS to, e.g. qn3_mob_12_mm
- On the right-hand side, next to the series window, verify that the following 
options are set
    -- iter=0
    -- Seats/Tramo to Tramo/Seats

4) Press the 'Model (+)' button. Verify the automatic procedure parameter is 
set to (RSA=)4. It is possible to select multiple series and apply the same *model* to it

5) Select a single series and press 'Run'

6) Click button 'Out Tables', choose option 'Out Table Tramo'
- this shows the Xorig, Xint, Xlin timeseries

7) At the bottom, press the right-most Save Excel button, save the file as 
[series name].xlsx, in our example qn3_mob_12_mm.xlsx.

- It is important that the series names are kept, as the first thing that STATA 
will do 'post TRAMO' (not literally post, see comment below) is to 
import the resulting series back into STATA.  

8) Close the Out Table window, returning to the main program

9) Click button 'Out Tables', choose option 'Out Table Seats'

10) Click Save Excel button at the bottom, save file as 
[series name]_seats.xlsx (in our example qn3_mob_12_mm_seats.xlsx)

- close 'Seats' window, and move to the next series (i.e. return to step 3)
- cycle through step 3 to step 10 for all series of interest.

11) Make sure (copy) that all the series produced by TSW are subsequently present in the output (`${outputdata}` directory (if not already saved there, by hand), where the input series were found. 
By default TSW will suggest the `.\TSW+\SAVED\` directory for saving files. 
At the end of saving all files in the latter, one could copy-paste 
these files into the `${outputdata}` directory.
This is not unimportant: below STATA will look for those files there. 
However, see below about using the reference series to keep STATA running,
and just verifying the TRAMO/SEATS at the very end of replication. 



Inside a subdirectory of our step 2 code directory (`${step2codedir}/Tramo_Seats_series_REFERENCE`), we have saved the before-TRAMO timeseries and after-TRAM timeseries we are using in our paper. This means that, when replicating, STATA can run uninterruptedly till the very end, and the TRAMO/SEATS step can be verified ex post

- check that the before-tramo series produced in the replication overlap
with the reference before-tramo series. 
- do tramo/seats (via TSW+) on the series produced in replication, and 
compare these to the after-tramo reference series. (If copying these
series into the `${outputdata}` directory, verify the timestamp 
to make sure that the series previously there is overwritten!)
To facilitate this comparison, the step2_5_2 do-file, at the beginning, placed a copy of 
these reference series in the `${outputdata}` directory, so all series are in one
location (though the after-tramo series were put there by hand).


