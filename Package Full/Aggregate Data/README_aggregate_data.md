# README Aggregate Data

In this file we document the source data series behind the unemployment and output per worker data series. 

In aggdata_prep.do, these series are (to the extent needed) processed/constructed, some already logged and HP filtered. 

These data are used in the subsequent CPS and, in particular, SIPP analysis.  

************

## UNEMPLOYMENT

Two source series
-UNRATE in UNRATE.xlsx:
U.S. Bureau of Labor Statistics, Unemployment Rate [UNRATE], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/UNRATE, December 4 2002 (Simple average of monthly unemployment rates)

-LNS14000000Q.xlsx: LNS14000000Q Quarterly unemployment rate series directly downloaded from the BLS, in november 2022. (https://data.bls.gov/cgi-bin/srgate)


-LNS14000000Q_2016.xlsx: LNS14000000Q Quarterly unemployment rate series directly downloaded from the BLS, in early 2017. 
These two LNS14000000Q data series differ 0.1 pp at a number of times. When constructing the data of the pictures, we used the  earlier series of Quarterly BLS unemployment, subsequently revised by 0.1pp in 2015q1 and 2016q1. This has a small impact on the hp filtered unemployment rates in 2012-2014. We mark the older series unrate_bls_0, and keep using it in the analysis in the paper as the differences are very slight. 


 

********

## OUTPUT PER WORKER

To construct it, use series

 -- Employment  CPS
LNS12032189Q   Q	(Seas) Employment Level - Nonagriculture, Private Industries Wage and Salary Workers	
LNS12032192Q   Q	(Seas) Employment Level - Nonagriculture, Self-employed Workers, Unincorporated	 (naturally private industries!)


-- Output and Productivity
PRS85006043	Major Sector Productivity and Costs	 Sector:	Nonfarm Business	Measure:	Output	 Duration:	Index, base year = 100				

Again, these series can be accessed via https://data.bls.gov/cgi-bin/srgate



************

## VACANCIES

Using the composite help-wanted index (version up to 2014m12) from Regis Barnichon. These data were constructed as part of the paper ""Building a composite Help-Wanted Index"" (Barnichon, 2010)."	
(According to Barnichon: anyone is free to download this data and use it in his/her own research, as long as he/she cites the aforementioned paper.) A more recent version than the one used for this paper is now available. 




****************

## SEPARATIONS AND JOB FINDING RATES DIRECTLY FROM THE BLS (in JF_SEPU_BLS.DTA)

File `jf_sepu_bls_creation.do` documents how these series were constructed. jf_sepu_bls.dta was created by copy-pasting 4 series into the data editor:
LNS17100000
LNS12000000 
LNS17400000
LNS13000000
These series were accessed in August 2018. 

The BLS job finding series constructed as LNS17100000/LNS12000000, the separation Rate series constructed as LNS17400000/LNS13000000.

These 4 LNS-series can be accessed via https://data.bls.gov/cgi-bin/srgate . For replication, we use the data here as accessed at the end of 2018. 

The BLS job finding and separation series are included in the package mainly to allow for robustness testing; the only series that uses jf_bls as an ingredient is the job finding time series for workers in NUN spells in the Online Appendix (where addressing censoring issues affects the number of quarters that can be used/have a large enough number of observations; we use the BLS jf rate to instrument this series).
