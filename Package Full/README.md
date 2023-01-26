# Replication Materials for "Unemployment and Endogenous Reallocation over the Business Cycle"

(c) 2023 Ludo Visschers and Carlos Carrillo-Tudela, see bottom of this file for more detail



## 1. Replication Package Contents: Subfolders

* Fortran: this contains the code for our computation (quantitative model), in 6 versions, using the estimated parameters :
 
    - Gross and Net Mobility version: this is the main version, with occupational heterogeneity along 4 'super' occupation categories: Non-routine Cognitive, Routine Cognitive, Non-routine Manual and Routine Manual, on top of mobility across 22 major occupational groups.

    - Excess Mobility version: this is the main alternative version (also in the main text), with occupation-wide heterogeneity shut down (but the dynamics of worker-occupation match effects remaining key, as in every version)
        
    - Robustness: 4 different further versions, discussed in the online appendix  

* Aggregate Data: contains the source series from the BLS and FRED (unemployment and aggregate output) and the STATA code that processes them.  These series are then used in the main analysis

* Computation Results Processing: contains the do-files that combine model and data output for tables and figures in the paper. Also copies results of the different versions to the results directory with the appropriate names (referring to the tables/figures in the paper in which the result is used and/or incorporating the version of the computed quantitative model)

* CPS and SIPP subfolders contain the do-files to each of the two different data sources. The work done on the SIPP is by far more extensive, and will take most time to run. The file `master_CTV.do` invokes the main steps one-by-one.   


## 2. How to Run the Complete Replication
----------------------------------------------------

1. Run the six Fortran programs inside the Fortran folder, so the results of these runs can be combined with the subsequent data analysis. For more details about how to do this, see README.txt in Fortran Folder.

2. Running the `master_CTV.do` file in STATA: **before running, set the current working directory in STATA to the root directory of the replication package**, then run `master_CTV.do`; **or better, set the 'workingdir' GLOBAL in the `global_paths.do` file to the root folder of the replication package, then run `master_CTV.do`**. The results/main outputs produced by master_CTV.do are covered in the README.md/README.pdf in the Results folder.

This do file governs the construction of the relevant SIPP datasets from 'scratch', by downloading the raw SIPP data from the NBER, converting these in dtas, and selecting and defining variables for our goals. Sometimes, it appears, the downloading can get disrupted (or not even start), this is not necessarily an issue of the do-file (we think). Overall, building our datasets from the original data costs a lot of space (although improvements can be made to the step0 cleaning code per panel!), about 100-200Gb. The datasets we work with, ultimately, are a lot smaller. 

To help link the results produced to the paper, the results directory has two pdf/markdown documents, one describing in more detail what each step2 do file does. The other (called 'results directory') gives the underlying results behind tables and pictures in the paper and online appendix (and often mentions the do-file where these 'result'-files originate). 



The Master_CTV.do processes in order:

i. Aggregate Data

ii.	CPS 

iii. SIPP, step 0 Preparation, this downloads the SIPP data from the NBER and prepares the intermediate dta files for the next two steps. 

We want to acknowledge the shoulders we are standing on: 

This step uses Jean Roth's do and dct files, which (in slightly adapted form) are included under the GNU GPL license. We express our immense thanks for this work. The SIPP raw data, dct-files and do-files (that label variables, etc.) can be downloaded from https://www.nber.org/research/data/survey-income-and-program-participation-sipp

Our default crosswalk for occupational codes is from CPS IPUMS. (Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren and Michael Westberry. Integrated Public Use Microdata Series, Current Population Survey: Version 10.0 [dataset]. Minneapolis, MN: IPUMS, 2022. https://doi.org/10.18128/D030.V10.0)

Some of the occupational recoding also uses David Dorn's crosswalk files (David Autor and David Dorn. "The Growth of Low Skill Service Jobs and the Polarization of the U.S. Labor Market." American Economic Review, 103(5), 1553-1597, 2013.). 

iv. SIPP, step 1: establish the properties of occupational miscoding.

v. SIPP, step 2: calculate the SIPP data moments used and discussed in the paper. This analysis has been split in more digestible chunks (we hope), in 12 steps (with some substeps). Master_CTV.do calls these, and provides some explanation about outputs in each step. 

To correlate labour market outcomes in the SIPP with vacancies, we use Regis Barnichon's Help Wanted Index (e.g. Barnichon, Regis. "Building a composite help-wanted index." Economics Letters 109.3 (2010): 175-178. and Barnichon, Regis, et al. "The ins and outs of forecasting unemployment: Using labor force flows to forecast the labor market." Brookings Papers on Economic Activity (2012): 83-131.)
     

vi. Computational Results Processing: bring the results of the computational models to the paper, possibly in combination/in comparison with the SIPP data 
     
Some of these steps are further covered in the README.txt inside each folder. 

## 3. There are two points where another program is needed to do a calculation


i. Matlab or Mathematica to calculate the de-garbling matrix from the miscoding matrix. For details, see `./Replication/SIPP 1. Miscoding/Readme.txt`

ii. The Tramo/Seats procedure to deal with timeseries with gaps before. We use the TSW+ program for this, which is included in this replication package (freeware, see license file for license info).It should also be available on the Bank of Spain website, but our most recent attempts ran into link rot. We have put an installation file online, https://github.com/CTVproject/Replication/raw/main/TRAMO-SEATS/TSW%2B%20x64%20.msi . 

A detailed description to use TRAMO/SEATS (by using TSW+) in the context of our replication package is provided in the main folder of this replication package. 


The overall running time (on a Dell 7760 mobile workstation) lies in the order of magnitude of a day (a little bit less than it) for the data analysis; around 25 minutes for the computation of the full model, around 10 minutes for the computation of the excess mobility model. Some of the robustness runs are somewhat faster.


-------------------------------
This version January 2023
    Ludo Visschers (ludo.visschers <at> ed.ac.uk; ludovisschers <at> gmail.com)
    Carlos Carrillo-Tudela (cocarr <at> essex.ac.uk)


-----------------------------------------------------
Copyright of all original material in the Replication Package
-----------------------------------------------------


 
 
> Copyright 2023 Ludo Visschers and Carlos Carrillo-Tudela
 
 
> Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

> 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

> 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

> 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

> 4. If using this code or its results (whole or in part, with or without modifications) in academic work, please cite, in the references of the publications associated with aforementioned academic work:  

>        Carrillo-Tudela, Carlos and Ludo Visschers, "Unemployment and Endogenous Reallocation over the Business Cycle", 
>        in its published version


> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
