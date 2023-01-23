
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


/* This program uses Jean Roth's/NBER do-files and dct files in the data 
extraction process. These are distributed under the GNU GPL, and (in only slightly 
modified form) included in this replication package in the ${extractcodedir} 
directory. We gratefully acknowledge the usefulness of these files for our 
research*/


/* NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE!

Sometimes, something goes wrong during downloading files from the www.nber.org 
website, making the do-file crash, without a bug in the do-file. 
In this case, restarting the program  (starting from the panel during which 
the program crashed) should hopefully already solve the issue. Of course, 
definitely do not discard the possibility of a bug! 

NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE!*/

clear
clear mata
cd "${workingdir}"			// !!! set in the global_paths.do, if necessary !!!
global workingdir "`c(pwd)'"			
do "${workingdir}/global_paths.do"
	
version 13
	
set more off
set varabbrev off
	
	
global lstarttime=c(current_time)
global lstartdate=c(current_date)
display "started at ${lstarttime} on ${lstartdate}"


	
	
global codedir "${step0codedir}"



display

*********************************
** !!!! SET GLOBALS CONTROLLING NBER DOWNLOAD AND CLEAN-UP
*********************************

global nber_download_ind=1 // download NBER's zip dat SIPP files, convert to dta 
global fileversion=1022    // version of the {panelyear}total_v$fileversion.dta
global keepwaveraw_dtas=0  // keep dta of each corewave and topical module
global keeptotalraw_dtas=0 // keep YYYYtotal_raw.dta and YYYYtotal_v$fileversion
global keepnberoriginals=0 // keep the zipped files downloaded from the nber
global keeplogs=0		   // remove logs after run is completed w/out break/err		
global tempdircleanup=0	   //  at end of step0, delete all files in tempdata dir   	
			
cd "${tempdata}"


 
****************
*** PROGRAM TO CONVERT DAT into DTA
****************



		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP



****************
**** 1984 ******
****************

cd "${rawdata}"
if ${nber_download_ind}==1 {



****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp84*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}


****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/1984/sipp84fp.zip sipp84fp.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_w1.zip sipp84w1.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_w2.zip sipp84w2.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_t3.zip sipp84rt3.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_t4.zip sipp84rt4.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_t5.zip sipp84rt5.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_w6.zip sipp84w6.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_t7.zip sipp84rt7.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_t8.zip sipp84rt8.zip
cap n copy https://data.nber.org/sipp/1984/sipp84_w9.zip sipp84w9.zip


****** ===> STEP 0.0.2: unzip files

cd "${rawdata}"

global files : dir . files "sipp84*.zip"

display ${files}

capture n erase sipp84w1.dat
capture n erase sipp84w2.dat
capture n erase sipp84rt3.dat
capture n erase sipp84rt4.dat
capture n erase sipp84rt5.dat
capture n erase sipp84w6.dat
capture n erase sipp84rt7.dat
capture n erase sipp84rt8.dat
capture n erase sipp84w9.dat


foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	}



capture shell ren sipp84_t3.dat sipp84rt3.dat
capture shell ren sipp84_t4.dat sipp84rt4.dat
capture shell ren sipp84_t5.dat sipp84rt5.dat
capture shell ren sipp84_t7.dat sipp84rt7.dat
capture shell ren sipp84_t8.dat sipp84rt8.dat

capture shell ren sipp84_w1.dat sipp84w1.dat
capture shell ren sipp84_w2.dat sipp84w2.dat
capture shell ren sipp84_w6.dat sipp84w6.dat
capture shell ren sipp84_w9.dat sipp84w9.dat



		
****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
	
//because the dct/do files for dat->dta conversion have own log	
dat2dta_exe sip84fp 
dat2dta_exe sip84w1 
dat2dta_exe sip84w2 
dat2dta_exe sip84rt3
dat2dta_exe sip84rt4
dat2dta_exe sip84rt5
dat2dta_exe sip84w6
dat2dta_exe sip84rt7
dat2dta_exe sip84rt8
dat2dta_exe sip84w9		

}		
		



****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
cd "$tempdata"
				
clear
clear matrix
				
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"


do "${step0codedir}1984_construction_ctv_step0_1_1.do"



***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/1984_construction_ctv_step0_1_2.do"



***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1984.dta
cap n erase temp1984core.dta
cap n erase temp1984tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	cap n erase sip84fp.log	
	cap n erase sip84rt3.log	
	cap n erase sip84rt4.log	
	cap n erase sip84rt5.log	
	cap n erase sip84rt7.log	
	cap n erase sip84rt8.log	
	cap n erase sip84w1.log	
	cap n erase sip84w2.log	
	cap n erase sip84w6.log	
	cap n erase sip84w9.log	
}

if $keepwaveraw_dtas != 1 {
	
	cap n erase  sip84fp.dta
	cap n erase  sip84w1.dta 
	cap n erase  sip84w2.dta
	cap n erase  sip84rt3.dta
	cap n erase  sip84rt4.dta
	cap n erase  sip84rt5.dta
	cap n erase  sip84w6.dta
	cap n erase  sip84rt7.dta
	cap n erase  sip84rt8.dta
	cap n erase  sip84w9.dta
	
}	

if $keepnberoriginals!= 1 {
	
	cap n erase  sip84fp.dat
	cap n erase  sip84w1.dat 
	cap n erase  sip84w2.dat
	cap n erase  sip84rt3.dat
	cap n erase  sip84rt4.dat
	cap n erase  sip84rt5.dat
	cap n erase  sip84w6.dat
	cap n erase  sip84rt7.dat
	cap n erase  sip84rt8.dat
	cap n erase  sip84w9.dat
	
	cap n erase  sip84fp.zip
	cap n erase  sip84w1.zip 
	cap n erase  sip84w2.zip
	cap n erase  sip84rt3.zip
	cap n erase  sip84rt4.zip
	cap n erase  sip84rt5.zip
	cap n erase  sip84w6.zip
	cap n erase  sip84rt7.zip
	cap n erase  sip84rt8.zip
	cap n erase  sip84w9.zip
	
}






****************
**** 1985 ******
****************

clear matrix
cd "${rawdata}"



if ${nber_download_ind}==1 {


****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp85*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}



****** ===> STEP 0.0.1: download data from NBER
		
	cap n copy https://data.nber.org/sipp/1985/sipp85fp.zip sipp85fp.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_r1.zip sipp85w1.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_r2.zip sipp85w2.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_t3.zip sipp85rt3.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_t4.zip sipp85rt4.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_r5.zip sipp85rt5.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_r6.zip sipp85w6.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_t7.zip sipp85rt7.zip
	cap n copy https://data.nber.org/sipp/1985/sipp85_t8.zip sipp85rt8.zip

****** ===> STEP 0.0.2: unzip files
	cd "${rawdata}"

	global files : dir . files "sipp85*.zip"

	display ${files}

	foreach f of global files {
		display "`f'"
		cap n unzipfile `f', replace
		
	}


	 *** CHECK THAT RENAMING PROCESS IS DONE CORRECTLY WHEN RUNNING THIS
	capture shell ren sipp85_r1.dat sipp85w1.dat
	capture shell ren sipp85_r2.dat sipp85w2.dat
	capture shell ren sipp85_t3.dat sipp85rt3.dat
	capture shell ren sipp85_t4.dat sipp85rt4.dat
	capture shell ren sipp85_r5.dat sipp85w5.dat
	capture shell ren sipp85_r6.dat sipp85w6.dat
	capture shell ren sipp85_t7.dat sipp85rt7.dat
	capture shell ren sipp85_t8.dat sipp85rt8.dat


	 
	global files : dir . files "sipp85*.dat"

	foreach f of global files {
		display "`f'"
		}
	display " ?=? sipp85fp.dat sipp85rt4.dat sipp85w5.dat sipp85rt3.dat"
	display " sipp85rt7.dat sipp85w6.dat sipp85w1.dat sipp85w8.dat sipp85w2.dat"
	 

	 
****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

	dat2dta_exe sip85fp
	dat2dta_exe sip85w1
	dat2dta_exe sip85w2
	dat2dta_exe sip85rt3
	dat2dta_exe sip85rt4
	dat2dta_exe sip85w5
	dat2dta_exe sip85w6
	dat2dta_exe sip85rt7
	dat2dta_exe sip85w8		

}


cd "$tempdata"

****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta

				
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"

clear matrix
do "${step0codedir}/1985_construction_ctv_step0_1_1.do"



***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"

	clear matrix
	cd "${outputdata}"
	do "${step0codedir}/1985_construction_ctv_step0_1_2.do"



***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1985.dta
cap n erase temp1985core.dta
cap n erase temp1985tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	cap n erase sip85fp.log	
	cap n erase sip85rt3.log	
	cap n erase sip85rt4.log	
	cap n erase sip85w5.log	
	cap n erase sip85rt7.log	
	cap n erase sip85rt8.log	
	cap n erase sip85w1.log	
	cap n erase sip85w2.log	
	cap n erase sip85w6.log	
	cap n erase sip85w9.log	
}

if $keepwaveraw_dtas != 1 {
	
	cap n erase  sip85fp.dta
	cap n erase  sip85w1.dta 
	cap n erase  sip85w2.dta
	cap n erase  sip85rt3.dta
	cap n erase  sip85rt4.dta
	cap n erase  sip85rt5.dta
	cap n erase  sip85w6.dta
	cap n erase  sip85rt7.dta
	cap n erase  sip85rt8.dta
	cap n erase  sip85w9.dta
	
}	

if $keepnberoriginals != 1 {
	
	cap n erase  sip85fp.dat
	cap n erase  sip85w1.dat 
	cap n erase  sip85w2.dat
	cap n erase  sip85rt3.dat
	cap n erase  sip85rt4.dat
	cap n erase  sip85rt5.dat
	cap n erase  sip85w6.dat
	cap n erase  sip85rt7.dat
	cap n erase  sip85rt8.dat
	cap n erase  sip85w9.dat
	
	cap n erase  sip85fp.zip
	cap n erase  sip85w1.zip 
	cap n erase  sip85w2.zip
	cap n erase  sip85rt3.zip
	cap n erase  sip85rt4.zip
	cap n erase  sip85rt5.zip
	cap n erase  sip85w6.zip
	cap n erase  sip85rt7.zip
	cap n erase  sip85rt8.zip
	cap n erase  sip85w9.zip
	
}



****************
**** 1986 ******
****************

clear 
clear matrix
cd "${rawdata}"
if ${nber_download_ind}==1 {


****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp86*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}




****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/1986/sipp86fp.zip sipp86fp.zip
cap n copy https://data.nber.org/sipp/1986/sipp86_w1.zip sipp86w1.zip
cap n copy https://data.nber.org/sipp/1986/sipp86_t2.zip sipp86rt2.zip
cap n copy https://data.nber.org/sipp/1986/sipp86_t3.zip sipp86rt3.zip
cap n copy https://data.nber.org/sipp/1986/sipp86_t4.zip sipp86rt4.zip
cap n copy https://data.nber.org/sipp/1986/sipp86_t5.zip sipp86rt5.zip
cap n copy https://data.nber.org/sipp/1986/sipp86_t6.zip sipp86rt6.zip
cap n copy https://data.nber.org/sipp/1986/sipp86_t7.zip sipp86rt7.zip


****** ===> STEP 0.0.2: unzip files

global files : dir . files "sipp86*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

	
cap shell ren sipp86_w1.dat sipp86w1.dat
cap shell ren sipp86_t2.dat sipp86rt2.dat
cap shell ren sipp86_t3.dat sipp86rt3.dat
cap shell ren sipp86_t4.dat sipp86rt4.dat
cap shell ren sipp86_t5.dat sipp86rt5.dat
cap shell ren sipp86_t6.dat sipp86rt6.dat
cap shell ren sipp86_t7.dat sipp86rt7.dat



****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip86fp
dat2dta_exe sip86w1
dat2dta_exe sip86rt2
dat2dta_exe sip86rt3
dat2dta_exe sip86rt4
dat2dta_exe sip86rt5
dat2dta_exe sip86rt6
dat2dta_exe sip86rt7
		
}
cd "$tempdata"

****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta

capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)


do "${step0codedir}1986_construction_ctv_step0_1_1.do"



***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"

	cd "${outputdata}"				

do "${step0codedir}/1986_construction_ctv_step0_1_2.do"



***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1986.dta
cap n erase temp1986core.dta
cap n erase temp1986tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	cap n erase sip86fp.log	
	cap n erase sip86rt3.log	
	cap n erase sip86rt4.log	
	cap n erase sip86rt5.log	
	cap n erase sip86rt7.log	
	cap n erase sip86w1.log	
	cap n erase sip86rt2.log	
	cap n erase sip86rt6.log	
	
}

if $keepwaveraw_dtas != 1 {
	
	cap n erase  sip86fp.dta
	cap n erase  sip86w1.dta 
	cap n erase  sip86rt2.dta
	cap n erase  sip86rt3.dta
	cap n erase  sip86rt4.dta
	cap n erase  sip86rt5.dta
	cap n erase  sip86rt6.dta
	cap n erase  sip86rt7.dta
	
}	

if $keepnberoriginals != 1 {
	
	cap n erase  sip86fp.dat
	cap n erase  sip86w1.dat 
	cap n erase  sip86rt2.dat
	cap n erase  sip86rt3.dat
	cap n erase  sip86rt4.dat
	cap n erase  sip86rt5.dat
	cap n erase  sip86rt6.dat
	cap n erase  sip86rt7.dat
	
	cap n erase  sip86fp.zip
	cap n erase  sip86w1.zip 
	cap n erase  sip86rt2.zip
	cap n erase  sip86rt3.zip
	cap n erase  sip86rt4.zip
	cap n erase  sip86rt5.zip
	cap n erase  sip86rt6.zip
	cap n erase  sip86rt7.zip
	
}



****************
**** 1987 ******
****************
clear
clear matrix
cd "${rawdata}"
if ${nber_download_ind}==1 {

****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp87*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}


****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/1987/sipp87fp.zip sipp87fp.zip
cap n copy https://data.nber.org/sipp/1987/sipp87_r1.zip sipp87w1.zip
cap n copy https://data.nber.org/sipp/1987/sipp87_t2.zip sipp87rt2.zip
cap n copy https://data.nber.org/sipp/1987/sipp87_t3.zip sipp87rt3.zip
cap n copy https://data.nber.org/sipp/1987/sipp87_t4.zip sipp87rt4.zip
cap n copy https://data.nber.org/sipp/1987/sipp87_t5.zip sipp87rt5.zip
cap n copy https://data.nber.org/sipp/1987/sipp87_t6.zip sipp87rt6.zip
cap n copy https://data.nber.org/sipp/1987/sipp87_t7.zip sipp87rt7.zip


****** ===> STEP 0.0.2: unzip files

global files : dir . files "sipp87*.zip"

display "1987 files in directory:"



foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}


	
cap shell ren sipp87_r1.dat sipp87w1.dat
cap shell ren sipp87_t2.dat sipp87rt2.dat
cap shell ren sipp87_t3.dat sipp87rt3.dat
cap shell ren sipp87_t4.dat sipp87rt4.dat
cap shell ren sipp87_t5.dat sipp87rt5.dat
cap shell ren sipp87_t6.dat sipp87rt6.dat
cap shell ren sipp87_t7.dat sipp87rt7.dat



****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip87fp
dat2dta_exe sip87w1
dat2dta_exe sip87rt2
dat2dta_exe sip87rt3
dat2dta_exe sip87rt4
dat2dta_exe sip87rt5
dat2dta_exe sip87rt6
dat2dta_exe sip87rt7
		
}
****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
cd "$tempdata"
				
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"


do "${step0codedir}1987_construction_ctv_step0_1_1.do"


***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"

	cd "${outputdata}"

do "${step0codedir}/1987_construction_ctv_step0_1_2.do"



***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1987.dta
cap n erase temp1987core.dta
cap n erase temp1987tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	cap n erase sip87fp.log	
	cap n erase sip87rt3.log	
	cap n erase sip87rt4.log	
	cap n erase sip87rt5.log	
	cap n erase sip87rt7.log	
	cap n erase sip87w1.log	
	cap n erase sip87rt2.log	
	cap n erase sip87rt6.log	
	
}

if $keepwaveraw_dtas != 1 {
	
	cap n erase  sip87fp.dta
	cap n erase  sip87w1.dta 
	cap n erase  sip87rt2.dta
	cap n erase  sip87rt3.dta
	cap n erase  sip87rt4.dta
	cap n erase  sip87rt5.dta
	cap n erase  sip87rt6.dta
	cap n erase  sip87rt7.dta
	
}	

if $keepnberoriginals != 1 {
	
	cap n erase  sip87fp.dat
	cap n erase  sip87w1.dat 
	cap n erase  sip87rt2.dat
	cap n erase  sip87rt3.dat
	cap n erase  sip87rt4.dat
	cap n erase  sip87rt5.dat
	cap n erase  sip87rt6.dat
	cap n erase  sip87rt7.dat
	
	cap n erase  sip87fp.zip
	cap n erase  sip87w1.zip 
	cap n erase  sip87rt2.zip
	cap n erase  sip87rt3.zip
	cap n erase  sip87rt4.zip
	cap n erase  sip87rt5.zip
	cap n erase  sip87rt6.zip
	cap n erase  sip87rt7.zip
	
}

	

****************
**** 1988 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {



****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp88*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}



****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/1988/sipp88fp.zip sipp88fp.zip
cap n copy https://data.nber.org/sipp/1988/sipp88_r1.zip sipp88w1.zip
cap n copy https://data.nber.org/sipp/1988/sipp88_t2.zip sipp88rt2.zip
cap n copy https://data.nber.org/sipp/1988/sipp88_t3.zip sipp88rt3.zip
cap n copy https://data.nber.org/sipp/1988/sipp88_t4.zip sipp88rt4.zip
cap n copy https://data.nber.org/sipp/1988/sipp88_t5.zip sipp88rt5.zip
cap n copy https://data.nber.org/sipp/1988/sipp88_t6.zip sipp88rt6.zip

****** ===> STEP 0.0.2: unzip files

global files : dir . files "sipp88*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}


cap shell ren sipp88_r1.dat sipp88w1.dat
cap shell ren sipp88_t2.dat sipp88rt2.dat
cap shell ren sipp88_t3.dat sipp88rt3.dat
cap shell ren sipp88_t4.dat sipp88rt4.dat
cap shell ren sipp88_t5.dat sipp88rt5.dat
cap shell ren sipp88_t6.dat sipp88rt6.dat



****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


dat2dta_exe sip88fp
dat2dta_exe sip88w1
dat2dta_exe sip88rt2
dat2dta_exe sip88rt3
dat2dta_exe sip88rt4
dat2dta_exe sip88rt5
dat2dta_exe sip88rt6
		
}
****** ===> STEP 0.1.1: first selection, putting all waves together
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)

display "-------step 0.0.1--------"
do "${step0codedir}1988_construction_ctv_step0_1_1.do"


***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"

do "${step0codedir}/1988_construction_ctv_step0_1_2.do"



***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1988.dta
cap n erase temp1988core.dta
cap n erase temp1988tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	cap n erase sip88fp.log	
	cap n erase sip88rt3.log	
	cap n erase sip88rt4.log	
	cap n erase sip88rt5.log	
	cap n erase sip88w1.log	
	cap n erase sip88rt2.log	
	cap n erase sip88rt6.log	
	
}

if $keepwaveraw_dtas != 1 {
	
	cap n erase  sip88fp.dta
	cap n erase  sip88w1.dta 
	cap n erase  sip88rt2.dta
	cap n erase  sip88rt3.dta
	cap n erase  sip88rt4.dta
	cap n erase  sip88rt5.dta
	cap n erase  sip88rt6.dta
	
}	

if $keepnberoriginals != 1 {
	
	cap n erase  sip88fp.dat
	cap n erase  sip88w1.dat 
	cap n erase  sip88rt2.dat
	cap n erase  sip88rt3.dat
	cap n erase  sip88rt4.dat
	cap n erase  sip88rt5.dat
	cap n erase  sip88rt6.dat
	
	cap n erase  sip88fp.zip
	cap n erase  sip88w1.zip 
	cap n erase  sip88rt2.zip
	cap n erase  sip88rt3.zip
	cap n erase  sip88rt4.zip
	cap n erase  sip88rt5.zip
	cap n erase  sip88rt6.zip
	
}

	


****************
**** 1990 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {


****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp90*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}

	cap n erase "sipp_revised_jobid_file_1990.zip"
	cap n erase "s90byod.zip"
	cap n erase "s90w1.zip"
	cap n erase "s90w2.zip"
	cap n erase "s90w3.zip"
	cap n erase "s90w4.zip" 
	cap n erase "s90w2tm.zip"
	cap n erase "s90w3tm.zip"
	cap n erase "s90w4tm.zip"
	
****** ===> STEP 0.0.1: download data from NBER
	
	/*  https://data.nber.org/sipp/1990/
		appears to have differently named files that cover
		the same wave core/TM data. Here we follow the website 
		links available on 
		www.nber.org/research/data/survey-income-and-program-participation-sipp
	*/
	
cap n copy https://data.nber.org/sipp/1990/sipp90fp.zip sipp90fp.zip

cap n copy https://data.nber.org/sipp/1990/s90w1.zip s90w1.zip
cap n copy https://data.nber.org/sipp/1990/s90w2.zip s90w2.zip
cap n copy https://data.nber.org/sipp/1990/s90w3.zip s90w3.zip
cap n copy https://data.nber.org/sipp/1990/s90w4.zip s90w4.zip

cap n copy https://data.nber.org/sipp/1990/s90w2tm.zip s90w2tm.zip
cap n copy https://data.nber.org/sipp/1990/s90w3tm.zip s90w3tm.zip
cap n copy https://data.nber.org/sipp/1990/s90w4tm.zip s90w4tm.zip

cap n copy https://data.nber.org/sipp/1990/sipp90w5.zip sipp90w5.zip
cap n copy https://data.nber.org/sipp/1990/sipp90w6.zip sipp90w6.zip
cap n copy https://data.nber.org/sipp/1990/sipp90w7.zip sipp90w7.zip
cap n copy https://data.nber.org/sipp/1990/sipp90w8.zip sipp90w8.zip

cap n copy https://data.nber.org/sipp/1990/sipp90t5.zip sipp90t5.zip
cap n copy https://data.nber.org/sipp/1990/sipp90t6.zip sipp90t6.zip
cap n copy https://data.nber.org/sipp/1990/sipp90t7.zip sipp90t7.zip
cap n copy https://data.nber.org/sipp/1990/sipp90t8.zip sipp90t8.zip

cap n copy https://data.nber.org/sipp/1990/s90byod.zip s90byod.zip
cap n copy https://data.nber.org/sipp/1990/sipp_revised_jobid_file_1990.zip sipp_revised_jobid_file_1990.zip

cap n copy https://data.nber.org/sipp/1990/sipp90r5.zip sipp90r5.zip
cap n copy https://data.nber.org/sipp/1990/sipp90r8.zip sipp90r8.zip

****** ===> STEP 0.0.2: unzip files

global files : dir . files "sipp90*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

cap n unzipfile sipp_revised_jobid_file_1990.zip , replace
cap n unzipfile s90w1.zip , replace
cap n unzipfile s90w2.zip , replace
cap n unzipfile s90w3.zip , replace
cap n unzipfile s90w4.zip , replace
cap n unzipfile s90w4tm.zip , replace
cap n unzipfile s90w3tm.zip , replace
cap n unzipfile s90w2tm.zip , replace
cap n unzipfile s90byod.zip , replace

cap shell ren sipp_revised_jobid_file_1990.dat sipp90jid.dat




****************
**** s90w ******
****************


global files : dir . files "s90w*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

cap shell ren s90w1 sipp90w1.dat
cap shell ren s90w2 sipp90w2.dat
cap shell ren s90w3 sipp90w3.dat
cap shell ren s90w4 sipp90w4.dat
cap shell ren s90w2tm sipp90t2.dat
cap shell ren s90w3tm sipp90t3.dat
cap shell ren s90w4tm sipp90t4.dat
cap shell ren s90byod sipp90db.dat





****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip90fp
dat2dta_exe sip90jid
dat2dta_exe sip90db

dat2dta_exe sip90w1
dat2dta_exe sip90w2
dat2dta_exe sip90w3
dat2dta_exe sip90w4
dat2dta_exe sip90w5
dat2dta_exe sip90w6
dat2dta_exe sip90w7
dat2dta_exe sip90w8

dat2dta_exe sip90t2
dat2dta_exe sip90t3
dat2dta_exe sip90t4
dat2dta_exe sip90t5
dat2dta_exe sip90t6
dat2dta_exe sip90t7
dat2dta_exe sip90t8

dat2dta_exe sip90r5
dat2dta_exe sip90r8

}
****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
cd "$tempdata"
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.1.1--------"
do "${step0codedir}1990_construction_ctv_step0_1_1.do"




***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/1990_construction_ctv_step0_1_2.do"





***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1990.dta
cap n erase temp1990core.dta
cap n erase temp1990tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	
	global files : dir . files "sip90*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sip90*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp90*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "sipp90*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	display "erasing the other s90 zip files"
	cap n erase sipp_revised_jobid_file_1990.zip  
	cap n erase s90w1.zip  
	cap n erase s90w2.zip  
	cap n erase s90w3.zip  
	cap n erase s90w4.zip  
	cap n erase s90w4tm.zip  
	cap n erase s90w3tm.zip  
	cap n erase s90w2tm.zip  
	cap n erase s90byod.zip  
}




****************
**** 1991 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {


****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp91*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}

****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/1991/sipp91fp.zip sipp91fp.zip

cap n copy https://data.nber.org/sipp/1991/sipp91w1.zip sipp91w1.zip
cap n copy https://data.nber.org/sipp/1991/sipp91w2.zip sipp91w2.zip
cap n copy https://data.nber.org/sipp/1991/sipp91w3.zip sipp91w3.zip
cap n copy https://data.nber.org/sipp/1991/sipp91w4.zip sipp91w4.zip
cap n copy https://data.nber.org/sipp/1991/sipp91w5.zip sipp91w5.zip
cap n copy https://data.nber.org/sipp/1991/sipp91w6.zip sipp91w6.zip
cap n copy https://data.nber.org/sipp/1991/sipp91w7.zip sipp91w7.zip
cap n copy https://data.nber.org/sipp/1991/sipp91w8.zip sipp91w8.zip

cap n copy https://data.nber.org/sipp/1991/sipp91t2.zip sipp91t2.zip
cap n copy https://data.nber.org/sipp/1991/sipp91t3.zip sipp91t3.zip
cap n copy https://data.nber.org/sipp/1991/sipp91t4.zip sipp91t4.zip
cap n copy https://data.nber.org/sipp/1991/sipp91t5.zip sipp91t5.zip
cap n copy https://data.nber.org/sipp/1991/sipp91t6.zip sipp91t6.zip
cap n copy https://data.nber.org/sipp/1991/sipp91t7.zip sipp91t7.zip
cap n copy https://data.nber.org/sipp/1991/sipp91t8.zip sipp91t8.zip

cap n copy https://data.nber.org/sipp/1991/sipp91r5.zip sipp91r5.zip
cap n copy https://data.nber.org/sipp/1991/sipp91r8.zip sipp91r8.zip

cap n copy https://data.nber.org/sipp/1991/sipp91jid.zip sipp91jid.zip
cap n copy https://data.nber.org/sipp/1991/sipp91db.zip sipp91db.zip


****** ===> STEP 0.0.2: unzip files

global files : dir . files "sipp91*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}


****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip91fp
dat2dta_exe sip91jid
dat2dta_exe sip91db

dat2dta_exe sip91w1
dat2dta_exe sip91w2
dat2dta_exe sip91w3
dat2dta_exe sip91w4
dat2dta_exe sip91w5
dat2dta_exe sip91w6
dat2dta_exe sip91w7
dat2dta_exe sip91w8

dat2dta_exe sip91t2
dat2dta_exe sip91t3
dat2dta_exe sip91t4
dat2dta_exe sip91t5
dat2dta_exe sip91t6
dat2dta_exe sip91t7
dat2dta_exe sip91t8

dat2dta_exe sip91r5
dat2dta_exe sip91r8
}

				
****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
cd "$tempdata"
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"
do "${step0codedir}1991_construction_ctv_step0_1_1.do"


***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/1991_construction_ctv_step0_1_2.do"




***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1991.dta
cap n erase temp1991core.dta
cap n erase temp1991tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	
	global files : dir . files "sip91*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sip91*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp91*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "sipp91*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	
}




****************
**** 1992 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {




****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp92*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}

****** ===> STEP 0.0.1: download data from NBER

cap n copy https://data.nber.org/sipp/1992/sipp92fp.zip sipp92fp.zip

cap n copy https://data.nber.org/sipp/1992/sipp92w1.zip sipp92w1.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w2.zip sipp92w2.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w3.zip sipp92w3.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w4.zip sipp92w4.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w5.zip sipp92w5.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w6.zip sipp92w6.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w7.zip sipp92w7.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w8.zip sipp92w8.zip
cap n copy https://data.nber.org/sipp/1992/sipp92w9.zip sipp92w9.zip

cap n copy https://data.nber.org/sipp/1992/sipp92t1.zip sipp92t1.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t2.zip sipp92t2.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t3.zip sipp92t3.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t4.zip sipp92t4.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t5.zip sipp92t5.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t6.zip sipp92t6.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t7.zip sipp92t7.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t8.zip sipp92t8.zip
cap n copy https://data.nber.org/sipp/1992/sipp92t9.zip sipp92t9.zip

cap n copy https://data.nber.org/sipp/1992/sipp92r5.zip sipp92r5.zip
cap n copy https://data.nber.org/sipp/1992/sipp92r8.zip sipp92r8.zip

cap n copy https://data.nber.org/sipp/1992/sipp92jid.zip sipp92jid.zip


****** ===> STEP 0.0.2: unzip files

global files : dir . files "sipp92*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}




****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip92fp
dat2dta_exe sip92jid
	*dat2dta_exe sip92db

dat2dta_exe sip92w1
dat2dta_exe sip92w2
dat2dta_exe sip92w3
dat2dta_exe sip92w4
dat2dta_exe sip92w5
dat2dta_exe sip92w6
dat2dta_exe sip92w7
dat2dta_exe sip92w8
dat2dta_exe sip92w9

dat2dta_exe sip92t1
dat2dta_exe sip92t2
dat2dta_exe sip92t3
dat2dta_exe sip92t4
dat2dta_exe sip92t5
dat2dta_exe sip92t6
dat2dta_exe sip92t7
dat2dta_exe sip92t8
dat2dta_exe sip92t9

dat2dta_exe sip92r5
dat2dta_exe sip92r8

}
****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
cd "$tempdata"
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"


do "${step0codedir}1992_construction_ctv_step0_1_1.do"


***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/1992_construction_ctv_step0_1_2.do"




***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1992.dta
cap n erase temp1992core.dta
cap n erase temp1992tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	
	global files : dir . files "sip92*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sip92*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp92*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "sipp92*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	
}





****************
**** 1993 ******
****************

clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {



****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp93*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}


****** ===> STEP 0.0.1: download data from NBER


cap n copy https://data.nber.org/sipp/1993/sipp93fp.zip sipp93fp.zip

cap n copy https://data.nber.org/sipp/1993/sipp93w1.zip sipp93w1.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w2.zip sipp93w2.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w3.zip sipp93w3.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w4.zip sipp93w4.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w5.zip sipp93w5.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w6.zip sipp93w6.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w7.zip sipp93w7.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w8.zip sipp93w8.zip
cap n copy https://data.nber.org/sipp/1993/sipp93w9.zip sipp93w9.zip

cap n copy https://data.nber.org/sipp/1993/sipp93t1.zip sipp93t1.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t2.zip sipp93t2.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t3.zip sipp93t3.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t4.zip sipp93t4.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t5.zip sipp93t5.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t6.zip sipp93t6.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t7.zip sipp93t7.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t8.zip sipp93t8.zip
cap n copy https://data.nber.org/sipp/1993/sipp93t9.zip sipp93t9.zip

cap n copy https://data.nber.org/sipp/1993/sipp93r5.zip sipp93r5.zip
cap n copy https://data.nber.org/sipp/1993/sipp93r8.zip sipp93r8.zip

cap n copy https://data.nber.org/sipp/1993/sipp93jid.zip sipp93jid.zip



****** ===> STEP 0.0.2: unzip files


global files : dir . files "sipp93*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}


****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip93fp
dat2dta_exe sip93jid
	*dat2dta_exe sip93db

dat2dta_exe sip93w1
dat2dta_exe sip93w2
dat2dta_exe sip93w3
dat2dta_exe sip93w4
dat2dta_exe sip93w5
dat2dta_exe sip93w6
dat2dta_exe sip93w7
dat2dta_exe sip93w8
dat2dta_exe sip93w9

dat2dta_exe sip93t1
dat2dta_exe sip93t2
dat2dta_exe sip93t3
dat2dta_exe sip93t4
dat2dta_exe sip93t5
dat2dta_exe sip93t6
dat2dta_exe sip93t7
dat2dta_exe sip93t8
dat2dta_exe sip93t9

dat2dta_exe sip93r5
dat2dta_exe sip93r8

}


****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
		
cd "$tempdata"
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"

do "${step0codedir}1993_construction_ctv_step0_1_1.do"




***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/1993_construction_ctv_step0_1_2.do"





***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"
cap n erase temp1993.dta
cap n erase temp1993core.dta
cap n erase temp1993tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	
	global files : dir . files "sip93*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sip93*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp93*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "sipp93*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	
}




****************
**** 1996 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {



****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"

	global files : dir . files "sipp96*.zip"

	display ${files}

	foreach f of global files {
		display "erasing `f'"
		cap n erase `f'
		
	}

	

	*longitudinal weights
cap n erase ctl_fer.zip
	*replic weights
cap n erase lrw96cy1.zip
cap n erase lrw96cy2.zip
cap n erase lrw96cy3.zip
cap n erase lrw96cy4.zip
cap n erase lrw96pnl.zip


****** ===> STEP 0.0.1: download data from NBER
	


cap n copy https://data.nber.org/sipp/1996/sipp96l1.zip sipp96l1.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l2.zip sipp96l2.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l3.zip sipp96l3.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l4.zip sipp96l4.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l5.zip sipp96l5.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l6.zip sipp96l6.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l7.zip sipp96l7.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l8.zip sipp96l8.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l9.zip sipp96l9.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l10.zip sipp96l10.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l11.zip sipp96l11.zip
cap n copy https://data.nber.org/sipp/1996/sipp96l12.zip sipp96l12.zip

cap n copy https://data.nber.org/sipp/1996/sipp96t1x.zip sipp96t1x.zip

cap n copy https://data.nber.org/sipp/1996/sipp96t1.zip sipp96t1.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t2.zip sipp96t2.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t3.zip sipp96t3.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t4.zip sipp96t4.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t5.zip sipp96t5.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t6.zip sipp96t6.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t7.zip sipp96t7.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t8.zip sipp96t8.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t9.zip sipp96t9.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t10.zip sipp96t10.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t11.zip sipp96t11.zip
cap n copy https://data.nber.org/sipp/1996/sipp96t12.zip sipp96t12.zip


	*longitudinal weights
cap n copy https://data.nber.org/sipp/1996/ctl_fer.zip ctl_fer.zip
	*replic weights
cap n copy https://data.nber.org/sipp/1996/lrw96cy1.zip lrw96cy1.zip
cap n copy https://data.nber.org/sipp/1996/lrw96cy2.zip lrw96cy2.zip
cap n copy https://data.nber.org/sipp/1996/lrw96cy3.zip lrw96cy3.zip
cap n copy https://data.nber.org/sipp/1996/lrw96cy4.zip lrw96cy4.zip
cap n copy https://data.nber.org/sipp/1996/lrw96pnl.zip lrw96pnl.zip

****** ===> STEP 0.0.2: unzip files

global files : dir . files "sipp96*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}


global files : dir . files "lrw96*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

cap n unzipfile ctl_fer.zip , replace




****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip96l1
dat2dta_exe sip96l2
dat2dta_exe sip96l3
dat2dta_exe sip96l4
dat2dta_exe sip96l5
dat2dta_exe sip96l6
dat2dta_exe sip96l7
dat2dta_exe sip96l8
dat2dta_exe sip96l9
dat2dta_exe sip96l10
dat2dta_exe sip96l11
dat2dta_exe sip96l12


dat2dta_exe sip96t1
dat2dta_exe sip96t2
dat2dta_exe sip96t3
dat2dta_exe sip96t4
dat2dta_exe sip96t5
dat2dta_exe sip96t6
dat2dta_exe sip96t7
dat2dta_exe sip96t8
dat2dta_exe sip96t9
dat2dta_exe sip96t10
dat2dta_exe sip96t11
dat2dta_exe sip96t12

dat2dta_exe sip96t1x

dat2dta_exe sipp1996ctl_fer
dat2dta_exe sipp1996lrw96cy1
dat2dta_exe sipp1996lrw96cy2
dat2dta_exe sipp1996lrw96cy3
dat2dta_exe sipp1996lrw96cy4
dat2dta_exe sipp1996lrw96pnl

}

****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta

cd "$tempdata"
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"

do "${step0codedir}1996_construction_ctv_step0_1_1.do"



***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/1996_construction_ctv_step0_1_2.do"



***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"		
cap n erase temp1996.dta
cap n erase temp1996core.dta
cap n erase temp1996tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	
	global files : dir . files "sip96*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sip96*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp96*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "sipp96*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}

	
		*longitudinal weights
	cap n erase ctl_fer.zip
		*replic weights
	cap n erase lrw96cy1.zip
	cap n erase lrw96cy2.zip
	cap n erase lrw96cy3.zip
	cap n erase lrw96cy4.zip
	cap n erase lrw96pnl.zip


	
}


****************
**** 2001 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {



****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"
global files : dir . files "l01puw*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "p01putm*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	

cap n erase sipp01lw9.zip

****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/2001/l01puw1.zip l01puw1.zip
cap n copy https://data.nber.org/sipp/2001/l01puw2.zip l01puw2.zip
cap n copy https://data.nber.org/sipp/2001/l01puw3.zip l01puw3.zip
cap n copy https://data.nber.org/sipp/2001/l01puw4.zip l01puw4.zip
cap n copy https://data.nber.org/sipp/2001/l01puw5.zip l01puw5.zip
cap n copy https://data.nber.org/sipp/2001/l01puw6.zip l01puw6.zip
cap n copy https://data.nber.org/sipp/2001/l01puw7.zip l01puw7.zip
cap n copy https://data.nber.org/sipp/2001/l01puw8.zip l01puw8.zip
cap n copy https://data.nber.org/sipp/2001/l01puw9.zip l01puw9.zip


cap n copy https://data.nber.org/sipp/2001/p01putm1.zip p01putm1.zip
cap n copy https://data.nber.org/sipp/2001/p01putm2.zip p01putm2.zip
cap n copy https://data.nber.org/sipp/2001/p01putm3.zip p01putm3.zip
cap n copy https://data.nber.org/sipp/2001/p01putm4.zip p01putm4.zip
cap n copy https://data.nber.org/sipp/2001/p01putm5.zip p01putm5.zip
cap n copy https://data.nber.org/sipp/2001/p01putm6.zip p01putm6.zip
cap n copy https://data.nber.org/sipp/2001/p01putm7.zip p01putm7.zip
cap n copy https://data.nber.org/sipp/2001/p01putm8.zip p01putm8.zip
cap n copy https://data.nber.org/sipp/2001/p01putm8x.zip p01putm8x.zip
cap n copy https://data.nber.org/sipp/2001/p01putm9.zip p01putm9.zip

cap n copy https://data.nber.org/sipp/2001/sipp01lw9.zip sipp01lw9.zip

****** ===> STEP 0.0.2: unzip files

global files : dir . files "p01*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

global files : dir . files "l01*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

cap n unzipfile sipp01lw9.zip , replace



** big renaming
cap n shell del sipp01w*.dat
cap n shell del sipp01t*.dat

cap n shell ren l01puw1.dat sipp01w1.dat
cap n shell ren l01puw2.dat sipp01w2.dat
cap n shell ren l01puw3.dat sipp01w3.dat
cap n shell ren l01puw4.dat sipp01w4.dat
cap n shell ren l01puw5.dat sipp01w5.dat
cap n shell ren l01puw6.dat sipp01w6.dat
cap n shell ren l01puw7.dat sipp01w7.dat
cap n shell ren l01puw8.dat sipp01w8.dat
cap n shell ren l01puw9.dat sipp01w9.dat

cap n shell ren p01putm1.dat sipp01t1.dat
cap n shell ren p01putm2.dat sipp01t2.dat
cap n shell ren p01putm3.dat sipp01t3.dat
cap n shell ren p01putm4.dat sipp01t4.dat
cap n shell ren p01putm5.dat sipp01t5.dat
cap n shell ren p01putm6.dat sipp01t6.dat
cap n shell ren p01putm7.dat sipp01t7.dat
cap n shell ren p01putm8.dat sipp01t8.dat
cap n shell ren p01putm8x.dat sipp01t8x.dat
cap n shell ren p01putm9.dat sipp01t9.dat




****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sip01w1
dat2dta_exe sip01w2
dat2dta_exe sip01w3
dat2dta_exe sip01w4
dat2dta_exe sip01w5
dat2dta_exe sip01w6
dat2dta_exe sip01w7
dat2dta_exe sip01w8
dat2dta_exe sip01w9


dat2dta_exe sip01t1
dat2dta_exe sip01t2
dat2dta_exe sip01t3
dat2dta_exe sip01t4
dat2dta_exe sip01t5
dat2dta_exe sip01t6
dat2dta_exe sip01t7
dat2dta_exe sip01t8
dat2dta_exe sip01t9


dat2dta_exe sip01t8x
dat2dta_exe sip01lw9

}
****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
cd "$tempdata"
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"

do "${step0codedir}2001_construction_ctv_step0_1_1.do"

***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/2001_construction_ctv_step0_1_2.do"




***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"		
cap n erase temp2001.dta
cap n erase temp2001core.dta
cap n erase temp2001tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta


if $keeplogs != 1 {
	
	global files : dir . files "sip01*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sip01*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp01*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "l01puw*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "p01putm*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	cap n erase sipp01lw9.zip

}




****************
**** 2004 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {


****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"
global files : dir . files "l04puw*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "p04putm*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	


cap n erase sipp04pw1.zip
cap n erase sipp04pt1.zip
cap n erase lgtwgt2004w12.zip


****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/2004/l04puw1.zip l04puw1.zip
cap n copy https://data.nber.org/sipp/2004/l04puw2.zip l04puw2.zip
cap n copy https://data.nber.org/sipp/2004/l04puw3.zip l04puw3.zip
cap n copy https://data.nber.org/sipp/2004/l04puw4.zip l04puw4.zip
cap n copy https://data.nber.org/sipp/2004/l04puw5.zip l04puw5.zip
cap n copy https://data.nber.org/sipp/2004/l04puw6.zip l04puw6.zip
cap n copy https://data.nber.org/sipp/2004/l04puw7.zip l04puw7.zip
cap n copy https://data.nber.org/sipp/2004/l04puw8.zip l04puw8.zip
cap n copy https://data.nber.org/sipp/2004/l04puw9.zip l04puw9.zip
cap n copy https://data.nber.org/sipp/2004/l04puw10.zip l04puw10.zip
cap n copy https://data.nber.org/sipp/2004/l04puw11.zip l04puw11.zip
cap n copy https://data.nber.org/sipp/2004/l04puw12.zip l04puw12.zip

cap n copy https://data.nber.org/sipp/2004/p04putm1.zip p04putm1.zip
cap n copy https://data.nber.org/sipp/2004/p04putm2.zip p04putm2.zip
cap n copy https://data.nber.org/sipp/2004/p04putm3.zip p04putm3.zip
cap n copy https://data.nber.org/sipp/2004/p04putm4.zip p04putm4.zip
cap n copy https://data.nber.org/sipp/2004/p04putm5.zip p04putm5.zip
cap n copy https://data.nber.org/sipp/2004/p04putm6.zip p04putm6.zip
cap n copy https://data.nber.org/sipp/2004/p04putm7.zip p04putm7.zip
cap n copy https://data.nber.org/sipp/2004/p04putm8.zip p04putm8.zip

cap n copy https://data.nber.org/sipp/2004/sipp04pw1.zip sipp04pw1.zip
cap n copy https://data.nber.org/sipp/2004/sipp04pt1.zip sipp04pt1.zip
cap n copy https://data.nber.org/sipp/2004/lgtwgt2004w12.zip lgtwgt2004w12.zip


****** ===> STEP 0.0.2: unzip files

global files : dir . files "p04*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

global files : dir . files "l04*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

*preliminary wave 1 files
cap n unzipfile sipp04pt1.zip , replace
cap n unzipfile sipp04pw1.zip , replace
* longitudinal panel weights
cap n unzipfile lgtwgt2004w12.zip , replace


*preliminary wave 1 
cap n shell ren sipp04t1.dat sipp04pt1.dat
cap n shell ren sipp04w1.dat sipp04pw1.dat


****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

dat2dta_exe sippl04puw1
dat2dta_exe sippl04puw2
dat2dta_exe sippl04puw3
dat2dta_exe sippl04puw4
dat2dta_exe sippl04puw5
dat2dta_exe sippl04puw6
dat2dta_exe sippl04puw7
dat2dta_exe sippl04puw8
dat2dta_exe sippl04puw9
dat2dta_exe sippl04puw10
dat2dta_exe sippl04puw11
dat2dta_exe sippl04puw12


cd "$tempdata"

cap n shell del sipp04w*.dta

cap n shell ren sippl04puw1.dta sipp04w1.dta
cap n shell ren sippl04puw2.dta sipp04w2.dta
cap n shell ren sippl04puw3.dta sipp04w3.dta
cap n shell ren sippl04puw4.dta sipp04w4.dta
cap n shell ren sippl04puw5.dta sipp04w5.dta
cap n shell ren sippl04puw6.dta sipp04w6.dta
cap n shell ren sippl04puw7.dta sipp04w7.dta
cap n shell ren sippl04puw8.dta sipp04w8.dta
cap n shell ren sippl04puw9.dta sipp04w9.dta
cap n shell ren sippl04puw10.dta sipp04w10.dta
cap n shell ren sippl04puw11.dta sipp04w11.dta
cap n shell ren sippl04puw12.dta sipp04w12.dta

dat2dta_exe sippp04putm1
dat2dta_exe sippp04putm2
dat2dta_exe sippp04putm3
dat2dta_exe sippp04putm4
dat2dta_exe sippp04putm5
dat2dta_exe sippp04putm6
dat2dta_exe sippp04putm7
dat2dta_exe sippp04putm8


cap n shell del sipp04tm*.dta

cap n shell ren sippp04putm1.dta sipp04tm1.dta
cap n shell ren sippp04putm2.dta sipp04tm2.dta
cap n shell ren sippp04putm3.dta sipp04tm3.dta
cap n shell ren sippp04putm4.dta sipp04tm4.dta
cap n shell ren sippp04putm5.dta sipp04tm5.dta
cap n shell ren sippp04putm6.dta sipp04tm6.dta
cap n shell ren sippp04putm7.dta sipp04tm7.dta
cap n shell ren sippp04putm8.dta sipp04tm8.dta


* longitudinal weights
dat2dta_exe sipplgtwgt2004w12
cap n shell copy sipplgtwgt2004w12.dta sipp04lgtwgt.dta

* preliminary wave 1, not used further
//cap n dat2dta_exe sip04pw1
//cap n dat2dta_exe sip04pt1


}


****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
cd "$tempdata"
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"


do "${step0codedir}2004_construction_ctv_step0_1_1.do"


***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/2004_construction_ctv_step0_1_2.do"

		


***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"		
cap n erase temp2004.dta
cap n erase temp2004core.dta
cap n erase temp2004tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	
	global files : dir . files "sip04*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sipp04*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp04*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	
	global files : dir . files "l04puw*.zip"

		foreach f of global files {
				display "erasing `f'"
				cap n erase `f'
		}
		
		global files : dir . files "p04putm*.zip"

		foreach f of global files {
				display "erasing `f'"
				cap n erase `f'
		}
		


	cap n erase sipp04pw1.zip
	cap n erase sipp04pt1.zip
	cap n erase lgtwgt2004w12.zip

	
}




****************
**** 2008 ******
****************
clear
clear matrix

cd "${rawdata}"
if ${nber_download_ind}==1 {


****** ===> STEP 0.0.0.1: erasing earlier files
	cd "${rawdata}"
global files : dir . files "l08puw*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "p08putm*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	

	cap n erase lgtwgt2008w16.zip

****** ===> STEP 0.0.1: download data from NBER
	
cap n copy https://data.nber.org/sipp/2008/l08puw1.zip l08puw1.zip
cap n copy https://data.nber.org/sipp/2008/l08puw2.zip l08puw2.zip
cap n copy https://data.nber.org/sipp/2008/l08puw3.zip l08puw3.zip
cap n copy https://data.nber.org/sipp/2008/l08puw4.zip l08puw4.zip
cap n copy https://data.nber.org/sipp/2008/l08puw5.zip l08puw5.zip
cap n copy https://data.nber.org/sipp/2008/l08puw6.zip l08puw6.zip
cap n copy https://data.nber.org/sipp/2008/l08puw7.zip l08puw7.zip
cap n copy https://data.nber.org/sipp/2008/l08puw8.zip l08puw8.zip
cap n copy https://data.nber.org/sipp/2008/l08puw9.zip l08puw9.zip
cap n copy https://data.nber.org/sipp/2008/l08puw10.zip l08puw10.zip
cap n copy https://data.nber.org/sipp/2008/l08puw11.zip l08puw11.zip
cap n copy https://data.nber.org/sipp/2008/l08puw12.zip l08puw12.zip
cap n copy https://data.nber.org/sipp/2008/l08puw13.zip l08puw13.zip
cap n copy https://data.nber.org/sipp/2008/l08puw14.zip l08puw14.zip
cap n copy https://data.nber.org/sipp/2008/l08puw15.zip l08puw15.zip
cap n copy https://data.nber.org/sipp/2008/l08puw16.zip l08puw16.zip

cap n copy https://data.nber.org/sipp/2008/p08putm1.zip p08putm1.zip
cap n copy https://data.nber.org/sipp/2008/p08putm2.zip p08putm2.zip
cap n copy https://data.nber.org/sipp/2008/p08putm3.zip p08putm3.zip
cap n copy https://data.nber.org/sipp/2008/p08putm4.zip p08putm4.zip
cap n copy https://data.nber.org/sipp/2008/p08putm5.zip p08putm5.zip
cap n copy https://data.nber.org/sipp/2008/p08putm6.zip p08putm6.zip
cap n copy https://data.nber.org/sipp/2008/p08putm7.zip p08putm7.zip
cap n copy https://data.nber.org/sipp/2008/p08putm8.zip p08putm8.zip
cap n copy https://data.nber.org/sipp/2008/p08putm9.zip p08putm9.zip
cap n copy https://data.nber.org/sipp/2008/p08putm10.zip p08putm10.zip
cap n copy https://data.nber.org/sipp/2008/p08putm11.zip p08putm11.zip
cap n copy https://data.nber.org/sipp/2008/p08putm13.zip p08putm13.zip

cap n copy https://data.nber.org/sipp/2008/lgtwgt2008w16.zip lgtwgt2008w16.zip


****** ===> STEP 0.0.2: unzip files


global files : dir . files "p08*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

global files : dir . files "l08*.zip"

display ${files}

foreach f of global files {
	display "`f'"
	cap n unzipfile `f', replace
	
}

	/*
	* REPLICATE WEIGHT FILES SKIPPED FOR NOW *
	
	global files : dir . files "rw08*.zip"

	display ${files}

	foreach f of global files {
		display "`f'"
		cap n unzipfile `f', replace
		
	}
	*/
cap n unzipfile lgtwgt2008w16.zip , replace




****** ===> STEP 0.1.0: convert dat files into dta files

		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop dat2dta_exe
		program define dat2dta_exe
				args dofilename filename
			cap n log close _all
			cap n label drop _all
			
			
			cd "${rawdata}"
			
			display "do-file name = `dofilename' "
			
			if "`filename'"=="" {
				local filename "`dofilename'"
			}
			
			capture noisily {
				quietly do "${extractcodedir}/`dofilename'.do"
				
				}
			if _rc!=0 {
				display "error running `dofilename'.do, now running nonstop"
				quietly do "${extractcodedir}/`dofilename'.do", nostop
			}
			
			display "saving ${tempdata}/`filename'.dta"
			save "${rawdata}/`filename'.dta", replace
		log close

		end
		//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


dat2dta_exe sippl08puw1
dat2dta_exe sippl08puw2
dat2dta_exe sippl08puw3
dat2dta_exe sippl08puw4
dat2dta_exe sippl08puw5
dat2dta_exe sippl08puw6
dat2dta_exe sippl08puw7
dat2dta_exe sippl08puw8
dat2dta_exe sippl08puw9
dat2dta_exe sippl08puw10
dat2dta_exe sippl08puw11
dat2dta_exe sippl08puw12
dat2dta_exe sippl08puw13
dat2dta_exe sippl08puw14
dat2dta_exe sippl08puw15
dat2dta_exe sippl08puw16


cd "$tempdata"
cap n shell del sipp08w*.dta

cap n shell ren sippl08puw1.dta sipp08w1.dta
cap n shell ren sippl08puw2.dta sipp08w2.dta
cap n shell ren sippl08puw3.dta sipp08w3.dta
cap n shell ren sippl08puw4.dta sipp08w4.dta
cap n shell ren sippl08puw5.dta sipp08w5.dta
cap n shell ren sippl08puw6.dta sipp08w6.dta
cap n shell ren sippl08puw7.dta sipp08w7.dta
cap n shell ren sippl08puw8.dta sipp08w8.dta
cap n shell ren sippl08puw9.dta sipp08w9.dta
cap n shell ren sippl08puw10.dta sipp08w10.dta
cap n shell ren sippl08puw11.dta sipp08w11.dta
cap n shell ren sippl08puw12.dta sipp08w12.dta
cap n shell ren sippl08puw13.dta sipp08w13.dta
cap n shell ren sippl08puw14.dta sipp08w14.dta
cap n shell ren sippl08puw15.dta sipp08w15.dta
cap n shell ren sippl08puw16.dta sipp08w16.dta


dat2dta_exe sippp08putm1
dat2dta_exe sippp08putm2
dat2dta_exe sippp08putm3
dat2dta_exe sippp08putm4
dat2dta_exe sippp08putm5
dat2dta_exe sippp08putm6
dat2dta_exe sippp08putm7
dat2dta_exe sippp08putm8
dat2dta_exe sippp08putm9
dat2dta_exe sippp08putm10
dat2dta_exe sippp08putm11
dat2dta_exe sippp08putm13

cd "$tempdata"

cap n shell del sipp08tm*.dta

cap n shell ren sippp08putm1.dta sipp08tm1.dta
cap n shell ren sippp08putm2.dta sipp08tm2.dta
cap n shell ren sippp08putm3.dta sipp08tm3.dta
cap n shell ren sippp08putm4.dta sipp08tm4.dta
cap n shell ren sippp08putm5.dta sipp08tm5.dta
cap n shell ren sippp08putm6.dta sipp08tm6.dta
cap n shell ren sippp08putm7.dta sipp08tm7.dta
cap n shell ren sippp08putm8.dta sipp08tm8.dta
cap n shell ren sippp08putm9.dta sipp08tm9.dta
cap n shell ren sippp08putm10.dta sipp08tm10.dta
cap n shell ren sippp08putm11.dta sipp08tm11.dta
cap n shell ren sippp08putm13.dta sipp08tm13.dta


* longitudinal weights
dat2dta_exe sipplgtwgt2008w16
cd "$tempdata"
cap n shell ren sipplgtwgt2008w16.dta sipp08lgtwgt.dta

* replicate weight files
	* CAN BE ADDED HERE -- FOR NOW SKIPPED *
	
	
}
****** ===> STEP 0.1.1: first selection, putting all waves into panel dta
				* produces: YYYYtotal_raw.dta
				* produces: YYYYtotal_v{$fileversion}.dta
				
				
capture log close step0log
capture noisily log using "${tempdata}/ctv_step0log_${logdate}.txt", append text name(step0log)
display "-------step 0.0.1--------"


cd "$tempdata"
do "${step0codedir}2008_construction_ctv_step0_1_1.do"

** set those with missing longitudinal weight to weight zero. 
local varwgtlist "lgtpn1wt lgtpn2wt lgtpn3wt lgtpn4wt lgtpn5wt lgtcy1wt lgtcy2wt lgtcy3wt lgtcy4wt lgtcy5wt"
foreach wgtvar of local varwgtlist {
	replace `wgtvar'=0 if `wgtvar'==. 

}
save, replace	



***** ====> STEP 0.1.2: further selection, creating panel extract.dta
				*produces  "YYYY_corewave_occmob.dta"
				*produces  "YYYY_corewave_occmob_min.dta"
do "${step0codedir}/2008_construction_ctv_step0_1_2.do"





***** ====> STEP 0.1.3: erase temporary files no longer needed 
cd "$tempdata"		
cap n erase temp2008.dta
cap n erase temp2008core.dta
cap n erase temp2008tm.dta
cap n erase testi.dta
cap n erase testm.dta
cap n erase testw.dta

if $keeplogs != 1 {
	
	global files : dir . files "sip08*.log"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
		
}

if $keepwaveraw_dtas != 1 {
	
	global files : dir . files "sipp08*.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
}	

if $keepnberoriginals != 1 {
	
	global files : dir . files "sipp08*.dat"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "sipp08*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	
	global files : dir . files "l08puw*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	global files : dir . files "p08putm*.zip"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	

	cap n erase lgtwgt2008w16.zip

}

	
	
********************************************************************************
/*______________________________________________________________________________

	at this stage, every data file from SIPP panels 1984-2008 has been converted
	to a .dta file
	
	then we have made an initial selection of variables and create extract 
	dta files for each panel (1984,85,86,87,88,90,91,92,93,96,2001,04,08)
	
	the next steps puts these panel-dtas together in large files covering 
	SIPP panels 1984-2008 (years 1983-2013) that are used
	for the subsequent CTV analysis
______________________________________________________________________________*/
********************************************************************************




********************************************************************************
**** STEP 0.2   CREATING RELEVANT VARIABLES AND INTERMEDIATE DTA FILES
********************************************************************************


do "${step0codedir}/1984_2008_construction_ctv_step0_2_0.do"
		// ------> saves to corewave_all_ctv.dta, the project's big master file 


******************************************************************
** MAJOR SELECTION 1: SELECT ONLY THOSE WITH AN N or U REALIZATION
******************************************************************	

do "${step0codedir}/1984_2008_construction_ctv_step0_2_1.do"
	
***** ADD OCCUPATIONS FOR COMPARISON BEFORE AND AFTER U/N SPELLS
//		- aggregation of occ/ind
do "${step0codedir}/1984_2008_construction_ctv_step0_2_2.do"
	* PRODUCES COREWAVE_U_N_CTV.DTA
	* PRODUCES REDUCED_U_N_CTV.DTA



	
****************************************************************************************
** MAJOR SELECTION 2: CONSIDER ONLY THOSE WITH SOME LABOUR FORCE ATTACHMENT DURING PANEL
*****************************************************************************************
	
	
	
do "${step0codedir}/1984_2008_construction_ctv_step0_2_3.do"
	* PRODUCES COREWAVE_OCCLFMIN_CTV.DTA
	* (optional) incorporation occupational_recodes_v2.do
	* leocc* variables
do "${step0codedir}/1984_2008_construction_ctv_step0_2_4.do"
	* SAVES UPDATED VERSION OF COREWAVE_OCCLFMIN_CTV.DTA 
	*			(corresponded to corewave_occlfmin_aug2018v2.dta)
	* PRODUCES COREWAVE_LF_WITH_U_CTV.DTA
	

	



********************************************************************************
**** STEP 0.3   GENERAL CLEAN-UP
********************************************************************************


** we can delete all the YYYYtotal_raw.dta files
if ${keepwaveraw_dtas}!=1 {
	
	cd "${outputdata}"
	
	global files : dir . files "????total_raw.dta"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
}

** one can go further, and delete all 1990s, 2000s YYYYtotal_v${fileversion}.dta
	** (the 1980s panels are used to estimate the miscoding, so keep them!)	



** clean the temp directory in its entirety?
	
if ${tempdircleanup}==1	{   //  at end of step0, delete all files in tempdata dir
	
	cd "${tempdata}"
	
	global files : dir . files "*.*"

	foreach f of global files {
			display "erasing `f'"
			cap n erase `f'
	}
	
	
}
	
********************************************************************************
global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"