********************************************************************************
** PATHS TO RUN THE REPLICATION PACKAGE
********************************************************************************



version 13


********************************
** PATHS SET MANUALLY 
********************************

		** COMMENT OUT, OR LEAVE EMPTY (i.e. "") 
		** IF USING THE DEFAULT PATHS, AND RUNNING THE REPLICATION
		** PACKAGE FROM THE ROOT DIRECTORY OF THE REPLICATION PACKAGE
		
		** IF outputdata and tempdata are not set to existing directory, default to default...
		

global workingdir "`c(pwd)'"      // root directory of the replication package. STRONGLY RECOMMEND SETTING THIS EXPLICITLY
global outputdata ""
global tempdata ""	
global rawdata "" 

global step0codedir ""			// location of code for downloading and preparing the datasets used
global extractcodedir ""		// location of .do & .dct files to read downloaded data into stata
global step1codedir ""			// location of code deriving miscoding matrix and location of correction matrix
global step2codedir ""			// location of STATA code deriving main results (data, calibration targets, tables, figures) from SIPP data
global sec45codedir ""			// location of STATA code deriving section 4 and section 5 results from SIPP data and computational results


global mainresultsdir "${workingdir}/results/"		// location of directory where main results are saved
global allresultsdir ""			// location of other results (robustness, etc)
global resultsdir ""			// overall results, not used

global cpsdir ""				// location of directory for CPS do files, data, etc.
global fortrandir ""			// root directory of the folder containing the computational code and computational results
global aggdatadir ""			// 

display "== WORKING DIRECTORY == "
cd "${workingdir}"

************************************
** PATHS SET AUTOMATICALLY
***********************************

global workingdir `c(pwd)'



	
     // Should be Replication Main Directory

if "$step0codedir"=="" {
	*display "step0codedir UNDEFINED, defining it now"
	global step0codedir "${workingdir}/SIPP 0. initial/"
	}
if "$extractcodedir"=="" {
	*display "extractcodedir UNDEFINED, defining it now"
	global extractcodedir "${workingdir}/SIPP 0. initial/data_extraction/"
	}
if "$step1codedir"=="" {
    *display "step1codedir UNDEFINED, defining it now"
	global step1codedir "${workingdir}/SIPP 1. Miscoding/"
	}
if "$step2codedir"=="" {
	*display "step2codedir UNDEFINED, defining it now"
	global step2codedir "${workingdir}/SIPP 2. main statistics/"
	}
if "$sec45codedir"=="" {
	*display "sec45codedir UNDEFINED, defining it now"
	global sec45codedir "${workingdir}/Computation Results Processing/"
	}

** results
if "${allresultsdir}" == "" {
	*display "allresultsdir UNDEFINED, defining it now"
	global allresultsdir "${workingdir}/results/Collection/"
	}
if "${mainresultsdir}" == "" {
	*display "mainresultsdir UNDEFINED, defining it now"
	global mainresultsdir "${workingdir}/results/"
	}
if "${resultsdir}" == "" {
	*display "mainresultsdir UNDEFINED, defining it now"
	global resultsdir "${workingdir}/results/"
	}

	
** other data and output
if "$cpsdir"=="" {
	*display "CPSDIR UNDEFINED, defining it now"
	global cpsdir "${workingdir}/CPS/"
	}
if "$aggdatadir"=="" {
	*display "aggdatadir UNDEFINED, defining it now"
	global aggdatadir "${workingdir}/Aggregate Data/"
	}
if "$fortrandir"=="" {
    *display "step1codedir UNDEFINED, defining it now"
	global fortrandir "${workingdir}/fortran/"
	}
	

if "${tempdata}"=="" {
	global tempdata "${workingdir}/temp/"
}
if "${rawdata}"=="" {
	global rawdata  "${tempdata}/"
}
if "${outputdata}"=="" {
	global outputdata  "${workingdir}/output/"
}
	
	
	
*** CHECK EXISTENCE  OF RESULTS DIRECTORIES, CREATE IF NECESSARY



mata : st_numscalar("OK", direxists("${resultsdir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====RESULTS  DIRECTORY====="
	di "${resultsdir}" 
	}
else {
	di "RESULTS DIRECTORY DOES NOT EXIST"
	di "change to/create output subdirectory of working directory:"
	global resultsdir	"${workingdir}/results/"
	
	mata : st_numscalar("OK2", direxists("${resultsdir}"))
	
	if scalar(OK2)==1 {
	*di scalar(OK2)
	di "success, results directory set to:"
	di "${resultsdir}" 
	}
	else {
	mkdir "${resultsdir}"
	di "NEW RESULTS DIRECTORY CREATED:"
	di "${resultsdir}"
	}
 
}


cap n di "All RESULTS DIRECTORY =${allresultsdir}"
mata : st_numscalar("OK", direxists("${allresultsdir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "-all results directory set to existing-"
	di "${allresultsdir}" 
	}
else {
	di "ALL RESULTS DIRECTORY DOES NOT EXIST"
	di "ALL RESULTS DIRECTORY needs to be created"
	mkdir "${allresultsdir}"
	}
 

 
 
*** CHECK EXISTENCE  OF CODE  DIRECTORIES, PAUSE IF CODE DIRECTORY NOT EXIST
mata : st_numscalar("OK", direxists("${step0codedir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====STEP 1 CODE DIRECTORY====="
	di "${step0codedir}" 
	}
else {
	di "ASSIGNED STEP 0 CODEDIR DOES NOT EXIST"
	di "CHANGE PATH TO STEP 0 CODEDIR IN global_paths.do"
    pause
    }
mata : st_numscalar("OK", direxists("${sextractcodedir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====EXTRACT CODE DIRECTORY====="
	di "${extractcodedir}" 
	}
else {
	di "ASSIGNED EXTRACT FILES CODE DIR DOES NOT EXIST"
	di "CHANGE PATH TO EXTRACTCODEDIR IN global_paths.do"
    pause
    }

mata : st_numscalar("OK", direxists("${step1codedir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====STEP 1 CODE DIRECTORY====="
	di "${step1codedir}" 
	}
else {
	di "ASSIGNED STEP 1 CODEDIR DOES NOT EXIST"
	di "CHANGE PATH TO STEP 1 CODEDIR IN global_paths.do"
    pause
    }



mata : st_numscalar("OK", direxists("${step2codedir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====STEP 2 CODE DIRECTORY====="
	di "${step2codedir}" 
	}
else {
	di "ASSIGNED STEP 2 CODEDIR DOES NOT EXIST"
	di "CHANGE PATH TO STEP 2 CODEDIR IN global_paths.do"
    pause
    }

	
mata : st_numscalar("OK", direxists("${sec45codedir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====DATA/MODEL RESULTS PROCESSING CODE DIRECTORY====="
	di "${sec45codedir}" 
	}
else {
	di "ASSIGNED SEC45CODEDIR DOES NOT EXIST"
	di "CHANGE PATH OF SEC45CODEDIR IN global_paths.do"
    pause
    }


mata : st_numscalar("OK", direxists("${cpsdir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====CPS CODE DIRECTORY====="
	di "${cpsdir}" 
	}
else {
	di "ASSIGNED CPS DIRECTORY DOES NOT EXIST"
	di "CHANGE PATH TO CPS CODEDIR IN global_paths.do"
	di "or comment out the CPS lines in global_paths.do "
	di "in the master do-file and the pause on the next line"
    pause
    }



mata : st_numscalar("OK", direxists("${fortrandir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====FORTRAN COMPUTATION DIRECTORY====="
	di "${fortrandir}" 
	}
else {
	di "ASSIGNED FORTRAN DIRECTORY DOES NOT EXIST"
	di "CHANGE PATH TO FORTRAN CODEDIR IN global_paths.do"
	di "or comment out the fortran lines in global_paths.do "
	di "in the master do-file and the pause on the next line"
    pause
    }



	
mata : st_numscalar("OK", direxists("${aggdatadir}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====AGGREGATE DATA DIRECTORY====="
	di "${aggdatadir}" 
	}
else {
	di "ASSIGNED AGGREGATE DATA DIRECTORY DOES NOT EXIST"
	di "CHANGE PATH OF AGGDATADIR IN global_paths.do"
    pause
    }

		
	
	
*** CHECK EXISTENCE  OF TEMP/OUTPUT DIRECTORIES, CREATE IF NECESSARY

cap n di "TEMP DIRECTORY =${tempdata}"
mata : st_numscalar("OK", direxists("${tempdata}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "-temp data directory set to existing-"
	di "${tempdata}" 
	}
else {
	di "TEMP DATA DIRECTORY DOES NOT EXIST"
	di "change to/create tempdata subdirectory of working directory:"
	global tempdata	"${workingdir}/temp/"
	
	mata : st_numscalar("OK2", direxists("${tempdata}"))
	
	if scalar(OK2)==1 {
	*di scalar(OK2)
	di "success, temp data directory set to:"
	di "${tempdata}" 
	}
	else {
	di "TEMP DATA DIRECTORY needs to be created"
	mkdir "${tempdata}"
	}
 
}
if "${rawdata}"=="" {
	global rawdata "$tempdata"
}
if "${rawdata}"!="${tempdata}" {
		mata : st_numscalar("OK", direxists("${rawdata}"))
		if scalar(OK)==1 {
			*di scalar(OK)
			di "-raw data directory-"
			di "${rawdata}" 
			}
		else {
			di "RAW DATA DIRECTORY DOES NOT EXIST"
			di "set to temp data directory"
			global rawdata "$tempdata"
		}
}		 
mata : st_numscalar("OK", direxists("${outputdata}"))
if scalar(OK)==1 {
	*di scalar(OK)
	di "=====OUTPUT DATA DIRECTORY====="
	di "${outputdata}" 
	}
else {
	di "OUTPUT DATA DIRECTORY DOES NOT EXIST"
	di "change to/create output subdirectory of working directory:"
	global outputdata	"${workingdir}/output/"
	
	mata : st_numscalar("OK2", direxists("${outputdata}"))
	
	if scalar(OK2)==1 {
	*di scalar(OK2)
	di "success, output data directory set to:"
	di "${outputdata}" 
	}
	else {
	mkdir "${outputdata}"
	di "OUTPUT DATA DIRECTORY CREATED:"
	di "${outputdata}"
	}
 
}

	
	
	


