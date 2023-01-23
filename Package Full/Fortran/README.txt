----------------------------------------------------------------------------------------------
This readme file contains the following information


I. versions - covers the different versions of the program included in the replication package
II. output  - covers the main output of the program
III. how to set it up  - if you want to run the program yourself, the information here might help you
IV. further info - additional info, not covered elsewhere. 
-----------------------------------------------------------------------------------------------






*********************************************************************************

					I.   VERSIONS

*********************************************************************************


1. Net Mobility Version
		This is the main version of the paper that includes excess mobility and net mobility across 4 super-occupations (non-routine cognitive (NRC), Non Routine manual (NRM), Routine Cognitive (RC) and Routine Manual (RM) occupations). 

2. Excess Mobility Version
		This is the other version covered in the main text of the paper

In the online appendix, we covered more details of the excess mobility version, and additional covered  

3. Robustness - NUN. We calibrate the Excess Mobility version using the data on NUN spells, i.e. nonemployment spells that are allowed to contain months of nonemployment without job search, but need to contain at least one month of job search.

4. Robustness - No reallocation. We shut down the reallocation option, and see how well the excess mobility model can do with the non-reallocation moments. There are two versions here that highlight a trade-off. 

5. Robustness - No human capital depreciation. We should down human capital depreciation in unemployment. 



*********************************************************************************

					II. OUTPUT FILES

*********************************************************************************

To facilitate comparison/checks, we have included a subdirectory "Reference Results" that contain
the results as used in the paper. When running Fortran again, the files in the highest level directory for the version in question are overwritten, but one look in the "reference results" directory for the output as we produced it ourselves. 

*********************************************************************************

				III. HOW TO SET UP THE COMPUTATION? 

*********************************************************************************

The versions provided in the replication package have been compiled in Visual Studio. We are including the project files in the replication package.

*******************
** FORTRAN CODE 
*******************

i. To make it run, besides the CTV specific code in the ./CODE/ directory, also need errcheck.inc and mkl_vsl.f90 as part of project for the random number generating (in addition to including MKL in the compiler/linker options). (Alternatively, can used standard, but possibly slower, random number generators, but this requires changing the code). 


MKL_VSL.F90, we found it at: ...\Program Files (x86)\Intel\oneAPI\mkl\2022.1.0\include
	-in the project file, we have added mkl_vsl.f90 manually to project
	-this location needs to be updated from our version (also given a possibly different version of MKL)
ERRCHECK.INC Location, in zip file: C:\Program Files (x86)\Intel\oneAPI\mkl\2022.1.0\examples\examples_core_f.zip\f\vsl\source
	- errcheck.inc extracted, copied to location where it can be included as a source file.
	- (here: we copied it directly to ...\Intel\oneAPI\mkl\2022.1.0\include directory, and added it manually to the project)

ii. Include MKL in the compiler options

iii. We ran the "release" configuration, then the files were produced in the main directory and should be picked up by STATA subsequently. If not, can set the location of these files by hand in the "global_paths.do" file in the main SIPP directory. 

**********************
** COMPILER OPTIONS
**********************

/nologo /O3 /heap-arrays0 /assume:recursion /module:"x64\Release\\" /object:"x64\Release\\" /Fd"x64\Release\vc170.pdb" /libs:dll /threads /Qmkl:sequential /c

- faster running with /O3 optimization option
- there is one small part of the code that uses a recursive routine
- multithread DLL for when running the estimation parallelized
- use MKL (but iirc run it sequential within a single parameter tuple evaluation -- which in the version at hand is by construction a single parameter tuple. 

** LINKER OPTIONS
/OUT:"x64\Release\netmob_ctv_replic.exe" 
/VERBOSE:LIB /NOLOGO /MANIFEST /MANIFESTFILE:"x64\Release\netmob_ctv_replic.exe.intermediate.manifest" 
/MANIFESTUAC:"level='asInvoker' uiAccess='false'" 
/SUBSYSTEM:CONSOLE 
/STACK:1800000000 
/IMPLIB:"C:\computation\netmob_oct2019\simple_ctv\x64\Release\netmob_ctv_replic.lib"

Note that we set the stack reserve size large







*********************************************************************************

					FURTHER INFO

*********************************************************************************



***********************************************************************
PROGRAM USED BEHIND THE RESULTS INCLUDED IN THE REPLICATION PACKAGE:
***********************************************************************


-Running this with visual studio 2022, community version, and oneAPI)

Microsoft Visual Studio Community 2022
Version 17.3.4
VisualStudio.17.Release/17.3.4+32901.215
Microsoft .NET Framework
Version 4.8.04161

Installed Version: Community

Intel Libraries for oneAPI   Package ID: w_oneAPI_2022.1.0.256
Intel Libraries for oneAPI – toolkit version: 2022.2.0, extension version 22.0.0.17, Package ID: w_oneAPI_2022.1.0.256, Copyright © 2019-2022 Intel Corporation. All rights reserved.
* Other names and brands may be claimed as the property of others.

Intel® Fortran Compiler   Package ID: w_oneAPI_2022.1.0.256
Intel® Fortran Compiler – toolkit version: 2022.2.0, extension version 22.0.0066.17, Package ID: w_oneAPI_2022.1.0.256, Copyright © 2002-2022 Intel Corporation. All rights reserved.
* Other names and brands may be claimed as the property of others.

Intel® oneAPI Menu & Samples   10.10.392.9731
Intel® oneAPI Visual Studio Menu & Samples Extension



