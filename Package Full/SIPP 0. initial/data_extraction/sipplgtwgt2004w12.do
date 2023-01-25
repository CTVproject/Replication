log using sipplgtwgt2004w12, text replace
set mem 1000m

**------------------------------------------------;

**  This program reads the 2004 SIPP12 Longitudnal Weight Data File 
**  Note:  This program is distributed under the GNU GPL. See end of
**  this file and http://www.gnu.org/licenses/ for details.
**  by Jean Roth Wed Nov  3 15:36:05 EDT 2010
**  Please report errors to jroth@nber.org
**  run with do sipplgtwgt2004w12

**-----------------------------------------------;

** The following line should contain
**   the complete path and name of the raw data file.
**   On a PC, use backslashes in paths as in C:\  

local dat_name "lgtwgt2004w12.dat"

** The following line should contain the path to your output '.dta' file 

local dta_name "sipplgtwgt2004w12.dta"

** The following line should contain the path to the data dictionary file 

local dct_name "sipplgtwgt2004w12.dct"

** The line below does NOT need to be changed 

quietly infile using "${extractcodedir}/`dct_name'", using("`dat_name'") clear

**  Decimal places have been made explict in the dictionary file.
**  Stata resolves a missing value of -1 / # of decimal places as a missing value.

**Everything below this point, aside from the final save, are value labels

#delimit ;

;
label values spanel   spanel; 
label define spanel  
	2004        "Panel Year"                    
;

#delimit cr
desc,short

sort ssuid epppnum 
saveold `dta_name' , replace



** Copyright 2010 shared by the National Bureau of Economic Research and Jean Roth ;

** National Bureau of Economic Research. ;
** 1050 Massachusetts Avenue ;
** Cambridge, MA 02138 ;
** jroth@nber.org ;

** This program and all programs referenced in it are free software. You ;
** can redistribute the program or modify it under the terms of the GNU ;
** General Public License as published by the Free Software Foundation; 
** either version 2 of the License, or (at your option) any later version. ;

** This program is distributed in the hope that it will be useful, ;
** but WITHOUT ANY WARRANTY, without even the implied warranty of ;
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the ;
** GNU General Public License for more details. ;

** You should have received a copy of the GNU General Public License ;
** along with this program, if not, write to the Free Software ;
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA. ;