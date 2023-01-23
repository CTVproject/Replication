log using sip90db, text replace
set mem 500m
*This program reads the 1990 SIPP Type of Death Benefit Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Mar 11 15:57:30 EST 2004
*Please report errors to jroth@nber.org
*run with do sip90db
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1990\sip90db.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip90db"

*Everything below this point are value labels

#delimit ;

;
label values tobd90   tobd90l;
label define tobd90l 
	0           "Not in sample in January of 1991."
	1           "All retired workers exlduding dually entitled females"
	2           "Disabled workers"              
	3           "Wife (excludes males receiving a spouse benefit)"
	4           "Dually entitled wife ( excludes males dually entitled males)"
	5           "Widow benefit (excludes males receiving benefit as a widower)"
	6           "Dually entitled widow ( excludes males dually entitled males)"
	7           "All other"                     
	9           "Matched and not in current pay, matched but no MBR record"
;
label values ssayod   ssayod; 
label define ssayod  
	0           "No year of death identified or identified year of death"
;

/*
Copyright 2004 shared by the National Bureau of Economic Research and Jean Roth

National Bureau of Economic Research.
1050 Massachusetts Avenue
Cambridge, MA 02138
jroth@nber.org

This program and all programs referenced in it are free software. You
can redistribute the program or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation;
either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
USA.
*/
