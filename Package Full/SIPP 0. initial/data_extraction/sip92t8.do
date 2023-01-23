log using sip92t8, text replace
set mem 1000m
*This program reads the 1992 SIPP Wave 8 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 18:33:57 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip92t8
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1992\sip92t8.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip92t8"

*Everything below this point are value labels

#delimit ;

;
label values tm9620   tm9620l;
label define tm9620l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm9622   tm9622l;
label define tm9622l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9626"           
;
label values tm9624   tm9624l;
label define tm9624l 
	0           "Not applicable"                
;
label values tm9626   tm9626l;
label define tm9626l 
	0           "Not applicable"                
	-3          "None received - End of"        
;
label values tm9628   tm9628l;
label define tm9628l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9632   tm9632l;
label define tm9632l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9636   tm9636l;
label define tm9636l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9640   tm9640l;
label define tm9640l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9644   tm9644l;
label define tm9644l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9648   tm9648l;
label define tm9648l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9652   tm9652l;
label define tm9652l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9656   tm9656l;
label define tm9656l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9660   tm9660l;
label define tm9660l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9664   tm9664l;
label define tm9664l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9668   tm9668l;
label define tm9668l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values tm9672   tm9672l;
label define tm9672l 
	0           "Not marked as received"        
	1           "Received"                      
;
label values imp9612  imp9612l;
label define imp9612l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9616  imp9616l;
label define imp9616l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9618  imp9618l;
label define imp9618l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9620  imp9620l;
label define imp9620l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9622  imp9622l;
label define imp9622l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9624  imp9624l;
label define imp9624l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9630  imp9630l;
label define imp9630l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9634  imp9634l;
label define imp9634l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9638  imp9638l;
label define imp9638l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9642  imp9642l;
label define imp9642l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9646  imp9646l;
label define imp9646l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9650  imp9650l;
label define imp9650l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9654  imp9654l;
label define imp9654l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9658  imp9658l;
label define imp9658l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9662  imp9662l;
label define imp9662l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9666  imp9666l;
label define imp9666l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9670  imp9670l;
label define imp9670l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9674  imp9674l;
label define imp9674l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmtedfin tmtedfin;
label define tmtedfin
	4500        "Amount from $4100 - 4999"      
	5500        "Amount from $5000 - 5999"      
	6500        "Amount from $6000 - 6999"      
	7500        "Amount from $7000 - 7999"      
	8500        "Amount from $8000 - 8999"      
	9500        "Amount from $9000 - 9999"      
	10500       "Amount from $10000 - 10999"    
	11000       "Amount from $11000 +"          
	0           "Not applicable"                
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
