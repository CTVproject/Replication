log using sip90t5, text replace
set mem 1000m
*This program reads the 1990 SIPP Wave 5 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 17:36:42 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip90t5
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1990\sip90t5.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
/* The following line should contain
   the complete path and name of the raw data file.
   On a PC, use backslashes in paths as in C:\	*/

local dat_name "sipp90t5.dat"

/* The following line should contain the path to your output '.dta' file */

local dta_name "sipp90t5.dta"

/* The following line should contain the path to the data dictionary file */

local dct_name "sip90t5.dct"

quietly infile using "${extractcodedir}/`dct_name'", using("`dat_name'") clear

*Everything below this point are value labels

#delimit ;

;
label values item36b  item36b;
label define item36b 
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other Type A"                  
	9           "Vacant"                        
	10          "Occupied by persons with URE"  
	11          "Unfit or to be demolished"     
	12          "Under construction, not ready" 
	13          "Converted to temporary business"
	14          "Unoccupied site for mobile home,"
	15          "Permit granted, construction not"
	17          "Demolished"                    
	18          "House or trailer moved"        
	19          "Converted to permanent business"
	20          "Merged"                        
	21          "Condemned"                     
	23          "Entire household deceased,"    
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
;
label values rrp      rrp;    
label define rrp     
	0           "Not a sample person in this"   
	1           "Household reference person,"   
	2           "Household reference person"    
	3           "Spouse of household reference" 
	4           "Child of household reference"  
	5           "Other relative of household"   
	6           "Non-relative of household"     
	7           "Non-relative of household"     
;
label values age      age;    
label define age     
	0           "Less than 1 full year"         
	1           "1 year"                        
;
label values sex      sex;    
label define sex     
	1           "Male"                          
	2           "Female"                        
;
label values race     race;   
label define race    
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
;
label values ms       ms;     
label define ms      
	0           "Not a sample person in this"   
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
;
label values pnsp     pnsp;   
label define pnsp    
	0           "Not a sample person in this"   
	999         "Not applicable"                
;
label values pnpt     pnpt;   
label define pnpt    
	0           "Not a sample person in this"   
	999         "Not applicable"                
;
label values higrade  higrade;
label define higrade 
	0           "Not applicable if under 15,"   
;
label values grd_cmpl grd_cmpl;
label define grd_cmpl
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ethnicty ethnicty;
label define ethnicty
	1           "German"                        
	2           "English"                       
	3           "Irish"                         
	4           "French"                        
	5           "Italian"                       
	6           "Scottish"                      
	7           "Polish"                        
	8           "Dutch"                         
	9           "Swedish"                       
	10          "Norwegian"                     
	11          "Russian"                       
	12          "Ukrainian"                     
	13          "Welsh"                         
	14          "Mexican-American"              
	15          "Chicano"                       
	16          "Mexican"                       
	17          "Puerto Rican"                  
	18          "Cuban"                         
	19          "Central or South American"     
	20          "Other Spanish"                 
	21          "Afro-American (Black or Negro)"
	30          "Another group not listed"      
	39          "Don't know"                    
;
label values tm9610   tm9610l;
label define tm9610l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - End of interview"         
;
label values tm9612   tm9612l;
label define tm9612l 
	0           "Not applicable"                
	1           "Elementary grades 1-8"         
	2           "High school grades 9-12"       
	3           "College year 1"                
	4           "College year 2"                
	5           "College year 3"                
	6           "College year 4"                
	7           "College year 5"                
	8           "College year 6+"               
	9           "Vocational school"             
	10          "Technical school"              
	11          "Business school"               
	12          "Other or DK"                   
;
label values tm9614   tm9614l;
label define tm9614l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9618"           
;
label values tm9616   tm9616l;
label define tm9616l 
	0           "Not applicable"                
	1           "Yes - End of interview"        
	2           "No"                            
;
label values tm9618   tm9618l;
label define tm9618l 
	4500        "Amount from $4000 to $4999"    
	5500        "Amount from $5000 to $5999"    
	07000       "Amount from $6000 or more"     
;
label values tm9620   tm9620l;
label define tm9620l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9622   tm9622l;
label define tm9622l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to 9626"             
;
label values tm9624   tm9624l;
label define tm9624l 
	0           "Not applicable"                
;
label values tm9626   tm9626l;
label define tm9626l 
	0           "Not applicable"                
	-3          "None - End of interview"       
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
label values tm_ifc1  tm_ifc1l;
label define tm_ifc1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifc2  tm_ifc2l;
label define tm_ifc2l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifc3  tm_ifc3l;
label define tm_ifc3l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifc4  tm_ifc4l;
label define tm_ifc4l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifc5  tm_ifc5l;
label define tm_ifc5l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifc6  tm_ifc6l;
label define tm_ifc6l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifc7  tm_ifc7l;
label define tm_ifc7l
	0           "Not imputed"                   
	1           "Imputed TM9630"                
;
label values tm_ifc8  tm_ifc8l;
label define tm_ifc8l
	0           "Not imputed"                   
	1           "Imputed TM9634"                
;
label values tm_ifc9  tm_ifc9l;
label define tm_ifc9l
	0           "Not imputed"                   
	1           "Imputed TM9638"                
;
label values tm_ifc10 tm_ifc1y;
label define tm_ifc1y
	0           "Not imputed"                   
	1           "Imputed TM9642"                
;
label values tm_ifc11 tm_ifc1k;
label define tm_ifc1k
	0           "Not imputed"                   
	1           "Imputed TM9646"                
;
label values tm_ifc12 tm_ifc1m;
label define tm_ifc1m
	0           "Not imputed"                   
	1           "Imputed TM9650"                
;
label values tm_ifc13 tm_ifc1n;
label define tm_ifc1n
	0           "Not imputed"                   
	1           "Imputed TM9654"                
;
label values tm_ifc14 tm_ifc1o;
label define tm_ifc1o
	0           "Not imputed"                   
	1           "Imputed TM9658"                
;
label values tm_ifc15 tm_ifc1p;
label define tm_ifc1p
	0           "Not imputed"                   
	1           "Imputed TM9662"                
;
label values tm_ifc16 tm_ifc1q;
label define tm_ifc1q
	0           "Not imputed"                   
	1           "Imputed TM9666"                
;
label values tm_ifc17 tm_ifc1r;
label define tm_ifc1r
	0           "Not imputed"                   
	1           "Imputed TM9670"                
;
label values tm_ifc18 tm_ifc1s;
label define tm_ifc1s
	0           "Not imputed"                   
	1           "Imputed TM9674"                
;
label values tmtedfin tmtedfin;
label define tmtedfin
	4500        "Amount from $4000 - $4999"     
	5500        "Amount from $5000 - $5999"     
	7000        "Amount $6000 or more"          
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
