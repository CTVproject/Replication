log using sip92t3, text replace
set mem 1000m
*This program reads the 1992 SIPP Wave 3 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 18:28:30 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip92t3
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1992\sip92t3.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip92t3"

*Everything below this point are value labels

#delimit ;

;
label values state    state;  
label define state   
	1           "Alabama"                       
	4           "Arizona"                       
	5           "Arkansas"                      
	6           "California"                    
	8           "Colorado"                      
	9           "Connecticut"                   
	10          "Delaware"                      
	11          "District of Columbia"          
	12          "Florida"                       
	13          "Georgia"                       
	15          "Hawaii"                        
	17          "Illinois"                      
	18          "Indiana"                       
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	28          "Mississippi"                   
	29          "Missouri"                      
	31          "Nebraska"                      
	32          "Nevada"                        
	33          "New Hampshire"                 
	34          "New Jersey"                    
	35          "New Mexico"                    
	36          "New York"                      
	37          "North Carolina"                
	39          "Ohio"                          
	40          "Oklahoma"                      
	41          "Oregon"                        
	42          "Pennsylvania"                  
	44          "Rhode Island"                  
	45          "South Carolina"                
	47          "Tennessee"                     
	48          "Texas"                         
	49          "Utah"                          
	51          "Virginia"                      
	53          "Washington"                    
	54          "West Virginia"                 
	55          "Wisconsin"                     
	61          "Maine,Vermont"                 
	62          "Iowa,North Dakota,South Dakota"
	63          "Alaska,Idaho,Montana,Wyoming"  
;
label values item36b  item36b;
label define item36b 
	1           "Interview D"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other type A"                  
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
	23          "Entire household deceased, moved"
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
	28          "Merged hhlds across panels"    
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - type Z refusal" 
	4           "Noninterview - type Z other"   
;
label values pp_mis   pp_mis; 
label define pp_mis  
	1           "Interview"                     
	2           "Non-interview"                 
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
	21          "Afro-American"                 
	30          "Another group not listed"      
	39          "Don't know"                    
;
label values tm8000   tm8000l;
label define tm8000l 
	1           "Yes"                           
	2           "No"                            
;
label values tm8148   tm8148l;
label define tm8148l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8150   tm8150l;
label define tm8150l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8152   tm8152l;
label define tm8152l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8154   tm8154l;
label define tm8154l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8156   tm8156l;
label define tm8156l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8158   tm8158l;
label define tm8158l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Very safe"                     
	2           "Fairly safe"                   
	3           "Fairly unsafe"                 
	4           "Very unsafe"                   
;
label values tm8160   tm8160l;
label define tm8160l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Very safe"                     
	2           "Fairly safe"                   
	3           "Fairly unsafe"                 
	4           "Very unsafe"                   
;
label values tm8162   tm8162l;
label define tm8162l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8184   tm8184l;
label define tm8184l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8204   tm8204l;
label define tm8204l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8300   tm8300l;
label define tm8300l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8304   tm8304l;
label define tm8304l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8306   tm8306l;
label define tm8306l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8310   tm8310l;
label define tm8310l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8312   tm8312l;
label define tm8312l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8316   tm8316l;
label define tm8316l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8318   tm8318l;
label define tm8318l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8322   tm8322l;
label define tm8322l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8324   tm8324l;
label define tm8324l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8328   tm8328l;
label define tm8328l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8330   tm8330l;
label define tm8330l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8334   tm8334l;
label define tm8334l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8336   tm8336l;
label define tm8336l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8340   tm8340l;
label define tm8340l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8342   tm8342l;
label define tm8342l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8344   tm8344l;
label define tm8344l 
	-3          "Not applicable"                
	-1          "Don't know"                    
	0           "Not in sample"                 
	1           "All of the help I/we need"     
	2           "Most of the help I/we need"    
	3           "Very little of the help I/we"  
	4           "No help"                       
;
label values tm8346   tm8346l;
label define tm8346l 
	-3          "Not applicable"                
	-1          "Don't know"                    
	0           "Not in sample"                 
	1           "All of the help I/we need"     
	2           "Most of the help I/we need"    
	3           "Very little of the help I/we"  
	4           "No help"                       
;
label values tm8348   tm8348l;
label define tm8348l 
	-3          "Not applicable"                
	-1          "Don't know"                    
	0           "Not in sample"                 
	1           "All of the help I/we need"     
	2           "Most of the help I/we need"    
	3           "Very little of the help I/we"  
	4           "No help"                       
;
label values tm8350   tm8350l;
label define tm8350l 
	-1          "Don't know - skip to Check"    
	0           "Not applicable"                
	1           "Enough of the kinds of food we"
	2           "Enough but not always the kinds"
	3           "Sometimes not enough to eat"   
	4           "Often not enough to eat"       
;
label values tm8368   tm8368l;
label define tm8368l 
	-3          "None - skip to Check Item C1"  
;
label values tm8370   tm8370l;
label define tm8370l 
	-1          "Don't know"                    
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
