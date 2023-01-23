log using sip93t8, text replace
set mem 1000m
*This program reads the 1993 SIPP Wave 8 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Mar 18 11:55:13 EST 2004
*Please report errors to jroth@nber.org
*run with do sip93t8
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1993\sip93t8.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip93t8"

*Everything below this point are value labels

#delimit ;

;
label values rotation rotation;
label define rotation
	1           "Interview month: September 1995"
	2           "Interview month: June 1995"    
	3           "Interview month: July 1995"    
	4           "Interview month: August 1995"  
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
	11          "District Of Columbia"          
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
	61          "Maine, Vermont"                
	62          "Iowa, North Dakota, South Dakota"
	63          "Alaska, Idaho, Montana, Wyoming"
;
label values item36b  item36b;
label define item36b 
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other Type A"                  
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
label values ppmis1   ppmis1l;
label define ppmis1l 
	1           "Interview"                     
	2           "Noninterview"                  
;
label values ppmis2   ppmis2l;
label define ppmis2l 
	1           "Interview"                     
	2           "Noninterview"                  
;
label values ppmis3   ppmis3l;
label define ppmis3l 
	1           "Interview"                     
	2           "Non-interview"                 
;
label values ppmis4   ppmis4l;
label define ppmis4l 
	1           "Interview"                     
	2           "Non-interview"                 
;
label values ppmis5   ppmis5l;
label define ppmis5l 
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
	84          "83+ years (topcoded)"          
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
	0           "Not applicable or not a sample"
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
label values grdcmpl  grdcmpl;
label define grdcmpl 
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
label values tm9610   tm9610l;
label define tm9610l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - End of section"           
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
	12          "Other or Don't Know"           
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
	1           "Yes - End of section"          
	2           "No"                            
;
label values tm9618   tm9618l;
label define tm9618l 
	0           "Not applicable"                
	-3          "None"                          
	4500        "Amount from $4000 - 4999"      
	5500        "Amount from $5000 - 5999"      
	6500        "Amount from $6000 - 6999"      
	7000        "Amount from $7000 +"           
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
