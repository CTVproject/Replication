log using sippp08putm13.log, replace

**------------------------------------------------;

**  This program reads the 2008 SIPP Wave 13 Topical Module Data File 
**  Note:  This program is distributed under the GNU GPL. See end of
**  this file and http://www.gnu.org/licenses/ for details.
**  by Jean Roth Wed Jan 29 17:47:30 EST 2014
**  Please report errors to jroth@nber.org
**  run with do sippp08putm13

**-----------------------------------------------;

** The following line should contain
**   the complete path and name of the raw data file.
**   On a PC, use backslashes in paths as in C:\  

local dat_name "p08putm13.dat"

** The following line should contain the path to your output '.dta' file 

local dta_name "sippp08putm13.dta"

** The following line should contain the path to the data dictionary file 

local dct_name "sippp08putm13.dct"

** The line below does NOT need to be changed 

quietly infile using "${extractcodedir}/`dct_name'", using("`dat_name'") clear

**  Decimal places have been made explict in the dictionary file.
**  Stata resolves a missing value of -1 / # of decimal places as a missing value.

**Everything below this point, aside from the final save, are value labels

#delimit ;

;
label values spanel   spanel; 
label define spanel  
	2008        "Panel Year"                    
;
label values tfipsst  tfipsst;
label define tfipsst 
	1           "Alabama"                       
	2           "Alaska"                        
	4           "Arizona"                       
	5           "Arkansas"                      
	6           "California"                    
	8           "Colorado"                      
	9           "Connecticut"                   
	10          "Delaware"                      
	11          "DC"                            
	12          "Florida"                       
	13          "Georgia"                       
	15          "Hawaii"                        
	16          "Idaho"                         
	17          "Illinois"                      
	18          "Indiana"                       
	19          "Iowa"                          
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	23          "Maine"                         
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	28          "Mississippi"                   
	29          "Missouri"                      
	30          "Montana"                       
	31          "Nebraska"                      
	32          "Nevada"                        
	33          "New Hampshire"                 
	34          "New Jersey"                    
	35          "New Mexico"                    
	36          "New York"                      
	37          "North Carolina"                
	38          "North Dakota"                  
	39          "Ohio"                          
	40          "Oklahoma"                      
	41          "Oregon"                        
	42          "Pennsylvania"                  
	44          "Rhode Island"                  
	45          "South Carolina"                
	46          "South Dakota"                  
	47          "Tennessee"                     
	48          "Texas"                         
	49          "Utah"                          
	50          "Vermont"                       
	51          "Virginia"                      
	53          "Washington"                    
	54          "West Virginia"                 
	55          "Wisconsin"                     
	56          "Wyoming"                       
;
label values eoutcome eoutcome;
label define eoutcome
	201         "Completed interview"           
	203         "Compl. partial- missing data; no"
	207         "Complete partial - TYPE-Z; no" 
	213         "TYPE-A, language problem"      
	216         "TYPE-A, no one home (noh)"     
	217         "TYPE-A, temporarily absent (ta)"
	218         "TYPE-A, hh refused"            
	219         "TYPE-A, other occupied (specify)"
	234         "TYPE-B, entire hh institut. or"
	248         "TYPE-C, other (specify)"       
	249         "TYPE-C, sample adjustment"     
	250         "TYPE-C, hh deceased"           
	251         "TYPE-C, moved out of country"  
	252         "TYPE-C, living in armed forces"
	253         "TYPE-C, on active duty in Armed"
	254         "TYPE-C, no one over age 15 years"
	255         "TYPE-C, no Wave 1 persons"     
	260         "TYPE-D, moved address unknown" 
	261         "TYPE-D, moved within U.S. but" 
	262         "TYPE-C, merged with another SIPP"
	270         "TYPE-C, mover, no longer located"
	271         "TYPE-C, mover, new address"    
	280         "TYPE-D, mover, no longer located"
;
label values rfid2    rfid2l; 
label define rfid2l  
	-1          "Not in Universe"               
;
label values epopstat epopstat;
label define epopstat
	1           "Adult (15 years of age or older)"
	2           "Child (Under 15 years of age)" 
;
label values eppintvw eppintvw;
label define eppintvw
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z"         
	4           "Noninterview - pseudo Type Z." 
	5           "Children under 15 during"      
;
label values eppmis4  eppmis4l;
label define eppmis4l
	1           "Interview"                     
	2           "Non-interview"                 
;
label values esex     esex;   
label define esex    
	1           "Male"                          
	2           "Female"                        
;
label values erace    erace;  
label define erace   
	1           "White alone"                   
	2           "Black alone"                   
	3           "Asian alone"                   
	4           "Residual"                      
;
label values eorigin  eorigin;
label define eorigin 
	1           "Yes"                           
	2           "No"                            
;
label values errp     errp;   
label define errp    
	1           "Reference person with related" 
	2           "Reference Person without related"
	3           "Spouse of reference person"    
	4           "Child of reference person"     
	5           "Grandchild of reference person"
	6           "Parent of reference person"    
	7           "Brother/sister of reference person"
	8           "Other relative of reference person"
	9           "Foster child of reference person"
	10          "Unmarried partner of reference"
	11          "Housemate/roommate"            
	12          "Roomer/boarder"                
	13          "Other non-relative of reference"
;
label values tage     tage;   
label define tage    
	0           "Less than 1 full year old"     
;
label values ems      ems;    
label define ems     
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never Married"                 
;
label values epnspous epnspous;
label define epnspous
	9999        "Spouse not in household or person"
;
label values epnmom   epnmom; 
label define epnmom  
	9999        "No mother in household"        
;
label values epndad   epndad; 
label define epndad  
	9999        "No father in household"        
;
label values epnguard epnguard;
label define epnguard
	-1          "Not in Universe"               
	9999        "Guardian not in household"     
;
label values rdesgpnt rdesgpnt;
label define rdesgpnt
	-1          "Not in Universe"               
	1           "Yes"                           
	2           "No"                            
;
label values eeducate eeducate;
label define eeducate
	-1          "Not in Universe"               
	31          "Less Than 1st Grade"           
	32          "1st, 2nd, 3rd or 4th grade"    
	33          "5th Or 6th Grade"              
	34          "7th Or 8th Grade"              
	35          "9th Grade"                     
	36          "10th Grade"                    
	37          "11th Grade"                    
	38          "12th grade, no diploma"        
	39          "High School Graduate - (diploma"
	40          "Some college, but no degree"   
	41          "Diploma or certificate from a" 
	43          "Associate (2-yr) college degree"
	44          "Bachelor's degree (for example:"
	45          "Master's degree (For example: MA,"
	46          "Professional School degree (for"
	47          "Doctorate degree (for example:"
;
label values sinthhid sinthhid;
label define sinthhid
	0           "Not In Universe"               
;
label values eaecunv  eaecunv;
label define eaecunv 
	-1          "Not in Universe"               
	1           "In universe"                   
;
label values iprocert iprocert;
label define iprocert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values iwhopcer iwhopcer;
label define iwhopcer
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Federal government"            
	2           "State government"              
	3           "Local government"              
	4           "Industry"                      
	5           "Business, company, or non-profit"
	6           "Professional Association"      
	7           "Other"                         
;
label values iwhypcer iwhypcer;
label define iwhypcer
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Mainly work-related"           
	2           "Mainly personal interest"      
;
label values ifldpcer ifldpcer;
label define ifldpcer
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Architecture and engineering"  
	2           "Computer networking and"       
	3           "Computer applications and design"
	4           "Business/finance management"   
	5           "Administrative support"        
	6           "Nursing/nurse assisting"       
	7           "Other medical/health care"     
	8           "Cosmetology"                   
	9           "Culinary arts"                 
	10          "Protective services"           
	11          "Legal and social services"     
	12          "Education"                     
	13          "Construction and manufacturing"
	14          "Transportation and material moving"
	15          "Public utilities"              
	16          "Other"                         
;
label values ijobpcer ijobpcer;
label define ijobpcer
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values irjpcert irjpcert;
label define irjpcert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	3           "Not applicable(never worked)"  
;
label values itrnpcer itrnpcer;
label define itrnpcer
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values iexpcert iexpcert;
label define iexpcert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values icdpcert icdpcert;
label define icdpcert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values icert    icert;  
label define icert   
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values ifldcert ifldcert;
label define ifldcert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Architecture and engineering"  
	2           "Communications"                
	3           "Computer and information sciences"
	4           "Engineering and related"       
	5           "Business management"           
	6           "Business support"              
	7           "Marketing"                     
	8           "Health professions, except nursing"
	9           "Nursing"                       
	10          "Health technologists and"      
	11          "Health aides"                  
	12          "Cosmetology"                   
	13          "Culinary arts"                 
	14          "Personal services(other than"  
	15          "Protective services"           
	16          "Public and social services(other"
	17          "Education"                     
	18          "Construction trades"           
	19          "Manufacturing"                 
	20          "Mechanic and repair technologies"
	21          "Transportation and material moving"
	22          "OTHER"                         
;
label values ischcert ischcert;
label define ischcert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "A community college"           
	2           "A university or college other" 
	3           "A trade, vocational, technical,"
	4           "Business or company"           
	5           "Professional organization"     
	6           "Trade union"                   
	7           "Non-profit organization"       
	8           "Federal, state, or local"      
	9           "Military"                      
	10          "Someplace else"                
;
label values isdycert isdycert;
label define isdycert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Mainly self-study"             
	2           "Mainly Instructor"             
;
label values itimcert itimcert;
label define itimcert
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
	1           "Less than one week"            
	2           "One week to one month"         
	3           "More than one month"           
;

#delimit cr
desc,short

sort ssuid shhadid eentaid epppnum swave srotaton 
saveold `dta_name' , replace



** Copyright 2014 shared by the National Bureau of Economic Research and Jean Roth ;

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
