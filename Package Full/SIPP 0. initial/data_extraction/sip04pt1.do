log using sip04t1, text replace
set mem 1000m

/*------------------------------------------------

  This program reads the 2004 SIPP Wave 1 Topical Module Data File 
  Note:  This program is distributed under the GNU GPL. See end of
  this file and http://www.gnu.org/licenses/ for details.
  by Jean Roth Mon Apr 10 14:42:09 EDT 2006
  Please report errors to jroth@nber.org
  run with do sip04t1
----------------------------------------------- */

/* The following line should contain
   the complete path and name of the raw data file.
   On a PC, use backslashes in paths as in C:\  */

local dat_name "sipp04pt1.dat"

/* The following line should contain the path to your output '.dta' file */

local dta_name "./sipp04pt1.dta"

/* The following line should contain the path to the data dictionary file */

local dct_name "./sip04pt1.dct"

/* The line below does NOT need to be changed */

quietly infile using "`dct_name'", using("`dat_name'") clear

/*------------------------------------------------

  Decimal places have been made explict in the dictionary file.
  Stata resolves a missing value of -1 / # of decimal places as a missing value.

 -----------------------------------------------*/

*Everything below this point, aside from the final save, are value labels

#delimit ;

;
label values spanel   spanel; 
label define spanel  
	1996        "Panel Year"                    
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
	62          "North Dakota, South Dakota,"   
;
label values sinthhid sinthhid;
label define sinthhid
	0           "Not in universe"               
;
label values eoutcome eoutcome;
label define eoutcome
	201         "Completed interview"           
	203         "Compl. partial- missing data; no"
	207         "Complete partial - TYPE-Z; no" 
	213         "TYPE-A, language problem"      
	215         "TYPE-A, insufficient partial"  
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
	261         "TYPE-D, moved w/in U.S. but"   
	262         "Merged with another SIPP household"
	270         "Mover, no longer located in same"
	271         "Mover, new address located in" 
	280         "Newly spawned case outside fr's"
;
label values rfid2    rfid2l; 
label define rfid2l  
	0           "Member of related subfamily"   
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
	4           "Nonintrvw - pseudo Type Z.  Left"
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
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Aleut, or Eskimo"
	4           "Asian or Pacific Islander"     
;
label values eorigin  eorigin;
label define eorigin 
	1           "Canadian"                      
	10          "Polish"                        
	11          "Russian"                       
	12          "Scandinavian"                  
	13          "Scotch-Irish"                  
	14          "Scottish"                      
	15          "Slovak"                        
	16          "Welsh"                         
	17          "Other European"                
	2           "Dutch"                         
	20          "Mexican"                       
	21          "Mexican-American"              
	22          "Chicano"                       
	23          "Puerto Rican"                  
	24          "Cuban"                         
	25          "Central American"              
	26          "South American"                
	27          "Dominican Republic"            
	28          "Other Hispanic"                
	3           "English"                       
	30          "African-American or Afro-American"
	31          "American Indian, Eskimo, or Aleut"
	32          "Arab"                          
	33          "Asian"                         
	34          "Pacific Islander"              
	35          "West Indian"                   
	39          "Another group not listed"      
	4           "French"                        
	40          "American"                      
	5           "French-Canadian"               
	6           "German"                        
	7           "Hungarian"                     
	8           "Irish"                         
	9           "Italian"                       
;
label values wpfinwgt wpfinwgt;
label define wpfinwgt
	0           "0000:999999.9999 .Final person weight"
;
label values errp     errp;   
label define errp    
	1           "Reference person w/ rel. persons"
	10          "Unmarried partner of reference"
	11          "Housemate/roommate"            
	12          "Roomer/boarder"                
	13          "Other non-relative of reference"
	2           "Reference Person w/out rel."   
	3           "Spouse of reference person"    
	4           "Child of reference person"     
	5           "Grandchild of reference person"
	6           "Parent of reference person"    
	7           "Brother/sister of reference person"
	8           "Other relative of reference person"
	9           "Foster child of reference person"
;
label values tage     tage;   
label define tage    
	0           "Less than 1 full year old"     
;
label values ems      ems;    
label define ems     
	1           "Married, spouse present"       
	2           "Married, Spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never Married"                 
;
label values epnspous epnspous;
label define epnspous
	9999        "Spouse not in hhld or person not"
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
	-1          "Not in universe"               
	9999        "Guardian not in household"     
;
label values rdesgpnt rdesgpnt;
label define rdesgpnt
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values eeducate eeducate;
label define eeducate
	-1          "Not in universe"               
	31          "Less than 1st grade"           
	32          "1st, 2nd, 3rd or 4th grade"    
	33          "5th or 6th grade"              
	34          "7th or 8th grade"              
	35          "9th grade"                     
	36          "10th grade"                    
	37          "11th grade"                    
	38          "12th grade"                    
	39          "High school graduate - high"   
	40          "Some college but no degree"    
	41          "Diploma or certificate from a" 
	42          "Associate degree in college -" 
	43          "Associate Degree in college -" 
	44          "Bachelors degree (For example:"
	45          "Master's degree (For example: MA,"
	46          "Professional School Degree (For"
	47          "Doctorate degree (For example:"
;
label values earcunv  earcunv;
label define earcunv 
	1           "In universe"                   
	-1          "Not in universe"               
;
label values ecurafdc ecurafdc;
label define ecurafdc
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values acurafdc acurafdc;
label define acurafdc
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values eevrgard eevrgard;
label define eevrgard
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values avergard avergard;
label define avergard
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values eaplafdc eaplafdc;
label define eaplafdc
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values aaplafdc aaplafdc;
label define aaplafdc
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values ercvafdc ercvafdc;
label define ercvafdc
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values arcvafdc arcvafdc;
label define arcvafdc
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tafdcsty tafdcsty;
label define tafdcsty
	-1          "Not in universe"               
;
label values aafdcsty aafdcsty;
label define aafdcsty
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tafdcly  tafdcly;
label define tafdcly 
	-1          "Not in universe"               
;
label values aafdcly  aafdcly;
label define aafdcly 
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tafdctim tafdctim;
label define tafdctim
	1           "One time on ADFC/TANF"         
	2           "Two times on ADFC/TANF"        
	3           "Three or more times on ADFC/TANF"
	-1          "Not in universe"               
;
label values aafdctim aafdctim;
label define aafdctim
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values ecurssi  ecurssi;
label define ecurssi 
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values acurssi  acurssi;
label define acurssi 
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values eaplssi  eaplssi;
label define eaplssi 
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values aaplssi  aaplssi;
label define aaplssi 
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values erecvssi erecvssi;
label define erecvssi
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values arecvssi arecvssi;
label define arecvssi
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tssistry tssistry;
label define tssistry
	-1          "Not in universe"               
;
label values assistry assistry;
label define assistry
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tssily   tssily; 
label define tssily  
	-1          "Not in universe"               
;
label values assily   assily; 
label define assily  
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values ecurfs   ecurfs; 
label define ecurfs  
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values acurfs   acurfs; 
label define acurfs  
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values eaplfs   eaplfs; 
label define eaplfs  
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values aaplfs   aaplfs; 
label define aaplfs  
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values erecvfs  erecvfs;
label define erecvfs 
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values arecvfs  arecvfs;
label define arecvfs 
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tfsstryr tfsstryr;
label define tfsstryr
	-1          "Not in universe"               
;
label values afsstryr afsstryr;
label define afsstryr
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tfsly    tfsly;  
label define tfsly   
	-1          "Not in universe"               
;
label values afsly    afsly;  
label define afsly   
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;
label values tfstimes tfstimes;
label define tfstimes
	1           "One time on food stamps"       
	2           "Two times on food stamps"      
	3           "Three or more times on food stamps"
	-1          "Not in universe"               
;
label values afstimes afstimes;
label define afstimes
	0           "Not imputed"                   
	1           "Statistical imputation (hot deck)"
	2           "Cold deck imputation"          
	3           "Logical imputation (derivation)"
;

#delimit cr
save `dta_name' , replace

/*
Copyright 2006 shared by the National Bureau of Economic Research and Jean Roth

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
