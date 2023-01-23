log using sip91t4, text replace
set mem 1000m
*This program reads the 1991 SIPP Wave 4 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 18:24:27 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip91t4
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1991\sip91t4.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip91t4"

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
	61          "Maine,Vermont"                 
	62          "Iowa,North Dakota,South Dakota"
	63          "Alaska,Idaho,Montana,Wyoming"  
;
label values item36b  item36b;
label define item36b 
	1           "Interviewed"                   
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
	14          "Unoccupied site for mobile"    
	15          "Permit granted, construction"  
	17          "Demolished"                    
	18          "House or trailer moved"        
	19          "Converted to permanent"        
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
	3           "Noninterview - type Z refusal" 
	4           "Noninterview - type Z other"   
;
label values pp_mis1  pp_mis1l;
label define pp_mis1l
	1           "Interview"                     
	2           "Non-interview"                 
;
label values pp_mis2  pp_mis2l;
label define pp_mis2l
	1           "Interview"                     
	2           "Non-interview"                 
;
label values pp_mis3  pp_mis3l;
label define pp_mis3l
	1           "Interview"                     
	2           "Non-interview"                 
;
label values pp_mis4  pp_mis4l;
label define pp_mis4l
	1           "Interview"                     
	2           "Non-interview"                 
;
label values pp_mis5  pp_mis5l;
label define pp_mis5l
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
	3           "American Indian, Eskimo or"    
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
label values sc4314   sc4314l;
label define sc4314l 
	3           "None"                          
	0           "Not in universe"               
;
label values sc4322   sc4322l;
label define sc4322l 
	3           "None"                          
	0           "Not in universe"               
;
label values sc4414   sc4414l;
label define sc4414l 
	3           "None"                          
	0           "Not in universe"               
;
label values sc4422   sc4422l;
label define sc4422l 
	3           "None"                          
	0           "Not in universe"               
;
label values tm8032   tm8032l;
label define tm8032l 
	0           "Not applicable"                
	1           "No spouse in household"        
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values tm8034   tm8034l;
label define tm8034l 
	0           "Not applicable"                
	-3          "None - skip to TM8042"         
;
label values tm8036   tm8036l;
label define tm8036l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8042   tm8042l;
label define tm8042l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code or" 
;
label values tm8044   tm8044l;
label define tm8044l 
	0           "Not applicable"                
;
label values tm8046   tm8046l;
label define tm8046l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8052   tm8052l;
label define tm8052l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4610"           
;
label values tm8054   tm8054l;
label define tm8054l 
	0           "Not applicable"                
	-3          "None - skip to SC4610"         
;
label values tm8056   tm8056l;
label define tm8056l 
	0           "Not applicable"                
	1           "Yes - all rental properties on"
	2           "Yes - some rental properties on"
	3           "No"                            
;
label values tm8068   tm8068l;
label define tm8068l 
	0           "Not applicable"                
;
label values tm8070   tm8070l;
label define tm8070l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8072   tm8072l;
label define tm8072l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4610"           
;
label values tm8074   tm8074l;
label define tm8074l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8076   tm8076l;
label define tm8076l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -skip to SC4618"            
;
label values tm8078   tm8078l;
label define tm8078l 
	0           "Not applicable"                
	-3          "None - skip to SC4618"         
;
label values tm8080   tm8080l;
label define tm8080l 
	0           "Not applicable"                
	1           "Yes - all rental properties on"
	2           "Yes - some rental properties on"
	3           "No"                            
;
label values tm8092   tm8092l;
label define tm8092l 
	0           "Not applicable"                
;
label values tm8094   tm8094l;
label define tm8094l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8096   tm8096l;
label define tm8096l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4618"           
;
label values tm8098   tm8098l;
label define tm8098l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8100   tm8100l;
label define tm8100l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -skip to next ISS code or"  
;
label values tm8102   tm8102l;
label define tm8102l 
	0           "Not applicable"                
	-3          "None - skip to next ISS"       
;
label values tm8116   tm8116l;
label define tm8116l 
	0           "Not applicable"                
;
label values tm8118   tm8118l;
label define tm8118l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8120   tm8120l;
label define tm8120l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8122   tm8122l;
label define tm8122l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8124   tm8124l;
label define tm8124l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8130   tm8130l;
label define tm8130l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8132   tm8132l;
label define tm8132l 
	0           "Not applicable"                
	-3          "None - skip to TM8200"         
;
label values tm8204   tm8204l;
label define tm8204l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8208"           
;
label values tm8206   tm8206l;
label define tm8206l 
	0           "Not applicable"                
;
label values tm8208   tm8208l;
label define tm8208l 
	0           "Not applicable"                
	1           "No spouse in household -"      
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values tm8209   tm8209l;
label define tm8209l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8212"           
;
label values tm8210   tm8210l;
label define tm8210l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8232   tm8232l;
label define tm8232l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8234"           
;
label values tm8233   tm8233l;
label define tm8233l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8258   tm8258l;
label define tm8258l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8526"           
;
label values tm8260   tm8260l;
label define tm8260l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8284"           
;
label values tm8262   tm8262l;
label define tm8262l 
	0           "Not applicable"                
;
label values tm8264   tm8264l;
label define tm8264l 
	0           "Not applicable"                
;
label values tm8266   tm8266l;
label define tm8266l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8284   tm8284l;
label define tm8284l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8308"           
;
label values tm8286   tm8286l;
label define tm8286l 
	0           "Not applicable"                
;
label values tm8288   tm8288l;
label define tm8288l 
	0           "Not applicable"                
;
label values tm8290   tm8290l;
label define tm8290l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8324"           
;
label values tm8310   tm8310l;
label define tm8310l 
	0           "Not applicable"                
;
label values tm8312   tm8312l;
label define tm8312l 
	0           "Not applicable"                
	1           "Term only"                     
	2           "Whole life only"               
	3           "Both types"                    
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8404   tm8404l;
label define tm8404l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8406   tm8406l;
label define tm8406l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8408   tm8408l;
label define tm8408l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8410   tm8410l;
label define tm8410l 
	0           "Not applicable"                
;
label values tm8412   tm8412l;
label define tm8412l 
	0           "Not applicable"                
	1           "15 years old - skip to TM8526" 
	2           "16-67 years old"               
	3           "68 years old or older -"       
;
label values tm8414   tm8414l;
label define tm8414l 
	0           "Not applicable"                
	1           "Yes - skip to TM8418"          
	2           "No"                            
;
label values tm8416   tm8416l;
label define tm8416l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8420"           
;
label values tm8418   tm8418l;
label define tm8418l 
	0           "Not applicable"                
	1           "Yes - skip to TM8422"          
	2           "No - skip to TM8526"           
;
label values tm8420   tm8420l;
label define tm8420l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8526"           
;
label values tm8422   tm8422l;
label define tm8422l 
	0           "Not applicable"                
	1           "Yes - skip to TM8526"          
	2           "No"                            
;
label values tm8424   tm8424l;
label define tm8424l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8526"           
;
label values tm8426   tm8426l;
label define tm8426l 
	0           "Not applicable"                
	1           "Yes - skip to TM8526"          
	2           "No"                            
;
label values tm8428   tm8428l;
label define tm8428l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values imp8400  imp8400l;
label define imp8400l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8402  imp8402l;
label define imp8402l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8404  imp8404l;
label define imp8404l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8406  imp8406l;
label define imp8406l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8410  imp8410l;
label define imp8410l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8418  imp8418l;
label define imp8418l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8420  imp8420l;
label define imp8420l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8424  imp8424l;
label define imp8424l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8426  imp8426l;
label define imp8426l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8428  imp8428l;
label define imp8428l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8526   tm8526l;
label define tm8526l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4800"           
;
label values tm8530   tm8530l;
label define tm8530l 
	0           "Not applicable"                
	1           "Owned or being bought"         
	2           "Rented for cash - skip to"     
	3           "Occupied without cash payment" 
;
label values tm8538   tm8538l;
label define tm8538l 
	-3          "None"                          
	0           "Not applicable"                
;
label values tm8540   tm8540l;
label define tm8540l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8542   tm8542l;
label define tm8542l 
	0           "Not applicable"                
	1           "One person household - skip to"
	2           "Married-couple household, no"  
	3           "Single parent household, no"   
	4           "Other composition"             
;
label values tm8544   tm8544l;
label define tm8544l 
	0           "Not applicable"                
	1           "Yes - skip to TM8548"          
	2           "No"                            
;
label values tm8546   tm8546l;
label define tm8546l 
	999         "No rent/mortgage or utilities" 
;
label values tm8554   tm8554l;
label define tm8554l 
	0           "Not applicable"                
;
label values tm8556   tm8556l;
label define tm8556l 
	0           "Not applicable"                
;
label values tm8558   tm8558l;
label define tm8558l 
	0           "Not applicable"                
;
label values tm8560   tm8560l;
label define tm8560l 
	0           "Not applicable"                
	2           "Two or more"                   
;
label values tm8562   tm8562l;
label define tm8562l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8658"           
;
label values tm8564   tm8564l;
label define tm8564l 
	0           "Not applicaable"               
;
label values tm8658   tm8658l;
label define tm8658l 
	0           "Not applicable"                
	1           "In a public housing project"   
	2           "Subsidized - skip to TM8714"   
	3           "Neither public nor subsidized" 
;
label values tm8660   tm8660l;
label define tm8660l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8714"           
;
label values tm8662   tm8662l;
label define tm8662l 
	0           "Not applicable"                
;
label values tm8664   tm8664l;
label define tm8664l 
	0           "Not applicable"                
;
label values tm8666   tm8666l;
label define tm8666l 
	0           "Not applicable"                
;
label values tm8714   tm8714l;
label define tm8714l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8770"           
;
label values tm8716   tm8716l;
label define tm8716l 
	0           "Not applicable"                
;
label values tm8730   tm8730l;
label define tm8730l 
	0           "Not applicable"                
;
label values tm8754   tm8754l;
label define tm8754l 
	0           "Not applicable"                
	1           "Money owed"                    
	2           "Free and clear - skip to TM8766"
;
label values tm8760   tm8760l;
label define tm8760l 
	0           "Not applicable"                
;
label values tm8763   tm8763l;
label define tm8763l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8766   tm8766l;
label define tm8766l 
	0           "Not applicable"                
	1           "Yes - skip to TM8720 for"      
	2           "No - skip to TM8770"           
;
label values tm8732   tm8732l;
label define tm8732l 
	0           "Not applicable"                
;
label values tm8756   tm8756l;
label define tm8756l 
	0           "Not applicable"                
	1           "Money owed"                    
	2           "Free and clear - skip to TM8768"
;
label values tm8761   tm8761l;
label define tm8761l 
	0           "Not applicable"                
;
label values tm8764   tm8764l;
label define tm8764l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8768   tm8768l;
label define tm8768l 
	0           "Not applicable"                
	1           "Yes - skip to TM8722 for"      
	2           "No - skip to TM8770"           
;
label values tm8734   tm8734l;
label define tm8734l 
	0           "Not applicable"                
;
label values tm8758   tm8758l;
label define tm8758l 
	0           "Not applicable"                
	1           "Money owed"                    
	2           "Free and clear - skip to TM8770"
;
label values tm8762   tm8762l;
label define tm8762l 
	0           "Not applicable"                
;
label values tm8765   tm8765l;
label define tm8765l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8780   tm8780l;
label define tm8780l 
	0           "Not a legal person number or"  
;
label values tm8784   tm8784l;
label define tm8784l 
	0           "Not a legal person number or"  
;
label values tm8788   tm8788l;
label define tm8788l 
	0           "Not applicable"                
;
label values tm8792   tm8792l;
label define tm8792l 
	0           "Not applicable"                
	1           "Money owed"                    
	2           "Free and clear - skip to TM8800"
;
label values tm8796   tm8796l;
label define tm8796l 
	0           "Not applicable"                
;
label values tm8800   tm8800l;
label define tm8800l 
	0           "Not applicable"                
	1           "Yes - skip to TM8782"          
	2           "No - skip to SC4800"           
;
label values tm8782   tm8782l;
label define tm8782l 
	0           "Not a legal person number or"  
;
label values tm8786   tm8786l;
label define tm8786l 
	0           "Not a legal person number or"  
;
label values tm8790   tm8790l;
label define tm8790l 
	0           "Not applicable"                
;
label values tm8794   tm8794l;
label define tm8794l 
	0           "Not applicable"                
	1           "Money owed"                    
	2           "Free and clear - skip to SC4800"
;
label values tm8798   tm8798l;
label define tm8798l 
	0           "Not applicable - skip to SC4800"
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
