log using sip93t7, text replace
set mem 1000m
*This program reads the 1993 SIPP Wave 7 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Mar 18 11:54:54 EST 2004
*Please report errors to jroth@nber.org
*run with do sip93t7
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1993\sip93t7.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip93t7"

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
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other type a"                  
	9           "Vacant"                        
	10          "Occupied by persons with URE"  
	11          "Unfit or to be demolished"     
	12          "Under construction, not ready" 
	13          "Converted to temporary business"
	14          "Unoccupied site for mobile"    
	15          "Permit granted, construction"  
	17          "Demolished"                    
	18          "House or trailer moved"        
	19          "Converted to permanent business"
	20          "Merged"                        
	21          "Condemned"                     
	23          "Entire household deceased,"    
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
	28          "Merged HHLDS across panels"    
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
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
	1           "1 year etc."                   
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
	0           "Not applicable if under 15, did"
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
label values tm8526   tm8526l;
label define tm8526l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4800"           
;
label values tm8528   tm8528l;
label define tm8528l 
	0           "Not applicable"                
	1           "Yes - skip to TM8608"          
	2           "No"                            
;
label values tm8530   tm8530l;
label define tm8530l 
	0           "Not applicable"                
	1           "Owned or being bought"         
	2           "Rented for cash - skip to TM8638"
	3           "Occupied without cash payment -"
;
label values tm8538   tm8538l;
label define tm8538l 
	0           "Not applicable"                
;
label values tm8539   tm8539l;
label define tm8539l 
	0           "Not applicable"                
	1995        "Year"                          
;
label values tm8540   tm8540l;
label define tm8540l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8598"           
;
label values tm8564   tm8564l;
label define tm8564l 
	0           "Not applicable"                
;
label values tm8568   tm8568l;
label define tm8568l 
	0           "Not applicable - skip to TM8572"
	1995        "Year - skip to TM8572"         
;
label values tm8569   tm8569l;
label define tm8569l 
	0           "Not applicable"                
;
label values tm8572   tm8572l;
label define tm8572l 
	0           "Not applicable"                
;
label values tm8576   tm8576l;
label define tm8576l 
	0           "Not applicable"                
	-8          "Not fixed"                     
;
label values tm8580   tm8580l;
label define tm8580l 
	0           "Not applicable"                
	9999        "Interest rate (percent)"       
;
label values tm8584   tm8584l;
label define tm8584l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8587   tm8587l;
label define tm8587l 
	0           "Not applicable"                
	1           "Yes - FHA"                     
	2           "Yes - VA"                      
	3           "No"                            
;
label values tm8592   tm8592l;
label define tm8592l 
	0           "Not applicable"                
	1           "Yes - skip to TM8566"          
	2           "No - skip to TM8598"           
;
label values tm8566   tm8566l;
label define tm8566l 
	0           "Not applicable"                
;
label values tm8570   tm8570l;
label define tm8570l 
	0           "Not applicable - skip to TM8574"
	1995        "Year - skip to TM8574"         
;
label values tm8571   tm8571l;
label define tm8571l 
	0           "Not applicable"                
;
label values tm8574   tm8574l;
label define tm8574l 
	0           "Not applicable"                
;
label values tm8578   tm8578l;
label define tm8578l 
	0           "Not applicable"                
	-8          "Not fixed"                     
;
label values tm8582   tm8582l;
label define tm8582l 
	0           "Not applicable"                
	9999        "Interest rate (percent)"       
;
label values tm8586   tm8586l;
label define tm8586l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8589   tm8589l;
label define tm8589l 
	0           "Not applicable"                
	1           "Yes - FHA"                     
	2           "Yes - VA"                      
	3           "No"                            
;
label values tm8594   tm8594l;
label define tm8594l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8598"           
;
label values tm8596   tm8596l;
label define tm8596l 
	0           "Not applicable"                
;
label values tm8598   tm8598l;
label define tm8598l 
	0           "Not applicable - skip to TM8638"
;
label values tm8608   tm8608l;
label define tm8608l 
	0           "Not applicable"                
	1           "Owned"                         
	2           "Rented - skip to TM8638"       
	3           "Occupied without cash rent -"  
;
label values tm8610   tm8610l;
label define tm8610l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8630"           
;
label values tm8612   tm8612l;
label define tm8612l 
	0           "Not applicable"                
	1           "Mobile home only"              
	2           "Site only"                     
	3           "Site and home"                 
;
label values tm8624   tm8624l;
label define tm8624l 
	0           "Not applicable"                
	48000       "Total amount"                  
;
label values tm8630   tm8630l;
label define tm8630l 
	0           "Not applicable"                
	75000       "Total amount - skip to"        
;
label values tm8638   tm8638l;
label define tm8638l 
	0           "Not applicable"                
	-1          "Don't know"                    
	-2          "Refused"                       
	-3          "None"                          
;
label values tm8640   tm8640l;
label define tm8640l 
	0           "Not applicable"                
	-1          "Don't know"                    
	-2          "Refused"                       
	-3          "None"                          
;
label values tm8642   tm8642l;
label define tm8642l 
	0           "not applicable"                
	1           "One person household - skip to"
	2           "Married-couple household, no"  
	3           "Single parent household, no"   
	4           "Other composition"             
;
label values tm8644   tm8644l;
label define tm8644l 
	0           "Not applicable"                
	1           "Yes - skip to 8647"            
	2           "No"                            
;
label values tm8646   tm8646l;
label define tm8646l 
	999         "No mortgage/rent or utilities" 
;
label values tm8650   tm8650l;
label define tm8650l 
	0           "Not applicable"                
;
label values tm8651   tm8651l;
label define tm8651l 
	0           "Not applicable"                
;
label values tm8652   tm8652l;
label define tm8652l 
	0           "Not applicable"                
;
label values tm8654   tm8654l;
label define tm8654l 
	0           "Not applicable"                
	1           "One - skip to TM8658"          
	2           "Two or more"                   
;
label values tm8656   tm8656l;
label define tm8656l 
	0           "Not applicable"                
	1           "One - skip to TM8658"          
	2           "Two or more"                   
;
label values tm8657   tm8657l;
label define tm8657l 
	0           "Not applicable"                
;
label values tm8658   tm8658l;
label define tm8658l 
	0           "Not applicable"                
	1           "In a public housing project -" 
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
	1995        "Year"                          
;
label values tm8754   tm8754l;
label define tm8754l 
	0           "Not applicable"                
	1           "Money owed"                    
	2           "Free and clear - skip to TM8763"
;
label values tm8760   tm8760l;
label define tm8760l 
	0           "Not applicable"                
	22000       "Total amount"                  
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
	1995        "Year"                          
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
	22000       "Total amount"                  
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
	1995        "Year"                          
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
	22000       "Total amount"                  
;
label values tm8765   tm8765l;
label define tm8765l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8788   tm8788l;
label define tm8788l 
	0           "Not applicable"                
	25000       "Total amount"                  
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
	40000       "Total amount"                  
;
label values tm8800   tm8800l;
label define tm8800l 
	0           "Not applicable"                
	1           "Yes - skip to TM8782"          
	2           "No - skip to SC4800"           
;
label values tm8790   tm8790l;
label define tm8790l 
	0           "Not applicable"                
	35000       "Total amount"                  
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
	56000       "Total amount - skip to"        
;
label values atsum01  atsum01l;
label define atsum01l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum02  atsum02l;
label define atsum02l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum03  atsum03l;
label define atsum03l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum04  atsum04l;
label define atsum04l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum05  atsum05l;
label define atsum05l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum06  atsum06l;
label define atsum06l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum07  atsum07l;
label define atsum07l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum08  atsum08l;
label define atsum08l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum09  atsum09l;
label define atsum09l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum10  atsum10l;
label define atsum10l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum11  atsum11l;
label define atsum11l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum12  atsum12l;
label define atsum12l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum13  atsum13l;
label define atsum13l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum14  atsum14l;
label define atsum14l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum15  atsum15l;
label define atsum15l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum16  atsum16l;
label define atsum16l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum17  atsum17l;
label define atsum17l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum18  atsum18l;
label define atsum18l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum19  atsum19l;
label define atsum19l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values atsum20  atsum20l;
label define atsum20l
	0           "Not applicable or no"          
	1           "Yes"                           
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC2262"           
;
label values tm8002   tm8002l;
label define tm8002l 
	0           "Not applicable"                
	1           "Yes - skip to TM8006"          
	2           "No"                            
;
label values tm8004   tm8004l;
label define tm8004l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8006   tm8006l;
label define tm8006l 
	0           "Not applicable"                
	1           "Yes - skip to SC2262"          
	2           "No"                            
;
label values tm8008   tm8008l;
label define tm8008l 
	0           "Not applicable"                
	-3          "None - skip to TM8012"         
;
label values tm8012   tm8012l;
label define tm8012l 
	0           "Not applicable"                
	-3          "None - skip to SC2262"         
;
label values tm8016   tm8016l;
label define tm8016l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8200"           
;
label values tm8018   tm8018l;
label define tm8018l 
	0           "Not applicable"                
	1           "Yes - skip to TM8022"          
	2           "No"                            
;
label values tm8020   tm8020l;
label define tm8020l 
	0           "Not applicable"                
;
label values tm8022   tm8022l;
label define tm8022l 
	0           "Not applicable"                
	1           "Yes - skip to TM8200"          
	2           "No"                            
;
label values tm8024   tm8024l;
label define tm8024l 
	0           "Not applicable"                
	-3          "None - skip to TM8028"         
;
label values tm8028   tm8028l;
label define tm8028l 
	0           "Not applicable"                
	-3          "None - skip to TM8200"         
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
	999999999   "Total amount - skip"           
	-3          "None - skip to TM8042"         
;
label values tm8038   tm8038l;
label define tm8038l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8042"           
;
label values tm8040   tm8040l;
label define tm8040l 
	0           "Not applicable"                
	999999      "Total amount  TM8200"          
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
	999999999   "Total amount - skip"           
;
label values tm8048   tm8048l;
label define tm8048l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code or" 
;
label values tm8050   tm8050l;
label define tm8050l 
	0           "Not applicable"                
	999999      "Total amount - skip"           
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
label values tm8067   tm8067l;
label define tm8067l 
	0           "Not applicable"                
	1           "Yes - all rental properties on"
	2           "Yes - some rental properties on"
	3           "No"                            
;
label values tm8068   tm8068l;
label define tm8068l 
	0           "Not applicable"                
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
label values tm8091   tm8091l;
label define tm8091l 
	0           "Not applicable"                
	1           "Yes - all rental properties on"
	2           "Yes - some rental properties"  
	3           "No"                            
;
label values tm8092   tm8092l;
label define tm8092l 
	0           "Not applicable"                
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
	-3          "None - skip to next ISS code"  
;
label values tm8116   tm8116l;
label define tm8116l 
	0           "Not applicable"                
;
label values tm8118   tm8118l;
label define tm8118l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8122"           
;
label values tm8120   tm8120l;
label define tm8120l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8122   tm8122l;
label define tm8122l 
	0           "Not applicable"                
;
label values tm8126   tm8126l;
label define tm8126l 
	0           "Not applicable"                
	999999999   "Total amount"                  
	-3          "None"                          
;
label values tm8128   tm8128l;
label define tm8128l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8130   tm8130l;
label define tm8130l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8200"           
;
label values tm8132   tm8132l;
label define tm8132l 
	0           "Not applicable"                
	999999999   "Total amount - skip"           
	-3          "None - skip to TM8200"         
;
label values tm8200   tm8200l;
label define tm8200l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8204"           
;
label values tm8202   tm8202l;
label define tm8202l 
	0           "Not in universe"               
	999999999   "Total amount"                  
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
	15000       "Total amount"                  
;
label values tm8208   tm8208l;
label define tm8208l 
	0           "Not applicable"                
	1           "No spouse in household - skip" 
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
label values tm8212   tm8212l;
label define tm8212l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8214   tm8214l;
label define tm8214l 
	0           "Not applicable"                
	999999      "Total amount"                  
;
label values tm8216   tm8216l;
label define tm8216l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8218   tm8218l;
label define tm8218l 
	0           "Not applicable"                
	999999      "Total amount"                  
;
label values tm8220   tm8220l;
label define tm8220l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8222   tm8222l;
label define tm8222l 
	0           "Not applicable"                
	999999      "Total amount"                  
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
label values tm8234   tm8234l;
label define tm8234l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8258"           
;
label values tm8236   tm8236l;
label define tm8236l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8238   tm8238l;
label define tm8238l 
	0           "Not applicable"                
	999999      "Total amount"                  
;
label values tm8240   tm8240l;
label define tm8240l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8242   tm8242l;
label define tm8242l 
	0           "Not applicable"                
	999999      "Total amount"                  
;
label values tm8244   tm8244l;
label define tm8244l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8246   tm8246l;
label define tm8246l 
	0           "Not applicable"                
	999999      "Total amount"                  
;
label values tm8258   tm8258l;
label define tm8258l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8308"           
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
	120000      "Total amount - skip"           
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
	339000      "Total amount -"                
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8400"           
;
label values tm8309   tm8309l;
label define tm8309l 
	0           "Not applicable"                
;
label values tm8312   tm8312l;
label define tm8312l 
	0           "Not applicable"                
	1           "Term only"                     
	2           "Whole life only"               
	3           "Both types"                    
;
label values tm8314   tm8314l;
label define tm8314l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8400"           
;
label values tm8316   tm8316l;
label define tm8316l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8400"           
;
label values tm8318   tm8318l;
label define tm8318l 
	0           "Not applicable"                
	300000      "Face value of life insurance"  
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
	-1          "Don't know"                    
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
	2           "No - skip to TM8412"           
;
label values tm8410   tm8410l;
label define tm8410l 
	0           "Not applicable"                
;
label values tm8412   tm8412l;
label define tm8412l 
	0           "Not applicable"                
	1           "15 years old - skip to TM8526" 
	2           "16 - 67 years old"             
	3           "68 years old or older - skip to"
;
label values tm8413   tm8413l;
label define tm8413l 
	0           "Not applicable"                
	1           "Item 18a is blank"             
	2           "'yes' in item 18a - skip to"   
	3           "'no' in item 18a - skip to"    
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
	1           "Yes - mark '171' on ISS"       
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
