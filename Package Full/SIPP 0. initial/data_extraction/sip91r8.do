log using sip91r8, text replace
set mem 1000m
*This program reads the 1991 SIPP Wave 8 Topical Module Research Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 18:29:46 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip91r8
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1991\sip91r8.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip91r8"

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
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not applicable"                
	1           "Yes- skip to TM8006"           
	2           "No"                            
;
label values tm8002   tm8002l;
label define tm8002l 
	0           "Not applicable"                
	1           "Yes- skip to TM9330"           
	2           "No"                            
;
label values tm8004   tm8004l;
label define tm8004l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9330"           
;
label values tm8006   tm8006l;
label define tm8006l 
	1           "1 business"                    
	2           "2 businesses"                  
	3           "3 + businesses"                
;
label values tmind1   tmind1l;
label define tmind1l 
	0           "Not applicable"                
;
label values tm8010   tm8010l;
label define tm8010l 
	-3          "Not listed on control card"    
	0           "Not applicable"                
;
label values tm8012   tm8012l;
label define tm8012l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8018"           
;
label values tm8014   tm8014l;
label define tm8014l 
	0           "Not applicable"                
;
label values tm8016   tm8016l;
label define tm8016l 
	-3          "None - skip to TM8274"         
	0           "Not applicable"                
;
label values tm8018   tm8018l;
label define tm8018l 
	-1          "DK"                            
	0           "Not applicable"                
	1           "Sole proprietorship"           
	2           "Partnership"                   
	3           "Corporation"                   
;
label values tm8020   tm8020l;
label define tm8020l 
	0           "Not applicable"                
	1           "Own home"                      
	2           "Somewhere else"                
;
label values tm8104   tm8104l;
label define tm8104l 
	0           "Not applicable"                
	1           "Yes- skip to TM8118"           
	2           "No"                            
;
label values tm8106   tm8106l;
label define tm8106l 
	-1          "DK - skip to TM8116"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8116"           
;
label values tm8108   tm8108l;
label define tm8108l 
	0           "Not applicable"                
;
label values tm8110   tm8110l;
label define tm8110l 
	0           "Not applicable"                
;
label values tm8112   tm8112l;
label define tm8112l 
	0           "Not applicable"                
	1           "Yes- skip to TM8116"           
	2           "No"                            
;
label values tm8114   tm8114l;
label define tm8114l 
	-1          "DK"                            
	1           "1- 50 percent"                 
	2           "51 - 100 percent"              
;
label values tm8116   tm8116l;
label define tm8116l 
	-1          "DK"                            
	1           "1 - 25 percent"                
	2           "26 -  49 percent"              
	3           "50 - 100 percent"              
;
label values tm8118   tm8118l;
label define tm8118l 
	-1          "DK"                            
	-2          "Refused"                       
	0           "Not applicable"                
;
label values tm8120   tm8120l;
label define tm8120l 
	0           "Not applicable"                
	-1          "DK"                            
	-2          "Refused"                       
;
label values tm8122   tm8122l;
label define tm8122l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8126"           
;
label values tm8124   tm8124l;
label define tm8124l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8126   tm8126l;
label define tm8126l 
	0           "Not applicable"                
	1           "Yes- skip to TM8274"           
	2           "No"                            
;
label values tm8202   tm8202l;
label define tm8202l 
	-1          "DK"                            
	-2          "Refused - skip to TM8208"      
	-3          "None - skip to TM8208"         
	0           "Not applicable"                
;
label values tm8206   tm8206l;
label define tm8206l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8208   tm8208l;
label define tm8208l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8274"           
;
label values tm8210   tm8210l;
label define tm8210l 
	-1          "DK - skip to TM8274"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8274"           
;
label values tm8212   tm8212l;
label define tm8212l 
	0           "Not applicable"                
;
label values tm8214   tm8214l;
label define tm8214l 
	-1          "DK"                            
	-2          "Refused"                       
	-3          "None"                          
	0           "Not applicable"                
;
label values tm8218   tm8218l;
label define tm8218l 
	0           "Not applicable"                
;
label values tm8220   tm8220l;
label define tm8220l 
	-1          "DK"                            
	-2          "Refused"                       
	-3          "None"                          
	0           "Not applicable"                
;
label values tm8274   tm8274l;
label define tm8274l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Go to TM9330"             
;
label values tmind2   tmind2l;
label define tmind2l 
	0           "Not applicable"                
;
label values tm8060   tm8060l;
label define tm8060l 
	-3          "Not listed on control card"    
	0           "Not applicable"                
;
label values tm8062   tm8062l;
label define tm8062l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8068"           
;
label values tm8064   tm8064l;
label define tm8064l 
	0           "Not applicable"                
;
label values tm8066   tm8066l;
label define tm8066l 
	-3          "None - skip to TM8276"         
	0           "Not applicable"                
;
label values tm8068   tm8068l;
label define tm8068l 
	-1          "DK"                            
	0           "Not applicable"                
	1           "Sole proprietorship"           
	2           "Partnership"                   
	3           "Corporation"                   
;
label values tm8070   tm8070l;
label define tm8070l 
	0           "Not applicable"                
	1           "Own home"                      
	2           "Somewhere else"                
;
label values tm8154   tm8154l;
label define tm8154l 
	0           "Not applicable"                
	1           "Yes- skip to TM8168"           
	2           "No"                            
;
label values tm8156   tm8156l;
label define tm8156l 
	-1          "DK - skip to TM8166"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8166"           
;
label values tm8158   tm8158l;
label define tm8158l 
	0           "Not applicable"                
;
label values tm8160   tm8160l;
label define tm8160l 
	0           "Not applicable"                
;
label values tm8162   tm8162l;
label define tm8162l 
	0           "Not applicable"                
	1           "Yes- skip to TM8166"           
	2           "No"                            
;
label values tm8164   tm8164l;
label define tm8164l 
	-1          "DK"                            
	1           "1 - 50 percent"                
	2           "51 - 100 percent"              
;
label values tm8166   tm8166l;
label define tm8166l 
	-1          "DK"                            
	1           "1 -  25 percent"               
	2           "26 - 49 percent"               
	3           "50 - 100 percent"              
;
label values tm8168   tm8168l;
label define tm8168l 
	-1          "DK"                            
	-2          "Refused"                       
	0           "Not applicable"                
;
label values tm8170   tm8170l;
label define tm8170l 
	-1          "DK"                            
	-2          "Refused"                       
	0           "Not applicable"                
;
label values tm8172   tm8172l;
label define tm8172l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8176"           
;
label values tm8174   tm8174l;
label define tm8174l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8176   tm8176l;
label define tm8176l 
	0           "Not applicable"                
	1           "Yes- skip to TM8276"           
	2           "No"                            
;
label values tm8252   tm8252l;
label define tm8252l 
	-1          "DK"                            
	-2          "Refused - skip to TM8258"      
	-3          "None - skip to TM8258"         
	0           "Not applicable"                
;
label values tm8256   tm8256l;
label define tm8256l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8258   tm8258l;
label define tm8258l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8276"           
;
label values tm8260   tm8260l;
label define tm8260l 
	-1          "DK - skip to TM8276"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8276"           
;
label values tm8262   tm8262l;
label define tm8262l 
	0           "Not applicable"                
;
label values tm8264   tm8264l;
label define tm8264l 
	-1          "DK"                            
	-2          "Refused"                       
	-3          "None"                          
	0           "Not applicable"                
;
label values tm8268   tm8268l;
label define tm8268l 
	0           "Not applicable"                
;
label values tm8270   tm8270l;
label define tm8270l 
	0           "Not applicable"                
	-1          "DK"                            
	-2          "Refused"                       
	-3          "None"                          
;
label values tm8276   tm8276l;
label define tm8276l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8282"           
;
label values tm8278   tm8278l;
label define tm8278l 
	-1          "DK"                            
	-2          "Refused"                       
	-3          "None"                          
	0           "Not applicable"                
;
label values tm9330   tm9330l;
label define tm9330l 
	-1          "DK - skip to TM9358"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9358"           
;
label values tm9332   tm9332l;
label define tm9332l 
	-1          "DK - skip to TM9336"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9336"           
;
label values tm9334   tm9334l;
label define tm9334l 
	-0002       "Refused"                       
	-1          "DK"                            
	0           "Not applicable"                
;
label values tm9336   tm9336l;
label define tm9336l 
	-1          "DK - skip to TM9340"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9340"           
;
label values tm9338   tm9338l;
label define tm9338l 
	-1          "DK"                            
	-0002       "Refused"                       
	0           "Not applicable"                
;
label values tm9340   tm9340l;
label define tm9340l 
	-1          "DK"                            
	-0002       "Refused"                       
	0           "Not applicable"                
;
label values tm9356   tm9356l;
label define tm9356l 
	1           "DK"                            
;
label values tm9358   tm9358l;
label define tm9358l 
	-1          "DK - skip to TM9385"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9385"           
;
label values tm9360   tm9360l;
label define tm9360l 
	-1          "DK - skip to TM9364"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9364"           
;
label values tm9362   tm9362l;
label define tm9362l 
	-1          "DK"                            
	-0002       "Refused"                       
	0           "Not applicable"                
;
label values tm9364   tm9364l;
label define tm9364l 
	-1          "DK - skip to TM9368"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9368"           
;
label values tm9366   tm9366l;
label define tm9366l 
	-1          "DK"                            
	-0002       "Refused"                       
	0           "Not applicable"                
;
label values tm9368   tm9368l;
label define tm9368l 
	-1          ".DK"                           
	-0002       "Refused"                       
	0           "Not applicable"                
;
label values tm9384   tm9384l;
label define tm9384l 
	0           "Not applicable"                
	1           "DK"                            
;
label values tm9385   tm9385l;
label define tm9385l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9390"           
;
label values tm9386   tm9386l;
label define tm9386l 
	-1          "DK - skip to TM9390"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9390"           
;
label values tm9388   tm9388l;
label define tm9388l 
	-1          "DK"                            
	-2          "Ref"                           
	-3          "None"                          
	0           "Not applicable"                
;
label values tm9390   tm9390l;
label define tm9390l 
	0           "Not applicable"                
	1           "Yes- skip to TM9486"           
	2           "No"                            
;
label values tm9392   tm9392l;
label define tm9392l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9486"           
;
label values tm9394   tm9394l;
label define tm9394l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9396   tm9396l;
label define tm9396l 
	0           "Not applicable"                
	1           "A single taxpayer?"            
	2           "Married, filing a joint return?"
	3           "Married, filing separately?"   
	4           "Unmarried head of household?"  
	5           "Qualifying widow(er) with"     
	6           "Don't know"                    
;
label values tm9398   tm9398l;
label define tm9398l 
	-1          "DK"                            
	0           "Not applicable"                
	1           "1 exemption"                   
	2           "2 exemptions"                  
	3           "3-5 exemptions"                
	4           "6 or more exemptions"          
;
label values tm9400   tm9400l;
label define tm9400l 
	0           "Not applicable"                
	1           "One - skip to TM9414"          
	2           "Two or more"                   
;
label values tm9412   tm9412l;
label define tm9412l 
	0           "Not applicable"                
	1           "None in the household beside ..."
;
label values tm9414   tm9414l;
label define tm9414l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9420"           
;
label values tm9420   tm9420l;
label define tm9420l 
	-1          "DK - skip to TM9428"           
	0           "Not applicable"                
	1           "Form 1040"                     
	2           "Form 1040A - skip to TM9428"   
	3           "Form 1040EZ - skip to TM9428"  
;
label values tm9428   tm9428l;
label define tm9428l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9472"           
;
label values tm9430   tm9430l;
label define tm9430l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9462"           
;
label values tm9432   tm9432l;
label define tm9432l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9438"           
;
label values tm9434   tm9434l;
label define tm9434l 
	-1          "DK"                            
	-2          "Ref"                           
;
label values tm9446   tm9446l;
label define tm9446l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9448   tm9448l;
label define tm9448l 
	-1          "DK"                            
	-2          "Ref"                           
;
label values tm9450   tm9450l;
label define tm9450l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9452   tm9452l;
label define tm9452l 
	-1          "DK"                            
	-0002       "Ref"                           
	1           "1-99"                          
	2           "100-499"                       
	3           "500+"                          
;
label values tm9458   tm9458l;
label define tm9458l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9462"           
;
label values tm9460   tm9460l;
label define tm9460l 
	-1          "DK"                            
	-2          "Ref"                           
	-3          "None"                          
	0           "Not applicable"                
;
label values tm9462   tm9462l;
label define tm9462l 
	-3          "None"                          
	-2          "Ref"                           
	0           "Not applicable"                
;
label values tm9464   tm9464l;
label define tm9464l 
	-3          "None"                          
	-2          "Ref"                           
	-1          "Dk"                            
	0           "Not applicable"                
;
label values tm9466   tm9466l;
label define tm9466l 
	0           "Not applicable"                
	1           "$22,370 or more"               
	2           "Less than $22,370"             
;
label values tm9472   tm9472l;
label define tm9472l 
	-1          "DK - skip to TM9486"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9486"           
;
label values tm9474   tm9474l;
label define tm9474l 
	-1          "DK"                            
	-2          "Refused"                       
	0           "Not applicable"                
;
label values tm9486   tm9486l;
label define tm9486l 
	0           "Not applicable"                
	1           "Owned or being bought?"        
	2           "Rented for cash? - skip"       
	3           "Occupied without cash payment? -"
;
label values tm9488   tm9488l;
label define tm9488l 
	0           "Not applicable"                
	1           "No spouse in household"        
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values tm9490   tm9490l;
label define tm9490l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9610"           
;
label values tm9492   tm9492l;
label define tm9492l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9498"           
;
label values tm9498   tm9498l;
label define tm9498l 
	-1          "DK"                            
	-2          "Ref"                           
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
