log using sip90r8, text replace
set mem 1000m
*This program reads the 1990 SIPP Wave 8 Topical Module Research Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 17:39:48 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip90r8
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1990\sip90r8.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip90r8"

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
	61          "Maine, Vermont"                
	62          "Iowa, North Dakota, South"     
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
	9           "Vacant"                        
	10          "Occupied by persons with URE"  
	11          "Unfit or to be demolished"     
	12          "Under construction, not ready" 
	13          "Converted to temporary business"
	14          "Unoccupied site for mobile home"
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
	28          "Merged household across panels"
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
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not applicable"                
	1           "Yes - skip to TM8006"          
	2           "No"                            
;
label values tm8002   tm8002l;
label define tm8002l 
	0           "Not applicable"                
	1           "Yes - skip to TM9330"          
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
	-3          "None - skip to TM9330"         
	0           "Not applicable"                
	1           "1 business"                    
	2           "2 businesses"                  
	3           "3+ businesses"                 
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
	0           "Not universe"                  
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
	0           "Not applicable"                
	1           "Sole proprietorship"           
	2           "Partnership"                   
	3           "Corporation"                   
	-1          "DK"                            
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
	1           "Yes - skip to TM8118"          
	2           "No"                            
;
label values tm8106   tm8106l;
label define tm8106l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8116"           
	-1          "DK - skip to TM8116"           
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
	1           "Yes - skip to TM8116"          
	2           "No"                            
;
label values tm8114   tm8114l;
label define tm8114l 
	0           "Not applicable"                
	-1          "DK"                            
;
label values tm8116   tm8116l;
label define tm8116l 
	0           "Not applicable"                
	-1          "DK"                            
;
label values tm8126   tm8126l;
label define tm8126l 
	0           "Not applicable"                
	1           "Yes - skip to TM8274"          
	2           "No"                            
;
label values tm8204   tm8204l;
label define tm8204l 
	0           "Not applicable"                
	-4          "Lost money - enter loss in"    
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
label values tm8216   tm8216l;
label define tm8216l 
	-4          "Lost money"                    
	0           "Not applicable"                
;
label values tm8218   tm8218l;
label define tm8218l 
	0           "Not applicable"                
;
label values tm8222   tm8222l;
label define tm8222l 
	-4          "Lost money"                    
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
	0           "Not universe"                  
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
	1           "Yes - skip to TM8168"          
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
	1           "Yes - skip to TM8166"          
	2           "No"                            
;
label values tm8164   tm8164l;
label define tm8164l 
	-1          "DK"                            
	0           "Not applicable"                
;
label values tm8166   tm8166l;
label define tm8166l 
	-1          "DK"                            
	0           "Not applicable"                
;
label values tm8176   tm8176l;
label define tm8176l 
	0           "Not applicable"                
	1           "Yes - skip to TM8276"          
	2           "No"                            
;
label values tm8254   tm8254l;
label define tm8254l 
	0           "Not applicable"                
	-4          "Lost money - enter loss in"    
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
label values tm8266   tm8266l;
label define tm8266l 
	-4          "Lost money"                    
	0           "Not applicable"                
;
label values tm8268   tm8268l;
label define tm8268l 
	0           "Not applicable"                
;
label values tm8272   tm8272l;
label define tm8272l 
	-4          "Lost money"                    
	0           "Not applicable"                
;
label values tm8276   tm8276l;
label define tm8276l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8282"           
;
label values tm8280   tm8280l;
label define tm8280l 
	-4          "Lost money"                    
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
label values tm9336   tm9336l;
label define tm9336l 
	-1          "DK - skip to TM9340"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9340"           
;
label values tm9356   tm9356l;
label define tm9356l 
	0           "Not applicable"                
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
label values tm9364   tm9364l;
label define tm9364l 
	-1          "DK - skip to TM9368"           
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9368"           
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
label values tm9390   tm9390l;
label define tm9390l 
	0           "Not applicable"                
	1           "Yes - skip to TM9486"          
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
	-1          "DK"                            
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
	-3          "None"                          
	0           "Not applicable"                
	1           "1 exemption"                   
	2           "2 exemptions"                  
	3           "3-5 exemptions"                
	4           "6 or more exemptions"          
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
label values tm9486   tm9486l;
label define tm9486l 
	0           "Not applicable"                
	1           "Owned or being bought?"        
	2           "Rented for cash? - skip to"    
	3           "Occupied without cash payment?"
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
	-2          "Refused"                       
	0           "Not applicable"                
	1           "Amount < $100"                 
	2           "Amount from $100 to $199"      
	3           "Amount from $200 to $299"      
	4           "Amount from $300 to $399"      
	5           "Amount from $400 to $499"      
	6           "Amount from $500 to $599"      
	7           "Amount from $600 to $699"      
	8           "Amount from $700 to $799"      
	9           "Amount from $800 to $899"      
	10          "Amount from $900 to $999"      
	11          "Amount from $1000 to $1099"    
	12          "Amount from $1100 to $1199"    
	13          "Amount from $1200 to $1299"    
	14          "Amount from $1300 to $1499"    
	15          "Amount from $1500 to $1799"    
	16          "Amount from $1800 to $2099"    
	17          "Amount of $2100 or more"       
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
	-3          "None"                          
	-1          "Don't know"                    
	0           "Not applicable"                
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
	-3          "None - End of interview"       
	0           "Not applicable"                
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
	07000       "Amount $6000 or more"          
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
