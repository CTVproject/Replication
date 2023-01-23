log using sip93t1, text replace
set mem 1000m
*This program reads the 1993 SIPP Wave 1 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Mar 18 11:53:11 EST 2004
*Please report errors to jroth@nber.org
*run with do sip93t1
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1993\sip93t1.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip93t1"

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
	61          "Maine, Vermont"                
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
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children under 1"
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
;
label values pp_mis1  pp_mis1l;
label define pp_mis1l
	1           "Interviewed"                   
	2           "Noninterview"                  
;
label values pp_mis2  pp_mis2l;
label define pp_mis2l
	1           "Interviewed"                   
	2           "Noninterview"                  
;
label values pp_mis3  pp_mis3l;
label define pp_mis3l
	1           "Interviewed"                   
	2           "Noninterview"                  
;
label values pp_mis4  pp_mis4l;
label define pp_mis4l
	1           "Interviewed"                   
	2           "Noninterview"                  
;
label values pp_mis5  pp_mis5l;
label define pp_mis5l
	1           "Interviewed"                   
	2           "Noninterview"                  
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
	21          "Afro-American (Black Or Negro)"
	30          "Another group not listed"      
	39          "Don't know"                    
;
label values tm8052   tm8052l;
label define tm8052l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -skip to TM8124"            
;
label values tm8054   tm8054l;
label define tm8054l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8058"           
;
label values tm8056   tm8056l;
label define tm8056l 
	0           "Not applicable"                
	1           "Yes - skip to TM8062"          
	2           "No - skip to TM8074"           
;
label values tm8058   tm8058l;
label define tm8058l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8074"           
;
label values tm8060   tm8060l;
label define tm8060l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8074"           
;
label values tm8062   tm8062l;
label define tm8062l 
	0           "Not applicable"                
;
label values tm8064   tm8064l;
label define tm8064l 
	0           "Not applicable"                
;
label values tm8066   tm8066l;
label define tm8066l 
	0           "Not applicable"                
;
label values tm8068   tm8068l;
label define tm8068l 
	0           "Not applicable"                
;
label values tm8072   tm8072l;
label define tm8072l 
	0           "Not applicable"                
;
label values tm8074   tm8074l;
label define tm8074l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8096"           
;
label values tm8076   tm8076l;
label define tm8076l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8080"           
;
label values tm8078   tm8078l;
label define tm8078l 
	0           "Not applicable"                
	1           "Yes - skip to TM8084"          
	2           "No - skip to TM8096"           
;
label values tm8080   tm8080l;
label define tm8080l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8096"           
;
label values tm8082   tm8082l;
label define tm8082l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8096"           
;
label values tm8084   tm8084l;
label define tm8084l 
	0           "Not applicable"                
;
label values tm8086   tm8086l;
label define tm8086l 
	0           "Not applicable"                
;
label values tm8088   tm8088l;
label define tm8088l 
	0           "Not applicable"                
;
label values tm8090   tm8090l;
label define tm8090l 
	0           "Not applicable"                
;
label values tm8094   tm8094l;
label define tm8094l 
	0           "Not applicable"                
;
label values tm8096   tm8096l;
label define tm8096l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8100"           
;
label values tm8098   tm8098l;
label define tm8098l 
	0           "Not applicable"                
	1           "Yes - skip to TM8104"          
	2           "No - skip to TM8114"           
;
label values tm8100   tm8100l;
label define tm8100l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8114"           
;
label values tm8102   tm8102l;
label define tm8102l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8114"           
;
label values tm8104   tm8104l;
label define tm8104l 
	0           "Not applicable"                
;
label values tm8106   tm8106l;
label define tm8106l 
	0           "Not applicable"                
;
label values tm8108   tm8108l;
label define tm8108l 
	0           "Not applicable"                
;
label values tm8110   tm8110l;
label define tm8110l 
	0           "Not applicable"                
;
label values tm8114   tm8114l;
label define tm8114l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8124"           
;
label values tm8116   tm8116l;
label define tm8116l 
	0           "Not applicable"                
	1           "Yes - skip to TM8124"          
	2           "No"                            
;
label values tm8118   tm8118l;
label define tm8118l 
	0           "Not applicable"                
;
label values tm8120   tm8120l;
label define tm8120l 
	0           "Not applicable"                
;
label values tm8122   tm8122l;
label define tm8122l 
	-3          "Never covered by Medicaid"     
	0           "Not applicable"                
;
label values tm8124   tm8124l;
label define tm8124l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8132"           
;
label values tm8126   tm8126l;
label define tm8126l 
	0           "Not applicable"                
;
label values tm8128   tm8128l;
label define tm8128l 
	0           "Not applicable"                
;
label values tm8130   tm8130l;
label define tm8130l 
	-3          "Have always had insurance -"   
	0           "Not applicable"                
;
label values tm8132   tm8132l;
label define tm8132l 
	0           "Not applicable"                
;
label values tm8134   tm8134l;
label define tm8134l 
	0           "Not applicable"                
;
label values tm8136   tm8136l;
label define tm8136l 
	-3          "Has never been covered"        
	0           "Not applicable"                
;
label values tm8138   tm8138l;
label define tm8138l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8200"           
;
label values tm8140   tm8140l;
label define tm8140l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8148"           
;
label values tm8142   tm8142l;
label define tm8142l 
	0           "Not applicable"                
;
label values tm8144   tm8144l;
label define tm8144l 
	0           "Not applicable"                
;
label values tm8146   tm8146l;
label define tm8146l 
	-3          "Have always lived in public"   
	0           "Not applicable"                
;
label values tm8148   tm8148l;
label define tm8148l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8200"           
;
label values tm8150   tm8150l;
label define tm8150l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm3000   tm3000l;
label define tm3000l 
	0           "Not applicable"                
	1           "Social Security"               
	2           "U.S. Government railroad"      
	3           "Fededral Supplemental Security"
	5           "State unemployment compensation"
	6           "Supplemental unemployment"     
	7           "Other unemployment compensation"
	8           "Veterans' compensation or pensio"
	10          "Workers compensation"          
	11          "State temporary sickness or"   
	12          "Employer or union temporary"   
	13          "Payments form a sickness,"     
	20          "Aid to families with dependent"
	21          "General assistance or general" 
	23          "Foster child care payments"    
	24          "Other welfare"                 
	25          "WIC (Women, Infants and Children"
	27          "Food stamps"                   
	28          "Child support payments"        
	29          "Alimony payments"              
	30          "Pension from company or union" 
	31          "Federal Civil Service or other"
	32          "U.S. Military retirement pay"  
	34          "State government pensions"     
	35          "Local government pensions"     
	36          "Income form paid-up life"      
	37          "Estates and trusts"            
	38          "Other payments for retirement,"
	40          "GI bill education benefits"    
	41          "Other VA education benefits"   
	50          "Income assistance from a"      
	51          "Money from relatives or friends"
	52          "Lump sum payments"             
	53          "Income from roomers or boarders"
	54          "National guard or reserve pay" 
	55          "Incidental or casual earnings" 
	56          "Other cash income not included"
;
label values tm8300   tm8300l;
label define tm8300l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3032"           
;
label values tm8302   tm8302l;
label define tm8302l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3032"           
;
label values tm8304   tm8304l;
label define tm8304l 
	0           "Not applicable"                
;
label values tm8306   tm8306l;
label define tm8306l 
	0           "Not applicable"                
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3086"           
;
label values tm8310   tm8310l;
label define tm8310l 
	0           "Not applicable"                
;
label values tm8312   tm8312l;
label define tm8312l 
	0           "Not applicable"                
;
label values tm8314   tm8314l;
label define tm8314l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to NEXT ISS CODE"    
;
label values tm8316   tm8316l;
label define tm8316l 
	0           "Not applicable"                
;
label values tm8318   tm8318l;
label define tm8318l 
	0           "Not applicable"                
;
label values tm8320   tm8320l;
label define tm8320l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8322   tm8322l;
label define tm8322l 
	0           "Not applicable"                
;
label values tm8324   tm8324l;
label define tm8324l 
	0           "Not applicable"                
;
label values tm3200   tm3200l;
label define tm3200l 
	0           "Not applicable"                
	1           "Social Security"               
	2           "U.S. Government Railroad"      
	3           "Federal Supplemental Security" 
	5           "State Unemployment compensation"
	6           "Supplemental Unemployment"     
	7           "Other Unemployment compensation"
	8           "Veterans' compensation or"     
	10          "Workers compensation"          
	11          "State temporary sickness or"   
	12          "Employer or union temporary"   
	13          "Payments from a sickness,"     
	20          "Aid to families with dependent"
	21          "General assistance or general" 
	23          "Foster child care payments"    
	24          "Other welfare"                 
	25          "WIC (Women, Infants and Children"
	27          "Food stamps"                   
	28          "Child support payments"        
	29          "Alimony payments"              
	30          "Pension from company or union" 
	31          "Federal Civil Service or other"
	32          "U.S. Military retirement pay"  
	34          "State government pensions"     
	35          "Local government pensions"     
	36          "Income from paid-up life"      
	37          "Estates and trusts"            
	38          "Other payments for retirement,"
	40          "GI Bill education benefits"    
	41          "Other VA education benefits"   
	50          "Income assistance from a"      
	51          "Money from relatives or friends"
	52          "Lump sum payments"             
	53          "Income from roomers or boarders"
	54          "National Guard or reserve pay" 
	55          "Incidental or casual earnings" 
	56          "Other cash income not included"
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3232"           
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3232"           
;
label values tm8404   tm8404l;
label define tm8404l 
	0           "Not applicable"                
;
label values tm8406   tm8406l;
label define tm8406l 
	0           "Not applicable"                
;
label values tm8408   tm8408l;
label define tm8408l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3286"           
;
label values tm8410   tm8410l;
label define tm8410l 
	0           "Not applicable"                
;
label values tm8412   tm8412l;
label define tm8412l 
	0           "Not applicable"                
;
label values tm8414   tm8414l;
label define tm8414l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to NEXT ISS CODE"    
;
label values tm8416   tm8416l;
label define tm8416l 
	0           "Not applicable"                
;
label values tm8418   tm8418l;
label define tm8418l 
	0           "Not applicable"                
;
label values tm8420   tm8420l;
label define tm8420l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to NEXT ISS CODE"    
;
label values tm8422   tm8422l;
label define tm8422l 
	0           "Not applicable"                
;
label values tm8424   tm8424l;
label define tm8424l 
	0           "Not applicable"                
;
label values tm3400   tm3400l;
label define tm3400l 
	0           "Not applicable"                
	1           "Social Security"               
	2           "U.S. Government Railroad"      
	3           "Federal Supplemental Security" 
	5           "State Unemployment compensation"
	6           "Supplemental Unemployment"     
	7           "Other Unemployment compensation"
	8           "Veterans' compensation or"     
	10          "Workers compensation"          
	11          "State temporary sickness or"   
	12          "Employer or union temporary"   
	13          "Payments from a sickness,"     
	20          "Aid to families with dependent"
	21          "General assistance or general" 
	23          "Foster child care payments"    
	24          "Other welfare"                 
	25          "WIC (Women, Infants And Children"
	27          "Food stamps"                   
	28          "Child support payments"        
	29          "Alimony payments"              
	30          "Pension from company or union" 
	31          "Federal Civil Service or other"
	32          "U.S. Military retirement pay"  
	34          "State government pensions"     
	35          "Local government pensions"     
	36          "Income from paid-up life"      
	37          "Estates and trusts"            
	38          "Other payments for retirement,"
	40          "GI Bill education benefits"    
	41          "Other VA education benefits"   
	50          "Income assistance from a"      
	51          "Money from relatives or friends"
	52          "Lump sum payments"             
	53          "Income from roomers or boarders"
	54          "National Guard or reserve pay" 
	55          "Incidental or casual earnings" 
	56          "Other cash income not included"
;
label values tm8500   tm8500l;
label define tm8500l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3432"           
;
label values tm8502   tm8502l;
label define tm8502l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3432"           
;
label values tm8504   tm8504l;
label define tm8504l 
	0           "Not applicable"                
;
label values tm8506   tm8506l;
label define tm8506l 
	0           "Not applicable"                
;
label values tm8508   tm8508l;
label define tm8508l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3486"           
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "Not applicable"                
;
label values tm8512   tm8512l;
label define tm8512l 
	0           "Not applicable"                
;
label values tm8514   tm8514l;
label define tm8514l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8516   tm8516l;
label define tm8516l 
	0           "Not applicable"                
;
label values tm8518   tm8518l;
label define tm8518l 
	0           "Not applicable"                
;
label values tm8520   tm8520l;
label define tm8520l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8522   tm8522l;
label define tm8522l 
	0           "Not applicable"                
;
label values tm8524   tm8524l;
label define tm8524l 
	0           "Not applicable"                
;
label values tm3600   tm3600l;
label define tm3600l 
	0           "Not applicable"                
	1           "Social Security"               
	2           "U.S. Government Railroad"      
	3           "Federal Supplemental Security" 
	5           "State Unemployment compensation"
	6           "Supplemental Unemployment"     
	7           "Other Unemployment compensation"
	8           "Veterans' compensation or"     
	10          "Workers compensation"          
	11          "State temporary sickness or"   
	12          "Employer or union temporary"   
	13          "Payments from a sickness,"     
	20          "Aid to families with dependent"
	21          "General assistance or general" 
	23          "Foster child care payments"    
	24          "Other welfare"                 
	25          "WIC (Women, Infants And Children"
	27          "Food stamps"                   
	28          "Child support payments"        
	29          "Alimony payments"              
	30          "Pension from company or union" 
	31          "Federal Civil Service or other"
	32          "U.S. Military retirement pay"  
	34          "State government pensions"     
	35          "Local government pensions"     
	36          "Income from paid-up life"      
	37          "Estates and trusts"            
	38          "Other payments for retirement,"
	40          "GI Bill education benefits"    
	41          "Other VA education benefits"   
	50          "Income assistance from a"      
	51          "Money from relatives or friends"
	52          "Lump sum payments"             
	53          "Income from roomers or boarders"
	54          "National Guard or reserve pay" 
	55          "Incidental or casual earnings" 
	56          "Other cash income not included"
;
label values tm8600   tm8600l;
label define tm8600l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3632"           
;
label values tm8602   tm8602l;
label define tm8602l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3632"           
;
label values tm8604   tm8604l;
label define tm8604l 
	0           "Not applicable"                
;
label values tm8606   tm8606l;
label define tm8606l 
	0           "Not applicable"                
;
label values tm8608   tm8608l;
label define tm8608l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3686"           
;
label values tm8610   tm8610l;
label define tm8610l 
	0           "Not applicable"                
;
label values tm8612   tm8612l;
label define tm8612l 
	0           "Not applicable"                
;
label values tm8614   tm8614l;
label define tm8614l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8616   tm8616l;
label define tm8616l 
	0           "Not applicable"                
;
label values tm8618   tm8618l;
label define tm8618l 
	0           "Not applicable"                
;
label values tm8620   tm8620l;
label define tm8620l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8622   tm8622l;
label define tm8622l 
	0           "Not applicable"                
;
label values tm8624   tm8624l;
label define tm8624l 
	0           "Not applicable"                
;
label values tm3800   tm3800l;
label define tm3800l 
	0           "Not applicable"                
	1           "Social Security"               
	2           "U.S. Government Railroad"      
	3           "Federal Supplemental Security" 
	5           "State Unemployment compensation"
	6           "Supplemental Unemployment"     
	7           "Other Unemployment compensation"
	8           "Veterans' compensation or"     
	10          "Workers compensation"          
	11          "State temporary sickness or"   
	12          "Employer or union temporary"   
	13          "Payments from a sickness,"     
	20          "Aid to families with dependent"
	21          "General assistance or general" 
	23          "Foster child care payments"    
	24          "Other welfare"                 
	25          "WIC (Women, Infants and Children"
	27          "Food stamps"                   
	28          "Child support payments"        
	29          "Alimony payments"              
	30          "Pension from company or union" 
	31          "Federal Civil Service or other"
	32          "U.S. Military retirement pay"  
	34          "State government pensions"     
	35          "Local government pensions"     
	36          "Income from paid-up life"      
	37          "Estates and trusts"            
	38          "Other payments for retirement,"
	40          "GI Bill education benefits"    
	41          "Other VA education benefits"   
	50          "Income assistance from a"      
	51          "Money from relatives or friends"
	52          "Lump sum payments"             
	53          "Income from roomers or boarders"
	54          "National Guard or reserve pay" 
	55          "Incidental or casual earnings" 
	56          "Other cash income not included"
;
label values tm8700   tm8700l;
label define tm8700l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3832"           
;
label values tm8702   tm8702l;
label define tm8702l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3832"           
;
label values tm8704   tm8704l;
label define tm8704l 
	0           "Not applicable"                
;
label values tm8706   tm8706l;
label define tm8706l 
	0           "Not applicable"                
;
label values tm8708   tm8708l;
label define tm8708l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC3886"           
;
label values tm8710   tm8710l;
label define tm8710l 
	0           "Not applicable"                
;
label values tm8712   tm8712l;
label define tm8712l 
	0           "Not applicable"                
;
label values tm8714   tm8714l;
label define tm8714l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8716   tm8716l;
label define tm8716l 
	0           "Not applicable"                
;
label values tm8718   tm8718l;
label define tm8718l 
	0           "Not applicable"                
;
label values tm8720   tm8720l;
label define tm8720l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8722   tm8722l;
label define tm8722l 
	0           "Not applicable"                
;
label values tm8724   tm8724l;
label define tm8724l 
	0           "Not applicable"                
;
label values tm4000   tm4000l;
label define tm4000l 
	0           "Not applicable"                
	1           "Social Security"               
	2           "U.S. Government Railroad"      
	3           "Federal Supplemental Security" 
	5           "State unemployment compensation"
	6           "Supplemental unemployment"     
	7           "Other unemployment compensation"
	8           "Veterans' compensation or"     
	10          "Workers compensation"          
	11          "State temporary sickness or"   
	12          "Employer or union temporary"   
	13          "Payments from a sickness,"     
	20          "Aid to families with dependent"
	21          "General assistance or general" 
	23          "Foster child care payments"    
	24          "Other welfare"                 
	25          "WIC (Women, Infants and Children"
	27          "Food stamps"                   
	28          "Child support payments"        
	29          "Alimony payments"              
	30          "Pension from company or union" 
	31          "Federal Civil Service or other"
	32          "U.S. Military retirement pay"  
	34          "State government pensions"     
	35          "Local government pensions"     
	36          "Income from paid-up life"      
	37          "Estates and trusts"            
	38          "Other payments for retirement,"
	40          "GI Bill education benefits"    
	41          "Other VA education benefits"   
	50          "Income assistance from a"      
	51          "Money from relatives or friends"
	52          "Lump sum payments"             
	53          "Income from roomers or boarders"
	54          "National Guard or reserve pay" 
	55          "Incidental or casual earnings" 
	56          "Other cash income not included"
;
label values tm8800   tm8800l;
label define tm8800l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4032"           
;
label values tm8802   tm8802l;
label define tm8802l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4032"           
;
label values tm8804   tm8804l;
label define tm8804l 
	0           "Not applicable"                
;
label values tm8806   tm8806l;
label define tm8806l 
	0           "Not applicable"                
;
label values tm8808   tm8808l;
label define tm8808l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to SC4086"           
;
label values tm8810   tm8810l;
label define tm8810l 
	0           "Not applicable"                
;
label values tm8812   tm8812l;
label define tm8812l 
	0           "Not applicable"                
;
label values tm8814   tm8814l;
label define tm8814l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8816   tm8816l;
label define tm8816l 
	0           "Not applicable"                
;
label values tm8818   tm8818l;
label define tm8818l 
	0           "Not applicable"                
;
label values tm8820   tm8820l;
label define tm8820l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values tm8822   tm8822l;
label define tm8822l 
	0           "Not applicable"                
;
label values tm8824   tm8824l;
label define tm8824l 
	0           "Not applicable"                
;
label values imp8058  imp8058l;
label define imp8058l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8060  imp8060l;
label define imp8060l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8062  imp8062l;
label define imp8062l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8064  imp8064l;
label define imp8064l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8066  imp8066l;
label define imp8066l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8068  imp8068l;
label define imp8068l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8072  imp8072l;
label define imp8072l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8080  imp8080l;
label define imp8080l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8082  imp8082l;
label define imp8082l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8084  imp8084l;
label define imp8084l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8086  imp8086l;
label define imp8086l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8088  imp8088l;
label define imp8088l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8090  imp8090l;
label define imp8090l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8094  imp8094l;
label define imp8094l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8100  imp8100l;
label define imp8100l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8102  imp8102l;
label define imp8102l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8104  imp8104l;
label define imp8104l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8106  imp8106l;
label define imp8106l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8108  imp8108l;
label define imp8108l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8110  imp8110l;
label define imp8110l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8118  imp8118l;
label define imp8118l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8120  imp8120l;
label define imp8120l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8122  imp8122l;
label define imp8122l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8126  imp8126l;
label define imp8126l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8128  imp8128l;
label define imp8128l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8130  imp8130l;
label define imp8130l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8132  imp8132l;
label define imp8132l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8134  imp8134l;
label define imp8134l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8136  imp8136l;
label define imp8136l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8142  imp8142l;
label define imp8142l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8144  imp8144l;
label define imp8144l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8146  imp8146l;
label define imp8146l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8150  imp8150l;
label define imp8150l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8304  imp8304l;
label define imp8304l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8306  imp8306l;
label define imp8306l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8310  imp8310l;
label define imp8310l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8312  imp8312l;
label define imp8312l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8316  imp8316l;
label define imp8316l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8318  imp8318l;
label define imp8318l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8322  imp8322l;
label define imp8322l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8324  imp8324l;
label define imp8324l
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
label values imp8412  imp8412l;
label define imp8412l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8416  imp8416l;
label define imp8416l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8418  imp8418l;
label define imp8418l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8422  imp8422l;
label define imp8422l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8424  imp8424l;
label define imp8424l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8504  imp8504l;
label define imp8504l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8506  imp8506l;
label define imp8506l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8510  imp8510l;
label define imp8510l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8512  imp8512l;
label define imp8512l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8516  imp8516l;
label define imp8516l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8518  imp8518l;
label define imp8518l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8522  imp8522l;
label define imp8522l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8524  imp8524l;
label define imp8524l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8604  imp8604l;
label define imp8604l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8606  imp8606l;
label define imp8606l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8610  imp8610l;
label define imp8610l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8612  imp8612l;
label define imp8612l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8616  imp8616l;
label define imp8616l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8618  imp8618l;
label define imp8618l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8622  imp8622l;
label define imp8622l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8624  imp8624l;
label define imp8624l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8704  imp8704l;
label define imp8704l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8706  imp8706l;
label define imp8706l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8710  imp8710l;
label define imp8710l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8712  imp8712l;
label define imp8712l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8716  imp8716l;
label define imp8716l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8718  imp8718l;
label define imp8718l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8722  imp8722l;
label define imp8722l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8724  imp8724l;
label define imp8724l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8804  imp8804l;
label define imp8804l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8806  imp8806l;
label define imp8806l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8810  imp8810l;
label define imp8810l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8812  imp8812l;
label define imp8812l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8816  imp8816l;
label define imp8816l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8818  imp8818l;
label define imp8818l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8822  imp8822l;
label define imp8822l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8824  imp8824l;
label define imp8824l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8200   tm8200l;
label define tm8200l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8300"           
;
label values tm8210   tm8210l;
label define tm8210l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8240"           
;
label values tm8214   tm8214l;
label define tm8214l 
	0           "Not applicable"                
;
label values tm8216   tm8216l;
label define tm8216l 
	0           "Not applicable"                
;
label values tm8218   tm8218l;
label define tm8218l 
	0           "Not applicable"                
;
label values tm8220   tm8220l;
label define tm8220l 
	0           "Not applicable"                
;
label values tm8222   tm8222l;
label define tm8222l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8248"           
;
label values tm8224   tm8224l;
label define tm8224l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 TO 99"                      
	3           "100 TO 999"                    
;
label values tm8226   tm8226l;
label define tm8226l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8248"           
;
label values tm8228   tm8228l;
label define tm8228l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 TO 99"                      
	3           "100 TO 499"                    
;
label values tm8234   tm8234l;
label define tm8234l 
	0           "Not applicable"                
;
label values tm8236   tm8236l;
label define tm8236l 
	0           "Not applicable"                
;
label values tm8240   tm8240l;
label define tm8240l 
	0           "Not applicable"                
;
label values tm8242   tm8242l;
label define tm8242l 
	0           "Not applicable"                
;
label values tm8244   tm8244l;
label define tm8244l 
	-3          "Never worked for 2 consecutive"
	0           "Not applicable"                
;
label values tm8246   tm8246l;
label define tm8246l 
	0           "Not applicable"                
	1           "Taking care of home or family" 
	2           "Ill or disabled"               
	3           "Going to school"               
	4           "Couldn't find work"            
	5           "Didn't want to work"           
	7           "Other"                         
;
label values tm8248   tm8248l;
label define tm8248l 
	0           "Not applicable"                
;
label values tm8250   tm8250l;
label define tm8250l 
	0           "Not applicable"                
;
label values tm8252   tm8252l;
label define tm8252l 
	-3          "Never had another job lasting" 
	0           "Not applicable"                
;
label values tm8254   tm8254l;
label define tm8254l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8284"           
;
label values tmind3   tmind3l;
label define tmind3l 
	0           "Not applicable"                
;
label values tmind4   tmind4l;
label define tmind4l 
	0           "Not applicable"                
;
label values tm8266   tm8266l;
label define tm8266l 
	0           "Not applicable"                
	1           "Worked for an employer"        
	2           "Self-employed"                 
;
label values tm8268   tm8268l;
label define tm8268l 
	0           "Not applicable"                
;
label values tm8270   tm8270l;
label define tm8270l 
	0           "Not applicable"                
;
label values tm8272   tm8272l;
label define tm8272l 
	0           "Not applicable"                
	1           "Layoff, plant closed"          
	2           "Discharged"                    
	3           "Job was temporary and ended"   
	4           "Found a better job"            
	5           "Retirement/old age"            
	6           "Did not like working conditions"
	7           "Dissatisfied with earnings"    
	8           "Did not like location"         
	9           "Going to school"               
	10          "Became pregnant/had child"     
	11          "Health reasons"                
	12          "Other family or personal reasons"
	13          "Other"                         
;
label values tm8274   tm8274l;
label define tm8274l 
	-3          "Never worked 6 straight months"
	0           "Not applicable"                
;
label values tm8276   tm8276l;
label define tm8276l 
	-1          "Don't know - skip to end"      
	0           "Not applicable"                
	1           "Yes - skip to end"             
	2           "No"                            
;
label values tm8278   tm8278l;
label define tm8278l 
	0           "Not applicable"                
;
label values tm8280   tm8280l;
label define tm8280l 
	0           "Not applicable"                
	1           "Yes - skip to TM8286"          
	2           "No"                            
;
label values tm8282   tm8282l;
label define tm8282l 
	-5          "All years"                     
	0           "Not applicable"                
;
label values tm8284   tm8284l;
label define tm8284l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8300"           
;
label values tm8286   tm8286l;
label define tm8286l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8300"           
;
label values tm8288   tm8288l;
label define tm8288l 
	0           "Not applicable"                
;
label values tm8290   tm8290l;
label define tm8290l 
	0           "Not applicable"                
;
label values tm8292   tm8292l;
label define tm8292l 
	0           "Not applicable"                
;
label values tm8294   tm8294l;
label define tm8294l 
	0           "Not applicable"                
	1           "Took care of family or home"   
	2           "Own illness or disability"     
	3           "Could not find work"           
	4           "Going to school"               
	5           "Became pregnant/had child"     
	6           "Other"                         
;
label values imp8218  imp8218l;
label define imp8218l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8220  imp8220l;
label define imp8220l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8224  imp8224l;
label define imp8224l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8226  imp8226l;
label define imp8226l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8228  imp8228l;
label define imp8228l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8234  imp8234l;
label define imp8234l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8240  imp8240l;
label define imp8240l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8242  imp8242l;
label define imp8242l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8244  imp8244l;
label define imp8244l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8246  imp8246l;
label define imp8246l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8248  imp8248l;
label define imp8248l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8250  imp8250l;
label define imp8250l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8252  imp8252l;
label define imp8252l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8266  imp8266l;
label define imp8266l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8268  imp8268l;
label define imp8268l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8270  imp8270l;
label define imp8270l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8272  imp8272l;
label define imp8272l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8274  imp8274l;
label define imp8274l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8276  imp8276l;
label define imp8276l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8278  imp8278l;
label define imp8278l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8282  imp8282l;
label define imp8282l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8286  imp8286l;
label define imp8286l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8288  imp8288l;
label define imp8288l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8290  imp8290l;
label define imp8290l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp829a  imp829a;
label define imp829a 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8292  imp8292l;
label define imp8292l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8294  imp8294l;
label define imp8294l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_ind  imp_ind;
label define imp_ind 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_occ  imp_occ;
label define imp_occ 
	0           "Not imputed"                   
	1           "Imputed"                       
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
