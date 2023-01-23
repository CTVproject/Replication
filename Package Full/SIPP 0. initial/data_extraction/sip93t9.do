log using sip93t9, text replace
set mem 1000m
*This program reads the 1993 SIPP Wave 9 Topical Module Data File 

****************************************************************
*
* NOTE: This complete dataset has over more than 2,047 variables,
* the maximum number of variables for Intercooled Stata 8.0. 
* So, variables at the end are commented out.  The commenting 
* can be removed in an editor by replacing '' with ''.
* Stata/SE can handle up to 32,766 variables, default=5000.
*
****************************************************************

*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Mar 18 11:55:28 EST 2004
*Please report errors to jroth@nber.org
*run with do sip93t9
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1993\sip93t9.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip93t9"

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
	9           "Vacant"                        
	10          "Occupied by persons with ure"  
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
	28          "Merged HHlds across panels"    
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
label values tm9200   tm9200l;
label define tm9200l 
	0           "Not applicable"                
	1           "Yes - Skip to TM9202"          
	2           "No"                            
;
label values tm9201   tm9201l;
label define tm9201l 
	0           "Not applicable"                
	1           "Yes - Skip to Check Item C1"   
	2           "No"                            
;
label values tm9202   tm9202l;
label define tm9202l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9204   tm9204l;
label define tm9204l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9210   tm9210l;
label define tm9210l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9216   tm9216l;
label define tm9216l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9222   tm9222l;
label define tm9222l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9228   tm9228l;
label define tm9228l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9234   tm9234l;
label define tm9234l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9240   tm9240l;
label define tm9240l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm9206   tm9206l;
label define tm9206l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9212   tm9212l;
label define tm9212l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9218   tm9218l;
label define tm9218l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9224   tm9224l;
label define tm9224l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9230   tm9230l;
label define tm9230l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9236   tm9236l;
label define tm9236l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9242   tm9242l;
label define tm9242l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9208   tm9208l;
label define tm9208l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9214   tm9214l;
label define tm9214l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9220   tm9220l;
label define tm9220l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9226   tm9226l;
label define tm9226l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9232   tm9232l;
label define tm9232l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9238   tm9238l;
label define tm9238l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9244   tm9244l;
label define tm9244l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9246   tm9246l;
label define tm9246l 
	0           "Not applicable"                
	1           "All of the help I/we need"     
	2           "Most of the help I/we need"    
	3           "Very little of the help I/we"  
	4           "No help"                       
	-1          "Don't know"                    
	-3          "Not applicable"                
;
label values tm9248   tm9248l;
label define tm9248l 
	0           "Not applicable"                
	1           "All of the help I/we need"     
	2           "Most of the help I/we need"    
	3           "Very little help I/we need"    
	4           "No help"                       
	-1          "Don't know"                    
	-3          "Not applicable"                
;
label values tm9250   tm9250l;
label define tm9250l 
	0           "Not applicable"                
	1           "All of the help I/we need"     
	2           "Most of the help I/we need"    
	3           "Very little help I/we need"    
	4           "No help"                       
	-1          "Don't know"                    
	-3          "Not applicable"                
;
label values tm9252   tm9252l;
label define tm9252l 
	0           "Not applicable"                
	1           "Enough food to eat"            
	2           "Sometimes not enough to eat -" 
	3           "Often not enough to eat - Skip"
;
label values tm9254   tm9254l;
label define tm9254l 
	0           "Not applicable"                
	1           "Enough and the kind - Skip to" 
;
label values tm9256   tm9256l;
label define tm9256l 
	0           "Not applicable"                
	2           "Enough but not always the kind -"
;
label values tm9258   tm9258l;
label define tm9258l 
	0           "Not applicable"                
	1           "Last month"                    
;
label values tm9260   tm9260l;
label define tm9260l 
	0           "Not applicable"                
	1           "Two months ago"                
;
label values tm9262   tm9262l;
label define tm9262l 
	0           "Not applicable"                
	1           "Three months ago"              
;
label values tm9264   tm9264l;
label define tm9264l 
	0           "Not applicable"                
	1           "Four months ago"               
;
label values tm9266   tm9266l;
label define tm9266l 
	0           "Not applicable"                
	1           "Not enough money for food"     
;
label values tm9268   tm9268l;
label define tm9268l 
	0           "Not applicable"                
	1           "Too hard to get to the store"  
;
label values tm9270   tm9270l;
label define tm9270l 
	0           "Not applicable"                
	1           "No working stove"              
;
label values tm9272   tm9272l;
label define tm9272l 
	0           "Not applicable"                
	1           "No working refrigerator"       
;
label values tm9274   tm9274l;
label define tm9274l 
	0           "Not applicable"                
	1           "Not able to cook or eat because"
;
label values tm9276   tm9276l;
label define tm9276l 
	0           "Not applicable"                
	-3          "None - Skip to TM9280"         
;
label values tm9278   tm9278l;
label define tm9278l 
	0           "Not applicable"                
	999999      "Amount short on food budget"   
;
label values tm9280   tm9280l;
label define tm9280l 
	0           "Not applicable"                
	1           "Delighted"                     
	2           "Pleased"                       
	3           "Mostly satisfied"              
	4           "Mixed (about equally satisfied"
	5           "Mostly dissatified"            
	6           "Unhappy"                       
	7           "Terrible"                      
;
label values tm9282   tm9282l;
label define tm9282l 
	0           "Not applicable"                
	1           "Check digit is an even number -"
	2           "Check digit is an odd number -"
;
label values tm9284   tm9284l;
label define tm9284l 
	0           "Not applicable"                
	999999      "Yearly - Skip to Check Item C1"
	-1          "Dk"                            
;
label values tm9286   tm9286l;
label define tm9286l 
	0           "Not applicable"                
	999999      "Per week - Skip to Check Item C1"
;
label values tm9288   tm9288l;
label define tm9288l 
	0           "Not applicable"                
	999999      "Biweekly - Skip to Check Item C1"
;
label values tm9290   tm9290l;
label define tm9290l 
	0           "Not applicable"                
	999999      "Per month - Skip to"           
;
label values tm9292   tm9292l;
label define tm9292l 
	0           "Not applicable"                
	999999      "Yearly - Skip to Check Item C1"
	-1          "Dk"                            
;
label values tm9294   tm9294l;
label define tm9294l 
	0           "Not applicable"                
	999999      "Per week - Skip to Check Item C1"
;
label values tm9296   tm9296l;
label define tm9296l 
	0           "Not applicable"                
	999999      "Biweekly - Skip to Check Item C1"
;
label values tm9298   tm9298l;
label define tm9298l 
	0           "Not applicable"                
	999999      "Per month - Skip to"           
;
label values tm6000   tm6000l;
label define tm6000l 
	0           "Not applicable"                
	1           "Yes - enter employer ID below" 
	2           "No - Skip to TM6136"           
;
label values tm6002   tm6002l;
label define tm6002l 
	0           "Not applicable"                
;
label values tm6004   tm6004l;
label define tm6004l 
	0           "Not applicable"                
;
label values tm6006   tm6006l;
label define tm6006l 
	0           "Not applicable"                
	1           "Under 10"                      
	2           "10 to 24"                      
	3           "25 to 99"                      
	4           "100 or more"                   
;
label values tm6008   tm6008l;
label define tm6008l 
	0           "Not applicable"                
	1           "Under 10"                      
	2           "10 to 24"                      
	3           "25 to 99"                      
	4           "100 or more"                   
;
label values tm6010   tm6010l;
label define tm6010l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6020"           
;
label values tm6012   tm6012l;
label define tm6012l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6020"           
;
label values tm6014   tm6014l;
label define tm6014l 
	0           "Not applicable"                
	1           "Under 10"                      
	2           "10 to 24"                      
	3           "25 to 99"                      
	4           "100 or more"                   
;
label values tm6016   tm6016l;
label define tm6016l 
	0           "Not applicable"                
	1           "Under 10"                      
	2           "10 to 24"                      
	3           "25 to 99"                      
	4           "100 or more"                   
;
label values tm6018   tm6018l;
label define tm6018l 
	0           "Not applicable"                
	1           "Yes - Skip to TM6026"          
	2           "No"                            
;
label values tm6020   tm6020l;
label define tm6020l 
	0           "Not applicable"                
	1           "Yes - Skip to TM6028"          
	2           "No"                            
;
label values tm6022   tm6022l;
label define tm6022l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6110"           
;
label values tm6024   tm6024l;
label define tm6024l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6112"           
;
label values tm6026   tm6026l;
label define tm6026l 
	0           "Not applicable"                
	1           "Yes - Skip to TM6062"          
	2           "No"                            
;
label values tm6028   tm6028l;
label define tm6028l 
	0           "Not applicable"                
	1           "Yes - Skip to TM6064"          
	2           "No"                            
;
label values tm6030   tm6030l;
label define tm6030l 
	0           "Not applicable"                
	1           "Chose not to belong"           
;
label values tm6032   tm6032l;
label define tm6032l 
	0           "Not applicable"                
	1           "Chose not to belong"           
;
label values tm6034   tm6034l;
label define tm6034l 
	0           "Not applicable"                
	1           "No one in...'s type of job"    
;
label values tm6036   tm6036l;
label define tm6036l 
	0           "Not applicable"                
	1           "No one in...'s type of job"    
;
label values tm6038   tm6038l;
label define tm6038l 
	0           "Not applicable"                
	1           "... does not work enough hours,"
;
label values tm6040   tm6040l;
label define tm6040l 
	0           "Not applicable"                
	1           "... does not work enough hours,"
;
label values tm6042   tm6042l;
label define tm6042l 
	0           "Not applicable"                
	1           "...  Started this job too close"
;
label values tm6044   tm6044l;
label define tm6044l 
	0           "Not applicable"                
	1           "...  Started this job too close"
;
label values tm6046   tm6046l;
label define tm6046l 
	0           "Not applicable"                
	1           "... is too young"              
;
label values tm6048   tm6048l;
label define tm6048l 
	0           "Not applicable"                
	1           "... is too young"              
;
label values tm6050   tm6050l;
label define tm6050l 
	0           "Not applicable"                
	1           "...  Has not worked for this"  
;
label values tm6052   tm6052l;
label define tm6052l 
	0           "Not applicable"                
	1           "...  Has not worked for this"  
;
label values tm6054   tm6054l;
label define tm6054l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm6056   tm6056l;
label define tm6056l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm6058   tm6058l;
label define tm6058l 
	0           "Not applicable"                
;
label values tm6060   tm6060l;
label define tm6060l 
	0           "Not applicable"                
;
label values tm6062   tm6062l;
label define tm6062l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6064   tm6064l;
label define tm6064l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6066   tm6066l;
label define tm6066l 
	0           "Not applicable"                
	1           "Based on years of service and" 
	2           "Based on the amount contributed"
	3           "Other"                         
;
label values tm6068   tm6068l;
label define tm6068l 
	0           "Not applicable"                
	1           "Based on years of service and" 
	2           "Based on the amount contributed"
	3           "Other"                         
;
label values tm6070   tm6070l;
label define tm6070l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6072   tm6072l;
label define tm6072l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6074   tm6074l;
label define tm6074l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6094"           
;
label values tm6076   tm6076l;
label define tm6076l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6096"           
;
label values tm6078   tm6078l;
label define tm6078l 
	0           "Not applicable"                
	999999      "Amount"                        
;
label values tm6080   tm6080l;
label define tm6080l 
	0           "Not applicable"                
	999999      "Amount"                        
;
label values tm6082   tm6082l;
label define tm6082l 
	0           "Not applicable"                
	1           "Week"                          
	2           "Biweekly"                      
	3           "Month"                         
	4           "Quarter"                       
	5           "Year"                          
;
label values tm6084   tm6084l;
label define tm6084l 
	0           "Not applicable"                
	1           "Week"                          
	2           "Biweekly"                      
	3           "Month"                         
	4           "Quarter"                       
	5           "Year"                          
;
label values tm6086   tm6086l;
label define tm6086l 
	0           "Not applicable"                
;
label values tm6088   tm6088l;
label define tm6088l 
	0           "Not applicable"                
;
label values tm6090   tm6090l;
label define tm6090l 
	0           "Not applicable"                
;
label values tm6092   tm6092l;
label define tm6092l 
	0           "Not applicable"                
;
label values tm6094   tm6094l;
label define tm6094l 
	0           "Not applicable"                
	1           "Less than a year"              
;
label values tm6096   tm6096l;
label define tm6096l 
	0           "Not applicable"                
	1           "Less than a year"              
;
label values tm6098   tm6098l;
label define tm6098l 
	0           "Not applicable"                
	1           "Yes - Skip to TM6106"          
	2           "No"                            
;
label values tm6100   tm6100l;
label define tm6100l 
	0           "Not applicable"                
	1           "Yes - Skip to TM6108"          
	2           "No"                            
;
label values tm6102   tm6102l;
label define tm6102l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6104   tm6104l;
label define tm6104l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6106   tm6106l;
label define tm6106l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6108   tm6108l;
label define tm6108l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6110   tm6110l;
label define tm6110l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6134"           
;
label values tm6112   tm6112l;
label define tm6112l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6134"           
;
label values tm6114   tm6114l;
label define tm6114l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6134"           
;
label values tm6116   tm6116l;
label define tm6116l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6134"           
;
label values tm6118   tm6118l;
label define tm6118l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6120   tm6120l;
label define tm6120l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6122   tm6122l;
label define tm6122l 
	0           "Not applicable"                
	90000       "Amount"                        
;
label values tm6124   tm6124l;
label define tm6124l 
	0           "Not applicable"                
	90000       "Amount"                        
;
label values tm6126   tm6126l;
label define tm6126l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6134"           
;
label values tm6128   tm6128l;
label define tm6128l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6134"           
;
label values tm6130   tm6130l;
label define tm6130l 
	0           "Not applicable"                
	1           "Same plan"                     
	2           "Different plan"                
;
label values tm6132   tm6132l;
label define tm6132l 
	0           "Not applicable"                
	1           "Same plan"                     
	2           "Different plan"                
;
label values tm6134   tm6134l;
label define tm6134l 
	0           "Not applicable"                
	1           "Yes - ask TM6008 for next"     
	2           "No - go to TM6136"             
;
label values tm6136   tm6136l;
label define tm6136l 
	0           "Not applicable"                
	1           "Yes - enter business ID number"
	2           "No - Skip to TM6146"           
;
label values tm6138   tm6138l;
label define tm6138l 
	0           "Not applicable"                
;
label values tm6140   tm6140l;
label define tm6140l 
	0           "Not applicable"                
;
label values tm6142   tm6142l;
label define tm6142l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6144   tm6144l;
label define tm6144l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm6146   tm6146l;
label define tm6146l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6164"           
;
label values tm6148   tm6148l;
label define tm6148l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM6164"           
;
label values tm6150   tm6150l;
label define tm6150l 
	0           "Not applicable"                
	1           "A private employer?"           
;
label values tm6152   tm6152l;
label define tm6152l 
	0           "Not applicable"                
	1           "Military?"                     
;
label values tm6154   tm6154l;
label define tm6154l 
	0           "Not applicable"                
	1           "Federal government (civilian)?"
;
label values tm6156   tm6156l;
label define tm6156l 
	0           "Not applicable"                
	1           "State or local government?"    
;
label values tm6158   tm6158l;
label define tm6158l 
	0           "Not applicable"                
	1           "A union?"                      
;
label values tm6160   tm6160l;
label define tm6160l 
	0           "Not applicable"                
	1           "Other?"                        
;
label values tm6162   tm6162l;
label define tm6162l 
	0           "Not applicable"                
;
label values tm6164   tm6164l;
label define tm6164l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8000"           
;
label values tm6166   tm6166l;
label define tm6166l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8000"           
;
label values tm6168   tm6168l;
label define tm6168l 
	0           "Not applicable"                
;
label values tm6170   tm6170l;
label define tm6170l 
	0           "Not applicable"                
;
label values tm6172   tm6172l;
label define tm6172l 
	0           "Not applicable"                
	66000       "Amount"                        
;
label values tm6174   tm6174l;
label define tm6174l 
	0           "Not applicable"                
	1           "Yes - Skip to TM8000"          
	2           "No"                            
;
label values tm6176   tm6176l;
label define tm6176l 
	0           "Not applicable"                
	1           "Purchased a home or paid off a"
;
label values tm6178   tm6178l;
label define tm6178l 
	0           "Not applicable"                
	1           "Used it for children's education"
;
label values tm6180   tm6180l;
label define tm6180l 
	0           "Not applicable"                
	1           "Used it for a period of"       
;
label values tm6182   tm6182l;
label define tm6182l 
	0           "Not applicable"                
	1           "Paid off loans, bills, or spent"
;
label values tm6184   tm6184l;
label define tm6184l 
	0           "Not applicable"                
	1           "Put it in a savings account"   
;
label values tm6186   tm6186l;
label define tm6186l 
	0           "Not applicable"                
	1           "Invested it in some other"     
;
label values tm6188   tm6188l;
label define tm6188l 
	0           "Not applicable"                
	1           "Used it to start or purchase a"
;
label values tm6190   tm6190l;
label define tm6190l 
	0           "Not applicable"                
	1           "Bought a car, boat, or other"  
;
label values tm6192   tm6192l;
label define tm6192l 
	0           "Not applicable"                
	1           "Paid medical or dental expenses"
;
label values tm6194   tm6194l;
label define tm6194l 
	0           "Not applicable"                
	1           "Used it for general everyday"  
;
label values tm6196   tm6196l;
label define tm6196l 
	0           "Not applicable"                
	1           "Other"                         
;
label values imp6006  imp6006l;
label define imp6006l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6008  imp6008l;
label define imp6008l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6010  imp6010l;
label define imp6010l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6012  imp6012l;
label define imp6012l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6014  imp6014l;
label define imp6014l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6016  imp6016l;
label define imp6016l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6018  imp6018l;
label define imp6018l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6020  imp6020l;
label define imp6020l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6022  imp6022l;
label define imp6022l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6024  imp6024l;
label define imp6024l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6026  imp6026l;
label define imp6026l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6028  imp6028l;
label define imp6028l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp30_54 imp30_5y;
label define imp30_5y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp32_56 imp32_5y;
label define imp32_5y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6062  imp6062l;
label define imp6062l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6064  imp6064l;
label define imp6064l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6066  imp6066l;
label define imp6066l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6068  imp6068l;
label define imp6068l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6070  imp6070l;
label define imp6070l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6072  imp6072l;
label define imp6072l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6074  imp6074l;
label define imp6074l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6076  imp6076l;
label define imp6076l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6078  imp6078l;
label define imp6078l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6086  imp6086l;
label define imp6086l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6088  imp6088l;
label define imp6088l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6094  imp6094l;
label define imp6094l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6096  imp6096l;
label define imp6096l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6098  imp6098l;
label define imp6098l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6100  imp6100l;
label define imp6100l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6102  imp6102l;
label define imp6102l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6104  imp6104l;
label define imp6104l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6106  imp6106l;
label define imp6106l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6108  imp6108l;
label define imp6108l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6110  imp6110l;
label define imp6110l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6112  imp6112l;
label define imp6112l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6114  imp6114l;
label define imp6114l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6116  imp6116l;
label define imp6116l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6118  imp6118l;
label define imp6118l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6120  imp6120l;
label define imp6120l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6122  imp6122l;
label define imp6122l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6124  imp6124l;
label define imp6124l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6126  imp6126l;
label define imp6126l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6128  imp6128l;
label define imp6128l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6130  imp6130l;
label define imp6130l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6132  imp6132l;
label define imp6132l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6142  imp6142l;
label define imp6142l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6144  imp6144l;
label define imp6144l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6148  imp6148l;
label define imp6148l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp50_60 imp50_6y;
label define imp50_6y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6162  imp6162l;
label define imp6162l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6166  imp6166l;
label define imp6166l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6168  imp6168l;
label define imp6168l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6170  imp6170l;
label define imp6170l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6172  imp6172l;
label define imp6172l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6174  imp6174l;
label define imp6174l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp76_96 imp76_9y;
label define imp76_9y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6178  imp6178l;
label define imp6178l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values cal6094  cal6094l;
label define cal6094l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values cal6096  cal6096l;
label define cal6096l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values cal6138  cal6138l;
label define cal6138l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values cal6140  cal6140l;
label define cal6140l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values cal6162  cal6162l;
label define cal6162l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9330"           
;
label values tm8001   tm8001l;
label define tm8001l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9330"           
;
label values tm8002   tm8002l;
label define tm8002l 
	0           "Not applicable"                
	1           "One"                           
	2           "Two"                           
	3           "Three or more"                 
;
label values tm8004   tm8004l;
label define tm8004l 
	0           "Not applicable"                
;
label values tm8006   tm8006l;
label define tm8006l 
	0           "Not applicable"                
;
label values tm8008   tm8008l;
label define tm8008l 
	0           "Not applicable"                
;
label values tm8010   tm8010l;
label define tm8010l 
	0           "Not applicable"                
;
label values tm8012   tm8012l;
label define tm8012l 
	0           "Not applicable"                
	1           "Monday through Friday (first"  
;
label values tm8014   tm8014l;
label define tm8014l 
	0           "Not applicable"                
	1           "Monday through Friday (second" 
;
label values tm8016   tm8016l;
label define tm8016l 
	0           "Not applicable"                
	1           "Sunday (first employer)"       
;
label values tm8018   tm8018l;
label define tm8018l 
	0           "Not applicable"                
	1           "Sunday (second employer)"      
;
label values tm8020   tm8020l;
label define tm8020l 
	0           "Not applicable"                
	1           "Monday (first employer)"       
;
label values tm8022   tm8022l;
label define tm8022l 
	0           "Not applicable"                
	1           "Monday (second employer)"      
;
label values tm8024   tm8024l;
label define tm8024l 
	0           "Not applicable"                
	1           "Tuesday (first employer)"      
;
label values tm8026   tm8026l;
label define tm8026l 
	0           "Not applicable"                
	1           "Tuesday (second employer)"     
;
label values tm8028   tm8028l;
label define tm8028l 
	0           "Not applicable"                
	1           "Wednesday (first employer)"    
;
label values tm8030   tm8030l;
label define tm8030l 
	0           "Not applicable"                
	1           "Wednesday (second employer)"   
;
label values tm8032   tm8032l;
label define tm8032l 
	0           "Not applicable"                
	1           "Thursday (first employer)"     
;
label values tm8034   tm8034l;
label define tm8034l 
	0           "Not applicable"                
	1           "Thursday (second employer)"    
;
label values tm8036   tm8036l;
label define tm8036l 
	0           "Not applicable"                
	1           "Friday (first employer)"       
;
label values tm8038   tm8038l;
label define tm8038l 
	0           "Not applicable"                
	1           "Friday (second employer)"      
;
label values tm8040   tm8040l;
label define tm8040l 
	0           "Not applicable"                
	1           "Saturday (first employer)"     
;
label values tm8042   tm8042l;
label define tm8042l 
	0           "Not applicable"                
	1           "Saturday (second employer)"    
;
label values tm8044   tm8044l;
label define tm8044l 
	0           "Not applicable"                
	-5          "All seven days (first employer)"
;
label values tm8046   tm8046l;
label define tm8046l 
	0           "Not applicable"                
	-5          "All seven days (second employer)"
;
label values tm8048   tm8048l;
label define tm8048l 
	0           "Not applicable"                
;
label values tm8050   tm8050l;
label define tm8050l 
	0           "Not applicable"                
	1           "A.M."                          
	2           "P.M."                          
;
label values tm8052   tm8052l;
label define tm8052l 
	0           "Not applicable"                
;
label values tm8054   tm8054l;
label define tm8054l 
	0           "Not applicable"                
	1           "A.M."                          
	2           "P.M."                          
;
label values tm8056   tm8056l;
label define tm8056l 
	0           "Not applicable"                
;
label values tm8058   tm8058l;
label define tm8058l 
	0           "Not applicable"                
	1           "A.M."                          
	2           "P.M."                          
;
label values tm8060   tm8060l;
label define tm8060l 
	0           "Not applicable"                
;
label values tm8062   tm8062l;
label define tm8062l 
	0           "Not applicable"                
	1           "A.M."                          
	2           "P.M."                          
;
label values tm8066   tm8066l;
label define tm8066l 
	0           "Not applicable"                
	-5          "Did not work at home (first"   
;
label values tm8067   tm8067l;
label define tm8067l 
	0           "Not applicable"                
	-5          "Did not work at home (second"  
;
label values tm8068   tm8068l;
label define tm8068l 
	0           "Not applicable"                
	1           "Monday through Friday (first"  
;
label values tm8069   tm8069l;
label define tm8069l 
	0           "Not applicable"                
	1           "Monday through Friday (second" 
;
label values tm8070   tm8070l;
label define tm8070l 
	0           "Not applicable"                
	1           "Sunday  (first Employer)"      
;
label values tm8071   tm8071l;
label define tm8071l 
	0           "Not applicable"                
	1           "Sunday (second employer)"      
;
label values tm8072   tm8072l;
label define tm8072l 
	0           "Not applicable"                
	1           "Monday  (first employer)"      
;
label values tm8073   tm8073l;
label define tm8073l 
	0           "Not applicable"                
	1           "Monday (second employer)"      
;
label values tm8074   tm8074l;
label define tm8074l 
	0           "Not applicable"                
	1           "Tuesday (first employer)"      
;
label values tm8075   tm8075l;
label define tm8075l 
	0           "Not applicable"                
	1           "Tuesday (second employer)"     
;
label values tm8076   tm8076l;
label define tm8076l 
	0           "Not applicable"                
	1           "Wednesday (first employer)"    
;
label values tm8077   tm8077l;
label define tm8077l 
	0           "Not applicable"                
	1           "Wednesday (second employer)"   
;
label values tm8078   tm8078l;
label define tm8078l 
	0           "Not applicable"                
	1           "Thursday (first employer)"     
;
label values tm8079   tm8079l;
label define tm8079l 
	0           "Not applicable"                
	1           "Thursday (second employer)"    
;
label values tm8080   tm8080l;
label define tm8080l 
	0           "Not applicable"                
	1           "Friday (first employer)"       
;
label values tm8081   tm8081l;
label define tm8081l 
	0           "Not applicable"                
	1           "Friday (second employer)"      
;
label values tm8082   tm8082l;
label define tm8082l 
	0           "Not applicable"                
	1           "Saturday (first employer)"     
;
label values tm8083   tm8083l;
label define tm8083l 
	0           "Not applicable"                
	1           "Saturday (second employer)"    
;
label values tm8084   tm8084l;
label define tm8084l 
	0           "Not applicable"                
	-5          "All seven days (first employer)"
;
label values tm8085   tm8085l;
label define tm8085l 
	0           "Not applicable"                
	-5          "All seven days (second employer)"
;
label values tm8086   tm8086l;
label define tm8086l 
	0           "Not applicable"                
	1           "Regular daytime schedule"      
	2           "Regular evening shift"         
	3           "Regular night shift"           
	4           "Rotating shift (one that changes"
	5           "Split shift (one consisting of"
	6           "Irregular schedule (one that"  
	7           "Other"                         
;
label values tm8087   tm8087l;
label define tm8087l 
	0           "Not applicable"                
	1           "Regular daytime schedule"      
	2           "Regular evening shift"         
	3           "Regular night shift"           
	4           "Rotating shift (one that changes"
	5           "Split shift (one consisting of"
	6           "Irregular schedule (one that"  
	7           "Other"                         
;
label values tm8088   tm8088l;
label define tm8088l 
	0           "Not applicable"                
	1           "Better child care arrangements"
	2           "Better pay"                    
	3           "Better arrangements for care of"
	4           "Allows time for school"        
	5           "Other voluntary reasons"       
	6           "Could not get any other job"   
	7           "Requirement of the job"        
	8           "Other involuntary reasons"     
;
label values tm8089   tm8089l;
label define tm8089l 
	0           "Not applicable"                
	1           "Better child care arrangements"
	2           "Better pay"                    
	3           "Better arrangements for care of"
	4           "Allows time for school"        
	5           "Other voluntary reasons"       
	6           "Could not get any other job"   
	7           "Requirements of the job"       
	8           "Other involuntary reasons"     
;
label values tm8090   tm8090l;
label define tm8090l 
	0           "Not applicable"                
	1           "Yes - ask items TM8006 though" 
	2           "No - go to Check Item"         
;
label values imp8001  imp8001l;
label define imp8001l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8004  imp8004l;
label define imp8004l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8006  imp8006l;
label define imp8006l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8050  imp8050l;
label define imp8050l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8054  imp8054l;
label define imp8054l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8086  imp8086l;
label define imp8086l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8087  imp8087l;
label define imp8087l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8088  imp8088l;
label define imp8088l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8089  imp8089l;
label define imp8089l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp08_40 imp08_4y;
label define imp08_4y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp10_42 imp10_4y;
label define imp10_4y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp16_40 imp16_4y;
label define imp16_4y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp18_42 imp18_4y;
label define imp18_4y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp48_50 imp48_5y;
label define imp48_5y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp48_58 imp48_5k;
label define imp48_5k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp52_54 imp52_5y;
label define imp52_5y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp52_62 imp52_6y;
label define imp52_6y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp56_58 imp56_5y;
label define imp56_5y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp60_62 imp60_6y;
label define imp60_6y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp66_82 imp66_8y;
label define imp66_8y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp67_83 imp67_8y;
label define imp67_8y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm9330   tm9330l;
label define tm9330l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip TM7000"              
;
label values tm9332   tm9332l;
label define tm9332l 
	0           "Not applicable"                
;
label values tm9334   tm9334l;
label define tm9334l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Check Item TM9338"
;
label values tm9336   tm9336l;
label define tm9336l 
	0           "Not applicable"                
	1           "Yes - Skip to Check Item TM9342"
	2           "No"                            
;
label values tm9338   tm9338l;
label define tm9338l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Check Item TM9344"
;
label values tm9340   tm9340l;
label define tm9340l 
	0           "Not applicable"                
	-1          "Hours varied"                  
	-2          "Don't know"                    
	-3          "Not enrolled last month"       
;
label values tm9342   tm9342l;
label define tm9342l 
	0           "Not applicable"                
	1           "Yes - Skip to TM9348"          
	2           "No"                            
;
label values tm9344   tm9344l;
label define tm9344l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Statement G"      
;
label values tm9346   tm9346l;
label define tm9346l 
	0           "Not applicable"                
	-1          "Hours varied - Skip to"        
	-2          "Don't know"                    
	-3          "Did not look for a job last"   
;
label values tm9348   tm9348l;
label define tm9348l 
	0           "Not applicable"                
;
label values tm9350   tm9350l;
label define tm9350l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to next child TM9546"
;
label values tm9352   tm9352l;
label define tm9352l 
	0           "Not applicable"                
	1           "Yes - ask TM9354 and TM9356"   
	2           "No"                            
;
label values tm9354   tm9354l;
label define tm9354l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9356   tm9356l;
label define tm9356l 
	0           "Not applicable"                
;
label values tm9358   tm9358l;
label define tm9358l 
	0           "Not applicable"                
	1           "Yes - ask TM9360 and TM9362"   
	2           "No"                            
;
label values tm9360   tm9360l;
label define tm9360l 
	0           "Not applicable"                
	1           "In ...'s home"                 
	2           "At work/at school"             
	3           "Some place else"               
;
label values tm9362   tm9362l;
label define tm9362l 
	0           "Not applicable"                
;
label values tm9364   tm9364l;
label define tm9364l 
	0           "Not applicable"                
	1           "Yes - ask TM9366 and TM9368"   
	2           "No"                            
;
label values tm9366   tm9366l;
label define tm9366l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9368   tm9368l;
label define tm9368l 
	0           "Not applicable"                
;
label values tm9370   tm9370l;
label define tm9370l 
	0           "Not applicable"                
	1           "Yes - ask TM9372 and TM9374"   
	2           "No"                            
;
label values tm9372   tm9372l;
label define tm9372l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9374   tm9374l;
label define tm9374l 
	0           "Not applicable"                
;
label values tm9376   tm9376l;
label define tm9376l 
	0           "Not applicable"                
	1           "Yes - ask TM9378 and TM9380"   
	2           "No"                            
;
label values tm9378   tm9378l;
label define tm9378l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9380   tm9380l;
label define tm9380l 
	0           "Not applicable"                
;
label values tm9382   tm9382l;
label define tm9382l 
	0           "Not applicable"                
	1           "Yes - ask TM9384 and TM9386"   
	2           "No"                            
;
label values tm9384   tm9384l;
label define tm9384l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Other place"                   
;
label values tm9386   tm9386l;
label define tm9386l 
	0           "Not applicable"                
;
label values tm9388   tm9388l;
label define tm9388l 
	0           "Not applicable"                
	1           "Yes - ask  - TM9390"           
	2           "No"                            
;
label values tm9390   tm9390l;
label define tm9390l 
	0           "Not applicable"                
;
label values tm9392   tm9392l;
label define tm9392l 
	0           "Not applicable"                
	1           "Yes - ask TM9394 and TM9396"   
	2           "No"                            
;
label values tm9394   tm9394l;
label define tm9394l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9396   tm9396l;
label define tm9396l 
	0           "Not applicable"                
;
label values tm9398   tm9398l;
label define tm9398l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older"      
;
label values tm9400   tm9400l;
label define tm9400l 
	0           "Not applicable"                
	1           "Yes - ask TM9402 and TM9404"   
	2           "No"                            
;
label values tm9402   tm9402l;
label define tm9402l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9404   tm9404l;
label define tm9404l 
	0           "Not applicable"                
;
label values tm9406   tm9406l;
label define tm9406l 
	0           "Not applicable"                
	1           "Yes - ask TM9408 and TM9410"   
	2           "No"                            
;
label values tm9408   tm9408l;
label define tm9408l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9410   tm9410l;
label define tm9410l 
	0           "Not applicable"                
;
label values tm9412   tm9412l;
label define tm9412l 
	0           "Not applicable"                
	1           "Yes - ask TM9414"              
	2           "No"                            
;
label values tm9414   tm9414l;
label define tm9414l 
	0           "Not applicable"                
;
label values tm9416   tm9416l;
label define tm9416l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip to"
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9418   tm9418l;
label define tm9418l 
	0           "Not applicable"                
	1           "Yes - ask TM9420 and TM9422"   
	2           "No"                            
;
label values tm9420   tm9420l;
label define tm9420l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9422   tm9422l;
label define tm9422l 
	0           "Not applicable"                
;
label values tm9424   tm9424l;
label define tm9424l 
	0           "Not applicable"                
	1           "Yes - ask TM9426 and TM9428"   
	2           "No"                            
;
label values tm9426   tm9426l;
label define tm9426l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9428   tm9428l;
label define tm9428l 
	0           "Not applicable"                
;
label values tm9430   tm9430l;
label define tm9430l 
	0           "Not applicable"                
	1           "Yes - ask TM9432 and TM9434"   
	2           "No"                            
;
label values tm9432   tm9432l;
label define tm9432l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9434   tm9434l;
label define tm9434l 
	0           "Not applicable"                
;
label values tm9436   tm9436l;
label define tm9436l 
	0           "Not applicable"                
	1           "Yes - ask TM9438 and TM9440"   
	2           "No"                            
;
label values tm9438   tm9438l;
label define tm9438l 
	0           "Not applicable"                
	1           "At work"                       
	2           "At school"                     
	3           "Someplace else"                
;
label values tm9440   tm9440l;
label define tm9440l 
	0           "Not applicable"                
;
label values tm9442   tm9442l;
label define tm9442l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9446"           
;
label values tm9444   tm9444l;
label define tm9444l 
	0           "Not applicable"                
;
label values tm9446   tm9446l;
label define tm9446l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9450"           
;
label values tm9448   tm9448l;
label define tm9448l 
	0           "Not applicable"                
;
label values tm9450   tm9450l;
label define tm9450l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9454"           
;
label values tm9452   tm9452l;
label define tm9452l 
	0           "Not applicable"                
	-4          "Less than an hour"             
;
label values tm9454   tm9454l;
label define tm9454l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9458"           
;
label values tm9456   tm9456l;
label define tm9456l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9458   tm9458l;
label define tm9458l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9462"           
;
label values tm9460   tm9460l;
label define tm9460l 
	0           "Not applicable"                
;
label values tm9462   tm9462l;
label define tm9462l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Statement F"      
;
label values tm9464   tm9464l;
label define tm9464l 
	0           "Not applicable"                
	1           "Yes, respondent lost time"     
	2           "Yes, spouse lost time"         
	3           "Both respondent an spouse lost"
	4           "No"                            
	-1          "Don't know"                    
;
label values tm9465   tm9465l;
label define tm9465l 
	0           "Not applicable"                
;
label values tm9466   tm9466l;
label define tm9466l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9546"           
;
label values tm9468   tm9468l;
label define tm9468l 
	0           "Not applicable"                
	1           "Yes - ask TM9470 and TM9472"   
	2           "No"                            
;
label values tm9470   tm9470l;
label define tm9470l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9472   tm9472l;
label define tm9472l 
	0           "Not applicable"                
;
label values tm9474   tm9474l;
label define tm9474l 
	0           "Not applicable"                
	1           "Yes - ask TM9476 and TM9478"   
	2           "No"                            
;
label values tm9476   tm9476l;
label define tm9476l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Someplace else"                
;
label values tm9478   tm9478l;
label define tm9478l 
	0           "Not applicable"                
;
label values tm9480   tm9480l;
label define tm9480l 
	0           "Not applicable"                
	1           "Yes - ask TM9482"              
	2           "No"                            
;
label values tm9482   tm9482l;
label define tm9482l 
	0           "Not applicable"                
;
label values tm9484   tm9484l;
label define tm9484l 
	0           "Not applicable"                
	1           "Yes - ask TM9486 and TM9488"   
	2           "No"                            
;
label values tm9486   tm9486l;
label define tm9486l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9488   tm9488l;
label define tm9488l 
	0           "Not applicable"                
;
label values tm9490   tm9490l;
label define tm9490l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older - Skip"
;
label values tm9492   tm9492l;
label define tm9492l 
	0           "Not applicable"                
	1           "Yes - ask TM9494"              
	2           "No"                            
;
label values tm9494   tm9494l;
label define tm9494l 
	0           "Not applicable"                
;
label values tm9496   tm9496l;
label define tm9496l 
	0           "Not applicable"                
	1           "Yes - ask TM9498"              
	2           "No"                            
;
label values tm9498   tm9498l;
label define tm9498l 
	0           "Not applicable"                
;
label values tm9500   tm9500l;
label define tm9500l 
	0           "Not applicable"                
	1           "Yes - ask TM9502"              
	2           "No"                            
;
label values tm9502   tm9502l;
label define tm9502l 
	0           "Not applicable"                
;
label values tm9504   tm9504l;
label define tm9504l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip to"
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9506   tm9506l;
label define tm9506l 
	0           "Not applicable"                
	1           "Yes - ask TM9508 and 9510"     
	2           "No"                            
;
label values tm9508   tm9508l;
label define tm9508l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9510   tm9510l;
label define tm9510l 
	0           "Not applicable"                
;
label values tm9512   tm9512l;
label define tm9512l 
	0           "Not applicable"                
	1           "Yes - ask TM9514 and 9516"     
	2           "No"                            
;
label values tm9514   tm9514l;
label define tm9514l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9516   tm9516l;
label define tm9516l 
	0           "Not applicable"                
;
label values tm9518   tm9518l;
label define tm9518l 
	0           "Not applicable"                
	1           "Yes - ask TM9520 and 9522"     
	2           "No"                            
;
label values tm9520   tm9520l;
label define tm9520l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9522   tm9522l;
label define tm9522l 
	0           "Not applicable"                
;
label values tm9524   tm9524l;
label define tm9524l 
	0           "Not applicable"                
	1           "Yes - ask TM9526 and 9528"     
	2           "No"                            
;
label values tm9526   tm9526l;
label define tm9526l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9528   tm9528l;
label define tm9528l 
	0           "Not applicable"                
;
label values tm9530   tm9530l;
label define tm9530l 
	0           "Not applicable"                
	1           "Yes - Skip to TM9544"          
	2           "No"                            
;
label values tm9532   tm9532l;
label define tm9532l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9536"           
;
label values tm9534   tm9534l;
label define tm9534l 
	0           "Not applicable"                
;
label values tm9536   tm9536l;
label define tm9536l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9540"           
;
label values tm9538   tm9538l;
label define tm9538l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9540   tm9540l;
label define tm9540l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9544"           
;
label values tm9542   tm9542l;
label define tm9542l 
	0           "Not applicable"                
;
label values tm9544   tm9544l;
label define tm9544l 
	0           "Not applicable"                
	-1          "None"                          
;
label values tm9546   tm9546l;
label define tm9546l 
	0           "Not applicable"                
	1           "Yes - go to TM9548 for second" 
	2           "No - go to TM7000"             
;
label values tm9548   tm9548l;
label define tm9548l 
	0           "Not applicable"                
	1           "Yes - Skip to Statement J"     
	2           "No"                            
;
label values tm9550   tm9550l;
label define tm9550l 
	0           "Not applicable"                
;
label values tm9552   tm9552l;
label define tm9552l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to next child TM9748"
;
label values tm9554   tm9554l;
label define tm9554l 
	0           "Not applicable"                
	1           "Yes - ask TM9556 and TM9598"   
	2           "No"                            
;
label values tm9556   tm9556l;
label define tm9556l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9558   tm9558l;
label define tm9558l 
	0           "Not applicable"                
;
label values tm9560   tm9560l;
label define tm9560l 
	0           "Not applicable"                
	1           "Yes - ask TM9562 and TM9564"   
	2           "No"                            
;
label values tm9562   tm9562l;
label define tm9562l 
	0           "Not applicable"                
	1           "In ...'s home"                 
	2           "At work/at school"             
	3           "Someplace else"                
;
label values tm9564   tm9564l;
label define tm9564l 
	0           "Not applicable"                
;
label values tm9566   tm9566l;
label define tm9566l 
	0           "Not applicable"                
	1           "Yes - ask TM9568 and TM9570"   
	2           "No"                            
;
label values tm9568   tm9568l;
label define tm9568l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9570   tm9570l;
label define tm9570l 
	0           "Not applicable"                
;
label values tm9572   tm9572l;
label define tm9572l 
	0           "Not applicable"                
	1           "Yes - ask TM9574 and TM9576"   
	2           "No"                            
;
label values tm9574   tm9574l;
label define tm9574l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9576   tm9576l;
label define tm9576l 
	0           "Not applicable"                
;
label values tm9578   tm9578l;
label define tm9578l 
	0           "Not applicable"                
	1           "Yes - ask TM9580 and TM9582"   
	2           "No"                            
;
label values tm9580   tm9580l;
label define tm9580l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9582   tm9582l;
label define tm9582l 
	0           "Not applicable"                
;
label values tm9584   tm9584l;
label define tm9584l 
	0           "Not applicable"                
	1           "Yes - ask TM9586 and TM9588"   
	2           "No"                            
;
label values tm9586   tm9586l;
label define tm9586l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Other place"                   
;
label values tm9588   tm9588l;
label define tm9588l 
	0           "Not applicable"                
;
label values tm9590   tm9590l;
label define tm9590l 
	0           "Not applicable"                
	1           "Yes - ask TM9592"              
	2           "No"                            
;
label values tm9592   tm9592l;
label define tm9592l 
	0           "Not applicable"                
;
label values tm9594   tm9594l;
label define tm9594l 
	0           "Not applicable"                
	1           "Yes - ask TM9596 and TM9598"   
	2           "No"                            
;
label values tm9596   tm9596l;
label define tm9596l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9598   tm9598l;
label define tm9598l 
	0           "Not applicable"                
;
label values tm9600   tm9600l;
label define tm9600l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older - Skip"
;
label values tm9602   tm9602l;
label define tm9602l 
	0           "Not applicable"                
	1           "Yes - ask TM9604 and TM9606"   
	2           "No"                            
;
label values tm9604   tm9604l;
label define tm9604l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9606   tm9606l;
label define tm9606l 
	0           "Not applicable"                
;
label values tm9608   tm9608l;
label define tm9608l 
	0           "Not applicable"                
	1           "Yes - ask TM9610 and TM9612"   
	2           "No"                            
;
label values tm9610   tm9610l;
label define tm9610l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9612   tm9612l;
label define tm9612l 
	0           "Not applicable"                
;
label values tm9614   tm9614l;
label define tm9614l 
	0           "Not applicable"                
	1           "Yes - ask TM9616"              
	2           "No"                            
;
label values tm9616   tm9616l;
label define tm9616l 
	0           "Not applicable"                
;
label values tm9618   tm9618l;
label define tm9618l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip to"
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9620   tm9620l;
label define tm9620l 
	0           "Not applicable"                
	1           "Yes - ask TM9622 and TM9624"   
	2           "No"                            
;
label values tm9622   tm9622l;
label define tm9622l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9624   tm9624l;
label define tm9624l 
	0           "Not applicable"                
;
label values tm9626   tm9626l;
label define tm9626l 
	0           "Not applicable"                
	1           "Yes - ask TM9628 and TM9630"   
	2           "No"                            
;
label values tm9628   tm9628l;
label define tm9628l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9630   tm9630l;
label define tm9630l 
	0           "Not applicable"                
;
label values tm9632   tm9632l;
label define tm9632l 
	0           "Not applicable"                
	1           "Yes - ask TM9634 and TM9636"   
	2           "No"                            
;
label values tm9634   tm9634l;
label define tm9634l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9636   tm9636l;
label define tm9636l 
	0           "Not applicable"                
;
label values tm9638   tm9638l;
label define tm9638l 
	0           "Not applicable"                
	1           "Yes - ask TM9640 and TM9642"   
	2           "No"                            
;
label values tm9640   tm9640l;
label define tm9640l 
	0           "Not applicable"                
	1           "At work"                       
	2           "At school"                     
	3           "Someplace else"                
;
label values tm9642   tm9642l;
label define tm9642l 
	0           "Not applicable"                
;
label values tm9644   tm9644l;
label define tm9644l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9648"           
;
label values tm9646   tm9646l;
label define tm9646l 
	0           "Not applicable"                
;
label values tm9648   tm9648l;
label define tm9648l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9652"           
;
label values tm9650   tm9650l;
label define tm9650l 
	0           "Not applicable"                
;
label values tm9652   tm9652l;
label define tm9652l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9656"           
;
label values tm9654   tm9654l;
label define tm9654l 
	0           "Not applicable"                
;
label values tm9656   tm9656l;
label define tm9656l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9660"           
;
label values tm9658   tm9658l;
label define tm9658l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9660   tm9660l;
label define tm9660l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9664"           
;
label values tm9662   tm9662l;
label define tm9662l 
	0           "Not applicable"                
;
label values tm9664   tm9664l;
label define tm9664l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Statement I"      
;
label values tm9666   tm9666l;
label define tm9666l 
	0           "Not applicable"                
	1           "Yes, respondent lost time"     
	2           "Yes, spouse lost time"         
	3           "Both respondent and spouse lost"
	4           "No"                            
;
label values tm9667   tm9667l;
label define tm9667l 
	0           "Not applicable"                
;
label values tm9668   tm9668l;
label define tm9668l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to next child TM9748"
;
label values tm9670   tm9670l;
label define tm9670l 
	0           "Not applicable"                
	1           "Yes - ask TM9672 and TM9674"   
	2           "No"                            
;
label values tm9672   tm9672l;
label define tm9672l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9674   tm9674l;
label define tm9674l 
	0           "Not applicable"                
;
label values tm9676   tm9676l;
label define tm9676l 
	0           "Not applicable"                
	1           "Yes - ask TM9678 and TM9680"   
	2           "No"                            
;
label values tm9678   tm9678l;
label define tm9678l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Someplace else"                
;
label values tm9680   tm9680l;
label define tm9680l 
	0           "Not applicable"                
;
label values tm9682   tm9682l;
label define tm9682l 
	0           "Not applicable"                
	1           "Yes - ask TM9684"              
	2           "No"                            
;
label values tm9684   tm9684l;
label define tm9684l 
	0           "Not applicable"                
;
label values tm9686   tm9686l;
label define tm9686l 
	0           "Not applicable"                
	1           "Yes - ask TM9688 and TM9690"   
	2           "No"                            
;
label values tm9688   tm9688l;
label define tm9688l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9692   tm9692l;
label define tm9692l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older - Skip"
;
label values tm9694   tm9694l;
label define tm9694l 
	0           "Not applicable"                
	1           "Yes - ask TM9696"              
	2           "No"                            
;
label values tm9696   tm9696l;
label define tm9696l 
	0           "Not applicable"                
;
label values tm9698   tm9698l;
label define tm9698l 
	0           "Not applicable"                
	1           "Yes - ask TM9700"              
	2           "No"                            
;
label values tm9700   tm9700l;
label define tm9700l 
	0           "Not applicable"                
;
label values tm9702   tm9702l;
label define tm9702l 
	0           "Not applicable"                
	1           "Yes - ask - TM9704"            
	2           "No"                            
;
label values tm9704   tm9704l;
label define tm9704l 
	0           "Not applicable"                
;
label values tm9706   tm9706l;
label define tm9706l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip to"
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9708   tm9708l;
label define tm9708l 
	0           "Not applicable"                
	1           "Yes - ask TM9710 and 9712"     
	2           "No"                            
;
label values tm9710   tm9710l;
label define tm9710l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9712   tm9712l;
label define tm9712l 
	0           "Not applicable"                
;
label values tm9714   tm9714l;
label define tm9714l 
	0           "Not applicable"                
	1           "Yes - ask 9716 and 9718"       
	2           "No"                            
;
label values tm9716   tm9716l;
label define tm9716l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9718   tm9718l;
label define tm9718l 
	0           "Not applicable"                
;
label values tm9720   tm9720l;
label define tm9720l 
	0           "Not applicable"                
	1           "Yes - ask TM9722 and 9724"     
	2           "No"                            
;
label values tm9722   tm9722l;
label define tm9722l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9724   tm9724l;
label define tm9724l 
	0           "Not applicable"                
;
label values tm9726   tm9726l;
label define tm9726l 
	0           "Not applicable"                
	1           "Yes - ask TM9728 and 9730"     
	2           "No"                            
;
label values tm9728   tm9728l;
label define tm9728l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9730   tm9730l;
label define tm9730l 
	0           "Not applicable"                
;
label values tm9732   tm9732l;
label define tm9732l 
	0           "Not applicable"                
	1           "Yes - Skip to TM9746"          
	2           "No"                            
;
label values tm9734   tm9734l;
label define tm9734l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9738"           
;
label values tm9736   tm9736l;
label define tm9736l 
	0           "Not applicable"                
;
label values tm9738   tm9738l;
label define tm9738l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9742"           
;
label values tm9740   tm9740l;
label define tm9740l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9742   tm9742l;
label define tm9742l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9746"           
;
label values tm9744   tm9744l;
label define tm9744l 
	0           "Not applicable"                
;
label values tm9746   tm9746l;
label define tm9746l 
	0           "Not applicable"                
	-1          "None Check"                    
;
label values tm9748   tm9748l;
label define tm9748l 
	0           "Not applicable"                
	1           "Yes - go to third child TM9751"
	2           "No - go to TM7000"             
;
label values tm9750   tm9750l;
label define tm9750l 
	0           "Not applicable"                
	1           "Yes - Skip to Statement M"     
	2           "No"                            
;
label values tm9751   tm9751l;
label define tm9751l 
	0           "Not applicable"                
;
label values tm9752   tm9752l;
label define tm9752l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "NO - SKIP TO NEXT CHILD TM9851"
;
label values tm9753   tm9753l;
label define tm9753l 
	0           "Not applicable"                
	1           "Yes - ask TM9754 and TM9755"   
	2           "No"                            
;
label values tm9754   tm9754l;
label define tm9754l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9755   tm9755l;
label define tm9755l 
	0           "Not applicable"                
;
label values tm9756   tm9756l;
label define tm9756l 
	0           "Not applicable"                
	1           "Yes - ask TM9757 and TM9758"   
	2           "No"                            
;
label values tm9757   tm9757l;
label define tm9757l 
	0           "Not applicable"                
	1           "In ...'s home"                 
	2           "At work/at school"             
	3           "Someplace else"                
;
label values tm9758   tm9758l;
label define tm9758l 
	0           "Not applicable"                
;
label values tm9759   tm9759l;
label define tm9759l 
	0           "Not applicable"                
	1           "Yes - ask TM9760 and TM9761"   
	2           "No"                            
;
label values tm9760   tm9760l;
label define tm9760l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9761   tm9761l;
label define tm9761l 
	0           "Not applicable"                
;
label values tm9762   tm9762l;
label define tm9762l 
	0           "Not applicable"                
	1           "Yes - ask TM9763 and TM9764"   
	2           "No"                            
;
label values tm9763   tm9763l;
label define tm9763l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9764   tm9764l;
label define tm9764l 
	0           "Not applicable"                
;
label values tm9765   tm9765l;
label define tm9765l 
	0           "Not applicable"                
	1           "Yes - ask TM9766 and TM9767"   
	2           "No"                            
;
label values tm9766   tm9766l;
label define tm9766l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9767   tm9767l;
label define tm9767l 
	0           "Not applicable"                
;
label values tm9768   tm9768l;
label define tm9768l 
	0           "Not applicable"                
	1           "Yes - ask TM9769 and TM9770"   
	2           "No"                            
;
label values tm9769   tm9769l;
label define tm9769l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Other place"                   
;
label values tm9770   tm9770l;
label define tm9770l 
	0           "Not applicable"                
;
label values tm9771   tm9771l;
label define tm9771l 
	0           "Not applicable"                
	1           "Yes - ask - TM9772"            
	2           "No"                            
;
label values tm9772   tm9772l;
label define tm9772l 
	0           "Not applicable"                
;
label values tm9773   tm9773l;
label define tm9773l 
	0           "Not applicable"                
	1           "Yes - ask TM9774 and TM9775"   
	2           "No"                            
;
label values tm9774   tm9774l;
label define tm9774l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9775   tm9775l;
label define tm9775l 
	0           "Not applicable"                
;
label values tm9776   tm9776l;
label define tm9776l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older - Skip"
;
label values tm9777   tm9777l;
label define tm9777l 
	0           "Not applicable"                
	1           "Yes - ask TM9778 and TM9779"   
	2           "No"                            
;
label values tm9778   tm9778l;
label define tm9778l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9779   tm9779l;
label define tm9779l 
	0           "Not applicable"                
;
label values tm9780   tm9780l;
label define tm9780l 
	0           "Not applicable"                
	1           "Yes - ask TM9781 and TM9782"   
	2           "No"                            
;
label values tm9781   tm9781l;
label define tm9781l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9782   tm9782l;
label define tm9782l 
	0           "Not applicable"                
;
label values tm9783   tm9783l;
label define tm9783l 
	0           "Not applicable"                
	1           "Yes - ask TM9484"              
	2           "No"                            
;
label values tm9784   tm9784l;
label define tm9784l 
	0           "Not applicable"                
;
label values tm9785   tm9785l;
label define tm9785l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip to"
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9786   tm9786l;
label define tm9786l 
	0           "Not applicable"                
	1           "Yes - ask TM9787 and TM9788"   
	2           "No"                            
;
label values tm9787   tm9787l;
label define tm9787l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9788   tm9788l;
label define tm9788l 
	0           "Not applicable"                
;
label values tm9789   tm9789l;
label define tm9789l 
	0           "Not applicable"                
	1           "Yes - ask TM9790 and TM9791"   
	2           "No"                            
;
label values tm9790   tm9790l;
label define tm9790l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9791   tm9791l;
label define tm9791l 
	0           "Not applicable"                
;
label values tm9792   tm9792l;
label define tm9792l 
	0           "Not applicable"                
	1           "Yes - ask TM9792 and TM9793"   
	2           "No"                            
;
label values tm9793   tm9793l;
label define tm9793l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9794   tm9794l;
label define tm9794l 
	0           "Not applicable"                
;
label values tm9795   tm9795l;
label define tm9795l 
	0           "Not applicable"                
	1           "Yes - ask TM9796 and TM9797"   
	2           "No"                            
;
label values tm9796   tm9796l;
label define tm9796l 
	0           "Not applicable"                
	1           "At work"                       
	2           "At school"                     
	3           "Someplace else"                
;
label values tm9797   tm9797l;
label define tm9797l 
	0           "Not applicable"                
;
label values tm9798   tm9798l;
label define tm9798l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9800"           
;
label values tm9799   tm9799l;
label define tm9799l 
	0           "Not applicable"                
;
label values tm9800   tm9800l;
label define tm9800l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9802"           
;
label values tm9801   tm9801l;
label define tm9801l 
	0           "Not applicable"                
;
label values tm9802   tm9802l;
label define tm9802l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9804"           
;
label values tm9803   tm9803l;
label define tm9803l 
	0           "Not applicable"                
;
label values tm9804   tm9804l;
label define tm9804l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9806"           
;
label values tm9805   tm9805l;
label define tm9805l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9806   tm9806l;
label define tm9806l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9808"           
;
label values tm9807   tm9807l;
label define tm9807l 
	0           "Not applicable"                
;
label values tm9808   tm9808l;
label define tm9808l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Statement L"      
;
label values tm9809   tm9809l;
label define tm9809l 
	0           "Not applicable"                
	1           "Yes, respondent lost time"     
	2           "Yes, spouse lost time"         
	3           "Both respondent an spouse lost"
	4           "No"                            
;
label values tm9810   tm9810l;
label define tm9810l 
	0           "Not applicable"                
;
label values tm9811   tm9811l;
label define tm9811l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to next child TM9851"
;
label values tm9812   tm9812l;
label define tm9812l 
	0           "Not applicable"                
	1           "Yes - ask TM9813 and TM9814"   
	2           "No"                            
;
label values tm9813   tm9813l;
label define tm9813l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9814   tm9814l;
label define tm9814l 
	0           "Not applicable"                
;
label values tm9815   tm9815l;
label define tm9815l 
	0           "Not applicable"                
	1           "Yes - ask TM9816 and TM9817"   
	2           "No"                            
;
label values tm9816   tm9816l;
label define tm9816l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Someplace else"                
;
label values tm9817   tm9817l;
label define tm9817l 
	0           "Not applicable"                
;
label values tm9818   tm9818l;
label define tm9818l 
	0           "Not applicable"                
	1           "Yes - ask TM9819"              
	2           "No"                            
;
label values tm9819   tm9819l;
label define tm9819l 
	0           "Not applicable"                
;
label values tm9820   tm9820l;
label define tm9820l 
	0           "Not applicable"                
	1           "Yes - ask TM9821 and TM9822"   
	2           "No"                            
;
label values tm9821   tm9821l;
label define tm9821l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9822   tm9822l;
label define tm9822l 
	0           "Not applicable"                
;
label values tm9823   tm9823l;
label define tm9823l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older - Skip"
;
label values tm9824   tm9824l;
label define tm9824l 
	0           "Not applicable"                
	1           "Yes - ask TM9825"              
	2           "No"                            
;
label values tm9825   tm9825l;
label define tm9825l 
	0           "Not applicable"                
;
label values tm9826   tm9826l;
label define tm9826l 
	0           "Not applicable"                
	1           "Yes - ask TM9827"              
	2           "No"                            
;
label values tm9827   tm9827l;
label define tm9827l 
	0           "Not applicable"                
;
label values tm9828   tm9828l;
label define tm9828l 
	0           "Not applicable"                
	1           "Yes - ask TM9829"              
	2           "No"                            
;
label values tm9829   tm9829l;
label define tm9829l 
	0           "Not applicable"                
;
label values tm9830   tm9830l;
label define tm9830l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip to"
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9831   tm9831l;
label define tm9831l 
	0           "Not applicable"                
	1           "Yes - ask TM9832 and TM9833"   
	2           "No"                            
;
label values tm9832   tm9832l;
label define tm9832l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9833   tm9833l;
label define tm9833l 
	0           "Not applicable"                
;
label values tm9834   tm9834l;
label define tm9834l 
	0           "Not applicable"                
	1           "Yes - ask TM9835 and TM9836"   
	2           "No"                            
;
label values tm9835   tm9835l;
label define tm9835l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9836   tm9836l;
label define tm9836l 
	0           "Not applicable"                
;
label values tm9837   tm9837l;
label define tm9837l 
	0           "Not applicable"                
	1           "Yes - ask TM9837 and TM9838"   
	2           "No"                            
;
label values tm9838   tm9838l;
label define tm9838l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9839   tm9839l;
label define tm9839l 
	0           "Not applicable"                
;
label values tm9840   tm9840l;
label define tm9840l 
	0           "Not applicable"                
	1           "Yes - ask TM9841 and TM9842"   
	2           "No"                            
;
label values tm9841   tm9841l;
label define tm9841l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9842   tm9842l;
label define tm9842l 
	0           "Not applicable"                
;
label values tm9843   tm9843l;
label define tm9843l 
	0           "Not applicable"                
	1           "Yes - Skip to TM9850"          
	2           "No"                            
;
label values tm9844   tm9844l;
label define tm9844l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9846"           
;
label values tm9845   tm9845l;
label define tm9845l 
	0           "Not applicable"                
;
label values tm9846   tm9846l;
label define tm9846l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9848"           
;
label values tm9847   tm9847l;
label define tm9847l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9848   tm9848l;
label define tm9848l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9850"           
;
label values tm9849   tm9849l;
label define tm9849l 
	0           "Not applicable"                
;
label values tm9850   tm9850l;
label define tm9850l 
	0           "Not applicable"                
	-1          "None Check Item T43"           
;
label values tm9851   tm9851l;
label define tm9851l 
	0           "Not applicable"                
	1           "Yes - go to fourth child -"    
	2           "No - go to TM7000"             
;
label values tm9852   tm9852l;
label define tm9852l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to Statement P"        
;
label values tm9853   tm9853l;
label define tm9853l 
	0           "Not applicable"                
;
label values tm9854   tm9854l;
label define tm9854l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7000"           
;
label values tm9855   tm9855l;
label define tm9855l 
	0           "Not applicable"                
	1           "Yes - ask TM9856 and TM9877"   
	2           "No"                            
;
label values tm9856   tm9856l;
label define tm9856l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9857   tm9857l;
label define tm9857l 
	0           "Not applicable"                
;
label values tm9858   tm9858l;
label define tm9858l 
	0           "Not applicable"                
	1           "Yes - ask TM9859 and TM9860"   
	2           "No"                            
;
label values tm9859   tm9859l;
label define tm9859l 
	0           "Not applicable"                
	1           "In ...'s home"                 
	2           "At work/at school"             
	3           "Someplace else"                
;
label values tm9860   tm9860l;
label define tm9860l 
	0           "Not applicable"                
;
label values tm9861   tm9861l;
label define tm9861l 
	0           "Not applicable"                
	1           "Yes - ask TM9862 and TM9863"   
	2           "No"                            
;
label values tm9862   tm9862l;
label define tm9862l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9863   tm9863l;
label define tm9863l 
	0           "Not applicable"                
;
label values tm9864   tm9864l;
label define tm9864l 
	0           "Not applicable"                
	1           "Yes - ask TM9865 and TM9866"   
	2           "No"                            
;
label values tm9865   tm9865l;
label define tm9865l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9866   tm9866l;
label define tm9866l 
	0           "Not applicable"                
;
label values tm9867   tm9867l;
label define tm9867l 
	0           "Not applicable"                
	1           "Yes - ask TM9868 and TM9869"   
	2           "No"                            
;
label values tm9868   tm9868l;
label define tm9868l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9869   tm9869l;
label define tm9869l 
	0           "Not applicable"                
;
label values tm9870   tm9870l;
label define tm9870l 
	0           "Not applicable"                
	1           "Yes - ask TM9871 and TM9872"   
	2           "No"                            
;
label values tm9871   tm9871l;
label define tm9871l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Other place"                   
;
label values tm9872   tm9872l;
label define tm9872l 
	0           "Not applicable"                
;
label values tm9873   tm9873l;
label define tm9873l 
	0           "Not applicable"                
	1           "Yes - ask TM9874"              
	2           "No"                            
;
label values tm9874   tm9874l;
label define tm9874l 
	0           "Not applicable"                
;
label values tm9875   tm9875l;
label define tm9875l 
	0           "Not applicable"                
	1           "Yes - ask TM9876 and TM9877"   
	2           "No"                            
;
label values tm9876   tm9876l;
label define tm9876l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9877   tm9877l;
label define tm9877l 
	0           "Not applicable"                
;
label values tm9878   tm9878l;
label define tm9878l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older - Skip"
;
label values tm9879   tm9879l;
label define tm9879l 
	0           "Not applicable"                
	1           "Yes - ask TM9880 and TM9881"   
	2           "No"                            
;
label values tm9880   tm9880l;
label define tm9880l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9881   tm9881l;
label define tm9881l 
	0           "Not applicable"                
;
label values tm9882   tm9882l;
label define tm9882l 
	0           "Not applicable"                
	1           "Yes - ask TM9883 and TM9884"   
	2           "No"                            
;
label values tm9883   tm9883l;
label define tm9883l 
	0           "Not applicable"                
	1           "At work (school)"              
	2           "Someplace else (includes ..."  
;
label values tm9884   tm9884l;
label define tm9884l 
	0           "Not applicable"                
;
label values tm9885   tm9885l;
label define tm9885l 
	0           "Not applicable"                
	1           "Yes - ask TM9886"              
	2           "No"                            
;
label values tm9886   tm9886l;
label define tm9886l 
	0           "Not applicable"                
;
label values tm9887   tm9887l;
label define tm9887l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip to"
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9888   tm9888l;
label define tm9888l 
	0           "Not applicable"                
	1           "Yes - ask TM9889 and TM9890"   
	2           "No"                            
;
label values tm9889   tm9889l;
label define tm9889l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9890   tm9890l;
label define tm9890l 
	0           "Not applicable"                
;
label values tm9891   tm9891l;
label define tm9891l 
	0           "Not applicable"                
	1           "Yes - ask TM9892 and TM9893"   
	2           "No"                            
;
label values tm9892   tm9892l;
label define tm9892l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9893   tm9893l;
label define tm9893l 
	0           "Not applicable"                
;
label values tm9894   tm9894l;
label define tm9894l 
	0           "Not applicable"                
	1           "Yes - ask TM9895 and TM9896"   
	2           "No"                            
;
label values tm9895   tm9895l;
label define tm9895l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9896   tm9896l;
label define tm9896l 
	0           "Not applicable"                
;
label values tm9897   tm9897l;
label define tm9897l 
	0           "Not applicable"                
	1           "Yes - ask TM9898 and TM9899"   
	2           "No"                            
;
label values tm9898   tm9898l;
label define tm9898l 
	0           "Not applicable"                
	1           "At work"                       
	2           "At school"                     
	3           "Someplace else"                
;
label values tm9899   tm9899l;
label define tm9899l 
	0           "Not applicable"                
;
label values tm9900   tm9900l;
label define tm9900l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9902"           
;
label values tm9901   tm9901l;
label define tm9901l 
	0           "Not applicable"                
;
label values tm9902   tm9902l;
label define tm9902l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9904"           
;
label values tm9903   tm9903l;
label define tm9903l 
	0           "Not applicable"                
;
label values tm9904   tm9904l;
label define tm9904l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9906"           
;
label values tm9905   tm9905l;
label define tm9905l 
	0           "Not applicable"                
;
label values tm9906   tm9906l;
label define tm9906l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9908"           
;
label values tm9907   tm9907l;
label define tm9907l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9908   tm9908l;
label define tm9908l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9462"           
;
label values tm9909   tm9909l;
label define tm9909l 
	0           "Not applicable"                
;
label values tm9910   tm9910l;
label define tm9910l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Statement O"      
;
label values tm9911   tm9911l;
label define tm9911l 
	0           "Not applicable"                
	1           "Yes, respondent lost time"     
	2           "Yes, spouse lost time"         
	3           "Both respondent an spouse lost"
	4           "No"                            
;
label values tm9912   tm9912l;
label define tm9912l 
	0           "Not applicable"                
;
label values tm9913   tm9913l;
label define tm9913l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7000"           
;
label values tm9914   tm9914l;
label define tm9914l 
	0           "Not applicable"                
	1           "Yes - ask TM9915 and TM9916"   
	2           "No"                            
;
label values tm9915   tm9915l;
label define tm9915l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Grandparent's home"            
	3           "Other place"                   
;
label values tm9916   tm9916l;
label define tm9916l 
	0           "Not applicable"                
;
label values tm9917   tm9917l;
label define tm9917l 
	0           "Not applicable"                
	1           "Yes - ask TM9918 and TM9919"   
	2           "No"                            
;
label values tm9918   tm9918l;
label define tm9918l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other relative's home"         
	3           "Someplace else"                
;
label values tm9919   tm9919l;
label define tm9919l 
	0           "Not applicable"                
;
label values tm9920   tm9920l;
label define tm9920l 
	0           "Not applicable"                
	1           "Yes - ask TM9921"              
	2           "No"                            
;
label values tm9921   tm9921l;
label define tm9921l 
	0           "Not applicable"                
;
label values tm9922   tm9922l;
label define tm9922l 
	0           "Not applicable"                
	1           "Yes - ask TM9923 and TM9924"   
	2           "No"                            
;
label values tm9923   tm9923l;
label define tm9923l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm9924   tm9924l;
label define tm9924l 
	0           "Not applicable"                
;
label values tm9925   tm9925l;
label define tm9925l 
	0           "Not applicable"                
	1           "Yes, less than 6 years old"    
	2           "No, 6 years old or older"      
;
label values tm9926   tm9926l;
label define tm9926l 
	0           "Not applicable"                
	1           "Yes - ask TM9927"              
	2           "No"                            
;
label values tm9927   tm9927l;
label define tm9927l 
	0           "Not applicable"                
;
label values tm9928   tm9928l;
label define tm9928l 
	0           "Not applicable"                
	1           "Yes - ask TM9929"              
	2           "No"                            
;
label values tm9929   tm9929l;
label define tm9929l 
	0           "Not applicable"                
;
label values tm9930   tm9930l;
label define tm9930l 
	0           "Not applicable"                
	1           "Yes - ask TM9931"              
	2           "No"                            
;
label values tm9931   tm9931l;
label define tm9931l 
	0           "Not applicable"                
;
label values tm9932   tm9932l;
label define tm9932l 
	0           "Not applicable"                
	1           "Less than 4 years old - Skip"  
	2           "4 to 5 years old - Skip to"    
	3           "6 or more years old"           
;
label values tm9933   tm9933l;
label define tm9933l 
	0           "Not applicable"                
	1           "Yes - ask TM9934 and TM9944"   
	2           "No"                            
;
label values tm9934   tm9934l;
label define tm9934l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9935   tm9935l;
label define tm9935l 
	0           "Not applicable"                
;
label values tm9936   tm9936l;
label define tm9936l 
	0           "Not applicable"                
	1           "Yes - ask TM9937 and TM9938"   
	2           "No"                            
;
label values tm9937   tm9937l;
label define tm9937l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9938   tm9938l;
label define tm9938l 
	0           "Not applicable"                
;
label values tm9939   tm9939l;
label define tm9939l 
	0           "Not applicable"                
	1           "Yes - ask TM9940 and TM9941"   
	2           "No"                            
;
label values tm9940   tm9940l;
label define tm9940l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9941   tm9941l;
label define tm9941l 
	0           "Not applicable"                
;
label values tm9942   tm9942l;
label define tm9942l 
	0           "Not applicable"                
	1           "Yes - ask TM9943 and TM9944"   
	2           "No"                            
;
label values tm9943   tm9943l;
label define tm9943l 
	0           "Not applicable"                
	1           "At school"                     
	2           "Someplace else"                
;
label values tm9944   tm9944l;
label define tm9944l 
	0           "Not applicable"                
;
label values tm9945   tm9945l;
label define tm9945l 
	0           "Not applicable"                
	1           "Yes - Skip to TM9952"          
	2           "No"                            
;
label values tm9946   tm9946l;
label define tm9946l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9948"           
;
label values tm9947   tm9947l;
label define tm9947l 
	0           "Not applicable"                
;
label values tm9948   tm9948l;
label define tm9948l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9950"           
;
label values tm9949   tm9949l;
label define tm9949l 
	0           "Not applicable"                
	-4          "Less than 1 hour"              
;
label values tm9950   tm9950l;
label define tm9950l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9952"           
;
label values tm9951   tm9951l;
label define tm9951l 
	0           "Not applicable"                
;
label values tm9952   tm9952l;
label define tm9952l 
	0           "Not applicable"                
	-1          "none"                          
;
label values im9340   im9340l;
label define im9340l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9346   im9346l;
label define im9346l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9352   im9352l;
label define im9352l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9354   im9354l;
label define im9354l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9356   im9356l;
label define im9356l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9360   im9360l;
label define im9360l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9362   im9362l;
label define im9362l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9366   im9366l;
label define im9366l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9368   im9368l;
label define im9368l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9372   im9372l;
label define im9372l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9374   im9374l;
label define im9374l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9378   im9378l;
label define im9378l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9380   im9380l;
label define im9380l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9384   im9384l;
label define im9384l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9386   im9386l;
label define im9386l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9390   im9390l;
label define im9390l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9394   im9394l;
label define im9394l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9396   im9396l;
label define im9396l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9400   im9400l;
label define im9400l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9402   im9402l;
label define im9402l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9404   im9404l;
label define im9404l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9408   im9408l;
label define im9408l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9410   im9410l;
label define im9410l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9414   im9414l;
label define im9414l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9418   im9418l;
label define im9418l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9420   im9420l;
label define im9420l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9422   im9422l;
label define im9422l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9426   im9426l;
label define im9426l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9428   im9428l;
label define im9428l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9432   im9432l;
label define im9432l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9434   im9434l;
label define im9434l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9438   im9438l;
label define im9438l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9440   im9440l;
label define im9440l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9442   im9442l;
label define im9442l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9444   im9444l;
label define im9444l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9446   im9446l;
label define im9446l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9448   im9448l;
label define im9448l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9450   im9450l;
label define im9450l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9452   im9452l;
label define im9452l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9454   im9454l;
label define im9454l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9456   im9456l;
label define im9456l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9458   im9458l;
label define im9458l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9460   im9460l;
label define im9460l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9462   im9462l;
label define im9462l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9464   im9464l;
label define im9464l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9468   im9468l;
label define im9468l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9470   im9470l;
label define im9470l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9472   im9472l;
label define im9472l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9476   im9476l;
label define im9476l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9478   im9478l;
label define im9478l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9482   im9482l;
label define im9482l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9486   im9486l;
label define im9486l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9488   im9488l;
label define im9488l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9492   im9492l;
label define im9492l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9494   im9494l;
label define im9494l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9498   im9498l;
label define im9498l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9502   im9502l;
label define im9502l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9506   im9506l;
label define im9506l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9508   im9508l;
label define im9508l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9510   im9510l;
label define im9510l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9514   im9514l;
label define im9514l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9516   im9516l;
label define im9516l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9520   im9520l;
label define im9520l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9522   im9522l;
label define im9522l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9526   im9526l;
label define im9526l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9528   im9528l;
label define im9528l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9532   im9532l;
label define im9532l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9534   im9534l;
label define im9534l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9536   im9536l;
label define im9536l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9538   im9538l;
label define im9538l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9540   im9540l;
label define im9540l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9542   im9542l;
label define im9542l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9544   im9544l;
label define im9544l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9554   im9554l;
label define im9554l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9556   im9556l;
label define im9556l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9558   im9558l;
label define im9558l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9562   im9562l;
label define im9562l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9564   im9564l;
label define im9564l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9568   im9568l;
label define im9568l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9570   im9570l;
label define im9570l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9574   im9574l;
label define im9574l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9576   im9576l;
label define im9576l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9580   im9580l;
label define im9580l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9582   im9582l;
label define im9582l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9586   im9586l;
label define im9586l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9588   im9588l;
label define im9588l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9592   im9592l;
label define im9592l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9596   im9596l;
label define im9596l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9598   im9598l;
label define im9598l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9602   im9602l;
label define im9602l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9604   im9604l;
label define im9604l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9606   im9606l;
label define im9606l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9610   im9610l;
label define im9610l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9612   im9612l;
label define im9612l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9616   im9616l;
label define im9616l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9620   im9620l;
label define im9620l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9622   im9622l;
label define im9622l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9624   im9624l;
label define im9624l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9628   im9628l;
label define im9628l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9630   im9630l;
label define im9630l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9634   im9634l;
label define im9634l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9636   im9636l;
label define im9636l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9640   im9640l;
label define im9640l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9642   im9642l;
label define im9642l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9644   im9644l;
label define im9644l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9646   im9646l;
label define im9646l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9648   im9648l;
label define im9648l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9650   im9650l;
label define im9650l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9652   im9652l;
label define im9652l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9654   im9654l;
label define im9654l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9656   im9656l;
label define im9656l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9658   im9658l;
label define im9658l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9660   im9660l;
label define im9660l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9662   im9662l;
label define im9662l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9664   im9664l;
label define im9664l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9666   im9666l;
label define im9666l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9670   im9670l;
label define im9670l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9672   im9672l;
label define im9672l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9674   im9674l;
label define im9674l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9678   im9678l;
label define im9678l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9680   im9680l;
label define im9680l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9684   im9684l;
label define im9684l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9688   im9688l;
label define im9688l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9690   im9690l;
label define im9690l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9694   im9694l;
label define im9694l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9696   im9696l;
label define im9696l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9700   im9700l;
label define im9700l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9704   im9704l;
label define im9704l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9708   im9708l;
label define im9708l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9710   im9710l;
label define im9710l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9712   im9712l;
label define im9712l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9716   im9716l;
label define im9716l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9718   im9718l;
label define im9718l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9722   im9722l;
label define im9722l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9724   im9724l;
label define im9724l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9728   im9728l;
label define im9728l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9730   im9730l;
label define im9730l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9734   im9734l;
label define im9734l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9736   im9736l;
label define im9736l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9738   im9738l;
label define im9738l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9740   im9740l;
label define im9740l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9742   im9742l;
label define im9742l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9744   im9744l;
label define im9744l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9746   im9746l;
label define im9746l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9753   im9753l;
label define im9753l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9754   im9754l;
label define im9754l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9755   im9755l;
label define im9755l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9757   im9757l;
label define im9757l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9758   im9758l;
label define im9758l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9760   im9760l;
label define im9760l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9761   im9761l;
label define im9761l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9763   im9763l;
label define im9763l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9764   im9764l;
label define im9764l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9766   im9766l;
label define im9766l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9767   im9767l;
label define im9767l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9769   im9769l;
label define im9769l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9770   im9770l;
label define im9770l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9772   im9772l;
label define im9772l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9774   im9774l;
label define im9774l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9775   im9775l;
label define im9775l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9777   im9777l;
label define im9777l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9778   im9778l;
label define im9778l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9779   im9779l;
label define im9779l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9781   im9781l;
label define im9781l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9782   im9782l;
label define im9782l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9784   im9784l;
label define im9784l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9786   im9786l;
label define im9786l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9787   im9787l;
label define im9787l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9788   im9788l;
label define im9788l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9790   im9790l;
label define im9790l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9791   im9791l;
label define im9791l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9793   im9793l;
label define im9793l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9794   im9794l;
label define im9794l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9796   im9796l;
label define im9796l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9797   im9797l;
label define im9797l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9798   im9798l;
label define im9798l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9799   im9799l;
label define im9799l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9800   im9800l;
label define im9800l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9801   im9801l;
label define im9801l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9802   im9802l;
label define im9802l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9803   im9803l;
label define im9803l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9804   im9804l;
label define im9804l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9805   im9805l;
label define im9805l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9806   im9806l;
label define im9806l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9807   im9807l;
label define im9807l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9808   im9808l;
label define im9808l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9809   im9809l;
label define im9809l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9812   im9812l;
label define im9812l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9813   im9813l;
label define im9813l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9814   im9814l;
label define im9814l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9816   im9816l;
label define im9816l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9817   im9817l;
label define im9817l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9819   im9819l;
label define im9819l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9821   im9821l;
label define im9821l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9822   im9822l;
label define im9822l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9824   im9824l;
label define im9824l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9825   im9825l;
label define im9825l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9827   im9827l;
label define im9827l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9829   im9829l;
label define im9829l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9831   im9831l;
label define im9831l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9832   im9832l;
label define im9832l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9833   im9833l;
label define im9833l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9835   im9835l;
label define im9835l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9836   im9836l;
label define im9836l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9838   im9838l;
label define im9838l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9839   im9839l;
label define im9839l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9841   im9841l;
label define im9841l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9842   im9842l;
label define im9842l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9844   im9844l;
label define im9844l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9845   im9845l;
label define im9845l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9846   im9846l;
label define im9846l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9847   im9847l;
label define im9847l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9848   im9848l;
label define im9848l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9849   im9849l;
label define im9849l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9850   im9850l;
label define im9850l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9855   im9855l;
label define im9855l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9856   im9856l;
label define im9856l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9857   im9857l;
label define im9857l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9859   im9859l;
label define im9859l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9860   im9860l;
label define im9860l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9862   im9862l;
label define im9862l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9863   im9863l;
label define im9863l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9865   im9865l;
label define im9865l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9866   im9866l;
label define im9866l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9868   im9868l;
label define im9868l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9869   im9869l;
label define im9869l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9871   im9871l;
label define im9871l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9872   im9872l;
label define im9872l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9874   im9874l;
label define im9874l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9876   im9876l;
label define im9876l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9877   im9877l;
label define im9877l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9879   im9879l;
label define im9879l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9880   im9880l;
label define im9880l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9881   im9881l;
label define im9881l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9883   im9883l;
label define im9883l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9884   im9884l;
label define im9884l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9886   im9886l;
label define im9886l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9888   im9888l;
label define im9888l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9889   im9889l;
label define im9889l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9890   im9890l;
label define im9890l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9892   im9892l;
label define im9892l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9893   im9893l;
label define im9893l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9895   im9895l;
label define im9895l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9896   im9896l;
label define im9896l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9898   im9898l;
label define im9898l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9899   im9899l;
label define im9899l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9900   im9900l;
label define im9900l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9901   im9901l;
label define im9901l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9902   im9902l;
label define im9902l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9903   im9903l;
label define im9903l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9904   im9904l;
label define im9904l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9905   im9905l;
label define im9905l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9906   im9906l;
label define im9906l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9907   im9907l;
label define im9907l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9908   im9908l;
label define im9908l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9909   im9909l;
label define im9909l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9910   im9910l;
label define im9910l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9911   im9911l;
label define im9911l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9914   im9914l;
label define im9914l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9915   im9915l;
label define im9915l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9916   im9916l;
label define im9916l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9918   im9918l;
label define im9918l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9919   im9919l;
label define im9919l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9921   im9921l;
label define im9921l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9923   im9923l;
label define im9923l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9924   im9924l;
label define im9924l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9926   im9926l;
label define im9926l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9927   im9927l;
label define im9927l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9929   im9929l;
label define im9929l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9931   im9931l;
label define im9931l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9933   im9933l;
label define im9933l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9934   im9934l;
label define im9934l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9935   im9935l;
label define im9935l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9937   im9937l;
label define im9937l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9938   im9938l;
label define im9938l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9940   im9940l;
label define im9940l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9941   im9941l;
label define im9941l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9943   im9943l;
label define im9943l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9944   im9944l;
label define im9944l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9946   im9946l;
label define im9946l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9947   im9947l;
label define im9947l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9948   im9948l;
label define im9948l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9949   im9949l;
label define im9949l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9950   im9950l;
label define im9950l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9951   im9951l;
label define im9951l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im9952   im9952l;
label define im9952l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm7000   tm7000l;
label define tm7000l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8400"           
;
label values tm7001   tm7001l;
label define tm7001l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7310"           
;
label values tm7002   tm7002l;
label define tm7002l 
	0           "Not applicable"                
;
label values tm7003   tm7003l;
label define tm7003l 
	0           "Not applicable"                
;
label values tm7004   tm7004l;
label define tm7004l 
	0           "Not applicable"                
;
label values tm7005   tm7005l;
label define tm7005l 
	0           "Not applicable"                
;
label values tm7006   tm7006l;
label define tm7006l 
	0           "Not applicable"                
;
label values tm7007   tm7007l;
label define tm7007l 
	0           "Not applicable"                
;
label values tm7008   tm7008l;
label define tm7008l 
	0           "Not applicable"                
;
label values tm7009   tm7009l;
label define tm7009l 
	0           "Not applicable"                
;
label values tm7010   tm7010l;
label define tm7010l 
	0           "Not applicable"                
;
label values tm7011   tm7011l;
label define tm7011l 
	0           "Not applicable"                
;
label values tm7012   tm7012l;
label define tm7012l 
	0           "Not applicable"                
;
label values tm7013   tm7013l;
label define tm7013l 
	0           "Not applicable"                
;
label values tm7014   tm7014l;
label define tm7014l 
	0           "Not applicable"                
;
label values tm7015   tm7015l;
label define tm7015l 
	0           "Not applicable"                
;
label values tm7016   tm7016l;
label define tm7016l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "Dk/no response"                
;
label values tm7017   tm7017l;
label define tm7017l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "Dk/no response"                
;
label values tm7018   tm7018l;
label define tm7018l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "Dk"                            
;
label values tm7019   tm7019l;
label define tm7019l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "Dk"                            
;
label values tm7020   tm7020l;
label define tm7020l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "Dk"                            
;
label values tm7021   tm7021l;
label define tm7021l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "Dk/no response"                
;
label values tm7022   tm7022l;
label define tm7022l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "Dk/no response"                
;
label values tm7261   tm7261l;
label define tm7261l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7017 or TM7310 if"
;
label values tm7262   tm7262l;
label define tm7262l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7018 or TM7310 if"
;
label values tm7263   tm7263l;
label define tm7263l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7019 or TM7310 if"
;
label values tm7264   tm7264l;
label define tm7264l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7020"           
;
label values tm7265   tm7265l;
label define tm7265l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7021"           
;
label values tm7266   tm7266l;
label define tm7266l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7022"           
;
label values tm7267   tm7267l;
label define tm7267l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7310"           
;
label values tm7268   tm7268l;
label define tm7268l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7269   tm7269l;
label define tm7269l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7270   tm7270l;
label define tm7270l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7271   tm7271l;
label define tm7271l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7272   tm7272l;
label define tm7272l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7273   tm7273l;
label define tm7273l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7274   tm7274l;
label define tm7274l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7275   tm7275l;
label define tm7275l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7276   tm7276l;
label define tm7276l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7277   tm7277l;
label define tm7277l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7278   tm7278l;
label define tm7278l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7279   tm7279l;
label define tm7279l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7280   tm7280l;
label define tm7280l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7281   tm7281l;
label define tm7281l 
	0           "Not applicable"                
	-3          "None"                          
	-9          "Dk/no response"                
;
label values tm7282   tm7282l;
label define tm7282l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7017 or TM7310 if"
;
label values tm7283   tm7283l;
label define tm7283l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7018 or TM7310 if"
;
label values tm7284   tm7284l;
label define tm7284l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7019 or TM7310 if"
;
label values tm7285   tm7285l;
label define tm7285l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7020 or TM7310 if"
;
label values tm7286   tm7286l;
label define tm7286l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7021 or TM7310 if"
;
label values tm7287   tm7287l;
label define tm7287l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm7288   tm7288l;
label define tm7288l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Check Item TM7310"
;
label values tm7289   tm7289l;
label define tm7289l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7290   tm7290l;
label define tm7290l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7291   tm7291l;
label define tm7291l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7292   tm7292l;
label define tm7292l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7293   tm7293l;
label define tm7293l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7294   tm7294l;
label define tm7294l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7295   tm7295l;
label define tm7295l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7296   tm7296l;
label define tm7296l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7297   tm7297l;
label define tm7297l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7298   tm7298l;
label define tm7298l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7299   tm7299l;
label define tm7299l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7300   tm7300l;
label define tm7300l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7301   tm7301l;
label define tm7301l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7302   tm7302l;
label define tm7302l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7303   tm7303l;
label define tm7303l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7304   tm7304l;
label define tm7304l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7305   tm7305l;
label define tm7305l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7306   tm7306l;
label define tm7306l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7307   tm7307l;
label define tm7307l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7308   tm7308l;
label define tm7308l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7309   tm7309l;
label define tm7309l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-9          "Dk/no response"                
;
label values tm7310   tm7310l;
label define tm7310l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Check"            
;
label values tm7311   tm7311l;
label define tm7311l 
	0           "Not applicable"                
;
label values tm7312   tm7312l;
label define tm7312l 
	0           "Not applicable"                
;
label values tm7313   tm7313l;
label define tm7313l 
	0           "Not applicable"                
;
label values tm7314   tm7314l;
label define tm7314l 
	0           "Not applicable"                
;
label values tm7315   tm7315l;
label define tm7315l 
	0           "Not applicable"                
;
label values tm7316   tm7316l;
label define tm7316l 
	0           "Not applicable"                
;
label values tm7317   tm7317l;
label define tm7317l 
	0           "Not applicable"                
;
label values tm7318   tm7318l;
label define tm7318l 
	0           "Not applicable"                
;
label values tm7319   tm7319l;
label define tm7319l 
	0           "Not applicable"                
;
label values tm7320   tm7320l;
label define tm7320l 
	0           "Not applicable"                
;
label values tm7321   tm7321l;
label define tm7321l 
	0           "Not applicable"                
;
label values tm7322   tm7322l;
label define tm7322l 
	0           "Not applicable"                
;
label values tm7323   tm7323l;
label define tm7323l 
	0           "Not applicable"                
;
label values tm7324   tm7324l;
label define tm7324l 
	0           "Not applicable"                
;
label values tm7577   tm7577l;
label define tm7577l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7578   tm7578l;
label define tm7578l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7579   tm7579l;
label define tm7579l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7580   tm7580l;
label define tm7580l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7581   tm7581l;
label define tm7581l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7582   tm7582l;
label define tm7582l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7583   tm7583l;
label define tm7583l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7584   tm7584l;
label define tm7584l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7585   tm7585l;
label define tm7585l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7586   tm7586l;
label define tm7586l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7587   tm7587l;
label define tm7587l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7588   tm7588l;
label define tm7588l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7589   tm7589l;
label define tm7589l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7590   tm7590l;
label define tm7590l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7591   tm7591l;
label define tm7591l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7592   tm7592l;
label define tm7592l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7593   tm7593l;
label define tm7593l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7594   tm7594l;
label define tm7594l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7595   tm7595l;
label define tm7595l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7596   tm7596l;
label define tm7596l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7597   tm7597l;
label define tm7597l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7598   tm7598l;
label define tm7598l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM7851"           
;
label values tm7599   tm7599l;
label define tm7599l 
	0           "Not applicable"                
;
label values tm7600   tm7600l;
label define tm7600l 
	0           "Not applicable"                
;
label values tm7601   tm7601l;
label define tm7601l 
	0           "Not applicable"                
;
label values tm7602   tm7602l;
label define tm7602l 
	0           "Not applicable"                
;
label values tm7603   tm7603l;
label define tm7603l 
	0           "Not applicable"                
;
label values tm7604   tm7604l;
label define tm7604l 
	0           "Not applicable"                
;
label values tm7605   tm7605l;
label define tm7605l 
	0           "Not applicable"                
;
label values tm7606   tm7606l;
label define tm7606l 
	0           "Not applicable"                
;
label values tm7607   tm7607l;
label define tm7607l 
	0           "Not applicable"                
;
label values tm7608   tm7608l;
label define tm7608l 
	0           "Not applicable"                
;
label values tm7609   tm7609l;
label define tm7609l 
	0           "Not applicable"                
;
label values tm7610   tm7610l;
label define tm7610l 
	0           "Not applicable"                
;
label values tm7611   tm7611l;
label define tm7611l 
	0           "Not applicable"                
;
label values tm7612   tm7612l;
label define tm7612l 
	0           "Not applicable"                
;
label values tm7613   tm7613l;
label define tm7613l 
	0           "Not applicable"                
	1           "Yes - Skip to TM7641"          
	2           "No"                            
;
label values tm7614   tm7614l;
label define tm7614l 
	0           "Not applicable"                
	1           "Yes - Skip to TM7642"          
	2           "No"                            
;
label values tm7615   tm7615l;
label define tm7615l 
	0           "Not applicable"                
	1           "Yes - Skip to TM7643"          
	2           "No"                            
;
label values tm7616   tm7616l;
label define tm7616l 
	0           "Not applicable"                
	1           "Yes - Skip to TM7644"          
	2           "No"                            
;
label values tm7617   tm7617l;
label define tm7617l 
	0           "Not applicable"                
	1           "Yes - Skip to TM7645"          
	2           "No"                            
;
label values tm7618   tm7618l;
label define tm7618l 
	0           "Not applicable"                
	1           "Yes - Skip to TM7646"          
	2           "No"                            
;
label values tm7619   tm7619l;
label define tm7619l 
	0           "Not applicable"                
	1           "Yes - Skip to TM7647"          
	2           "No"                            
;
label values tm7620   tm7620l;
label define tm7620l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7621   tm7621l;
label define tm7621l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7622   tm7622l;
label define tm7622l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7623   tm7623l;
label define tm7623l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7624   tm7624l;
label define tm7624l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7625   tm7625l;
label define tm7625l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7626   tm7626l;
label define tm7626l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7627   tm7627l;
label define tm7627l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7628   tm7628l;
label define tm7628l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7629   tm7629l;
label define tm7629l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7630   tm7630l;
label define tm7630l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7631   tm7631l;
label define tm7631l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7632   tm7632l;
label define tm7632l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7633   tm7633l;
label define tm7633l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7634   tm7634l;
label define tm7634l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7635   tm7635l;
label define tm7635l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7636   tm7636l;
label define tm7636l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7637   tm7637l;
label define tm7637l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7638   tm7638l;
label define tm7638l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7639   tm7639l;
label define tm7639l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7640   tm7640l;
label define tm7640l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7641   tm7641l;
label define tm7641l 
	0           "Not applicable"                
	1           "Yes"                           
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7642   tm7642l;
label define tm7642l 
	0           "Not applicable"                
	1           "Yes"                           
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7643   tm7643l;
label define tm7643l 
	0           "Not applicable"                
	1           "Yes"                           
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7644   tm7644l;
label define tm7644l 
	0           "Not applicable"                
	1           "Yes"                           
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7645   tm7645l;
label define tm7645l 
	0           "Not applicable"                
	1           "Yes"                           
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7646   tm7646l;
label define tm7646l 
	0           "Not applicable"                
	1           "Yes"                           
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7647   tm7647l;
label define tm7647l 
	0           "Not applicable"                
	1           "Yes"                           
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7648   tm7648l;
label define tm7648l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7649   tm7649l;
label define tm7649l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7650   tm7650l;
label define tm7650l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7651   tm7651l;
label define tm7651l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7652   tm7652l;
label define tm7652l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7653   tm7653l;
label define tm7653l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7654   tm7654l;
label define tm7654l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7655   tm7655l;
label define tm7655l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7656   tm7656l;
label define tm7656l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7657   tm7657l;
label define tm7657l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7658   tm7658l;
label define tm7658l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7659   tm7659l;
label define tm7659l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7660   tm7660l;
label define tm7660l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7661   tm7661l;
label define tm7661l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
	-2          "Ref."                          
	-9          "Dk/no response"                
;
label values tm7662   tm7662l;
label define tm7662l 
	0           "Not applicable"                
;
label values tm7663   tm7663l;
label define tm7663l 
	0           "Not applicable"                
;
label values tm7664   tm7664l;
label define tm7664l 
	0           "Not applicable"                
;
label values tm7665   tm7665l;
label define tm7665l 
	0           "Not applicable"                
;
label values tm7666   tm7666l;
label define tm7666l 
	0           "Not applicable"                
;
label values tm7667   tm7667l;
label define tm7667l 
	0           "Not applicable"                
;
label values tm7668   tm7668l;
label define tm7668l 
	0           "Not applicable"                
;
label values tm7669   tm7669l;
label define tm7669l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM7620 for next child"
	-9          "Dk/no response"                
;
label values tm7670   tm7670l;
label define tm7670l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM7621 for next child"
	-9          "Dk/no response"                
;
label values tm7671   tm7671l;
label define tm7671l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM7622 for next child"
	-9          "Dk/no response if this is last"
;
label values tm7672   tm7672l;
label define tm7672l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM7623 for next child"
	-9          "Dk/no response"                
;
label values tm7673   tm7673l;
label define tm7673l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM7624 for next child"
	-9          "Dk/no response"                
;
label values tm7674   tm7674l;
label define tm7674l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM7625 for next child"
	-9          "Dk/no response"                
;
label values tm7675   tm7675l;
label define tm7675l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM7626 for next child"
	-9          "Dk/no response"                
;
label values tm7676   tm7676l;
label define tm7676l 
	0           "Not applicable"                
	1           "One injury - Skip to TM7683"   
	2           "More than one injury - Skip to"
	-9          "Dk/no response"                
;
label values tm7677   tm7677l;
label define tm7677l 
	0           "Not applicable"                
	1           "One injury - Skip to TM7684"   
	2           "More than one injury - Skip to"
	-9          "Dk/no response"                
;
label values tm7678   tm7678l;
label define tm7678l 
	0           "Not applicable"                
	1           "One injury - Skip to TM7684"   
	2           "More than one injury - Skip to"
	-9          "Dk/no response"                
;
label values tm7679   tm7679l;
label define tm7679l 
	0           "Not applicable"                
	1           "One injury - Skip to TM7685"   
	2           "More than one injury - Skip to"
	-9          "Dk/no response"                
;
label values tm7680   tm7680l;
label define tm7680l 
	0           "Not applicable"                
	1           "One injury - Skip to TM7686"   
	2           "More than one injury - Skip to"
	-9          "Dk/no response"                
;
label values tm7681   tm7681l;
label define tm7681l 
	0           "Not applicable"                
	1           "One injury - Skip to TM7687"   
	2           "More than one injury - Skip to"
	-9          "Dk/no response"                
;
label values tm7682   tm7682l;
label define tm7682l 
	0           "Not applicable"                
	1           "One injury - Skip to TM7688"   
	2           "More than one injury - Skip to"
	-9          "Dk/no response"                
;
label values tm7683   tm7683l;
label define tm7683l 
	0           "Not applicable"                
	1           "Organized"                     
	2           "Informal/organized"            
	-9          "Dk/no response"                
;
label values tm7684   tm7684l;
label define tm7684l 
	0           "Not applicable"                
	1           "Organized"                     
	2           "Informal/organized"            
	-9          "Dk/no response"                
;
label values tm7685   tm7685l;
label define tm7685l 
	0           "Not applicable"                
	1           "Organized"                     
	2           "Informal/organized"            
	-9          "Dk/no response"                
;
label values tm7686   tm7686l;
label define tm7686l 
	0           "Not applicable"                
	1           "Organized"                     
	2           "Informal/organized"            
	-9          "Dk/no response"                
;
label values tm7687   tm7687l;
label define tm7687l 
	0           "Not applicable"                
	1           "Organized"                     
	2           "Informal/organized"            
	-9          "Dk/no response"                
;
label values tm7688   tm7688l;
label define tm7688l 
	0           "Not applicable"                
	1           "Organized"                     
	2           "Informal/organized"            
	-9          "Dk/no response"                
;
label values tm7689   tm7689l;
label define tm7689l 
	0           "Not applicable"                
	1           "Organized"                     
	2           "Informal/organized"            
	-9          "Dk/no response"                
;
label values tm7690   tm7690l;
label define tm7690l 
	0           "Not applicable"                
	1           "School activity besides"       
	-9          "Dk/no response"                
;
label values tm7691   tm7691l;
label define tm7691l 
	0           "Not applicable"                
	1           "School activity besides"       
	-9          "Dk/no response"                
;
label values tm7692   tm7692l;
label define tm7692l 
	0           "Not applicable"                
	1           "School activity besides"       
	-9          "Dk/no response"                
;
label values tm7693   tm7693l;
label define tm7693l 
	0           "Not applicable"                
	1           "School activity besides"       
	-9          "Dk/no response"                
;
label values tm7694   tm7694l;
label define tm7694l 
	0           "Not applicable"                
	1           "School activity besides"       
	-9          "Dk/no response"                
;
label values tm7695   tm7695l;
label define tm7695l 
	0           "Not applicable"                
	1           "School activity besides"       
	-9          "Dk/no response"                
;
label values tm7696   tm7696l;
label define tm7696l 
	0           "Not applicable"                
	1           "School activity besides"       
	-9          "Dk/no response"                
;
label values tm7697   tm7697l;
label define tm7697l 
	0           "Not applicable"                
	1           "Community, club and church"    
	-9          "Dk/no response"                
;
label values tm7698   tm7698l;
label define tm7698l 
	0           "Not applicable"                
	1           "Community, club and church"    
	-9          "Dk/no response"                
;
label values tm7699   tm7699l;
label define tm7699l 
	0           "Not applicable"                
	1           "Community, club and church"    
	-9          "Dk/no response"                
;
label values tm7700   tm7700l;
label define tm7700l 
	0           "Not applicable"                
	1           "Community, club and church"    
	-9          "Dk/no response"                
;
label values tm7701   tm7701l;
label define tm7701l 
	0           "Not applicable"                
	1           "Community, club and church"    
	-9          "Dk/no response"                
;
label values tm7702   tm7702l;
label define tm7702l 
	0           "Not applicable"                
	1           "Community, club and church"    
	-9          "Dk/no response"                
;
label values tm7703   tm7703l;
label define tm7703l 
	0           "Not applicable"                
	1           "Community, club and church"    
	-9          "Dk/no response"                
;
label values tm7704   tm7704l;
label define tm7704l 
	0           "Not applicable"                
	1           "Other recreational activity"   
	-9          "Dk/no response"                
;
label values tm7705   tm7705l;
label define tm7705l 
	0           "Not applicable"                
	1           "Other recreational activity"   
	-9          "Dk/no response"                
;
label values tm7706   tm7706l;
label define tm7706l 
	0           "Not applicable"                
	1           "Other recreational activity"   
	-9          "Dk/no response"                
;
label values tm7707   tm7707l;
label define tm7707l 
	0           "Not applicable"                
	1           "Other recreational activity"   
	-9          "Dk/no response"                
;
label values tm7708   tm7708l;
label define tm7708l 
	0           "Not applicable"                
	1           "Other recreational activity"   
	-9          "Dk/no response"                
;
label values tm7709   tm7709l;
label define tm7709l 
	0           "Not applicable"                
	1           "Other recreational activity"   
	-9          "Dk/no response"                
;
label values tm7710   tm7710l;
label define tm7710l 
	0           "Not applicable"                
	1           "Other recreational activity"   
	-9          "Dk/no response"                
;
label values tm7711   tm7711l;
label define tm7711l 
	0           "Not applicable"                
	1           "Hanging out, fooling around,"  
	-9          "Dk/no response"                
;
label values tm7712   tm7712l;
label define tm7712l 
	0           "Not applicable"                
	1           "Hanging out, fooling around,"  
	-9          "Dk/no response"                
;
label values tm7713   tm7713l;
label define tm7713l 
	0           "Not applicable"                
	1           "Hanging out, fooling around,"  
	-9          "Dk/no response"                
;
label values tm7714   tm7714l;
label define tm7714l 
	0           "Not applicable"                
	1           "Hanging out, fooling around,"  
	-9          "Dk/no response"                
;
label values tm7715   tm7715l;
label define tm7715l 
	0           "Not applicable"                
	1           "Hanging out, fooling around,"  
	-9          "Dk/no response"                
;
label values tm7716   tm7716l;
label define tm7716l 
	0           "Not applicable"                
	1           "Hanging out, fooling around,"  
	-9          "Dk/no response"                
;
label values tm7717   tm7717l;
label define tm7717l 
	0           "Not applicable"                
	1           "Hanging out, fooling around,"  
	-9          "Dk/no response"                
;
label values tm7718   tm7718l;
label define tm7718l 
	0           "Not applicable"                
	1           "For income"                    
	2           "Not for income"                
	7           "For income"                    
	8           "Not for income"                
	-9          "Dk/no response"                
;
label values tm7719   tm7719l;
label define tm7719l 
	0           "Not applicable"                
	1           "For income"                    
	2           "Not for income"                
	7           "For income"                    
	8           "Not for income"                
	-9          "Dk/no response"                
;
label values tm7720   tm7720l;
label define tm7720l 
	0           "Not applicable"                
	1           "For income"                    
	2           "Not for income"                
	7           "For income"                    
	8           "Not for income"                
	-9          "Dk/no response"                
;
label values tm7721   tm7721l;
label define tm7721l 
	0           "Not applicable"                
	1           "For income"                    
	2           "Not for income"                
	7           "For income"                    
	8           "Not for income"                
	-9          "Dk/no response"                
;
label values tm7722   tm7722l;
label define tm7722l 
	0           "Not applicable"                
	1           "For income"                    
	2           "Not for income"                
	7           "For income"                    
	8           "Not for income"                
	-9          "Dk/no response"                
;
label values tm7723   tm7723l;
label define tm7723l 
	0           "Not applicable"                
	1           "For income"                    
	2           "Not for income"                
	7           "For income"                    
	8           "Not for income"                
	-9          "Dk/no response"                
;
label values tm7724   tm7724l;
label define tm7724l 
	0           "Not applicable"                
	1           "For income"                    
	2           "Not for income"                
	7           "For income"                    
	8           "Not for income"                
	-9          "Dk/no response"                
;
label values tm7725   tm7725l;
label define tm7725l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7726   tm7726l;
label define tm7726l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7727   tm7727l;
label define tm7727l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7728   tm7728l;
label define tm7728l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7729   tm7729l;
label define tm7729l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7730   tm7730l;
label define tm7730l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7731   tm7731l;
label define tm7731l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7732   tm7732l;
label define tm7732l 
	0           "Not applicable"                
	9           "Passenger vehicle (includes"   
	10          "Truck or other commercial"     
	11          "Motorcycle, ATV"               
	12          "Work equipment"                
	13          "Other"                         
	-9          "Dk/no response"                
;
label values tm7733   tm7733l;
label define tm7733l 
	0           "Not applicable"                
	14          "Passenger vehicle (includes"   
	15          "Truck or other commercial"     
	16          "Motorcycle, ATV"               
	17          "Work equipment"                
	18          "Other"                         
	-9          "Dk/no response"                
;
label values tm7734   tm7734l;
label define tm7734l 
	0           "Not applicable"                
	14          "Passenger vehicle (includes"   
	15          "Truck or other commercial"     
	16          "Motorcycle, ATV"               
	17          "Work equipment"                
	18          "Other"                         
	-9          "Dk/no response"                
;
label values tm7735   tm7735l;
label define tm7735l 
	0           "Not applicable"                
	14          "Passenger vehicle (includes"   
	15          "Truck or other commercial"     
	16          "Motorcycle, ATV"               
	17          "Work equipment"                
	18          "Other"                         
	-9          "Dk/no response"                
;
label values tm7736   tm7736l;
label define tm7736l 
	0           "Not applicable"                
	14          "Passenger vehicle (includes"   
	15          "Truck or other commercial"     
	16          "Motorcycle, ATV"               
	17          "Work equipment"                
	18          "Other"                         
	-9          "Dk/no response"                
;
label values tm7737   tm7737l;
label define tm7737l 
	0           "Not applicable"                
	14          "Passenger vehicle (includes"   
	15          "Truck or other commercial"     
	16          "Motorcycle, ATV"               
	17          "Work equipment"                
	18          "Other"                         
	-9          "Dk/no response"                
;
label values tm7738   tm7738l;
label define tm7738l 
	0           "Not applicable"                
	14          "Passenger vehicle (includes"   
	15          "Truck or other commercial"     
	16          "Motorcycle, ATV"               
	17          "Work equipment"                
	18          "Other"                         
	-9          "Dk/no response"                
;
label values tm7739   tm7739l;
label define tm7739l 
	0           "Not applicable"                
	19          "Bikes"                         
	20          "Skates, skateboards"           
	21          "Horse, other animal"           
	22          "Other"                         
	-9          "Dk/no response"                
;
label values tm7740   tm7740l;
label define tm7740l 
	0           "Not applicable"                
	19          "Bikes"                         
	20          "Skates, skateboards"           
	21          "Horse, other animal"           
	22          "Other"                         
	-9          "Dk/no response"                
;
label values tm7741   tm7741l;
label define tm7741l 
	0           "Not applicable"                
	19          "Bikes"                         
	20          "Skates, skateboards"           
	21          "Horse, other animal"           
	22          "Other"                         
	-9          "Dk/no response"                
;
label values tm7742   tm7742l;
label define tm7742l 
	0           "Not applicable"                
	19          "Bikes"                         
	20          "Skates, skateboards"           
	21          "Horse, other animal"           
	22          "Other"                         
	-9          "Dk/no response"                
;
label values tm7743   tm7743l;
label define tm7743l 
	0           "Not applicable"                
	19          "Bikes"                         
	20          "Skates, skateboards"           
	21          "Horse, other animal"           
	22          "Other"                         
	-9          "Dk/no response"                
;
label values tm7744   tm7744l;
label define tm7744l 
	0           "Not applicable"                
	19          "Bikes"                         
	20          "Skates, skateboards"           
	21          "Horse, other animal"           
	22          "Other"                         
	-9          "Dk/no response"                
;
label values tm7745   tm7745l;
label define tm7745l 
	0           "Not applicable"                
	19          "Bikes"                         
	20          "Skates, skateboards"           
	21          "Horse, other animal"           
	22          "Other"                         
	-9          "Dk/no response"                
;
label values tm7746   tm7746l;
label define tm7746l 
	0           "Not applicable"                
	1           "Cooking (at home or work)"     
	-9          "Dk/no response"                
;
label values tm7747   tm7747l;
label define tm7747l 
	0           "Not applicable"                
	1           "Cooking (at home or work)"     
	-9          "Dk/no response"                
;
label values tm7748   tm7748l;
label define tm7748l 
	0           "Not applicable"                
	1           "Cooking (at home or work)"     
	-9          "Dk/no response"                
;
label values tm7749   tm7749l;
label define tm7749l 
	0           "Not applicable"                
	1           "Cooking (at home or work)"     
	-9          "Dk/no response"                
;
label values tm7750   tm7750l;
label define tm7750l 
	0           "Not applicable"                
	1           "Cooking (at home or work)"     
	-9          "Dk/no response"                
;
label values tm7751   tm7751l;
label define tm7751l 
	0           "Not applicable"                
	1           "Cooking (at home or work)"     
	-9          "Dk/no response"                
;
label values tm7752   tm7752l;
label define tm7752l 
	0           "Not applicable"                
	1           "Cooking (at home or work)"     
	-9          "Dk/no response"                
;
label values tm7753   tm7753l;
label define tm7753l 
	0           "Not applicable"                
	1           "Eating, drinking"              
	-9          "Dk/no response"                
;
label values tm7754   tm7754l;
label define tm7754l 
	0           "Not applicable"                
	1           "Eating, drinking"              
	-9          "Dk/no response"                
;
label values tm7755   tm7755l;
label define tm7755l 
	0           "Not applicable"                
	1           "Eating, drinking"              
	-9          "Dk/no response"                
;
label values tm7756   tm7756l;
label define tm7756l 
	0           "Not applicable"                
	1           "Eating, drinking"              
	-9          "Dk/no response"                
;
label values tm7757   tm7757l;
label define tm7757l 
	0           "Not applicable"                
	1           "Eating, drinking"              
	-9          "Dk/no response"                
;
label values tm7758   tm7758l;
label define tm7758l 
	0           "Not applicable"                
	1           "Eating, drinking"              
	-9          "Dk/no response"                
;
label values tm7759   tm7759l;
label define tm7759l 
	0           "Not applicable"                
	1           "Eating, drinking"              
	-9          "Dk/no response"                
;
label values tm7760   tm7760l;
label define tm7760l 
	0           "Not applicable"                
	1           "Sleeping"                      
	-9          "Dk/no response"                
;
label values tm7761   tm7761l;
label define tm7761l 
	0           "Not applicable"                
	1           "Sleeping"                      
	-9          "Dk/no response"                
;
label values tm7762   tm7762l;
label define tm7762l 
	0           "Not applicable"                
	1           "Sleeping"                      
	-9          "Dk/no response"                
;
label values tm7763   tm7763l;
label define tm7763l 
	0           "Not applicable"                
	1           "Sleeping"                      
	-9          "Dk/no response"                
;
label values tm7764   tm7764l;
label define tm7764l 
	0           "Not applicable"                
	1           "Sleeping"                      
	-9          "Dk/no response"                
;
label values tm7765   tm7765l;
label define tm7765l 
	0           "Not applicable"                
	1           "Sleeping"                      
	-9          "Dk/no response"                
;
label values tm7766   tm7766l;
label define tm7766l 
	0           "Not applicable"                
	1           "Sleeping"                      
	-9          "Dk/no response"                
;
label values tm7767   tm7767l;
label define tm7767l 
	0           "Not applicable"                
	1           "Unspecified"                   
	-9          "Dk/no response"                
;
label values tm7768   tm7768l;
label define tm7768l 
	0           "Not applicable"                
	1           "Unspecified"                   
	-9          "Dk/no response"                
;
label values tm7769   tm7769l;
label define tm7769l 
	0           "Not applicable"                
	1           "Unspecified"                   
	-9          "Dk/no response"                
;
label values tm7770   tm7770l;
label define tm7770l 
	0           "Not applicable"                
	1           "Unspecified"                   
	-9          "Dk/no response"                
;
label values tm7771   tm7771l;
label define tm7771l 
	0           "Not applicable"                
	1           "Unspecified"                   
	-9          "Dk/no response"                
;
label values tm7772   tm7772l;
label define tm7772l 
	0           "Not applicable"                
	1           "Unspecified"                   
	-9          "Dk/no response"                
;
label values tm7773   tm7773l;
label define tm7773l 
	0           "Not applicable"                
	1           "Unspecified"                   
	-9          "Dk/no response"                
;
label values tm7774   tm7774l;
label define tm7774l 
	0           "Not applicable"                
;
label values tm7775   tm7775l;
label define tm7775l 
	0           "Not applicable"                
;
label values tm7776   tm7776l;
label define tm7776l 
	0           "Not applicable"                
;
label values tm7777   tm7777l;
label define tm7777l 
	0           "Not applicable"                
;
label values tm7778   tm7778l;
label define tm7778l 
	0           "Not applicable"                
;
label values tm7779   tm7779l;
label define tm7779l 
	0           "Not applicable"                
;
label values tm7780   tm7780l;
label define tm7780l 
	0           "Not applicable"                
;
label values tm7781   tm7781l;
label define tm7781l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7782   tm7782l;
label define tm7782l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7783   tm7783l;
label define tm7783l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7784   tm7784l;
label define tm7784l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7785   tm7785l;
label define tm7785l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7786   tm7786l;
label define tm7786l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7787   tm7787l;
label define tm7787l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7788   tm7788l;
label define tm7788l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	3           "Inside"                        
	4           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7789   tm7789l;
label define tm7789l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	3           "Inside"                        
	4           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7790   tm7790l;
label define tm7790l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	3           "Inside"                        
	4           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7791   tm7791l;
label define tm7791l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	3           "Inside"                        
	4           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7792   tm7792l;
label define tm7792l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	3           "Inside"                        
	4           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7793   tm7793l;
label define tm7793l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	3           "Inside"                        
	4           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7794   tm7794l;
label define tm7794l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	3           "Inside"                        
	4           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7795   tm7795l;
label define tm7795l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	5           "Inside"                        
	6           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7796   tm7796l;
label define tm7796l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	5           "Inside"                        
	6           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7797   tm7797l;
label define tm7797l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	5           "Inside"                        
	6           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7798   tm7798l;
label define tm7798l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	5           "Inside"                        
	6           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7799   tm7799l;
label define tm7799l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	5           "Inside"                        
	6           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7800   tm7800l;
label define tm7800l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	5           "Inside"                        
	6           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7801   tm7801l;
label define tm7801l 
	0           "Not applicable"                
	1           "Inside"                        
	2           "Outside"                       
	5           "Inside"                        
	6           "Outside"                       
	-9          "Dk/no response"                
;
label values tm7802   tm7802l;
label define tm7802l 
	0           "Not applicable"                
	1           "Street/highway, sidewalk"      
	-9          "Dk/no response"                
;
label values tm7803   tm7803l;
label define tm7803l 
	0           "Not applicable"                
	1           "Street/highway, sidewalk"      
	-9          "Dk/no response"                
;
label values tm7804   tm7804l;
label define tm7804l 
	0           "Not applicable"                
	1           "Street/highway, sidewalk"      
	-9          "Dk/no response"                
;
label values tm7805   tm7805l;
label define tm7805l 
	0           "Not applicable"                
	1           "Street/highway, sidewalk"      
	-9          "Dk/no response"                
;
label values tm7806   tm7806l;
label define tm7806l 
	0           "Not applicable"                
	1           "Street/highway, sidewalk"      
	-9          "Dk/no response"                
;
label values tm7807   tm7807l;
label define tm7807l 
	0           "Not applicable"                
	1           "Street/highway, sidewalk"      
	-9          "Dk/no response"                
;
label values tm7808   tm7808l;
label define tm7808l 
	0           "Not applicable"                
	1           "Street/highway, sidewalk"      
	-9          "Dk/no response"                
;
label values tm7809   tm7809l;
label define tm7809l 
	0           "Not applicable"                
	1           "Parking lot"                   
	-9          "Dk/no response"                
;
label values tm7810   tm7810l;
label define tm7810l 
	0           "Not applicable"                
	1           "Parking lot"                   
	-9          "Dk/no response"                
;
label values tm7811   tm7811l;
label define tm7811l 
	0           "Not applicable"                
	1           "Parking lot"                   
	-9          "Dk/no response"                
;
label values tm7812   tm7812l;
label define tm7812l 
	0           "Not applicable"                
	1           "Parking lot"                   
	-9          "Dk/no response"                
;
label values tm7813   tm7813l;
label define tm7813l 
	0           "Not applicable"                
	1           "Parking lot"                   
	-9          "Dk/no response"                
;
label values tm7814   tm7814l;
label define tm7814l 
	0           "Not applicable"                
	1           "Parking lot"                   
	-9          "Dk/no response"                
;
label values tm7815   tm7815l;
label define tm7815l 
	0           "Not applicable"                
	1           "Parking lot"                   
	-9          "Dk/no response"                
;
label values tm7816   tm7816l;
label define tm7816l 
	0           "Not applicable"                
	1           "Recreation center, sport"      
	-9          "Dk/no response"                
;
label values tm7817   tm7817l;
label define tm7817l 
	0           "Not applicable"                
	1           "Recreation center, sport"      
	-9          "Dk/no response"                
;
label values tm7818   tm7818l;
label define tm7818l 
	0           "Not applicable"                
	1           "Recreation center, sport"      
	-9          "Dk/no response"                
;
label values tm7819   tm7819l;
label define tm7819l 
	0           "Not applicable"                
	1           "Recreation center, sport"      
	-9          "Dk/no response"                
;
label values tm7820   tm7820l;
label define tm7820l 
	0           "Not applicable"                
	1           "Recreation center, sport"      
	-9          "Dk/no response"                
;
label values tm7821   tm7821l;
label define tm7821l 
	0           "Not applicable"                
	1           "Recreation center, sport"      
	-9          "Dk/no response"                
;
label values tm7822   tm7822l;
label define tm7822l 
	0           "Not applicable"                
	1           "Recreation center, sport"      
	-9          "Dk/no response"                
;
label values tm7823   tm7823l;
label define tm7823l 
	0           "Not applicable"                
	1           "Park, play-grounds, playing"   
	-9          "Dk/no response"                
;
label values tm7824   tm7824l;
label define tm7824l 
	0           "Not applicable"                
	1           "Park, play-grounds, playing"   
	-9          "Dk/no response"                
;
label values tm7825   tm7825l;
label define tm7825l 
	0           "Not applicable"                
	1           "Park, play-grounds, playing"   
	-9          "Dk/no response"                
;
label values tm7826   tm7826l;
label define tm7826l 
	0           "Not applicable"                
	1           "Park, play-grounds, playing"   
	-9          "Dk/no response"                
;
label values tm7827   tm7827l;
label define tm7827l 
	0           "Not applicable"                
	1           "Park, play-grounds, playing"   
	-9          "Dk/no response"                
;
label values tm7828   tm7828l;
label define tm7828l 
	0           "Not applicable"                
	1           "Park, play-grounds, playing"   
	-9          "Dk/no response"                
;
label values tm7829   tm7829l;
label define tm7829l 
	0           "Not applicable"                
	1           "Park, play-grounds, playing"   
	-9          "Dk/no response"                
;
label values tm7830   tm7830l;
label define tm7830l 
	0           "Not applicable"                
	1           "Pool"                          
	2           "Other"                         
	11          "Pool"                          
	12          "Other"                         
	-9          "Dk/no response"                
;
label values tm7831   tm7831l;
label define tm7831l 
	0           "Not applicable"                
	1           "Pool"                          
	2           "Other"                         
	11          "Pool"                          
	12          "Other"                         
	-9          "Dk/no response"                
;
label values tm7832   tm7832l;
label define tm7832l 
	0           "Not applicable"                
	1           "Pool"                          
	2           "Other"                         
	11          "Pool"                          
	12          "Other"                         
	-9          "Dk/no response"                
;
label values tm7833   tm7833l;
label define tm7833l 
	0           "Not applicable"                
	1           "Pool"                          
	2           "Other"                         
	11          "Pool"                          
	12          "Other"                         
	-9          "Dk/no response"                
;
label values tm7834   tm7834l;
label define tm7834l 
	0           "Not applicable"                
	1           "Pool"                          
	2           "Other"                         
	11          "Pool"                          
	12          "Other"                         
	-9          "Dk/no response"                
;
label values tm7835   tm7835l;
label define tm7835l 
	0           "Not applicable"                
	1           "Pool"                          
	2           "Other"                         
	11          "Pool"                          
	12          "Other"                         
	-9          "Dk/no response"                
;
label values tm7836   tm7836l;
label define tm7836l 
	0           "Not applicable"                
	1           "Pool"                          
	2           "Other"                         
	11          "Pool"                          
	12          "Other"                         
	-9          "Dk/no response"                
;
label values tm7837   tm7837l;
label define tm7837l 
	0           "Not applicable"                
	1           "Farm"                          
	-9          "Dk/no response"                
;
label values tm7838   tm7838l;
label define tm7838l 
	0           "Not applicable"                
	1           "Farm"                          
	-9          "Dk/no response"                
;
label values tm7839   tm7839l;
label define tm7839l 
	0           "Not applicable"                
	1           "Farm"                          
	-9          "Dk/no response"                
;
label values tm7840   tm7840l;
label define tm7840l 
	0           "Not applicable"                
	1           "Farm"                          
	-9          "Dk/no response"                
;
label values tm7841   tm7841l;
label define tm7841l 
	0           "Not applicable"                
	1           "Farm"                          
	-9          "Dk/no response"                
;
label values tm7842   tm7842l;
label define tm7842l 
	0           "Not applicable"                
	1           "Farm"                          
	-9          "Dk/no response"                
;
label values tm7843   tm7843l;
label define tm7843l 
	0           "Not applicable"                
	1           "Farm"                          
	-9          "Dk/no response"                
;
label values tm7844   tm7844l;
label define tm7844l 
	0           "Not applicable"                
	1           "Other"                         
	-9          "Dk/no response go to TM7621 for"
;
label values tm7845   tm7845l;
label define tm7845l 
	0           "Not applicable"                
	1           "Other"                         
	-9          "Dk/no response go to TM7622 for"
;
label values tm7846   tm7846l;
label define tm7846l 
	0           "Not applicable"                
	1           "Other"                         
	-9          "Dk/no response go to TM7623 for"
;
label values tm7847   tm7847l;
label define tm7847l 
	0           "Not applicable"                
	1           "Other"                         
	-9          "Dk/no response go to TM7624 for"
;
label values tm7848   tm7848l;
label define tm7848l 
	0           "Not applicable"                
	1           "Other"                         
	-9          "Dk/no response go to TM7625 for"
;
label values tm7849   tm7849l;
label define tm7849l 
	0           "Not applicable"                
	1           "Other"                         
	-9          "Dk/no response go to TM7626 for"
;
label values tm7850   tm7850l;
label define tm7850l 
	0           "Not applicable"                
	1           "Other"                         
	-9          "Dk/no response go to TM7851"   
;
label values tm7851   tm7851l;
label define tm7851l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8400"           
;
label values tm7852   tm7852l;
label define tm7852l 
	-1          "Dk"                            
	-3          "Na - item missing"             
	-9          "Out of universe for the child" 
	99          "In universe, but missing or"   
;
label values tm7853   tm7853l;
label define tm7853l 
	-1          "Dk"                            
	-3          "Na - item missing"             
	-9          "Out of universe for the child" 
	9           "In universe, but missing or"   
;
label values tm7854   tm7854l;
label define tm7854l 
	-1          "Dk"                            
	-3          "Na - item missing"             
	-9          "Out of universe for the child" 
	99          "In universe, but missing or"   
;
label values tm7855   tm7855l;
label define tm7855l 
	-1          "Dk"                            
	-3          "Na - item missing"             
	-9          "Out of universe for the child" 
	99          "In universe, but missing or"   
;
label values tm7856   tm7856l;
label define tm7856l 
	-1          "Dk"                            
	-3          "Na - item missing"             
	-9          "Out of universe for the child" 
	99          "In universe, but missing or"   
;
label values tm7857   tm7857l;
label define tm7857l 
	-1          "Dk"                            
	-3          "Na - item missing"             
	-9          "Out of universe for the child" 
	99          "In universe, but missing or"   
;
label values tm7858   tm7858l;
label define tm7858l 
	-1          "Dk"                            
	-3          "Na - item missing"             
	-9          "Out of universe for the child" 
	99          "In universe, but missing or"   
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Part F (TM9002)"  
;
label values tm8401   tm8401l;
label define tm8401l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Part F (TM9002)"  
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not applicable"                
;
label values tm8403   tm8403l;
label define tm8403l 
	0           "Not applicable"                
;
label values tm8404   tm8404l;
label define tm8404l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8405   tm8405l;
label define tm8405l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8406   tm8406l;
label define tm8406l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8407   tm8407l;
label define tm8407l 
	0           "Not applicable"                
;
label values tm8408   tm8408l;
label define tm8408l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8409   tm8409l;
label define tm8409l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8410   tm8410l;
label define tm8410l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8411   tm8411l;
label define tm8411l 
	0           "Not applicable"                
;
label values tm8412   tm8412l;
label define tm8412l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8413   tm8413l;
label define tm8413l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8414   tm8414l;
label define tm8414l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8415   tm8415l;
label define tm8415l 
	0           "Not applicable"                
;
label values tm8416   tm8416l;
label define tm8416l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8417   tm8417l;
label define tm8417l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8418   tm8418l;
label define tm8418l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8419   tm8419l;
label define tm8419l 
	0           "Not applicable"                
;
label values tm8420   tm8420l;
label define tm8420l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8421   tm8421l;
label define tm8421l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8422   tm8422l;
label define tm8422l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8423   tm8423l;
label define tm8423l 
	0           "Not applicable"                
;
label values tm8424   tm8424l;
label define tm8424l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8425   tm8425l;
label define tm8425l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8426   tm8426l;
label define tm8426l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8427   tm8427l;
label define tm8427l 
	0           "Not applicable"                
;
label values tm8428   tm8428l;
label define tm8428l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8429   tm8429l;
label define tm8429l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8430   tm8430l;
label define tm8430l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8431   tm8431l;
label define tm8431l 
	0           "Not applicable"                
;
label values tm8432   tm8432l;
label define tm8432l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8433   tm8433l;
label define tm8433l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8434   tm8434l;
label define tm8434l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8435   tm8435l;
label define tm8435l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - for each child listed in" 
;
label values tm8436   tm8436l;
label define tm8436l 
	0           "Not applicable"                
	1           "Yes - mark the 'yes' box in"   
	2           "No"                            
;
label values tm8437   tm8437l;
label define tm8437l 
	0           "Not applicable"                
;
label values tm8438   tm8438l;
label define tm8438l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to 1j"               
;
label values tm8439   tm8439l;
label define tm8439l 
	0           "Not applicable"                
;
label values tm8440   tm8440l;
label define tm8440l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8441"           
;
label values tm8441   tm8441l;
label define tm8441l 
	0           "Not applicable"                
	1           "Voluntary written agreement"   
	2           "Court-ordered agreement"       
	3           "Other type of written agreement"
	4           "Non written (verbal agreement)"
;
label values tm8442   tm8442l;
label define tm8442l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm8443   tm8443l;
label define tm8443l 
	0           "Not applicable"                
;
label values tm8444   tm8444l;
label define tm8444l 
	0           "Not applicable"                
;
label values tm8445   tm8445l;
label define tm8445l 
	0           "Not applicable"                
;
label values tm8446   tm8446l;
label define tm8446l 
	0           "Not applicable"                
;
label values tm8447   tm8447l;
label define tm8447l 
	0           "Not applicable"                
	-1          "Don't know (dk)"               
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8451"           
;
label values tm8449   tm8449l;
label define tm8449l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "Not applicable"                
	1           "1993 -1996 - Skip to TM8452"   
	2           "1992 or earlier - Skip to TM8460"
	3           "Dk - Skip to TM8460"           
;
label values tm8451   tm8451l;
label define tm8451l 
	0           "Not applicable"                
	1           "1993 - 1996 - Skip to TM8458"  
	2           "1992 or earlier - Skip to TM8460"
	3           "Dk - Skip to TM8460"           
;
label values tm8452   tm8452l;
label define tm8452l 
	0           "Not applicable"                
;
label values tm8453   tm8453l;
label define tm8453l 
	0           "Not applicable"                
;
label values tm8454   tm8454l;
label define tm8454l 
	0           "Not applicable"                
;
label values tm8455   tm8455l;
label define tm8455l 
	0           "Not applicable"                
	60000       "Dollars per year"              
;
label values tm8456   tm8456l;
label define tm8456l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8457   tm8457l;
label define tm8457l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8458   tm8458l;
label define tm8458l 
	0           "Not applicable"                
	1           "Yes - Skip to TM8460"          
	2           "No"                            
;
label values tm8459   tm8459l;
label define tm8459l 
	0           "Not applicable"                
	1           "Child(ren) over the age limit" 
	2           "Other parent not working"      
	3           "Other parent in jail or"       
	4           "Payment suspended by court or" 
	5           "Other"                         
;
label values tm8460   tm8460l;
label define tm8460l 
	0           "Not applicable"                
	13200       "Amount supposed to have"       
	-1          "Don't know"                    
;
label values tm8461   tm8461l;
label define tm8461l 
	0           "Not applicable"                
	1           "Directly from the other parent?"
	2           "Through a court?"              
	3           "Through the welfare or child"  
	4           "Some other method"             
	-1          "Don't know"                    
;
label values tm8462   tm8462l;
label define tm8462l 
	0           "Not applicable"                
	13200       "Total amount received"         
	-3          "None - Skip to TM8465"         
	-1          "Don't know"                    
;
label values tm8463   tm8463l;
label define tm8463l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Check Item TM8475"
;
label values tm8464   tm8464l;
label define tm8464l 
	0           "Not applicable"                
	1           "All of the time"               
	2           "Most of the time"              
	3           "Some of the time"              
	4           "None of the time"              
;
label values tm8465   tm8465l;
label define tm8465l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8467-72"        
	-1          "Don't know"                    
;
label values tm8466   tm8466l;
label define tm8466l 
	0           "Not applicable"                
	1           "Less than $500"                
	2           "Between $500 and $5, 000"      
	3           "More than $5, 000"             
	-1          "Don't know"                    
;
label values tm8467   tm8467l;
label define tm8467l 
	0           "Not applicable"                
	1           "Non-custodial parent to provide"
;
label values tm8468   tm8468l;
label define tm8468l 
	0           "Not applicable"                
	1           "Custodial parent to provide"   
;
label values tm8469   tm8469l;
label define tm8469l 
	0           "Not applicable"                
	1           "Non-custodial parent to pay"   
;
label values tm8470   tm8470l;
label define tm8470l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm8471   tm8471l;
label define tm8471l 
	0           "Not applicable"                
	1           "None"                          
;
label values tm8472   tm8472l;
label define tm8472l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8473   tm8473l;
label define tm8473l 
	0           "Not applicable"                
	1           "Joint legal and physical custody"
	2           "Joint legal with mother physical"
	3           "Joint legal with father physical"
	4           "Mother legal and physical"     
	5           "Father legal and physical"     
	6           "Split custody"                 
	7           "Other"                         
;
label values tm8474   tm8474l;
label define tm8474l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8475   tm8475l;
label define tm8475l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8477-81"        
;
label values tm8476   tm8476l;
label define tm8476l 
	0           "Not applicable"                
	1           "Yes ask TM8477 for all children"
	2           "No  ask TM8477 for oldest child"
;
label values tm8477   tm8477l;
label define tm8477l 
	0           "Not applicable"                
;
label values tm8478   tm8478l;
label define tm8478l 
	0           "Not applicable"                
;
label values tm8479   tm8479l;
label define tm8479l 
	0           "Not applicable"                
;
label values tm8480   tm8480l;
label define tm8480l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8481   tm8481l;
label define tm8481l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm8482   tm8482l;
label define tm8482l 
	0           "Not applicable"                
	1           "Same county/city"              
	2           "Same state (different county/" 
	3           "Different state"               
	4           "Other parent now deceased -"   
	5           "Other"                         
	6           "Unknown - Skip to TM8668"      
;
label values tm8483   tm8483l;
label define tm8483l 
	1           "Yes"                           
	2           "No - Skip to Check Item TM8668"
;
label values tm8484   tm8484l;
label define tm8484l 
	0           "Not applicable"                
	1           "Yes - Skip to TM8662"          
	2           "No"                            
;
label values tm8485   tm8485l;
label define tm8485l 
	0           "Not applicable"                
	1           "Respondent - Skip TM8668"      
	2           "Other parent - Skip TM8668"    
	3           "Both respondent and other"     
;
label values tm8486   tm8486l;
label define tm8486l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8487   tm8487l;
label define tm8487l 
	0           "Not applicable"                
;
label values tm8488   tm8488l;
label define tm8488l 
	0           "Not applicable"                
;
label values tm8489   tm8489l;
label define tm8489l 
	0           "Not applicable"                
;
label values tm8490   tm8490l;
label define tm8490l 
	0           "Not applicable"                
	999999      "Dollars per year"              
;
label values tm8491   tm8491l;
label define tm8491l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8492   tm8492l;
label define tm8492l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8495"           
;
label values tm8493   tm8493l;
label define tm8493l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8494   tm8494l;
label define tm8494l 
	0           "Not applicable"                
	1           "1993 -1996 - Skip to TM8496"   
	2           "1992 or earlier - Skip to TM8503"
	3           "Dk - Skip to TM8503"           
;
label values tm8495   tm8495l;
label define tm8495l 
	0           "Not applicable"                
	1           "1993 -1996 - Skip to TM8496"   
	2           "1992 or earlier - Skip to TM8503"
	3           "Dk - Skip to TM8503"           
;
label values tm8496   tm8496l;
label define tm8496l 
	0           "Not applicable"                
	999999      "Dollars per week"              
;
label values tm8497   tm8497l;
label define tm8497l 
	0           "Not applicable"                
	999999      "Dollars per biweekly"          
;
label values tm8498   tm8498l;
label define tm8498l 
	0           "Not applicable"                
;
label values tm8499   tm8499l;
label define tm8499l 
	0           "Not applicable"                
	999999      "Dollars per year"              
;
label values tm8500   tm8500l;
label define tm8500l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8501   tm8501l;
label define tm8501l 
	0           "Not applicable"                
	1           "Yes - Skip to TM8503"          
	2           "No"                            
;
label values tm8502   tm8502l;
label define tm8502l 
	0           "Not applicable"                
	1           "Child(ren) too old"            
	2           "Other parent not working"      
	3           "Other parent in jail or"       
	4           "Other"                         
;
label values tm8503   tm8503l;
label define tm8503l 
	0           "Not applicable"                
	13200       "Total amount that ... was"     
	-1          "Don't know"                    
;
label values tm8504   tm8504l;
label define tm8504l 
	0           "Not applicable"                
	13200       "Total amount that ... are"     
	-1          "Don't know"                    
	-3          "None - Skip to TM8507"         
;
label values tm8505   tm8505l;
label define tm8505l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8517"           
;
label values tm8506   tm8506l;
label define tm8506l 
	0           "Not applicable"                
	1           "All of the time"               
	2           "Most of the time"              
	3           "Some of the time"              
	4           "None of the time"              
;
label values tm8507   tm8507l;
label define tm8507l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8509-14"        
	-1          "Dk"                            
;
label values tm8508   tm8508l;
label define tm8508l 
	0           "Not applicable"                
	1           "Less than $500"                
	2           "Between $500 and $5, 000"      
	3           "More than $5, 000"             
	-1          "Dk"                            
;
label values tm8509   tm8509l;
label define tm8509l 
	0           "Not applicable"                
	1           "Non-custodial parent to provide"
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "Not applicable"                
	1           "Custodial parent to provide"   
;
label values tm8511   tm8511l;
label define tm8511l 
	0           "Not applicable"                
	1           "Non-custodial parent to pay"   
;
label values tm8512   tm8512l;
label define tm8512l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm8513   tm8513l;
label define tm8513l 
	0           "Not applicable"                
	1           "None"                          
;
label values tm8514   tm8514l;
label define tm8514l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8515   tm8515l;
label define tm8515l 
	0           "Not applicable"                
	1           "Child(ren) live with mother"   
	2           "Child(ren) live with father"   
	3           "Child(ren) live with mother and"
	4           "None"                          
	5           "Other"                         
;
label values tm8516   tm8516l;
label define tm8516l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8517   tm8517l;
label define tm8517l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8519-23"        
;
label values tm8518   tm8518l;
label define tm8518l 
	0           "Not applicable"                
	1           "Yes ask TM8519-23 for all"     
	2           "No ask TM8519-23 for oldest"   
;
label values tm8519   tm8519l;
label define tm8519l 
	0           "Not applicable"                
;
label values tm8520   tm8520l;
label define tm8520l 
	0           "Not applicable"                
;
label values tm8521   tm8521l;
label define tm8521l 
	0           "Not applicable"                
;
label values tm8522   tm8522l;
label define tm8522l 
	-3          "None"                          
;
label values tm8523   tm8523l;
label define tm8523l 
	-1          "Don't know"                    
;
label values tm8524   tm8524l;
label define tm8524l 
	0           "Not applicable"                
	1           "Male - Skip to TM8655"         
	2           "Female"                        
;
label values tm8525   tm8525l;
label define tm8525l 
	0           "Not applicable"                
	1           "Never married - go to TM8526"  
	2           "All others - Skip to TM8590"   
;
label values tm8526   tm8526l;
label define tm8526l 
	0           "Not applicable"                
;
label values tm8527   tm8527l;
label define tm8527l 
	0           "Not applicable"                
;
label values tm8528   tm8528l;
label define tm8528l 
	0           "Not applicable"                
;
label values tm8529   tm8529l;
label define tm8529l 
	0           "Not applicable"                
;
label values tm8530   tm8530l;
label define tm8530l 
	0           "Not applicable"                
;
label values tm8531   tm8531l;
label define tm8531l 
	0           "Not applicable"                
;
label values tm8532   tm8532l;
label define tm8532l 
	0           "Not applicable"                
;
label values tm8533   tm8533l;
label define tm8533l 
	0           "Not applicable"                
;
label values tm8534   tm8534l;
label define tm8534l 
	0           "Not applicable"                
;
label values tm8535   tm8535l;
label define tm8535l 
	0           "Not applicable"                
;
label values tm8536   tm8536l;
label define tm8536l 
	0           "Not applicable"                
;
label values tm8537   tm8537l;
label define tm8537l 
	0           "Not applicable"                
;
label values tm8538   tm8538l;
label define tm8538l 
	0           "Not applicable"                
;
label values tm8539   tm8539l;
label define tm8539l 
	0           "Not applicable"                
;
label values tm8540   tm8540l;
label define tm8540l 
	0           "Not applicable"                
;
label values tm8541   tm8541l;
label define tm8541l 
	0           "Not applicable"                
;
label values tm8542   tm8542l;
label define tm8542l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8543   tm8543l;
label define tm8543l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8544   tm8544l;
label define tm8544l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8545   tm8545l;
label define tm8545l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8546   tm8546l;
label define tm8546l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8547   tm8547l;
label define tm8547l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8548   tm8548l;
label define tm8548l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8549   tm8549l;
label define tm8549l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8550   tm8550l;
label define tm8550l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8551   tm8551l;
label define tm8551l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8552   tm8552l;
label define tm8552l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8553   tm8553l;
label define tm8553l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8554   tm8554l;
label define tm8554l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8555   tm8555l;
label define tm8555l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8556   tm8556l;
label define tm8556l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8557   tm8557l;
label define tm8557l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8558   tm8558l;
label define tm8558l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8559   tm8559l;
label define tm8559l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8560   tm8560l;
label define tm8560l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8561   tm8561l;
label define tm8561l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8562   tm8562l;
label define tm8562l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8563   tm8563l;
label define tm8563l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8564   tm8564l;
label define tm8564l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8565   tm8565l;
label define tm8565l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8566   tm8566l;
label define tm8566l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8567   tm8567l;
label define tm8567l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8568   tm8568l;
label define tm8568l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8569   tm8569l;
label define tm8569l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8570   tm8570l;
label define tm8570l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8571   tm8571l;
label define tm8571l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8572   tm8572l;
label define tm8572l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8573   tm8573l;
label define tm8573l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8574   tm8574l;
label define tm8574l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8575   tm8575l;
label define tm8575l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8576   tm8576l;
label define tm8576l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8577   tm8577l;
label define tm8577l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8578   tm8578l;
label define tm8578l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8579   tm8579l;
label define tm8579l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8580   tm8580l;
label define tm8580l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8581   tm8581l;
label define tm8581l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8582   tm8582l;
label define tm8582l 
	0           "Not applicable"                
	1           "Yes - ask TM8542 through TM8574"
	2           "No  - Skip to TM8655"          
;
label values tm8583   tm8583l;
label define tm8583l 
	0           "Not applicable"                
	1           "Yes - ask TM8542 through TM8574"
	2           "No  - Skip to TM8655"          
;
label values tm8584   tm8584l;
label define tm8584l 
	0           "Not applicable"                
	1           "Yes - ask TM8542 through TM8574"
	2           "No  - Skip to TM8655"          
;
label values tm8585   tm8585l;
label define tm8585l 
	0           "Not applicable"                
	1           "Yes - ask TM8542 through TM8574"
	2           "No  - Skip to TM8655"          
;
label values tm8586   tm8586l;
label define tm8586l 
	0           "Not applicable"                
	1           "Yes - ask TM8542 through TM8574"
	2           "No  - Skip to TM8655"          
;
label values tm8587   tm8587l;
label define tm8587l 
	0           "Not applicable"                
	1           "Yes - ask TM8542 through TM8574"
	2           "No  - Skip to TM8655"          
;
label values tm8588   tm8588l;
label define tm8588l 
	0           "Not applicable"                
	1           "Yes - ask TM8542 through TM8574"
	2           "No  - Skip to TM8655"          
;
label values tm8590   tm8590l;
label define tm8590l 
	0           "Not applicalbe"                
;
label values tm8591   tm8591l;
label define tm8591l 
	0           "Not applicalbe"                
;
label values tm8592   tm8592l;
label define tm8592l 
	0           "Not applicalbe"                
;
label values tm8593   tm8593l;
label define tm8593l 
	0           "Not applicalbe"                
;
label values tm8594   tm8594l;
label define tm8594l 
	0           "Not applicalbe"                
;
label values tm8595   tm8595l;
label define tm8595l 
	0           "Not applicalbe"                
;
label values tm8596   tm8596l;
label define tm8596l 
	0           "Not applicalbe"                
;
label values tm8597   tm8597l;
label define tm8597l 
	0           "Not applicalbe"                
;
label values tm8598   tm8598l;
label define tm8598l 
	0           "Not applicable"                
;
label values tm8599   tm8599l;
label define tm8599l 
	0           "Not applicable"                
;
label values tm8600   tm8600l;
label define tm8600l 
	0           "Not applicable"                
;
label values tm8601   tm8601l;
label define tm8601l 
	0           "Not applicable"                
;
label values tm8602   tm8602l;
label define tm8602l 
	0           "Not applicable"                
;
label values tm8603   tm8603l;
label define tm8603l 
	0           "Not applicable"                
;
label values tm8604   tm8604l;
label define tm8604l 
	0           "Not applicable"                
;
label values tm8605   tm8605l;
label define tm8605l 
	0           "Not applicable"                
;
label values tm8606   tm8606l;
label define tm8606l 
	0           "Not applicable"                
	1           "Yes - Skip to TM8655"          
	2           "No"                            
;
label values tm8607   tm8607l;
label define tm8607l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8608   tm8608l;
label define tm8608l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8609   tm8609l;
label define tm8609l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8610   tm8610l;
label define tm8610l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8611   tm8611l;
label define tm8611l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8612   tm8612l;
label define tm8612l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8613   tm8613l;
label define tm8613l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8614   tm8614l;
label define tm8614l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8615   tm8615l;
label define tm8615l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8616   tm8616l;
label define tm8616l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8617   tm8617l;
label define tm8617l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8618   tm8618l;
label define tm8618l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8619   tm8619l;
label define tm8619l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8620   tm8620l;
label define tm8620l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8621   tm8621l;
label define tm8621l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8622   tm8622l;
label define tm8622l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8623   tm8623l;
label define tm8623l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8624   tm8624l;
label define tm8624l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8625   tm8625l;
label define tm8625l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8626   tm8626l;
label define tm8626l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8627   tm8627l;
label define tm8627l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8628   tm8628l;
label define tm8628l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8629   tm8629l;
label define tm8629l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8630   tm8630l;
label define tm8630l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8631   tm8631l;
label define tm8631l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8632   tm8632l;
label define tm8632l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8633   tm8633l;
label define tm8633l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8634   tm8634l;
label define tm8634l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8635   tm8635l;
label define tm8635l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8636   tm8636l;
label define tm8636l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8637   tm8637l;
label define tm8637l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8638   tm8638l;
label define tm8638l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8639   tm8639l;
label define tm8639l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8640   tm8640l;
label define tm8640l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8641   tm8641l;
label define tm8641l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8642   tm8642l;
label define tm8642l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8643   tm8643l;
label define tm8643l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8644   tm8644l;
label define tm8644l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8645   tm8645l;
label define tm8645l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8646   tm8646l;
label define tm8646l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't Know"                    
;
label values tm8647   tm8647l;
label define tm8647l 
	0           "Not Applicable"                
	1           "Yes - Ask TM8607 Through TM8639"
	2           "No  - Skip To TM8655"          
;
label values tm8648   tm8648l;
label define tm8648l 
	0           "Not Applicable"                
	1           "Yes - Ask TM8608 Through TM8639"
	2           "No  - Skip To TM8655"          
;
label values tm8649   tm8649l;
label define tm8649l 
	0           "Not Applicable"                
	1           "Yes - Ask TM8609 Through TM8639"
	2           "No  - Skip To TM8655"          
;
label values tm8650   tm8650l;
label define tm8650l 
	0           "Not Applicable"                
	1           "Yes - Ask TM8610 Through TM8639"
	2           "No  - Skip To TM8655"          
;
label values tm8651   tm8651l;
label define tm8651l 
	0           "Not Applicable"                
	1           "Yes - Ask TM8611 Through TM8639"
	2           "No  - Skip To TM8655"          
;
label values tm8652   tm8652l;
label define tm8652l 
	0           "Not Applicable"                
	1           "Yes - Ask TM8600 Through TM8639"
	2           "No  - Skip To TM8655"          
;
label values tm8653   tm8653l;
label define tm8653l 
	0           "Not Applicable"                
	1           "Yes - Ask TM8612 Through TM8639"
	2           "No  - Skip To TM8655"          
;
label values tm8655   tm8655l;
label define tm8655l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No - Skip To TM8664"           
;
label values tm8656   tm8656l;
label define tm8656l 
	0           "Not Applicable"                
	1           "Legal Paternity Not Established"
;
label values tm8657   tm8657l;
label define tm8657l 
	0           "Not Applicable"                
	1           "Unable To Locate Parent"       
;
label values tm8658   tm8658l;
label define tm8658l 
	0           "Not Applicable"                
	1           "Other Parent Unable To Pay"    
;
label values tm8659   tm8659l;
label define tm8659l 
	0           "Not Applicable"                
	1           "Final Agreement Pending"       
;
label values tm8660   tm8660l;
label define tm8660l 
	0           "Not Applicable"                
	1           "Accepted Property Settlement"  
;
label values tm8661   tm8661l;
label define tm8661l 
	0           "Not Applicable"                
	1           "Do Not Want A Legal Child"     
;
label values tm8662   tm8662l;
label define tm8662l 
	0           "Not Applicable"                
	1           "Did Not Pursue Award"          
;
label values tm8663   tm8663l;
label define tm8663l 
	0           "Not Applicable"                
	1           "Other"                         
;
label values tm8664   tm8664l;
label define tm8664l 
	0           "Not Applicable"                
	1           "Same County / City"            
	2           "Same State (Different County /"
	3           "Different State"               
	4           "Other Parent Now Deceased - Skip"
	5           "Other"                         
	6           "Unknown - Skip To Check Item"  
;
label values tm8665   tm8665l;
label define tm8665l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No - Skip To TM8668"           
;
label values tm8666   tm8666l;
label define tm8666l 
	0           "Not Applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8667   tm8667l;
label define tm8667l 
	0           "Not Applicable"                
	1           "Respondent"                    
	2           "Other Parent"                  
	3           "Both Respondent And Other Parent"
;
label values tm8668   tm8668l;
label define tm8668l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8676"           
;
label values tm8669   tm8669l;
label define tm8669l 
	0           "Not applicable"                
;
label values tm8670   tm8670l;
label define tm8670l 
	0           "Not applicable"                
	999999      "Dollars per biweek"            
;
label values tm8671   tm8671l;
label define tm8671l 
	0           "Not applicable"                
;
label values tm8672   tm8672l;
label define tm8672l 
	0           "Not applicable"                
;
label values tm8673   tm8673l;
label define tm8673l 
	0           "Not applicable"                
	-1          "Don't know"                    
	-3          "None"                          
;
label values tm8674   tm8674l;
label define tm8674l 
	0           "Not applicable"                
	13200       "Total amount that ... actually"
	-1          "Don't know"                    
	-3          "None"                          
;
label values tm8676   tm8676l;
label define tm8676l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Check Item TM8710"
;
label values tm8678   tm8678l;
label define tm8678l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8680   tm8680l;
label define tm8680l 
	0           "Not applicable"                
	1           "Locate the other parent"       
;
label values tm8682   tm8682l;
label define tm8682l 
	0           "Not applicable"                
	1           "Establish paternity"           
;
label values tm8684   tm8684l;
label define tm8684l 
	0           "Not applicable"                
	1           "Establish support obligation"  
;
label values tm8686   tm8686l;
label define tm8686l 
	0           "Not applicable"                
	1           "Establish medical support"     
;
label values tm8688   tm8688l;
label define tm8688l 
	0           "Not applicable"                
	1           "Enforce support order"         
;
label values tm8690   tm8690l;
label define tm8690l 
	0           "Not applicable"                
	1           "Modify an order"               
;
label values tm8692   tm8692l;
label define tm8692l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8694   tm8694l;
label define tm8694l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to Check Item TM8710"
;
label values tm8696   tm8696l;
label define tm8696l 
	0           "Not applicable"                
	1           "Locate the other parent"       
;
label values tm8698   tm8698l;
label define tm8698l 
	0           "Not applicable"                
	1           "Establish paternity"           
;
label values tm8700   tm8700l;
label define tm8700l 
	0           "Not applicable"                
	1           "Establish support obligation"  
;
label values tm8702   tm8702l;
label define tm8702l 
	0           "Not applicable"                
	1           "Establish medical support"     
;
label values tm8704   tm8704l;
label define tm8704l 
	0           "Not applicable"                
	1           "Enforce support order"         
;
label values tm8706   tm8706l;
label define tm8706l 
	0           "Not applicable"                
	1           "Modify an order"               
;
label values tm8708   tm8708l;
label define tm8708l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8710   tm8710l;
label define tm8710l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8910"           
;
label values tm8712   tm8712l;
label define tm8712l 
	0           "Not applicable"                
	1           "Male - Skip to Check Item TM8866"
	2           "Female"                        
;
label values tm8714   tm8714l;
label define tm8714l 
	0           "Not applicable"                
	1           "Never married"                 
	2           "All others - Skip to Check"    
;
label values tm8715   tm8715l;
label define tm8715l 
	0           "Not applicalbe"                
;
label values tm8716   tm8716l;
label define tm8716l 
	0           "Not applicalbe"                
;
label values tm8717   tm8717l;
label define tm8717l 
	0           "Not applicalbe"                
;
label values tm8718   tm8718l;
label define tm8718l 
	0           "Not applicalbe"                
;
label values tm8719   tm8719l;
label define tm8719l 
	0           "Not applicalbe"                
;
label values tm8720   tm8720l;
label define tm8720l 
	0           "Not applicalbe"                
;
label values tm8721   tm8721l;
label define tm8721l 
	0           "Not applicalbe"                
;
label values tm8722   tm8722l;
label define tm8722l 
	0           "Not applicalbe"                
;
label values tm8723   tm8723l;
label define tm8723l 
	0           "Not applicable"                
;
label values tm8724   tm8724l;
label define tm8724l 
	0           "Not applicable"                
;
label values tm8725   tm8725l;
label define tm8725l 
	0           "Not applicable"                
;
label values tm8726   tm8726l;
label define tm8726l 
	0           "Not applicable"                
;
label values tm8727   tm8727l;
label define tm8727l 
	0           "Not applicable"                
;
label values tm8728   tm8728l;
label define tm8728l 
	0           "Not applicable"                
;
label values tm8729   tm8729l;
label define tm8729l 
	0           "Not applicable"                
;
label values tm8730   tm8730l;
label define tm8730l 
	0           "Not applicable"                
;
label values tm8731   tm8731l;
label define tm8731l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8732   tm8732l;
label define tm8732l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8733   tm8733l;
label define tm8733l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8734   tm8734l;
label define tm8734l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8735   tm8735l;
label define tm8735l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8736   tm8736l;
label define tm8736l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8737   tm8737l;
label define tm8737l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8738   tm8738l;
label define tm8738l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8739   tm8739l;
label define tm8739l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8740   tm8740l;
label define tm8740l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8741   tm8741l;
label define tm8741l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8742   tm8742l;
label define tm8742l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8743   tm8743l;
label define tm8743l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8744   tm8744l;
label define tm8744l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8745   tm8745l;
label define tm8745l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8746   tm8746l;
label define tm8746l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8747   tm8747l;
label define tm8747l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8748   tm8748l;
label define tm8748l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8749   tm8749l;
label define tm8749l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8750   tm8750l;
label define tm8750l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8751   tm8751l;
label define tm8751l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8752   tm8752l;
label define tm8752l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8753   tm8753l;
label define tm8753l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8754   tm8754l;
label define tm8754l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8755   tm8755l;
label define tm8755l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8756   tm8756l;
label define tm8756l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8757   tm8757l;
label define tm8757l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8758   tm8758l;
label define tm8758l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8759   tm8759l;
label define tm8759l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8760   tm8760l;
label define tm8760l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8761   tm8761l;
label define tm8761l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8762   tm8762l;
label define tm8762l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8763   tm8763l;
label define tm8763l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8764   tm8764l;
label define tm8764l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8765   tm8765l;
label define tm8765l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8766   tm8766l;
label define tm8766l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8767   tm8767l;
label define tm8767l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8768   tm8768l;
label define tm8768l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8769   tm8769l;
label define tm8769l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8770   tm8770l;
label define tm8770l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8771   tm8771l;
label define tm8771l 
	0           "Not applicable"                
	1           "Yes - ask TM8731 through TM8763"
	2           "No - Skip to TM8864"           
;
label values tm8772   tm8772l;
label define tm8772l 
	0           "Not applicable"                
	1           "Yes - ask TM8732 through TM8763"
	2           "No - Skip to TM8864"           
;
label values tm8773   tm8773l;
label define tm8773l 
	0           "Not applicable"                
	1           "Yes - ask TM8733 through TM8763"
	2           "No - Skip to TM8864"           
;
label values tm8774   tm8774l;
label define tm8774l 
	0           "Not applicable"                
	1           "Yes - ask TM8734 through TM8763"
	2           "No - Skip to TM8864"           
;
label values tm8775   tm8775l;
label define tm8775l 
	0           "Not applicable"                
	1           "Yes - ask TM8735 through TM8763"
	2           "No - Skip to TM8864"           
;
label values tm8776   tm8776l;
label define tm8776l 
	0           "Not applicable"                
	1           "Yes - ask TM8736 through TM8763"
	2           "No - Skip to TM8864"           
;
label values tm8777   tm8777l;
label define tm8777l 
	0           "Not applicable"                
	1           "Yes - ask TM8737 through TM8763"
	2           "No - Skip to TM8864"           
;
label values tm8779   tm8779l;
label define tm8779l 
	0           "Not applicalbe"                
;
label values tm8780   tm8780l;
label define tm8780l 
	0           "Not applicalbe"                
;
label values tm8781   tm8781l;
label define tm8781l 
	0           "Not applicalbe"                
;
label values tm8782   tm8782l;
label define tm8782l 
	0           "Not applicalbe"                
;
label values tm8783   tm8783l;
label define tm8783l 
	0           "Not applicalbe"                
;
label values tm8784   tm8784l;
label define tm8784l 
	0           "Not applicalbe"                
;
label values tm8785   tm8785l;
label define tm8785l 
	0           "Not applicalbe"                
;
label values tm8786   tm8786l;
label define tm8786l 
	0           "Not applicalbe"                
;
label values tm8787   tm8787l;
label define tm8787l 
	0           "Not applicable"                
;
label values tm8788   tm8788l;
label define tm8788l 
	0           "Not applicable"                
;
label values tm8789   tm8789l;
label define tm8789l 
	0           "Not applicable"                
;
label values tm8790   tm8790l;
label define tm8790l 
	0           "Not applicable"                
;
label values tm8791   tm8791l;
label define tm8791l 
	0           "Not applicable"                
;
label values tm8792   tm8792l;
label define tm8792l 
	0           "Not applicable"                
;
label values tm8793   tm8793l;
label define tm8793l 
	0           "Not applicable"                
;
label values tm8794   tm8794l;
label define tm8794l 
	0           "Not applicable"                
;
label values tm8795   tm8795l;
label define tm8795l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8805 for this"  
;
label values tm8796   tm8796l;
label define tm8796l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - Skip to TM8805 for this"  
;
label values tm8797   tm8797l;
label define tm8797l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - Skip to TM8805 for child" 
;
label values tm8798   tm8798l;
label define tm8798l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - Skip to TM8805 for this"  
;
label values tm8799   tm8799l;
label define tm8799l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - Skip to TM8805 for this"  
;
label values tm8800   tm8800l;
label define tm8800l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - Skip to TM8805 for this"  
;
label values tm8801   tm8801l;
label define tm8801l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - Skip to TM8805 for this"  
;
label values tm8802   tm8802l;
label define tm8802l 
	0           "Not applicable"                
	1           "Yes -  skip to Check Item T79" 
	2           "No - Skip to TM8805 for child" 
;
label values tm8803   tm8803l;
label define tm8803l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -  ask TM8869 through TM8907"
;
label values tm8804   tm8804l;
label define tm8804l 
	0           "Not applicable"                
	1           "Yes - ask TM8869 through TM8907"
	2           "No - go to 7a for the next child"
;
label values tm8805   tm8805l;
label define tm8805l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8806   tm8806l;
label define tm8806l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8807   tm8807l;
label define tm8807l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8808   tm8808l;
label define tm8808l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8809   tm8809l;
label define tm8809l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8810   tm8810l;
label define tm8810l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8811   tm8811l;
label define tm8811l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8812   tm8812l;
label define tm8812l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8813   tm8813l;
label define tm8813l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8814   tm8814l;
label define tm8814l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8815   tm8815l;
label define tm8815l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8816   tm8816l;
label define tm8816l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8817   tm8817l;
label define tm8817l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8818   tm8818l;
label define tm8818l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8819   tm8819l;
label define tm8819l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8820   tm8820l;
label define tm8820l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8821   tm8821l;
label define tm8821l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8822   tm8822l;
label define tm8822l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8823   tm8823l;
label define tm8823l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8824   tm8824l;
label define tm8824l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8825   tm8825l;
label define tm8825l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8826   tm8826l;
label define tm8826l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8827   tm8827l;
label define tm8827l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8828   tm8828l;
label define tm8828l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8829   tm8829l;
label define tm8829l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8830   tm8830l;
label define tm8830l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8831   tm8831l;
label define tm8831l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8832   tm8832l;
label define tm8832l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8833   tm8833l;
label define tm8833l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8834   tm8834l;
label define tm8834l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8835   tm8835l;
label define tm8835l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8836   tm8836l;
label define tm8836l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8837   tm8837l;
label define tm8837l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8838   tm8838l;
label define tm8838l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8839   tm8839l;
label define tm8839l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8840   tm8840l;
label define tm8840l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8841   tm8841l;
label define tm8841l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8842   tm8842l;
label define tm8842l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8843   tm8843l;
label define tm8843l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8844   tm8844l;
label define tm8844l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Don't know"                    
;
label values tm8845   tm8845l;
label define tm8845l 
	0           "Not applicable"                
	1           "Yes - go to TM8796 for next"   
	2           "No - Skip to TM8869"           
;
label values tm8846   tm8846l;
label define tm8846l 
	0           "Not applicable"                
	1           "Yes - go to TM8797 for next"   
	2           "No - Skip to TM8869"           
;
label values tm8847   tm8847l;
label define tm8847l 
	0           "Not applicable"                
	1           "Yes - go to TM8798 for next"   
	2           "No - Skip to TM8869"           
;
label values tm8848   tm8848l;
label define tm8848l 
	0           "Not applicable"                
	1           "Yes - go to TM8799 for next"   
	2           "No - Skip to TM8869"           
;
label values tm8849   tm8849l;
label define tm8849l 
	0           "Not applicable"                
	1           "Yes - go to TM8800 for next"   
	2           "No - Skip to TM8869"           
;
label values tm8850   tm8850l;
label define tm8850l 
	0           "Not applicable"                
	1           "Yes - go to TM8801 for next"   
	2           "No - Skip to TM8869"           
;
label values tm8851   tm8851l;
label define tm8851l 
	0           "Not applicable"                
	1           "Yes - go to TM8802 for next"   
	2           "No - Skip to TM8869"           
;
label values tm8853   tm8853l;
label define tm8853l 
	0           "Not applicable"                
	1           "Yes - Skip to Check"           
	2           "No - Skip to TM8862"           
;
label values tm8854   tm8854l;
label define tm8854l 
	0           "Not applicable"                
	1           "Yes - Skip to Check"           
	2           "No - Skip to TM8862"           
;
label values tm8855   tm8855l;
label define tm8855l 
	0           "Not applicable"                
	1           "Yes - Skip to Check"           
	2           "No - Skip to TM8862"           
;
label values tm8856   tm8856l;
label define tm8856l 
	0           "Not applicable"                
	1           "Yes - Skip to Check"           
	2           "No - Skip to TM8862"           
;
label values tm8857   tm8857l;
label define tm8857l 
	0           "Not applicable"                
	1           "Yes - Skip to Check"           
	2           "No - Skip to TM8862"           
;
label values tm8858   tm8858l;
label define tm8858l 
	0           "Not applicable"                
	1           "Yes - Skip to Check"           
	2           "No - Skip to TM8862"           
;
label values tm8859   tm8859l;
label define tm8859l 
	0           "Not applicable"                
	1           "Yes - Skip to Check"           
	2           "No - Skip to TM8862"           
;
label values tm8862   tm8862l;
label define tm8862l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8864   tm8864l;
label define tm8864l 
	0           "Not applicable"                
	1           "Yes - ask TM8715-TM8722 for"   
	2           "No - ask TM8715-TM8722 for first"
;
label values tm8866   tm8866l;
label define tm8866l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - ask TM8715-TM8722 for child"
;
label values tm8868   tm8868l;
label define tm8868l 
	0           "Not applicable"                
	1           "Yes - ask TM8715-TM8722 for"   
	2           "No - ask TM8715-TM8722 for"    
;
label values tm8869   tm8869l;
label define tm8869l 
	0           "Not applicalbe"                
;
label values tm8870   tm8870l;
label define tm8870l 
	0           "Not applicalbe"                
;
label values tm8871   tm8871l;
label define tm8871l 
	0           "Not applicable"                
	1           "Legal paternity not established"
;
label values tm8872   tm8872l;
label define tm8872l 
	0           "Not applicable"                
	1           "Legal paternity not established"
;
label values tm8873   tm8873l;
label define tm8873l 
	0           "Not applicable"                
	1           "Unable to locate parent"       
;
label values tm8874   tm8874l;
label define tm8874l 
	0           "Not applicable"                
	1           "Unable to locate parent"       
;
label values tm8875   tm8875l;
label define tm8875l 
	0           "Not applicable"                
	1           "Other parent unable to pay"    
;
label values tm8876   tm8876l;
label define tm8876l 
	0           "Not applicable"                
	1           "Other parent unable to pay"    
;
label values tm8877   tm8877l;
label define tm8877l 
	0           "Not applicable"                
	1           "Final agreement pending"       
;
label values tm8878   tm8878l;
label define tm8878l 
	0           "Not applicable"                
	1           "Final agreement pending"       
;
label values tm8879   tm8879l;
label define tm8879l 
	0           "Not applicable"                
	1           "Accepted property or cash"     
;
label values tm8880   tm8880l;
label define tm8880l 
	0           "Not applicable"                
	1           "Accepted property or cash"     
;
label values tm8881   tm8881l;
label define tm8881l 
	0           "Not applicable"                
	1           "Do not want child support"     
;
label values tm8882   tm8882l;
label define tm8882l 
	0           "Not applicable"                
	1           "Do not want child support"     
;
label values tm8883   tm8883l;
label define tm8883l 
	0           "Not applicable"                
	1           "Did not pursue award"          
;
label values tm8884   tm8884l;
label define tm8884l 
	0           "Not applicable"                
	1           "Did not pursue award"          
;
label values tm8885   tm8885l;
label define tm8885l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8886   tm8886l;
label define tm8886l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8887   tm8887l;
label define tm8887l 
	0           "Not applicable"                
	1           "Same county/city"              
;
label values tm8888   tm8888l;
label define tm8888l 
	0           "Not applicable"                
	1           "Same county/city"              
;
label values tm8889   tm8889l;
label define tm8889l 
	0           "Not applicable"                
	1           "Same state (different"         
;
label values tm8890   tm8890l;
label define tm8890l 
	0           "Not applicable"                
	1           "Same state (different"         
;
label values tm8891   tm8891l;
label define tm8891l 
	0           "Not applicable"                
	1           "Different state"               
;
label values tm8892   tm8892l;
label define tm8892l 
	0           "Not applicable"                
	1           "Different state"               
;
label values tm8893   tm8893l;
label define tm8893l 
	0           "Not applicable"                
	1           "Other parent deceased - Skip"  
;
label values tm8894   tm8894l;
label define tm8894l 
	0           "Not applicable"                
	1           "Other parent deceased - Skip"  
;
label values tm8895   tm8895l;
label define tm8895l 
	0           "Not applicable"                
	1           "Other"                         
	-1          "Unknown"                       
;
label values tm8896   tm8896l;
label define tm8896l 
	0           "Not applicable"                
	1           "Other"                         
	-1          "Unknown what is the total amount"
;
label values tm8897   tm8897l;
label define tm8897l 
	0           "Not applicable"                
;
label values tm8898   tm8898l;
label define tm8898l 
	0           "Not applicable"                
;
label values tm8900   tm8900l;
label define tm8900l 
	0           "Not applicable"                
;
label values tm8901   tm8901l;
label define tm8901l 
	0           "Not applicable"                
;
label values tm8902   tm8902l;
label define tm8902l 
	0           "Not applicable"                
;
label values tm8903   tm8903l;
label define tm8903l 
	0           "Not applicable"                
;
label values tm8904   tm8904l;
label define tm8904l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8905   tm8905l;
label define tm8905l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8906   tm8906l;
label define tm8906l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8907   tm8907l;
label define tm8907l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8908   tm8908l;
label define tm8908l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8910"           
;
label values tm8909   tm8909l;
label define tm8909l 
	0           "Not applicable"                
	13200       "Dollars"                       
	-1          "Don't know"                    
;
label values tm8910   tm8910l;
label define tm8910l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values im8401   im8401l;
label define im8401l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8403   im8403l;
label define im8403l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8439   im8439l;
label define im8439l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8441   im8441l;
label define im8441l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8442   im8442l;
label define im8442l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8443   im8443l;
label define im8443l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8448   im8448l;
label define im8448l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8449   im8449l;
label define im8449l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8452   im8452l;
label define im8452l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8457   im8457l;
label define im8457l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8458   im8458l;
label define im8458l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8459   im8459l;
label define im8459l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8460   im8460l;
label define im8460l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8461   im8461l;
label define im8461l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8462   im8462l;
label define im8462l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8464   im8464l;
label define im8464l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8465   im8465l;
label define im8465l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8466   im8466l;
label define im8466l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8467   im8467l;
label define im8467l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8473   im8473l;
label define im8473l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8474   im8474l;
label define im8474l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8476   im8476l;
label define im8476l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8477   im8477l;
label define im8477l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8482   im8482l;
label define im8482l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8484   im8484l;
label define im8484l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8485   im8485l;
label define im8485l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8486   im8486l;
label define im8486l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8487   im8487l;
label define im8487l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8492   im8492l;
label define im8492l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8493   im8493l;
label define im8493l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8494   im8494l;
label define im8494l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8496   im8496l;
label define im8496l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8501   im8501l;
label define im8501l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8502   im8502l;
label define im8502l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8503   im8503l;
label define im8503l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8504   im8504l;
label define im8504l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8506   im8506l;
label define im8506l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8507   im8507l;
label define im8507l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8508   im8508l;
label define im8508l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8509   im8509l;
label define im8509l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8515   im8515l;
label define im8515l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8516   im8516l;
label define im8516l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8518   im8518l;
label define im8518l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8519   im8519l;
label define im8519l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8542   im8542l;
label define im8542l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8543   im8543l;
label define im8543l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8544   im8544l;
label define im8544l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8545   im8545l;
label define im8545l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8546   im8546l;
label define im8546l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8547   im8547l;
label define im8547l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8548   im8548l;
label define im8548l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8549   im8549l;
label define im8549l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8550   im8550l;
label define im8550l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8551   im8551l;
label define im8551l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8552   im8552l;
label define im8552l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8553   im8553l;
label define im8553l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8554   im8554l;
label define im8554l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8555   im8555l;
label define im8555l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8556   im8556l;
label define im8556l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8557   im8557l;
label define im8557l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8558   im8558l;
label define im8558l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8559   im8559l;
label define im8559l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8560   im8560l;
label define im8560l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8561   im8561l;
label define im8561l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8562   im8562l;
label define im8562l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8563   im8563l;
label define im8563l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8564   im8564l;
label define im8564l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8565   im8565l;
label define im8565l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8566   im8566l;
label define im8566l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8567   im8567l;
label define im8567l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8568   im8568l;
label define im8568l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8569   im8569l;
label define im8569l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8570   im8570l;
label define im8570l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8571   im8571l;
label define im8571l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8572   im8572l;
label define im8572l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8573   im8573l;
label define im8573l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8574   im8574l;
label define im8574l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8575   im8575l;
label define im8575l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8576   im8576l;
label define im8576l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8577   im8577l;
label define im8577l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8578   im8578l;
label define im8578l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8579   im8579l;
label define im8579l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8580   im8580l;
label define im8580l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8581   im8581l;
label define im8581l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8607   im8607l;
label define im8607l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8608   im8608l;
label define im8608l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8609   im8609l;
label define im8609l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8610   im8610l;
label define im8610l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8611   im8611l;
label define im8611l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8612   im8612l;
label define im8612l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8613   im8613l;
label define im8613l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8614   im8614l;
label define im8614l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8615   im8615l;
label define im8615l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8616   im8616l;
label define im8616l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8617   im8617l;
label define im8617l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8618   im8618l;
label define im8618l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8619   im8619l;
label define im8619l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8620   im8620l;
label define im8620l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8621   im8621l;
label define im8621l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8622   im8622l;
label define im8622l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8623   im8623l;
label define im8623l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8624   im8624l;
label define im8624l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8625   im8625l;
label define im8625l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8626   im8626l;
label define im8626l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8627   im8627l;
label define im8627l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8628   im8628l;
label define im8628l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8629   im8629l;
label define im8629l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8630   im8630l;
label define im8630l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8631   im8631l;
label define im8631l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8632   im8632l;
label define im8632l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8633   im8633l;
label define im8633l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8634   im8634l;
label define im8634l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8635   im8635l;
label define im8635l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8636   im8636l;
label define im8636l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8637   im8637l;
label define im8637l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8638   im8638l;
label define im8638l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8639   im8639l;
label define im8639l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8640   im8640l;
label define im8640l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8641   im8641l;
label define im8641l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8642   im8642l;
label define im8642l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8643   im8643l;
label define im8643l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8644   im8644l;
label define im8644l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8645   im8645l;
label define im8645l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8646   im8646l;
label define im8646l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8656   im8656l;
label define im8656l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8664   im8664l;
label define im8664l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8666   im8666l;
label define im8666l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8667   im8667l;
label define im8667l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8669   im8669l;
label define im8669l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8674   im8674l;
label define im8674l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8676   im8676l;
label define im8676l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8678   im8678l;
label define im8678l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8680   im8680l;
label define im8680l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8694   im8694l;
label define im8694l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8696   im8696l;
label define im8696l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8731   im8731l;
label define im8731l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8732   im8732l;
label define im8732l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8733   im8733l;
label define im8733l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8734   im8734l;
label define im8734l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8735   im8735l;
label define im8735l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8736   im8736l;
label define im8736l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8737   im8737l;
label define im8737l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8738   im8738l;
label define im8738l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8739   im8739l;
label define im8739l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8740   im8740l;
label define im8740l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8741   im8741l;
label define im8741l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8742   im8742l;
label define im8742l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8743   im8743l;
label define im8743l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8744   im8744l;
label define im8744l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8745   im8745l;
label define im8745l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8746   im8746l;
label define im8746l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8747   im8747l;
label define im8747l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8748   im8748l;
label define im8748l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8749   im8749l;
label define im8749l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8750   im8750l;
label define im8750l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8751   im8751l;
label define im8751l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8752   im8752l;
label define im8752l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8753   im8753l;
label define im8753l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8754   im8754l;
label define im8754l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8755   im8755l;
label define im8755l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8756   im8756l;
label define im8756l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8757   im8757l;
label define im8757l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8758   im8758l;
label define im8758l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8759   im8759l;
label define im8759l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8760   im8760l;
label define im8760l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8761   im8761l;
label define im8761l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8762   im8762l;
label define im8762l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8763   im8763l;
label define im8763l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8764   im8764l;
label define im8764l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8765   im8765l;
label define im8765l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8766   im8766l;
label define im8766l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8767   im8767l;
label define im8767l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8768   im8768l;
label define im8768l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8769   im8769l;
label define im8769l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8770   im8770l;
label define im8770l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8795   im8795l;
label define im8795l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8796   im8796l;
label define im8796l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8797   im8797l;
label define im8797l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8798   im8798l;
label define im8798l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8799   im8799l;
label define im8799l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8800   im8800l;
label define im8800l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8801   im8801l;
label define im8801l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8802   im8802l;
label define im8802l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8804   im8804l;
label define im8804l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8805   im8805l;
label define im8805l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8806   im8806l;
label define im8806l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8807   im8807l;
label define im8807l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8808   im8808l;
label define im8808l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8809   im8809l;
label define im8809l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8810   im8810l;
label define im8810l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8811   im8811l;
label define im8811l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8812   im8812l;
label define im8812l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8813   im8813l;
label define im8813l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8814   im8814l;
label define im8814l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8815   im8815l;
label define im8815l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8816   im8816l;
label define im8816l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8817   im8817l;
label define im8817l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8818   im8818l;
label define im8818l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8819   im8819l;
label define im8819l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8820   im8820l;
label define im8820l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8821   im8821l;
label define im8821l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8822   im8822l;
label define im8822l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8823   im8823l;
label define im8823l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8824   im8824l;
label define im8824l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8825   im8825l;
label define im8825l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8826   im8826l;
label define im8826l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8827   im8827l;
label define im8827l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8828   im8828l;
label define im8828l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8829   im8829l;
label define im8829l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8830   im8830l;
label define im8830l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8831   im8831l;
label define im8831l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8832   im8832l;
label define im8832l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8833   im8833l;
label define im8833l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8834   im8834l;
label define im8834l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8835   im8835l;
label define im8835l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8836   im8836l;
label define im8836l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8837   im8837l;
label define im8837l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8838   im8838l;
label define im8838l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8839   im8839l;
label define im8839l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8840   im8840l;
label define im8840l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8841   im8841l;
label define im8841l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8842   im8842l;
label define im8842l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8843   im8843l;
label define im8843l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8844   im8844l;
label define im8844l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8868   im8868l;
label define im8868l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8871   im8871l;
label define im8871l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8872   im8872l;
label define im8872l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8887   im8887l;
label define im8887l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8888   im8888l;
label define im8888l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8897   im8897l;
label define im8897l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8898   im8898l;
label define im8898l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8908   im8908l;
label define im8908l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8909   im8909l;
label define im8909l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values im8910   im8910l;
label define im8910l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm9100   tm9100l;
label define tm9100l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9148"           
	-1          "Don't know - Skip to TM9148"   
;
label values tm9102   tm9102l;
label define tm9102l 
	0           "Not applicable"                
	1           "Regular"                       
	2           "Lump-sum"                      
	3           "Both"                          
;
label values tm9104   tm9104l;
label define tm9104l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9106   tm9106l;
label define tm9106l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9108   tm9108l;
label define tm9108l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9146"           
;
label values tm9110   tm9110l;
label define tm9110l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9112   tm9112l;
label define tm9112l 
	0           "Not applicable"                
	1           "Voluntary written agreement"   
	2           "Court-ordered agreement"       
	3           "Other type of written agreement"
	4           "Non-written agreement"         
;
label values tm9114   tm9114l;
label define tm9114l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9116   tm9116l;
label define tm9116l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9122"           
	-1          "Don't know - Skip to TM9122"   
;
label values tm9118   tm9118l;
label define tm9118l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9120   tm9120l;
label define tm9120l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9122   tm9122l;
label define tm9122l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9124   tm9124l;
label define tm9124l 
	0           "Not applicable"                
	15600       "Amount paid"                   
	-1          "Don't know"                    
;
label values tm9126   tm9126l;
label define tm9126l 
	0           "Not applicable"                
	1           "Through employment related wage"
	2           "Directly to the other parent"  
	3           "Directly to the court"         
	4           "Directly to a child support"   
	5           "Other"                         
	-1          "Don't know"                    
;
label values tm9128   tm9128l;
label define tm9128l 
	0           "Not applicable"                
	1           "Non-custodial parent to provide"
;
label values tm9130   tm9130l;
label define tm9130l 
	0           "Not applicable"                
	1           "Custodial parent to provide"   
;
label values tm9132   tm9132l;
label define tm9132l 
	0           "Not applicable"                
	1           "Non-custodial parent to pay"   
;
label values tm9134   tm9134l;
label define tm9134l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm9136   tm9136l;
label define tm9136l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm9138   tm9138l;
label define tm9138l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm9140   tm9140l;
label define tm9140l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9042"           
;
label values tm9142   tm9142l;
label define tm9142l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9144   tm9144l;
label define tm9144l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to part e"           
;
label values tm9146   tm9146l;
label define tm9146l 
	0           "Not applicable"                
	18000       "The amount of child"           
	-1          "Don't know"                    
;
label values tm9148   tm9148l;
label define tm9148l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9200 - part g"  
;
label values tm9150   tm9150l;
label define tm9150l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9152   tm9152l;
label define tm9152l 
	0           "Not applicable"                
	1           "Parent"                        
	2           "Spouse"                        
	3           "Ex-spouse"                     
	4           "Child under 21"                
	5           "Child 21 or older"             
	6           "Other relative"                
	7           "Not related"                   
;
label values tm9153   tm9153l;
label define tm9153l 
	0           "Not applicable"                
	1           "Parent"                        
	2           "Spouse"                        
	3           "Ex-spouse"                     
	4           "Child under 21"                
	5           "Child 21 or older"             
	6           "Other relative"                
	7           "Not related"                   
;
label values tm9154   tm9154l;
label define tm9154l 
	0           "Not applicable"                
	1           "Private home or apt"           
	2           "Nursing home"                  
	3           "Someplace else"                
;
label values tm9155   tm9155l;
label define tm9155l 
	0           "Not applicable"                
	1           "Private home or apt"           
	2           "Nursing home"                  
	3           "Someplace else"                
;
label values tm9156   tm9156l;
label define tm9156l 
	0           "Not applicable"                
	18000       "Per week"                      
;
label values tm9157   tm9157l;
label define tm9157l 
	0           "Not applicable"                
	10000       "Per week"                      
;
label values tm9158   tm9158l;
label define tm9158l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM9200"           
;
label values tm9159   tm9159l;
label define tm9159l 
	0           "Not applicable"                
	50000       "Amount"                        
	-1          "Dk"                            
;
label values imp9100  imp9100l;
label define imp9100l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9102  imp9102l;
label define imp9102l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9104  imp9104l;
label define imp9104l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9106  imp9106l;
label define imp9106l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9108  imp9108l;
label define imp9108l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9110  imp9110l;
label define imp9110l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9112  imp9112l;
label define imp9112l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9114  imp9114l;
label define imp9114l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9116  imp9116l;
label define imp9116l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9118  imp9118l;
label define imp9118l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9120  imp9120l;
label define imp9120l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9122  imp9122l;
label define imp9122l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9124  imp9124l;
label define imp9124l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9126  imp9126l;
label define imp9126l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp28_38 imp28_3y;
label define imp28_3y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9140  imp9140l;
label define imp9140l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9142  imp9142l;
label define imp9142l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9144  imp9144l;
label define imp9144l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9146  imp9146l;
label define imp9146l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9148  imp9148l;
label define imp9148l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9150  imp9150l;
label define imp9150l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9152  imp9152l;
label define imp9152l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9153  imp9153l;
label define imp9153l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9154  imp9154l;
label define imp9154l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9155  imp9155l;
label define imp9155l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9156  imp9156l;
label define imp9156l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9157  imp9157l;
label define imp9157l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9158  imp9158l;
label define imp9158l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp9159  imp9159l;
label define imp9159l
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
