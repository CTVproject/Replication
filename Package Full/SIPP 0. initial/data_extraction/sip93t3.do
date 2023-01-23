log using sip93t3, text replace
set mem 1000m
*This program reads the 1993 SIPP Wave 3 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Mar 18 11:53:43 EST 2004
*Please report errors to jroth@nber.org
*run with do sip93t3
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1993\sip93t3.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip93t3"

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
	28          "Merged hhlds across panels   SPP"
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
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
	21          "Afro-American (Black or Negro)"
	30          "Another group not listed"      
	39          "Don't know"                    
;
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to Check Item TM8100"
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
	1           "Monday through Friday"         
;
label values tm8014   tm8014l;
label define tm8014l 
	0           "Not applicable"                
	1           "Monday through Friday"         
;
label values tm8016   tm8016l;
label define tm8016l 
	0           "Not applicable"                
	1           "Sunday"                        
;
label values tm8018   tm8018l;
label define tm8018l 
	0           "Not applicable"                
	1           "Sunday"                        
;
label values tm8020   tm8020l;
label define tm8020l 
	0           "Not applicable"                
	1           "Monday"                        
;
label values tm8022   tm8022l;
label define tm8022l 
	0           "Not applicable"                
	1           "Monday"                        
;
label values tm8024   tm8024l;
label define tm8024l 
	0           "Not applicable"                
	1           "Tuesday"                       
;
label values tm8026   tm8026l;
label define tm8026l 
	0           "Not applicable"                
	1           "Tuesday"                       
;
label values tm8028   tm8028l;
label define tm8028l 
	0           "Not applicable"                
	1           "Wednesday"                     
;
label values tm8030   tm8030l;
label define tm8030l 
	0           "Not applicable"                
	1           "Wednesday"                     
;
label values tm8032   tm8032l;
label define tm8032l 
	0           "Not applicable"                
	1           "Thursday"                      
;
label values tm8034   tm8034l;
label define tm8034l 
	0           "Not applicable"                
	1           "Thursday"                      
;
label values tm8036   tm8036l;
label define tm8036l 
	0           "Not applicable"                
	1           "Friday"                        
;
label values tm8038   tm8038l;
label define tm8038l 
	0           "Not applicable"                
	1           "Friday"                        
;
label values tm8040   tm8040l;
label define tm8040l 
	0           "Not applicable"                
	1           "Saturday"                      
;
label values tm8042   tm8042l;
label define tm8042l 
	0           "Not applicable"                
	1           "Saturday"                      
;
label values tm8044   tm8044l;
label define tm8044l 
	-5          "All days (First employer)"     
	0           "Not applicable"                
;
label values tm8046   tm8046l;
label define tm8046l 
	-5          "All days (Second employer)"    
	0           "Not applicable"                
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
label values tm8064   tm8064l;
label define tm8064l 
	0           "Not applicable"                
	1           "Regular daytime schedule"      
	2           "Regular evening shift"         
	3           "Regular night shift"           
	4           "Rotating shift (one that"      
	5           "Split shift (one consisting of"
	6           "Irregular schedule (one that"  
	7           "Other"                         
;
label values tm8066   tm8066l;
label define tm8066l 
	0           "Not applicable"                
	1           "Regular daytime schedule"      
	2           "Regular evening shift"         
	3           "Regular night shift"           
	4           "Rotating shift (one that"      
	5           "Split shift (one consisting of"
	6           "Irregular schedule (one that"  
	7           "Other"                         
;
label values tm8068   tm8068l;
label define tm8068l 
	0           "Not applicable"                
	1           "Better child care arrangements"
	2           "Better pay"                    
	3           "Better arrangements for care"  
	4           "Allows time for school"        
	5           "Other voluntary reasons"       
	6           "Could not get any other job"   
	7           "Requirements of the job"       
	8           "Other involuntary reasons"     
;
label values tm8070   tm8070l;
label define tm8070l 
	0           "Not applicable"                
	1           "Better child care arrangements"
	2           "Better pay"                    
	3           "Better arrangements for care"  
	4           "Allows time for school"        
	5           "Other voluntary reasons"       
	6           "Could not get any other job"   
	7           "Requirement of the job"        
	8           "Other involuntary reasons"     
;
label values tm8072   tm8072l;
label define tm8072l 
	0           "Not applicable"                
	1           "Yes - ask Items 1B through"    
	2           "No - go to Check Item T2"      
;
label values tm8100   tm8100l;
label define tm8100l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip Item TM8400"         
;
label values tm8101   tm8101l;
label define tm8101l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8105"           
;
label values tm8102   tm8102l;
label define tm8102l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8105"           
;
label values tm8103   tm8103l;
label define tm8103l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item"       
;
label values tm8104   tm8104l;
label define tm8104l 
	0           "Not applicable"                
;
label values tm8105   tm8105l;
label define tm8105l 
	0           "Not applicable"                
	1           "Yes - skip to check"           
	2           "No"                            
;
label values tm8106   tm8106l;
label define tm8106l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item"       
;
label values tm8107   tm8107l;
label define tm8107l 
	-1          "Hours varied - skip to check"  
	-2          "Don't know - skip to check item"
	-3          "Not enrolled last month"       
	0           "Not applicable"                
;
label values tm8108   tm8108l;
label define tm8108l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8400"           
;
label values tm8109   tm8109l;
label define tm8109l 
	-1          "Hours varied"                  
	-2          "Don't know"                    
	-3          "Did not look for a job last"   
	0           "Not applicable"                
;
label values tm8116   tm8116l;
label define tm8116l 
	0           "Not applicable"                
;
label values tm8118   tm8118l;
label define tm8118l 
	0           "Not applicable"                
;
label values tm8120   tm8120l;
label define tm8120l 
	0           "Not applicable"                
	1           "Child's other parent/"         
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care"       
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "...works at home"              
	12          "...cares for child at work"    
	13          "Child not born as of last"     
	14          "...did not work (not enrolled" 
;
label values tm8122   tm8122l;
label define tm8122l 
	0           "Not applicable"                
	1           "Child's other parent/"         
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care"       
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "...works at home"              
	12          "...cares for child at work"    
	13          "Child not born as of last"     
;
label values tm8124   tm8124l;
label define tm8124l 
	0           "Not applicable"                
	1           "Child's other parent/"         
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care"       
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "...works at home"              
	12          "...cares for child at work"    
	13          "Child not born as of last"     
;
label values tm8126   tm8126l;
label define tm8126l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm8128   tm8128l;
label define tm8128l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm8130   tm8130l;
label define tm8130l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm8132   tm8132l;
label define tm8132l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8158"           
;
label values tm8134   tm8134l;
label define tm8134l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8160"           
;
label values tm8136   tm8136l;
label define tm8136l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8162"           
;
label values tm8138   tm8138l;
label define tm8138l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8158"           
;
label values tm8140   tm8140l;
label define tm8140l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8160"           
;
label values tm8142   tm8142l;
label define tm8142l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8162"           
;
label values tm8144   tm8144l;
label define tm8144l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8152"           
;
label values tm8146   tm8146l;
label define tm8146l 
	0           "Not applicable"                
	1           "Payment for youngest child"    
	2           "Includes another child"        
;
label values tm8148   tm8148l;
label define tm8148l 
	0           "Not applicable"                
	1           "Payment for second youngest"   
	2           "Includes another child"        
;
label values tm8150   tm8150l;
label define tm8150l 
	0           "Not applicable"                
	1           "Payment for third youngest"    
	2           "Includes another child"        
;
label values tm8152   tm8152l;
label define tm8152l 
	0           "Not applicable"                
	140         "140+ amount paid per week"     
	-1          "Don't know"                    
;
label values tm8154   tm8154l;
label define tm8154l 
	0           "Not applicable"                
	140         "140+ amount paid per week"     
	-1          "Don't know"                    
	-2          "Previously recorded for"       
;
label values tm8156   tm8156l;
label define tm8156l 
	140         "140+ amount paid per week"     
	-1          "Don't know"                    
	-2          "Previously recorded for"       
	-3          "Previously recorded for"       
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
;
label values tm8164   tm8164l;
label define tm8164l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next child or"    
;
label values tm8166   tm8166l;
label define tm8166l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to next child or"    
;
label values tm8168   tm8168l;
label define tm8168l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to Check Item T11"   
;
label values tm8170   tm8170l;
label define tm8170l 
	0           "Not applicable"                
	1           "Child's other parent/step-"    
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care"       
	7           "Child in nursery/preschool"    
	8           "Child in organized school-"    
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          ".. .works at home"             
	12          "...cares for child at work"    
;
label values tm8172   tm8172l;
label define tm8172l 
	0           "Not applicable"                
	1           "Child's other parent/step-"    
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care"       
	7           "Child in nursery/preschool"    
	8           "Child in organized school-"    
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "...works at home"              
	12          "...cares for child at work"    
;
label values tm8174   tm8174l;
label define tm8174l 
	0           "Not applicable"                
	1           "Child's other parent/step-"    
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care"       
	7           "Child in nursery/preschool"    
	8           "Child in organized school-"    
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "...works at home"              
	12          "...cares for child at work"    
;
label values tm8176   tm8176l;
label define tm8176l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm8178   tm8178l;
label define tm8178l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm8180   tm8180l;
label define tm8180l 
	0           "Not applicable"                
	1           "Child's home"                  
	2           "Other private home"            
	3           "Other place"                   
;
label values tm8182   tm8182l;
label define tm8182l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8208"           
;
label values tm8184   tm8184l;
label define tm8184l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8210"           
;
label values tm8186   tm8186l;
label define tm8186l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8212"           
;
label values tm8188   tm8188l;
label define tm8188l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8208"           
;
label values tm8190   tm8190l;
label define tm8190l 
	0           "Not applicable"                
	1           "Yes - skip to TM8198"          
	2           "No - skip to TM8210"           
;
label values tm8192   tm8192l;
label define tm8192l 
	0           "Not applicable"                
	1           "Yes - skip to TM8200"          
	2           "No - skip to TM8212"           
;
label values tm8194   tm8194l;
label define tm8194l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8202"           
;
label values tm8196   tm8196l;
label define tm8196l 
	0           "Not applicable"                
	1           "Payment for youngest child"    
	2           "Includes another child"        
;
label values tm8198   tm8198l;
label define tm8198l 
	0           "Not applicable"                
	1           "Payment for second youngest"   
	2           "Includes another child"        
;
label values tm8200   tm8200l;
label define tm8200l 
	0           "Not applicable"                
	1           "Payment for third youngest"    
	2           "Includes another child"        
;
label values tm8202   tm8202l;
label define tm8202l 
	-1          "Don't know"                    
	0           "Not applicable"                
	80          "80+amount paid per week"       
;
label values tm8204   tm8204l;
label define tm8204l 
	-1          "Don't know"                    
	-2          "Previously recorded for"       
	0           "Not applicable"                
;
label values tm8206   tm8206l;
label define tm8206l 
	-3          "Previously recorded for"       
	-2          "Previously recorded for"       
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8208   tm8208l;
label define tm8208l 
	0           "Not applicable"                
;
label values tm8210   tm8210l;
label define tm8210l 
	0           "Not applicable"                
;
label values tm8212   tm8212l;
label define tm8212l 
	0           "Not applicable"                
;
label values tm8214   tm8214l;
label define tm8214l 
	0           "Not applicable"                
	1           "Less than 5 years old"         
	2           "5 or more years old - skip"    
;
label values tm8216   tm8216l;
label define tm8216l 
	0           "Not applicable"                
	2           "5 or more years old - skip"    
;
label values tm8218   tm8218l;
label define tm8218l 
	0           "Not applicable"                
	1           "Less than 5 years"             
	2           "5 or more years old - skip"    
;
label values tm8220   tm8220l;
label define tm8220l 
	0           "Not applicable"                
	1           "Yes - skip to TM8232"          
	2           "No - skip to next child or"    
	3           "Stopped working (attending"    
;
label values tm8222   tm8222l;
label define tm8222l 
	0           "Not applicable"                
	1           "Yes - skip to TM8234"          
	2           "No - skip to next child or"    
	3           "Stopped working (attending"    
;
label values tm8224   tm8224l;
label define tm8224l 
	0           "Not applicable"                
	1           "Yes - skip to TM8236"          
	2           "No - skip to next child or"    
	3           "Stopped working (attending"    
;
label values tm8226   tm8226l;
label define tm8226l 
	0           "Not applicable"                
	1           "Yes - skip to TM8232"          
	2           "No - skip to next child or"    
	3           "Stopped working (attending"    
;
label values tm8228   tm8228l;
label define tm8228l 
	0           "Not applicable"                
	1           "Yes - skip to TM8234"          
	2           "No - skip to next child or"    
	3           "Stopped working (attending"    
;
label values tm8230   tm8230l;
label define tm8230l 
	0           "Not applicable"                
	1           "Yes - skip to TM8236"          
	2           "No - skip to next child or"    
	3           "Stopped working (attending"    
;
label values tm8232   tm8232l;
label define tm8232l 
	0           "Not applicable"                
	99          "Arrangement(s)"                
;
label values tm8234   tm8234l;
label define tm8234l 
	0           "Not applicable"                
	99          "Arrangement(s)"                
;
label values tm8236   tm8236l;
label define tm8236l 
	0           "Not applicable"                
;
label values tm8238   tm8238l;
label define tm8238l 
	0           "Not applicable"                
	1           "Beginning/ending/changes in"   
;
label values tm8240   tm8240l;
label define tm8240l 
	0           "Not applicable"                
	1           "Beginning/ending/changes in"   
;
label values tm8242   tm8242l;
label define tm8242l 
	0           "Not applicable"                
	1           "Beginning/ending/changes in"   
;
label values tm8244   tm8244l;
label define tm8244l 
	0           "Not applicable"                
	1           "Beginning/ending/changes"      
;
label values tm8246   tm8246l;
label define tm8246l 
	0           "Not applicable"                
	1           "Beginning/ending/changes"      
;
label values tm8248   tm8248l;
label define tm8248l 
	0           "Not applicable"                
	1           "Beginning/ending/changes"      
;
label values tm8250   tm8250l;
label define tm8250l 
	0           "Not applicable"                
	1           "Beginning/ending/changes"      
;
label values tm8252   tm8252l;
label define tm8252l 
	0           "Not applicable"                
	1           "Beginning/ending/changes in"   
;
label values tm8254   tm8254l;
label define tm8254l 
	0           "Not applicable"                
	1           "Beginning/ending/changes in"   
;
label values tm8256   tm8256l;
label define tm8256l 
	0           "Not applicable"                
	1           "Cost"                          
;
label values tm8258   tm8258l;
label define tm8258l 
	0           "Not applicable"                
	1           "Cost"                          
;
label values tm8260   tm8260l;
label define tm8260l 
	0           "Not applicable"                
	1           "Cost"                          
;
label values tm8262   tm8262l;
label define tm8262l 
	0           "Not applicable"                
	1           "Availability or hours of"      
;
label values tm8264   tm8264l;
label define tm8264l 
	0           "Not applicable"                
	1           "Availability or hours of"      
;
label values tm8266   tm8266l;
label define tm8266l 
	0           "Not applicable"                
	1           "Availability or hours of"      
;
label values tm8268   tm8268l;
label define tm8268l 
	0           "Not applicable"                
	1           "Reliability of care provider"  
;
label values tm8270   tm8270l;
label define tm8270l 
	0           "Not applicable"                
	1           "Reliability of care provider"  
;
label values tm8272   tm8272l;
label define tm8272l 
	0           "Not applicable"                
	1           "Reliability of care provider"  
;
label values tm8274   tm8274l;
label define tm8274l 
	0           "Not applicable"                
	1           "Quality of care provided"      
;
label values tm8276   tm8276l;
label define tm8276l 
	1           "Quality of care provided"      
;
label values tm8278   tm8278l;
label define tm8278l 
	0           "Not applicable"                
	1           "Quality of care provided"      
;
label values tm8280   tm8280l;
label define tm8280l 
	0           "Not applicable"                
	1           "Location or accessability"     
;
label values tm8282   tm8282l;
label define tm8282l 
	0           "Not applicable"                
	1           "Location or accessability"     
;
label values tm8284   tm8284l;
label define tm8284l 
	0           "Not applicable"                
	1           "Location or accessability"     
;
label values tm8286   tm8286l;
label define tm8286l 
	0           "Not applicable"                
	1           "Found better/less expensive/"  
;
label values tm8288   tm8288l;
label define tm8288l 
	0           "Not applicable"                
	1           "Found better/less expensive/"  
;
label values tm8290   tm8290l;
label define tm8290l 
	0           "Not applicable"                
	1           "Found better/less expensive/"  
;
label values tm8292   tm8292l;
label define tm8292l 
	0           "Not applicable"                
	1           "Never had any regular"         
;
label values tm8294   tm8294l;
label define tm8294l 
	0           "Not applicable"                
	1           "Never had any regular"         
;
label values tm8296   tm8296l;
label define tm8296l 
	0           "Not applicable"                
	1           "Never had any regular"         
;
label values tm8298   tm8298l;
label define tm8298l 
	0           "Not applicable"                
	1           "Child outgrew arrangement"     
;
label values tm8300   tm8300l;
label define tm8300l 
	0           "Not applicable"                
	1           "Child outgrew arrangement"     
;
label values tm8302   tm8302l;
label define tm8302l 
	0           "Not applicable"                
	1           "Child outgrew arrangement"     
;
label values tm8304   tm8304l;
label define tm8304l 
	0           "Not applicable"                
	1           "No longer eligible for"        
;
label values tm8306   tm8306l;
label define tm8306l 
	0           "Not applicable"                
	1           "No longer eligible for"        
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not applicable"                
	1           "No longer eligible for"        
;
label values tm8310   tm8310l;
label define tm8310l 
	0           "Not applicable"                
	1           "Arrangement no longer available"
;
label values tm8312   tm8312l;
label define tm8312l 
	0           "Not applicable"                
	1           "Arrangement no longer available"
;
label values tm8314   tm8314l;
label define tm8314l 
	0           "Not applicable"                
	1           "Arrangement no longer available"
;
label values tm8316   tm8316l;
label define tm8316l 
	0           "Not applicable"                
	1           "Other - skip to next child"    
;
label values tm8318   tm8318l;
label define tm8318l 
	0           "Not applicable"                
	1           "Other - skip to next child"    
;
label values tm8320   tm8320l;
label define tm8320l 
	0           "Not applicable"                
	1           "Other - skip to TM8322"        
;
label values tm8322   tm8322l;
label define tm8322l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8326"           
;
label values tm8324   tm8324l;
label define tm8324l 
	-2          "All costs already recorded for"
	0           "Not applicable"                
;
label values tm8326   tm8326l;
label define tm8326l 
	0           "Not available"                 
	1           "Yes"                           
	2           "No - skip to TM8400"           
;
label values tm8328   tm8328l;
label define tm8328l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes, respondent lost time"     
	2           "Yes, spouse lost time"         
	3           "Both, respondent and spouse"   
	4           "No"                            
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to Part D TM8700"    
;
label values tm8401   tm8401l;
label define tm8401l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8700"           
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not applicable"                
	4           "04+ children"                  
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
label values tm8435   tm8435l;
label define tm8435l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - for each child listed in" 
;
label values tm8436   tm8436l;
label define tm8436l 
	0           "Not applicable"                
	4           "4+ children"                   
;
label values tm8437   tm8437l;
label define tm8437l 
	0           "Not applicable"                
	1           "Yes - skip to 1I"              
	2           "No"                            
;
label values tm8438   tm8438l;
label define tm8438l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to 1I"               
;
label values tm8439   tm8439l;
label define tm8439l 
	0           "Not applicable"                
	1           "Number of agreements"          
	2           "2+ number of agreements"       
;
label values tm8440   tm8440l;
label define tm8440l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8441"           
;
label values tm8441   tm8441l;
label define tm8441l 
	0           "Not applicabple"               
	1           "Voluntary written agreement"   
	2           "Court-ordered agreement."      
	3           "Other type of written agreement"
	4           "Non written (verbal agreement)"
;
label values tm8442   tm8442l;
label define tm8442l 
	-1          "Don't know"                    
	0           "Not applicable"                
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
	99999       "Dollars per year"              
	100000      "100000+ dollars per year"      
;
label values tm8447   tm8447l;
label define tm8447l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "Not appplicable"               
	1           "Yes"                           
	2           "No - skip to TM8456"           
;
label values tm8449   tm8449l;
label define tm8449l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "Not applicable"                
;
label values tm8451   tm8451l;
label define tm8451l 
	0           "Not applicable"                
;
label values tm8452   tm8452l;
label define tm8452l 
	0           "Not applicable"                
;
label values tm8453   tm8453l;
label define tm8453l 
	0           "Not applicable"                
	99999       "Dollars per year"              
	100000      "100000+ dollars per year"      
;
label values tm8454   tm8454l;
label define tm8454l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8455   tm8455l;
label define tm8455l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8456   tm8456l;
label define tm8456l 
	0           "Not applicable"                
	1           "Yes - skip to TM8458"          
	2           "No"                            
;
label values tm8457   tm8457l;
label define tm8457l 
	0           "Not applicable"                
	1           "Child(ren) over the age limit" 
	2           "Other parent not working"      
	3           "Other parent in jail or"       
	4           "Payment suspended by court or" 
	5           "Other"                         
;
label values tm8458   tm8458l;
label define tm8458l 
	-1          "Don't know"                    
	0           "Not applicable"                
	99999       "Amount supposed to have received"
	100000      "100000+ amount supposed to have"
;
label values tm8459   tm8459l;
label define tm8459l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Directly from the other parent?"
	2           "Through a court?"              
	3           "Through the welfare or child"  
	4           "Some other method"             
;
label values tm8460   tm8460l;
label define tm8460l 
	-1          "Don't know"                    
	0           "Not applicable"                
	99999       "Total amount received"         
	100000      "100000+ total amount received" 
;
label values tm8461   tm8461l;
label define tm8461l 
	0           "Not applicable"                
	1           "All of the time"               
	2           "Most of the time"              
	3           "Some of the time"              
	4           "None of the time"              
;
label values tm8462   tm8462l;
label define tm8462l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes - skip to TM8464"          
	2           "No"                            
;
label values tm8463   tm8463l;
label define tm8463l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Less than $500"                
	2           "Between $500 and $5,000"       
	3           "More than $5,000"              
;
label values tm8464   tm8464l;
label define tm8464l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm8465   tm8465l;
label define tm8465l 
	0           "Not applicable"                
	1           "Custodian parent to provide"   
;
label values tm8466   tm8466l;
label define tm8466l 
	0           "Not applicable"                
	1           "Non-custodial parent to pay"   
;
label values tm8467   tm8467l;
label define tm8467l 
	0           "Not Applicable"                
	1           "Child support payments to"     
;
label values tm8468   tm8468l;
label define tm8468l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8469   tm8469l;
label define tm8469l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8470   tm8470l;
label define tm8470l 
	0           "Not applicable"                
	1           "Joint legal and physical"      
	2           "Joint legal with mother"       
	3           "Joint legal with father"       
	4           "Mother legal and physical"     
	5           "Father legal and physical"     
	6           "Split custody"                 
	7           "Other"                         
;
label values tm8471   tm8471l;
label define tm8471l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8472   tm8472l;
label define tm8472l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8474"           
;
label values tm8473   tm8473l;
label define tm8473l 
	0           "Not applicable"                
	1           "Yes Ask TM8474 for all children"
	2           "No  Ask TM8484 for oldest child"
;
label values tm8474   tm8474l;
label define tm8474l 
	0           "Not applicable"                
;
label values tm8475   tm8475l;
label define tm8475l 
	0           "Not applicable"                
;
label values tm8476   tm8476l;
label define tm8476l 
	0           "Not applicable"                
;
label values tm8477   tm8477l;
label define tm8477l 
	-3          "None"                          
;
label values tm8478   tm8478l;
label define tm8478l 
	-1          "Don't know"                    
;
label values tm8479   tm8479l;
label define tm8479l 
	0           "Not applicable"                
	1           "Same county/city"              
	2           "Same state (different"         
	3           "Different state"               
	4           "Other parent now deceased -"   
	5           "Other"                         
	6           "Unknown - skip to TM8528"      
;
label values tm8480   tm8480l;
label define tm8480l 
	0           "Not applicable"                
	1           "Yes - skip to TM8528"          
	2           "No"                            
;
label values tm8481   tm8481l;
label define tm8481l 
	0           "Not applicable"                
	1           "Respondent - skip TM8528"      
	2           "Other parent - skip TM8528"    
	3           "Both respondent and other"     
;
label values tm8482   tm8482l;
label define tm8482l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8483   tm8483l;
label define tm8483l 
	0           "Not applicable"                
;
label values tm8484   tm8484l;
label define tm8484l 
	0           "Not applicable"                
;
label values tm8485   tm8485l;
label define tm8485l 
	0           "Not applicable"                
;
label values tm8486   tm8486l;
label define tm8486l 
	0           "Not applicable"                
	99999       "Dollars per year"              
	100000      "100000+ dollars per year"      
;
label values tm8487   tm8487l;
label define tm8487l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8488   tm8488l;
label define tm8488l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8495"           
;
label values tm8489   tm8489l;
label define tm8489l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8490   tm8490l;
label define tm8490l 
	0           "Not applicable"                
;
label values tm8491   tm8491l;
label define tm8491l 
	0           "Not applicable"                
;
label values tm8492   tm8492l;
label define tm8492l 
	0           "Not applicable"                
;
label values tm8493   tm8493l;
label define tm8493l 
	0           "Not applicable"                
	99999       "Dollars per year"              
	100000      "100000+ dollars per year"      
;
label values tm8494   tm8494l;
label define tm8494l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm8495   tm8495l;
label define tm8495l 
	0           "Not applicable"                
	1           "Yes - skip to TM8497"          
	2           "No"                            
;
label values tm8496   tm8496l;
label define tm8496l 
	0           "Not applicable"                
	1           "Child(ren) too old"            
	2           "Other parent not working"      
	3           "Other parent in jail or institut"
	4           "Other - skip to TM8500"        
;
label values tm8497   tm8497l;
label define tm8497l 
	-1          "Don't know"                    
	0           "Not applicable"                
	99999       "Total amount that... was"      
	100000      "100000+ total amount that...Was"
;
label values tm8498   tm8498l;
label define tm8498l 
	-1          "Don't know"                    
	-3          "None - skip to TM8500"         
	0           "Not applicable"                
	99999       "Total amount that...actually"  
	100000      "100000+ total amount that..."  
;
label values tm8499   tm8499l;
label define tm8499l 
	0           "Not applicable"                
	1           "All of the time"               
	2           "Most of the time"              
	3           "Some of the time"              
	4           "None of the time"              
;
label values tm8500   tm8500l;
label define tm8500l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8502"           
;
label values tm8501   tm8501l;
label define tm8501l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Less than $500"                
	2           "Between $500 and $5,000"       
	3           "More"                          
;
label values tm8502   tm8502l;
label define tm8502l 
	0           "Not applicable"                
	1           "Non-custodial parent to provide"
;
label values tm8503   tm8503l;
label define tm8503l 
	0           "Not applicable"                
	1           "Custodial parent to provide"   
;
label values tm8504   tm8504l;
label define tm8504l 
	0           "Not applicable"                
	1           "Non-custodial parent to pay"   
;
label values tm8505   tm8505l;
label define tm8505l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm8506   tm8506l;
label define tm8506l 
	0           "Not applicable"                
	1           "None"                          
;
label values tm8507   tm8507l;
label define tm8507l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8508   tm8508l;
label define tm8508l 
	0           "Not applicable"                
	1           "Child(ren) live with mother"   
	2           "Child(ren) live with father"   
	3           "Child(ren) live with mother and"
	4           "None"                          
	5           "Other"                         
;
label values tm8509   tm8509l;
label define tm8509l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8512"           
;
label values tm8511   tm8511l;
label define tm8511l 
	0           "Not applicable"                
	1           "Yes ask TM8412 for all children"
	2           "No ask TM8412 for oldest child"
;
label values tm8512   tm8512l;
label define tm8512l 
	0           "Not applicable"                
;
label values tm8513   tm8513l;
label define tm8513l 
	0           "Not applicable"                
;
label values tm8514   tm8514l;
label define tm8514l 
	0           "Not applicable"                
;
label values tm8516   tm8516l;
label define tm8516l 
	-1          "Don't know"                    
;
label values tm8517   tm8517l;
label define tm8517l 
	0           "Not applicable"                
	1           "Legal paternity not established"
;
label values tm8518   tm8518l;
label define tm8518l 
	0           "Not applicable"                
	1           "Unable to locate parent"       
;
label values tm8519   tm8519l;
label define tm8519l 
	0           "Not applicable"                
	1           "Other parent unable to pay"    
;
label values tm8520   tm8520l;
label define tm8520l 
	0           "Not applicable"                
	1           "Final agreement pending"       
;
label values tm8521   tm8521l;
label define tm8521l 
	0           "Not applicable"                
	1           "Accepted property settlement in"
;
label values tm8522   tm8522l;
label define tm8522l 
	0           "Not applicable"                
	1           "Do not want a legal child suppor"
;
label values tm8523   tm8523l;
label define tm8523l 
	0           "Not applicable"                
	1           "Did not pursue award"          
;
label values tm8524   tm8524l;
label define tm8524l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8525   tm8525l;
label define tm8525l 
	0           "Not applicable"                
	1           "Same county/city"              
	2           "Same state (different county/cit"
	3           "Different state"               
	4           "Other parent now deceased - skip"
	5           "Other"                         
	6           "Unknown - skip to TM8528"      
;
label values tm8526   tm8526l;
label define tm8526l 
	0           "Not applicable"                
	1           "Yes - skip to TM8528"          
	2           "No"                            
;
label values tm8527   tm8527l;
label define tm8527l 
	0           "Not applicable"                
	1           "Respondent"                    
	2           "Other parent"                  
	3           "Both respondent and other parent"
;
label values tm8528   tm8528l;
label define tm8528l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8535"           
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
	99999       "Dollars per year"              
	100000      "100000+ dollars per year"      
;
label values tm8533   tm8533l;
label define tm8533l 
	0           "Not applicable"                
	-1          "Don't know"                    
	-3          "None"                          
;
label values tm8534   tm8534l;
label define tm8534l 
	-1          "Don't know"                    
	-3          "None"                          
	0           "Not applicable"                
	99999       "Total amount... actually"      
	100000      "100000+ total amount... actually"
;
label values tm8535   tm8535l;
label define tm8535l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8552"           
;
label values tm8536   tm8536l;
label define tm8536l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8537   tm8537l;
label define tm8537l 
	0           "Not applicable"                
	1           "Locate the other parent"       
;
label values tm8538   tm8538l;
label define tm8538l 
	0           "Not applicable"                
	1           "Establish paternity/maternity" 
;
label values tm8539   tm8539l;
label define tm8539l 
	0           "Not applicable"                
	1           "Establish support obligation"  
;
label values tm8540   tm8540l;
label define tm8540l 
	0           "Not applicable"                
	1           "Establish medical support"     
;
label values tm8541   tm8541l;
label define tm8541l 
	0           "Not applicable"                
	1           "Enforce support order"         
;
label values tm8542   tm8542l;
label define tm8542l 
	0           "Not applicable"                
	1           "Modify an order"               
;
label values tm8543   tm8543l;
label define tm8543l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8544   tm8544l;
label define tm8544l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8552"           
;
label values tm8545   tm8545l;
label define tm8545l 
	0           "Not applicable"                
	1           "Locate the other parent"       
;
label values tm8546   tm8546l;
label define tm8546l 
	1           "Establish paternity/maternity" 
;
label values tm8547   tm8547l;
label define tm8547l 
	0           "Not applicable"                
	1           "Establish support obligation"  
;
label values tm8548   tm8548l;
label define tm8548l 
	0           "Not applicable"                
	1           "Establish medical support"     
;
label values tm8549   tm8549l;
label define tm8549l 
	0           "Not applicable"                
	1           "Enforce support order"         
;
label values tm8550   tm8550l;
label define tm8550l 
	0           "Not applicable"                
	1           "Modify an order"               
;
label values tm8551   tm8551l;
label define tm8551l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8552   tm8552l;
label define tm8552l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8595"           
;
label values tm8553   tm8553l;
label define tm8553l 
	0           "Not applicable"                
	4           "4+ Children"                   
;
label values tm8554   tm8554l;
label define tm8554l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8555   tm8555l;
label define tm8555l 
	0           "Not applicable"                
;
label values tm8556   tm8556l;
label define tm8556l 
	0           "Not applicable"                
;
label values tm8557   tm8557l;
label define tm8557l 
	0           "Not applicable"                
	1           "Legal paternity not established"
;
label values tm8558   tm8558l;
label define tm8558l 
	0           "Not applicable"                
	1           "Legal paternity not established"
;
label values tm8559   tm8559l;
label define tm8559l 
	0           "Not applicable"                
	1           "Unable to locate parent"       
;
label values tm8560   tm8560l;
label define tm8560l 
	0           "Not applicable"                
	1           "Unable to locate parent"       
;
label values tm8561   tm8561l;
label define tm8561l 
	0           "Not applicable"                
	1           "Other parent unable to pay"    
;
label values tm8562   tm8562l;
label define tm8562l 
	0           "Not applicable"                
	1           "Other parent unable to pay"    
;
label values tm8563   tm8563l;
label define tm8563l 
	0           "Not applicable"                
	1           "Final agreement pending"       
;
label values tm8564   tm8564l;
label define tm8564l 
	0           "Not applicable"                
	1           "Final agreement pending"       
;
label values tm8565   tm8565l;
label define tm8565l 
	0           "Not applicable"                
	1           "Accepted property"             
;
label values tm8566   tm8566l;
label define tm8566l 
	0           "Not applicable"                
	1           "Accepted property"             
;
label values tm8567   tm8567l;
label define tm8567l 
	0           "Not applicable"                
	1           "Do not want child support"     
;
label values tm8568   tm8568l;
label define tm8568l 
	0           "Not applicable"                
	1           "Do not want child support"     
;
label values tm8569   tm8569l;
label define tm8569l 
	0           "Not applicable"                
	1           "Did not pursue award"          
;
label values tm8570   tm8570l;
label define tm8570l 
	0           "Not applicable"                
	1           "Unable to locate parent"       
;
label values tm8571   tm8571l;
label define tm8571l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8572   tm8572l;
label define tm8572l 
	0           "Not applicable"                
	1           "Father unable to pay"          
;
label values tm8573   tm8573l;
label define tm8573l 
	0           "Not applicable"                
	1           "Same county/city"              
;
label values tm8575   tm8575l;
label define tm8575l 
	0           "Not applicable"                
	1           "Same state(diferent county/city)"
;
label values tm8576   tm8576l;
label define tm8576l 
	0           "Not applicable"                
	1           "Same state (different"         
;
label values tm8577   tm8577l;
label define tm8577l 
	0           "Not applicable"                
	1           "Different state"               
;
label values tm8578   tm8578l;
label define tm8578l 
	0           "Not applicable"                
	1           "Different state"               
;
label values tm8579   tm8579l;
label define tm8579l 
	0           "Not applicable"                
	1           "Other parent deceased"         
;
label values tm8580   tm8580l;
label define tm8580l 
	0           "Not applicable"                
	1           "Other parent deceased"         
;
label values tm8581   tm8581l;
label define tm8581l 
	-1          "Unknown"                       
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8582   tm8582l;
label define tm8582l 
	-1          "Unknown"                       
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8583   tm8583l;
label define tm8583l 
	0           "Not applicable"                
;
label values tm8584   tm8584l;
label define tm8584l 
	0           "Not applicable"                
;
label values tm8585   tm8585l;
label define tm8585l 
	0           "Not applicable"                
;
label values tm8586   tm8586l;
label define tm8586l 
	0           "Not applicable"                
;
label values tm8587   tm8587l;
label define tm8587l 
	0           "Not applicable"                
;
label values tm8588   tm8588l;
label define tm8588l 
	0           "Not applicable"                
;
label values tm8589   tm8589l;
label define tm8589l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8590   tm8590l;
label define tm8590l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8591   tm8591l;
label define tm8591l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8592   tm8592l;
label define tm8592l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8593   tm8593l;
label define tm8593l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8595"           
;
label values tm8594   tm8594l;
label define tm8594l 
	-1          "Don't know"                    
	0           "Not applicable"                
	99999       "Dollar amount received"        
	100000      "100000+ dollar amount received"
;
label values tm8595   tm8595l;
label define tm8595l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8700   tm8700l;
label define tm8700l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8800"           
;
label values tm8702   tm8702l;
label define tm8702l 
	0           "Not applicable"                
	1           "Regular"                       
	2           "Lump-sum"                      
	3           "Both"                          
;
label values tm8704   tm8704l;
label define tm8704l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8748"           
	-1          "Don't know - skip to TM8748"   
;
label values tm8706   tm8706l;
label define tm8706l 
	0           "Not applicable"                
	4           "4+ children"                   
	-1          "Don't know"                    
;
label values tm8708   tm8708l;
label define tm8708l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8744"           
;
label values tm8710   tm8710l;
label define tm8710l 
	-1          "Don't know"                    
	0           "Not applicable"                
	4           "4+ children"                   
;
label values tm8712   tm8712l;
label define tm8712l 
	0           "Not applicable"                
	1           "Voluntary written agreement"   
	2           "Court-ordered agreement"       
	3           "Other type of written agreement"
	4           "Non-written agreement - skip"  
;
label values tm8714   tm8714l;
label define tm8714l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8716   tm8716l;
label define tm8716l 
	-1          "Don't know - skip to TM8720"   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8720"           
;
label values tm8718   tm8718l;
label define tm8718l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8719   tm8719l;
label define tm8719l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8720   tm8720l;
label define tm8720l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8722   tm8722l;
label define tm8722l 
	-1          "Don't know"                    
	0           "Not applicable"                
	10000       "10000+ amount paid"            
;
label values tm8724   tm8724l;
label define tm8724l 
	-1          "Don't know"                    
	0           "Not applicable"                
	1           "Through employment related wage"
	2           "Directly to the other parent"  
	3           "Directly to the court"         
	4           "Directly to a child support agen"
	5           "Other"                         
;
label values tm8726   tm8726l;
label define tm8726l 
	0           "Not applicable"                
	1           "Non-custodial parent to provide"
;
label values tm8728   tm8728l;
label define tm8728l 
	0           "Not applicable"                
	1           "Custodial parent to provide"   
;
label values tm8730   tm8730l;
label define tm8730l 
	0           "Not applicable"                
	1           "Non=custodial parent to pay"   
;
label values tm8732   tm8732l;
label define tm8732l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm8734   tm8734l;
label define tm8734l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8736   tm8736l;
label define tm8736l 
	-3          "None"                          
	0           "Not applicable"                
;
label values tm8738   tm8738l;
label define tm8738l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8742"           
;
label values tm8740   tm8740l;
label define tm8740l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8742   tm8742l;
label define tm8742l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8746"           
;
label values tm8744   tm8744l;
label define tm8744l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8746   tm8746l;
label define tm8746l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8800"           
;
label values tm8748   tm8748l;
label define tm8748l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8750   tm8750l;
label define tm8750l 
	0           "Not applicable"                
	1           "Parent"                        
	2           "Spouse"                        
	3           "Ex-spouse"                     
	4           "Child under 21"                
	5           "Child 21 or older"             
	6           "Other relative"                
	7           "Not related"                   
;
label values tm8752   tm8752l;
label define tm8752l 
	0           "Not applicable"                
	1           "Parent"                        
	2           "Spouse"                        
	3           "Ex-spouse"                     
	4           "Child under 21"                
	5           "Child 21 or older"             
	6           "Other relative"                
	7           "Not related"                   
;
label values tm8754   tm8754l;
label define tm8754l 
	0           "Not applicable"                
	1           "Private home or apartment"     
	2           "Nursing home"                  
	3           "Someplace else"                
;
label values tm8756   tm8756l;
label define tm8756l 
	0           "Not applicable"                
	1           "Private home or apartment"     
	2           "Nursing home"                  
	3           "Someplace else"                
;
label values tm8758   tm8758l;
label define tm8758l 
	-1          "Don't know"                    
	0           "Not applicable"                
	12000       "12000+ the amount ... paid for"
;
label values tm8760   tm8760l;
label define tm8760l 
	-1          "Don't know"                    
	0           "Not applicable"                
	12000       "12000+ the amount ... paid for"
;
label values tm8762   tm8762l;
label define tm8762l 
	1           "Yes"                           
	2           "No - skip to TM8800"           
;
label values tm8764   tm8764l;
label define tm8764l 
	-1          "Don't know"                    
	0           "Not applicable"                
	12000       "12000+ the amount ... paid"    
;
label values tm8800   tm8800l;
label define tm8800l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
;
label values tm8802   tm8802l;
label define tm8802l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8804   tm8804l;
label define tm8804l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8806   tm8806l;
label define tm8806l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - Skip to TM8810"           
;
label values tm8808   tm8808l;
label define tm8808l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8810   tm8810l;
label define tm8810l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty- skip to TM8814" 
;
label values tm8812   tm8812l;
label define tm8812l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8814   tm8814l;
label define tm8814l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty- Skip to TM8818" 
;
label values tm8816   tm8816l;
label define tm8816l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8818   tm8818l;
label define tm8818l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty - Skip To TM8822"
;
label values tm8820   tm8820l;
label define tm8820l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8822   tm8822l;
label define tm8822l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty- skip to TM8826" 
;
label values tm8824   tm8824l;
label define tm8824l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8826   tm8826l;
label define tm8826l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty- skip to TM8830" 
;
label values tm8828   tm8828l;
label define tm8828l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8830   tm8830l;
label define tm8830l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty-skip to TM8834"  
;
label values tm8832   tm8832l;
label define tm8832l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8834   tm8834l;
label define tm8834l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty-skip to TM8838"  
;
label values tm8836   tm8836l;
label define tm8836l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8838   tm8838l;
label define tm8838l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8839   tm8839l;
label define tm8839l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8840   tm8840l;
label define tm8840l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No Difficulty"                 
;
label values tm8841   tm8841l;
label define tm8841l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8842   tm8842l;
label define tm8842l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8843   tm8843l;
label define tm8843l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8844   tm8844l;
label define tm8844l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8845   tm8845l;
label define tm8845l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8846   tm8846l;
label define tm8846l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8847   tm8847l;
label define tm8847l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8848   tm8848l;
label define tm8848l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8849   tm8849l;
label define tm8849l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8850   tm8850l;
label define tm8850l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8851   tm8851l;
label define tm8851l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8852   tm8852l;
label define tm8852l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8853   tm8853l;
label define tm8853l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8854   tm8854l;
label define tm8854l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8855   tm8855l;
label define tm8855l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8856   tm8856l;
label define tm8856l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty"                 
;
label values tm8857   tm8857l;
label define tm8857l 
	0           " Not applicable"               
	1           " Yes"                          
	2           " No"                           
;
label values tm8858   tm8858l;
label define tm8858l 
	0           " Not applicable"               
	1           " Has difficulty"               
	2           " No difficulty"                
;
label values tm8859   tm8859l;
label define tm8859l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8860   tm8860l;
label define tm8860l 
	0           "Not applicable"                
	1           "Yes - Go to TM8876"            
	2           "No - skip to TM8890"           
;
label values tm8876   tm8876l;
label define tm8876l 
	0           "Not applicable"                
	1           "Son"                           
	2           "Daughter"                      
	3           "Spouse"                        
	4           "Parent"                        
	5           "Other relative"                
	6           "Friend or neighbor"            
	7           "Paid help"                     
	8           "Other nonrelative"             
	9           "Did not receive help -"        
;
label values tm8878   tm8878l;
label define tm8878l 
	0           "Not applicable"                
	1           "Son"                           
	2           "Daughter"                      
	3           "Spouse"                        
	4           "Parent"                        
	5           "Other relative"                
	6           "Friend or neighbor"            
	7           "Paid help"                     
	8           "Other nonrelative"             
;
label values tm8880   tm8880l;
label define tm8880l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8882   tm8882l;
label define tm8882l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm8883   tm8883l;
label define tm8883l 
	0           "Not applicable"                
;
label values tm8884   tm8884l;
label define tm8884l 
	0           "Not applicable"                
;
label values tm8885   tm8885l;
label define tm8885l 
	0           "Not applicable"                
	1           "No"                            
;
label values tm8886   tm8886l;
label define tm8886l 
	0           "Not applicable"                
	1           "No"                            
;
label values tm8887   tm8887l;
label define tm8887l 
	0           "Not applicable"                
	1           "Less than 6 months"            
	2           "6 to 11 months"                
	3           "1 to 2 years"                  
	4           "3 to 5 years"                  
	5           "More than 5 years"             
;
label values tm8888   tm8888l;
label define tm8888l 
	-1          "Don't know - skip to TM8892"   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8892"           
;
label values tm8889   tm8889l;
label define tm8889l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8890   tm8890l;
label define tm8890l 
	0           "Not appicable"                 
	1           "Yes"                           
	2           "No  - skip to TM8902"          
;
label values tm8892   tm8892l;
label define tm8892l 
	0           "Not applicable"                
;
label values tm8894   tm8894l;
label define tm8894l 
	0           "Not applicable"                
;
label values tm8896   tm8896l;
label define tm8896l 
	0           "Not applicable"                
;
label values tm8898   tm8898l;
label define tm8898l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8902"           
;
label values tm8900   tm8900l;
label define tm8900l 
	0           "Not applicable"                
;
label values tm8902   tm8902l;
label define tm8902l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8904   tm8904l;
label define tm8904l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8906   tm8906l;
label define tm8906l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8908   tm8908l;
label define tm8908l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8910   tm8910l;
label define tm8910l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8912   tm8912l;
label define tm8912l 
	0           "Not applicable"                
	1           "15 years old - skip to TM8942" 
	2           "16 to 67 years old"            
	3           "68 years old or older - skip"  
;
label values tm8914   tm8914l;
label define tm8914l 
	0           "Not applicable"                
	1           "Yes - skip to TM8918"          
	2           "No"                            
;
label values tm8916   tm8916l;
label define tm8916l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8920"           
;
label values tm8918   tm8918l;
label define tm8918l 
	0           "Not applicable"                
	1           "Yes - skip to TM8922"          
	2           "No - skip to TM8926"           
;
label values tm8920   tm8920l;
label define tm8920l 
	0           "Not applicable"                
	1           "Yes - Mark '171' on ISS"       
	2           "No - Skip to TM8926"           
;
label values tm8922   tm8922l;
label define tm8922l 
	0           "Not applicable"                
	1           "Yes - skip to TM8926"          
	2           "No"                            
;
label values tm8924   tm8924l;
label define tm8924l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8926   tm8926l;
label define tm8926l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8930"           
;
label values tm8928   tm8928l;
label define tm8928l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8930   tm8930l;
label define tm8930l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -  Skip to TM8942"          
;
label values tm8932   tm8932l;
label define tm8932l 
	0           "Not applicable"                
;
label values tm8934   tm8934l;
label define tm8934l 
	0           "Not applicable"                
;
label values tm8936   tm8936l;
label define tm8936l 
	0           "Not applicable"                
;
label values tm8938   tm8938l;
label define tm8938l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8942"           
;
label values tm8940   tm8940l;
label define tm8940l 
	0           "Not applicable"                
;
label values tm8941   tm8941l;
label define tm8941l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9022"           
;
label values tm8942   tm8942l;
label define tm8942l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8960"           
;
label values tm8944   tm8944l;
label define tm8944l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8952"           
;
label values tm8946   tm8946l;
label define tm8946l 
	0           "Not applicable"                
;
label values tm8948   tm8948l;
label define tm8948l 
	0           "Not applicable"                
;
label values tm8950   tm8950l;
label define tm8950l 
	0           "Not applicable"                
;
label values tm8952   tm8952l;
label define tm8952l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8960"           
;
label values tm8954   tm8954l;
label define tm8954l 
	0           "Not applicable"                
;
label values tm8956   tm8956l;
label define tm8956l 
	0           "Not applicable"                
;
label values tm8958   tm8958l;
label define tm8958l 
	0           "Not applicable"                
;
label values tm8960   tm8960l;
label define tm8960l 
	0           "Not appilcable"                
	1           "Yes"                           
	2           "No - skip to TM8986"           
;
label values tm8962   tm8962l;
label define tm8962l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8970"           
;
label values tm8964   tm8964l;
label define tm8964l 
	0           "Not applicable"                
;
label values tm8966   tm8966l;
label define tm8966l 
	0           "Not applicable"                
;
label values tm8968   tm8968l;
label define tm8968l 
	0           "Not applicable"                
;
label values tm8970   tm8970l;
label define tm8970l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8986"           
;
label values tm8972   tm8972l;
label define tm8972l 
	0           "Not applicable"                
;
label values tm8974   tm8974l;
label define tm8974l 
	0           "Not applicable"                
;
label values tm8976   tm8976l;
label define tm8976l 
	0           "Not applicable"                
;
label values tm8978   tm8978l;
label define tm8978l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8986"           
;
label values tm8980   tm8980l;
label define tm8980l 
	0           "Not applicable"                
;
label values tm8982   tm8982l;
label define tm8982l 
	0           "Not applicable"                
;
label values tm8984   tm8984l;
label define tm8984l 
	0           "Not applicable"                
;
label values tm8986   tm8986l;
label define tm8986l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8996"           
;
label values tm8988   tm8988l;
label define tm8988l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8996"           
;
label values tm8990   tm8990l;
label define tm8990l 
	0           "Not applicable"                
;
label values tm8992   tm8992l;
label define tm8992l 
	0           "Not applicable"                
;
label values tm8994   tm8994l;
label define tm8994l 
	0           "Not applicable"                
;
label values tm8996   tm8996l;
label define tm8996l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9022"           
;
label values tm8998   tm8998l;
label define tm8998l 
	0           "Not applicable"                
;
label values tm9000   tm9000l;
label define tm9000l 
	0           "Not applicable"                
;
label values tm9002   tm9002l;
label define tm9002l 
	0           "Not applicable"                
;
label values tm9004   tm9004l;
label define tm9004l 
	0           "Not applicable"                
;
label values tm9006   tm9006l;
label define tm9006l 
	0           "Not applicable"                
;
label values tm9008   tm9008l;
label define tm9008l 
	0           "Not applicable"                
;
label values tm9010   tm9010l;
label define tm9010l 
	0           "Not applicable"                
;
label values tm9012   tm9012l;
label define tm9012l 
	0           "Not applicable"                
;
label values tm9014   tm9014l;
label define tm9014l 
	0           "Not applicable"                
;
label values tm9016   tm9016l;
label define tm9016l 
	0           "Not applicable"                
;
label values tm9018   tm9018l;
label define tm9018l 
	0           "Not applicable"                
;
label values tm9020   tm9020l;
label define tm9020l 
	0           "Not applicable"                
;
label values tm9022   tm9022l;
label define tm9022l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9036"           
;
label values tm9024   tm9024l;
label define tm9024l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9038"           
;
label values tm9026   tm9026l;
label define tm9026l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9100   tm9100l;
label define tm9100l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9122"           
;
label values tm9102   tm9102l;
label define tm9102l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm9104   tm9104l;
label define tm9104l 
	0           "Not applicable"                
	1           "Child birth"                   
;
label values tm9106   tm9106l;
label define tm9106l 
	0           "Not applicable"                
	1           "Surgery or operation (incl. bone"
;
label values tm9108   tm9108l;
label define tm9108l 
	0           "Not applicable"                
	1           "Other medical"                 
;
label values tm9110   tm9110l;
label define tm9110l 
	0           "Not applicable"                
	1           "Mental or emotional problem or"
;
label values tm9112   tm9112l;
label define tm9112l 
	0           "Not applicable"                
	1           "Drug or alcohol abuse problem" 
;
label values tm9114   tm9114l;
label define tm9114l 
	0           "Not applicable"                
	1           "Yes, Military"                 
	2           "Yes, VA"                       
	3           "Yes, both military and VA"     
	4           "No"                            
;
label values tm9116   tm9116l;
label define tm9116l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9118   tm9118l;
label define tm9118l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm9120   tm9120l;
label define tm9120l 
	0           "Not applicable"                
	-5          "All nights"                    
	-3          "None"                          
	-1          "Don't know"                    
;
label values tm9122   tm9122l;
label define tm9122l 
	0           "Not applicable"                
	-5          "All days"                      
	-3          "None"                          
	-1          "Don't know"                    
;
label values tm9124   tm9124l;
label define tm9124l 
	-1          "Don't know - skip to TM9128"   
	-3          "None - skip to TM9128"         
	0           "Not applicable"                
;
label values tm9126   tm9126l;
label define tm9126l 
	-1          "Don't know"                    
	-3          "None"                          
	0           "Not applicable"                
;
label values tm9127   tm9127l;
label define tm9127l 
	-1          " Don't know - skip to TM9120"  
	-3          " None - skip to TM9120"        
	0           " Not applicable"               
;
label values tm9128   tm9128l;
label define tm9128l 
	-1          " Don't know"                   
	-3          " None"                         
	0           " Not applicable"               
	2           " No - skip to TM9132"          
;
label values tm9129   tm9129l;
label define tm9129l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9132"           
;
label values tm9130   tm9130l;
label define tm9130l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic (not"
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hostipal"          
	10          "Private practice psychiatrist or"
	11          "Other"                         
;
label values tm9132   tm9132l;
label define tm9132l 
	0           "Not applicable"                
	1           "Yes - skip to next record"     
	2           "No"                            
;
label values tm9133   tm9133l;
label define tm9133l 
	0           "Not applicable"                
	1           "Yes - skip to next record"     
	2           "No"                            
;
label values tm9134   tm9134l;
label define tm9134l 
	0           "Not applicable"                
	1           "Yes - skip to end"             
	2           "No"                            
;
label values tm9136   tm9136l;
label define tm9136l 
	0           "Not applicable"                
	1           "Correct"                       
	2           "Incorrect - covered by some"   
;
label values tm9138   tm9138l;
label define tm9138l 
	0           "Not applicable"                
	1           "Job layoff, job loss, or any"  
	2           "Employer does not offer health"
	3           "Can't obtain health insurance" 
	4           "Too expensive, can't afford"   
	5           "Don't believe in health"       
	6           "Have been healthy, not much"   
	7           "Able to go to VA or military"  
	8           "Covered by some other health"  
	9           "Other"                         
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
