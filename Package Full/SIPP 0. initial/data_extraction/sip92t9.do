log using sip92t9, text replace
set mem 1000m
*This program reads the 1992 SIPP Wave 9 Topical Module Data File 

****************************************************************
*
* NOTE: This complete dataset has over more than 2,047 variables,
* the maximum number of variables for Intercooled Stata 8.0. 
* So, variables at the end are commented out.  The commenting 
* can be removed in an editor by replacing '*#' with ''.
* Stata/SE can handle up to 32,766 variables, default=5000.
*
****************************************************************

*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 18:35:07 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip92t9
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1992\sip92t9.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip92t9"

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
	6           "Other  Type A, Type B"         
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
	28          "Merged HHLDs across panels"    
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children"      
	1           "Interview (Self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
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
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T2"    
;
label values tm8001   tm8001l;
label define tm8001l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T2"    
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
	-5          "All days (first employer)"     
;
label values tm8046   tm8046l;
label define tm8046l 
	0           "Not applicable"                
	-5          "All days (second employer)"    
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
	3           "Better arrangements for care of"
	4           "Allows time for school"        
	5           "Other voluntary reasons"       
	6           "Could not get any other job"   
	7           "Requirement of the job"        
	8           "Other involuntary reasons"     
;
label values tm8070   tm8070l;
label define tm8070l 
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
label values tm8072   tm8072l;
label define tm8072l 
	0           "Not applicable"                
	1           "Yes - ask items 1c though 1i"  
	2           "No - go to check item T2"      
;
label values tm7990   tm7990l;
label define tm7990l 
	1           "Under 10"                      
	2           "10-24"                         
	3           "25-99"                         
	4           "100-499"                       
	5           "500-999"                       
	6           "1000+"                         
;
label values tm7992   tm7992l;
label define tm7992l 
	1           "Under 10"                      
	2           "10-24"                         
	3           "25-99"                         
	4           "100-499"                       
	5           "500-999"                       
	6           "1000+"                         
;
label values tm8100   tm8100l;
label define tm8100l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip TM8400"              
;
label values tm8105   tm8105l;
label define tm8105l 
	0           "Not applicable"                
	1           "Yes - skip to check item T6"   
	2           "No"                            
;
label values tm8106   tm8106l;
label define tm8106l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T5"    
;
label values tm8107   tm8107l;
label define tm8107l 
	0           "Not applicable"                
	-1          "Hours varied - skip to check"  
	-2          "Dk - skip to check item T6"    
	-3          "Not enrolled last month"       
;
label values tm8108   tm8108l;
label define tm8108l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8400"           
;
label values tm8109   tm8109l;
label define tm8109l 
	0           "Not applicable"                
	-1          "Hours varied"                  
	-2          "Dk"                            
	-3          "Did not look for a job last"   
;
label values tm8114   tm8114l;
label define tm8114l 
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
	1           "Child's other parent/stepparent"
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care center"
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "... works at home"             
	12          "... cares for child at work (in"
	13          "Child not born as of last month"
	14          "... did not work, go to school,"
;
label values tm8122   tm8122l;
label define tm8122l 
	0           "Not applicable"                
	1           "Child's other parent/stepparent"
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care center"
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "... works at home"             
	12          "... cares for child at work (in"
	13          "Child not born as of last month"
;
label values tm8124   tm8124l;
label define tm8124l 
	0           "Not applicable"                
	1           "Child's other parent/stepparent"
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care center"
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "... works at home"             
	12          "... cares for child at work (in"
	13          "Child not born as of last month"
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
	1           "Yes- skip to TM8148"           
	2           "No - skip to TM8160"           
;
label values tm8142   tm8142l;
label define tm8142l 
	0           "Not applicable"                
	1           "Yes- skip to TM8150"           
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
	-1          "Dk"                            
;
label values tm8154   tm8154l;
label define tm8154l 
	0           "Not applicable"                
	-1          "Dk"                            
	-2          "Previously recorded for"       
;
label values tm8156   tm8156l;
label define tm8156l 
	0           "Not applicable"                
	-1          "Dk"                            
	-2          "Previously recorded for"       
	-3          "Previously recorded for second"
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
	2           "No - skip to check item T11"   
;
label values tm8170   tm8170l;
label define tm8170l 
	0           "Not applicable"                
	1           "Child's other parent/step-"    
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care center"
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten"         
	10          "Child cares for self"          
	11          "... works at home"             
	12          "... cares for child at work (in"
;
label values tm8172   tm8172l;
label define tm8172l 
	0           "Not applicable"                
	1           "Child's other parent/stepparent"
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care center"
	7           "Child in nursery/preschool"    
	8           "Child in organized school-based"
	9           "Child in kindergarten"         
	10          "Child cares for self"          
	11          "... works at home"             
	12          "... cares for child at work (in"
;
label values tm8174   tm8174l;
label define tm8174l 
	0           "Not applicable"                
	1           "Child's other parent/stepparent"
	2           "Child's brother/sister"        
	3           "Child's grandparent"           
	4           "Other relative of child"       
	5           "Nonrelative of child"          
	6           "Child in day/group care center"
	7           "Child in nursey/preschool"     
	8           "Child in organized school-based"
	9           "Child in kindergarten,"        
	10          "Child cares for self"          
	11          "... works at home"             
	12          "... cares for child at work (in"
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
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm8204   tm8204l;
label define tm8204l 
	0           "Not applicable"                
	-1          "Dk"                            
	-2          "Previously recorded for"       
;
label values tm8206   tm8206l;
label define tm8206l 
	0           "Not applicable"                
	-1          "Dk"                            
	-2          "Previously recorded for"       
	-3          "Previously recorded for second"
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
label values tm8322   tm8322l;
label define tm8322l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8326"           
;
label values tm8324   tm8324l;
label define tm8324l 
	0           "Not applicable"                
	999999      "Per week"                      
	-2          "All costs already recorded for"
;
label values tm8326   tm8326l;
label define tm8326l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8400"           
;
label values tm8328   tm8328l;
label define tm8328l 
	0           "Not applicable"                
	1           "Yes, respondent lost time"     
	2           "Yes spouse lost time"          
	3           "Both, respondent and spouse"   
	4           "No"                            
	-1          "Don't know"                    
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to part d (TM8700)"  
;
label values tm8401   tm8401l;
label define tm8401l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to part d (TM8700)"  
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not applicable"                
	4           "04+ children with a parent"    
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
	2           "No - for each child listed"    
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
	2           "No - skip to 1j"               
;
label values tm8439   tm8439l;
label define tm8439l 
	0           "Not applicable"                
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
	3           "Other type of written"         
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
	-1          "Don't know"                    
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8456"           
;
label values tm8449   tm8449l;
label define tm8449l 
	-1          "Dk"                            
	0           "Not applicable"                
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "not applicable"                
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
	100000      "Dollars per year"              
;
label values tm8454   tm8454l;
label define tm8454l 
	0           "Not applicable"                
	-1          "Don't know"                    
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
	5           "Other skip to TM8462"          
;
label values tm8458   tm8458l;
label define tm8458l 
	0           "Not applicable"                
	100000      "Amount supposed"               
	-1          "Dk"                            
;
label values tm8459   tm8459l;
label define tm8459l 
	0           "Not applicable"                
	1           "Directly from the other parent?"
	2           "Through a court?"              
	3           "Through the welfare or child"  
	4           "Some other method"             
	-1          "Dk"                            
;
label values tm8460   tm8460l;
label define tm8460l 
	0           "Not applicable"                
	100000      "Total amount received"         
	-3          "None - skip to TM8462"         
	-1          "Dk"                            
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
	0           "Not applicable"                
	1           "Yes - skip to TM8464"          
	2           "No"                            
	-1          "Dk"                            
;
label values tm8463   tm8463l;
label define tm8463l 
	0           "Not applicable"                
	1           "Less than $500"                
	2           "Between $500 and $5,000"       
	3           "More than $5,000"              
	-1          "Dk"                            
;
label values tm8464   tm8464l;
label define tm8464l 
	0           "Not applicable"                
	1           "Non-custodial parent to provide"
;
label values tm8465   tm8465l;
label define tm8465l 
	0           "Not applicable"                
	1           "Custodial parent to provide"   
;
label values tm8466   tm8466l;
label define tm8466l 
	0           "Not applicable"                
	1           "Non-custodial parent to pay"   
;
label values tm8467   tm8467l;
label define tm8467l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm8468   tm8468l;
label define tm8468l 
	0           "Not applicable"                
	1           "None"                          
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
	1           "Yes ask TM8474 for all children"
	2           "No  ask TM8484 for oldest child"
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
	-1          "Dk"                            
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
	0           "Not applicable"                
	-1          "Dk"                            
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
	0           "Not applicable"                
	-1          "Dk"                            
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
	3           "Other parent in jail or"       
	4           "Other skip to TM8500"          
;
label values tm8497   tm8497l;
label define tm8497l 
	0           "Not applicable"                
	99999       "Total amount that ...was"      
	100000      "100000+ total amount that ...was"
	-1          "Dk"                            
;
label values tm8498   tm8498l;
label define tm8498l 
	0           "Not applicable"                
	99999       "Total amount that ... actually"
	100000      "100000+ total amount that ..." 
	-1          "Dk"                            
	-3          "None - skip to TM8500"         
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
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8502"           
	-1          "Dk"                            
;
label values tm8501   tm8501l;
label define tm8501l 
	0           "Not applicable"                
	1           "Less than $500"                
	2           "Between $500 and $5,000"       
	3           "More"                          
	-1          "Dk"                            
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
label values tm8515   tm8515l;
label define tm8515l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm8516   tm8516l;
label define tm8516l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm8517   tm8517l;
label define tm8517l 
	0           "Not applicable"                
	1           "Male - skip to TM8648"         
	2           "Female"                        
;
label values tm8518   tm8518l;
label define tm8518l 
	0           "Not applicable"                
	1           "Never married - go to TM8519"  
	2           "All others - skip to TM8583"   
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
	0           "Not applicable"                
;
label values tm8523   tm8523l;
label define tm8523l 
	0           "Not applicable"                
;
label values tm8524   tm8524l;
label define tm8524l 
	0           "Not applicable"                
;
label values tm8525   tm8525l;
label define tm8525l 
	0           "Not applicable"                
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
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8536   tm8536l;
label define tm8536l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8537   tm8537l;
label define tm8537l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8538   tm8538l;
label define tm8538l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8539   tm8539l;
label define tm8539l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8540   tm8540l;
label define tm8540l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8541   tm8541l;
label define tm8541l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
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
	1           "Yes - ask TM8535 through TM8574"
	2           "No  - skip to TM8648"          
;
label values tm8576   tm8576l;
label define tm8576l 
	0           "Not applicable"                
	1           "Yes - ask TM8535 through TM8574"
	2           "No  - skip to TM8648"          
;
label values tm8577   tm8577l;
label define tm8577l 
	0           "Not applicable"                
	1           "Yes - ask TM8535 through TM8574"
	2           "No  - skip to TM8648"          
;
label values tm8578   tm8578l;
label define tm8578l 
	0           "Not applicable"                
	1           "Yes - ask TM8535 through TM8574"
	2           "No  - skip to TM8648"          
;
label values tm8579   tm8579l;
label define tm8579l 
	0           "Not applicable"                
	1           "Yes - ask TM8535 through TM8574"
	2           "No  - skip to TM8648"          
;
label values tm8580   tm8580l;
label define tm8580l 
	0           "Not applicable"                
	1           "Yes - ask TM8535 through TM8574"
	2           "No  - skip to TM8648"          
;
label values tm8581   tm8581l;
label define tm8581l 
	0           "Not applicable"                
	1           "Yes - ask TM8535 through TM8574"
	2           "No  - skip to TM8648"          
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
;
label values tm8590   tm8590l;
label define tm8590l 
	0           "Not applicable"                
;
label values tm8591   tm8591l;
label define tm8591l 
	0           "Not applicable"                
;
label values tm8592   tm8592l;
label define tm8592l 
	0           "Not applicable"                
;
label values tm8593   tm8593l;
label define tm8593l 
	0           "Not applicable"                
;
label values tm8594   tm8594l;
label define tm8594l 
	0           "Not applicable"                
;
label values tm8595   tm8595l;
label define tm8595l 
	0           "Not applicable"                
;
label values tm8596   tm8596l;
label define tm8596l 
	0           "Not applicable"                
;
label values tm8597   tm8597l;
label define tm8597l 
	0           "Not applicable"                
;
label values tm8598   tm8598l;
label define tm8598l 
	0           "Not applicable"                
;
label values tm8599   tm8599l;
label define tm8599l 
	0           "Not applicable"                
	1           "Yes - skip to TM8648"          
	2           "No"                            
;
label values tm8600   tm8600l;
label define tm8600l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8601   tm8601l;
label define tm8601l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8602   tm8602l;
label define tm8602l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8603   tm8603l;
label define tm8603l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8604   tm8604l;
label define tm8604l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8605   tm8605l;
label define tm8605l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8606   tm8606l;
label define tm8606l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8607   tm8607l;
label define tm8607l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8608   tm8608l;
label define tm8608l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8609   tm8609l;
label define tm8609l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8610   tm8610l;
label define tm8610l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8611   tm8611l;
label define tm8611l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8612   tm8612l;
label define tm8612l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8613   tm8613l;
label define tm8613l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8614   tm8614l;
label define tm8614l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8615   tm8615l;
label define tm8615l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8616   tm8616l;
label define tm8616l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8617   tm8617l;
label define tm8617l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8618   tm8618l;
label define tm8618l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8619   tm8619l;
label define tm8619l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8620   tm8620l;
label define tm8620l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8621   tm8621l;
label define tm8621l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8622   tm8622l;
label define tm8622l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8623   tm8623l;
label define tm8623l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8624   tm8624l;
label define tm8624l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8625   tm8625l;
label define tm8625l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8626   tm8626l;
label define tm8626l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8627   tm8627l;
label define tm8627l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8628   tm8628l;
label define tm8628l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8629   tm8629l;
label define tm8629l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8630   tm8630l;
label define tm8630l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8631   tm8631l;
label define tm8631l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8632   tm8632l;
label define tm8632l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8633   tm8633l;
label define tm8633l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8634   tm8634l;
label define tm8634l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8635   tm8635l;
label define tm8635l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8636   tm8636l;
label define tm8636l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8637   tm8637l;
label define tm8637l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8638   tm8638l;
label define tm8638l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8639   tm8639l;
label define tm8639l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8640   tm8640l;
label define tm8640l 
	0           "Not applicable"                
	1           "Yes - ask TM8600 through TM8639"
	2           "No  - skip to TM8648"          
;
label values tm8641   tm8641l;
label define tm8641l 
	0           "Not applicable"                
	1           "Yes - ask TM8600 through TM8639"
	2           "No  - skip to TM8648"          
;
label values tm8642   tm8642l;
label define tm8642l 
	0           "Not applicable"                
	1           "Yes - ask TM8600 through TM8639"
	2           "No  - skip to TM8648"          
;
label values tm8643   tm8643l;
label define tm8643l 
	0           "Not applicable"                
	1           "Yes - ask TM8600 through TM8639"
	2           "No  - skip to TM8648"          
;
label values tm8644   tm8644l;
label define tm8644l 
	0           "Not applicable"                
	1           "Yes - ask TM8600 through TM8639"
	2           "No  - skip to TM8648"          
;
label values tm8645   tm8645l;
label define tm8645l 
	0           "Not applicable"                
	1           "Yes - ask TM8600 through TM8639"
	2           "No  - skip to TM8648"          
;
label values tm8646   tm8646l;
label define tm8646l 
	0           "Not applicable"                
	1           "Yes - ask TM8600 through TM8639"
	2           "No  - skip to TM8648"          
;
label values tm8648   tm8648l;
label define tm8648l 
	0           "Not applicable"                
	1           "Legal paternity not established"
;
label values tm8649   tm8649l;
label define tm8649l 
	0           "Not applicable"                
	1           "Unable to locate parent"       
;
label values tm8650   tm8650l;
label define tm8650l 
	0           "Not applicable"                
	1           "Other parent unable to pay"    
;
label values tm8651   tm8651l;
label define tm8651l 
	0           "Not applicable"                
	1           "Final agreement pending"       
;
label values tm8652   tm8652l;
label define tm8652l 
	0           "Not applicable"                
	1           "Accepted property settlement in"
;
label values tm8653   tm8653l;
label define tm8653l 
	0           "Not applicable"                
	1           "Do not want a legal child"     
;
label values tm8654   tm8654l;
label define tm8654l 
	0           "Not applicable"                
	1           "Did not pursue award"          
;
label values tm8655   tm8655l;
label define tm8655l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8656   tm8656l;
label define tm8656l 
	0           "Not applicable"                
	1           "Same county / city"            
	2           "Same state (different county /"
	3           "Different state"               
	4           "Other parent now deceased -"   
	5           "Other"                         
	6           "Unknown - skip to check"       
;
label values tm8658   tm8658l;
label define tm8658l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8660   tm8660l;
label define tm8660l 
	0           "Not applicable"                
	1           "Respondent"                    
	2           "Other parent"                  
	3           "Both respondent and other"     
;
label values tm8662   tm8662l;
label define tm8662l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8676"           
;
label values tm8664   tm8664l;
label define tm8664l 
	0           "Not applicable"                
;
label values tm8666   tm8666l;
label define tm8666l 
	0           "Not applicable"                
	999999      "Dollars biweekly"              
;
label values tm8668   tm8668l;
label define tm8668l 
	0           "Not applicable"                
;
label values tm8670   tm8670l;
label define tm8670l 
	0           "Not applicable"                
;
label values tm8672   tm8672l;
label define tm8672l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm8674   tm8674l;
label define tm8674l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm8676   tm8676l;
label define tm8676l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T18"   
;
label values tm8678   tm8678l;
label define tm8678l 
	0           "Not applicable"                
	-1          "Dk"                            
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
	2           "No - skip to check item T18"   
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
	2           "No - skip to 12"               
;
label values tm8712   tm8712l;
label define tm8712l 
	0           "Not applicable"                
	1           "Male - skip to check item T27" 
	2           "Female"                        
;
label values tm8714   tm8714l;
label define tm8714l 
	0           "Not applicable"                
	1           "Never married"                 
	2           "All others - skip to"          
;
label values tm8715   tm8715l;
label define tm8715l 
	0           "Not applicable"                
;
label values tm8716   tm8716l;
label define tm8716l 
	0           "Not applicable"                
;
label values tm8717   tm8717l;
label define tm8717l 
	0           "Not applicable"                
;
label values tm8718   tm8718l;
label define tm8718l 
	0           "Not applicable"                
;
label values tm8719   tm8719l;
label define tm8719l 
	0           "Not applicable"                
;
label values tm8720   tm8720l;
label define tm8720l 
	0           "Not applicable"                
;
label values tm8721   tm8721l;
label define tm8721l 
	0           "Not applicable"                
;
label values tm8722   tm8722l;
label define tm8722l 
	0           "Not applicable"                
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
	-1          "Dk"                            
;
label values tm8732   tm8732l;
label define tm8732l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8733   tm8733l;
label define tm8733l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8734   tm8734l;
label define tm8734l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8735   tm8735l;
label define tm8735l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8736   tm8736l;
label define tm8736l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8737   tm8737l;
label define tm8737l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8738   tm8738l;
label define tm8738l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8739   tm8739l;
label define tm8739l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8740   tm8740l;
label define tm8740l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8741   tm8741l;
label define tm8741l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8742   tm8742l;
label define tm8742l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8743   tm8743l;
label define tm8743l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8744   tm8744l;
label define tm8744l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8745   tm8745l;
label define tm8745l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8746   tm8746l;
label define tm8746l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8747   tm8747l;
label define tm8747l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8748   tm8748l;
label define tm8748l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8749   tm8749l;
label define tm8749l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8750   tm8750l;
label define tm8750l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8751   tm8751l;
label define tm8751l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8752   tm8752l;
label define tm8752l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8753   tm8753l;
label define tm8753l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8754   tm8754l;
label define tm8754l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8755   tm8755l;
label define tm8755l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8756   tm8756l;
label define tm8756l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8757   tm8757l;
label define tm8757l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8758   tm8758l;
label define tm8758l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8759   tm8759l;
label define tm8759l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8760   tm8760l;
label define tm8760l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8761   tm8761l;
label define tm8761l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8762   tm8762l;
label define tm8762l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8763   tm8763l;
label define tm8763l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8764   tm8764l;
label define tm8764l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8765   tm8765l;
label define tm8765l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8766   tm8766l;
label define tm8766l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8767   tm8767l;
label define tm8767l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8768   tm8768l;
label define tm8768l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8769   tm8769l;
label define tm8769l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8770   tm8770l;
label define tm8770l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk check item T21b"            
;
label values tm8771   tm8771l;
label define tm8771l 
	0           "Not applicable"                
	1           "Yes - ask 6a through 6e for"   
	2           "No - skip to 9a"               
;
label values tm8772   tm8772l;
label define tm8772l 
	0           "Not applicable"                
	1           "Yes - ask 6a through 6e for"   
	2           "No - skip to 9a"               
;
label values tm8773   tm8773l;
label define tm8773l 
	0           "Not applicable"                
	1           "Yes - ask 6a through 6e for"   
	2           "No - skip to 9a"               
;
label values tm8774   tm8774l;
label define tm8774l 
	0           "Not applicable"                
	1           "Yes - ask 6a through 6e for"   
	2           "No - skip to 9a"               
;
label values tm8775   tm8775l;
label define tm8775l 
	0           "Not applicable"                
	1           "Yes - ask 6a through 6e for"   
	2           "No - skip to 9a"               
;
label values tm8776   tm8776l;
label define tm8776l 
	0           "Not applicable"                
	1           "Yes - ask 6a through 6e for"   
	2           "No - skip to 9a"               
;
label values tm8777   tm8777l;
label define tm8777l 
	0           "Not applicable"                
	1           "Yes - ask 6a through 6e for"   
	2           "No - skip to 9 a skip to 9a"   
;
label values tm8779   tm8779l;
label define tm8779l 
	0           "Not applicable"                
;
label values tm8780   tm8780l;
label define tm8780l 
	0           "Not applicable"                
;
label values tm8781   tm8781l;
label define tm8781l 
	0           "Not applicable"                
;
label values tm8782   tm8782l;
label define tm8782l 
	0           "Not applicable"                
;
label values tm8783   tm8783l;
label define tm8783l 
	0           "Not applicable"                
;
label values tm8784   tm8784l;
label define tm8784l 
	0           "Not applicable"                
;
label values tm8785   tm8785l;
label define tm8785l 
	0           "Not applicable"                
;
label values tm8786   tm8786l;
label define tm8786l 
	0           "Not applicable"                
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
	2           "No - skip to TM8805 for this"  
;
label values tm8796   tm8796l;
label define tm8796l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - skip to TM8805 for this"  
;
label values tm8797   tm8797l;
label define tm8797l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - skip to TM8805 for child" 
;
label values tm8798   tm8798l;
label define tm8798l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - skip to TM8805 for this"  
;
label values tm8799   tm8799l;
label define tm8799l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - skip to TM8805 for this"  
;
label values tm8800   tm8800l;
label define tm8800l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - skip to TM8805 for this"  
;
label values tm8801   tm8801l;
label define tm8801l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - skip to TM8805 for this"  
;
label values tm8802   tm8802l;
label define tm8802l 
	0           "Not applicable"                
	1           "Yes - if last child skip to"   
	2           "No - skip to TM8805 for child" 
;
label values tm8803   tm8803l;
label define tm8803l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to 9a and ask 9a"    
;
label values tm8804   tm8804l;
label define tm8804l 
	0           "Not applicable"                
	1           "Yes - skip to 9a and ask 9a"   
	2           "No - go to 7a for the next"    
;
label values tm8805   tm8805l;
label define tm8805l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8806   tm8806l;
label define tm8806l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8807   tm8807l;
label define tm8807l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8808   tm8808l;
label define tm8808l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8809   tm8809l;
label define tm8809l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8810   tm8810l;
label define tm8810l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8811   tm8811l;
label define tm8811l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8812   tm8812l;
label define tm8812l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8813   tm8813l;
label define tm8813l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8814   tm8814l;
label define tm8814l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8815   tm8815l;
label define tm8815l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8816   tm8816l;
label define tm8816l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8817   tm8817l;
label define tm8817l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8818   tm8818l;
label define tm8818l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8819   tm8819l;
label define tm8819l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8820   tm8820l;
label define tm8820l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8821   tm8821l;
label define tm8821l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8822   tm8822l;
label define tm8822l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8823   tm8823l;
label define tm8823l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8824   tm8824l;
label define tm8824l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8825   tm8825l;
label define tm8825l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8826   tm8826l;
label define tm8826l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8827   tm8827l;
label define tm8827l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8828   tm8828l;
label define tm8828l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8829   tm8829l;
label define tm8829l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8830   tm8830l;
label define tm8830l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8831   tm8831l;
label define tm8831l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8832   tm8832l;
label define tm8832l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8833   tm8833l;
label define tm8833l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8834   tm8834l;
label define tm8834l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8835   tm8835l;
label define tm8835l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8836   tm8836l;
label define tm8836l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8837   tm8837l;
label define tm8837l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8838   tm8838l;
label define tm8838l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8839   tm8839l;
label define tm8839l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8840   tm8840l;
label define tm8840l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8841   tm8841l;
label define tm8841l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8842   tm8842l;
label define tm8842l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8843   tm8843l;
label define tm8843l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk"                            
;
label values tm8844   tm8844l;
label define tm8844l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	-1          "Dk check item T24"             
;
label values tm8845   tm8845l;
label define tm8845l 
	0           "Not applicable"                
	1           "Yes - go to 7a for next child" 
	2           "No - skip to 9a"               
;
label values tm8846   tm8846l;
label define tm8846l 
	0           "Not applicable"                
	1           "Yes - go to 7a for next child" 
	2           "No"                            
;
label values tm8847   tm8847l;
label define tm8847l 
	0           "Not applicable"                
	1           "Yes - go to 7a for next child" 
	2           "No"                            
;
label values tm8848   tm8848l;
label define tm8848l 
	0           "Not applicable"                
	1           "Yes - go to 7a for next child" 
	2           "No"                            
;
label values tm8849   tm8849l;
label define tm8849l 
	0           "Not applicable"                
	1           "Yes - go to 7a for next child" 
	2           "No"                            
;
label values tm8850   tm8850l;
label define tm8850l 
	0           "Not applicable"                
	1           "Yes - go to 7a for next child" 
	2           "No"                            
;
label values tm8851   tm8851l;
label define tm8851l 
	0           "Not applicable"                
	1           "Yes - go to 7a for next child" 
	2           "No"                            
;
label values tm8853   tm8853l;
label define tm8853l 
	0           "Not applicable"                
	1           "Yes - skip to check item T26"  
	2           "No - skip to 8a"               
;
label values tm8854   tm8854l;
label define tm8854l 
	0           "Not applicable"                
	1           "Yes - skip to check item T26"  
	2           "No - skip to 8a"               
;
label values tm8855   tm8855l;
label define tm8855l 
	0           "Not applicable"                
	1           "Yes - skip to check item T26"  
	2           "No - skip to 8a"               
;
label values tm8856   tm8856l;
label define tm8856l 
	0           "Not applicable"                
	1           "Yes - skip to check item T26"  
	2           "No - skip to 8a"               
;
label values tm8857   tm8857l;
label define tm8857l 
	0           "Not applicable"                
	1           "Yes - skip to check item T26"  
	2           "No - skip to 8a"               
;
label values tm8858   tm8858l;
label define tm8858l 
	0           "Not applicable"                
	1           "Yes - skip to check item T26"  
	2           "No - skip to 8a"               
;
label values tm8859   tm8859l;
label define tm8859l 
	0           "Not applicable"                
	1           "Yes - skip to check item T26"  
	2           "No - skip to 8a"               
;
label values tm8862   tm8862l;
label define tm8862l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No check item T26"             
;
label values tm8864   tm8864l;
label define tm8864l 
	0           "Not applicable"                
	1           "Yes - ask 9a-9c for first child"
	2           "No - ask 9a-9c for first and"  
;
label values tm8866   tm8866l;
label define tm8866l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - ask 9a-9c for child marked"
;
label values tm8868   tm8868l;
label define tm8868l 
	0           "Not applicable"                
	1           "Yes - ask 9a-9c for youngest"  
	2           "No - ask 9a-9c for youngest and"
;
label values tm8869   tm8869l;
label define tm8869l 
	0           "Not applicable"                
;
label values tm8870   tm8870l;
label define tm8870l 
	0           "Not applicable"                
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
	1           "Same state (different county/" 
;
label values tm8890   tm8890l;
label define tm8890l 
	0           "Not applicable"                
	1           "Same state (different county/" 
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
	1           "Other parent deceased -"       
;
label values tm8894   tm8894l;
label define tm8894l 
	0           "Not applicable"                
	1           "Other parent deceased -"       
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
	-1          "Unknown"                       
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
	-1          "Dk"                            
;
label values tm8907   tm8907l;
label define tm8907l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm8908   tm8908l;
label define tm8908l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to 12"               
;
label values tm8909   tm8909l;
label define tm8909l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm8910   tm8910l;
label define tm8910l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9002   tm9002l;
label define tm9002l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to part e"           
	-1          "Dk - skip to part e"           
;
label values tm9004   tm9004l;
label define tm9004l 
	0           "Not applicable"                
	1           "Regular"                       
	2           "Lump-sum"                      
	3           "Both"                          
;
label values tm9006   tm9006l;
label define tm9006l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9007   tm9007l;
label define tm9007l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9008   tm9008l;
label define tm9008l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to 4d"               
;
label values tm9010   tm9010l;
label define tm9010l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9012   tm9012l;
label define tm9012l 
	0           "Not applicable"                
	1           "Voluntary written agreement"   
	2           "Court-ordered agreement"       
	3           "Other type of written agreement"
	4           "Non-written agreement"         
;
label values tm9014   tm9014l;
label define tm9014l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9016   tm9016l;
label define tm9016l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9020"           
	-1          "Dk - skip to TM9020"           
;
label values tm9018   tm9018l;
label define tm9018l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9019   tm9019l;
label define tm9019l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9020   tm9020l;
label define tm9020l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9022   tm9022l;
label define tm9022l 
	0           "Not applicable"                
	012800      "Amount paid"                   
	-1          "Dk"                            
;
label values tm9024   tm9024l;
label define tm9024l 
	0           "Not applicable"                
	1           "Through employment related wage"
	2           "Directly to the other parent"  
	3           "Directly to the court"         
	4           "Directly to a child support"   
	5           "Other"                         
	-1          "Dk"                            
;
label values tm9026   tm9026l;
label define tm9026l 
	0           "Not applicable"                
	1           "Non-custodial parent to provide"
;
label values tm9028   tm9028l;
label define tm9028l 
	0           "Not applicable"                
	1           "Custodial parent to provide"   
;
label values tm9030   tm9030l;
label define tm9030l 
	0           "Not applicable"                
	1           "Non-custodial parent to pay"   
;
label values tm9032   tm9032l;
label define tm9032l 
	0           "Not applicable"                
	1           "Child support payments to"     
;
label values tm9034   tm9034l;
label define tm9034l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm9036   tm9036l;
label define tm9036l 
	0           "Not applicable"                
	-3          "None"                          
;
label values tm9038   tm9038l;
label define tm9038l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9042"           
;
label values tm9040   tm9040l;
label define tm9040l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9042   tm9042l;
label define tm9042l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to part e"           
;
label values tm9044   tm9044l;
label define tm9044l 
	0           "Not applicable"                
	010600      "The amount of child support"   
	-1          "Dk"                            
;
label values tm9100   tm9100l;
label define tm9100l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
;
label values tm9102   tm9102l;
label define tm9102l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9104   tm9104l;
label define tm9104l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm9106   tm9106l;
label define tm9106l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9110"           
;
label values tm9108   tm9108l;
label define tm9108l 
	1           "Yes"                           
	2           "No"                            
;
label values tm9110   tm9110l;
label define tm9110l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty - skip to TM9114"
;
label values tm9112   tm9112l;
label define tm9112l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9114   tm9114l;
label define tm9114l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty - skip to TM9118"
;
label values tm9116   tm9116l;
label define tm9116l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9118   tm9118l;
label define tm9118l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty  - skip to TM9122"
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
	1           "Has difficulty"                
	2           "No diffculty- skip to TM9126"  
;
label values tm9124   tm9124l;
label define tm9124l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9126   tm9126l;
label define tm9126l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty - skip to TM9130"
;
label values tm9128   tm9128l;
label define tm9128l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9130   tm9130l;
label define tm9130l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty - skip to TM9134"
;
label values tm9132   tm9132l;
label define tm9132l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9134   tm9134l;
label define tm9134l 
	0           "Not applicable"                
	1           "Has difficulty"                
	2           "No difficulty - skip to TM9138"
;
label values tm9136   tm9136l;
label define tm9136l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9138   tm9138l;
label define tm9138l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9139"   
	2           "No difficulty"                 
;
label values tm9139   tm9139l;
label define tm9139l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9140   tm9140l;
label define tm9140l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9141"   
	2           "No difficulty"                 
;
label values tm9141   tm9141l;
label define tm9141l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9142   tm9142l;
label define tm9142l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9143"   
	2           "No difficulty"                 
;
label values tm9143   tm9143l;
label define tm9143l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9144   tm9144l;
label define tm9144l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM8845"   
	2           "No difficulty"                 
;
label values tm9145   tm9145l;
label define tm9145l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9146   tm9146l;
label define tm9146l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9147"   
	2           "No difficulty"                 
;
label values tm9147   tm9147l;
label define tm9147l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9148   tm9148l;
label define tm9148l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9149"   
	2           "No difficulty"                 
;
label values tm9149   tm9149l;
label define tm9149l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9150   tm9150l;
label define tm9150l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9151"   
	2           "No difficulty"                 
;
label values tm9151   tm9151l;
label define tm9151l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9152   tm9152l;
label define tm9152l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9153"   
	2           "No difficulty"                 
;
label values tm9153   tm9153l;
label define tm9153l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9154   tm9154l;
label define tm9154l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9155"   
	2           "No difficulty"                 
;
label values tm9155   tm9155l;
label define tm9155l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9156   tm9156l;
label define tm9156l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9157"   
	2           "No difficulty"                 
;
label values tm9157   tm9157l;
label define tm9157l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9158   tm9158l;
label define tm9158l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9159"   
	2           "No difficulty"                 
;
label values tm9159   tm9159l;
label define tm9159l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9160   tm9160l;
label define tm9160l 
	0           "Not applicable"                
	1           "Has difficulty - ask TM9161"   
	2           "No difficulty"                 
;
label values tm9161   tm9161l;
label define tm9161l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No check item T29"             
;
label values tm9162   tm9162l;
label define tm9162l 
	0           "Not applicable"                
	1           "Yes - go to TM9176"            
	2           "No - skip to TM9190"           
;
label values tm9176   tm9176l;
label define tm9176l 
	0           "Not applicable"                
	1           "Son"                           
	2           "Daughter"                      
	3           "Spouse"                        
	4           "Parent"                        
	5           "Other  relative nonrelative"   
	6           "Friend or neighbor"            
	7           "Paid help"                     
	8           "Other nonrelative"             
	9           "Did not receive help - skip to"
;
label values tm9178   tm9178l;
label define tm9178l 
	0           "Not applicable"                
	1           "Son"                           
	2           "Daughter"                      
	3           "Spouse"                        
	4           "Parent"                        
	5           "Other relative nonrelative"    
	6           "Friend or neighbor"            
	7           "Paid help"                     
	8           "Other nonrelative"             
;
label values tm9180   tm9180l;
label define tm9180l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm9182   tm9182l;
label define tm9182l 
	0           "Not applicable"                
	1           "Yes"                           
;
label values tm9183   tm9183l;
label define tm9183l 
	0           "Not applicable"                
;
label values tm9184   tm9184l;
label define tm9184l 
	0           "Not applicable"                
;
label values tm9185   tm9185l;
label define tm9185l 
	0           "Not applicable"                
	1           "No"                            
;
label values tm9186   tm9186l;
label define tm9186l 
	0           "Not applicable"                
	1           "No"                            
;
label values tm9187   tm9187l;
label define tm9187l 
	0           "Not applicable"                
	1           "Less than 6 months"            
	2           "6 to 11 months"                
	3           "1 to 2 years"                  
	4           "3 to 5 years"                  
	5           "More than 5 years"             
;
label values tm9188   tm9188l;
label define tm9188l 
	0           "Not applicable"                
	2           "No - skip to TM9192"           
	-1          "Don't know - skip to TM9192"   
;
label values tm9189   tm9189l;
label define tm9189l 
	0           "Not applicable"                
	999999      "Dollars"                       
	-1          "Dk"                            
;
label values tm9190   tm9190l;
label define tm9190l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No  - skip to TM9202"          
;
label values tm9192   tm9192l;
label define tm9192l 
	0           "Not applicable"                
;
label values tm9194   tm9194l;
label define tm9194l 
	0           "Not applicable"                
;
label values tm9196   tm9196l;
label define tm9196l 
	0           "Not applicable"                
;
label values tm9197   tm9197l;
label define tm9197l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No check item T31"             
;
label values tm9198   tm9198l;
label define tm9198l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -  skip to TM9202"          
;
label values tm9200   tm9200l;
label define tm9200l 
	0           "Not applicable"                
;
label values tm9202   tm9202l;
label define tm9202l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9204   tm9204l;
label define tm9204l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9206   tm9206l;
label define tm9206l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9208   tm9208l;
label define tm9208l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9210   tm9210l;
label define tm9210l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9212   tm9212l;
label define tm9212l 
	0           "Not applicable"                
	1           "15 years old - skip to TM8941" 
	2           "16 to 67 years old"            
	3           "68 years old or older - skip to"
;
label values tm9214   tm9214l;
label define tm9214l 
	0           "Not applicable"                
	1           "Yes - skip to TM9218"          
	2           "No"                            
;
label values tm9216   tm9216l;
label define tm9216l 
	0           "Not applicable"                
	1           "Item 18a is blank - skip to"   
	2           "Yes"                           
	3           "No - skip to TM9226"           
;
label values tm9218   tm9218l;
label define tm9218l 
	0           "Not applicable"                
	1           "Yes - skip to TM9222"          
	2           "No - skip to TM9226"           
;
label values tm9220   tm9220l;
label define tm9220l 
	0           "Not applicable"                
	1           "Yes - mark '171' on iss"       
	2           "No skip to TM9226"             
;
label values tm9222   tm9222l;
label define tm9222l 
	0           "Not applicable"                
	1           "Yes - skip to TM9226"          
	2           "No"                            
;
label values tm9224   tm9224l;
label define tm9224l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9226   tm9226l;
label define tm9226l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9230"           
;
label values tm9228   tm9228l;
label define tm9228l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9230   tm9230l;
label define tm9230l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -  skip to TM9300"          
;
label values tm9232   tm9232l;
label define tm9232l 
	0           "Not applicable"                
;
label values tm9234   tm9234l;
label define tm9234l 
	0           "Not applicable"                
;
label values tm9236   tm9236l;
label define tm9236l 
	0           "Not applicable"                
;
label values tm9238   tm9238l;
label define tm9238l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9300"           
;
label values tm9240   tm9240l;
label define tm9240l 
	0           "Not applicable"                
;
label values tm9242   tm9242l;
label define tm9242l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9300"           
;
label values tm9300   tm9300l;
label define tm9300l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9322"           
;
label values tm9302   tm9302l;
label define tm9302l 
	0           "Not applicable"                
	-1          "Don't know"                    
;
label values tm9304   tm9304l;
label define tm9304l 
	0           "Not applicable"                
	1           "Child birth"                   
;
label values tm9306   tm9306l;
label define tm9306l 
	0           "Not applicable"                
	1           "Surgery or operation(incl."    
;
label values tm9308   tm9308l;
label define tm9308l 
	0           "Not applicable"                
	1           "Other medical"                 
;
label values tm9310   tm9310l;
label define tm9310l 
	0           "Not applicable"                
	1           "Mental or emotional problem or"
;
label values tm9312   tm9312l;
label define tm9312l 
	0           "Not applicable"                
	1           "Drug or alcohol abuse problem" 
;
label values tm9314   tm9314l;
label define tm9314l 
	0           "Not applicable"                
	1           "Yes, military"                 
	2           "Yes, VA"                       
	3           "Yes, both military and va"     
	4           "No"                            
;
label values tm9316   tm9316l;
label define tm9316l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9318   tm9318l;
label define tm9318l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9320   tm9320l;
label define tm9320l 
	0           "Not applicable"                
	-5          "All nights"                    
	-3          "None"                          
	-1          "Dk"                            
;
label values tm9322   tm9322l;
label define tm9322l 
	0           "Not applicable"                
	-5          "All days"                      
	-3          "None"                          
	-1          "Dk"                            
;
label values tm9324   tm9324l;
label define tm9324l 
	0           "Not applicable"                
	-3          "None - skip to TM9327"         
	-1          "Dk"                            
;
label values tm9326   tm9326l;
label define tm9326l 
	0           "Not applicable"                
	-3          "None"                          
	-1          "Dk"                            
;
label values tm9327   tm9327l;
label define tm9327l 
	-3          "None - skip to TM9329"         
	-1          "Dk"                            
	0           "Not applicable"                
;
label values tm9328   tm9328l;
label define tm9328l 
	-3          "None"                          
	-1          "Don't know (Dk)"               
	0           "Not applicable"                
;
label values tm9329   tm9329l;
label define tm9329l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T38"   
;
label values tm9330   tm9330l;
label define tm9330l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic (not"
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9332   tm9332l;
label define tm9332l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T40"   
;
label values tm9333   tm9333l;
label define tm9333l 
	0           "Not applicable"                
	1           "Yes - skip to check item T41"  
	2           "No"                            
;
label values tm9334   tm9334l;
label define tm9334l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9336"           
;
label values tm9335   tm9335l;
label define tm9335l 
	0           "Not applicable"                
	1           "Yes - skip to check item T41"  
	2           "No"                            
;
label values tm9336   tm9336l;
label define tm9336l 
	0           "Not applicable"                
	1           "Correct"                       
	2           "Incorrect - covered by some"   
;
label values tm9338   tm9338l;
label define tm9338l 
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
label values tm9400   tm9400l;
label define tm9400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item TM7936"
;
label values tm9401   tm9401l;
label define tm9401l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item TM9437"
;
label values tm9402   tm9402l;
label define tm9402l 
	0           "Not applicable"                
;
label values tm9403   tm9403l;
label define tm9403l 
	0           "Not applicable"                
;
label values tm9404   tm9404l;
label define tm9404l 
	0           "Not applicable"                
;
label values tm9405   tm9405l;
label define tm9405l 
	0           "Not applicable"                
;
label values tm9406   tm9406l;
label define tm9406l 
	0           "Not applicable"                
;
label values tm9407   tm9407l;
label define tm9407l 
	0           "Not applicable"                
;
label values tm9408   tm9408l;
label define tm9408l 
	0           "Not applicable"                
;
label values tm9409   tm9409l;
label define tm9409l 
	0           "Not applicable"                
;
label values tm9410   tm9410l;
label define tm9410l 
	0           "Not applicable"                
;
label values tm9411   tm9411l;
label define tm9411l 
	0           "Not applicable"                
;
label values tm9412   tm9412l;
label define tm9412l 
	0           "Not applicable"                
;
label values tm9413   tm9413l;
label define tm9413l 
	0           "Not applicable"                
;
label values tm9414   tm9414l;
label define tm9414l 
	0           "Not applicable"                
;
label values tm9415   tm9415l;
label define tm9415l 
	0           "Not applicable"                
;
label values tm9416   tm9416l;
label define tm9416l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9417   tm9417l;
label define tm9417l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9418   tm9418l;
label define tm9418l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9419   tm9419l;
label define tm9419l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9420   tm9420l;
label define tm9420l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9421   tm9421l;
label define tm9421l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9422   tm9422l;
label define tm9422l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9423   tm9423l;
label define tm9423l 
	0           "Not applicable"                
	1           "Yes - skip to next child, or"  
	2           "No - skip to next child, or"   
;
label values tm9424   tm9424l;
label define tm9424l 
	0           "Not applicable"                
	1           "Yes - skip to next child, or"  
	2           "No - skip to next child, or"   
;
label values tm9425   tm9425l;
label define tm9425l 
	0           "Not applicable"                
	1           "Yes - skip to next child, or"  
	2           "No - skip to next child, or"   
;
label values tm9426   tm9426l;
label define tm9426l 
	0           "Not applicable"                
	1           "Yes - skip to next child, or"  
	2           "No - skip to next child, or"   
;
label values tm9427   tm9427l;
label define tm9427l 
	0           "Not applicable"                
	1           "Yes - skip to next child, or"  
	2           "No - skip to next child, or"   
;
label values tm9428   tm9428l;
label define tm9428l 
	0           "Not applicable"                
	1           "Yes - skip to next child, or"  
	2           "No - skip to next child, or"   
;
label values tm9429   tm9429l;
label define tm9429l 
	0           "Not applicable"                
	1           "Yes - skip to next child, or"  
	2           "No - skip to next child, or"   
;
label values tm9430   tm9430l;
label define tm9430l 
	0           "Not applicable"                
	1           "Yes - go to next child, or"    
	2           "No - go to next child, or check"
;
label values tm9431   tm9431l;
label define tm9431l 
	0           "Not applicable"                
	1           "Yes - go to next child, or"    
	2           "No - go to next child, or check"
;
label values tm9432   tm9432l;
label define tm9432l 
	0           "Not applicable"                
	1           "Yes - go to next child, or"    
	2           "No - go to next child, or check"
;
label values tm9433   tm9433l;
label define tm9433l 
	0           "Not applicable"                
	1           "Yes - go to next child, or"    
	2           "No - go to next child, or check"
;
label values tm9434   tm9434l;
label define tm9434l 
	0           "Not applicable"                
	1           "Yes - go to next child, or"    
	2           "No - go to next child, or check"
;
label values tm9435   tm9435l;
label define tm9435l 
	0           "Not applicable"                
	1           "Yes - go to next child, or"    
	2           "No - go to next child, or check"
;
label values tm9436   tm9436l;
label define tm9436l 
	0           "Not applicable"                
	1           "Yes - go to next child, or"    
	2           "No - go to next child, or check"
;
label values tm9737   tm9737l;
label define tm9737l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item TM9696"
;
label values tm9437   tm9437l;
label define tm9437l 
	0           "Not applicable"                
;
label values tm9438   tm9438l;
label define tm9438l 
	0           "Not applicable"                
;
label values tm9439   tm9439l;
label define tm9439l 
	0           "Not applicable"                
;
label values tm9440   tm9440l;
label define tm9440l 
	0           "Not applicable"                
;
label values tm9441   tm9441l;
label define tm9441l 
	0           "Not applicable"                
;
label values tm9442   tm9442l;
label define tm9442l 
	0           "Not applicable"                
;
label values tm9443   tm9443l;
label define tm9443l 
	0           "Not applicable"                
;
label values tm9444   tm9444l;
label define tm9444l 
	0           "Not applicable"                
;
label values tm9445   tm9445l;
label define tm9445l 
	0           "Not applicable"                
;
label values tm9446   tm9446l;
label define tm9446l 
	0           "Not applicable"                
;
label values tm9447   tm9447l;
label define tm9447l 
	0           "Not applicable"                
;
label values tm9448   tm9448l;
label define tm9448l 
	0           "Not applicable"                
;
label values tm9449   tm9449l;
label define tm9449l 
	0           "Not applicable"                
;
label values tm9450   tm9450l;
label define tm9450l 
	0           "Not applicable"                
;
label values tm9451   tm9451l;
label define tm9451l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9452   tm9452l;
label define tm9452l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9453   tm9453l;
label define tm9453l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9454   tm9454l;
label define tm9454l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9455   tm9455l;
label define tm9455l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9456   tm9456l;
label define tm9456l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9457   tm9457l;
label define tm9457l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9458   tm9458l;
label define tm9458l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9459   tm9459l;
label define tm9459l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9460   tm9460l;
label define tm9460l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9461   tm9461l;
label define tm9461l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9462   tm9462l;
label define tm9462l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9463   tm9463l;
label define tm9463l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9464   tm9464l;
label define tm9464l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9465   tm9465l;
label define tm9465l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9466   tm9466l;
label define tm9466l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9467   tm9467l;
label define tm9467l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9468   tm9468l;
label define tm9468l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9469   tm9469l;
label define tm9469l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9470   tm9470l;
label define tm9470l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9471   tm9471l;
label define tm9471l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9472   tm9472l;
label define tm9472l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9473   tm9473l;
label define tm9473l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9474   tm9474l;
label define tm9474l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9475   tm9475l;
label define tm9475l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9476   tm9476l;
label define tm9476l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9477   tm9477l;
label define tm9477l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9478   tm9478l;
label define tm9478l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9479   tm9479l;
label define tm9479l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9480   tm9480l;
label define tm9480l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9481   tm9481l;
label define tm9481l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9482   tm9482l;
label define tm9482l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9483   tm9483l;
label define tm9483l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9484   tm9484l;
label define tm9484l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9485   tm9485l;
label define tm9485l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9486   tm9486l;
label define tm9486l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9500"           
;
label values tm9487   tm9487l;
label define tm9487l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9500"           
;
label values tm9488   tm9488l;
label define tm9488l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9500"           
;
label values tm9489   tm9489l;
label define tm9489l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9500"           
;
label values tm9490   tm9490l;
label define tm9490l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9500"           
;
label values tm9491   tm9491l;
label define tm9491l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9500"           
;
label values tm9492   tm9492l;
label define tm9492l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9500"           
;
label values tm9493   tm9493l;
label define tm9493l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9494   tm9494l;
label define tm9494l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9495   tm9495l;
label define tm9495l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9496   tm9496l;
label define tm9496l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9497   tm9497l;
label define tm9497l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9498   tm9498l;
label define tm9498l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9499   tm9499l;
label define tm9499l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9500   tm9500l;
label define tm9500l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9501   tm9501l;
label define tm9501l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9502   tm9502l;
label define tm9502l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9503   tm9503l;
label define tm9503l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9504   tm9504l;
label define tm9504l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9505   tm9505l;
label define tm9505l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9506   tm9506l;
label define tm9506l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9507   tm9507l;
label define tm9507l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9508   tm9508l;
label define tm9508l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9509   tm9509l;
label define tm9509l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9510   tm9510l;
label define tm9510l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9511   tm9511l;
label define tm9511l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9512   tm9512l;
label define tm9512l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9513   tm9513l;
label define tm9513l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9514   tm9514l;
label define tm9514l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9535"           
;
label values tm9515   tm9515l;
label define tm9515l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9535"           
;
label values tm9516   tm9516l;
label define tm9516l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9535"           
;
label values tm9517   tm9517l;
label define tm9517l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9535"           
;
label values tm9518   tm9518l;
label define tm9518l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9535"           
;
label values tm9519   tm9519l;
label define tm9519l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9535"           
;
label values tm9520   tm9520l;
label define tm9520l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9535"           
;
label values tm9521   tm9521l;
label define tm9521l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9522   tm9522l;
label define tm9522l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9523   tm9523l;
label define tm9523l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9524   tm9524l;
label define tm9524l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9525   tm9525l;
label define tm9525l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9526   tm9526l;
label define tm9526l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9527   tm9527l;
label define tm9527l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9528   tm9528l;
label define tm9528l 
	0           "Not applicable"                
;
label values tm9529   tm9529l;
label define tm9529l 
	0           "Not applicable"                
;
label values tm9530   tm9530l;
label define tm9530l 
	0           "Not applicable"                
;
label values tm9531   tm9531l;
label define tm9531l 
	0           "Not applicable"                
;
label values tm9532   tm9532l;
label define tm9532l 
	0           "Not applicable"                
;
label values tm9533   tm9533l;
label define tm9533l 
	0           "Not applicable"                
;
label values tm9534   tm9534l;
label define tm9534l 
	0           "Not applicable"                
;
label values tm9535   tm9535l;
label define tm9535l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9549"           
;
label values tm9536   tm9536l;
label define tm9536l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9549"           
;
label values tm9537   tm9537l;
label define tm9537l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9549"           
;
label values tm9538   tm9538l;
label define tm9538l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9549"           
;
label values tm9539   tm9539l;
label define tm9539l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9549"           
;
label values tm9540   tm9540l;
label define tm9540l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9549"           
;
label values tm9541   tm9541l;
label define tm9541l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9549"           
;
label values tm9542   tm9542l;
label define tm9542l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9543   tm9543l;
label define tm9543l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9544   tm9544l;
label define tm9544l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9545   tm9545l;
label define tm9545l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9546   tm9546l;
label define tm9546l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9547   tm9547l;
label define tm9547l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9548   tm9548l;
label define tm9548l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9549   tm9549l;
label define tm9549l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9563"           
;
label values tm9550   tm9550l;
label define tm9550l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9563"           
;
label values tm9551   tm9551l;
label define tm9551l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9563"           
;
label values tm9552   tm9552l;
label define tm9552l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9563"           
;
label values tm9553   tm9553l;
label define tm9553l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9563"           
;
label values tm9554   tm9554l;
label define tm9554l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9563"           
;
label values tm9555   tm9555l;
label define tm9555l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9563"           
;
label values tm9556   tm9556l;
label define tm9556l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9557   tm9557l;
label define tm9557l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9558   tm9558l;
label define tm9558l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9559   tm9559l;
label define tm9559l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9560   tm9560l;
label define tm9560l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9561   tm9561l;
label define tm9561l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9562   tm9562l;
label define tm9562l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9563   tm9563l;
label define tm9563l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9564   tm9564l;
label define tm9564l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9565   tm9565l;
label define tm9565l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9566   tm9566l;
label define tm9566l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9567   tm9567l;
label define tm9567l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9568   tm9568l;
label define tm9568l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9569   tm9569l;
label define tm9569l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9570   tm9570l;
label define tm9570l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9584"           
;
label values tm9571   tm9571l;
label define tm9571l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9585"           
;
label values tm9572   tm9572l;
label define tm9572l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9586"           
;
label values tm9573   tm9573l;
label define tm9573l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9587"           
;
label values tm9574   tm9574l;
label define tm9574l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9588"           
;
label values tm9575   tm9575l;
label define tm9575l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9589"           
;
label values tm9576   tm9576l;
label define tm9576l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9590"           
;
label values tm9577   tm9577l;
label define tm9577l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9578   tm9578l;
label define tm9578l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9579   tm9579l;
label define tm9579l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9580   tm9580l;
label define tm9580l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9581   tm9581l;
label define tm9581l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9582   tm9582l;
label define tm9582l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9583   tm9583l;
label define tm9583l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9584   tm9584l;
label define tm9584l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9598"           
;
label values tm9585   tm9585l;
label define tm9585l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9599"           
;
label values tm9586   tm9586l;
label define tm9586l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9600"           
;
label values tm9587   tm9587l;
label define tm9587l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9601"           
;
label values tm9588   tm9588l;
label define tm9588l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9602"           
;
label values tm9589   tm9589l;
label define tm9589l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9603"           
;
label values tm9590   tm9590l;
label define tm9590l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9604"           
;
label values tm9591   tm9591l;
label define tm9591l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9592   tm9592l;
label define tm9592l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9593   tm9593l;
label define tm9593l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9594   tm9594l;
label define tm9594l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9595   tm9595l;
label define tm9595l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9596   tm9596l;
label define tm9596l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9597   tm9597l;
label define tm9597l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9598   tm9598l;
label define tm9598l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9619"           
;
label values tm9599   tm9599l;
label define tm9599l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9620"           
;
label values tm9600   tm9600l;
label define tm9600l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9621"           
;
label values tm9601   tm9601l;
label define tm9601l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9622"           
;
label values tm9602   tm9602l;
label define tm9602l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9623"           
;
label values tm9603   tm9603l;
label define tm9603l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9624"           
;
label values tm9604   tm9604l;
label define tm9604l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9625"           
;
label values tm9605   tm9605l;
label define tm9605l 
	0           "Not applicable"                
;
label values tm9606   tm9606l;
label define tm9606l 
	0           "Not applicable"                
;
label values tm9607   tm9607l;
label define tm9607l 
	0           "Not applicable"                
;
label values tm9608   tm9608l;
label define tm9608l 
	0           "Not applicable"                
;
label values tm9609   tm9609l;
label define tm9609l 
	0           "Not applicable"                
;
label values tm9610   tm9610l;
label define tm9610l 
	0           "Not applicable"                
;
label values tm9611   tm9611l;
label define tm9611l 
	0           "Not applicable"                
;
label values tm9612   tm9612l;
label define tm9612l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9613   tm9613l;
label define tm9613l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9614   tm9614l;
label define tm9614l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9615   tm9615l;
label define tm9615l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9616   tm9616l;
label define tm9616l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9617   tm9617l;
label define tm9617l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9618   tm9618l;
label define tm9618l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9619   tm9619l;
label define tm9619l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9633"           
;
label values tm9620   tm9620l;
label define tm9620l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9634"           
;
label values tm9621   tm9621l;
label define tm9621l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9635"           
;
label values tm9622   tm9622l;
label define tm9622l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9636"           
;
label values tm9623   tm9623l;
label define tm9623l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9637"           
;
label values tm9624   tm9624l;
label define tm9624l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9638"           
;
label values tm9625   tm9625l;
label define tm9625l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9639"           
;
label values tm9626   tm9626l;
label define tm9626l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9627   tm9627l;
label define tm9627l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9628   tm9628l;
label define tm9628l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9629   tm9629l;
label define tm9629l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9630   tm9630l;
label define tm9630l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9631   tm9631l;
label define tm9631l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9632   tm9632l;
label define tm9632l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9633   tm9633l;
label define tm9633l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9647"           
;
label values tm9634   tm9634l;
label define tm9634l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9648"           
;
label values tm9635   tm9635l;
label define tm9635l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9649"           
;
label values tm9636   tm9636l;
label define tm9636l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9650"           
;
label values tm9637   tm9637l;
label define tm9637l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9651"           
;
label values tm9638   tm9638l;
label define tm9638l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9652"           
;
label values tm9639   tm9639l;
label define tm9639l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9653"           
;
label values tm9640   tm9640l;
label define tm9640l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9641   tm9641l;
label define tm9641l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9642   tm9642l;
label define tm9642l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9643   tm9643l;
label define tm9643l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9644   tm9644l;
label define tm9644l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9645   tm9645l;
label define tm9645l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9646   tm9646l;
label define tm9646l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9647   tm9647l;
label define tm9647l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T47"   
;
label values tm9648   tm9648l;
label define tm9648l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T47"   
;
label values tm9649   tm9649l;
label define tm9649l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T47"   
;
label values tm9650   tm9650l;
label define tm9650l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T47"   
;
label values tm9651   tm9651l;
label define tm9651l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T47"   
;
label values tm9652   tm9652l;
label define tm9652l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T47"   
;
label values tm9653   tm9653l;
label define tm9653l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T47"   
;
label values tm9654   tm9654l;
label define tm9654l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9655   tm9655l;
label define tm9655l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9656   tm9656l;
label define tm9656l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9657   tm9657l;
label define tm9657l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9658   tm9658l;
label define tm9658l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9659   tm9659l;
label define tm9659l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9660   tm9660l;
label define tm9660l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9661   tm9661l;
label define tm9661l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM9451 for next"    
;
label values tm9662   tm9662l;
label define tm9662l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM9452 for next"    
;
label values tm9663   tm9663l;
label define tm9663l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM9453 for next"    
;
label values tm9664   tm9664l;
label define tm9664l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM9454 for next"    
;
label values tm9665   tm9665l;
label define tm9665l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM9455 for next"    
;
label values tm9666   tm9666l;
label define tm9666l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to TM9456 for next"    
;
label values tm9667   tm9667l;
label define tm9667l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T48"   
;
label values tm9668   tm9668l;
label define tm9668l 
	0           "Not applicable"                
;
label values tm9669   tm9669l;
label define tm9669l 
	0           "Not applicable"                
;
label values tm9670   tm9670l;
label define tm9670l 
	0           "Not applicable"                
;
label values tm9671   tm9671l;
label define tm9671l 
	0           "Not applicable"                
;
label values tm9672   tm9672l;
label define tm9672l 
	0           "Not applicable"                
;
label values tm9673   tm9673l;
label define tm9673l 
	0           "Not applicable"                
;
label values tm9674   tm9674l;
label define tm9674l 
	0           "Not applicable"                
;
label values tm9675   tm9675l;
label define tm9675l 
	0           "Not applicable"                
;
label values tm9676   tm9676l;
label define tm9676l 
	0           "Not applicable"                
;
label values tm9677   tm9677l;
label define tm9677l 
	0           "Not applicable"                
;
label values tm9678   tm9678l;
label define tm9678l 
	0           "Not applicable"                
;
label values tm9679   tm9679l;
label define tm9679l 
	0           "Not applicable"                
;
label values tm9680   tm9680l;
label define tm9680l 
	0           "Not applicable"                
;
label values tm9681   tm9681l;
label define tm9681l 
	0           "Not applicable"                
;
label values tm9682   tm9682l;
label define tm9682l 
	0           "Not applicable"                
;
label values tm9683   tm9683l;
label define tm9683l 
	0           "Not applicable"                
;
label values tm9684   tm9684l;
label define tm9684l 
	0           "Not applicable"                
;
label values tm9685   tm9685l;
label define tm9685l 
	0           "Not applicable"                
;
label values tm9686   tm9686l;
label define tm9686l 
	0           "Not applicable"                
;
label values tm9687   tm9687l;
label define tm9687l 
	0           "Not applicable"                
;
label values tm9688   tm9688l;
label define tm9688l 
	0           "Not applicable"                
;
label values tm9689   tm9689l;
label define tm9689l 
	0           "Not applicable"                
	1           "Yes - go to TM9451 for next"   
	2           "No - go to TM9451 for next"    
;
label values tm9690   tm9690l;
label define tm9690l 
	0           "Not applicable"                
	1           "Yes - go to TM9452 for next"   
	2           "No - go to TM9452 for next"    
;
label values tm9691   tm9691l;
label define tm9691l 
	0           "Not applicable"                
	1           "Yes - go to TM9453 for next"   
	2           "No - go to TM9453 for next"    
;
label values tm9692   tm9692l;
label define tm9692l 
	0           "Not applicable"                
	1           "Yes - go to TM9454 for next"   
	2           "No - go to TM9454 for next"    
;
label values tm9693   tm9693l;
label define tm9693l 
	0           "Not applicable"                
	1           "Yes - go to TM9455 for next"   
	2           "No - go to TM9455 for next"    
;
label values tm9694   tm9694l;
label define tm9694l 
	0           "Not applicable"                
	1           "Yes - go to TM9456 for next"   
	2           "No - go to TM9456 for next"    
;
label values tm9695   tm9695l;
label define tm9695l 
	0           "Not applicable"                
	1           "Yes - go to check item T48"    
	2           "No - go to check item T48"     
;
label values tm9696   tm9696l;
label define tm9696l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T51"   
;
label values tm9697   tm9697l;
label define tm9697l 
	0           "Not applicable"                
;
label values tm9698   tm9698l;
label define tm9698l 
	0           "Not applicable"                
;
label values tm9699   tm9699l;
label define tm9699l 
	0           "Not applicable"                
;
label values tm9700   tm9700l;
label define tm9700l 
	0           "Not applicable"                
;
label values tm9701   tm9701l;
label define tm9701l 
	0           "Not applicable"                
;
label values tm9702   tm9702l;
label define tm9702l 
	0           "Not applicable"                
;
label values tm9703   tm9703l;
label define tm9703l 
	0           "Not applicable"                
;
label values tm9704   tm9704l;
label define tm9704l 
	0           "Not applicable"                
;
label values tm9705   tm9705l;
label define tm9705l 
	0           "Not applicable"                
;
label values tm9706   tm9706l;
label define tm9706l 
	0           "Not applicable"                
;
label values tm9707   tm9707l;
label define tm9707l 
	0           "Not applicable"                
;
label values tm9708   tm9708l;
label define tm9708l 
	0           "Not applicable"                
;
label values tm9709   tm9709l;
label define tm9709l 
	0           "Not applicable"                
;
label values tm9710   tm9710l;
label define tm9710l 
	0           "Not applicable"                
;
label values tm9711   tm9711l;
label define tm9711l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9712   tm9712l;
label define tm9712l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9713   tm9713l;
label define tm9713l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9714   tm9714l;
label define tm9714l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9715   tm9715l;
label define tm9715l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9716   tm9716l;
label define tm9716l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9717   tm9717l;
label define tm9717l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9718   tm9718l;
label define tm9718l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to next child, or"     
;
label values tm9719   tm9719l;
label define tm9719l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to next child, or"     
;
label values tm9720   tm9720l;
label define tm9720l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to next child, or"     
;
label values tm9721   tm9721l;
label define tm9721l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to next child, or"     
;
label values tm9722   tm9722l;
label define tm9722l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to next child, or"     
;
label values tm9723   tm9723l;
label define tm9723l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to next child, or"     
;
label values tm9724   tm9724l;
label define tm9724l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - go to next child, or"     
;
label values tm9725   tm9725l;
label define tm9725l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9726   tm9726l;
label define tm9726l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9727   tm9727l;
label define tm9727l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9728   tm9728l;
label define tm9728l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9729   tm9729l;
label define tm9729l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9730   tm9730l;
label define tm9730l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9731   tm9731l;
label define tm9731l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm9800   tm9800l;
label define tm9800l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7000"           
;
label values tm9801   tm9801l;
label define tm9801l 
	0           "Not applicable"                
;
label values tm9802   tm9802l;
label define tm9802l 
	0           "Not applicable"                
;
label values tm9803   tm9803l;
label define tm9803l 
	0           "Not applicable"                
;
label values tm9804   tm9804l;
label define tm9804l 
	0           "Not applicable"                
;
label values tm9805   tm9805l;
label define tm9805l 
	0           "Not applicable"                
;
label values tm9806   tm9806l;
label define tm9806l 
	0           "Not applicable"                
;
label values tm9807   tm9807l;
label define tm9807l 
	0           "Not applicable"                
;
label values tm9808   tm9808l;
label define tm9808l 
	0           "Not applicable"                
;
label values tm9809   tm9809l;
label define tm9809l 
	0           "Not applicable"                
;
label values tm9810   tm9810l;
label define tm9810l 
	0           "Not applicable"                
;
label values tm9811   tm9811l;
label define tm9811l 
	0           "Not applicable"                
;
label values tm9812   tm9812l;
label define tm9812l 
	0           "Not applicable"                
;
label values tm9813   tm9813l;
label define tm9813l 
	0           "Not applicable"                
;
label values tm9814   tm9814l;
label define tm9814l 
	0           "Not applicable"                
;
label values tm9815   tm9815l;
label define tm9815l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9850"           
;
label values tm9816   tm9816l;
label define tm9816l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9851"           
;
label values tm9817   tm9817l;
label define tm9817l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9852"           
;
label values tm9818   tm9818l;
label define tm9818l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9853"           
;
label values tm9819   tm9819l;
label define tm9819l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9854"           
;
label values tm9820   tm9820l;
label define tm9820l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9855"           
;
label values tm9821   tm9821l;
label define tm9821l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9856"           
;
label values tm9822   tm9822l;
label define tm9822l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9823   tm9823l;
label define tm9823l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9824   tm9824l;
label define tm9824l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9825   tm9825l;
label define tm9825l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9826   tm9826l;
label define tm9826l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9827   tm9827l;
label define tm9827l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9828   tm9828l;
label define tm9828l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9829   tm9829l;
label define tm9829l 
	0           "Not applicable"                
	1           "Surgery or operation (include" 
	2           "Other medical"                 
	3           "Mental or emotional problem or"
	4           "Drug or alcohol abuse problem" 
	5           "Child birth"                   
;
label values tm9830   tm9830l;
label define tm9830l 
	0           "Not applicable"                
	1           "Surgery or operation (include" 
	2           "Other medical"                 
	3           "Mental or emotional problem or"
	4           "Drug or alcohol abuse problem" 
	5           "Child birth"                   
;
label values tm9831   tm9831l;
label define tm9831l 
	0           "Not applicable"                
	1           "Surgery or operation (include" 
	2           "Other medical"                 
	3           "Mental or emotional problem or"
	4           "Drug or alcohol abuse problem" 
	5           "Child birth"                   
;
label values tm9832   tm9832l;
label define tm9832l 
	0           "Not applicable"                
	1           "Surgery or operation (include" 
	2           "Other medical"                 
	3           "Mental or emotional problem or"
	4           "Drug or alcohol abuse problem" 
	5           "Child birth"                   
;
label values tm9833   tm9833l;
label define tm9833l 
	0           "Not applicable"                
	1           "Surgery or operation (include" 
	2           "Other medical"                 
	3           "Mental or emotional problem or"
	4           "Drug or alcohol abuse problem" 
	5           "Child birth"                   
;
label values tm9834   tm9834l;
label define tm9834l 
	0           "Not applicable"                
	1           "Surgery or operation (include" 
	2           "Other medical"                 
	3           "Mental or emotional problem or"
	4           "Drug or alcohol abuse problem" 
	5           "Child birth"                   
;
label values tm9835   tm9835l;
label define tm9835l 
	0           "Not applicable"                
	1           "Surgery or operation (include" 
	2           "Other medical"                 
	3           "Mental or emotional problem or"
	4           "Drug or alcohol abuse problem" 
	5           "Child birth"                   
;
label values tm9836   tm9836l;
label define tm9836l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9837   tm9837l;
label define tm9837l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9838   tm9838l;
label define tm9838l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9839   tm9839l;
label define tm9839l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9840   tm9840l;
label define tm9840l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9841   tm9841l;
label define tm9841l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9842   tm9842l;
label define tm9842l 
	0           "Not applicable"                
	-1          "Dk"                            
;
label values tm9843   tm9843l;
label define tm9843l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All nights"                    
;
label values tm9844   tm9844l;
label define tm9844l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All nights"                    
;
label values tm9845   tm9845l;
label define tm9845l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All nights"                    
;
label values tm9846   tm9846l;
label define tm9846l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All nights"                    
;
label values tm9847   tm9847l;
label define tm9847l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All nights"                    
;
label values tm9848   tm9848l;
label define tm9848l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All nights"                    
;
label values tm9849   tm9849l;
label define tm9849l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All nights"                    
;
label values tm9850   tm9850l;
label define tm9850l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All days"                      
;
label values tm9851   tm9851l;
label define tm9851l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All days"                      
;
label values tm9852   tm9852l;
label define tm9852l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All days"                      
;
label values tm9853   tm9853l;
label define tm9853l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All days"                      
;
label values tm9854   tm9854l;
label define tm9854l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All days"                      
;
label values tm9855   tm9855l;
label define tm9855l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All days"                      
;
label values tm9856   tm9856l;
label define tm9856l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
	-5          "All days"                      
;
label values tm9857   tm9857l;
label define tm9857l 
	0           "Not applicable"                
;
label values tm9858   tm9858l;
label define tm9858l 
	0           "Not applicable"                
;
label values tm9859   tm9859l;
label define tm9859l 
	0           "Not applicable"                
;
label values tm9860   tm9860l;
label define tm9860l 
	0           "Not applicable"                
;
label values tm9861   tm9861l;
label define tm9861l 
	0           "Not applicable"                
;
label values tm9862   tm9862l;
label define tm9862l 
	0           "Not applicable"                
;
label values tm9863   tm9863l;
label define tm9863l 
	0           "Not applicable"                
;
label values tm9864   tm9864l;
label define tm9864l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9878"         
;
label values tm9865   tm9865l;
label define tm9865l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9879"         
;
label values tm9866   tm9866l;
label define tm9866l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9880"         
;
label values tm9867   tm9867l;
label define tm9867l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9881"         
;
label values tm9868   tm9868l;
label define tm9868l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9882"         
;
label values tm9869   tm9869l;
label define tm9869l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9883"         
;
label values tm9870   tm9870l;
label define tm9870l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9884"         
;
label values tm9871   tm9871l;
label define tm9871l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9872   tm9872l;
label define tm9872l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9873   tm9873l;
label define tm9873l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9874   tm9874l;
label define tm9874l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9875   tm9875l;
label define tm9875l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9876   tm9876l;
label define tm9876l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9877   tm9877l;
label define tm9877l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9878   tm9878l;
label define tm9878l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9892"         
;
label values tm9879   tm9879l;
label define tm9879l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9893"         
;
label values tm9880   tm9880l;
label define tm9880l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9894"         
;
label values tm9881   tm9881l;
label define tm9881l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9895"         
;
label values tm9882   tm9882l;
label define tm9882l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9896"         
;
label values tm9883   tm9883l;
label define tm9883l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9897"         
;
label values tm9884   tm9884l;
label define tm9884l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None - skip to TM9898"         
;
label values tm9885   tm9885l;
label define tm9885l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9886   tm9886l;
label define tm9886l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9887   tm9887l;
label define tm9887l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9888   tm9888l;
label define tm9888l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9889   tm9889l;
label define tm9889l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9890   tm9890l;
label define tm9890l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9891   tm9891l;
label define tm9891l 
	0           "Not applicable"                
	-1          "Dk"                            
	-3          "None"                          
;
label values tm9892   tm9892l;
label define tm9892l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T53"   
;
label values tm9893   tm9893l;
label define tm9893l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T53"   
;
label values tm9894   tm9894l;
label define tm9894l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T53"   
;
label values tm9895   tm9895l;
label define tm9895l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T53"   
;
label values tm9896   tm9896l;
label define tm9896l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T53"   
;
label values tm9897   tm9897l;
label define tm9897l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T53"   
;
label values tm9898   tm9898l;
label define tm9898l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T53"   
;
label values tm9899   tm9899l;
label define tm9899l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic (not"
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9900   tm9900l;
label define tm9900l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic (not"
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9901   tm9901l;
label define tm9901l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic (not"
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9902   tm9902l;
label define tm9902l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic (not"
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9903   tm9903l;
label define tm9903l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic (not"
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9904   tm9904l;
label define tm9904l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic"    
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9905   tm9905l;
label define tm9905l 
	0           "Not applicable"                
	1           "Doctor's office (or HMO)"      
	2           "VA hospital"                   
	3           "Military hospital"             
	4           "Hospital outpatient clinic"    
	5           "Hospital emergency room"       
	6           "Company or industry clinic"    
	7           "Health center (neighborhood"   
	8           "Psychiatric clinic"            
	9           "Psychiatric hospital"          
	10          "Private practice psychiatrist" 
	11          "Other"                         
;
label values tm9906   tm9906l;
label define tm9906l 
	0           "Not applicable"                
	1           "Yes - ask items 1a through 5b" 
	2           "No - skip to check item T54"   
;
label values tm9907   tm9907l;
label define tm9907l 
	0           "Not applicable"                
	1           "Yes - ask items 1a through 5b" 
	2           "No - skip to check item T54"   
;
label values tm9908   tm9908l;
label define tm9908l 
	0           "Not applicable"                
	1           "Yes - ask items 1a through 5b" 
	2           "No - skip to check item T54"   
;
label values tm9909   tm9909l;
label define tm9909l 
	0           "Not applicable"                
	1           "Yes - ask items 1a through 5b" 
	2           "No - skip to check item T54"   
;
label values tm9910   tm9910l;
label define tm9910l 
	0           "Not applicable"                
	1           "Yes - ask items 1a through 5b" 
	2           "No - skip to check item T54"   
;
label values tm9911   tm9911l;
label define tm9911l 
	0           "Not applicable"                
	1           "Yes - ask items 1a through 5b" 
	2           "No - skip to check item T54"   
;
label values tm9913   tm9913l;
label define tm9913l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM9915"           
;
label values tm9914   tm9914l;
label define tm9914l 
	0           "Not applicable"                
	1           "Yes - skip to TM7000"          
	2           "No"                            
;
label values tm9915   tm9915l;
label define tm9915l 
	0           "Not applicable"                
	1           "Yes - skip to TM7000"          
	2           "No"                            
;
label values tm9916   tm9916l;
label define tm9916l 
	0           "Not applicable"                
	1           "Correct"                       
	2           "Incorrect - covered by some"   
;
label values tm9917   tm9917l;
label define tm9917l 
	0           "Not applicable"                
	1           "Job layoff, job loss, or any"  
	2           "Employer does not offer health"
	3           "Can't obtain health insurance" 
	4           "Too expensive; can't afford"   
	5           "Don't believe in health"       
	6           "Have been healthy; not much"   
	7           "Able to go to VA or military"  
	8           "Covered by some other health"  
	9           "Other"                         
;
label values tm7000   tm7000l;
label define tm7000l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T77"   
;
label values tm7001   tm7001l;
label define tm7001l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T68"   
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
	-9          "No response"                   
;
label values tm7017   tm7017l;
label define tm7017l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "No response"                   
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
	-9          "No response"                   
;
label values tm7022   tm7022l;
label define tm7022l 
	0           "Not applicable"                
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
	-9          "No response"                   
;
label values tm7023   tm7023l;
label define tm7023l 
	0           "Not applicable"                
;
label values tm7024   tm7024l;
label define tm7024l 
	0           "Not applicable"                
;
label values tm7025   tm7025l;
label define tm7025l 
	0           "Not applicable"                
;
label values tm7026   tm7026l;
label define tm7026l 
	0           "Not applicable"                
;
label values tm7027   tm7027l;
label define tm7027l 
	0           "Not applicable"                
;
label values tm7028   tm7028l;
label define tm7028l 
	0           "Not applicable"                
;
label values tm7029   tm7029l;
label define tm7029l 
	0           "Not applicable"                
;
label values tm7030   tm7030l;
label define tm7030l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7031   tm7031l;
label define tm7031l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7032   tm7032l;
label define tm7032l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7033   tm7033l;
label define tm7033l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7034   tm7034l;
label define tm7034l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7035   tm7035l;
label define tm7035l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7036   tm7036l;
label define tm7036l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7037   tm7037l;
label define tm7037l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7038   tm7038l;
label define tm7038l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7039   tm7039l;
label define tm7039l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7040   tm7040l;
label define tm7040l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7041   tm7041l;
label define tm7041l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7042   tm7042l;
label define tm7042l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7043   tm7043l;
label define tm7043l 
	0           "Not applicable"                
	-1          "Dk"                            
	-9          "No response"                   
;
label values tm7044   tm7044l;
label define tm7044l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T61"   
;
label values tm7045   tm7045l;
label define tm7045l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T61"   
;
label values tm7046   tm7046l;
label define tm7046l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T61"   
;
label values tm7047   tm7047l;
label define tm7047l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T61"   
;
label values tm7048   tm7048l;
label define tm7048l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T61"   
;
label values tm7049   tm7049l;
label define tm7049l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T61"   
;
label values tm7050   tm7050l;
label define tm7050l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to check item T61"   
;
label values tm7051   tm7051l;
label define tm7051l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7065"           
	-9          "No response"                   
;
label values tm7052   tm7052l;
label define tm7052l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7066"           
	-9          "No response"                   
;
label values tm7053   tm7053l;
label define tm7053l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7067"           
	-9          "No response"                   
;
label values tm7054   tm7054l;
label define tm7054l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7068"           
	-9          "No response"                   
;
label values tm7055   tm7055l;
label define tm7055l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7069"           
	-9          "No response"                   
;
label values tm7056   tm7056l;
label define tm7056l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7070"           
	-9          "No response"                   
;
label values tm7057   tm7057l;
label define tm7057l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7071"           
	-9          "No response"                   
;
label values tm7058   tm7058l;
label define tm7058l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7059   tm7059l;
label define tm7059l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7060   tm7060l;
label define tm7060l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7061   tm7061l;
label define tm7061l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7062   tm7062l;
label define tm7062l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7063   tm7063l;
label define tm7063l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7064   tm7064l;
label define tm7064l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7065   tm7065l;
label define tm7065l 
	0           "Not applicable"                
	1           "Yes - skip to TM7093"          
	2           "No"                            
	-9          "No response"                   
;
label values tm7066   tm7066l;
label define tm7066l 
	0           "Not applicable"                
	1           "Yes - skip to TM7094"          
	2           "No"                            
	-9          "No response"                   
;
label values tm7067   tm7067l;
label define tm7067l 
	0           "Not applicable"                
	1           "Yes - skip to TM7095"          
	2           "No"                            
	-9          "No response"                   
;
label values tm7068   tm7068l;
label define tm7068l 
	0           "Not applicable"                
	1           "Yes - skip to TM7096"          
	2           "No"                            
	-9          "No response"                   
;
label values tm7069   tm7069l;
label define tm7069l 
	0           "Not applicable"                
	1           "Yes - skip to TM7097"          
	2           "No"                            
	-9          "No response"                   
;
label values tm7070   tm7070l;
label define tm7070l 
	0           "Not applicable"                
	1           "Yes - skip to TM7098"          
	2           "No"                            
	-9          "No response"                   
;
label values tm7071   tm7071l;
label define tm7071l 
	0           "Not applicable"                
	1           "Yes - skip to TM7099"          
	2           "No"                            
	-9          "No response"                   
;
label values tm7072   tm7072l;
label define tm7072l 
	0           "Not applicable"                
	1           "Yes - skip to TM7093"          
	2           "No"                            
;
label values tm7073   tm7073l;
label define tm7073l 
	0           "Not applicable"                
	1           "Yes - skip to TM7094"          
	2           "No"                            
;
label values tm7074   tm7074l;
label define tm7074l 
	0           "Not applicable"                
	1           "Yes - skip to TM7095"          
	2           "No"                            
;
label values tm7075   tm7075l;
label define tm7075l 
	0           "Not applicable"                
	1           "Yes - skip to TM7096"          
	2           "No"                            
;
label values tm7076   tm7076l;
label define tm7076l 
	0           "Not applicable"                
	1           "Yes - skip to TM7097"          
	2           "No"                            
;
label values tm7077   tm7077l;
label define tm7077l 
	0           "Not applicable"                
	1           "Yes - skip to TM7098"          
	2           "No"                            
;
label values tm7078   tm7078l;
label define tm7078l 
	0           "Not applicable"                
	1           "Yes - skip to TM7099"          
	2           "No"                            
;
label values tm7079   tm7079l;
label define tm7079l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7093"           
	-9          "No response"                   
;
label values tm7080   tm7080l;
label define tm7080l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7094"           
	-9          "No response"                   
;
label values tm7081   tm7081l;
label define tm7081l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7095"           
	-9          "No response"                   
;
label values tm7082   tm7082l;
label define tm7082l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7096"           
	-9          "No response"                   
;
label values tm7083   tm7083l;
label define tm7083l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7097"           
	-9          "No response"                   
;
label values tm7084   tm7084l;
label define tm7084l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7098"           
	-9          "No response"                   
;
label values tm7085   tm7085l;
label define tm7085l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7099"           
	-9          "No response"                   
;
label values tm7086   tm7086l;
label define tm7086l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7087   tm7087l;
label define tm7087l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7088   tm7088l;
label define tm7088l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7089   tm7089l;
label define tm7089l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7090   tm7090l;
label define tm7090l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7091   tm7091l;
label define tm7091l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7092   tm7092l;
label define tm7092l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7093   tm7093l;
label define tm7093l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7107"           
	-9          "No response"                   
;
label values tm7094   tm7094l;
label define tm7094l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7108"           
	-9          "No response"                   
;
label values tm7095   tm7095l;
label define tm7095l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7109"           
	-9          "No response"                   
;
label values tm7096   tm7096l;
label define tm7096l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7110"           
	-9          "No response"                   
;
label values tm7097   tm7097l;
label define tm7097l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7111"           
	-9          "No response"                   
;
label values tm7098   tm7098l;
label define tm7098l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7112"           
	-9          "No response"                   
;
label values tm7099   tm7099l;
label define tm7099l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7113"           
	-9          "No response"                   
;
label values tm7100   tm7100l;
label define tm7100l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7101   tm7101l;
label define tm7101l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7102   tm7102l;
label define tm7102l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7103   tm7103l;
label define tm7103l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7104   tm7104l;
label define tm7104l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7105   tm7105l;
label define tm7105l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7106   tm7106l;
label define tm7106l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7107   tm7107l;
label define tm7107l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7135"           
	-9          "No response"                   
;
label values tm7108   tm7108l;
label define tm7108l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7136"           
	-9          "No response"                   
;
label values tm7109   tm7109l;
label define tm7109l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7137"           
	-9          "No response"                   
;
label values tm7110   tm7110l;
label define tm7110l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7138"           
	-9          "No response"                   
;
label values tm7111   tm7111l;
label define tm7111l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7139"           
	-9          "No response"                   
;
label values tm7112   tm7112l;
label define tm7112l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7140"           
	-9          "No response"                   
;
label values tm7113   tm7113l;
label define tm7113l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM7141"           
	-9          "No response"                   
;
label values tm7114   tm7114l;
label define tm7114l 
	0           "Not applicable"                
;
label values tm7115   tm7115l;
label define tm7115l 
	0           "Not applicable"                
;
label values tm7116   tm7116l;
label define tm7116l 
	0           "Not applicable"                
;
label values tm7117   tm7117l;
label define tm7117l 
	0           "Not applicable"                
;
label values tm7118   tm7118l;
label define tm7118l 
	0           "Not applicable"                
;
label values tm7119   tm7119l;
label define tm7119l 
	0           "Not applicable"                
;
label values tm7120   tm7120l;
label define tm7120l 
	0           "Not applicable"                
;
label values tm7121   tm7121l;
label define tm7121l 
	0           "Not applicable"                
	1           "Related"                       
	2           "Not related"                   
	3           "Both"                          
	-9          "No response"                   
;
label values tm7122   tm7122l;
label define tm7122l 
	0           "Not applicable"                
	1           "Related"                       
	2           "Not related"                   
	3           "Both"                          
	-9          "No response"                   
;
label values tm7123   tm7123l;
label define tm7123l 
	0           "Not applicable"                
	1           "Related"                       
	2           "Not related"                   
	3           "Both"                          
	-9          "No response"                   
;
label values tm7124   tm7124l;
label define tm7124l 
	0           "Not applicable"                
	1           "Related"                       
	2           "Not related"                   
	3           "Both"                          
	-9          "No response"                   
;
label values tm7125   tm7125l;
label define tm7125l 
	0           "Not applicable"                
	1           "Related"                       
	2           "Not related"                   
	3           "Both"                          
	-9          "No response"                   
;
label values tm7126   tm7126l;
label define tm7126l 
	0           "Not applicable"                
	1           "Related"                       
	2           "Not related"                   
	3           "Both"                          
	-9          "No response"                   
;
label values tm7127   tm7127l;
label define tm7127l 
	0           "Not applicable"                
	1           "Related"                       
	2           "Not related"                   
	3           "Both"                          
	-9          "No response"                   
;
label values tm7128   tm7128l;
label define tm7128l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7129   tm7129l;
label define tm7129l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7130   tm7130l;
label define tm7130l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7131   tm7131l;
label define tm7131l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7132   tm7132l;
label define tm7132l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7133   tm7133l;
label define tm7133l 
	0           "Not applicable"                
	-9          "No response"                   
;
label values tm7134   tm7134l;
label define tm7134l 
	0           "Not applicable"                
	-9          "No response"                   
;
*#label values tm7135   tm7135l;
*#label define tm7135l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T62"   
*#	-9          "No response"                   
;
*#label values tm7136   tm7136l;
*#label define tm7136l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T62"   
*#	-9          "No response"                   
;
*#label values tm7137   tm7137l;
*#label define tm7137l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T62"   
*#	-9          "No response"                   
;
*#label values tm7138   tm7138l;
*#label define tm7138l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T62"   
*#	-9          "No response"                   
;
*#label values tm7139   tm7139l;
*#label define tm7139l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T62"   
*#	-9          "No response"                   
;
*#label values tm7140   tm7140l;
*#label define tm7140l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T62"   
*#	-9          "No response"                   
;
*#label values tm7141   tm7141l;
*#label define tm7141l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T62"   
*#	-9          "No response"                   
;
*#label values tm7142   tm7142l;
*#label define tm7142l 
*#	0           "Not applicable"                
*#	1           "Related"                       
*#	2           "Not related"                   
*#	3           "Both"                          
*#	-9          "No response"                   
;
*#label values tm7143   tm7143l;
*#label define tm7143l 
*#	0           "Not applicable"                
*#	1           "Related"                       
*#	2           "Not related"                   
*#	3           "Both"                          
*#	-9          "No response"                   
;
*#label values tm7144   tm7144l;
*#label define tm7144l 
*#	0           "Not applicable"                
*#	1           "Related"                       
*#	2           "Not related"                   
*#	3           "Both"                          
*#	-9          "No response"                   
;
*#label values tm7145   tm7145l;
*#label define tm7145l 
*#	0           "Not applicable"                
*#	1           "Related"                       
*#	2           "Not related"                   
*#	3           "Both"                          
*#	-9          "No response"                   
;
*#label values tm7146   tm7146l;
*#label define tm7146l 
*#	0           "Not applicable"                
*#	1           "Related"                       
*#	2           "Not related"                   
*#	3           "Both"                          
*#	-9          "No response"                   
;
*#label values tm7147   tm7147l;
*#label define tm7147l 
*#	0           "Not applicable"                
*#	1           "Related"                       
*#	2           "Not related"                   
*#	3           "Both"                          
*#	-9          "No response"                   
;
*#label values tm7148   tm7148l;
*#label define tm7148l 
*#	0           "Not applicable"                
*#	1           "Related"                       
*#	2           "Not related"                   
*#	3           "Both"                          
*#	-9          "No response"                   
;
*#label values tm7149   tm7149l;
*#label define tm7149l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7150   tm7150l;
*#label define tm7150l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7151   tm7151l;
*#label define tm7151l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7152   tm7152l;
*#label define tm7152l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7153   tm7153l;
*#label define tm7153l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7154   tm7154l;
*#label define tm7154l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7155   tm7155l;
*#label define tm7155l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7156   tm7156l;
*#label define tm7156l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to check item T65"  
*#	2           "No"                            
;
*#label values tm7157   tm7157l;
*#label define tm7157l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to check item T65"  
*#	2           "No"                            
;
*#label values tm7158   tm7158l;
*#label define tm7158l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to check item T65"  
*#	2           "No"                            
;
*#label values tm7159   tm7159l;
*#label define tm7159l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to check item T65"  
*#	2           "No"                            
;
*#label values tm7160   tm7160l;
*#label define tm7160l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to check item T65"  
*#	2           "No"                            
;
*#label values tm7161   tm7161l;
*#label define tm7161l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to check item T65"  
*#	2           "No"                            
;
*#label values tm7162   tm7162l;
*#label define tm7162l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to check item T65"  
*#	2           "No"                            
;
*#label values tm7163   tm7163l;
*#label define tm7163l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7164   tm7164l;
*#label define tm7164l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7165   tm7165l;
*#label define tm7165l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7166   tm7166l;
*#label define tm7166l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7167   tm7167l;
*#label define tm7167l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7168   tm7168l;
*#label define tm7168l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7169   tm7169l;
*#label define tm7169l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7170   tm7170l;
*#label define tm7170l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T64"   
*#	-9          "No response"                   
;
*#label values tm7171   tm7171l;
*#label define tm7171l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T64"   
*#	-9          "No response"                   
;
*#label values tm7172   tm7172l;
*#label define tm7172l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T64"   
*#	-9          "No response"                   
;
*#label values tm7173   tm7173l;
*#label define tm7173l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T64"   
*#	-9          "No response"                   
;
*#label values tm7174   tm7174l;
*#label define tm7174l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T64"   
*#	-9          "No response"                   
;
*#label values tm7175   tm7175l;
*#label define tm7175l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T64"   
*#	-9          "No response"                   
;
*#label values tm7176   tm7176l;
*#label define tm7176l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T64"   
*#	-9          "No response"                   
;
*#label values tm7177   tm7177l;
*#label define tm7177l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7178   tm7178l;
*#label define tm7178l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7179   tm7179l;
*#label define tm7179l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7180   tm7180l;
*#label define tm7180l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7181   tm7181l;
*#label define tm7181l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7182   tm7182l;
*#label define tm7182l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7183   tm7183l;
*#label define tm7183l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7184   tm7184l;
*#label define tm7184l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7185   tm7185l;
*#label define tm7185l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7186   tm7186l;
*#label define tm7186l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7187   tm7187l;
*#label define tm7187l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7188   tm7188l;
*#label define tm7188l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7189   tm7189l;
*#label define tm7189l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7190   tm7190l;
*#label define tm7190l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7191   tm7191l;
*#label define tm7191l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7192   tm7192l;
*#label define tm7192l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7193   tm7193l;
*#label define tm7193l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7194   tm7194l;
*#label define tm7194l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7195   tm7195l;
*#label define tm7195l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7196   tm7196l;
*#label define tm7196l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7197   tm7197l;
*#label define tm7197l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T65"   
;
*#label values tm7198   tm7198l;
*#label define tm7198l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7199   tm7199l;
*#label define tm7199l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7200   tm7200l;
*#label define tm7200l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7201   tm7201l;
*#label define tm7201l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7202   tm7202l;
*#label define tm7202l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7203   tm7203l;
*#label define tm7203l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7204   tm7204l;
*#label define tm7204l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7205   tm7205l;
*#label define tm7205l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7206   tm7206l;
*#label define tm7206l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7207   tm7207l;
*#label define tm7207l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7208   tm7208l;
*#label define tm7208l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7209   tm7209l;
*#label define tm7209l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7210   tm7210l;
*#label define tm7210l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7211   tm7211l;
*#label define tm7211l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7212   tm7212l;
*#label define tm7212l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7233"          
*#	2           "No"                            
;
*#label values tm7213   tm7213l;
*#label define tm7213l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7234"          
*#	2           "No"                            
;
*#label values tm7214   tm7214l;
*#label define tm7214l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7235"          
*#	2           "No"                            
;
*#label values tm7215   tm7215l;
*#label define tm7215l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7236"          
*#	2           "No"                            
;
*#label values tm7216   tm7216l;
*#label define tm7216l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7237"          
*#	2           "No"                            
;
*#label values tm7217   tm7217l;
*#label define tm7217l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7238"          
*#	2           "No"                            
;
*#label values tm7218   tm7218l;
*#label define tm7218l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7239"          
*#	2           "No"                            
;
*#label values tm7219   tm7219l;
*#label define tm7219l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7254"           
*#	-9          "No response"                   
;
*#label values tm7220   tm7220l;
*#label define tm7220l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7255"           
*#	-9          "No response"                   
;
*#label values tm7221   tm7221l;
*#label define tm7221l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7256"           
*#	-9          "No response"                   
;
*#label values tm7222   tm7222l;
*#label define tm7222l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7257"           
*#	-9          "No response"                   
;
*#label values tm7223   tm7223l;
*#label define tm7223l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7258"           
*#	-9          "No response"                   
;
*#label values tm7224   tm7224l;
*#label define tm7224l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7259"           
*#	-9          "No response"                   
;
*#label values tm7225   tm7225l;
*#label define tm7225l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7260"           
*#	-9          "No response"                   
;
*#label values tm7226   tm7226l;
*#label define tm7226l 
*#	0           "Not applicable"                
;
*#label values tm7227   tm7227l;
*#label define tm7227l 
*#	0           "Not applicable"                
;
*#label values tm7228   tm7228l;
*#label define tm7228l 
*#	0           "Not applicable"                
;
*#label values tm7229   tm7229l;
*#label define tm7229l 
*#	0           "Not applicable"                
;
*#label values tm7230   tm7230l;
*#label define tm7230l 
*#	0           "Not applicable"                
;
*#label values tm7231   tm7231l;
*#label define tm7231l 
*#	0           "Not applicable"                
;
*#label values tm7232   tm7232l;
*#label define tm7232l 
*#	0           "Not applicable"                
;
*#label values tm7233   tm7233l;
*#label define tm7233l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7234   tm7234l;
*#label define tm7234l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7235   tm7235l;
*#label define tm7235l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7236   tm7236l;
*#label define tm7236l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7237   tm7237l;
*#label define tm7237l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7238   tm7238l;
*#label define tm7238l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7239   tm7239l;
*#label define tm7239l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7240   tm7240l;
*#label define tm7240l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7241   tm7241l;
*#label define tm7241l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7242   tm7242l;
*#label define tm7242l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7243   tm7243l;
*#label define tm7243l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7244   tm7244l;
*#label define tm7244l 
*#	0           "Not applicable"                
;
*#label values tm7245   tm7245l;
*#label define tm7245l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7246   tm7246l;
*#label define tm7246l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7247   tm7247l;
*#label define tm7247l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7248   tm7248l;
*#label define tm7248l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7249   tm7249l;
*#label define tm7249l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7250   tm7250l;
*#label define tm7250l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7251   tm7251l;
*#label define tm7251l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7252   tm7252l;
*#label define tm7252l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7253   tm7253l;
*#label define tm7253l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7254   tm7254l;
*#label define tm7254l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7255   tm7255l;
*#label define tm7255l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7256   tm7256l;
*#label define tm7256l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7257   tm7257l;
*#label define tm7257l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7258   tm7258l;
*#label define tm7258l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7259   tm7259l;
*#label define tm7259l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7260   tm7260l;
*#label define tm7260l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7261   tm7261l;
*#label define tm7261l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7275"           
;
*#label values tm7262   tm7262l;
*#label define tm7262l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7275"           
;
*#label values tm7263   tm7263l;
*#label define tm7263l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7276"           
;
*#label values tm7264   tm7264l;
*#label define tm7264l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7277"           
;
*#label values tm7265   tm7265l;
*#label define tm7265l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7278"           
;
*#label values tm7266   tm7266l;
*#label define tm7266l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7279"           
;
*#label values tm7267   tm7267l;
*#label define tm7267l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7280"           
;
*#label values tm7268   tm7268l;
*#label define tm7268l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7269   tm7269l;
*#label define tm7269l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7270   tm7270l;
*#label define tm7270l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7271   tm7271l;
*#label define tm7271l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7272   tm7272l;
*#label define tm7272l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7273   tm7273l;
*#label define tm7273l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7274   tm7274l;
*#label define tm7274l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7275   tm7275l;
*#label define tm7275l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7276   tm7276l;
*#label define tm7276l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7277   tm7277l;
*#label define tm7277l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7278   tm7278l;
*#label define tm7278l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7279   tm7279l;
*#label define tm7279l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7280   tm7280l;
*#label define tm7280l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7281   tm7281l;
*#label define tm7281l 
*#	0           "Not applicable"                
*#	-3          "None"                          
*#	-9          "No response"                   
;
*#label values tm7282   tm7282l;
*#label define tm7282l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T68"   
;
*#label values tm7283   tm7283l;
*#label define tm7283l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T68"   
;
*#label values tm7284   tm7284l;
*#label define tm7284l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T68"   
;
*#label values tm7285   tm7285l;
*#label define tm7285l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T68"   
;
*#label values tm7286   tm7286l;
*#label define tm7286l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T68"   
;
*#label values tm7287   tm7287l;
*#label define tm7287l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T68"   
;
*#label values tm7288   tm7288l;
*#label define tm7288l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T68"   
;
*#label values tm7289   tm7289l;
*#label define tm7289l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7290   tm7290l;
*#label define tm7290l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7291   tm7291l;
*#label define tm7291l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7292   tm7292l;
*#label define tm7292l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7293   tm7293l;
*#label define tm7293l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7294   tm7294l;
*#label define tm7294l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7295   tm7295l;
*#label define tm7295l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7296   tm7296l;
*#label define tm7296l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7297   tm7297l;
*#label define tm7297l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7298   tm7298l;
*#label define tm7298l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7299   tm7299l;
*#label define tm7299l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7300   tm7300l;
*#label define tm7300l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7301   tm7301l;
*#label define tm7301l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7302   tm7302l;
*#label define tm7302l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7303   tm7303l;
*#label define tm7303l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7304   tm7304l;
*#label define tm7304l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7305   tm7305l;
*#label define tm7305l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7306   tm7306l;
*#label define tm7306l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7307   tm7307l;
*#label define tm7307l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7308   tm7308l;
*#label define tm7308l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7309   tm7309l;
*#label define tm7309l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7310   tm7310l;
*#label define tm7310l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T72"   
;
*#label values tm7311   tm7311l;
*#label define tm7311l 
*#	0           "Not applicable"                
;
*#label values tm7312   tm7312l;
*#label define tm7312l 
*#	0           "Not applicable"                
;
*#label values tm7313   tm7313l;
*#label define tm7313l 
*#	0           "Not applicable"                
;
*#label values tm7314   tm7314l;
*#label define tm7314l 
*#	0           "Not applicable"                
;
*#label values tm7315   tm7315l;
*#label define tm7315l 
*#	0           "Not applicable"                
;
*#label values tm7316   tm7316l;
*#label define tm7316l 
*#	0           "Not applicable"                
;
*#label values tm7317   tm7317l;
*#label define tm7317l 
*#	0           "Not applicable"                
;
*#label values tm7318   tm7318l;
*#label define tm7318l 
*#	0           "Not applicable"                
;
*#label values tm7319   tm7319l;
*#label define tm7319l 
*#	0           "Not applicable"                
;
*#label values tm7320   tm7320l;
*#label define tm7320l 
*#	0           "Not applicable"                
;
*#label values tm7321   tm7321l;
*#label define tm7321l 
*#	0           "Not applicable"                
;
*#label values tm7322   tm7322l;
*#label define tm7322l 
*#	0           "Not applicable"                
;
*#label values tm7323   tm7323l;
*#label define tm7323l 
*#	0           "Not applicable"                
;
*#label values tm7324   tm7324l;
*#label define tm7324l 
*#	0           "Not applicable"                
;
*#label values tm7325   tm7325l;
*#label define tm7325l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7326   tm7326l;
*#label define tm7326l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7327   tm7327l;
*#label define tm7327l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7328   tm7328l;
*#label define tm7328l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7329   tm7329l;
*#label define tm7329l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7330   tm7330l;
*#label define tm7330l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7331   tm7331l;
*#label define tm7331l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7332   tm7332l;
*#label define tm7332l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7388"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7333   tm7333l;
*#label define tm7333l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7389"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7334   tm7334l;
*#label define tm7334l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7390"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7335   tm7335l;
*#label define tm7335l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7391"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7336   tm7336l;
*#label define tm7336l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7392"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7337   tm7337l;
*#label define tm7337l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7393"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7338   tm7338l;
*#label define tm7338l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7394"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7339   tm7339l;
*#label define tm7339l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7556"           
*#	-9          "No response"                   
;
*#label values tm7340   tm7340l;
*#label define tm7340l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7557"           
*#	-9          "No response"                   
;
*#label values tm7341   tm7341l;
*#label define tm7341l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7558"           
*#	-9          "No response"                   
;
*#label values tm7342   tm7342l;
*#label define tm7342l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7559"           
*#	-9          "No response"                   
;
*#label values tm7343   tm7343l;
*#label define tm7343l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7560"           
*#	-9          "No response"                   
;
*#label values tm7344   tm7344l;
*#label define tm7344l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7561"           
*#	-9          "No response"                   
;
*#label values tm7345   tm7345l;
*#label define tm7345l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7562"           
*#	-9          "No response"                   
;
*#label values tm7346   tm7346l;
*#label define tm7346l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7360" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7347   tm7347l;
*#label define tm7347l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7361" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7348   tm7348l;
*#label define tm7348l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7362" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7349   tm7349l;
*#label define tm7349l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7363" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7350   tm7350l;
*#label define tm7350l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7364" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7351   tm7351l;
*#label define tm7351l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7365" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7352   tm7352l;
*#label define tm7352l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7366" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7353   tm7353l;
*#label define tm7353l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T71"   
*#	-9          "No response"                   
;
*#label values tm7354   tm7354l;
*#label define tm7354l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T71"   
*#	-9          "No response"                   
;
*#label values tm7355   tm7355l;
*#label define tm7355l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T71"   
*#	-9          "No response"                   
;
*#label values tm7356   tm7356l;
*#label define tm7356l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T71"   
*#	-9          "No response"                   
;
*#label values tm7357   tm7357l;
*#label define tm7357l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T71"   
*#	-9          "No response"                   
;
*#label values tm7358   tm7358l;
*#label define tm7358l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T71"   
*#	-9          "No response"                   
;
*#label values tm7359   tm7359l;
*#label define tm7359l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T71"   
*#	-9          "No response"                   
;
*#label values tm7360   tm7360l;
*#label define tm7360l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7361   tm7361l;
*#label define tm7361l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7362   tm7362l;
*#label define tm7362l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7363   tm7363l;
*#label define tm7363l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7364   tm7364l;
*#label define tm7364l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7365   tm7365l;
*#label define tm7365l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7366   tm7366l;
*#label define tm7366l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7367   tm7367l;
*#label define tm7367l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7368   tm7368l;
*#label define tm7368l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7369   tm7369l;
*#label define tm7369l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7370   tm7370l;
*#label define tm7370l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7371   tm7371l;
*#label define tm7371l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7372   tm7372l;
*#label define tm7372l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7373   tm7373l;
*#label define tm7373l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7374   tm7374l;
*#label define tm7374l 
*#	0           "Not applicable"                
;
*#label values tm7375   tm7375l;
*#label define tm7375l 
*#	0           "Not applicable"                
;
*#label values tm7376   tm7376l;
*#label define tm7376l 
*#	0           "Not applicable"                
;
*#label values tm7377   tm7377l;
*#label define tm7377l 
*#	0           "Not applicable"                
;
*#label values tm7378   tm7378l;
*#label define tm7378l 
*#	0           "Not applicable"                
;
*#label values tm7379   tm7379l;
*#label define tm7379l 
*#	0           "Not applicable"                
;
*#label values tm7380   tm7380l;
*#label define tm7380l 
*#	0           "Not applicable"                
;
*#label values tm7381   tm7381l;
*#label define tm7381l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7557"          
*#	2           "No - skip to TM7459"           
;
*#label values tm7382   tm7382l;
*#label define tm7382l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7558"          
*#	2           "No - skip to TM7460"           
;
*#label values tm7383   tm7383l;
*#label define tm7383l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7559"          
*#	2           "No - skip to TM7461"           
;
*#label values tm7384   tm7384l;
*#label define tm7384l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7560"          
*#	2           "No - skip to TM7462"           
;
*#label values tm7385   tm7385l;
*#label define tm7385l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7561"          
*#	2           "No - skip to TM7463"           
;
*#label values tm7386   tm7386l;
*#label define tm7386l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7562"          
*#	2           "No - skip to TM7464"           
;
*#label values tm7387   tm7387l;
*#label define tm7387l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7563"          
*#	2           "No - skip to TM7465"           
;
*#label values tm7388   tm7388l;
*#label define tm7388l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
;
*#label values tm7389   tm7389l;
*#label define tm7389l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
;
*#label values tm7390   tm7390l;
*#label define tm7390l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
;
*#label values tm7391   tm7391l;
*#label define tm7391l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
;
*#label values tm7392   tm7392l;
*#label define tm7392l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
;
*#label values tm7393   tm7393l;
*#label define tm7393l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
;
*#label values tm7394   tm7394l;
*#label define tm7394l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade or higher"        
;
*#label values tm7395   tm7395l;
*#label define tm7395l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7409"      
*#	-9          "No response"                   
;
*#label values tm7396   tm7396l;
*#label define tm7396l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7410"      
*#	-9          "No response"                   
;
*#label values tm7397   tm7397l;
*#label define tm7397l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7411"      
*#	-9          "No response"                   
;
*#label values tm7398   tm7398l;
*#label define tm7398l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7412"      
*#	-9          "No response"                   
;
*#label values tm7399   tm7399l;
*#label define tm7399l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7413"      
*#	-9          "No response"                   
;
*#label values tm7400   tm7400l;
*#label define tm7400l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7414"      
*#	-9          "No response"                   
;
*#label values tm7401   tm7401l;
*#label define tm7401l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7415"      
*#	-9          "No response"                   
;
*#label values tm7402   tm7402l;
*#label define tm7402l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7416"     
*#	2           "Chosen - skip to TM7416"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7403   tm7403l;
*#label define tm7403l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7417"     
*#	2           "Chosen - skip to TM7417"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7404   tm7404l;
*#label define tm7404l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7418"     
*#	2           "Chosen - skip to TM7418"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7405   tm7405l;
*#label define tm7405l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7419"     
*#	2           "Chosen - skip to TM7419"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7406   tm7406l;
*#label define tm7406l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7420"     
*#	2           "Chosen - skip to TM7420"       
*#	3           "Assigned school is school of"  
;
*#label values tm7407   tm7407l;
*#label define tm7407l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7421"     
*#	2           "Chosen - skip to TM7421"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7408   tm7408l;
*#label define tm7408l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7422"     
*#	2           "Chosen - skip to TM7422"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7409   tm7409l;
*#label define tm7409l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7410   tm7410l;
*#label define tm7410l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7411   tm7411l;
*#label define tm7411l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7412   tm7412l;
*#label define tm7412l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7413   tm7413l;
*#label define tm7413l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7414   tm7414l;
*#label define tm7414l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7415   tm7415l;
*#label define tm7415l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7416   tm7416l;
*#label define tm7416l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7444"           
*#	-9          "No response"                   
;
*#label values tm7417   tm7417l;
*#label define tm7417l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7445"           
*#	-9          "No response"                   
;
*#label values tm7418   tm7418l;
*#label define tm7418l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7446"           
*#	-9          "No response"                   
;
*#label values tm7419   tm7419l;
*#label define tm7419l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7447"           
*#	-9          "No response"                   
;
*#label values tm7420   tm7420l;
*#label define tm7420l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7448"           
*#	-9          "No response"                   
;
*#label values tm7421   tm7421l;
*#label define tm7421l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7449"           
*#	-9          "No response"                   
;
*#label values tm7422   tm7422l;
*#label define tm7422l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7450"           
*#	-9          "No response"                   
;
*#label values tm7423   tm7423l;
*#label define tm7423l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7424   tm7424l;
*#label define tm7424l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7425   tm7425l;
*#label define tm7425l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7426   tm7426l;
*#label define tm7426l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7427   tm7427l;
*#label define tm7427l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7428   tm7428l;
*#label define tm7428l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7429   tm7429l;
*#label define tm7429l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7430   tm7430l;
*#label define tm7430l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7431   tm7431l;
*#label define tm7431l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7432   tm7432l;
*#label define tm7432l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7433   tm7433l;
*#label define tm7433l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7434   tm7434l;
*#label define tm7434l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7435   tm7435l;
*#label define tm7435l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7436   tm7436l;
*#label define tm7436l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7437   tm7437l;
*#label define tm7437l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7556"          
*#	2           "No"                            
;
*#label values tm7438   tm7438l;
*#label define tm7438l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7557"          
*#	2           "No"                            
;
*#label values tm7439   tm7439l;
*#label define tm7439l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7558"          
*#	2           "No"                            
;
*#label values tm7440   tm7440l;
*#label define tm7440l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7559"          
*#	2           "No"                            
;
*#label values tm7441   tm7441l;
*#label define tm7441l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7560"          
*#	2           "No"                            
;
*#label values tm7442   tm7442l;
*#label define tm7442l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7561"          
*#	2           "No"                            
;
*#label values tm7443   tm7443l;
*#label define tm7443l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7562"          
*#	2           "No"                            
;
*#label values tm7444   tm7444l;
*#label define tm7444l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7445   tm7445l;
*#label define tm7445l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7446   tm7446l;
*#label define tm7446l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7447   tm7447l;
*#label define tm7447l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7448   tm7448l;
*#label define tm7448l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7449   tm7449l;
*#label define tm7449l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7450   tm7450l;
*#label define tm7450l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7451   tm7451l;
*#label define tm7451l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7452   tm7452l;
*#label define tm7452l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7453   tm7453l;
*#label define tm7453l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7454   tm7454l;
*#label define tm7454l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7455   tm7455l;
*#label define tm7455l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7456   tm7456l;
*#label define tm7456l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7457   tm7457l;
*#label define tm7457l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7458   tm7458l;
*#label define tm7458l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7479"           
*#	-1          "Dk - skip to TM7479"           
*#	-2          "Ref. - skip to TM7479"         
*#	-9          "No response - skip to TM7479"  
;
*#label values tm7459   tm7459l;
*#label define tm7459l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7480"           
*#	-1          "Dk - skip to TM7480"           
*#	-2          "Ref. - skip to TM7480"         
*#	-9          "No response - skip to TM7480"  
;
*#label values tm7460   tm7460l;
*#label define tm7460l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7481"           
*#	-1          "Dk - skip to TM7481"           
*#	-2          "Ref. - skip to TM7481"         
*#	-9          "No response - skip to TM7481"  
;
*#label values tm7461   tm7461l;
*#label define tm7461l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7482"           
*#	-1          "Dk - skip to TM7482"           
*#	-2          "Ref. - skip to TM7482"         
*#	-9          "No response - skip to TM7482"  
;
*#label values tm7462   tm7462l;
*#label define tm7462l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7483"           
*#	-1          "No response - skip to TM7483"  
*#	-2          "Ref. - skip to TM7483"         
*#	-9          "No response - skip to TM7483"  
;
*#label values tm7463   tm7463l;
*#label define tm7463l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7484"           
*#	-1          "No response - skip to TM7484"  
*#	-2          "Ref. - skip to TM7484"         
*#	-9          "No response - skip to TM7484"  
;
*#label values tm7464   tm7464l;
*#label define tm7464l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7485"           
*#	-1          "Dk - skip to TM7485"           
*#	-2          "Ref. - skip to TM7485"         
*#	-9          "No response - skip to TM7485"  
;
*#label values tm7465   tm7465l;
*#label define tm7465l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7466   tm7466l;
*#label define tm7466l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7467   tm7467l;
*#label define tm7467l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7468   tm7468l;
*#label define tm7468l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7469   tm7469l;
*#label define tm7469l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7470   tm7470l;
*#label define tm7470l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7471   tm7471l;
*#label define tm7471l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7472   tm7472l;
*#label define tm7472l 
*#	0           "Not applicable"                
;
*#label values tm7473   tm7473l;
*#label define tm7473l 
*#	0           "Not applicable"                
;
*#label values tm7474   tm7474l;
*#label define tm7474l 
*#	0           "Not applicable"                
;
*#label values tm7475   tm7475l;
*#label define tm7475l 
*#	0           "Not applicable"                
;
*#label values tm7476   tm7476l;
*#label define tm7476l 
*#	0           "Not applicable"                
;
*#label values tm7477   tm7477l;
*#label define tm7477l 
*#	0           "Not applicable"                
;
*#label values tm7478   tm7478l;
*#label define tm7478l 
*#	0           "Not applicable"                
;
*#label values tm7479   tm7479l;
*#label define tm7479l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7549"           
*#	-1          "Dk - skip to TM7549"           
*#	-2          "Ref.- skip to TM7549"          
*#	-9          "No response"                   
;
*#label values tm7480   tm7480l;
*#label define tm7480l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7550"           
*#	-1          "Dk - skip to TM7550"           
*#	-2          "Ref.- skip to TM7550"          
*#	-9          "No response"                   
;
*#label values tm7481   tm7481l;
*#label define tm7481l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7551"           
*#	-1          "Dk - skip to TM7551"           
*#	-2          "Ref.- skip to TM7551"          
*#	-9          "No response"                   
;
*#label values tm7482   tm7482l;
*#label define tm7482l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7552"           
*#	-1          "Dk - skip to TM7552"           
*#	-2          "Ref.- skip to TM7552"          
*#	-9          "No response"                   
;
*#label values tm7483   tm7483l;
*#label define tm7483l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7553"           
*#	-1          "Dk - skip to TM7553"           
*#	-2          "Ref.- skip to TM7553"          
*#	-9          "No response"                   
;
*#label values tm7484   tm7484l;
*#label define tm7484l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7554"           
*#	-1          "Dk - skip to TM7554"           
*#	-2          "Ref.- skip to TM7554"          
*#	-9          "No response"                   
;
*#label values tm7485   tm7485l;
*#label define tm7485l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7555"           
*#	-1          "Dk - skip to TM7555"           
*#	-2          "Ref.- skip to TM7555"          
*#	-9          "No response"                   
;
*#label values tm7486   tm7486l;
*#label define tm7486l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7487   tm7487l;
*#label define tm7487l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7488   tm7488l;
*#label define tm7488l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7489   tm7489l;
*#label define tm7489l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7490   tm7490l;
*#label define tm7490l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7491   tm7491l;
*#label define tm7491l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7492   tm7492l;
*#label define tm7492l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7493   tm7493l;
*#label define tm7493l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7494   tm7494l;
*#label define tm7494l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7495   tm7495l;
*#label define tm7495l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7496   tm7496l;
*#label define tm7496l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7497   tm7497l;
*#label define tm7497l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7498   tm7498l;
*#label define tm7498l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7499   tm7499l;
*#label define tm7499l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7500   tm7500l;
*#label define tm7500l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7501   tm7501l;
*#label define tm7501l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7502   tm7502l;
*#label define tm7502l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7503   tm7503l;
*#label define tm7503l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7504   tm7504l;
*#label define tm7504l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7505   tm7505l;
*#label define tm7505l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7506   tm7506l;
*#label define tm7506l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7507   tm7507l;
*#label define tm7507l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7508   tm7508l;
*#label define tm7508l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7509   tm7509l;
*#label define tm7509l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7510   tm7510l;
*#label define tm7510l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7511   tm7511l;
*#label define tm7511l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7512   tm7512l;
*#label define tm7512l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7513   tm7513l;
*#label define tm7513l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7514   tm7514l;
*#label define tm7514l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7515   tm7515l;
*#label define tm7515l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7516   tm7516l;
*#label define tm7516l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7517   tm7517l;
*#label define tm7517l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7518   tm7518l;
*#label define tm7518l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7519   tm7519l;
*#label define tm7519l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7520   tm7520l;
*#label define tm7520l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7521   tm7521l;
*#label define tm7521l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7522   tm7522l;
*#label define tm7522l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7523   tm7523l;
*#label define tm7523l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7524   tm7524l;
*#label define tm7524l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7525   tm7525l;
*#label define tm7525l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7526   tm7526l;
*#label define tm7526l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7527   tm7527l;
*#label define tm7527l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7528   tm7528l;
*#label define tm7528l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7529   tm7529l;
*#label define tm7529l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7530   tm7530l;
*#label define tm7530l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7531   tm7531l;
*#label define tm7531l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7532   tm7532l;
*#label define tm7532l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7533   tm7533l;
*#label define tm7533l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7534   tm7534l;
*#label define tm7534l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7535   tm7535l;
*#label define tm7535l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7536   tm7536l;
*#label define tm7536l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7537   tm7537l;
*#label define tm7537l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7538   tm7538l;
*#label define tm7538l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7539   tm7539l;
*#label define tm7539l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7540   tm7540l;
*#label define tm7540l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7541   tm7541l;
*#label define tm7541l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7542   tm7542l;
*#label define tm7542l 
*#	0           "Not applicable"                
*#	1           "Eighth grade or higher"        
;
*#label values tm7543   tm7543l;
*#label define tm7543l 
*#	0           "Not applicable"                
*#	1           "Eighth grade or higher"        
;
*#label values tm7544   tm7544l;
*#label define tm7544l 
*#	0           "Not applicable"                
*#	1           "Eighth grade or higher"        
;
*#label values tm7545   tm7545l;
*#label define tm7545l 
*#	0           "Not applicable"                
*#	1           "Eighth grade or higher"        
;
*#label values tm7546   tm7546l;
*#label define tm7546l 
*#	0           "Not applicable"                
*#	1           "Eighth grade or higher"        
;
*#label values tm7547   tm7547l;
*#label define tm7547l 
*#	0           "Not applicable"                
*#	1           "Eighth grade or higher"        
;
*#label values tm7548   tm7548l;
*#label define tm7548l 
*#	0           "Not applicable"                
*#	1           "Eighth grade or higher"        
;
*#label values tm7549   tm7549l;
*#label define tm7549l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7550   tm7550l;
*#label define tm7550l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7551   tm7551l;
*#label define tm7551l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7552   tm7552l;
*#label define tm7552l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7553   tm7553l;
*#label define tm7553l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7554   tm7554l;
*#label define tm7554l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7555   tm7555l;
*#label define tm7555l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7556   tm7556l;
*#label define tm7556l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7557   tm7557l;
*#label define tm7557l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7558   tm7558l;
*#label define tm7558l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7559   tm7559l;
*#label define tm7559l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7560   tm7560l;
*#label define tm7560l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7561   tm7561l;
*#label define tm7561l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7562   tm7562l;
*#label define tm7562l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7563   tm7563l;
*#label define tm7563l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7564   tm7564l;
*#label define tm7564l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7565   tm7565l;
*#label define tm7565l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7566   tm7566l;
*#label define tm7566l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7567   tm7567l;
*#label define tm7567l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7568   tm7568l;
*#label define tm7568l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7569   tm7569l;
*#label define tm7569l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7570   tm7570l;
*#label define tm7570l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7571   tm7571l;
*#label define tm7571l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7572   tm7572l;
*#label define tm7572l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7573   tm7573l;
*#label define tm7573l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7574   tm7574l;
*#label define tm7574l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7575   tm7575l;
*#label define tm7575l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7576   tm7576l;
*#label define tm7576l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7577   tm7577l;
*#label define tm7577l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7578   tm7578l;
*#label define tm7578l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7579   tm7579l;
*#label define tm7579l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7580   tm7580l;
*#label define tm7580l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7581   tm7581l;
*#label define tm7581l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7582   tm7582l;
*#label define tm7582l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7583   tm7583l;
*#label define tm7583l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7584   tm7584l;
*#label define tm7584l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
*#	-2          "Ref."                          
;
*#label values tm7585   tm7585l;
*#label define tm7585l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7586   tm7586l;
*#label define tm7586l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7587   tm7587l;
*#label define tm7587l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7588   tm7588l;
*#label define tm7588l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7589   tm7589l;
*#label define tm7589l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7590   tm7590l;
*#label define tm7590l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7591   tm7591l;
*#label define tm7591l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7592   tm7592l;
*#label define tm7592l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7593   tm7593l;
*#label define tm7593l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7594   tm7594l;
*#label define tm7594l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7595   tm7595l;
*#label define tm7595l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7596   tm7596l;
*#label define tm7596l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7597   tm7597l;
*#label define tm7597l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7598   tm7598l;
*#label define tm7598l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T76"   
;
*#label values tm7599   tm7599l;
*#label define tm7599l 
*#	0           "Not applicable"                
;
*#label values tm7600   tm7600l;
*#label define tm7600l 
*#	0           "Not applicable"                
;
*#label values tm7601   tm7601l;
*#label define tm7601l 
*#	0           "Not applicable"                
;
*#label values tm7602   tm7602l;
*#label define tm7602l 
*#	0           "Not applicable"                
;
*#label values tm7603   tm7603l;
*#label define tm7603l 
*#	0           "Not applicable"                
;
*#label values tm7604   tm7604l;
*#label define tm7604l 
*#	0           "Not applicable"                
;
*#label values tm7605   tm7605l;
*#label define tm7605l 
*#	0           "Not applicable"                
;
*#label values tm7606   tm7606l;
*#label define tm7606l 
*#	0           "Not applicable"                
;
*#label values tm7607   tm7607l;
*#label define tm7607l 
*#	0           "Not applicable"                
;
*#label values tm7608   tm7608l;
*#label define tm7608l 
*#	0           "Not applicable"                
;
*#label values tm7609   tm7609l;
*#label define tm7609l 
*#	0           "Not applicable"                
;
*#label values tm7610   tm7610l;
*#label define tm7610l 
*#	0           "Not applicable"                
;
*#label values tm7611   tm7611l;
*#label define tm7611l 
*#	0           "Not applicable"                
;
*#label values tm7612   tm7612l;
*#label define tm7612l 
*#	0           "Not applicable"                
;
*#label values tm7613   tm7613l;
*#label define tm7613l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7669"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7614   tm7614l;
*#label define tm7614l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7670"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7615   tm7615l;
*#label define tm7615l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7671"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7616   tm7616l;
*#label define tm7616l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7672"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7617   tm7617l;
*#label define tm7617l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7673"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7618   tm7618l;
*#label define tm7618l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7674"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7619   tm7619l;
*#label define tm7619l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7675"          
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7620   tm7620l;
*#label define tm7620l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7879"           
*#	-9          "No response"                   
;
*#label values tm7621   tm7621l;
*#label define tm7621l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7880"           
*#	-9          "No response"                   
;
*#label values tm7622   tm7622l;
*#label define tm7622l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7881"           
*#	-9          "No response"                   
;
*#label values tm7623   tm7623l;
*#label define tm7623l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7882"           
*#	-9          "No response"                   
;
*#label values tm7624   tm7624l;
*#label define tm7624l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7883"           
*#	-9          "No response"                   
;
*#label values tm7625   tm7625l;
*#label define tm7625l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7884"           
*#	-9          "No response"                   
;
*#label values tm7626   tm7626l;
*#label define tm7626l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7885"           
*#	-9          "No response"                   
;
*#label values tm7627   tm7627l;
*#label define tm7627l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7641" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	14          "College - one year or more"    
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7628   tm7628l;
*#label define tm7628l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7642" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	14          "College - one year or more"    
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7629   tm7629l;
*#label define tm7629l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7643" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	14          "College - one year or more"    
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7630   tm7630l;
*#label define tm7630l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7644" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	14          "College - one year or more"    
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7631   tm7631l;
*#label define tm7631l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7645" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	14          "College - one year or more"    
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7632   tm7632l;
*#label define tm7632l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7646" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	14          "College - one year or more"    
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7633   tm7633l;
*#label define tm7633l 
*#	0           "Not applicable"                
*#	1           "Kindergarten - skip to TM7647" 
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	14          "College - one year or more"    
*#	-3          "No grade completed"            
*#	-9          "No response"                   
;
*#label values tm7634   tm7634l;
*#label define tm7634l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T75"   
*#	-9          "No response"                   
;
*#label values tm7635   tm7635l;
*#label define tm7635l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T75"   
*#	-9          "No response"                   
;
*#label values tm7636   tm7636l;
*#label define tm7636l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T75"   
*#	-9          "No response"                   
;
*#label values tm7637   tm7637l;
*#label define tm7637l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T75"   
*#	-9          "No response"                   
;
*#label values tm7638   tm7638l;
*#label define tm7638l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T75"   
*#	-9          "No response"                   
;
*#label values tm7639   tm7639l;
*#label define tm7639l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T75"   
*#	-9          "No response"                   
;
*#label values tm7640   tm7640l;
*#label define tm7640l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T75"   
*#	-9          "No response"                   
;
*#label values tm7641   tm7641l;
*#label define tm7641l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7642   tm7642l;
*#label define tm7642l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7643   tm7643l;
*#label define tm7643l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7644   tm7644l;
*#label define tm7644l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7645   tm7645l;
*#label define tm7645l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7646   tm7646l;
*#label define tm7646l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7647   tm7647l;
*#label define tm7647l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7648   tm7648l;
*#label define tm7648l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7649   tm7649l;
*#label define tm7649l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7650   tm7650l;
*#label define tm7650l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7651   tm7651l;
*#label define tm7651l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7652   tm7652l;
*#label define tm7652l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7653   tm7653l;
*#label define tm7653l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7654   tm7654l;
*#label define tm7654l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7655   tm7655l;
*#label define tm7655l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7879"          
*#	2           "No - skip to TM7739"           
*#	-9          "No response"                   
;
*#label values tm7656   tm7656l;
*#label define tm7656l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7880"          
*#	2           "No - skip to TM7740"           
*#	-9          "No response"                   
;
*#label values tm7657   tm7657l;
*#label define tm7657l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7881"          
*#	2           "No - skip to TM7741"           
*#	-9          "No response"                   
;
*#label values tm7658   tm7658l;
*#label define tm7658l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7882"          
*#	2           "No - skip to TM7742"           
*#	-9          "No response"                   
;
*#label values tm7659   tm7659l;
*#label define tm7659l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7883"          
*#	2           "No - skip to TM7743"           
*#	-9          "No response"                   
;
*#label values tm7660   tm7660l;
*#label define tm7660l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7884"          
*#	2           "No - skip to TM7744"           
*#	-9          "No response"                   
;
*#label values tm7661   tm7661l;
*#label define tm7661l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7885"          
*#	2           "No - skip to TM7745"           
*#	-9          "No response"                   
;
*#label values tm7662   tm7662l;
*#label define tm7662l 
*#	0           "Not applicable"                
;
*#label values tm7663   tm7663l;
*#label define tm7663l 
*#	0           "Not applicable"                
;
*#label values tm7664   tm7664l;
*#label define tm7664l 
*#	0           "Not applicable"                
;
*#label values tm7665   tm7665l;
*#label define tm7665l 
*#	0           "Not applicable"                
;
*#label values tm7666   tm7666l;
*#label define tm7666l 
*#	0           "Not applicable"                
;
*#label values tm7667   tm7667l;
*#label define tm7667l 
*#	0           "Not applicable"                
;
*#label values tm7668   tm7668l;
*#label define tm7668l 
*#	0           "Not applicable"                
;
*#label values tm7669   tm7669l;
*#label define tm7669l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
*#	2           "Fifth grade"                   
*#	3           "Sixth grade"                   
*#	4           "Seventh grade"                 
*#	5           "Eighth grade"                  
*#	6           "Ninth grade"                   
*#	7           "Tenth grade"                   
*#	8           "Eleventh grade"                
*#	9           "Twelfth grade"                 
*#	10          "College - one year or more"    
;
*#label values tm7670   tm7670l;
*#label define tm7670l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
*#	2           "Fifth grade"                   
*#	3           "Sixth grade"                   
*#	4           "Seventh grade"                 
*#	5           "Eighth grade"                  
*#	6           "Ninth grade"                   
*#	7           "Tenth grade"                   
*#	8           "Eleventh grade"                
*#	9           "Twelfth grade"                 
*#	10          "College - one year or more"    
;
*#label values tm7671   tm7671l;
*#label define tm7671l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
*#	2           "Fifth grade"                   
*#	3           "Sixth grade"                   
*#	4           "Seventh grade"                 
*#	5           "Eighth grade"                  
*#	6           "Ninth grade"                   
*#	7           "Tenth grade"                   
*#	8           "Eleventh grade"                
*#	9           "Twelfth grade"                 
*#	10          "College - one year or more"    
;
*#label values tm7672   tm7672l;
*#label define tm7672l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
*#	2           "Fifth grade"                   
*#	3           "Sixth grade"                   
*#	4           "Seventh grade"                 
*#	5           "Eighth grade"                  
*#	6           "Ninth grade"                   
*#	7           "Tenth grade"                   
*#	8           "Eleventh grade"                
*#	9           "Twelfth grade"                 
*#	10          "College - one year or more"    
;
*#label values tm7673   tm7673l;
*#label define tm7673l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
*#	2           "Fifth grade"                   
*#	3           "Sixth grade"                   
*#	4           "Seventh grade"                 
*#	5           "Eighth grade"                  
*#	6           "Ninth grade"                   
*#	7           "Tenth grade"                   
*#	8           "Eleventh grade"                
*#	9           "Twelfth grade"                 
*#	10          "College - one year or more"    
;
*#label values tm7674   tm7674l;
*#label define tm7674l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
*#	2           "Fifth grade"                   
*#	3           "Sixth grade"                   
*#	4           "Seventh grade"                 
*#	5           "Eighth grade"                  
*#	6           "Ninth grade"                   
*#	7           "Tenth grade"                   
*#	8           "Eleventh grade"                
*#	9           "Twelfth grade"                 
*#	10          "College - one year or more"    
;
*#label values tm7675   tm7675l;
*#label define tm7675l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
*#	2           "Fifth grade"                   
*#	3           "Sixth grade"                   
*#	4           "Seventh grade"                 
*#	5           "Eighth grade"                  
*#	6           "Ninth grade"                   
*#	7           "Tenth grade"                   
*#	8           "Eleventh grade"                
*#	9           "Twelfth grade"                 
*#	10          "College - one year or more"    
;
*#label values tm7676   tm7676l;
*#label define tm7676l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7690"      
*#	-9          "No response"                   
;
*#label values tm7677   tm7677l;
*#label define tm7677l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7691"      
*#	-9          "No response"                   
;
*#label values tm7678   tm7678l;
*#label define tm7678l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7692"      
*#	-9          "No response"                   
;
*#label values tm7679   tm7679l;
*#label define tm7679l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7693"      
*#	-9          "No response"                   
;
*#label values tm7680   tm7680l;
*#label define tm7680l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7694"      
*#	-9          "No response"                   
;
*#label values tm7681   tm7681l;
*#label define tm7681l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7695"      
*#	-9          "No response"                   
;
*#label values tm7682   tm7682l;
*#label define tm7682l 
*#	0           "Not applicable"                
*#	1           "Public"                        
*#	2           "Private - skip to TM7696"      
*#	-9          "No response"                   
;
*#label values tm7683   tm7683l;
*#label define tm7683l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7697"     
*#	2           "Chosen - skip to TM7697"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7684   tm7684l;
*#label define tm7684l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7698"     
*#	2           "Chosen - skip to TM7698"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7685   tm7685l;
*#label define tm7685l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7699"     
*#	2           "Chosen - skip to TM7699"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7686   tm7686l;
*#label define tm7686l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7700"     
*#	2           "Chosen - skip to TM7700"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7687   tm7687l;
*#label define tm7687l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7701"     
*#	2           "Chosen - skip to TM7701"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7688   tm7688l;
*#label define tm7688l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7702"     
*#	2           "Chosen - skip to TM7702"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7689   tm7689l;
*#label define tm7689l 
*#	0           "Not applicable"                
*#	1           "Assigned - skip to TM7703"     
*#	2           "Chosen - skip to TM7703"       
*#	3           "Assigned school is school of"  
*#	-9          "No response"                   
;
*#label values tm7690   tm7690l;
*#label define tm7690l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7691   tm7691l;
*#label define tm7691l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7692   tm7692l;
*#label define tm7692l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7693   tm7693l;
*#label define tm7693l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7694   tm7694l;
*#label define tm7694l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7695   tm7695l;
*#label define tm7695l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7696   tm7696l;
*#label define tm7696l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-9          "No response"                   
;
*#label values tm7697   tm7697l;
*#label define tm7697l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7725"           
*#	-9          "No response"                   
;
*#label values tm7698   tm7698l;
*#label define tm7698l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7726"           
*#	-9          "No response"                   
;
*#label values tm7699   tm7699l;
*#label define tm7699l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7727"           
*#	-9          "No response"                   
;
*#label values tm7700   tm7700l;
*#label define tm7700l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7728"           
*#	-9          "No response"                   
;
*#label values tm7701   tm7701l;
*#label define tm7701l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7729"           
*#	-9          "No response"                   
;
*#label values tm7702   tm7702l;
*#label define tm7702l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7730"           
*#	-9          "No response"                   
;
*#label values tm7703   tm7703l;
*#label define tm7703l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7731"           
*#	-9          "No response"                   
;
*#label values tm7704   tm7704l;
*#label define tm7704l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7705   tm7705l;
*#label define tm7705l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7706   tm7706l;
*#label define tm7706l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7707   tm7707l;
*#label define tm7707l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7708   tm7708l;
*#label define tm7708l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7709   tm7709l;
*#label define tm7709l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7710   tm7710l;
*#label define tm7710l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7711   tm7711l;
*#label define tm7711l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7712   tm7712l;
*#label define tm7712l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7713   tm7713l;
*#label define tm7713l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7714   tm7714l;
*#label define tm7714l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7715   tm7715l;
*#label define tm7715l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7716   tm7716l;
*#label define tm7716l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7717   tm7717l;
*#label define tm7717l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7718   tm7718l;
*#label define tm7718l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7879"          
;
*#label values tm7719   tm7719l;
*#label define tm7719l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7880"          
;
*#label values tm7720   tm7720l;
*#label define tm7720l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7881"          
;
*#label values tm7721   tm7721l;
*#label define tm7721l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7882"          
;
*#label values tm7722   tm7722l;
*#label define tm7722l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7883"          
;
*#label values tm7723   tm7723l;
*#label define tm7723l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7884"          
;
*#label values tm7724   tm7724l;
*#label define tm7724l 
*#	0           "Not applicable"                
*#	1           "Yes - skip to TM7885"          
;
*#label values tm7725   tm7725l;
*#label define tm7725l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7726   tm7726l;
*#label define tm7726l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7727   tm7727l;
*#label define tm7727l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7728   tm7728l;
*#label define tm7728l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7729   tm7729l;
*#label define tm7729l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7730   tm7730l;
*#label define tm7730l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7731   tm7731l;
*#label define tm7731l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7732   tm7732l;
*#label define tm7732l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7733   tm7733l;
*#label define tm7733l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7734   tm7734l;
*#label define tm7734l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7735   tm7735l;
*#label define tm7735l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7736   tm7736l;
*#label define tm7736l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7737   tm7737l;
*#label define tm7737l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7738   tm7738l;
*#label define tm7738l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7739   tm7739l;
*#label define tm7739l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7760"           
*#	-1          "Dk - skip to TM7760"           
*#	-2          "Ref. - skip to TM7760"         
*#	-9          "No response"                   
;
*#label values tm7740   tm7740l;
*#label define tm7740l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7761"           
*#	-1          "Dk - skip to TM7761"           
*#	-2          "Ref. - skip to TM7761"         
*#	-9          "No response"                   
;
*#label values tm7741   tm7741l;
*#label define tm7741l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7762"           
*#	-1          "Dk - skip to TM7762"           
*#	-2          "Ref. - skip to TM7762"         
*#	-9          "No response"                   
;
*#label values tm7742   tm7742l;
*#label define tm7742l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7763"           
*#	-1          "Dk - skip to TM7763"           
*#	-2          "Ref. - skip to TM7763"         
*#	-9          "No response"                   
;
*#label values tm7743   tm7743l;
*#label define tm7743l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7764"           
*#	-1          "Dk - skip to TM7764"           
*#	-2          "Ref. - skip to TM7764"         
*#	-9          "No response - skip to TM7764"  
;
*#label values tm7744   tm7744l;
*#label define tm7744l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7765"           
*#	-1          "Dk - skip to TM7765"           
*#	-2          "Ref. - skip to TM7765"         
*#	-9          "No response - skip to TM7765"  
;
*#label values tm7745   tm7745l;
*#label define tm7745l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7766"           
*#	-1          "Dk - skip to TM7766"           
*#	-2          "Ref. - skip to TM7766"         
*#	-9          "No response - skip to TM7766"  
;
*#label values tm7746   tm7746l;
*#label define tm7746l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7747   tm7747l;
*#label define tm7747l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7748   tm7748l;
*#label define tm7748l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7749   tm7749l;
*#label define tm7749l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7750   tm7750l;
*#label define tm7750l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7751   tm7751l;
*#label define tm7751l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7752   tm7752l;
*#label define tm7752l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7753   tm7753l;
*#label define tm7753l 
*#	0           "Not applicable"                
;
*#label values tm7754   tm7754l;
*#label define tm7754l 
*#	0           "Not applicable"                
;
*#label values tm7755   tm7755l;
*#label define tm7755l 
*#	0           "Not applicable"                
;
*#label values tm7756   tm7756l;
*#label define tm7756l 
*#	0           "Not applicable"                
;
*#label values tm7757   tm7757l;
*#label define tm7757l 
*#	0           "Not applicable"                
;
*#label values tm7758   tm7758l;
*#label define tm7758l 
*#	0           "Not applicable"                
;
*#label values tm7759   tm7759l;
*#label define tm7759l 
*#	0           "Not applicable"                
;
*#label values tm7760   tm7760l;
*#label define tm7760l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7858"           
*#	-1          "Dk - skip to TM7858"           
*#	-2          "Ref. - skip to TM7858"         
*#	-9          "No response - skip to TM7858"  
;
*#label values tm7761   tm7761l;
*#label define tm7761l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7859"           
*#	-1          "Dk - skip to TM7859"           
*#	-2          "Ref. - skip to TM7859"         
*#	-9          "No response - skip to TM7859"  
;
*#label values tm7762   tm7762l;
*#label define tm7762l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7860"           
*#	-1          "Dk - skip to TM7860"           
*#	-2          "Ref. - skip to TM7860"         
*#	-9          "No response - skip to TM7860"  
;
*#label values tm7763   tm7763l;
*#label define tm7763l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7861"           
*#	-1          "Dk - skip to TM7861"           
*#	-2          "Ref. - skip to TM7861"         
*#	-9          "No response - skip to TM7861"  
;
*#label values tm7764   tm7764l;
*#label define tm7764l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7862"           
*#	-1          "Dk - skip to TM7862"           
*#	-2          "Ref. - skip to TM7862"         
*#	-9          "Dk - skip to TM7862"           
;
*#label values tm7765   tm7765l;
*#label define tm7765l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7863"           
*#	-1          "Dk - skip to TM7863"           
*#	-2          "Ref. - skip to TM7863"         
*#	-9          "No response - skip to TM7863"  
;
*#label values tm7766   tm7766l;
*#label define tm7766l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7864"           
*#	-1          "Dk - skip to TM7864"           
*#	-2          "Ref. - skip to TM7864"         
*#	-9          "No response - skip to TM7864"  
;
*#label values tm7767   tm7767l;
*#label define tm7767l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7768   tm7768l;
*#label define tm7768l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7769   tm7769l;
*#label define tm7769l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7770   tm7770l;
*#label define tm7770l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7771   tm7771l;
*#label define tm7771l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7772   tm7772l;
*#label define tm7772l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7773   tm7773l;
*#label define tm7773l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
;
*#label values tm7774   tm7774l;
*#label define tm7774l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7775   tm7775l;
*#label define tm7775l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7776   tm7776l;
*#label define tm7776l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7777   tm7777l;
*#label define tm7777l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7778   tm7778l;
*#label define tm7778l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7779   tm7779l;
*#label define tm7779l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7780   tm7780l;
*#label define tm7780l 
*#	0           "Not applicable"                
*#	1           "First grade"                   
;
*#label values tm7781   tm7781l;
*#label define tm7781l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7782   tm7782l;
*#label define tm7782l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7783   tm7783l;
*#label define tm7783l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7784   tm7784l;
*#label define tm7784l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7785   tm7785l;
*#label define tm7785l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7786   tm7786l;
*#label define tm7786l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7787   tm7787l;
*#label define tm7787l 
*#	0           "Not applicable"                
*#	1           "Second grade"                  
;
*#label values tm7788   tm7788l;
*#label define tm7788l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7789   tm7789l;
*#label define tm7789l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7790   tm7790l;
*#label define tm7790l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7791   tm7791l;
*#label define tm7791l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7792   tm7792l;
*#label define tm7792l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7793   tm7793l;
*#label define tm7793l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7794   tm7794l;
*#label define tm7794l 
*#	0           "Not applicable"                
*#	1           "Third grade"                   
;
*#label values tm7795   tm7795l;
*#label define tm7795l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7796   tm7796l;
*#label define tm7796l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7797   tm7797l;
*#label define tm7797l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7798   tm7798l;
*#label define tm7798l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7799   tm7799l;
*#label define tm7799l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7800   tm7800l;
*#label define tm7800l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7801   tm7801l;
*#label define tm7801l 
*#	0           "Not applicable"                
*#	1           "Fourth grade"                  
;
*#label values tm7802   tm7802l;
*#label define tm7802l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7803   tm7803l;
*#label define tm7803l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7804   tm7804l;
*#label define tm7804l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7805   tm7805l;
*#label define tm7805l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7806   tm7806l;
*#label define tm7806l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7807   tm7807l;
*#label define tm7807l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7808   tm7808l;
*#label define tm7808l 
*#	0           "Not applicable"                
*#	1           "Fifth grade"                   
;
*#label values tm7809   tm7809l;
*#label define tm7809l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7810   tm7810l;
*#label define tm7810l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7811   tm7811l;
*#label define tm7811l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7812   tm7812l;
*#label define tm7812l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7813   tm7813l;
*#label define tm7813l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7814   tm7814l;
*#label define tm7814l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7815   tm7815l;
*#label define tm7815l 
*#	0           "Not applicable"                
*#	1           "Sixth grade"                   
;
*#label values tm7816   tm7816l;
*#label define tm7816l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7817   tm7817l;
*#label define tm7817l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7818   tm7818l;
*#label define tm7818l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7819   tm7819l;
*#label define tm7819l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7820   tm7820l;
*#label define tm7820l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7821   tm7821l;
*#label define tm7821l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7822   tm7822l;
*#label define tm7822l 
*#	0           "Not applicable"                
*#	1           "Seventh grade"                 
;
*#label values tm7823   tm7823l;
*#label define tm7823l 
*#	0           "Not applicable"                
*#	1           "Eighth grade"                  
;
*#label values tm7824   tm7824l;
*#label define tm7824l 
*#	0           "Not applicable"                
*#	1           "Eighth grade"                  
;
*#label values tm7825   tm7825l;
*#label define tm7825l 
*#	0           "Not applicable"                
*#	1           "Eighth grade"                  
;
*#label values tm7826   tm7826l;
*#label define tm7826l 
*#	0           "Not applicable"                
*#	1           "Eighth grade"                  
;
*#label values tm7827   tm7827l;
*#label define tm7827l 
*#	0           "Not applicable"                
*#	1           "Eighth grade"                  
;
*#label values tm7828   tm7828l;
*#label define tm7828l 
*#	0           "Not applicable"                
*#	1           "Eighth grade"                  
;
*#label values tm7829   tm7829l;
*#label define tm7829l 
*#	0           "Not applicable"                
*#	1           "Eighth grade"                  
;
*#label values tm7830   tm7830l;
*#label define tm7830l 
*#	0           "Not applicable"                
*#	1           "Ninth grade"                   
;
*#label values tm7831   tm7831l;
*#label define tm7831l 
*#	0           "Not applicable"                
*#	1           "Ninth grade"                   
;
*#label values tm7832   tm7832l;
*#label define tm7832l 
*#	0           "Not applicable"                
*#	1           "Ninth grade"                   
;
*#label values tm7833   tm7833l;
*#label define tm7833l 
*#	0           "Not applicable"                
*#	1           "Ninth grade"                   
;
*#label values tm7834   tm7834l;
*#label define tm7834l 
*#	0           "Not applicable"                
*#	1           "Ninth grade"                   
;
*#label values tm7835   tm7835l;
*#label define tm7835l 
*#	0           "Not applicable"                
*#	1           "Ninth grade"                   
;
*#label values tm7836   tm7836l;
*#label define tm7836l 
*#	0           "Not applicable"                
*#	1           "Ninth grade"                   
;
*#label values tm7837   tm7837l;
*#label define tm7837l 
*#	0           "Not applicable"                
*#	1           "Tenth grade"                   
;
*#label values tm7838   tm7838l;
*#label define tm7838l 
*#	0           "Not applicable"                
*#	1           "Tenth grade"                   
;
*#label values tm7839   tm7839l;
*#label define tm7839l 
*#	0           "Not applicable"                
*#	1           "Tenth grade"                   
;
*#label values tm7840   tm7840l;
*#label define tm7840l 
*#	0           "Not applicable"                
*#	1           "Tenth grade"                   
;
*#label values tm7841   tm7841l;
*#label define tm7841l 
*#	0           "Not applicable"                
*#	1           "Tenth grade"                   
;
*#label values tm7842   tm7842l;
*#label define tm7842l 
*#	0           "Not applicable"                
*#	1           "Tenth grade"                   
;
*#label values tm7843   tm7843l;
*#label define tm7843l 
*#	0           "Not applicable"                
*#	1           "Tenth grade"                   
;
*#label values tm7844   tm7844l;
*#label define tm7844l 
*#	0           "Not applicable"                
*#	1           "Eleventh grade"                
;
*#label values tm7845   tm7845l;
*#label define tm7845l 
*#	0           "Not applicable"                
*#	1           "Eleventh grade"                
;
*#label values tm7846   tm7846l;
*#label define tm7846l 
*#	0           "Not applicable"                
*#	1           "Eleventh grade"                
;
*#label values tm7847   tm7847l;
*#label define tm7847l 
*#	0           "Not applicable"                
*#	1           "Eleventh grade"                
;
*#label values tm7848   tm7848l;
*#label define tm7848l 
*#	0           "Not applicable"                
*#	1           "Eleventh grade"                
;
*#label values tm7849   tm7849l;
*#label define tm7849l 
*#	0           "Not applicable"                
*#	1           "Eleventh grade"                
;
*#label values tm7850   tm7850l;
*#label define tm7850l 
*#	0           "Not applicable"                
*#	1           "Eleventh grade"                
;
*#label values tm7851   tm7851l;
*#label define tm7851l 
*#	0           "Not applicable"                
*#	1           "Twelfth grade"                 
;
*#label values tm7852   tm7852l;
*#label define tm7852l 
*#	0           "Not applicable"                
*#	1           "Twelfth grade"                 
;
*#label values tm7853   tm7853l;
*#label define tm7853l 
*#	0           "Not applicable"                
*#	1           "Twelfth grade"                 
;
*#label values tm7854   tm7854l;
*#label define tm7854l 
*#	0           "Not applicable"                
*#	1           "Twelfth grade"                 
;
*#label values tm7855   tm7855l;
*#label define tm7855l 
*#	0           "Not applicable"                
*#	1           "Twelfth grade"                 
;
*#label values tm7856   tm7856l;
*#label define tm7856l 
*#	0           "Not applicable"                
*#	1           "Twelfth grade"                 
;
*#label values tm7857   tm7857l;
*#label define tm7857l 
*#	0           "Not applicable"                
*#	1           "Twelfth grade"                 
;
*#label values tm7858   tm7858l;
*#label define tm7858l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7879"           
*#	-1          "Dk - skip to TM7879"           
*#	-2          "Ref. - skip to TM7879"         
*#	-9          "No response - skip to TM7879"  
;
*#label values tm7859   tm7859l;
*#label define tm7859l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7880"           
*#	-1          "Dk - skip to TM7880"           
*#	-2          "Ref. - skip to TM7880"         
*#	-9          "No response - skip to TM7880"  
;
*#label values tm7860   tm7860l;
*#label define tm7860l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7881"           
*#	-1          "Dk - skip to TM7881"           
*#	-2          "Ref. - skip to TM7881"         
*#	-9          "No response - skip to TM7881"  
;
*#label values tm7861   tm7861l;
*#label define tm7861l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7882"           
*#	-1          "Dk - skip to TM7882"           
*#	-2          "Ref. - skip to TM7882"         
*#	-9          "No response - skip to TM7882"  
;
*#label values tm7862   tm7862l;
*#label define tm7862l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7883"           
*#	-1          "Dk - skip to TM7883"           
*#	-2          "Ref. - skip to TM7883"         
*#	-9          "No response - skip to TM7883"  
;
*#label values tm7863   tm7863l;
*#label define tm7863l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7884"           
*#	-1          "Dk - skip to TM7884"           
*#	-2          "Ref. - skip to TM7884"         
*#	-9          "No response - skip to TM7884"  
;
*#label values tm7864   tm7864l;
*#label define tm7864l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to TM7885"           
*#	-1          "Dk - skip to TM7885"           
*#	-2          "Ref. - skip to TM7885"         
*#	-9          "No response - skip to TM7885"  
;
*#label values tm7865   tm7865l;
*#label define tm7865l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7866   tm7866l;
*#label define tm7866l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7867   tm7867l;
*#label define tm7867l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7868   tm7868l;
*#label define tm7868l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7869   tm7869l;
*#label define tm7869l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7870   tm7870l;
*#label define tm7870l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7871   tm7871l;
*#label define tm7871l 
*#	0           "Not applicable"                
*#	-9          "No response"                   
;
*#label values tm7872   tm7872l;
*#label define tm7872l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	-1          "Dk"                            
*#	-9          "No response"                   
;
*#label values tm7873   tm7873l;
*#label define tm7873l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	-1          "Dk"                            
*#	-9          "No response"                   
;
*#label values tm7874   tm7874l;
*#label define tm7874l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	-1          "Dk"                            
*#	-9          "No response"                   
;
*#label values tm7875   tm7875l;
*#label define tm7875l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	-1          "Dk"                            
*#	-9          "No response"                   
;
*#label values tm7876   tm7876l;
*#label define tm7876l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	-1          "Dk"                            
*#	-9          "No response"                   
;
*#label values tm7877   tm7877l;
*#label define tm7877l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	-1          "Dk"                            
*#	-9          "No response"                   
;
*#label values tm7878   tm7878l;
*#label define tm7878l 
*#	0           "Not applicable"                
*#	1           "Kindergarten"                  
*#	2           "First grade"                   
*#	3           "Second grade"                  
*#	4           "Third grade"                   
*#	5           "Fourth grade"                  
*#	6           "Fifth grade"                   
*#	7           "Sixth grade"                   
*#	8           "Seventh grade"                 
*#	9           "Eighth grade"                  
*#	10          "Ninth grade"                   
*#	11          "Tenth grade"                   
*#	12          "Eleventh grade"                
*#	13          "Twelfth grade"                 
*#	-1          "Dk"                            
*#	-9          "No response"                   
;
*#label values tm7879   tm7879l;
*#label define tm7879l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7880   tm7880l;
*#label define tm7880l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7881   tm7881l;
*#label define tm7881l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7882   tm7882l;
*#label define tm7882l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7883   tm7883l;
*#label define tm7883l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7884   tm7884l;
*#label define tm7884l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7885   tm7885l;
*#label define tm7885l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7886   tm7886l;
*#label define tm7886l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7887   tm7887l;
*#label define tm7887l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7888   tm7888l;
*#label define tm7888l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7889   tm7889l;
*#label define tm7889l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7890   tm7890l;
*#label define tm7890l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7891   tm7891l;
*#label define tm7891l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7892   tm7892l;
*#label define tm7892l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7893   tm7893l;
*#label define tm7893l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7894   tm7894l;
*#label define tm7894l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7895   tm7895l;
*#label define tm7895l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7896   tm7896l;
*#label define tm7896l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7897   tm7897l;
*#label define tm7897l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7898   tm7898l;
*#label define tm7898l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7899   tm7899l;
*#label define tm7899l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7900   tm7900l;
*#label define tm7900l 
*#	0           "Not applicable"                
;
*#label values tm7901   tm7901l;
*#label define tm7901l 
*#	0           "Not applicable"                
;
*#label values tm7902   tm7902l;
*#label define tm7902l 
*#	0           "Not applicable"                
;
*#label values tm7903   tm7903l;
*#label define tm7903l 
*#	0           "Not applicable"                
;
*#label values tm7904   tm7904l;
*#label define tm7904l 
*#	0           "Not applicable"                
;
*#label values tm7905   tm7905l;
*#label define tm7905l 
*#	0           "Not applicable"                
;
*#label values tm7906   tm7906l;
*#label define tm7906l 
*#	0           "Not applicable"                
;
*#label values tm7907   tm7907l;
*#label define tm7907l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7908   tm7908l;
*#label define tm7908l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7909   tm7909l;
*#label define tm7909l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7910   tm7910l;
*#label define tm7910l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7911   tm7911l;
*#label define tm7911l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7912   tm7912l;
*#label define tm7912l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7913   tm7913l;
*#label define tm7913l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7914   tm7914l;
*#label define tm7914l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7915   tm7915l;
*#label define tm7915l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7916   tm7916l;
*#label define tm7916l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7917   tm7917l;
*#label define tm7917l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7918   tm7918l;
*#label define tm7918l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7919   tm7919l;
*#label define tm7919l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7920   tm7920l;
*#label define tm7920l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7921   tm7921l;
*#label define tm7921l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7922   tm7922l;
*#label define tm7922l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7923   tm7923l;
*#label define tm7923l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7924   tm7924l;
*#label define tm7924l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7925   tm7925l;
*#label define tm7925l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7926   tm7926l;
*#label define tm7926l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7927   tm7927l;
*#label define tm7927l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No"                            
*#	-1          "Dk"                            
*#	-2          "Ref."                          
*#	-9          "No response"                   
;
*#label values tm7928   tm7928l;
*#label define tm7928l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item T77"   
;
*#label values tm7929   tm7929l;
*#label define tm7929l 
*#	-9          "Blank"                         
;
*#label values tm7931   tm7931l;
*#label define tm7931l 
*#	-9          "Blank"                         
;
*#label values tm7932   tm7932l;
*#label define tm7932l 
*#	-9          "Blank"                         
;
*#label values tm7933   tm7933l;
*#label define tm7933l 
*#	-9          "Blank"                         
;
*#label values tm7934   tm7934l;
*#label define tm7934l 
*#	-9          "Blank"                         
;
*#label values tm7935   tm7935l;
*#label define tm7935l 
*#	-9          "Blank"                         
;
*#label values tm7936   tm7936l;
*#label define tm7936l 
*#	0           "Not applicable"                
*#	1           "Yes"                           
*#	2           "No - skip to check item c1"    
;
*#label values tm7937   tm7937l;
*#label define tm7937l 
*#	-9          "Blank"                         
;
*#label values tm7938   tm7938l;
*#label define tm7938l 
*#	-9          "Blank"                         
;
*#label values tm7939   tm7939l;
*#label define tm7939l 
*#	-9          "Blank"                         
;
*#label values tm7940   tm7940l;
*#label define tm7940l 
*#	0           "Not applicable"                
*#	1           "Very safe"                     
*#	2           "Fairly safe"                   
*#	3           "Fairly unsafe"                 
*#	4           "Very unsafe"                   
*#	-1          "Dk"                            
;
*#label values tm7941   tm7941l;
*#label define tm7941l 
*#	0           "Not applicable"                
*#	1           "Very safe"                     
*#	2           "Fairly safe"                   
*#	3           "Fairly unsafe"                 
*#	4           "Very unsafe"                   
*#	-1          "Dk"                            
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
