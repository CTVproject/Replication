log using sip90t4, text replace
set mem 1000m
*This program reads the 1990 SIPP Wave 4 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 17:36:01 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip90t4
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1990\sip90t4.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip90t4"

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
	2           "Rented for cash - skip to"     
	3           "Occupied without cash payment" 
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
	1           "Yes"                           
	2           "No - skip to TM8598"           
;
label values tm8564   tm8564l;
label define tm8564l 
	0           "Not applicable"                
;
label values tm8568   tm8568l;
label define tm8568l 
	0           "Not applicable -"              
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
	1           "Value combined with TM8564"    
;
label values tm8570   tm8570l;
label define tm8570l 
	0           "Not applicable -"              
;
label values tm8571   tm8571l;
label define tm8571l 
	0           "Not applicable"                
;
label values tm8574   tm8574l;
label define tm8574l 
	0           "Not applicable"                
	1           "Value combined with TM8572"    
;
label values tm8578   tm8578l;
label define tm8578l 
	0           "Not applicable"                
	-8          "Not fixed"                     
;
label values tm8582   tm8582l;
label define tm8582l 
	0           "Not applicable"                
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
	1           "Value combined with TM8564"    
;
label values tm8598   tm8598l;
label define tm8598l 
	0           "Not applicable -"              
;
label values tm8608   tm8608l;
label define tm8608l 
	0           "Not applicable"                
	1           "Owned"                         
	2           "Rented - skip to TM8658"       
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
;
label values tm8630   tm8630l;
label define tm8630l 
	0           "Not applicable"                
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
label values tm8762   tm8762l;
label define tm8762l 
	0           "Not applicable"                
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
label values tm8764   tm8764l;
label define tm8764l 
	0           "Not applicable"                
;
label values tm8770   tm8770l;
label define tm8770l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values tm8772   tm8772l;
label define tm8772l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values tm8774   tm8774l;
label define tm8774l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values tm8776   tm8776l;
label define tm8776l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values tm8778   tm8778l;
label define tm8778l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
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
label values hh_ats01 hh_ats0y;
label define hh_ats0y
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats02 hh_ats0k;
label define hh_ats0k
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats03 hh_ats0l;
label define hh_ats0l
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats04 hh_ats0m;
label define hh_ats0m
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats05 hh_ats0n;
label define hh_ats0n
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats06 hh_ats0o;
label define hh_ats0o
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats07 hh_ats0p;
label define hh_ats0p
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats08 hh_ats0q;
label define hh_ats0q
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats09 hh_ats0r;
label define hh_ats0r
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats10 hh_ats1y;
label define hh_ats1y
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats11 hh_ats1k;
label define hh_ats1k
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats12 hh_ats1l;
label define hh_ats1l
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats13 hh_ats1m;
label define hh_ats1m
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats14 hh_ats1n;
label define hh_ats1n
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats15 hh_ats1o;
label define hh_ats1o
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats16 hh_ats1p;
label define hh_ats1p
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats17 hh_ats1q;
label define hh_ats1q
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats18 hh_ats1r;
label define hh_ats1r
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats19 hh_ats1s;
label define hh_ats1s
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values hh_ats20 hh_ats2y;
label define hh_ats2y
	1           "ISS code 100 - savings accounts"
	2           "ISS code 101 - money market"   
	3           "ISS code 102 - certificates of"
	4           "ISS code 103 - NOW accounts"   
	5           "ISS code 104 - money market"   
	6           "ISS code 105 - U.S. Government"
	7           "ISS code 106 - municipal or"   
	8           "ISS code 107 - other interest" 
	9           "ISS code 110 - stocks or mutual"
	10          "ISS code 120 - rental property"
	11          "ISS code 130 - mortgages"      
	12          "ISS code 140 - royalties"      
	13          "ISS code 150 - other financial"
	14          "Self-employed owns business"   
	15          "Sale of business or property"  
	16          "Checking accounts w/o interest"
	17          "U. S. Savings bond (E, EE)"    
	18          "IRA accounts"                  
	19          "KEOGH accounts"                
	20          "Real estate"                   
;
label values tm_ip149 tm_ip14y;
label define tm_ip14y
	0           "Not imputed"                   
	1           "Imputed TM8540"                
	2           "Imputed TM8542"                
	3           "Imputed TM8540 and TM8542"     
;
label values tm_ip150 tm_ip15y;
label define tm_ip15y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip151 tm_ip15k;
label define tm_ip15k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip152 tm_ip15l;
label define tm_ip15l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip153 tm_ip15m;
label define tm_ip15m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip154 tm_ip15n;
label define tm_ip15n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip155 tm_ip15o;
label define tm_ip15o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip156 tm_ip15p;
label define tm_ip15p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip157 tm_ip15q;
label define tm_ip15q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip159 tm_ip15r;
label define tm_ip15r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip160 tm_ip16y;
label define tm_ip16y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip161 tm_ip16k;
label define tm_ip16k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip162 tm_ip16l;
label define tm_ip16l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_cal11 tm_cal1y;
label define tm_cal1y
	0           "Not calculated"                
	1           "Calculated"                    
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
	1           "Value combined with TM8008"    
	-3          "None - skip to TM8028"         
;
label values tm8028   tm8028l;
label define tm8028l 
	0           "Not applicable"                
	1           "Value combined with TM8012"    
	-3          "None - skip to TM8200"         
;
label values tm_ip001 tm_ip00y;
label define tm_ip00y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values sc4314   sc4314l;
label define sc4314l 
	-3          "None"                          
	0           "Not in universe"               
;
label values sc4322   sc4322l;
label define sc4322l 
	-3          "None"                          
	0           "Not in universe"               
;
label values sc4414   sc4414l;
label define sc4414l 
	-3          "None"                          
	0           "Not in universe"               
;
label values sc4422   sc4422l;
label define sc4422l 
	-3          "None"                          
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
	999999999   "Total amount - skip to"        
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
	999999999   "Total amount - skip to"        
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
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
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
label values tm8080   tm8080l;
label define tm8080l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values tm8091   tm8091l;
label define tm8091l 
	0           "Not applicable"                
	1           "Yes - all rental properties on"
	2           "Yes - some rental properties on"
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
	-3          "None - skip to next ISS"       
;
label values tm8104   tm8104l;
label define tm8104l 
	0           "Not marked as a kind"          
	1           "Marked as a kind of"           
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
	-3          "None"                          
;
label values tm8126   tm8126l;
label define tm8126l 
	0           "Not applicable"                
	999999999   "Total amount - Range ="        
	-3          "None"                          
;
label values tm8128   tm8128l;
label define tm8128l 
	0           "Not applicable"                
	-3          "None"                          
	999999999   "Total amount- Range ="         
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
	999999999   "Total amount - Range ="        
	-3          "None - skip to TM8200"         
;
label values tm_ip009 tm_ip00k;
label define tm_ip00k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8200   tm8200l;
label define tm8200l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8204"           
;
label values tm8202   tm8202l;
label define tm8202l 
	0           "Not in umiverse"               
	999999999   "Total amount - Range ="        
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
label values tm8212   tm8212l;
label define tm8212l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8214   tm8214l;
label define tm8214l 
	0           "Not applicable"                
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
label values tm8268   tm8268l;
label define tm8268l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of IRA"       
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
label values tm8292   tm8292l;
label define tm8292l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of KEOGH"     
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8324"           
;
label values tm8309   tm8309l;
label define tm8309l 
	0           "Not applicable"                
	999999999   "Total amount - Range ="        
;
label values tm8311   tm8311l;
label define tm8311l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8324"           
;
label values tm8312   tm8312l;
label define tm8312l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8324"           
;
label values tm8313   tm8313l;
label define tm8313l 
	0           "Not applicable"                
;
label values tm_ip038 tm_ip03y;
label define tm_ip03y
	0           "Not imputed"                   
	1           "imputed"                       
;
label values tm8324   tm8324l;
label define tm8324l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -skip to TM8448"            
;
label values tm8326   tm8326l;
label define tm8326l 
	0           "Not applicable"                
;
label values tm8330   tm8330l;
label define tm8330l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 or more - skip to TM8342"  
;
label values tm8334   tm8334l;
label define tm8334l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -skip to TM8342"            
;
label values tm8338   tm8338l;
label define tm8338l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 or more"                   
;
label values tm8342   tm8342l;
label define tm8342l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8382"           
;
label values tm8346   tm8346l;
label define tm8346l 
	0           "Not applicable"                
	1           "Yes- skip to TM8384"           
	2           "No"                            
;
label values tm8350   tm8350l;
label define tm8350l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values tm8382   tm8382l;
label define tm8382l 
	0           "Not applicable"                
	1           "Yes - skip to TM8328"          
	2           "No - skip to TM8448"           
;
label values tm8384   tm8384l;
label define tm8384l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8388   tm8388l;
label define tm8388l 
	0           "Not applicable"                
	1           "Yes - skip to TM8396"          
	2           "No"                            
;
label values tm8392   tm8392l;
label define tm8392l 
	0           "Not applicable"                
	1           "Based on years of service and" 
	2           "Based on the amount contributed"
;
label values tm8396   tm8396l;
label define tm8396l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8420"           
;
label values tm8404   tm8404l;
label define tm8404l 
	0           "Not applicable"                
;
label values tm8408   tm8408l;
label define tm8408l 
	0           "Not applicable"                
	1           "Week"                          
	2           "Biweekly"                      
	3           "Month"                         
	4           "Quarter"                       
	5           "Year"                          
;
label values tm8412   tm8412l;
label define tm8412l 
	0           "Not applicable"                
;
label values tm8420   tm8420l;
label define tm8420l 
	0           "Not applicable"                
	1           "Less than 1 year"              
;
label values tm8424   tm8424l;
label define tm8424l 
	0           "Not applicable"                
	1           "Yes - skip to TM8432"          
	2           "No"                            
;
label values tm8428   tm8428l;
label define tm8428l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8432   tm8432l;
label define tm8432l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8438   tm8438l;
label define tm8438l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8446"           
;
label values tm8442   tm8442l;
label define tm8442l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8446"           
;
label values tm8443   tm8443l;
label define tm8443l 
	0           "Not applicable"                
;
label values tm8446   tm8446l;
label define tm8446l 
	0           "Not applicable"                
	1           "Yes - skip to TM8328"          
	2           "No - skip to TM8448"           
;
label values tm8328   tm8328l;
label define tm8328l 
	0           "Not applicable"                
;
label values tm8332   tm8332l;
label define tm8332l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 or more - skip to TM8344"  
;
label values tm8336   tm8336l;
label define tm8336l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No -skip to TM8344"            
;
label values tm8340   tm8340l;
label define tm8340l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 or more"                   
;
label values tm8344   tm8344l;
label define tm8344l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8448"           
;
label values tm8348   tm8348l;
label define tm8348l 
	0           "Not applicable"                
	1           "Yes- skip to TM8386"           
	2           "No - skip to TM8448"           
;
label values tm8352   tm8352l;
label define tm8352l 
	0           "Not marked as a kind of"       
	1           "marked as a kind of"           
;
label values tm8386   tm8386l;
label define tm8386l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8390   tm8390l;
label define tm8390l 
	0           "Not applicable"                
	1           "Yes - skip to TM8398"          
	2           "No"                            
;
label values tm8394   tm8394l;
label define tm8394l 
	0           "Not applicable"                
	1           "Based on years of service"     
	2           "Based on the amount contributed"
;
label values tm8398   tm8398l;
label define tm8398l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8422"           
;
label values tm8406   tm8406l;
label define tm8406l 
	0           "Not applicable"                
;
label values tm8410   tm8410l;
label define tm8410l 
	0           "Not applicable"                
	1           "Week"                          
	2           "Biweekly"                      
	3           "Month"                         
	4           "Quarter"                       
	5           "Year"                          
;
label values tm8414   tm8414l;
label define tm8414l 
	0           "Not applicable"                
;
label values tm8422   tm8422l;
label define tm8422l 
	0           "Not applicable"                
	1           "Less than 1 year"              
;
label values tm8426   tm8426l;
label define tm8426l 
	0           "Not applicable"                
	1           "Yes - skip to TM8436"          
	2           "No"                            
;
label values tm8430   tm8430l;
label define tm8430l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8436   tm8436l;
label define tm8436l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8440   tm8440l;
label define tm8440l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8448"           
;
label values tm8444   tm8444l;
label define tm8444l 
	0           "Not applicable - skip to TM8448"
	1           "Yes"                           
	2           "No - skip to TM8448"           
;
label values tm8445   tm8445l;
label define tm8445l 
	0           "Not applicable"                
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8458"           
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "Not applicable"                
;
label values tm8454   tm8454l;
label define tm8454l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8452   tm8452l;
label define tm8452l 
	0           "Not applicable"                
;
label values tm8456   tm8456l;
label define tm8456l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8458   tm8458l;
label define tm8458l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8475"           
;
label values tm8460   tm8460l;
label define tm8460l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8462   tm8462l;
label define tm8462l 
	0           "Not marked as a possible"      
	1           "Marked as a possible"          
;
label values tm8474   tm8474l;
label define tm8474l 
	0           "Not applicable"                
;
label values tm8475   tm8475l;
label define tm8475l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8482"           
;
label values tm8476   tm8476l;
label define tm8476l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
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
	1           "Yes - skip to TM8482"          
	2           "No"                            
;
label values tm8481   tm8481l;
label define tm8481l 
	0           "Not applicable"                
	1           "Purchased a home or paid off"  
	2           "Used it for children's"        
	3           "Used it for a period of"       
	4           "Paid off loans, bills, or spent"
	5           "Other"                         
;
label values tm8482   tm8482l;
label define tm8482l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No skip to check item T11-"    
;
label values tm8483   tm8483l;
label define tm8483l 
	0           "Not applicable"                
	1           "Retired from job"              
	2           "Some other reason - skip to"   
;
label values tmind    tmind;  
label define tmind   
	0           "Not applicable"                
;
label values tmocc    tmocc;  
label define tmocc   
	0           "Not applicable"                
;
label values tm8492   tm8492l;
label define tm8492l 
	0           "Not applicable"                
	1           "A private company or union"    
	2           "Federal government (exclude"   
	3           "State government"              
	4           "Local government"              
	5           "Armed Forces"                  
	6           "Unpaid in family business"     
;
label values tm8494   tm8494l;
label define tm8494l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 to 499"                    
	4           "500 to 999"                    
	5           "100 or more - skip to TM8500"  
;
label values tm8496   tm8496l;
label define tm8496l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8500"           
;
label values tm8498   tm8498l;
label define tm8498l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 or more"                   
;
label values tm8500   tm8500l;
label define tm8500l 
	0           "Not applicable"                
;
label values tm8502   tm8502l;
label define tm8502l 
	0           "Not applicable"                
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
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "Not applicable"                
	1           "Week"                          
	2           "Month"                         
	3           "Year"                          
;
label values tm8514   tm8514l;
label define tm8514l 
	0           "Not applicable"                
;
label values tm8516   tm8516l;
label define tm8516l 
	0           "Not applicable"                
	1           "Based on years of service and" 
	2           "Based on the amount contributed"
;
label values tm8518   tm8518l;
label define tm8518l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8520   tm8520l;
label define tm8520l 
	0           "Not applicable"                
	1           "Yes - skip to TM8524"          
	2           "No"                            
;
label values tm8522   tm8522l;
label define tm8522l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8524   tm8524l;
label define tm8524l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm_ip079 tm_ip07c;
label define tm_ip07c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip080 tm_ip08c;
label define tm_ip08c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip081 tm_ip08k;
label define tm_ip08k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip082 tm_ip08l;
label define tm_ip08l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip083 tm_ip08m;
label define tm_ip08m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip084 tm_ip08n;
label define tm_ip08n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip085 tm_ip08o;
label define tm_ip08o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip086 tm_ip08p;
label define tm_ip08p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip087 tm_ip08q;
label define tm_ip08q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip088 tm_ip08r;
label define tm_ip08r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip089 tm_ip08s;
label define tm_ip08s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip090 tm_ip09c;
label define tm_ip09c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip091 tm_ip09k;
label define tm_ip09k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip092 tm_ip09l;
label define tm_ip09l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip093 tm_ip09m;
label define tm_ip09m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip094 tm_ip09n;
label define tm_ip09n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip095 tm_ip09o;
label define tm_ip09o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip096 tm_ip09p;
label define tm_ip09p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip097 tm_ip09q;
label define tm_ip09q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip098 tm_ip09r;
label define tm_ip09r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip099 tm_ip09s;
label define tm_ip09s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip100 tm_ip10c;
label define tm_ip10c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip101 tm_ip10k;
label define tm_ip10k
	0           "Not imputed"                   
	1           "Imputed TM8404"                
	2           "Imputed TM8408"                
	3           "Imputed TM8404 & TM8408"       
;
label values tm_ip102 tm_ip10l;
label define tm_ip10l
	0           "Not imputed"                   
	1           "Imputed TM8406"                
	2           "Imputed TM8410"                
	3           "Imputed TM8406 & TM8410"       
;
label values tm_ip103 tm_ip10m;
label define tm_ip10m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip104 tm_ip10n;
label define tm_ip10n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip105 tm_ip10o;
label define tm_ip10o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip106 tm_ip10p;
label define tm_ip10p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip107 tm_ip10q;
label define tm_ip10q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip108 tm_ip10r;
label define tm_ip10r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip109 tm_ip10s;
label define tm_ip10s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip110 tm_ip11c;
label define tm_ip11c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip111 tm_ip11k;
label define tm_ip11k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip112 tm_ip11l;
label define tm_ip11l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip113 tm_ip11m;
label define tm_ip11m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip114 tm_ip11n;
label define tm_ip11n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip115 tm_ip11o;
label define tm_ip11o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip116 tm_ip11p;
label define tm_ip11p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip117 tm_ip11q;
label define tm_ip11q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip118 tm_ip11r;
label define tm_ip11r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip119 tm_ip11s;
label define tm_ip11s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip120 tm_ip12c;
label define tm_ip12c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip121 tm_ip12k;
label define tm_ip12k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip122 tm_ip12l;
label define tm_ip12l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip123 tm_ip12m;
label define tm_ip12m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip124 tm_ip12n;
label define tm_ip12n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip125 tm_ip12o;
label define tm_ip12o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip126 tm_ip12p;
label define tm_ip12p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip127 tm_ip12q;
label define tm_ip12q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip128 tm_ip12r;
label define tm_ip12r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip129 tm_ip12s;
label define tm_ip12s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip130 tm_ip13c;
label define tm_ip13c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip131 tm_ip13k;
label define tm_ip13k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip132 tm_ip13l;
label define tm_ip13l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip133 tm_ip13m;
label define tm_ip13m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip134 tm_ip13n;
label define tm_ip13n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip135 tm_ip13o;
label define tm_ip13o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip136 tm_ip13p;
label define tm_ip13p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip137 tm_ip13q;
label define tm_ip13q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip138 tm_ip13r;
label define tm_ip13r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip139 tm_ip13s;
label define tm_ip13s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip140 tm_ip14c;
label define tm_ip14c
	0           "Not imputed"                   
	1           "Imputed TM8508"                
	2           "Imputed TM8510"                
	3           "Imputed TM8508 & TM8510"       
;
label values tm_ip141 tm_ip14k;
label define tm_ip14k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip142 tm_ip14l;
label define tm_ip14l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip143 tm_ip14m;
label define tm_ip14m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip144 tm_ip14n;
label define tm_ip14n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ip145 tm_ip14o;
label define tm_ip14o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ipind tm_ipind;
label define tm_ipind
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ipocp tm_ipocp;
label define tm_ipocp
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_cal03 tm_cal0p;
label define tm_cal0p
	0           "Not calculated"                
	1           "Calculated"                    
;
label values tm_cal04 tm_cal0k;
label define tm_cal0k
	0           "Not calculated"                
	1           "Calculated"                    
;
label values tm_cal05 tm_cal0l;
label define tm_cal0l
	0           "Not calculated"                
	1           "Calculated"                    
;
label values tm_cal06 tm_cal0m;
label define tm_cal0m
	0           "Not calculated"                
	1           "Calculated"                    
;
label values tm_cal07 tm_cal0n;
label define tm_cal0n
	0           "Not calculated"                
	1           "Calculated"                    
;
label values tm_cal08 tm_cal0o;
label define tm_cal0o
	0           "Not calculated"                
	1           "Calculated"                    
;
label values tm_cal09 tm_cal0q;
label define tm_cal0q
	0           "Not calculated"                
	1           "Calculated"                    
;
label values tm_cal10 tm_cal1p;
label define tm_cal1p
	0           "Not calculated"                
	1           "Calculated"                    
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
