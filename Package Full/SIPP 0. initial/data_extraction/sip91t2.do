log using sip91t2, text replace
set mem 1000m
*This program reads the 1991 SIPP Wave 2 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Aug 18 17:16:59 EDT 2005
*Please report errors to jroth@nber.org
*run with do sip91t2
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1991\sip91t2.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip91t2"

*Everything below this point are value labels

#delimit ;

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
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
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
label values tm9266   tm9266l;
label define tm9266l 
	0           "Not in universe"               
	1           "One person HH - skip to SC5000"
	2           "Two person HH consisting of"   
	3           "Two person HH consisting of"   
	4           "Other"                         
;
label values tm9300   tm9300l;
label define tm9300l 
	0           "Not in universe"               
;
label values tm9330   tm9330l;
label define tm9330l 
	0           "Not in universe"               
;
label values tm9332   tm9332l;
label define tm9332l 
	1           "Husband"                       
	2           "Wife"                          
	10          "Natural father(biological)"    
	11          "Stepfather"                    
	12          "Adoptive father"               
	13          "Foster father"                 
	14          "Natural mother"                
	15          "Stepmother"                    
	16          "Adoptive mother"               
	17          "Foster mother"                 
	18          "Unknown male parent type"      
	19          "Unknown female parent type"    
	20          "Natural son"                   
	21          "Stepson"                       
	22          "Adopted son"                   
	23          "Foster son"                    
	24          "Natural daughter"              
	25          "Stepdaughter"                  
	26          "Adopted daughter"              
	27          "Foster daughter"               
	28          "Unknown male child type"       
	29          "Unknown female child type"     
	30          "Full brother"                  
	31          "Half brother"                  
	32          "Stepbrother"                   
	33          "Adoptive brother"              
	34          "Full sister"                   
	35          "Half sister"                   
	36          "Stepsister"                    
	37          "Adoptive sister"               
	38          "Unknown sibling type"          
	40          "Grandfather"                   
	41          "Grandmother"                   
	42          "Grandson"                      
	43          "Granddaughter"                 
	44          "Uncle"                         
	45          "Aunt"                          
	46          "Nephew"                        
	47          "Niece"                         
	50          "Father-in-law"                 
	51          "Mother-in-law"                 
	52          "Son-in-law"                    
	53          "Daughter-in-law"               
	54          "Brother-in-law"                
	55          "Sister-in-law"                 
	60          "Cousin,etc."                   
	70          "Not related"                   
	88          "Member of column with no"      
	98          "Not found"                     
	99          "No response"                   
;
label values tm9360   tm9360l;
label define tm9360l 
	0           "Not in universe"               
;
label values tm9390   tm9390l;
label define tm9390l 
	0           "Not in universe"               
;
label values tm9420   tm9420l;
label define tm9420l 
	0           "Not in universe"               
;
label values tm9450   tm9450l;
label define tm9450l 
	0           "Not in universe"               
;
label values tm9480   tm9480l;
label define tm9480l 
	0           "Not in universe"               
;
label values tm9510   tm9510l;
label define tm9510l 
	0           "Not in universe"               
;
label values tm9540   tm9540l;
label define tm9540l 
	0           "Not in universe"               
;
label values tm9570   tm9570l;
label define tm9570l 
	0           "Not in universe"               
;
label values tm9600   tm9600l;
label define tm9600l 
	0           "Not in universe"               
;
label values tm9630   tm9630l;
label define tm9630l 
	0           "Not in universe"               
;
label values tm9660   tm9660l;
label define tm9660l 
	0           "Not in universe"               
;
label values tm9690   tm9690l;
label define tm9690l 
	0           "Not in universe"               
;
label values u_tm9266 u_tm926y;
label define u_tm926y
	0           "Not in universe"               
	1           "One person hh - skip to SC5000"
	2           "Two person hh consisting of"   
	3           "Two person hh consisting of"   
	4           "Other"                         
;
label values u_tm9300 u_tm930y;
label define u_tm930y
	0           "Not in universe"               
;
label values u_tm9330 u_tm933y;
label define u_tm933y
	0           "Not in universe"               
;
label values u_tm9332 u_tm933k;
label define u_tm933k
	1           "Husband"                       
	2           "Wife"                          
	10          "Natural father(biological)"    
	11          "Stepfather"                    
	12          "Adoptive father"               
	13          "Foster father"                 
	14          "Natural mother"                
	15          "Stepmother"                    
	16          "Adoptive mother"               
	17          "Foster mother"                 
	18          "Unknown male parent type"      
	19          "Unknown female parent type"    
	20          "Natural son"                   
	21          "Stepson"                       
	22          "Adopted son"                   
	23          "Foster son"                    
	24          "Natural daughter"              
	25          "Stepdaughter"                  
	26          "Adopted daughter"              
	27          "Foster daughter"               
	28          "Unknown male child type"       
	29          "Unknown female child type"     
	30          "Full brother"                  
	31          "Half brother"                  
	32          "Stepbrother"                   
	33          "Adoptive brother"              
	34          "Full sister"                   
	35          "Half sister"                   
	36          "Stepsister"                    
	37          "Adoptive sister"               
	38          "Unknown sibling type"          
	40          "Grandfather"                   
	41          "Grandmother"                   
	42          "Grandson"                      
	43          "Granddaughter"                 
	44          "Uncle"                         
	45          "Aunt"                          
	46          "Nephew"                        
	47          "Niece"                         
	50          "Father-in-law"                 
	51          "Mother-in-law"                 
	52          "Son-in-law"                    
	53          "Daughter-in-law"               
	54          "Brother-in-law"                
	55          "Sister-in-law"                 
	60          "Cousin,etc."                   
	70          "Not related"                   
	88          "Member of column with no"      
	98          "Not found"                     
	99          "No response"                   
;
label values u_tm9360 u_tm936y;
label define u_tm936y
	0           "Not in universe"               
;
label values u_tm9390 u_tm939y;
label define u_tm939y
	0           "Not in universe"               
;
label values u_tm9420 u_tm942y;
label define u_tm942y
	0           "Not in universe"               
;
label values u_tm9450 u_tm945y;
label define u_tm945y
	0           "Not in universe"               
;
label values u_tm9480 u_tm948y;
label define u_tm948y
	0           "Not in universe"               
;
label values u_tm9510 u_tm951y;
label define u_tm951y
	0           "Not in universe"               
;
label values u_tm9540 u_tm954y;
label define u_tm954y
	0           "Not in universe"               
;
label values u_tm9570 u_tm957y;
label define u_tm957y
	0           "Not in universe"               
;
label values u_tm9600 u_tm960y;
label define u_tm960y
	0           "Not in universe"               
;
label values u_tm9630 u_tm963y;
label define u_tm963y
	0           "Not in universe"               
;
label values u_tm9660 u_tm966y;
label define u_tm966y
	0           "Not in universe"               
;
label values u_tm9690 u_tm969y;
label define u_tm969y
	0           "Not in universe"               
;
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8052"           
;
label values tm8002   tm8002l;
label define tm8002l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8052"           
;
label values tm8006   tm8006l;
label define tm8006l 
	0           "Not applicable"                
;
label values tm8008   tm8008l;
label define tm8008l 
	0           "Not applicable"                
;
label values tm8012   tm8012l;
label define tm8012l 
	0           "Not applicable"                
;
label values tm8014   tm8014l;
label define tm8014l 
	0           "Not applicable"                
;
label values tm8018   tm8018l;
label define tm8018l 
	0           "Not applicable"                
;
label values tm8020   tm8020l;
label define tm8020l 
	0           "Not applicable"                
;
label values tm8024   tm8024l;
label define tm8024l 
	0           "Not applicable"                
;
label values tm8026   tm8026l;
label define tm8026l 
	0           "Not applicable"                
;
label values tm8030   tm8030l;
label define tm8030l 
	0           "Not applicable"                
;
label values tm8032   tm8032l;
label define tm8032l 
	0           "Not applicable"                
;
label values tm8036   tm8036l;
label define tm8036l 
	0           "Not applicable"                
;
label values tm8038   tm8038l;
label define tm8038l 
	0           "Not applicable"                
;
label values tm8042   tm8042l;
label define tm8042l 
	0           "Not applicable"                
;
label values tm8044   tm8044l;
label define tm8044l 
	0           "Not applicable"                
;
label values tm8048   tm8048l;
label define tm8048l 
	0           "Not applicable"                
;
label values tm8050   tm8050l;
label define tm8050l 
	0           "Not applicable"                
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
label values tm_ifa0  tm_ifa0l;
label define tm_ifa0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa1  tm_ifa1l;
label define tm_ifa1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa2  tm_ifa2l;
label define tm_ifa2l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa3  tm_ifa3l;
label define tm_ifa3l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa4  tm_ifa4l;
label define tm_ifa4l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa5  tm_ifa5l;
label define tm_ifa5l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa6  tm_ifa6l;
label define tm_ifa6l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa7  tm_ifa7l;
label define tm_ifa7l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa8  tm_ifa8l;
label define tm_ifa8l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa9  tm_ifa9l;
label define tm_ifa9l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa10 tm_ifa1y;
label define tm_ifa1y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa11 tm_ifa1k;
label define tm_ifa1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa12 tm_ifa1m;
label define tm_ifa1m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa13 tm_ifa1n;
label define tm_ifa1n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa14 tm_ifa1o;
label define tm_ifa1o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa15 tm_ifa1p;
label define tm_ifa1p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa16 tm_ifa1q;
label define tm_ifa1q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa17 tm_ifa1r;
label define tm_ifa1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa18 tm_ifa1s;
label define tm_ifa1s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa19 tm_ifa1t;
label define tm_ifa1t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa20 tm_ifa2y;
label define tm_ifa2y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa21 tm_ifa2k;
label define tm_ifa2k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa22 tm_ifa2m;
label define tm_ifa2m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa23 tm_ifa2n;
label define tm_ifa2n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa24 tm_ifa2o;
label define tm_ifa2o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa25 tm_ifa2p;
label define tm_ifa2p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa26 tm_ifa2q;
label define tm_ifa2q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa27 tm_ifa2r;
label define tm_ifa2r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa28 tm_ifa2s;
label define tm_ifa2s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa29 tm_ifa2t;
label define tm_ifa2t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa30 tm_ifa3y;
label define tm_ifa3y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa31 tm_ifa3k;
label define tm_ifa3k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa32 tm_ifa3m;
label define tm_ifa3m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa33 tm_ifa3n;
label define tm_ifa3n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa34 tm_ifa3o;
label define tm_ifa3o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa35 tm_ifa3p;
label define tm_ifa3p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa36 tm_ifa3q;
label define tm_ifa3q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa37 tm_ifa3r;
label define tm_ifa3r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa38 tm_ifa3s;
label define tm_ifa3s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_ifa39 tm_ifa3t;
label define tm_ifa3t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8200   tm8200l;
label define tm8200l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8300"           
;
label values tm8202   tm8202l;
label define tm8202l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8210"           
;
label values tm8206   tm8206l;
label define tm8206l 
	0           "Not applicable"                
;
label values tm8208   tm8208l;
label define tm8208l 
	0           "Not applicable"                
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
	2           "25 to 99"                      
	3           "100 to 499"                    
	4           "500 to 999"                    
	5           "1000 or more - skip to TM8230" 
;
label values tm8226   tm8226l;
label define tm8226l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No (skip to TM8248)"           
;
label values tm8228   tm8228l;
label define tm8228l 
	0           "Not applicable"                
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 to 499"                    
	4           "500 to 999"                    
	5           "1000 or more"                  
;
label values tm8230   tm8230l;
label define tm8230l 
	0           "Don't know"                    
	1           "Yes"                           
	2           "No"                            
;
label values tm8232   tm8232l;
label define tm8232l 
	0           "Don't know"                    
	1           "Yes"                           
	2           " No"                           
;
label values tm8234   tm8234l;
label define tm8234l 
	0           "Don't know"                    
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
	0           "Don't know"                    
	1           "Taking care of home or family" 
	2           "Ill or disabled"               
	3           "Going go school"               
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
	0           "Don't know"                    
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
	2           "No (skip to TM8284)"           
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
	-1          "Don't know - skip to TM8286"   
	0           "Not applicable"                
;
label values tm8276   tm8276l;
label define tm8276l 
	-1          "Don't know - skip to TM8300"   
	0           "Not applicable"                
	1           "Yes - skip To TM8300"          
	2           "No"                            
;
label values tm8278   tm8278l;
label define tm8278l 
	0           "Not applicable"                
;
label values tm8280   tm8280l;
label define tm8280l 
	0           "Not applicable"                
	1           "Yes - skip To TM8286"          
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
	2           "No - skip To TM8300"           
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
label values imp_8218 imp_821y;
label define imp_821y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8220 imp_822y;
label define imp_822y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8226 imp_822k;
label define imp_822k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8224 imp_822l;
label define imp_822l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8228 imp_822m;
label define imp_822m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8230 imp_823y;
label define imp_823y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8232 imp_823k;
label define imp_823k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8234 imp_823l;
label define imp_823l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8240 imp_824y;
label define imp_824y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8242 imp_824k;
label define imp_824k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8244 imp_824l;
label define imp_824l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8246 imp_824m;
label define imp_824m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8248 imp_824n;
label define imp_824n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8250 imp_825y;
label define imp_825y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8252 imp_825k;
label define imp_825k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8266 imp_826y;
label define imp_826y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8268 imp_826k;
label define imp_826k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8270 imp_827y;
label define imp_827y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8272 imp_827k;
label define imp_827k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8274 imp_827l;
label define imp_827l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8276 imp_827m;
label define imp_827m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8278 imp_827n;
label define imp_827n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8282 imp_828y;
label define imp_828y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8286 imp_828k;
label define imp_828k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8288 imp_828l;
label define imp_828l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8290 imp_829y;
label define imp_829y
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_829a imp_829a;
label define imp_829a
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8292 imp_829k;
label define imp_829k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8294 imp_829l;
label define imp_829l
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
label values tm8300   tm8300l;
label define tm8300l 
	0           "Not applicable"                
	1           "15 years - skip to TM8400"     
	2           "16 to 67 years"                
	3           "68 years or over - skip to"    
;
label values tm8302   tm8302l;
label define tm8302l 
	0           "Not applicable"                
	1           "Yes - skip To TM8306"          
	2           "No"                            
;
label values tm8304   tm8304l;
label define tm8304l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip To TM8308"           
;
label values tm8306   tm8306l;
label define tm8306l 
	0           "Not applicable"                
	1           "Yes - skip To TM8310"          
	2           "No - skip To TM8400"           
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not applicable"                
	1           "Yes - Mark '171' on ISS"       
	2           "No - skip To TM8400"           
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
	-5          "Person became limited after"   
	-3          "Person was limited before"     
	0           "Not applicable"                
;
label values tm8316   tm8316l;
label define tm8316l 
	0           "Not applicable"                
	1           "Yes - skip To TM8324"          
	2           "No"                            
;
label values tm8318   tm8318l;
label define tm8318l 
	0           "Not applicable"                
;
label values tm8320   tm8320l;
label define tm8320l 
	0           "Not applicable"                
;
label values tm8322   tm8322l;
label define tm8322l 
	-3          "Had never been employed before"
	0           "Not applicable"                
;
label values tm8324   tm8324l;
label define tm8324l 
	0           "Not applicable"                
	1           "Arthritis or rheumatism"       
	2           "Autism"                        
	3           "Back or spine problems (including"
	4           "Blindness or vision problems"  
	5           "Cancer"                        
	6           "Cerebral palsy"                
	7           "Deafness or serious trouble"   
	8           "Diabetes"                      
	9           "Epilepsy"                      
	10          "Head or spinal cord injury"    
	11          "Heart trouble (including heart"
	12          "Hernia or rupture"             
	13          "High blood pressure(hypertension)"
	14          "Kidney stones or chronic kidney"
	15          "Lung or respiratory trouble"   
	16          "Mental illness"                
	17          "Mental retardation"            
	18          "Missing legs, feet, arms, hands,"
	19          "Nervous or emotional problems, or"
	20          "Paralysis of any kind"         
	21          "Senility (Alzheimer's Disease)"
	22          "Stiffness or deformity of the" 
	23          "Stomach trouble (including ulcers,"
	24          "Stroke"                        
	25          "Thyroid trouble or goiter"     
	26          "Tumor, cyst or growth"         
	27          "Other"                         
;
label values tm8326   tm8326l;
label define tm8326l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip To TM8330"           
;
label values tm8328   tm8328l;
label define tm8328l 
	0           "Not applicable"                
	1           "On the job?"                   
	2           "During service in the Armed"   
	3           "In the home?"                  
	4           "Somewhere else?"               
;
label values tm8330   tm8330l;
label define tm8330l 
	0           "Not applicable"                
	1           "Yes - skip to TM8340"          
	2           "No"                            
;
label values tm8332   tm8332l;
label define tm8332l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8342"           
;
label values tm8334   tm8334l;
label define tm8334l 
	0           "Not applicable"                
;
label values tm8336   tm8336l;
label define tm8336l 
	0           "Not applicable"                
;
label values tm8338   tm8338l;
label define tm8338l 
	-3          "Has never been able to work at"
	0           "Not applicable"                
;
label values tm8340   tm8340l;
label define tm8340l 
	0           "Not applicable"                
	1           "Yes -skip to TM8344"           
	2           "No"                            
;
label values tm8342   tm8342l;
label define tm8342l 
	0           "Not applicable"                
	1           "Full-time"                     
	2           "Part-time"                     
	3           "Not able to work - skip to"    
;
label values tm8344   tm8344l;
label define tm8344l 
	0           "Not applicable"                
	1           "Regularly"                     
	2           "Only occasionally or"          
	3           "Not able to work - skip to"    
;
label values tm8346   tm8346l;
label define tm8346l 
	0           "Not applicable"                
	1           "Yes, able to do same kind of"  
	2           "No, not able to do same kind of"
	3           "Did not work before limitation"
;
label values imp_8306 imp_830c;
label define imp_830c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8308 imp_830k;
label define imp_830k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8310 imp_831c;
label define imp_831c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_1014 imp_101c;
label define imp_101c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8318 imp_831k;
label define imp_831k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_1822 imp_182c;
label define imp_182c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8324 imp_832c;
label define imp_832c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8326 imp_832k;
label define imp_832k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8328 imp_832l;
label define imp_832l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8332 imp_833c;
label define imp_833c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8334 imp_833k;
label define imp_833k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_3438 imp_343c;
label define imp_343c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8342 imp_834c;
label define imp_834c
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8344 imp_834k;
label define imp_834k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp_8346 imp_834l;
label define imp_834l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8410"           
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not applicable"                
;
label values tm8404   tm8404l;
label define tm8404l 
	0           "Not applicable"                
;
label values tm8406   tm8406l;
label define tm8406l 
	0           "Not applicable"                
	1           "Currently attending - skip to" 
	2           "Never attended"                
;
label values tm8408   tm8408l;
label define tm8408l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8444"           
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
	1           "Public"                        
	2           "Private, church-related"       
	3           "Private, not church-related"   
	4           "Did not attend high school"    
;
label values tm8416   tm8416l;
label define tm8416l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8444"           
;
label values tm8418   tm8418l;
label define tm8418l 
	0           "Not applicable"                
;
label values tm8420   tm8420l;
label define tm8420l 
	0           "Not applicable"                
;
label values tm8422   tm8422l;
label define tm8422l 
	0           "Not applicable"                
	1           "Ph.D. or equivalent"           
	2           "Professional degree such as"   
	3           "Master's degree"               
	4           "Bachelor's degree"             
	5           "Associate degree"              
	6           "Vocational certificate or"     
	7           "Has not earned a degree - skip"
;
label values tm8424   tm8424l;
label define tm8424l 
	0           "Not applicable"                
;
label values tm8426   tm8426l;
label define tm8426l 
	0           "Not applicable"                
;
label values tm8428   tm8428l;
label define tm8428l 
	0           "Not applicable"                
	1           "Agriculture or forestry"       
	2           "Biology"                       
	3           "Business or management"        
	4           "Economics"                     
	5           "Education"                     
	6           "Engineering (including"        
	7           "English or journalism"         
	8           "Home economics"                
	9           "Law"                           
	10          "Liberal arts or humanities"    
	11          "Mathematics or statistics"     
	12          "Medicine or dentistry"         
	13          "Nursing, pharmacy, or health"  
	14          "Physical or earth sciences"    
	15          "Police science or law"         
	16          "Psychology"                    
	17          "Religion or theology"          
	18          "Social sciences (history,"     
	19          "Vocational or technical studies"
	20          "Other"                         
;
label values tm8430   tm8430l;
label define tm8430l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8444"           
;
label values tm8432   tm8432l;
label define tm8432l 
	0           "Not applicable"                
;
label values tm8434   tm8434l;
label define tm8434l 
	0           "Not applicable"                
;
label values tm8436   tm8436l;
label define tm8436l 
	0           "Not applicable"                
;
label values tm8438   tm8438l;
label define tm8438l 
	0           "Not applicable"                
;
label values tm8440   tm8440l;
label define tm8440l 
	0           "Not applicable"                
;
label values tm8442   tm8442l;
label define tm8442l 
	0           "Not applicable"                
	1           "Is still a student"            
;
label values tm8444   tm8444l;
label define tm8444l 
	0           "Not applicable"                
	1           "Yes - skip to TM8550"          
	2           "No"                            
;
label values tm8446   tm8446l;
label define tm8446l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - skip to TM8550"           
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "Not applicable"                
	1           "Job Training Partnership"      
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "Not applicable"                
	1           "Comprehensive Employment"      
;
label values tm8452   tm8452l;
label define tm8452l 
	0           "Not applicable"                
	1           "Work Incentive Program (WIN)"  
;
label values tm8454   tm8454l;
label define tm8454l 
	0           "Not applicable"                
	1           "Trade Adjustment Assistance"   
;
label values tm8456   tm8456l;
label define tm8456l 
	0           "Not applicable"                
	1           "Veterans' Training Programs"   
;
label values tm8458   tm8458l;
label define tm8458l 
	0           "Not applicable"                
	1           "Other - skip to TM8472"        
;
label values tm8460   tm8460l;
label define tm8460l 
	0           "Not applicable"                
	1           "Classroom training - job"      
;
label values tm8462   tm8462l;
label define tm8462l 
	0           "Not applicable"                
	1           "Classroom training - basic"    
;
label values tm8464   tm8464l;
label define tm8464l 
	0           "Not applicable"                
	1           "On-the-job training - skip to" 
;
label values tm8466   tm8466l;
label define tm8466l 
	0           "Not applicable"                
	1           "Job search assistance - skip"  
;
label values tm8468   tm8468l;
label define tm8468l 
	0           "Not applicable"                
	1           "Work experience - skip to"     
;
label values tm8470   tm8470l;
label define tm8470l 
	0           "Not applicable"                
	1           "Other - skip to TM8496"        
;
label values tm8472   tm8472l;
label define tm8472l 
	0           "Not applicable"                
	1           "Apprenticeship program"        
;
label values tm8474   tm8474l;
label define tm8474l 
	0           "Not applicable"                
	1           "Business, commercial, or"      
;
label values tm8476   tm8476l;
label define tm8476l 
	0           "Not applicable"                
	1           "Junior or community college"   
;
label values tm8478   tm8478l;
label define tm8478l 
	0           "Not applicable"                
	1           "Program completed at a 4 year" 
;
label values tm8480   tm8480l;
label define tm8480l 
	0           "Not applicable"                
	1           "High school vocational program"
;
label values tm8482   tm8482l;
label define tm8482l 
	0           "Not applicable"                
	1           "Training program at work"      
;
label values tm8484   tm8484l;
label define tm8484l 
	0           "Not applicable"                
	1           "Military (exclude basic"       
;
label values tm8486   tm8486l;
label define tm8486l 
	0           "Not applicable"                
	1           "Correspondence course"         
;
label values tm8488   tm8488l;
label define tm8488l 
	0           "Not applicable"                
	1           "Training or experience"        
;
label values tm8490   tm8490l;
label define tm8490l 
	0           "Not applicable"                
	1           "Sheltered workshop"            
;
label values tm8492   tm8492l;
label define tm8492l 
	0           "Not applicable"                
	1           "Vocational rehabilitation"     
;
label values tm8494   tm8494l;
label define tm8494l 
	0           "Not applicable"                
	1           "Other"                         
;
label values tm8496   tm8496l;
label define tm8496l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8498   tm8498l;
label define tm8498l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8500   tm8500l;
label define tm8500l 
	-1          "Don't know"                    
	0           "Not applicable"                
;
label values tm8502   tm8502l;
label define tm8502l 
	0           "Not applicable"                
;
label values tm8504   tm8504l;
label define tm8504l 
	-4          "Less than 1 week"              
	-3          "Currently attending"           
	0           "Not applicable"                
;
label values tm8506   tm8506l;
label define tm8506l 
	0           "Not applicable"                
	1           "Self or family"                
;
label values tm8508   tm8508l;
label define tm8508l 
	0           "Not applicable"                
	1           "Employer"                      
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "Not applicable"                
	1           "Federal, State, or local"      
;
label values tm8512   tm8512l;
label define tm8512l 
	0           "Not applicable"                
	1           "Someone else"                  
;
label values tm_ifd0  tm_ifd0l;
label define tm_ifd0l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd1  tm_ifd1l;
label define tm_ifd1l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd2  tm_ifd2l;
label define tm_ifd2l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd3  tm_ifd3l;
label define tm_ifd3l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd4  tm_ifd4l;
label define tm_ifd4l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd5  tm_ifd5l;
label define tm_ifd5l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd6  tm_ifd6l;
label define tm_ifd6l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd7  tm_ifd7l;
label define tm_ifd7l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd8  tm_ifd8l;
label define tm_ifd8l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd9  tm_ifd9l;
label define tm_ifd9l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd10 tm_ifd1c;
label define tm_ifd1c
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd11 tm_ifd1k;
label define tm_ifd1k
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd12 tm_ifd1m;
label define tm_ifd1m
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd13 tm_ifd1n;
label define tm_ifd1n
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd14 tm_ifd1o;
label define tm_ifd1o
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm_ifd15 tm_ifd1p;
label define tm_ifd1p
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Computed"                      
;
label values tm8600   tm8600l;
label define tm8600l 
	0           "Not in universe"               
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married - skip to"       
;
label values tm8602   tm8602l;
label define tm8602l 
	0           "Not in universe"               
	1           "1 - skip to TM8638"            
	2           "2"                             
	3           "3"                             
	4           "4+"                            
;
label values tm8604   tm8604l;
label define tm8604l 
	0           "Not in universe"               
;
label values tm8606   tm8606l;
label define tm8606l 
	0           "Not in universe"               
;
label values tm8608   tm8608l;
label define tm8608l 
	0           "Not in universe"               
	1           "Widowhood"                     
	2           "Divorce"                       
;
label values tm8610   tm8610l;
label define tm8610l 
	0           "Not in universe"               
;
label values tm8612   tm8612l;
label define tm8612l 
	0           "Not in universe"               
;
label values tm8614   tm8614l;
label define tm8614l 
	0           "Not in universe"               
	1           "Yes - skip to TM8620"          
	2           "No"                            
;
label values tm8616   tm8616l;
label define tm8616l 
	0           "Not in universe"               
;
label values tm8618   tm8618l;
label define tm8618l 
	0           "Not in universe"               
;
label values tm8620   tm8620l;
label define tm8620l 
	0           "Not in universe"               
	1           "2 - skip to TM8638"            
	2           "3+"                            
;
label values tm8622   tm8622l;
label define tm8622l 
	0           "Not in universe"               
;
label values tm8624   tm8624l;
label define tm8624l 
	0           "Not in universe"               
;
label values tm8626   tm8626l;
label define tm8626l 
	0           "Not in universe"               
	1           "Widowhood"                     
	2           "Divorce"                       
;
label values tm8628   tm8628l;
label define tm8628l 
	0           "Not in universe"               
;
label values tm8630   tm8630l;
label define tm8630l 
	0           "Not in universe"               
;
label values tm8632   tm8632l;
label define tm8632l 
	0           "Not in universe"               
	1           "Yes - skip to TM8638"          
	2           "No"                            
;
label values tm8634   tm8634l;
label define tm8634l 
	0           "Not in universe"               
;
label values tm8636   tm8636l;
label define tm8636l 
	0           "Not in universe"               
;
label values tm8638   tm8638l;
label define tm8638l 
	0           "Not in universe"               
	1           "Yes - skip to TM8700"          
	2           "No"                            
	3           "No, no spouse in household"    
;
label values tm8640   tm8640l;
label define tm8640l 
	0           "Not in universe"               
;
label values tm8642   tm8642l;
label define tm8642l 
	0           "Not in universe"               
;
label values tm8644   tm8644l;
label define tm8644l 
	0           "Not in universe"               
	1           "Married, spouse present - skip"
	2           "Married, spouse absent - skip" 
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated - skip to TM8652"    
;
label values tm8646   tm8646l;
label define tm8646l 
	0           "Not in universe"               
;
label values tm8648   tm8648l;
label define tm8648l 
	0           "Not in universe"               
;
label values tm8650   tm8650l;
label define tm8650l 
	0           "Not in universe"               
	1           "Yes - skip to TM8700"          
	2           "No"                            
;
label values tm8652   tm8652l;
label define tm8652l 
	0           "Not in universe"               
;
label values tm8654   tm8654l;
label define tm8654l 
	0           "Not in universe"               
;
label values fafm     fafm;   
label define fafm    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values faft     faft;   
label define faft    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values fafs     fafs;   
label define fafs    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values fasm     fasm;   
label define fasm    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values fast     fast;   
label define fast    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values fass     fass;   
label define fass    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values falm     falm;   
label define falm    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values falt     falt;   
label define falt    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values fals     fals;   
label define fals    
	0           "Not in universe"               
	1           "Everything is acceptable"      
	2           "Month was not acceptable"      
	3           "Year and/or month was not"     
;
label values ftim     ftim;   
label define ftim    
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ffme     ffme;   
label define ffme    
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values fsme     fsme;   
label define fsme    
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8700   tm8700l;
label define tm8700l 
	0           "Not in universe"               
;
label values tm8702   tm8702l;
label define tm8702l 
	-4          "Always lived here/born"        
	0           "Not in universe"               
;
label values tm8704   tm8704l;
label define tm8704l 
	0           "Not in universe"               
	1           "Same state, same county"       
	2           "Same state, different county"  
;
label values tm8706   tm8706l;
label define tm8706l 
	0           "Not in universe"               
	1           "Alabama"                       
	2           "Alaska"                        
	3           "Arizona"                       
	4           "Arkansas"                      
	5           "California"                    
	6           "Colorado"                      
	7           "Connecticut"                   
	8           "Delaware"                      
	9           "District of Columbia"          
	10          "Florida"                       
	11          "Georgia"                       
	12          "Hawaii"                        
	13          "Idaho"                         
	14          "Illinois"                      
	15          "Indiana"                       
	16          "Iowa"                          
	17          "Kansas"                        
	18          "Kentucky"                      
	19          "Louisiana"                     
	20          "Maine"                         
	21          "Maryland"                      
	22          "Massachusetts"                 
	23          "Michigan"                      
	24          "Minnesota"                     
	25          "Mississippi"                   
	26          "Missouri"                      
	27          "Montana"                       
	28          "Nebraska"                      
	29          "Nevada"                        
	30          "New Hampshire"                 
	31          "New Jersey"                    
	32          "New Mexico"                    
	33          "New York"                      
	34          "North Carolina"                
	35          "North Dakota"                  
	36          "Ohio"                          
	37          "Oklahoma"                      
	38          "Oregon"                        
	39          "Pennsylvania"                  
	40          "Rhode Island"                  
	41          "South Carolina"                
	42          "South Dakota"                  
	43          "Tennessee"                     
	44          "Texas"                         
	45          "Utah"                          
	46          "Vermont"                       
	47          "Virginia"                      
	48          "Washington"                    
	49          "West Virginia"                 
	50          "Wisconsin"                     
	51          "Wyoming"                       
	60          "Puerto Rico"                   
	61          "Outlying area of U.S."         
	62          "Austria"                       
	63          "Canada"                        
	64          "China"                         
	65          "Cuba"                          
	66          "Czechoslovakia"                
	67          "Dominican Republic"            
	68          "Germany"                       
	69          "Greece"                        
	70          "Hungary"                       
	71          "India"                         
	72          "Ireland"                       
	73          "Italy"                         
	74          "Jamaica"                       
	75          "Japan"                         
	76          "Korea"                         
	77          "Mexico"                        
	78          "Norway"                        
	79          "Philippines"                   
	80          "Poland"                        
	81          "Portugal"                      
	82          "Sweden"                        
	83          "United Kingdom"                
	84          "U. S. S. R."                   
	85          "Vietnam"                       
	86          "Other Europe"                  
	87          "Other Asia"                    
	88          "Central America"               
	89          "South America"                 
	90          "Middle East"                   
	91          "Africa"                        
	99          "Other (specify)"               
;
label values tm8708   tm8708l;
label define tm8708l 
	0           "Not in universe"               
	1           "Alabama"                       
	2           "Alaska"                        
	3           "Arizona"                       
	4           "Arkansas"                      
	5           "California"                    
	6           "Colorado"                      
	7           "Connecticut"                   
	8           "Delaware"                      
	9           "District of Columbia"          
	10          "Florida"                       
	11          "Georgia"                       
	12          "Hawaii"                        
	13          "Idaho"                         
	14          "Illinois"                      
	15          "Indiana"                       
	16          "Iowa"                          
	17          "Kansas"                        
	18          "Kentucky"                      
	19          "Louisiana"                     
	20          "Maine"                         
	21          "Maryland"                      
	22          "Massachusetts"                 
	23          "Michigan"                      
	24          "Minnesota"                     
	25          "Mississippi"                   
	26          "Missouri"                      
	27          "Montana"                       
	28          "Nebraska"                      
	29          "Nevada"                        
	30          "New Hampshire"                 
	31          "New Jersey"                    
	32          "New Mexico"                    
	33          "New York"                      
	34          "North Carolina"                
	35          "North Dakota"                  
	36          "Ohio"                          
	37          "Oklahoma"                      
	38          "Oregon"                        
	39          "Pennsylvania"                  
	40          "Rhode Island"                  
	41          "South Carolina"                
	42          "South Dakota"                  
	43          "Tennessee"                     
	44          "Texas"                         
	45          "Utah"                          
	46          "Vermont"                       
	47          "Virginia"                      
	48          "Washington"                    
	49          "West Virginia"                 
	50          "Wisconsin"                     
	51          "Wyoming"                       
	60          "Puerto Rico"                   
	61          "Outlying area of U. S."        
	62          "Austria"                       
	63          "Canada"                        
	64          "China"                         
	65          "Cuba"                          
	66          "Czechoslovakia"                
	67          "Dominican Republic"            
	68          "Germany"                       
	69          "Greece"                        
	70          "Hungary"                       
	71          "India"                         
	72          "Ireland"                       
	73          "Italy"                         
	74          "Jamaica"                       
	75          "Japan"                         
	76          "Korea"                         
	77          "Mexico"                        
	78          "Norway"                        
	79          "Philippines"                   
	80          "Poland"                        
	81          "Portugal"                      
	82          "Sweden"                        
	83          "United Kingdom"                
	84          "U. S. S. R."                   
	85          "Vietnam"                       
	86          "Other Europe"                  
	87          "Other Asia"                    
	88          "Central America"               
	89          "South America"                 
	90          "Middle East"                   
	91          "Africa"                        
	99          "Other(specify)"                
;
label values tm8709   tm8709l;
label define tm8709l 
	-4          "Lived there since birth"       
	0           "Not in universe"               
;
label values tm8710   tm8710l;
label define tm8710l 
	0           "Not in universe"               
;
label values tm8712   tm8712l;
label define tm8712l 
	0           "Not in universe"               
;
label values tm8714   tm8714l;
label define tm8714l 
	0           "Not in universe"               
;
label values tm8716   tm8716l;
label define tm8716l 
	0           "Not in universe"               
;
label values tm8718   tm8718l;
label define tm8718l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8730"           
;
label values tm8720   tm8720l;
label define tm8720l 
	0           "Not in universe"               
	1           "Alabama"                       
	2           "Alaska"                        
	3           "Arizona"                       
	4           "Arkansas"                      
	5           "California"                    
	6           "Colorado"                      
	7           "Connecticut"                   
	8           "Delaware"                      
	9           "District of Columbia"          
	10          "Florida"                       
	11          "Georgia"                       
	12          "Hawaii"                        
	13          "Idaho"                         
	14          "Illinois"                      
	15          "Indiana"                       
	16          "Iowa"                          
	17          "Kansas"                        
	18          "Kentucky"                      
	19          "Louisiana"                     
	20          "Maine"                         
	21          "Maryland"                      
	22          "Massachusetts"                 
	23          "Michigan"                      
	24          "Minnesota"                     
	25          "Mississippi"                   
	26          "Missouri"                      
	27          "Montana"                       
	28          "Nebraska"                      
	29          "Nevada"                        
	30          "New Hampshire"                 
	31          "New Jersey"                    
	32          "New Nexico"                    
	33          "New York"                      
	34          "North Carolina"                
	35          "North Dakota"                  
	36          "Ohio"                          
	37          "Oklahoma"                      
	38          "Oregon"                        
	39          "Pennsylvania"                  
	40          "Rhode island"                  
	41          "South Carolina"                
	42          "South Dakota"                  
	43          "Tennessee"                     
	44          "Texas"                         
	45          "Utah"                          
	46          "Vermont"                       
	47          "Virginia"                      
	48          "Washington"                    
	49          "West Virginia"                 
	50          "Wisconsin"                     
	51          "Wyoming"                       
	52          "United States (Unknown State)" 
	60          "Puerto Rico"                   
	61          "Outlying area of U. S."        
	62          "Austria"                       
	63          "Canada"                        
	64          "China"                         
	65          "Cuba"                          
	66          "Czechoslovakia"                
	67          "Dominican Republic"            
	68          "Germany"                       
	69          "Greece"                        
	70          "Hungary"                       
	71          "India"                         
	72          "Ireland"                       
	73          "Italy"                         
	74          "Jamaica"                       
	75          "Japan"                         
	76          "Korea"                         
	77          "Mexico"                        
	78          "Norway"                        
	79          "Philippines"                   
	80          "Poland"                        
	81          "Portugal"                      
	82          "Sweden"                        
	83          "United Kingdom"                
	84          "U. S. S. R."                   
	85          "Vietnam"                       
	86          "Other Europe"                  
	87          "Other Asia"                    
	88          "Central America"               
	89          "South America"                 
	90          "Middle East"                   
	91          "Africa"                        
	92          "Overseas (country unknown)"    
	99          "Other(specify)"                
;
label values tm8722   tm8722l;
label define tm8722l 
	0           "Not in universe"               
;
label values tm8724   tm8724l;
label define tm8724l 
	0           "Not in universe"               
;
label values tm8726   tm8726l;
label define tm8726l 
	0           "Not in universe"               
;
label values tm8728   tm8728l;
label define tm8728l 
	0           "Not in universe"               
;
label values tm8730   tm8730l;
label define tm8730l 
	0           "Not in universe"               
	1           "Alabama"                       
	2           "Alaska"                        
	3           "Arizona"                       
	4           "Arkansas"                      
	5           "California"                    
	6           "Colorado"                      
	7           "Connecticut"                   
	8           "Delaware"                      
	9           "District of Columbia"          
	10          "Florida"                       
	11          "Georgia"                       
	12          "Hawaii"                        
	13          "Idaho"                         
	14          "Illinois"                      
	15          "Indiana"                       
	16          "Iowa"                          
	17          "Kansas"                        
	18          "Kentucky"                      
	19          "Louisiana"                     
	20          "Maine"                         
	21          "Maryland"                      
	22          "Massachusetts"                 
	23          "Michigan"                      
	24          "Minnesota"                     
	25          "Mississippi"                   
	26          "Missouri"                      
	27          "Montana"                       
	28          "Nebraska"                      
	29          "Nevada"                        
	30          "New Hampshire"                 
	31          "New Jersey"                    
	32          "New Mexico"                    
	33          "New York"                      
	34          "North Carolina"                
	35          "North Dakota"                  
	36          "Ohio"                          
	37          "Oklahoma"                      
	38          "Oregon"                        
	39          "Pennsylvania"                  
	40          "Rhode Island"                  
	41          "South Carolina"                
	42          "South Dakota"                  
	43          "Tennessee"                     
	44          "Texas"                         
	45          "Utah"                          
	46          "Vermont"                       
	47          "Virginia"                      
	48          "Washington"                    
	49          "West Virginia"                 
	50          "Wisconsin"                     
	51          "Wyoming"                       
	52          "United States (Unknown State)" 
	60          "Puerto Rico"                   
	61          "Outlying area of U. S."        
	62          "Austria"                       
	63          "Canada"                        
	64          "China"                         
	65          "Cuba"                          
	66          "Czechoslovakia"                
	67          "Dominican Republic"            
	68          "Germany"                       
	69          "Greece"                        
	70          "Hungary"                       
	71          "India"                         
	72          "Ireland"                       
	73          "Italy"                         
	74          "Jamaica"                       
	75          "Japan"                         
	76          "Korea"                         
	77          "Mexico"                        
	78          "Norway"                        
	79          "Philippines"                   
	80          "Poland"                        
	81          "Portugal"                      
	82          "Sweden"                        
	83          "United Kingdom"                
	84          "U. S. S. R."                   
	85          "Vietnam"                       
	86          "Other Europe"                  
	87          "Other Asia"                    
	88          "Central America"               
	89          "South America"                 
	90          "Middle East"                   
	91          "Africa"                        
	92          "Overseas (country unknown)"    
	99          "Other(specify)"                
;
label values tm8732   tm8732l;
label define tm8732l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8750"           
;
label values tm8734   tm8734l;
label define tm8734l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
	3           "No, born abroad of American"   
;
label values tm8736   tm8736l;
label define tm8736l 
	0           "Not applicable"                
	1           "Before 1959"                   
	2           "1960 - 1964"                   
	3           "1965 - 1969"                   
	4           "1970 - 1974"                   
	5           "1975 - 1979"                   
	6           "1980 - 1981"                   
	7           "1982 - 1984"                   
	8           "1985 - 1991"                   
;
label values tm_mig1  tm_mig1l;
label define tm_mig1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ct8700   ct8700l;
label define ct8700l 
	0           "Not computed"                  
	1           "Computed"                      
;
label values tm_mig2  tm_mig2l;
label define tm_mig2l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_mig3  tm_mig3l;
label define tm_mig3l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_mig4  tm_mig4l;
label define tm_mig4l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_mig5  tm_mig5l;
label define tm_mig5l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ct8710   ct8710l;
label define ct8710l 
	0           "Not computed"                  
	1           "Computed"                      
;
label values ct8714   ct8714l;
label define ct8714l 
	0           "Not computed"                  
	1           "Computed"                      
;
label values tm_mig8  tm_mig8l;
label define tm_mig8l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_mig9  tm_mig9l;
label define tm_mig9l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ct8722   ct8722l;
label define ct8722l 
	0           "Not computed"                  
	1           "Computed"                      
;
label values tm_mig10 tm_mig1e;
label define tm_mig1e
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ct8726   ct8726l;
label define ct8726l 
	0           "Not computed"                  
	1           "Computed"                      
;
label values tm_mig11 tm_mig1k;
label define tm_mig1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_mig12 tm_mig1m;
label define tm_mig1m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_mig13 tm_mig1n;
label define tm_mig1n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_mig14 tm_mig1o;
label define tm_mig1o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8750   tm8750l;
label define tm8750l 
	0           "Not in universe"               
	1           "Female- skip to TM8754"        
	2           "Male, 18+ years old"           
	3           "Male, 15 - 17 years old -"     
;
label values tm8752   tm8752l;
label define tm8752l 
	-1          "Don't know - skip to the end"  
	-3          "None - skip to the end"        
	0           "Not in universe"               
	99          "Don't know for men only"       
;
label values tm8754   tm8754l;
label define tm8754l 
	-3          "None - skip to the end"        
	0           "Not in universe"               
;
label values tm8756   tm8756l;
label define tm8756l 
	0           "Not in universe"               
	1           "Yes - skip to the end"         
	2           "No"                            
;
label values tm8758   tm8758l;
label define tm8758l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8778"           
;
label values tm8760   tm8760l;
label define tm8760l 
	0           "Not in universe"               
;
label values tm8762   tm8762l;
label define tm8762l 
	0           "Not in universe"               
;
label values tm8764   tm8764l;
label define tm8764l 
	999         "Edited person number"          
;
label values tm8766   tm8766l;
label define tm8766l 
	0           "Not in universe"               
;
label values tm8768   tm8768l;
label define tm8768l 
	0           "Not in universe"               
;
label values tm8770   tm8770l;
label define tm8770l 
	999         "Edited person number"          
;
label values tm8778   tm8778l;
label define tm8778l 
	0           "Not in universe"               
	1           "One child - skip to TM9266"    
	2           "2+ children"                   
;
label values tm8780   tm8780l;
label define tm8780l 
	0           "Not in universe"               
;
label values tm8782   tm8782l;
label define tm8782l 
	0           "Not in universe"               
;
label values tm8784   tm8784l;
label define tm8784l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8792"           
;
label values tm8786   tm8786l;
label define tm8786l 
	0           "Not in universe"               
	1           "Resides in this household -"   
	2           "In his/her own household"      
	3           "With own father - skip to"     
	4           "With own grandparent(s) - skip"
	5           "With adoptive parents - skip"  
	6           "Other relatives - skip"        
	7           "In foster care/foster family"  
	8           "In an institution (hospital)"  
	9           "In school - skip to TM8792"    
	10          "In correctional facility -"    
	11          "Other - skip to TM8792"        
	12          "Deceased- skip to TM8792"      
	13          "Don't know"                    
;
label values tm8788   tm8788l;
label define tm8788l 
	999         "Edited person number"          
;
label values tm8792   tm8792l;
label define tm8792l 
	0           "Not in universe"               
;
label values tm8794   tm8794l;
label define tm8794l 
	0           "Not in universe"               
;
label values tm8796   tm8796l;
label define tm8796l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM9266"           
;
label values tm8798   tm8798l;
label define tm8798l 
	0           "Not in universe"               
	1           "Resides in this household -"   
	2           "In his/her own household -"    
	3           "With own father - skip to TM9266"
	4           "With own grandparent(s) - skip"
	5           "With adoptive parents - skip"  
	6           "Other relatives - skip to TM9266"
	7           "In foster care/foster family"  
	8           "In an institution (hospital)"  
	9           "In school - skip to TM9266"    
	10          "In correctional facility -"    
	11          "Other - skip to TM9266"        
	12          "Deceased - skip to TM9266"     
	13          "Don't know"                    
;
label values tm8800   tm8800l;
label define tm8800l 
	999         "Edited person number"          
;
label values tm_fer1  tm_fer1l;
label define tm_fer1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer2  tm_fer2l;
label define tm_fer2l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer2a tm_fer2a;
label define tm_fer2a
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer3  tm_fer3l;
label define tm_fer3l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer4  tm_fer4l;
label define tm_fer4l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer5  tm_fer5l;
label define tm_fer5l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer6  tm_fer6l;
label define tm_fer6l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer7  tm_fer7l;
label define tm_fer7l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm_fer9  tm_fer9l;
label define tm_fer9l
	0           "Not imputed"                   
	1           "Imputed"                       
;

/*
Copyright 2005 shared by the National Bureau of Economic Research and Jean Roth

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
