log using sip92t2, text replace
set mem 1000m
*This program reads the 1992 SIPP Wave 2 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 18:27:24 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip92t2
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1992\sip92t2.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip92t2"

*Everything below this point are value labels

#delimit ;

;
label values rotation rotation;
label define rotation
	1           "Interview month:  September 1992"
	2           "Interview month:  June 1992"   
	3           "Interview month:  July 1992"   
	4           "Interview month:  August 1992" 
;
label values item36b  item36b;
label define item36b 
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other Type A"                  
	23          "Entire household deceased,"    
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
	28          "Merged households across panels"
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
;
label values ppmis1   ppmis1l;
label define ppmis1l 
	1           "Interview"                     
	2           "Noninterview"                  
;
label values ppmis2   ppmis2l;
label define ppmis2l 
	1           "Interview"                     
	2           "Noninterview"                  
;
label values ppmis3   ppmis3l;
label define ppmis3l 
	1           "Interview"                     
	2           "Noninterview"                  
;
label values ppmis4   ppmis4l;
label define ppmis4l 
	1           "Interview"                     
	2           "Noninterview"                  
;
label values ppmis5   ppmis5l;
label define ppmis5l 
	1           "Interview"                     
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
label values grdcmpl  grdcmpl;
label define grdcmpl 
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
	1           "One person HH"                 
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
	18          "Unknown father type"           
	19          "Unknown mother type"           
	20          "Natural son"                   
	21          "Stepson"                       
	22          "Adopted son"                   
	23          "Foster son"                    
	24          "Natural daughter"              
	25          "Stepdaughter"                  
	26          "Adopted daughter"              
	27          "Foster daughter"               
	28          "Unknown son type"              
	29          "Unknown daughter type"         
	30          "Full brother"                  
	31          "Half brother"                  
	32          "Stepbrother"                   
	33          "Adoptive brother"              
	34          "Full sister"                   
	35          "Half sister"                   
	36          "Stepsister"                    
	37          "Adoptive sister"               
	38          "Unknown brother type"          
	39          "Unknown sister type"           
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
	60          "Other relative (Cousin, etc.)" 
	70          "Nonrelative"                   
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
label values tm8300   tm8300l;
label define tm8300l 
	0           "Not applicable, 15 or under,"  
	2           "16 to 67"                      
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
	1           "Yes - skip To TM8310 or TM8314"
	2           "No - end of section"           
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not applicable"                
	1           "Yes - skip to TM8310"          
	2           "No - end of section"           
;
label values tm8310   tm8310l;
label define tm8310l 
	-1          "Don't know"                    
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
	2           "No - skip to TM8318 or TM8322" 
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
	2           "No - skip to TM8330"           
;
label values tm8328   tm8328l;
label define tm8328l 
	0           "Not applicable"                
	1           "On the job"                    
	2           "During service in the Armed"   
	3           "In the home"                   
	4           "Somewhere else"                
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
	1           "Yes - skip to TM8334 or TM8338"
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
	3           "Not able to work - end of section"
;
label values tm8344   tm8344l;
label define tm8344l 
	0           "Not applicable"                
	1           "Regularly"                     
	2           "Only occasionally or"          
	3           "Not able to work - end of"     
;
label values tm8346   tm8346l;
label define tm8346l 
	0           "Not applicable"                
	1           "Yes, able to do same kind of"  
	2           "No, not able to do same kind of"
	3           "Did not work before limitation"
;
label values imp8306  imp8306l;
label define imp8306l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8308  imp8308l;
label define imp8308l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8310  imp8310l;
label define imp8310l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp1214  imp1214l;
label define imp1214l
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
label values imp2022  imp2022l;
label define imp2022l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8324  imp8324l;
label define imp8324l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8326  imp8326l;
label define imp8326l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8328  imp8328l;
label define imp8328l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8332  imp8332l;
label define imp8332l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8334  imp8334l;
label define imp8334l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp3438  imp3438l;
label define imp3438l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8342  imp8342l;
label define imp8342l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8344  imp8344l;
label define imp8344l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8346  imp8346l;
label define imp8346l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not applicable"                
	1           "No - skip to TM8402 or TM8406" 
	2           "Yes - skip to TM8410"          
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
	1           "Currently attending - end of"  
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
	6           "Vocational, technical, or"     
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
	1           "Yes - end of section"          
	2           "No"                            
;
label values tm8446   tm8446l;
label define tm8446l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No - end of section"           
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8452   tm8452l;
label define tm8452l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8454   tm8454l;
label define tm8454l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8456   tm8456l;
label define tm8456l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8458   tm8458l;
label define tm8458l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8460   tm8460l;
label define tm8460l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8462   tm8462l;
label define tm8462l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8464   tm8464l;
label define tm8464l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8466   tm8466l;
label define tm8466l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8468   tm8468l;
label define tm8468l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8470   tm8470l;
label define tm8470l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8472   tm8472l;
label define tm8472l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8474   tm8474l;
label define tm8474l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8476   tm8476l;
label define tm8476l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8478   tm8478l;
label define tm8478l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8480   tm8480l;
label define tm8480l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8482   tm8482l;
label define tm8482l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8484   tm8484l;
label define tm8484l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8486   tm8486l;
label define tm8486l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8488   tm8488l;
label define tm8488l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8490   tm8490l;
label define tm8490l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8492   tm8492l;
label define tm8492l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8494   tm8494l;
label define tm8494l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8496   tm8496l;
label define tm8496l 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values tm8498   tm8498l;
label define tm8498l 
	0           "Not applicable"                
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
	-4          "Less than 1 week"              
	0           "Not applicable"                
;
label values tm8506   tm8506l;
label define tm8506l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8508   tm8508l;
label define tm8508l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values tm8512   tm8512l;
label define tm8512l 
	0           "Not applicable or No"          
	1           "Yes"                           
;
label values imp8414  imp8414l;
label define imp8414l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8420  imp8420l;
label define imp8420l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8426 or TM8434"
;
label values imp8422  imp8422l;
label define imp8422l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8428=9,12, or 17"
;
label values imp8426  imp8426l;
label define imp8426l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8420 > TM8426"
;
label values imp8428  imp8428l;
label define imp8428l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8434  imp8434l;
label define imp8434l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8420 and TM8426"
;
label values imp8436  imp8436l;
label define imp8436l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8440  imp8440l;
label define imp8440l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8420 > TM8440"
;
label values imp8446  imp8446l;
label define imp8446l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp4858  imp4858l;
label define imp4858l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp6070  imp6070l;
label define imp6070l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp7294  imp7294l;
label define imp7294l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8496  imp8496l;
label define imp8496l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8500  imp8500l;
label define imp8500l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8504=-3"
;
label values imp8502  imp8502l;
label define imp8502l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp0612  imp0612l;
label define imp0612l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8600   tm8600l;
label define tm8600l 
	0           "Not in universe"               
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
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
	1           "Yes - skip to TM8640"          
	2           "No - skip to TM8640"           
	3           "No, no spouse in household -"  
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
	1           "Married, spouse present - end" 
	2           "Married, spouse absent - end"  
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
	1           "Yes - end of section"          
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
label values imp8602  imp8602l;
label define imp8602l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8608  imp8608l;
label define imp8608l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8626  imp8626l;
label define imp8626l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8700   tm8700l;
label define tm8700l 
	0           "Not in universe"               
;
label values tm8702   tm8702l;
label define tm8702l 
	-004        "Always lived here/born"        
	0           "Not in universe"               
;
label values tm8704   tm8704l;
label define tm8704l 
	0           "Not in universe"               
	1           "Same state, same county -"     
	2           "Same state, different county -"
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
	52          "United States (State unknown)" 
;
label values tm8708   tm8708l;
label define tm8708l 
	0           "Not in universe"               
	60          "Puerto Rico"                   
	61          "Outlying area of U. S."        
	62          "Austria"                       
	63          "Canada"                        
	64          "China (Includes Mainland China,"
	65          "Cuba"                          
	66          "Czechoslovakia"                
	67          "Dominican Republic"            
	68          "Germany (East and West Germany)"
	69          "Greece"                        
	70          "Hungary"                       
	71          "India"                         
	72          "Ireland (Excludes Northern Ireland)"
	73          "Italy"                         
	74          "Jamaica"                       
	75          "Japan"                         
	76          "Korea (Includes North and South Korea)"
	77          "Mexico"                        
	78          "Norway"                        
	79          "Philippines"                   
	80          "Poland"                        
	81          "Portugal"                      
	82          "Sweden"                        
	83          "United Kingdom (Includes England,"
	84          "U.S.S.R."                      
	85          "Vietnam"                       
	86          "Other Europe"                  
	87          "Other Asia"                    
	88          "Central America"               
	89          "South America"                 
	90          "Middle East"                   
	91          "Africa"                        
	92          "Overseas (country unknown)"    
	99          "Other"                         
;
label values tm8709   tm8709l;
label define tm8709l 
	-4          "Lived in the previous home"    
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
	61          "Outlying area of U. S.(Includes"
	62          "Austria"                       
	63          "Canada"                        
	64          "China (Includes Mainland China,"
	65          "Cuba"                          
	66          "Czechoslovakia"                
	67          "Dominican Republic"            
	68          "Germany (East and West Germany)"
	69          "Greece"                        
	70          "Hungary"                       
	71          "India"                         
	72          "Ireland (Excludes Northern Ireland)"
	73          "Italy"                         
	74          "Jamaica"                       
	75          "Japan"                         
	76          "Korea (Includes North and South"
	77          "Mexico"                        
	78          "Norway"                        
	79          "Philippines"                   
	80          "Poland"                        
	81          "Portugal"                      
	82          "Sweden"                        
	83          "United Kingdom (Includes England,"
	84          "U.S.S.R."                      
	85          "Vietnam"                       
	86          "Other Europe"                  
	87          "Other Asia"                    
	88          "Central America"               
	89          "South America"                 
	90          "Middle East"                   
	91          "Africa"                        
	92          "Overseas (country unknown)"    
	99          "Other"                         
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
	61          "Outlying area of U. S.(Includes"
	62          "Austria"                       
	63          "Canada"                        
	64          "China (Includes Mainland China,"
	65          "Cuba"                          
	66          "Czechoslovakia"                
	67          "Dominican Republic"            
	68          "Germany (East and West Germany)"
	69          "Greece"                        
	70          "Hungary"                       
	71          "India"                         
	72          "Ireland (Excludes Northern Ireland)"
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
	83          "United Kingdom (Includes England,"
	84          "U.S.S.R."                      
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
	2           "No - end of section"           
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
	0           "Not in universe"               
	1           "1959 or earlier"               
	2           "1960-1964"                     
	3           "1965-1969"                     
	4           "1970-1974"                     
	5           "1975-1979"                     
	6           "1980-1981"                     
	7           "1982-1984"                     
	8           "1985-1992"                     
;
label values imp0002  imp0002l;
label define imp0002l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8704=1 or 2"
	3           "Logical assignment based on counter"
	4           "Logical assignment based on month entered/left"
	5           "Imputed: Set according to month of interview"
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
label values imp8708  imp8708l;
label define imp8708l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp1012  imp1012l;
label define imp1012l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Imputed:  Logical assignment based on TM8714 is not valid"
	3           "Logical assignment based on TM8716 is less than TM8702"
	4           "Logical assignment based on counter"
;
label values imp8720  imp8720l;
label define imp8720l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp2224  imp2224l;
label define imp2224l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Logical assignment based on TM8724 is valid and"
;
label values imp2628  imp2628l;
label define imp2628l
	0           "Not imputed"                   
	1           "Imputed"                       
	2           "Imputed: Logical assignment based on TM8702 is not valid"
	3           "Imputed: Logical assignment based on TM8726 is not valid"
	4           "Logical assignment based on month entered/left"
;
label values imp8730  imp8730l;
label define imp8730l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8732  imp8732l;
label define imp8732l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8734  imp8734l;
label define imp8734l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8736  imp8736l;
label define imp8736l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8750   tm8750l;
label define tm8750l 
	0           "Not in universe"               
	1           "Female, 15 or older - skip to TM8754"
	2           "Male, 18+ years old - skip to TM8752"
	3           "Male, 15 - 17 years old - end of section"
;
label values tm8752   tm8752l;
label define tm8752l 
	-1          "Don't know - end of section"   
	-3          "None - end of section"         
	0           "Not in universe"               
;
label values tm8754   tm8754l;
label define tm8754l 
	-3          "None - end of section"         
	0           "Not in universe"               
;
label values tm8756   tm8756l;
label define tm8756l 
	0           "Not in universe"               
	1           "Yes - end of section"          
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
	999         "Child not located in"          
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
	999         "Child not located in"          
;
label values tm8778   tm8778l;
label define tm8778l 
	0           "Not in universe"               
	1           "One child - skip to TM8792"    
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
	12          "Child is deceased- skip to TM8792"
	13          "Don't know"                    
;
label values tm8788   tm8788l;
label define tm8788l 
	999         "Child not located in the"      
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
	2           "No - end of section"           
;
label values tm8798   tm8798l;
label define tm8798l 
	0           "Not in universe"               
	1           "Resides in this household -"   
	2           "In his/her own household -"    
	3           "With own father - end of section"
	4           "With own grandparent(s) - end" 
	5           "With adoptive parents - end"   
	6           "Other relatives - end of section"
	7           "In foster care/foster family"  
	8           "In an institution (hospital)"  
	9           "In school - end of section"    
	10          "In correctional facility -"    
	11          "Other - end of section"        
	12          "Child is deceased - end of section"
	13          "Don't know"                    
;
label values tm8800   tm8800l;
label define tm8800l 
	999         "Child not found in the"        
;
label values imp8754  imp8754l;
label define imp8754l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8782  imp8782l;
label define imp8782l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8780  imp8780l;
label define imp8780l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8786  imp8786l;
label define imp8786l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8758  imp8758l;
label define imp8758l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8794  imp8794l;
label define imp8794l
	0           "Not imputed"                   
	1           "Imputed.  Universe:  TM8778=1" 
	2           "Imputed.  Universe:  TM8778=1" 
	3           "Imputed.  Universe:  TM8778"   
	4           "Imputed.  Universe:  TM8778"   
;
label values imp8792  imp8792l;
label define imp8792l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imp8798  imp8798l;
label define imp8798l
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
