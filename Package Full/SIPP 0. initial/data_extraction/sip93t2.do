log using sip93t2, text replace
set mem 1000m
*This program reads the 1993 SIPP Wave 2 Topical Module Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Thu Mar 18 11:53:26 EST 2004
*Please report errors to jroth@nber.org
*run with do sip93t2
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1993\sip93t2.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip93t2"

*Everything below this point are value labels

#delimit ;

;
label values state    state;  
label define state   
	1           "ALABAMA"                       
	4           "ARIZONA"                       
	5           "ARKANSAS"                      
	6           "CALIFORNIA"                    
	8           "COLORADO"                      
	9           "CONNECTICUT"                   
	10          "DELAWARE"                      
	11          "DISTRICT OF COLUMBIA"          
	12          "FLORIDA"                       
	13          "GEORGIA"                       
	15          "HAWAII"                        
	17          "ILLINOIS"                      
	18          "INDIANA"                       
	20          "KANSAS"                        
	21          "KENTUCKY"                      
	22          "LOUISIANA"                     
	24          "MARYLAND"                      
	25          "MASSACHUSETTS"                 
	26          "MICHIGAN"                      
	27          "MINNESOTA"                     
	28          "MISSISSIPPI"                   
	29          "MISSOURI"                      
	31          "NEBRASKA"                      
	32          "NEVADA"                        
	33          "NEW HAMPSHIRE"                 
	34          "NEW JERSEY"                    
	35          "NEW MEXICO"                    
	36          "NEW YORK"                      
	37          "NORTH CAROLINA"                
	39          "OHIO"                          
	40          "OKLAHOMA"                      
	41          "OREGON"                        
	42          "PENNSYLVANIA"                  
	44          "RHODE ISLAND"                  
	45          "SOUTH CAROLINA"                
	47          "TENNESSEE"                     
	48          "TEXAS"                         
	49          "UTAH"                          
	51          "VIRGINIA"                      
	53          "WASHINGTON"                    
	54          "WEST VIRGINIA"                 
	55          "WISCONSIN"                     
	61          "MAINE,VERMONT"                 
	62          "IOWA,NORTH DAKOTA,SOUTH DAKOTA"
	63          "ALASKA,IDAHO,MONTANA,WYOMING"  
;
label values item36b  item36b;
label define item36b 
	1           "INTERVIEWE D"                  
	2           "NO ONE HOME"                   
	3           "TEMPORARILY ABSENT"            
	4           "REFUSED"                       
	5           "UNABLE TO LOCATE"              
	6           "OTHER TYPE A"                  
	9           "VACANT"                        
	10          "OCCUPIED BY PERSONS WITH URE"  
	11          "UNFIT OR TO BE DEMOLISHED"     
	12          "UNDER CONSTRUCTION, NOT READY" 
	13          "CONVERTED TO TEMPORARY BUSINESS"
	14          "UNOCCUPIED SITE FOR MOBILE"    
	15          "PERMIT GRANTED, CONSTRUCTION"  
	17          "DEMOLISHED"                    
	18          "HOUSE OR TRAILER MOVED"        
	19          "CONVERTED TO PERMANENT BUSINESS"
	20          "MERGED"                        
	21          "CONDEMNED"                     
	23          "ENTIRE HOUSEHOLD DECEASED,"    
	24          "MOVED, ADDRESS UNKNOWN"        
	25          "MOVED WITHIN COUNTRY BEYOND"   
	26          "ALL SAMPLE PERSONS RELISTED ON"
	28          "MERGED HHLDS ACROSS PANELS"    
;
label values intvw    intvw;  
label define intvw   
	0           "NOT APPLICABLE (CHILDREN"      
	1           "INTERVIEW (SELF)"              
	2           "INTERVIEW (PROXY)"             
	3           "NONINTERVIEW - TYPE Z REFUSAL" 
	4           "NONINTERVIEW - TYPE Z OTHER"   
;
label values pp_mis5  pp_mis5l;
label define pp_mis5l
	1           "INTERVIEW"                     
	2           "NON-INTERVIEW"                 
;
label values rrp      rrp;    
label define rrp     
	0           "NOT A SAMPLE PERSON IN THIS"   
	1           "HOUSEHOLD REFERENCE PERSON,"   
	2           "HOUSEHOLD REFERENCE PERSON"    
	3           "SPOUSE OF HOUSEHOLD REFERENCE" 
	4           "CHILD OF HOUSEHOLD REFERENCE"  
	5           "OTHER RELATIVE OF HOUSEHOLD"   
	6           "NON-RELATIVE OF HOUSEHOLD"     
	7           "NON-RELATIVE OF HOUSEHOLD"     
;
label values age      age;    
label define age     
	0           "LESS THAN 1 FULL YEAR"         
	1           "1 YEAR ETC."                   
;
label values sex      sex;    
label define sex     
	1           "MALE"                          
	2           "FEMALE"                        
;
label values race     race;   
label define race    
	1           "WHITE"                         
	2           "BLACK"                         
	3           "AMERICAN INDIAN, ESKIMO OR ALEUT"
	4           "ASIAN OR PACIFIC ISLANDER"     
;
label values ms       ms;     
label define ms      
	0           "NOT A SAMPLE PERSON IN THIS"   
	1           "MARRIED, SPOUSE PRESENT"       
	2           "MARRIED, SPOUSE ABSENT"        
	3           "WIDOWED"                       
	4           "DIVORCED"                      
	5           "SEPARATED"                     
	6           "NEVER MARRIED"                 
;
label values pnsp     pnsp;   
label define pnsp    
	0           "NOT A SAMPLE PERSON IN THIS"   
	999         "NOT APPLICABLE"                
;
label values pnpt     pnpt;   
label define pnpt    
	0           "NOT A SAMPLE PERSON IN THIS"   
	999         "NOT APPLICABLE"                
;
label values higrade  higrade;
label define higrade 
	0           "NOT APPLICABLE IF UNDER 15, DID"
;
label values grd_cmpl grd_cmpl;
label define grd_cmpl
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ethnicty ethnicty;
label define ethnicty
	1           "GERMAN"                        
	2           "ENGLISH"                       
	3           "IRISH"                         
	4           "FRENCH"                        
	5           "ITALIAN"                       
	6           "SCOTTISH"                      
	7           "POLISH"                        
	8           "DUTCH"                         
	9           "SWEDISH"                       
	10          "NORWEGIAN"                     
	11          "RUSSIAN"                       
	12          "UKRAINIAN"                     
	13          "WELSH"                         
	14          "MEXICAN-AMERICAN"              
	15          "CHICANO"                       
	16          "MEXICAN"                       
	17          "PUERTO RICAN"                  
	18          "CUBAN"                         
	19          "CENTRAL OR SOUTH AMERICAN"     
	20          "OTHER SPANISH"                 
	21          "AFRO-AMERICAN (BLACK OR NEGRO)"
	30          "ANOTHER GROUP NOT LISTED"      
	39          "DON'T KNOW"                    
;
label values tm9266   tm9266l;
label define tm9266l 
	0           "NOT IN UNIVERSE"               
	1           "ONE PERSON HH"                 
	2           "TWO PERSON HH CONSISTING OF"   
	3           "TWO PERSON HH CONSISTING OF"   
	4           "OTHER"                         
;
label values tm9300   tm9300l;
label define tm9300l 
	0           "NOT IN UNIVERSE"               
;
label values tm9330   tm9330l;
label define tm9330l 
	0           "NOT IN UNIVERSE"               
;
label values tm9332   tm9332l;
label define tm9332l 
	1           "HUSBAND"                       
	2           "WIFE"                          
	10          "NATURAL FATHER(BIOLOGICAL)"    
	11          "STEPFATHER"                    
	12          "ADOPTIVE FATHER"               
	13          "FOSTER FATHER"                 
	14          "NATURAL MOTHER"                
	15          "STEPMOTHER"                    
	16          "ADOPTIVE MOTHER"               
	17          "FOSTER MOTHER"                 
	18          "UNKNOWN PARENT TYPE"           
	20          "NATURAL SON"                   
	21          "STEPSON"                       
	22          "ADOPTED SON"                   
	23          "FOSTER SON"                    
	24          "NATURAL DAUGHTER"              
	25          "STEP DAUGHTER"                 
	26          "ADOPTED DAUGHTER"              
	27          "FOSTER DAUGHTER"               
	28          "UNKNOWN CHILD TYPE"            
	30          "FULL BROTHER"                  
	31          "HALF BROTHER"                  
	32          "STEPBROTHER"                   
	33          "ADOPTIVE BROTHER"              
	34          "FULL SISTER"                   
	35          "HALF SISTER"                   
	36          "STEPSISTER"                    
	37          "ADOPTIVE SISTER"               
	38          "UNKNOWN SIBLING TYPE"          
	40          "GRANDFATHER"                   
	41          "GRANDMOTHER"                   
	42          "GRANDSON"                      
	43          "GRANDDAUGHTER"                 
	44          "UNCLE"                         
	45          "AUNT"                          
	46          "NEPHEW"                        
	47          "NIECE"                         
	50          "FATHER-IN-LAW"                 
	51          "MOTHER-IN-LAW"                 
	52          "SON-IN-LAW"                    
	53          "DAUGHTER-IN-LAW"               
	54          "BROTHER-IN-LAW"                
	55          "SISTER-IN-LAW"                 
	60          "COUSIN,ETC."                   
	70          "NOT RELATED"                   
	88          "MEMBER OF COLUMN WITH NO"      
	98          "NOT FOUND"                     
	99          "NO RESPONSE"                   
;
label values tm9360   tm9360l;
label define tm9360l 
	0           "NOT IN UNIVERSE"               
;
label values tm9364   tm9364l;
label define tm9364l 
	0           "NOT IN UNIVERSE"               
;
label values tm9390   tm9390l;
label define tm9390l 
	0           "NOT IN UNIVERSE"               
;
label values tm9420   tm9420l;
label define tm9420l 
	0           "NOT IN UNIVERSE"               
;
label values tm9450   tm9450l;
label define tm9450l 
	0           "NOT IN UNIVERSE"               
;
label values tm9480   tm9480l;
label define tm9480l 
	0           "NOT IN UNIVERSE"               
;
label values tm9510   tm9510l;
label define tm9510l 
	0           "NOT IN UNIVERSE"               
;
label values tm9540   tm9540l;
label define tm9540l 
	0           "NOT IN UNIVERSE"               
;
label values tm9570   tm9570l;
label define tm9570l 
	0           "NOT IN UNIVERSE"               
;
label values tm9600   tm9600l;
label define tm9600l 
	0           "NOT IN UNIVERSE"               
;
label values tm9630   tm9630l;
label define tm9630l 
	0           "NOT IN UNIVERSE"               
;
label values tm9660   tm9660l;
label define tm9660l 
	0           "NOT IN UNIVERSE"               
;
label values tm9690   tm9690l;
label define tm9690l 
	0           "NOT IN UNIVERSE"               
;
label values u_tm9266 u_tm926y;
label define u_tm926y
	0           "NOT IN UNIVERSE"               
	1           "ONE PERSON HH"                 
	2           "TWO PERSON HH CONSISTING OF"   
	3           "TWO PERSON HH CONSISTING OF"   
	4           "OTHER"                         
;
label values u_tm9300 u_tm930y;
label define u_tm930y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9330 u_tm933y;
label define u_tm933y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9332 u_tm933k;
label define u_tm933k
	1           "HUSBAND"                       
	2           "WIFE"                          
	10          "NATURAL FATHER(BIOLOGICAL)"    
	11          "STEPFATHER"                    
	12          "ADOPTIVE FATHER"               
	13          "FOSTER FATHER"                 
	14          "NATURAL MOTHER"                
	15          "STEPMOTHER"                    
	16          "ADOPTIVE MOTHER"               
	17          "FOSTER MOTHER"                 
	18          "UNKNOWN PARENT TYPE"           
	20          "NATURAL SON"                   
	21          "STEPSON"                       
	22          "ADOPTED SON"                   
	23          "FOSTER SON"                    
	24          "NATURAL DAUGHTER"              
	25          "STEP DAUGHTER"                 
	26          "ADOPTED DAUGHTER"              
	27          "FOSTER DAUGHTER"               
	28          "UNKNOWN CHILD TYPE"            
	30          "FULL BROTHER"                  
	31          "HALF BROTHER"                  
	32          "STEPBROTHER"                   
	33          "ADOPTIVE BROTHER"              
	34          "FULL SISTER"                   
	35          "HALF SISTER"                   
	36          "STEPSISTER"                    
	37          "ADOPTIVE SISTER"               
	38          "UNKNOWN SIBLING TYPE"          
	40          "GRANDFATHER"                   
	41          "GRANDMOTHER"                   
	42          "GRANDSON"                      
	43          "GRANDDAUGHTER"                 
	44          "UNCLE"                         
	45          "AUNT"                          
	46          "NEPHEW"                        
	47          "NIECE"                         
	50          "FATHER-IN-LAW"                 
	51          "MOTHER-IN-LAW"                 
	52          "SON-IN-LAW"                    
	53          "DAUGHTER-IN-LAW"               
	54          "BROTHER-IN-LAW"                
	55          "SISTER-IN-LAW"                 
	60          "COUSIN,ETC."                   
	70          "NOT RELATED"                   
	88          "MEMBER OF COLUMN WITH NO"      
	98          "NOT FOUND"                     
	99          "NO RESPONSE"                   
;
label values u_tm9360 u_tm936y;
label define u_tm936y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9390 u_tm939y;
label define u_tm939y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9420 u_tm942y;
label define u_tm942y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9450 u_tm945y;
label define u_tm945y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9480 u_tm948y;
label define u_tm948y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9510 u_tm951y;
label define u_tm951y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9540 u_tm954y;
label define u_tm954y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9570 u_tm957y;
label define u_tm957y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9600 u_tm960y;
label define u_tm960y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9630 u_tm963y;
label define u_tm963y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9660 u_tm966y;
label define u_tm966y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9690 u_tm969y;
label define u_tm969y
	0           "NOT IN UNIVERSE"               
;
label values u_tm9716 u_tm971y;
label define u_tm971y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values tm8300   tm8300l;
label define tm8300l 
	0           "NOT APPLICABLE"                
	1           "15 YEARS - SKIP TO TM8400"     
	2           "16 TO 67 YEARS"                
	3           "68 YEARS OR OVER - SKIP TO TM840"
;
label values tm8302   tm8302l;
label define tm8302l 
	0           "NOT APPLICABLE"                
	1           "YES - SKIP TO TM8306"          
	2           "NO"                            
;
label values tm8304   tm8304l;
label define tm8304l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO .SKIP TO TM8308"            
;
label values tm8306   tm8306l;
label define tm8306l 
	0           "NOT APPLICABLE"                
	1           "YES - SKIP TO TM8310"          
	2           "NO - SKIP TO TM8400"           
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "NOT APPLICABLE"                
	1           "YES - MARK '171' ON ISS"       
	2           "NO - SKIP TO TM8400"           
;
label values tm8310   tm8310l;
label define tm8310l 
	0           "NOT APPLICABLE"                
;
label values tm8312   tm8312l;
label define tm8312l 
	0           "NOT APPLICABLE"                
;
label values tm8314   tm8314l;
label define tm8314l 
	-5          "PERSON BECAME LIMITED AFTER"   
	-3          "PERSON WAS LIMITED BEFORE"     
	0           "NOT APPLICABLE"                
;
label values tm8316   tm8316l;
label define tm8316l 
	0           "NOT APPLICABLE"                
	1           "YES - SKIP TO TM8324"          
	2           "NO"                            
;
label values tm8318   tm8318l;
label define tm8318l 
	0           "NOT APPLICABLE"                
;
label values tm8320   tm8320l;
label define tm8320l 
	0           "NOT APPLICABLE"                
;
label values tm8322   tm8322l;
label define tm8322l 
	-3          "HAD NEVER BEEN EMPLOYED"       
	0           "NOT APPLICABLE"                
;
label values tm8324   tm8324l;
label define tm8324l 
	0           "NOT APPLICABLE"                
	1           "ALCOHOL OR DRUG PROBLEM OR"    
	2           "AIDS OR AIDS RELATED"          
	3           "ARTHRITIS OR RHEUMATISM"       
	4           "BACK OR SPINE PROBLEMS"        
	5           "BLINDNESS OR VISION PROBLEMS"  
	6           "BROKEN BONE/FRACTURE"          
	7           "CANCER"                        
	8           "CEREBRAL PALSY"                
	9           "DEAFNESS OR SERIOUS TROUBLE"   
	10          "DIABETES"                      
	11          "EPILEPSY"                      
	12          "HEAD OR SPINAL CORD INJURY"    
	13          "HEART TROUBLE (INCLUDING"      
	14          "HERNIA OR RUPTURE"             
	15          "HIGH BLOOD PRESSURE"           
	16          "KIDNEY STONES OR CHRONIC"      
	17          "LEARNING DISABILITY"           
	18          "LUNG OR RESPIRATORY TROUBLE"   
	19          "MENTAL ILLNESS"                
	20          "MENTAL RETARDATION"            
	21          "MISSING LEGS, FEET, ARMS,"     
	22          "PARALYSIS OF ANY KIND"         
	23          "SENILITY/DEMENTIA/ALZHEIMER'S" 
	24          "SPEECH DISORDER"               
	25          "STIFFNESS OR DEFORMITY OF"     
	26          "STOMACH TROUBLE (INCLUDING"    
	27          "STROKE"                        
	28          "THYROID TROUBLE OR GOITER"     
	29          "TUMOR, CYST OR GROWTH"         
	30          "OTHER"                         
;
label values tm8326   tm8326l;
label define tm8326l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO - SKIP TO TM8330"           
;
label values tm8328   tm8328l;
label define tm8328l 
	0           "NOT APPLICABLE"                
	1           "ON THE JOB?"                   
	2           "DURING SERVICE IN THE ARMED"   
	3           "IN THE HOME?"                  
	4           "SOMEWHERE ELSE?"               
;
label values tm8330   tm8330l;
label define tm8330l 
	0           "NOT APPLICABLE"                
	1           "YES - SKIP TO TM8340"          
	2           "NO"                            
;
label values tm8332   tm8332l;
label define tm8332l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO - SKIP TO TM8342"           
;
label values tm8334   tm8334l;
label define tm8334l 
	0           "NOT APPLICABLE"                
;
label values tm8336   tm8336l;
label define tm8336l 
	0           "NOT APPLICABLE"                
;
label values tm8338   tm8338l;
label define tm8338l 
	-3          "HAS NEVER BEEN ABLE TO WORK AT A"
	0           "NOT APPLICABLE JOB - SKIP"     
;
label values tm8340   tm8340l;
label define tm8340l 
	0           "NOT APPLICABLE"                
	1           "YES -SKIP TO TM8344"           
	2           "NO"                            
;
label values tm8342   tm8342l;
label define tm8342l 
	0           "NOT APPLICABLE"                
	1           "FULL-TIME"                     
	2           "PART-TIME"                     
	3           "NOT ABLE TO WORK - SKIP TO STMT"
;
label values tm8344   tm8344l;
label define tm8344l 
	0           "NOT APPLICABLE"                
	1           "REGULARLY"                     
	2           "ONLY OCCASIONALLY OR IRREGULARLY"
	3           "NOT ABLE TO WORK - SKIP TO STMT"
;
label values tm8346   tm8346l;
label define tm8346l 
	0           "NOT APPLICABLE"                
	1           "YES, ABLE TO DO SAME KIND OF"  
	2           "NO, NOT ABLE TO DO SAME KIND"  
	3           "DID NOT WORK BEFORE LIMITATION"
;
label values imp8306  imp8306l;
label define imp8306l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8308  imp8308l;
label define imp8308l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8310  imp8310l;
label define imp8310l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp12_14 imp12_1y;
label define imp12_1y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8316  imp8316l;
label define imp8316l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8318  imp8318l;
label define imp8318l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp20_22 imp20_2y;
label define imp20_2y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8324  imp8324l;
label define imp8324l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8326  imp8326l;
label define imp8326l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8328  imp8328l;
label define imp8328l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8332  imp8332l;
label define imp8332l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8334  imp8334l;
label define imp8334l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp34_38 imp34_3y;
label define imp34_3y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8342  imp8342l;
label define imp8342l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8344  imp8344l;
label define imp8344l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8346  imp8346l;
label define imp8346l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO - SKIP TO TM8410"           
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "NOT APPLICABLE"                
;
label values tm8406   tm8406l;
label define tm8406l 
	0           "NOT APPLICABLE"                
	1           "CURRENTLY ATTENDING - SKIP"    
	2           "NEVER ATTENDED"                
;
label values tm8408   tm8408l;
label define tm8408l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO - SKIP TO TM8444"           
;
label values tm8410   tm8410l;
label define tm8410l 
	0           "NOT APPLICABLE"                
;
label values tm8412   tm8412l;
label define tm8412l 
	0           "NOT APPLICABLE"                
;
label values tm8414   tm8414l;
label define tm8414l 
	0           "NOT APPLICABLE"                
	1           "PUBLIC"                        
	2           "PRIVATE, CHURCH-RELATED"       
	3           "PRIVATE, NOT CHURCH-RELATED"   
	4           "DID NOT ATTEND HIGH SCHOOL"    
;
label values tm8416   tm8416l;
label define tm8416l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO - SKIP TO TM8444"           
;
label values tm8418   tm8418l;
label define tm8418l 
	0           "NOT APPLICABLE"                
;
label values tm8420   tm8420l;
label define tm8420l 
	0           "NOT APPLICABLE"                
;
label values tm8422   tm8422l;
label define tm8422l 
	0           "NOT APPLICABLE"                
	1           "PHD OR EQUIVALENT"             
	2           "PROFESSIONAL DEGREE SUCH AS"   
	3           "MASTER'S DEGREE"               
	4           "BACHELOR'S DEGREE"             
	5           "ASSOCIATE DEGREE"              
	6           "VOCATIONAL CERTIFICATE OR"     
	7           "HAS NOT EARNED A DEGREE - SKIP"
;
label values tm8424   tm8424l;
label define tm8424l 
	0           "NOT APPLICABLE"                
;
label values tm8426   tm8426l;
label define tm8426l 
	0           "NOT APPLICABLE"                
;
label values tm8428   tm8428l;
label define tm8428l 
	0           "NOT APPLICABLE"                
	1           "AGRICULTURE OR FORESTRY"       
	2           "BIOLOGY"                       
	3           "BUSINESS OR MANAGEMENT"        
	4           "ECONOMICS"                     
	5           "EDUCATION"                     
	6           "ENGINEERING (INCLUDING"        
	7           "ENGLISH OR JOURNALISM"         
	8           "HOME ECONOMICS"                
	9           "LAW"                           
	10          "LIBERAL ARTRS OR HUMANITIES"   
	11          "MATHEMATICS OR STATISTICS"     
	12          "MEDICINE OR DENTISTRY"         
	13          "NURSING, PHARMACY, OR HEALTH"  
	14          "PHYSICAL OR EARTH SCIENCES"    
	15          "POLICE SCIENCE OR LAW"         
	16          "PSYCHOLOGY"                    
	17          "RELIGION OR THEOLOGY"          
	18          "SOCIAL SCIENCES (HISTORY,"     
	19          "VOCATIONAL OR TECHNICAL STUDIES"
	20          "OTHER"                         
;
label values tm8430   tm8430l;
label define tm8430l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO - SKIP TO TM8444"           
;
label values tm8432   tm8432l;
label define tm8432l 
	0           "NOT APPLICABLE"                
;
label values tm8434   tm8434l;
label define tm8434l 
	0           "NOT APPLICABLE"                
;
label values tm8436   tm8436l;
label define tm8436l 
	-1          "DON'T KNOW"                    
	0           "NOT APPLICABLE"                
;
label values tm8438   tm8438l;
label define tm8438l 
	0           "NOT APPLICABLE"                
;
label values tm8440   tm8440l;
label define tm8440l 
	0           "NOT APPLICABLE"                
;
label values tm8442   tm8442l;
label define tm8442l 
	0           "NOT APPLICABLE"                
	1           "IS STILL A STUDENT"            
;
label values tm8444   tm8444l;
label define tm8444l 
	0           "NOT APPLICABLE"                
	1           "YES - SKIP TO TM8600"          
	2           "NO"                            
;
label values tm8446   tm8446l;
label define tm8446l 
	-1          "DON'T KNOW - SKIP TO TM8550"   
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO - SKIP TO TM8550"           
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "NOT APPLICABLE"                
	1           "JOB TRAINING PARTNERSHIP"      
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "NOT APPLICABLE"                
	1           "JOB OPPORTUNITIES AND BASIC"   
;
label values tm8452   tm8452l;
label define tm8452l 
	0           "NOT APPLICABLE"                
	1           "FOOD STAMPS WORK PROGRAM"      
;
label values tm8454   tm8454l;
label define tm8454l 
	0           "NOT APPLICABLE"                
	1           "OTHER PROGRAM SPONSORED BY"    
;
label values tm8456   tm8456l;
label define tm8456l 
	0           "NOT APPLICABLE"                
	1           "VETERANS' TRAINING PROGRAMS"   
;
label values tm8458   tm8458l;
label define tm8458l 
	0           "NOT APPLICABLE"                
	1           "NO"                            
;
label values tm8460   tm8460l;
label define tm8460l 
	0           "NOT APPLICABLE"                
	1           "CLASSROOM TRAINING - JOB SKILLS"
;
label values tm8462   tm8462l;
label define tm8462l 
	0           "NOT APPLICABLE"                
	1           "CLASSROOM TRAINING - BASIC"    
;
label values tm8464   tm8464l;
label define tm8464l 
	0           "NOT APPLICABLE"                
	1           "ON-THE-JOB TRAINING"           
;
label values tm8466   tm8466l;
label define tm8466l 
	0           "NOT APPLICABLE"                
	1           "JOB SEARCH ASSISTANCE"         
;
label values tm8468   tm8468l;
label define tm8468l 
	0           "NOT APPLICABLE"                
	1           "WORK EXPERIENCE"               
;
label values tm8470   tm8470l;
label define tm8470l 
	0           "NOT APPLICABLE"                
	1           "OTHER"                         
;
label values tm8472   tm8472l;
label define tm8472l 
	0           "NOT APPLICABLE"                
	1           "APPRENTICESHIP PROGRAM"        
;
label values tm8474   tm8474l;
label define tm8474l 
	0           "NOT APPLICABLE"                
	1           "BUSINESS, COMMERCIAL, OR"      
;
label values tm8476   tm8476l;
label define tm8476l 
	0           "NOT APPLICABLE"                
	1           "JUNIOR OR COMMUNITY COLLEGE"   
;
label values tm8478   tm8478l;
label define tm8478l 
	0           "NOT APPLICABLE"                
	1           "PROGRAM COMPLETED AT A 4 YEAR" 
;
label values tm8480   tm8480l;
label define tm8480l 
	0           "NOT APPLICABLE"                
	1           "HIGH SCHOOL VOCATION PROGRAM"  
;
label values tm8482   tm8482l;
label define tm8482l 
	0           "NOT APPLICABLE"                
	1           "TRAINING PROGRAM AT WORK"      
;
label values tm8484   tm8484l;
label define tm8484l 
	0           "NOT APPLICABLE"                
	1           "MILITARY (EXCLUDE BASIC TRAINING"
;
label values tm8486   tm8486l;
label define tm8486l 
	0           "NOT APPLICABLE"                
	1           "CORRESPONDENCE COURSE"         
;
label values tm8488   tm8488l;
label define tm8488l 
	0           "NOT APPLICABLE"                
	1           "TRAINING OR EXPERIENCE"        
;
label values tm8490   tm8490l;
label define tm8490l 
	0           "NOT APPLICABLE"                
	1           "SHELTERED WORKSHOP"            
;
label values tm8492   tm8492l;
label define tm8492l 
	0           "NOT APPLICABLE"                
	1           "VOCATIONAL REEHABILITATION"    
;
label values tm8494   tm8494l;
label define tm8494l 
	0           "NOT APPLICABLE"                
	1           "OTHER"                         
;
label values tm8496   tm8496l;
label define tm8496l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values tm8498   tm8498l;
label define tm8498l 
	0           "NOT APPLICABLE"                
;
label values tm8500   tm8500l;
label define tm8500l 
	0           "NOT APPLICABLE"                
;
label values tm8502   tm8502l;
label define tm8502l 
	0           "NOT APPLICABLE"                
;
label values tm8504   tm8504l;
label define tm8504l 
	-4          "LESS THAN 1 WEEK"              
	-3          "CURRENTLY ATTENDING"           
	-1          "DON'T KNOW"                    
	0           "NOT APPLICABLE"                
;
label values tm8506   tm8506l;
label define tm8506l 
	0           "NOT APPLICABLE"                
	1           "SELF OR FAMILY"                
;
label values tm8508   tm8508l;
label define tm8508l 
	0           "NOT APPLICABLE"                
	1           "EMPLOYER"                      
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "NOT APPLICABLE"                
	1           "FEDERAL, STATE, OR LOCAL"      
;
label values tm8512   tm8512l;
label define tm8512l 
	0           "NOT APPLICABLE"                
	1           "SOMEONE ELSE"                  
;
label values imp8414  imp8414l;
label define imp8414l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8420  imp8420l;
label define imp8420l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8422  imp8422l;
label define imp8422l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8426  imp8426l;
label define imp8426l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8428  imp8428l;
label define imp8428l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8434  imp8434l;
label define imp8434l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8436  imp8436l;
label define imp8436l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8440  imp8440l;
label define imp8440l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8446  imp8446l;
label define imp8446l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp48_58 imp48_5y;
label define imp48_5y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp60_70 imp60_7y;
label define imp60_7y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp72_94 imp72_9y;
label define imp72_9y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8496  imp8496l;
label define imp8496l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8500  imp8500l;
label define imp8500l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8502  imp8502l;
label define imp8502l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp06_12 imp06_1y;
label define imp06_1y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values tm8600   tm8600l;
label define tm8600l 
	0           "NOT IN UNIVERSE"               
	1           "MARRIED, SPOUSE PRESENT"       
	2           "MARRIED, SPOUSE ABSENT"        
	3           "WIDOWED"                       
	4           "DIVORCED"                      
	5           "SEPARATED"                     
	6           "NEVER MARRIED - SKIP TO TM8700"
;
label values tm8602   tm8602l;
label define tm8602l 
	0           "NOT IN UNIVERSE"               
	1           "1 - SKIP TO TM8638"            
	2           "2"                             
	3           "3"                             
	4           "4+"                            
;
label values tm8604   tm8604l;
label define tm8604l 
	0           "NOT IN UNIVERSE"               
;
label values tm8606   tm8606l;
label define tm8606l 
	0           "NOT IN UNIVERSE"               
;
label values tm8608   tm8608l;
label define tm8608l 
	0           "NOT IN UNIVERSE"               
	1           "WIDOWHOOD"                     
	2           "DIVORCE"                       
;
label values tm8610   tm8610l;
label define tm8610l 
	0           "NOT IN UNIVERSE"               
;
label values tm8612   tm8612l;
label define tm8612l 
	0           "NOT IN UNIVERSE"               
;
label values tm8614   tm8614l;
label define tm8614l 
	0           "NOT IN UNIVERSE"               
	1           "YES - SKIP TO TM8620"          
	2           "NO"                            
;
label values tm8616   tm8616l;
label define tm8616l 
	0           "NOT IN UNIVERSE"               
;
label values tm8618   tm8618l;
label define tm8618l 
	0           "NOT IN UNIVERSE"               
;
label values tm8620   tm8620l;
label define tm8620l 
	0           "NOT IN UNIVERSE"               
	1           "2 - SKIP TO TM8638"            
	2           "3 +"                           
;
label values tm8622   tm8622l;
label define tm8622l 
	0           "NOT IN UNIVERSE"               
;
label values tm8624   tm8624l;
label define tm8624l 
	0           "NOT IN UNIVERSE"               
;
label values tm8626   tm8626l;
label define tm8626l 
	0           "NOT IN UNIVERSE"               
	1           "WIDOWHOOD"                     
	2           "DIVORCE"                       
;
label values tm8628   tm8628l;
label define tm8628l 
	0           "NOT IN UNIVERSE"               
;
label values tm8630   tm8630l;
label define tm8630l 
	0           "NOT IN UNIVERSE"               
;
label values tm8632   tm8632l;
label define tm8632l 
	0           "NOT IN UNIVERSE"               
	1           "YES - SKIP TO TM8638"          
	2           "NO"                            
;
label values tm8634   tm8634l;
label define tm8634l 
	0           "NOT IN UNIVERSE"               
;
label values tm8636   tm8636l;
label define tm8636l 
	0           "NOT IN UNIVERSE"               
;
label values tm8638   tm8638l;
label define tm8638l 
	0           "NOT IN UNIVERSE"               
	1           "YES - SKIP TO TM8700"          
	2           "NO"                            
	3           "NO, NO SPOUSE IN HOUSEHOLD"    
;
label values tm8640   tm8640l;
label define tm8640l 
	0           "NOT IN UNIVERSE"               
;
label values tm8642   tm8642l;
label define tm8642l 
	0           "NOT IN UNIVERSE"               
;
label values tm8644   tm8644l;
label define tm8644l 
	0           "NOT IN UNIVERSE"               
	1           "MARRIED, SPOUSE PRESENT -"     
	2           "MARRIED, SPOUSE ABSENT -"      
	3           "WIDOWED"                       
	4           "DIVORCED"                      
	5           "SEPARATED - SKIP TO TM8652"    
;
label values tm8646   tm8646l;
label define tm8646l 
	0           "NOT IN UNIVERSE"               
;
label values tm8648   tm8648l;
label define tm8648l 
	0           "NOT IN UNIVERSE"               
;
label values tm8650   tm8650l;
label define tm8650l 
	0           "NOT IN UNIVERSE"               
	1           "YES - SKIP TO TM8700"          
	2           "NO"                            
;
label values tm8652   tm8652l;
label define tm8652l 
	0           "NOT IN UNIVERSE"               
;
label values tm8654   tm8654l;
label define tm8654l 
	0           "NOT IN UNIVERSE"               
;
label values imp8602  imp8602l;
label define imp8602l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8608  imp8608l;
label define imp8608l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8626  imp8626l;
label define imp8626l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values tm8700   tm8700l;
label define tm8700l 
	0           "NOT IN UNIVERSE"               
;
label values tm8702   tm8702l;
label define tm8702l 
	-4          "ALWAYS LIVED HERE / BORN"      
	0           "NOT IN UNIVERSE"               
;
label values tm8704   tm8704l;
label define tm8704l 
	0           "NOT IN UNIVERSE"               
	1           "SAME STATE,SAME COUNTY"        
	2           "SAME STATE,DIFFERENT COUNTY"   
;
label values tm8706   tm8706l;
label define tm8706l 
	0           "NOT IN UNIVERSE"               
	1           "ALABAMA"                       
	2           "ALASKA"                        
	3           "ARIZONA"                       
	4           "ARKANSAS"                      
	5           "CALIFORNIA"                    
	6           "COLORADO"                      
	7           "CONNECTICUT"                   
	8           "DELAWARE"                      
	9           "DISTRICT OF COLUMBIA"          
	10          "FLORIDA"                       
	11          "GEORGIA"                       
	12          "HAWAII"                        
	13          "IDAHO"                         
	14          "ILLINOIS"                      
	15          "INDIANA"                       
	16          "IOWA"                          
	17          "KANSAS"                        
	18          "KENTUCKY"                      
	19          "LOUISIANA"                     
	20          "MAINE"                         
	21          "MARYLAND"                      
	22          "MASSACHUSETTS"                 
	23          "MICHIGAN"                      
	24          "MINNESOTA"                     
	25          "MISSISSIPPI"                   
	26          "MISSOURI"                      
	27          "MONTANA"                       
	28          "NEBRASKA"                      
	29          "NEVADA"                        
	30          "NEW HAMPSHIRE"                 
	31          "NEW JERSEY"                    
	32          "NEW MEXICO"                    
	33          "NEW YORK"                      
	34          "NORTH CAROLINA"                
	35          "NORTH DAKOTA"                  
	36          "OHIO"                          
	37          "OKLAHOMA"                      
	38          "OREGON"                        
	39          "PENNSYLVANIA"                  
	40          "RHODE ISLAND"                  
	41          "SOUTH CAROLINA"                
	42          "SOUTH DAKOTA"                  
	43          "TENNESSEE"                     
	44          "TEXAS"                         
	45          "UTAH"                          
	46          "VERMONT"                       
	47          "VIRGINIA"                      
	48          "WASHINGTON"                    
	49          "WEST VIRGINIA"                 
	50          "WISCONSIN"                     
	51          "WYOMING"                       
	52          "UNITED STATES (STATE UNKNOWN)" 
	60          "PUERTO RICO"                   
	61          "OUTLYING AREA OF U. S."        
	62          "AUSTRIA"                       
	63          "CANADA"                        
	64          "CHINA"                         
	65          "CUBA"                          
	66          "CZECHOSLOVAKIA"                
	67          "DOMINICAN REPUBLIC"            
	68          "GERMANY"                       
	69          "GREECE"                        
	70          "HUNGARY"                       
	71          "INDIA"                         
	72          "IRELAND"                       
	73          "ITALY"                         
	74          "JAMAICA"                       
	75          "JAPAN"                         
	76          "KOREA"                         
	77          "MEXICO"                        
	78          "NORWAY"                        
	79          "PHILIPPINES"                   
	80          "POLAND"                        
	81          "PORTUGAL"                      
	82          "SWEDEN"                        
	83          "UNITED KINGDOM"                
	84          "U. S. S. R."                   
	85          "VIETNAM"                       
	86          "OTHER EUROPE"                  
	87          "OTHER ASIA"                    
	88          "CENTRAL AMERICA"               
	89          "SOUTH AMERICA"                 
	90          "MIDDLE EAST"                   
	91          "AFRICA"                        
	92          "OVERSEAS (COUNTRY UNKNOWN)"    
	99          "OTHER(SPECIFY)"                
;
label values tm8708   tm8708l;
label define tm8708l 
	0           "NOT IN UNIVERSE"               
	1           "ALABAMA"                       
	2           "ALASKA"                        
	3           "ARIZONA"                       
	4           "ARKANSAS"                      
	5           "CALIFORNIA"                    
	6           "COLORADO"                      
	7           "CONNECTICUT"                   
	8           "DELAWARE"                      
	9           "DISTRICT OF COLUMBIA"          
	10          "FLORIDA"                       
	11          "GEORGIA"                       
	12          "HAWAII"                        
	13          "IDAHO"                         
	14          "ILLINOIS"                      
	15          "INDIANA"                       
	16          "IOWA"                          
	17          "KANSAS"                        
	18          "KENTUCKY"                      
	19          "LOUISIANA"                     
	20          "MAINE"                         
	21          "MARYLAND"                      
	22          "MASSACHUSETTS"                 
	23          "MICHIGAN"                      
	24          "MINNESOTA"                     
	25          "MISSISSIPPI"                   
	26          "MISSOURI"                      
	27          "MONTANA"                       
	28          "NEBRASKA"                      
	29          "NEVADA"                        
	30          "NEW HAMPSHIRE"                 
	31          "NEW JERSEY"                    
	32          "NEW MEXICO"                    
	33          "NEW YORK"                      
	34          "NORTH CAROLINA"                
	35          "NORTH DAKOTA"                  
	36          "OHIO"                          
	37          "OKLAHOMA"                      
	38          "OREGON"                        
	39          "PENNSYLVANIA"                  
	40          "RHODE ISLAND"                  
	41          "SOUTH CAROLINA"                
	42          "SOUTH DAKOTA"                  
	43          "TENNESSEE"                     
	44          "TEXAS"                         
	45          "UTAH"                          
	46          "VERMONT"                       
	47          "VIRGINIA"                      
	48          "WASHINGTON"                    
	49          "WEST VIRGINIA"                 
	50          "WISCONSIN"                     
	51          "WYOMING"                       
	60          "PUERTO RICO"                   
	61          "OUTLYING AREA OF U. S."        
	62          "AUSTRIA"                       
	63          "CANADA"                        
	64          "CHINA"                         
	65          "CUBA"                          
	66          "CZECHOSLOVAKIA"                
	67          "DOMINICAN REPUBLIC"            
	68          "GERMANY"                       
	69          "GREECE"                        
	70          "HUNGARY"                       
	71          "INDIA"                         
	72          "IRELAND"                       
	73          "ITALY"                         
	74          "JAMAICA"                       
	75          "JAPAN"                         
	76          "KOREA"                         
	77          "MEXICO"                        
	78          "NORWAY"                        
	79          "PHILIPPINES"                   
	80          "POLAND"                        
	81          "PORTUGAL"                      
	82          "SWEDEN"                        
	83          "UNITED KINGDOM"                
	84          "U. S. S. R."                   
	85          "VIETNAM"                       
	86          "OTHER EUROPE"                  
	87          "OTHER ASIA"                    
	88          "CENTRAL AMERICA"               
	89          "SOUTH AMERICA"                 
	90          "MIDDLE EAST"                   
	91          "AFRICA"                        
	99          "OTHER(SPECIFY)"                
;
label values tm8709   tm8709l;
label define tm8709l 
	-4          "LIVED THERE SINCE BIRTH -"     
	0           "NOT IN UNIVERSE"               
;
label values tm8710   tm8710l;
label define tm8710l 
	0           "NOT IN UNIVERSE"               
;
label values tm8712   tm8712l;
label define tm8712l 
	0           "NOT IN UNIVERSE"               
;
label values tm8714   tm8714l;
label define tm8714l 
	0           "NOT IN UNIVERSE"               
;
label values tm8716   tm8716l;
label define tm8716l 
	0           "NOT IN UNIVERSE"               
;
label values tm8718   tm8718l;
label define tm8718l 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO - SKIP TO TM8730"           
;
label values tm8720   tm8720l;
label define tm8720l 
	0           "NOT IN UNIVERSE"               
	1           "ALABAMA"                       
	2           "ALASKA"                        
	3           "ARIZONA"                       
	4           "ARKANSAS"                      
	5           "CALIFORNIA"                    
	6           "COLORADO"                      
	7           "CONNECTICUT"                   
	8           "DELAWARE"                      
	9           "DISTRICT OF COLUMBIA"          
	10          "FLORIDA"                       
	11          "GEORGIA"                       
	12          "HAWAII"                        
	13          "IDAHO"                         
	14          "ILLINOIS"                      
	15          "INDIANA"                       
	16          "IOWA"                          
	17          "KANSAS"                        
	18          "KENTUCKY"                      
	19          "LOUISIANA"                     
	20          "MAINE"                         
	21          "MARYLAND"                      
	22          "MASSACHUSETTS"                 
	23          "MICHIGAN"                      
	24          "MINNESOTA"                     
	25          "MISSISSIPPI"                   
	26          "MISSOURI"                      
	27          "MONTANA"                       
	28          "NEBRASKA"                      
	29          "NEVADA"                        
	30          "NEW HAMPSHIRE"                 
	31          "NEW JERSEY"                    
	32          "NEW MEXICO"                    
	33          "NEW YORK"                      
	34          "NORTH CAROLINA"                
	35          "NORTH DAKOTA"                  
	36          "OHIO"                          
	37          "OKLAHOMA"                      
	38          "OREGON"                        
	39          "PENNSYLVANIA"                  
	40          "RHODE ISLAND"                  
	41          "SOUTH CAROLINA"                
	42          "SOUTH DAKOTA"                  
	43          "TENNESSEE"                     
	44          "TEXAS"                         
	45          "UTAH"                          
	46          "VERMONT"                       
	47          "VIRGINIA"                      
	48          "WASHINGTON"                    
	49          "WEST VIRGINIA"                 
	50          "WISCONSIN"                     
	51          "WYOMING"                       
	52          "UNITED STATES (STATE UNKNOWN)" 
	60          "PUERTO RICO"                   
	61          "OUTLYING AREA OF U. S."        
	62          "AUSTRIA"                       
	63          "CANADA"                        
	64          "CHINA"                         
	65          "CUBA"                          
	66          "CZECHOSLOVAKIA"                
	67          "DOMINICAN REPUBLIC"            
	68          "GERMANY"                       
	69          "GREECE"                        
	70          "HUNGARY"                       
	71          "INDIA"                         
	72          "IRELAND"                       
	73          "ITALY"                         
	74          "JAMAICA"                       
	75          "JAPAN"                         
	76          "KOREA"                         
	77          "MEXICO"                        
	78          "NORWAY"                        
	79          "PHILIPPINES"                   
	80          "POLAND"                        
	81          "PORTUGAL"                      
	82          "SWEDEN"                        
	83          "UNITED KINGDOM"                
	84          "U. S. S. R."                   
	85          "VIETNAM"                       
	86          "OTHER EUROPE"                  
	87          "OTHER ASIA"                    
	88          "CENTRAL AMERICA"               
	89          "SOUTH AMERICA"                 
	90          "MIDDLE EAST"                   
	91          "AFRICA"                        
	92          "OVERSEAS (COUNTRY UNKNOWN)"    
	99          "OTHER(SPECIFY)"                
;
label values tm8722   tm8722l;
label define tm8722l 
	0           "NOT IN UNIVERSE"               
;
label values tm8724   tm8724l;
label define tm8724l 
	0           "NOT IN UNIVERSE"               
;
label values tm8726   tm8726l;
label define tm8726l 
	0           "NOT IN UNIVERSE"               
;
label values tm8728   tm8728l;
label define tm8728l 
	0           "NOT IN UNIVERSE"               
;
label values tm8730   tm8730l;
label define tm8730l 
	0           "NOT IN UNIVERSE"               
	1           "ALABAMA"                       
	2           "ALASKA"                        
	3           "ARIZONA"                       
	4           "ARKANSAS"                      
	5           "CALIFORNIA"                    
	6           "COLORADO"                      
	7           "CONNECTICUT"                   
	8           "DELAWARE"                      
	9           "DISTRICT OF COLUMBIA"          
	10          "FLORIDA"                       
	11          "GEORGIA"                       
	12          "HAWAII"                        
	13          "IDAHO"                         
	14          "ILLINOIS"                      
	15          "INDIANA"                       
	16          "IOWA"                          
	17          "KANSAS"                        
	18          "KENTUCKY"                      
	19          "LOUISIANA"                     
	20          "MAINE"                         
	21          "MARYLAND"                      
	22          "MASSACHUSETTS"                 
	23          "MICHIGAN"                      
	24          "MINNESOTA"                     
	25          "MISSISSIPPI"                   
	26          "MISSOURI"                      
	27          "MONTANA"                       
	28          "NEBRASKA"                      
	29          "NEVADA"                        
	30          "NEW HAMPSHIRE"                 
	31          "NEW JERSEY"                    
	32          "NEW MEXICO"                    
	33          "NEW YORK"                      
	34          "NORTH CAROLINA"                
	35          "NORTH DAKOTA"                  
	36          "OHIO"                          
	37          "OKLAHOMA"                      
	38          "OREGON"                        
	39          "PENNSYLVANIA"                  
	40          "RHODE ISLAND"                  
	41          "SOUTH CAROLINA"                
	42          "SOUTH DAKOTA"                  
	43          "TENNESSEE"                     
	44          "TEXAS"                         
	45          "UTAH"                          
	46          "VERMONT"                       
	47          "VIRGINIA"                      
	48          "WASHINGTON"                    
	49          "WEST VIRGINIA"                 
	50          "WISCONSIN"                     
	51          "WYOMING"                       
	52          "UNITED STATES (STATE UNKNOWN)" 
	60          "PUERTO RICO"                   
	61          "OUTLYING AREA OF U. S."        
	62          "AUSTRIA"                       
	63          "CANADA"                        
	64          "CHINA"                         
	65          "CUBA"                          
	66          "CZECHOSLOVAKIA"                
	67          "DOMINICAN REPUBLIC"            
	68          "GERMANY"                       
	69          "GREECE"                        
	70          "HUNGARY"                       
	71          "INDIA"                         
	72          "IRELAND"                       
	73          "ITALY"                         
	74          "JAMAICA"                       
	75          "JAPAN"                         
	76          "KOREA"                         
	77          "MEXICO"                        
	78          "NORWAY"                        
	79          "PHILIPPINES"                   
	80          "POLAND"                        
	81          "PORTUGAL"                      
	82          "SWEDEN"                        
	83          "UNITED KINGDOM"                
	84          "U. S. S. R."                   
	85          "VIETNAM"                       
	86          "OTHER EUROPE"                  
	87          "OTHER ASIA"                    
	88          "CENTRAL AMERICA"               
	89          "SOUTH AMERICA"                 
	90          "MIDDLE EAST"                   
	91          "AFRICA"                        
	92          "OVERSEAS (COUNTRY UNKNOWN)"    
	99          "OTHER(SPECIFY)"                
;
label values tm8732   tm8732l;
label define tm8732l 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO - SKIP TO TM8750"           
;
label values tm8734   tm8734l;
label define tm8734l 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO"                            
	3           "NO, BORN ABROAD OF AMERICAN"   
;
label values tm8736   tm8736l;
label define tm8736l 
	0           "NOT IN UNIVERSE"               
	1           "1911 - 1959"                   
	2           "1960 - 1964"                   
	3           "1965 - 1969"                   
	4           "1970 - 1974"                   
	5           "1975 - 1979"                   
	6           "1980 - 1981"                   
	7           "1982 - 1984"                   
	8           "1985 - 1993"                   
;
label values imp00_02 imp00_0y;
label define imp00_0y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
	2           "SET BY SAME STATE COUNTER"     
	3           "SET BY COUNTER"                
	4           "CALULATION"                    
	5           "SET BY ROTATION MONTH"         
;
label values imp8704  imp8704l;
label define imp8704l
	0           "NOT IMPUTED"                   
;
label values imp8706  imp8706l;
label define imp8706l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8708  imp8708l;
label define imp8708l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp10_12 imp10_1y;
label define imp10_1y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
	2           "SET BY COUNTER"                
	3           "GOOD YEAR  BAD MONTH (TM8714)" 
	4           "GOOD YEAR BAD MONTH (TM8712)"  
;
label values imp8720  imp8720l;
label define imp8720l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp22_24 imp22_2y;
label define imp22_2y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
	2           "GOOD YEAR BAD MONTH"           
;
label values imp26_28 imp26_2y;
label define imp26_2y
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
	2           "GOOD YEAR BAD MONTH (TM8716)"  
	3           "GOOD YEAR BAD MONTH"           
;
label values imp8730  imp8730l;
label define imp8730l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8732  imp8732l;
label define imp8732l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8734  imp8734l;
label define imp8734l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8736  imp8736l;
label define imp8736l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values tm8750   tm8750l;
label define tm8750l 
	0           "NOT IN UNIVERSE"               
	1           "FEMALE- SKIP TO TM8754"        
	2           "MALE, 18 + YEARS OLD"          
	3           "MALE, 15 - 17 YEARS OLD --"    
;
label values tm8752   tm8752l;
label define tm8752l 
	-3          "NONE - SKIP TO THE END"        
	0           "NOT IN UNIVERSE"               
	99          "DON'T KNOW FOR MEN ONLY"       
;
label values tm8754   tm8754l;
label define tm8754l 
	-3          "NONE - SKIP TO THE END"        
	0           "NOT IN UNIVERSE"               
;
label values tm8756   tm8756l;
label define tm8756l 
	0           "NOT IN UNIVERSE"               
	1           "YES - SKIP TO TM9266"          
	2           "NO"                            
;
label values tm8758   tm8758l;
label define tm8758l 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO - SKIP TO TM8778"           
;
label values tm8760   tm8760l;
label define tm8760l 
	0           "NOT IN UNIVERSE"               
;
label values tm8762   tm8762l;
label define tm8762l 
	0           "NOT IN UNIVERSE"               
;
label values tm8764   tm8764l;
label define tm8764l 
	999         "EDITED PERSON NUMBER"          
;
label values tm8766   tm8766l;
label define tm8766l 
	0           "NOT IN UNIVERSE"               
;
label values tm8768   tm8768l;
label define tm8768l 
	0           "NOT IN UNIVERSE"               
;
label values tm8770   tm8770l;
label define tm8770l 
	999         "EDITED PERSON NUMBER"          
;
label values tm8778   tm8778l;
label define tm8778l 
	0           "NOT IN UNIVERSE"               
	1           "ONE CHILD - SKIP TO TM8792"    
	2           "2 + CHILDREN"                  
;
label values tm8780   tm8780l;
label define tm8780l 
	0           "NOT IN UNIVERSE"               
;
label values tm8782   tm8782l;
label define tm8782l 
	0           "NOT IN UNIVERSE"               
;
label values tm8784   tm8784l;
label define tm8784l 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO - SKIP TO TM8792"           
;
label values tm8786   tm8786l;
label define tm8786l 
	0           "NOT IN UNIVERSE"               
	1           "RESIDES IN THIS HOUSEHOLD -"   
	2           "IN HIS/HER OWN HOUSEHOLD -"    
	3           "WITH OWN FATHER - SKIP TO"     
	4           "WITH OWN GRANDPARENT(S) -"     
	5           "WITH ADOPTIVE PARENTS - SKIP"  
	6           "WITH OTHER RELATIVES - SKIP"   
	7           "IN FOSTER CARE/FOSTER FAMILY"  
	8           "IN AN INSTITUTION (HOSPITAL)"  
	9           "IN SCHOOL - SKIP TO TM8792"    
	10          "IN CORRECTIONAL FACILITY -"    
	11          "OTHER - SKIP TO TM8792"        
	12          "DECREASE - SKIP TO TM8792"     
	13          "DON'T KNOW (DK)"               
;
label values tm8788   tm8788l;
label define tm8788l 
	999         "EDITED PERSON NUMBER"          
;
label values tm8792   tm8792l;
label define tm8792l 
	0           "NOT IN UNIVERSE"               
;
label values tm8794   tm8794l;
label define tm8794l 
	0           "NOT IN UNIVERSE"               
;
label values tm8796   tm8796l;
label define tm8796l 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO - SKIP TO TM9266"           
;
label values tm8798   tm8798l;
label define tm8798l 
	0           "NOT IN UNIVERSE"               
	1           "RESIDES IN THIS HOUSEHOLD -"   
	2           "IN HIS/HER OWN HOUSEHOLD -"    
	3           "WITH OWN FATHER - SKIP TO"     
	4           "WITH OWN GRANDPARENT(S) -"     
	5           "WITH ADOPTIVE PARENTS -"       
	6           "WITH OTHER RELATIVES -"        
	7           "IN FOSTER CARE/FOSTER FAMILY"  
	8           "IN AN INSTITUTION (HOSPITAL)"  
	9           "IN SCHOOL - SKIP TO TM9266"    
	10          "IN CORRECTIONAL FACILITY -"    
	11          "OTHER - SKIP TO TM9266"        
	12          "DECREASE - SKIP TO TM9266"     
	13          "DON'T KNOW (DK)"               
;
label values tm8800   tm8800l;
label define tm8800l 
	999         "EDITED PERSON NUMBER"          
;
label values imp8754  imp8754l;
label define imp8754l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8782  imp8782l;
label define imp8782l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8780  imp8780l;
label define imp8780l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8786  imp8786l;
label define imp8786l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8758  imp8758l;
label define imp8758l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8794  imp8794l;
label define imp8794l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8792  imp8792l;
label define imp8792l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values imp8798  imp8798l;
label define imp8798l
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
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
