log using sip85w5, text replace
set mem 1000m
*This program reads the 1985 SIPP Wave 5 Core Data File 

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
*by Jean Roth Wed Jun  9 16:43:36 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip85w5
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1985\sip85w5.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip85w5"

*Everything below this point are value labels

#delimit ;

;
label values state    state;  
label define state   
	0           "Not applicable"                
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
label values su_rgc   su_rgc; 
label define su_rgc  
	0           "Not applicable for coverage"   
;
label values h1_mis   h1_mis; 
label define h1_mis  
	0           "Not applicable"                
	1           "Interview"                     
	2           "Noninterview"                  
	3           "Not in sample"                 
;
label values h1_int1  h1_int1l;
label define h1_int1l
	0           "Not a wave 1 household"        
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	9           "Vacant"                        
	10          "Occupied by persons with URE"  
	11          "Unfit or to be demolished"     
	12          "Under construction, not ready" 
	13          "Converted to temporary business"
	14          "Unoccupied site for mobile home,"
	15          "Permit granted, construction not"
	16          "Other type B"                  
	17          "Demolished"                    
	18          "House or trailer moved"        
	19          "Converted to permanent business"
	20          "Merged"                        
	21          "Condemned"                     
	22          "Other"                         
;
label values h1_state h1_state;
label define h1_state
	0           "Not applicable"                
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
label values h1_metro h1_metro;
label define h1_metro
	0           "Not applicable"                
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subsample" 
;
label values h1_msa   h1_msa; 
label define h1_msa  
	0           "Not in universe or not identifiable"
	7           "Boston-Lawrence-Salem, MA-NH"  
	10          "Buffalo-Niagara Falls, NY"     
	14          "Chicago-Gary-Lake County, IL-IN"
	21          "Cincinnati-Hamilton, OH-KY"    
	28          "Cleveland-Akron-Lorain, OH"    
	31          "Dallas-Fort Worth, TX"         
	34          "Denver-Boulder, CO"            
	35          "Detroit-Ann Arbor, MI"         
	41          "Hartford-New Britain-Middletown, CT"
	42          "Houston, TX"                   
	49          "Los Angeles-Anaheim-Riverside, CA"
	56          "Miami-Fort Lauderdale, FL"     
	63          "Milwaukee-Racine, WI"          
	70          "New York-N. New Jersey-Long"   
	77          "Philadelphia-Wilmington-Trenton,"
	78          "Pittsburgh-Beaver Valley, PA"  
	79          "Portland-Vancouver, OR"        
	82          "St. Louis, IL-MO"              
	84          "San Francisco-Oakland-San Jose, CA"
	91          "Seattle-Tacoma, WA"            
	160         "Albany-Schenectady-Troy, NY"   
	200         "Albequerque, NM"               
	520         "Atlanta, GA"                   
	640         "Austin, TX"                    
	0680        "Bakersfield, CA"               
	760         "Baton Rouge, LA"               
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1520        "Charlotte-Gastonia-Rock Hill, NC"
	1720        "Colorado Springs, CO"          
	1840        "Columbus, OH"                  
	1880        "Corpus Christi, TX"            
	2000        "Dayton-Springfield, OH"        
	2320        "El Paso, TX"                   
	2400        "Eugene-Springfield, OR"        
	2560        "Fayetville, NC"                
	2700        "Ft. Myers-Cape Coral, FL"      
	2760        "Fort Wayne, IN"                
	2840        "Fresno, CA"                    
	3120        "Greensboro-Winston-Salem-High Point, NC"
	3160        "Greenville - Spartansburg, SC" 
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3320        "Honolulu, HI"                  
	3480        "Indianapolis, IN"              
	3600        "Jacksonville, FL"              
	3840        "Knoxville, TN"                 
	3980        "Lakeland-Winterhaven, FL"      
	4040        "Lansing-East Lansing, MI"      
	4720        "Madison, WI"                   
	4880        "McCallen-Edinburg-Mission, TX" 
	4900        "Melbourne-Titusville-Palm Bay, FL"
	4920        "Memphis, TN"                   
	5120        "Minneapolis-St. Paul, MN"      
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5720        "Norfolk-VA Beach-Newport News, VA"
	5880        "Oklahoma City, OK"             
	5960        "Orlando, FL"                   
	6080        "Pensacola, FL"                 
	6200        "Phoenix, AZ"                   
	6640        "Raleigh-Durham, NC"            
	6840        "Rochester, NY"                 
	6880        "Rockford, IL"                  
	6920        "Sacramento, CA"                
	7120        "Salinas-Seaside-Monterey, CA"  
	7160        "Salt Lake City-Ogden, UT"      
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8120        "Stockton, CA"                  
	8160        "Syracuse, NY"                  
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8680        "Utica-Rome, NY"                
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
;
label values h1_acces h1_acces;
label define h1_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h1_lvqtr h1_lvqtr;
label define h1_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values h1_units h1_units;
label define h1_units
	0           "Not applicable"                
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values h1_tenur h1_tenur;
label define h1_tenur
	0           "Not applicable"                
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
;
label values h1_pubhs h1_pubhs;
label define h1_pubhs
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1_lornt h1_lornt;
label define h1_lornt
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1_race  h1_race;
label define h1_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
;
label values h1_sex   h1_sex; 
label define h1_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
;
label values h1_size  h1_size;
label define h1_size 
	0           "Not applicable"                
;
label values h1_seg   h1_seg; 
label define h1_seg  
	0           "Not applicable"                
	1           "Address"                       
	2           "Unit"                          
	3           "Permit"                        
	4           "Area"                          
	5           "Special place"                 
;
label values u1frmsle u1frmsle;
label define u1frmsle
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	3           "Not applicable"                
;
label values u1_acces u1_acces;
label define u1_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values u1_lvqtr u1_lvqtr;
label define u1_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values u1_units u1_units;
label define u1_units
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values u1_tenur u1_tenur;
label define u1_tenur
	0           "Not answered (types B and C)"  
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
	9           "Not answered"                  
;
label values u1_pubhs u1_pubhs;
label define u1_pubhs
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u1_lornt u1_lornt;
label define u1_lornt
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1itm36b h1itm36b;
label define h1itm36b
	0           "Not applicable"                
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	22          "Deleted (sample adjustment, error)"
	23          "Entire household deceased, moved"
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
;
label values u1_race  u1_race;
label define u1_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
	9           "Not answered"                  
;
label values u1_sex   u1_sex; 
label define u1_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
	9           "Not answered"                  
;
label values u1_size  u1_size;
label define u1_size 
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u1totvst u1totvst;
label define u1totvst
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u1totphn u1totphn;
label define u1totphn
	0           "Not applicable"                
;
label values u1ccrspp u1ccrspp;
label define u1ccrspp
	0           "Not applicable"                
;
label values h1_0010  h1_0010l;
label define h1_0010l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h1_0012  h1_0012l;
label define h1_0012l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h1_0014  h1_0014l;
label define h1_0014l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h1_0016  h1_0016l;
label define h1_0016l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h1_0018  h1_0018l;
label define h1_0018l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1_0020  h1_0020l;
label define h1_0020l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1_00221 h1_0022l;
label values h1_00222 h1_0022l;
label values h1_00223 h1_0022l;
label values h1_00224 h1_0022l;
label values h1_00225 h1_0022l;
label values h1_00226 h1_0022l;
label values h1_00227 h1_0022l;
label define h1_0022l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1_00241 h1_0024l;
label values h1_00242 h1_0024l;
label values h1_00243 h1_0024l;
label values h1_00244 h1_0024l;
label values h1_00245 h1_0024l;
label values h1_00246 h1_0024l;
label values h1_00247 h1_0024l;
label define h1_0024l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1_means h1_means;
label define h1_means
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h1_cash  h1_cash;
label define h1_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h1ncashb h1ncashb;
label define h1ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received"
;
label values h1_enrgy h1_enrgy;
label define h1_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to"   
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers"
	5           "Checks sent to household and"  
	6           "Coupons or voucher sent to"    
	7           "All three types of assistance" 
;
label values h1_lunch h1_lunch;
label define h1_lunch
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h1_break h1_break;
label define h1_break
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h1_4816  h1_4816l;
label define h1_4816l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to H1-4826"          
;
label values h1_4818  h1_4818l;
label define h1_4818l
	0           "Not marked as a place where"   
	1           "Marked as a place where"       
;
label values h1_4820  h1_4820l;
label define h1_4820l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h1_4822  h1_4822l;
label define h1_4822l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h1_4824  h1_4824l;
label define h1_4824l
	0           "Not in universe"               
;
label values h1_4826  h1_4826l;
label define h1_4826l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h1_4828  h1_4828l;
label define h1_4828l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h1_4830  h1_4830l;
label define h1_4830l
	0           "Not in universe"               
;
label values h1_4832  h1_4832l;
label define h1_4832l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No- skip to H1-4840"           
;
label values h1_4834  h1_4834l;
label define h1_4834l
	0           "Not in universe"               
;
label values h1_4836  h1_4836l;
label define h1_4836l
	0           "Not marked as a free lunch or" 
	1           "Marked as a free lunch"        
;
label values h1_4838  h1_4838l;
label define h1_4838l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h1_4840  h1_4840l;
label define h1_4840l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h1_4842  h1_4842l;
label define h1_4842l
	0           "Not in universe"               
;
label values h1_4844  h1_4844l;
label define h1_4844l
	0           "Not marked as a free breakfast"
	1           "Marked as a free breakfast"    
;
label values h1_4846  h1_4846l;
label define h1_4846l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h1_imp06 h1_imp0k;
label define h1_imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp07 h1_imp0l;
label define h1_imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp08 h1_imp0m;
label define h1_imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp09 h1_imp0n;
label define h1_imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp10 h1_imp1k;
label define h1_imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp11 h1_imp1l;
label define h1_imp1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp12 h1_imp1m;
label define h1_imp1m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp13 h1_imp1n;
label define h1_imp1n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp14 h1_imp1o;
label define h1_imp1o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp15 h1_imp1p;
label define h1_imp1p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp16 h1_imp1q;
label define h1_imp1q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp17 h1_imp1r;
label define h1_imp1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp18 h1_imp1s;
label define h1_imp1s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp19 h1_imp1t;
label define h1_imp1t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_imp20 h1_imp2k;
label define h1_imp2k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_mis   h2_mis; 
label define h2_mis  
	0           "Not applicable"                
	1           "Interview"                     
	2           "Noninterview"                  
	3           "Not in sample"                 
;
label values h2_int1  h2_int1l;
label define h2_int1l
	0           "Not a wave 1 household"        
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	9           "Vacant"                        
	10          "Occupied by persons with URE"  
	11          "Unfit or to be demolished"     
	12          "Under construction, not ready" 
	13          "Converted to temporary business"
	14          "Unoccupied site for mobile home,"
	15          "Permit granted, construction not"
	16          "Other type B"                  
	17          "Demolished"                    
	18          "House or trailer moved"        
	19          "Converted to permanent business"
	20          "Merged"                        
	21          "Condemned"                     
	22          "Other"                         
;
label values h2_state h2_state;
label define h2_state
	0           "Not applicable"                
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
label values h2_metro h2_metro;
label define h2_metro
	0           "Not applicable"                
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subfamily" 
;
label values h2_msa   h2_msa; 
label define h2_msa  
	0           "Not in universe or not identifiable"
	7           "Boston-Lawrence-Salem, MA-NH"  
	10          "Buffalo-Niagara Falls, NY"     
	14          "Chicago-Gary-Lake County, IL-IN"
	21          "Cincinnati-Hamilton, OH-KY"    
	28          "Cleveland-Akron-Lorain, OH"    
	31          "Dallas-Fort Worth, TX"         
	34          "Denver-Boulder, CO"            
	35          "Detroit-Ann Arbor, MI"         
	41          "Hartford-New Britain-Middletown, CT"
	42          "Houston, TX"                   
	49          "Los Angeles-Anaheim-Riverside, CA"
	56          "Miami-Fort Lauderdale, FL"     
	63          "Milwaukee-Racine, WI"          
	70          "New York-N. New Jersey-Long"   
	77          "Philadelphia-Wilmington-Trenton,"
	78          "Pittsburgh-Beaver Valley, PA"  
	79          "Portland-Vancouver, OR"        
	82          "St. Louis, IL-MO"              
	84          "San Francisco-Oakland-San Jose, CA"
	91          "Seattle-Tacoma, WA"            
	160         "Albany-Schenectady-Troy, NY"   
	200         "Albequerque, NM"               
	520         "Atlanta, GA"                   
	640         "Austin, TX"                    
	0680        "Bakersfield, CA"               
	760         "Baton Rouge, LA"               
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1520        "Charlotte-Gastonia-Rock Hill, NC"
	1720        "Colorado Springs, CO"          
	1840        "Columbus, OH"                  
	1880        "Corpus Christi, TX"            
	2000        "Dayton-Springfield, OH"        
	2320        "El Paso, TX"                   
	2400        "Eugene-Springfield, OR"        
	2560        "Fayetville, NC"                
	2700        "Ft. Myers-Cape Coral, FL"      
	2760        "Fort Wayne, IN"                
	2840        "Fresno, CA"                    
	3120        "Greensboro-Winston-Salem-High Point, NC"
	3160        "Greenville - Spartansburg, SC" 
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3320        "Honolulu, HI"                  
	3480        "Indianapolis, IN"              
	3600        "Jacksonville, FL"              
	3840        "Knoxville, TN"                 
	3980        "Lakeland-Winterhaven, FL"      
	4040        "Lansing-East Lansing, MI"      
	4720        "Madison, WI"                   
	4880        "McCallen-Edinburg-Mission, TX" 
	4900        "Melbourne-Titusville-Palm Bay, FL"
	4920        "Memphis, TN"                   
	5120        "Minneapolis-St. Paul, MN"      
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5720        "Norfolk-VA Beach-Newport News, VA"
	5880        "Oklahoma City, OK"             
	5960        "Orlando, FL"                   
	6080        "Pensacola, FL"                 
	6200        "Phoenix, AZ"                   
	6640        "Raleigh-Durham, NC"            
	6840        "Rochester, NY"                 
	6880        "Rockford, IL"                  
	6920        "Sacramento, CA"                
	7120        "Salinas-Seaside-Monterey, CA"  
	7160        "Salt Lake City-Ogden, UT"      
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8120        "Stockton, CA"                  
	8160        "Syracuse, NY"                  
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8680        "Utica-Rome, NY"                
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
;
label values h2_acces h2_acces;
label define h2_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h2_lvqtr h2_lvqtr;
label define h2_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values h2_units h2_units;
label define h2_units
	0           "Not applicable"                
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values h2_tenur h2_tenur;
label define h2_tenur
	0           "Not applicable"                
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
;
label values h2_pubhs h2_pubhs;
label define h2_pubhs
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2_lornt h2_lornt;
label define h2_lornt
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2_race  h2_race;
label define h2_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
;
label values h2_sex   h2_sex; 
label define h2_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
;
label values h2_size  h2_size;
label define h2_size 
	0           "Not applicable"                
;
label values h2_seg   h2_seg; 
label define h2_seg  
	0           "Not applicable"                
	1           "Address"                       
	2           "Unit"                          
	3           "Permit"                        
	4           "Area"                          
	5           "Special place"                 
;
label values u2frmsle u2frmsle;
label define u2frmsle
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	3           "Not applicable"                
;
label values u2_acces u2_acces;
label define u2_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values u2_lvqtr u2_lvqtr;
label define u2_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values u2_units u2_units;
label define u2_units
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values u2_tenur u2_tenur;
label define u2_tenur
	0           "Not answered (types B and C)"  
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
	9           "Not answered"                  
;
label values u2_pubhs u2_pubhs;
label define u2_pubhs
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u2_lornt u2_lornt;
label define u2_lornt
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2itm36b h2itm36b;
label define h2itm36b
	0           "Not applicable"                
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	23          "Entire household out-of-scope" 
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
;
label values u2_race  u2_race;
label define u2_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
	9           "Not answered"                  
;
label values u2_sex   u2_sex; 
label define u2_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
	9           "Not answered"                  
;
label values u2_size  u2_size;
label define u2_size 
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u2totvst u2totvst;
label define u2totvst
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u2totphn u2totphn;
label define u2totphn
	0           "Not applicable"                
;
label values u2ccrspp u2ccrspp;
label define u2ccrspp
	0           "Not applicable"                
;
label values h2_0010  h2_0010l;
label define h2_0010l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h2_0012  h2_0012l;
label define h2_0012l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h2_0014  h2_0014l;
label define h2_0014l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h2_0016  h2_0016l;
label define h2_0016l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h2_0018  h2_0018l;
label define h2_0018l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2_0020  h2_0020l;
label define h2_0020l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2_00221 h2_0022l;
label values h2_00222 h2_0022l;
label values h2_00223 h2_0022l;
label values h2_00224 h2_0022l;
label values h2_00225 h2_0022l;
label values h2_00226 h2_0022l;
label values h2_00227 h2_0022l;
label define h2_0022l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2_00241 h2_0024l;
label values h2_00242 h2_0024l;
label values h2_00243 h2_0024l;
label values h2_00244 h2_0024l;
label values h2_00245 h2_0024l;
label values h2_00246 h2_0024l;
label values h2_00247 h2_0024l;
label define h2_0024l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2_means h2_means;
label define h2_means
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h2_cash  h2_cash;
label define h2_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h2ncashb h2ncashb;
label define h2ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received"
;
label values h2_enrgy h2_enrgy;
label define h2_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to"   
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers"
	5           "Checks sent to household and"  
	6           "Coupons or voucher sent to"    
	7           "All three types of assistance" 
;
label values h2_lunch h2_lunch;
label define h2_lunch
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h2_break h2_break;
label define h2_break
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h2_4816  h2_4816l;
label define h2_4816l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to H2-4826"          
;
label values h2_4818  h2_4818l;
label define h2_4818l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h2_4820  h2_4820l;
label define h2_4820l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h2_4822  h2_4822l;
label define h2_4822l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h2_4824  h2_4824l;
label define h2_4824l
	0           "Not in universe"               
;
label values h2_4826  h2_4826l;
label define h2_4826l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h2_4828  h2_4828l;
label define h2_4828l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h2_4830  h2_4830l;
label define h2_4830l
	0           "Not in universe"               
;
label values h2_4832  h2_4832l;
label define h2_4832l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No- skip to H2-4840"           
;
label values h2_4834  h2_4834l;
label define h2_4834l
	0           "Not in universe"               
;
label values h2_4836  h2_4836l;
label define h2_4836l
	0           "Not marked as a free lunch or" 
	1           "Marked as a free lunch"        
;
label values h2_4838  h2_4838l;
label define h2_4838l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h2_4840  h2_4840l;
label define h2_4840l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h2_4842  h2_4842l;
label define h2_4842l
	0           "Not in universe"               
;
label values h2_4844  h2_4844l;
label define h2_4844l
	0           "Not marked as a free breakfast"
	1           "Marked as a free breakfast"    
;
label values h2_4846  h2_4846l;
label define h2_4846l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h2_imp06 h2_imp0k;
label define h2_imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp07 h2_imp0l;
label define h2_imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp08 h2_imp0m;
label define h2_imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp09 h2_imp0n;
label define h2_imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp10 h2_imp1k;
label define h2_imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp11 h2_imp1l;
label define h2_imp1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp12 h2_imp1m;
label define h2_imp1m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp13 h2_imp1n;
label define h2_imp1n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp14 h2_imp1o;
label define h2_imp1o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp15 h2_imp1p;
label define h2_imp1p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp16 h2_imp1q;
label define h2_imp1q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp17 h2_imp1r;
label define h2_imp1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp18 h2_imp1s;
label define h2_imp1s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp19 h2_imp1t;
label define h2_imp1t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h2_imp20 h2_imp2k;
label define h2_imp2k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_mis   h3_mis; 
label define h3_mis  
	0           "Not applicable"                
	1           "Interview"                     
	2           "Noninterview"                  
	3           "Not in sample"                 
;
label values h3_int1  h3_int1l;
label define h3_int1l
	0           "Not a wave 1 household"        
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	9           "Vacant"                        
	10          "Occupied by persons with URE"  
	11          "Unfit or to be demolished"     
	12          "Under construction, not ready" 
	13          "Converted to temporary business"
	14          "Unoccupied site for mobile home,"
	15          "Permit granted, construction not"
	16          "Other type B"                  
	17          "Demolished"                    
	18          "House or trailer moved"        
	19          "Converted to permanent business"
	20          "Merged"                        
	21          "Condemned"                     
	22          "Other"                         
;
label values h3_state h3_state;
label define h3_state
	0           "Not applicable"                
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
label values h3_metro h3_metro;
label define h3_metro
	0           "Not applicable"                
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subfamily" 
;
label values h3_msa   h3_msa; 
label define h3_msa  
	0           "Not in universe or not identifiable"
	7           "Boston-Lawrence-Salem, MA-NH"  
	10          "Buffalo-Niagara Falls, NY"     
	14          "Chicago-Gary-Lake County, IL-IN"
	21          "Cincinnati-Hamilton, OH-KY"    
	28          "Cleveland-Akron-Lorain, OH"    
	31          "Dallas-Fort Worth, TX"         
	34          "Denver-Boulder, CO"            
	35          "Detroit-Ann Arbor, MI"         
	41          "Hartford-New Britain-Middletown, CT"
	42          "Houston, TX"                   
	49          "Los Angeles-Anaheim-Riverside, CA"
	56          "Miami-Fort Lauderdale, FL"     
	63          "Milwaukee-Racine, WI"          
	70          "New York-N. New Jersey-Long"   
	77          "Philadelphia-Wilmington-Trenton,"
	78          "Pittsburgh-Beaver Valley, PA"  
	79          "Portland-Vancouver, OR"        
	82          "St. Louis, IL-MO"              
	84          "San Francisco-Oakland-San Jose, CA"
	91          "Seattle-Tacoma, WA"            
	160         "Albany-Schenectady-Troy, NY"   
	200         "Albequerque, NM"               
	520         "Atlanta, GA"                   
	640         "Austin, TX"                    
	0680        "Bakersfield, CA"               
	760         "Baton Rouge, LA"               
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1520        "Charlotte-Gastonia-Rock Hill, NC"
	1720        "Colorado Springs, CO"          
	1840        "Columbus, OH"                  
	1880        "Corpus Christi, TX"            
	2000        "Dayton-Springfield, OH"        
	2320        "El Paso, TX"                   
	2400        "Eugene-Springfield, OR"        
	2560        "Fayetville, NC"                
	2700        "Ft. Myers-Cape Coral, FL"      
	2760        "Fort Wayne, IN"                
	2840        "Fresno, CA"                    
	3120        "Greensboro-Winston-Salem-High Point, NC"
	3160        "Greenville - Spartansburg, SC" 
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3320        "Honolulu, HI"                  
	3480        "Indianapolis, IN"              
	3600        "Jacksonville, FL"              
	3840        "Knoxville, TN"                 
	3980        "Lakeland-Winterhaven, FL"      
	4040        "Lansing-East Lansing, MI"      
	4720        "Madison, WI"                   
	4880        "McCallen-Edinburg-Mission, TX" 
	4900        "Melbourne-Titusville-Palm Bay, FL"
	4920        "Memphis, TN"                   
	5120        "Minneapolis-St. Paul, MN"      
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5720        "Norfolk-VA Beach-Newport News, VA"
	5880        "Oklahoma City, OK"             
	5960        "Orlando, FL"                   
	6080        "Pensacola, FL"                 
	6200        "Phoenix, AZ"                   
	6640        "Raleigh-Durham, NC"            
	6840        "Rochester, NY"                 
	6880        "Rockford, IL"                  
	6920        "Sacramento, CA"                
	7120        "Salinas-Seaside-Monterey, CA"  
	7160        "Salt Lake City-Ogden, UT"      
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8120        "Stockton, CA"                  
	8160        "Syracuse, NY"                  
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8680        "Utica-Rome, NY"                
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
;
label values h3_acces h3_acces;
label define h3_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h3_lvqtr h3_lvqtr;
label define h3_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values h3_units h3_units;
label define h3_units
	0           "Not applicable"                
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values h3_tenur h3_tenur;
label define h3_tenur
	0           "Not applicable"                
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
;
label values h3_pubhs h3_pubhs;
label define h3_pubhs
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3_lornt h3_lornt;
label define h3_lornt
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3_race  h3_race;
label define h3_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
;
label values h3_sex   h3_sex; 
label define h3_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
;
label values h3_size  h3_size;
label define h3_size 
	0           "Not applicable"                
;
label values h3_seg   h3_seg; 
label define h3_seg  
	0           "Not applicable"                
	1           "Address"                       
	2           "Unit"                          
	3           "Permit"                        
	4           "Area"                          
	5           "Special place"                 
;
label values u3frmsle u3frmsle;
label define u3frmsle
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	3           "Not applicable"                
;
label values u3_acces u3_acces;
label define u3_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values u3_lvqtr u3_lvqtr;
label define u3_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values u3_units u3_units;
label define u3_units
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values u3_tenur u3_tenur;
label define u3_tenur
	0           "Not answered (types B and C)"  
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
	9           "Not answered"                  
;
label values u3_pubhs u3_pubhs;
label define u3_pubhs
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u3_lornt u3_lornt;
label define u3_lornt
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3itm36b h3itm36b;
label define h3itm36b
	0           "Not applicable"                
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	22          "Deleted (sample adjustment, error)"
	23          "Entire household deceased, moved"
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
;
label values u3_race  u3_race;
label define u3_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
	9           "Not answered"                  
;
label values u3_sex   u3_sex; 
label define u3_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
	9           "Not answered"                  
;
label values u3_size  u3_size;
label define u3_size 
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u3totvst u3totvst;
label define u3totvst
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u3totphn u3totphn;
label define u3totphn
	0           "Not applicable"                
;
label values u3ccrspp u3ccrspp;
label define u3ccrspp
	0           "Not applicable"                
;
label values h3_0010  h3_0010l;
label define h3_0010l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h3_0012  h3_0012l;
label define h3_0012l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h3_0014  h3_0014l;
label define h3_0014l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h3_0016  h3_0016l;
label define h3_0016l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h3_0018  h3_0018l;
label define h3_0018l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3_0020  h3_0020l;
label define h3_0020l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3_00221 h3_0022l;
label values h3_00222 h3_0022l;
label values h3_00223 h3_0022l;
label values h3_00224 h3_0022l;
label values h3_00225 h3_0022l;
label values h3_00226 h3_0022l;
label values h3_00227 h3_0022l;
label define h3_0022l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3_00241 h3_0024l;
label values h3_00242 h3_0024l;
label values h3_00243 h3_0024l;
label values h3_00244 h3_0024l;
label values h3_00245 h3_0024l;
label values h3_00246 h3_0024l;
label values h3_00247 h3_0024l;
label define h3_0024l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3_means h3_means;
label define h3_means
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h3_cash  h3_cash;
label define h3_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h3ncashb h3ncashb;
label define h3ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received"
;
label values h3_enrgy h3_enrgy;
label define h3_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to"   
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers"
	5           "Checks sent to household and"  
	6           "Coupons or voucher sent to"    
	7           "All three types of assistance" 
;
label values h3_lunch h3_lunch;
label define h3_lunch
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h3_break h3_break;
label define h3_break
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h3_4816  h3_4816l;
label define h3_4816l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to H3-4826"          
;
label values h3_4818  h3_4818l;
label define h3_4818l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h3_4820  h3_4820l;
label define h3_4820l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h3_4822  h3_4822l;
label define h3_4822l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h3_4824  h3_4824l;
label define h3_4824l
	0           "Not in universe"               
;
label values h3_4826  h3_4826l;
label define h3_4826l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h3_4828  h3_4828l;
label define h3_4828l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h3_4830  h3_4830l;
label define h3_4830l
	0           "Not in universe"               
;
label values h3_4832  h3_4832l;
label define h3_4832l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No- skip to H3-4840"           
;
label values h3_4834  h3_4834l;
label define h3_4834l
	0           "Not in universe"               
;
label values h3_4836  h3_4836l;
label define h3_4836l
	0           "Not marked as a free lunch or" 
	1           "Marked as a free lunch"        
;
label values h3_4838  h3_4838l;
label define h3_4838l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h3_4840  h3_4840l;
label define h3_4840l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h3_4842  h3_4842l;
label define h3_4842l
	0           "Not in universe"               
;
label values h3_4844  h3_4844l;
label define h3_4844l
	0           "Not marked as a free breakfast"
	1           "Marked as a free breakfast"    
;
label values h3_4846  h3_4846l;
label define h3_4846l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h3_imp06 h3_imp0k;
label define h3_imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp07 h3_imp0l;
label define h3_imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp08 h3_imp0m;
label define h3_imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp09 h3_imp0n;
label define h3_imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp10 h3_imp1k;
label define h3_imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp11 h3_imp1l;
label define h3_imp1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp12 h3_imp1m;
label define h3_imp1m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp13 h3_imp1n;
label define h3_imp1n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp14 h3_imp1o;
label define h3_imp1o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp15 h3_imp1p;
label define h3_imp1p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp16 h3_imp1q;
label define h3_imp1q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp17 h3_imp1r;
label define h3_imp1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp18 h3_imp1s;
label define h3_imp1s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp19 h3_imp1t;
label define h3_imp1t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h3_imp20 h3_imp2k;
label define h3_imp2k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_mis   h4_mis; 
label define h4_mis  
	0           "Not applicable"                
	1           "Interview"                     
	2           "Noninterview"                  
	3           "Not in sample"                 
;
label values h4_int1  h4_int1l;
label define h4_int1l
	0           "Not a wave 1 household"        
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	9           "Vacant"                        
	10          "Occupied by persons with URE"  
	11          "Unfit or to be demolished"     
	12          "Under construction, not ready" 
	13          "Converted to temporary business"
	14          "Unoccupied site for mobile home,"
	15          "Permit granted, construction not"
	16          "Other type B"                  
	17          "Demolished"                    
	18          "House or trailer moved"        
	19          "Converted to permanent business"
	20          "Merged"                        
	21          "Condemned"                     
	22          "Other"                         
;
label values h4_state h4_state;
label define h4_state
	0           "Not applicable"                
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
label values h4_metro h4_metro;
label define h4_metro
	0           "Not applicable"                
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subfamily" 
;
label values h4_msa   h4_msa; 
label define h4_msa  
	0           "Not in universe or not identifiable"
	7           "Boston-Lawrence-Salem, MA-NH"  
	10          "Buffalo-Niagara Falls, NY"     
	14          "Chicago-Gary-Lake County, IL-IN"
	21          "Cincinnati-Hamilton, OH-KY"    
	28          "Cleveland-Akron-Lorain, OH"    
	31          "Dallas-Fort Worth, TX"         
	34          "Denver-Boulder, CO"            
	35          "Detroit-Ann Arbor, MI"         
	41          "Hartford-New Britain-Middletown, CT"
	42          "Houston, TX"                   
	49          "Los Angeles-Anaheim-Riverside, CA"
	56          "Miami-Fort Lauderdale, FL"     
	63          "Milwaukee-Racine, WI"          
	70          "New York-N. New Jersey-Long"   
	77          "Philadelphia-Wilmington-Trenton,"
	78          "Pittsburgh-Beaver Valley, PA"  
	79          "Portland-Vancouver, OR"        
	82          "St. Louis, IL-MO"              
	84          "San Francisco-Oakland-San Jose, CA"
	91          "Seattle-Tacoma, WA"            
	160         "Albany-Schenectady-Troy, NY"   
	200         "Albequerque, NM"               
	520         "Atlanta, GA"                   
	640         "Austin, TX"                    
	0680        "Bakersfield, CA"               
	760         "Baton Rouge, LA"               
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1520        "Charlotte-Gastonia-Rock Hill, NC"
	1720        "Colorado Springs, CO"          
	1840        "Columbus, OH"                  
	1880        "Corpus Christi, TX"            
	2000        "Dayton-Springfield, OH"        
	2320        "El Paso, TX"                   
	2400        "Eugene-Springfield, OR"        
	2560        "Fayetville, NC"                
	2700        "Ft. Myers-Cape Coral, FL"      
	2760        "Fort Wayne, IN"                
	2840        "Fresno, CA"                    
	3120        "Greensboro-Winston-Salem-High Point, NC"
	3160        "Greenville - Spartansburg, SC" 
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3320        "Honolulu, HI"                  
	3480        "Indianapolis, IN"              
	3600        "Jacksonville, FL"              
	3840        "Knoxville, TN"                 
	3980        "Lakeland-Winterhaven, FL"      
	4040        "Lansing-East Lansing, MI"      
	4720        "Madison, WI"                   
	4880        "McCallen-Edinburg-Mission, TX" 
	4900        "Melbourne-Titusville-Palm Bay, FL"
	4920        "Memphis, TN"                   
	5120        "Minneapolis-St. Paul, MN"      
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5720        "Norfolk-VA Beach-Newport News, VA"
	5880        "Oklahoma City, OK"             
	5960        "Orlando, FL"                   
	6080        "Pensacola, FL"                 
	6200        "Phoenix, AZ"                   
	6640        "Raleigh-Durham, NC"            
	6840        "Rochester, NY"                 
	6880        "Rockford, IL"                  
	6920        "Sacramento, CA"                
	7120        "Salinas-Seaside-Monterey, CA"  
	7160        "Salt Lake City-Ogden, UT"      
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8120        "Stockton, CA"                  
	8160        "Syracuse, NY"                  
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8680        "Utica-Rome, NY"                
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
;
label values h4_acces h4_acces;
label define h4_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h4_lvqtr h4_lvqtr;
label define h4_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values h4_units h4_units;
label define h4_units
	0           "Not applicable"                
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values h4_tenur h4_tenur;
label define h4_tenur
	0           "Not applicable"                
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
;
label values h4_pubhs h4_pubhs;
label define h4_pubhs
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4_lornt h4_lornt;
label define h4_lornt
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4_race  h4_race;
label define h4_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
;
label values h4_sex   h4_sex; 
label define h4_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
;
label values h4_size  h4_size;
label define h4_size 
	0           "Not applicable"                
;
label values h4_seg   h4_seg; 
label define h4_seg  
	0           "Not applicable"                
	1           "Address"                       
	2           "Unit"                          
	3           "Permit"                        
	4           "Area"                          
	5           "Special place"                 
;
label values u4frmsle u4frmsle;
label define u4frmsle
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	3           "Not applicable"                
;
label values u4_acces u4_acces;
label define u4_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values u4_lvqtr u4_lvqtr;
label define u4_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values u4_units u4_units;
label define u4_units
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "Only 'other' units"            
	2           "Mobile home or trailer"        
	3           "One, detached"                 
	4           "One, attached"                 
	5           "Two"                           
	6           "3-4"                           
	7           "5-9"                           
	8           "10-19"                         
	9           "20-49"                         
	10          "50 or more"                    
;
label values u4_tenur u4_tenur;
label define u4_tenur
	0           "Not answered (types B and C)"  
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash"
	9           "Not answered"                  
;
label values u4_pubhs u4_pubhs;
label define u4_pubhs
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u4_lornt u4_lornt;
label define u4_lornt
	-1          "DK"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4itm36b h4itm36b;
label define h4itm36b
	0           "Not applicable"                
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	22          "Deleted (sample adjustment, error)"
	23          "Entire household deceased, moved"
	24          "Moved, address unknown"        
	25          "Moved within country beyond"   
	26          "All sample persons relisted on"
;
label values u4_race  u4_race;
label define u4_race 
	0           "Not applicable"                
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
	9           "Not answered"                  
;
label values u4_sex   u4_sex; 
label define u4_sex  
	0           "Not applicable"                
	1           "Male"                          
	2           "Female"                        
	9           "Not answered"                  
;
label values u4_size  u4_size;
label define u4_size 
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u4totvst u4totvst;
label define u4totvst
	-9          "Not answered"                  
	0           "Not applicable"                
;
label values u4totphn u4totphn;
label define u4totphn
	0           "Not applicable"                
;
label values u4ccrspp u4ccrspp;
label define u4ccrspp
	0           "Not applicable"                
;
label values h4_0010  h4_0010l;
label define h4_0010l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h4_0012  h4_0012l;
label define h4_0012l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h4_0014  h4_0014l;
label define h4_0014l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h4_0016  h4_0016l;
label define h4_0016l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values h4_0018  h4_0018l;
label define h4_0018l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4_0020  h4_0020l;
label define h4_0020l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4_00221 h4_0022l;
label values h4_00222 h4_0022l;
label values h4_00223 h4_0022l;
label values h4_00224 h4_0022l;
label values h4_00225 h4_0022l;
label values h4_00226 h4_0022l;
label values h4_00227 h4_0022l;
label define h4_0022l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4_00241 h4_0024l;
label values h4_00242 h4_0024l;
label values h4_00243 h4_0024l;
label values h4_00244 h4_0024l;
label values h4_00245 h4_0024l;
label values h4_00246 h4_0024l;
label values h4_00247 h4_0024l;
label define h4_0024l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4_means h4_means;
label define h4_means
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h4_cash  h4_cash;
label define h4_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received"
;
label values h4ncashb h4ncashb;
label define h4ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received"
;
label values h4_enrgy h4_enrgy;
label define h4_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to"   
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers"
	5           "Checks sent to household and"  
	6           "Coupons or voucher sent to"    
	7           "All three types of assistance" 
;
label values h4_lunch h4_lunch;
label define h4_lunch
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h4_break h4_break;
label define h4_break
	0           "Not applicable"                
	1           "Free"                          
	2           "Reduced-price"                 
	3           "Both"                          
;
label values h4_4816  h4_4816l;
label define h4_4816l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to H4-4826"          
;
label values h4_4818  h4_4818l;
label define h4_4818l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h4_4820  h4_4820l;
label define h4_4820l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h4_4822  h4_4822l;
label define h4_4822l
	0           "Not marked as a place where"   
	1           "Marked as a place where payments"
;
label values h4_4824  h4_4824l;
label define h4_4824l
	0           "Not in universe"               
;
label values h4_4826  h4_4826l;
label define h4_4826l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h4_4828  h4_4828l;
label define h4_4828l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h4_4830  h4_4830l;
label define h4_4830l
	0           "Not in universe"               
;
label values h4_4832  h4_4832l;
label define h4_4832l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No- skip to H4-4840"           
;
label values h4_4834  h4_4834l;
label define h4_4834l
	0           "Not in universe"               
;
label values h4_4836  h4_4836l;
label define h4_4836l
	0           "Not marked as a free lunch or" 
	1           "Marked as a free lunch"        
;
label values h4_4838  h4_4838l;
label define h4_4838l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h4_4840  h4_4840l;
label define h4_4840l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values h4_4842  h4_4842l;
label define h4_4842l
	0           "Not in universe"               
;
label values h4_4844  h4_4844l;
label define h4_4844l
	0           "Not marked as a free breakfast"
	1           "Marked as a free breakfast"    
;
label values h4_4846  h4_4846l;
label define h4_4846l
	0           "Not marked as a reduced-price" 
	1           "Marked as a reduced-price"     
;
label values h4_imp06 h4_imp0k;
label define h4_imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp07 h4_imp0l;
label define h4_imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp08 h4_imp0m;
label define h4_imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp09 h4_imp0n;
label define h4_imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp10 h4_imp1k;
label define h4_imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp11 h4_imp1l;
label define h4_imp1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp12 h4_imp1m;
label define h4_imp1m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp13 h4_imp1n;
label define h4_imp1n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp14 h4_imp1o;
label define h4_imp1o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp15 h4_imp1p;
label define h4_imp1p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp16 h4_imp1q;
label define h4_imp1q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp17 h4_imp1r;
label define h4_imp1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp18 h4_imp1s;
label define h4_imp1s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp19 h4_imp1t;
label define h4_imp1t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h4_imp20 h4_imp2k;
label define h4_imp2k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h5_mis   h5_mis; 
label define h5_mis  
	0           "Not applicable"                
	1           "Interview"                     
	2           "Noninterview"                  
	3           "Not in sample"                 
;
label values h5_lvqtr h5_lvqtr;
label define h5_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Housing/other unit (HU) in"    
	3           "Hu, permanent in transient"    
	4           "Housing/other unit (HU) in"    
	5           "Mobile home or trailer with one"
	6           "Mobile home or trailer with one"
	7           "Housing/other unit (HU) not"   
	8           "Quarters not Housing/other unit"
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values f1_type  f1_type;
label define f1_type 
	0           "Not applicable"                
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f1_kind  f1_kind;
label define f1_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f1ownkid f1ownkid;
label define f1ownkid
	0           "None"                          
;
label values f1oklt18 f1oklt1d;
label define f1oklt1d
	0           "None"                          
;
label values f2_type  f2_type;
label define f2_type 
	0           "Not applicable"                
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f2_kind  f2_kind;
label define f2_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f2ownkid f2ownkid;
label define f2ownkid
	0           "None"                          
;
label values f2oklt18 f2oklt1d;
label define f2oklt1d
	0           "None"                          
;
label values f3_type  f3_type;
label define f3_type 
	0           "Not applicable"                
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f3_kind  f3_kind;
label define f3_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f3ownkid f3ownkid;
label define f3ownkid
	0           "None"                          
;
label values f3oklt18 f3oklt1d;
label define f3oklt1d
	0           "None"                          
;
label values f4_type  f4_type;
label define f4_type 
	0           "Not applicable"                
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f4_kind  f4_kind;
label define f4_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f4ownkid f4ownkid;
label define f4ownkid
	0           "None"                          
;
label values f4oklt18 f4oklt1d;
label define f4oklt1d
	0           "None"                          
;
label values s1_type  s1_type;
label define s1_type 
	0           "Not applicable"                
	2           "Related subfamily"             
;
label values s1_kind  s1_kind;
label define s1_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s1ownkid s1ownkid;
label define s1ownkid
	0           "None"                          
;
label values s1oklt18 s1oklt1d;
label define s1oklt1d
	0           "None"                          
;
label values s2_type  s2_type;
label define s2_type 
	0           "Not applicable"                
	2           "Related subfamily"             
;
label values s2_kind  s2_kind;
label define s2_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s2ownkid s2ownkid;
label define s2ownkid
	0           "None"                          
;
label values s2oklt18 s2oklt1d;
label define s2oklt1d
	0           "None"                          
;
label values s3_type  s3_type;
label define s3_type 
	0           "Not applicable"                
	2           "Related subfamily"             
;
label values s3_kind  s3_kind;
label define s3_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s3ownkid s3ownkid;
label define s3ownkid
	0           "None"                          
;
label values s3oklt18 s3oklt1d;
label define s3oklt1d
	0           "None"                          
;
label values s4_type  s4_type;
label define s4_type 
	0           "Not applicable"                
	2           "Related subfamily"             
;
label values s4_kind  s4_kind;
label define s4_kind 
	0           "Not applicable"                
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s4ownkid s4ownkid;
label define s4ownkid
	0           "None"                          
;
label values s4oklt18 s4oklt1d;
label define s4oklt1d
	0           "None"                          
;
label values pp_intvw pp_intvw;
label define pp_intvw
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - type Z refusal" 
	4           "Noninterview - type Z other"   
;
label values pp_mis_1 pp_mis_w;
label define pp_mis_w
	1           "Interview"                     
	2           "Noninterview"                  
;
label values pp_mis_2 pp_mis_k;
label define pp_mis_k
	1           "Interview"                     
	2           "Noninterview"                  
;
label values pp_mis_3 pp_mis_l;
label define pp_mis_l
	1           "Interview"                     
	2           "Noninterview"                  
;
label values pp_mis_4 pp_mis_m;
label define pp_mis_m
	1           "Interview"                     
	2           "Noninterview"                  
;
label values pp_mis_5 pp_mis_n;
label define pp_mis_n
	1           "Interview"                     
	2           "Noninterview"                  
;
label values pw_rrp   pw_rrp; 
label define pw_rrp  
	0           "Nonrelative of reference person"
	1           "Reference person with relatives"
	2           "Reference person with no"      
	3           "Husband of reference person"   
	4           "Wife of reference person"      
	5           "Own child of reference person" 
	6           "Parent of reference person"    
	7           "Brother/sister of reference"   
	8           "Other relative of reference"   
	9           "Nonrelative of reference person"
;
label values pw_ms    pw_ms;  
label define pw_ms   
	0           "Not applicable"                
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
	9           "Not answered"                  
;
label values pw_pnsp  pw_pnsp;
label define pw_pnsp 
	-09         "Not answered"                  
	0           "Not in universe"               
	999         "Not 'married, spouse present'" 
;
label values pw_pnpt  pw_pnpt;
label define pw_pnpt 
	-09         "Not answered"                  
	0           "Not in universe"               
	999         "None"                          
;
label values pw_popst pw_popst;
label define pw_popst
	0           "Not applicable"                
	1           "Child (under 15 at interview)" 
	2           "Adult (15 years of age or older)"
;
label values pw_addit pw_addit;
label define pw_addit
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values pw_intvw pw_intvw;
label define pw_intvw
	0           "Not applicable or not in"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - type-Z"         
	4           "Noninterview - type-Z other"   
;
label values rrp_1    rrp_1l; 
label define rrp_1l  
	0           "Not a sample person this month"
	1           "Household reference person,"   
	2           "Household reference person"    
	3           "Spouse of household reference" 
	4           "Child of household reference"  
	5           "Other relative of household"   
	6           "Nonrelative of household"      
	7           "Nonrelative of household"      
;
label values rrp_2    rrp_2l; 
label define rrp_2l  
	0           "Not a sample person this month"
	1           "Household reference person,"   
	2           "Household reference person"    
	3           "Spouse of household reference" 
	4           "Child of household reference"  
	5           "Other relative of household"   
	6           "Nonrelative of household"      
	7           "Nonrelative of household"      
;
label values rrp_3    rrp_3l; 
label define rrp_3l  
	0           "Not a sample person this month"
	1           "Household reference person,"   
	2           "Household reference person"    
	3           "Spouse of household reference" 
	4           "Child of household reference"  
	5           "Other relative of household"   
	6           "Nonrelative of household"      
	7           "Nonrelative of household"      
;
label values rrp_4    rrp_4l; 
label define rrp_4l  
	0           "Not a sample person this month"
	1           "Household reference person,"   
	2           "Household reference person"    
	3           "Spouse of household reference" 
	4           "Child of household reference"  
	5           "Other relative of household"   
	6           "Nonrelative of household"      
	7           "Nonrelative of household"      
;
label values rrp_5    rrp_5l; 
label define rrp_5l  
	0           "Not a sample person this month"
	1           "Household reference person,"   
	2           "Household reference person"    
	3           "Spouse of household reference" 
	4           "Child of household reference"  
	5           "Other relative of household"   
	6           "Nonrelative of household"      
	7           "Nonrelative of household"      
;
label values age_1    age_1l; 
label define age_1l  
	0           "Less than 1 full year or not a"
	85          "85 years old or older"         
;
label values age_2    age_2l; 
label define age_2l  
	0           "Less than 1 full year or not a"
	85          "85 years old or older"         
;
label values age_3    age_3l; 
label define age_3l  
	0           "Less than 1 full year or not a"
	85          "85 years old or older"         
;
label values age_4    age_4l; 
label define age_4l  
	0           "Less than 1 full year or not a"
	85          "85 years old or older"         
;
label values age_5    age_5l; 
label define age_5l  
	0           "Less than 1 full year or not a"
	85          "85 years old or older"         
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
label values ms_1     ms_1l;  
label define ms_1l   
	0           "Not a sample person this month"
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
;
label values ms_2     ms_2l;  
label define ms_2l   
	0           "Not a sample person this month"
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
;
label values ms_3     ms_3l;  
label define ms_3l   
	0           "Not a sample person this month"
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
;
label values ms_4     ms_4l;  
label define ms_4l   
	0           "Not a sample person this month"
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
;
label values ms_5     ms_5l;  
label define ms_5l   
	0           "Not a sample person this month"
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
;
label values famtyp_1 famtyp_e;
label define famtyp_e
	0           "Primary family or not in sample"
	1           "Secondary individual (not a"   
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_2 famtyp_k;
label define famtyp_k
	0           "Primary family or not in sample"
	1           "Secondary individual (not a"   
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_3 famtyp_l;
label define famtyp_l
	0           "Primary family or not in sample"
	1           "Secondary individual (not a"   
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_4 famtyp_m;
label define famtyp_m
	0           "Primary family or not in sample"
	1           "Secondary individual (not a"   
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_5 famtyp_n;
label define famtyp_n
	0           "Primary family or not in sample"
	1           "Secondary individual (not a"   
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famrel_1 famrel_e;
label define famrel_e
	0           "Not applicable (person not in" 
	1           "Reference person of family"    
	2           "Spouse of family reference"    
	3           "Child of family reference person"
	4           "Other relative of family"      
;
label values famrel_2 famrel_k;
label define famrel_k
	0           "Not applicable (person not in" 
	1           "Reference person of family"    
	2           "Spouse of family reference"    
	3           "Child of family reference person"
	4           "Other relative of family"      
;
label values famrel_3 famrel_l;
label define famrel_l
	0           "Not applicable (person not in" 
	1           "Reference person of family"    
	2           "Spouse of family reference"    
	3           "Child of family reference person"
	4           "Other relative of family"      
;
label values famrel_4 famrel_m;
label define famrel_m
	0           "Not applicable (person not in" 
	1           "Reference person of family"    
	2           "Spouse of family reference"    
	3           "Child of family reference person"
	4           "Other relative of family"      
;
label values famrel_5 famrel_n;
label define famrel_n
	0           "Not applicable (person not in" 
	1           "Reference person of family"    
	2           "Spouse of family reference"    
	3           "Child of family reference person"
	4           "Other relative of family"      
;
label values famnum_1 famnum_e;
label define famnum_e
	0           "Not applicable (person not in" 
;
label values famnum_2 famnum_k;
label define famnum_k
	0           "Not applicable (person not in" 
;
label values famnum_3 famnum_l;
label define famnum_l
	0           "Not applicable (person not in" 
;
label values famnum_4 famnum_m;
label define famnum_m
	0           "Not applicable (person not in" 
;
label values famnum_5 famnum_n;
label define famnum_n
	0           "Not applicable (person not in" 
;
label values pop_stat pop_stat;
label define pop_stat
	1           "Adult (15 years of age or older)"
	2           "Child (under 15 at interview)" 
;
label values pnsp_1   pnsp_1l;
label define pnsp_1l 
	0           "Not in sample this month"      
	999         "Not 'married, spouse present'" 
;
label values pnsp_2   pnsp_2l;
label define pnsp_2l 
	0           "Not in sample this month"      
	999         "Not 'married, spouse present'" 
;
label values pnsp_3   pnsp_3l;
label define pnsp_3l 
	0           "Not in sample this month"      
	999         "Not 'married, spouse present'" 
;
label values pnsp_4   pnsp_4l;
label define pnsp_4l 
	0           "Not in sample this month"      
	999         "Not 'married, spouse present'" 
;
label values pnsp_5   pnsp_5l;
label define pnsp_5l 
	0           "Not in sample this month"      
	999         "Not 'married, spouse present'" 
;
label values pnpt_1   pnpt_1l;
label define pnpt_1l 
	0           "Not in sample this month"      
	999         "None"                          
;
label values pnpt_2   pnpt_2l;
label define pnpt_2l 
	0           "Not in sample this month"      
	999         "None"                          
;
label values pnpt_3   pnpt_3l;
label define pnpt_3l 
	0           "Not in sample this month"      
	999         "None"                          
;
label values pnpt_4   pnpt_4l;
label define pnpt_4l 
	0           "Not in sample this month"      
	999         "None"                          
;
label values pnpt_5   pnpt_5l;
label define pnpt_5l 
	0           "Not in sample this month"      
	999         "None"                          
;
label values vetstat  vetstat;
label define vetstat 
	0           "Not applicable (under 15) or"  
	1           "Yes"                           
	2           "No"                            
;
label values higrade  higrade;
label define higrade 
	0           "Not applicable if under 15;"   
;
label values grd_cmpl grd_cmpl;
label define grd_cmpl
	0           "Not applicable if under 15."   
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
label values in_af    in_af;  
label define in_af   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u_rrp    u_rrp;  
label define u_rrp   
	0           "Nonrelative of reference person"
	1           "Reference person with relatives"
	2           "Reference person with no rel. In"
	3           "Husband of reference person"   
	4           "Wife of reference person"      
	5           "Own child of reference person" 
	6           "Parent of reference person"    
	7           "Brother/sister of reference"   
	8           "Other relative of reference"   
	9           "Nonrelative of reference person"
;
label values u_hhmem  u_hhmem;
label define u_hhmem 
	1           "Yes - current HH member"       
	2           "No"                            
;
label values u_entlf1 u_entlfm;
label define u_entlfm
	0           "Not applicable or not answered"
	1           "Entered - birth"               
	2           "Entered - marriage"            
	3           "Entered - other"               
	4           "Entered (before sample people)"
	5           "Left - deceased"               
	6           "Left - institutionalized"      
	7           "Left - living in Armed Forces barracks"
	8           "Left - moved outside of country"
	9           "Left - separation or divorce"  
	10          "Left - person #201 no longer"  
	11          "Left - other"                  
	12          "Entered merged household"      
	13          "Reentered household"           
	16          "Entered - from institution"    
	17          "From armed forces barracks"    
	18          "From outside the country"      
	19          "Due to separation or divorce"  
	21          "Entered - birth"               
	22          "Entered - marriage"            
	23          "Entered - other"               
	24          "Entered sample person added in"
	25          "Left - deceased"               
	26          "Left - institutionalized"      
	27          "Left - living in armed forces barracks"
	28          "Left - moved outside of country"
	29          "Left - separation or divorce"  
	30          "Left - 201+ persons not longer"
	31          "Left - other"                  
	99          "Listed in error"               
;
label values u_monlft u_monlft;
label define u_monlft
	0           "Not applicable (person did not move"
;
label values u_monent u_monent;
label define u_monent
	0           "Not applicable (person did not move"
;
label values u_daylft u_daylft;
label define u_daylft
	0           "Not applicable (person did not move"
;
label values u_dayent u_dayent;
label define u_dayent
	0           "Not applicable (person did not move"
;
label values cc_adlft cc_adlft;
label define cc_adlft
	0           "Not applicable (person did not move"
;
label values cc_adent cc_adent;
label define cc_adent
	0           "Not applicable (person did not move"
;
label values u_brthmn u_brthmn;
label define u_brthmn
	-9          "Not answered"                  
;
label values u_brthyr u_brthyr;
label define u_brthyr
	-009        "Not answered"                  
	1900        "Born 1900 or earlier"          
;
label values u_pnpt   u_pnpt; 
label define u_pnpt  
	-09         "Not answered"                  
	0           "Not applicable"                
	9           "None"                          
;
label values u_ms     u_ms;   
label define u_ms    
	0           "Not applicable"                
	1           "Married - spouse present"      
	2           "Married - spouse absent"       
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
	9           "Not answered"                  
;
label values u_pnsp   u_pnsp; 
label define u_pnsp  
	-09         "Not answered"                  
	0           "Not applicable"                
	999         "Not 'married, spouse present'" 
;
label values u_pngd   u_pngd; 
label define u_pngd  
	-09         "Not answered"                  
	0           "Not applicable"                
;
label values u_sex    u_sex;  
label define u_sex   
	1           "Male"                          
	2           "Female"                        
	9           "Not answered"                  
;
label values u_race   u_race; 
label define u_race  
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Eskimo or Aleut"
	4           "Asian or Pacific Islander"     
	5           "Other"                         
	9           "Not answered"                  
;
label values u_origin u_origin;
label define u_origin
	-9          "Not answered"                  
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
label values u_higrde u_higrde;
label define u_higrde
	-9          "Not answered"                  
	0           "Not applicable, did not attend,"
;
label values u_compl  u_compl;
label define u_compl 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	9           "Not answered"                  
;
label values u_vet    u_vet;  
label define u_vet   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	9           "Not answered"                  
;
label values u_srvdte u_srvdte;
label define u_srvdte
	0           "Not applicable"                
	1           "Vietnam era (Aug'64-Apr'75)"   
	2           "Korean conflict (June'50-Jan'55)"
	3           "World war II (Sept'40-July'47)"
	4           "World warI (Apr'17-Nov'18)"    
	5           "May 1975 or later"             
	6           "Other service"                 
	9           "Not answered"                  
;
label values u_af     u_af;   
label define u_af    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	9           "Not answered"                  
;
label values u_ssndk  u_ssndk;
label define u_ssndk 
	0           "Not answered, not applicable"  
	1           "Don't know"                    
	2           "Refusal"                       
	3           "None"                          
;
label values recips01 recipsum;
label values recips02 recipsum;
label values recips03 recipsum;
label values recips04 recipsum;
label values recips05 recipsum;
label values recips06 recipsum;
label values recips07 recipsum;
label values recips08 recipsum;
label values recips09 recipsum;
label values recips10 recipsum;
label values recips11 recipsum;
label values recips12 recipsum;
label values recips13 recipsum;
label values recips14 recipsum;
label values recips15 recipsum;
label values recips16 recipsum;
label values recips17 recipsum;
label values recips18 recipsum;
label values recips19 recipsum;
label values recips20 recipsum;
label values recips21 recipsum;
label values recips22 recipsum;
label values recips23 recipsum;
label values recips24 recipsum;
label values recips25 recipsum;
label values recips26 recipsum;
label values recips27 recipsum;
label values recips28 recipsum;
label values recips29 recipsum;
label values recips30 recipsum;
label values recips31 recipsum;
label values recips32 recipsum;
label values recips33 recipsum;
label values recips34 recipsum;
label values recips35 recipsum;
label values recips36 recipsum;
label values recips37 recipsum;
label values recips38 recipsum;
label values recips39 recipsum;
label values recips40 recipsum;
label values recips41 recipsum;
label values recips42 recipsum;
label values recips43 recipsum;
label values recips44 recipsum;
label values recips45 recipsum;
label values recips46 recipsum;
label values recips47 recipsum;
label values recips48 recipsum;
label values recips49 recipsum;
label values recips50 recipsum;
label values recips51 recipsum;
label values recips52 recipsum;
label values recips53 recipsum;
label values recips54 recipsum;
label values recips55 recipsum;
label values recips56 recipsum;
label values recips57 recipsum;
label values recips58 recipsum;
label values recips59 recipsum;
label values recips60 recipsum;
label values recips61 recipsum;
label values recips62 recipsum;
label values recips63 recipsum;
label values recips64 recipsum;
label values recips65 recipsum;
label values recips66 recipsum;
label values recips67 recipsum;
label values recips68 recipsum;
label values recips69 recipsum;
label values recips70 recipsum;
label values recips71 recipsum;
label values recips72 recipsum;
label values recips73 recipsum;
label values recips74 recipsum;
label values recips75 recipsum;
label define recipsum
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values assets01 assetsum;
label values assets02 assetsum;
label values assets03 assetsum;
label values assets04 assetsum;
label values assets05 assetsum;
label values assets06 assetsum;
label values assets07 assetsum;
label values assets08 assetsum;
label values assets09 assetsum;
label values assets10 assetsum;
label values assets11 assetsum;
label values assets12 assetsum;
label values assets13 assetsum;
label define assetsum
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values esr_1    esr_1l; 
label define esr_1l  
	0           "Not applicable"                
	1           "With a job entire month, worked"
	2           "With a job entire month, missed"
	3           "With a job entire month, missed"
	4           "With job one or more weeks, no"
	5           "With job one or more weeks,"   
	6           "No job during month, spent"    
	7           "No job during month, spent one"
	8           "No job during month, no time"  
;
label values esr_2    esr_2l; 
label define esr_2l  
	0           "Not applicable"                
	1           "With a job entire month, worked"
	2           "With a job entire month, missed"
	3           "With a job entire month, missed"
	4           "With job one or more weeks, no"
	5           "With job one or more weeks,"   
	6           "No job during month, spent"    
	7           "No job during month, spent one"
	8           "No job during month, no time"  
;
label values esr_3    esr_3l; 
label define esr_3l  
	0           "Not applicable"                
	1           "With a job entire month, worked"
	2           "With a job entire month, missed"
	3           "With a job entire month, missed"
	4           "With job one or more weeks, no"
	5           "With job one or more weeks,"   
	6           "No job during month, spent"    
	7           "No job during month, spent one"
	8           "No job during month, no time"  
;
label values esr_4    esr_4l; 
label define esr_4l  
	0           "Not applicable"                
	1           "With a job entire month, worked"
	2           "With a job entire month, missed"
	3           "With a job entire month, missed"
	4           "With job one or more weeks, no"
	5           "With job one or more weeks,"   
	6           "No job during month, spent"    
	7           "No job during month, spent one"
	8           "No job during month, no time"  
;
label values wksper1  wksper1l;
label define wksper1l
	0           "Not applicable"                
	4           "Four weeks"                    
	5           "Five weeks"                    
;
label values wksper2  wksper2l;
label define wksper2l
	0           "Not applicable"                
	4           "Four weeks"                    
	5           "Five weeks"                    
;
label values wksper3  wksper3l;
label define wksper3l
	0           "Not applicable"                
	4           "Four weeks"                    
	5           "Five weeks"                    
;
label values wksper4  wksper4l;
label define wksper4l
	0           "Not applicable"                
	4           "Four weeks"                    
	5           "Five weeks"                    
;
label values wksjb1   wksjb1l;
label define wksjb1l 
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wksjb2   wksjb2l;
label define wksjb2l 
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wksjb3   wksjb3l;
label define wksjb3l 
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wksjb4   wksjb4l;
label define wksjb4l 
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wkwjob01 wkwjob; 
label values wkwjob02 wkwjob; 
label values wkwjob03 wkwjob; 
label values wkwjob04 wkwjob; 
label values wkwjob05 wkwjob; 
label values wkwjob06 wkwjob; 
label values wkwjob07 wkwjob; 
label values wkwjob08 wkwjob; 
label values wkwjob09 wkwjob; 
label values wkwjob10 wkwjob; 
label values wkwjob11 wkwjob; 
label values wkwjob12 wkwjob; 
label values wkwjob13 wkwjob; 
label values wkwjob14 wkwjob; 
label values wkwjob15 wkwjob; 
label values wkwjob16 wkwjob; 
label values wkwjob17 wkwjob; 
label values wkwjob18 wkwjob; 
label define wkwjob  
	0           "Not applicable ('have-job' = no)"
	1           "Yes"                           
	2           "No"                            
;
label values wiswop1  wiswop1l;
label define wiswop1l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wiswop2  wiswop2l;
label define wiswop2l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wiswop3  wiswop3l;
label define wiswop3l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wiswop4  wiswop4l;
label define wiswop4l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values weeksa01 weeksab;
label values weeksa02 weeksab;
label values weeksa03 weeksab;
label values weeksa04 weeksab;
label values weeksa05 weeksab;
label values weeksa06 weeksab;
label values weeksa07 weeksab;
label values weeksa08 weeksab;
label values weeksa09 weeksab;
label values weeksa10 weeksab;
label values weeksa11 weeksab;
label values weeksa12 weeksab;
label values weeksa13 weeksab;
label values weeksa14 weeksab;
label values weeksa15 weeksab;
label values weeksa16 weeksab;
label values weeksa17 weeksab;
label values weeksa18 weeksab;
label define weeksab 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wislok1  wislok1l;
label define wislok1l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wislok2  wislok2l;
label define wislok2l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wislok3  wislok3l;
label define wislok3l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values wislok4  wislok4l;
label define wislok4l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for"  
;
label values weeksl01 weekslk;
label values weeksl02 weekslk;
label values weeksl03 weekslk;
label values weeksl04 weekslk;
label values weeksl05 weekslk;
label values weeksl06 weekslk;
label values weeksl07 weekslk;
label values weeksl08 weekslk;
label values weeksl09 weekslk;
label values weeksl10 weekslk;
label values weeksl11 weekslk;
label values weeksl12 weekslk;
label values weeksl13 weekslk;
label values weeksl14 weekslk;
label values weeksl15 weekslk;
label values weeksl16 weekslk;
label values weeksl17 weekslk;
label values weeksl18 weekslk;
label define weekslk 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values carecov1 carecovk;
label define carecovk
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values carecov2 carecovl;
label define carecovl
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values carecov3 carecovm;
label define carecovm
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values carecov4 carecovn;
label define carecovn
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values medicaid medicaid;
label define medicaid
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values caidcov1 caidcovd;
label define caidcovd
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values caidcov2 caidcovk;
label define caidcovk
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values caidcov3 caidcovl;
label define caidcovl
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values caidcov4 caidcovm;
label define caidcovm
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wiccov1  wiccov1l;
label define wiccov1l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wiccov2  wiccov2l;
label define wiccov2l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wiccov3  wiccov3l;
label define wiccov3l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wiccov4  wiccov4l;
label define wiccov4l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wicval1  wicval1l;
label define wicval1l
	0           "Not applicable"                
;
label values wicval2  wicval2l;
label define wicval2l
	0           "Not applicable"                
;
label values wicval3  wicval3l;
label define wicval3l
	0           "Not applicable"                
;
label values wicval4  wicval4l;
label define wicval4l
	0           "Not applicable"                
;
label values hiind    hiind;  
label define hiind   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values himnth1  himnth1l;
label define himnth1l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values himnth2  himnth2l;
label define himnth2l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values himnth3  himnth3l;
label define himnth3l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values himnth4  himnth4l;
label define himnth4l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values incsou01 incsourc;
label values incsou02 incsourc;
label values incsou03 incsourc;
label values incsou04 incsourc;
label values incsou05 incsourc;
label values incsou06 incsourc;
label values incsou07 incsourc;
label values incsou08 incsourc;
label values incsou09 incsourc;
label values incsou10 incsourc;
label define incsourc
	0           "Not applicable"                
	1           "Social security"               
	2           "Railroad retirement"           
	3           "Federal supplemental security" 
	5           "State unemployment compensation"
	6           "Supplemental unemployment"     
	7           "Other unemployment compensation"
	8           "Veterans compensation or"      
	10          "Workers compensation"          
	12          "Employer or union temporary"   
	13          "Payments from a sickness,"     
	20          "Aid to families with dependent"
	21          "General assistance or general" 
	23          "Foster child care payments"    
	24          "Other welfare"                 
	25          "WIC"                           
	27          "Food stamps"                   
	28          "Child support payments"        
	29          "Alimony payments"              
	30          "Pension from company or union" 
	31          "Federal civil service or other"
	32          "U.S. military retirement pay"  
	34          "State government pensions"     
	35          "Local government pensions"     
	36          "Income from paid up life"      
	37          "Estates and trusts"            
	38          "Other payments for retirement,"
	40          "GI bill education benefits"    
	50          "Income assistance from a"      
	51          "Money from relatives or friends"
	52          "Lump sum payments"             
	53          "Income from roomers or boarders"
	54          "National guard or reserve pay" 
	55          "Incidental or casual earnings" 
	56          "Other cash income not included"
	75          "Recoded for confidentiality (ISS"
;
label values astsou01 astsourc;
label values astsou02 astsourc;
label values astsou03 astsourc;
label values astsou04 astsourc;
label values astsou05 astsourc;
label values astsou06 astsourc;
label values astsou07 astsourc;
label values astsou08 astsourc;
label values astsou09 astsourc;
label values astsou10 astsourc;
label values astsou11 astsourc;
label values astsou12 astsourc;
label values astsou13 astsourc;
label define astsourc
	0           "Not applicable"                
	100         "Regular/passbook savings"      
	101         "Money market deposit accounts" 
	102         "Certificates of deposit or other"
	103         "NOW, super NOW, or other"      
	104         "Money market funds"            
	105         "U.S. government securities"    
	106         "Municipal or corporate bonds"  
	107         "Other interest earning assets" 
	110         "Stocks or mutual fund shares"  
	120         "Rental property"               
	130         "Mortgages"                     
	140         "Royalties"                     
	150         "Other financial investments"   
;
label values vets1    vets1l; 
label define vets1l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values vets2    vets2l; 
label define vets2l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values vets3    vets3l; 
label define vets3l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values vets4    vets4l; 
label define vets4l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values afdc1    afdc1l; 
label define afdc1l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values afdc2    afdc2l; 
label define afdc2l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values afdc3    afdc3l; 
label define afdc3l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values afdc4    afdc4l; 
label define afdc4l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values foodstp1 foodstpc;
label define foodstpc
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values foodstp2 foodstpk;
label define foodstpk
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values foodstp3 foodstpl;
label define foodstpl
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values foodstp4 foodstpm;
label define foodstpm
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values genasst1 genasstc;
label define genasstc
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values genasst2 genasstk;
label define genasstk
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values genasst3 genasstl;
label define genasstl
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values genasst4 genasstm;
label define genasstm
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values fostkid1 fostkidc;
label define fostkidc
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values fostkid2 fostkidk;
label define fostkidk
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values fostkid3 fostkidl;
label define fostkidl
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values fostkid4 fostkidm;
label define fostkidm
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values othwelf1 othwelfc;
label define othwelfc
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values othwelf2 othwelfk;
label define othwelfk
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values othwelf3 othwelfl;
label define othwelfl
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values othwelf4 othwelfm;
label define othwelfm
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values socsec1  socsec1l;
label define socsec1l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values socsec2  socsec2l;
label define socsec2l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values socsec3  socsec3l;
label define socsec3l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values socsec4  socsec4l;
label define socsec4l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values railrd1  railrd1l;
label define railrd1l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values railrd2  railrd2l;
label define railrd2l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values railrd3  railrd3l;
label define railrd3l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values railrd4  railrd4l;
label define railrd4l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values it7a     it7a;   
label define it7a    
	0           "Not answered"                  
	1           "Self"                          
	2           "Proxy"                         
;
label values it7c     it7c;   
label define it7c    
	0           "Not answered"                  
	1           "Type Z refusal"                
	2           "Type Z other"                  
;
label values sc0900   sc0900l;
label define sc0900l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1000"           
;
label values sc0901   sc0901l;
label define sc0901l 
	0           "Not in universe"               
	1           "Yes - skip to SC1000"          
	2           "No"                            
;
label values sc1000   sc1000l;
label define sc1000l 
	0           "Not in universe"               
	1           "Yes - skip to SC1056"          
	2           "No"                            
;
label values sc1002   sc1002l;
label define sc1002l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1048"           
;
label values sc1004   sc1004l;
label define sc1004l 
	0           "Not marked as looking or on"   
	1           "Marked as looking or on layoff"
;
label values sc1006   sc1006l;
label define sc1006l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1008   sc1008l;
label define sc1008l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1010   sc1010l;
label define sc1010l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1012   sc1012l;
label define sc1012l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1014   sc1014l;
label define sc1014l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1016   sc1016l;
label define sc1016l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1018   sc1018l;
label define sc1018l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1020   sc1020l;
label define sc1020l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1022   sc1022l;
label define sc1022l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1024   sc1024l;
label define sc1024l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1026   sc1026l;
label define sc1026l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1028   sc1028l;
label define sc1028l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1030   sc1030l;
label define sc1030l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1032   sc1032l;
label define sc1032l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1034   sc1034l;
label define sc1034l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1036   sc1036l;
label define sc1036l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1038   sc1038l;
label define sc1038l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1040   sc1040l;
label define sc1040l 
	0           "Not marked as a week looking or"
	1           "Marked as a week looking or on"
;
label values sc1042   sc1042l;
label define sc1042l 
	0           "Not in universe"               
	1           "Yes - skip to SC1046"          
	2           "No"                            
;
label values sc1044   sc1044l;
label define sc1044l 
	0           "Not in universe"               
	1           "Already had a job"             
	2           "Temporary illness"             
	3           "School"                        
	4           "Other"                         
;
label values sc1046   sc1046l;
label define sc1046l 
	0           "Not in universe"               
	1           "Yes - skip to SC1240"          
	2           "No - skip to SC1050"           
;
label values sc1048   sc1048l;
label define sc1048l 
	0           "Not in universe"               
	1           "Yes - skip to SC1052"          
	2           "No - skip to SC1248"           
;
label values sc1050   sc1050l;
label define sc1050l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1240"           
;
label values sc1052   sc1052l;
label define sc1052l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1240"           
;
label values sc1054   sc1054l;
label define sc1054l 
	-1          "DK - skip to SC1240"           
	0           "Not in universe"               
	1           "Believes no work available in" 
	2           "Couldn't find any work - skip to"
	3           "Lacks necessary schooling,"    
	4           "Employers think too young or too"
	5           "Other personal handicap in"    
	6           "Can't arrange child care - skip"
	7           "Family responsibilities - skip"
	8           "In school or other training -" 
	9           "Ill health, physical disability"
	10          "Other - skip to SC1240"        
;
label values sc1056   sc1056l;
label define sc1056l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1100 through"   
;
label values sc1058   sc1058l;
label define sc1058l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1230"           
;
label values sc1060   sc1060l;
label define sc1060l 
	0           "Not marked as absent without pay"
	1           "Marked as a week absent without"
;
label values sc1062   sc1062l;
label define sc1062l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1064   sc1064l;
label define sc1064l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1066   sc1066l;
label define sc1066l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1068   sc1068l;
label define sc1068l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1070   sc1070l;
label define sc1070l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1072   sc1072l;
label define sc1072l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1074   sc1074l;
label define sc1074l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1076   sc1076l;
label define sc1076l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1078   sc1078l;
label define sc1078l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1080   sc1080l;
label define sc1080l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1082   sc1082l;
label define sc1082l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1084   sc1084l;
label define sc1084l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1086   sc1086l;
label define sc1086l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1088   sc1088l;
label define sc1088l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1090   sc1090l;
label define sc1090l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1092   sc1092l;
label define sc1092l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1094   sc1094l;
label define sc1094l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1096   sc1096l;
label define sc1096l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1098   sc1098l;
label define sc1098l 
	0           "Not in universe"               
	1           "On layoff - skip to SC1230"    
	2           "Own illness - skip to SC1230"  
	3           "On vacation - skip to SC1230"  
	4           "Bad weather - skip to SC1230"  
	5           "Labor dispute - skip to SC1230"
	6           "New job to begin within 30 days"
	7           "Other - skip to SC1230"        
;
label values sc1100   sc1100l;
label define sc1100l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1102   sc1102l;
label define sc1102l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1104   sc1104l;
label define sc1104l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1106   sc1106l;
label define sc1106l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1108   sc1108l;
label define sc1108l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1110   sc1110l;
label define sc1110l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1112   sc1112l;
label define sc1112l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1114   sc1114l;
label define sc1114l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1116   sc1116l;
label define sc1116l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1118   sc1118l;
label define sc1118l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1120   sc1120l;
label define sc1120l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1122   sc1122l;
label define sc1122l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1124   sc1124l;
label define sc1124l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1126   sc1126l;
label define sc1126l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1128   sc1128l;
label define sc1128l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1130   sc1130l;
label define sc1130l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1132   sc1132l;
label define sc1132l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1134   sc1134l;
label define sc1134l 
	0           "Not marked as a week having a" 
	1           "Marked as a week having a job" 
;
label values sc1136   sc1136l;
label define sc1136l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1176"           
;
label values sc1138   sc1138l;
label define sc1138l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1140   sc1140l;
label define sc1140l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1142   sc1142l;
label define sc1142l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1144   sc1144l;
label define sc1144l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1146   sc1146l;
label define sc1146l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1148   sc1148l;
label define sc1148l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1150   sc1150l;
label define sc1150l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1152   sc1152l;
label define sc1152l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1154   sc1154l;
label define sc1154l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1156   sc1156l;
label define sc1156l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1158   sc1158l;
label define sc1158l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1160   sc1160l;
label define sc1160l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1162   sc1162l;
label define sc1162l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1164   sc1164l;
label define sc1164l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1166   sc1166l;
label define sc1166l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1168   sc1168l;
label define sc1168l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1170   sc1170l;
label define sc1170l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1172   sc1172l;
label define sc1172l 
	0           "Not marked as a week absent"   
	1           "Marked as a week absent without"
;
label values sc1174   sc1174l;
label define sc1174l 
	0           "Not in universe"               
	1           "On layoff"                     
	2           "Own illness"                   
	3           "On vacation"                   
	4           "Bad weather"                   
	5           "Labor dispute"                 
	6           "New job to begin within 30 days"
	7           "Other"                         
;
label values sc1176   sc1176l;
label define sc1176l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1222"           
;
label values sc1178   sc1178l;
label define sc1178l 
	0           "Not marked as looking for work"
	1           "Marked as looking for work or on"
;
label values sc1180   sc1180l;
label define sc1180l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1182   sc1182l;
label define sc1182l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1184   sc1184l;
label define sc1184l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1186   sc1186l;
label define sc1186l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1188   sc1188l;
label define sc1188l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1190   sc1190l;
label define sc1190l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1192   sc1192l;
label define sc1192l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1194   sc1194l;
label define sc1194l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1196   sc1196l;
label define sc1196l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1198   sc1198l;
label define sc1198l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1200   sc1200l;
label define sc1200l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1202   sc1202l;
label define sc1202l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1204   sc1204l;
label define sc1204l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1206   sc1206l;
label define sc1206l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1208   sc1208l;
label define sc1208l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1210   sc1210l;
label define sc1210l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1212   sc1212l;
label define sc1212l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1214   sc1214l;
label define sc1214l 
	0           "Not marked as a week looking for"
	1           "Marked as a week looking for"  
;
label values sc1216   sc1216l;
label define sc1216l 
	0           "Not in universe"               
	1           "Yes - skip to SC1220"          
	2           "No"                            
;
label values sc1218   sc1218l;
label define sc1218l 
	0           "Not in universe"               
	1           "Already had a job"             
	2           "Temporary illness"             
	3           "School"                        
	4           "Other"                         
;
label values sc1220   sc1220l;
label define sc1220l 
	0           "Not in universe"               
	1           "Yes - skip to SC1230"          
	2           "No - skip to SC1224"           
;
label values sc1222   sc1222l;
label define sc1222l 
	0           "Not in universe"               
	1           "Yes - skip to SC1226"          
	2           "No - skip to SC1230"           
;
label values sc1224   sc1224l;
label define sc1224l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1230"           
;
label values sc1226   sc1226l;
label define sc1226l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1230"           
;
label values sc1228   sc1228l;
label define sc1228l 
	0           "Not in universe"               
	1           "Believes no work available in" 
	2           "Couldn't find any work"        
	3           "Lacks necessary schooling,"    
	4           "Employers think too young or"  
	5           "Other personal handicap in"    
	6           "Can't arrange child care"      
	7           "Family responsibilities"       
	8           "In school or other training"   
	9           "Ill health, physical disability"
	10          "Other"                         
;
label values sc1230   sc1230l;
label define sc1230l 
	-9          "Not in universe"               
	0           "None - skip to SC1239"         
;
label values sc1239   sc1239l;
label define sc1239l 
	0           "Not in universe"               
	1           "Yes (or blank)"                
	2           "No - skip to SC1244"           
;
label values sc1240   sc1240l;
label define sc1240l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1244"           
;
label values sc1242   sc1242l;
label define sc1242l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1244   sc1244l;
label define sc1244l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No skip to SC1248"             
;
label values sc1246   sc1246l;
label define sc1246l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1248   sc1248l;
label define sc1248l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1330"           
;
label values sc1250   sc1250l;
label define sc1250l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1284"           
;
label values sc1251   sc1251l;
label define sc1251l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1252   sc1252l;
label define sc1252l 
	0           "Not in universe"               
;
label values sc1254   sc1254l;
label define sc1254l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1255   sc1255l;
label define sc1255l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1256   sc1256l;
label define sc1256l 
	0           "Not in universe"               
;
label values sc1258   sc1258l;
label define sc1258l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1259   sc1259l;
label define sc1259l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1260   sc1260l;
label define sc1260l 
	0           "Not in universe"               
;
label values sc1262   sc1262l;
label define sc1262l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1263   sc1263l;
label define sc1263l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed, should have been"
;
label values sc1264   sc1264l;
label define sc1264l 
	0           "Not in universe"               
;
label values sc1266   sc1266l;
label define sc1266l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1267   sc1267l;
label define sc1267l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1268   sc1268l;
label define sc1268l 
	0           "Not in universe"               
;
label values sc1270   sc1270l;
label define sc1270l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1271   sc1271l;
label define sc1271l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1272   sc1272l;
label define sc1272l 
	0           "Not in universe"               
;
label values sc1274   sc1274l;
label define sc1274l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1275   sc1275l;
label define sc1275l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1276   sc1276l;
label define sc1276l 
	0           "Not in universe"               
;
label values sc1278   sc1278l;
label define sc1278l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1279   sc1279l;
label define sc1279l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1280   sc1280l;
label define sc1280l 
	0           "Not in universe"               
;
label values sc1282   sc1282l;
label define sc1282l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1283   sc1283l;
label define sc1283l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1284   sc1284l;
label define sc1284l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1296"           
;
label values sc1286   sc1286l;
label define sc1286l 
	0           "Not marked as a kind of income"
	1           "Marked as kind of income"      
;
label values sc1288   sc1288l;
label define sc1288l 
	0           "Not marked as a kind of income"
	1           "Marked as kind of income"      
;
label values sc1290   sc1290l;
label define sc1290l 
	0           "Not marked as a kind of income"
	1           "Marked as kind of income"      
;
label values sc1292   sc1292l;
label define sc1292l 
	0           "Not marked as a kind of income"
	1           "Marked as kind of income"      
;
label values sc1294   sc1294l;
label define sc1294l 
	0           "Not in universe"               
;
label values sc1296   sc1296l;
label define sc1296l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1324"           
;
label values sc1298   sc1298l;
label define sc1298l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1302   sc1302l;
label define sc1302l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1304   sc1304l;
label define sc1304l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1306   sc1306l;
label define sc1306l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1308   sc1308l;
label define sc1308l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1310   sc1310l;
label define sc1310l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1314   sc1314l;
label define sc1314l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1316   sc1316l;
label define sc1316l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1318   sc1318l;
label define sc1318l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1320   sc1320l;
label define sc1320l 
	0           "Not marked as a source of income"
	1           "Marked as a source of income"  
;
label values sc1322   sc1322l;
label define sc1322l 
	0           "Not in universe"               
;
label values sc1324   sc1324l;
label define sc1324l 
	0           "Not in universe"               
	1           "Yes - skip to SC1474"          
	2           "No"                            
;
label values sc1326   sc1326l;
label define sc1326l 
	0           "Not in universe"               
	1           "Yes - skip to SC1462"          
	2           "No"                            
;
label values sc1328   sc1328l;
label define sc1328l 
	0           "Not in universe"               
	1           "Yes - skip to SC1462"          
	2           "No - skip to SC1474"           
;
label values sc1330   sc1330l;
label define sc1330l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1340"           
;
label values sc1332   sc1332l;
label define sc1332l 
	-1          "DK"                            
	0           "Not in universe"               
	1           "Less than 6 months"            
	2           "6 to 23 months"                
	3           "2 to 19 years"                 
	4           "20 or more years"              
;
label values sc1334   sc1334l;
label define sc1334l 
	-1          "DK"                            
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1338"           
;
label values sc1336   sc1336l;
label define sc1336l 
	0           "Not in universe"               
	1           "1-10 percent"                  
	2           "11-29 percent"                 
	3           "30-49 percent"                 
	4           "50 percent"                    
	5           "51-89 percent"                 
	6           "90-99 percent"                 
	7           "100 percent"                   
;
label values sc1338   sc1338l;
label define sc1338l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1340   sc1340l;
label define sc1340l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1386"           
;
label values sc1342   sc1342l;
label define sc1342l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1350"           
;
label values sc1344   sc1344l;
label define sc1344l 
	0           "Not in universe"               
	1           "Yes - skip to SC1354"          
	2           "No"                            
;
label values sc1346   sc1346l;
label define sc1346l 
	-1          "DK - skip to SC1354"           
	0           "Not in universe"               
	1           "Retired"                       
	2           "Disabled"                      
	3           "Widow(ed) or surviving child"  
	4           "Spouse or dependent child"     
	5           "Some other reason - skip to"   
;
label values sc1348   sc1348l;
label define sc1348l 
	-1          "DK - skip to SC1354"           
	0           "Not in universe"               
	1           "Retired - skip to SC1354"      
	2           "Disabled - skip to SC1354"     
	3           "Widow(ed) or surviving child -"
	4           "Spouse or dependent child -"   
	5           "No other reason - skip to SC1354"
;
label values sc1350   sc1350l;
label define sc1350l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1354"           
;
label values sc1352   sc1352l;
label define sc1352l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1354   sc1354l;
label define sc1354l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1358"           
;
label values sc1358   sc1358l;
label define sc1358l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1386"           
;
label values sc1360   sc1360l;
label define sc1360l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1384"           
;
label values sc1362   sc1362l;
label define sc1362l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1382"           
;
label values sc1364   sc1364l;
label define sc1364l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of retirement"
;
label values sc1366   sc1366l;
label define sc1366l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of retirement"
;
label values sc1368   sc1368l;
label define sc1368l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of retirement"
;
label values sc1370   sc1370l;
label define sc1370l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of retirement"
;
label values sc1374   sc1374l;
label define sc1374l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of retirement"
;
label values sc1376   sc1376l;
label define sc1376l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of retirement"
;
label values sc1378   sc1378l;
label define sc1378l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of retirement"
;
label values sc1380   sc1380l;
label define sc1380l 
	0           "Not in universe based on"      
;
label values sc1382   sc1382l;
label define sc1382l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1384   sc1384l;
label define sc1384l 
	0           "Not in universe"               
	1           "Yes - skip to SC1414"          
	2           "No"                            
;
label values sc1386   sc1386l;
label define sc1386l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1414"           
;
label values sc1388   sc1388l;
label define sc1388l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1414"           
;
label values sc1390   sc1390l;
label define sc1390l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1394   sc1394l;
label define sc1394l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1396   sc1396l;
label define sc1396l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1398   sc1398l;
label define sc1398l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1400   sc1400l;
label define sc1400l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1402   sc1402l;
label define sc1402l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1406   sc1406l;
label define sc1406l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1408   sc1408l;
label define sc1408l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1410   sc1410l;
label define sc1410l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1412   sc1412l;
label define sc1412l 
	0           "Not in universe based on"      
;
label values sc1414   sc1414l;
label define sc1414l 
	0           "Not in universe"               
	1           "Married - skip to SC1418"      
	2           "Widowed - skip to SC1426"      
	3           "Divorced"                      
	4           "Separated"                     
	5           "Never married - skip to SC1420"
;
label values sc1416   sc1416l;
label define sc1416l 
	0           "Not in universe"               
	1           "Yes - skip to SC1420"          
	2           "No - skip to SC1420"           
;
label values sc1418   sc1418l;
label define sc1418l 
	0           "Not in universe"               
	1           "Widowed - skip to SC1426"      
	2           "Divorced"                      
	3           "Both widowed and divorced"     
	4           "No - skip to SC1458"           
;
label values sc1420   sc1420l;
label define sc1420l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1424"           
;
label values sc1422   sc1422l;
label define sc1422l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1424   sc1424l;
label define sc1424l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1458"           
;
label values sc1426   sc1426l;
label define sc1426l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1458"           
;
label values sc1428   sc1428l;
label define sc1428l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1430   sc1430l;
label define sc1430l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1434   sc1434l;
label define sc1434l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1436   sc1436l;
label define sc1436l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1438   sc1438l;
label define sc1438l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1442   sc1442l;
label define sc1442l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1444   sc1444l;
label define sc1444l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1446   sc1446l;
label define sc1446l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1448   sc1448l;
label define sc1448l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1450   sc1450l;
label define sc1450l 
	0           "Not marked as a kind of income"
	1           "Marked as a kind of income"    
;
label values sc1452   sc1452l;
label define sc1452l 
	0           "Not in universe based on"      
;
label values sc1454   sc1454l;
label define sc1454l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1458"           
;
label values sc1456   sc1456l;
label define sc1456l 
	0           "Not in universe"               
	1           "Yes, in the service"           
	2           "Yes, from service-related injury"
	3           "No"                            
;
label values sc1458   sc1458l;
label define sc1458l 
	0           "Not in universe"               
	1           "Yes - skip to SC1462"          
	2           "No"                            
;
label values sc1460   sc1460l;
label define sc1460l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1474"           
;
label values sc1462   sc1462l;
label define sc1462l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1474"           
;
label values sc1468   sc1468l;
label define sc1468l 
	0           "Not in universe"               
	1           "Hospital only (type A) - skip" 
	2           "Medical only (type B) - skip to"
	3           "Both hospital and medical (type"
	4           "Card not available"            
;
label values sc1470   sc1470l;
label define sc1470l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1472   sc1472l;
label define sc1472l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1474   sc1474l;
label define sc1474l 
	0           "Not in universe"               
	1           "Yes - skip to SC1478"          
	2           "No"                            
;
label values sc1476   sc1476l;
label define sc1476l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1536"           
;
label values sc1478   sc1478l;
label define sc1478l 
	0           "Not in universe"               
	1           "Yes - skip to 1482"            
	2           "No"                            
;
label values sc1480   sc1480l;
label define sc1480l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1482   sc1482l;
label define sc1482l 
	0           "Not in universe"               
	1           "No spouse in household"        
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values sc1484   sc1484l;
label define sc1484l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1500"           
;
label values sc1486   sc1486l;
label define sc1486l 
	0           "Not marked as a kind of welfare"
	1           "Marked as a kind of welfare"   
;
label values sc1488   sc1488l;
label define sc1488l 
	0           "Not marked as a kind of welfare"
	1           "Marked as a kind of welfare"   
;
label values sc1492   sc1492l;
label define sc1492l 
	0           "Not marked as a kind of welfare"
	1           "Marked as a kind of welfare"   
;
label values sc1494   sc1494l;
label define sc1494l 
	0           "Not marked as a kind of welfare"
	1           "Marked as a kind of welfare"   
;
label values sc1496   sc1496l;
label define sc1496l 
	0           "Not marked as a kind of welfare"
	1           "Marked as a kind of welfare"   
;
label values sc1498   sc1498l;
label define sc1498l 
	0           "Not in universe"               
;
label values sc1500   sc1500l;
label define sc1500l 
	0           "Not in universe"               
	1           "Yes - skip to SC1504"          
	2           "No"                            
;
label values sc1502   sc1502l;
label define sc1502l 
	0           "Not in universe"               
	1           "Yes - skip to SC1506"          
	2           "No - skip to SC1506"           
;
label values sc1504   sc1504l;
label define sc1504l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1506   sc1506l;
label define sc1506l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1524"           
;
label values sc1508   sc1508l;
label define sc1508l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1524"           
;
label values sc1510   sc1510l;
label define sc1510l 
	-5          "All children (skip to SC1524)" 
	0           "Not applicable (SC1508 = 2)"   
	1           "1 child"                       
	2           "2 children"                    
	3           "3 children"                    
	4           "4 children"                    
	5           "5 or more children but not all"
;
label values sc1512   sc1512l;
label define sc1512l 
	0           "Not in universe"               
;
label values sc1514   sc1514l;
label define sc1514l 
	0           "Not in universe"               
;
label values sc1516   sc1516l;
label define sc1516l 
	0           "Not in universe"               
;
label values sc1518   sc1518l;
label define sc1518l 
	0           "Not in universe"               
;
label values sc1520   sc1520l;
label define sc1520l 
	0           "Not in universe"               
;
label values sc1524   sc1524l;
label define sc1524l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1536"           
;
label values sc1526   sc1526l;
label define sc1526l 
	0           "Not in universe"               
	1           "Yes - ship to SC1536"          
	2           "No"                            
;
label values sc1528   sc1528l;
label define sc1528l 
	0           "Not marked as a month covered or"
	1           "Marked as a month covered"     
;
label values sc1530   sc1530l;
label define sc1530l 
	0           "Not marked as a month covered or"
	1           "Marked as a month covered"     
;
label values sc1532   sc1532l;
label define sc1532l 
	0           "Not marked as a month covered or"
	1           "Marked as a month covered"     
;
label values sc1534   sc1534l;
label define sc1534l 
	0           "Not marked as a month covered or"
	1           "Marked as a month covered"     
;
label values sc1536   sc1536l;
label define sc1536l 
	0           "Not in universe"               
	1           "Yes - skip to SC1538"          
	2           "No"                            
;
label values sc1537   sc1537l;
label define sc1537l 
	0           "Not in universe"               
	1           "Yes - skip to SC1568"          
	2           "No - skip to SC1568"           
;
label values sc1538   sc1538l;
label define sc1538l 
	0           "Not in universe"               
	1           "Yes - skip to SC1548"          
	2           "No"                            
;
label values sc1540   sc1540l;
label define sc1540l 
	0           "Not marked as a month with the"
	1           "Marked as a month with the plan"
;
label values sc1542   sc1542l;
label define sc1542l 
	0           "Not marked as a month with the"
	1           "Marked as a month with the plan"
;
label values sc1544   sc1544l;
label define sc1544l 
	0           "Not marked as a month with the"
	1           "Marked as a month with the plan"
;
label values sc1546   sc1546l;
label define sc1546l 
	0           "Not marked as a month with the"
	1           "Marked as a month with the plan"
;
label values sc1548   sc1548l;
label define sc1548l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1552"           
;
label values sc1550   sc1550l;
label define sc1550l 
	0           "Not in universe"               
	1           "All"                           
	2           "Part"                          
	3           "None"                          
;
label values sc1552   sc1552l;
label define sc1552l 
	0           "Not in universe"               
	1           "Individual - skip to SC1568"   
	2           "Family"                        
;
label values sc1554   sc1554l;
label define sc1554l 
	0           "Not in universe"               
	1           "Yes - skip to SC1588"          
	2           "No"                            
;
label values sc1556   sc1556l;
label define sc1556l 
	0           "Not in universe"               
;
label values sc1558   sc1558l;
label define sc1558l 
	0           "Not in universe"               
;
label values sc1560   sc1560l;
label define sc1560l 
	0           "Not in universe"               
;
label values sc1562   sc1562l;
label define sc1562l 
	0           "Not in universe"               
;
label values sc1564   sc1564l;
label define sc1564l 
	0           "Not in universe"               
;
label values sc1566   sc1566l;
label define sc1566l 
	0           "Not in universe, some others"  
	1           "No one else covered"           
;
label values sc1568   sc1568l;
label define sc1568l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1588"           
;
label values sc1570   sc1570l;
label define sc1570l 
	-1          "DK - skip to SC1574 through"   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1574 through"   
;
label values sc1572   sc1572l;
label define sc1572l 
	0           "Not in universe"               
	1           "Yes - skip to SC1588"          
	2           "No"                            
;
label values sc1574   sc1574l;
label define sc1574l 
	-5          "All children - skip to SC1588" 
	0           "Not applicable"                
	1           "1 child"                       
	2           "2 children"                    
	3           "3 children"                    
	4           "4 children"                    
	5           "5 or more children but not all"
;
label values sc1576   sc1576l;
label define sc1576l 
	0           "Not in universe"               
;
label values sc1578   sc1578l;
label define sc1578l 
	0           "Not in universe"               
;
label values sc1580   sc1580l;
label define sc1580l 
	0           "Not in universe"               
;
label values sc1582   sc1582l;
label define sc1582l 
	0           "Not in universe"               
;
label values sc1584   sc1584l;
label define sc1584l 
	0           "Not in universe"               
;
label values sc1586   sc1586l;
label define sc1586l 
	0           "Not in universe, some children"
	1           "No children covered"           
;
label values sc1588   sc1588l;
label define sc1588l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1622"           
;
label values sc1589   sc1589l;
label define sc1589l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1590   sc1590l;
label define sc1590l 
	0           "Not in universe"               
;
label values sc1592   sc1592l;
label define sc1592l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1593   sc1593l;
label define sc1593l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1594   sc1594l;
label define sc1594l 
	0           "Not in universe"               
;
label values sc1596   sc1596l;
label define sc1596l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1597   sc1597l;
label define sc1597l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1598   sc1598l;
label define sc1598l 
	0           "Not in universe"               
;
label values sc1600   sc1600l;
label define sc1600l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1601   sc1601l;
label define sc1601l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1602   sc1602l;
label define sc1602l 
	0           "Not in universe"               
;
label values sc1604   sc1604l;
label define sc1604l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1605   sc1605l;
label define sc1605l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1606   sc1606l;
label define sc1606l 
	0           "Not in universe"               
;
label values sc1608   sc1608l;
label define sc1608l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1609   sc1609l;
label define sc1609l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1610   sc1610l;
label define sc1610l 
	0           "Not in universe"               
;
label values sc1612   sc1612l;
label define sc1612l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1613   sc1613l;
label define sc1613l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1614   sc1614l;
label define sc1614l 
	0           "Not in universe"               
;
label values sc1616   sc1616l;
label define sc1616l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1617   sc1617l;
label define sc1617l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1618   sc1618l;
label define sc1618l 
	0           "Not in universe"               
;
label values sc1620   sc1620l;
label define sc1620l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1621   sc1621l;
label define sc1621l 
	0           "Not in universe"               
	1           "Should not have been listed"   
	2           "Was not listed; should have been"
;
label values sc1622   sc1622l;
label define sc1622l 
	-1          "DK - skip to SC1656"           
	-2          "REF. - skip to SC1656"         
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1656"           
;
label values sc1626   sc1626l;
label define sc1626l 
	0           "Not marked as a kind of asset" 
	1           "Marked as a kind of asset"     
;
label values sc1628   sc1628l;
label define sc1628l 
	0           "Not marked as a kind of asset" 
	1           "Marked as a kind of asset"     
;
label values sc1630   sc1630l;
label define sc1630l 
	0           "Not marked as a kind of asset" 
	1           "Marked as a kind of asset"     
;
label values sc1632   sc1632l;
label define sc1632l 
	0           "Not marked as a kind of asset" 
	1           "Marked as a kind of asset"     
;
label values sc1636   sc1636l;
label define sc1636l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1638   sc1638l;
label define sc1638l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1640   sc1640l;
label define sc1640l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1642   sc1642l;
label define sc1642l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1644   sc1644l;
label define sc1644l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1646   sc1646l;
label define sc1646l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1648   sc1648l;
label define sc1648l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1650   sc1650l;
label define sc1650l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1652   sc1652l;
label define sc1652l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1654   sc1654l;
label define sc1654l 
	0           "Not marked as a kind of asset or"
	1           "Marked as a kind of asset"     
;
label values sc1694   sc1694l;
label define sc1694l 
	0           "Not in universe"               
	1           "Married, spouse absent"        
	2           "Other - skip to SC1698"        
;
label values sc1696   sc1696l;
label define sc1696l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1698   sc1698l;
label define sc1698l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1704"           
;
label values sc1700   sc1700l;
label define sc1700l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1702   sc1702l;
label define sc1702l 
	0           "Not in universe"               
	1           "Yes - skip to SC1706 through"  
	2           "No - skip to SC1712"           
;
label values sc1704   sc1704l;
label define sc1704l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc1706   sc1706l;
label define sc1706l 
	0           "Not in universe"               
;
label values sc1708   sc1708l;
label define sc1708l 
	0           "Not in universe"               
;
label values sc1710   sc1710l;
label define sc1710l 
	0           "Not in universe"               
;
label values sc1712   sc1712l;
label define sc1712l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to first ISS code"   
;
label values sc1714   sc1714l;
label define sc1714l 
	0           "Not in universe"               
	1           "Worked for employer only"      
	2           "Self-employed only"            
	3           "Both worked for employer and"  
;
label values sc1716   sc1716l;
label define sc1716l 
	0           "Not in universe"               
	1           "1 employer"                    
	2           "2 employers"                   
	3           "3 or more employers"           
;
label values pi01     pi01l;  
label define pi01l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi02     pi02l;  
label define pi02l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi03     pi03l;  
label define pi03l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi04     pi04l;  
label define pi04l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi05     pi05l;  
label define pi05l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi06     pi06l;  
label define pi06l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi07     pi07l;  
label define pi07l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi08     pi08l;  
label define pi08l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi09     pi09l;  
label define pi09l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi10     pi10l;  
label define pi10l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi11     pi11l;  
label define pi11l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi12     pi12l;  
label define pi12l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi13     pi13l;  
label define pi13l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi14     pi14l;  
label define pi14l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi15     pi15l;  
label define pi15l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi16     pi16l;  
label define pi16l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi17     pi17l;  
label define pi17l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi18     pi18l;  
label define pi18l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi19     pi19l;  
label define pi19l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi20     pi20l;  
label define pi20l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi21     pi21l;  
label define pi21l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi22     pi22l;  
label define pi22l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi23     pi23l;  
label define pi23l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi24     pi24l;  
label define pi24l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi25     pi25l;  
label define pi25l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi26     pi26l;  
label define pi26l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi27     pi27l;  
label define pi27l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi28     pi28l;  
label define pi28l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi29     pi29l;  
label define pi29l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi30     pi30l;  
label define pi30l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi31     pi31l;  
label define pi31l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi32     pi32l;  
label define pi32l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi33     pi33l;  
label define pi33l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi34     pi34l;  
label define pi34l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi35     pi35l;  
label define pi35l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi36     pi36l;  
label define pi36l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi37     pi37l;  
label define pi37l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi38     pi38l;  
label define pi38l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi39     pi39l;  
label define pi39l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi40     pi40l;  
label define pi40l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi41     pi41l;  
label define pi41l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi42     pi42l;  
label define pi42l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi43     pi43l;  
label define pi43l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi44     pi44l;  
label define pi44l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi45     pi45l;  
label define pi45l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi46     pi46l;  
label define pi46l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi47     pi47l;  
label define pi47l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi48     pi48l;  
label define pi48l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi49     pi49l;  
label define pi49l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi50     pi50l;  
label define pi50l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi51     pi51l;  
label define pi51l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi52     pi52l;  
label define pi52l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi53     pi53l;  
label define pi53l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi54     pi54l;  
label define pi54l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi55     pi55l;  
label define pi55l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi56     pi56l;  
label define pi56l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi57     pi57l;  
label define pi57l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi58     pi58l;  
label define pi58l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi59     pi59l;  
label define pi59l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi60     pi60l;  
label define pi60l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi61     pi61l;  
label define pi61l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi62     pi62l;  
label define pi62l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi63     pi63l;  
label define pi63l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi75     pi75l;  
label define pi75l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi76     pi76l;  
label define pi76l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi77     pi77l;  
label define pi77l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi78     pi78l;  
label define pi78l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi79     pi79l;  
label define pi79l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi83     pi83l;  
label define pi83l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi84     pi84l;  
label define pi84l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi85     pi85l;  
label define pi85l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi86     pi86l;  
label define pi86l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi87     pi87l;  
label define pi87l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi88     pi88l;  
label define pi88l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi89     pi89l;  
label define pi89l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi90     pi90l;  
label define pi90l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi91     pi91l;  
label define pi91l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi92     pi92l;  
label define pi92l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi93     pi93l;  
label define pi93l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi94     pi94l;  
label define pi94l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi95     pi95l;  
label define pi95l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi96     pi96l;  
label define pi96l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi97     pi97l;  
label define pi97l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi98     pi98l;  
label define pi98l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi99     pi99l;  
label define pi99l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi100    pi100l; 
label define pi100l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi101    pi101l; 
label define pi101l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi102    pi102l; 
label define pi102l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi103    pi103l; 
label define pi103l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi104    pi104l; 
label define pi104l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi105    pi105l; 
label define pi105l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi106    pi106l; 
label define pi106l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi107    pi107l; 
label define pi107l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1_occ  ws1_occ;
label define ws1_occ 
	0           "Not in universe"               
;
label values ws1_ind  ws1_ind;
label define ws1_ind 
	0           "Not in universe"               
;
label values ws1_wks1 ws1_wksd;
label define ws1_wksd
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws1_wks2 ws1_wksk;
label define ws1_wksk
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws1_wks3 ws1_wksl;
label define ws1_wksl
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws1_wks4 ws1_wksm;
label define ws1_wksm
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws1reci1 ws1recid;
label define ws1recid
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws1reci2 ws1recik;
label define ws1recik
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws1reci3 ws1recil;
label define ws1recil
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws1reci4 ws1recim;
label define ws1recim
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws1_amt1 ws1_amtd;
label define ws1_amtd
	-0009       "Not in universe"               
;
label values ws1_amt2 ws1_amtk;
label define ws1_amtk
	-0009       "Not in universe"               
;
label values ws1_amt3 ws1_amtl;
label define ws1_amtl
	-0009       "Not in universe"               
;
label values ws1_amt4 ws1_amtm;
label define ws1_amtm
	-0009       "Not in universe"               
;
label values ws1_2012 ws1_201d;
label define ws1_201d
	0           "Not answered"                  
	1           "A private company or individual"
	2           "Federal government"            
	3           "State government"              
	4           "Local government"              
	5           "Armed Forces"                  
	6           "Unpaid in family business or"  
	9           "Not in universe"               
;
label values ws1_2014 ws1_201k;
label define ws1_201k
	0           "Not in universe"               
	1           "Yes - skip to WS1-2024"        
	2           "No"                            
;
label values ws1_2016 ws1_201l;
label define ws1_201l
	0           "Not in universe"               
;
label values ws1_2018 ws1_201m;
label define ws1_201m
	0           "Not in universe"               
;
label values ws1_2020 ws1_202d;
label define ws1_202d
	0           "Not in universe"               
;
label values ws1_2022 ws1_202k;
label define ws1_202k
	0           "Not in universe"               
;
label values ws1_2024 ws1_202l;
label define ws1_202l
	-9          "Not in universe"               
	0           "None"                          
;
label values ws1_2026 ws1_202m;
label define ws1_202m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to WS1-2030"         
;
label values ws1_2028 ws1_202n;
label define ws1_202n
	0           "Not in universe"               
;
label values ws1_2030 ws1_203d;
label define ws1_203d
	0           "Not in universe"               
	1           "Once a week"                   
	2           "Once each 2 weeks"             
	3           "Once a month"                  
	4           "Twice a month"                 
	5           "Some other way"                
;
label values ws1_2032 ws1_203k;
label define ws1_203k
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws1_2034 ws1_203l;
label define ws1_203l
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws1_2036 ws1_203m;
label define ws1_203m
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws1_2038 ws1_203n;
label define ws1_203n
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws1_2040 ws1_204d;
label define ws1_204d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to WS1-2044"         
;
label values ws1_2042 ws1_204k;
label define ws1_204k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws1imp01 ws1imp0d;
label define ws1imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1imp02 ws1imp0k;
label define ws1imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1imp03 ws1imp0l;
label define ws1imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1imp04 ws1imp0m;
label define ws1imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1imp05 ws1imp0n;
label define ws1imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1imp06 ws1imp0o;
label define ws1imp0o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1cal01 ws1cal0d;
label define ws1cal0d
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values ws1cal02 ws1cal0k;
label define ws1cal0k
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values ws1cal03 ws1cal0l;
label define ws1cal0l
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values ws1cal04 ws1cal0m;
label define ws1cal0m
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values ws2_occ  ws2_occ;
label define ws2_occ 
	0           "Not in universe"               
;
label values ws2_ind  ws2_ind;
label define ws2_ind 
	0           "Not in universe"               
;
label values ws2_wks1 ws2_wksd;
label define ws2_wksd
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws2_wks2 ws2_wksk;
label define ws2_wksk
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws2_wks3 ws2_wksl;
label define ws2_wksl
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws2_wks4 ws2_wksm;
label define ws2_wksm
	0           "None or not in universe if"    
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws2reci1 ws2recid;
label define ws2recid
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws2reci2 ws2recik;
label define ws2recik
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws2reci3 ws2recil;
label define ws2recil
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws2reci4 ws2recim;
label define ws2recim
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws2_amt1 ws2_amtd;
label define ws2_amtd
	-0009       "Not in universe"               
;
label values ws2_amt2 ws2_amtk;
label define ws2_amtk
	-0009       "Not in universe"               
;
label values ws2_amt3 ws2_amtl;
label define ws2_amtl
	-0009       "Not in universe"               
;
label values ws2_amt4 ws2_amtm;
label define ws2_amtm
	-0009       "Not in universe"               
;
label values ws2_2112 ws2_211d;
label define ws2_211d
	0           "Not answered"                  
	1           "A private company or individual"
	2           "Federal government"            
	3           "State government"              
	4           "Local government"              
	5           "Armed Forces"                  
	6           "Unpaid in family business or"  
	9           "Not in universe"               
;
label values ws2_2114 ws2_211k;
label define ws2_211k
	0           "Not in universe"               
	1           "Yes - skip to WS2-2024"        
	2           "No"                            
;
label values ws2_2116 ws2_211l;
label define ws2_211l
	0           "Not in universe"               
;
label values ws2_2118 ws2_211m;
label define ws2_211m
	0           "Not in universe"               
;
label values ws2_2120 ws2_212d;
label define ws2_212d
	0           "Not in universe"               
;
label values ws2_2122 ws2_212k;
label define ws2_212k
	0           "Not in universe"               
;
label values ws2_2124 ws2_212l;
label define ws2_212l
	-9          "Not in universe"               
	0           "None"                          
;
label values ws2_2126 ws2_212m;
label define ws2_212m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to WS2-2030"         
;
label values ws2_2128 ws2_212n;
label define ws2_212n
	0           "Not in universe"               
;
label values ws2_2130 ws2_213d;
label define ws2_213d
	0           "Not in universe"               
	1           "Once a week"                   
	2           "Once each 2 weeks"             
	3           "Once a month"                  
	4           "Twice a month"                 
	5           "Some other way"                
;
label values ws2_2132 ws2_213k;
label define ws2_213k
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2134 ws2_213l;
label define ws2_213l
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2136 ws2_213m;
label define ws2_213m
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2138 ws2_213n;
label define ws2_213n
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2140 ws2_214d;
label define ws2_214d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to WS2-2044"         
;
label values ws2_2142 ws2_214k;
label define ws2_214k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws2imp01 ws2imp0d;
label define ws2imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws2imp02 ws2imp0k;
label define ws2imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws2imp03 ws2imp0l;
label define ws2imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws2imp04 ws2imp0m;
label define ws2imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws2imp05 ws2imp0n;
label define ws2imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws2imp06 ws2imp0o;
label define ws2imp0o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws2cal01 ws2cal0d;
label define ws2cal0d
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values ws2cal02 ws2cal0k;
label define ws2cal0k
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values ws2cal03 ws2cal0l;
label define ws2cal0l
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values ws2cal04 ws2cal0m;
label define ws2cal0m
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se1_occ  se1_occ;
label define se1_occ 
	0           "Not in universe"               
;
label values se1_indr se1_indr;
label define se1_indr
	1           "Agriculture, forestry, fisheries"
	2           "Mining"                        
	3           "Construction"                  
	4           "Manufacturing-nondurable goods"
	5           "Manufacturing-durable goods"   
	6           "Transportation, communications,"
	7           "Wholesale trade-durable goods" 
	8           "Wholdsale trade-nondurable goods"
	9           "Retail trade"                  
	10          "Finance, insurance, real estate"
	11          "Business and repair services"  
	12          "Personal services"             
	13          "Entertainment and recreation"  
	14          "Professional and related services"
	15          "Public administration"         
	16          "Industry not reported"         
;
label values se1wks1  se1wks1l;
label define se1wks1l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se1wks2  se1wks2l;
label define se1wks2l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se1wks3  se1wks3l;
label define se1wks3l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se1wks4  se1wks4l;
label define se1wks4l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se1rec1  se1rec1l;
label define se1rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se1rec2  se1rec2l;
label define se1rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se1rec3  se1rec3l;
label define se1rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se1rec4  se1rec4l;
label define se1rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se1amt1  se1amt1l;
label define se1amt1l
	-0009       "Not in universe"               
;
label values se1amt2  se1amt2l;
label define se1amt2l
	-0009       "Not in universe"               
;
label values se1amt3  se1amt3l;
label define se1amt3l
	-0009       "Not in universe"               
;
label values se1amt4  se1amt4l;
label define se1amt4l
	-0009       "Not in universe"               
;
label values se12212  se12212l;
label define se12212l
	-9          "Not in universe"               
	0           "None"                          
;
label values se12214  se12214l;
label define se12214l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE1-2260"         
;
label values se12216  se12216l;
label define se12216l
	0           "Not in universe"               
	1           "Yes - skip to SE12232"         
	2           "No"                            
;
label values se12218  se12218l;
label define se12218l
	0           "Not in universe"               
	1           "1 employee"                    
	2           "2 employees"                   
	3           "3 - 5 employees"               
	4           "6 or more employees"           
;
label values se12220  se12220l;
label define se12220l
	0           "Not in universe"               
	1           "Yes - skip to SE12224"         
	2           "No"                            
;
label values se12222  se12222l;
label define se12222l
	0           "Not in universe"               
	1           "Sole proprietorship - skip to" 
	2           "Partnership"                   
;
label values se12224  se12224l;
label define se12224l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE12232"          
;
label values se12226  se12226l;
label define se12226l
	0           "Not in universe"               
;
label values se12228  se12228l;
label define se12228l
	0           "Not in universe"               
;
label values se12230  se12230l;
label define se12230l
	0           "Not in universe"               
;
label values se12232  se12232l;
label define se12232l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12234  se12234l;
label define se12234l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12236  se12236l;
label define se12236l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE12250"          
;
label values se12238  se12238l;
label define se12238l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se12240  se12240l;
label define se12240l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se12242  se12242l;
label define se12242l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se12244  se12244l;
label define se12244l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se12246  se12246l;
label define se12246l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE12250"          
;
label values se12248  se12248l;
label define se12248l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12250  se12250l;
label define se12250l
	0           "Not in universe"               
	1           "Yes - skip to SE12262"         
	2           "No"                            
;
label values se12252  se12252l;
label define se12252l
	0           "Not in universe"               
	1           "Yes - skip to SE12262"         
	2           "No"                            
;
label values se12254  se12254l;
label define se12254l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE12262"          
;
label values se12256  se12256l;
label define se12256l
	0           "Not in universe"               
;
label values se12260  se12260l;
label define se12260l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se1imp01 se1imp0r;
label define se1imp0r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp02 se1imp0k;
label define se1imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp03 se1imp0l;
label define se1imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp04 se1imp0m;
label define se1imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp05 se1imp0n;
label define se1imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp06 se1imp0o;
label define se1imp0o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp07 se1imp0p;
label define se1imp0p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp08 se1imp0q;
label define se1imp0q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp09 se1imp0s;
label define se1imp0s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp10 se1imp1r;
label define se1imp1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp11 se1imp1k;
label define se1imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1cal01 se1cal0r;
label define se1cal0r
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se1cal02 se1cal0k;
label define se1cal0k
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se1cal03 se1cal0l;
label define se1cal0l
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se1cal04 se1cal0m;
label define se1cal0m
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se2occ   se2occ; 
label define se2occ  
	0           "Not in universe"               
;
label values se2_indr se2_indr;
label define se2_indr
	1           "Agriculture, forestry, fisheries"
	2           "Mining"                        
	3           "Construction"                  
	4           "Manufacturing-nondurable goods"
	5           "Manufacturing-durable goods"   
	6           "Transportation, communications,"
	7           "Wholesale trade-durable goods" 
	8           "Wholdsale trade-nondurable goods"
	9           "Retail trade"                  
	10          "Finance, insurance, real estate"
	11          "Business and repair services"  
	12          "Personal services"             
	13          "Entertainment and recreation"  
	14          "Professional and related services"
	15          "Public administration"         
	16          "Industry not reported"         
;
label values se2wks1  se2wks1l;
label define se2wks1l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se2wks2  se2wks2l;
label define se2wks2l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se2wks3  se2wks3l;
label define se2wks3l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se2wks4  se2wks4l;
label define se2wks4l
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se2rec1  se2rec1l;
label define se2rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se2rec2  se2rec2l;
label define se2rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se2rec3  se2rec3l;
label define se2rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se2rec4  se2rec4l;
label define se2rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se2amt1  se2amt1l;
label define se2amt1l
	-0009       "Not in universe"               
;
label values se2amt2  se2amt2l;
label define se2amt2l
	-0009       "Not in universe"               
;
label values se2amt3  se2amt3l;
label define se2amt3l
	-0009       "Not in universe"               
;
label values se2amt4  se2amt4l;
label define se2amt4l
	-0009       "Not in universe"               
;
label values se22312  se22312l;
label define se22312l
	-9          "Not in universe"               
	0           "None"                          
;
label values se22314  se22314l;
label define se22314l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22260"          
;
label values se22316  se22316l;
label define se22316l
	0           "Not in universe"               
	1           "Yes - skip to SE22232"         
	2           "No"                            
;
label values se22318  se22318l;
label define se22318l
	0           "Not in universe"               
	1           "1 employee"                    
	2           "2 employees"                   
	3           "3 - 5 employees"               
	4           "6 or more employees"           
;
label values se22320  se22320l;
label define se22320l
	0           "Not in universe"               
	1           "Yes - skip to SE22224"         
	2           "No"                            
;
label values se22322  se22322l;
label define se22322l
	0           "Not in universe"               
	1           "Sole proprietorship - skip to" 
	2           "Partnership"                   
;
label values se22324  se22324l;
label define se22324l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22232"          
;
label values se22326  se22326l;
label define se22326l
	0           "Not in universe"               
;
label values se22328  se22328l;
label define se22328l
	0           "Not in universe"               
;
label values se22330  se22330l;
label define se22330l
	0           "Not in universe"               
;
label values se22332  se22332l;
label define se22332l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22334  se22334l;
label define se22334l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22336  se22336l;
label define se22336l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22250"          
;
label values se22338  se22338l;
label define se22338l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22340  se22340l;
label define se22340l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22342  se22342l;
label define se22342l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22344  se22344l;
label define se22344l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22346  se22346l;
label define se22346l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22250"          
;
label values se22348  se22348l;
label define se22348l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22350  se22350l;
label define se22350l
	0           "Not in universe"               
	1           "Yes - skip to SE22262"         
	2           "No"                            
;
label values se22352  se22352l;
label define se22352l
	0           "Not in universe"               
	1           "Yes - skip to SE22262"         
	2           "No"                            
;
label values se22354  se22354l;
label define se22354l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22262"          
;
label values se22356  se22356l;
label define se22356l
	0           "Not in universe"               
;
label values se22360  se22360l;
label define se22360l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se2imp01 se2imp0r;
label define se2imp0r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp02 se2imp0k;
label define se2imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp03 se2imp0l;
label define se2imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp04 se2imp0m;
label define se2imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp05 se2imp0n;
label define se2imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp06 se2imp0o;
label define se2imp0o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp07 se2imp0p;
label define se2imp0p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp08 se2imp0q;
label define se2imp0q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp09 se2imp0s;
label define se2imp0s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp10 se2imp1r;
label define se2imp1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp11 se2imp1k;
label define se2imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2cal01 se2cal0r;
label define se2cal0r
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se2cal02 se2cal0k;
label define se2cal0k
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se2cal03 se2cal0l;
label define se2cal0l
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values se2cal04 se2cal0m;
label define se2cal0m
	0           "Not calculated"                
	1           "Imputed input"                 
	2           "No imputed input"              
;
label values i01rec1  i01rec1l;
label define i01rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i01rec2  i01rec2l;
label define i01rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i01rec3  i01rec3l;
label define i01rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i01rec4  i01rec4l;
label define i01rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i01amt1  i01amt1l;
label define i01amt1l
	-0009       "Not in universe"               
;
label values i01amt2  i01amt2l;
label define i01amt2l
	-0009       "Not in universe"               
;
label values i01amt3  i01amt3l;
label define i01amt3l
	-0009       "Not in universe"               
;
label values i01amt4  i01amt4l;
label define i01amt4l
	-0009       "Not in universe"               
;
label values kidssyn1 kidssynr;
label define kidssynr
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kidssyn2 kidssynk;
label define kidssynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kidssyn3 kidssynl;
label define kidssynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kidssyn4 kidssynm;
label define kidssynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kdssamt1 kdssamtr;
label define kdssamtr
	-009        "Not in universe"               
;
label values kdssamt2 kdssamtk;
label define kdssamtk
	-009        "Not in universe"               
;
label values kdssamt3 kdssamtl;
label define kdssamtl
	-009        "Not in universe"               
;
label values kdssamt4 kdssamtm;
label define kdssamtm
	-009        "Not in universe"               
;
label values ssrecind ssrecind;
label define ssrecind
	0           "Not in universe"               
	1           "Adult benefits received in own"
	2           "Only adult benefits received"  
	3           "Only child benefits received"  
	4           "Adult benefits received in own"
	5           "Adult benefits received jointly"
;
label values ss3004   ss3004l;
label define ss3004l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ss3006   ss3006l;
label define ss3006l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ss3008   ss3008l;
label define ss3008l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC3070 through"   
;
label values ss3012   ss3012l;
label define ss3012l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC3016 through"   
;
label values ss3014   ss3014l;
label define ss3014l 
	0           "Not in universe"               
	1           "Yes - skip to next ISS code"   
	2           "No"                            
;
label values ss3064   ss3064l;
label define ss3064l 
	-1          "DK"                            
	0           "Not in universe"               
	1           "Green"                         
	2           "Gold"                          
	3           "Other"                         
;
label values ss3066   ss3066l;
label define ss3066l 
	-1          "DK"                            
	0           "Not in universe"               
	1           "First"                         
	2           "Third"                         
	3           "Other"                         
;
label values ss3068   ss3068l;
label define ss3068l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values ss3086   ss3086l;
label define ss3086l 
	0           "Not in universe"               
	1           "Yes - skip to next ISS code"   
	2           "No"                            
;
label values ss3088   ss3088l;
label define ss3088l 
	0           "Not in universe"               
;
label values ss3090   ss3090l;
label define ss3090l 
	0           "Not in universe"               
;
label values ss3092   ss3092l;
label define ss3092l 
	0           "Not in universe"               
;
label values ss3094   ss3094l;
label define ss3094l 
	0           "Not in universe"               
;
label values ss3096   ss3096l;
label define ss3096l 
	0           "Not in universe"               
;
label values ss3098   ss3098l;
label define ss3098l 
	0           "Not in universe"               
;
label values i01imp01 i01imp0d;
label define i01imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp02 i01imp0k;
label define i01imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp03 i01imp0l;
label define i01imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp04 i01imp0m;
label define i01imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp05 i01imp0n;
label define i01imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp06 i01imp0o;
label define i01imp0o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp07 i01imp0p;
label define i01imp0p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp08 i01imp0q;
label define i01imp0q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i01imp09 i01imp0r;
label define i01imp0r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02rec1  i02rec1l;
label define i02rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i02rec2  i02rec2l;
label define i02rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i02rec3  i02rec3l;
label define i02rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i02rec4  i02rec4l;
label define i02rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i02amt1  i02amt1l;
label define i02amt1l
	-0009       "Not in universe"               
;
label values i02amt2  i02amt2l;
label define i02amt2l
	-0009       "Not in universe"               
;
label values i02amt3  i02amt3l;
label define i02amt3l
	-0009       "Not in universe"               
;
label values i02amt4  i02amt4l;
label define i02amt4l
	-0009       "Not in universe"               
;
label values kidrryn1 kidrrynd;
label define kidrrynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kidrryn2 kidrrynk;
label define kidrrynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kidrryn3 kidrrynl;
label define kidrrynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kidrryn4 kidrrynm;
label define kidrrynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values kdrramt1 kdrramtd;
label define kdrramtd
	-009        "Not in universe"               
;
label values kdrramt2 kdrramtk;
label define kdrramtk
	-009        "Not in universe"               
;
label values kdrramt3 kdrramtl;
label define kdrramtl
	-009        "Not in universe"               
;
label values kdrramt4 kdrramtm;
label define kdrramtm
	-009        "Not in universe"               
;
label values rrrecind rrrecind;
label define rrrecind
	0           "Not in universe"               
	1           "Adult benefits received in own"
	2           "Only adult benefits received"  
	3           "Only child benefits received"  
	4           "Adult benefits received in own"
	5           "Adult benefits received jointly"
;
label values rr3004   rr3004l;
label define rr3004l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values rr3006   rr3006l;
label define rr3006l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values rr3008   rr3008l;
label define rr3008l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC3070 through"   
;
label values rr3012   rr3012l;
label define rr3012l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC3016 through"   
;
label values rr3014   rr3014l;
label define rr3014l 
	0           "Not in universe"               
	1           "Yes - skip to next ISS code"   
	2           "No"                            
;
label values rr3064   rr3064l;
label define rr3064l 
	-1          "DK"                            
	0           "Not in universe"               
	1           "Green"                         
	2           "Gold"                          
	3           "Other"                         
;
label values rr3066   rr3066l;
label define rr3066l 
	-1          "DK"                            
	0           "Not in universe"               
	1           "First"                         
	2           "Third"                         
	3           "Other"                         
;
label values rr3068   rr3068l;
label define rr3068l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values rr3086   rr3086l;
label define rr3086l 
	0           "Not in universe"               
	1           "Yes - skip to next ISS code"   
	2           "No"                            
;
label values rr3088   rr3088l;
label define rr3088l 
	0           "Not in universe"               
;
label values rr3090   rr3090l;
label define rr3090l 
	0           "Not in universe"               
;
label values rr3092   rr3092l;
label define rr3092l 
	0           "Not in universe"               
;
label values rr3094   rr3094l;
label define rr3094l 
	0           "Not in universe"               
;
label values rr3096   rr3096l;
label define rr3096l 
	0           "Not in universe"               
;
label values rr3098   rr3098l;
label define rr3098l 
	0           "Not in universe"               
;
label values i02imp01 i02imp0d;
label define i02imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp02 i02imp0k;
label define i02imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp03 i02imp0l;
label define i02imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp04 i02imp0m;
label define i02imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp05 i02imp0n;
label define i02imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp06 i02imp0o;
label define i02imp0o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp07 i02imp0p;
label define i02imp0p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp08 i02imp0q;
label define i02imp0q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i02imp09 i02imp0r;
label define i02imp0r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i03rec1  i03rec1l;
label define i03rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i03rec2  i03rec2l;
label define i03rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i03rec3  i03rec3l;
label define i03rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i03rec4  i03rec4l;
label define i03rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i03amt1  i03amt1l;
label define i03amt1l
	-0009       "Not in universe"               
;
label values i03amt2  i03amt2l;
label define i03amt2l
	-0009       "Not in universe"               
;
label values i03amt3  i03amt3l;
label define i03amt3l
	-0009       "Not in universe"               
;
label values i03amt4  i03amt4l;
label define i03amt4l
	-0009       "Not in universe"               
;
label values i03imp01 i03imp0d;
label define i03imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i03imp02 i03imp0k;
label define i03imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i03imp03 i03imp0l;
label define i03imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i03imp04 i03imp0m;
label define i03imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i05rec1  i05rec1l;
label define i05rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i05rec2  i05rec2l;
label define i05rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i05rec3  i05rec3l;
label define i05rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i05rec4  i05rec4l;
label define i05rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i05amt1  i05amt1l;
label define i05amt1l
	-0009       "Not in universe"               
;
label values i05amt2  i05amt2l;
label define i05amt2l
	-0009       "Not in universe"               
;
label values i05amt3  i05amt3l;
label define i05amt3l
	-0009       "Not in universe"               
;
label values i05amt4  i05amt4l;
label define i05amt4l
	-0009       "Not in universe"               
;
label values i05imp01 i05imp0d;
label define i05imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i05imp02 i05imp0k;
label define i05imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i05imp03 i05imp0l;
label define i05imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i05imp04 i05imp0m;
label define i05imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i06rec1  i06rec1l;
label define i06rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i06rec2  i06rec2l;
label define i06rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i06rec3  i06rec3l;
label define i06rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i06rec4  i06rec4l;
label define i06rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i06amt1  i06amt1l;
label define i06amt1l
	-0009       "Not in universe"               
;
label values i06amt2  i06amt2l;
label define i06amt2l
	-0009       "Not in universe"               
;
label values i06amt3  i06amt3l;
label define i06amt3l
	-0009       "Not in universe"               
;
label values i06amt4  i06amt4l;
label define i06amt4l
	-0009       "Not in universe"               
;
label values i06imp01 i06imp0d;
label define i06imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i06imp02 i06imp0k;
label define i06imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i06imp03 i06imp0l;
label define i06imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i06imp04 i06imp0m;
label define i06imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i07rec1  i07rec1l;
label define i07rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i07rec2  i07rec2l;
label define i07rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i07rec3  i07rec3l;
label define i07rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i07rec4  i07rec4l;
label define i07rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i07amt1  i07amt1l;
label define i07amt1l
	-0009       "Not in universe"               
;
label values i07amt2  i07amt2l;
label define i07amt2l
	-0009       "Not in universe"               
;
label values i07amt3  i07amt3l;
label define i07amt3l
	-0009       "Not in universe"               
;
label values i07amt4  i07amt4l;
label define i07amt4l
	-0009       "Not in universe"               
;
label values i07imp01 i07imp0d;
label define i07imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i07imp02 i07imp0k;
label define i07imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i07imp03 i07imp0l;
label define i07imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i07imp04 i07imp0m;
label define i07imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i08rec1  i08rec1l;
label define i08rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i08rec2  i08rec2l;
label define i08rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i08rec3  i08rec3l;
label define i08rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i08rec4  i08rec4l;
label define i08rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i08amt1  i08amt1l;
label define i08amt1l
	-0009       "Not in universe"               
;
label values i08amt2  i08amt2l;
label define i08amt2l
	-0009       "Not in universe"               
;
label values i08amt3  i08amt3l;
label define i08amt3l
	-0009       "Not in universe"               
;
label values i08amt4  i08amt4l;
label define i08amt4l
	-0009       "Not in universe"               
;
label values vet3034  vet3034l;
label define vet3034l
	0           "Not in universe"               
	1           "Yes - skip to next income source"
	2           "No"                            
;
label values vet3036  vet3036l;
label define vet3036l
	0           "Not in universe"               
;
label values vet3038  vet3038l;
label define vet3038l
	0           "Not in universe"               
;
label values vet3040  vet3040l;
label define vet3040l
	0           "Not in universe"               
;
label values vet3042  vet3042l;
label define vet3042l
	0           "Not in universe"               
;
label values vet3044  vet3044l;
label define vet3044l
	0           "Not in universe"               
;
label values vet3046  vet3046l;
label define vet3046l
	0           "Not in universe"               
;
label values vet3048  vet3048l;
label define vet3048l
	0           "Not in universe"               
;
label values vet3050  vet3050l;
label define vet3050l
	0           "Not in universe"               
;
label values vet3052  vet3052l;
label define vet3052l
	0           "Not in universe"               
;
label values vet3054  vet3054l;
label define vet3054l
	0           "Not in universe"               
;
label values vet3060  vet3060l;
label define vet3060l
	-1          "DK - skip to next ISS code"    
	0           "Not in universe"               
	1           "Yes - skip to next ISS code"   
	2           "No - skip to next ISS code"    
;
label values i08imp01 i08imp0d;
label define i08imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i08imp02 i08imp0k;
label define i08imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i08imp03 i08imp0l;
label define i08imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i08imp04 i08imp0m;
label define i08imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i10rec1  i10rec1l;
label define i10rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i10rec2  i10rec2l;
label define i10rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i10rec3  i10rec3l;
label define i10rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i10rec4  i10rec4l;
label define i10rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i10amt1  i10amt1l;
label define i10amt1l
	-0009       "Not in universe"               
;
label values i10amt2  i10amt2l;
label define i10amt2l
	-0009       "Not in universe"               
;
label values i10amt3  i10amt3l;
label define i10amt3l
	-0009       "Not in universe"               
;
label values i10amt4  i10amt4l;
label define i10amt4l
	-0009       "Not in universe"               
;
label values i10imp01 i10imp0d;
label define i10imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i10imp02 i10imp0k;
label define i10imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i10imp03 i10imp0l;
label define i10imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i10imp04 i10imp0m;
label define i10imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i12rec1  i12rec1l;
label define i12rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i12rec2  i12rec2l;
label define i12rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i12rec3  i12rec3l;
label define i12rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i12rec4  i12rec4l;
label define i12rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i12amt1  i12amt1l;
label define i12amt1l
	-0009       "Not in universe"               
;
label values i12amt2  i12amt2l;
label define i12amt2l
	-0009       "Not in universe"               
;
label values i12amt3  i12amt3l;
label define i12amt3l
	-0009       "Not in universe"               
;
label values i12amt4  i12amt4l;
label define i12amt4l
	-0009       "Not in universe"               
;
label values i12imp01 i12imp0d;
label define i12imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i12imp02 i12imp0k;
label define i12imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i12imp03 i12imp0l;
label define i12imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i12imp04 i12imp0m;
label define i12imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i13rec1  i13rec1l;
label define i13rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i13rec2  i13rec2l;
label define i13rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i13rec3  i13rec3l;
label define i13rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i13rec4  i13rec4l;
label define i13rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i13amt1  i13amt1l;
label define i13amt1l
	-0009       "Not in universe"               
;
label values i13amt2  i13amt2l;
label define i13amt2l
	-0009       "Not in universe"               
;
label values i13amt3  i13amt3l;
label define i13amt3l
	-0009       "Not in universe"               
;
label values i13amt4  i13amt4l;
label define i13amt4l
	-0009       "Not in universe"               
;
label values i13imp01 i13imp0d;
label define i13imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i13imp02 i13imp0k;
label define i13imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i13imp03 i13imp0l;
label define i13imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i13imp04 i13imp0m;
label define i13imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i20rec1  i20rec1l;
label define i20rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i20rec2  i20rec2l;
label define i20rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i20rec3  i20rec3l;
label define i20rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i20rec4  i20rec4l;
label define i20rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i20amt1  i20amt1l;
label define i20amt1l
	-0009       "Not in universe"               
;
label values i20amt2  i20amt2l;
label define i20amt2l
	-0009       "Not in universe"               
;
label values i20amt3  i20amt3l;
label define i20amt3l
	-0009       "Not in universe"               
;
label values i20amt4  i20amt4l;
label define i20amt4l
	-0009       "Not in universe"               
;
label values afdc3034 afdc303d;
label define afdc303d
	0           "Not in universe"               
	1           "Yes - skip to next income source"
	2           "No"                            
;
label values afdc3036 afdc303k;
label define afdc303k
	0           "Not in universe"               
;
label values afdc3038 afdc303l;
label define afdc303l
	0           "Not in universe"               
;
label values afdc3040 afdc304d;
label define afdc304d
	0           "Not in universe"               
;
label values afdc3042 afdc304k;
label define afdc304k
	0           "Not in universe"               
;
label values afdc3044 afdc304l;
label define afdc304l
	0           "Not in universe"               
;
label values afdc3046 afdc304m;
label define afdc304m
	0           "Not in universe"               
;
label values afdc3048 afdc304n;
label define afdc304n
	0           "Not in universe"               
;
label values afdc3050 afdc305d;
label define afdc305d
	0           "Not in universe"               
;
label values afdc3052 afdc305k;
label define afdc305k
	0           "Not in universe"               
;
label values afdc3054 afdc305l;
label define afdc305l
	0           "Not in universe"               
;
label values i20imp01 i20imp0d;
label define i20imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i20imp02 i20imp0k;
label define i20imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i20imp03 i20imp0l;
label define i20imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i20imp04 i20imp0m;
label define i20imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i21rec1  i21rec1l;
label define i21rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i21rec2  i21rec2l;
label define i21rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i21rec3  i21rec3l;
label define i21rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i21rec4  i21rec4l;
label define i21rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i21amt1  i21amt1l;
label define i21amt1l
	-0009       "Not in universe"               
;
label values i21amt2  i21amt2l;
label define i21amt2l
	-0009       "Not in universe"               
;
label values i21amt3  i21amt3l;
label define i21amt3l
	-0009       "Not in universe"               
;
label values i21amt4  i21amt4l;
label define i21amt4l
	-0009       "Not in universe"               
;
label values ga3034   ga3034l;
label define ga3034l 
	0           "Not in universe"               
	1           "Yes - skip to next income source"
	2           "No"                            
;
label values ga3036   ga3036l;
label define ga3036l 
	0           "Not in universe"               
;
label values ga3038   ga3038l;
label define ga3038l 
	0           "Not in universe"               
;
label values ga3040   ga3040l;
label define ga3040l 
	0           "Not in universe"               
;
label values ga3042   ga3042l;
label define ga3042l 
	0           "Not in universe"               
;
label values ga3044   ga3044l;
label define ga3044l 
	0           "Not in universe"               
;
label values ga3046   ga3046l;
label define ga3046l 
	0           "Not in universe"               
;
label values ga3048   ga3048l;
label define ga3048l 
	0           "Not in universe"               
;
label values ga3050   ga3050l;
label define ga3050l 
	0           "Not in universe"               
;
label values ga3052   ga3052l;
label define ga3052l 
	0           "Not in universe"               
;
label values ga3054   ga3054l;
label define ga3054l 
	0           "Not in universe"               
;
label values i21imp01 i21imp0d;
label define i21imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i21imp02 i21imp0k;
label define i21imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i21imp03 i21imp0l;
label define i21imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i21imp04 i21imp0m;
label define i21imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i23rec1  i23rec1l;
label define i23rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i23rec2  i23rec2l;
label define i23rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i23rec3  i23rec3l;
label define i23rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i23rec4  i23rec4l;
label define i23rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i23amt1  i23amt1l;
label define i23amt1l
	-0009       "Not in universe"               
;
label values i23amt2  i23amt2l;
label define i23amt2l
	-0009       "Not in universe"               
;
label values i23amt3  i23amt3l;
label define i23amt3l
	-0009       "Not in universe"               
;
label values i23amt4  i23amt4l;
label define i23amt4l
	-0009       "Not in universe"               
;
label values fcc3034  fcc3034l;
label define fcc3034l
	0           "Not in universe"               
	1           "Yes - skip to next income source"
	2           "No"                            
;
label values fcc3036  fcc3036l;
label define fcc3036l
	0           "Not in universe"               
;
label values fcc3038  fcc3038l;
label define fcc3038l
	0           "Not in universe"               
;
label values fcc3040  fcc3040l;
label define fcc3040l
	0           "Not in universe"               
;
label values fcc3042  fcc3042l;
label define fcc3042l
	0           "Not in universe"               
;
label values fcc3044  fcc3044l;
label define fcc3044l
	0           "Not in universe"               
;
label values fcc3046  fcc3046l;
label define fcc3046l
	0           "Not in universe"               
;
label values fcc3048  fcc3048l;
label define fcc3048l
	0           "Not in universe"               
;
label values fcc3050  fcc3050l;
label define fcc3050l
	0           "Not in universe"               
;
label values fcc3052  fcc3052l;
label define fcc3052l
	0           "Not in universe"               
;
label values fcc3054  fcc3054l;
label define fcc3054l
	0           "Not in universe"               
;
label values i23imp01 i23imp0d;
label define i23imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i23imp02 i23imp0k;
label define i23imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i23imp03 i23imp0l;
label define i23imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i23imp04 i23imp0m;
label define i23imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i24rec1  i24rec1l;
label define i24rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i24rec2  i24rec2l;
label define i24rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i24rec3  i24rec3l;
label define i24rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i24rec4  i24rec4l;
label define i24rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i24amt1  i24amt1l;
label define i24amt1l
	-0009       "Not in universe"               
;
label values i24amt2  i24amt2l;
label define i24amt2l
	-0009       "Not in universe"               
;
label values i24amt3  i24amt3l;
label define i24amt3l
	-0009       "Not in universe"               
;
label values i24amt4  i24amt4l;
label define i24amt4l
	-0009       "Not in universe"               
;
label values ow3034   ow3034l;
label define ow3034l 
	0           "Not in universe"               
	1           "Yes - skip to next income source"
	2           "No"                            
;
label values ow3036   ow3036l;
label define ow3036l 
	0           "Not in universe"               
;
label values ow3038   ow3038l;
label define ow3038l 
	0           "Not in universe"               
;
label values ow3040   ow3040l;
label define ow3040l 
	0           "Not in universe"               
;
label values ow3042   ow3042l;
label define ow3042l 
	0           "Not in universe"               
;
label values ow3044   ow3044l;
label define ow3044l 
	0           "Not in universe"               
;
label values ow3046   ow3046l;
label define ow3046l 
	0           "Not in universe"               
;
label values ow3048   ow3048l;
label define ow3048l 
	0           "Not in universe"               
;
label values ow3050   ow3050l;
label define ow3050l 
	0           "Not in universe"               
;
label values ow3052   ow3052l;
label define ow3052l 
	0           "Not in universe"               
;
label values ow3054   ow3054l;
label define ow3054l 
	0           "Not in universe"               
;
label values i24imp01 i24imp0d;
label define i24imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i24imp02 i24imp0k;
label define i24imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i24imp03 i24imp0l;
label define i24imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i24imp04 i24imp0m;
label define i24imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values wic3138  wic3138l;
label define wic3138l
	0           "No"                            
	1           "Yes"                           
	9           "Not in universe"               
;
label values wic3140  wic3140l;
label define wic3140l
	0           "No"                            
	1           "Yes"                           
	9           "Not in universe"               
;
label values wic3142  wic3142l;
label define wic3142l
	0           "No"                            
	1           "Yes"                           
	9           "Not in universe"               
;
label values wic3144  wic3144l;
label define wic3144l
	0           "No"                            
	1           "Yes"                           
	9           "Not in universe"               
;
label values i25imp01 i25imp0d;
label define i25imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i25imp02 i25imp0k;
label define i25imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i25imp03 i25imp0l;
label define i25imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i25imp04 i25imp0m;
label define i25imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i27rec1  i27rec1l;
label define i27rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i27rec2  i27rec2l;
label define i27rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i27rec3  i27rec3l;
label define i27rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i27rec4  i27rec4l;
label define i27rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i27amt1  i27amt1l;
label define i27amt1l
	-0009       "Not in universe"               
;
label values i27amt2  i27amt2l;
label define i27amt2l
	-0009       "Not in universe"               
;
label values i27amt3  i27amt3l;
label define i27amt3l
	-0009       "Not in universe"               
;
label values i27amt4  i27amt4l;
label define i27amt4l
	-0009       "Not in universe"               
;
label values fs3100   fs3100l;
label define fs3100l 
	0           "Not in universe"               
	1           "Yes - skip to next income source"
	2           "No"                            
;
label values fs3102   fs3102l;
label define fs3102l 
	0           "Not in universe"               
;
label values fs3104   fs3104l;
label define fs3104l 
	0           "Not in universe"               
;
label values fs3106   fs3106l;
label define fs3106l 
	0           "Not in universe"               
;
label values fs3108   fs3108l;
label define fs3108l 
	0           "Not in universe"               
;
label values fs3110   fs3110l;
label define fs3110l 
	0           "Not in universe"               
;
label values fs3112   fs3112l;
label define fs3112l 
	0           "Not in universe"               
;
label values fs3114   fs3114l;
label define fs3114l 
	0           "Not in universe"               
;
label values fs3116   fs3116l;
label define fs3116l 
	0           "Not in universe"               
;
label values fs3118   fs3118l;
label define fs3118l 
	0           "Not in universe"               
;
label values fs3120   fs3120l;
label define fs3120l 
	0           "Not in universe"               
;
label values i27imp01 i27imp0d;
label define i27imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i27imp02 i27imp0k;
label define i27imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i27imp03 i27imp0l;
label define i27imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i27imp04 i27imp0m;
label define i27imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i28rec1  i28rec1l;
label define i28rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i28rec2  i28rec2l;
label define i28rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i28rec3  i28rec3l;
label define i28rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i28rec4  i28rec4l;
label define i28rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i28amt1  i28amt1l;
label define i28amt1l
	-0009       "Not in universe"               
;
label values i28amt2  i28amt2l;
label define i28amt2l
	-0009       "Not in universe"               
;
label values i28amt3  i28amt3l;
label define i28amt3l
	-0009       "Not in universe"               
;
label values i28amt4  i28amt4l;
label define i28amt4l
	-0009       "Not in universe"               
;
label values i28imp01 i28imp0d;
label define i28imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i28imp02 i28imp0k;
label define i28imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i28imp03 i28imp0l;
label define i28imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i28imp04 i28imp0m;
label define i28imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i29rec1  i29rec1l;
label define i29rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i29rec2  i29rec2l;
label define i29rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i29rec3  i29rec3l;
label define i29rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i29rec4  i29rec4l;
label define i29rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i29amt1  i29amt1l;
label define i29amt1l
	-0009       "Not in universe"               
;
label values i29amt2  i29amt2l;
label define i29amt2l
	-0009       "Not in universe"               
;
label values i29amt3  i29amt3l;
label define i29amt3l
	-0009       "Not in universe"               
;
label values i29amt4  i29amt4l;
label define i29amt4l
	-0009       "Not in universe"               
;
label values i29imp01 i29imp0d;
label define i29imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i29imp02 i29imp0k;
label define i29imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i29imp03 i29imp0l;
label define i29imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i29imp04 i29imp0m;
label define i29imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i30rec1  i30rec1l;
label define i30rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i30rec2  i30rec2l;
label define i30rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i30rec3  i30rec3l;
label define i30rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i30rec4  i30rec4l;
label define i30rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i30amt1  i30amt1l;
label define i30amt1l
	-0009       "Not in universe"               
;
label values i30amt2  i30amt2l;
label define i30amt2l
	-0009       "Not in universe"               
;
label values i30amt3  i30amt3l;
label define i30amt3l
	-0009       "Not in universe"               
;
label values i30amt4  i30amt4l;
label define i30amt4l
	-0009       "Not in universe"               
;
label values i30imp01 i30imp0d;
label define i30imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i30imp02 i30imp0k;
label define i30imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i30imp03 i30imp0l;
label define i30imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i30imp04 i30imp0m;
label define i30imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i31rec1  i31rec1l;
label define i31rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i31rec2  i31rec2l;
label define i31rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i31rec3  i31rec3l;
label define i31rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i31rec4  i31rec4l;
label define i31rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i31amt1  i31amt1l;
label define i31amt1l
	-0009       "Not in universe"               
;
label values i31amt2  i31amt2l;
label define i31amt2l
	-0009       "Not in universe"               
;
label values i31amt3  i31amt3l;
label define i31amt3l
	-0009       "Not in universe"               
;
label values i31amt4  i31amt4l;
label define i31amt4l
	-0009       "Not in universe"               
;
label values i31imp01 i31imp0d;
label define i31imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i31imp02 i31imp0k;
label define i31imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i31imp03 i31imp0l;
label define i31imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i31imp04 i31imp0m;
label define i31imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i32rec1  i32rec1l;
label define i32rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i32rec2  i32rec2l;
label define i32rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i32rec3  i32rec3l;
label define i32rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i32rec4  i32rec4l;
label define i32rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i32amt1  i32amt1l;
label define i32amt1l
	-0009       "Not in universe"               
;
label values i32amt2  i32amt2l;
label define i32amt2l
	-0009       "Not in universe"               
;
label values i32amt3  i32amt3l;
label define i32amt3l
	-0009       "Not in universe"               
;
label values i32amt4  i32amt4l;
label define i32amt4l
	-0009       "Not in universe"               
;
label values i32imp01 i32imp0d;
label define i32imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i32imp02 i32imp0k;
label define i32imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i32imp03 i32imp0l;
label define i32imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i32imp04 i32imp0m;
label define i32imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i34rec1  i34rec1l;
label define i34rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i34rec2  i34rec2l;
label define i34rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i34rec3  i34rec3l;
label define i34rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i34rec4  i34rec4l;
label define i34rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i34amt1  i34amt1l;
label define i34amt1l
	-0009       "Not in universe"               
;
label values i34amt2  i34amt2l;
label define i34amt2l
	-0009       "Not in universe"               
;
label values i34amt3  i34amt3l;
label define i34amt3l
	-0009       "Not in universe"               
;
label values i34amt4  i34amt4l;
label define i34amt4l
	-0009       "Not in universe"               
;
label values i34imp01 i34imp0d;
label define i34imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i34imp02 i34imp0k;
label define i34imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i34imp03 i34imp0l;
label define i34imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i34imp04 i34imp0m;
label define i34imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i35rec1  i35rec1l;
label define i35rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i35rec2  i35rec2l;
label define i35rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i35rec3  i35rec3l;
label define i35rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i35rec4  i35rec4l;
label define i35rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i35amt1  i35amt1l;
label define i35amt1l
	-0009       "Not in universe"               
;
label values i35amt2  i35amt2l;
label define i35amt2l
	-0009       "Not in universe"               
;
label values i35amt3  i35amt3l;
label define i35amt3l
	-0009       "Not in universe"               
;
label values i35amt4  i35amt4l;
label define i35amt4l
	-0009       "Not in universe"               
;
label values i35imp01 i35imp0d;
label define i35imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i35imp02 i35imp0k;
label define i35imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i35imp03 i35imp0l;
label define i35imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i35imp04 i35imp0m;
label define i35imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i36rec1  i36rec1l;
label define i36rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i36rec2  i36rec2l;
label define i36rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "no"                            
;
label values i36rec3  i36rec3l;
label define i36rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "no"                            
;
label values i36rec4  i36rec4l;
label define i36rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i36amt1  i36amt1l;
label define i36amt1l
	-0009       "Not in universe"               
;
label values i36amt2  i36amt2l;
label define i36amt2l
	-0009       "Not in universe"               
;
label values i36amt3  i36amt3l;
label define i36amt3l
	-0009       "Not in universe"               
;
label values i36amt4  i36amt4l;
label define i36amt4l
	-0009       "Not in universe"               
;
label values i36imp01 i36imp0d;
label define i36imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i36imp02 i36imp0k;
label define i36imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i36imp03 i36imp0l;
label define i36imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i36imp04 i36imp0m;
label define i36imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i37rec1  i37rec1l;
label define i37rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i37rec2  i37rec2l;
label define i37rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i37rec3  i37rec3l;
label define i37rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i37rec4  i37rec4l;
label define i37rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i37amt1  i37amt1l;
label define i37amt1l
	-0009       "Not in universe"               
;
label values i37amt2  i37amt2l;
label define i37amt2l
	-0009       "Not in universe"               
;
label values i37amt3  i37amt3l;
label define i37amt3l
	-0009       "Not in universe"               
;
label values i37amt4  i37amt4l;
label define i37amt4l
	-0009       "Not in universe"               
;
label values i37imp01 i37imp0d;
label define i37imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i37imp02 i37imp0k;
label define i37imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i37imp03 i37imp0l;
label define i37imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i37imp04 i37imp0m;
label define i37imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i38rec1  i38rec1l;
label define i38rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i38rec2  i38rec2l;
label define i38rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i38rec3  i38rec3l;
label define i38rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i38rec4  i38rec4l;
label define i38rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i38amt1  i38amt1l;
label define i38amt1l
	-0009       "Not in universe"               
;
label values i38amt2  i38amt2l;
label define i38amt2l
	-0009       "Not in universe"               
;
label values i38amt3  i38amt3l;
label define i38amt3l
	-0009       "Not in universe"               
;
label values i38amt4  i38amt4l;
label define i38amt4l
	-0009       "Not in universe"               
;
label values i38imp01 i38imp0d;
label define i38imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i38imp02 i38imp0k;
label define i38imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i38imp03 i38imp0l;
label define i38imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i38imp04 i38imp0m;
label define i38imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i40rec1  i40rec1l;
label define i40rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i40rec2  i40rec2l;
label define i40rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i40rec3  i40rec3l;
label define i40rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i40rec4  i40rec4l;
label define i40rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i40amt1  i40amt1l;
label define i40amt1l
	-0009       "Not in universe"               
;
label values i40amt2  i40amt2l;
label define i40amt2l
	-0009       "Not in universe"               
;
label values i40amt3  i40amt3l;
label define i40amt3l
	-0009       "Not in universe"               
;
label values i40amt4  i40amt4l;
label define i40amt4l
	-0009       "Not in universe"               
;
label values i40imp01 i40imp0d;
label define i40imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i40imp02 i40imp0k;
label define i40imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i40imp03 i40imp0l;
label define i40imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i40imp04 i40imp0m;
label define i40imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i50rec1  i50rec1l;
label define i50rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i50rec2  i50rec2l;
label define i50rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i50rec3  i50rec3l;
label define i50rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i50rec4  i50rec4l;
label define i50rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i50amt1  i50amt1l;
label define i50amt1l
	-0009       "Not in universe"               
;
label values i50amt2  i50amt2l;
label define i50amt2l
	-0009       "Not in universe"               
;
label values i50amt3  i50amt3l;
label define i50amt3l
	-0009       "Not in universe"               
;
label values i50amt4  i50amt4l;
label define i50amt4l
	-0009       "Not in universe"               
;
label values i50imp01 i50imp0d;
label define i50imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i50imp02 i50imp0k;
label define i50imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i50imp03 i50imp0l;
label define i50imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i50imp04 i50imp0m;
label define i50imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i51rec1  i51rec1l;
label define i51rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i51rec2  i51rec2l;
label define i51rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "no"                            
;
label values i51rec3  i51rec3l;
label define i51rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i51rec4  i51rec4l;
label define i51rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i51amt1  i51amt1l;
label define i51amt1l
	-0009       "Not in universe"               
;
label values i51amt2  i51amt2l;
label define i51amt2l
	-0009       "Not in universe"               
;
label values i51amt3  i51amt3l;
label define i51amt3l
	-0009       "Not in universe"               
;
label values i51amt4  i51amt4l;
label define i51amt4l
	-0009       "Not in universe"               
;
label values i51imp01 i51imp0d;
label define i51imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i51imp02 i51imp0k;
label define i51imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i51imp03 i51imp0l;
label define i51imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i51imp04 i51imp0m;
label define i51imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i52rec1  i52rec1l;
label define i52rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i52rec2  i52rec2l;
label define i52rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i52rec3  i52rec3l;
label define i52rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i52rec4  i52rec4l;
label define i52rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i52amt1  i52amt1l;
label define i52amt1l
	-0009       "Not in universe"               
;
label values i52amt2  i52amt2l;
label define i52amt2l
	-0009       "Not in universe"               
;
label values i52amt3  i52amt3l;
label define i52amt3l
	-0009       "Not in universe"               
;
label values i52amt4  i52amt4l;
label define i52amt4l
	-0009       "Not in universe"               
;
label values i52imp01 i52imp0d;
label define i52imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i52imp02 i52imp0k;
label define i52imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i52imp03 i52imp0l;
label define i52imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i52imp04 i52imp0m;
label define i52imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i53rec1  i53rec1l;
label define i53rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i53rec2  i53rec2l;
label define i53rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i53rec3  i53rec3l;
label define i53rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i53rec4  i53rec4l;
label define i53rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i53amt1  i53amt1l;
label define i53amt1l
	-0009       "Not in universe"               
;
label values i53amt2  i53amt2l;
label define i53amt2l
	-0009       "Not in universe"               
;
label values i53amt3  i53amt3l;
label define i53amt3l
	-0009       "Not in universe"               
;
label values i53amt4  i53amt4l;
label define i53amt4l
	-0009       "Not in universe"               
;
label values i53imp01 i53imp0d;
label define i53imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i53imp02 i53imp0k;
label define i53imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i53imp03 i53imp0l;
label define i53imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i53imp04 i53imp0m;
label define i53imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i54rec1  i54rec1l;
label define i54rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i54rec2  i54rec2l;
label define i54rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i54rec3  i54rec3l;
label define i54rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i54rec4  i54rec4l;
label define i54rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i54amt1  i54amt1l;
label define i54amt1l
	-0009       "Not in universe"               
;
label values i54amt2  i54amt2l;
label define i54amt2l
	-0009       "Not in universe"               
;
label values i54amt3  i54amt3l;
label define i54amt3l
	-0009       "Not in universe"               
;
label values i54amt4  i54amt4l;
label define i54amt4l
	-0009       "Not in universe"               
;
label values i54imp01 i54imp0d;
label define i54imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i54imp02 i54imp0k;
label define i54imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i54imp03 i54imp0l;
label define i54imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i54imp04 i54imp0m;
label define i54imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i55rec1  i55rec1l;
label define i55rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i55rec2  i55rec2l;
label define i55rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i55rec3  i55rec3l;
label define i55rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i55rec4  i55rec4l;
label define i55rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i55amt1  i55amt1l;
label define i55amt1l
	-0009       "Not in universe"               
;
label values i55amt2  i55amt2l;
label define i55amt2l
	-0009       "Not in universe"               
;
label values i55amt3  i55amt3l;
label define i55amt3l
	-0009       "Not in universe"               
;
label values i55amt4  i55amt4l;
label define i55amt4l
	-0009       "Not in universe"               
;
label values i55imp01 i55imp0d;
label define i55imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i55imp02 i55imp0k;
label define i55imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i55imp03 i55imp0l;
label define i55imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i55imp04 i55imp0m;
label define i55imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i56rec1  i56rec1l;
label define i56rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i56rec2  i56rec2l;
label define i56rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i56rec3  i56rec3l;
label define i56rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i56rec4  i56rec4l;
label define i56rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i56amt1  i56amt1l;
label define i56amt1l
	-0009       "Not in universe"               
;
label values i56amt2  i56amt2l;
label define i56amt2l
	-0009       "Not in universe"               
;
label values i56amt3  i56amt3l;
label define i56amt3l
	-0009       "Not in universe"               
;
label values i56amt4  i56amt4l;
label define i56amt4l
	-0009       "Not in universe"               
;
label values i56imp01 i56imp0d;
label define i56imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i56imp02 i56imp0k;
label define i56imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i56imp03 i56imp0l;
label define i56imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i56imp04 i56imp0m;
label define i56imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i75rec1  i75rec1l;
label define i75rec1l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i75rec2  i75rec2l;
label define i75rec2l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i75rec3  i75rec3l;
label define i75rec3l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i75rec4  i75rec4l;
label define i75rec4l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i75amt1  i75amt1l;
label define i75amt1l
	-0009       "Not in universe"               
;
label values i75amt2  i75amt2l;
label define i75amt2l
	-0009       "Not in universe"               
;
label values i75amt3  i75amt3l;
label define i75amt3l
	-0009       "Not in universe"               
;
label values i75amt4  i75amt4l;
label define i75amt4l
	-0009       "Not in universe"               
;
label values i75imp01 i75imp0d;
label define i75imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i75imp02 i75imp0k;
label define i75imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i75imp03 i75imp0l;
label define i75imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i75imp04 i75imp0m;
label define i75imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values j100yn_1 j100yn_d;
label define j100yn_d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j100yn_2 j100yn_k;
label define j100yn_k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j100yn_3 j100yn_l;
label define j100yn_l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j100yn_4 j100yn_m;
label define j100yn_m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jint1001 jint100d;
label define jint100d
	-009        "Not in universe"               
	0           "None"                          
;
label values jint1002 jint100k;
label define jint100k
	-009        "Not in universe"               
	0           "None"                          
;
label values jint1003 jint100l;
label define jint100l
	-009        "Not in universe"               
	0           "None"                          
;
label values jint1004 jint100m;
label define jint100m
	-009        "Not in universe"               
	0           "None"                          
;
label values o100yn_1 o100yn_d;
label define o100yn_d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o100yn_2 o100yn_k;
label define o100yn_k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o100yn_3 o100yn_l;
label define o100yn_l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o100yn_4 o100yn_m;
label define o100yn_m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values oint1001 oint100d;
label define oint100d
	-009        "Not in universe"               
	0           "None"                          
;
label values oint1002 oint100k;
label define oint100k
	-009        "Not in universe"               
	0           "None"                          
;
label values oint1003 oint100l;
label define oint100l
	-009        "Not in universe"               
	0           "None"                          
;
label values oint1004 oint100m;
label define oint100m
	-009        "Not in universe"               
	0           "None"                          
;
label values jcalc100 jcalc10d;
label define jcalc10d
	0           "No, not calculated"            
	1           "Yes, interest was calculated"  
	9           "Not in universe"               
;
label values ocalc100 ocalc10d;
label define ocalc10d
	0           "No, not calculated"            
	1           "Yes, calculated"               
	9           "Not in universe"               
;
label values j104yn_1 j104yn_d;
label define j104yn_d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j104yn_2 j104yn_k;
label define j104yn_k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j104yn_3 j104yn_l;
label define j104yn_l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j104yn_4 j104yn_m;
label define j104yn_m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jint1041 jint104d;
label define jint104d
	-009        "Not in universe"               
	0           "None"                          
;
label values jint1042 jint104k;
label define jint104k
	-009        "Not in universe"               
	0           "None"                          
;
label values jint1043 jint104l;
label define jint104l
	-009        "Not in universe"               
	0           "None"                          
;
label values jint1044 jint104m;
label define jint104m
	-009        "Not in universe"               
	0           "None"                          
;
label values o104yn_1 o104yn_d;
label define o104yn_d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o104yn_2 o104yn_k;
label define o104yn_k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o104yn_3 o104yn_l;
label define o104yn_l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o104yn_4 o104yn_m;
label define o104yn_m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values oint1041 oint104d;
label define oint104d
	-009        "Not in universe"               
	0           "None"                          
;
label values oint1042 oint104k;
label define oint104k
	-009        "Not in universe"               
	0           "None"                          
;
label values oint1043 oint104l;
label define oint104l
	-009        "Not in universe"               
	0           "None"                          
;
label values oint1044 oint104m;
label define oint104m
	-009        "Not in universe"               
	0           "None"                          
;
label values jcalc104 jcalc10k;
label define jcalc10k
	0           "No, not calculated"            
	1           "Yes, interest was calculated"  
	9           "Not in universe"               
;
label values ocalc104 ocalc10k;
label define ocalc10k
	0           "No, not calculated"            
	1           "Yes, calculated"               
	9           "Not in universe"               
;
label values j110ryn1 j110rynd;
label define j110rynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j110ryn2 j110rynk;
label define j110rynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j110ryn3 j110rynl;
label define j110rynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j110ryn4 j110rynm;
label define j110rynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jdir1101 jdir110d;
label define jdir110d
	-009        "Not in universe"               
	0           "None"                          
;
label values jdir1102 jdir110k;
label define jdir110k
	-009        "Not in universe"               
	0           "None"                          
;
label values jdir1103 jdir110l;
label define jdir110l
	-009        "Not in universe"               
	0           "None"                          
;
label values jdir1104 jdir110m;
label define jdir110m
	-009        "Not in universe"               
	0           "None"                          
;
label values o110ryn1 o110rynd;
label define o110rynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o110ryn2 o110rynk;
label define o110rynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o110ryn3 o110rynl;
label define o110rynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o110ryn4 o110rynm;
label define o110rynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values odir1101 odir110d;
label define odir110d
	-009        "Not in universe"               
	0           "None"                          
;
label values odir1102 odir110k;
label define odir110k
	-009        "Not in universe"               
	0           "None"                          
;
label values odir1103 odir110l;
label define odir110l
	-009        "Not in universe"               
	0           "None"                          
;
label values odir1104 odir110m;
label define odir110m
	-009        "Not in universe"               
	0           "None"                          
;
label values j110cyn1 j110cynd;
label define j110cynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j110cyn2 j110cynk;
label define j110cynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j110cyn3 j110cynl;
label define j110cynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j110cyn4 j110cynm;
label define j110cynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jdic1101 jdic110d;
label define jdic110d
	-009        "Not in universe"               
	0           "None"                          
;
label values jdic1102 jdic110k;
label define jdic110k
	-009        "Not in universe"               
	0           "None"                          
;
label values jdic1103 jdic110l;
label define jdic110l
	-009        "Not in universe"               
	0           "None"                          
;
label values jdic1104 jdic110m;
label define jdic110m
	-009        "Not in universe"               
	0           "None"                          
;
label values o110cyn1 o110cynd;
label define o110cynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o110cyn2 o110cynk;
label define o110cynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o110cyn3 o110cynl;
label define o110cynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o110cyn4 o110cynm;
label define o110cynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values odic1101 odic110d;
label define odic110d
	-009        "Not in universe"               
	0           "None"                          
;
label values odic1102 odic110k;
label define odic110k
	-009        "Not in universe"               
	0           "None"                          
;
label values odic1103 odic110l;
label define odic110l
	-009        "Not in universe"               
	0           "None"                          
;
label values odic1104 odic110m;
label define odic110m
	-009        "Not in universe"               
	0           "None"                          
;
label values j120yn_1 j120yn_d;
label define j120yn_d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j120yn_2 j120yn_k;
label define j120yn_k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j120yn_3 j120yn_l;
label define j120yn_l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values j120yn_4 j120yn_m;
label define j120yn_m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jrnt1201 jrnt120d;
label define jrnt120d
	-9999       "Not in universe"               
	0           "None"                          
;
label values jrnt1202 jrnt120k;
label define jrnt120k
	-9999       "Not in universe"               
	0           "None"                          
;
label values jrnt1203 jrnt120l;
label define jrnt120l
	-9999       "Not in universe"               
	0           "None"                          
;
label values jrnt1204 jrnt120m;
label define jrnt120m
	-9999       "Not in universe"               
	0           "None"                          
;
label values o120yn_1 o120yn_d;
label define o120yn_d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o120yn_2 o120yn_k;
label define o120yn_k
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o120yn_3 o120yn_l;
label define o120yn_l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o120yn_4 o120yn_m;
label define o120yn_m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ornt1201 ornt120d;
label define ornt120d
	-9999       "Not in universe"               
	0           "None"                          
;
label values ornt1202 ornt120k;
label define ornt120k
	-9999       "Not in universe"               
	0           "None"                          
;
label values ornt1203 ornt120l;
label define ornt120l
	-9999       "Not in universe"               
	0           "None"                          
;
label values ornt1204 ornt120m;
label define ornt120m
	-9999       "Not in universe"               
	0           "None"                          
;
label values oj120yn1 oj120ynd;
label define oj120ynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values oj120yn2 oj120ynk;
label define oj120ynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values oj120yn3 oj120ynl;
label define oj120ynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values oj120yn4 oj120ynm;
label define oj120ynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ojrt1201 ojrt120d;
label define ojrt120d
	-9999       "Not in universe"               
	0           "None"                          
;
label values ojrt1202 ojrt120k;
label define ojrt120k
	-9999       "Not in universe"               
	0           "None"                          
;
label values ojrt1203 ojrt120l;
label define ojrt120l
	-9999       "Not in universe"               
	0           "None"                          
;
label values ojrt1204 ojrt120m;
label define ojrt120m
	-9999       "Not in universe"               
	0           "None"                          
;
label values jmtgnyn1 jmtgnynd;
label define jmtgnynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmtgnyn2 jmtgnynk;
label define jmtgnynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmtgnyn3 jmtgnynl;
label define jmtgnynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmtgnyn4 jmtgnynm;
label define jmtgnynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmortyn1 jmortynd;
label define jmortynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmortyn2 jmortynk;
label define jmortynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmortyn3 jmortynl;
label define jmortynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmortyn4 jmortynm;
label define jmortynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values jmam1301 jmam130d;
label define jmam130d
	-009        "Not in universe"               
	0           "None"                          
;
label values jmam1302 jmam130k;
label define jmam130k
	-009        "Not in universe"               
	0           "None"                          
;
label values jmam1303 jmam130l;
label define jmam130l
	-009        "Not in universe"               
	0           "None"                          
;
label values jmam1304 jmam130m;
label define jmam130m
	-009        "Not in universe"               
	0           "None"                          
;
label values omtgnyn1 omtgnynd;
label define omtgnynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omtgnyn2 omtgnynk;
label define omtgnynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omtgnyn3 omtgnynl;
label define omtgnynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omtgnyn4 omtgnynm;
label define omtgnynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omortyn1 omortynd;
label define omortynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omortyn2 omortynk;
label define omortynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omortyn3 omortynl;
label define omortynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omortyn4 omortynm;
label define omortynm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values omam1301 omam130d;
label define omam130d
	-009        "Not in universe"               
	0           "None"                          
;
label values omam1302 omam130k;
label define omam130k
	-009        "Not in universe"               
	0           "None"                          
;
label values omam1303 omam130l;
label define omam130l
	-009        "Not in universe"               
	0           "None"                          
;
label values omam1304 omam130m;
label define omam130m
	-009        "Not in universe"               
	0           "None"                          
;
label values o4050yn1 o4050ynd;
label define o4050ynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o4040yn2 o4040ynd;
label define o4040ynd
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o4050yn3 o4050ynk;
label define o4050ynk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values o4050yn4 o4050ynl;
label define o4050ynl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ram40501 ram4050d;
label define ram4050d
	-9999       "Not in universe"               
	0           "None"                          
;
label values ram40502 ram4050k;
label define ram4050k
	-9999       "Not in universe"               
	0           "None"                          
;
label values ram40503 ram4050l;
label define ram4050l
	-9999       "Not in universe"               
	0           "None"                          
;
label values ram40504 ram4050m;
label define ram4050m
	-9999       "Not in universe"               
	0           "None"                          
;
label values sc4300   sc4300l;
label define sc4300l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4302   sc4302l;
label define sc4302l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4304   sc4304l;
label define sc4304l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4306   sc4306l;
label define sc4306l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4308   sc4308l;
label define sc4308l 
	0           "Not in universe"               
	1           "No spouse in household - skip" 
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values sc4310   sc4310l;
label define sc4310l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4320"           
;
label values sc4312   sc4312l;
label define sc4312l 
	0           "Not in universe"               
;
label values sc4318   sc4318l;
label define sc4318l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values sc4320   sc4320l;
label define sc4320l 
	0           "Not in universe"               
;
label values sc4400   sc4400l;
label define sc4400l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4402   sc4402l;
label define sc4402l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4404   sc4404l;
label define sc4404l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4406   sc4406l;
label define sc4406l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4408   sc4408l;
label define sc4408l 
	0           "Not in universe"               
	1           "No spouse in household - skip" 
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values sc4410   sc4410l;
label define sc4410l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4420"           
;
label values sc4412   sc4412l;
label define sc4412l 
	0           "Not in universe"               
;
label values sc4418   sc4418l;
label define sc4418l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values sc4420   sc4420l;
label define sc4420l 
	0           "Not in universe"               
;
label values sc4500   sc4500l;
label define sc4500l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4512"           
;
label values sc4502   sc4502l;
label define sc4502l 
	0           "Not in universe"               
	1           "No spouse in household - skip" 
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values sc4504   sc4504l;
label define sc4504l 
	-0009       "Not in universe"               
	0           "None - skip to SC4508"         
;
label values sc4508   sc4508l;
label define sc4508l 
	-0009       "Not in universe"               
	0           "None - skip to SC4512"         
;
label values sc4512   sc4512l;
label define sc4512l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values sc4514   sc4514l;
label define sc4514l 
	0           "Not in universe"               
	1           "No spouse in household - skip" 
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values sc4516   sc4516l;
label define sc4516l 
	-0009       "Not in universe"               
	0           "None"                          
;
label values sc4518   sc4518l;
label define sc4518l 
	-0009       "Not in universe"               
	0           "None - skip to next ISS code"  
;
label values sc4600   sc4600l;
label define sc4600l 
	0           "Not in universe"               
	1           "No spouse in household - skip" 
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values sc4602   sc4602l;
label define sc4602l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4610"           
;
label values sc4604   sc4604l;
label define sc4604l 
	0           "Not in universe"               
;
label values sc4606   sc4606l;
label define sc4606l 
	0           "Not in universe"               
;
label values sc4610   sc4610l;
label define sc4610l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4618"           
;
label values sc4612   sc4612l;
label define sc4612l 
	0           "Not in universe"               
;
label values sc4614   sc4614l;
label define sc4614l 
	0           "Not in universe"               
;
label values sc4618   sc4618l;
label define sc4618l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to next ISS code"    
;
label values sc4620   sc4620l;
label define sc4620l 
	0           "Not in universe"               
;
label values sc4700   sc4700l;
label define sc4700l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4702   sc4702l;
label define sc4702l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4704   sc4704l;
label define sc4704l 
	0           "Not marked as a type of asset" 
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4706   sc4706l;
label define sc4706l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4720"           
;
label values sc4708   sc4708l;
label define sc4708l 
	0           "Not in universe"               
	1           "No spouse in household - skip" 
	2           "Interview for spouse not yet"  
	3           "Interview for spouse already"  
;
label values sc4710   sc4710l;
label define sc4710l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4716"           
;
label values sc4712   sc4712l;
label define sc4712l 
	-0009       "Not in universe"               
	0           "None"                          
;
label values sc4714   sc4714l;
label define sc4714l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC4718"           
;
label values sc4716   sc4716l;
label define sc4716l 
	-0009       "Not in universe"               
	0           "None"                          
;
label values sc4718   sc4718l;
label define sc4718l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values sc4720   sc4720l;
label define sc4720l 
	-99999      "Not in universe"               
	0           "None"                          
;
label values g2_imp01 g2_imp0d;
label define g2_imp0d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp02 g2_imp0k;
label define g2_imp0k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp03 g2_imp0l;
label define g2_imp0l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp04 g2_imp0m;
label define g2_imp0m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp05 g2_imp0n;
label define g2_imp0n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp06 g2_imp0o;
label define g2_imp0o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp07 g2_imp0p;
label define g2_imp0p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp08 g2_imp0q;
label define g2_imp0q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp09 g2_imp0r;
label define g2_imp0r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp10 g2_imp1d;
label define g2_imp1d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp11 g2_imp1k;
label define g2_imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp12 g2_imp1l;
label define g2_imp1l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp13 g2_imp1m;
label define g2_imp1m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp14 g2_imp1n;
label define g2_imp1n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp15 g2_imp1o;
label define g2_imp1o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp16 g2_imp1p;
label define g2_imp1p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp17 g2_imp1q;
label define g2_imp1q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp18 g2_imp1r;
label define g2_imp1r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp19 g2_imp1s;
label define g2_imp1s
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp20 g2_imp2d;
label define g2_imp2d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp21 g2_imp2k;
label define g2_imp2k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp22 g2_imp2l;
label define g2_imp2l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp23 g2_imp2m;
label define g2_imp2m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp24 g2_imp2n;
label define g2_imp2n
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp25 g2_imp2o;
label define g2_imp2o
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp26 g2_imp2p;
label define g2_imp2p
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values g2_imp27 g2_imp2q;
label define g2_imp2q
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values version  version;
label define version 
	1           "Original version"              
;
label values jcalb100 jcalb10n;
label define jcalb10n
	0           "No, not calculated"            
	1           "Yes, balance was calculated"   
;
label values ocalb100 ocalb10n;
label define ocalb10n
	0           "No, not calculated"            
	1           "Yes, balance was calculated"   
;
label values jcalb104 jcalb10k;
label define jcalb10k
	0           "No, not calculated"            
	1           "Yes, balance was calculated"   
;
label values ocalb104 ocalb10k;
label define ocalb10k
	0           "No, not calculated"            
	1           "Yes, balance was calculated"   
;
label values ws1_2002 ws1_200n;
label define ws1_200n
	0           "Not answered"                  
	-9          "Not in universe"               
;
label values ws1_2044 ws1_204n;
label define ws1_204n
	0           "Not in universe"               
	1           "Yes - skip to SC2048"          
	2           "No"                            
;
label values ws1_2046 ws1_204l;
label define ws1_204l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws2_2102 ws2_210n;
label define ws2_210n
	0           "Not answered"                  
	-9          "Not in universe"               
;
label values ws2_2144 ws2_214n;
label define ws2_214n
	0           "Not in universe"               
	1           "Yes - skip to SC2148"          
	2           "No"                            
;
label values ws2_2146 ws2_214l;
label define ws2_214l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se1_2202 se1_220n;
label define se1_220n
	0           "Not answered"                  
	-9          "Not in universe"               
;
label values se2_2202 se2_220n;
label define se2_220n
	0           "Not answered"                  
	-9          "Not in universe"               
;
label values i41recp1 i41recpn;
label define i41recpn
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i41recp2 i41recpk;
label define i41recpk
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i41recp3 i41recpl;
label define i41recpl
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i41recp4 i41recpm;
label define i41recpm
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values i41amt1  i41amt1l;
label define i41amt1l
	0           "None"                          
	-9          "Not in universe"               
;
label values i41amt2  i41amt2l;
label define i41amt2l
	0           "None"                          
	-9          "Not in universe"               
;
label values i41amt3  i41amt3l;
label define i41amt3l
	0           "None"                          
	-9          "Not in universe"               
;
label values i41amt4  i41amt4l;
label define i41amt4l
	0           "None"                          
	-9          "Not in universe"               
;
label values i41impo1 i41impon;
label define i41impon
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i41impo2 i41impok;
label define i41impok
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i41impo3 i41impol;
label define i41impol
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values i41impo4 i41impom;
label define i41impom
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h1_type  h1_type;
label define h1_type 
	0           "Not in household"              
	1           "Married couple family HH"      
	2           "Male householder family HH"    
	3           "Female householder family HH"  
	4           "Male householder nonfamily HH" 
	5           "Female householder nonfamily HH"
	6           "Group quarters"                
;
label values u1_reint u1_reint;
label define u1_reint
	0           "Not answered"                  
	1           "Observed"                      
	2           "Reinterview"                   
;
label values h2_type  h2_type;
label define h2_type 
	0           "Not in household"              
	1           "Married couple family HH"      
	2           "Male householder family HH"    
	3           "Female householder family HH"  
	4           "Male householder nonfamily HH" 
	5           "Female householder nonfamily HH"
	6           "Group quarters"                
;
label values u2_reint u2_reint;
label define u2_reint
	0           "Not answered"                  
	1           "Observed"                      
	2           "Reinterview"                   
;
label values h3_type  h3_type;
label define h3_type 
	0           "Not in household"              
	1           "Married couple family HH"      
	2           "Male householder family HH"    
	3           "Female householder family HH"  
	4           "Male householder nonfamily HH" 
	5           "Female householder nonfamily HH"
	6           "Group quarters"                
;
label values u3_reint u3_reint;
label define u3_reint
	0           "Not answered"                  
	1           "Observed"                      
	2           "Reinterview"                   
;
label values h4_type  h4_type;
label define h4_type 
	0           "Not in household"              
	1           "Married couple family HH"      
	2           "Male householder family HH"    
	3           "Female householder family HH"  
	4           "Male householder nonfamily HH" 
	5           "Female householder nonfamily HH"
	6           "Group quarters"                
;
label values u4_reint u4_reint;
label define u4_reint
	0           "Not answered"                  
	1           "Observed"                      
	2           "Reinterview"                   
;
label values h5_type  h5_type;
label define h5_type 
	0           "Not in household"              
	1           "Married couple family HH"      
	2           "Male householder family HH"    
	3           "Female householder family HH"  
	4           "Male householder nonfamily HH" 
	5           "Female householder nonfamily HH"
	6           "Group quarters"                
;
label values sc0064   sc0064l;
label define sc0064l 
	0           "Not in universe"               
	5           "1985 panel"                    
;
label values sc0066   sc0066l;
label define sc0066l 
	0           "Not in universe"               
;
label values sc0068   sc0068l;
label define sc0068l 
	0           "Not in universe"               
;
label values medcode  medcode;
label define medcode 
	0           "Not in universe"               
	1           "Retired or disabled worker"    
	2           "Spouse or retired or"          
	3           "Widow or retired or disabled"  
	4           "Adult disabled as a child"     
	5           "Uninsured"                     
	7           "Other or invalid code"         
	9           "Missing code"                  
;
label values sc1231   sc1231l;
label define sc1231l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1233"           
;
label values sc1232   sc1232l;
label define sc1232l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1239"           
;
label values sc1233   sc1233l;
label define sc1233l 
	0           "Not in universe"               
	-5          "All weeks working"             
;
label values sc1234   sc1234l;
label define sc1234l 
	0           "Not marked as a month"         
;
label values sc1235   sc1235l;
label define sc1235l 
	0           "Not marked as a month"         
;
label values sc1236   sc1236l;
label define sc1236l 
	0           "Not marked as a month"         
;
label values sc1237   sc1237l;
label define sc1237l 
	0           "Not marked as a month"         
;
label values sc1238   sc1238l;
label define sc1238l 
	0           "Not in universe"               
	1           "Could not find a full-time"    
	2           "Wanted to work part-time"      
	3           "Health condition or"           
	4           "Normal working hours are"      
	5           "Slack work or material"        
	6           "Other"                         
;
label values sc1656   sc1656l;
label define sc1656l 
	0           "Not in universe"               
	1           "Yes, full-time"                
	2           "Yes, part-time"                
	3           "No - skip to SC1694"           
;
label values sc1658   sc1658l;
label define sc1658l 
	0           "Not marked as a month"         
	1           "Marked as a month"             
;
label values sc1660   sc1660l;
label define sc1660l 
	0           "Not marked as a month"         
	1           "Marked as a month"             
;
label values sc1662   sc1662l;
label define sc1662l 
	0           "Not marked as a month"         
	1           "Marked as a month"             
;
label values sc1664   sc1664l;
label define sc1664l 
	0           "Not marked as a month"         
	1           "Marked as a month"             
;
label values sc1666   sc1666l;
label define sc1666l 
	0           "Not marked as a month"         
	1           "Marked as a month"             
;
label values sc1668   sc1668l;
label define sc1668l 
	0           "Not in universe"               
	1           "Elementary grades 1-8"         
	2           "High school grades 9-12"       
	3           "College year 1"                
	4           "College year 2"                
	5           "College year 3"                
	6           "College year 4"                
	7           "College year 5"                
	8           "College year 6"                
	9           "Vocational school"             
	10          "Technical school"              
	11          "Business school"               
;
label values sc1670   sc1670l;
label define sc1670l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1694"           
;
label values sc1672   sc1672l;
label define sc1672l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1674   sc1674l;
label define sc1674l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1676   sc1676l;
label define sc1676l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1678   sc1678l;
label define sc1678l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1680   sc1680l;
label define sc1680l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1682   sc1682l;
label define sc1682l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1684   sc1684l;
label define sc1684l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1686   sc1686l;
label define sc1686l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1688   sc1688l;
label define sc1688l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1690   sc1690l;
label define sc1690l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc1692   sc1692l;
label define sc1692l 
	0           "Not marked as a kind of"       
	1           "Marked as a kind of"           
;
label values sc0914   sc0914l;
label define sc0914l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC0918"           
	9           "Not answered"                  
	-1          "DK - skip to SC0918"           
;
label values sc0916   sc0916l;
label define sc0916l 
	0           "Not in universe"               
	1           "Armed Forces Barracks"         
	2           "Outside the United States"     
	3           "Nonhousehold setting"          
	9           "Not answered"                  
;
label values sc0918   sc0918l;
label define sc0918l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1000"           
;
label values sc0920   sc0920l;
label define sc0920l 
	0           "Not in universe"               
	1           "Yes - skip to SC1000"          
	2           "No"                            
	9           "Not answered"                  
	-1          "DK - skip to SC1000"           
	-2          "Ref. - skip to SC1000"         
;
label values sc0922   sc0922l;
label define sc0922l 
	0           "Not in universe"               
	1           "Yes - skip to SC1000"          
	2           "No"                            
	9           "Not answered"                  
	-1          "DK - skip to SC0928"           
	-2          "Ref. - skip to SC0928"         
;
label values sc0924   sc0924l;
label define sc0924l 
	0           "Not in universe"               
	-1          "DK"                            
	-2          "Ref."                          
	-3          "None"                          
;
label values sc0926   sc0926l;
label define sc0926l 
	0           "Not in universe"               
	99          "Not answered"                  
	-1          "DK"                            
	-2          "Ref"                           
	-3          "None"                          
;
label values sc0928   sc0928l;
label define sc0928l 
	0           "Not in universe"               
	1           "Yes - skip to SC1000"          
	2           "No"                            
	9           "Not answered"                  
	-1          "DK - skip to SC1000"           
	-2          "REF. - skip to SC1000"         
;
label values sc0930   sc0930l;
label define sc0930l 
	0           "Not in universe"               
	1           "Spouse"                        
	2           "Other"                         
	9           "Not answered"                  
	-1          "DK"                            
	-2          "Ref."                          
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
