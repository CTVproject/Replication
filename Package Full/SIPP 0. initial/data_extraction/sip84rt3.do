log using sip84rt3, text replace
set mem 1000m

/*------------------------------------------------

  This program reads the 1984 SIPP Wave 3 Rectangular and Topical Data File 

****************************************************************
*
* NOTE: This complete dataset has over more than 2,047 variables,
* the maximum number of variables for Intercooled Stata 8.0. 
* So, variables at the end are commented out.  The commenting 
* can be removed in an editor by replacing '' with ''.
* Stata/SE can handle up to 32,766 variables, default=5000.
*
****************************************************************

  Note:  This program is distributed under the GNU GPL. See end of
  this file and http://www.gnu.org/licenses/ for details.
  by Jean Roth Mon Apr 17 09:05:43 EDT 2006
  Please report errors to jroth@nber.org
  run with do sip84rt3

----------------------------------------------- */

/* The following line should contain
   the complete path and name of the raw data file.
   On a PC, use backslashes in paths as in C:\  */

local dat_name "sipp84rt3.dat"

/* The following line should contain the path to your output '.dta' file */

local dta_name "$sipp84rt3.dta"

/* The following line should contain the path to the data dictionary file */

local dct_name "${extractcodedir}\sip84rt3.dct"

/* The line below does NOT need to be changed */

quietly infile using "`dct_name'", using("`dat_name'") clear

/*------------------------------------------------

  Decimal places have been made explict in the dictionary file.
  Stata resolves a missing value of -1 / # of decimal places as a missing value.

 -----------------------------------------------*/

*Everything below this point, aside from the final save, are value labels

#delimit ;

;
label values su_state su_state;
label define su_state
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
	19          "Iowa"                          
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	23          "Maine"                         
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	29          "Missouri"                      
	31          "Nebraska"                      
	34          "New Jersey"                    
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
	51          "Virginia"                      
	53          "Washington"                    
	55          "Wisconsin"                     
	90          "Idaho, New Mexico, South Dakota, Wyoming"
	91          "Mississippi, West Virginia"    
;
label values su_rgc   su_rgc; 
label define su_rgc  
	0           "Not applicable for coverage improvement"
;
label values h1_mis   h1_mis; 
label define h1_mis  
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
	14          "Unoccupied tent or trailer site"
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
	19          "Iowa"                          
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	23          "Maine"                         
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	29          "Missouri"                      
	30          "Montana"                       
	31          "Nebraska"                      
	32          "Nevada"                        
	33          "New Hampshire"                 
	34          "New Jersey"                    
	36          "New York"                      
	37          "North Carolina"                
	38          "North Dakota"                  
	39          "Ohio"                          
	40          "Oklahoma"                      
	41          "Oregon"                        
	42          "Pennsylvania"                  
	44          "Rhode Island"                  
	45          "South Carolina"                
	47          "Tennessee"                     
	48          "Texas"                         
	49          "Utah"                          
	50          "Vermont"                       
	51          "Virginia"                      
	53          "Washington"                    
	55          "Wisconsin"                     
	90          "Idaho, New Mexico, South Dakota, Wyoming"
	91          "Mississippi, West Virginia"    
;
label values h1_metro h1_metro;
label define h1_metro
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subsample (may be"
;
label values h1_msa   h1_msa; 
label define h1_msa  
	0           "Not in metropolitan subsample or not"
	160         "Albany-Schenectady-Troy, NY"   
	640         "Austin, TX"                    
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1122        "Boston-Lawrence-Salem, MA-NH CMSA"
	1282        "Buffalo-Niagara Falls, NY CMSA"
	1602        "Chicago-Gary-Lake County (IL),"
	1642        "Cincinnati-Hamilton, OH-KY-IN CMSA"
	1692        "Cleveland-Akron-Lorain, OH CMSA"
	1840        "Columbus, OH"                  
	1922        "Dallas-Fort Worth, TX CMSA"    
	2000        "Dayton-Springfield, OH"        
	2082        "Denver-Boulder, CO CMSA"       
	2162        "Detroit-Ann Arbor, MI CMSA"    
	2400        "Eugene-Springfield, OR"        
	2840        "Fresno, CA"                    
	3120        "Greensboro--Winston-Salem--High"
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3282        "Hartford-New Britain-Middletown,"
	3320        "Honolulu, HI"                  
	3362        "Houston-Galveston-Brazoria, TX CMSA"
	3480        "Indianapolis, IN"              
	3762        "Kansas City, MO-Kansas City, KS CMSA"
	4000        "Lancaster, PA"                 
	4472        "Los Angeles-Anaheim-Riverside, CA"
	4720        "Madison, WI"                   
	4920        "Memphis, TN-AR-MS (part in TN only)"
	4992        "Miami-Fort Lauderdale, FL CMSA"
	5082        "Milwaukee-Racine, WI CMSA"     
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5602        "New York-Northern NJ-Long Island,"
	5880        "Oklahoma City, OK"             
	5920        "Omaha, NE-IA (part in NE only)"
	5960        "Orlando, FL"                   
	6162        "Philadelphia-Wilmington-Trenton,"
	6200        "Phoenix, AZ"                   
	6282        "Pittsburgh-Beaver Valley, PA CMSA"
	6442        "Portland-Vancouver, OR-WA CMSA"
	6840        "Rochester, NY"                 
	6920        "Sacramento, CA"                
	7042        "St. Louis-E. St Louis-Alton, MO-IL"
	7080        "Salem, OR"                     
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7362        "San Francisco-Oakland-San Jose, CA"
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
	9280        "York, PA"                      
	9320        "Youngstown-Warren, OH"         
;
label values h1_acces h1_acces;
label define h1_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h1_ktchn h1_ktchn;
label define h1_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values h1_lvqtr h1_lvqtr;
label define h1_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
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
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash rent"
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
label values u1_ktchn u1_ktchn;
label define u1_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values u1_lvqtr u1_lvqtr;
label define u1_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
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
	3           "Occupied without payment of cash rent"
	9           "Not answered"                  
;
label values u1_pubhs u1_pubhs;
label define u1_pubhs
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u1_lornt u1_lornt;
label define u1_lornt
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h1itm36b h1itm36b;
label define h1itm36b
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	23          "Entire household out-of-scope" 
	24          "Moved, address unknown"        
	25          "Moved within country beyond limit"
	26          "All sample persons relisted on new"
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
	2           "No person in household received benefits"
;
label values h1_cash  h1_cash;
label define h1_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received cash"
;
label values h1ncashb h1ncashb;
label define h1ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received noncash"
;
label values h1_enrgy h1_enrgy;
label define h1_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to household"
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers sent to"
	5           "Checks sent to household and payments"
	6           "Coupons or voucher sent to household"
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
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h1_4820  h1_4820l;
label define h1_4820l
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h1_4822  h1_4822l;
label define h1_4822l
	0           "Not marked as a place where payments"
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
	0           "Not marked as a free lunch or not"
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
	0           "Not marked as a free breakfast or"
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
	14          "Unoccupied tent or trailer site"
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
	19          "Iowa"                          
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	23          "Maine"                         
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	29          "Missouri"                      
	30          "Montana"                       
	31          "Nebraska"                      
	32          "Nevada"                        
	33          "New Hampshire"                 
	34          "New Jersey"                    
	36          "New York"                      
	37          "North Carolina"                
	38          "North Dakota"                  
	39          "Ohio"                          
	40          "Oklahoma"                      
	41          "Oregon"                        
	42          "Pennsylvania"                  
	44          "Rhode Island"                  
	45          "South Carolina"                
	47          "Tennessee"                     
	48          "Texas"                         
	49          "Utah"                          
	50          "Vermont"                       
	51          "Virginia"                      
	53          "Washington"                    
	55          "Wisconsin"                     
	90          "Idaho, New Mexico, South Dakota, Wyoming"
	91          "Mississippi, West Virginia"    
;
label values h2_metro h2_metro;
label define h2_metro
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subfamily (may be"
;
label values h2_msa   h2_msa; 
label define h2_msa  
	0           "Not in metropolitan subsample or not"
	160         "Albany-Schenectady-Troy, NY"   
	640         "Austin, TX"                    
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1122        "Boston-Lawrence-Salem, MA-NH CMSA"
	1282        "Buffalo-Niagara Falls, NY CMSA"
	1602        "Chicago-Gary-Lake County (IL),"
	1642        "Cincinnati-Hamilton, OH-KY-IN CMSA"
	1692        "Cleveland-Akron-Lorain, OH CMSA"
	1840        "Columbus, OH"                  
	1922        "Dallas-Fort Worth, TX CMSA"    
	2000        "Dayton-Springfield, OH"        
	2082        "Denver-Boulder, CO CMSA"       
	2162        "Detroit-Ann Arbor, MI CMSA"    
	2400        "Eugene-Springfield, OR"        
	2840        "Fresno, CA"                    
	3120        "Greensboro--Winston-Salem--High"
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3282        "Hartford-New Britain-Middletown, CT"
	3320        "Honolulu, HI"                  
	3362        "Houston-Galveston-Brazoria, TX CMSA"
	3480        "Indianapolis, IN"              
	3762        "Kansas City, MO-Kansas City, KS CMSA"
	4000        "Lancaster, PA"                 
	4472        "Los Angeles-Anaheim-Riverside, CA"
	4720        "Madison, WI"                   
	4920        "Memphis, TN-AR-MS (part in TN only)"
	4992        "Miami-Fort Lauderdale, FL CMSA"
	5082        "Milwaukee-Racine, WI CMSA"     
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5602        "New York-Northern NJ-Long Island,"
	5880        "Oklahoma City, OK"             
	5920        "Omaha, NE-IA (part in NE only)"
	5960        "Orlando, FL"                   
	6162        "Philadelphia-Wilmington-Trenton,"
	6200        "Phoenix, AZ"                   
	6282        "Pittsburgh-Beaver Valley, PA CMSA"
	6442        "Portland-Vancouver, OR-WA CMSA"
	6840        "Rochester, NY"                 
	6920        "Sacramento, CA"                
	7042        "St. Louis-E. St Louis-Alton, MO-IL"
	7080        "Salem, OR"                     
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7362        "San Francisco-Oakland-San Jose, CA"
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
	9280        "York, PA"                      
	9320        "Youngstown-Warren, OH"         
;
label values h2_acces h2_acces;
label define h2_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h2_ktchn h2_ktchn;
label define h2_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values h2_lvqtr h2_lvqtr;
label define h2_lvqtr
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values h2_units h2_units;
label define h2_units
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
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash rent"
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
label values u2_ktchn u2_ktchn;
label define u2_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values u2_lvqtr u2_lvqtr;
label define u2_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
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
	3           "Occupied without payment of cash rent"
	9           "Not answered"                  
;
label values u2_pubhs u2_pubhs;
label define u2_pubhs
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u2_lornt u2_lornt;
label define u2_lornt
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h2itm36b h2itm36b;
label define h2itm36b
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	23          "Entire household out-of-scope" 
	24          "Moved, address unknown"        
	25          "Moved within country beyond limit"
	26          "All sample persons relisted on new"
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
	2           "No person in household received benefits"
;
label values h2_cash  h2_cash;
label define h2_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received cash"
;
label values h2ncashb h2ncashb;
label define h2ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received noncash"
;
label values h2_enrgy h2_enrgy;
label define h2_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to household"
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers sent to"
	5           "Checks sent to household and payments"
	6           "Coupons or voucher sent to household"
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
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h2_4820  h2_4820l;
label define h2_4820l
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h2_4822  h2_4822l;
label define h2_4822l
	0           "Not marked as a place where payments"
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
	0           "Not marked as a free lunch or not"
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
	0           "Not marked as a free breakfast or"
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
	14          "Unoccupied tent or trailer site"
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
	19          "Iowa"                          
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	23          "Maine"                         
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	29          "Missouri"                      
	30          "Montana"                       
	31          "Nebraska"                      
	32          "Nevada"                        
	33          "New Hampshire"                 
	34          "New Jersey"                    
	36          "New York"                      
	37          "North Carolina"                
	38          "North Dakota"                  
	39          "Ohio"                          
	40          "Oklahoma"                      
	41          "Oregon"                        
	42          "Pennsylvania"                  
	44          "Rhode Island"                  
	45          "South Carolina"                
	47          "Tennessee"                     
	48          "Texas"                         
	49          "Utah"                          
	50          "Vermont"                       
	51          "Virginia"                      
	53          "Washington"                    
	55          "Wisconsin"                     
	90          "Idaho, New Mexico, South Dakota, Wyoming"
	91          "Mississippi, West Virginia"    
;
label values h3_metro h3_metro;
label define h3_metro
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subfamily (may be"
;
label values h3_msa   h3_msa; 
label define h3_msa  
	0           "Not in metropolitan subsample or not"
	160         "Albany-Schenectady-Troy, NY"   
	640         "Austin, TX"                    
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1122        "Boston-Lawrence-Salem, MA-NH CMSA"
	1282        "Buffalo-Niagara Falls, NY CMSA"
	1602        "Chicago-Gary-Lake County (IL),"
	1642        "Cincinnati-Hamilton, OH-KY-IN CMSA"
	1692        "Cleveland-Akron-Lorain, OH CMSA"
	1840        "Columbus, OH"                  
	1922        "Dallas-Fort Worth, TX CMSA"    
	2000        "Dayton-Springfield, OH"        
	2082        "Denver-Boulder, CO CMSA"       
	2162        "Detroit-Ann Arbor, MI CMSA"    
	2400        "Eugene-Springfield, OR"        
	2840        "Fresno, CA"                    
	3120        "Greensboro--Winston-Salem--High"
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3282        "Hartford-New Britain-Middletown, CT"
	3320        "Honolulu, HI"                  
	3362        "Houston-Galveston-Brazoria, TX CMSA"
	3480        "Indianapolis, IN"              
	3762        "Kansas City, MO-Kansas City, KS CMSA"
	4000        "Lancaster, PA"                 
	4472        "Los Angeles-Anaheim-Riverside, CA"
	4720        "Madison, WI"                   
	4920        "Memphis, TN-AR-MS (part in TN only)"
	4992        "Miami-Fort Lauderdale, FL CMSA"
	5082        "Milwaukee-Racine, WI CMSA"     
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5602        "New York-Northern NJ-Long Island,"
	5880        "Oklahoma City, OK"             
	5920        "Omaha, NE-IA (part in NE only)"
	5960        "Orlando, FL"                   
	6162        "Philadelphia-Wilmington-Trenton,"
	6200        "Phoenix, AZ"                   
	6282        "Pittsburgh-Beaver Valley, PA CMSA"
	6442        "Portland-Vancouver, OR-WA CMSA"
	6840        "Rochester, NY"                 
	6920        "Sacramento, CA"                
	7042        "St. Louis-E. St Louis-Alton, MO-IL"
	7080        "Salem, OR"                     
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7362        "San Francisco-Oakland-San Jose, CA"
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
	9280        "York, PA"                      
	9320        "Youngstown-Warren, OH"         
;
label values h3_acces h3_acces;
label define h3_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h3_ktchn h3_ktchn;
label define h3_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values h3_lvqtr h3_lvqtr;
label define h3_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
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
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash rent"
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
label values u3_ktchn u3_ktchn;
label define u3_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values u3_lvqtr u3_lvqtr;
label define u3_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
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
	3           "Occupied without payment of cash rent"
	9           "Not answered"                  
;
label values u3_pubhs u3_pubhs;
label define u3_pubhs
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u3_lornt u3_lornt;
label define u3_lornt
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h3itm36b h3itm36b;
label define h3itm36b
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	23          "Entire household out-of-scope" 
	24          "Moved, address unknown"        
	25          "Moved within country beyond limit"
	26          "All sample persons relisted on new"
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
	2           "No person in household received benefits"
;
label values h3_cash  h3_cash;
label define h3_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received cash"
;
label values h3ncashb h3ncashb;
label define h3ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received noncash"
;
label values h3_enrgy h3_enrgy;
label define h3_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to household"
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers sent to"
	5           "Checks sent to household and payments"
	6           "Coupons or voucher sent to household"
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
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h3_4820  h3_4820l;
label define h3_4820l
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h3_4822  h3_4822l;
label define h3_4822l
	0           "Not marked as a place where payments"
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
	0           "Not marked as a free lunch or not"
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
	0           "Not marked as a free breakfast or"
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
	14          "Unoccupied tent or trailer site"
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
	19          "Iowa"                          
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	23          "Maine"                         
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	29          "Missouri"                      
	30          "Montana"                       
	31          "Nebraska"                      
	32          "Nevada"                        
	33          "New Hampshire"                 
	34          "New Jersey"                    
	36          "New York"                      
	37          "North Carolina"                
	38          "North Dakota"                  
	39          "Ohio"                          
	40          "Oklahoma"                      
	41          "Oregon"                        
	42          "Pennsylvania"                  
	44          "Rhode Island"                  
	45          "South Carolina"                
	47          "Tennessee"                     
	48          "Texas"                         
	49          "Utah"                          
	50          "Vermont"                       
	51          "Virginia"                      
	53          "Washington"                    
	55          "Wisconsin"                     
	90          "Idaho, New Mexico, South Dakota, Wyoming"
	91          "Mississippi, West Virginia"    
;
label values h4_metro h4_metro;
label define h4_metro
	1           "In metropolitan subsample"     
	2           "Not in metropolitan subfamily (may be"
;
label values h4_msa   h4_msa; 
label define h4_msa  
	0           "Not in metropolitan subsample or not"
	160         "Albany-Schenectady-Troy, NY"   
	640         "Austin, TX"                    
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1122        "Boston-Lawrence-Salem, MA-NH CMSA"
	1282        "Buffalo-Niagara Falls, NY CMSA"
	1602        "Chicago-Gary-Lake County (IL),"
	1642        "Cincinnati-Hamilton, OH-KY-IN CMSA"
	1692        "Cleveland-Akron-Lorain, OH CMSA"
	1840        "Columbus, OH"                  
	1922        "Dallas-Fort Worth, TX CMSA"    
	2000        "Dayton-Springfield, OH"        
	2082        "Denver-Boulder, CO CMSA"       
	2162        "Detroit-Ann Arbor, MI CMSA"    
	2400        "Eugene-Springfield, OR"        
	2840        "Fresno, CA"                    
	3120        "Greensboro--Winston-Salem--High"
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3282        "Hartford-New Britain-Middletown, CT"
	3320        "Honolulu, HI"                  
	3362        "Houston-Galveston-Brazoria, TX CMSA"
	3480        "Indianapolis, IN"              
	3762        "Kansas City, MO-Kansas City, KS CMSA"
	4000        "Lancaster, PA"                 
	4472        "Los Angeles-Anaheim-Riverside, CA"
	4720        "Madison, WI"                   
	4920        "Memphis, TN-AR-MS (part in TN only)"
	4992        "Miami-Fort Lauderdale, FL CMSA"
	5082        "Milwaukee-Racine, WI CMSA"     
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5602        "New York-Northern NJ-Long Island,"
	5880        "Oklahoma City, OK"             
	5920        "Omaha, NE-IA (part in NE only)"
	5960        "Orlando, FL"                   
	6162        "Philadelphia-Wilmington-Trenton,"
	6200        "Phoenix, AZ"                   
	6282        "Pittsburgh-Beaver Valley, PA CMSA"
	6442        "Portland-Vancouver, OR-WA CMSA"
	6840        "Rochester, NY"                 
	6920        "Sacramento, CA"                
	7042        "St. Louis-E. St Louis-Alton, MO-IL"
	7080        "Salem, OR"                     
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7362        "San Francisco-Oakland-San Jose, CA"
	7560        "Scranton-Wilkes-Barre, PA"     
	8000        "Springfield, MA"               
	8280        "Tampa-St. Petersburg-Clearwater, FL"
	8400        "Toledo, OH"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-Delray"
	9240        "Worcester, MA"                 
	9280        "York, PA"                      
	9320        "Youngstown-Warren, OH"         
;
label values h4_acces h4_acces;
label define h4_acces
	0           "Not applicable"                
	1           "Direct - skip to housunit"     
	2           "Through another unit"          
;
label values h4_ktchn h4_ktchn;
label define h4_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values h4_lvqtr h4_lvqtr;
label define h4_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
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
	1           "Owned or being bought by someone"
	2           "Rented for cash"               
	3           "Occupied without payment of cash rent"
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
label values u4_ktchn u4_ktchn;
label define u4_ktchn
	0           "Not applicable"                
	1           "For this unit only"            
	2           "Also used by another household"
	3           "None"                          
;
label values u4_lvqtr u4_lvqtr;
label define u4_lvqtr
	-9          "Not answered"                  
	0           "Not answered (types B and C)"  
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with no permanent"
	6           "Mobile home or trailer with one or more"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or boarding"
	9           "Unit not permanent in transient hotel,"
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
	3           "Occupied without payment of cash rent"
	9           "Not answered"                  
;
label values u4_pubhs u4_pubhs;
label define u4_pubhs
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values u4_lornt u4_lornt;
label define u4_lornt
	-1          "Dk"                            
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values h4itm36b h4itm36b;
label define h4itm36b
	1           "Interviewed"                   
	2           "No one home"                   
	3           "Temporarily absent"            
	4           "Refused"                       
	5           "Unable to locate"              
	6           "Other"                         
	23          "Entire household out-of-scope" 
	24          "Moved, address unknown"        
	25          "Moved within country beyond limit"
	26          "All sample persons relisted on new"
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
	2           "No person in household received benefits"
;
label values h4_cash  h4_cash;
label define h4_cash 
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "No person in household received cash"
;
label values h4ncashb h4ncashb;
label define h4ncashb
	0           "Not in universe or no persons" 
	1           "One or more persons in household"
	2           "One or more persons in household"
	3           "No person in household received noncash"
;
label values h4_enrgy h4_enrgy;
label define h4_enrgy
	0           "Not applicable"                
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to household"
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers sent to"
	5           "Checks sent to household and payments"
	6           "Coupons or voucher sent to household"
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
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h4_4820  h4_4820l;
label define h4_4820l
	0           "Not marked as a place where payments"
	1           "Marked as a place where payments"
;
label values h4_4822  h4_4822l;
label define h4_4822l
	0           "Not marked as a place where payments"
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
	0           "Not marked as a free lunch or not"
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
	0           "Not marked as a free breakfast or"
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
	1           "Interview"                     
	2           "Noninterview"                  
	3           "Not in sample"                 
;
label values h5_lvqtr h5_lvqtr;
label define h5_lvqtr
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "Hu in nontransient hotel, motel etc."
	3           "Hu, permanent in transient hotel,"
	4           "Hu in rooming house"           
	5           "Mobile home or trailer with one or"
	6           "Mobile home or trailer with one or"
	7           "Hu not specified above"        
	8           "Quarters not hu in rooming or" 
	9           "Unit not permanent in transient"
	10          "Unoccupied tent or trailer site"
	11          "Other unit not specified above"
;
label values f1_type  f1_type;
label define f1_type 
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f1_kind  f1_kind;
label define f1_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f1ownkid f1ownkid;
label define f1ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values f1oklt18 f1oklt1d;
label define f1oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values f2_type  f2_type;
label define f2_type 
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f2_kind  f2_kind;
label define f2_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f2ownkid f2ownkid;
label define f2ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values f2oklt18 f2oklt1d;
label define f2oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values f3_type  f3_type;
label define f3_type 
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f3_kind  f3_kind;
label define f3_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f3ownkid f3ownkid;
label define f3ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values f3oklt18 f3oklt1d;
label define f3oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values f4_type  f4_type;
label define f4_type 
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values f4_kind  f4_kind;
label define f4_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values f4ownkid f4ownkid;
label define f4ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values f4oklt18 f4oklt1d;
label define f4oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s1_type  s1_type;
label define s1_type 
	2           "Related subfamily"             
;
label values s1_kind  s1_kind;
label define s1_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s1ownkid s1ownkid;
label define s1ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s1oklt18 s1oklt1d;
label define s1oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s2_type  s2_type;
label define s2_type 
	2           "Related subfamily"             
;
label values s2_kind  s2_kind;
label define s2_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s2ownkid s2ownkid;
label define s2ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s2oklt18 s2oklt1d;
label define s2oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s3_type  s3_type;
label define s3_type 
	2           "Related subfamily"             
;
label values s3_kind  s3_kind;
label define s3_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s3ownkid s3ownkid;
label define s3ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s3oklt18 s3oklt1d;
label define s3oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s4_type  s4_type;
label define s4_type 
	2           "Related subfamily"             
;
label values s4_kind  s4_kind;
label define s4_kind 
	1           "Headed by husband/ wife"       
	2           "Male reference person"         
	3           "Female reference person"       
;
label values s4ownkid s4ownkid;
label define s4ownkid
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values s4oklt18 s4oklt1d;
label define s4oklt1d
	0           "None"                          
	1           "One"                           
	2           "Two"                           
;
label values pp_intvw pp_intvw;
label define pp_intvw
	0           "Not applicable (children under 15)"
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
	2           "Reference person with no relatives"
	3           "Husband of reference person"   
	4           "Wife of reference person"      
	5           "Own child of reference person" 
	6           "Parent of reference person"    
	7           "Brother/sister of reference person"
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
	999         "Not applicable"                
;
label values pw_pnpt  pw_pnpt;
label define pw_pnpt 
	-09         "Not answered"                  
	0           "Not in universe"               
	999         "Not applicable"                
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
	0           "Not applicable or not in universe"
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - type-Z"         
	4           "Noninterview - type-Z other"   
;
label values rrp_1    rrp_1l; 
label define rrp_1l  
	0           "Not a sample person this month"
	1           "Household reference person, living"
	2           "Household reference person living"
	3           "Spouse of household reference person"
	4           "Child of household reference person"
	5           "Other relative of household reference"
	6           "Nonrelative of household reference"
	7           "Nonrelative of household reference"
;
label values rrp_2    rrp_2l; 
label define rrp_2l  
	0           "Not a sample person this month"
	1           "Household reference person, living"
	2           "Household reference person living"
	3           "Spouse of household reference person"
	4           "Child of household reference person"
	5           "Other relative of household reference"
	6           "Nonrelative of household reference"
	7           "Nonrelative of household reference"
;
label values rrp_3    rrp_3l; 
label define rrp_3l  
	0           "Not a sample person this month"
	1           "Household reference person, living"
	2           "Household reference person living"
	3           "Spouse of household reference person"
	4           "Child of household reference person"
	5           "Other relative of household reference"
	6           "Nonrelative of household reference"
	7           "Nonrelative of household reference"
;
label values rrp_4    rrp_4l; 
label define rrp_4l  
	0           "Not a sample person this month"
	1           "Household reference person, living"
	2           "Household reference person living"
	3           "Spouse of household reference person"
	4           "Child of household reference person"
	5           "Other relative of household reference"
	6           "Nonrelative of household reference"
	7           "Nonrelative of household reference"
;
label values rrp_5    rrp_5l; 
label define rrp_5l  
	0           "Not a sample person this month"
	1           "Household reference person, living"
	2           "Household reference person living"
	3           "Spouse of household reference person"
	4           "Child of household reference person"
	5           "Other relative of household reference"
	6           "Nonrelative of household reference"
	7           "Nonrelative of household reference"
;
label values age_1    age_1l; 
label define age_1l  
	0           "Less than 1 full year or not a sample"
	85          "85 years old or older"         
;
label values age_2    age_2l; 
label define age_2l  
	0           "Less than 1 full year or not a sample"
	85          "85 years old or older"         
;
label values age_3    age_3l; 
label define age_3l  
	0           "Less than 1 full year or not a sample"
	85          "85 years old or older"         
;
label values age_4    age_4l; 
label define age_4l  
	0           "Less than 1 full year or not a sample"
	85          "85 years old or older"         
;
label values age_5    age_5l; 
label define age_5l  
	0           "Less than 1 full year or not a sample"
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
	1           "Secondary individual (not a family"
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_2 famtyp_k;
label define famtyp_k
	0           "Primary family or not in sample"
	1           "Secondary individual (not a family"
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_3 famtyp_l;
label define famtyp_l
	0           "Primary family or not in sample"
	1           "Secondary individual (not a family"
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_4 famtyp_m;
label define famtyp_m
	0           "Primary family or not in sample"
	1           "Secondary individual (not a family"
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famtyp_5 famtyp_n;
label define famtyp_n
	0           "Primary family or not in sample"
	1           "Secondary individual (not a family"
	2           "Unrelated sub (secondary) family"
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famrel_1 famrel_e;
label define famrel_e
	0           "Not applicable (person not in related"
	1           "Reference person of family"    
	2           "Spouse of family reference person"
	3           "Child of family reference person"
	4           "Other relative of family reference"
;
label values famrel_2 famrel_k;
label define famrel_k
	0           "Not applicable (person not in related"
	1           "Reference person of family"    
	2           "Spouse of family reference person"
	3           "Child of family reference person"
	4           "Other relative of family reference"
;
label values famrel_3 famrel_l;
label define famrel_l
	0           "Not applicable (person not in related"
	1           "Reference person of family"    
	2           "Spouse of family reference person"
	3           "Child of family reference person"
	4           "Other relative of family reference"
;
label values famrel_4 famrel_m;
label define famrel_m
	0           "Not applicable (person not in related"
	1           "Reference person of family"    
	2           "Spouse of family reference person"
	3           "Child of family reference person"
	4           "Other relative of family reference"
;
label values famrel_5 famrel_n;
label define famrel_n
	0           "Not applicable (person not in related"
	1           "Reference person of family"    
	2           "Spouse of family reference person"
	3           "Child of family reference person"
	4           "Other relative of family reference"
;
label values famnum_1 famnum_e;
label define famnum_e
	0           "Not applicable (person not in related"
;
label values famnum_2 famnum_k;
label define famnum_k
	0           "Not applicable (person not in related"
;
label values famnum_3 famnum_l;
label define famnum_l
	0           "Not applicable (person not in related"
;
label values famnum_4 famnum_m;
label define famnum_m
	0           "Not applicable (person not in related"
;
label values famnum_5 famnum_n;
label define famnum_n
	0           "Not applicable (person not in related"
;
label values pop_stat pop_stat;
label define pop_stat
	1           "Adult (15 years of age or older)"
	2           "Child (under 15 at interview)" 
;
label values pnsp_1   pnsp_1l;
label define pnsp_1l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnsp_2   pnsp_2l;
label define pnsp_2l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnsp_3   pnsp_3l;
label define pnsp_3l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnsp_4   pnsp_4l;
label define pnsp_4l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnsp_5   pnsp_5l;
label define pnsp_5l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnpt_1   pnpt_1l;
label define pnpt_1l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnpt_2   pnpt_2l;
label define pnpt_2l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnpt_3   pnpt_3l;
label define pnpt_3l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnpt_4   pnpt_4l;
label define pnpt_4l 
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnpt_5   pnpt_5l;
label define pnpt_5l 
	0           "Not in sample this month"      
	999         "Not applicable"                
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
	19          "Central or South American (Spanish)"
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
	1           "Reference person with relatives in HH"
	2           "Reference person with no rel. In HH"
	3           "Husband of reference person"   
	4           "Wife of reference person"      
	5           "Own child of reference person" 
	6           "Parent of reference person"    
	7           "Brother/sister of reference person"
	8           "Other relative of reference person"
	9           "Nonrelative of reference person"
;
label values u_hhmem  u_hhmem;
label define u_hhmem 
	1           "Yes - current HH member"       
	2           "No"                            
;
label values u_realft u_realft;
label define u_realft
	0           "Not applicable (person did not move or"
	5           "Left - deceased"               
	6           "Left - institutionalized"      
	7           "Left - living in Armed Forces barracks"
	8           "Left - moved outside of country"
	9           "Left - separation or divorce"  
	10          "Left - person 201 or greater no longer"
	11          "Left - other"                  
	99          "Interviewed in previous wave but not"
;
label values u_reaent u_reaent;
label define u_reaent
	0           "Not applicable (person did not move"
	1           "Entered - birth"               
	2           "Entered - marriage"            
	3           "Entered - other"               
	4           "Entered (before sample people)"
	12          "Entered merged household"      
	13          "Reentered household"           
;
label values u_monlft u_monlft;
label define u_monlft
	0           "Not applicable (person did not move or"
;
label values u_monent u_monent;
label define u_monent
	0           "Not applicable (person did not move or"
;
label values u_daylft u_daylft;
label define u_daylft
	0           "Not applicable (person did not move or"
;
label values u_dayent u_dayent;
label define u_dayent
	0           "Not applicable (person did not move or"
;
label values cc_adlft cc_adlft;
label define cc_adlft
	0           "Not applicable (person did not move or"
;
label values cc_adent cc_adent;
label define cc_adent
	0           "Not applicable (person did not move or"
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
label values sc0064   sc0064l;
label define sc0064l 
	0           "Not in universe"               
;
label values sc0066   sc0066l;
label define sc0066l 
	0           "Not in universe"               
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
	1           "With a job entire month, worked all"
	2           "With a job entire month, missed one or"
	3           "With a job entire month, missed one or"
	4           "With job one or more weeks, no time"
	5           "With job one or more weeks, spent one"
	6           "No job during month, spent entire month"
	7           "No job during month, spent one or more"
	8           "No job during month, no time spent"
;
label values esr_2    esr_2l; 
label define esr_2l  
	0           "Not applicable"                
	1           "With a job entire month, worked all"
	2           "With a job entire month, missed one or"
	3           "With a job entire month, missed one or"
	4           "With job one or more weeks, no time"
	5           "With job one or more weeks, spent one or"
	6           "No job during month, spent entire month"
	7           "No job during month, spent one or more"
	8           "No job during month, no time spent"
;
label values esr_3    esr_3l; 
label define esr_3l  
	0           "Not applicable"                
	1           "With a job entire month, worked all"
	2           "With a job entire month, missed one or"
	3           "With a job entire month, missed one or"
	4           "With job one or more weeks, no time"
	5           "With job one or more weeks, spent"
	6           "No job during month, spent entire month"
	7           "No job during month, spent one or more"
	8           "No job during month, no time spent"
;
label values esr_4    esr_4l; 
label define esr_4l  
	0           "Not applicable"                
	1           "With a job entire month, worked all"
	2           "With a job entire month, missed one or"
	3           "With a job entire month, missed one or"
	4           "With job one or more weeks, no time"
	5           "With job one or more weeks, spent"
	6           "No job during month, spent entire month"
	7           "No job during month, spent one or more"
	8           "No job during month, no time spent"
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
	5           "5 weeks (only applicable for months"
;
label values wksjb2   wksjb2l;
label define wksjb2l 
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wksjb3   wksjb3l;
label define wksjb3l 
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wksjb4   wksjb4l;
label define wksjb4l 
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
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
label values wkswop1  wkswop1l;
label define wkswop1l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wkswop2  wkswop2l;
label define wkswop2l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wkswop3  wkswop3l;
label define wkswop3l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wkswop4  wkswop4l;
label define wkswop4l
	0           "0 weeks or not applicable"     
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
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
label values wkslok1  wkslok1l;
label define wkslok1l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wkslok2  wkslok2l;
label define wkslok2l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wkslok3  wkslok3l;
label define wkslok3l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
;
label values wkslok4  wkslok4l;
label define wkslok4l
	0           "None or not applicable"        
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks (only applicable for months"
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
	0           "Not applicable                                                             ^M"
	1           "Social security                                                            ^M"
	2           "Railroad retirement                                                        ^M"
	3           "Federal supplemental security income                                       ^M"
	5           "State unemployment compensation                                            ^M"
	6           "Supplemental unemployment benefits                                         ^M"
	7           "Other unemployment compensation                                            ^M"
	8           "Veterans compensation or pensions                                          ^M"
	10          "Workers compensation                                                       ^M"
	12          "Employer or union temporary sickness policy                                ^M"
	13          "Payments from a sickness, accident, or                                     ^M"
	20          "Aid to families with dependent                                             ^M"
	21          "General assistance or general relief                                       ^M"
	23          "Foster child care payments                                                 ^M"
	24          "Other welfare                                                              ^M"
	25          "WIC                                                                        ^M"
	27          "Food stamps                                                                ^M"
	28          "Child support payments                                                     ^M"
	29          "Alimony payments                                                           ^M"
	30          "Pension from company or union                                              ^M"
	31          "Federal civil service or other Federal                                     ^M"
	32          "U.S. military retirement pay                                               ^M"
	34          "State government pensions                                                  ^M"
	35          "Local government pensions                                                  ^M"
	36          "Income from paid up life insurance policies                                ^M"
	37          "Estates and trusts                                                         ^M"
	38          "Other payments for retirement, disability or                               ^M"
	40          "GI bill education benefits                                                 ^M"
	50          "Income assistance from a charitable group                                  ^M"
	51          "Money from relatives or friends                                            ^M"
	52          "Lump sum payments                                                          ^M"
	53          "Income from roomers or boarders                                            ^M"
	54          "National guard or reserve pay                                              ^M"
	55          "Incidental or casual earnings                                              ^M"
	56          "Other cash income not included elsewhere                                   ^M"
	75          "Recoded for confidentiality (ISS codes 4,9,11,22,33)"
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
	100         "Regular/passbook savings accounts"
	101         "Money market deposit accounts" 
	102         "Certificates of deposit or other"
	103         "NOW, super NOW, or other interest"
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
	0           " Not applicable"               
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
label values sc0902   sc0902l;
label define sc0902l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC0906"           
;
label values sc0904   sc0904l;
label define sc0904l 
	0           "Not in universe"               
	1           "Armed Forces barracks - skip to"
	2           "Outside the United States - skip to"
	3           "Nonhousehold setting - skip to SC1000"
;
label values sc0906   sc0906l;
label define sc0906l 
	0           "Not in universe"               
	1           "Yes - skip to SC1000"          
	2           "No"                            
;
label values sc0910   sc0910l;
label define sc0910l 
	0           "Not in universe"               
	1           "Yes - skip to SC1000"          
	2           "No"                            
;
label values sc0912   sc0912l;
label define sc0912l 
	0           "Not in universe"               
	1           "Husband"                       
	2           "Wife"                          
	3           "Own child (son or daughter)"   
	4           "Parent"                        
	5           "Brother/sister"                
	6           "Other relative"                
	7           "Nonrelative"                   
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
	0           "Not marked as looking or on layoff"
	1           "Marked as looking or on layoff all"
;
label values sc1006   sc1006l;
label define sc1006l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on"
;
label values sc1008   sc1008l;
label define sc1008l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on"
;
label values sc1010   sc1010l;
label define sc1010l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1012   sc1012l;
label define sc1012l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1014   sc1014l;
label define sc1014l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1016   sc1016l;
label define sc1016l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1018   sc1018l;
label define sc1018l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1020   sc1020l;
label define sc1020l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1022   sc1022l;
label define sc1022l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1024   sc1024l;
label define sc1024l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1026   sc1026l;
label define sc1026l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1028   sc1028l;
label define sc1028l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1030   sc1030l;
label define sc1030l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1032   sc1032l;
label define sc1032l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1034   sc1034l;
label define sc1034l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1036   sc1036l;
label define sc1036l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1038   sc1038l;
label define sc1038l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
;
label values sc1040   sc1040l;
label define sc1040l 
	0           "Not marked as a week looking or on"
	1           "Marked as a week looking or on layoff"
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
	1           "Believes no work available in line"
	2           "Couldn't find any work - skip to"
	3           "Lacks necessary schooling, training,"
	4           "Employers think too young or too"
	5           "Other personal handicap in finding"
	6           "Can't arrange child care - skip to SC124"
	7           "Family responsibilities - skip to SC1240"
	8           "In school or other training - skip to"
	9           "Ill health, physical disability - skip"
	10          "Other - skip to SC1240"        
;
label values sc1056   sc1056l;
label define sc1056l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1100 through SC1134"
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
	1           "Marked as a week absent without pay"
;
label values sc1062   sc1062l;
label define sc1062l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1064   sc1064l;
label define sc1064l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1066   sc1066l;
label define sc1066l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1068   sc1068l;
label define sc1068l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1070   sc1070l;
label define sc1070l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1072   sc1072l;
label define sc1072l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1074   sc1074l;
label define sc1074l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1076   sc1076l;
label define sc1076l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1078   sc1078l;
label define sc1078l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1080   sc1080l;
label define sc1080l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1082   sc1082l;
label define sc1082l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1084   sc1084l;
label define sc1084l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1086   sc1086l;
label define sc1086l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1088   sc1088l;
label define sc1088l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1090   sc1090l;
label define sc1090l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1092   sc1092l;
label define sc1092l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1094   sc1094l;
label define sc1094l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1096   sc1096l;
label define sc1096l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1098   sc1098l;
label define sc1098l 
	0           "Not in universe"               
	1           "On layoff - skip to SC1230"    
	2           "Own illness - skip to SC1230"  
	3           "On vacation - skip to SC1230"  
	4           "Bad weather - skip to SC1230"  
	5           "Labor dispute - skip to SC1230"
	6           "New job to begin within 30 days -"
	7           "Other - skip to SC1230"        
;
label values sc1100   sc1100l;
label define sc1100l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1102   sc1102l;
label define sc1102l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1104   sc1104l;
label define sc1104l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1106   sc1106l;
label define sc1106l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1108   sc1108l;
label define sc1108l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1110   sc1110l;
label define sc1110l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1112   sc1112l;
label define sc1112l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1114   sc1114l;
label define sc1114l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1116   sc1116l;
label define sc1116l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1118   sc1118l;
label define sc1118l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1120   sc1120l;
label define sc1120l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1122   sc1122l;
label define sc1122l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1124   sc1124l;
label define sc1124l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1126   sc1126l;
label define sc1126l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1128   sc1128l;
label define sc1128l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1130   sc1130l;
label define sc1130l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1132   sc1132l;
label define sc1132l 
	0           "Not marked as a week having a job"
	1           "Marked as a week having a job" 
;
label values sc1134   sc1134l;
label define sc1134l 
	0           "Not marked as a week having a job"
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
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1140   sc1140l;
label define sc1140l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1142   sc1142l;
label define sc1142l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1144   sc1144l;
label define sc1144l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1146   sc1146l;
label define sc1146l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1148   sc1148l;
label define sc1148l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1150   sc1150l;
label define sc1150l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1152   sc1152l;
label define sc1152l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1154   sc1154l;
label define sc1154l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1156   sc1156l;
label define sc1156l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1158   sc1158l;
label define sc1158l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1160   sc1160l;
label define sc1160l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1162   sc1162l;
label define sc1162l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1164   sc1164l;
label define sc1164l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1166   sc1166l;
label define sc1166l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1168   sc1168l;
label define sc1168l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1170   sc1170l;
label define sc1170l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
;
label values sc1172   sc1172l;
label define sc1172l 
	0           "Not marked as a week absent without"
	1           "Marked as a week absent without pay"
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
	0           "Not marked as looking for work or on"
	1           "Marked as looking for work or on"
;
label values sc1180   sc1180l;
label define sc1180l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1182   sc1182l;
label define sc1182l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1184   sc1184l;
label define sc1184l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1186   sc1186l;
label define sc1186l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1188   sc1188l;
label define sc1188l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1190   sc1190l;
label define sc1190l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1192   sc1192l;
label define sc1192l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1194   sc1194l;
label define sc1194l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1196   sc1196l;
label define sc1196l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1198   sc1198l;
label define sc1198l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1200   sc1200l;
label define sc1200l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1202   sc1202l;
label define sc1202l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1204   sc1204l;
label define sc1204l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1206   sc1206l;
label define sc1206l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1208   sc1208l;
label define sc1208l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1210   sc1210l;
label define sc1210l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1212   sc1212l;
label define sc1212l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
;
label values sc1214   sc1214l;
label define sc1214l 
	0           "Not marked as a week looking for work"
	1           "Marked as a week looking for work or"
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
label values sc1232   sc1232l;
label define sc1232l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1236"           
;
label values sc1234   sc1234l;
label define sc1234l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1239"           
;
label values sc1236   sc1236l;
label define sc1236l 
	-5          "All weeks in reference period" 
	0           "Not in universe"               
;
label values sc1238   sc1238l;
label define sc1238l 
	0           "Not in universe"               
	1           "Could not find a full-time job"
	2           "Wanted to work part-time"      
	3           "Health condition or disability"
	4           "Normal working hours are less" 
	5           "Slack work or material shortage"
	6           "Other"                         
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
	0           "Not marked as a kind of income or"
	1           "Marked as kind of income"      
;
label values sc1288   sc1288l;
label define sc1288l 
	0           "Not marked as a kind of income or"
	1           "Marked as kind of income"      
;
label values sc1290   sc1290l;
label define sc1290l 
	0           "Not marked as a kind of income or"
	1           "Marked as kind of income"      
;
label values sc1292   sc1292l;
label define sc1292l 
	0           "Not marked as a kind of income or"
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
	-1          "Dk"                            
	0           "Not in universe"               
	1           "Less than 6 months"            
	2           "6 to 23 months"                
	3           "2 to 19 years"                 
	4           "20 or more years"              
;
label values sc1334   sc1334l;
label define sc1334l 
	-1          "Dk"                            
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
	-1          "Dk - skip to SC1354"           
	0           "Not in universe"               
	1           "Retired"                       
	2           "Disabled"                      
	3           "Widow(ed) or surviving child"  
	4           "Spouse or dependent child"     
	5           "Some other reason - skip to SC1354"
;
label values sc1348   sc1348l;
label define sc1348l 
	-1          "Dk - skip to SC1354"           
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
	0           "Not marked as a kind of retirement"
	1           "Marked as a kind of retirement income"
;
label values sc1366   sc1366l;
label define sc1366l 
	0           "Not marked as a kind of retirement"
	1           "Marked as a kind of retirement income"
;
label values sc1368   sc1368l;
label define sc1368l 
	0           "Not marked as a kind of retirement"
	1           "Marked as a kind of retirement income"
;
label values sc1370   sc1370l;
label define sc1370l 
	0           "Not marked as a kind of retirement"
	1           "Marked as a kind of retirement income"
;
label values sc1374   sc1374l;
label define sc1374l 
	0           "Not marked as a kind of retirement"
	1           "Marked as a kind of retirement income"
;
label values sc1376   sc1376l;
label define sc1376l 
	0           "Not marked as a kind of retirement"
	1           "Marked as a kind of retirement income"
;
label values sc1378   sc1378l;
label define sc1378l 
	0           "Not marked as a kind of retirement"
	1           "Marked as a kind of retirement income"
;
label values sc1380   sc1380l;
label define sc1380l 
	0           "Not in universe based on response to"
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
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1394   sc1394l;
label define sc1394l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1396   sc1396l;
label define sc1396l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1398   sc1398l;
label define sc1398l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1400   sc1400l;
label define sc1400l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1402   sc1402l;
label define sc1402l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1406   sc1406l;
label define sc1406l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1408   sc1408l;
label define sc1408l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1410   sc1410l;
label define sc1410l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1412   sc1412l;
label define sc1412l 
	0           "Not in universe based on response to"
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
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1430   sc1430l;
label define sc1430l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1434   sc1434l;
label define sc1434l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1436   sc1436l;
label define sc1436l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1438   sc1438l;
label define sc1438l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1442   sc1442l;
label define sc1442l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1444   sc1444l;
label define sc1444l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1446   sc1446l;
label define sc1446l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1448   sc1448l;
label define sc1448l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1450   sc1450l;
label define sc1450l 
	0           "Not marked as a kind of income or"
	1           "Marked as a kind of income"    
;
label values sc1452   sc1452l;
label define sc1452l 
	0           "Not in universe based on response to"
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
	0           "Not marked as a kind of welfare or"
	1           "Marked as a kind of welfare"   
;
label values sc1488   sc1488l;
label define sc1488l 
	0           "Not marked as a kind of welfare or"
	1           "Marked as a kind of welfare"   
;
label values sc1492   sc1492l;
label define sc1492l 
	0           "Not marked as a kind of welfare or"
	1           "Marked as a kind of welfare"   
;
label values sc1494   sc1494l;
label define sc1494l 
	0           "Not marked as a kind of welfare or"
	1           "Marked as a kind of welfare"   
;
label values sc1496   sc1496l;
label define sc1496l 
	0           "Not marked as a kind of welfare or"
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
	0           "Not in universe, some others covered"
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
	-1          "DK - skip to SC1574 through SC1586"
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1574 through SC1586"
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
	0           "Not in universe, some children covered"
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
label values sc1656   sc1656l;
label define sc1656l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1694"           
;
label values sc1658   sc1658l;
label define sc1658l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1694"           
;
label values sc1660   sc1660l;
label define sc1660l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1692"           
;
label values sc1662   sc1662l;
label define sc1662l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1664   sc1664l;
label define sc1664l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1666   sc1666l;
label define sc1666l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1668   sc1668l;
label define sc1668l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1670   sc1670l;
label define sc1670l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1672   sc1672l;
label define sc1672l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1674   sc1674l;
label define sc1674l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1676   sc1676l;
label define sc1676l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1678   sc1678l;
label define sc1678l 
	0           "Not marked as a kind of educational"
	1           "Marked as a kind of educational"
;
label values sc1680   sc1680l;
label define sc1680l 
	0           "Not in universe"               
	1           "Semester"                      
	2           "Trimester"                     
	3           "Quarter"                       
	4           "Other"                         
;
label values sc1682   sc1682l;
label define sc1682l 
	0           "Not in universe"               
	4000        "4000 or more assistance"       
;
label values sc1684   sc1684l;
label define sc1684l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1688"           
;
label values sc1688   sc1688l;
label define sc1688l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC1692"           
;
label values sc1690   sc1690l;
label define sc1690l 
	0           "Not in universe"               
	4000        "4000 or more assistance"       
;
label values sc1692   sc1692l;
label define sc1692l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
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
	1           "Yes - skip to SC1706 through SC1710"
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
label values pi80     pi80l;  
label define pi80l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi81     pi81l;  
label define pi81l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values pi82     pi82l;  
label define pi82l   
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
	0           "None or not in universe if classwk = 6"
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws1_wks2 ws1_wksk;
label define ws1_wksk
	0           "None or not in universe if classwk = 6"
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws1_wks3 ws1_wksl;
label define ws1_wksl
	0           "None or not in universe if classwk = 6"
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws1_wks4 ws1_wksm;
label define ws1_wksm
	0           "None or not in universe if classwk = 6"
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
label values ws1_2002 ws1_200d;
label define ws1_200d
	0           "Not answered"                  
	9           "Not in universe"               
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
	0           "None or not in universe if classwk = 6"
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws2_wks2 ws2_wksk;
label define ws2_wksk
	0           "None or not in universe if classwk = 6"
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws2_wks3 ws2_wksl;
label define ws2_wksl
	0           "None or not in universe if classwk = 6"
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values ws2_wks4 ws2_wksm;
label define ws2_wksm
	0           "None or not in universe if classwk = 6"
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
label values ws2_2002 ws2_200d;
label define ws2_200d
	0           "Not answered"                  
	9           "Not in universe"               
;
label values ws2_2012 ws2_201d;
label define ws2_201d
	0           "Not answered"                  
	1           "A private company or individual"
	2           "Federal government"            
	3           "State government"              
	4           "Local government"              
	5           "Armed Forces"                  
	6           "Unpaid in family business or"  
	9           "Not in universe"               
;
label values ws2_2014 ws2_201k;
label define ws2_201k
	0           "Not in universe"               
	1           "Yes - skip to WS2-2024"        
	2           "No"                            
;
label values ws2_2016 ws2_201l;
label define ws2_201l
	0           "Not in universe"               
;
label values ws2_2018 ws2_201m;
label define ws2_201m
	0           "Not in universe"               
;
label values ws2_2020 ws2_202d;
label define ws2_202d
	0           "Not in universe"               
;
label values ws2_2022 ws2_202k;
label define ws2_202k
	0           "Not in universe"               
;
label values ws2_2024 ws2_202l;
label define ws2_202l
	-9          "Not in universe"               
	0           "None"                          
;
label values ws2_2026 ws2_202m;
label define ws2_202m
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to WS2-2030"         
;
label values ws2_2028 ws2_202n;
label define ws2_202n
	0           "Not in universe"               
;
label values ws2_2030 ws2_203d;
label define ws2_203d
	0           "Not in universe"               
	1           "Once a week"                   
	2           "Once each 2 weeks"             
	3           "Once a month"                  
	4           "Twice a month"                 
	5           "Some other way"                
;
label values ws2_2032 ws2_203k;
label define ws2_203k
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2034 ws2_203l;
label define ws2_203l
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2036 ws2_203m;
label define ws2_203m
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2038 ws2_203n;
label define ws2_203n
	-0009       "Not in universe"               
	0           "None"                          
;
label values ws2_2040 ws2_204d;
label define ws2_204d
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to WS2-2044"         
;
label values ws2_2042 ws2_204k;
label define ws2_204k
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
label values se1occ   se1occ; 
label define se1occ  
	0           "Not in universe"               
;
label values se1ind   se1ind; 
label define se1ind  
	0           "Not in universe"               
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
label values se12202  se12202l;
label define se12202l
	0           "Not answered"                  
	9           "Not in universe"               
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
	1           "Sole proprietorship - skip to SE12232"
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
label values se1imp01 se1imp0d;
label define se1imp0d
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
label values se1imp09 se1imp0r;
label define se1imp0r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp10 se1imp1d;
label define se1imp1d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1imp11 se1imp1k;
label define se1imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se1cal01 se1cal0d;
label define se1cal0d
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
label values se2ind   se2ind; 
label define se2ind  
	0           "Not in universe"               
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
label values se22202  se22202l;
label define se22202l
	0           "Not answered"                  
	9           "Not in universe"               
;
label values se22212  se22212l;
label define se22212l
	-9          "Not in universe"               
	0           "None"                          
;
label values se22214  se22214l;
label define se22214l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22260"          
;
label values se22216  se22216l;
label define se22216l
	0           "Not in universe"               
	1           "Yes - skip to SE22232"         
	2           "No"                            
;
label values se22218  se22218l;
label define se22218l
	0           "Not in universe"               
	1           "1 employee"                    
	2           "2 employees"                   
	3           "3 - 5 employees"               
	4           "6 or more employees"           
;
label values se22220  se22220l;
label define se22220l
	0           "Not in universe"               
	1           "Yes - skip to SE22224"         
	2           "No"                            
;
label values se22222  se22222l;
label define se22222l
	0           "Not in universe"               
	1           "Sole proprietorship - skip to SE22232"
	2           "Partnership"                   
;
label values se22224  se22224l;
label define se22224l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22232"          
;
label values se22226  se22226l;
label define se22226l
	0           "Not in universe"               
;
label values se22228  se22228l;
label define se22228l
	0           "Not in universe"               
;
label values se22230  se22230l;
label define se22230l
	0           "Not in universe"               
;
label values se22232  se22232l;
label define se22232l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22234  se22234l;
label define se22234l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22236  se22236l;
label define se22236l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22250"          
;
label values se22238  se22238l;
label define se22238l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22240  se22240l;
label define se22240l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22242  se22242l;
label define se22242l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22244  se22244l;
label define se22244l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se22246  se22246l;
label define se22246l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22250"          
;
label values se22248  se22248l;
label define se22248l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22250  se22250l;
label define se22250l
	0           "Not in universe"               
	1           "Yes - skip to SE22262"         
	2           "No"                            
;
label values se22252  se22252l;
label define se22252l
	0           "Not in universe"               
	1           "Yes - skip to SE22262"         
	2           "No"                            
;
label values se22254  se22254l;
label define se22254l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SE22262"          
;
label values se22256  se22256l;
label define se22256l
	0           "Not in universe"               
;
label values se22260  se22260l;
label define se22260l
	-0009       "Not in universe"               
	0           "None"                          
;
label values se2imp01 se2imp0d;
label define se2imp0d
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
label values se2imp09 se2imp0r;
label define se2imp0r
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp10 se2imp1d;
label define se2imp1d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2imp11 se2imp1k;
label define se2imp1k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values se2cal01 se2cal0d;
label define se2cal0d
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
label values kidssyn1 kidssynd;
label define kidssynd
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
label values kdssamt1 kdssamtd;
label define kdssamtd
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
	1           "Adult benefits received in own name only"
	2           "Only adult benefits received jointly wit"
	3           "Only child benefits received"  
	4           "Adult benefits received in own name and"
	5           "Adult benefits received jointly with"
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
	2           "No - skip to SC3070 through SC3084"
;
label values ss3012   ss3012l;
label define ss3012l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC3016 through SC3030"
;
label values ss3014   ss3014l;
label define ss3014l 
	0           "Not in universe"               
	1           "Yes - skip to next ISS code"   
	2           "No"                            
;
label values ss3064   ss3064l;
label define ss3064l 
	-1          "Dk"                            
	0           "Not in universe"               
	1           "Green"                         
	2           "Gold"                          
	3           "Other"                         
;
label values ss3066   ss3066l;
label define ss3066l 
	-1          "Dk"                            
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
	1           "Adult benefits received in own name only"
	2           "Only adult benefits received jointly"
	3           "Only child benefits received"  
	4           "Adult benefits received in own name and"
	5           "Adult benefits received jointly with"
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
	2           "No - skip to SC3070 through SC3084"
;
label values rr3012   rr3012l;
label define rr3012l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to SC3016 through SC3030"
;
label values rr3014   rr3014l;
label define rr3014l 
	0           "Not in universe"               
	1           "Yes - skip to next ISS code"   
	2           "No"                            
;
label values rr3064   rr3064l;
label define rr3064l 
	-1          "Dk"                            
	0           "Not in universe"               
	1           "Green"                         
	2           "Gold"                          
	3           "Other"                         
;
label values rr3066   rr3066l;
label define rr3066l 
	-1          "Dk"                            
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
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4302   sc4302l;
label define sc4302l 
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4304   sc4304l;
label define sc4304l 
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4306   sc4306l;
label define sc4306l 
	0           "Not marked as a type of asset owned"
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
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4402   sc4402l;
label define sc4402l 
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4404   sc4404l;
label define sc4404l 
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4406   sc4406l;
label define sc4406l 
	0           "Not marked as a type of asset owned"
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
	0           "NOne - skip to SC4512"         
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
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4702   sc4702l;
label define sc4702l 
	0           "Not marked as a type of asset owned"
	1           "Marked as a type of asset owned"
	9           "Not in universe"               
;
label values sc4704   sc4704l;
label define sc4704l 
	0           "Not marked as a type of asset owned"
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
	2           "First re-release, etc."        
;
label values tm8000   tm8000l;
label define tm8000l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8334"           
;
label values tm8002   tm8002l;
label define tm8002l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8024"           
;
label values tm8004   tm8004l;
label define tm8004l 
	0           "Not in universe"               
	1           "Academic or college preparatory"
	2           "Vocational"                    
	3           "Business or commercial"        
	4           "General"                       
	5           "Some other type"               
;
label values tm8006   tm8006l;
label define tm8006l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8008   tm8008l;
label define tm8008l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8010   tm8010l;
label define tm8010l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8012   tm8012l;
label define tm8012l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8014   tm8014l;
label define tm8014l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8016   tm8016l;
label define tm8016l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8018   tm8018l;
label define tm8018l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8020   tm8020l;
label define tm8020l 
	0           "Not in universe"               
	1           "Public"                        
	2           "Private"                       
;
label values tm8022   tm8022l;
label define tm8022l 
	0           "Not in universe"               
	1           "Yes - skip to TM8026"          
	2           "No"                            
;
label values tm8024   tm8024l;
label define tm8024l 
	0           "Not in universe"               
	1           "Yes - skip to TM8042"          
	2           "No - skip to TM8042"           
;
label values tm8026   tm8026l;
label define tm8026l 
	-1          "Dk"                            
	0           "Not in universe"               
;
label values tm8028   tm8028l;
label define tm8028l 
	0           "Not in universe"               
	1           "PHD or equivalent"             
	2           "Professional degree such as"   
	3           "Master's degree"               
	4           "Bachelor's degree"             
	5           "Associate degree"              
	6           "Vocational certificate or diploma"
	7           "Has not earned a degree -"     
;
label values tm8030   tm8030l;
label define tm8030l 
	-1          "Dk"                            
	0           "Not in universe"               
;
label values tm8032   tm8032l;
label define tm8032l 
	-1          "Dk"                            
	0           "Not in universe"               
	1           "Agriculture or Forestry"       
	2           "Biology"                       
	3           "Business or Management"        
	4           "Economics"                     
	5           "Education"                     
	6           "Engineering (including computers"
	7           "English or Journalism"         
	8           "Home Economics"                
	9           "Law"                           
	10          "Liberal Arts or Humanities"    
	11          "Mathematics or Statistics"     
	12          "Medicine or Dentistry"         
	13          "Nursing, Pharmacy, or Health"  
	14          "Physical or Earth Sciences"    
	15          "Police Science or Law Enforcement"
	16          "Psychology"                    
	17          "Religion or Theology"          
	18          "Social Sciences (history,"     
	19          "Vocational - Technical Studies"
	20          "Other"                         
;
label values tm8034   tm8034l;
label define tm8034l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8042"           
;
label values tm8036   tm8036l;
label define tm8036l 
	-1          "Dk - skip to TM8042"           
	0           "Not in universe"               
;
label values tm8038   tm8038l;
label define tm8038l 
	0           "Not in universe"               
	1           "Agriculture or Forestry"       
	2           "Biology"                       
	3           "Business or Management"        
	4           "Economics"                     
	5           "Education"                     
	6           "Engineering (including computers"
	7           "English or Journalism"         
	8           "Home economics"                
	9           "Law"                           
	10          "Liberal Arts or Humanities"    
	11          "Mathematics or Statistics"     
	12          "Medicine or Dentistry"         
	13          "Nursing, Pharmacy, or Health"  
	14          "Physical or Earth Sciences"    
	15          "Police Science or Law Enforcement"
	16          "Psychology"                    
	17          "Religion or Theology"          
	18          "Social Sciences (history,"     
	19          "Vocational - Technical Studies"
	20          "Other"                         
;
label values tm8040   tm8040l;
label define tm8040l 
	-1          "Dk"                            
	0           "Not in universe"               
	0001        "Is still a student"            
;
label values tm8042   tm8042l;
label define tm8042l 
	0           "Not in universe"               
	1           "Yes - skip to TM8136"          
	2           "No"                            
;
label values tm8044   tm8044l;
label define tm8044l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8136"           
;
label values tm8046   tm8046l;
label define tm8046l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8048   tm8048l;
label define tm8048l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8050   tm8050l;
label define tm8050l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8052   tm8052l;
label define tm8052l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8054   tm8054l;
label define tm8054l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8056   tm8056l;
label define tm8056l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8058   tm8058l;
label define tm8058l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8060   tm8060l;
label define tm8060l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8062   tm8062l;
label define tm8062l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8064   tm8064l;
label define tm8064l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8066   tm8066l;
label define tm8066l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8068   tm8068l;
label define tm8068l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8070   tm8070l;
label define tm8070l 
	0           "Not marked as training or not in"
	1           "Marked as training"            
;
label values tm8072   tm8072l;
label define tm8072l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8076"           
;
label values tm8074   tm8074l;
label define tm8074l 
	0           "Not in universe"               
	1           "Apprenticeship program"        
	2           "Business, commercial, or"      
	3           "Junior or community college"   
	4           "Program completed at a 4 year" 
	5           "High school vocational program"
	6           "Training program at work"      
	7           "Military (exclude basic training)"
	8           "Correspondence course"         
	9           "Training or experience received"
	10          "Sheltered workshop"            
	11          "Vocational rehabilitation centers"
	12          "Other"                         
;
label values tm8076   tm8076l;
label define tm8076l 
	-1          "Dk - skip to TM8136"           
	0           "Not in universe"               
	1           "Now attending"                 
	2           "1984"                          
	3           "1983"                          
	4           "1982"                          
	5           "1981"                          
	6           "1980"                          
	7           "1979 or before - skip to TM8136"
;
label values tm8078   tm8078l;
label define tm8078l 
	0           "Not in universe"               
;
label values tm8080   tm8080l;
label define tm8080l 
	-1          "Dk"                            
	0           "Not in universe"               
	1           "Less than one week"            
;
label values tm8082   tm8082l;
label define tm8082l 
	0           "Not in universe"               
	1           "Self or family"                
	2           "Employer"                      
	3           "Federal, state, or local"      
	4           "Someone else"                  
;
label values tm8084   tm8084l;
label define tm8084l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8136"           
;
label values tm8086   tm8086l;
label define tm8086l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8088   tm8088l;
label define tm8088l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8094   tm8094l;
label define tm8094l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8136"           
;
label values tm8096   tm8096l;
label define tm8096l 
	0           "Not in universe"               
	1           "The Job Training Partnership Act"
	2           "The Work Incentive Program (WIN)"
;
label values tm8098   tm8098l;
label define tm8098l 
	0           "Not in universe"               
	1           "1984"                          
	2           "1983"                          
	3           "1982"                          
;
label values tm8100   tm8100l;
label define tm8100l 
	0           "Not in universe"               
;
label values tm8102   tm8102l;
label define tm8102l 
	0           "Not in universe"               
	1           "Less than 1 week"              
;
label values tm8104   tm8104l;
label define tm8104l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8106   tm8106l;
label define tm8106l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8108   tm8108l;
label define tm8108l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8110   tm8110l;
label define tm8110l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8112   tm8112l;
label define tm8112l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8114   tm8114l;
label define tm8114l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8116   tm8116l;
label define tm8116l 
	0           "Not in universe"               
	1           "The Job Training Partnership Act"
	2           "The Work Incentive Program (WIN)"
;
label values tm8118   tm8118l;
label define tm8118l 
	0           "Not in universe"               
	1           "1984"                          
	2           "1983"                          
	3           "1982"                          
;
label values tm8120   tm8120l;
label define tm8120l 
	0           "Not in universe"               
;
label values tm8122   tm8122l;
label define tm8122l 
	0           "Not in universe"               
	1           "Less than 1 week"              
;
label values tm8124   tm8124l;
label define tm8124l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8126   tm8126l;
label define tm8126l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8128   tm8128l;
label define tm8128l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8130   tm8130l;
label define tm8130l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8132   tm8132l;
label define tm8132l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8134   tm8134l;
label define tm8134l 
	0           "Not marked as a type of training"
	1           "Marked as a type of training"  
;
label values tm8136   tm8136l;
label define tm8136l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8140"           
;
label values tm8138   tm8138l;
label define tm8138l 
	0           "Not in universe"               
	1           "Worked for an employer - skip" 
	2           "Self-employed - skip to TM8176"
;
label values tm8140   tm8140l;
label define tm8140l 
	-3          "Never worked for 2 consecutive"
	0           "Not in universe"               
;
label values tm8142   tm8142l;
label define tm8142l 
	0           "Not in universe"               
	1           "Taking care of home or family -"
	2           "Ill or disabled - skip to"     
	3           "Going to school - skip to"     
	4           "Couldn't find work - skip to"  
	5           "Didn't want to work - skip to" 
	7           "Other - skip to TM8334"        
;
label values tm8160   tm8160l;
label define tm8160l 
	0           "Not in universe"               
	1           "Worked for an employer"        
	2           "Self-employed"                 
;
label values tm8162   tm8162l;
label define tm8162l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8286"           
;
label values tm8164   tm8164l;
label define tm8164l 
	0           "Not in universe"               
	1           "Yes - skip to TM8176 through"  
	2           "No"                            
;
label values tm8166   tm8166l;
label define tm8166l 
	0           "Not in universe"               
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 or more"                   
;
label values tm8168   tm8168l;
label define tm8168l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8172"           
;
label values tm8170   tm8170l;
label define tm8170l 
	0           "Not in universe"               
	1           "Under 25"                      
	2           "25 to 99"                      
	3           "100 to 499"                    
	4           "500 to 999"                    
	5           "1000 or more"                  
;
label values tm8172   tm8172l;
label define tm8172l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8174   tm8174l;
label define tm8174l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8176   tm8176l;
label define tm8176l 
	0           "Not in universe"               
;
label values tm8178   tm8178l;
label define tm8178l 
	0           "Not in universe"               
;
label values tm8182   tm8182l;
label define tm8182l 
	0           "Not in universe"               
;
label values tm8184   tm8184l;
label define tm8184l 
	0           "Not in universe"               
;
label values tm8186   tm8186l;
label define tm8186l 
	0           "Not in universe"               
;
label values tm8188   tm8188l;
label define tm8188l 
	0           "Not in universe"               
;
label values tm8192   tm8192l;
label define tm8192l 
	-1          "Dk"                            
	0           "Not in universe"               
;
label values tm8194   tm8194l;
label define tm8194l 
	0           "Not in universe"               
;
label values tm8196   tm8196l;
label define tm8196l 
	0           "Not in universe"               
;
label values tm8200   tm8200l;
label define tm8200l 
	0           "Not in universe"               
	1           "Yes - skip TM8204"             
	2           "No"                            
;
label values tm8202   tm8202l;
label define tm8202l 
	0           "Not in universe"               
	1           "Layoff, plant closed"          
	2           "Discharged"                    
	3           "Found a better job"            
	4           "Retirement"                    
	5           "Did not like working conditions"
	6           "Dissatisfied with earnings"    
	7           "Family or personal reasons"    
	8           "Did not like location"         
	9           "Other"                         
;
label values tm8204   tm8204l;
label define tm8204l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8334"           
;
label values tm8206   tm8206l;
label define tm8206l 
	0           "Not in universe"               
	1           "Yes - skip to TM8288"          
	2           "No"                            
;
label values tm8208   tm8208l;
label define tm8208l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8288"           
;
label values tm8260   tm8260l;
label define tm8260l 
	0           "Not in universe"               
	1           "Worked for an employer"        
	2           "Self-employed"                 
;
label values tm8262   tm8262l;
label define tm8262l 
	-1          "Dk"                            
	0           "Not in universe"               
;
label values tm8264   tm8264l;
label define tm8264l 
	-1          "Dk"                            
	0           "Not in universe"               
;
label values tm8266   tm8266l;
label define tm8266l 
	-1          "Dk"                            
	0           "Not in universe"               
;
label values tm8268   tm8268l;
label define tm8268l 
	0           "Not in universe"               
;
label values tm8270   tm8270l;
label define tm8270l 
	0           "Not in universe"               
;
label values tm8272   tm8272l;
label define tm8272l 
	0           "Not in universe"               
;
label values tm8274   tm8274l;
label define tm8274l 
	0           "Not in universe"               
;
label values tm8278   tm8278l;
label define tm8278l 
	0           "Not in universe"               
;
label values tm8280   tm8280l;
label define tm8280l 
	0           "Not in universe"               
;
label values tm8282   tm8282l;
label define tm8282l 
	0           "Not in universe"               
;
label values tm8284   tm8284l;
label define tm8284l 
	-1          "Dk"                            
	-3          "None"                          
	0           "Not in universe"               
;
label values tm8286   tm8286l;
label define tm8286l 
	0           "Not in universe"               
	1           "Layoff, plant closed"          
	2           "Discharged"                    
	3           "Found a better job"            
	4           "Retirement"                    
	5           "Did not like working conditions"
	6           "Dissatisfied with earnings"    
	7           "Family or personal reasons"    
	8           "Did not like location"         
	9           "Other"                         
;
label values tm8288   tm8288l;
label define tm8288l 
	-1          "Dk - skip to TM8294"           
	-3          "Never worked 6 straight months"
	0           "Not in universe"               
;
label values tm8290   tm8290l;
label define tm8290l 
	-1          "Dk"                            
	-5          "All years"                     
	0           "Not in universe"               
;
label values tm8292   tm8292l;
label define tm8292l 
	0           "Not in universe"               
	1           "Full-time"                     
	2           "Part-time"                     
;
label values tm8294   tm8294l;
label define tm8294l 
	0           "Not in universe"               
	1           "Yes - skip to TM8334"          
	2           "No"                            
;
label values tm8296   tm8296l;
label define tm8296l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8334"           
;
label values tm8298   tm8298l;
label define tm8298l 
	0           "Not in universe"               
;
label values tm8300   tm8300l;
label define tm8300l 
	0           "Not in universe"               
;
label values tm8304   tm8304l;
label define tm8304l 
	0           "Not in universe"               
	1           "Took care of family or home"   
	2           "Own illness or disability"     
	3           "Could not find work"           
	4           "Going to school"               
	5           "Other"                         
;
label values tm8306   tm8306l;
label define tm8306l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8334"           
;
label values tm8308   tm8308l;
label define tm8308l 
	0           "Not in universe"               
	1           "One time"                      
	2           "Two times"                     
	3           "Three or more times"           
;
label values tm8310   tm8310l;
label define tm8310l 
	0           "Not in universe"               
;
label values tm8312   tm8312l;
label define tm8312l 
	0           "Not in universe"               
;
label values tm8316   tm8316l;
label define tm8316l 
	0           "Not in universe"               
	1           "Took care of family or home"   
	2           "Own illness or disability"     
	3           "Could not find work"           
	4           "Going to school"               
	5           "Other"                         
;
label values tm8318   tm8318l;
label define tm8318l 
	0           "Not in universe"               
;
label values tm8320   tm8320l;
label define tm8320l 
	0           "Not in universe"               
;
label values tm8324   tm8324l;
label define tm8324l 
	0           "Not in universe"               
	1           "Took care of family or home"   
	2           "Own illness or disability"     
	3           "Could not find work"           
	4           "Going to school"               
	5           "Other"                         
;
label values tm8326   tm8326l;
label define tm8326l 
	0           "Not in universe"               
;
label values tm8328   tm8328l;
label define tm8328l 
	0           "Not in universe"               
;
label values tm8332   tm8332l;
label define tm8332l 
	0           "Not in universe"               
	1           "Took care of family or home"   
	2           "Own illness or disability"     
	3           "Could not find work"           
	4           "Going to school"               
	5           "Other"                         
;
label values tmimp01  tmimp01l;
label define tmimp01l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp02  tmimp02l;
label define tmimp02l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp03  tmimp03l;
label define tmimp03l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp04  tmimp04l;
label define tmimp04l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp05  tmimp05l;
label define tmimp05l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp06  tmimp06l;
label define tmimp06l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp07  tmimp07l;
label define tmimp07l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp08  tmimp08l;
label define tmimp08l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp09  tmimp09l;
label define tmimp09l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp10  tmimp10l;
label define tmimp10l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp11  tmimp11l;
label define tmimp11l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp12  tmimp12l;
label define tmimp12l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp13  tmimp13l;
label define tmimp13l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp14  tmimp14l;
label define tmimp14l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp15  tmimp15l;
label define tmimp15l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp16  tmimp16l;
label define tmimp16l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp17  tmimp17l;
label define tmimp17l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp18  tmimp18l;
label define tmimp18l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp19  tmimp19l;
label define tmimp19l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp20  tmimp20l;
label define tmimp20l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp21  tmimp21l;
label define tmimp21l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp22  tmimp22l;
label define tmimp22l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp23  tmimp23l;
label define tmimp23l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp24  tmimp24l;
label define tmimp24l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp25  tmimp25l;
label define tmimp25l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp26  tmimp26l;
label define tmimp26l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp27  tmimp27l;
label define tmimp27l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp30  tmimp30l;
label define tmimp30l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp31  tmimp31l;
label define tmimp31l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp32  tmimp32l;
label define tmimp32l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp33  tmimp33l;
label define tmimp33l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp34  tmimp34l;
label define tmimp34l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp35  tmimp35l;
label define tmimp35l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp36  tmimp36l;
label define tmimp36l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp37  tmimp37l;
label define tmimp37l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp38  tmimp38l;
label define tmimp38l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp39  tmimp39l;
label define tmimp39l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp40  tmimp40l;
label define tmimp40l
	0           "Not imputed"                   
	1           "IMputed"                       
;
label values tmimp41  tmimp41l;
label define tmimp41l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp42  tmimp42l;
label define tmimp42l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp43  tmimp43l;
label define tmimp43l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp44  tmimp44l;
label define tmimp44l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp45  tmimp45l;
label define tmimp45l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp46  tmimp46l;
label define tmimp46l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp47  tmimp47l;
label define tmimp47l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp48  tmimp48l;
label define tmimp48l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp49  tmimp49l;
label define tmimp49l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp55  tmimp55l;
label define tmimp55l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp56  tmimp56l;
label define tmimp56l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp57  tmimp57l;
label define tmimp57l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp58  tmimp58l;
label define tmimp58l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp59  tmimp59l;
label define tmimp59l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp60  tmimp60l;
label define tmimp60l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp61  tmimp61l;
label define tmimp61l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp62  tmimp62l;
label define tmimp62l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp63  tmimp63l;
label define tmimp63l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp64  tmimp64l;
label define tmimp64l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp65  tmimp65l;
label define tmimp65l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp66  tmimp66l;
label define tmimp66l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp67  tmimp67l;
label define tmimp67l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp68  tmimp68l;
label define tmimp68l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp69  tmimp69l;
label define tmimp69l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp70  tmimp70l;
label define tmimp70l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp71  tmimp71l;
label define tmimp71l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp72  tmimp72l;
label define tmimp72l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp73  tmimp73l;
label define tmimp73l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp74  tmimp74l;
label define tmimp74l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp75  tmimp75l;
label define tmimp75l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp76  tmimp76l;
label define tmimp76l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp77  tmimp77l;
label define tmimp77l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp78  tmimp78l;
label define tmimp78l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp79  tmimp79l;
label define tmimp79l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp80  tmimp80l;
label define tmimp80l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim147  tmim147l;
label define tmim147l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim148  tmim148l;
label define tmim148l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim153  tmim153l;
label define tmim153l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tm8334   tm8334l;
label define tm8334l 
	0           "Not in universe"               
	1           "Excellent"                     
	2           "Very good"                     
	3           "Good"                          
	4           "Fair"                          
	5           "Poor"                          
;
label values tm8336   tm8336l;
label define tm8336l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8340"           
;
label values tm8338   tm8338l;
label define tm8338l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8340   tm8340l;
label define tm8340l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8342   tm8342l;
label define tm8342l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8346"           
;
label values tm8344   tm8344l;
label define tm8344l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8346   tm8346l;
label define tm8346l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8348   tm8348l;
label define tm8348l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8350   tm8350l;
label define tm8350l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8354"           
;
label values tm8352   tm8352l;
label define tm8352l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8354   tm8354l;
label define tm8354l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8358"           
;
label values tm8356   tm8356l;
label define tm8356l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8358   tm8358l;
label define tm8358l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8362"           
;
label values tm8360   tm8360l;
label define tm8360l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8362   tm8362l;
label define tm8362l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8396"           
;
label values tm8364   tm8364l;
label define tm8364l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8366   tm8366l;
label define tm8366l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8370"           
;
label values tm8368   tm8368l;
label define tm8368l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8370   tm8370l;
label define tm8370l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8374"           
;
label values tm8372   tm8372l;
label define tm8372l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8374   tm8374l;
label define tm8374l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8396"           
;
label values tm8376   tm8376l;
label define tm8376l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8378   tm8378l;
label define tm8378l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8380   tm8380l;
label define tm8380l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8382   tm8382l;
label define tm8382l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8384   tm8384l;
label define tm8384l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8386   tm8386l;
label define tm8386l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8388   tm8388l;
label define tm8388l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8390   tm8390l;
label define tm8390l 
	0           "Not in universe or receives help"
	1           "Does not receive help - skip to"
;
label values tm8392   tm8392l;
label define tm8392l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8394   tm8394l;
label define tm8394l 
	0           "Not in universe"               
	1           "Arthritis or rheumatism"       
	2           "Back or spine problems (including"
	3           "Blindness or vision problems"  
	4           "Cancer"                        
	5           "Deafness or serious trouble"   
	6           "Diabetes"                      
	7           "Heart trouble (including heart"
	8           "Hernia or rupture"             
	9           "High blood pressure (hypertension)"
	10          "Kidney stones or chronic kidney"
	11          "Lung or respiratory trouble"   
	12          "Mental illness"                
	13          "Mental retardation"            
	14          "Missing legs, feet, arms, hands,"
	15          "Nervous or emotional problems, or"
	16          "Paralysis of any kind"         
	17          "Senility (Altzheimer's Disease)"
	18          "Stiffness or deformity of the" 
	19          "Stomach trouble (including ulcers,"
	20          "Stroke"                        
	21          "Thyroid trouble or goiter"     
	22          "Tumor, cyst or growth"         
	23          "Other"                         
;
label values tm8396   tm8396l;
label define tm8396l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8398   tm8398l;
label define tm8398l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8400   tm8400l;
label define tm8400l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8424"           
;
label values tm8402   tm8402l;
label define tm8402l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8404   tm8404l;
label define tm8404l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8406   tm8406l;
label define tm8406l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8408   tm8408l;
label define tm8408l 
	0           "Not marked as someone who helps or"
	1           "Marked as someone who helps"   
;
label values tm8410   tm8410l;
label define tm8410l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8412   tm8412l;
label define tm8412l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8414   tm8414l;
label define tm8414l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8416   tm8416l;
label define tm8416l 
	0           "Not in universe or receives help"
	1           "Does not receive help - skip to"
;
label values tm8418   tm8418l;
label define tm8418l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8420   tm8420l;
label define tm8420l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8422   tm8422l;
label define tm8422l 
	0           "Not in universe"               
	1           "Arthritis or rheumatism"       
	2           "Back or spine problems (including"
	3           "Blindness or vision problems"  
	4           "Cancer"                        
	5           "Deafness or serious trouble"   
	6           "Diabetes"                      
	7           "Heart trouble (including heart"
	8           "Hernia or rupture"             
	9           "High blood pressure (hypertension)"
	10          "Kidney stones or chronic kidney"
	11          "Lung or respiratory trouble"   
	12          "Mental illness"                
	13          "Mental retardation"            
	14          "Missing legs, feet, arms, hands,"
	15          "Nervous or emotional problems, or"
	16          "Paralysis of any kind"         
	17          "Senility (Altzheimer's Disease)"
	18          "Stiffness or deformity of the" 
	19          "Stomach trouble (including ulcers,"
	20          "Stroke"                        
	21          "Thyroid trouble or goiter"     
	22          "Tumor, cyst or growth"         
	23          "Other"                         
;
label values tm8424   tm8424l;
label define tm8424l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8444"           
;
label values tm8426   tm8426l;
label define tm8426l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8428   tm8428l;
label define tm8428l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8430   tm8430l;
label define tm8430l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8432   tm8432l;
label define tm8432l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8434   tm8434l;
label define tm8434l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8436   tm8436l;
label define tm8436l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8438   tm8438l;
label define tm8438l 
	0           "Not marked as someone who helps"
	1           "Marked as someone who helps"   
;
label values tm8440   tm8440l;
label define tm8440l 
	0           "Not in universe or receives help"
	1           "Does not receive help - skip to"
;
label values tm8442   tm8442l;
label define tm8442l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8444   tm8444l;
label define tm8444l 
	0           "Not in universe"               
	1           "15 years - skip to TM8484"     
	2           "16 to 72 years"                
	3           "73 years or over - skip to"    
;
label values tm8446   tm8446l;
label define tm8446l 
	0           "Not in universe"               
	1           "Yes - skip to TM8450"          
	2           "No"                            
;
label values tm8448   tm8448l;
label define tm8448l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8452"           
;
label values tm8450   tm8450l;
label define tm8450l 
	0           "Not in universe"               
	1           "Yes - skip to TM8454"          
	2           "No - skip to TM8484"           
;
label values tm8452   tm8452l;
label define tm8452l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8484"           
;
label values tm8454   tm8454l;
label define tm8454l 
	0           "Not in universe"               
	0001        "Person was limited before person"
;
label values tm8456   tm8456l;
label define tm8456l 
	0           "Not in universe"               
;
label values tm8458   tm8458l;
label define tm8458l 
	0           "Not in universe"               
	1           "Yes - skip to TM8462"          
	2           "No"                            
;
label values tm8460   tm8460l;
label define tm8460l 
	0           "Not in universe"               
	0001        "Had never been employed before"
;
label values tm8462   tm8462l;
label define tm8462l 
	0           "Not in universe"               
	1           "Arthritis or rheumatism"       
	2           "Back or spine problems (including"
	3           "Blindness or vision problems"  
	4           "Cancer"                        
	5           "Deafness or serious trouble"   
	6           "Diabetes"                      
	7           "Heart trouble (including heart"
	8           "Hernia or rupture"             
	9           "High blood pressure (hypertension)"
	10          "Kidney stones or chronic kidney"
	11          "Lung or respiratory trouble"   
	12          "Mental illness"                
	13          "Mental retardation"            
	14          "Missing legs, feet, arms, hands,"
	15          "Nervous or emotional problems, or"
	16          "Paralysis of any kind"         
	17          "Senility (Altzheimer's Disease)"
	18          "Stiffness or deformity of the" 
	19          "Stomach trouble (including ulcers,"
	20          "Stroke"                        
	21          "Thyroid trouble or goiter"     
	22          "Tumor, cyst or growth"         
	23          "Other"                         
;
label values tm8464   tm8464l;
label define tm8464l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8468"           
;
label values tm8466   tm8466l;
label define tm8466l 
	0           "Not in universe"               
	1           "On your job"                   
	2           "During service in the Armed"   
	3           "In your home"                  
	4           "Somewhere else"                
;
label values tm8468   tm8468l;
label define tm8468l 
	0           "Not in universe"               
	1           "Yes - skip to TM8476"          
	2           "No"                            
;
label values tm8470   tm8470l;
label define tm8470l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8478"           
;
label values tm8472   tm8472l;
label define tm8472l 
	0           "Not in universe"               
	0001        "Has never been able to work at"
;
label values tm8474   tm8474l;
label define tm8474l 
	0           "Not in universe"               
;
label values tm8476   tm8476l;
label define tm8476l 
	0           "Not in universe"               
	1           "Yes - skip to TM8480"          
	2           "No"                            
;
label values tm8478   tm8478l;
label define tm8478l 
	0           "Not in universe"               
	1           "Full time"                     
	2           "Part time"                     
;
label values tm8480   tm8480l;
label define tm8480l 
	0           "Not in universe"               
	1           "Regularly"                     
	2           "Only occasionally or irregularly"
;
label values tm8482   tm8482l;
label define tm8482l 
	0           "Not in universe"               
	1           "Yes, able to do same kind of"  
	2           "No, not able to do same kind of"
	3           "Did not work before limitation"
;
label values tm8484   tm8484l;
label define tm8484l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8496"           
;
label values tm8486   tm8486l;
label define tm8486l 
	0           "Not in universe"               
;
label values tm8488   tm8488l;
label define tm8488l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8490   tm8490l;
label define tm8490l 
	0           "Not in universe"               
;
label values tm8492   tm8492l;
label define tm8492l 
	-5          "All nights"                    
	0           "Not in universe"               
;
label values tm8494   tm8494l;
label define tm8494l 
	-3          "None"                          
	0           "Not in universe"               
;
label values tm8496   tm8496l;
label define tm8496l 
	-3          "None"                          
	-5          "All days"                      
	0           "Not in universe"               
;
label values tm8498   tm8498l;
label define tm8498l 
	0           "Not in universe"               
;
label values tm8500   tm8500l;
label define tm8500l 
	-3          "None"                          
	0           "Not in universe"               
;
label values tm8502   tm8502l;
label define tm8502l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8506"           
;
label values tm8504   tm8504l;
label define tm8504l 
	0           "Not in universe"               
	1           "Doctor's office (private doctor)"
	2           "VA or military hospital"       
	3           "Hospital outpatient clinic (not"
	4           "Hospital emergency room"       
	5           "Company or industry clinic"    
	6           "Health center (neighborhood"   
	7           "Other"                         
;
label values tm8506   tm8506l;
label define tm8506l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8518"           
;
label values tm8508   tm8508l;
label define tm8508l 
	0           "Not in universe"               
	1           "Blue Cross/Blue Shield"        
	2           "Other"                         
;
label values tm8510   tm8510l;
label define tm8510l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8512   tm8512l;
label define tm8512l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8540"           
;
label values tm8514   tm8514l;
label define tm8514l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8516   tm8516l;
label define tm8516l 
	0           "Not in universe"               
	1           "Yes - skip to TM8540"          
	2           "No - skip to TM8540"           
;
label values tm8518   tm8518l;
label define tm8518l 
	0           "Not in universe"               
	1           "Yes - skip to TM8540"          
	2           "No"                            
;
label values tm8520   tm8520l;
label define tm8520l 
	0           "Not in universe"               
	1           "Yes - skip to TM8540"          
	2           "No"                            
;
label values tm8522   tm8522l;
label define tm8522l 
	0           "Not in universe"               
	1           "Correct"                       
	2           "CHAMPUS"                       
	4           "Some other plan - skip to"     
;
label values tm8524   tm8524l;
label define tm8524l 
	0           "Not in universe"               
	1           "Job layoff, job loss, or any"  
	2           "Can't obtain insurance because"
	3           "Too expensive, can't afford"   
	4           "Dissatisfied with previous"    
	5           "Don't believe in insurance"    
	6           "Have been healthy, not much"   
	7           "Able to go to VA or military"  
	8           "Covered by some other health plan"
	9           "Other"                         
;
label values tm8526   tm8526l;
label define tm8526l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8540"           
;
label values tm8528   tm8528l;
label define tm8528l 
	0           "Not in universe"               
	1           "Private"                       
	2           "Medicaid"                      
	3           "CHAMPUS, CHAMPVA"              
	4           "Other"                         
;
label values tm8530   tm8530l;
label define tm8530l 
	0           "Not in universe"               
;
label values tm8532   tm8532l;
label define tm8532l 
	0           "Not in universe"               
;
label values tm8534   tm8534l;
label define tm8534l 
	0           "Not in universe"               
	1           "Lost job or changed employers" 
	2           "Spouse (parent) lost job or"   
	3           "Death of spouse or parent"     
	4           "Became divorced or separated"  
	5           "Became ineligible because of age"
	6           "Other"                         
;
label values tm8536   tm8536l;
label define tm8536l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8540"           
;
label values tm8538   tm8538l;
label define tm8538l 
	0           "Not in universe"               
	1           "Could not afford"              
	2           "Was rejected"                  
	3           "Other"                         
;
label values tm8540   tm8540l;
label define tm8540l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8542   tm8542l;
label define tm8542l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8556"           
;
label values tm8544   tm8544l;
label define tm8544l 
	0           "Not marked as a child or not in"
;
label values tm8546   tm8546l;
label define tm8546l 
	0           "Not marked as a child or not in"
;
label values tm8548   tm8548l;
label define tm8548l 
	0           "Not marked as a child or not in"
;
label values tm8550   tm8550l;
label define tm8550l 
	0           "Not marked as a health condition"
	1           "Arthritis or rheumatism"       
	2           "Back or spine problems (including"
	3           "Blindness or vision problems"  
	4           "Cancer"                        
	5           "Deafness or serious trouble"   
	6           "Diabetes"                      
	7           "Heart trouble (including heart"
	8           "Hernia or rupture"             
	9           "High blood pressure (hypertension)"
	10          "Kidney stones or chronic kidney"
	11          "Lung or respiratory trouble"   
	12          "Mental illness"                
	13          "Mental retardation"            
	14          "Missing legs, feet, arms, hands,"
	15          "Nervous or emotional problems, or"
	16          "Paralysis of any kind"         
	17          "Senility (Altzheimer's Disease)"
	18          "Stiffness or deformity of the" 
	19          "Stomach trouble (including ulcers,"
	20          "Stroke"                        
	21          "Thyroid trouble or goiter"     
	22          "Tumor, cyst or growth"         
	23          "Other"                         
;
label values tm8552   tm8552l;
label define tm8552l 
	0           "Not marked as a health condition"
	1           "Arthritis or rheumatism"       
	2           "Back or spine problems (including"
	3           "Blindness or vision problems"  
	4           "Cancer"                        
	5           "Deafness or serious trouble"   
	6           "Diabetes"                      
	7           "Heart trouble (including heart"
	8           "Hernia or rupture"             
	9           "High blood pressure (hypertension)"
	10          "Kidney stones or chronic kidney"
	11          "Lung or respiratory trouble"   
	12          "Mental illness"                
	13          "Mental retardation"            
	14          "Missing legs, feet, arms, hands,"
	15          "Nervous or emotional problems, or"
	16          "Paralysis of any kind"         
	17          "Senility (Altzheimer's Disease)"
	18          "Stiffness or deformity of the" 
	19          "Stomach trouble (including ulcers,"
	20          "Stroke"                        
	21          "Thyroid trouble or goiter"     
	22          "Tumor, cyst or growth"         
	23          "Other"                         
;
label values tm8554   tm8554l;
label define tm8554l 
	0           "Not marked as a health condition"
	1           "Arthritis or rheumatism"       
	2           "Back or spine problems (including"
	3           "Blindness or vision problems"  
	4           "Cancer"                        
	5           "Deafness or serious trouble"   
	6           "Diabetes"                      
	7           "Heart trouble (including heart"
	8           "Hernia or rupture"             
	9           "High blood pressure (hypertension)"
	10          "Kidney stones or chronic kidney"
	11          "Lung or respiratory trouble"   
	12          "Mental illness"                
	13          "Mental retardation"            
	14          "Missing legs, feet, arms, hands,"
	15          "Nervous or emotional problems, or"
	16          "Paralysis of any kind"         
	17          "Senility (Altzheimer's Disease)"
	18          "Stiffness or deformity of the" 
	19          "Stomach trouble (including ulcers,"
	20          "Stroke"                        
	21          "Thyroid trouble or goiter"     
	22          "Tumor, cyst or growth"         
	23          "Other"                         
;
label values tm8556   tm8556l;
label define tm8556l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No - skip to TM8564"           
;
label values tm8558   tm8558l;
label define tm8558l 
	0           "Not marked as a child or not in"
;
label values tm8560   tm8560l;
label define tm8560l 
	0           "Not marked as a child or not in"
;
label values tm8562   tm8562l;
label define tm8562l 
	0           "Not marked as a child or not in"
;
label values tm8564   tm8564l;
label define tm8564l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8566   tm8566l;
label define tm8566l 
	0           "Not marked as a child or not in"
;
label values tm8568   tm8568l;
label define tm8568l 
	0           "Not marked as a child or not in"
;
label values tm8570   tm8570l;
label define tm8570l 
	0           "Not marked as a child or not in"
;
label values tm8572   tm8572l;
label define tm8572l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8574   tm8574l;
label define tm8574l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tm8576   tm8576l;
label define tm8576l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values tmimp81  tmimp81l;
label define tmimp81l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp82  tmimp82l;
label define tmimp82l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp83  tmimp83l;
label define tmimp83l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp84  tmimp84l;
label define tmimp84l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp85  tmimp85l;
label define tmimp85l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp86  tmimp86l;
label define tmimp86l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp87  tmimp87l;
label define tmimp87l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp88  tmimp88l;
label define tmimp88l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp89  tmimp89l;
label define tmimp89l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp90  tmimp90l;
label define tmimp90l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp91  tmimp91l;
label define tmimp91l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp92  tmimp92l;
label define tmimp92l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp93  tmimp93l;
label define tmimp93l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp94  tmimp94l;
label define tmimp94l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp95  tmimp95l;
label define tmimp95l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp96  tmimp96l;
label define tmimp96l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp97  tmimp97l;
label define tmimp97l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp98  tmimp98l;
label define tmimp98l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmimp99  tmimp99l;
label define tmimp99l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim100  tmim100l;
label define tmim100l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim101  tmim101l;
label define tmim101l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim102  tmim102l;
label define tmim102l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim103  tmim103l;
label define tmim103l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim104  tmim104l;
label define tmim104l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim105  tmim105l;
label define tmim105l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim106  tmim106l;
label define tmim106l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim107  tmim107l;
label define tmim107l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim108  tmim108l;
label define tmim108l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim109  tmim109l;
label define tmim109l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim110  tmim110l;
label define tmim110l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim111  tmim111l;
label define tmim111l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim112  tmim112l;
label define tmim112l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim113  tmim113l;
label define tmim113l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim114  tmim114l;
label define tmim114l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim115  tmim115l;
label define tmim115l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim116  tmim116l;
label define tmim116l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim117  tmim117l;
label define tmim117l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim118  tmim118l;
label define tmim118l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim119  tmim119l;
label define tmim119l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim120  tmim120l;
label define tmim120l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim121  tmim121l;
label define tmim121l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim122  tmim122l;
label define tmim122l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim123  tmim123l;
label define tmim123l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim124  tmim124l;
label define tmim124l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim125  tmim125l;
label define tmim125l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim126  tmim126l;
label define tmim126l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim127  tmim127l;
label define tmim127l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim128  tmim128l;
label define tmim128l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim129  tmim129l;
label define tmim129l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim130  tmim130l;
label define tmim130l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim131  tmim131l;
label define tmim131l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim132  tmim132l;
label define tmim132l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim133  tmim133l;
label define tmim133l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim134  tmim134l;
label define tmim134l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim135  tmim135l;
label define tmim135l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim136  tmim136l;
label define tmim136l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim137  tmim137l;
label define tmim137l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim138  tmim138l;
label define tmim138l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim139  tmim139l;
label define tmim139l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim140  tmim140l;
label define tmim140l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim141  tmim141l;
label define tmim141l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim142  tmim142l;
label define tmim142l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim143  tmim143l;
label define tmim143l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim149  tmim149l;
label define tmim149l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim150  tmim150l;
label define tmim150l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim151  tmim151l;
label define tmim151l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim152  tmim152l;
label define tmim152l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim155  tmim155l;
label define tmim155l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values tmim156  tmim156l;
label define tmim156l
	0           "Not imputed"                   
	1           "Imputed"                       
;

#delimit cr
*save `dta_name' , replace

/*
Copyright 2006 shared by the National Bureau of Economic Research and Jean Roth

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
