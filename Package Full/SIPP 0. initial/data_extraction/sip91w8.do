log using sip91w8, text replace
set mem 1000m
*This program reads the 1991 SIPP Wave 8 Core Data File 
*Note:  This program is distributed under the GNU GPL. See end of
*this file and http://www.gnu.org/licenses/ for details.
*by Jean Roth Mon Jun  7 18:27:44 EDT 2004
*Please report errors to jroth@nber.org
*run with do sip91w8
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1991\sip91w8.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip91w8"

*Everything below this point are value labels

#delimit ;

;
label values sustate  sustate;
label define sustate 
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
label values surgc    surgc;  
label define surgc   
	0           "Not applicable for coverage"   
;
label values htype    htype;  
label define htype   
	0           "Not in household"              
	1           "Married couple family household"
	2           "Male householder family"       
	3           "Female householder family"     
	4           "Male householder nonfamily"    
	5           "Female householder nonfamily"  
	6           "Group quarters"                
;
label values hstate   hstate; 
label define hstate  
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
label values hmetro   hmetro; 
label define hmetro  
	0           "Not applicable"                
	1           "Metro"                         
	2           "Residual"                      
;
label values hmsa     hmsa;   
label define hmsa    
	0           "Not in universe or not"        
	7           "Boston-Lawrence-Salem, MA-NH"  
	10          "Buffalo-Niagara Falls, NY"     
	14          "Chicago-Gary Lake County, IL-IN"
	21          "Cincinnati-Hamilton, OH-KY"    
	28          "Cleveland-Akron-Lorraine, OH"  
	31          "Dallas-Fort Worth, TX"         
	34          "Denver-Boulder, CO"            
	35          "Detroit-Ann Arbor, MI"         
	41          "Hartford-New Britain-"         
	42          "Houston, TX"                   
	49          "Los Angeles-Anaheim-"          
	56          "Miami-Ft. Lauderdale, FL"      
	63          "Milwaukee-Racine, WI"          
	70          "New York-New Jersey-Long"      
	77          "Philadelphia-Wilmington-"      
	78          "Pittsburgh-Beaver Valley, PA"  
	79          "Portland-Vancouver, OR"        
	82          "St. Louis, MO"                 
	84          "San Francisco-Oakland-"        
	91          "Seattle-Tacoma, WA"            
	160         "Albany-Schenectady-Troy, NY"   
	200         "Albequerque, NM"               
	520         "Atlanta, CA"                   
	640         "Austin, TX"                    
	0680        "Bakersfield, CA"               
	760         "Baton Rouge, LA"               
	840         "Beaumont-Port Arthur, TX"      
	1000        "Birmingham, AL"                
	1520        "Charlotte-Gastonia-"           
	1720        "Colorado Springs, CO"          
	1840        "Columbus, OH"                  
	1880        "Corpus Christi, TX"            
	2000        "Dayton-Springfield, OH"        
	2320        "El Paso, TX"                   
	2400        "Eugene-Springfield, OR"        
	2560        "Fayetteville, NC"              
	2700        "Ft. Myers, FL"                 
	2760        "Fort Wayne, IN"                
	2840        "Fresno, CA"                    
	3120        "Greensboro--Winston-Salem--"   
	3160        "Greensville-Spartanburg, SC"   
	3240        "Harrisburg-Lebanon-Carlisle, PA"
	3320        "Honolulu, HI"                  
	3480        "Indianapolis, IN"              
	3600        "Jacksonville, FL"              
	3840        "Knoxville, TN"                 
	3980        "Lakeland-Winterhaven, FL"      
	4040        "Lansing-East Lansing, MI"      
	4720        "Madison, WI"                   
	4880        "McCallen-Edinburg-Mission, TX" 
	4900        "Melbourne-Titusville-"         
	4920        "Memphis, TN"                   
	5120        "Minneapolis-St. Paul, MN"      
	5160        "Mobile, AL"                    
	5360        "Nashville, TN"                 
	5480        "New Haven-Meriden, CT"         
	5560        "New Orleans, LA"               
	5720        "Norfolk-VA Beach-Newport"      
	5880        "Oklahoma City, OK"             
	5960        "Orlando, FL"                   
	6080        "Pensacola,  FL"                
	6200        "Phoenix, AZ"                   
	6640        "Raleigh-Durham, NC"            
	6840        "Rochester, NY"                 
	6880        "Rockford, IL"                  
	6920        "Sacramento, CA"                
	7120        "Salinas-Seaside-Monterey, CA"  
	7160        "Salt Lake City-Ogden, UT"      
	7240        "San Antonio, TX"               
	7320        "San Diego, CA"                 
	7560        "Scranton--Wilkes-Barre, PA"    
	8000        "Springfield, MA"               
	8120        "Stockton, CA"                  
	8160        "Syracuse, NY"                  
	8280        "Tampa-St.Petersburg-"          
	8400        "Toledo, Oh"                    
	8520        "Tucson, AZ"                    
	8560        "Tulsa, OK"                     
	8680        "Utica-Rome, NY"                
	8840        "Washington, DC-MD-VA"          
	8960        "West Palm Beach-Boca Raton-"   
	9240        "Worcester, MA"                 
;
label values hnssr    hnssr;  
label define hnssr   
	0           "Not applicable"                
;
label values haccess  haccess;
label define haccess 
	0           "Not applicable"                
	1           "Direct - skip to housing unit" 
	2           "Through another unit"          
;
label values hlvqtr   hlvqtr; 
label define hlvqtr  
	0           "Not applicable"                
	1           "House, apartment, flat"        
	2           "HU in nontransient hotel,"     
	3           "HU, permanent in transient"    
	4           "HU in rooming house"           
	5           "Mobile home or trailer with no"
	6           "Mobile home or trailer with one"
	7           "HU not specified above"        
	8           "Quarters not HU in rooming or" 
	9           "Unit not permanent in transient"
	10          "Unoccupied site for mobile home,"
	11          "Other unit not specified above"
;
label values hunits   hunits; 
label define hunits  
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
label values htenure  htenure;
label define htenure 
	0           "Not applicable"                
	1           "Owned or being bought by you or"
	2           "Rented for cash"               
	3           "Occupied without payment of"   
;
label values hpubhs   hpubhs; 
label define hpubhs  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values hlornt   hlornt; 
label define hlornt  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values htenureu htenureu;
label define htenureu
	0           "Not answered (Types B and C)"  
	1           "Owned or being bought by you or"
	2           "Rented for cash"               
	3           "Occupied without payment of"   
	9           "Not answered"                  
;
label values hpubhsu  hpubhsu;
label define hpubhsu 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	3           "Don't know"                    
;
label values hlorntu  hlorntu;
label define hlorntu 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
	3           "Don't know"                    
;
label values hitm36b  hitm36b;
label define hitm36b 
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
;
label values hmeans   hmeans; 
label define hmeans  
	0           "Not in universe or no persons" 
	1           "One or more persons in"        
	2           "No person in household received"
;
label values hcash    hcash;  
label define hcash   
	1           "One or more persons in"        
	2           "No person in household received"
;
label values hncash   hncash; 
label define hncash  
	1           "One or more persons in"        
	2           "One or more persons in"        
	3           "No person in hhld. received"   
;
label values phrent   phrent; 
label define phrent  
	0           "None, or not in universe"      
;
label values utils    utils;  
label define utils   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
	3           "Don't know"                    
;
label values henrgy   henrgy; 
label define henrgy  
	0           "No energy assistance"          
	1           "Checks sent to household"      
	2           "Coupons or vouchers sent to"   
	3           "Payments sent elsewhere"       
	4           "Checks and coupons or vouchers"
	5           "Checks sent to household and"  
	6           "Coupons or voucher sent to"    
	7           "All three types of assistance" 
;
label values eastamt  eastamt;
label define eastamt 
	0           "Not in universe"               
;
label values lunch    lunch;  
label define lunch   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values nkidshl  nkidshl;
label define nkidshl 
	0           "Not in universe"               
;
label values lchtot   lchtot; 
label define lchtot  
	0           "Not in universe"               
;
label values lchpt    lchpt;  
label define lchpt   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values lchfree  lchfree;
label define lchfree 
	0           "Not in universe"               
	1           "Free lunch"                    
	2           "Reduced-price lunch"           
	3           "Full-price lunch"              
;
label values lchcost  lchcost;
label define lchcost 
	0           "Not in universe"               
;
label values breakf   breakf; 
label define breakf  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values nkidsbf  nkidsbf;
label define nkidsbf 
	0           "Not in universe"               
;
label values bftot    bftot;  
label define bftot   
	-1          "Don't know"                    
	0           "Not in universe"               
;
label values bffree   bffree; 
label define bffree  
	0           "Not in universe"               
	1           "Free breakfast"                
	2           "Reduced-price breakfast"       
	3           "Full-price breakfast"          
;
label values iphrent  iphrent;
label define iphrent 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iutils   iutils; 
label define iutils  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ihenrgy  ihenrgy;
label define ihenrgy 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ieastamt ieastamt;
label define ieastamt
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ilunch   ilunch; 
label define ilunch  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values inkidshl inkidshl;
label define inkidshl
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ilchtot  ilchtot;
label define ilchtot 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ilchpt   ilchpt; 
label define ilchpt  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ilchfree ilchfree;
label define ilchfree
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ilchcost ilchcost;
label define ilchcost
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ibreakf  ibreakf;
label define ibreakf 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values inkidsbf inkidsbf;
label define inkidsbf
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ibftot   ibftot; 
label define ibftot  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ibffree  ibffree;
label define ibffree 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values h5ref    h5ref;  
label define h5ref   
	0           "Not in household in the"       
;
label values h5np     h5np;   
label define h5np    
	0           "Not in household in the"       
;
label values h5mis    h5mis;  
label define h5mis   
	0           "Not in household in the"       
	1           "Interview"                     
	2           "Non-interview"                 
	3           "Not in sample"                 
;
label values h5wgt    h5wgt;  
label define h5wgt   
	0           "Not in household in the"       
;
label values fid2     fid2l;  
label define fid2l   
	0           "Not in universe"               
;
label values ftype    ftype;  
label define ftype   
	1           "Primary family"                
	3           "Unrelated subfamily"           
	4           "Primary individual"            
	5           "Secondary individual"          
;
label values fkind    fkind;  
label define fkind   
	1           "Headed by husband/wife"        
	2           "Male reference person"         
	3           "Female reference person"       
;
label values fnkids   fnkids; 
label define fnkids  
	0           "None"                          
	1           "One"                           
	2           "Two"                           
	30          "Thirty or more"                
;
label values fownkid  fownkid;
label define fownkid 
	0           "None"                          
	1           "One"                           
	2           "Two"                           
	30          "Thirty or more"                
;
label values foklt18  foklt18l;
label define foklt18l
	0           "None"                          
	1           "One"                           
	2           "Two"                           
	30          "Thirty or more"                
;
label values fnssr    fnssr;  
label define fnssr   
	0           "None"                          
;
label values sid      sid;    
label define sid     
	0           "Not in sub family"             
;
label values stype    stype;  
label define stype   
	0           "Not in related subfamily"      
	2           "Related subfamily"             
;
label values skind    skind;  
label define skind   
	0           "Not applicable"                
	1           "Headed by husband/wife"        
	2           "Male reference person"         
	3           "Female reference person"       
;
label values sownkid  sownkid;
label define sownkid 
	0           "None"                          
	1           "One"                           
	2           "Two"                           
	30          "Thirty or more"                
;
label values soklt18  soklt18l;
label define soklt18l
	0           "None"                          
	1           "One"                           
	2           "Two"                           
	30          "Thirty or more"                
;
label values intvw    intvw;  
label define intvw   
	0           "Not applicable (children"      
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z refusal" 
	4           "Noninterview - Type Z other"   
	5           "Noninterview - left before"    
;
label values mis5     mis5l;  
label define mis5l   
	1           "Interview"                     
	2           "Non-interview"                 
;
label values rrp      rrp;    
label define rrp     
	1           "Household reference person,"   
	2           "Household reference person"    
	3           "Spouse of household reference" 
	4           "Child of household reference"  
	5           "Other relative of household"   
	6           "Non-relative of household"     
	7           "Non-relative of household"     
;
label values rrpu     rrpu;   
label define rrpu    
	1           "Reference person with"         
	2           "Reference person with no"      
	3           "Husband/wife of reference"     
	4           "Natural/adopted child of"      
	5           "Stepchild of reference person" 
	6           "Foster child of reference person"
	7           "Grandchild of reference person"
	8           "Parent of reference person"    
	9           "Brother/sister of reference"   
	10          "Other relative of reference"   
	11          "Non-relative of reference person"
	12          "Partner/roommate of reference" 
	13          "Non-relative of reference person"
;
label values age      age;    
label define age     
	0           "Less than 1 full year"         
	1           "1 year"                        
	85          "85 years or older"             
;
label values popstat  popstat;
label define popstat 
	1           "Adult  (15 years of age or"    
	2           "Child  (under 15 at interview)"
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
label values ethncty  ethncty;
label define ethncty 
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
label values ms       ms;     
label define ms      
	0           "Not a sample person this month"
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
;
label values msu      msu;    
label define msu     
	0           "Not applicable"                
	1           "Married, spouse present"       
	2           "Married, spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never married"                 
	9           "Not answered"                  
;
label values ewid     ewid;   
label define ewid    
	0           "Not in universe"               
	1           "Widowed"                       
	2           "Divorced"                      
	3           "Both widowed and divorced"     
	4           "No"                            
;
label values famtyp   famtyp; 
label define famtyp  
	0           "Primary family or not in"      
	1           "Secondary individual (not a"   
	2           "Unrelated sub (secondary)"     
	3           "Related subfamily"             
	4           "Primary individual"            
;
label values famrel   famrel; 
label define famrel  
	0           "Not applicable or not in"      
	1           "Reference person of family"    
	2           "Spouse of family reference"    
	3           "Child of family reference person"
;
label values pnsp     pnsp;   
label define pnsp    
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnspu    pnspu;  
label define pnspu   
	0           "Not answered"                  
	999         "Not applicable"                
;
label values pnpt     pnpt;   
label define pnpt    
	0           "Not in sample this month"      
	999         "Not applicable"                
;
label values pnptu    pnptu;  
label define pnptu   
	0           "Not answered"                  
	999         "Not applicable"                
;
label values pngdu    pngdu;  
label define pngdu   
	0           "Not answered / not applicable" 
	999         "No parent or guardian"         
;
label values desgpnpt desgpnpt;
label define desgpnpt
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values realft   realft; 
label define realft  
	0           "Not applicable or not answered"
	5           "Left - deceased"               
	6           "Left - institutionalized"      
	7           "Left - living in Armed Forces" 
	8           "Left - moved outside of country"
	9           "Left - separation or divorce"  
	10          "Left - person #201+  no"       
	11          "Left - other"                  
	12          "Left - entered merged household"
	25          "Left - deceased"               
	26          "Left - institutionalized"      
	27          "Left - living in Armed Forces" 
	28          "Left - moved outside of country"
	29          "Left - separation or divorce"  
	30          "Left - 201+ person no longer"  
	31          "Left - other"                  
	99          "Listed in error"               
;
label values reaent   reaent; 
label define reaent  
	0           "Not applicable or not answered"
	1           "Entered - birth"               
	2           "Entered - marriage"            
	3           "Entered - other"               
	4           "Entered (before sample people)"
	13          "Reentered household after"     
	16          "Entered - from institution"    
	17          "Entered - from Armed Forces"   
	18          "Entered - from outside the"    
	19          "Entered - due to separation or"
	21          "Entered - birth"               
	22          "Entered - marriage"            
	23          "Entered - other"               
	24          "Entered - sample person added" 
	36          "Entered - from institution"    
	37          "Entered - from Armed Forces"   
	38          "Entered - from outside the"    
	39          "Entered - due to separation"   
;
label values hchange  hchange;
label define hchange 
	1           "Yes"                           
	2           "No"                            
;
label values fchange  fchange;
label define fchange 
	1           "Yes"                           
	2           "No"                            
;
label values schange  schange;
label define schange 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values sc1000   sc1000l;
label define sc1000l 
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values esr      esr;    
label define esr     
	0           "Not applicable"                
	1           "With a job entire month,"      
	2           "With a job entire month,"      
	3           "With a job entire month,"      
	4           "With job one or more weeks, no"
	5           "With job one or more weeks,"   
	6           "No job during month, spent"    
	7           "No job during month, spent one"
	8           "No job during month, no time"  
;
label values weeks    weeks;  
label define weeks   
	4           "Four weeks"                    
	5           "Five weeks"                    
;
label values wesr1    wesr1l; 
label define wesr1l  
	0           "Not in universe"               
	1           "With a job this week"          
	2           "With a job, absent without pay,"
	3           "With a job, absent without pay,"
	4           "Looking for a job this week"   
	5           "Without a job, not looking for"
;
label values wesr2    wesr2l; 
label define wesr2l  
	0           "Not in universe"               
	1           "With a job this week"          
	2           "With a job, absent without pay,"
	3           "With a job, absent without pay,"
	4           "Looking for a job this week"   
	5           "Without a job, not looking for"
;
label values wesr3    wesr3l; 
label define wesr3l  
	0           "Not in universe"               
	1           "With a job this week"          
	2           "With a job, absent without pay,"
	3           "With a job, absent without pay,"
	4           "Looking for a job this week"   
	5           "Without a job, not looking for"
;
label values wesr4    wesr4l; 
label define wesr4l  
	0           "Not in universe"               
	1           "With a job this week"          
	2           "With a job, absent without pay,"
	3           "With a job, absent without pay,"
	4           "Looking for a job this week"   
	5           "Without a job, not looking for"
;
label values wesr5    wesr5l; 
label define wesr5l  
	0           "Not in universe or no week 5"  
	1           "With a job this week"          
	2           "With a job, absent without pay,"
	3           "With a job, absent without pay,"
	4           "Looking for a job this week"   
	5           "Without a job, not looking for"
;
label values wksjob   wksjob; 
label define wksjob  
	0           "None, or not in universe"      
	1           "1 week"                        
	5           "5 weeks"                       
;
label values wkswop   wkswop; 
label define wkswop  
	0           "None, or not in universe"      
	1           "1 week"                        
	5           "5 weeks"                       
;
label values wkslok   wkslok; 
label define wkslok  
	0           "None, or not in universe"      
	1           "1 week"                        
	5           "5 weeks"                       
;
label values reasab   reasab; 
label define reasab  
	0           "Not in universe"               
	1           "On layoff"                     
	2           "Own illness"                   
	3           "On vacation"                   
	4           "Bad weather"                   
	5           "Labor dispute"                 
	6           "New job to begin within 30 days"
	7           "Other"                         
;
label values takjob   takjob; 
label define takjob  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values takjobn  takjobn;
label define takjobn 
	0           "Not in universe"               
	1           "Already had a job"             
	2           "Temporary illness"             
	3           "School"                        
	4           "Other"                         
;
label values cwork    cwork;  
label define cwork   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values uhours   uhours; 
label define uhours  
	0           "Not in universe or none"       
;
label values wkspt    wkspt;  
label define wkspt   
	0           "None, or not in universe"      
	1           "1 week"                        
	5           "5 weeks"                       
;
label values wksptr   wksptr; 
label define wksptr  
	0           "Not in universe"               
	1           "Could not find a full-time job"
	2           "Wanted to work part-time"      
	3           "Health condition or disability"
	4           "Normal working hours are less" 
	5           "Slack work or material shortage"
	6           "Other"                         
;
label values empled   empled; 
label define empled  
	0           "Not in universe"               
	1           "Worked for employer only"      
	2           "Self-employed only"            
	3           "Worked for both employer and"  
;
label values disab    disab;  
label define disab   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values rhcdis   rhcdis; 
label define rhcdis  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values vetstat  vetstat;
label define vetstat 
	0           "Not applicable (under 15) or"  
	1           "Yes"                           
	2           "No"                            
;
label values inaf     inaf;   
label define inaf    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values spinaf   spinaf; 
label define spinaf  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values usrvdt1  usrvdt1l;
label define usrvdt1l
	0           "Not applicable"                
	1           "Vietnam era (Aug'64-Apr'75)"   
	2           "Korean conflict (June'50-Jan'55)"
	3           "World War II (Sept'40-July'47)"
	5           "May 1975 to August 1980"       
	6           "September 1980 or later"       
	7           "Other service (all other"      
	9           "Not answered"                  
;
label values usrvdt2  usrvdt2l;
label define usrvdt2l
	0           "Not applicable"                
	1           "Vietnam era (Aug'64-Apr'75)"   
	2           "Korean conflict (June'50-Jan'55)"
	3           "World War II (Sept'40-July'47)"
	5           "May 1975 to August 1980"       
	6           "September 1980 or later"       
	7           "Other service (all other"      
	9           "Not answered"                  
;
label values usrvdt3  usrvdt3l;
label define usrvdt3l
	0           "Not applicable"                
	1           "Vietnam era (Aug'64-Apr'75)"   
	2           "Korean conflict (June'50-Jan'55)"
	3           "World War II (Sept'40-July'47)"
	5           "May 1975 to August 1980"       
	6           "September 1980 or later"       
	7           "Other service (all other"      
	9           "Not answered"                  
;
label values aftime   aftime; 
label define aftime  
	0           "Not in universe"               
	1           "Less than 6 months"            
	2           "6 to 23 months"                
	3           "2 to 19 years"                 
	4           "20 or more years"              
	5           "Don't know"                    
;
label values afdsab   afdsab; 
label define afdsab  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
	3           "Don't know"                    
;
label values afdpct   afdpct; 
label define afdpct  
	0           "Not in universe"               
	1           "1-10   percent"                
	2           "11-29  percent"                
	3           "30-49  percent"                
	4           "50     percent"                
	5           "51-89  percent"                
	6           "90-99  percent"                
	7           "100    percent"                
;
label values spdaf    spdaf;  
label define spdaf   
	0           "Not in universe"               
	1           "Yes, in the service"           
	2           "Yes, from service-related"     
	3           "No"                            
;
label values vets     vets;   
label define vets    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values vetsmt   vetsmt; 
label define vetsmt  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
	3           "Don't know"                    
;
label values vetnum   vetnum; 
label define vetnum  
	0           "Not applicable"                
;
label values retird   retird; 
label define retird  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values socsec   socsec; 
label define socsec  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values socsr1   socsr1l;
label define socsr1l 
	0           "Not in universe"               
	1           "Retired"                       
	2           "Disabled"                      
	3           "Widow(ed) or surviving child"  
	4           "Spouse or dependent child"     
	5           "Some other reason"             
	6           "Don't know"                    
;
label values socsr2   socsr2l;
label define socsr2l 
	0           "Not in universe"               
	1           "Retired"                       
	2           "Disabled"                      
	3           "Widow(ed) or surviving child"  
	4           "Spouse or dependent child"     
	5           "No other reason"               
	6           "Don't know"                    
;
label values disage   disage; 
label define disage  
	0           "Not in universe"               
;
label values railrd   railrd; 
label define railrd  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values carecov  carecov;
label define carecov 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values medcode  medcode;
label define medcode 
	0           "Not in universe"               
	1           "Retired or disabled worker"    
	2           "Spouse of retired or disabled" 
	3           "Widow of retired or disabled"  
	4           "Adult disabled as a child"     
	5           "Uninsured"                     
	7           "Other or invalid code"         
	9           "Missing code"                  
;
label values mcopt    mcopt;  
label define mcopt   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values foodstp  foodstp;
label define foodstp 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values afdc     afdc;   
label define afdc    
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values genasst  genasst;
label define genasst 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values fostkid  fostkid;
label define fostkid 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values othwelf  othwelf;
label define othwelf 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wiccov   wiccov; 
label define wiccov  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values wicval   wicval; 
label define wicval  
	0           "None"                          
;
label values caidcov  caidcov;
label define caidcov 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values hiind    hiind;  
label define hiind   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values hinonh   hinonh; 
label define hinonh  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values champ    champ;  
label define champ   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values hiown    hiown;  
label define hiown   
	0           "Not in universe"               
	1           "Plan in own name"              
	2           "Someone else's plan"           
	3           "Both"                          
;
label values hisrc    hisrc;  
label define hisrc   
	0           "Not in universe"               
	1           "Current employer or union"     
	2           "Former employer"               
	3           "CHAMPUS"                       
	4           "CHAMPVA"                       
	5           "Military"                      
	6           "Other"                         
;
label values hipay    hipay;  
label define hipay   
	0           "Not in universe"               
	1           "All"                           
	2           "Part"                          
	3           "None"                          
;
label values hitype   hitype; 
label define hitype  
	0           "Not in universe"               
	1           "Individual"                    
	2           "Family"                        
;
label values hifam    hifam;  
label define hifam   
	0           "Not applicable"                
	1           "Yes, All persons covered"      
	2           "No, some persons covered"      
	3           "No one else covered"           
;
label values nonhhi   nonhhi; 
label define nonhhi  
	0           "Not applicable"                
	1           "Yes, spouse"                   
	2           "Yes, child(ren)"               
	3           "Yes, someone else"             
	4           "Yes, spouse and child(ren)"    
	5           "Yes, spouse and someone else"  
	6           "Yes, child(ren) and someone"   
	7           "Yes, spouse, children, and"    
	8           "No"                            
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
label values enrold   enrold; 
label define enrold  
	0           "Not in universe"               
	1           "Yes, full-time"                
	2           "Yes, part-time"                
	3           "No"                            
;
label values level    level;  
label define level   
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
label values edasst   edasst; 
label define edasst  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values othinc   othinc; 
label define othinc  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values noinc    noinc;  
label define noinc   
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values pwrrp    pwrrp;  
label define pwrrp   
	0           "Not applicable"                
	1           "Reference person with"         
	2           "Reference person with no rel." 
	3           "Husband/wife of reference person"
	4           "Natural/adopted child of"      
	5           "Stepchild of reference person" 
	6           "Foster child of reference"     
	7           "Grandchild of reference person"
	8           "Parent of reference person"    
	9           "Brother/sister of reference"   
	10          "Other relative of reference"   
	11          "Non-relative of reference person"
	12          "Partner/roommate of reference" 
	13          "Non-relative of reference person"
;
label values isex     isex;   
label define isex    
	0           "Not changed"                   
	1           "Changed"                       
;
label values irace    irace;  
label define irace   
	0           "Not changed"                   
	1           "Changed"                       
;
label values iethncty iethncty;
label define iethncty
	0           "Not changed"                   
	1           "Changed"                       
;
label values ihigrade ihigrade;
label define ihigrade
	0           "Not changed"                   
	1           "Changed"                       
;
label values igrdcmpl igrdcmpl;
label define igrdcmpl
	0           "Not changed"                   
	1           "Changed"                       
;
label values iewid    iewid;  
label define iewid   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iwksjob  iwksjob;
label define iwksjob 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iwkswop  iwkswop;
label define iwkswop 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iwkslok  iwkslok;
label define iwkslok 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ireasab  ireasab;
label define ireasab 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values itakjob  itakjob;
label define itakjob 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values itakjobn itakjobn;
label define itakjobn
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values icwork   icwork; 
label define icwork  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iuhours  iuhours;
label define iuhours 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iwkspt   iwkspt; 
label define iwkspt  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iwksptr  iwksptr;
label define iwksptr 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values idisab   idisab; 
label define idisab  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values idisage  idisage;
label define idisage 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values irhcdis  irhcdis;
label define irhcdis 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ivetstat ivetstat;
label define ivetstat
	0           "Not changed"                   
	1           "Changed"                       
;
label values iinaf    iinaf;  
label define iinaf   
	0           "Not changed"                   
	1           "Changed"                       
;
label values ispinaf  ispinaf;
label define ispinaf 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ispdaf   ispdaf; 
label define ispdaf  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iretird  iretird;
label define iretird 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values icarecov icarecov;
label define icarecov
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values imcopt   imcopt; 
label define imcopt  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values icaidcov icaidcov;
label define icaidcov
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ihiind   ihiind; 
label define ihiind  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ihiown   ihiown; 
label define ihiown  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ihisrc   ihisrc; 
label define ihisrc  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ihipay   ihipay; 
label define ihipay  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ihitype  ihitype;
label define ihitype 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values inonhhi  inonhhi;
label define inonhhi 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ienrold  ienrold;
label define ienrold 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ilevel   ilevel; 
label define ilevel  
	0           "Not Imputed"                   
	1           "Imputed"                       
;
label values iedasst  iedasst;
label define iedasst 
	0           "Not Imputed"                   
	1           "Imputed"                       
;
label values igibill  igibill;
label define igibill 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iothvet  iothvet;
label define iothvet 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iwkstdy  iwkstdy;
label define iwkstdy 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ipell    ipell;  
label define ipell   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values isupped  isupped;
label define isupped 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values indsl    indsl;  
label define indsl   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values istloan  istloan;
label define istloan 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ijtpa    ijtpa;  
label define ijtpa   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iemplyr  iemplyr;
label define iemplyr 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ifsship  ifsship;
label define ifsship 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iothaid  iothaid;
label define iothaid 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values njobs    njobs;  
label define njobs   
	0           "Not in universe"               
	1           "Only 1 job"                    
	2           "Dual jobs all month"           
	3           "Dual jobs not all month"       
	4           "Dual jobs without overlapping" 
	5           "No jobs this month"            
;
label values ws12003  ws12003l;
label define ws12003l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws12004  ws12004l;
label define ws12004l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws1occ   ws1occ; 
label define ws1occ  
	0           "Not in universe"               
;
label values ws1ind   ws1ind; 
label define ws1ind  
	0           "Not in universe"               
;
label values ws1wks   ws1wks; 
label define ws1wks  
	0           "None, or not in universe"      
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
;
label values ws1amt   ws1amt; 
label define ws1amt  
	0           "None, or not in universe"      
;
label values ws12002  ws12002l;
label define ws12002l
	0           "Not in universe"               
;
label values ws12012  ws12012l;
label define ws12012l
	0           "Not in universe"               
	1           "A private for-profit company or"
	2           "A private not-for-profit,"     
	3           "Federal government ?"          
	4           "State government ?"            
	5           "Local government ?"            
	6           "Armed Forces ?"                
	7           "Unpaid in family business or"  
;
label values ws1chg   ws1chg; 
label define ws1chg  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws12018  ws12018l;
label define ws12018l
	0           "Not in universe"               
;
label values ws12016  ws12016l;
label define ws12016l
	0           "Not in universe"               
;
label values ws12022  ws12022l;
label define ws12022l
	0           "Not in universe"               
;
label values ws12020  ws12020l;
label define ws12020l
	0           "Not in universe"               
;
label values ws12023  ws12023l;
label define ws12023l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws12024  ws12024l;
label define ws12024l
	0           "Not in universe"               
	1           "Laid off"                      
	2           "Retired"                       
	3           "Discharged"                    
	4           "Job was temporary and ended"   
	5           "Quit to take another job"      
	6           "Quit for some other reason"    
;
label values ws12025  ws12025l;
label define ws12025l
	0           "Not in universe / none"        
;
label values ws12026  ws12026l;
label define ws12026l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws12028  ws12028l;
label define ws12028l
	0           "Not in universe"               
;
label values ws12029  ws12029l;
label define ws12029l
	0           "Not in universe"               
	1           "Once a week"                   
	2           "Once each 2 weeks"             
	3           "Once a month"                  
	4           "Twice a month"                 
	5           "Unpaid in family business or"  
	6           "Some other way"                
;
label values ws12031  ws12031l;
label define ws12031l
	0           "Not in universe"               
	32          "Not paid during reference"     
;
label values ws12030  ws12030l;
label define ws12030l
	0           "Not in universe"               
	13          "Not paid during reference"     
;
label values ws12044  ws12044l;
label define ws12044l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws12046  ws12046l;
label define ws12046l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values iws1occ  iws1occ;
label define iws1occ 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws1ind  iws1ind;
label define iws1ind 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12012 iws1201d;
label define iws1201d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12024 iws1202d;
label define iws1202d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12026 iws1202k;
label define iws1202k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12028 iws1202l;
label define iws1202l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12029 iws1202m;
label define iws1202m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12031 iws1203d;
label define iws1203d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12030 iws1203k;
label define iws1203k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12044 iws1204d;
label define iws1204d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws12046 iws1204k;
label define iws1204k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws1calc  ws1calc;
label define ws1calc 
	0           "Not in universe"               
	1           "Imputed"                       
	2           "Not imputed"                   
;
label values ws22103  ws22103l;
label define ws22103l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws22104  ws22104l;
label define ws22104l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws2occ   ws2occ; 
label define ws2occ  
	0           "Not in universe"               
;
label values ws2ind   ws2ind; 
label define ws2ind  
	0           "Not in universe"               
;
label values ws2wks   ws2wks; 
label define ws2wks  
	0           "None, or not in universe"      
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
;
label values ws2amt   ws2amt; 
label define ws2amt  
	0           "None, or not in universe"      
;
label values ws22102  ws22102l;
label define ws22102l
	0           "Not in universe"               
;
label values ws22112  ws22112l;
label define ws22112l
	0           "Not in universe"               
	1           "A private for-profit company or"
	2           "A private not-for-profit,"     
	3           "Federal government"            
	4           "State government ?"            
	5           "Local government ?"            
	6           "Armed Forces ?"                
	7           "Unpaid in family business or"  
;
label values ws2chg   ws2chg; 
label define ws2chg  
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws22118  ws22118l;
label define ws22118l
	0           "Not in universe"               
;
label values ws22116  ws22116l;
label define ws22116l
	0           "Not in universe"               
;
label values ws22122  ws22122l;
label define ws22122l
	0           "Not in universe"               
;
label values ws22120  ws22120l;
label define ws22120l
	0           "Not in universe"               
;
label values ws22123  ws22123l;
label define ws22123l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws22124  ws22124l;
label define ws22124l
	0           "Not in universe"               
	1           "Laid off"                      
	2           "Retired"                       
	3           "Discharged"                    
	4           "Job was temporary and ended"   
	5           "Quit to take another job"      
	6           "Quit for some other reason"    
;
label values ws22125  ws22125l;
label define ws22125l
	0           "Not in universe / none"        
;
label values ws22126  ws22126l;
label define ws22126l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws22128  ws22128l;
label define ws22128l
	0           "Not in universe"               
;
label values ws22129  ws22129l;
label define ws22129l
	0           "Not in universe"               
	1           "Once a week"                   
	2           "Once each 2 weeks"             
	3           "Once a month"                  
	4           "Twice a month"                 
	5           "Unpaid in family business or"  
	6           "Some other way"                
;
label values ws22131  ws22131l;
label define ws22131l
	0           "Not in universe"               
	32          "Not paid during reference"     
;
label values ws22130  ws22130l;
label define ws22130l
	0           "Not in universe"               
	13          "Not paid during reference period"
;
label values ws22144  ws22144l;
label define ws22144l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values ws22146  ws22146l;
label define ws22146l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values iws2occ  iws2occ;
label define iws2occ 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws2ind  iws2ind;
label define iws2ind 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22112 iws2211d;
label define iws2211d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22124 iws2212d;
label define iws2212d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22126 iws2212k;
label define iws2212k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22128 iws2212l;
label define iws2212l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22129 iws2212m;
label define iws2212m
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22131 iws2213d;
label define iws2213d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22130 iws2213k;
label define iws2213k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22144 iws2214d;
label define iws2214d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iws22146 iws2214k;
label define iws2214k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ws2calc  ws2calc;
label define ws2calc 
	0           "Not in universe"               
	1           "Imputed"                       
	2           "Not imputed"                   
;
label values se12202  se12202l;
label define se12202l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12203  se12203l;
label define se12203l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se1ind   se1ind; 
label define se1ind  
	0           "Not in universe"               
	1           "Agriculture,forestry,fisheries"
	2           "Mining (040-050)"              
	3           "Construction (060)"            
	4           "Manufacturing-nondurable goods"
	5           "Manufacturing-durable goods"   
	6           "Transportation,communications,"
	7           "Wholesale trade-durable goods" 
	8           "Wholesale trade-nondurable"    
	9           "Retail trade (580-691)"        
	10          "Finance,insurance,real estate" 
	11          "Business and repair services"  
	12          "Personal services (761-791)"   
	13          "Entertainment and recreation"  
	14          "Professional and related"      
	15          "Public administration (900-932)"
	16          "Industry not reported (990)"   
;
label values se1occ   se1occ; 
label define se1occ  
	0           "Not in universe"               
;
label values se1wks   se1wks; 
label define se1wks  
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se1amt   se1amt; 
label define se1amt  
	0           "None, or not in universe"      
;
label values se12201  se12201l;
label define se12201l
	0           "Not answered"                  
;
label values se12212  se12212l;
label define se12212l
	0           "Not in universe / none"        
;
label values se12214  se12214l;
label define se12214l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12218  se12218l;
label define se12218l
	0           "Not in universe"               
	3           "3 - 5 employees"               
	4           "6 or more employees"           
;
label values se12220  se12220l;
label define se12220l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12222  se12222l;
label define se12222l
	0           "Not in universe"               
	1           "Sole proprietorship"           
	2           "Partnership"                   
;
label values se12224  se12224l;
label define se12224l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
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
label values se12252  se12252l;
label define se12252l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12254  se12254l;
label define se12254l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se12256  se12256l;
label define se12256l
	0           "None, or not in universe"      
;
label values se12260  se12260l;
label define se12260l
	0           "None, or not in universe"      
;
label values ise1occ  ise1occ;
label define ise1occ 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise1ind  ise1ind;
label define ise1ind 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12214 ise1221d;
label define ise1221d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12218 ise1221k;
label define ise1221k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12220 ise1222d;
label define ise1222d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12222 ise1222k;
label define ise1222k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12232 ise1223d;
label define ise1223d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12234 ise1223k;
label define ise1223k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12254 ise1225d;
label define ise1225d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12256 ise1225k;
label define ise1225k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise12260 ise1226d;
label define ise1226d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise1amt  ise1amt;
label define ise1amt 
	0           "Not in universe"               
	1           "Imputed"                       
	2           "Not inputed"                   
;
label values se22302  se22302l;
label define se22302l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22303  se22303l;
label define se22303l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se2ind   se2ind; 
label define se2ind  
	0           "Not in universe"               
	1           "Agriculture,forestry,fisheries"
	2           "Mining (040-050)"              
	3           "Construction (060)"            
	4           "Manufacturing-nondurable"      
	5           "Manufacturing-durable"         
	6           "Transportation,communications,"
	7           "Wholesale trade-durable"       
	8           "Wholesale trade-nondurable"    
	9           "Retail trade (580-691)"        
	10          "Finance,insurance,real estate" 
	11          "Business and repair services"  
	12          "Personal services (761-791)"   
	13          "Entertainment and recreation"  
	14          "Professional and related"      
	15          "Public administration (900-932)"
	16          "Industry not reported (990)"   
;
label values se2occ   se2occ; 
label define se2occ  
	0           "Not in universe"               
;
label values se2wks   se2wks; 
label define se2wks  
	0           "None"                          
	1           "1 week"                        
	2           "2 weeks"                       
	3           "3 weeks"                       
	4           "4 weeks"                       
	5           "5 weeks"                       
	9           "Not in universe"               
;
label values se2amt   se2amt; 
label define se2amt  
	0           "None, or not in universe"      
;
label values se22301  se22301l;
label define se22301l
	0           "Not answered"                  
;
label values se22312  se22312l;
label define se22312l
	0           "Not in universe / none"        
;
label values se22314  se22314l;
label define se22314l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22318  se22318l;
label define se22318l
	0           "Not in universe"               
	3           "3 - 5 employees"               
	4           "6 or more employees"           
;
label values se22320  se22320l;
label define se22320l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22322  se22322l;
label define se22322l
	0           "Not in universe"               
	1           "Sole proprietorship"           
	2           "Partnership"                   
;
label values se22324  se22324l;
label define se22324l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
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
label values se22352  se22352l;
label define se22352l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22354  se22354l;
label define se22354l
	0           "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values se22356  se22356l;
label define se22356l
	0           "Not in universe"               
;
label values se22360  se22360l;
label define se22360l
	0           "None, or not in universe"      
;
label values ise2occ  ise2occ;
label define ise2occ 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise2ind  ise2ind;
label define ise2ind 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22314 ise2231d;
label define ise2231d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22318 ise2231k;
label define ise2231k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22320 ise2232d;
label define ise2232d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22322 ise2232k;
label define ise2232k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22332 ise2233d;
label define ise2233d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22334 ise2233k;
label define ise2233k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22354 ise2235d;
label define ise2235d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22356 ise2235k;
label define ise2235k
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise22360 ise2236d;
label define ise2236d
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ise2amt  ise2amt;
label define ise2amt 
	0           "Not in universe"               
	1           "Imputed"                       
	2           "Not imputed"                   
;
label values r01a     r01a;   
label define r01a    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r01k     r01k;   
label define r01k    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r02a     r02a;   
label define r02a    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r02k     r02k;   
label define r02k    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r03      r03l;   
label define r03l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r05      r05l;   
label define r05l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r06      r06l;   
label define r06l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r07      r07l;   
label define r07l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r08      r08l;   
label define r08l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r10      r10l;   
label define r10l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r12      r12l;   
label define r12l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r13      r13l;   
label define r13l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r20      r20l;   
label define r20l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r21      r21l;   
label define r21l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r23      r23l;   
label define r23l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r24      r24l;   
label define r24l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r25      r25l;   
label define r25l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r27      r27l;   
label define r27l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r28      r28l;   
label define r28l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r29      r29l;   
label define r29l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r30      r30l;   
label define r30l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r31      r31l;   
label define r31l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r32      r32l;   
label define r32l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r34      r34l;   
label define r34l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r35      r35l;   
label define r35l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r36      r36l;   
label define r36l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r37      r37l;   
label define r37l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r38      r38l;   
label define r38l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r40      r40l;   
label define r40l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r41      r41l;   
label define r41l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r50      r50l;   
label define r50l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r51      r51l;   
label define r51l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r52      r52l;   
label define r52l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r53      r53l;   
label define r53l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r54      r54l;   
label define r54l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r55      r55l;   
label define r55l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r56      r56l;   
label define r56l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values r75      r75l;   
label define r75l    
	0           "Not applicable"                
	1           "Yes, recipiency this month"    
	2           "No recipiency this month"      
;
label values s01amta  s01amta;
label define s01amta 
	0           "None"                          
;
label values s01amtk  s01amtk;
label define s01amtk 
	0           "None"                          
;
label values s02amta  s02amta;
label define s02amta 
	0           "None"                          
;
label values s02amtk  s02amtk;
label define s02amtk 
	0           "None"                          
;
label values s03amt   s03amt; 
label define s03amt  
	0           "None"                          
;
label values s05amt   s05amt; 
label define s05amt  
	0           "None"                          
;
label values s06amt   s06amt; 
label define s06amt  
	0           "None"                          
;
label values s07amt   s07amt; 
label define s07amt  
	0           "None"                          
;
label values s08amt   s08amt; 
label define s08amt  
	0           "None"                          
;
label values s10amt   s10amt; 
label define s10amt  
	0           "None"                          
;
label values s12amt   s12amt; 
label define s12amt  
	0           "None"                          
;
label values s13amt   s13amt; 
label define s13amt  
	0           "None"                          
;
label values s20amt   s20amt; 
label define s20amt  
	0           "None"                          
;
label values s21amt   s21amt; 
label define s21amt  
	0           "None"                          
;
label values s23amt   s23amt; 
label define s23amt  
	0           "None"                          
;
label values s24amt   s24amt; 
label define s24amt  
	0           "None"                          
;
label values s27amt   s27amt; 
label define s27amt  
	0           "None"                          
;
label values s28amt   s28amt; 
label define s28amt  
	0           "None"                          
;
label values s29amt   s29amt; 
label define s29amt  
	0           "None"                          
;
label values s30amt   s30amt; 
label define s30amt  
	0           "None"                          
;
label values s31amt   s31amt; 
label define s31amt  
	0           "None"                          
;
label values s32amt   s32amt; 
label define s32amt  
	0           "None"                          
;
label values s34amt   s34amt; 
label define s34amt  
	0           "None"                          
;
label values s35amt   s35amt; 
label define s35amt  
	0           "None"                          
;
label values s36amt   s36amt; 
label define s36amt  
	0           "None"                          
;
label values s37amt   s37amt; 
label define s37amt  
	0           "None"                          
;
label values s38amt   s38amt; 
label define s38amt  
	0           "None"                          
;
label values s40amt   s40amt; 
label define s40amt  
	0           "None"                          
;
label values s41amt   s41amt; 
label define s41amt  
	0           "None"                          
;
label values s50amt   s50amt; 
label define s50amt  
	0           "None"                          
;
label values s51amt   s51amt; 
label define s51amt  
	0           "None"                          
;
label values s52amt   s52amt; 
label define s52amt  
	0           "None"                          
;
label values s53amt   s53amt; 
label define s53amt  
	0           "None"                          
;
label values s54amt   s54amt; 
label define s54amt  
	0           "None"                          
;
label values s55amt   s55amt; 
label define s55amt  
	0           "None"                          
;
label values s56amt   s56amt; 
label define s56amt  
	0           "None"                          
;
label values s75amt   s75amt; 
label define s75amt  
	0           "None"                          
;
label values ir01a    ir01a;  
label define ir01a   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir01k    ir01k;  
label define ir01k   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir02a    ir02a;  
label define ir02a   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir03     ir03l;  
label define ir03l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir05     ir05l;  
label define ir05l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir06     ir06l;  
label define ir06l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir07     ir07l;  
label define ir07l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir08     ir08l;  
label define ir08l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir10     ir10l;  
label define ir10l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir12     ir12l;  
label define ir12l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir13     ir13l;  
label define ir13l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir20     ir20l;  
label define ir20l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir21     ir21l;  
label define ir21l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir23     ir23l;  
label define ir23l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir24     ir24l;  
label define ir24l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir25     ir25l;  
label define ir25l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir27     ir27l;  
label define ir27l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir28     ir28l;  
label define ir28l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir29     ir29l;  
label define ir29l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir30     ir30l;  
label define ir30l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir31     ir31l;  
label define ir31l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir32     ir32l;  
label define ir32l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir34     ir34l;  
label define ir34l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir35     ir35l;  
label define ir35l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir36     ir36l;  
label define ir36l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir37     ir37l;  
label define ir37l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir38     ir38l;  
label define ir38l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir40     ir40l;  
label define ir40l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir41     ir41l;  
label define ir41l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir50     ir50l;  
label define ir50l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir51     ir51l;  
label define ir51l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir52     ir52l;  
label define ir52l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir53     ir53l;  
label define ir53l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir54     ir54l;  
label define ir54l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir55     ir55l;  
label define ir55l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir56     ir56l;  
label define ir56l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is01a    is01a;  
label define is01a   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is01k    is01k;  
label define is01k   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is02a    is02a;  
label define is02a   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is02k    is02k;  
label define is02k   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is03     is03l;  
label define is03l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is05     is05l;  
label define is05l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is06     is06l;  
label define is06l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is07     is07l;  
label define is07l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is08     is08l;  
label define is08l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is10     is10l;  
label define is10l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is12     is12l;  
label define is12l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is13     is13l;  
label define is13l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is20     is20l;  
label define is20l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is21     is21l;  
label define is21l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is23     is23l;  
label define is23l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is24     is24l;  
label define is24l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is27     is27l;  
label define is27l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is28     is28l;  
label define is28l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is29     is29l;  
label define is29l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is30     is30l;  
label define is30l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is31     is31l;  
label define is31l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is32     is32l;  
label define is32l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is34     is34l;  
label define is34l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is35     is35l;  
label define is35l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is36     is36l;  
label define is36l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is37     is37l;  
label define is37l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is38     is38l;  
label define is38l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is40     is40l;  
label define is40l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is41     is41l;  
label define is41l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is50     is50l;  
label define is50l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is51     is51l;  
label define is51l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is52     is52l;  
label define is52l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is53     is53l;  
label define is53l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is54     is54l;  
label define is54l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is55     is55l;  
label define is55l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is56     is56l;  
label define is56l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values is75     is75l;  
label define is75l   
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values r100     r100l;  
label define r100l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r101     r101l;  
label define r101l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r102     r102l;  
label define r102l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r103     r103l;  
label define r103l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values rj10003  rj10003l;
label define rj10003l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ro10003  ro10003l;
label define ro10003l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r104     r104l;  
label define r104l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r105     r105l;  
label define r105l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r106     r106l;  
label define r106l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r107     r107l;  
label define r107l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values rj10407  rj10407l;
label define rj10407l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ro10407  ro10407l;
label define ro10407l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r110     r110l;  
label define r110l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values rj110    rj110l; 
label define rj110l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ro110    ro110l; 
label define ro110l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values rj110ri  rj110ri;
label define rj110ri 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ro110ri  ro110ri;
label define ro110ri 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r120     r120l;  
label define r120l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values rj120    rj120l; 
label define rj120l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ro120    ro120l; 
label define ro120l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values rj120ot  rj120ot;
label define rj120ot 
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r130     r130l;  
label define r130l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values rj130    rj130l; 
label define rj130l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ro130    ro130l; 
label define ro130l  
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r140     r140l;  
label define r140l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values r150     r150l;  
label define r150l   
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values ro14050  ro14050l;
label define ro14050l
	0           "Not applicable"                
	1           "Yes"                           
	2           "No"                            
;
label values j10003   j10003l;
label define j10003l 
	0           "None"                          
;
label values o10003   o10003l;
label define o10003l 
	0           "None"                          
;
label values j10407   j10407l;
label define j10407l 
	0           "None"                          
;
label values o10407   o10407l;
label define o10407l 
	0           "None"                          
;
label values j110     j110l;  
label define j110l   
	0           "None"                          
;
label values o110     o110l;  
label define o110l   
	0           "None"                          
;
label values j110ri   j110ri; 
label define j110ri  
	0           "None"                          
;
label values o110ri   o110ri; 
label define o110ri  
	0           "None"                          
;
label values jgrent   jgrent; 
label define jgrent  
	0           "Not in universe"               
;
label values jnrent   jnrent; 
label define jnrent  
	0           "Not in universe"               
;
label values ogrent   ogrent; 
label define ogrent  
	0           "Not in universe"               
;
label values onrent   onrent; 
label define onrent  
	0           "Not in universe"               
;
label values j120ot   j120ot; 
label define j120ot  
	0           "None"                          
;
label values j130     j130l;  
label define j130l   
	0           "None"                          
;
label values o130     o130l;  
label define o130l   
	0           "None"                          
;
label values o14050   o14050l;
label define o14050l 
	0           "None"                          
;
label values cj10003  cj10003l;
label define cj10003l
	0           "No, not calculated"            
	1           "Yes, interest was calculated"  
;
label values co10003  co10003l;
label define co10003l
	0           "No, not calculated"            
	1           "Yes, calculated"               
;
label values cj10407  cj10407l;
label define cj10407l
	0           "No, not calculated"            
	1           "Yes, interest was calculated"  
;
label values co10407  co10407l;
label define co10407l
	0           "No, not calculated"            
	1           "Yes, calculated"               
;
label values ir100    ir100l; 
label define ir100l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir101    ir101l; 
label define ir101l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir102    ir102l; 
label define ir102l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir103    ir103l; 
label define ir103l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values irj10003 irj1000t;
label define irj1000t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iro10003 iro1000t;
label define iro1000t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir104    ir104l; 
label define ir104l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir105    ir105l; 
label define ir105l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir106    ir106l; 
label define ir106l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir107    ir107l; 
label define ir107l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values irj10407 irj1040t;
label define irj1040t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iro10407 iro1040t;
label define iro1040t
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir110    ir110l; 
label define ir110l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ijo110   ijo110l;
label define ijo110l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ijo110ri ijo110ri;
label define ijo110ri
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir120    ir120l; 
label define ir120l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values irj120   irj120l;
label define irj120l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iro120   iro120l;
label define iro120l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values irj120ot irj120ot;
label define irj120ot
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir130    ir130l; 
label define ir130l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values irj130   irj130l;
label define irj130l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iro130   iro130l;
label define iro130l 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir140    ir140l; 
label define ir140l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ir150    ir150l; 
label define ir150l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ij10003  ij10003l;
label define ij10003l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values io10003  io10003l;
label define io10003l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ij10407  ij10407l;
label define ij10407l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values io10407  io10407l;
label define io10407l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ij110    ij110l; 
label define ij110l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values io110    io110l; 
label define io110l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ij110ri  ij110ri;
label define ij110ri 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values io110ri  io110ri;
label define io110ri 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ijgrent  ijgrent;
label define ijgrent 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ijnrent  ijnrent;
label define ijnrent 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values iogrent  iogrent;
label define iogrent 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ionrent  ionrent;
label define ionrent 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ij120ot  ij120ot;
label define ij120ot 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ij130    ij130l; 
label define ij130l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values io130    io130l; 
label define io130l  
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values io14050  io14050l;
label define io14050l
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values vettyp   vettyp; 
label define vettyp  
	0           "Not in universe"               
	1           "Service connected"             
	2           "Survivor benefits"             
	3           "Veterans' pension"             
	4           "Other Veterans' payments"      
;
label values ivettyp  ivettyp;
label define ivettyp 
	0           "Not imputed"                   
	1           "Imputed"                       
;
label values ssunit   ssunit; 
label define ssunit  
	0           "Not in universe"               
	1           "Individual husband-wife"       
	2           "Joint husband-wife benefits and"
	3           "Individual husband-wife"       
	4           "Joint husband-wife benefits,"  
	5           "Individual adult benefit"      
	6           "Individual adult benefit and"  
	7           "Child benefits only"           
;
label values senvelop senvelop;
label define senvelop
	-1          "Don't know"                    
	0           "Not in universe"               
	1           "Blue"                          
	2           "Buff"                          
	3           "Direct deposit"                
	4           "Other"                         
;
label values ssday    ssday;  
label define ssday   
	-1          "Don't know"                    
	0           "Not in universe"               
	1           "First"                         
	2           "Third"                         
	3           "Other"                         
;
label values renvelop renvelop;
label define renvelop
	-1          "Don't know"                    
	0           "Not in universe"               
	1           "Blue"                          
	2           "Buff"                          
	3           "Direct deposit"                
	4           "Other"                         
;
label values rrday    rrday;  
label define rrday   
	-1          "Don't know"                    
	0           "Not in universe"               
	1           "First"                         
	2           "Third"                         
	3           "Other"                         
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
